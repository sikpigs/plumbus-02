.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "defs.inc"
.include "structs.inc"
.include "macros.inc"

.importzp bios_zp
.import delay_10
.import delay_100

.export putcmd_lcd
.export putchar_lcd
.export lcd_init

lcdcmd_args = bios_zp + .sizeof(BIOSPrivate)

.segment "BIOSRAM"
putcmd_lcd_delay:   .res 1

.segment "BIOS"
clear_lcd:
lcd_init:
    lda     #$51
    jsr     putcmd_lcd
    rts


LCD_CMD_BASE = $41
LCD_CMD_COUNT = ($56 - LCD_CMD_BASE + 1)
;
; Write a command in accumulator to the lcd
;
putcmd_lcd:
    ; Verify input
    sec
    sbc     #LCD_CMD_BASE
    bmi     @putcmd_lcd_done        ; A was beyond low range
    cmp     #LCD_CMD_COUNT
    bcs     @putcmd_lcd_done        ; A was beyond high range

    ; Save registers on the stack
    phy
    phx

    ; Lookup delay from table
    asl                             ; A *= 2 - to index into the command table
    tay
    lda     lcd_cmd_table + 1, y    ; Get the delay by 100uS count
    beq     @putcmd_lcd_pull        ; If delay == 0, instruction is not supported
    sta     putcmd_lcd_delay

    lda     #$FE
    jsr     putchar_lcd

    ; Restore A to original value and send
    tya
    lsr
    clc
    adc     #LCD_CMD_BASE
    jsr     putchar_lcd
    
    ldx     lcd_cmd_table, y        ; X = parameter count
    cpx     #0
    beq     @putcmd_lcd_delay

    ldy     #0
@putcmd_lcd_params:
    lda     (lcdcmd_args + LCDCmdArgs::data_ptr), y
    jsr     putchar_lcd
    iny
    dex
    bne     @putcmd_lcd_params

@putcmd_lcd_delay:
    ldx     putcmd_lcd_delay
    jsr     delay_100

@putcmd_lcd_pull:
    ; Pull registers off the stack
    plx
    ply

@putcmd_lcd_done:
    rts

;
; Writes a char in accumulator to the lcd
;
putchar_lcd:
    sta     ACIA0_DATA
    phx
    ldx     #10             ; @ 9600 baud, each bit is ~104 uS and there are 10 bits
    jsr     delay_100       ; delay 1,000uS
    nop
    nop                 ; two no-ops for 4uS to give 104uS
    plx
    rts


.segment "RODATA"
lcd_cmd_table:
    ; offset by $41
    ; even byte -> parameter count
    ; odd byte -> # of 100uS to delay
    ; $41 - Display On
    .byte 0, 1
    ; $42 Display OFF
    .byte 0, 1
    ; $43, $44 unused
    .repeat 4
        .byte 0
    .endrepeat
    ; $45 Set cursor position
    .byte 1, 1
    ; $46 Cursor home
    .byte 0, 15
    ; $47 Underline cursor On
    .byte 0, 15
    ; $48 Underline cursor OFF
    .byte 0, 15
    ; $49 Move cursor left one place
    .byte 0, 1
    ; $4A Move cursor right one place
    .byte 0, 1
    ; $4B Blinking cursor on
    .byte 0, 1
    ; $4C Blinking cursor off
    .byte 0, 1
    ; $4D unused
    .byte 0, 0
    ; $4E Backspace
    .byte 0, 1
    ; $4F, $50 unused
    .repeat 4
        .byte 0
    .endrepeat
    ; $51 Clear screen
    .byte 0, 15
    ; $52 Set contrast
    .byte 1, 5
    ; $53 Set backlight brightness
    .byte 1, 1
    ; $54 Load custom character
    .byte 9, 2
    ; $55 Move display one place to the right
    .byte 0, 1
    ; $56 Move display one place to the right
    .byte 0, 1

