.setcpu "65C02"
.debuginfo
.include "hw.inc"

.export rst_handler
.export nmi_handler
.export irq_handler
.export putchar
.export getchar
.export putchar_lcd
.export putcmd_lcd
.export bios_params
.import main

.zeropage
read_ptr:           .res 1
write_ptr:          .res 1
bios_params:        .res 2
bios_data_ptr:      .res 2

.segment "BIOSRAM"
input_buffer:       .res $100
putcmd_lcd_delay:   .res 1
sr_data_ptr:        .res 2
sr_bytes_left:      .res 1
sr_status:          .res 1

.segment "BIOS"

rst_handler:
    cli
    cld
    ldx #$FF
    txs

    jsr init_acia_0
    jsr init_lcd
    jsr init_acia_1
    jsr init_buffer
    jsr init_via
    jsr main
:
    jmp :-

RTS_BIT_MASK = %00001000
RECEIVER_FULL_MASK = %00001000

irq_handler:
    pha
    phy
    ; Check for activity on ACIA1
    lda ACIA1_STATUS
    and #RECEIVER_FULL_MASK
    beq @via_irq
    lda ACIA1_DATA
    jsr write_buffer
    
    ; Check buffer size, assert RTS if too full
    jsr buffer_size
    cmp #$F0
    bcc @via_irq
    lda ACIA1_CMD
    and #^RTS_BIT_MASK
    sta ACIA1_CMD

@via_irq:
    lda VIA_IFR
    and VIA_IER
    and #%00000100      ; Shift Register?
    beq @irq_done
    
    lda sr_bytes_left
    beq @sr_done

    lda sr_data_ptr
    sta bios_data_ptr
    lda sr_data_ptr + 1
    sta bios_data_ptr
  
    lda (bios_data_ptr)
    sta VIA_SR

    inc sr_data_ptr
    bne :+
    inc sr_data_ptr + 1
:

    dec sr_bytes_left
    jmp @sr_clr_irq

@sr_done:  
    ; Disable interrupt
    lda #%00000100
    sta VIA_IER

    lda #0
    sta sr_status

@sr_clr_irq:
    lda #%00000100
    sta VIA_IFR

@irq_done:
    ply
    pla

nmi_handler:
    rti

init_via:
    lda #$80            ; Port A, all input, except PA7
    sta VIA_DDRA
    stz VIA_PORTA       ; Port A, all zeroes
    stz VIA_DDRB        ; Port B, all input
    rts

;
; Add char in accumulator to buffer
;
write_buffer:
    phx
    ldx write_ptr
    sta input_buffer, x
    inc write_ptr
    plx
    rts

;
; Read a char from the buffer into accumulator
;
read_buffer:
    phx
    ldx read_ptr
    lda input_buffer, x
    inc read_ptr
    plx
    rts
;
;
; Returns the buffer size in accumulator
;
buffer_size:
    lda write_ptr
    sec
    sbc read_ptr
    rts

init_buffer:
    stz read_ptr
    stz write_ptr
    rts

ACIA_CTRL_9600_8N1 = %00011110
ACIA_CMD_TX_ONLY = %00001011

init_acia_0:
    pha
    stz ACIA0_STATUS
    lda #ACIA_CTRL_9600_8N1
    sta ACIA0_CTRL
    lda #ACIA_CMD_TX_ONLY
    sta ACIA0_CMD
    pla
    rts

clear_lcd:
init_lcd:
    lda #$51
    jsr putcmd_lcd
    rts

ACIA1_CTRL_19200_8N1 = %00011111
ACIA1_CMD_TX_RX_INTR = %10001001

init_acia_1:
    pha
    stz ACIA1_STATUS
    lda #ACIA1_CTRL_19200_8N1
    sta ACIA1_CTRL
    lda #ACIA1_CMD_TX_RX_INTR
    sta ACIA1_CMD
    pla
    rts

;
; Set Shift Register Clock Rate
; Fout = PHI2 / (2 x (T2 + 1))
;
; Input: A = T2 reload low
;        X = T2 reload high
;
shift_set_clk:
    sta VIA_T2CL
    stx VIA_T2CH
    rts

;
; Set Shift Register Data
;
; Inputs:
;   A = byte count
;   X = data pointer low
;   Y = data pointer high
; 
shift_out:
    ; Bail early on 0 bytes
    cmp #0
    beq @sr_busy

    ; Check if we're already active, return if so
    pha
    lda sr_status
    bne @sr_busy
    pla

    sta sr_bytes_left
    stx bios_data_ptr
    stx sr_data_ptr
    sty bios_data_ptr + 1
    sty sr_data_ptr

    lda #1
    sta sr_status
    ; Send 1st byte
    lda (bios_data_ptr)
    sta VIA_SR

    inc bios_data_ptr
    bne :+
    inc bios_data_ptr + 1
:

    dec sr_bytes_left
    beq @sr_done

    lda #%10000100    
    sta VIA_IER
    rts

@sr_done:
    lda #0
    sta sr_status

@sr_busy:
    rts

;
; Read a char into accumulator, sets carry if char was read
; 
getchar:
    sei
    jsr buffer_size
    beq @no_char            ; buffer is empty?
    cmp #$AA
    bcs @skip_rts           ; size >= $AA, do not change RTSB setting
    lda ACIA1_CMD           ; read current cmd reg
    ora #RTS_BIT_MASK       ; set RTSB low
    sta ACIA1_CMD           ; write cmd reg
@skip_rts:
    jsr read_buffer
    jsr putchar
    sec
    cli
    rts
@no_char:
    clc
    cli
    rts

;
; Write a char in accumulator to ACIA1
;
putchar:
    sta ACIA1_DATA
    phx
    ldx #52             ; @ 19200 baud, each it takes ~52uS, there are 10 bits
    jsr delay_10        ; delay 520uS
    nop
    plx
    rts

LCD_CMD_BASE = $41
LCD_CMD_COUNT = ($56 - LCD_CMD_BASE + 1)
;
; Write a command in accumulator to the lcd
;
putcmd_lcd:
    ; Verify input
    sec
    sbc #LCD_CMD_BASE
    bmi @putcmd_lcd_done        ; A was beyond low range
    cmp #LCD_CMD_COUNT
    bcs @putcmd_lcd_done        ; A was beyone high range

    ; Save registers on the stack
    phy
    phx

    ; Lookup delay from table
    asl                         ; A *= 2 - to index into the command table
    tay
    lda lcd_cmd_table + 1, y    ; Get the delay by 100uS count
    beq @putcmd_lcd_pull        ; If delay == 0, instruction is not supported
    sta putcmd_lcd_delay

    lda #$FE
    jsr putchar_lcd

    ; Restore A to original value and send
    tya
    lsr
    clc
    adc #LCD_CMD_BASE
    jsr putchar_lcd
    
    ldx lcd_cmd_table, y        ; X = parameter count
    cpx #0
    beq @putcmd_lcd_delay

    ldy #0
@putcmd_lcd_params:
    lda (bios_params), y
    jsr putchar_lcd
    iny
    dex
    bne @putcmd_lcd_params

@putcmd_lcd_delay:
    ldx putcmd_lcd_delay
    jsr delay_100

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
    sta ACIA0_DATA
    phx
    ldx #10             ; @ 9600 baud, each bit is ~104 uS and there are 10 bits
    jsr delay_100       ; delay 1,000uS
    nop
    nop                 ; two no-ops for 4uS to give 104uS
    plx
    rts

;
; Delay X hundred microseconds
; 
delay_10:
    cpx #0
    beq @delay_10_complete
    phy
@delay_10_loop_0:
    ldy #3
@delay_10_loop_1:
    dey
    bne @delay_10_loop_1
    dex
    bne @delay_10_loop_0
    ply
@delay_10_complete:
    rts

;
; Delay X hundred microseconds
; 
delay_100:
    cpx #0
    beq @delay_100_complete
    phy
@delay_100_loop_0:
    ldy #24
@delay_100_loop_1:
    dey
    bne @delay_100_loop_1
    dex
    bne @delay_100_loop_0
    ply
@delay_100_complete:
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
