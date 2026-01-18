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
.export bios_data_ptr
.export uptime_print

.import main
.import shift_irq
.import shift_init

.zeropage
read_ptr:           .res 1
write_ptr:          .res 1
bios_params:        .res 2
bios_data_ptr:      .res 2
scratch_ptr:        .res 2

.segment "BIOSRAM"
input_buffer:       .res $100
putcmd_lcd_delay:   .res 1
via_ier_mirror:     .res 1

; For Uptime IRQ
uptime_ticks:       .res 1
uptime_seconds:     .res 1
uptime_minutes:     .res 1
uptime_hours:       .res 2

uptime_seconds_latched: .res 1
uptime_minutes_latched: .res 1
uptime_hours_latched:   .res 2

; For divmod_16
rem:        .res 1
base:       .res 1

; For print_u16 / print_u8_02
u16_work:       .res 2
digit_buffer:   .res 16
digit_cnt:      .res 1

.segment "BIOS"

rst_handler:
    cli
    cld
    ldx     #$FF
    txs
    
    stz     putcmd_lcd_delay

    jsr     init_uptime
    jsr     init_acia_0
    jsr     init_lcd
    jsr     init_acia_1
    jsr     init_buffer
    jsr     init_via
    jsr     shift_init
    jsr     main
:
    jmp     :-

RTS_BIT_MASK        = %00001000
RECEIVER_FULL_MASK  = %00001000

irq_handler:
    pha
    phy
    phx

    ; Check for activity on ACIA1
    lda     ACIA1_STATUS
    and     #RECEIVER_FULL_MASK
    beq     @via_irq
    lda     ACIA1_DATA
    jsr     write_buffer
    
    ; Check buffer size, assert RTS if too full
    jsr     buffer_size
    cmp     #$F0
    bcc     @via_irq
    lda     ACIA1_CMD
    and     #^RTS_BIT_MASK
    sta     ACIA1_CMD

@via_irq:
    lda     VIA_IFR
    and     VIA_IER
    sta     via_ier_mirror

@via_sr_irq:
    and     #%00000100      ; Shift Register?
    beq     @via_t1_irq
    jsr     shift_irq

@via_t1_irq:
    lda     via_ier_mirror
    and     #%01000000      ; T1 (uptime)?
    beq     @irq_done
    jsr     uptime_irq

@irq_done:
    plx
    ply
    pla

nmi_handler:
    rti

init_via:
    lda     #$80            ; Port A, all input, except PA7
    sta     VIA_DDRA
    stz     VIA_PORTA       ; Port A, all zeroes
    stz     VIA_DDRB        ; Port B, all input
    rts

init_uptime:
    stz     uptime_ticks
    stz     uptime_seconds
    stz     uptime_minutes
    stz     uptime_hours
    stz     uptime_hours + 1

    lda     #<$270F
    sta     VIA_T1CL
    lda     #>$270F
    sta     VIA_T1CH

    lda     VIA_ACR
    ora     #%11000000
    sta     VIA_ACR

    lda     #%11000000
    sta     VIA_IER

    rts

uptime_irq:
    inc     uptime_ticks
    lda     uptime_ticks
    cmp     #100
    bcc     @uptime_irq_done

    stz     uptime_ticks
    inc     uptime_seconds
    lda     uptime_seconds
    cmp     #60
    bcc     @uptime_irq_done

    stz     uptime_seconds
    inc     uptime_minutes
    lda     uptime_minutes
    cmp     #60
    bcc     @uptime_irq_done

    stz     uptime_minutes
    inc     uptime_hours
    bne     @uptime_irq_done
    inc     uptime_hours + 1

@uptime_irq_done:
    ; Clear IRQ by reading T1CL
    lda     VIA_T1CL
    rts

;
; Add char in accumulator to buffer
;
write_buffer:
    phx
    ldx     write_ptr
    sta     input_buffer, x
    inc     write_ptr
    plx
    rts

;
; Read a char from the buffer into accumulator
;
read_buffer:
    phx
    ldx     read_ptr
    lda     input_buffer, x
    inc     read_ptr
    plx
    rts
;
;
; Returns the buffer size in accumulator
;
buffer_size:
    lda     write_ptr
    sec
    sbc     read_ptr
    rts

init_buffer:
    stz     read_ptr
    stz     write_ptr
    rts

ACIA_CTRL_9600_8N1 = %00011110
ACIA_CMD_TX_ONLY = %00001011

init_acia_0:
    pha
    stz     ACIA0_STATUS
    lda     #ACIA_CTRL_9600_8N1
    sta     ACIA0_CTRL
    lda     #ACIA_CMD_TX_ONLY
    sta     ACIA0_CMD
    pla
    rts

clear_lcd:
init_lcd:
    lda     #$51
    jsr     putcmd_lcd
    rts

ACIA1_CTRL_19200_8N1 = %00011111
ACIA1_CMD_TX_RX_INTR = %10001001

init_acia_1:
    pha
    stz     ACIA1_STATUS
    lda     #ACIA1_CTRL_19200_8N1
    sta     ACIA1_CTRL
    lda     #ACIA1_CMD_TX_RX_INTR
    sta     ACIA1_CMD
    pla
    rts

;
; Read a char into accumulator, sets carry if char was read
; 
getchar:
    sei
    jsr     buffer_size
    beq     @no_char            ; buffer is empty?
    cmp     #$AA
    bcs     @skip_rts           ; size >= $AA, do not change RTSB setting
    lda     ACIA1_CMD           ; read current cmd reg
    ora     #RTS_BIT_MASK       ; set RTSB low
    sta     ACIA1_CMD           ; write cmd reg
@skip_rts:
    jsr     read_buffer
    jsr     putchar
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
    sta     ACIA1_DATA
    phx
    ldx     #52             ; @ 19200 baud, each it takes ~52uS, there are 10 bits
    jsr     delay_10        ; delay 520uS
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
    lda     (bios_params), y
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

;
; Delay X hundred microseconds
; 
delay_10:
    cpx     #0
    beq     @delay_10_complete
    phy
@delay_10_loop_0:
    ldy     #3
@delay_10_loop_1:
    dey
    bne     @delay_10_loop_1
    dex
    bne     @delay_10_loop_0
    ply
@delay_10_complete:
    rts

;
; Delay X hundred microseconds
; 
delay_100:
    cpx     #0
    beq     @delay_100_complete
    phy
@delay_100_loop_0:
    ldy     #24
@delay_100_loop_1:
    dey
    bne     @delay_100_loop_1
    dex
    bne     @delay_100_loop_0
    ply
@delay_100_complete:
    rts

;
; Print a 16-bit number
; Input:
; (Y, X) - pointer to u16
;
print_u16:
    stx     scratch_ptr
    sty     scratch_ptr + 1

    ldy     #0
    lda     (scratch_ptr), y
    sta     u16_work
    iny
    lda     (scratch_ptr), y
    sta     u16_work + 1

    stz     digit_cnt

@print_u16_loop:
    ldx     #<u16_work
    ldy     #>u16_work
    lda     #10
    jsr     divmod_16
    phx
    clc
    adc     #'0'
    ldx     digit_cnt
    sta     digit_buffer, x
    inc     digit_cnt
    plx
    stx     u16_work
    sty     u16_work + 1
    txa
    ora     u16_work + 1
    bne     @print_u16_loop

@print_u16_print:
    dec     digit_cnt
    ldx     digit_cnt
    lda     digit_buffer, x
    jsr     putchar
    lda     digit_cnt
    bne     @print_u16_print

    rts

;
; Print an 8-bit number, leading 0
; Input:
; A - The number to print
;
print_u8_02:
    sta     u16_work
    stz     u16_work + 1
    ldx     #<u16_work
    ldy     #>u16_work
    lda     #10
    jsr     divmod_16
    txa                     ; A has remainder, but so does rem
    clc
    adc     #'0'
    jsr     putchar
    lda     rem
    clc
    adc     #'0'
    jsr     putchar
    rts

;
; Divide a 16-bit number by base and calculate modulus.
; Input:
; (Y, X)    - pointer to 16-bit number
; A         - Base
; Output:
; <Y, X> - Quotient
; A - Remainder
;
divmod_16:
    beq     @divmod_16_div_zero
    stx     scratch_ptr
    sty     scratch_ptr + 1
    sta     base

    ldy     #0
    lda     (scratch_ptr), y
    sta     u16_work
    iny
    lda     (scratch_ptr), y
    sta     u16_work + 1
    
    stz     rem
    
    ldx     #16
@divmod_16_loop:
    asl     u16_work
    rol     u16_work + 1
    rol     rem

    lda     rem
    cmp     base
    bcc     :+
    sbc     base
    sta     rem

    ;
    ; u16_work is doing double duty, it's also a counter for the number of
    ; times base is in dividend.
    ;
    inc     u16_work
:
    dex
    bne     @divmod_16_loop

    lda     rem
    ldx     u16_work
    ldy     u16_work + 1

    rts

@divmod_16_div_zero:
    lda     #$FF
    ldx     #$FF
    ldy     #$FF
    
    rts

uptime_load:
    pha
@uptime_load_retry:

    lda     uptime_seconds
    sta     uptime_seconds_latched

    lda     uptime_minutes
    sta     uptime_minutes_latched

    lda     uptime_hours
    sta     uptime_hours_latched
    lda     uptime_hours + 1
    sta     uptime_hours_latched + 1

    lda     uptime_seconds
    cmp     uptime_seconds_latched
    bne     @uptime_load_retry

    pla
    rts

print_newline:
    pha
    lda     #$0D
    jsr     putchar
    lda     #$0A
    jsr     putchar
    pla
    rts

uptime_print:
    phx
    phy
    pha
    jsr     print_newline
    jsr     uptime_load
    ldx     #<uptime_hours_latched
    ldy     #>uptime_hours_latched
    jsr     print_u16
    lda     #':'
    jsr     putchar
    lda     uptime_minutes_latched
    jsr     print_u8_02
    lda     #':'
    jsr     putchar
    lda     uptime_seconds_latched
    jsr     print_u8_02
    jsr     print_newline
    pla
    ply
    plx
    rts

.ifdef TEST_DIVMOD_16
.import PRBYTE
divmod_16_test:
    lda     #<1234
    sta     bios_params
    lda     #>1234
    sta     bios_params + 1

    ldx     #<bios_params
    ldy     #>bios_params
    lda     #16

    jsr     divmod_16

    jsr     PRBYTE
    tya
    jsr     PRBYTE
    txa
    jsr     PRBYTE

    jmp     $FF00
.endif

.ifdef TEST_PRINT_U16
print_u16_test:
    lda     #<1234
    sta     bios_params
    lda     #>1234
    sta     bios_params + 1

    ldx     #<bios_params
    ldy     #>bios_params

    jsr     print_u16

    jmp     $FF00
.endif

.ifdef TEST_UPTIME
uptime_print_test:
    jsr     uptime_print
    jmp     $FF00
.endif

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

