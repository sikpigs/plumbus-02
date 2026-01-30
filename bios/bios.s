.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "structs.inc"
.include "macros.inc"
.include "defs.inc"

.import main
.import shift_irq
.import shift_init
.import lcd_init
.import uptime_init
.import uptime_irq
.import divmod_16

.export rst_handler
.export nmi_handler
.export irq_handler
.export putchar
.export getchar
.export bios_public
.export bios_zp
.export call
.export delay_10
.export delay_100

.zeropage
bios_zp:        .tag BIOSZP

bios_private    = bios_zp
read_ix         = bios_private + BIOSPrivate::read_ix
write_ix        = bios_private + BIOSPrivate::write_ix
via_ier_mirror  = bios_private + BIOSPrivate::via_ier_mirror

bios_public     = bios_private + .sizeof(BIOSPrivate)

.segment "BIOSRAM"
input_buffer:       .res $100

.segment "BIOS"

rst_handler:
    cli
    cld
    ldx     #$FF
    txs

.ifdef ZERO_ZP
    ldx     #.sizeof(BIOSZP)
    lda     #0
:
    sta     bios_zp, x
    dex
    bne     :-
.endif

    jsr     uptime_init
    jsr     init_buffer
    jsr     init_acia_0
    jsr     init_acia_1
    jsr     init_via
    jsr     lcd_init
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

;
; Add char in accumulator to buffer
;
write_buffer:
    phx
    ldx     write_ix
    sta     input_buffer, x
    inc     write_ix
    plx
    rts

;
; Read a char from the buffer into accumulator
;
read_buffer:
    phx
    ldx     read_ix
    lda     input_buffer, x
    inc     read_ix
    plx
    rts
;
;
; Returns the buffer size in accumulator
;
buffer_size:
    lda     write_ix
    sec
    sbc     read_ix
    rts

init_buffer:
    stz     read_ix
    stz     write_ix
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
; Call a sub-routine
; (Y, X) - address of sub-routine
;
call:
    stx     bios_private + BIOSPrivate::ptr + Ptr::lo
    sty     bios_private + BIOSPrivate::ptr + Ptr::hi

    lda     #>(@call_return - 1)
    pha
    lda     #<(@call_return - 1)
    pha

    jmp     (bios_private + BIOSPrivate::ptr)

@call_return:
    rts

.ifdef TEST_CALL
call_test_cb:
    lda     #'A'
    jsr     putchar
    jsr     print_newline
    rts

call_test:
    ldx     #<call_test_cb
    ldy     #>call_test_cb
    jsr     call
    lda     #'B'
    jsr     putchar
    jsr     print_newline
    jmp     $FF00
.endif
