.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "structs.inc"
.include "macros.inc"
.include "defs.inc"

.importzp bios_zp
.import putchar
.import divmod_16

.export print_u16
.export print_newline
.export print_u8_02

bios_private = bios_zp

.segment "BIOSRAM"
u16_work:       .res 2
digit_buffer:   .res 16
digit_cnt:      .res 1

.segment "BIOS"

;
; Print a 16-bit number
; Input:
; (Y, X) - pointer to u16
;
print_u16:
    stx     bios_private + BIOSPrivate::ptr + Ptr::lo
    sty     bios_private + BIOSPrivate::ptr + Ptr::hi

    ldy     #0
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     u16_work
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
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
    tay
    txa
    clc
    adc     #'0'
    jsr     putchar
    tya
    clc
    adc     #'0'
    jsr     putchar
    rts

print_newline:
    pha
    lda     #$0D
    jsr     putchar
    lda     #$0A
    jsr     putchar
    pla
    rts

.ifdef TEST_PRINT_U16
print_u16_test:
    lda     #<1234
    sta     bios_public
    lda     #>1234
    sta     bios_public + 1

    ldx     #<bios_public
    ldy     #>bios_public

    jsr     print_u16

    jmp     $FF00
.endif
