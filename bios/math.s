.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "structs.inc"
.include "macros.inc"
.include "defs.inc"

.importzp bios_zp

.export divmod_16

bios_private = bios_zp

.segment "BIOSRAM"

; For divmod_16
rem:    .res 1
base:   .res 1
u16:    .res 2    

.segment "BIOS"

;
; Divide a 16-bit number by base and calculate modulus.
; Input:
; (Y, X)    - pointer to 16-bit number
; A         - Base
; Output:
; <Y, X>    - Quotient
; A         - Remainder
;
divmod_16:
    beq     @divmod_16_div_zero
    stx     bios_private + BIOSPrivate::ptr + Ptr::lo
    sty     bios_private + BIOSPrivate::ptr + Ptr::hi
    sta     base

    ldy     #0
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     u16
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     u16 + 1
    
    stz     rem
    
    ldx     #16
@divmod_16_loop:
    asl     u16
    rol     u16 + 1
    rol     rem

    lda     rem
    cmp     base
    bcc     :+
    sbc     base
    sta     rem

    ;
    ; u16 is doing double duty, it's also a counter for the number of
    ; times base is in dividend.
    ;
    inc     u16
:
    dex
    bne     @divmod_16_loop

    lda     rem
    ldx     u16
    ldy     u16 + 1

    rts

@divmod_16_div_zero:
    lda     #$FF
    ldx     #$FF
    ldy     #$FF
    
    rts

.ifdef TEST_DIVMOD_16
.import PRBYTE
divmod_16_test:
    lda     #<1234
    sta     bios_public
    lda     #>1234
    sta     bios_public + 1

    ldx     #<bios_public
    ldy     #>bios_public
    lda     #16

    jsr     divmod_16

    jsr     PRBYTE
    tya
    jsr     PRBYTE
    txa
    jsr     PRBYTE

    jmp     $FF00
.endif
