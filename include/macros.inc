.macro PTRINC base
    inc     base + Ptr::lo
    bne     :+
    inc     base + Ptr::hi
:
.endmacro

.macro PTRDEC base
    lda     base + Ptr::lo
    sec
    sbc     #1
    sta     base + Ptr::lo
    lda     base + Ptr::hi
    sbc     #0
    sta     base + Ptr::hi
.endmacro

.macro U16DEC value
    lda     value + UInt16::lo
    sec
    sbc     #1
    sta     value + UInt16::lo
    lda     value + UInt16::hi
    sbc     #0
    sta     value + UInt16::hi
.endmacro

RNG_STATE .set $A5

; 8-bit LCG: X_{n+1} = (X * 17 + 29) mod 256
.macro RAND8
    RNG_STATE .set (RNG_STATE * 17 + 29) & $FF
    .byte RNG_STATE
.endmacro

