
.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "defs.inc"
.include "structs.inc"
.include "macros.inc"

.importzp bios_zp

.export strcmp
.export strncmp

mem_args = bios_zp + .sizeof(BIOSPrivate)

.segment "BIOS"

strcmp_common:
@strcmp_loop:
    lda     mem_args + MemArgs::count
    ora     mem_args + MemArgs::count + 1
    beq     @strcmp_equal

    lda     (mem_args + MemArgs::src)
    cmp     (mem_args + MemArgs::dst)
    bne     @strcmp_diff

    cmp     #0
    beq     @strcmp_equal

    PTRINC  mem_args + MemArgs::src
    PTRINC  mem_args + MemArgs::dst

    U16DEC  mem_args + MemArgs::count
    jmp     @strcmp_loop

@strcmp_diff:
    bcc     :+
    lda     #1
    rts
:
    lda     #$FF
    rts
@strcmp_equal:
    lda     #0
    rts

;
; Inputs:
; MemArgs.src   - Block A
; MemArgs.dst   - Block B
; MemArgs.count - Byte Count
;
; Outputs:
; A             - 0 -> Equal, $FF A < B, $01 A > B
;
strncmp:
    jmp     strcmp_common

;
; Inputs:
; MemArgs.src   - Block A
; MemArgs.dst   - Block B
; Outputs:
; A             - 0 -> Equal, $FF A < B, $01 A > B
;
strcmp:
    lda     #$FF
    sta     mem_args + MemArgs::count
    sta     mem_args + MemArgs::count + 1
    jmp     strcmp_common