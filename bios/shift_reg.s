.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "defs.inc"
.include "structs.inc"

.export shift_irq
.export shift_set_clk
.export shift_out
.export shift_set_callback
.export shift_init

.importzp bios_zp

bios_private    = bios_zp

.segment "BIOSRAM"
sr_data_ptr:        .res 2
sr_callback:        .res 2
sr_bytes_left:      .res 1
sr_status:          .res 1

.segment "BIOS"

shift_init:
    stz     sr_status
    stz     sr_bytes_left
    stz     sr_data_ptr
    stz     sr_data_ptr + 1
    stz     sr_callback
    stz     sr_callback + 1

    rts

shift_irq:
    lda     sr_bytes_left
    beq     @sr_done

    lda     sr_data_ptr
    sta     bios_private + BIOSPrivate::shift_data_ptr
    lda     sr_data_ptr + 1
    sta     bios_private + BIOSPrivate::shift_data_ptr + 1
  
    lda     (bios_private + BIOSPrivate::shift_data_ptr)
    sta     VIA_SR

    inc     sr_data_ptr
    bne     :+
    inc     sr_data_ptr + 1
:

    dec     sr_bytes_left
    jmp     @sr_clr_irq

@sr_done:  
    ; Disable interrupt
    lda     #%00000100
    sta     VIA_IER

    lda     #0
    sta     sr_status

    jsr     exec_shift_callback

@sr_clr_irq:
    lda     #%00000100
    sta     VIA_IFR
    rts

;
; Set Shift Register Clock Rate
; Fout = PHI2 / (2 x (T2 + 1))
;
; A - T2 reload low
; X - T2 reload high
;
shift_set_clk:
    sta     VIA_T2CL
    stx     VIA_T2CH
    rts

exec_shift_callback:
    ldx     sr_callback
    ldy     sr_callback + 1
    cpx     #$00
    bne     :+
    cpy     #$00
    beq     @sr_callback_return
:
    stx     bios_private + BIOSPrivate::shift_func_ptr
    sty     bios_private + BIOSPrivate::shift_func_ptr + 1

    lda     #<(@sr_callback_return - 1)
    pha
    lda     #>(@sr_callback_return - 1)
    pha

    jmp     (bios_private + BIOSPrivate::shift_func_ptr)

@sr_callback_return:
    rts

;
; Set Shift Register Data
;
; Inputs:
;   A       - byte count
;   (Y, X)  - data pointer
; 
shift_out:
    ; Bail early on 0 bytes
    cmp     #0
    beq     @sr_busy

    ; Check if we're already active, return if so
    bit     sr_status
    bmi     @sr_busy

    sta     sr_bytes_left
    
    stx     bios_private + BIOSPrivate::shift_data_ptr
    stx     sr_data_ptr
    sty     bios_private + BIOSPrivate::shift_data_ptr + 1
    sty     sr_data_ptr + 1

    lda     #80
    sta     sr_status
    ; Send 1st byte
    lda     (bios_private + BIOSPrivate::shift_data_ptr)
    sta     VIA_SR

    inc     bios_private + BIOSPrivate::shift_data_ptr
    bne     :+
    inc     bios_private + BIOSPrivate::shift_data_ptr + 1
:

    dec     sr_bytes_left
    beq     @sr_done

    lda     #%10000100    
    sta     VIA_IER
    rts

@sr_done:
    lda     #0
    sta     sr_status
    jsr     exec_shift_callback

@sr_busy:
    rts

;
; Clears Shift Register Mode Control Bits (ACR[4..2])
;
shift_clear_mode:
    pha
    lda     VIA_ACR
    and     #SR_CTRL_MASKN
    sta     VIA_ACR
    pla
    rts

;
; Sets Shift Register Mode Control Bits (ACR[4..2])
; A - Shift Register Mode
;
shift_set_mode:
    and     #SR_CTRL_MASK
    sta     VIA_ACR
    rts

;
; Set address of routine to invoke up SR completion.
; <Y, X> - Callback when SR has completed.
; <0, 0>: Disable callback
;
shift_set_callback:
    stx     sr_callback
    sty     sr_callback + 1
    rts          

.ifdef TEST_SHIFT

shift_test_done:
    jsr     shift_clear_mode

    ldx     #$00
    ldy     #$00
    jsr     shift_set_callback

    rts

shift_test:
    ldx     #<shift_test_done
    ldy     #>shift_test_done
    jsr     shift_set_callback

    lda     #$FE
    ldx     #$00
    jsr     shift_set_clk   ; 1.953kHz

    lda     #SR_CTRL_OUT_ONCE_T2
    jsr     shift_set_mode

    ldx     #<shift_reg_test_data
    ldy     #>shift_reg_test_data
    lda     #$FF
    jsr     shift_out

    jmp     $FF00

.segment "RODATA"
shift_reg_test_data:
    .repeat 256, I
        .byte I
    .endrepeat

.endif