.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "structs.inc"
.include "macros.inc"
.include "defs.inc"

.importzp bios_zp
.import putchar
.import print_u16
.import print_u8_02
.import print_newline

.export uptime_init
.export uptime_print
.export uptime_load
.export uptime_irq

bios_private    = bios_zp
ticks           = bios_private + BIOSPrivate::ticks
uptime          = bios_private + BIOSPrivate::uptime

.segment "BIOSRAM"
uptime_latched:     .tag Time

.segment "BIOS"

uptime_irq:
    inc     ticks
    lda     ticks
    cmp     #100
    bcc     @uptime_irq_done

    stz     ticks
    inc     uptime + Time::seconds
    lda     uptime + Time::seconds
    cmp     #60
    bcc     @uptime_irq_done

    stz     uptime + Time::seconds
    inc     uptime + Time::minutes
    lda     uptime + Time::minutes
    cmp     #60
    bcc     @uptime_irq_done

    stz     uptime + Time::minutes
    inc     uptime + Time::hours
    bne     @uptime_irq_done
    inc     uptime + Time::hours + 1

@uptime_irq_done:
    ; Clear IRQ by reading T1CL
    lda     VIA_T1CL
    rts

uptime_init:
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

;
; (Y, X) - Address of Time struct to populate
;
uptime_load:
    pha
@uptime_load_retry:
    stx     bios_private + BIOSPrivate::ptr + Ptr::lo
    sty     bios_private + BIOSPrivate::ptr + Ptr::hi

    lda     uptime + Time::seconds
    ldy     #Time::seconds
    sta     (bios_private + BIOSPrivate::ptr), y

    lda     uptime + Time::minutes
    ldy     #Time::minutes
    sta     (bios_private + BIOSPrivate::ptr), y

    lda     uptime + Time::hours
    ldy     #Time::hours
    sta     (bios_private + BIOSPrivate::ptr), y
    lda     uptime + Time::hours + 1
    ldy     #Time::hours + 1
    sta     (bios_private + BIOSPrivate::ptr), y

    ldy     #Time::seconds
    lda     (bios_private + BIOSPrivate::ptr), y
    cmp     uptime + Time::seconds
    bne     @uptime_load_retry

    pla
    rts

uptime_print:
    phx
    phy
    pha
    jsr     print_newline

    ldx     #<uptime_latched
    ldy     #>uptime_latched

    jsr     uptime_load

    ldx     #<(uptime_latched + Time::hours)
    ldy     #>(uptime_latched + Time::hours)
    jsr     print_u16

    lda     #':'
    jsr     putchar
    lda     uptime_latched + Time::minutes
    jsr     print_u8_02

    lda     #':'
    jsr     putchar
    lda     uptime_latched + Time::seconds
    jsr     print_u8_02
    jsr     print_newline

    pla
    ply
    plx
    rts

.ifdef TEST_UPTIME
uptime_print_test:
    jsr     uptime_print
    jmp     $FF00
.endif
