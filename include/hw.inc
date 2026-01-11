; -------------------------
; ACIA #0 ($2000–$200F)
; -------------------------
ACIA0_BASE   = $2000
ACIA0_DATA   = ACIA0_BASE + 0
ACIA0_STATUS = ACIA0_BASE + 1
ACIA0_CMD    = ACIA0_BASE + 2
ACIA0_CTRL   = ACIA0_BASE + 3

; -------------------------
; ACIA #1 ($2010–$201F)
; -------------------------
ACIA1_BASE   = $2010
ACIA1_DATA   = ACIA1_BASE + 0
ACIA1_STATUS = ACIA1_BASE + 1
ACIA1_CMD    = ACIA1_BASE + 2
ACIA1_CTRL   = ACIA1_BASE + 3

; -------------------------
; VIA ($2020–$202F)
; -------------------------
VIA_BASE     = $2020
VIA_PORTB    = VIA_BASE + 0
VIA_PORTA    = VIA_BASE + 1
VIA_DDRB     = VIA_BASE + 2
VIA_DDRA     = VIA_BASE + 3
VIA_T1CL     = VIA_BASE + 4
VIA_T1CH     = VIA_BASE + 5
VIA_T1LL     = VIA_BASE + 6
VIA_T1LH     = VIA_BASE + 7
VIA_T2CL     = VIA_BASE + 8
VIA_T2CH     = VIA_BASE + 9
VIA_SR       = VIA_BASE + 10
VIA_ACR      = VIA_BASE + 11
VIA_PCR      = VIA_BASE + 12
VIA_IFR      = VIA_BASE + 13
VIA_IER      = VIA_BASE + 14