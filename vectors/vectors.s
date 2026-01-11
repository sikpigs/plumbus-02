.setcpu "65C02"

.import rst_handler
.import nmi_handler
.import irq_handler

.segment "PROC_VECTORS"
    .word nmi_handler
    .word rst_handler
    .word irq_handler
