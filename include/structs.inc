.struct Ptr
    lo      .byte
    hi      .byte
.endstruct

.struct Time
    seconds     .byte
    minutes     .byte
    hours       .word
.endstruct

.struct LCDCmdArgs
    data_ptr    .tag Ptr
.endstruct

.struct MemArgs
    dst     .tag Ptr
    src     .tag Ptr
    count   .word
.endstruct

.struct BIOSPrivate
    ; read buffer indices
    read_ix         .byte
    write_ix        .byte
    ; for general purpose use
    ptr             .tag Ptr
    ; for IRQ use
    shift_func_ptr  .tag Ptr
    shift_data_ptr  .tag Ptr
    via_ier_mirror  .byte
    ; for system uptime
    ticks           .byte
    uptime          .tag Time
.endstruct

.struct BIOSZP
    zp  .byte .max(.max(.sizeof(LCDCmdArgs), .sizeof(MemArgs)), .sizeof(Time)) + .sizeof(BIOSPrivate)
.endstruct