.setcpu "65C02"
.debuginfo
.include "hw.inc"
.include "defs.inc"
.include "structs.inc"
.include "macros.inc"

.importzp bios_zp

.export memcpy
.export memset
.export memmove
.export memcmp

bios_private    = bios_zp
mem_args        = bios_zp + .sizeof(BIOSPrivate)

.segment "BIOSRAM"
mem_scratch_ptr:    .res 2

.segment "BIOS"

;
; Inputs:
; MemArgs.src   - Block A
; MemArgs.dst   - Block B
; MemArgs.count - Byte Count
;
; Outputs:
; A             - 0 -> Equal, $FF A < B, $01 A > B
;
memcmp:
    phy
    lda     mem_args + MemArgs::count + 1
    bne     @memcmp_loop_init
    lda     mem_args + MemArgs::count
    beq     @memcmp_equal
    bra     @memcmp_tail
@memcmp_loop_init:
    ldy     #0
@memcmp_loop:
    lda     (mem_args + MemArgs::src), y
    cmp     (mem_args + MemArgs::dst), y
    bne     @memcmp_diff
    iny
    bne     @memcmp_loop
    inc     mem_args + MemArgs::src + Ptr::hi
    inc     mem_args + MemArgs::dst + Ptr::hi
    dec     mem_args + MemArgs::count + 1
    bne     @memcmp_loop
    lda     mem_args + MemArgs::count
    beq     @memcmp_equal
@memcmp_tail:
    ldy     #0
@memcmp_tail_loop:
    lda     (mem_args + MemArgs::src), y
    cmp     (mem_args + MemArgs::dst), y
    bne     @memcmp_diff
    iny
    dec     mem_args + MemArgs::count
    bne     @memcmp_tail_loop
@memcmp_equal:
    lda     #0
    ply
    rts

@memcmp_diff:
    bcc     :+
    lda     #1
    ply
    rts
:
    lda     #$FF
    ply
    rts

; TEST_MEMCMP := 1

.ifdef TEST_MEMCMP

.segment "RODATA"

buf_eq_1:
    .byte "HELLO", $FF
buf_eq_2:
    .byte "HELLO", 0

buf_less:
    .byte "ABC", $FF
buf_greater:
    .byte "ABD", 0

buf_one_1:
    .byte $FF
buf_one_2:
    .byte $01
buf_one_guard:
    .byte $FF

buf_zero_1:
    .byte 0
buf_zero_2:
    .byte 0

buf_diff_early_1:
    .byte $01, $FF, $FF, $02
buf_diff_early_2:
    .byte $02, $FF, $FF, $01

buf_large_greater:
    .repeat 256
        .byte $FF
    .endrepeat
    .byte $FF
    .byte $AA
    .byte $00

buf_large_less:
    .repeat 256
        .byte $FF
    .endrepeat
    .byte $FF
    .byte $55
    .byte $FF

.struct MemCmpTest
    src     .addr
    dst     .addr
    count   .word
    expect  .byte
.endstruct

.macro CMPTST src, dst, count, expect
    .addr   src
    .addr   dst
    .word   count
    .byte   expect
.endmacro

memcmp_tests:
    CMPTST  buf_eq_1, buf_eq_2, 5, 0                    ; .
    CMPTST  buf_greater, buf_less, 3, 1                 ; E
    CMPTST  buf_zero_1, buf_zero_2, 0, 0                ; .
    CMPTST  buf_one_1, buf_one_2, 1, 1                  ; E
    CMPTST  buf_diff_early_1, buf_diff_early_2, 3, $FF  ; E
    CMPTST  buf_large_greater, buf_large_less, 258, 1   ; E
    CMPTST  buf_large_greater, buf_large_less, 257, 0   ; .
memcmp_tests_end:

memcmp_test_count = (memcmp_tests_end - memcmp_tests) / .sizeof(MemCmpTest)

.segment "BIOSRAM"

memcmp_expect:      .res 1

.segment "BIOS"

.import putchar
.import print_newline
.import PRBYTE

memcmp_run_tests:
    ldx     #0
    lda     #<memcmp_tests
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     #>memcmp_tests
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi

@next_test:
    jsr     print_newline
    txa
    jsr     PRBYTE

    ; load src
    ldy     #MemCmpTest::src
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::lo
    iny     
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::hi

    ; load dst
    ldy     #MemCmpTest::dst
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::dst + Ptr::lo
    iny     
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load count
    ldy     #MemCmpTest::count
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count
    iny     
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count + 1

    ; load expected result
    ldy     #MemCmpTest::expect
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     memcmp_expect

    lda     #'R'
    jsr     putchar

    jsr     memcmp
    pha
    jsr     PRBYTE
    pla
    cmp     memcmp_expect
    beq     :+
    lda     #':'
    jsr     putchar
    lda     #'E'
    jsr     putchar
    jmp     @prep_next_test
:
    lda     #':'
    jsr     putchar
    lda     #'.'
    jsr     putchar    

@prep_next_test:
    clc
    lda     bios_private + BIOSPrivate::ptr + Ptr::lo
    adc     #.sizeof(MemCmpTest)
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     bios_private + BIOSPrivate::ptr + Ptr::hi
    adc     #0
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi
    
    inx
    cpx     #memcmp_test_count
    bne     @next_test

    jmp     $FF00

.endif ; TEST_MEMCMP

;
; Inputs:
; MemArgs.dst   - Destination
; MemArgs.src   - Source
; MemArgs.count - Byte Count
;
memcpy:
    phy
    lda     mem_args + MemArgs::count + 1
    bne     @memcpy_loop_init
    lda     mem_args + MemArgs::count
    beq     @memcpy_done
    bra     @memcpy_tail
@memcpy_loop_init:
    ldy     #0
@memcpy_loop:
    lda     (mem_args + MemArgs::src), y
    sta     (mem_args + MemArgs::dst), y
    iny
    bne     @memcpy_loop
    inc     mem_args + MemArgs::src + Ptr::hi
    inc     mem_args + MemArgs::dst + Ptr::hi
    dec     mem_args + MemArgs::count + 1
    bne     @memcpy_loop
    lda     mem_args + MemArgs::count
    beq     @memcpy_done   
@memcpy_tail:
    ldy     #0
@memcpy_tail_loop:
    lda     (mem_args + MemArgs::src), y
    sta     (mem_args + MemArgs::dst), y
    iny
    dec     mem_args + MemArgs::count
    bne     @memcpy_tail_loop
@memcpy_done:
    ply
    rts

; TEST_MEMCPY := 1

.ifdef TEST_MEMCPY

.struct MemCpyTest
    src         .addr
    copy_count  .word
    expect      .addr
.endstruct

.macro CPYTST src, count, expect
    .addr   src
    .word   count
    .addr   expect
.endmacro

.segment "RODATA"
CPY_GUARD_SIZE  = 16
CPY_ARENA_SIZE  = 512
CPY_TOTAL_SIZE  = CPY_GUARD_SIZE + CPY_ARENA_SIZE + CPY_GUARD_SIZE
CPY_GUARD_BYTE  = $A5
CPY_ZERO_TEST   = 0
CPY_ONE_TEST    = 1
CPY_FOUR_TEST   = 4
CPY_255_TEST    = 255
CPY_256_TEST    = 256
CPY_FULL_TEST   = CPY_ARENA_SIZE

.macro CPY_TEST_SRC count
    .repeat count
        RAND8
    .endrepeat
.endmacro

.macro CPY_TEST_EXP count
    .repeat CPY_GUARD_SIZE
        .byte CPY_GUARD_BYTE
    .endrepeat
    .repeat count
        RAND8
    .endrepeat
    .repeat CPY_ARENA_SIZE - count
        .byte CPY_GUARD_BYTE
    .endrepeat
    .repeat CPY_GUARD_SIZE
        .byte CPY_GUARD_BYTE
    .endrepeat
.endmacro

RNG_STATE .set $A5
memcpy_zero_src: CPY_TEST_SRC CPY_ZERO_TEST
RNG_STATE .set $A5
memcpy_zero_exp: CPY_TEST_EXP CPY_ZERO_TEST

RNG_STATE .set $5A
memcpy_one_src: CPY_TEST_SRC CPY_ONE_TEST
RNG_STATE .set $5A
memcpy_one_exp: CPY_TEST_EXP CPY_ONE_TEST

RNG_STATE .set $AA
memcpy_four_src: CPY_TEST_SRC CPY_FOUR_TEST
RNG_STATE .set $AA
memcpy_four_exp: CPY_TEST_EXP CPY_FOUR_TEST

RNG_STATE .set $55
memcpy_255_src: CPY_TEST_SRC CPY_255_TEST
RNG_STATE .set $55
memcpy_255_exp: CPY_TEST_EXP CPY_255_TEST

RNG_STATE .set $DE
memcpy_256_src: CPY_TEST_SRC CPY_256_TEST
RNG_STATE .set $DE
memcpy_256_exp: CPY_TEST_EXP CPY_256_TEST

RNG_STATE .set $AD
memcpy_full_src: CPY_TEST_SRC CPY_FULL_TEST
RNG_STATE .set $AD
memcpy_full_exp: CPY_TEST_EXP CPY_FULL_TEST

memcpy_tests:
    CPYTST  memcpy_zero_src, CPY_ZERO_TEST, memcpy_zero_exp
    CPYTST  memcpy_one_src, CPY_ONE_TEST, memcpy_one_exp
    CPYTST  memcpy_four_src, CPY_FOUR_TEST, memcpy_four_exp
    CPYTST  memcpy_255_src, CPY_255_TEST, memcpy_255_exp
    CPYTST  memcpy_256_src, CPY_256_TEST, memcpy_256_exp
    CPYTST  memcpy_full_src, CPY_FULL_TEST, memcpy_full_exp
memcpy_tests_end:

.segment "BIOSRAM"

memcpy_test_dst:
guard_lo:
    .res CPY_GUARD_SIZE
copy_region:
    .res CPY_ARENA_SIZE
guard_hi:
    .res CPY_GUARD_SIZE

memcpy_test_count = (memcpy_tests_end - memcpy_tests) / .sizeof(MemCpyTest)

.segment "BIOS"

.import putchar
.import PRBYTE
.import print_newline

memcpy_run_tests:
    ldx     #0
    lda     #<memcpy_tests
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     #>memcpy_tests
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi

@next_test:
    jsr     print_newline
    ; Print test number
    txa
    jsr     PRBYTE

    ; load dest
    lda     #<memcpy_test_dst
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>memcpy_test_dst
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load count
    lda     #<CPY_TOTAL_SIZE
    sta     mem_args + MemArgs::count
    lda     #>CPY_TOTAL_SIZE
    sta     mem_args + MemArgs::count + 1

    ; initialize dst
    lda     #CPY_GUARD_BYTE
    jsr     memset

    ; load src
    ldy     #MemCpyTest::src
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::lo
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::hi

    ; load dest
    lda     #<copy_region
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>copy_region
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load number of bytes to copy into count
    ldy     #MemCpyTest::copy_count
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count + 1
    
    lda     #'R'
    jsr     putchar

    ; perform test
    jsr     memcpy

    ; load expect into src
    ldy     #MemCpyTest::expect
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::lo
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::hi

    ; load dest
    lda     #<memcpy_test_dst
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>memcpy_test_dst
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load total bytes into count
    lda     #<CPY_TOTAL_SIZE
    sta     mem_args + MemArgs::count
    lda     #>CPY_TOTAL_SIZE
    sta     mem_args + MemArgs::count + 1

    lda     #':'
    jsr     putchar

    jsr     memcmp
    beq     :+
    lda     #'E'
    jsr     putchar
    jmp     @prep_next_test
:
    lda     #'.'
    jsr     putchar
@prep_next_test:
    clc
    lda     bios_private + BIOSPrivate::ptr + Ptr::lo
    adc     #.sizeof(MemCpyTest)
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     bios_private + BIOSPrivate::ptr + Ptr::hi
    adc     #0
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi

    inx
    cpx     #memcpy_test_count
    beq     :+
    jmp     @next_test
:
    jmp     $FF00

.endif ; TEST_MEMCPY

;
; Inputs:
; A             - Fill Value
; MemArgs.dst   - Destination
; MemArgs.count - Byte Count
;
memset:
    phy
    ldy     mem_args + MemArgs::count + 1
    bne     @memset_loop_init
    ldy     mem_args + MemArgs::count
    beq     @memset_done
    bra     @memset_tail
@memset_loop_init:
    ldy     #0
@memset_loop:
    sta     (mem_args + MemArgs::dst), y
    iny
    bne     @memset_loop
    inc     mem_args + MemArgs::dst + Ptr::hi
    dec     mem_args + MemArgs::count + 1
    bne     @memset_loop
    ldy     mem_args + MemArgs::count
    beq     @memset_done
@memset_tail:
    ldy     #0
@memset_tail_loop:
    sta     (mem_args + MemArgs::dst), y
    iny   
    dec     mem_args + MemArgs::count
    bne     @memset_tail_loop
@memset_done:
    ply
    rts

RUN_MEMSET := 1

.ifdef RUN_MEMSET
run_memset:
    lda     $1A00
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     $1A01
    sta     mem_args + MemArgs::dst + Ptr::hi
    lda     $1A04
    sta     mem_args + MemArgs::count
    lda     $1A05
    sta     mem_args + MemArgs::count + 1
    lda     $1A06
    jsr     memset
    jmp     $FF00
.endif

; TEST_MEMSET := 1

.ifdef TEST_MEMSET

.struct MemSetTest
    count   .word
    expect  .addr
.endstruct

.macro SETTST count, expect
    .word   count
    .addr   expect
.endmacro

.segment "RODATA"
SET_GUARD_SIZE      = 16
SET_ARENA_SIZE      = 448
SET_TOTAL_SIZE      = SET_GUARD_SIZE + SET_ARENA_SIZE + SET_GUARD_SIZE
SET_GUARD_BYTE      = $A5
SET_PATTERN_BYTE    = $5A
SET_ZERO_TEST       = 0
SET_ONE_TEST        = 1
SET_FOUR_TEST       = 4
SET_255_TEST        = 255
SET_256_TEST        = 256
SET_257_TEST        = 257
SET_FULL_TEST       = SET_ARENA_SIZE

.macro SET_TEST_EXP count
    .repeat SET_GUARD_SIZE
        .byte SET_GUARD_BYTE
    .endrepeat
    .repeat count
        .byte SET_PATTERN_BYTE
    .endrepeat
    .repeat SET_ARENA_SIZE - count
        .byte SET_GUARD_BYTE
    .endrepeat
    .repeat SET_GUARD_SIZE
        .byte SET_GUARD_BYTE
    .endrepeat
.endmacro

memset_test_init_data:
memset_zero_exp:
SET_TEST_EXP SET_ZERO_TEST
memset_one_exp:
SET_TEST_EXP SET_ONE_TEST
memset_four_exp:
SET_TEST_EXP SET_FOUR_TEST
memset_255_exp:
SET_TEST_EXP SET_255_TEST
memset_256_exp:
SET_TEST_EXP SET_256_TEST
memset_257_exp:
SET_TEST_EXP SET_257_TEST
memset_full_exp:
SET_TEST_EXP SET_FULL_TEST

memset_tests:
    SETTST  SET_ZERO_TEST,  memset_zero_exp
    SETTST  SET_ONE_TEST,   memset_one_exp
    SETTST  SET_FOUR_TEST,  memset_four_exp
    SETTST  SET_255_TEST,   memset_255_exp
    SETTST  SET_256_TEST,   memset_256_exp
    SETTST  SET_257_TEST,   memset_257_exp
    SETTST  SET_FULL_TEST,  memset_full_exp    
memset_tests_end:
memset_test_count = (memset_tests_end - memset_tests) / .sizeof(MemSetTest)

.segment "BIOSRAM"

memset_test_dst:
memset_guard_lo:
    .res SET_GUARD_SIZE
memset_arena:
    .res SET_ARENA_SIZE
memset_guard_hi:
    .res SET_GUARD_SIZE

.segment "BIOS"

.import putchar
.import print_newline
.import PRBYTE

memset_run_tests:
    ldx     #0
    lda     #<memset_tests
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     #>memset_tests
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi
@next_test:
    jsr     print_newline
    txa
    jsr     PRBYTE

    ; copy test source data to initialize memory
    ; load dst
    lda     #<memset_test_dst
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>memset_test_dst
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load src
    lda     #<memset_test_init_data
    sta     mem_args + MemArgs::src + Ptr::lo
    lda     #>memset_test_init_data
    sta     mem_args + MemArgs::src + Ptr::hi

    ; load count
    lda     #<SET_TOTAL_SIZE
    sta     mem_args + MemArgs::count
    lda     #>SET_TOTAL_SIZE
    sta     mem_args + MemArgs::count + 1

    jsr     memcpy
    
    ; now prepare the test
    ; load dst
    lda     #<memset_arena
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>memset_arena
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load number of bytes to set into count
    ldy     #MemSetTest::count
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count + 1
    
    lda     #'R'
    jsr     putchar

    lda     #SET_PATTERN_BYTE  
    jsr     memset

    ; load expect into src
    ldy     #MemSetTest::expect
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::lo
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::src + Ptr::hi

    ; load dest
    lda     #<memset_test_dst
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>memset_test_dst
    sta     mem_args + MemArgs::dst + Ptr::hi

    ; load total bytes into count
    lda     #<SET_TOTAL_SIZE
    sta     mem_args + MemArgs::count
    lda     #>SET_TOTAL_SIZE
    sta     mem_args + MemArgs::count + 1

    jsr     memcmp
    pha
    jsr     PRBYTE
    lda     #':'
    jsr     putchar
    pla    
    beq     :+
    lda     #'E'
    jsr     putchar
    jmp     @prep_next_test
:
    lda     #'.'
    jsr     putchar
@prep_next_test:
    clc
    lda     bios_private + BIOSPrivate::ptr + Ptr::lo
    adc     #.sizeof(MemSetTest)
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     bios_private + BIOSPrivate::ptr + Ptr::hi
    adc     #0
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi

    inx
    cpx     #memset_test_count
    beq     :+
    jmp     @next_test
:
    jmp     $FF00

.endif ; TEST_MEMSET

;
; Inputs:
; MemArgs.dst   - Destination
; MemArgs.src   - Source
; MemArgs.count - Byte Count
;
memmove:
    ; bail early if count == 0
    lda     mem_args + MemArgs::count
    ora     mem_args + MemArgs::count + 1
    beq     @memmove_done
    ; mem_scratch_ptr = src + count
    clc
    lda     mem_args + MemArgs::src + Ptr::lo
    adc     mem_args + MemArgs::count
    sta     mem_scratch_ptr + Ptr::lo
    lda     mem_args + MemArgs::src + Ptr::hi
    adc     mem_args + MemArgs::count + 1
    sta     mem_scratch_ptr + Ptr::hi
    ; check copy order:
    ; if dst < src then forwards
    ; elif dst == src then done
    ; else
    ;   if dest < src+count then backwards
    ;   else forwards
    ; check high bytes first
    lda     mem_args + MemArgs::dst + Ptr::hi
    cmp     mem_args + MemArgs::src + Ptr::hi
    bne     @memmove_check_dst_vs_end           ; dst+1 > src+1
    bcc     @memmove_forward                    ; dst+1 < src+1
    ; else dst+1 == src+1
    lda     mem_args + MemArgs::dst + Ptr::lo
    cmp     mem_args + MemArgs::src + Ptr::lo
    bcc     @memmove_forward                    ; dst < src
    beq     @memmove_done                       ; dst == src
    ; else dst > src
@memmove_check_dst_vs_end:
    ; if we're here that means dst+1 > src+1 or (dst+1 == src+1 and dst > src)
    ; does destination overlap the end of source?
    lda     mem_args + MemArgs::dst + Ptr::hi
    cmp     mem_scratch_ptr + Ptr::hi
    bcc     @memmove_backward                   ; dst < src+count
    bne     @memmove_forward                    ; dst > src+count
    ; else dst == src
    lda     mem_args + MemArgs::dst + Ptr::lo
    cmp     mem_scratch_ptr + Ptr::lo
    bcc     @memmove_backward                   ; dst < src+count
@memmove_forward:
    jmp     memcpy
@memmove_backward:
    ; backward memcpy
    ; dst += count
    ; src += count
    ;   *dst = *src;
    ;   --count
    ; until count == 0
    ; dst += count
    clc
    lda     mem_args + MemArgs::dst + Ptr::lo
    adc     mem_args + MemArgs::count
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     mem_args + MemArgs::dst + Ptr::hi
    adc     mem_args + MemArgs::count + 1
    sta     mem_args + MemArgs::dst + Ptr::hi
    ; src += count
    clc
    lda     mem_args + MemArgs::src + Ptr::lo
    adc     mem_args + MemArgs::count
    sta     mem_args + MemArgs::src + Ptr::lo
    lda     mem_args + MemArgs::src + Ptr::hi
    adc     mem_args + MemArgs::count + 1
    sta     mem_args + MemArgs::src + Ptr::hi
@memmove_loop:
    PTRDEC  mem_args + MemArgs::src
    PTRDEC  mem_args + MemArgs::dst
    U16DEC  mem_args + MemArgs::count

    lda     (mem_args + MemArgs::src)
    sta     (mem_args + MemArgs::dst)

    lda     mem_args + MemArgs::count
    ora     mem_args + MemArgs::count + 1
    bne     @memmove_loop
@memmove_done:
    rts

RUN_MEMMOVE := 1

.ifdef RUN_MEMMOVE
run_memmove:
    lda     $1A00
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     $1A01
    sta     mem_args + MemArgs::dst + Ptr::hi
    lda     $1A02
    sta     mem_args + MemArgs::src + Ptr::lo
    lda     $1A03
    sta     mem_args + MemArgs::src + Ptr::hi
    lda     $1A04
    sta     mem_args + MemArgs::count
    lda     $1A05
    sta     mem_args + MemArgs::count + 1
    jsr     memmove
    jmp     $FF00
.endif

; TEST_MEMMOVE := 1

.ifdef TEST_MEMMOVE

.struct MemMovTest
    src         .addr
    copy_count  .word
    expect      .addr
.endstruct

.macro MOVTST src, count, expect
    .addr   src
    .word   count
    .addr   expect
.endmacro

MOV_GUARD_SIZE  = 16
MOV_ARENA_SIZE  = 448
MOV_OVERLAP     = 50

DST_PATTERN         = $00
SRC_PATTERN         = $FF
SRC_GUARD_PATTERN   = $AA
DST_GUARD_PATTERN   = $55

.union MemMov
    .struct Src
        guard_hi    .byte 16    ; MOV_GUARD_SIZE
        src         .byte 448   ; MOV_ARENA_SIZE
        guard_lo    .byte 16    ; MOV_GUARD_SIZE
    .endstruct
    .struct Dst
        overlap     .byte 50    ; MOV_OVERLAP
        dst         .byte 448   ; MOV_ARENA_SIZE
        guard_lo    .byte 16    ; MOV_GUARD_SIZE
    .endstruct
.endunion

.struct MemMovExp
    guard_hi    .byte 16        ; MOV_GUARD_SIZE
    exp         .byte 448       ; MOV_ARENA_SIZE
    guard_lo    .byte 16        ; MOV_GUARD_SIZE
.endstruct

.struct MemMovInit
    dst     .addr
    count   .word
    pattern .byte
.endstruct

.macro MEMSET dst, count, pattern
    .addr   dst
    .word   count
    .byte   pattern
.endmacro

.segment "RODATA"
memmove_init:
    MEMSET  memmove_test_dst + MemMov::Dst::dst, MOV_ARENA_SIZE, DST_PATTERN
    MEMSET  memmove_test_dst + MemMov::Dst::guard_lo, MOV_GUARD_SIZE, DST_GUARD_PATTERN
    MEMSET  memmove_test_src + MemMov::Src::src, MOV_ARENA_SIZE, SRC_PATTERN
    MEMSET  memmove_test_src + MemMov::Src::guard_hi, MOV_GUARD_SIZE, SRC_GUARD_PATTERN
    MEMSET  memmove_test_src + MemMov::Src::guard_lo, MOV_GUARD_SIZE, SRC_GUARD_PATTERN
    MEMSET  memmove_test_exp + MemMovExp::exp, MOV_ARENA_SIZE, SRC_PATTERN
    MEMSET  memmove_test_exp + MemMov::Src::guard_hi, MOV_GUARD_SIZE, DST_GUARD_PATTERN
    MEMSET  memmove_test_exp + MemMov::Src::guard_lo, MOV_GUARD_SIZE, DST_GUARD_PATTERN
memmove_init_end:

MEMMOVE_INIT_COUNT = (memmove_init_end - memmove_init) / .sizeof(MemMovInit)
MOV_AREA_SIZE = .sizeof(MemMov)

.segment "BIOSRAM"

memmove_area:       .res MOV_AREA_SIZE
memmove_test_exp:   .res .sizeof(MemMovExp)
memmove_test_src = memmove_area
memmove_test_dst = memmove_area

.segment "BIOS"

memmove_test:
    ldx     #0
    lda     #<memmove_init
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     #>memmove_init
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi
@next_init:
    ; set dst
    ldy     #MemMovInit::dst
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::dst + Ptr::lo
    iny
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::dst + Ptr::hi
    ; set count
    ldy     #MemMovInit::count
    lda     (bios_private + BIOSPrivate::ptr), y
    sta     mem_args + MemArgs::count
    iny
    lda     (bios_private + BIOSPrivate::ptr), y 
    sta     mem_args + MemArgs::count + 1
    ; load the pattern
    ldy     #MemMovInit::pattern
    lda     (bios_private + BIOSPrivate::ptr), y
    ; go
    jsr     memset
    ; increment the pointer to the next init struct
    clc
    lda     bios_private + BIOSPrivate::ptr + Ptr::lo
    adc     #.sizeof(MemMovInit)
    sta     bios_private + BIOSPrivate::ptr + Ptr::lo
    lda     bios_private + BIOSPrivate::ptr + Ptr::hi
    adc     #0
    sta     bios_private + BIOSPrivate::ptr + Ptr::hi
    ; check for last 
    inx
    cpx     #MEMMOVE_INIT_COUNT
    bne     @next_init
    ; setup for test
    ; set dst
    lda     #<(memmove_test_dst + MemMov::Dst::dst)
    sta     mem_args + MemArgs::dst + Ptr::lo
    lda     #>(memmove_test_dst + MemMov::Dst::dst)
    sta     mem_args + MemArgs::dst + Ptr::hi
    ; set src
    lda     #<(memmove_test_src + MemMov::Src::src)
    sta     mem_args + MemArgs::src + Ptr::lo
    lda     #>(memmove_test_src + MemMov::Src::src)
    sta     mem_args + MemArgs::src + Ptr::hi
SET_COUNT:
    ; set count
    lda     #<MOV_ARENA_SIZE
    sta     mem_args + MemArgs::count
    lda     #>MOV_ARENA_SIZE
    sta     mem_args + MemArgs::count + 1
    ; go
    jsr     memmove
    jmp     $FF00

.endif