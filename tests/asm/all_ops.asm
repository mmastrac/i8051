; Exercise each instruction form from crates/i8051/src/cpu.rs op table.
; Success: MOVX 0x42 at XDATA 0x8000. Failure: P2=0xBD, then spin.

.module all_ops
.area CSEG (CODE)

; Inline failure: P2←BD, spin (no long jump).
.macro fail_halt
        mov     A, #0xBD                 ; fail marker port A0 (P2)
        mov     P2, A
        sjmp    .                        ; halt
.endm

_start:
    ljmp main

halt:
    sjmp halt

main:
    mov  DPTR, #0x8000
    mov  SP, #0x5F

    lcall t_nop_clr_inc_cpl_swap_mul_div ; NOP,CLR,INC/DEC,CPL,SWAP,MUL,DIV
    lcall t_add_addc_subb                ; ADD,ADDC,SUBB (all addr modes)
    lcall t_rotate_shift                 ; RLC,RRC,RL,RR
    lcall t_anl_orl                      ; ANL,ORL (A + dir/#/@R/Rn)
    lcall t_xrl_da                       ; XRL, DA
    lcall t_mov_forms                    ; MOV*, INC DPTR
    lcall t_dptr_movx                    ; MOVX @DPTR,@R0,@R1
    lcall t_movc_xch_stack               ; MOVC,XCH,XCHD,PUSH,POP
    lcall t_carry_jb_movc_bits           ; JC..CPL bit, ANL/ORL C
    lcall t_cjne_djnz_jz                 ; CJNE,DJNZ,JZ,JNZ
    lcall t_flow_ajmp_acall_ljmp_lcall_reti_jmpa ; SJMP,AJMP,ACALL,LCALL,RETI,LJMP,JMP@A+DPTR

    mov  DPTR, #0x8000
    mov  A, #0x42
    movx @DPTR, A                        ; success → XDATA
    sjmp halt

; Each routine: keeprel branches within ±127 bytes; end with `ret`.
; Failure targets use `fail_halt` macro (in range; no ajmp to shared handler).

t_nop_clr_inc_cpl_swap_mul_div:
    nop                                  ; NOP
    mov  A, #0xFF
    clr  A                               ; CLR A
    cjne A, #0x00, L0b
    mov  A, #0x0F
    inc  A                               ; INC/DEC A
    cjne A, #0x10, L0b
    dec  A
    cjne A, #0x0F, L0b
    mov  0x30, #0x41
    inc  0x30                            ; INC/DEC direct
    mov  A, 0x30
    cjne A, #0x42, L0b
    dec  0x30
    mov  A, 0x30
    cjne A, #0x41, L0b
    mov  R0, #0x40
    mov  0x40, #0x10
    inc  @R0                             ; INC/DEC @R0, INC/DEC Rn
    mov  A, 0x40
    cjne A, #0x11, L0b
    dec  @R0
    mov  A, 0x40
    cjne A, #0x10, L0b
    mov  R7, #0x05
    inc  R7
    mov  A, R7
    cjne A, #0x06, L0b
    dec  R7
    mov  A, R7
    cjne A, #0x05, L0b
    mov  A, #0x00
    cpl  A                               ; CPL A
    cjne A, #0xFF, L0b
    mov  A, #0x12
    swap A                               ; SWAP A
    cjne A, #0x21, L0b
    mov  A, #0x04
    mov  B, #0x08
    mul  AB                              ; MUL AB
    cjne A, #0x20, L0b
    mov  A, B
    cjne A, #0x00, L0b
    mov  A, #0x11
    mov  B, #0x04
    div  AB                              ; DIV AB
    cjne A, #0x04, L0b
    mov  A, B
    cjne A, #0x01, L0b
    ret
L0b:
    fail_halt

t_add_addc_subb:
    mov  A, #0x10
    add  A, #0x20                      ; ADD A,# / dir / @R / Rn
    cjne A, #0x30, L1b
    mov  0x31, #0x0F
    add  A, 0x31
    cjne A, #0x3F, L1b
    mov  R0, #0x32
    mov  0x32, #0x01
    add  A, @R0
    cjne A, #0x40, L1b
    mov  R1, #0x15
    add  A, R1
    cjne A, #0x55, L1b
    clr  C
    mov  A, #0x0A
    addc A, #0x0B                      ; ADDC (same addr modes)
    cjne A, #0x15, L1b
    setb C
    mov  A, #0xF0
    mov  0x33, #0x0F
    addc A, 0x33
    ; CJNE overwrites C — verify carry before comparing A
    jnc  L1b
    cjne A, #0x00, L1b
    clr  C
    mov  A, #0x20
    mov  R0, #0x34
    mov  0x34, #0x20
    addc A, @R0
    cjne A, #0x40, L1b
    mov  R2, #0x05
    addc A, R2
    cjne A, #0x45, L1b
    setb C
    mov  A, #0x50
    subb A, #0x10                      ; SUBB (same addr modes)
    cjne A, #0x3F, L1b
    clr  C
    mov  A, #0x40
    mov  0x35, #0x10
    subb A, 0x35
    cjne A, #0x30, L1b
    mov  R0, #0x36
    mov  0x36, #0x08
    subb A, @R0
    cjne A, #0x28, L1b
    mov  R3, #0x05
    subb A, R3
    cjne A, #0x23, L1b
    ret
L1b:
    fail_halt

t_rotate_shift:
    clr  C
    mov  A, #0x80
    rlc  A                               ; RLC A
    ; CJNE overwrites C — check carry from RLC before comparing A
    jnc  L2b
    cjne A, #0x00, L2b
    clr  C
    mov  A, #0x01
    rrc  A                               ; RRC A
    jnc  L2b
    cjne A, #0x00, L2b
    mov  A, #0xE0
    rl   A                               ; RL A
    cjne A, #0xC1, L2b
    mov  A, #0x07
    rr   A                               ; RR A
    cjne A, #0x83, L2b
    ret
L2b:
    fail_halt

t_anl_orl:
    mov  A, #0xF0
    anl  A, #0x33                      ; ANL A,# / dir / @R / Rn
    cjne A, #0x30, L20b
    mov  0x37, #0x0F
    anl  A, 0x37
    cjne A, #0x00, L20b
    mov  R0, #0x38
    mov  0x38, #0xFF
    mov  A, #0x55
    anl  A, @R0
    cjne A, #0x55, L20b
    mov  R4, #0xAA
    anl  A, R4
    cjne A, #0x00, L20b
    mov  0x39, #0xF0
    mov  A, #0x0F
    anl  0x39, A                         ; ANL dir,A ; ANL dir,#
    mov  A, 0x39
    cjne A, #0x00, L20b
    mov  0x3A, #0x0F
    anl  0x3A, #0xF3
    mov  A, 0x3A
    cjne A, #0x03, L20b
    mov  A, #0x30
    orl  A, #0x05                      ; ANL/ORL dir,A and dir,#
    cjne A, #0x35, L20b
    mov  0x3B, #0x40
    orl  A, 0x3B
    cjne A, #0x75, L20b
    mov  R0, #0x3C
    mov  0x3C, #0x08
    orl  A, @R0
    cjne A, #0x7D, L20b
    mov  R5, #0x02
    orl  A, R5
    cjne A, #0x7F, L20b
    mov  0x3D, #0x80
    orl  0x3D, A
    mov  A, 0x3D
    cjne A, #0xFF, L20b
    mov  0x3E, #0x80
    orl  0x3E, #0x0F
    mov  A, 0x3E
    cjne A, #0x8F, L20b
    ret
L20b:
    fail_halt

t_xrl_da:
    mov  A, #0xFF
    xrl  A, #0xF0                      ; XRL A,# / dir / @R / Rn
    cjne A, #0x0F, L21b
    mov  0x45, #0x55
    xrl  A, 0x45
    cjne A, #0x5A, L21b
    mov  R0, #0x46
    mov  0x46, #0x0A
    xrl  A, @R0
    cjne A, #0x50, L21b
    mov  R6, #0x05
    xrl  A, R6
    cjne A, #0x55, L21b
    mov  0x47, #0xFF
    xrl  0x47, A                         ; XRL dir,A ; XRL dir,#
    mov  A, 0x47
    cjne A, #0xAA, L21b
    mov  0x48, #0xF0
    xrl  0x48, #0x3C
    mov  A, 0x48
    cjne A, #0xCC, L21b
    mov  A, #0x19
    mov  B, #0x28
    add  A, B
    da   A                               ; DA A
    cjne A, #0x47, L21b
    ret
L21b:
    fail_halt

t_mov_forms:
    mov  A, #0xC3
    mov  R7, #0x71
    mov  A, R7                           ; MOV Rn,# ; MOV A,Rn
    cjne A, #0x71, L3b
    mov  A, #0xC3
    mov  R0, #0x38
    mov  @R0, A                          ; MOV @R,A ; MOV A,@R
    mov  A, 0x38
    cjne A, #0xC3, L3b
    mov  A, #0x00
    mov  A, @R0
    cjne A, #0xC3, L3b
    mov  A, #0x22
    mov  R7, A                           ; MOV Rn,A
    mov  A, R7
    cjne A, #0x22, L3b
    mov  0x49, #0x00
    mov  0x49, A                         ; MOV dir,A ; MOV A,dir
    mov  A, #0x00
    mov  A, 0x49
    cjne A, #0x22, L3b
    mov  0x4A, #0x33
    mov  0x4B, #0x44
    mov  0x4A, 0x4B                      ; MOV dir,dir
    mov  A, 0x4A
    cjne A, #0x44, L3b
    mov  R0, #0x4C
    mov  0x4C, #0x77
    mov  0x4D, #0x00
    mov  0x4D, @R0                       ; MOV dir,@R
    mov  A, 0x4D
    cjne A, #0x77, L3b
    mov  R0, #0x4E
    mov  0x4E, #0x88
    mov  R4, #0x00
    mov  R4, 0x4E                        ; MOV Rn,dir
    mov  A, R4
    cjne A, #0x88, L3b
    mov  R0, #0x50
    mov  @R0, #0x66                      ; MOV @R,#
    mov  A, 0x50
    cjne A, #0x66, L3b
    mov  DPTR, #0x8000
    inc  DPTR                            ; MOV DPTR,# ; INC DPTR
    mov  A, DPH
    cjne A, #0x80, L3b
    mov  A, DPL
    cjne A, #0x01, L3b
    ret
L3b:
    fail_halt

t_dptr_movx:
    mov  DPTR, #0x8000
    mov  A, #0x11
    movx @DPTR, A                        ; MOVX @DPTR,A ; A,@DPTR
    mov  A, #0x00
    movx A, @DPTR
    cjne A, #0x11, L4b
    mov  P2, #0x80
    mov  R0, #0x02
    mov  A, #0x22
    movx @R0, A                         ; MOVX @R0 (P2=hi)
    mov  A, #0x00
    movx A, @R0
    cjne A, #0x22, L4b
    mov  P2, #0x80
    mov  R1, #0x03
    mov  A, #0x33
    movx @R1, A                         ; MOVX @R1
    mov  A, #0x00
    movx A, @R1
    cjne A, #0x33, L4b
    ret
L4b:
    fail_halt

t_movc_xch_stack:
    mov  DPTR, #movc_byte
    mov  A, #0x00
    movc A, @A+DPTR                      ; MOVC @A+DPTR
    cjne A, #0x37, L5b
    mov  A, #0x00
    movc A, @A+PC                        ; MOVC @A+PC
mo_vc_pc_imm:
    .db 0x55
    cjne A, #0x55, L5b
    mov  A, #0xAB
    mov  0x51, #0x12
    xch  A, 0x51                         ; XCH A,dir
    cjne A, #0x12, L5b
    mov  A, 0x51
    cjne A, #0xAB, L5b
    mov  R0, #0x52
    mov  A, #0x11
    mov  0x52, #0x22
    xch  A, @R0                          ; XCH A,@R
    cjne A, #0x22, L5b
    mov  A, 0x52
    cjne A, #0x11, L5b
    mov  A, #0xCD
    mov  R1, #0xEF
    xch  A, R1                           ; XCH A,Rn
    cjne A, #0xEF, L5b
    mov  A, R1
    cjne A, #0xCD, L5b
    mov  A, #0x12
    mov  R0, #0x53
    mov  0x53, #0x34
    xchd A, @R0                          ; XCHD A,@R
    cjne A, #0x14, L5b
    mov  A, 0x53
    cjne A, #0x32, L5b
    mov  A, #0x99
    mov  0x54, A
    push 0x54                            ; PUSH/POP direct
    mov  0x54, #0x00
    pop  0x54
    mov  A, 0x54
    cjne A, #0x99, L5b
    ret
L5b:
    fail_halt

t_carry_jb_movc_bits:
    setb C
    jc   c_ok                            ; JC / JNC
    sjmp L60b
c_ok:
    clr  C
    jnc  nc_ok
    sjmp L60b
nc_ok:
    setb C
    cpl  C                               ; CPL C
    jc   L60b
    setb C
    anl  C, /PSW.7                       ; ANL C,/CY ; ORL C,/CY
    jc   L60b
    setb C
    orl  C, /PSW.7
    jnc  L60b
    mov  0x20, #0x00
    setb 0x00
    jb   0x00, jb_ok                     ; JB / JNB / JBC iram bit
    sjmp L60b
jb_ok:
    clr  0x00
    jnb  0x00, jnb_ok
    sjmp L60b
jnb_ok:
    setb 0x00
    jbc  0x00, jbc_ok
    sjmp L60b
jbc_ok:
    mov  A, 0x20
    jnz  L60b                            ; JNZ (A)
    mov  0x20, #0xFF
    clr  0x00
    setb 0x00
    cpl  0x00                            ; CLR/SETB/CPL bit
    mov  A, 0x20
    cjne A, #0xFE, L60b
    clr  C
    setb 0x01
    mov  C, 0x01                         ; MOV C,bit ; MOV bit,C
    jnc  L60b
    clr  0x01
    mov  0x02, C
    jb   0x02, cb2_ok
    sjmp L60b
cb2_ok:
    mov  0x21, #0x00
    setb 0x08
    setb C
    anl  C, 0x08                         ; ANL/ORL C,bit & /bit
    jnc  L60b
    clr  C
    anl  C, 0x08
    jc   L60b
    setb C
    anl  C, /0x08
    jc   L60b
    clr  C
    setb 0x09
    orl  C, 0x09
    jnc  L60b
    clr  C
    orl  C, /0x09
    jc   L60b
    setb C
    orl  C, /0x09
    jnc  L60b
    ret
L60b:
    fail_halt

t_cjne_djnz_jz:
    mov  0x55, #0x10
    mov  A, #0x20
    clr  C
    cjne A, 0x55, cj_ge                  ; CJNE A,dir
    sjmp L61b
cj_ge:
    jc   L61b
    mov  R0, #0x56
    mov  0x56, #0x88
    mov  A, #0x00
    cjne A, #0x01, cj_imm_ok             ; CJNE A,# (fall-through)
    sjmp L61b
cj_imm_ok:
    mov  R1, #0x05
    mov  0x5F, R1
    mov  A, #0x05
    cjne A, 0x5F, L61b                   ; CJNE A,dir (==Rn)
    mov  R0, #0x57
    mov  @R0, #0x77
    mov  A, #0x00
    cjne @R0, #0x78, cj_at_ok            ; CJNE @R,#
    sjmp L61b
cj_at_ok:
    mov  A, #0x11
    cjne A, #0x22, cj_a_ok               ; CJNE A,# (taken branch)
    sjmp L61b
cj_a_ok:
    mov  0x58, #0x02
    djnz 0x58, dj1                       ; DJNZ direct
    sjmp L61b
dj1:
    mov  A, 0x58
    cjne A, #0x01, L61b
    djnz 0x58, L61b
    mov  A, 0x58
    cjne A, #0x00, L61b
    mov  R7, #0x02
    djnz R7, dj2
    sjmp L61b
dj2:
    djnz R7, L61b                        ; DJNZ Rn
    mov  A, R7
    cjne A, #0x00, L61b
    mov  A, #0x00
    jz   jz_ok                           ; JZ / JNZ
    sjmp L61b
jz_ok:
    mov  A, #0x01
    jnz  jnz_ok
    sjmp L61b
jnz_ok:
    ret
L61b:
    fail_halt

t_flow_ajmp_acall_ljmp_lcall_reti_jmpa:
    sjmp  sjmp_ok                        ; SJMP
    sjmp  L7b
sjmp_ok:
    ajmp  aj1                            ; AJMP
    sjmp  L7b
aj1:
    acall sub_acall                      ; ACALL / RET
after_acall:
    lcall sub_lcall                      ; LCALL / RET
after_lcall:
    acall sub_reti                       ; RETI
after_reti:
    ljmp  lj1                            ; LJMP
    sjmp  L7b
lj1:
    mov   DPTR, #jmp_tab
    mov   A, #0x00
    jmp   @A+DPTR                        ; JMP @A+DPTR
    sjmp  L7b
jmp_tab:
    ljmp  jmp_ind_ok                     ; LJMP (from table)
    sjmp  L7b
jmp_ind_ok:
    ret
L7b:
    fail_halt

sub_acall:
    ret                                  ; RET (from ACALL)

sub_lcall:
    ret                                  ; RET (from LCALL)

sub_reti:
    reti                                 ; RETI

movc_byte:
    .db 0x37                             ; MOVC table byte

.area DSEG (DATA)
.area XSEG (XDATA)
.area HOME (CODE)
.area PSEG (CODE)
