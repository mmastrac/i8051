; 8051 arithmetic and flag test suite
; Tests ADDC, SUBB, DA, and flag operations
; Output: 0xFF 0xFF at 0x8000 if all tests pass, error code if failed

.module arithmetic_test
.area CSEG (CODE)

_start:
    ; Set up DPTR to point to result memory
    mov DPTR, #0x8000
    
    ; Test 1: Basic ADDC without carry
    mov R7, #0x01      ; Test number
    mov A, #0x25
    mov R0, #0x17
    add A, R0          ; 0x25 + 0x17 = 0x3C, should clear carry
    jc halt_fail       ; If carry set, fail
    cjne A, #0x3C, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 2: ADDC with carry
    mov R7, #0x02      ; Test number
    mov A, #0xFF
    mov R1, #0x01
    add A, R1          ; 0xFF + 0x01 = 0x00, should set carry
    jnc halt_fail      ; If carry not set, fail
    cjne A, #0x00, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 3: SUBB without borrow
    mov R7, #0x03      ; Test number
    mov A, #0x50
    mov R2, #0x30
    subb A, R2         ; 0x50 - 0x30 = 0x20, should clear carry (no borrow)
    jc halt_fail       ; If carry set (borrow), fail
    cjne A, #0x20, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 4: SUBB with borrow
    mov R7, #0x04      ; Test number
    mov A, #0x10
    mov R3, #0x20
    setb C             ; Set carry to ensure borrow
    subb A, R3         ; 0x10 - 0x20 - 1 = 0xEF, should set carry (borrow)
    jnc halt_fail      ; If carry not set, fail
    cjne A, #0xEF, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 5: DA (Decimal Adjust) - BCD addition
    mov R7, #0x05      ; Test number
    mov A, #0x15       ; 15 in BCD
    mov R4, #0x27      ; 27 in BCD
    add A, R4          ; 0x15 + 0x27 = 0x3C
    da A               ; Adjust for BCD: should become 0x42 (42 decimal)
    cjne A, #0x42, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 6: DA with carry generation
    mov R7, #0x06      ; Test number
    mov A, #0x85       ; 85 in BCD
    mov R5, #0x25      ; 25 in BCD
    add A, R5          ; 0x85 + 0x25 = 0xAA
    da A               ; Adjust for BCD: should become 0x10 with carry set
    jnc halt_fail      ; If carry not set, fail
    cjne A, #0x10, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 7: AC (Auxiliary Carry) flag in PSW bit 6
    mov R7, #0x07      ; Test number
    mov A, #0x0F
    mov R6, #0x01
    add A, R6          ; 0x0F + 0x01 = 0x10, should set AC
    mov A, PSW         ; Load PSW to check AC flag
    anl A, #0x40       ; Mask bit 6 (AC flag)
    jz halt_fail       ; AC should be set, so result should be non-zero
    ; Fall through to next test if successful
    
    ; Test 8: OV (Overflow) flag in PSW bit 2
    mov R7, #0x08      ; Test number
    mov A, #0x7F       ; 127 (positive)
    mov R0, #0x01      ; +1
    add A, R0          ; 0x7F + 0x01 = 0x80 (should overflow)
    mov A, PSW         ; Load PSW to check OV flag
    anl A, #0x04       ; Mask bit 2 (OV flag)
    jz halt_fail       ; OV should be set, so result should be non-zero
    ; Fall through to next test if successful
    
    ; Test 9: Complex ADDC chain
    mov R7, #0x09      ; Test number
    clr C              ; Clear carry
    mov A, #0x12
    mov R0, #0x34
    addc A, R0         ; A = 0x12 + 0x34 + 0 = 0x46
    mov R1, #0x56
    addc A, R1         ; A = 0x46 + 0x56 + 0 = 0x9C
    cjne A, #0x9C, halt_fail  ; If result wrong, fail
    ; Fall through to next test if successful
    
    ; Test 10: P (Parity) flag in PSW bit 0
    mov R7, #0x0A      ; Test number
    mov A, #0x01       ; 00000001 (odd number of 1s)
    ; P flag is automatically updated after any operation on A
    mov A, PSW         ; Load PSW to check P flag
    anl A, #0x01       ; Mask bit 0 (P flag)
    jz halt_fail       ; P should be set for odd parity, so result should be non-zero
    ; Fall through to success if all tests passed

success:
    ; All tests passed - write success indicator
    mov A, #0xFF
    movx @DPTR, A      ; First byte: 0xFF
    inc DPTR
    mov A, #0xFF
    movx @DPTR, A      ; Second byte: 0xFF
    sjmp halt

halt_fail:
    ; Write test failure code to memory
    mov A, R7          ; Get test number from R7
    movx @DPTR, A      ; Write error code to 0x8000
    mov A, #0x00
    inc DPTR
    movx @DPTR, A      ; Write 0x00 to 0x8001

halt:
    ; Endless loop to halt execution
    sjmp halt

.area DSEG (DATA)
.area XSEG (XDATA)
.area HOME (CODE)
.area PSEG (CODE)