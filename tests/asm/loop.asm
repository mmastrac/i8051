.module loop
.area CSEG (CODE)

_start:
    ; Set up DPTR to point to 0x8000
    mov DPTR, #0x8000

    ; Outer loop: 25 iterations (R7)
    mov r7, #0xff
loop0:
    ; Inner loop: 25 iterations (R6)
    mov r6, #0xff
loop1:
    mov a, #0x01
    movx @DPTR, A
    djnz r6, loop1
    djnz r7, loop0

    ; Halt execution
halt:
    sjmp halt

.area DSEG (DATA)
.area XSEG (XDATA)
.area HOME (CODE)
.area PSEG (CODE)
