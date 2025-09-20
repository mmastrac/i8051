ASM_BUILD_DIR := "target/asm-tests/build"
ASM_OUTPUT_DIR := "target/asm-tests"

make-build-dirs:
    @mkdir -p {{ASM_BUILD_DIR}} 2>/dev/null || true
    @mkdir -p {{ASM_OUTPUT_DIR}}/asm 2>/dev/null || true
    @mkdir -p {{ASM_OUTPUT_DIR}}/c 2>/dev/null || true

build-asm NAME:
    @echo "[ASM] tests/asm/{{NAME}}..."
    @mkdir -p "{{ASM_BUILD_DIR}}"/asm/{{NAME}}
    sdas8051 -o {{ASM_BUILD_DIR}}/asm/{{NAME}}/{{NAME}}.rel tests/asm/{{NAME}}.asm
    sdas8051 -l {{ASM_BUILD_DIR}}/asm/{{NAME}}/{{NAME}}.lst tests/asm/{{NAME}}.asm
    sdas8051 -s {{ASM_BUILD_DIR}}/asm/{{NAME}}/{{NAME}}.sym tests/asm/{{NAME}}.asm
    sdcc -mmcs51 --code-size 8192 -o {{ASM_BUILD_DIR}}/asm/{{NAME}}/{{NAME}} {{ASM_BUILD_DIR}}/asm/{{NAME}}/{{NAME}}.rel
    sdobjcopy -I ihex -O binary {{ASM_BUILD_DIR}}/asm/{{NAME}}/{{NAME}} {{ASM_OUTPUT_DIR}}/asm/{{NAME}}.bin

build-c NAME:
    @echo "[C] tests/c/{{NAME}}..."
    @mkdir -p "{{ASM_BUILD_DIR}}"/c/{{NAME}}
    sdcc -mmcs51 --code-size 8192 -o {{ASM_BUILD_DIR}}/{{NAME}}/{{NAME}} tests/c/{{NAME}}.c
    sdobjcopy -I ihex -O binary {{ASM_BUILD_DIR}}/{{NAME}}/{{NAME}} {{ASM_OUTPUT_DIR}}/c/{{NAME}}.bin

build-asm-tests: make-build-dirs
    @just build-asm smoketest
    @just build-asm arithmetic

    @just build-c smoketest
    @just build-c math
    @just build-c mul
