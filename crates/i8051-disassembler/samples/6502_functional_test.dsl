set_cpu(name="mos6502")
map_bytes(address=CODE:0x0, file="6502_functional_test.bin", file_offset=0x0, size=0x10000)
auto_disassemble(address=CODE:0x400)
