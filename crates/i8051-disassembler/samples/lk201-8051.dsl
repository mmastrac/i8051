set_cpu(name="i8051")
map_bytes(address=CODE:0x0, file="lk201-8051.bin", file_offset=0x0, size=0x1000)
auto_disassemble(address=CODE:0x0)
