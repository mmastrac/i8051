set_cpu(name="i8051")
map_bytes(address=CODE:0x0, file="lk201-8051.bin", file_offset=0x0, size=0x1000)
auto_disassemble(address=CODE:0x0)
set_note(address=CODE:0x0..0x1, note=Note(content=r#"The LK201 keyboard controller is an 8051-based board with a 4KB ROM.
It has four LEDs: Hold Screen, Lock, Compose, Wait.
The schematic labels P3 as "keyboard ID" and P2 as a buzzer level output and LED control.
P0 and P1 are the keyboard matrix driven through two LS145s.
"#, fields={}, id="00000000000000000000000000", links=[], tags=[]))
