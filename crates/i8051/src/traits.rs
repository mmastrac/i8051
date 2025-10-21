use crate::Interrupt;
use crate::sfr::{SFR_BASE, SFR_P2};

/// A (mostly) read-only view of the CPU state with the ability to trigger
/// interrupts.
pub trait CpuView {
    /// Read a byte from external data memory.
    fn read_xdata(&self, addr: u16) -> u8;
    /// Read a byte from code memory.
    fn read_code(&self, addr: u16) -> u8;

    fn pc(&self) -> u16;
    fn a(&self) -> u8;
    fn b(&self) -> u8;
    fn dptr(&self) -> u16;
    fn dpl(&self) -> u8;
    fn dph(&self) -> u8;
    fn psw(&self, flag: u8) -> bool;
    fn sp(&self) -> u8;
    fn r(&self, x: u8) -> u8;

    fn sfr(&self, addr: u8) -> u8;

    /// Trigger an interrupt. This modifies the internal CPU state.
    fn interrupt(&self, interrupt: Interrupt);
}

/// A context for the CPU. This can be implemented by hand, but is most
/// conveniently implemented using a tuple of `(PortMapper, MemoryMapper, MemoryMapper)`
/// for ports, xdata, and code.
pub trait CpuContext {
    type Ports: PortMapper;
    type Xdata: MemoryMapper;
    type Code: ReadOnlyMemoryMapper;

    fn ports(&self) -> &Self::Ports;
    fn xdata(&self) -> &Self::Xdata;
    fn code(&self) -> &Self::Code;
    fn ports_mut(&mut self) -> &mut Self::Ports;
    fn xdata_mut(&mut self) -> &mut Self::Xdata;
    fn code_mut(&mut self) -> &mut Self::Code;
}

/// A trait to provide memory read/write operations.
pub trait MemoryMapper {
    type WriteValue;
    fn read<C: CpuView>(&self, cpu: &C, addr: u16) -> u8;
    fn prepare_write<C: CpuView>(&self, cpu: &C, addr: u16, value: u8) -> Self::WriteValue;
    fn write(&mut self, value: Self::WriteValue);
}

/// A trait to provide read-only memory read operations.
pub trait ReadOnlyMemoryMapper {
    fn read<C: CpuView>(&self, cpu: &C, addr: u16) -> u8;
}

/// A trait to provide port read/write operations.
pub trait PortMapper {
    type WriteValue;
    /// Returns whether the given address is of interest to this port mapper.
    fn interest<C: CpuView>(&self, cpu: &C, addr: u8) -> bool;
    /// Extend a read through `@R0` or `@R1` to a 16-bit address. By default, this
    /// uses the value of `P2` as the high byte.
    fn extend_short_read<C: CpuView>(&self, cpu: &C, addr: u8) -> u16 {
        addr as u16 | (cpu.sfr(SFR_P2) as u16) << 8
    }
    /// Read the value at the given address.
    fn read<C: CpuView>(&self, cpu: &C, addr: u8) -> u8;
    /// Read the latch value at the given address. Only used for
    /// `SFR_P0`..`SFR_P3`, and only for instructions that operate on latched
    /// data rather than the input data.
    ///
    /// Instructions in [`crate::ops`] that operate on latch are indicated by
    /// use of `PBIT` and `PDATA` operands (rather than `BIT` and `DATA`
    /// operands).
    fn read_latch<C: CpuView>(&self, _cpu: &C, #[expect(unused)] addr: u8) -> u8 {
        unreachable!()
    }
    /// Prepare to write the given value to the given port address.
    fn prepare_write<C: CpuView>(&self, cpu: &C, addr: u8, value: u8) -> Self::WriteValue;

    /// Write the given value to the given port address.
    fn write(&mut self, value: Self::WriteValue);

    /// Perform any necessary internal state updates between CPU steps.
    fn tick<C: CpuView>(&mut self, cpu: &C);
}

impl MemoryMapper for () {
    type WriteValue = ();
    fn read<C: CpuView>(&self, _cpu: &C, _addr: u16) -> u8 {
        0
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, _addr: u16, _value: u8) -> Self::WriteValue {
        ()
    }
    fn write(&mut self, _value: Self::WriteValue) {}
}

impl ReadOnlyMemoryMapper for () {
    fn read<C: CpuView>(&self, _cpu: &C, _addr: u16) -> u8 {
        0
    }
}

pub struct DefaultPortMapper {
    sfr: [u8; 128],
}

impl Default for DefaultPortMapper {
    fn default() -> Self {
        Self { sfr: [0; 128] }
    }
}

impl PortMapper for DefaultPortMapper {
    type WriteValue = (u8, u8);
    fn interest<C: CpuView>(&self, _cpu: &C, _addr: u8) -> bool {
        true
    }
    fn extend_short_read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u16 {
        addr as u16
    }
    fn read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        self.sfr[addr.wrapping_sub(SFR_BASE) as usize]
    }
    fn read_latch<C: CpuView>(&self, _cpu: &C, addr: u8) -> u8 {
        self.sfr[addr.wrapping_sub(SFR_BASE) as usize]
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        (addr, value)
    }
    fn write(&mut self, (addr, value): Self::WriteValue) {
        self.sfr[addr.wrapping_sub(SFR_BASE) as usize] = value;
    }
    fn tick<C: CpuView>(&mut self, _cpu: &C) {}
}

impl PortMapper for () {
    type WriteValue = ();
    fn interest<C: CpuView>(&self, _cpu: &C, _addr: u8) -> bool {
        false
    }
    fn extend_short_read<C: CpuView>(&self, _cpu: &C, addr: u8) -> u16 {
        addr as u16
    }
    fn read<C: CpuView>(&self, _cpu: &C, _addr: u8) -> u8 {
        unreachable!()
    }
    fn prepare_write<C: CpuView>(&self, _cpu: &C, _addr: u8, _value: u8) -> Self::WriteValue {
        unreachable!()
    }
    fn write(&mut self, _value: Self::WriteValue) {
        unreachable!()
    }
    fn tick<C: CpuView>(&mut self, _cpu: &C) {}
}

pub enum WriteChoice<A, B> {
    A(A),
    B(B),
}

impl<A, B> PortMapper for (A, B)
where
    A: PortMapper,
    B: PortMapper,
{
    type WriteValue = WriteChoice<A::WriteValue, B::WriteValue>;
    fn interest<C: CpuView>(&self, cpu: &C, addr: u8) -> bool {
        self.0.interest(cpu, addr) || self.1.interest(cpu, addr)
    }
    fn extend_short_read<C: CpuView>(&self, cpu: &C, addr: u8) -> u16 {
        if self.0.interest(cpu, addr) {
            self.0.extend_short_read(cpu, addr)
        } else if self.1.interest(cpu, addr) {
            self.1.extend_short_read(cpu, addr)
        } else {
            unreachable!()
        }
    }
    fn read<C: CpuView>(&self, cpu: &C, addr: u8) -> u8 {
        if self.0.interest(cpu, addr) {
            self.0.read(cpu, addr)
        } else if self.1.interest(cpu, addr) {
            self.1.read(cpu, addr)
        } else {
            unreachable!()
        }
    }
    fn read_latch<C: CpuView>(&self, cpu: &C, addr: u8) -> u8 {
        if self.0.interest(cpu, addr) {
            self.0.read_latch(cpu, addr)
        } else if self.1.interest(cpu, addr) {
            self.1.read_latch(cpu, addr)
        } else {
            unreachable!()
        }
    }
    fn prepare_write<C: CpuView>(&self, cpu: &C, addr: u8, value: u8) -> Self::WriteValue {
        if self.0.interest(cpu, addr) {
            WriteChoice::A(self.0.prepare_write(cpu, addr, value))
        } else if self.1.interest(cpu, addr) {
            WriteChoice::B(self.1.prepare_write(cpu, addr, value))
        } else {
            unreachable!()
        }
    }
    fn write(&mut self, value: Self::WriteValue) {
        match value {
            WriteChoice::A(value) => self.0.write(value),
            WriteChoice::B(value) => self.1.write(value),
        }
    }
    fn tick<C: CpuView>(&mut self, cpu: &C) {
        self.0.tick(cpu);
        self.1.tick(cpu);
    }
}

impl CpuContext for () {
    type Ports = ();
    type Xdata = ();
    type Code = ();
    fn ports(&self) -> &Self::Ports {
        &()
    }
    fn xdata(&self) -> &Self::Xdata {
        &()
    }
    fn code(&self) -> &Self::Code {
        &()
    }
    fn ports_mut(&mut self) -> &mut Self::Ports {
        self
    }
    fn xdata_mut(&mut self) -> &mut Self::Xdata {
        self
    }
    fn code_mut(&mut self) -> &mut Self::Code {
        self
    }
}

impl<A: PortMapper, X: MemoryMapper, C: ReadOnlyMemoryMapper> CpuContext for (A, X, C) {
    type Ports = A;
    type Xdata = X;
    type Code = C;
    fn ports(&self) -> &Self::Ports {
        &self.0
    }
    fn ports_mut(&mut self) -> &mut Self::Ports {
        &mut self.0
    }
    fn xdata(&self) -> &Self::Xdata {
        &self.1
    }
    fn xdata_mut(&mut self) -> &mut Self::Xdata {
        &mut self.1
    }
    fn code(&self) -> &Self::Code {
        &self.2
    }
    fn code_mut(&mut self) -> &mut Self::Code {
        &mut self.2
    }
}
