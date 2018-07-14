use std::num::Wrapping;

use super::isa::{CC, Reg, RegM, RegPair};

/// Holds the CPU condition flags.
///
/// This is represented as a struct of bools, rather than a packed byte, because
/// doing so significantly improves emulator performance.
#[derive(Debug)]
pub struct Flags {
    /// Result was zero.
    pub zero: bool,
    /// Result produced carry.
    pub carry: bool,
    /// Result had even parity.
    pub parity: bool,
    /// Result produced auxiliary (half) carry.
    pub aux: bool,
    /// Result was negative.
    pub sign: bool,
}

/// Reset state for `Flags`.
impl Default for Flags {
    fn default() -> Self {
        Flags {
            zero: false,
            carry: false,
            parity: false,
            aux: false,
            sign: false,
        }
    }
}

impl Flags {
    /// Packs the flags into a `u8`. Used when pushing PSW.
    pub fn bits(&self) -> u8 {
        (self.carry as u8)
            | (1u8 << 1)  // UN1 flag always observes as 1
            | ((self.parity as u8) << 2)
            | ((self.aux as u8) << 4)
            | ((self.zero as u8) << 6)
            | ((self.sign as u8) << 7)
    }

    /// Unpacks the flags from a `u8`. Used when popping PSW.
    pub fn from_bits(&mut self, val: u8) {
        self.carry  = (val & (1 << 0)) != 0;
        self.zero   = (val & (1 << 6)) != 0;
        self.parity = (val & (1 << 2)) != 0;
        self.aux    = (val & (1 << 4)) != 0;
        self.sign   = (val & (1 << 7)) != 0;
    }

    /// Evaluates a condition given the current state of the flags.
    pub fn condition(&self, cc: CC) -> bool {
        match cc {
            CC::NZ => !self.zero,
            CC::Z  =>  self.zero,
            CC::NC => !self.carry,
            CC::C  =>  self.carry,
            CC::PO => !self.parity,
            CC::PE =>  self.parity,
            CC::P  => !self.sign,
            CC::N  =>  self.sign,
        }
    }
}

/// Shorthand for a `u8` where overflow is accepted and causes wrapping.
pub type W8 = Wrapping<u8>;
/// Shorthand for a `u16` where overflow is accepted and causes wrapping.
pub type W16 = Wrapping<u16>;

/// Emulator state.
pub struct Emu {
    /// 8-bit registers in encoding order, with placeholder at index 6.
    registers: [W8; 8],
    /// Stack pointer. The stack is full-descending: this points to the most
    /// recently pushed value, and the next value will be pushed 2 lower. Stack
    /// pointer alignment is not required or maintained.
    sp: W16,
    /// Program counter.
    pc: W16,
    /// Memory image. You can't tell from the type, but this is 64kiB.
    pub mem: Vec<u8>,
    /// Cycle counter, increments for every 8080 cycle (*not* every emulated
    /// instruction).
    pub cycles: usize,
    /// Interrupts enabled?
    interrupts: bool,

    /// CPU flags.
    pub flags: Flags,
}

/// Emulator reset state.
impl Default for Emu {
    fn default() -> Self {
        Emu {
            registers: [Wrapping(0); 8],
            sp: Wrapping(0),
            pc: Wrapping(0),
            mem: vec![0; 0x1_0000],
            cycles: 0,
            interrupts: false,
            flags: Flags::default(),
        }
    }
}

impl Emu {
    /// Accesses the current value of a register.
    #[inline]
    pub fn reg(&self, i: Reg) -> W8 {
        self.registers[i as usize]
    }

    /// Accesses either a register or memory through `(HL)`.
    pub fn reg_m(&self, i: RegM) -> W8 {
        match i {
            RegM::M => {
                let addr = self.reg_pair(RegPair::HL);
                self.load(addr)
            },
            RegM::R(r) => self.reg(r),
        }
    }

    /// Updates a register.
    pub fn set_reg(&mut self, i: Reg, v: W8) {
        self.registers[i as usize] = v
    }

    /// Updates either a register or memory through `(HL)`.
    pub fn set_reg_m(&mut self, i: RegM, v: W8) {
        match i {
            RegM::M => {
                let addr = self.reg_pair(RegPair::HL);
                self.store(addr, v)
            },
            RegM::R(r) => self.set_reg(r, v),
        }
    }

    /// Accesses a 16-bit register pair.
    #[inline]
    pub fn reg_pair(&self, i: RegPair) -> W16 {
        match i {
            RegPair::SP => self.sp,
            _ => {
                let offset = 2 * (i as usize);
                debug_assert!(offset != 5 && offset != 6);
                Wrapping(
                    ((self.registers[offset].0 as u16) << 8)
                    | (self.registers[offset + 1].0 as u16))
            },
        }
    }

    /// Updates a 16-bit register pair.
    pub fn set_reg_pair(&mut self, i: RegPair, v: W16) {
        match i {
            RegPair::SP => self.sp = v,
            _ => {
                let offset = 2 * (i as usize);
                debug_assert!(offset != 5 && offset != 6);
                self.registers[offset] = Wrapping((v >> 8).0 as u8);
                self.registers[offset + 1] = Wrapping(v.0 as u8);
            },
        }
    }

    /// Increments the cycle counter by `n`.
    ///
    /// This is often the last thing to be called by a dispatch function, so it
    /// returns `false` (meaning "not halted") for convenience.
    pub fn advance(&mut self, n: usize) -> bool {
        self.cycles += n;
        false
    }

    /// Stores `val` to memory at `addr`.
    pub fn store(&mut self, addr: W16, val: W8) {
        self.mem[addr.0 as usize] = val.0
    }

    /// Loads from memory at `addr`.
    #[inline]
    pub fn load(&self, addr: W16) -> W8 {
        Wrapping(self.mem[addr.0 as usize])
    }

    /// Stores a 16-bit `val` to memory, in little-endian order, at `addr` and
    /// `addr+1`.
    pub fn store16(&mut self, addr: W16, val: W16) {
        self.mem[addr.0 as usize] = val.0 as u8;
        self.mem[addr.0 as usize + 1] = (val.0 >> 8) as u8;
    }

    /// Loads a 16-bit word from memory, in little-endian order, at `addr` and
    /// `addr+1`.
    pub fn load16(&self, addr: W16) -> W16 {
        Wrapping((self.mem[addr.0 as usize] as u16)
                 | ((self.mem[(addr + Wrapping(1)).0 as usize] as u16)
                        << 8))
    }

    /// Consumes an immediate byte from the instruction stream, advancing PC.
    #[inline]
    pub fn take_imm8(&mut self) -> W8 {
        let v = self.load(self.pc);
        self.pc += Wrapping(1);
        v
    }

    /// Consumes an immediate word from the instruction stream, in little-endian
    /// order, advancing PC by two.
    pub fn take_imm16(&mut self) -> W16 {
        let v = self.load16(self.pc);
        self.pc += Wrapping(2);
        v
    }

    /// Effects a jump to `addr` by replacing the PC.
    pub fn jump(&mut self, addr: W16) {
        self.pc = addr
    }

    /// Effects a call to `addr` by pushing the PC.
    pub fn call(&mut self, addr: W16) {
        let pc = self.pc;
        self.push(pc);
        self.pc = addr
    }

    /// Effects a return by popping the PC.
    pub fn ret(&mut self) {
        self.pc = self.pop()
    }

    /// Pushes `val` onto the stack.
    pub fn push(&mut self, val: W16) {
        self.sp -= Wrapping(2);
        let sp = self.sp;
        self.store16(sp, val)
    }

    /// Pops a word from the stack.
    pub fn pop(&mut self) -> W16 {
        let v = self.load16(self.sp);
        self.sp += Wrapping(2);
        v
    }

    pub fn set_interrupt_flag(&mut self, f: bool) {
        self.interrupts = f
    }

    pub fn memory(&self) -> &[u8] {
        &self.mem[..]
    }
    
    pub fn memory_mut(&mut self) -> &mut [u8] {
        &mut self.mem[..]
    }

    #[inline]
    pub fn get_pc(&self) -> u16 {
        self.pc.0
    }
}

pub trait Ports {
    fn write_port(&mut self, port: u8, val: u8);
    fn read_port(&mut self, port: u8) -> u8;
}

impl Ports for () {
    fn write_port(&mut self, _: u8, _: u8) {}
    fn read_port(&mut self, _: u8) -> u8 { 0 }
}
