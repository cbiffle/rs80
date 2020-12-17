
use rs80_common::isa::{CC, Reg, RegM, RegPair, Opcode};
use super::ops;

/// Holds the CPU condition flags.
///
/// This is represented as a struct of bools, rather than a packed byte, because
/// doing so significantly improves emulator performance.
#[derive(Copy, Clone, Debug)]
pub struct Flags {
    /// Result was zero.
    pub zero: bool,
    /// Result produced carry.
    pub carry: bool,
    /// Result had even parity.
    pub parity_input: u8,
    /// Result produced auxiliary (half) carry.
    pub aux_input: (u8, u8, u8, bool),
    /// Result was negative.
    pub sign: bool,
}

/// Reset state for `Flags`.
impl Default for Flags {
    fn default() -> Self {
        Flags {
            zero: false,
            carry: false,
            parity_input: 1,
            aux_input: (0, 0, 0, false),
            sign: false,
        }
    }
}

impl Flags {
    pub fn parity(&self) -> bool {
        self.parity_input.count_ones() % 2 == 0
    }

    pub fn aux(&self) -> bool {
        let (a, b, c, f) = self.aux_input;
        f | (((a & 0xF) + (b & 0xF) + c) & 0x10 != 0)
    }

    /// Packs the flags into the low bits of a `u16`. Used when pushing PSW.
    pub fn bits(&self) -> u16 {
        (self.carry as u16)
            | (1u16 << 1)  // UN1 flag always observes as 1
            | ((self.parity() as u16) << 2)
            | ((self.aux() as u16) << 4)
            | ((self.zero as u16) << 6)
            | ((self.sign as u16) << 7)
    }

    /// Unpacks the flags from the PSW. Used when popping.
    pub fn from_psw(&mut self, val: u16) {
        self.carry  = (val & (1 << 0)) != 0;
        self.zero   = (val & (1 << 6)) != 0;
        self.parity_input = (val as u8 & (1 << 2)) | 1;
        self.aux_input = (0, 0, 0, (val & (1 << 4)) != 0);
        self.sign   = (val & (1 << 7)) != 0;
    }

    /// Evaluates a condition given the current state of the flags.
    pub fn condition(&self, cc: CC) -> bool {
        match cc {
            CC::NZ => !self.zero,
            CC::Z  =>  self.zero,
            CC::NC => !self.carry,
            CC::C  =>  self.carry,
            CC::PO => !self.parity(),
            CC::PE =>  self.parity(),
            CC::P  => !self.sign,
            CC::N  =>  self.sign,
        }
    }
}

/// Emulator state.
#[derive(Clone)]
pub struct Emu {
    /// 8-bit registers in encoding order, with placeholder at index 6.
    registers: [u8; 8],
    /// Stack pointer. The stack is full-descending: this points to the most
    /// recently pushed value, and the next value will be pushed 2 lower. Stack
    /// pointer alignment is not required or maintained.
    sp: u16,
    /// Program counter.
    pc: u16,
    /// Cycle counter, increments for every 8080 cycle (*not* every emulated
    /// instruction).
    pub cycles: usize,
    /// Interrupts enabled?
    interrupts: bool,

    /// CPU flags.
    pub flags: Flags,

    pub inst_count: usize,
    /// Memory image.
    ///
    /// This is 64kiB + 1 so that we don't have to check for wrap during
    /// accesses of 16-bit quantities -- we just read "off the end." (The last
    /// byte is maintained in the store operations.)
    pub mem: [u8; 65536 + 1],
}

/// Emulator reset state.
impl Default for Emu {
    fn default() -> Self {
        Emu {
            registers: [0; 8],
            sp: 0,
            pc: 0,
            mem: [0; 0x1_0001],
            cycles: 0,
            interrupts: false,
            flags: Flags::default(),
            inst_count: 0,
        }
    }
}

impl Emu {
    /// Accesses the current value of a register.
    #[inline]
    pub fn reg(&self, i: Reg) -> u8 {
        self.registers[i as usize]
    }

    /// Accesses either a register or memory through `(HL)`.
    pub fn reg_m(&self, i: RegM) -> u8 {
        match i {
            RegM::M => {
                let addr = self.reg_pair(RegPair::HL);
                self.load(addr)
            },
            RegM::R(r) => self.reg(r),
        }
    }

    /// Updates a register.
    pub fn set_reg(&mut self, i: Reg, v: u8) {
        self.registers[i as usize] = v
    }

    /// Updates either a register or memory through `(HL)`.
    pub fn set_reg_m(&mut self, i: RegM, v: u8) {
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
    pub fn reg_pair(&self, i: RegPair) -> u16 {
        match i {
            RegPair::SP => self.sp,
            _ => {
                let offset = 2 * (i as usize);
                debug_assert!(offset != 5 && offset != 6);
                ((self.registers[offset] as u16) << 8)
                    | (self.registers[offset + 1] as u16)
            },
        }
    }

    /// Updates a 16-bit register pair.
    pub fn set_reg_pair(&mut self, i: RegPair, v: u16) {
        match i {
            RegPair::SP => self.sp = v,
            _ => {
                let offset = 2 * (i as usize);
                debug_assert!(offset != 5 && offset != 6);
                self.registers[offset] = (v >> 8) as u8;
                self.registers[offset + 1] = v as u8;
            },
        }
    }

    /// Increments the cycle counter by `n`.
    ///
    /// This is often the last thing to be called by a dispatch function, so it
    /// returns `false` (meaning "not halted") for convenience.
    pub fn advance(&mut self, _: usize) -> bool {
        false
    }

    /// Stores `val` to memory at `addr`.
    pub fn store(&mut self, addr: u16, val: u8) {
        self.mem[addr as usize] = val;
        // Maintain last byte to make loads cheaper.
        self.mem[65536] = self.mem[0];
    }

    /// Loads from memory at `addr`.
    #[inline]
    pub fn load(&self, addr: u16) -> u8 {
        self.mem[usize::from(addr)]
    }

    /// Stores a 16-bit `val` to memory, in little-endian order, at `addr` and
    /// `addr+1`.
    pub fn store16(&mut self, addr: u16, val: u16) {
        self.mem[addr as usize] = val as u8;
        self.mem[addr.wrapping_add(1) as usize] = (val >> 8) as u8;
        // Maintain last byte to make loads cheaper.
        self.mem[65536] = self.mem[0];
    }

    /// Loads a 16-bit word from memory, in little-endian order, at `addr` and
    /// `addr+1`.
    pub fn load16(&self, addr: u16) -> u16 {
        let a = usize::from(addr);
        // Exploit extra byte in memory image to avoid checking for wrap.
        (self.mem[a] as u16)
            | ((self.mem[a + 1] as u16) << 8)
    }

    /// Consumes an immediate byte from the instruction stream, advancing PC.
    #[inline]
    pub fn take_imm8(&mut self) -> u8 {
        let v = self.load(self.pc);
        self.pc = self.pc.wrapping_add(1);
        v
    }

    /// Consumes an immediate word from the instruction stream, in little-endian
    /// order, advancing PC by two.
    pub fn take_imm16(&mut self) -> u16 {
        let v = self.load16(self.pc);
        self.pc = self.pc.wrapping_add(2);
        v
    }

    /// Effects a jump to `addr` by replacing the PC.
    pub fn jump(&mut self, addr: u16) {
        self.pc = addr
    }

    /// Effects a call to `addr` by pushing the PC.
    pub fn call(&mut self, addr: u16) {
        let pc = self.pc;
        self.push(pc);
        self.pc = addr
    }

    /// Effects a return by popping the PC.
    pub fn ret(&mut self) {
        self.pc = self.pop()
    }

    /// Pushes `val` onto the stack.
    pub fn push(&mut self, val: u16) {
        self.sp = self.sp.wrapping_sub(2);
        let sp = self.sp;
        self.store16(sp, val)
    }

    /// Pops a word from the stack.
    pub fn pop(&mut self) -> u16 {
        let v = self.load16(self.sp);
        self.sp = self.sp.wrapping_add(2);
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
        self.pc
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

#[derive(Copy, Clone, Debug)]
pub enum RunError {
    /// Instruction not implemented at address.
    UnimplementedInstruction(u8, u16),
}

/// Runs the emulation until the machine encounters a `HLT` (or an illegal
/// instruction).
///
/// On halt, returns `Ok(last_pc, pc)`, where `pc` is the address of the `HLT`
/// instruction, and `last_pc` is the address of the instruction executed just
/// before it. (This is often `pc-1`, but may differ if the `HLT` was reached by
/// a call or jump.)
#[inline]
pub fn run(emu: &mut Emu, io: &mut dyn Ports) -> Result<(u16, u16), RunError> {
    let mut pc = 0xFFFF;
    let mut last_pc;

    let mut ctx = ops::Ctx { io };

    loop {
        // Move last instruction start into previous buffer.
        last_pc = pc;
        // Record start of this instruction.
        pc = emu.get_pc();
        let op = emu.take_imm8();
        emu.inst_count += 1;
        let halted = ops::dispatch(emu, &mut ctx, Opcode(op));
        if halted { return Ok((last_pc, pc)) }
    }
}

/// Steps the emulation by one instruction.
///
/// On success, returns `Ok(halted)`, where `halted` is a flag indicating
/// whether the executed instruction was a `HLT`.
#[inline]
pub fn step(emu: &mut Emu, io: &mut dyn Ports) -> Result<bool, RunError> {
    let mut ctx = ops::Ctx { io };

    let op = emu.take_imm8();
    emu.inst_count += 1;
    let halted = ops::dispatch(emu, &mut ctx, Opcode(op));
    Ok(halted)
}
