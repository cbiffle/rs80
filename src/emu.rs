//! Emulator state and step routines.
//!
//! This module is mostly concerned with representing machine state, flags, and
//! stepping efficiently. For implementations of arithmetic primitives and such,
//! see the `ops` module. For the code for each instruction in the 8080
//! instruction set, see `isa-ops.txt` at the root of the project.

use rs80_common::isa::{CC, Reg, RegM, RegPair, Opcode};
use super::ops;

/// Holds the CPU condition flags.
///
/// The flags register in the 8080 is 8 bits wide containing 5 actual flags:
/// Zero, Parity, Sign, Carry, and Aux / half carry. As a result, you might
/// reasonably expect me to represent `Flags` as a `u8` -- yet doing so hurts
/// performance significantly. There are two reasons for this.
///
/// 1. The binary representation of the flags register is only visible after the
///    `PUSH PSW` instruction, which is rare compared to arithmetic instructions
///    that affect flags.
///
/// 2. Some of the flag conditions are more expensive to compute than others,
///    yet their results are rarely used. (Looking at you, Aux.)
///
/// After experiments, I've settled on a hybrid approach.
///
/// The Zero, Parity, and Sign flags are packed into a byte in the same bit
/// positions they occupy in the flags register; this is because those three
/// flags are always computed the same way, and their values only depend on the
/// 8-bit ALU output -- so we can compute all three at once using a lookup
/// table.
///
/// The carry flag is stored separately as a `bool` to make it faster to write
/// and access.
///
/// Finally, the aux flag is _lazily_ computed using four bytes as input to a
/// routine (see `aux()`). In practice, this only happens when
///
/// - The PSW is read via `PUSH PSW`, or
/// - A program actually attempts decimal arithmetic using `DAA`.
///
/// There are no conditional jumps based on the Aux flag.
#[derive(Copy, Clone, Debug, Default)]
pub struct Flags {
    /// Zero, Parity, and Sign, represented as they appear in the flags
    /// register.
    pub zps: u8,

    /// Carry flag.
    pub carry: bool,
    /// Inputs to auxiliary/half-carry calculation.
    pub aux_input: (u8, u8, u8, bool),
}

impl Flags {
    /// Reads out the S flag.
    pub fn sign(&self) -> bool {
        self.zps & (1 << 7) != 0
    }

    /// Reads out the Z flag.
    pub fn zero(&self) -> bool {
        self.zps & (1 << 6) != 0
    }

    /// Reads out the PE flag.
    pub fn parity(&self) -> bool {
        self.zps & (1 << 2) != 0
    }

    /// Reads out the aux/half-carry flag.
    pub fn aux(&self) -> bool {
        let (a, b, c, f) = self.aux_input;
        f | (((a & 0xF) + (b & 0xF) + c) & 0x10 != 0)
    }

    /// Packs the flags into the low bits of a `u16`. Used when pushing PSW.
    pub fn bits(&self) -> u16 {
        u16::from(self.zps)
            | u16::from(self.carry)
            | (1u16 << 1)  // UN1 flag always observes as 1
            | ((self.aux() as u16) << 4)
    }

    /// Unpacks the flags from the PSW representation. Used when popping PSW.
    pub fn from_psw(&mut self, val: u16) {
        self.zps = (val as u8) & 0b1100_0100;

        self.carry  = (val & (1 << 0)) != 0;
        self.aux_input = (0, 0, 0, (val & (1 << 4)) != 0);
    }

    /// Evaluates a condition given the current state of the flags.
    pub fn condition(&self, cc: CC) -> bool {
        match cc {
            CC::NZ => !self.zero(),
            CC::Z  =>  self.zero(),
            CC::NC => !self.carry,
            CC::C  =>  self.carry,
            CC::PO => !self.parity(),
            CC::PE =>  self.parity(),
            CC::P  => !self.sign(),
            CC::N  =>  self.sign(),
        }
    }
}

/// Core CPU state.
#[derive(Clone)]
pub struct Emu {
    /// 8-bit registers in modified encoding order.
    ///
    /// 8-bit register/memory operands are identified in instructions by a 3-bit
    /// index, corresponding to B, C, D, E, H, L, M, and A, respectively.
    ///
    /// When 8-bit registers are paired into 16-bit registers, they are BC, DE,
    /// HL, with the B/D/H registers in the more significant position. This
    /// means the encoding view of the register file is effectively _big
    /// endian._
    ///
    /// For performance on little endian hosts, we store the registers in a
    /// modified order by inverting the bottom bit of the index: C, B, E, D, L,
    /// H, A, M. This allows register access code to use 16-bit operations
    /// without byte swaps.
    ///
    /// M is not a real register, but we allocate a byte for it in `registers`
    /// for consistency. That byte is unused.
    registers: [u8; 8],
    /// Stack pointer. The stack is full-descending: this points to the most
    /// recently pushed value, and the next value will be pushed 2 lower. Stack
    /// pointer alignment is not required or maintained.
    ///
    /// This is maintained as a `usize` for efficiency but only the bottom 16
    /// bits are significant.
    sp: usize,
    /// Program counter.
    ///
    /// This is maintained as a `usize` for efficiency but only the bottom 16
    /// bits are significant.
    ///
    /// Note that during a `run` this doesn't get updated at every instruction.
    /// It's only guaranteed to get updated before `run` returns.
    pc: usize,
    /// Cycle counter, increments for every 8080 cycle (*not* every emulated
    /// instruction).
    ///
    /// This feature is optional because it hurts performance a bit.
    #[cfg(feature = "count-cycles")]
    pub cycles: usize,
    /// Interrupts enabled?
    interrupts: bool,

    /// CPU flags.
    pub flags: Flags,
    /// Number of 8080 instructions issued.
    ///
    /// Like `pc` this is maintained intermittently during a `run` for
    /// performance reasons.
    pub inst_count: usize,
    /// Memory image.
    ///
    /// This is 64kiB + 1 so that we don't have to check for wrap during
    /// accesses of 16-bit quantities -- we just read "off the end." (The last
    /// byte is maintained in the store operations.)
    ///
    /// Memory is a simple byte array because memory-mapped I/O is pretty rare
    /// in 8080 systems.
    pub mem: [u8; 65536 + 1],
}

/// Emulator reset state.
impl Default for Emu {
    fn default() -> Self {
        Emu {
            registers: Default::default(),
            sp: Default::default(),
            pc: Default::default(),
            #[cfg(feature = "count-cycles")]
            cycles: Default::default(),
            interrupts: Default::default(),
            flags: Default::default(),
            inst_count: Default::default(),

            // This has no Default impl as of rustc 1.48, preventing us from
            // deriving Default for Emu overall
            mem: [0; 0x1_0001],
        }
    }
}

impl Emu {
    /// Accesses the current value of a register.
    #[inline]
    pub fn reg(&self, i: Reg) -> u8 {
        self.registers[i as usize ^ 1]
    }

    /// Accesses either a register or memory through `(HL)`.
    pub fn reg_m(&self, i: RegM) -> u8 {
        match i {
            RegM::M => self.load(self.reg_pair(RegPair::HL)),
            RegM::R(r) => self.reg(r),
        }
    }

    /// Updates a register.
    pub fn set_reg(&mut self, i: Reg, v: u8) {
        self.registers[i as usize ^ 1] = v
    }

    /// Updates either a register or memory through `(HL)`.
    pub fn set_reg_m(&mut self, i: RegM, v: u8) {
        match i {
            RegM::M => self.store(self.reg_pair(RegPair::HL), v),
            RegM::R(r) => self.set_reg(r, v),
        }
    }

    /// Accesses a 16-bit register pair.
    #[inline]
    pub fn reg_pair(&self, i: RegPair) -> u16 {
        match i {
            // SP "shadows" the last two bytes in this context.
            RegPair::SP => self.sp as u16,
            _ => {
                let offset = 2 * (i as usize);
                (u16::from(self.registers[offset + 1]) << 8)
                    | u16::from(self.registers[offset])
            },
        }
    }

    /// Updates a 16-bit register pair.
    pub fn set_reg_pair(&mut self, i: RegPair, v: u16) {
        match i {
            RegPair::SP => self.sp = usize::from(v),
            _ => {
                let offset = 2 * (i as usize);
                self.registers[offset + 1] = (v >> 8) as u8;
                self.registers[offset] = v as u8;
            },
        }
    }

    /// Stores `val` to memory at `addr`.
    pub fn store(&mut self, addr: u16, val: u8) {
        self.mem[usize::from(addr)] = val;
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
        self.mem[usize::from(addr)] = val as u8;
        self.mem[usize::from(addr.wrapping_add(1))] = (val >> 8) as u8;
        // Maintain last byte to make loads cheaper.
        self.mem[65536] = self.mem[0];
    }

    /// Loads a 16-bit word from memory, in little-endian order, at `addr` and
    /// `addr+1`.
    pub fn load16(&self, addr: u16) -> u16 {
        let a = usize::from(addr);
        // Exploit extra byte in memory image to avoid checking for wrap.
        u16::from(self.mem[a])
            | (u16::from(self.mem[a + 1]) << 8)
    }

    /// Effects a jump to `addr` by replacing the PC.
    pub fn jump(&mut self, addr: u16) {
        self.pc = usize::from(addr)
    }

    /// Effects a call to `addr` by pushing the PC and then jumping.
    pub fn call(&mut self, addr: u16) {
        self.push(self.pc as u16);
        self.jump(addr);
    }

    /// Effects a return by popping the PC.
    pub fn ret(&mut self) {
        self.pc = usize::from(self.pop())
    }

    /// Pushes `val` onto the stack.
    pub fn push(&mut self, val: u16) {
        self.sp = self.sp.wrapping_sub(2);
        self.store16(self.sp as u16, val)
    }

    /// Pops a word from the stack.
    pub fn pop(&mut self) -> u16 {
        let v = self.load16(self.sp as u16);
        self.sp = self.sp.wrapping_add(2);
        v
    }

    /// Alters the state of the interrupt flag.
    pub fn set_interrupt_flag(&mut self, f: bool) {
        self.interrupts = f
    }

    /// Returns a slice referencing all of emulated memory.
    pub fn memory(&self) -> &[u8] {
        &self.mem[..]
    }
    
    /// Returns a mutable slice referencing all of emulated memory.
    pub fn memory_mut(&mut self) -> &mut [u8] {
        &mut self.mem[..]
    }

    /// Returns the current PC.
    #[inline]
    pub fn get_pc(&self) -> u16 {
        self.pc as u16
    }
}

/// Trait implemented by things that emulate I/O port subsystems.
pub trait Ports {
    /// Writes `val` to `port`.
    fn write_port(&mut self, port: u8, val: u8);
    /// Reads from `port`.
    fn read_port(&mut self, port: u8) -> u8;
}

/// Handy impl if you don't want to have any useful I/O ports.
impl Ports for () {
    fn write_port(&mut self, _: u8, _: u8) {}
    fn read_port(&mut self, _: u8) -> u8 { 0 }
}

const TRACE: bool = false;

/// Runs the emulation until the machine encounters a `HLT` (or an illegal
/// instruction).
///
/// On halt, returns `(last_pc, pc)`, where `pc` is the address of the `HLT`
/// instruction, and `last_pc` is the address of the instruction executed just
/// before it. (This is often `pc-1`, but may differ if the `HLT` was reached by
/// a call or jump.)
///
/// The PC of `emu` will be set to one past the `HLT` instruction, so the
/// machine can be resumed after the halt easily.
#[inline]
pub fn run(emu: &mut Emu, io: &mut dyn Ports) -> (u16, u16) {
    let mut pc = emu.get_pc() as usize;
    let mut last_pc = 0xFFFF;

    let mut ctx = ops::Ctx { io };

    let mut inst_count = 0;

    let (last_pc, pc) = loop {
        if TRACE {
            eprint!("{:04X}\t", pc);
            crate::dis::disassemble(
                &mut emu.mem[usize::from(pc)..].iter().cloned().map(Ok),
                &mut std::io::stderr(),
            ).unwrap();
            eprintln!();
        }
        let op = emu.mem[pc & 0xFFFF];
        inst_count += 1;
        let (halted, next_pc) = ops::dispatch(emu, &mut ctx, pc, Opcode(op));
        if halted { break (last_pc as u16, pc as u16) }
        last_pc = pc;
        pc = next_pc;
    };

    emu.inst_count = emu.inst_count.wrapping_add(inst_count);
    emu.jump(pc.wrapping_add(1));

    (last_pc, pc)
}

/// Steps the emulation by one instruction.
///
/// On success, returns a flag indicating whether the executed instruction was a
/// `HLT`.
#[inline]
pub fn step(emu: &mut Emu, io: &mut dyn Ports) -> bool {
    let mut ctx = ops::Ctx { io };

    let pc = emu.get_pc() as usize;
    let op = emu.mem[pc & 0xFFFF];
    emu.inst_count += 1;
    let (halted, next_pc) = ops::dispatch(emu, &mut ctx, pc, Opcode(op));
    emu.jump(next_pc as u16);
    halted
}
