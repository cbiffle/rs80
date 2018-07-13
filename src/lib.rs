/// A simple emulator for the 8080 (or, really, a clone thereof -- this is
/// tested against an emulator for the Soviet KR580VM80A).

mod lazy;
use lazy::Lazy;

use std::cell::Cell;
use std::num::Wrapping;
use std::io::{self, Read};
use std::ops::{Deref,DerefMut};

/// Encodings for eight-bit registers in instruction operand positions.
#[derive(Copy, Clone, Debug)]
pub enum Reg8 {
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
    L = 0b101,
    // M = 0b110 but is handled separately (see RegM)
    A = 0b111,
}

impl From<u8> for Reg8 {
    fn from(x: u8) -> Reg8 {
        match x {
            0b000 => Reg8::B,
            0b001 => Reg8::C,
            0b010 => Reg8::D,
            0b011 => Reg8::E,
            0b100 => Reg8::H,
            0b101 => Reg8::L,
            0b110 => panic!("M encoding for Reg8 made it past decoder"),
            0b111 => Reg8::A,
            _ => panic!("Encoding for Reg8 out of range: {}", x),
        }
    }
}

/// Operands that can accomodate either a register or memory without
/// significantly changing the meaning of the instruction.
#[derive(Copy, Clone, Debug)]
pub enum RegM {
    M,
    R(Reg8),
}

impl RegM {
    /// Returns one value or the other, depending on whether `self` is a
    /// register or memory operand. This is useful for changing cycle counts.
    fn reg_or_m<T>(self, r: T, m: T) -> T {
        match self {
            RegM::M => m,
            RegM::R(_) => r,
        }
    }
}

impl From<u8> for RegM {
    fn from(x: u8) -> RegM {
        match x {
            0b110 => RegM::M,
            _ => RegM::R(x.into()),
        }
    }
}

/// A 16-bit register pair operand.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RegPair {
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    SP = 0b11,
}

impl From<u8> for RegPair {
    fn from(x: u8) -> RegPair {
        match x {
            0b00 => RegPair::BC,
            0b01 => RegPair::DE,
            0b10 => RegPair::HL,
            0b11 => RegPair::SP,
            _ => panic!("Encoding for RegPair out of range: {}", x),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CC {
    NZ = 0b000,
    Z  = 0b001,
    NC = 0b010,
    C  = 0b011,
    PO = 0b100,
    PE = 0b101,
    P  = 0b110,
    N  = 0b111,
}

impl From<u8> for CC {
    fn from(x: u8) -> CC {
        match x {
            0b000 => CC::NZ,
            0b001 => CC::Z ,
            0b010 => CC::NC,
            0b011 => CC::C ,
            0b100 => CC::PO,
            0b101 => CC::PE,
            0b110 => CC::P ,
            0b111 => CC::N ,
            _ => panic!("Encoding for CC out of range: {}", x),
        }
    }
}

#[derive(Debug)]
struct Flags {
    zero: bool,
    carry: bool,
    rare: Lazy<RFS, RareFlags>,
}

#[derive(Debug)]
struct RareFlags {
    parity: bool,
    aux: bool,
    sign: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Flags {
            zero: Default::default(),
            carry: Default::default(),

            rare: Default::default(),
        }
    }
}

impl Default for RareFlags {
    fn default() -> Self {
        RareFlags {
            parity: false,
            aux: false,
            sign: false,
        }
    }
}

impl Flags {
    /// Used when pushing PSW.
    fn bits(&self) -> u8 {
        (self.carry as u8)
            | (1u8 << 1)  // UN1 flag always observes as 1
            | ((self.rare.get().parity as u8) << 2)
            | ((self.rare.get().aux as u8) << 4)
            | ((self.zero as u8) << 6)
            | ((self.rare.get().sign as u8) << 7)
    }

    fn from_bits(&mut self, val: u8) {
        self.carry  = (val & (1 << 0)) != 0;
        self.zero   = (val & (1 << 6)) != 0;
        self.rare = Lazy::evaluated(RareFlags {
            parity: (val & (1 << 2)) != 0,
            aux: (val & (1 << 4)) != 0,
            sign: (val & (1 << 7)) != 0,
        });
    }

    fn condition(&self, cc: CC) -> bool {
        match cc {
            CC::NZ => !self.zero,
            CC::Z  =>  self.zero,
            CC::NC => !self.carry,
            CC::C  =>  self.carry,
            CC::PO => !self.rare.get().parity,
            CC::PE =>  self.rare.get().parity,
            CC::P  => !self.rare.get().sign,
            CC::N  =>  self.rare.get().sign,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum RFS {
    Add(u8, u8, u8, u8),
    Inc(u8),
    Dec(u8),
    And(u8, u8, u8),
}

impl lazy::Thunk for RFS {
    type Result = RareFlags;

    fn thunk(self) -> Self::Result {
        match self {
            RFS::Add(a, b, c, sum) => RareFlags {
                sign: sum & 0x80 != 0,
                aux: ((a & 0xF) + (b & 0xF) + c) & 0x10 != 0,
                parity: sum.count_ones() % 2 == 0,
            },
            RFS::Inc(sum) => RareFlags {
                aux: (sum & 0xF) == 0,
                parity: sum.count_ones() % 2 == 0,
                sign: sum & 0x80 != 0,
            },
            RFS::Dec(sum) => RareFlags {
                aux: (sum & 0xF) != 0xF,
                parity: sum.count_ones() % 2 == 0,
                sign: sum & 0x80 != 0,
            },
            RFS::And(a, b, ab) => RareFlags {
                aux: (a & 0x8) | (b & 0x8) != 0,
                sign: ab & 0x80 != 0,
                parity: ab.count_ones() % 2 == 0,
            },
        }
    }
}

type Imm8 = u8;
type Imm16 = u16;
type Addr = u16;
type Restart = u8;
type Port = u8;

#[derive(Copy, Clone, Debug)]
pub enum Insn {
    Mov(Reg8, Reg8),
    MovToM(Reg8),
    MovFromM(Reg8),

    Mvi(Reg8, Imm8),
    MviToM(Imm8),

    Lxi(RegPair, Imm16),

    Lda(Addr),
    Sta(Addr),
    Lhld(Addr),
    Shld(Addr),
    Ldax(RegPair),
    Stax(RegPair),

    Xchg,

    Add(RegM),
    Adi(Imm8),
    Adc(RegM),
    Aci(Imm8),
    Sub(RegM),
    Sui(Imm8),
    Sbb(RegM),
    Sbi(Imm8),
    Inr(RegM),
    Dcr(RegM),
    Inx(RegPair),
    Dcx(RegPair),
    Dad(RegPair),
    
    Daa,

    Ana(RegM),
    Ani(Imm8),
    Ora(RegM),
    Ori(Imm8),
    Xra(RegM),
    Xri(Imm8),
    Cmp(RegM),
    Cpi(Imm8),
    
    Rlc,
    Rrc,
    Ral,
    Rar,
    Cma,
    Cmc,
    Stc,

    Jmp(Addr),
    J(CC, Addr),
    Call(Addr),
    C(CC, Addr),
    Ret,
    R(CC),
    Rst(Restart),

    Pchl,
    Push(RegPair),
    PushPSW,
    Pop(RegPair),
    PopPSW,
    Xthl,
    Sphl,
    In(Port),
    Out(Port),
    Ei,
    Di,
    Hlt,
    Nop,
}

impl Insn {
    #[allow(unused)]
    fn is_valid(self) -> bool {
        match self {
            Insn::Ldax(rp) if rp == RegPair::HL || rp == RegPair::SP => false,
            Insn::Stax(rp) if rp == RegPair::HL || rp == RegPair::SP => false,
            Insn::Rst(r) if r > 7 => false,
            Insn::Push(RegPair::SP) => false,
            Insn::Pop(RegPair::SP) => false,
            _ => true,
        }
    }
}

fn u16_from_slice(slice: &[u8]) -> u16 {
    (slice[0] as u16) | ((slice[1] as u16) << 8)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Opcode(pub u8);

impl Opcode {
    fn bits<T>(self, hi: u32, lo: u32) -> T
        where u8: Into<T>
    {
        debug_assert!(hi >= lo,
                      "backwards bit range {}:{}", hi, lo);
        debug_assert!(hi < 8 && lo < 8,
                      "bits out of range for u8: {}:{}", hi, lo);
        let width = hi - lo + 1;
        ((self.0 >> lo) & ((1 << width) - 1)).into()
    }
}

////////////////////////////////////////////////////////////////////////////////

type W8 = Wrapping<u8>;
type W16 = Wrapping<u16>;

pub struct Emu {
    /// 8-bit registers in encoding order, with placeholder at index 6.
    registers: [W8; 8],
    sp: W16,
    pc: W16,
    pub mem: Vec<u8>,
    halted: bool,
    pub cycles: usize,
    interrupts: bool,

    flags: Flags,
}

impl Default for Emu {
    fn default() -> Self {
        Emu {
            registers: [Wrapping(0); 8],
            sp: Wrapping(0),
            pc: Wrapping(0),
            mem: vec![0; 0x1_0000],
            halted: false,
            cycles: 0,
            interrupts: false,
            flags: Flags::default(),
        }
    }
}

impl Emu {
    fn reg(&self, i: Reg8) -> W8 {
        self.registers[i as usize]
    }

    fn reg_m(&self, i: RegM) -> W8 {
        match i {
            RegM::M => {
                let addr = self.reg_pair(RegPair::HL);
                self.load(addr)
            },
            RegM::R(r) => self.reg(r),
        }
    }

    fn set_reg(&mut self, i: Reg8, v: W8) {
        self.registers[i as usize] = v
    }

    fn set_reg_m(&mut self, i: RegM, v: W8) {
        match i {
            RegM::M => {
                let addr = self.reg_pair(RegPair::HL);
                self.store(addr, v)
            },
            RegM::R(r) => self.set_reg(r, v),
        }
    }
    fn reg_pair(&self, i: RegPair) -> W16 {
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

    fn set_reg_pair(&mut self, i: RegPair, v: W16) {
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

    fn advance(&mut self, n: usize) {
        self.cycles += n
    }

    fn store(&mut self, addr: W16, val: W8) {
        self.mem[addr.0 as usize] = val.0
    }

    fn load(&self, addr: W16) -> W8 {
        Wrapping(self.mem[addr.0 as usize])
    }

    fn store16(&mut self, addr: W16, val: W16) {
        self.mem[addr.0 as usize] = val.0 as u8;
        self.mem[addr.0 as usize + 1] = (val.0 >> 8) as u8;
    }

    fn load16(&self, addr: W16) -> W16 {
        Wrapping((self.mem[addr.0 as usize] as u16)
                 | ((self.mem[(addr + Wrapping(1)).0 as usize] as u16)
                        << 8))
    }

    fn take_imm8(&mut self) -> W8 {
        let v = self.load(self.pc);
        self.pc += Wrapping(1);
        v
    }

    fn take_imm16(&mut self) -> W16 {
        let v = self.load16(self.pc);
        self.pc += Wrapping(2);
        v
    }

    fn halt(&mut self) {
        self.halted = true
    }

    fn call(&mut self, addr: W16) {
        let pc = self.pc;
        self.push(pc);
        self.pc = addr
    }

    fn ret(&mut self) {
        self.pc = self.pop()
    }

    fn push(&mut self, val: W16) {
        self.sp -= Wrapping(2);
        let sp = self.sp;
        self.store16(sp, val)
    }

    fn pop(&mut self) -> W16 {
        let v = self.load16(self.sp);
        self.sp += Wrapping(2);
        v
    }
}

fn add(a: W8, b: W8, c: u8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 + b.0 as u16 + c as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = sum16 & 0x100 != 0;
    flags.rare = Lazy::suspended(RFS::Add(a.0, b.0, c, sum8));

    Wrapping(sum8)
}

fn inc(a: W8, flags: &mut Flags) -> W8 {
    let sum = a + Wrapping(1);
   
    flags.zero = sum.0 == 0;
    flags.rare = Lazy::suspended(RFS::Inc(sum.0));

    sum
}

fn dec(a: W8, flags: &mut Flags) -> W8 {
    let sum = a - Wrapping(1);
   
    flags.zero = sum.0 == 0;
    flags.rare = Lazy::suspended(RFS::Dec(sum.0));

    sum
}

fn and(a: W8, b: W8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 & b.0 as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.rare = Lazy::suspended(RFS::And(a.0, b.0, sum8));
    flags.carry = false;

    Wrapping(sum8)
}

fn or(a: W8, b: W8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 | b.0 as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = false;
    flags.rare = Lazy::suspended(RFS::And(0, 0, sum8));

    Wrapping(sum8)
}

fn xor(a: W8, b: W8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 ^ b.0 as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = false;
    flags.rare= Lazy::suspended(RFS::And(0, 0, sum8));

    Wrapping(sum8)
}

fn sub(a: W8, b: W8, c: u8, flags: &mut Flags) -> W8 {
    let sum16 = (Wrapping(a.0 as u16) - Wrapping(b.0 as u16) - Wrapping(c as u16)).0;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = sum16 & 0x100 != 0;
    flags.rare= Lazy::suspended(RFS::Add(a.0, !b.0, 1 - c, sum8));

    Wrapping(sum8)
}

fn compare(a: W8, b: W8, flags: &mut Flags) {
    sub(a, b, 0, flags);
}

type IsaDef = (&'static [u8],
               fn(Opcode, &[u8]) -> Insn,
               fn(Opcode, &mut Emu));
static ISA_DEFS: &[IsaDef] = &[
    (b"01110110",
     |_, _| Insn::Hlt,
     |_, st| {
         st.halt();
         st.advance(4);  // Seriously.
     }
    ),
    (b"01110sss",
     |opcode, _| Insn::MovToM(opcode.bits(2,0)),
     |opcode, st| {
         let v = st.reg(opcode.bits(2,0));
         let addr = st.reg_pair(RegPair::HL);
         st.store(addr, v);
         st.advance(7)
     }
    ),
    (b"01ddd110",
     |opcode, _| Insn::MovFromM(opcode.bits(5,3)),
     |opcode, st| {
         let addr = st.reg_pair(RegPair::HL);
         let v = st.load(addr);
         st.set_reg(opcode.bits(5,3), v);
         st.advance(7)
     }
    ),
    (b"01dddsss",
     |opcode, _| Insn::Mov(opcode.bits(5,3), opcode.bits(2,0)),
     |opcode, st| {
         let v = st.reg(opcode.bits(2,0));
         st.set_reg(opcode.bits(5,3), v);
         st.advance(5)
     }
    ),
    (b"00110110",
     |_, b| Insn::MviToM(b[0]),
     |_, st| {
         let v = st.take_imm8();
         let addr = st.reg_pair(RegPair::HL);
         st.store(addr, v);
         st.advance(10)
     }
    ),
    (b"00ddd110",
     |opcode, b| Insn::Mvi(opcode.bits(5,3), b[0]),
     |opcode, st| {
         let v = st.take_imm8();
         st.set_reg(opcode.bits(5,3), v);
         st.advance(10)
     }
    ),
    (b"00pp0001",
     |opcode, b| Insn::Lxi(opcode.bits(5,4), u16_from_slice(b)),
     |opcode, st| {
         let v = st.take_imm16();
         st.set_reg_pair(opcode.bits(5,4), v);
         st.advance(10)
     }
    ),
    (b"00101010",
     |_, b| Insn::Lhld(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.load16(addr);
         st.set_reg_pair(RegPair::HL, v);
         st.advance(16)
     }
    ),
    (b"00100010",
     |_, b| Insn::Shld(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.reg_pair(RegPair::HL);
         st.store16(addr, v);
         st.advance(16)
     }
    ),
    (b"00111010",
     |_, b| Insn::Lda(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.load(addr);
         st.set_reg(Reg8::A, v);
         st.advance(13)
     }
    ),
    (b"00pp1010",
     |opcode, _| Insn::Ldax(opcode.bits(5,4)),
     |opcode, st| {
         let addr = st.reg_pair(opcode.bits(5,4));
         let v = st.load(addr);
         st.set_reg(Reg8::A, v);
         st.advance(7)
     }
    ),
    (b"00110010",
     |_, b| Insn::Sta(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.reg(Reg8::A);
         st.store(addr, v);
         st.advance(13)
     }
    ),
    (b"00pp0010",
     |opcode, _| Insn::Stax(opcode.bits(5,4)),
     |opcode, st| {
         let addr = st.reg_pair(opcode.bits(5,4));
         let v = st.reg(Reg8::A);
         st.store(addr, v);
         st.advance(7)
     }
    ),
    (b"11101011",
     |_, _| Insn::Xchg,
     |_, st| {
         let de = st.reg_pair(RegPair::DE);
         let hl = st.reg_pair(RegPair::HL);
         st.set_reg_pair(RegPair::DE, hl);
         st.set_reg_pair(RegPair::HL, de);
         st.advance(4)
     }
    ),
    (b"11nnn111",
     |opcode, _| Insn::Rst(opcode.bits(5,3)),
     |opcode, st| {
         let addr = Wrapping(opcode.bits::<u16>(5,3) * 8);
         st.call(addr);
         st.advance(11)
     }
    ),
    (b"11001101",
     |_, b| Insn::Call(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         st.call(addr);
         st.advance(17)
     }
    ),
    (b"11ccc100",
     |opcode, b| Insn::C(opcode.bits(5,3), u16_from_slice(b)),
     |opcode, st| {
         let addr = st.take_imm16();
         if st.flags.condition(opcode.bits(5,3)) {
             st.call(addr);
             st.advance(17)
         } else {
             st.advance(11)
         }
     }
    ),
    (b"11001001",
     |_, _| Insn::Ret,
     |_, st| {
         st.ret();
         st.advance(10)
     }
    ),
    (b"11ccc000",
     |opcode, _| Insn::R(opcode.bits(5,3)),
     |opcode, st| {
         if st.flags.condition(opcode.bits(5,3)) {
             st.ret();
             st.advance(11)
         } else {
             st.advance(5)
         }
     }
    ),
    (b"11110101",
     |_, _| Insn::PushPSW,
     |_, st| {
         let psw = ((st.reg(Reg8::A).0 as u16) << 8)
             | (st.flags.bits() as u16);
         st.push(Wrapping(psw));
         st.advance(11)
     }
    ),
    (b"11pp0101",
     |opcode, _| Insn::Push(opcode.bits(5,4)),
     |opcode, st| {
         let v = st.reg_pair(opcode.bits(5,4));
         st.push(v);
         st.advance(11)
     }
    ),

    (b"11110001",
     |_, _| Insn::PopPSW,
     |_, st| {
         let psw = st.pop().0;
         st.set_reg(Reg8::A, Wrapping((psw >> 8) as u8));
         st.flags.from_bits(psw as u8);
         st.advance(10)
     }
    ),
    (b"11pp0001",
     |opcode, _| Insn::Pop(opcode.bits(5,4)),
     |opcode, st| {
         let v = st.pop();
         st.set_reg_pair(opcode.bits(5,4), v);
         st.advance(10)
     }
    ),
    (b"11000011",
     |_, b| Insn::Jmp(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         st.pc = addr;
         st.advance(10)
     }
    ),
    (b"11ccc010",
     |opcode, b| Insn::J(opcode.bits(5,3), u16_from_slice(b)),
     |opcode, st| {
         let addr = st.take_imm16();
         if st.flags.condition(opcode.bits(5,3)) {
             st.pc = addr;
         }
         st.advance(10)
     }
    ),

    (b"00pp1001",
     |opcode, _| Insn::Dad(opcode.bits(5,4)),
     |opcode, st| {
         let rp = opcode.bits(5,4);
         let a = st.reg_pair(rp);
         let r32 = a.0 as u32 + st.reg_pair(RegPair::HL).0 as u32;
         st.set_reg_pair(RegPair::HL, Wrapping(r32 as u16));
         
         st.flags.carry = r32 & 0x10000 != 0;
         st.advance(10)
     },
    ),
    (b"10000sss",
     |opcode, _| Insn::Add(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let a = add(a, b, 0, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4,7))
     },
    ),
    (b"10001sss",
     |opcode, _| Insn::Adc(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let c = st.flags.carry as u8;
         let a = add(a, b, c, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"10010sss",
     |opcode, _| Insn::Sub(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let a = sub(a, b, 0, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"10011sss",
     |opcode, _| Insn::Sbb(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let c = st.flags.carry as u8;
         let a = sub(a, b, c, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11000110",
     |_, b| Insn::Adi(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let a = add(a, b, 0, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"11001110",
     |_, b| Insn::Aci(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let c = st.flags.carry as u8;
         let a = add(a, b, c, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"11010110",
     |_, b| Insn::Sui(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let a = sub(a, b, 0, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"11011110",
     |_, b| Insn::Sbi(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let c = st.flags.carry as u8;
         let a = sub(a, b, c, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"10100sss",
     |opcode, _| Insn::Ana(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let a = and(a, b, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11100110",
     |_, b| Insn::Ani(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let a = and(a, b, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"10110sss",
     |opcode, _| Insn::Ora(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let a = or(a, b, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11110110",
     |_, b| Insn::Ori(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let a = or(a, b, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"10101sss",
     |opcode, _| Insn::Xra(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         let a = xor(a, b, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11101110",
     |_, b| Insn::Xri(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         let a = xor(a, b, &mut st.flags);
         st.set_reg(Reg8::A, a);
         st.advance(7)
     },
    ),
    (b"10111sss",
     |opcode, _| Insn::Cmp(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg8::A);
         let b = st.reg_m(rm);
         compare(a, b, &mut st.flags);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11111110",
     |_, b| Insn::Cpi(b[0]),
     |_, st| {
         let a = st.reg(Reg8::A);
         let b = st.take_imm8();
         compare(a, b, &mut st.flags);
         st.advance(7)
     },
    ),
    (b"00ddd100",
     |opcode, _| Insn::Inr(opcode.bits(5,3)),
     |opcode, st| {
        let rm = opcode.bits(5,3);
        let a = st.reg_m(rm);
        let r = inc(a, &mut st.flags);
        st.set_reg_m(rm, r);
        st.advance(rm.reg_or_m(5, 10))
     },
    ),
    (b"00ddd101",
     |opcode, _| Insn::Dcr(opcode.bits(5,3)),
     |opcode, st| {
        let rm = opcode.bits(5,3);
        let a = st.reg_m(rm);
        let r = dec(a, &mut st.flags);
        st.set_reg_m(rm, r);
        st.advance(rm.reg_or_m(5, 10))
     },
    ),
    (b"00pp0011",
     |opcode, _| Insn::Inx(opcode.bits(5,4)),
     |opcode, st| {
        let rp = opcode.bits(5,4);
        let a = st.reg_pair(rp);
        st.set_reg_pair(rp, a + Wrapping(1));
        st.advance(5)
     },
    ),
    (b"00pp1011",
     |opcode, _| Insn::Dcx(opcode.bits(5,4)),
     |opcode, st| {
        let rp = opcode.bits(5,4);
        let a = st.reg_pair(rp);
        st.set_reg_pair(rp, a - Wrapping(1));
        st.advance(5)
     },
    ),
    (b"00110111",
     |_, _| Insn::Stc,
     |_, st| {
         st.flags.carry = true;
         st.advance(4)
     },
    ),
    (b"00101111",
     |_, _| Insn::Cma,
     |_, st| {
         let a = st.reg(Reg8::A);
         st.set_reg(Reg8::A, !a);
         st.advance(4)
     },
    ),
    (b"00111111",
     |_, _| Insn::Cmc,
     |_, st| {
         let c = st.flags.carry;
         st.flags.carry = !c;
         st.advance(4)
     },
    ),
    (b"00000111",
     |_, _| Insn::Rlc,
     |_, st| {
         let a = st.reg(Reg8::A);
         let r = (a << 1) | (a >> 7);

         st.flags.carry = a.0 & 0x80 != 0;

         st.set_reg(Reg8::A, r);
         st.advance(4)
     },
    ),
    (b"00010111",
     |_, _| Insn::Ral,
     |_, st| {
         let a = st.reg(Reg8::A);
         let r = (a << 1) | Wrapping(st.flags.carry as u8);

         st.flags.carry = a.0 & 0x80 != 0;

         st.set_reg(Reg8::A, r);
         st.advance(4)
     },
    ),
    (b"00001111",
     |_, _| Insn::Rrc,
     |_, st| {
         let a = st.reg(Reg8::A);
         let r = (a >> 1) | (a << 7);

         st.flags.carry = a.0 & 1 != 0;

         st.set_reg(Reg8::A, r);
         st.advance(4)
     },
    ),
    (b"00011111",
     |_, _| Insn::Rar,
     |_, st| {
         let a = st.reg(Reg8::A);
         let r = (a >> 1) | Wrapping((st.flags.carry as u8) << 7);

         st.flags.carry = a.0 & 1 != 0;

         st.set_reg(Reg8::A, r);
         st.advance(4)
     },
    ),
    (b"00100111",
     |_, _| Insn::Daa,
     |_, st| {
         // back up carry flag.
         let mut carry = st.flags.carry;
         let a = st.reg(Reg8::A);
         let mut b = 0;
         if st.flags.rare.get().aux || (a.0 & 0xF) > 9 {
             b = 6;
         }
         if st.flags.carry ||
             (a.0 >> 4) > 9 ||
             ((a.0 >> 4) == 9 && (a.0 & 0xF) > 9) {
            b |= 0x60;
            carry |= true;
         }
         let r = add(a, Wrapping(b), 0, &mut st.flags);
         st.set_reg(Reg8::A, r);
         st.flags.carry |= carry;
         st.advance(4)
     },
    ),
    (b"11100011",
     |_, _| Insn::Xthl,
     |_, st| {
         let sp = st.sp;
         let s = st.load16(sp);
         let hl = st.reg_pair(RegPair::HL);
         st.store16(sp, hl);
         st.set_reg_pair(RegPair::HL, s);
         st.advance(18)
     },
    ),
    (b"11111001",
     |_, _| Insn::Sphl,
     |_, st| {
         let hl = st.reg_pair(RegPair::HL);
         st.sp = hl;
         st.advance(5)
     },
    ),
    (b"11101001",
     |_, _| Insn::Pchl,
     |_, st| {
         let hl = st.reg_pair(RegPair::HL);
         st.pc = hl;
         st.advance(5)
     },
    ),
    (b"11111011",
     |_, _| Insn::Ei,
     |_, st| {
         st.interrupts = true;
         st.advance(4)
     },
    ),
    (b"11110011",
     |_, _| Insn::Di,
     |_, st| {
         st.interrupts = false;
         st.advance(4)
     },
    ),
    (b"00___000",
     |_, _| Insn::Nop,
     |_, st| {
         st.advance(4)
     },
    ),

];

////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct Pat(u8, u8);

impl Pat {
    fn parse(text: &'static [u8]) -> Pat {
        debug_assert!(text.len() == 8, "Pattern has wrong length: {:?}", text);

        let mut mask = 0;
        let mut bits = 0;
        for i in 0..8 {
            if text[i] == b'0' || text[i] == b'1' {
                mask |= 1 << (7 - i);
            }
            if text[i] == b'1' {
                bits |= 1 << (7 - i);
            }
        }

        Pat(mask, bits)
    }

    fn matches(&self, bits: u8) -> bool {
        bits & self.0 == self.1
    }
}

/// Order patterns by specificity, from most 1s in mask to fewest.
impl std::cmp::Ord for Pat {
    fn cmp(&self, other: &Pat) -> std::cmp::Ordering {
        other.0.count_ones().cmp(&self.0.count_ones())
            .then(self.0.cmp(&other.0))
            .then(self.1.cmp(&other.1))
    }
}

impl std::cmp::PartialOrd for Pat {
    fn partial_cmp(&self, other: &Pat) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub fn make_dispatch_table() -> Vec<Option<fn(Opcode, &mut Emu)>> {
    let mut defs: Vec<_> = ISA_DEFS.iter()
        .map(|(pat, _, op)| (Pat::parse(pat), op))
        .collect();
    defs.sort_unstable_by_key(|&(p, _)| p);
    let defs = defs;

    let mut table = Vec::new();

    for opc in 0..256u32 {
        let opc = opc as u8;
        table.push(None);

        for &(pat, &op) in &defs {
            if pat.matches(opc) {
                *table.last_mut().unwrap() = Some(op);
                break;
            }
        }
    }

    table
}

pub fn make_decode_table() -> Vec<Option<fn(Opcode, &[u8]) -> Insn>> {
    let mut defs: Vec<_> = ISA_DEFS.iter()
        .map(|(pat, dec, _)| (Pat::parse(pat), dec))
        .collect();
    defs.sort_unstable_by_key(|&(p, _)| p);
    let defs = defs;

    let mut table = Vec::new();

    for opc in 0..=255 {
        table.push(None);

        for &(pat, &op) in &defs {
            if pat.matches(opc) {
                *table.last_mut().unwrap() = Some(op);
                break;
            }
        }
    }

    table
}

const TRACE: bool = false;

pub fn load_image(path: String, emu: &mut Emu) -> io::Result<()> {
    let mut file = std::fs::File::open(path)?;
    file.read(&mut emu.mem[0x100..])?;
    Ok(())
}

#[derive(Debug)]
pub enum RunError {
    UnhandledBdosCall(u8, u16),
    BadJump(u16, u16),
    UnimplementedInstruction(u8, u16),
    Halted(u16),
    Out(io::Error),
}

pub fn run<W>(emu: &mut Emu,
              table: &[Option<fn(Opcode, &mut Emu)>],
              mut out: W)
    -> Result<u16, RunError>
    where W: io::Write
{
    // Start at 0x100 like CP/M.
    emu.pc = Wrapping(0x100);
    emu.sp = Wrapping(0);
    emu.halted = false;
    // Place a RET to handle CP/M restarts.
    emu.mem[5] = 0xC9;

    let mut last_pc = 0;

    while !emu.halted {
        if emu.pc.0 < 0x100 {
            match emu.pc.0 {
                5 => {
                    // CP/M syscall restart address
                    match emu.reg(Reg8::C).0 {
                        9 => {
                            // Type string
                            let mut addr = emu.reg_pair(RegPair::DE);
                            loop {
                                let c = emu.load(addr).0;
                                if c == b'$' { break }
                                out.write(&[c])
                                    .map_err(RunError::Out)?;
                                addr += Wrapping(1);
                            }
                        },
                        2 => {
                            let c = emu.reg(Reg8::E).0;
                            out.write(&[c])
                                    .map_err(RunError::Out)?;
                        },
                        _ => return Err(RunError::UnhandledBdosCall(
                                emu.reg(Reg8::C).0, last_pc)),
                    }
                },
                0 => {
                    // Jump to exit vector
                    return Ok(last_pc)
                },
                _ => return Err(RunError::BadJump(emu.pc.0, last_pc)),
            }
        }

        last_pc = emu.pc.0;

        let op = emu.take_imm8().0;
        match table[op as usize] {
            None => return Err(RunError::UnimplementedInstruction(
                    op, last_pc)),
            Some(f) => f(Opcode(op), emu),
        }
    }

    Err(RunError::Halted(last_pc))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn check_isa_def_uniqueness() {
        let mut seen: HashMap<(u8, u8), Vec<_>> = HashMap::new();

        for (pat_str, dec, _) in ISA_DEFS.iter() {
            let pat = Pat::parse(pat_str);
            seen.entry((pat.0, pat.1))
                .and_modify(|v| v.push((pat_str, dec)))
                .or_insert_with(|| vec![(pat_str, dec)]);
        }

        let mut dups: Vec<Vec<_>> = Vec::new();
        for (key, pats) in seen {
            if pats.len() > 1 {
                dups.push(pats.into_iter().map(|(p, dec)| {
                    let insn = dec(Opcode(key.1), &[0, 0]);
                    let txt = String::from_utf8_lossy(p);
                    (txt, insn)
                }).collect());
            }
        }

        if !dups.is_empty() {
            panic!("Duplicate pattern groups: {:#?}", dups)
        }
    }
}
