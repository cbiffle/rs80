//! This module consumes the data structures produced by the parser and
//! generates implementations of each instruction, specialized for each
//! of 256 opcodes. Moving the decoding of e.g. bit field operands to
//! codegen time improves performance by 16%.

use std::io;
use std::collections::HashMap;
use super::*;

use rs80_common::isa::CC;
use rs80_common::insn_info::IType;

fn find_def(defs: &[Def], op: u8) -> (&Def, HashMap<char, u8>) {
    // Scan the defs vec until we find a matching one.
    for def in defs {
        if let Some(fields) = def.bits.matches(op) {
            let fields: HashMap<_, _> = fields.into_iter()
                .collect();
            return (def, fields)
        }
    }
    panic!("No instruction matches opcode {:02X}", op)
}

/// Generates a comment citing `def` for debugging purposes.
fn comment(def: &Def, fields: &HashMap<char, u8>, out: &mut impl io::Write)
    -> io::Result<()>
{
    write!(out, "// {}", def.mnem.0)?;
    for (c, v) in fields {
        write!(out, " {}={}", c, v)?;
    }
    writeln!(out)
}

fn format_operand(op: Option<&AOperand>,
                  fields: &HashMap<char, u8>) -> String {
    match op {
        None => "None".into(),
        Some(Operand::PSW) => "Some(Operand::PSW)".into(),
        Some(Operand::F(c, f)) =>
            format!("Some(Operand::F({:?}, (FType::{:?}, {})))",
            c, f, fields[c]),
            Some(Operand::I(c, f)) =>
                format!("Some(Operand::I({:?}, IType::{:?}))", c, f),
    }
}

pub fn disassemble(defs: &[Def], out: &mut impl io::Write)
    -> io::Result<()>
{
    writeln!(out, "[")?;
    for op in 0..=255 {
        let (def, fields) = find_def(&defs, op);
        comment(&def, &fields, out)?;

        let cc = match def.mnem.1 {
            Some(n) =>
                format!("Some(CC::{:?})", CC::from(fields[&n])),
            None => "None".into(),
        };

        writeln!(out, "InsnInfo {{")?;
        writeln!(out, "  mnemonic: Mnemonic {{")?;
        writeln!(out, "    label: \"{}\",", def.mnem.0)?;
        writeln!(out, "    cc: {},", cc)?;
        writeln!(out, "  }},")?;
        writeln!(out, "  a: {},",
                 format_operand(def.operands.get(0), &fields))?;
        writeln!(out, "  b: {},",
                 format_operand(def.operands.get(1), &fields))?;
        writeln!(out, "}},")?;
    }
    writeln!(out, "]")?;

    Ok(())
}

/// Generates the `dispatch` entry point from `defs` to `out`.
pub fn dispatch(defs: &[Def], out: &mut impl io::Write)
    -> io::Result<()>
{

    // First we'll generate an entry point for each opcode. For a hex
    // opcode 'xx' the entry point is named 'opcode_xx'. Doing this improves
    // performance by 10-50% compared to a giant 'match' (and makes profiles
    // *significantly* easier to read).
    for op in 0..=255 {
        let (def, fields) = find_def(&defs, op);

        comment(&def, &fields, out)?;

        // Opcode entry point. We allow(unused) because not every
        // opcode uses the parameters to its function; we know all
        // opcode entry points are used because we use them below.
        // (We can't allow(unused) an argument in current Rust.)
        writeln!(out, "#[allow(unused)]")?;
        writeln!(out, "#[allow(clippy::erasing_op, clippy::identity_op)]")?;
        writeln!(out, "fn opcode_{:02x}(st: &mut Emu, \
                                        ctx: &mut Ctx,
                                        pc: usize) -> (bool, usize) {{", op)?;
        // Standard bindings
        writeln!(out, "    let opcode = Opcode({});", op)?;

        // Bind condition success field if present.
        let mut has_condition = None;
        if let Some(c) = def.mnem.1 {
            condition_into_scope(c, fields[&c], out)?;
            has_condition = Some(c);
        }

        // Bind any named fields.
        let mut insn_len = 1;
        let mut has_m_ref = false;
        for op in &def.operands {
            match op {
                Operand::F(c, o) => {
                    field_into_scope(*c, o, fields[&c], out)?;
                    if *o == FType::RM && fields[&c] == 0b110 {
                        has_m_ref = true;
                    }
                },
                Operand::I(c, o) => {
                    write!(out, "      ")?; // common indent
                    match o {
                        IType::I8 => {
                            writeln!(out, "let {} = st.load(pc.wrapping_add(1) as u16);", c)?;
                        }
                        IType::I16 | IType::Address => {
                            writeln!(out, "let {} = st.load16(pc.wrapping_add(1) as u16);", c)?;
                        }
                    }
                    insn_len += o.length();
                },
                _ => (),  // ignore the literals
            }
        }

        writeln!(out, "    let mut next_pc = pc.wrapping_add({});", insn_len)?;
        writeln!(out, "    let mut halted = false;")?;

        writeln!(out, "    #[cfg(feature = \"count-cycles\")] {{")?;

        let cycles_lo = def.cycles.0;
        if let Some(cycles_hi) = def.cycles.1 {
            if let Some(c) = has_condition {
                writeln!(out,
                    "        let cycles = if {} {{ {} }} else {{ {} }};",
                    c, cycles_hi, cycles_lo)?;
            } else if has_m_ref {
                writeln!(out, "        let cycles = {};", cycles_hi)?;
            } else {
                writeln!(out, "        let cycles = {};", cycles_lo)?;
            }
        } else {
            writeln!(out, "        let cycles = {};", cycles_lo)?;
        }

        writeln!(out, "        st.cycles += cycles;")?;

        writeln!(out, "    }}")?; // end count-cycles block

        // Output the Rust code.
        for line in &def.body {
            writeln!(out, "    {}", line)?;
        }
        writeln!(out, "    (halted, next_pc)")?;
        writeln!(out, "}}")?;
    }

    // Now, generate a facade that dispatches between the opcode entry
    // points.
    writeln!(out, "#[inline]")?;
    writeln!(out, "pub fn dispatch(st: &mut Emu, \
                                   ctx: &mut Ctx, \
                                   pc: usize, \
                                   opcode: Opcode) -> (bool, usize) {{")?;
    // The dispatch table itself:
    writeln!(out, "  static TABLE: [fn(&mut Emu, &mut Ctx, usize) -> (bool, usize); 256] = [")?;
    for op in 0..=255 {
        writeln!(out, "    opcode_{:02x},", op)?;
    }
    writeln!(out, "  ];")?;

    // The actual code is quite simple: dispatch through the table.
    writeln!(out, "  TABLE[opcode.0 as usize](st, ctx, pc)")?;
    writeln!(out, "}}")
}

fn condition_into_scope(n: char, bits: u8, out: &mut impl io::Write)
    -> io::Result<()>
{
    writeln!(out, "      let {} = st.flags.condition(CC::from({}u8));",
    n, bits)
}

fn field_into_scope(n: char, o: &FType, bits: u8,
                    out: &mut impl io::Write)
    -> io::Result<()>
{
    write!(out, "      ")?; // common indent
    match o {
        FType::RM =>
            writeln!(out, "let {} = RegM::from({}u8);", n, bits),
        FType::RP =>
            writeln!(out, "let {} = RegPair::from({}u8);", n, bits),
        FType::C3 =>
            writeln!(out, "let {} = {}u16 * 8;", n, bits),
    }
}

pub fn write_flag_accel(out: &mut impl io::Write) -> io::Result<()> {
    // Zero, Parity, Sign can be accelerated using a common table.
    writeln!(out, "static ZPS_ACCEL: [u8; 256] = [")?;
    for i in 0..=255u8 {
        let zero = i == 0;
        let sign = i > 127;
        let parity = i.count_ones() % 2 == 0;

        let entry = u8::from(parity) << 2
            | u8::from(zero) << 6
            | u8::from(sign) << 7;

        writeln!(out, "    0b{:08b},", entry)?;
    }
    writeln!(out, "];")?;

    writeln!(out, "#[inline]")?;
    writeln!(out, "pub fn calculate_zps(byte: u8) -> u8 {{")?;
    writeln!(out, "    ZPS_ACCEL[byte as usize]")?;
    writeln!(out, "}}")?;

    Ok(())
}

