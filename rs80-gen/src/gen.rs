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
                                        ctx: &mut Ctx) -> bool {{", op)?;
        // Standard bindings
        writeln!(out, "    let opcode = Opcode({});", op)?;

        // Bind condition success field if present.
        if let Some(c) = def.mnem.1 {
            condition_into_scope(c, fields[&c], out)?
        }

        // Bind any named fields.
        for op in &def.operands {
            match op {
                Operand::F(c, o) => {
                    field_into_scope(*c, o, fields[&c], out)?
                },
                Operand::I(c, o) => {
                    inline_into_scope(*c, o, out)?
                },
                _ => (),  // ignore the literals
            }
        }

        // Output the Rust code.
        for line in &def.body {
            writeln!(out, "    {}", line)?;
        }
        // Tack on a 'false' meaning 'we have not halted'. The
        // only exception to this is HLT, which contains an explicit
        // early return. Rustc does not currently complain about
        // unreachable code past an early return.
        writeln!(out, "    false")?;
        writeln!(out, "}}")?;
    }

    // Now, generate a facade that dispatches between the opcode entry
    // points.
    writeln!(out, "#[inline]")?;
    writeln!(out, "pub fn dispatch(st: &mut Emu, \
                                   ctx: &mut Ctx, \
                                   opcode: Opcode) -> bool {{")?;
    // The dispatch table itself:
    writeln!(out, "  static TABLE: [fn(&mut Emu, &mut Ctx) -> bool; 256] = [")?;
    for op in 0..=255 {
        writeln!(out, "    opcode_{:02x},", op)?;
    }
    writeln!(out, "  ];")?;

    // The actual code is quite simple: dispatch through the table.
    writeln!(out, "  TABLE[opcode.0 as usize](st, ctx)")?;
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

fn inline_into_scope(n: char, o: &IType, out: &mut impl io::Write)
    -> io::Result<()>
{
    write!(out, "      ")?; // common indent
    match o {
        IType::I8 =>
            writeln!(out, "let {} = st.take_imm8();", n),
            IType::I16 | IType::Address =>
                writeln!(out, "let {} = st.take_imm16();", n),
    }
}

fn table_immediate_into_scope(n: char, o: &IType, out: &mut impl io::Write)
    -> io::Result<()>
{
    write!(out, "      ")?; // common indent
    match o {
        IType::I8 =>
            writeln!(out, "let {} = *table_immediate as u8;", n),
        IType::I16 | IType::Address =>
            writeln!(out, "let {} = *table_immediate as u16;", n),
    }
}

/// Generates the predecode-related stuff from `defs` to `out`.
pub fn predecode(defs: &[Def], out: &mut impl io::Write)
    -> io::Result<()>
{
    writeln!(out, "pub type OpFn = fn(&mut Emu, &mut Ctx, &usize) -> bool;")?;

    // Generate an evaluation function for each possible opcode, like in the
    // dispatch table approach. Signatures are slightly different.
    for op in 0..=255 {
        let (def, fields) = find_def(&defs, op);

        comment(&def, &fields, out)?;

        // Opcode entry point. We allow(unused) because not every
        // opcode uses the parameters to its function; we know all
        // opcode entry points are used because we use them below.
        // (We can't allow(unused) an argument in current Rust.)
        writeln!(out, "#[allow(unused)]")?;
        writeln!(out, "#[allow(clippy::erasing_op, clippy::identity_op)]")?;
        writeln!(out, "fn thread_{:02x}(st: &mut Emu, \
                                        ctx: &mut Ctx, \
                                        table_immediate: &usize) -> bool {{", op)?;
        // Standard bindings
        writeln!(out, "    let opcode = Opcode({});", op)?;

        // Bind condition success field if present.
        if let Some(c) = def.mnem.1 {
            condition_into_scope(c, fields[&c], out)?
        }

        let mut insn_len = 1;

        // Bind any named fields.
        for op in &def.operands {
            match op {
                Operand::F(c, o) => {
                    field_into_scope(*c, o, fields[&c], out)?
                },
                Operand::I(c, o) => {
                    table_immediate_into_scope(*c, o, out)?;

                    match o {
                        IType::I8 => insn_len += 1,
                        IType::I16 | IType::Address => insn_len += 2,
                    }
                },
                _ => (),  // ignore the literals
            }
        }

        writeln!(out, "    st.skip({});", insn_len)?;

        // Output the Rust code.
        for line in &def.body {
            writeln!(out, "    {}", line)?;
        }
        // Tack on a 'false' meaning 'we have not halted'. The
        // only exception to this is HLT, which contains an explicit
        // early return. Rustc does not currently complain about
        // unreachable code past an early return.
        writeln!(out, "    false")?;
        writeln!(out, "}}")?;
    }

    // Now, output a _decoder_ function for each opcode that produces the
    // appropriate tuple.
    for op in 0..=255 {
        let (def, fields) = find_def(&defs, op);

        comment(&def, &fields, out)?;

        // Opcode entry point. We allow(unused) because not every
        // opcode uses the parameters to its function; we know all
        // opcode entry points are used because we use them below.
        // (We can't allow(unused) an argument in current Rust.)
        writeln!(out, "#[allow(unused)]")?;
        writeln!(out, "#[allow(clippy::erasing_op, clippy::identity_op)]")?;
        writeln!(out, "fn decode_{:02x}(addr: usize, \
                                        mem: &[u8]) -> (OpFn, usize) {{", op)?;

        let mut immediate_present = false;
        for op in &def.operands {
            match op {
                Operand::I(_, o) => {
                    immediate_present = true;
                    match o {
                        IType::I8 => writeln!(
                            out,
                            "let immediate = usize::from(mem[addr + 1]);",
                        )?,
                        IType::I16 | IType::Address => writeln!(
                            out,
                            "let immediate = usize::from(mem[addr + 1]) | usize::from(mem[addr + 2]) << 8;",
                        )?,
                    }
                },
                _ => (),  // ignore the literals
            }
        }

        if !immediate_present {
            writeln!(out, "    let immediate = 0;")?;
        }

        writeln!(out, "    (thread_{:02x}, immediate)", op)?;
        writeln!(out, "}}")?;
    }

    // Now, generate a facade that dispatches between the opcode entry
    // points.
    writeln!(out, "pub fn predecode(mem: &[u8]) -> Vec<(OpFn, usize)> {{")?;
    // Decode table
    writeln!(out, "  static TABLE: [fn(usize, &[u8]) -> (OpFn, usize); 256] = [")?;
    for op in 0..=255 {
        writeln!(out, "    decode_{:02x},", op)?;
    }
    writeln!(out, "  ];")?;

    writeln!(out, "  mem.iter().enumerate() \
                         .map(|(i, &b)| TABLE[usize::from(b)](i, mem))
                         .collect()")?;
    writeln!(out, "}}")
}
