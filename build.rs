//! Build script for rs80.
//!
//! This program consumes and parses the `isa-ops.txt` that defines the
//! instruction set, and produces `dispatch.rs` implementing the execution
//! engine.
//!
//! It consists of four main parts:
//!
//! 1. `main`
//! 2. Data structure definitions.
//! 3. `mod parse` implementing the file parser.
//! 4. `mod gen` implementing the code generator.

extern crate combine;

use std::cmp::Reverse;
use std::io::{self, Read};
use std::fs;
use std::env;
use std::path::Path;

use combine::stream::state::State;
use combine::Parser;

////////////////////////////////////////////////////////////////////////////////
// Entry point

fn main() -> io::Result<()> {
    println!("cargo:rerun-if-changed=isa-ops.txt");

    let mut f = fs::File::open("isa-ops.txt")?;
    let mut text = String::new();
    f.read_to_string(&mut text)?;

    let (items, _) = parse::spec_file().easy_parse(State::new(&*text)).unwrap();

    let out_dir = env::var("OUT_DIR").unwrap();

    // Filter out comments.
    let mut defs: Vec<_> = items.into_iter()
        .filter_map(|item| {
            if let Item::Def(def) = item {
                Some(def)
            } else {
                None
            }
        })
        .collect();

    // Process more-specific defs before less-specific defs
    defs.sort_unstable_by_key(|d| Reverse(d.bits.specificity()));

    {
        let out_path = Path::new(&out_dir).join("dispatch.rs");
        let mut out = fs::File::create(out_path)?;
        gen::dispatch(&defs, &mut out)?;
    }

    {
        let out_path = Path::new(&out_dir).join("disassemble.rs");
        let mut out = fs::File::create(out_path)?;
        gen::disassemble(&defs, &mut out)?;
    }

    Ok(())
}

////////////////////////////////////////////////////////////////////////////////
// Data structures

/// An item found in the spec file.
#[derive(Clone, Debug)]
pub enum Item {
    Def(Def),
    Comment(String),
}

/// An instruction definition.
#[derive(Clone, Debug)]
pub struct Def {
    /// Bit pattern.
    bits: Pat,
    /// Mnemonic and optional condition code field identifier.
    mnem: (String, Option<char>),
    /// Operands from the assembly template.
    operands: Vec<Operand>,
    /// Primary and optional secondary cycle count.
    cycles: (usize, Option<usize>),
    /// Rust code body.
    body: Vec<String>,
}

/// An operand from the assembly template.
#[derive(Clone, Debug)]
enum Operand {
    /// Textual 'PSW' used for PUSH/POP instructions.
    LiteralPSW,
    /// Identifier and type of an opcode field.
    Field(char, FieldOperand),
    /// Identifier and type of an inline value.
    Inline(char, InlineOperand),
}

/// Type of an opcode field.
#[derive(Clone, Debug)]
enum FieldOperand {
    Reg16,
    RegOrM,
    Restart,
}

/// Type of an inline value.
#[derive(Clone, Debug)]
enum InlineOperand {
    I8,
    I16,
    /// This is mostly equivalent to `I16` but I'm keeping them distinguished in
    /// case I want to format them differently in the disassembler.
    Address,
}

/// A bit pattern.
#[derive(Clone, Debug)]
struct Pat(Vec<PatPart>);

impl Pat {
    /// Checks whether a literal byte matches this pattern. If so, returns
    /// `Some(v)` where `v` maps field identifiers to values.
    fn matches(&self, mut val: u8) -> Option<Vec<(char, u8)>> {
        let mut fields = Vec::new();
        for part in &self.0 {
            if let Ok(r) = part.matches(val) {
                if let Some(field) = r {
                    fields.push(field)
                }
            } else {
                return None
            }
            if part.len() != 8 {
                val <<= part.len();
            } else {
                val = 0;
            }
        }
        Some(fields)
    }

    /// Counts literally specified bits in this pattern as a measure of pattern
    /// specificity. More specific patterns will be applied before less specific
    /// patterns.
    fn specificity(&self) -> usize {
        let mut s = 0;
        for part in &self.0 {
            if let &PatPart::Bits(ref v) = part {
                s += v.len();
            }
        }
        s
    }
}

/// A field within a pattern.
#[derive(Clone, Debug)]
enum PatPart {
    /// Some literal bits.
    Bits(Vec<bool>),
    /// A named field of the given number of bits.
    Var(char, usize),
    /// A certain number of don't care bits.
    Ignore(usize),
}

impl PatPart {
    /// Determines the number of bits in this part.
    fn len(&self) -> usize {
        match self {
            &PatPart::Bits(ref v) => v.len(),
            &PatPart::Var(_, n) => n,
            &PatPart::Ignore(n) => n,
        }
    }

    /// Checks whether this part matches the *most significant bits* of `val`.
    ///
    /// If literal bits fail to match, returns `Err`.
    ///
    /// Otherwise, returns `Ok(Some((n, v)))` if a field `v` collected bits `n`,
    /// or `Ok(None)` if the bits were don't-care.
    fn matches(&self, mut val: u8) -> Result<Option<(char, u8)>, ()> {
        match self {
            &PatPart::Bits(ref v) => {
                for bit in v {
                    if *bit != ((val & 0x80) != 0) {
                        return Err(())
                    }
                    val = val << 1;
                }
                Ok(None)
            },
            &PatPart::Var(c, n) => {
                Ok(Some((c, val >> (8 - n))))
            },
            &PatPart::Ignore(_) => Ok(None),
        }
    }
}

mod parse {
    //! This parser is implemented using combinators; each function in this
    //! module represents an entity in the spec file grammar. Only the top
    //! level `spec_file` is exposed.

    use super::*;
    use combine::*;
    use combine::parser::char::*;

    fn bit_pattern<I>() -> impl Parser<Input = I, Output = Pat>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        let literal_bits = many1(choice((char('0'), char('1')))
                                 .map(|s| s == '1'))
            .map(PatPart::Bits);

        let captured_bits = letter()
            .then(|c| many(char(c))
                  .map(move |s: Vec<_>| PatPart::Var(c, s.len() + 1)));

        let ignored_bits = many1(char('_'))
            .map(|cs: Vec<_>| PatPart::Ignore(cs.into_iter().count()));

        many1(choice((
                    literal_bits,
                    captured_bits,
                    ignored_bits,
                    )))
            .map(Pat)
    }

    fn operand<I>() -> impl Parser<Input = I, Output = Operand>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        choice((
                string("PSW").map(|_| Operand::LiteralPSW),
                char('#').with(choice((
                            char('#').with(letter())
                                .map(|c| Operand::Inline(c,
                                                         InlineOperand::I16)),
                            letter()
                                .map(|c| Operand::Inline(c,
                                                         InlineOperand::I8))))),
                char('&').with(letter())
                    .map(|c| Operand::Field(c, FieldOperand::Reg16)),
                char('?').with(letter())
                    .map(|c| Operand::Field(c, FieldOperand::RegOrM)),
                char('@').with(letter())
                    .map(|c| Operand::Inline(c, InlineOperand::Address)),
                char('*').with(letter())
                    .map(|c| Operand::Field(c, FieldOperand::Restart)),
                ))
    }

    fn mnemonic<I>() -> impl Parser<Input = I, Output = (String, Option<char>)>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        (many1(upper()), optional(lower()))
    }

    fn cycles<I>() -> impl Parser<Input = I, Output = (usize, Option<usize>)>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        let integer = || many1(digit()).map(|s: String| s.parse().unwrap());
        (integer(), optional(char('/').with(integer())))
    }

    fn def<I>() -> impl Parser<Input = I, Output = Def>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        (
            bit_pattern(),
            (spaces(), char('-'), spaces()),
            mnemonic(),
            spaces(),
            sep_by(operand(), (char(','), spaces())),
            (spaces(), char('-'), spaces()),
            cycles(),
            spaces(),
            body(),
        ).map(|(b, _, m, _, os, _, c, _, body)| Def {
            bits: b,
            mnem: m,
            operands: os,
            cycles: c,
            body: body,
        })
    }

    fn body<I>() -> impl Parser<Input = I, Output = Vec<String>>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        between((char('{'), newline()),
                (char('}'), choice((eof(), newline().map(|_|())))),
            many(
                try(many(satisfy(|c| c != '\n')).skip(newline())
                    .then(|s: String| {
                        if s.starts_with("}") {
                            unexpected("}").map(|_| "".to_string()).right()
                        } else {
                            value(s).left()
                        }
                    })
            )))
    }

    pub fn spec_file<I>() -> impl Parser<Input = I, Output = Vec<Item>>
        where I: Stream<Item = char>,
              I::Error: ParseError<I::Item, I::Range, I::Position>,
    {
        let comment = char('#')
            .with(many::<String, _>(none_of(['\n'].iter().cloned())))
            .skip(newline());

        many(skip_many(newline())
             .with(choice((
                         comment.map(Item::Comment),
                         def().map(Item::Def),
                         ))))
            .skip(eof())
    }
}

mod gen {
    //! This module consumes the data structures produced by the parser and
    //! generates implementations of each instruction, specialized for each
    //! of 256 opcodes. Moving the decoding of e.g. bit field operands to
    //! codegen time improves performance by 16%.

    use std::io;
    use std::collections::HashMap;
    use super::*;

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
        writeln!(out, "")
    }

    fn format_operand(op: Option<&Operand>,
                      fields: &HashMap<char, u8>) -> &'static str {
        match op {
            None => "None",
            Some(Operand::LiteralPSW) => "Some(Operand::PSW)",
            Some(Operand::Field(c, f)) => {
                let val = fields[c];
                match f {
                    FieldOperand::Reg16 => match val {
                        0 => "Some(Operand::RP(RegPair::BC))",
                        1 => "Some(Operand::RP(RegPair::DE))",
                        2 => "Some(Operand::RP(RegPair::HL))",
                        _ => "Some(Operand::RP(RegPair::SP))",
                    },
                    FieldOperand::RegOrM => match val {
                        0 => "Some(Operand::RM(RegM::R(Reg::B)))",
                        1 => "Some(Operand::RM(RegM::R(Reg::C)))",
                        2 => "Some(Operand::RM(RegM::R(Reg::D)))",
                        3 => "Some(Operand::RM(RegM::R(Reg::E)))",
                        4 => "Some(Operand::RM(RegM::R(Reg::H)))",
                        5 => "Some(Operand::RM(RegM::R(Reg::L)))",
                        6 => "Some(Operand::RM(RegM::M))",
                        _ => "Some(Operand::RM(RegM::R(Reg::A)))",
                    },
                    FieldOperand::Restart => match val {
                        // TODO this is real silly
                        0 => "Some(Operand::C3(0))",
                        1 => "Some(Operand::C3(1))",
                        2 => "Some(Operand::C3(2))",
                        3 => "Some(Operand::C3(3))",
                        4 => "Some(Operand::C3(4))",
                        5 => "Some(Operand::C3(5))",
                        6 => "Some(Operand::C3(6))",
                        _ => "Some(Operand::C3(7))",
                    },
                }
            },
            Some(Operand::Inline(_, f)) => match f {
                InlineOperand::I8 => "Some(Operand::I8)",
                InlineOperand::I16 => "Some(Operand::I16)",
                InlineOperand::Address => "Some(Operand::Addr)",
            },
        }
    }

    pub fn disassemble(defs: &Vec<Def>, out: &mut impl io::Write)
        -> io::Result<()>
    {
        writeln!(out, "[")?;
        for op in 0..=255 {
            let (def, fields) = find_def(&defs, op);
            comment(&def, &fields, out)?;

            let cc = match def.mnem.1 {
                Some(n) => match fields[&n] {
                    0 => "Some(CC::NZ)",
                    1 => "Some(CC::Z)",
                    2 => "Some(CC::NC)",
                    3 => "Some(CC::C)",
                    4 => "Some(CC::PO)",
                    5 => "Some(CC::PE)",
                    6 => "Some(CC::P)",
                    _ => "Some(CC::N)",
                },
                None => "None",
            };

            writeln!(out, "InsnInfo {{")?;
            writeln!(out, "  mnemonic: Mnemonic {{")?;
            writeln!(out, "    label: \"{}\",", def.mnem.0)?;
            writeln!(out, "    cc: {},", cc)?;
            writeln!(out, "  }},")?;
            writeln!(out, "  a: {},", format_operand(def.operands.get(0), &fields))?;
            writeln!(out, "  b: {},", format_operand(def.operands.get(1), &fields))?;
            writeln!(out, "}},")?;
        }
        writeln!(out, "]")?;

        Ok(())
    }

    /// Generates the `dispatch` entry point from `defs` to `out`.
    pub fn dispatch(defs: &Vec<Def>, out: &mut impl io::Write)
        -> io::Result<()>
    {
        // First we'll generate an entry point for each opcode. For a hex
        // opcode 'xx' the entry point is named 'opcode_xx'. Doing this improves
        // performance significantly compared to a giant 'match' (and makes
        // profiles *significantly* easier to read).
        for op in 0..=255 {
            let (def, fields) = find_def(&defs, op);

            comment(&def, &fields, out)?;

            // Opcode entry point. We allow(unused) because not every
            // opcode uses the parameters to its function; we know all
            // opcode entry points are used because we use them below.
            // (We can't allow(unused) an argument in current Rust.)
            writeln!(out, "#[allow(unused)]")?;
            writeln!(out, "fn opcode_{:02x}(st: &mut Emu, \
                                                    ctx: &mut Ctx) -> bool {{",
                                                    op)?;
            // Standard bindings
            writeln!(out, "    let opcode = Opcode({});", op)?;

            // Bind condition success field if present.
            if let Some(c) = def.mnem.1 {
                condition_into_scope(c, fields[&c], out)?
            }

            // Bind any named fields.
            for op in &def.operands {
                match op {
                    &Operand::Field(c, ref o) => {
                        field_into_scope(c, o, fields[&c], out)?
                    },
                    &Operand::Inline(c, ref o) => {
                        inline_into_scope(c, o, out)?
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
        writeln!(out, "  static TABLE: [fn(&mut Emu, &mut Ctx) -> bool; 256] \
                              = [")?;
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

    fn field_into_scope(n: char, o: &FieldOperand, bits: u8,
                        out: &mut impl io::Write)
        -> io::Result<()>
    {
        write!(out, "      ")?; // common indent
        match o {
            FieldOperand::RegOrM =>
                writeln!(out, "let {} = RegM::from({}u8);", n, bits),
            FieldOperand::Reg16 =>
                writeln!(out, "let {} = RegPair::from({}u8);", n, bits),
            FieldOperand::Restart =>
                writeln!(out, "let {} = {}u16 * 8;", n, bits),
        }
    }
    fn inline_into_scope(n: char, o: &InlineOperand, out: &mut impl io::Write)
        -> io::Result<()>
    {
        write!(out, "      ")?; // common indent
        match o {
            InlineOperand::I8 =>
                writeln!(out, "let {} = st.take_imm8();", n),
            InlineOperand::I16 | InlineOperand::Address =>
                writeln!(out, "let {} = st.take_imm16();", n),
        }
    }
}
