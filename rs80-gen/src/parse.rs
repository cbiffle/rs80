//! ISA definitions parser for rs80.
//!
//! This parser is implemented using combinators; each function in this
//! module represents an entity in the spec file grammar. Only the top
//! level `spec_file` is exposed.

use rs80_common::insn_info::{FType, IType, Operand};

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

fn operand<I>() -> impl Parser<Input = I, Output = AOperand>
where I: Stream<Item = char>,
      I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
            string("PSW").map(|_| Operand::PSW),
            char('#').with(choice((
                        char('#').with(letter())
                        .map(|c| Operand::I(c, IType::I16)),
                        letter()
                        .map(|c| Operand::I(c, IType::I8))))),
                        char('&').with(letter())
                        .map(|c| Operand::F(c, FType::RP)),
                        char('?').with(letter())
                        .map(|c| Operand::F(c, FType::RM)),
                        char('@').with(letter())
                        .map(|c| Operand::I(c, IType::Address)),
                        char('*').with(letter())
                        .map(|c| Operand::F(c, FType::C3)),
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
        body,
    })
}

fn body<I>() -> impl Parser<Input = I, Output = Vec<String>>
where I: Stream<Item = char>,
      I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between((char('{'), newline()),
    (char('}'), choice((eof(), newline().map(|_|())))),
    many(
        attempt(many(satisfy(|c| c != '\n')).skip(newline())
            .then(|s: String| {
                if s.starts_with('}') {
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
