
/// A simple emulator for the 8080 (or, really, a clone thereof -- this is
/// tested against an emulator for the Soviet KR580VM80A).

#[macro_use]
extern crate lazy_static;

pub mod isa;
pub mod emu;
pub mod ops;
pub mod dis;
pub mod bdos;
pub mod dbg;

pub use emu::Emu;
pub use ops::make_decode_table;
