
//! A simple emulator for the 8080 (or, really, a clone thereof -- this is
//! tested against an emulator for the Soviet KR580VM80A).

pub mod emu;
pub mod emu2;
pub mod ops;
pub mod dis;
pub mod bdos;
pub mod dbg;

pub use emu::Emu;
