//! A simple emulator for the Intel 8080.

pub mod emu;
pub mod ops;
pub mod dis;
pub mod bdos;
pub mod dbg;

pub use emu::Emu;
