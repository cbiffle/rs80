//! Common types and definitions needed by both the emulator and codegen. This
//! is a separate crate for phase-ordering reasons: it's needed at build-time by
//! codegen and runtime by the emulator.

pub mod isa;
pub mod insn_info;
