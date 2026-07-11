//! Analysis passes over a disassembly database.

pub mod completeness;

#[cfg(feature = "analysis")]
pub mod graph;
