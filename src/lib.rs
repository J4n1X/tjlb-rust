//! TJLB Language Parser
//!
//! A parser for the TJLB programming language, a C-like language with specific
//! syntax rules and constraints.

pub mod lexer;
pub mod parser;
pub mod symbol;
pub mod codegen;

pub use lexer::*;
pub use parser::*;
pub use symbol::*;
pub use codegen::*;