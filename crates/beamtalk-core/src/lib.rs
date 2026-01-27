// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler core.
//!
//! This crate contains the core compiler functionality:
//! - Lexical analysis (tokenization)
//! - Parsing (AST construction)
//! - Semantic analysis (type checking, name resolution)
//! - Code generation (Core Erlang output)
//!
//! The compiler is designed as a language service, prioritizing
//! IDE responsiveness over batch compilation speed.

#![doc = include_str!("../../../README.md")]

pub mod ast;
pub mod parse;

/// Re-export commonly used types.
pub mod prelude {
    pub use crate::ast::{Expression, Identifier, Literal, Module};
    pub use crate::parse::Span;
}
