// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parsing infrastructure for Beamtalk source code.
//!
//! This module contains the lexer, parser, and AST definitions.
//!
//! # Lexical Analysis
//!
//! The [`Lexer`] converts source text into a stream of [`Token`]s. Each token
//! carries its source location via [`Span`] and supports trivia tracking
//! (whitespace and comments) for formatting tools.
//!
//! ```
//! use beamtalk_core::parse::{Lexer, TokenKind};
//!
//! let tokens: Vec<_> = Lexer::new("x + 1").collect();
//! assert_eq!(tokens.len(), 3); // x, +, 1
//! ```
//!
//! See [`TokenKind`] for all supported syntactic elements.

mod lexer;
mod span;
mod token;

pub use lexer::{Lexer, lex, lex_with_eof};
pub use span::Span;
pub use token::{Token, TokenKind, Trivia};
