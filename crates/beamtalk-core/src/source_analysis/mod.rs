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
//!
//! # Parsing
//!
//! The [`parse`] function converts tokens into a [`Module`](crate::ast::Module) AST.
//! Binary operator precedence uses Pratt parsing for correct associativity and
//! easy extensibility (see [`parser`] module for details).
//!
//! # Error Handling
//!
//! The lexer uses error recovery: invalid input is converted into
//! [`TokenKind::Error`] tokens rather than stopping. These tokens carry all the
//! information needed for diagnostics, so downstream code should inspect
//! `TokenKind::Error` variants when reporting lexing problems.
//!
//! Use [`LexError`] to construct structured diagnostics with miette integration.

mod error;
mod lexer;
mod parser;
mod span;
mod token;

pub use error::{LexError, LexErrorKind};
pub use lexer::{Lexer, lex, lex_with_eof};
pub use parser::{Diagnostic, Severity, parse};
pub use span::Span;
pub use token::{Token, TokenKind, Trivia};
