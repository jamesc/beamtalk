// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parsing infrastructure for Beamtalk source code.
//!
//! This module contains the lexer, parser, and AST definitions.
//!
//! # Token Types
//!
//! The [`Token`] type carries source location via [`Span`] and supports
//! trivia tracking for formatting tools. See [`TokenKind`] for all
//! supported syntactic elements.

mod span;
mod token;

pub use span::Span;
pub use token::{Token, TokenKind, Trivia};
