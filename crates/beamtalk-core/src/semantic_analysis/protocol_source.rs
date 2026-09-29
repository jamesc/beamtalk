// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source identity of a provision-bearing protocol (ADR 0127 §3, "Source
//! locations").
//!
//! **DDD Context:** Semantic Analysis
//!
//! A `uses:` class's flattened provisions keep the *spans* of the protocol
//! file they were parsed from — [`Span`](crate::source_analysis::Span) is
//! byte offsets only, with no file tag. Anything that turns one of those
//! spans into a line (BEAM line annotations, stack traces, per-method source
//! slices) therefore has to map it through the *protocol's* source text, not
//! the using class's. [`ProtocolSource`] is that text plus its path, keyed by
//! protocol name in a [`ProtocolSourceMap`] that every compile path builds
//! from the same files it already read to obtain the protocol's AST.

use std::collections::HashMap;

use ecow::EcoString;

/// The source file a provision-bearing protocol was parsed from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolSource {
    /// Path of the protocol's `.bt` file, when it has one (a REPL-defined
    /// protocol has text but no path).
    pub path: Option<EcoString>,
    /// The full source text of the protocol's file.
    pub text: EcoString,
}

/// Protocol name → [`ProtocolSource`].
pub type ProtocolSourceMap = HashMap<EcoString, ProtocolSource>;
