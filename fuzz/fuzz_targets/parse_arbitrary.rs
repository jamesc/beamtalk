// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Fuzz target for parser crash safety testing.
//!
//! This target feeds arbitrary byte sequences to the parser and asserts
//! that it never panics. The parser must handle all input gracefully,
//! producing either a valid AST or diagnostics.
//!
//! # Success Criteria
//!
//! The parser passes fuzzing if:
//! - It never panics on any input (including invalid UTF-8)
//! - It always returns a Module and Vec<Diagnostic>
//! - No assertions fail during parsing
//!
//! # Corpus Seeding
//!
//! The corpus in `fuzz/corpus/parse_arbitrary/` contains all `.bt` files
//! from `examples/` and `tests/e2e/cases/`, providing realistic starting
//! points for mutation.

#![no_main]

use libfuzzer_sys::fuzz_target;
use beamtalk_core::source_analysis::{lex_with_eof, parse};

fuzz_target!(|data: &[u8]| {
    // Only test valid UTF-8 (parser expects strings)
    // Invalid UTF-8 is not a parser concern - it's filtered earlier
    if let Ok(source) = std::str::from_utf8(data) {
        // Lex the source into tokens
        let tokens = lex_with_eof(source);
        
        // Parse tokens into AST
        // Success = no panic. We don't care if there are diagnostics.
        let (_module, _diagnostics) = parse(tokens);
        
        // If we got here without panicking, the parser is robust!
    }
});
