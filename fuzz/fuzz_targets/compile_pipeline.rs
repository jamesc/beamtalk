// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Fuzz target for the full compile pipeline: lex → parse → analyse → codegen.
//!
//! `parse_arbitrary` (the other fuzz target) stops at the parser. This target
//! pushes arbitrary input all the way through semantic analysis and Core
//! Erlang code generation — the deeper failures live here: inputs that parse
//! and analyse cleanly, then generate invalid or panicking codegen (BT-3124).
//!
//! # Success Criteria
//!
//! The pipeline passes fuzzing if:
//! - No stage panics on any input (including replacement characters from
//!   invalid UTF-8)
//! - Whenever `generate_module` returns `Ok`, the output is structurally
//!   valid Core Erlang — the same checks
//!   `core_erlang_validity_tests.rs`'s proptest suite runs, shared via
//!   `beamtalk_core::test_helpers::test_support` so the two never drift.
//!
//! We do not assert anything about `analyse`'s or `generate_module`'s
//! diagnostics/errors: arbitrary byte soup is expected to fail analysis or
//! codegen far more often than it succeeds. Only a panic, or a structurally
//! broken `Ok` output, counts as a fuzzing failure.
//!
//! # Corpus Seeding
//!
//! `fuzz/corpus/compile_pipeline/` is seeded from `stdlib/test/*.bt` and
//! `tests/repl-protocol/cases/*.btscript` (richer, more semantically valid
//! programs than `parse_arbitrary`'s corpus needs, since this target's
//! interesting mutations start from code that gets *past* the parser).

#![no_main]

use beamtalk_core::codegen::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis::analyse;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_core::test_helpers::test_support::core_erlang_structural_issues;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Convert to UTF-8 using lossy conversion so invalid bytes become U+FFFD
    // replacement characters rather than being skipped entirely.
    let source = String::from_utf8_lossy(data);

    let tokens = lex_with_eof(&source);
    let (module, _parse_diagnostics) = parse(tokens);

    // Success = no panic. We don't care whether analysis finds errors — most
    // fuzzer-mutated input will fail semantic checks, and that's fine.
    let analysis = analyse(&module);

    let options = CodegenOptions::new("fuzz_compile_pipeline").with_analysis(analysis);

    // Success = no panic. When codegen succeeds, the output must be
    // structurally valid Core Erlang — the same bar the proptest suite holds
    // codegen to, via the shared checker.
    if let Ok(output) = generate_module(&module, options) {
        let issues = core_erlang_structural_issues(&output);
        assert!(
            issues.is_empty(),
            "generate_module produced structurally invalid Core Erlang for input {:?}:\n{}",
            source,
            issues.join("\n"),
        );
    }
});
