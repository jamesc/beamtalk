// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4811: a class that defines one selector twice (class body plus a
//! same-file extension, or repeated extensions) is reported by codegen with the
//! selector named, instead of emitting two `'name'/N` Core Erlang functions in
//! one module (which `erlc` rejects with an internal error in
//! `beam_ssa_throw`). Semantic analysis reports the same input as an error, so
//! a front end that honours its diagnostics never reaches this backstop.

use super::*;
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

fn gen_err(src: &str) -> String {
    let (module, _) = parse(lex_with_eof(src));
    match generate_module(&module, CodegenOptions::new("dup")) {
        Ok(code) => panic!("expected a duplicate-method error, got:\n{code}"),
        Err(e) => e.to_string(),
    }
}

#[test]
fn body_plus_extension_instance_duplicate_is_reported() {
    let msg = gen_err("Object subclass: Foo\n  bar => 1\n\nFoo >> bar => 2\n");
    assert!(
        msg.contains("duplicate instance method `bar` in class `Foo`"),
        "{msg}"
    );
}

#[test]
fn body_plus_extension_class_side_duplicate_is_reported() {
    let msg = gen_err(
        "Object subclass: Foo\n  class greeting => \"a\"\n\nFoo class >> greeting => \"b\"\n",
    );
    assert!(
        msg.contains("duplicate class method `greeting` in class `Foo`"),
        "{msg}"
    );
}

#[test]
fn repeated_extensions_are_reported() {
    let msg = gen_err(
        "Object subclass: Foo\n  x => 1\n\nFoo class >> make => 1\nFoo class >> make => 2\nFoo class >> make => 3\n",
    );
    assert!(msg.contains("duplicate class method `make`"), "{msg}");
}

#[test]
fn distinct_selectors_and_sides_still_generate() {
    // Same name on the instance and class side is not a duplicate.
    let (module, diags) = parse(lex_with_eof(
        "Object subclass: Foo\n  bar => 1\n  class bar => 2\n\nFoo >> baz => 3\n",
    ));
    assert!(
        diags.iter().all(|d| d.severity != Severity::Error),
        "{diags:?}"
    );
    generate_module(&module, CodegenOptions::new("ok")).expect("no duplicate, codegen succeeds");
}

/// The shape of the two nightly `corpus-through-erlc` failures
/// (`class_builder_capstone`, `late_slots`): a REPL-style script that redefines
/// a class method. After the build pipeline's same-file fold, analysis reports
/// an error (so the corpus generator skips the input) and codegen refuses it by
/// name.
#[test]
fn redefined_class_method_is_rejected_by_analysis_and_codegen() {
    use beamtalk_core::compilation::fold_same_file_extensions;
    use beamtalk_core::semantic_analysis::analyse;

    let src = "Actor subclass: Cap\n  class greeting => \"before\"\n\n\
               Cap class >> greeting => \"after\"\n\
               Cap class >> getCurrent => 1\n\
               Cap class >> getCurrent => 2\n";
    let (mut module, mut diags) = parse(lex_with_eof(src));
    diags.extend(fold_same_file_extensions(&mut module));
    diags.extend(analyse(&module).diagnostics.iter().cloned());
    for selector in ["greeting", "getCurrent"] {
        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message
                    .contains(&format!("Duplicate class method `{selector}`"))),
            "analysis should name the duplicate `{selector}`: {diags:?}"
        );
    }
    let err = generate_module(&module, CodegenOptions::new("corpus_input"))
        .expect_err("codegen must not emit a duplicate function");
    assert!(err.to_string().contains("duplicate class method"), "{err}");
}
