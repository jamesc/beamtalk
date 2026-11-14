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

/// The two nightly `corpus-through-erlc` failures: each whole `.btscript` is
/// one module that redefines a class method. After the build pipeline's
/// same-file fold, analysis reports an error (so the corpus generator skips the
/// input) and codegen refuses it by name.
#[test]
fn corpus_inputs_are_rejected_by_analysis_and_codegen() {
    use beamtalk_core::compilation::fold_same_file_extensions;
    use beamtalk_core::semantic_analysis::analyse;

    let root =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../tests/repl-protocol/cases");
    for file in ["class_builder_capstone.btscript", "late_slots.btscript"] {
        let src = std::fs::read_to_string(root.join(file)).expect("read corpus input");
        let (mut module, mut diags) = parse(lex_with_eof(&src));
        diags.extend(fold_same_file_extensions(&mut module));
        let analysis = analyse(&module);
        diags.extend(analysis.diagnostics.iter().cloned());
        assert!(
            diags.iter().any(|d| d.severity == Severity::Error),
            "{file}: expected an error diagnostic"
        );
        if file == "late_slots.btscript" {
            assert!(
                diags
                    .iter()
                    .any(|d| d.message.contains("Duplicate class method `getCurrent`")),
                "{file}: analysis should name the duplicate: {diags:?}"
            );
        }
        let err = generate_module(&module, CodegenOptions::new("corpus_input"))
            .expect_err("codegen must not emit a duplicate function");
        assert!(err.to_string().contains("duplicate"), "{file}: {err}");
    }
}
