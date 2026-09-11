// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Typed block-parameter unused-variable suppression,
//! `KnowledgeScope` plumbing, and project-wide cross-file
//! extension/selector-hint visibility (ADR 0100 Rule 2).

use super::*;

// ── Typed block parameters should not cause false-positive
//    "Unused variable" warnings for method-local names read on a later line.

#[test]
fn typed_block_param_in_typed_class_nested_if_false_no_unused_warning() {
    // In a `typed` class, a local assigned inside a deeply nested
    // `ifFalse:` block and read on the next line must not be reported as
    // "Unused" — the typed block param `:: Dictionary` on the inner block
    // must not confuse the parser or corrupt the surrounding AST.
    let source = r#"
typed Value subclass: UnusedInTyped
  field: states :: List(String) = #()

  check: issue :: Dictionary -> Boolean =>
    state := issue at: "state" ifAbsent: [nil]
    state isNil ifTrue: [^false]

    (state == "todo")
      ifTrue: [
        blockers := issue at: "blocked_by" ifAbsent: [#()]
        blockers isEmpty
          ifFalse: [
            hasNonTerminal := blockers
              anySatisfy: [:b :: Dictionary |
                bState := b at: "state" ifAbsent: [nil]
                bState isNil
              ]
            hasNonTerminal ifTrue: [^false]
          ]
      ]

    true
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);

    let unused: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert!(
        unused.is_empty(),
        "`hasNonTerminal` is read on the next line; no unused warning expected, got: {unused:?}"
    );
}

#[test]
fn typed_block_param_does_not_affect_unused_variable_pass() {
    // The Unused-variable pass must behave identically with or
    // without a `typed` class modifier. Both variants should be warning-free.
    let typed_src = r"
typed Value subclass: UnusedInTypedSmall
  check: xs :: List(Dictionary) -> Boolean =>
    hasMore := xs anySatisfy: [:b :: Dictionary | b isNil]
    hasMore ifTrue: [^false]
    true
";
    let untyped_src = r"
Value subclass: UnusedUntypedSmall
  check: xs =>
    hasMore := xs anySatisfy: [:b :: Dictionary | b isNil]
    hasMore ifTrue: [^false]
    true
";

    for source in [typed_src, untyped_src] {
        let tokens = crate::source_analysis::lex_with_eof(source);
        let (module, _) = crate::source_analysis::parse(tokens);
        let result = analyse(&module);
        let unused: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("Unused variable"))
            .collect();
        assert!(
            unused.is_empty(),
            "Expected no unused warnings for source:\n{source}\ngot: {unused:?}"
        );
    }
}

// ── KnowledgeScope plumbing ───────────────────────────────────────────────────

#[test]
fn analyse_stamps_default_knowledge_scope() {
    let tokens = crate::source_analysis::lex_with_eof("Object subclass: ScopeDefault\n  m => 1\n");
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = analyse(&module);
    assert_eq!(
        result.class_hierarchy.knowledge_scope(),
        KnowledgeScope::ModuleOnly,
        "analysis without a project orchestrator must keep the conservative default"
    );
}

#[test]
fn analyse_with_options_stamps_project_complete_scope() {
    let tokens = crate::source_analysis::lex_with_eof("Object subclass: ScopeFull\n  m => 1\n");
    let (module, _) = crate::source_analysis::parse(tokens);
    let options = crate::CompilerOptions {
        knowledge_scope: KnowledgeScope::ProjectComplete,
        ..Default::default()
    };
    let result = analyse_full(&module, AnalysisContext::default().with_options(&options));
    assert_eq!(
        result.class_hierarchy.knowledge_scope(),
        KnowledgeScope::ProjectComplete,
        "the orchestrator's completeness claim must reach the hierarchy"
    );
}

// ── Project-wide cross-file extension visibility ──────────────────────────────

#[test]
fn cross_file_extension_resolves_instead_of_dnu_hint() {
    use crate::compilation::extension_index::ExtensionIndex;

    // Another file defines `String >> shoutLouder`.
    let ext_tokens = crate::source_analysis::lex_with_eof("String >> shoutLouder => self\n");
    let (ext_module, _) = crate::source_analysis::parse(ext_tokens);
    let mut cross_file_extensions = ExtensionIndex::new();
    cross_file_extensions.add_module(&ext_module, std::path::Path::new("other.bt"));

    // The current file sends it to a String receiver.
    let source = "Object subclass: UseShout\n  class demo =>\n    \"abc\" shoutLouder\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);

    // Without the index: false Dnu hint (today's behaviour).
    let result = analyse(&module);
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("shoutLouder")),
        "without cross-file extensions the send must produce a Dnu hint"
    );

    // With the index: the extension resolves, the hint disappears.
    let options = crate::CompilerOptions::default();
    let result = analyse_full(
        &module,
        AnalysisContext::default()
            .with_options(&options)
            .with_cross_file_extensions(&cross_file_extensions),
    );
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("shoutLouder"))
        .collect();
    assert!(
        dnu.is_empty(),
        "cross-file extension should resolve instead of hinting, got: {dnu:?}"
    );
}

#[test]
fn genuinely_unresolved_selector_still_hints_with_extensions_registered() {
    use crate::compilation::extension_index::ExtensionIndex;

    let ext_tokens = crate::source_analysis::lex_with_eof("String >> shoutLouder => self\n");
    let (ext_module, _) = crate::source_analysis::parse(ext_tokens);
    let mut cross_file_extensions = ExtensionIndex::new();
    cross_file_extensions.add_module(&ext_module, std::path::Path::new("other.bt"));

    // A genuine typo on a closed receiver still hints (ADR 0100 Rule 2:
    // improved resolution removes false positives, not true ones).
    let source = "Object subclass: UseTypo\n  class demo =>\n    \"abc\" reverssed\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let options = crate::CompilerOptions::default();
    let result = analyse_full(
        &module,
        AnalysisContext::default()
            .with_options(&options)
            .with_cross_file_extensions(&cross_file_extensions),
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("reverssed")),
        "a genuine typo must still produce a Dnu hint"
    );
}

// ── ADR 0100 Rule 2 end-to-end ─────────────────────────────────────────────────

#[test]
fn typo_hints_in_project_complete_dependency_free_package() {
    // Rule 2: knowing more makes the Hint trustworthy, not fatal — a genuine
    // typo on a closed receiver still hints in a project-complete,
    // dependency-free build.
    let source = "Object subclass: TypoDemo\n  class demo =>\n    \"abc\" reverssed\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let options = crate::CompilerOptions {
        knowledge_scope: KnowledgeScope::ProjectComplete,
        has_package_dependencies: false,
        ..Default::default()
    };
    let result = analyse_full(&module, AnalysisContext::default().with_options(&options));
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("reverssed")),
        "typo must still hint in a dependency-free project-complete build"
    );
}

#[test]
fn dependency_package_suppresses_unresolved_selector_hints_pre_ws3() {
    // Pre-WS3 guard: with dependencies declared, a dependency could
    // extend any class, so unresolved-selector hints are withheld until WS3
    // loads cross-package extension metadata (ADR 0100 Rule 1, third
    // downgrade; hints go down, not up).
    let source = "Object subclass: DepDemo\n  class demo =>\n    \"abc\" reverssed\n";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let options = crate::CompilerOptions {
        knowledge_scope: KnowledgeScope::ProjectComplete,
        has_package_dependencies: true,
        ..Default::default()
    };
    let result = analyse_full(&module, AnalysisContext::default().with_options(&options));
    let dnu: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("reverssed"))
        .collect();
    assert!(
        dnu.is_empty(),
        "with dependencies present, unresolved-selector hints are withheld pre-WS3, got: {dnu:?}"
    );
}
