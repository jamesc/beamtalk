// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4059 pin: analysis runs on the tree with redundant send-operand
//! parentheses peeled (`ast::normalize`), so a diagnostic about a
//! parenthesised operand lands on the operand's own span — narrower than the
//! `( … )` it was written in — and reads exactly like the bare form's.

use crate::semantic_analysis::{AnalysisContext, analyse_full};
use crate::source_analysis::Severity;
use crate::test_helpers::test_support::parse_bt;

/// Every error diagnostic of `src`, as (message, the source text it spans).
fn errors(src: &str) -> Vec<(String, String)> {
    let result = analyse_full(&parse_bt(src), AnalysisContext::default());
    result
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .map(|d| {
            let text = &src[d.span.start() as usize..d.span.end() as usize];
            (d.message.to_string(), text.to_string())
        })
        .collect()
}

#[test]
fn diagnostic_on_a_parenthesised_block_argument_spans_the_block() {
    let wrapped = errors("Object subclass: A\n  run => #(1) inject: 0 into: ([:a | a])\n");
    let bare = errors("Object subclass: A\n  run => #(1) inject: 0 into: [:a | a]\n");
    assert_eq!(wrapped.len(), 1, "{wrapped:?}");
    assert_eq!(wrapped[0].1, "[:a | a]", "{wrapped:?}");
    assert_eq!(wrapped, bare);
}

/// BT-4237/BT-4238 pin: type inference and `local_threading_diagnostics` read
/// the operand-normalised tree whatever they are handed, so a raw parse (the
/// language service's, `beamtalk-lint`'s) gives exactly what the peeled
/// module gives, and no type-map entry names a removed wrapper.
#[test]
fn raw_module_entries_read_the_normalised_tree() {
    use crate::ast::normalize::{first_parenthesised_operand, peel_operand_parens};
    use crate::semantic_analysis::{ClassHierarchy, infer_types, local_threading_diagnostics};

    for src in [
        "x := 42.\ny := (x) abs",
        "Object subclass: A\n  run: e => [:v | v] on: (Error) do: ([:err | err messageText])\n",
        "Object subclass: A\n  run: t => (t) ifNotNil: ([:v | v size])\n",
        "Value subclass: B\n  run =>\n    n := 0\n    #(1) do: ([:i | n := n + i])\n    n\n",
    ] {
        let raw = parse_bt(src);
        let wrapper = first_parenthesised_operand(&raw).expect("a parenthesised operand");
        let mut peeled = raw.clone();
        peel_operand_parens(&mut peeled);
        let hierarchy = ClassHierarchy::build(&raw).0.unwrap();

        let sorted = |module: &crate::ast::Module| {
            let map = infer_types(module, &hierarchy, None);
            let mut entries: Vec<_> = map
                .iter()
                .map(|(span, ty)| (span.start(), span.end(), format!("{ty:?}")))
                .collect();
            entries.sort();
            entries
        };
        let raw_types = sorted(&raw);
        assert_eq!(raw_types, sorted(&peeled), "{src}");
        assert!(
            !raw_types
                .iter()
                .any(|(start, end, _)| (*start, *end) == (wrapper.start(), wrapper.end())),
            "{src}: a removed wrapper has a type-map entry"
        );

        let diags = |module: &crate::ast::Module| {
            local_threading_diagnostics(module, &hierarchy)
                .iter()
                .map(|d| (d.message.to_string(), d.span))
                .collect::<Vec<_>>()
        };
        assert_eq!(diags(&raw), diags(&peeled), "{src}");
    }
}
