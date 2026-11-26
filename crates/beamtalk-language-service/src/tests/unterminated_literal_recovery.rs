// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A typo that leaves a string open or types a stray `/*` must not drop the
//! classes after it from the editor (BT-5182): they stay in the document
//! outline and the project index, and the one diagnostic names the construct
//! and where it starts instead of echoing the rest of the file.

use super::common::*;

/// Opens `source` as `typo.bt`: the class names in its outline, the
/// declarations of `B` in the project index, and the diagnostic messages.
fn open(source: &str) -> (Vec<String>, usize, Vec<String>) {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("typo.bt");
    service.update_file(file.clone(), source.to_string());
    let outline = service
        .document_symbols(&file)
        .into_iter()
        .map(|symbol| symbol.name.to_string())
        .collect();
    let b_declarations = service.find_class_declarations("B").len();
    let diagnostics = service
        .diagnostics(&file)
        .into_iter()
        .map(|d| d.message.to_string())
        .collect();
    (outline, b_declarations, diagnostics)
}

#[test]
fn classes_after_an_unterminated_string_are_still_indexed() {
    let (outline, b_declarations, diagnostics) = open(
        "Object subclass: A\n  greet => \"hello\n  other => \"x\"\n\n\
         Object subclass: B\n  size => 1\n",
    );
    assert!(
        outline.iter().any(|name| name.starts_with('B')),
        "{outline:?}"
    );
    assert_eq!(b_declarations, 1);
    assert!(
        diagnostics
            .iter()
            // The missing quote on line 2 pairs with the first quote on
            // line 3, so the string left open is the one line 3 opens.
            .any(|m| {
                m.starts_with(
                    "string literal starting at line 3, column 14 is not closed before the \
                     definition on line 5",
                )
            }),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics.iter().all(|m| !m.contains("<error")),
        "token kind leaked: {diagnostics:?}"
    );
}

#[test]
fn classes_after_a_stray_block_comment_opener_are_still_indexed() {
    let (outline, b_declarations, diagnostics) = open(
        "Object subclass: A\n  /* never closed\n  greet => 1\n\n\
         Object subclass: B\n  size => 1\n",
    );
    assert!(
        outline.iter().any(|name| name.starts_with('B')),
        "{outline:?}"
    );
    assert_eq!(b_declarations, 1);
    assert!(
        diagnostics
            .iter()
            .any(|m| m == "block comments are not supported; use `//` on each line"),
        "{diagnostics:?}"
    );
}
