// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4923: E0402 (leaked visibility) for a standalone `Foo >> sel` method.
//!
//! The rule is the one ADR 0071 states for an inline method: an internal class
//! of the current package must not appear in the signature of a public method
//! of a public class, and an `internal` method must not satisfy a public
//! protocol requirement. A same-file extension is folded into the class body
//! before the checks run; only an extension in a *different* file than its
//! class reaches the standalone-method path, so these tests hand `analyse_full`
//! the class as a pre-loaded declaration.

use super::*;
use crate::source_analysis::{Diagnostic, Severity};

const TOKEN_BUFFER: &str = "internal Value subclass: TokenBuffer\n  size => 0\n";
const PARSER: &str = "Object subclass: Parser\n  go => 1\n";

/// Analyses `ext_src` as package `app`, with the classes of `(package, src)`
/// pairs pre-loaded as cross-file declarations.
fn analyse_ext(preloaded: &[(&str, &str)], ext_src: &str) -> Vec<Diagnostic> {
    let mut classes = Vec::new();
    for (package, src) in preloaded {
        classes.extend(declared_by(package, src));
    }
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(classes);
    analyse_full(&parse_module(ext_src), ctx).diagnostics
}

fn leaks(d: &[Diagnostic]) -> Vec<&Diagnostic> {
    d.iter()
        .filter(|d| d.message.contains("appears in public signature"))
        .collect()
}

/// The message the inline form of the same method produces, for comparison.
fn inline_leak_message(class_src: &str) -> String {
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by("app", TOKEN_BUFFER));
    let d = analyse_full(&parse_module(class_src), ctx).diagnostics;
    let hits = leaks(&d);
    assert_eq!(hits.len(), 1, "{d:?}");
    hits[0].message.to_string()
}

#[test]
fn standalone_return_type_leaking_internal_class_is_an_error() {
    let d = analyse_ext(
        &[("app", PARSER), ("app", TOKEN_BUFFER)],
        "Parser >> tokenize: s :: String -> TokenBuffer => nil\n",
    );
    let hits = leaks(&d);
    assert_eq!(hits.len(), 1, "{d:?}");
    assert_eq!(hits[0].severity, Severity::Error);
    // Same message as the inline form.
    assert_eq!(
        hits[0].message.as_str(),
        inline_leak_message(
            "Object subclass: Parser\n  tokenize: s :: String -> TokenBuffer => nil\n"
        )
    );
}

#[test]
fn standalone_parameter_type_leaking_internal_class_is_an_error() {
    let d = analyse_ext(
        &[("app", PARSER), ("app", TOKEN_BUFFER)],
        "Parser >> feed: buffer :: TokenBuffer => nil\n",
    );
    assert_eq!(leaks(&d).len(), 1, "{d:?}");
}

#[test]
fn standalone_class_side_signature_leaking_internal_class_is_an_error() {
    let d = analyse_ext(
        &[("app", PARSER), ("app", TOKEN_BUFFER)],
        "Parser class >> make -> TokenBuffer => nil\n",
    );
    assert_eq!(leaks(&d).len(), 1, "{d:?}");
}

#[test]
fn standalone_on_another_packages_public_class_is_an_error() {
    // `Parser` is `json`'s public class; the signature names `app`'s own
    // internal class, which the extension exposes on that public class.
    let d = analyse_ext(
        &[("json", PARSER), ("app", TOKEN_BUFFER)],
        "Parser >> tokenize: s :: String -> TokenBuffer => nil\n",
    );
    assert_eq!(leaks(&d).len(), 1, "{d:?}");
}

#[test]
fn standalone_internal_method_is_exempt_like_an_inline_internal_method() {
    let d = analyse_ext(
        &[("app", PARSER), ("app", TOKEN_BUFFER)],
        "Parser >> internal tokenize: s :: String -> TokenBuffer => nil\n",
    );
    assert!(leaks(&d).is_empty(), "{d:?}");
}

#[test]
fn standalone_on_an_internal_class_is_exempt_like_an_inline_method() {
    // All methods on an internal class are effectively internal (ADR 0071).
    let d = analyse_ext(
        &[
            ("app", "internal Object subclass: Parser\n  go => 1\n"),
            ("app", TOKEN_BUFFER),
        ],
        "Parser >> tokenize: s :: String -> TokenBuffer => nil\n",
    );
    assert!(leaks(&d).is_empty(), "{d:?}");
}

#[test]
fn standalone_naming_a_public_class_is_silent() {
    let d = analyse_ext(
        &[("app", PARSER)],
        "Parser >> tokenize: s :: String -> Integer => 1\n",
    );
    assert!(leaks(&d).is_empty(), "{d:?}");
}

#[test]
fn standalone_naming_another_packages_internal_class_stays_e0401_only() {
    let d = analyse_ext(
        &[("app", PARSER), ("json", TOKEN_BUFFER)],
        "Parser >> tokenize: s :: String -> TokenBuffer => nil\n",
    );
    assert!(leaks(&d).is_empty(), "{d:?}");
    assert!(
        d.iter()
            .any(|d| d.message.contains("is internal to package")),
        "{d:?}"
    );
}

#[test]
fn same_file_extension_leak_is_reported_once() {
    // Folded into the class body, so the inline path reports it; the
    // standalone path must not report it a second time.
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by("app", TOKEN_BUFFER));
    let src = "Object subclass: Parser\n  go => 1\n\
               Parser >> tokenize: s :: String -> TokenBuffer => nil\n";
    let d = analyse_full(&parse_module(src), ctx).diagnostics;
    assert_eq!(leaks(&d).len(), 1, "{d:?}");
}

// ---- an `internal` standalone method satisfying a public protocol ----

#[test]
fn standalone_internal_method_satisfying_a_public_protocol_is_an_error() {
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by("app", PARSER));
    let src = "Protocol define: Tokenizing\n  tokenize -> Integer\n\
               Parser >> internal tokenize -> Integer => 1\n";
    let d = analyse_full(&parse_module(src), ctx).diagnostics;
    let hits: Vec<_> = d
        .iter()
        .filter(|d| d.message.contains("satisfies public protocol"))
        .collect();
    assert_eq!(hits.len(), 1, "{d:?}");
    assert!(
        hits[0].message.contains("'tokenize' on 'Parser'"),
        "{}",
        hits[0].message
    );
}

#[test]
fn standalone_internal_class_method_satisfying_a_public_protocol_is_an_error() {
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by("app", PARSER));
    let src = "Protocol define: Making\n  class make -> Integer\n\
               Parser class >> internal make -> Integer => 1\n";
    let d = analyse_full(&parse_module(src), ctx).diagnostics;
    let hits: Vec<_> = d
        .iter()
        .filter(|d| d.message.contains("satisfies public protocol"))
        .collect();
    assert_eq!(hits.len(), 1, "{d:?}");
    assert!(
        hits[0]
            .message
            .contains("Internal class method 'make' on 'Parser'"),
        "{}",
        hits[0].message
    );
}
