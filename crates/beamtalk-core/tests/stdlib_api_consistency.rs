// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stdlib API consistency rules (BT-5090, `docs/development/stdlib-api-consistency.md`).
//!
//! Parses every `stdlib/src/*.bt` and asserts four rules over the declared
//! class metadata:
//!
//! 1. `List` and `Array` both answer every selector in [`SEQUENCE_SELECTORS`].
//! 2. `Enumerable` provides every query in [`ENUMERABLE_QUERIES`].
//! 3. Every parameter of a keyword whose name ends in one of
//!    [`TIME_KEYWORD_SUFFIXES`] (or is listed in [`TIME_PARAMETER_EXTRAS`])
//!    is typed so that a `Duration` is accepted.
//! 4. A class that defines `reverse` also defines `reversed`.
//!
//! A rule that the stdlib does not yet satisfy is named in
//! [`EXPECTED_FAILURES`]. This is not a skip: the test fails both when a
//! violation is *not* listed and when a listed entry no longer violates, so
//! each later PR must delete the entries it fixes. The list is empty when
//! BT-5090 is done.

use beamtalk_core::ast::{
    ClassDefinition, MessageSelector, MethodDefinition, Module, ProtocolDefinition, TypeAnnotation,
};
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use std::collections::{BTreeMap, BTreeSet};

/// Order-dependent, non-growing selectors both `List` and `Array` answer
/// (decision 1).
const SEQUENCE_SELECTORS: &[&str] = &["reversed", "indexOf:", "take:", "drop:", "sort", "sort:"];

/// Queries `Collection` has that `Enumerable` must provide (decision 4).
/// `detect:` is included because `Collection` has the one-argument form
/// (checked by `collection_has_one_argument_detect`).
const ENUMERABLE_QUERIES: &[&str] = &[
    "reject:",
    "allSatisfy:",
    "noneSatisfy:",
    "includes:",
    "detect:",
];

/// A keyword part ending in one of these (case-insensitive, colon included)
/// names a time parameter (decision 3).
const TIME_KEYWORD_SUFFIXES: &[&str] = &["timeout:", "after:", "every:", "sleep:", "within:"];

/// Time parameters whose keyword does not end in a suffix above
/// (`Class>>selector:keyword`): the decision-3 outliers.
const TIME_PARAMETER_EXTRAS: &[(&str, &str, &str)] = &[
    ("TimeoutProxy", "setTimeoutMs:", "setTimeoutMs:"),
    (
        "RetryPolicy",
        "withInitialInterval:",
        "withInitialInterval:",
    ),
    (
        "RetryPolicy",
        "withMaximumInterval:",
        "withMaximumInterval:",
    ),
];

/// `Value` fields get a generated `withField:` setter typed from the field,
/// so a field that stores milliseconds needs an explicit `withField:`
/// definition for the setter to accept a `Duration`. `(class, setter)`.
const TIME_GENERATED_SETTERS: &[(&str, &str)] = &[
    ("RetryPolicy", "withInitialInterval:"),
    ("RetryPolicy", "withMaximumInterval:"),
];

/// Rules the stdlib does not yet satisfy. Each entry is
/// `"<rule>: <Class> <detail>"`; every PR that fixes one deletes it.
const EXPECTED_FAILURES: &[&str] = &[
    "enumerable: Enumerable does not provide allSatisfy:",
    "enumerable: Enumerable does not provide detect:",
    "enumerable: Enumerable does not provide includes:",
    "enumerable: Enumerable does not provide noneSatisfy:",
    "enumerable: Enumerable does not provide reject:",
    "naming: String defines reverse but not reversed",
    "sequence: Array does not answer drop:",
    "sequence: Array does not answer indexOf:",
    "sequence: Array does not answer reversed",
    "sequence: Array does not answer sort",
    "sequence: Array does not answer sort:",
    "sequence: Array does not answer take:",
    "time: Actor stopWith:timeout: parameter for timeout: does not accept Duration",
    "time: Announcer announceAndWait:timeout: parameter for timeout: does not accept Duration",
    "time: DynamicSupervisor stopWithin: parameter for stopWithin: does not accept Duration",
    "time: OS class run:timeout: parameter for timeout: does not accept Duration",
    "time: ProtoObject perform:withArguments:timeout: parameter for timeout: does not accept Duration",
    "time: RetryPolicy withInitialInterval: is a generated setter typed from an Integer field; define it explicitly to accept Duration",
    "time: RetryPolicy withMaximumInterval: is a generated setter typed from an Integer field; define it explicitly to accept Duration",
    "time: Supervisor stopWithin: parameter for stopWithin: does not accept Duration",
    "time: SystemAnnouncer announceAndWait:timeout: parameter for timeout: does not accept Duration",
    "time: TimeoutProxy setTimeoutMs: parameter for setTimeoutMs: does not accept Duration",
];

fn stdlib_modules() -> Vec<(String, Module)> {
    let dir = beamtalk_workspace::cargo_workspace_root!().join("stdlib/src");
    let mut files: Vec<_> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("read {}: {e}", dir.display()))
        .map(|e| e.expect("dir entry").path())
        .filter(|p| p.extension().is_some_and(|x| x == "bt"))
        .collect();
    files.sort();
    files
        .into_iter()
        .map(|path| {
            let source = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
            let (module, _) = parse(lex_with_eof(&source));
            (
                path.file_name().unwrap().to_string_lossy().into_owned(),
                module,
            )
        })
        .collect()
}

struct Stdlib {
    classes: BTreeMap<String, ClassDefinition>,
    protocols: BTreeMap<String, ProtocolDefinition>,
    aliases: BTreeMap<String, TypeAnnotation>,
}

fn load() -> Stdlib {
    let mut s = Stdlib {
        classes: BTreeMap::new(),
        protocols: BTreeMap::new(),
        aliases: BTreeMap::new(),
    };
    for (_, module) in stdlib_modules() {
        for c in module.classes {
            s.classes.insert(c.name.name.to_string(), c);
        }
        for p in module.protocols {
            s.protocols.insert(p.name.name.to_string(), p);
        }
        for a in module.type_aliases {
            s.aliases.insert(a.name.name.to_string(), a.annotation);
        }
    }
    s
}

impl Stdlib {
    /// Instance-side selectors a class defines itself.
    fn own_selectors(&self, class: &str) -> BTreeSet<String> {
        self.classes
            .get(class)
            .map(|c| {
                c.methods
                    .iter()
                    .map(|m| m.selector.name().to_string())
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Does an instance of `class` answer `selector` (own or inherited)?
    fn answers(&self, class: &str, selector: &str) -> bool {
        let mut current = Some(class.to_string());
        let mut seen = BTreeSet::new();
        while let Some(name) = current {
            if !seen.insert(name.clone()) {
                break;
            }
            let Some(def) = self.classes.get(&name) else {
                break;
            };
            if def
                .methods
                .iter()
                .any(|m| m.selector.name().as_str() == selector)
            {
                return true;
            }
            current = def.superclass.as_ref().map(|s| s.name.to_string());
        }
        false
    }

    /// Does `ty` accept a `Duration`, expanding aliases?
    fn accepts_duration(&self, ty: &TypeAnnotation, depth: usize) -> bool {
        if depth > 8 {
            return false;
        }
        match ty {
            TypeAnnotation::Simple(id) => {
                id.name.as_str() == "Duration"
                    || self
                        .aliases
                        .get(id.name.as_str())
                        .is_some_and(|t| self.accepts_duration(t, depth + 1))
            }
            TypeAnnotation::Union { types, .. } => {
                types.iter().any(|t| self.accepts_duration(t, depth + 1))
            }
            _ => false,
        }
    }
}

fn keyword_parts(m: &MethodDefinition) -> Vec<String> {
    match &m.selector {
        MessageSelector::Keyword(parts) => parts.iter().map(|p| p.keyword.to_string()).collect(),
        _ => Vec::new(),
    }
}

fn is_time_keyword(keyword: &str) -> bool {
    let lower = keyword.to_ascii_lowercase();
    TIME_KEYWORD_SUFFIXES.iter().any(|s| lower.ends_with(s))
}

fn check_time_method(s: &Stdlib, owner: &str, m: &MethodDefinition, out: &mut BTreeSet<String>) {
    let selector = m.selector.name();
    for (i, kw) in keyword_parts(m).iter().enumerate() {
        let extra = TIME_PARAMETER_EXTRAS
            .iter()
            .any(|(c, sel, k)| *c == owner && *sel == selector.as_str() && k == kw);
        if !(is_time_keyword(kw) || extra) {
            continue;
        }
        let ok = m
            .parameters
            .get(i)
            .and_then(|p| p.type_annotation.as_ref())
            .is_some_and(|t| s.accepts_duration(t, 0));
        if !ok {
            let side = if m.is_class_method { "class " } else { "" };
            out.insert(format!(
                "time: {owner} {side}{selector} parameter for {kw} does not accept Duration"
            ));
        }
    }
}

fn violations(s: &Stdlib) -> BTreeSet<String> {
    let mut out = BTreeSet::new();

    // Rule 1: List and Array answer the sequence selectors.
    for class in ["List", "Array"] {
        for sel in SEQUENCE_SELECTORS {
            if !s.answers(class, sel) {
                out.insert(format!("sequence: {class} does not answer {sel}"));
            }
        }
    }

    // Rule 2: Enumerable provides the Collection queries.
    let enumerable = s.protocols.get("Enumerable").expect("Enumerable protocol");
    for sel in ENUMERABLE_QUERIES {
        let provided = enumerable
            .provided_methods
            .iter()
            .any(|m| m.selector.name().as_str() == *sel);
        if !provided {
            out.insert(format!("enumerable: Enumerable does not provide {sel}"));
        }
    }

    // Rule 3: time parameters accept Duration.
    for (name, c) in &s.classes {
        for m in c.methods.iter().chain(&c.class_methods) {
            check_time_method(s, name, m, &mut out);
        }
    }
    for (name, p) in &s.protocols {
        for m in &p.provided_methods {
            check_time_method(s, name, m, &mut out);
        }
        for sig in p.method_signatures.iter().chain(&p.class_method_signatures) {
            if let MessageSelector::Keyword(parts) = &sig.selector {
                for (i, part) in parts.iter().enumerate() {
                    let ok = !is_time_keyword(&part.keyword)
                        || sig
                            .parameters
                            .get(i)
                            .and_then(|p| p.type_annotation.as_ref())
                            .is_some_and(|t| s.accepts_duration(t, 0));
                    if !ok {
                        out.insert(format!(
                            "time: {name} {} parameter for {} does not accept Duration",
                            sig.selector.name(),
                            part.keyword
                        ));
                    }
                }
            }
        }
    }
    for (class, setter) in TIME_GENERATED_SETTERS {
        if !s.own_selectors(class).contains(*setter) {
            out.insert(format!(
                "time: {class} {setter} is a generated setter typed from an Integer field; \
                 define it explicitly to accept Duration"
            ));
        }
    }

    // Rule 4: a class that defines `reverse` also defines `reversed`.
    for name in s.classes.keys() {
        let own = s.own_selectors(name);
        if own.contains("reverse") && !own.contains("reversed") {
            out.insert(format!("naming: {name} defines reverse but not reversed"));
        }
    }

    out
}

#[test]
fn collection_has_one_argument_detect() {
    let s = load();
    assert!(
        s.classes["Collection"]
            .methods
            .iter()
            .any(|m| m.selector.name().as_str() == "detect:"),
        "Collection lost `detect:`; drop it from ENUMERABLE_QUERIES (decision 4)"
    );
}

#[test]
fn stdlib_api_consistency_rules_hold_except_named_expected_failures() {
    let s = load();
    let found = violations(&s);
    let expected: BTreeSet<String> = EXPECTED_FAILURES.iter().map(|e| (*e).to_string()).collect();

    let unexpected: Vec<_> = found.difference(&expected).collect();
    let stale: Vec<_> = expected.difference(&found).collect();
    assert!(
        unexpected.is_empty(),
        "stdlib API consistency violations not in EXPECTED_FAILURES:\n{}",
        unexpected
            .iter()
            .map(|v| format!("    \"{v}\","))
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert!(
        stale.is_empty(),
        "EXPECTED_FAILURES entries that no longer violate (delete them):\n{}",
        stale
            .iter()
            .map(|v| format!("    \"{v}\","))
            .collect::<Vec<_>>()
            .join("\n")
    );
}
