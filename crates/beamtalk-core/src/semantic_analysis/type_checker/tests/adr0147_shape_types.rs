// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0147 (BT-4729): `InferredType::Shape` — resolution from annotations
//! and aliases, display, and the `Dictionary` view every non-keyed send sees.
//! Assignability is BT-4730; literal inference and keyed operations come
//! later.

use super::common::*;

fn analyse_source(source: &str) -> crate::semantic_analysis::AnalysisResult {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module)
}

fn checker_messages(source: &str) -> Vec<String> {
    analyse_source(source)
        .diagnostics
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .map(|d| d.message.to_string())
        .collect()
}

/// A source with one method per case; `{body}` is spliced into class `Probe`.
fn probe(body: &str) -> Vec<String> {
    checker_messages(&format!("Object subclass: Probe\n{body}\n"))
}

#[test]
fn shape_receiver_answers_dictionary_messages_without_warnings() {
    let msgs = probe(
        "  keysOf: opts :: #{#a => Integer, ?#b => String} => opts keys\n  \
           sizeOf: opts :: #{#a => Integer} => opts size\n  \
           openKeys: opts :: #{#a => Integer, ...} => opts keys\n  \
           emptyKeys: opts :: #{} => opts keys\n  \
           nested: opts :: #{#inner => #{#x => Integer}} => opts size",
    );
    assert!(msgs.is_empty(), "no warnings expected, got: {msgs:#?}");
}

#[test]
fn shape_receiver_unknown_message_reports_dnu_against_dictionary() {
    let msgs = probe("  bad: opts :: #{#a => Integer} => opts frobnicate");
    assert_eq!(
        msgs,
        vec!["Dictionary does not understand 'frobnicate'".to_string()],
        "a shape receiver is the Dictionary view for every other send"
    );
}

#[test]
fn shape_union_receiver_resolves_each_member_as_dictionary() {
    let msgs = probe(
        "  ok: opts :: #{#a => Integer} | #{#b => String} => opts size\n  \
           bad: opts :: #{#a => Integer} | #{#b => String} => opts frobnicate",
    );
    assert_eq!(msgs.len(), 1, "only the DNU is expected, got: {msgs:#?}");
    assert!(msgs[0].contains("frobnicate"), "{msgs:#?}");
}

#[test]
fn dictionary_and_shape_interoperate_silently() {
    // Assignability proper is BT-4730; until then a shape and a `Dictionary`
    // flow either way without a diagnostic, exactly as the `Dictionary`
    // fallback behaved.
    let msgs = probe(
        "  takesShape: opts :: #{#a => Integer} => opts\n  \
           takesDict: d :: Dictionary => d\n  \
           fromDict => self takesShape: Dictionary new\n  \
           toDict: opts :: #{#a => Integer} => self takesDict: opts",
    );
    assert!(msgs.is_empty(), "no warnings expected, got: {msgs:#?}");
}

#[test]
fn non_dictionary_argument_to_shape_parameter_is_still_reported() {
    let msgs = probe(
        "  takesShape: opts :: #{#a => Integer} => opts\n  \
           wrong => self takesShape: 3",
    );
    assert_eq!(msgs.len(), 1, "got: {msgs:#?}");
    assert!(msgs[0].contains("Integer"), "{msgs:#?}");
}

// ── Hot reload (ADR 0108's existing trigger) ───────────────────────────────

#[test]
fn shape_alias_use_sites_record_the_alias_dependency() {
    // The hot-reload re-check trigger keys on `referenced_aliases`: every
    // alias a module's annotations name. A shape alias must be recorded from
    // each position a shape can sit in — plain parameter, generic argument,
    // inside an anonymous shape's field type, return type and state slot.
    let result = analyse_source(
        "type Options = #{#port => Integer}\n\
         type Mode = #fast | #slow\n\
         type Tagged = #{#tag => Integer}\n\
         Object subclass: Server\n  \
           state: conf :: Tagged = nil\n  \
           start: opts :: Options => opts\n  \
           many: opts :: List(Options) => opts\n  \
           nested: opts :: #{#mode => Mode} => opts\n",
    );
    let referenced: Vec<&str> = result
        .referenced_aliases
        .iter()
        .map(EcoString::as_str)
        .collect();
    for name in ["Options", "Mode", "Tagged"] {
        assert!(
            referenced.contains(&name),
            "{name} missing from referenced_aliases: {referenced:?}"
        );
    }
}

#[test]
fn redefining_a_shape_alias_changes_what_its_use_sites_resolve_to() {
    use crate::semantic_analysis::alias_registry::AliasRegistry;
    use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
    use crate::semantic_analysis::type_checker::type_resolver::resolve_type_annotation_with_alias_deps;

    let parse = |src: &str| {
        let (module, diags) =
            crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
        assert!(diags.is_empty(), "{diags:?}");
        module
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let protocols = ProtocolRegistry::new();

    let mut registry = AliasRegistry::new();
    let first = parse("type Options = #{#port => Integer}\n");
    assert!(
        registry
            .register_module(&first, &hierarchy, &protocols)
            .is_empty()
    );
    let reference = crate::ast::TypeAnnotation::Simple(crate::ast::Identifier {
        name: "Options".into(),
        span: Span::default(),
    });
    let (before, deps) =
        resolve_type_annotation_with_alias_deps(&reference, &HashMap::new(), None, Some(&registry));
    assert_eq!(deps, vec![EcoString::from("Options")]);
    assert_eq!(before.display_for_diagnostic().unwrap(), "Options");
    assert_eq!(before.display_name(), "#{#port => Integer}");

    // The live redefinition lands in the same registry; the same use site now
    // resolves to the new shape, and its dependency edge is unchanged — which
    // is what lets ADR 0108's trigger find and re-check it.
    let second = parse("type Options = #{#port => Integer, ?#host => String}\n");
    assert!(
        registry
            .redefine_alias(&second.type_aliases[0], &hierarchy, &protocols)
            .is_empty()
    );
    let (after, deps) =
        resolve_type_annotation_with_alias_deps(&reference, &HashMap::new(), None, Some(&registry));
    assert_eq!(deps, vec![EcoString::from("Options")]);
    assert_eq!(
        after.display_name(),
        "#{#port => Integer, ?#host => String}"
    );
    assert_ne!(before, after);
}
