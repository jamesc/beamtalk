// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::value_type_codegen::{AutoSlotMethods, compute_auto_slot_methods};
use beamtalk_core::ast::{
    ClassDefinition, ClassKind, DeclaredKeyword, Identifier, Literal, Module, StateDeclaration,
};
use beamtalk_core::source_analysis::Span;
use beamtalk_core::test_helpers::test_support::make_actor_class;

fn s() -> Span {
    Span::new(0, 0)
}

fn make_value_class(name: &str, slots: &[&str]) -> ClassDefinition {
    let state = slots
        .iter()
        .map(|slot_name| StateDeclaration {
            name: Identifier::new(*slot_name, s()),
            type_annotation: None,
            default_value: Some(beamtalk_core::ast::Expression::Literal(
                Literal::Integer(0),
                s(),
            )),
            expect: None,
            comments: beamtalk_core::ast::CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: s(),
        })
        .collect();
    let mut class = ClassDefinition::new(
        Identifier::new(name, s()),
        Identifier::new("Value", s()),
        state,
        vec![],
        s(),
    );
    // Explicitly set class_kind to avoid relying on constructor inference
    class.class_kind = ClassKind::Value;
    class
}

#[test]
fn test_with_star_selector_single_char() {
    assert_eq!(AutoSlotMethods::with_star_selector("x"), "withX:");
}

#[test]
fn test_with_star_selector_multi_char() {
    assert_eq!(
        AutoSlotMethods::with_star_selector("firstName"),
        "withFirstName:"
    );
}

#[test]
fn test_compute_auto_slot_methods_actor_returns_none() {
    let class = make_actor_class("Counter");
    assert!(
        compute_auto_slot_methods(&class).is_none(),
        "actor classes should not get auto slot methods"
    );
}

#[test]
fn test_compute_auto_slot_methods_value_class_returns_getters_setters() {
    let class = make_value_class("Point", &["x", "y"]);
    let auto = compute_auto_slot_methods(&class).unwrap();
    assert!(auto.getters.contains(&"x".to_string()));
    assert!(auto.getters.contains(&"y".to_string()));
    assert!(auto.setters.contains(&"x".to_string()));
    assert!(auto.setters.contains(&"y".to_string()));
}

#[test]
fn test_compute_auto_slot_methods_keyword_constructor() {
    let class = make_value_class("Point", &["x", "y"]);
    let auto = compute_auto_slot_methods(&class).unwrap();
    assert_eq!(
        auto.keyword_constructor,
        Some("x:y:".to_string()),
        "should generate keyword constructor selector from slot names"
    );
}

#[test]
fn test_compute_auto_slot_methods_no_slots() {
    let class = make_value_class("Empty", &[]);
    let auto = compute_auto_slot_methods(&class).unwrap();
    assert!(auto.getters.is_empty());
    assert!(auto.setters.is_empty());
    assert!(auto.keyword_constructor.is_none());
}

// ─── Opaque `native:` representations ────────────────────────────────────

fn parse_one_class(source: &str) -> ClassDefinition {
    beamtalk_core::test_helpers::test_support::parse_bt(source)
        .classes
        .into_iter()
        .next()
        .expect("source should declare a class")
}

#[test]
fn test_bt_2998_has_opaque_native_representation() {
    use super::has_opaque_native_representation;

    // `native:` + no declared fields — nothing for `basicNew` to build.
    assert!(has_opaque_native_representation(&parse_one_class(
        "Value subclass: Uuid native: beamtalk_uuid\n  version -> Integer => self delegate\n"
    )));
    // `native:` but carrying its own fields (`Package`, `SupervisionNode`).
    assert!(!has_opaque_native_representation(&parse_one_class(
        "Value subclass: Package native: beamtalk_package\n  field: name = nil\n"
    )));
    // Plain value type — the ordinary `basicNew` case.
    assert!(!has_opaque_native_representation(&parse_one_class(
        "Value subclass: Point\n  field: x = 0\n"
    )));
    // Fieldless *non*-native class: `~{'$beamtalk_class' => 'X'}~` is its
    // complete and correct instance, so it stays constructible.
    assert!(!has_opaque_native_representation(&parse_one_class(
        "Value subclass: Marker\n  isMarker -> Boolean => true\n"
    )));
}

#[test]
fn test_bt_2998_constructor_selectors_filter_by_return_type() {
    use super::native_constructor_selectors;

    let class = parse_one_class(concat!(
        "Value subclass: DateTime native: beamtalk_datetime\n",
        "  class new: _ :: Object -> Nil => self error: \"nope\"\n",
        "  class sealed now -> DateTime => self delegate\n",
        "  class sealed monotonicNow -> Integer => self delegate\n",
        "  class sealed year: y :: Integer month: m :: Integer -> DateTime => self delegate\n",
        "  class sealed parse: s :: String -> Result(DateTime, Error) => self delegate\n",
        "  year -> Integer => self delegate\n",
    ));
    assert_eq!(
        native_constructor_selectors(&class, "new"),
        vec![
            "now".to_string(),
            "year:month:".to_string(),
            "parse:".to_string(),
        ],
        "only class methods returning the class (directly or nested in a \
         generic) count; `monotonicNow`, the `-> Nil` `new:` refusal and \
         the instance-side `year` do not"
    );
}

#[test]
fn test_bt_2998_constructor_selectors_keep_the_other_new() {
    use super::native_constructor_selectors;

    // `Queue` declares a working `new` but no `new:`, so its `new:`
    // refusal must point back at `Queue new` rather than claim it has no
    // constructor at all.
    let class = parse_one_class(concat!(
        "Value subclass: Queue native: beamtalk_queue\n",
        "  class sealed new -> Queue => self delegate\n",
    ));
    assert_eq!(
        native_constructor_selectors(&class, "new:"),
        vec!["new".to_string()]
    );
    assert!(native_constructor_selectors(&class, "new").is_empty());
}

#[test]
fn test_bt_2998_constructor_selectors_empty_for_namespace_class() {
    use super::native_constructor_selectors;

    // Namespace-style native classes (`Console`, `System`, …) have no
    // constructor to point at.
    let class = parse_one_class(concat!(
        "Object subclass: Console native: beamtalk_console\n",
        "  class sealed log: msg :: String -> Nil => self delegate\n",
    ));
    assert!(native_constructor_selectors(&class, "new").is_empty());
}

#[test]
fn test_bt_2998_native_new_error_hint_names_constructors() {
    let class = parse_one_class(concat!(
        "Value subclass: Uuid native: beamtalk_uuid\n",
        "  class sealed v4 -> Uuid => self delegate\n",
        "  class sealed fromString: s :: String -> Uuid => self delegate\n",
    ));
    let hint = CoreErlangGenerator::native_new_error_hint(&class, "new");
    assert_eq!(
        hint,
        "Uuid instances are built by the beamtalk_uuid module, not from field \
         defaults — `new` cannot produce a usable one. Use one of: Uuid v4, \
         Uuid fromString:"
    );
}

#[test]
fn test_bt_2998_native_new_error_hint_without_constructors() {
    let class = parse_one_class(concat!(
        "Object subclass: Console native: beamtalk_console\n",
        "  class sealed log: msg :: String -> Nil => self delegate\n",
    ));
    let hint = CoreErlangGenerator::native_new_error_hint(&class, "new:");
    assert!(
        hint.ends_with("`new:` cannot produce a usable one. It has no class-side constructor."),
        "hint should say there is no constructor. Got: {hint}"
    );
}

#[test]
fn test_bt_2998_native_new_error_hint_caps_long_constructor_lists() {
    use std::fmt::Write as _;

    let mut source = String::from("Value subclass: Wide native: beamtalk_wide\n");
    for i in 0..(super::MAX_HINTED_CONSTRUCTORS + 3) {
        let _ = writeln!(source, "  class sealed make{i} -> Wide => self delegate");
    }
    let hint = CoreErlangGenerator::native_new_error_hint(&parse_one_class(&source), "new");
    assert!(
        hint.contains("Wide make0, ") && hint.ends_with(", and 3 more"),
        "hint should cap the list and count the remainder. Got: {hint}"
    );
    assert!(
        !hint.contains("Wide make6"),
        "hint should not list beyond the cap. Got: {hint}"
    );
}

#[test]
fn test_generate_value_type_module_includes_class_name() {
    let class = make_value_class("Point", &["x", "y"]);
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let mut generator = CoreErlangGenerator::new("point");
    let doc = generator.generate_value_type_module(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'Point'"),
        "generated module should reference class name. Got: {output}"
    );
    assert!(
        output.contains("'new'"),
        "generated module should include new constructor. Got: {output}"
    );
}

/// ADR 0119 step 0: `is_known_stdlib_type` must agree with
/// `ClassHierarchy::with_builtins()` — every real built-in class name
/// registered there (via `generated_builtins.rs::is_generated_builtin_class`,
/// the `beamtalk build-stdlib`-generated table of *actual* parsed
/// classes) must be recognised as a known stdlib type, with the one
/// expected exception, `'Future'`: a runtime-only built-in with no
/// `stdlib/src/Future.bt` source (see `builtins.rs::is_builtin_class`'s
/// doc) that compiles to the hand-written `beamtalk_future` native module
/// (ADR 0056), not `bt@stdlib@future`.
///
/// This used to be a superset check against `STDLIB_CLASS_NAMES` (a
/// `build.rs` file-stem directory scan that also matched protocol-only
/// files declaring no class, e.g. `printable.bt`). Now that
/// `is_known_stdlib_type` delegates directly to
/// `is_generated_builtin_class`, the two can never disagree except for
/// the documented `Future` exception — so this is a strict equality
/// check, not a superset one.
#[test]
fn test_is_known_stdlib_type_matches_builtin_classes() {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;

    let hierarchy = ClassHierarchy::with_builtins();
    for name in hierarchy.class_names() {
        let expected = name != "Future";
        assert_eq!(
            CoreErlangGenerator::is_known_stdlib_type(name),
            expected,
            "is_known_stdlib_type('{name}') should be {expected} — every \
             real generated_builtins.rs class is a known stdlib type \
             except the runtime-only 'Future' built-in"
        );
    }
}

// ── ADR 0119: registry-based `compiled_module_name*` ─────────────────────

#[test]
fn test_compiled_module_name_registry_hit_takes_precedence_over_heuristic() {
    // A class present in class_module_index resolves through the
    // registry even when it lives in a subdirectory the bare
    // `bt@{pkg}@{snake}` fallback convention could never reconstruct.
    let mut generator = CoreErlangGenerator::new("bt@sicp@main");
    let mut index = std::collections::HashMap::new();
    index.insert("SchemeEnv".to_string(), "bt@sicp@scheme@env".to_string());
    generator.set_class_module_index(index);

    assert_eq!(
        generator.compiled_module_name("SchemeEnv"),
        "bt@sicp@scheme@env"
    );
}

#[test]
fn test_compiled_module_name_falls_back_to_stdlib_convention_on_miss() {
    // A stdlib class absent from class_module_index still resolves via
    // the closed-form bt@stdlib@{snake} convention (ADR 0100 open-world
    // policy: a registry miss is not an error).
    let generator = CoreErlangGenerator::new("bt@my_app@main");
    assert_eq!(
        generator.compiled_module_name("Dictionary"),
        "bt@stdlib@dictionary"
    );
}

#[test]
fn test_compiled_module_name_falls_back_to_package_convention_on_miss() {
    // A user-defined class absent from class_module_index falls back to
    // the package-root convention (own package name, no subdirectory
    // info to recover — the same best-effort guess the deleted
    // `user_package_prefix` made, now derived from a typed `PackageId`
    // instead of re-parsing `self.module_name`).
    let generator = CoreErlangGenerator::new("bt@my_app@sub@main");
    assert_eq!(generator.compiled_module_name("Helper"), "bt@my_app@helper");
}

#[test]
fn test_compiled_module_name_falls_back_to_single_file_convention_on_miss() {
    let generator = CoreErlangGenerator::new("counter");
    assert_eq!(generator.compiled_module_name("Helper"), "bt@helper");
}

#[test]
fn test_compiled_module_name_qualified_resolves_subdirectory_class_via_registry() {
    // ADR 0119 Context item 5: an explicitly-qualified `pkg@Class`
    // reference to a class in a package subdirectory used to bypass
    // class_module_index entirely and compose `bt@{pkg}@{snake}`
    // directly — disagreeing with the same class's unqualified
    // resolution. Routing through the registry closes that divergence.
    let mut generator = CoreErlangGenerator::new("bt@sicp@main");
    let mut index = std::collections::HashMap::new();
    index.insert("SchemeEnv".to_string(), "bt@sicp@scheme@env".to_string());
    generator.set_class_module_index(index);

    assert_eq!(
        generator.compiled_module_name_qualified("SchemeEnv", Some("sicp")),
        generator.compiled_module_name("SchemeEnv"),
        "qualified and unqualified references to the same subdirectory \
         class must resolve identically"
    );
    assert_eq!(
        generator.compiled_module_name_qualified("SchemeEnv", Some("sicp")),
        "bt@sicp@scheme@env"
    );
}

#[test]
fn test_compiled_module_name_qualified_falls_back_on_registry_miss() {
    // No class_module_index entry for the referenced package/class pair
    // (e.g. a genuine cross-package reference this registry doesn't
    // cover) — falls back to `resolve_qualified_module_name`'s
    // deterministic `bt@{package}@{snake}` composition, unchanged.
    let generator = CoreErlangGenerator::new("bt@my_app@main");
    assert_eq!(
        generator.compiled_module_name_qualified("Parser", Some("json")),
        "bt@json@parser"
    );
}

#[test]
fn test_compiled_module_name_qualified_falls_back_when_index_only_covers_own_package() {
    // set_class_module_index keys every entry under this generation
    // unit's own PackageId (see that method's doc) — including a
    // dependency's classes merged in by beamtalk-cli's path-dependency
    // build. A qualified reference naming that *other* package therefore
    // still misses the registry and must fall back to
    // resolve_qualified_module_name's convention, exactly as before this
    // ADR: it never regresses to something worse than a miss.
    let mut generator = CoreErlangGenerator::new("bt@my_app@main");
    let mut index = std::collections::HashMap::new();
    // A dependency class merged into this package's index, as
    // beamtalk-cli's deps/path.rs does — real module name deliberately
    // does NOT follow the bt@json@{snake} convention, so a false-positive
    // registry hit would be obviously wrong here.
    index.insert("Parser".to_string(), "bt@json@v2@parser".to_string());
    generator.set_class_module_index(index);

    assert_eq!(
        generator.compiled_module_name_qualified("Parser", Some("json")),
        "bt@json@parser",
        "a qualified reference to a different package must not pick up \
         an index entry keyed under this unit's own package"
    );
}

#[test]
fn test_compiled_module_name_qualified_without_package_delegates_to_unqualified() {
    let generator = CoreErlangGenerator::new("bt@my_app@main");
    assert_eq!(
        generator.compiled_module_name_qualified("Helper", None),
        generator.compiled_module_name("Helper")
    );
}
