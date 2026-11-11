// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0144 (BT-4435): `P class` for a protocol `P` is the class object of
//! any class conforming to `P`. `C class <: P class` iff
//! `ProtocolRegistry::check_conformance(C, P)` is `Ok`; sends resolve against
//! `P`'s `class` requirements, then the class-object protocol
//! (`Class` → `Behaviour` → `Object`).

use super::common::*;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

/// Every diagnostic `analyse` reports, after asserting the source parses.
fn diagnostics(source: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module).diagnostics
}

/// Type and DNU diagnostics only (the ones this feature produces).
fn checker_diagnostics(source: &str) -> Vec<Diagnostic> {
    diagnostics(source)
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .collect()
}

fn messages(diags: &[Diagnostic]) -> Vec<String> {
    diags.iter().map(|d| d.message.to_string()).collect()
}

/// A protocol with two `class` requirements and two sealed conformers, plus
/// `Integer` as a non-conformer. `{body}` is appended verbatim.
fn wire_source(body: &str) -> String {
    format!(
        r#"
Protocol define: Wire
  class send: data :: Object -> Result(Nil, Object)
  class label -> String

sealed Object subclass: Tcp
  class send: data :: Object -> Result(Nil, Object) => Result ok: nil
  class label -> String => "tcp"

sealed Object subclass: Ssl
  class send: data :: Object -> Result(Nil, Object) => Result ok: nil
  class label -> String => "ssl"
{body}"#
    )
}

#[test]
fn class_of_protocol_resolves_to_protocol_metatype() {
    let source = wire_source("");
    let tokens = crate::source_analysis::lex_with_eof(&source);
    let (module, _) = crate::source_analysis::parse(tokens);
    let mut registry = ProtocolRegistry::new();
    let hierarchy = ClassHierarchy::with_builtins();
    let _ = registry.register_module(&module, &hierarchy);
    let ann = TypeAnnotation::ClassOf {
        class_name: ident("Wire"),
        span: span(),
    };
    let resolved = type_resolver::resolve_type_annotation(
        &ann,
        &type_resolver::SubstitutionMap::new(),
        Some(&registry),
        None,
    );
    assert_eq!(resolved, InferredType::protocol_meta("Wire"));
    assert_eq!(resolved.display_name(), "Wire class");
    // A class name is unchanged (ADR 0083).
    let class_ann = TypeAnnotation::ClassOf {
        class_name: ident("Integer"),
        span: span(),
    };
    let class_resolved = type_resolver::resolve_type_annotation(
        &class_ann,
        &type_resolver::SubstitutionMap::new(),
        Some(&registry),
        None,
    );
    assert_eq!(class_resolved, InferredType::meta("Integer"));
}

#[test]
fn conforming_class_assigns_to_protocol_metatype_field() {
    let source = wire_source(
        r"
Value subclass: Holder
  field: wire :: Wire class = Tcp

  useSsl -> Holder => self withWire: Ssl
",
    );
    let diags = checker_diagnostics(&source);
    assert!(diags.is_empty(), "conformers are accepted: {diags:#?}");
}

#[test]
fn non_conforming_class_warns_with_missing_class_selectors() {
    let source = wire_source(
        r"
Value subclass: Holder
  field: wire :: Wire class = Integer
",
    );
    let diags = checker_diagnostics(&source);
    let warning = diags
        .iter()
        .find(|d| d.message == "Integer class does not conform to Wire class")
        .unwrap_or_else(|| panic!("conformance warning expected: {diags:#?}"));
    assert_eq!(warning.severity, crate::source_analysis::Severity::Warning);
    let hint = warning.hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("class send:") && hint.contains("class label"),
        "the hint names the missing class-side selectors: {hint}"
    );
}

#[test]
fn non_conforming_local_and_argument_warn() {
    let source = wire_source(
        r"
Object subclass: Caller
  take: w :: Wire class -> Wire class => w
  run =>
    local :: Wire class := Integer
    self take: String
    local
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    assert!(
        msgs.contains(&"Integer class does not conform to Wire class".to_string()),
        "local annotation: {msgs:#?}"
    );
    assert!(
        msgs.contains(&"String class does not conform to Wire class".to_string()),
        "argument: {msgs:#?}"
    );
}

#[test]
fn protocol_metatype_flows_into_class_behaviour_and_object() {
    let source = wire_source(
        r"
Object subclass: Caller
  asClass: w :: Wire class -> Class => w
  run: w :: Wire class =>
    c :: Class := w
    b :: Behaviour := w
    o :: Object := w
    self asClass: w
    c
",
    );
    let diags = checker_diagnostics(&source);
    assert!(diags.is_empty(), "P class <: Class: {diags:#?}");
}

#[test]
fn class_into_protocol_metatype_is_a_narrowing_mismatch() {
    let source = wire_source(
        r"
Object subclass: Caller
  run: c :: Class =>
    w :: Wire class := c
    w
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    assert!(
        msgs.iter()
            .any(|m| m.contains("Type mismatch") && m.contains("Wire class")),
        "Class → Wire class is reported: {msgs:#?}"
    );
}

#[test]
fn send_resolves_from_class_requirement_return_type() {
    // `(w send: d) isOk` resolves on `Result`; a misspelled selector on the
    // result and a mistyped `String` binding prove the result types.
    let source = wire_source(
        r"
Object subclass: Caller
  run: w :: Wire class -> Boolean =>
    n :: Integer := w label
    (w send: 1) bogusSelector
    (w send: 2) isOk
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    assert!(
        msgs.contains(&"Type mismatch: declared as Integer, got String".to_string()),
        "`label` is typed String: {msgs:#?}"
    );
    assert!(
        msgs.iter()
            .any(|m| m.starts_with("Result") && m.contains("'bogusSelector'")),
        "`send:` is typed Result: {msgs:#?}"
    );
    assert!(
        !msgs.iter().any(|m| m.contains("'isOk'")),
        "`isOk` resolves on Result: {msgs:#?}"
    );
}

#[test]
fn send_argument_checked_against_requirement_parameter_type() {
    let source = r#"
Protocol define: Sized
  class ofSize: n :: Integer -> Object

sealed Object subclass: Box
  class ofSize: n :: Integer -> Object => n

Object subclass: Caller
  run: s :: Sized class => s ofSize: "big"
"#;
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.iter().any(|m| m.contains("Argument 1 of 'ofSize:'")
            && m.contains("Sized class")
            && m.contains("expects Integer")),
        "argument checked against the requirement: {msgs:#?}"
    );
}

#[test]
fn class_object_protocol_fallback_resolves() {
    let source = wire_source(
        r"
Object subclass: Caller
  run: w :: Wire class -> Boolean =>
    s :: Symbol := w name
    w printString size
    w respondsTo: #label
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        diags.is_empty(),
        "`name` / `printString` / `respondsTo:` resolve on the tower: {diags:#?}"
    );
}

#[test]
fn unknown_selector_is_does_not_understand() {
    let source = wire_source(
        r"
Object subclass: Caller
  run: w :: Wire class => w sned: 1
",
    );
    let diags = checker_diagnostics(&source);
    let dnu = diags
        .iter()
        .find(|d| d.message == "Wire class does not understand 'sned:'")
        .unwrap_or_else(|| panic!("DNU expected: {diags:#?}"));
    // Same severity as a `:: P` instance receiver's DNU.
    assert_eq!(dnu.severity, crate::source_analysis::Severity::Hint);
}

#[test]
fn new_infers_the_protocol_instance_type() {
    // No `class new` requirement: `new` / `basicNew` answer `P` (an instance
    // of some conformer), so binding it to an unrelated class is reported.
    let source = r"
Protocol define: Shape
  area -> Integer
  class unit -> Integer

Object subclass: Square
  area -> Integer => 1
  class unit -> Integer => 1

Object subclass: Caller
  run: s :: Shape class =>
    a :: Integer := s new
    b :: Integer := s basicNew
    a
";
    let msgs = messages(&checker_diagnostics(source));
    let shape_mismatches = msgs
        .iter()
        .filter(|m| m.as_str() == "Type mismatch: declared as Integer, got Shape")
        .count();
    assert_eq!(shape_mismatches, 2, "new / basicNew infer Shape: {msgs:#?}");
}

#[test]
fn class_new_requirement_return_type_wins() {
    let source = r"
Protocol define: Maker
  class new -> Integer

sealed Object subclass: IntMaker
  class new -> Integer => 1

Object subclass: Caller
  run: m :: Maker class =>
    s :: String := m new
    s
";
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.contains(&"Type mismatch: declared as String, got Integer".to_string()),
        "the requirement's return type is used: {msgs:#?}"
    );
}

#[test]
fn nilable_protocol_metatype_narrows_after_nil_check() {
    let source = wire_source(
        r"
Value subclass: Holder
  field: wire :: Wire class | Nil = nil

  run =>
    self.wire isNil ifFalse: [self.wire sned: 1]
  local: w :: Wire class | Nil =>
    w isNil ifFalse: [w sned: 1]
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    let dnus = msgs
        .iter()
        .filter(|m| m.as_str() == "Wire class does not understand 'sned:'")
        .count();
    assert_eq!(
        dnus, 2,
        "narrowed to Wire class in both the field and the local: {msgs:#?}"
    );
    let source_ok = wire_source(
        r"
Value subclass: Holder
  field: wire :: Wire class | Nil = nil

  useTcp -> Holder => self withWire: Tcp
",
    );
    let diags = checker_diagnostics(&source_ok);
    assert!(
        diags.is_empty(),
        "`Tcp` fits `Wire class | Nil`: {diags:#?}"
    );
}

#[test]
fn intersection_requires_conformance_to_both() {
    let source = r#"
Protocol define: Labelled
  class label -> String

Protocol define: Counted
  class count -> Integer

sealed Object subclass: Both
  class label -> String => "b"
  class count -> Integer => 1

sealed Object subclass: OnlyLabel
  class label -> String => "l"

Object subclass: Caller
  take: c :: Labelled class & Counted class => c
  run =>
    self take: Both
    self take: OnlyLabel
"#;
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.contains(&"OnlyLabel class does not conform to Counted class".to_string()),
        "the missing half is reported: {msgs:#?}"
    );
    assert!(
        !msgs.iter().any(|m| m.contains("Both class")),
        "a class conforming to both is accepted: {msgs:#?}"
    );
}

#[test]
fn extending_protocol_metatype_is_assignable_to_base_protocol_metatype() {
    let source = r"
Protocol define: Base
  class base -> Integer

Protocol define: Derived
  extending: Base
  class derived -> Integer

Object subclass: Caller
  run: d :: Derived class =>
    b :: Base class := d
    d base
";
    let diags = checker_diagnostics(source);
    assert!(
        diags.is_empty(),
        "Derived class <: Base class, and inherited requirements resolve: {diags:#?}"
    );
}

#[test]
fn open_world_conformer_is_accepted() {
    // A class with a `doesNotUnderstand:` override is open-world (ADR 0100):
    // the checker cannot prove it misses a selector, so it conforms.
    let source = wire_source(
        r"
Object subclass: Anything
  doesNotUnderstand: msg :: Object args: args :: Object => nil
  class doesNotUnderstand: msg :: Object args: args :: Object => nil

Value subclass: Holder
  field: wire :: Wire class = Anything
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        !diags.iter().any(|d| d.message.contains("does not conform")),
        "an open-world class conforms: {diags:#?}"
    );
}

#[test]
fn cross_file_conformer_is_accepted() {
    // A superclass outside this file makes the class's surface incomplete
    // (ADR 0100 Rule 1): it conforms by default.
    let source = wire_source(
        r"
ElsewhereDefined subclass: Remote

Value subclass: Holder
  field: wire :: Wire class = Remote
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        !diags.iter().any(|d| d.message.contains("does not conform")),
        "a cross-file conformer is accepted: {diags:#?}"
    );
}

#[test]
fn generic_protocol_metatype_is_name_only() {
    // `Collection(E) class` is not a type: the parser rejects it exactly as
    // it rejects `List(E) class`.
    for source in [
        "Object subclass: A\n  run: c :: Collection(Integer) class => c\n",
        "Object subclass: A\n  run: c :: List(Integer) class => c\n",
    ] {
        let tokens = crate::source_analysis::lex_with_eof(source);
        let (_, parse_diags) = crate::source_analysis::parse(tokens);
        assert!(
            !parse_diags.is_empty(),
            "a parameterised metatype is rejected: {source}"
        );
    }
}

#[test]
fn protocol_literal_and_protocol_typed_values_are_unaffected() {
    // A value that *is* a protocol object is typed `:: Protocol`; the bare
    // protocol literal keeps its ADR 0083 metatype, so its reflective sends
    // are unchanged.
    let source = wire_source(
        r"
Object subclass: Caller
  run =>
    p := Wire
    p requiredMethods
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        !diags.iter().any(|d| d.message.contains("Wire class")),
        "the protocol object is not a `Wire class` value: {diags:#?}"
    );
}

#[test]
fn protocol_typed_requirement_parameter_is_conformance_checked() {
    let source = r#"
Protocol define: Named
  label -> String

Protocol define: Registry
  class register: item :: Named -> Integer

sealed Object subclass: Store
  class register: item :: Named -> Integer => 1

Object subclass: Thing
  label -> String => "t"

Object subclass: Caller
  run: r :: Registry class =>
    r register: Thing new
    r register: 42
"#;
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.contains(&"Integer does not conform to protocol Named".to_string()),
        "a `:: Named` requirement parameter is checked: {msgs:#?}"
    );
    assert!(
        !msgs.iter().any(|m| m.starts_with("Thing")),
        "a conforming argument passes: {msgs:#?}"
    );
}

#[test]
fn intersection_of_protocol_metatypes_is_not_never() {
    let resolved = InferredType::intersect(
        &InferredType::protocol_meta("Labelled"),
        &InferredType::protocol_meta("Counted"),
        TypeProvenance::Inferred(span()),
        None,
        None,
    );
    assert!(
        matches!(resolved, InferredType::Intersection { ref members, .. } if members.len() == 2),
        "`Labelled class & Counted class` is irreducible: {resolved:?}"
    );
}

#[test]
fn protocol_metatype_assignability_between_protocols_is_structural() {
    // `Wire` restates `Labelled`'s requirement without `extending:` it:
    // every `Wire` conformer conforms to `Labelled`, so `Wire class` flows
    // into `Labelled class` — the same structural rule as `check_conformance`.
    // `Counted` needs a class selector `Wire` lacks, so it stays a mismatch.
    let source = wire_source(
        r"
Protocol define: Labelled
  class label -> String

Protocol define: Counted
  class count -> Integer

Object subclass: Caller
  take: l :: Labelled class => l label
  count: c :: Counted class => c count
  run: w :: Wire class =>
    self take: w
    self count: w
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    assert!(
        !msgs.iter().any(|m| m.contains("Labelled class")),
        "Wire class <: Labelled class structurally: {msgs:#?}"
    );
    assert!(
        msgs.iter().any(|m| m.contains("Counted class")),
        "Wire class is not assignable to Counted class: {msgs:#?}"
    );
}

#[test]
fn unannotated_accessor_keeps_protocol_metatype() {
    // An unannotated method's inferred `P class` return type is cached like
    // `C class`, so sends through the accessor are checked against `Wire`.
    let source = wire_source(
        r"
Value subclass: Holder
  field: wire :: Wire class = Tcp

  transport => self.wire
  run => self transport sned: 1
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    assert!(
        msgs.contains(&"Wire class does not understand 'sned:'".to_string()),
        "the accessor answers Wire class, not Dynamic: {msgs:#?}"
    );
}

#[test]
fn protocol_literal_is_not_a_protocol_metatype_value() {
    // The protocol object `Wire` does not answer `Wire`'s class
    // requirements, so it is not the class object of a conformer — at a
    // field default, an argument and an annotated local.
    let source = wire_source(
        r"
Value subclass: Holder
  field: wire :: Wire class = Wire

Object subclass: Caller
  take: w :: Wire class => w label
  run => self take: Wire
  local =>
    w :: Wire class := Wire
    w
",
    );
    let diags = checker_diagnostics(&source);
    let lines: Vec<usize> = diags
        .iter()
        .filter(|d| d.message.contains("Wire class") && !d.message.contains("understand"))
        .map(|d| source[..d.span.start() as usize].lines().count())
        .collect();
    let line_of = |needle: &str| {
        source[..source.find(needle).expect("fixture line")]
            .lines()
            .count()
    };
    for (site, needle) in [
        ("field default", "field: wire"),
        ("argument", "run => self take: Wire"),
        ("annotated local", "w :: Wire class := Wire"),
    ] {
        assert!(
            lines.contains(&line_of(needle)),
            "{site} rejects the protocol literal: {diags:#?}"
        );
    }
}

#[test]
fn protocol_literal_in_a_union_is_not_a_protocol_metatype_value() {
    // BT-4445: `Meta{Wire}` (the protocol literal) and `ProtocolMeta{Wire}`
    // both spell `Wire class`; a union carrying the literal, or a
    // non-conformer's class object, must not slip past a declared
    // `Wire class | Nil` on the name comparison.
    let source = wire_source(
        r"
Object subclass: Caller
  literal: flag :: Boolean =>
    w :: Wire class | Nil := flag ifTrue: [Wire] ifFalse: [nil]
    w
  narrowLiteral: flag :: Boolean =>
    w :: Wire class := flag ifTrue: [Wire] ifFalse: [nil]
    w
  widenWithNil: flag :: Boolean =>
    w :: Wire class := flag ifTrue: [Tcp] ifFalse: [nil]
    w
  nonconformer: flag :: Boolean =>
    w :: Wire class | Nil := flag ifTrue: [Integer] ifFalse: [nil]
    w
  fine: flag :: Boolean =>
    a :: Wire class | Nil := flag ifTrue: [Tcp] ifFalse: [nil]
    b :: Wire class := flag ifTrue: [Tcp] ifFalse: [Ssl]
    c :: Object := flag ifTrue: [Wire] ifFalse: [nil]
    d :: Class | Nil := flag ifTrue: [Integer] ifFalse: [nil]
    e :: Integer | Nil := flag ifTrue: [3] ifFalse: [nil]
    f :: Dynamic := Integer
    g :: Dynamic := 3
    h :: Object := Wire
    i :: Tcp class := flag ifTrue: [Tcp] ifFalse: [nil]
    k :: Tcp class | Nil := flag ifTrue: [Tcp] ifFalse: [3]
    a
",
    );
    let diags = checker_diagnostics(&source);
    let line_of = |needle: &str| {
        source[..source.find(needle).expect("fixture line")]
            .lines()
            .count()
    };
    let at = |needle: &str| -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| source[..d.span.start() as usize].lines().count() == line_of(needle))
            .collect()
    };
    let literal = at("ifTrue: [Wire] ifFalse: [nil]\n    w");
    assert!(
        literal
            .iter()
            .any(|d| d.message.contains("Wire class | Nil")),
        "the protocol literal in a union is refused: {diags:#?}"
    );
    assert!(
        !at("w :: Wire class := flag ifTrue: [Wire]").is_empty(),
        "narrowing a union carrying the protocol literal to `Wire class` is refused: {diags:#?}"
    );
    assert!(
        !at("w :: Wire class := flag ifTrue: [Tcp]").is_empty(),
        "`Tcp class | Nil` into `Wire class` neither widens nor narrows: {diags:#?}"
    );
    let nonconformer = at("ifTrue: [Integer] ifFalse: [nil]\n    w");
    assert!(
        nonconformer.iter().any(|d| d
            .message
            .contains("Integer class does not conform to Wire class")),
        "a non-conformer in a union reports its non-conformance: {diags:#?}"
    );
    let fine = line_of("fine: flag");
    assert!(
        diags
            .iter()
            .all(|d| source[..d.span.start() as usize].lines().count() < fine),
        "conformers, `Object`, `Class`, `Dynamic` and narrowing accept class objects in a union: {diags:#?}"
    );
}

#[test]
fn class_objects_in_a_union_are_judged_at_arguments_fields_defaults_and_returns() {
    // BT-4454: the slot rule applies to each class-object member of a union
    // value at the other checked sites, not only annotated locals.
    let source = wire_source(
        r"
typed Actor subclass: Caller
  state: slot :: Wire class | Nil = nil
  take: w :: Wire class | Nil => w
  takeStr: s :: String class | Nil => s
  badArg: flag :: Boolean => self take: (flag ifTrue: [Integer] ifFalse: [nil])
  badNonProtocolArg: flag :: Boolean => self takeStr: (flag ifTrue: [Integer] ifFalse: [nil])
  badStore: flag :: Boolean => self.slot := flag ifTrue: [Integer] ifFalse: [nil]
  badReturn: flag :: Boolean -> Wire class | Nil => flag ifTrue: [Integer] ifFalse: [nil]
  fineArg: flag :: Boolean => self take: (flag ifTrue: [Tcp] ifFalse: [Ssl])
  fineArgNil: flag :: Boolean => self take: (flag ifTrue: [Tcp] ifFalse: [nil])
  fineStore: flag :: Boolean => self.slot := flag ifTrue: [Tcp] ifFalse: [Ssl]
  fineReturn: flag :: Boolean -> Wire class | Nil => flag ifTrue: [Tcp] ifFalse: [nil]
",
    );
    let diags = checker_diagnostics(&source);
    let line_of = |needle: &str| {
        source[..source.find(needle).expect("fixture line")]
            .lines()
            .count()
    };
    let at = |needle: &str| -> Vec<&Diagnostic> {
        diags
            .iter()
            .filter(|d| source[..d.span.start() as usize].lines().count() == line_of(needle))
            .collect()
    };
    for (site, needle) in [
        ("argument", "badArg:"),
        ("field assignment", "badStore:"),
        ("return", "badReturn:"),
    ] {
        assert!(
            at(needle).iter().any(|d| d
                .message
                .contains("Integer class does not conform to Wire class")),
            "{site}: a non-conformer in a union reports its non-conformance: {diags:#?}"
        );
    }
    assert!(
        at("badNonProtocolArg:")
            .iter()
            .any(|d| d.message.contains("Integer class")),
        "a non-protocol class object in a union is refused: {diags:#?}"
    );
    let fine = line_of("fineArg:");
    assert!(
        diags
            .iter()
            .all(|d| source[..d.span.start() as usize].lines().count() < fine),
        "conformers in the same union are accepted: {diags:#?}"
    );
}

#[test]
fn class_objects_in_a_union_state_default_are_judged() {
    let source = wire_source(
        r"
Actor subclass: Caller
  state: bad :: Wire class | Nil = (true ifTrue: [Integer] ifFalse: [nil])
  state: ok :: Wire class | Nil = (true ifTrue: [Tcp] ifFalse: [nil])
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        diags.iter().any(|d| d
            .message
            .contains("Integer class does not conform to Wire class")),
        "a non-conformer default is refused: {diags:#?}"
    );
    assert_eq!(
        diags.len(),
        1,
        "the conformer default is accepted: {diags:#?}"
    );
}

/// BT-4450: a caller that already holds the registry in an `Arc` (the
/// language service builds one per completion/hover request) shares it with
/// the checker — no second deep clone of every `ProtocolInfo`.
#[test]
fn set_protocol_registry_shares_the_callers_arc() {
    let registry = std::sync::Arc::new(ProtocolRegistry::new());
    let mut checker = TypeChecker::new();
    checker.set_protocol_registry(std::sync::Arc::clone(&registry));
    assert!(
        std::sync::Arc::ptr_eq(
            checker.protocol_registry().expect("registry is set"),
            &registry
        ),
        "the checker holds the caller's registry, not a copy"
    );
}

/// BT-4440: the return-type diagnostics naming `selector` (one per method).
fn return_diagnostics<'a>(
    diags: &'a [Diagnostic],
    source: &str,
    selector: &str,
) -> Vec<&'a Diagnostic> {
    let line = source[..=source
        .find(&format!("\n  {selector} "))
        .expect("fixture method")]
        .lines()
        .count()
        + 1;
    diags
        .iter()
        .filter(|d| source[..d.span.start() as usize].lines().count() == line)
        .collect()
}

#[test]
fn protocol_metatype_return_accepts_conformers_and_protocol_metatypes() {
    let source = wire_source(
        r"
Object subclass: Factory
  tcp -> Wire class => Tcp
  pass: w :: Wire class -> Wire class => w
  either: flag :: Boolean -> Wire class => flag ifTrue: [Tcp] ifFalse: [Ssl]
  open -> Object class => Tcp
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        !diags
            .iter()
            .any(|d| d.message.contains("return type") || d.message.contains("does not conform")),
        "conforming class objects are valid `-> Wire class` returns: {diags:#?}"
    );
}

#[test]
fn non_conforming_protocol_metatype_return_warns_with_missing_selectors() {
    let source = wire_source(
        r"
Object subclass: Factory
  bad -> Wire class => Integer
  literal -> Wire class => Wire
",
    );
    let diags = checker_diagnostics(&source);
    let bad = return_diagnostics(&diags, &source, "bad");
    let warning = bad
        .iter()
        .find(|d| d.message == "Integer class does not conform to Wire class")
        .unwrap_or_else(|| panic!("conformance warning on `bad`: {diags:#?}"));
    assert_eq!(warning.severity, crate::source_analysis::Severity::Warning);
    let hint = warning.hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("class send:") && hint.contains("class label"),
        "the hint names the missing class-side selectors: {hint}"
    );
    // The protocol object is not the class object of a conformer.
    assert!(
        !return_diagnostics(&diags, &source, "literal").is_empty(),
        "a protocol literal is not a `-> Wire class` return: {diags:#?}"
    );
}

#[test]
fn protocol_metatype_return_into_unrelated_protocol_metatype_warns() {
    let source = wire_source(
        r"
Protocol define: Counted
  class count -> Integer

Object subclass: Factory
  convert: w :: Wire class -> Counted class => w
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        diags.iter().any(|d| d.message
            == "Method 'convert:' in Factory declares return type Counted class, but body returns Wire class"),
        "`Wire class` is not `Counted class`: {diags:#?}"
    );
}

#[test]
fn class_metatype_return_is_checked_against_the_metaclass_tower() {
    let source = r"
Object subclass: Factory
  number -> Number class => Integer
  behaviour -> Behaviour => Integer
  wrong -> Number class => String
  instance -> Integer => Integer
";
    let diags = checker_diagnostics(source);
    assert!(
        return_diagnostics(&diags, source, "number").is_empty()
            && return_diagnostics(&diags, source, "behaviour").is_empty(),
        "a subclass's class object fits `-> Number class` and `-> Behaviour`: {diags:#?}"
    );
    assert!(
        diags.iter().any(|d| d.message
            == "Method 'wrong' in Factory declares return type Number class, but body returns String class"),
        "`String class` is not `Number class`: {diags:#?}"
    );
    assert!(
        diags.iter().any(|d| d.message
            == "Method 'instance' in Factory declares return type Integer, but body returns Integer class"),
        "a class object is not an instance: {diags:#?}"
    );
}

#[test]
fn protocol_literal_return_fits_protocol_and_object() {
    let source = wire_source(
        r"
Object subclass: Factory
  proto -> Protocol => Wire
  anything -> Object => Wire
  take: p :: Protocol => p
  pass => self take: Wire
",
    );
    let diags = checker_diagnostics(&source);
    assert!(
        return_diagnostics(&diags, &source, "proto").is_empty()
            && return_diagnostics(&diags, &source, "anything").is_empty()
            && return_diagnostics(&diags, &source, "pass").is_empty(),
        "the protocol object is a `Protocol`: {diags:#?}"
    );
}

// ── BT-4441: return-type refinement and class-side block context ─────────

#[test]
fn protocol_metatype_subsumption_checks_class_side_return_types() {
    // `Wire` answers `class label` with a `String`; `Numbered` promises an
    // `Integer`, so `Wire class` must not flow into `Numbered class` even
    // though it has the selector. `Described` promises an `Object`, which
    // `String` refines.
    let source = wire_source(
        r"
Protocol define: Numbered
  class label -> Integer

Protocol define: Described
  class label -> Object

Object subclass: Caller
  numbered: n :: Numbered class => n label
  described: d :: Described class => d label
  run: w :: Wire class =>
    self numbered: w
    self described: w
",
    );
    let msgs = messages(&checker_diagnostics(&source));
    assert!(
        msgs.iter().any(|m| m.contains("Numbered class")),
        "Wire's `class label -> String` does not refine Integer: {msgs:#?}"
    );
    assert!(
        !msgs.iter().any(|m| m.contains("Described class")),
        "String refines Object: {msgs:#?}"
    );
}

#[test]
fn protocol_metatype_subsumption_checks_instance_side_return_types() {
    let source = r"
Protocol define: Named
  name -> String

Protocol define: Sized
  name -> Integer

Protocol define: Vague
  name

Object subclass: Caller
  sized: s :: Sized class => s
  named: n :: Named class => n
  run: n :: Named class with: v :: Vague class =>
    self sized: n
    self named: v
";
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.iter().any(|m| m.contains("Sized class")),
        "`name -> String` does not refine `name -> Integer`: {msgs:#?}"
    );
    assert!(
        msgs.iter().any(|m| m.contains("Named class")),
        "an unannotated `name` promises nothing, so does not refine String: {msgs:#?}"
    );
}

#[test]
fn class_side_requirement_return_type_mismatch_warns() {
    // `Badge` responds to `class label` but answers an Integer where
    // `Labelled` promises a String: passing a `Badge` to `:: Labelled`
    // reports the class-side mismatch, as an instance-side one would be.
    let source = r#"
Protocol define: Labelled
  class label -> String

sealed Object subclass: Badge
  class label -> Integer => 7

sealed Object subclass: Tag
  class label -> String => "tag"

Object subclass: Caller
  take: l :: Labelled => l
  run =>
    self take: Badge new
    self take: Tag new
"#;
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.contains(
            &"Badge class>>label returns Integer, incompatible with protocol Labelled's String"
                .to_string()
        ),
        "class-side return mismatch is reported: {msgs:#?}"
    );
    assert!(
        !msgs.iter().any(|m| m.starts_with("Tag")),
        "a refining class-side return is silent: {msgs:#?}"
    );
}

#[test]
fn block_argument_to_protocol_metatype_gets_requirement_param_types() {
    // `Each`'s `class each:` hands the block an Integer: the block param
    // is typed from the requirement, so a misspelled selector on it is
    // reported against Integer rather than passing as Dynamic.
    let source = r"
Protocol define: Each
  class each: blk :: Block(Integer, Nil) -> Nil

Object subclass: Caller
  run: e :: Each class =>
    e each: [:x | x bogusSelector. nil]
";
    let msgs = messages(&checker_diagnostics(source));
    assert!(
        msgs.iter()
            .any(|m| m.starts_with("Integer") && m.contains("'bogusSelector'")),
        "block param is typed Integer from the class requirement: {msgs:#?}"
    );
}
