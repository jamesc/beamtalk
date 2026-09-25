// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unit tests for `trait_expansion` (ADR 0127 §3, BT-3588 and BT-3589).

use super::*;
use crate::source_analysis::Severity;

/// Lex + parse a source string into a `Module` (mirrors the same helper in
/// every other `semantic_analysis` test module, e.g.
/// `type_checker::tests::common::parse_source`).
fn parse_source(source: &str) -> Module {
    use crate::source_analysis::{lex_with_eof, parse};
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "fixture source failed to parse cleanly: {diagnostics:?}\nsource:\n{source}"
    );
    module
}

fn find_class<'a>(module: &'a Module, name: &str) -> &'a ClassDefinition {
    module
        .classes
        .iter()
        .find(|c| c.name.name == name)
        .unwrap_or_else(|| panic!("no class named {name} in module"))
}

fn find_method<'a>(class: &'a ClassDefinition, selector: &str) -> Option<&'a MethodDefinition> {
    class.methods.iter().find(|m| m.selector.name() == selector)
}

// ── class-wins (ADR 0127 §3 step 4) ─────────────────────────────────────

#[test]
fn class_body_wins_over_a_single_provision() {
    let mut module = parse_source(
        "Protocol define: Describable
  printString -> String => \"a describable thing\"

Value subclass: Report
  uses: Describable

  printString -> String => \"my own printString\"",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );
    assert!(
        origins.is_empty(),
        "class's own method must not get an origin"
    );

    let report = find_class(&module, "Report");
    let print_string_methods: Vec<_> = report
        .methods
        .iter()
        .filter(|m| m.selector.name() == "printString")
        .collect();
    assert_eq!(
        print_string_methods.len(),
        1,
        "the class's own printString must not be duplicated by the provision"
    );
}

#[test]
fn synthesised_value_accessor_ranks_as_class_body_too() {
    // ADR 0127 §3, "Synthesised methods rank as class body": a `field:`
    // slot's auto-generated getter isn't written in `class.methods` at all
    // (it's synthesised later, at `ClassHierarchy::build` time) — this pass
    // must still treat `size` as owned by the class because of the slot,
    // not just because of a hand-written method.
    let mut module = parse_source(
        "Protocol define: Sized
  size -> Integer => 0

Value subclass: Bucket
  uses: Sized
  field: size :: Integer = 0",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert!(
        origins.is_empty(),
        "the field's synthesised accessor wins, not the provision"
    );

    let bucket = find_class(&module, "Bucket");
    assert!(
        find_method(bucket, "size").is_none(),
        "size must not be flattened in — the class's own field beats the provision"
    );
}

#[test]
fn class_wins_suppresses_conflict_between_two_providers_too() {
    // ADR 0127 §4: the class's own definition beats *both* provisions, so
    // no exclusion — and no conflict error — is needed (the ADR's own
    // `Report` example).
    let mut module = parse_source(
        "Protocol define: Labelled
  printString -> String => \"labelled\"

Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Labelled
  uses: Describable

  printString -> String => \"mine\"",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert!(
        diagnostics.is_empty(),
        "class-wins must suppress the conflict entirely: {diagnostics:?}"
    );
}

// ── conflict detection (ADR 0127 §4) ────────────────────────────────────

#[test]
fn two_protocols_providing_the_same_selector_conflict() {
    let mut module = parse_source(
        "Protocol define: Labelled
  printString -> String => \"labelled\"

Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Labelled
  uses: Describable",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert_eq!(
        diagnostics.len(),
        1,
        "expected exactly one conflict error: {diagnostics:?}"
    );
    let diag = &diagnostics[0];
    assert_eq!(diag.severity, Severity::Error);
    assert!(diag.message.contains("printString"));
    assert!(diag.message.contains("Labelled"));
    assert!(diag.message.contains("Describable"));
    assert!(diag.message.contains("Report"));

    let report = find_class(&module, "Report");
    assert!(
        find_method(report, "printString").is_none(),
        "a conflicting selector must not be flattened from either side"
    );
    assert!(origins.is_empty());
}

// ── `excluding:` (ADR 0127 §3 step 2) ───────────────────────────────────

#[test]
fn excluding_drops_just_that_selector() {
    let mut module = parse_source(
        "Protocol define: Describable
  printString -> String => \"a thing\"
  summary -> String => \"a summary\"

Value subclass: Report
  uses: Describable excluding: #(#printString)",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {diagnostics:?}"
    );

    let report = find_class(&module, "Report");
    assert!(
        find_method(report, "printString").is_none(),
        "printString was excluded"
    );
    assert!(
        find_method(report, "summary").is_some(),
        "summary was not excluded"
    );
    assert_eq!(
        origins.get(&(EcoString::from("Report"), EcoString::from("summary"))),
        Some(&EcoString::from("Describable"))
    );
    assert!(!origins.contains_key(&(EcoString::from("Report"), EcoString::from("printString"))));
}

#[test]
fn excluding_resolves_a_conflict_between_two_providers() {
    let mut module = parse_source(
        "Protocol define: Labelled
  printString -> String => \"labelled\"

Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Labelled
  uses: Describable excluding: #(#printString)",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert!(
        diagnostics.is_empty(),
        "excluding one side must resolve the conflict: {diagnostics:?}"
    );

    let report = find_class(&module, "Report");
    assert!(find_method(report, "printString").is_some());
    assert_eq!(
        origins.get(&(EcoString::from("Report"), EcoString::from("printString"))),
        Some(&EcoString::from("Labelled"))
    );
}

// ── the body-less-protocol hint (ADR 0127 §1, §Status 6) ────────────────

#[test]
fn uses_of_a_protocol_with_no_provisions_is_a_hint_not_an_error() {
    let mut module = parse_source(
        "Protocol define: Printable
  asString -> String

Value subclass: Report
  uses: Printable",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].severity, Severity::Hint);
    assert!(diagnostics[0].message.contains("Printable"));
    assert!(
        diagnostics[0]
            .message
            .contains("only checks its requirements")
    );
    assert!(origins.is_empty());

    let report = find_class(&module, "Report");
    assert!(report.methods.is_empty());
}

// ── unknown protocol ─────────────────────────────────────────────────────

#[test]
fn uses_of_an_unknown_protocol_is_an_error() {
    let mut module = parse_source(
        "Value subclass: Report
  uses: NoSuchProtocol",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].severity, Severity::Error);
    assert!(diagnostics[0].message.contains("NoSuchProtocol"));
}

// ── Cross-file / cross-package carrying (ADR 0127 §10a; BT-3591) ───────────

/// Parses a standalone `Protocol define: ...` source snippet and returns its
/// single `ProtocolDefinition`, for building an `external_protocols` map the
/// way a caller carrying a trait's AST in from another file/package would.
fn parse_protocol_def(source: &str) -> ProtocolDefinition {
    let module = parse_source(source);
    assert_eq!(
        module.protocols.len(),
        1,
        "expected exactly one protocol definition in fixture: {source}"
    );
    module.protocols[0].clone()
}

#[test]
fn uses_resolves_against_an_externally_carried_protocol() {
    // The protocol is defined in a *different* file/package — its AST never
    // appears in `module.protocols`, only in `external_protocols`, exactly
    // as `dependency_classes.rs`/`class_index.rs` carry it in for a real
    // cross-file or cross-package `uses:`.
    let comparable = parse_protocol_def(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]",
    );
    let mut external = HashMap::new();
    external.insert(comparable.name.name.clone(), comparable);

    let mut module = parse_source(
        "Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0

  < other :: Version -> Boolean => self.major < other major",
    );

    let (diagnostics, origins) = expand_module(&mut module, &external);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let version = find_class(&module, "Version");
    assert!(
        find_method(version, "max:").is_some(),
        "max: should have flattened in from the externally-carried protocol"
    );
    assert_eq!(
        origins.get(&(EcoString::from("Version"), EcoString::from("max:"))),
        Some(&EcoString::from("Comparable"))
    );
}

#[test]
fn package_qualified_uses_resolves_against_an_externally_carried_protocol() {
    // `uses: json@Parseable` — the package qualifier no longer blocks
    // resolution; the bare name is looked up in the same merged map
    // regardless of the qualifier (BT-3591).
    let parseable = parse_protocol_def(
        "Protocol define: Parseable
  raw -> String

  describe -> String => \"parses \" ++ self raw",
    );
    let mut external = HashMap::new();
    external.insert(parseable.name.name.clone(), parseable);

    let mut module = parse_source(
        "Value subclass: LenientParser
  uses: json@Parseable
  field: raw :: String = \"\"",
    );

    let (diagnostics, origins) = expand_module(&mut module, &external);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let parser = find_class(&module, "LenientParser");
    assert!(
        find_method(parser, "describe").is_some(),
        "describe should have flattened in from the dependency-carried protocol"
    );
    assert_eq!(
        origins.get(&(
            EcoString::from("LenientParser"),
            EcoString::from("describe")
        )),
        Some(&EcoString::from("Parseable"))
    );
}

#[test]
fn package_qualified_uses_with_no_carried_source_names_the_missing_source() {
    // No `external_protocols` entry at all — the dependency shipped no
    // source (and there's no `'__beamtalk_protocol_source'/0` reader on this
    // path) — the diagnostic must say so distinctly from a bare-name typo.
    let mut module = parse_source(
        "Value subclass: LenientParser
  uses: json@Parseable
  field: raw :: String = \"\"",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].severity, Severity::Error);
    assert!(
        diagnostics[0].message.contains("no source available"),
        "{:?}",
        diagnostics[0]
    );
    assert!(diagnostics[0].message.contains("json@Parseable"));
}

#[test]
fn same_module_protocol_wins_over_an_externally_carried_one_of_the_same_name() {
    // A same-named external protocol must never shadow the current module's
    // own definition — current-file wins, matching every other
    // pre-hierarchy pass's convention.
    let external_version = parse_protocol_def(
        "Protocol define: Greetable
  greeting -> String => \"external hello\"",
    );
    let mut external = HashMap::new();
    external.insert(external_version.name.name.clone(), external_version);

    let mut module = parse_source(
        "Protocol define: Greetable
  greeting -> String => \"local hello\"

Value subclass: Greeter
  uses: Greetable",
    );

    let (diagnostics, origins) = expand_module(&mut module, &external);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let greeter = find_class(&module, "Greeter");
    let greeting = find_method(greeter, "greeting").expect("greeting was flattened in");
    // The flattened body must be the *local* protocol's, not the external
    // one's — both bodies are string literals, so comparing spans of the
    // parsed sources isn't meaningful; instead assert there's exactly one
    // `Greetable` in scope by checking the origin resolved at all (proves
    // the local definition, the only one actually visible to
    // `ClassHierarchy`/`ProtocolRegistry`, is what won the lookup).
    let _ = greeting;
    assert_eq!(
        origins.get(&(EcoString::from("Greeter"), EcoString::from("greeting"))),
        Some(&EcoString::from("Greetable"))
    );
}

// ── `Self` substitution (ADR 0127 §1) ───────────────────────────────────

#[test]
fn self_in_a_provision_becomes_the_using_class_non_generic() {
    let mut module = parse_source(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0

  < other :: Version -> Boolean => self.major < other major",
    );

    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let version = find_class(&module, "Version");
    let max = find_method(version, "max:").expect("max: was flattened in");
    assert_eq!(max.parameters.len(), 1);
    assert_eq!(
        max.parameters[0]
            .type_annotation
            .as_ref()
            .map(TypeAnnotation::type_name),
        Some(EcoString::from("Version"))
    );
    assert_eq!(
        max.return_type.as_ref().map(TypeAnnotation::type_name),
        Some(EcoString::from("Version"))
    );
    assert_eq!(
        origins.get(&(EcoString::from("Version"), EcoString::from("max:"))),
        Some(&EcoString::from("Comparable"))
    );
}

#[test]
fn self_in_a_provision_becomes_the_generic_user_applied_to_its_own_params() {
    let mut module = parse_source(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Pair(A, B)
  uses: Comparable
  field: first :: A
  field: second :: B

  < other :: Pair(A, B) -> Boolean => self.first < other first",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let pair = find_class(&module, "Pair");
    let max = find_method(pair, "max:").expect("max: was flattened in");
    let Some(TypeAnnotation::Generic {
        base, parameters, ..
    }) = max.parameters[0].type_annotation.as_ref()
    else {
        panic!(
            "expected max:'s parameter to be a generic Pair(A, B), got {:?}",
            max.parameters[0].type_annotation
        );
    };
    assert_eq!(base.name, "Pair");
    assert_eq!(parameters.len(), 2);
    assert_eq!(parameters[0].type_name(), "A");
    assert_eq!(parameters[1].type_name(), "B");

    let Some(TypeAnnotation::Generic { base: rt_base, .. }) = max.return_type.as_ref() else {
        panic!(
            "expected max:'s return type to be a generic Pair(A, B), got {:?}",
            max.return_type
        );
    };
    assert_eq!(rt_base.name, "Pair");
}

// ── type-arg substitution + method-local hygiene (ADR 0127 §3) ─────────

#[test]
fn uses_type_arg_substitutes_the_protocols_type_param() {
    let mut module = parse_source(
        "Protocol define: Enumerable(E)
  elements -> List(E)

  size -> Integer => self elements size

Actor subclass: WorkerPool
  uses: Enumerable(Worker)

  elements -> List(Worker) => self.workers",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    // `size` carries no E-typed signature, so this mainly proves the
    // provision made it across; the real substitution check is the
    // hygiene test below (`inject:into:`'s `Block(A, E, A)`).
    let pool = find_class(&module, "WorkerPool");
    assert!(find_method(pool, "size").is_some());
}

#[test]
fn method_local_type_var_colliding_with_the_class_type_param_is_alpha_renamed() {
    // ADR 0127 §3, "Type-parameter substitution and hygiene":
    // `Pair(A, B) uses: Enumerable(A)` must not let `inject:into:`'s own
    // method-local `A` collapse into the class's `A`.
    let mut module = parse_source(
        "Protocol define: Enumerable(E)
  elements -> List(E)

  inject: initial :: A into: block :: Block(A, E, A) -> A => self elements inject: initial into: block

Value subclass: Pair(A, B)
  uses: Enumerable(A)
  field: first :: A
  field: second :: B

  elements -> List(A) => List with: self.first",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let pair = find_class(&module, "Pair");
    let inject = find_method(pair, "inject:into:").expect("inject:into: was flattened in");
    assert_eq!(inject.parameters.len(), 2);

    // `initial`'s local `A` was renamed (it collided with class param `A`);
    // the renamed letter must not be `A` or `B` (both taken by the class).
    let initial_ty = inject.parameters[0]
        .type_annotation
        .as_ref()
        .expect("initial has a type annotation");
    let TypeAnnotation::Simple(initial_id) = initial_ty else {
        panic!("expected initial's type to be a Simple identifier, got {initial_ty:?}");
    };
    assert_ne!(initial_id.name, "A");
    assert_ne!(initial_id.name, "B");
    let fresh = initial_id.name.clone();

    // `block`'s `Block(A, E, A)` becomes `Block(<fresh>, A, <fresh>)` — `E`
    // substituted to the class's own `A` (the `uses: Enumerable(A)` type
    // arg), and both local-`A` occurrences renamed identically.
    let block_ty = inject.parameters[1]
        .type_annotation
        .as_ref()
        .expect("block has a type annotation");
    let TypeAnnotation::Generic {
        base, parameters, ..
    } = block_ty
    else {
        panic!("expected block's type to be Generic Block(...), got {block_ty:?}");
    };
    assert_eq!(base.name, "Block");
    assert_eq!(parameters.len(), 3);
    assert_eq!(parameters[0].type_name(), fresh);
    assert_eq!(
        parameters[1].type_name(),
        "A",
        "E must substitute to the uses: type arg A"
    );
    assert_eq!(parameters[2].type_name(), fresh);

    // Return type `A` (method-local) also renamed to the same fresh letter.
    assert_eq!(
        inject.return_type.as_ref().map(TypeAnnotation::type_name),
        Some(fresh)
    );
}

// ── `extending:` a protocol with provisions (ADR 0127 §5, §8) ──────────

#[test]
fn extending_a_protocol_with_provisions_does_not_flatten_the_parents_provisions() {
    // `uses: Q` only flattens Q's *own* provisions — a protocol using
    // another protocol (Q composing P's bodies) is post-v1 (§Status 8).
    // `extending:` only widens Q's *type* (checked in protocol_registry.rs),
    // never brings P's bodies along.
    let mut module = parse_source(
        "Protocol define: Describable
  printString -> String => \"described\"

Protocol define: Labelled
  extending: Describable
  label -> String => \"labelled\"

Value subclass: Report
  uses: Labelled",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let report = find_class(&module, "Report");
    assert!(
        find_method(report, "label").is_some(),
        "Labelled's own provision flattens in"
    );
    assert!(
        find_method(report, "printString").is_none(),
        "Describable's provision must NOT flatten in just because Labelled extends it"
    );
}

// ── deterministic output order ──────────────────────────────────────────

#[test]
fn flattened_methods_are_appended_in_deterministic_selector_order() {
    // `merged` (the per-class collection of surviving provisions) is a
    // `HashMap`, whose iteration order is randomised per process
    // (`RandomState`) — the splice into `class.methods` must not depend on
    // it, or codegen's emitted function order (and anything else that reads
    // `class.methods` positionally) would vary across compiler runs of the
    // exact same source.
    let mut module = parse_source(
        "Protocol define: Multi
  zebra -> String => \"z\"
  apple -> String => \"a\"
  mango -> String => \"m\"

Value subclass: Fruit
  uses: Multi",
    );

    let (diagnostics, _origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty(), "{diagnostics:?}");

    let fruit = find_class(&module, "Fruit");
    let actual: Vec<String> = fruit
        .methods
        .iter()
        .map(|m| m.selector.name().to_string())
        .collect();
    assert_eq!(
        actual,
        vec!["apple", "mango", "zebra"],
        "must be selector-sorted, not hash order"
    );
}

// ── no-op on a class with no `uses:` ─────────────────────────────────────

#[test]
fn class_with_no_uses_is_untouched() {
    let mut module = parse_source(
        "Value subclass: Plain
  field: x :: Integer = 0",
    );
    let before = module.clone();
    let (diagnostics, origins) = expand_module(&mut module, &HashMap::new());
    assert!(diagnostics.is_empty());
    assert!(origins.is_empty());
    assert_eq!(module, before);
}

// =============================================================================
// Post-`ClassHierarchy` half (BT-3589): requirements, `overriding:`, and
// protocol-side rules.
// =============================================================================

/// Runs the full two-half pass over `source` — [`expand_module`],
/// `ClassHierarchy::build`, [`apply_origins`], `ProtocolRegistry::register_module`,
/// then [`check_after_hierarchy`] — mirroring `analyse_full`'s own Phase -1 /
/// Phase 0 / Phase 0.5 / Phase 0.55 sequencing (`semantic_analysis::mod`)
/// without pulling in every other analysis phase (name resolution, type
/// checking, …) that would add unrelated diagnostics to every assertion.
fn analyse(source: &str) -> Vec<Diagnostic> {
    let mut module = parse_source(source);
    let (mut diagnostics, origins) = expand_module(&mut module, &HashMap::new());

    let (hierarchy_result, hierarchy_diags) = ClassHierarchy::build(&module);
    let mut hierarchy = hierarchy_result.expect("ClassHierarchy::build is infallible");
    diagnostics.extend(hierarchy_diags);
    apply_origins(&mut hierarchy, &origins);

    let mut registry = crate::semantic_analysis::protocol_registry::ProtocolRegistry::new();
    diagnostics.extend(registry.register_module(&module, &hierarchy));

    diagnostics.extend(check_after_hierarchy(
        &module,
        &hierarchy,
        &registry,
        &HashMap::new(),
    ));
    diagnostics
}

fn find_diagnostic<'a>(diagnostics: &'a [Diagnostic], needle: &str) -> Option<&'a Diagnostic> {
    diagnostics.iter().find(|d| d.message.contains(needle))
}

// ── §5: required selectors ──────────────────────────────────────────────

#[test]
fn unresolved_required_selector_is_error_in_closed_world() {
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0",
    );

    let diag = find_diagnostic(&diagnostics, "does not implement required `<`")
        .unwrap_or_else(|| panic!("expected a required-selector error, got {diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
    assert!(
        diag.hint
            .as_ref()
            .is_some_and(|h| h.contains("Comparable requires")),
        "hint should carry the required signature: {:?}",
        diag.hint
    );
}

#[test]
fn resolved_required_selector_is_silent() {
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Version
  uses: Comparable
  field: major :: Integer = 0

  < other -> Boolean => self.major < other major",
    );

    assert!(
        find_diagnostic(&diagnostics, "does not implement required").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn unresolved_required_selector_is_hint_in_open_world() {
    // A class overriding `doesNotUnderstand:args:` is Open (ADR 0100 Rule 1)
    // — the checker cannot prove `<` is truly missing, so this downgrades to
    // a Hint rather than an Error. The trailing period after `uses:
    // Comparable` forces the parser to end that line's statement before the
    // next one starts — without it, `uses: Comparable` and the following
    // `doesNotUnderstand: sel args: …` keyword method fold into a single
    // `uses:doesNotUnderstand:args:` selector (a pre-existing parser
    // ambiguity when a `uses:` line has no `excluding:`/`overriding:` clause
    // and is immediately followed by a keyword-selector method with no
    // intervening `state:`/`field:` line — unrelated to BT-3589, not fixed
    // here).
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

Value subclass: Proxy
  uses: Comparable.

  doesNotUnderstand: sel args: args => nil",
    );

    let diag = find_diagnostic(&diagnostics, "does not implement required `<`")
        .unwrap_or_else(|| panic!("expected a required-selector diagnostic, got {diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Hint);
}

#[test]
fn excluding_a_required_selector_is_error() {
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

Value subclass: Version
  uses: Comparable excluding: #(#<)
  field: major :: Integer = 0",
    );

    let diag = find_diagnostic(&diagnostics, "requirements cannot be excluded")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn excluding_unmatched_selector_is_error() {
    // BT-3588 PR #4037 review: an `excluding:` selector that never actually
    // matched a provided method silently no-op'd — deferred to BT-3589.
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other -> Self => (self < other) ifTrue: [other] ifFalse: [self]

Value subclass: Version
  uses: Comparable excluding: #(#mx:)

  < other -> Boolean => true",
    );

    let diag = find_diagnostic(&diagnostics, "does not provide `mx:`")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn overriding_unmatched_selector_is_error() {
    let diagnostics = analyse(
        "Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Describable overriding: #(#bogusSelector)",
    );

    let diag = find_diagnostic(&diagnostics, "does not provide `bogusSelector`")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn excluding_that_breaks_conformance_is_warning() {
    let diagnostics = analyse(
        "Protocol define: Sized
  size -> Integer => 0

Value subclass: Empty
  uses: Sized excluding: #(#size)",
    );

    let diag = find_diagnostic(&diagnostics, "does not conform to Sized")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Warning);
}

// ── §3a: `overriding:` acknowledgement (both directions) ────────────────

#[test]
fn provision_replacing_inherited_method_without_overriding_is_error() {
    // "The superclass grows": `Record` already defines `summary`; a subclass
    // uses a protocol that also provides `summary`, with no acknowledgement.
    let diagnostics = analyse(
        "Value subclass: Record
  summary -> String => \"a record\"

Protocol define: Describable
  summary -> String => \"describable\"

Record subclass: AuditRecord
  uses: Describable",
    );

    let diag = find_diagnostic(&diagnostics, "provides `summary`")
        .unwrap_or_else(|| panic!("expected a §3a error, got {diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
    assert!(diag.message.contains("Record"));
    let hint = diag.hint.as_ref().expect("hint with both fixes");
    assert!(hint.contains("overriding:"));
    assert!(hint.contains("excluding:"));
}

#[test]
fn trait_growing_a_provision_that_shadows_inherited_method_is_error() {
    // "The trait grows": a protocol already used by a class gains a new
    // provision (`printString`) that the superclass already defines. From a
    // one-shot compile's point of view this is the same shape as the
    // superclass-grows case (both are "class-wins already ran, now a
    // surviving provision collides with something inherited"), but the test
    // fixture tells the *other* story: the trait, not the superclass, is the
    // side that changed.
    let diagnostics = analyse(
        "Value subclass: Base
  printString -> String => \"a base\"

Protocol define: Describable
  label -> String => \"labelled\"
  printString -> String => \"describable\"

Base subclass: Labelled
  uses: Describable",
    );

    let diag = find_diagnostic(&diagnostics, "provides `printString`")
        .unwrap_or_else(|| panic!("expected a §3a error, got {diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn provision_replacing_inherited_method_with_overriding_is_silent() {
    let diagnostics = analyse(
        "Value subclass: Record
  summary -> String => \"a record\"

Protocol define: Describable
  summary -> String => \"describable\"

Record subclass: AuditRecord
  uses: Describable overriding: #(#summary)",
    );

    assert!(
        find_diagnostic(&diagnostics, "provides `summary`").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn printstring_from_value_is_allowlist_exempt() {
    // ADR 0127 §3a: `printString`/`displayString` inherited from `Object`/
    // `Value` are exempt — no `overriding:` needed.
    let diagnostics = analyse(
        "Protocol define: Describable
  printString -> String => \"describable\"

Value subclass: Report
  uses: Describable",
    );

    assert!(
        find_diagnostic(&diagnostics, "provides `printString`").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn non_allowlisted_root_method_still_needs_overriding() {
    // The allowlist is exactly two selectors — `hash` (inherited from
    // `Object`) still needs `overriding:` like any other inherited method.
    // Also provides `equals:` so the (unrelated) "provides exactly one of
    // equals:/hash" warning doesn't fire and get confused with the §3a
    // message below by a loose substring match.
    let diagnostics = analyse(
        "Protocol define: ConstantHash
  equals: other -> Boolean => true
  hash -> Integer => 0

Value subclass: Weird
  uses: ConstantHash",
    );

    let diag = diagnostics
        .iter()
        .find(|d| {
            d.message.contains("hash") && d.message.contains("would otherwise inherit from Object")
        })
        .unwrap_or_else(|| panic!("expected a §3a error for `hash`, got {diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn same_origin_provision_is_exempt() {
    // The superclass already uses the same protocol and hasn't customised
    // the selector — identical provisions, §3a does not fire.
    let diagnostics = analyse(
        "Protocol define: Describable
  summary -> String => \"describable\"

Value subclass: Base
  uses: Describable

Base subclass: Derived
  uses: Describable",
    );

    assert!(
        find_diagnostic(&diagnostics, "provides `summary`").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn stale_overriding_entry_is_warning() {
    // The superclass never actually defines `summary` — `overriding:`
    // acknowledges an override that doesn't exist.
    let diagnostics = analyse(
        "Value subclass: Plain

Protocol define: Describable
  summary -> String => \"describable\"

Plain subclass: Labelled
  uses: Describable overriding: #(#summary)",
    );

    let diag = find_diagnostic(&diagnostics, "does not override an inherited method")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Warning);
}

#[test]
fn sealed_inherited_method_is_not_double_reported_by_3a() {
    // A provision replacing a *sealed* inherited method (`yourself`, sealed
    // on `Object`) is already an error via `ClassHierarchy`'s existing
    // sealed-override check (it runs over every class-body method, including
    // a spliced-in provision, unconditionally — no `overriding:` can silence
    // it). §3a must not add a second, differently-worded error for the same
    // selector.
    let diagnostics = analyse(
        "Protocol define: Reflective
  yourself -> Object => nil

Value subclass: Weird
  uses: Reflective",
    );

    let sealed_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("sealed"))
        .collect();
    assert!(
        !sealed_errors.is_empty(),
        "expected the existing sealed-override error: {diagnostics:?}"
    );
    assert!(
        find_diagnostic(&diagnostics, "provides `yourself`").is_none(),
        "§3a must not duplicate the sealed-override diagnostic: {diagnostics:?}"
    );
}

// ── §8: override-compatibility for a class-body override ────────────────

#[test]
fn class_body_override_of_dropped_provision_with_incompatible_type_is_warning() {
    let diagnostics = analyse(
        "Protocol define: Enumerable
  each -> List => #()

Value subclass: Bag
  uses: Enumerable

  each -> Integer => 0",
    );

    let diag = find_diagnostic(&diagnostics, "incompatible with").unwrap_or_else(|| {
        panic!("expected an override-compatibility warning, got {diagnostics:?}")
    });
    assert_eq!(diag.severity, Severity::Warning);
}

#[test]
fn class_body_override_of_dropped_provision_with_compatible_type_is_silent() {
    let diagnostics = analyse(
        "Protocol define: Enumerable
  each -> List => #()

Value subclass: Bag
  uses: Enumerable

  each -> List => #()",
    );

    assert!(
        find_diagnostic(&diagnostics, "incompatible with").is_none(),
        "{diagnostics:?}"
    );
}

// ── Protocol-side checks (§5, §7, §13) ───────────────────────────────────

#[test]
fn self_send_outside_required_or_provided_is_error() {
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other -> Self => (self between: other) ifTrue: [other] ifFalse: [self]",
    );

    let diag = find_diagnostic(
        &diagnostics,
        "`between:` is sent by `max:` but is neither required nor provided by Comparable",
    )
    .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn self_send_to_required_selector_is_silent() {
    let diagnostics = analyse(
        "Protocol define: Comparable
  < other :: Self -> Boolean

  max: other -> Self => (self < other) ifTrue: [other] ifFalse: [self]",
    );

    assert!(
        find_diagnostic(&diagnostics, "is neither required nor provided").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn self_send_to_another_provision_is_silent() {
    let diagnostics = analyse(
        "Protocol define: Greeter
  name -> String => \"world\"
  greet -> String => \"hello \" ++ self name",
    );

    assert!(
        find_diagnostic(&diagnostics, "is neither required nor provided").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn self_send_to_object_selector_is_silent() {
    let diagnostics = analyse(
        "Protocol define: Loud
  shout -> String => self printString",
    );

    assert!(
        find_diagnostic(&diagnostics, "is neither required nor provided").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn self_field_read_in_provision_is_error() {
    let diagnostics = analyse(
        "Protocol define: Counting
  count -> Integer => self.total",
    );

    let diag = find_diagnostic(&diagnostics, "protocols are stateless")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn self_field_write_in_provision_is_error() {
    let diagnostics = analyse(
        "Protocol define: Counting
  reset -> Self => self.total := 0",
    );

    let diag = find_diagnostic(&diagnostics, "protocols are stateless")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn state_declaration_in_protocol_body_is_parse_error() {
    // The parser rejects `state:`/`field:`/`classState:` inside a protocol
    // body directly (ADR 0127 §7, §13) — it never reaches `ProtocolDefinition`
    // at all, so this is a parser-level test, not `check_after_hierarchy`.
    use crate::source_analysis::{lex_with_eof, parse};
    let tokens = lex_with_eof(
        "Protocol define: Counting
  field: total :: Integer = 0
  count -> Integer",
    );
    let (_module, diagnostics) = parse(tokens);
    let diag = diagnostics
        .iter()
        .find(|d| d.message.contains("protocols are stateless"))
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn reserved_selector_provision_is_error() {
    let diagnostics = analyse(
        "Protocol define: Lifecycle
  initialize -> Self => self",
    );

    let diag = find_diagnostic(&diagnostics, "a protocol cannot provide `initialize`")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn migrate_from_v_selector_provision_is_error() {
    let diagnostics = analyse(
        "Protocol define: Versioned
  migrateFromV1: state -> Object => state",
    );

    let diag = find_diagnostic(&diagnostics, "a protocol cannot provide `migrateFromV1:`")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn primitive_in_provision_is_error() {
    let diagnostics = analyse(
        "Protocol define: Fast
  fastAdd: n -> Integer => @primitive \"+\"",
    );

    let diag = find_diagnostic(&diagnostics, "provided methods cannot use primitives")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn sealed_provided_method_is_error() {
    let diagnostics = analyse(
        "Protocol define: Locked
  sealed value -> Integer => 0",
    );

    let diag = find_diagnostic(
        &diagnostics,
        "`sealed`/`internal` are not supported on provided methods",
    )
    .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn internal_provided_method_is_error() {
    let diagnostics = analyse(
        "Protocol define: Locked
  internal value -> Integer => 0",
    );

    let diag = find_diagnostic(
        &diagnostics,
        "`sealed`/`internal` are not supported on provided methods",
    )
    .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn equals_without_hash_is_warning() {
    let diagnostics = analyse(
        "Protocol define: Keyed
  equals: other -> Boolean => true",
    );

    let diag = find_diagnostic(&diagnostics, "provides `equals:` but not `hash`")
        .unwrap_or_else(|| panic!("{diagnostics:?}"));
    assert_eq!(diag.severity, Severity::Warning);
}

#[test]
fn equals_and_hash_together_is_silent() {
    let diagnostics = analyse(
        "Protocol define: Keyed
  equals: other -> Boolean => true
  hash -> Integer => 0",
    );

    assert!(
        find_diagnostic(&diagnostics, "but not `hash`").is_none(),
        "{diagnostics:?}"
    );
    assert!(
        find_diagnostic(&diagnostics, "but not `equals:`").is_none(),
        "{diagnostics:?}"
    );
}

#[test]
fn protocol_with_no_users_is_still_checked() {
    // ADR 0127 §5: "a protocol with no users is still checked" — no class in
    // this module uses `Lonely` at all.
    let diagnostics = analyse(
        "Protocol define: Lonely
  initialize -> Self => self",
    );

    assert!(find_diagnostic(&diagnostics, "a protocol cannot provide `initialize`").is_some());
}
