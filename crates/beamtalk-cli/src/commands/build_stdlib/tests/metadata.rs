// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the `metadata` side of the stdlib build.
//!
//! **DDD Context:** CLI / Build System

use super::super::generators::*;
use super::super::metadata::*;
use super::{sample_class_meta, temp_utf8_dir};
use beamtalk_core::semantic_analysis::alias_registry::AliasRegistry;
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use camino::Utf8PathBuf;
use std::fs;

#[test]
fn test_extract_alias_source_snippets_captures_verbatim_declaration() {
    let (_temp, lib_dir) = temp_utf8_dir();
    let file = lib_dir.join("Fixture.bt");
    fs::write(
        &file,
        "type RestartStrategy = #temporary | #transient | #permanent\n\
         Object subclass: Fixture\n  noop => nil\n",
    )
    .unwrap();

    let snippets = extract_alias_source_snippets(&file).unwrap();
    assert_eq!(snippets.len(), 1);
    assert_eq!(
        snippets[0],
        "type RestartStrategy = #temporary | #transient | #permanent"
    );

    // Round-trips through the read side of the generated table.
    let info = AliasRegistry::from_source_text(&snippets[0])
        .expect("verbatim declaration text should re-parse");
    assert_eq!(info.name.as_str(), "RestartStrategy");
    assert!(!info.is_internal);
}

#[test]
fn test_extract_alias_source_snippets_captures_internal_modifier() {
    let (_temp, lib_dir) = temp_utf8_dir();
    let file = lib_dir.join("Fixture.bt");
    fs::write(&file, "internal type Scratch = Integer\n").unwrap();

    let snippets = extract_alias_source_snippets(&file).unwrap();
    assert_eq!(snippets, vec!["internal type Scratch = Integer"]);

    let info = AliasRegistry::from_source_text(&snippets[0]).unwrap();
    assert!(
        info.is_internal,
        "the `internal` modifier should survive the source-text round trip"
    );
}

#[test]
fn test_extract_alias_source_snippets_no_aliases() {
    let (_temp, lib_dir) = temp_utf8_dir();
    let file = lib_dir.join("Fixture.bt");
    fs::write(&file, "Object subclass: Fixture\n  noop => nil\n").unwrap();

    assert!(extract_alias_source_snippets(&file).unwrap().is_empty());
}

/// Pass 2 (system review) edge case: a declaration that spans multiple
/// source lines (a long union wrapped for readability) embeds a literal
/// newline in the sliced snippet. Rust string literals accept a raw
/// newline verbatim (no escaping needed — `generate_alias_sources_section`
/// only escapes `\` and `"`), so this must still round-trip cleanly
/// through generation and `AliasRegistry::from_source_text` reparse.
#[test]
fn test_extract_alias_source_snippets_handles_multiline_declaration() {
    let (_temp, lib_dir) = temp_utf8_dir();
    let file = lib_dir.join("Fixture.bt");
    fs::write(
        &file,
        "type Wrapped =\n    #a\n    | #b\n    | #c\n\nObject subclass: Fixture\n  noop => nil\n",
    )
    .unwrap();

    let snippets = extract_alias_source_snippets(&file).unwrap();
    assert_eq!(snippets.len(), 1);
    assert!(
        snippets[0].contains('\n'),
        "expected the multiline declaration to be captured verbatim, got: {:?}",
        snippets[0]
    );

    // Generation must produce valid Rust even with an embedded newline.
    let mut code = String::new();
    generate_alias_sources_section(&mut code, &snippets);
    assert!(code.contains("type Wrapped ="));

    // And the read side must still reconstruct the alias correctly.
    let info = AliasRegistry::from_source_text(&snippets[0])
        .expect("a multiline declaration should still re-parse");
    assert_eq!(info.name.as_str(), "Wrapped");
}

#[test]
fn test_mark_value_subclasses_follows_chain_across_files() {
    // BT-4132: `Collection(E) subclass: Bag(E)` parses as `Object`; its
    // chain (which spans files) reaches `Value`, so it is anchored like
    // a direct `Value subclass:`. `Value` itself and non-Value chains
    // are not; a cycle terminates.
    let named = |name: &str, superclass: &str| {
        let mut m = sample_class_meta();
        m.class_name = name.to_string();
        m.superclass_name = superclass.to_string();
        m
    };
    let mut metas = vec![
        named("ProtoObject", "none"),
        named("Object", "ProtoObject"),
        named("Value", "Object"),
        named("Collection", "Value"),
        named("Bag", "Collection"),
        named("Actor", "Object"),
        named("Loop1", "Loop2"),
        named("Loop2", "Loop1"),
    ];
    mark_value_subclasses(&mut metas);
    let flags: Vec<(&str, bool)> = metas
        .iter()
        .map(|m| (m.class_name.as_str(), m.is_value_subclass))
        .collect();
    assert_eq!(
        flags,
        vec![
            ("ProtoObject", false),
            ("Object", false),
            ("Value", false),
            ("Collection", true),
            ("Bag", true),
            ("Actor", false),
            ("Loop1", false),
            ("Loop2", false),
        ]
    );
}

#[test]
fn test_synthesized_keyword_agrees_with_anchor() {
    // The generator's anchor (`is_value_subclass` + `auto_kw`, from the
    // real per-file `extract_class_metadata` and the cross-file
    // `mark_value_subclasses`) must agree with the hierarchy's
    // `auto_ctors`, or `generated_builtins.rs` and fresh extraction
    // disagree about synthesized constructors. BT-4132: indirect Value
    // subclasses (whose AST kind is `Object`) included.
    let cases: &[&[&str]] = &[
        &["Value subclass: Pt\n  field: x = 0\n  field: y = 0\n"],
        &["Value subclass: Empty\n"],
        &["Value subclass: Pt\n  field: x = 0\n  class x: v => Pt new\n"],
        &["Value subclass: Pt\n  field: x = 0\n  class new => Pt x: 1\n"],
        &["Object subclass: Box\n  field: x = 0\n"],
        &[
            "Value subclass: Base\n  field: x = 0\n",
            "Base subclass: Leaf\n  field: y = 0\n",
        ],
        &[
            "Value subclass: Base\n",
            "Base subclass: Leaf\n  field: y = 0\n  class y: v => Leaf new\n",
        ],
        &[
            "Value subclass: Base\n  field: x = 0\n",
            "Base subclass: Leaf\n",
        ],
        &[
            "Object subclass: Plain\n",
            "Plain subclass: Leaf\n  field: y = 0\n",
        ],
    ];
    let no_protocols = std::collections::HashMap::new();
    for files in cases {
        let dir = tempfile::tempdir().expect("tempdir");
        let mut metas: Vec<ClassMeta> = files
            .iter()
            .enumerate()
            .map(|(i, src)| {
                let path = Utf8PathBuf::from_path_buf(dir.path().join(format!("c{i}.bt")))
                    .expect("utf8 temp path");
                fs::write(&path, src).expect("write fixture");
                extract_class_metadata(&path, &format!("bt@stdlib@c{i}"), &no_protocols)
                    .expect("extract fixture metadata")
            })
            .collect();
        mark_value_subclasses(&mut metas);
        let combined = files.concat();
        let module = beamtalk_core::test_helpers::test_support::parse_bt(&combined);
        let hierarchy =
            beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
                .0
                .expect("fixture hierarchy should build");
        for meta in &metas {
            let generated = meta.is_value_subclass.then(|| meta.auto_kw.clone());
            let anchor = hierarchy
                .get_class(meta.class_name.as_str())
                .expect("class should be present")
                .auto_ctors
                .as_ref()
                .map(|auto| auto.keyword_constructor.as_ref().map(ToString::to_string));
            assert_eq!(
                generated, anchor,
                "generator and anchor must agree for {}. Sources:\n{combined}",
                meta.class_name
            );
        }
    }
}

fn project_root() -> Utf8PathBuf {
    beamtalk_workspace::cargo_workspace_root!()
        .try_into()
        .expect("workspace root is valid UTF-8")
}

/// The selectors `beamtalk_primitive:is_string_binary_shared_selector/1`
/// routes straight to the Binary module — the `binary.bt` instance
/// selectors that `string.bt` inherits unchanged — are the `String` row's
/// `special_selectors` in the shared routing fixture
/// (`runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json`,
/// BT-4292). This test recomputes that set from the real `.bt` sources —
/// Binary's own instance selectors minus whatever String redefines — and
/// fails if it drifts from the fixture; the Erlang half
/// (`beamtalk_tagged_map_routing_conformance_tests`) checks the Erlang
/// function against the same fixture row, so a future edit to either
/// file that changes the override relationship is caught instead of
/// silently reintroducing a binary/string dispatch mismatch.
///
/// This only sees overrides made by editing `binary.bt`/`string.bt`
/// directly — it has no visibility into selectors added via the `extend`
/// mechanism (ADR 0066), which lives in separate extension sources, not the
/// class bodies this test parses. That's a known, currently-low-risk gap
/// (an `extend` can't override a class-body-defined method per ADR 0066, and
/// `beamtalk_primitive:module_for_value/2` separately checks the extension
/// registry at runtime for `String`-side overrides), not a guarantee this
/// test makes today.
#[test]
fn test_binary_string_shared_selectors_stay_in_sync() {
    let root = project_root();
    let binary_path = root.join("stdlib/src/binary.bt");
    let string_path = root.join("stdlib/src/string.bt");

    let binary_meta = extract_class_metadata(
        &binary_path,
        "bt@stdlib@binary",
        &std::collections::HashMap::new(),
    )
    .expect("binary.bt should parse");
    let string_meta = extract_class_metadata(
        &string_path,
        "bt@stdlib@string",
        &std::collections::HashMap::new(),
    )
    .expect("string.bt should parse");

    let string_overrides: std::collections::HashSet<&str> = string_meta
        .methods
        .iter()
        .map(|m| m.selector.as_str())
        .collect();

    let inherited_unchanged: std::collections::BTreeSet<&str> = binary_meta
        .methods
        .iter()
        .map(|m| m.selector.as_str())
        .filter(|selector| !string_overrides.contains(selector))
        .collect();

    let fixture_path = root
        .join("runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json");
    let fixture: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&fixture_path).expect("routing fixture should be readable"),
    )
    .expect("routing fixture is JSON");
    let string_row = fixture["rows"]
        .as_array()
        .expect("fixture `rows` list")
        .iter()
        .find(|row| row["class"] == "String")
        .expect("the routing fixture has a String row");
    let fixture_shared: std::collections::BTreeSet<&str> = string_row["special_selectors"]
        .as_array()
        .expect("String row `special_selectors` list")
        .iter()
        .map(|selector| selector.as_str().expect("selector is a string"))
        .collect();

    assert_eq!(
        inherited_unchanged, fixture_shared,
        "binary.bt selectors NOT overridden by string.bt (left) no longer \
         match the String row's special_selectors in \
         tagged_map_routing_conformance.json (right). Update the fixture \
         and beamtalk_primitive.erl's is_string_binary_shared_selector/1 — \
         a selector only belongs there if string.bt truly inherits it \
         unchanged from binary.bt."
    );
}

// --- mark_spawns_for_selectors and its Timer/Parallel/Collection callers ---

fn method_meta(selector: &str) -> MethodMeta {
    MethodMeta {
        arity: selector.matches(':').count(),
        selector: selector.to_string(),
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: false,
        spawns_block: false,
        return_type: None,
        param_types: vec![],
        doc: None,
        primitive: None,
        constant_value: None,
        answers_receiver: false,
    }
}

#[test]
fn test_mark_spawns_for_selectors_marks_only_matching_selectors() {
    let mut methods = vec![method_meta("after:do:"), method_meta("other:")];
    mark_spawns_for_selectors(&mut methods, &["after:do:"], "Test").unwrap();
    assert!(methods[0].spawns_block);
    assert!(!methods[1].spawns_block);
}

#[test]
fn test_mark_spawns_for_selectors_errors_on_unmatched_selector() {
    let mut methods = vec![method_meta("foo")];
    let err = mark_spawns_for_selectors(&mut methods, &["after:do:", "foo"], "Timer").unwrap_err();
    let message = err.to_string();
    assert!(message.contains("Timer"), "got: {message}");
    assert!(message.contains("after:do:"), "got: {message}");
}

#[test]
fn test_mark_timer_spawns_marks_expected_selectors() {
    let mut class_methods = vec![
        method_meta("after:do:"),
        method_meta("every:do:"),
        method_meta("other:"),
    ];
    mark_timer_spawns(&mut class_methods).unwrap();
    assert!(class_methods[0].spawns_block);
    assert!(class_methods[1].spawns_block);
    assert!(!class_methods[2].spawns_block);
}

#[test]
fn test_mark_timer_spawns_errors_when_selectors_missing() {
    let mut class_methods = vec![method_meta("other:")];
    assert!(mark_timer_spawns(&mut class_methods).is_err());
}

#[test]
fn test_mark_parallel_spawns_marks_all_expected_selectors() {
    let mut class_methods = vec![
        method_meta("all:"),
        method_meta("all:timeout:"),
        method_meta("any:"),
    ];
    mark_parallel_spawns(&mut class_methods).unwrap();
    assert!(class_methods.iter().all(|m| m.spawns_block));
}

#[test]
fn test_mark_parallel_spawns_errors_when_selectors_missing() {
    let mut class_methods = vec![method_meta("other:")];
    assert!(mark_parallel_spawns(&mut class_methods).is_err());
}

#[test]
fn test_mark_parallel_collect_spawns_marks_expected_selectors() {
    let mut methods = vec![
        method_meta("parallelCollect:"),
        method_meta("parallelCollect:maxConcurrency:"),
    ];
    mark_parallel_collect_spawns(&mut methods).unwrap();
    assert!(methods.iter().all(|m| m.spawns_block));
}

#[test]
fn test_mark_parallel_collect_spawns_errors_when_selectors_missing() {
    let mut methods = vec![method_meta("other:")];
    assert!(mark_parallel_collect_spawns(&mut methods).is_err());
}

// --- extract_class_metadata error paths and Timer/Parallel/Collection triggers ---

#[test]
fn test_extract_class_metadata_no_class_definition_errors() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("Empty.bt");
    fs::write(&file, "// just a comment, no class here\n").unwrap();

    let err = extract_class_metadata(&file, "bt@stdlib@empty", &std::collections::HashMap::new())
        .err()
        .unwrap();
    assert!(err.to_string().contains("No class definition"));
}

#[test]
fn test_extract_class_metadata_multiple_classes_errors() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("Two.bt");
    fs::write(
        &file,
        "Object subclass: FirstOne\n  noop => nil\n\
         Object subclass: SecondOne\n  noop => nil\n",
    )
    .unwrap();

    let err = extract_class_metadata(&file, "bt@stdlib@two", &std::collections::HashMap::new())
        .err()
        .unwrap();
    assert!(err.to_string().contains("Expected exactly one class"));
}

#[test]
fn test_extract_class_metadata_initialize_assigns_populated() {
    // ADR 0124 A2a: extract_class_metadata computes the class's own
    // `initialize` definitely-assigned slot summary from its AST body.
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("logger.bt");
    fs::write(
        &file,
        "Actor subclass: StdlibLogger\n  \
         state: log :: String = \"\"\n  \
         initialize => self.log := self.log ++ \"A\"\n",
    )
    .unwrap();

    let meta = extract_class_metadata(
        &file,
        "bt@stdlib@stdlib_logger",
        &std::collections::HashMap::new(),
    )
    .unwrap();
    assert_eq!(meta.initialize_assigns, vec!["log".to_string()]);
}

#[test]
fn test_extract_class_metadata_no_initialize_leaves_assigns_empty() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("plain.bt");
    fs::write(&file, "Object subclass: StdlibPlain\n  noop => nil\n").unwrap();

    let meta = extract_class_metadata(
        &file,
        "bt@stdlib@stdlib_plain",
        &std::collections::HashMap::new(),
    )
    .unwrap();
    assert!(meta.initialize_assigns.is_empty());
}

#[test]
fn test_extract_class_metadata_timer_marks_spawning_class_methods() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("timer.bt");
    fs::write(
        &file,
        "Object subclass: Timer\n  \
         class after: ms do: block => nil\n  \
         class every: ms do: block => nil\n",
    )
    .unwrap();

    let meta = extract_class_metadata(&file, "bt@stdlib@timer", &std::collections::HashMap::new())
        .unwrap();
    assert!(
        meta.class_methods
            .iter()
            .find(|m| m.selector == "after:do:")
            .expect("after:do: should be present")
            .spawns_block
    );
    assert!(
        meta.class_methods
            .iter()
            .find(|m| m.selector == "every:do:")
            .expect("every:do: should be present")
            .spawns_block
    );
}

#[test]
fn test_extract_class_metadata_timer_missing_selectors_errors() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("timer.bt");
    fs::write(&file, "Object subclass: Timer\n  class noop => nil\n").unwrap();

    let err = extract_class_metadata(&file, "bt@stdlib@timer", &std::collections::HashMap::new())
        .err()
        .unwrap();
    assert!(err.to_string().contains("Timer"));
}

#[test]
fn test_extract_class_metadata_parallel_marks_spawning_class_methods() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("parallel.bt");
    fs::write(
        &file,
        "Object subclass: Parallel\n  \
         class all: blocks => nil\n  \
         class all: blocks timeout: ms => nil\n  \
         class any: blocks => nil\n",
    )
    .unwrap();

    let meta = extract_class_metadata(
        &file,
        "bt@stdlib@parallel",
        &std::collections::HashMap::new(),
    )
    .unwrap();
    assert!(meta.class_methods.iter().all(|m| m.spawns_block));
}

#[test]
fn test_extract_class_metadata_collection_marks_spawning_methods() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("collection.bt");
    fs::write(
        &file,
        "Object subclass: Collection\n  \
         parallelCollect: block => nil\n  \
         parallelCollect: block maxConcurrency: n => nil\n",
    )
    .unwrap();

    let meta = extract_class_metadata(
        &file,
        "bt@stdlib@collection",
        &std::collections::HashMap::new(),
    )
    .unwrap();
    assert!(meta.methods.iter().all(|m| m.spawns_block));
}

// --- synthesize_value_auto_methods (via extract_class_metadata) ---

#[test]
fn test_extract_class_metadata_value_class_generates_all_auto_methods() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("Wrapper.bt");
    fs::write(
        &file,
        "Value subclass: Wrapper\n  field: value :: Integer = 0\n",
    )
    .unwrap();

    let meta = extract_class_metadata(
        &file,
        "bt@stdlib@wrapper",
        &std::collections::HashMap::new(),
    )
    .unwrap();

    let getter = meta
        .methods
        .iter()
        .find(|m| m.selector == "value")
        .expect("auto getter should be generated");
    assert_eq!(getter.arity, 0);
    assert_eq!(getter.return_type, Some(DeclaredType::simple("Integer")));

    let setter = meta
        .methods
        .iter()
        .find(|m| m.selector == "withValue:")
        .expect("auto functional updater should be generated");
    assert_eq!(setter.arity, 1);
    assert_eq!(setter.return_type, Some(DeclaredType::SelfType));

    let ctor = meta
        .class_methods
        .iter()
        .find(|m| m.selector == "value:")
        .expect("auto keyword constructor should be generated");
    assert_eq!(ctor.arity, 1);
    assert_eq!(ctor.return_type, Some(DeclaredType::simple("Wrapper")));
}

#[test]
fn test_extract_class_metadata_value_class_skips_auto_methods_user_already_defines() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("Point2.bt");
    fs::write(
        &file,
        "Value subclass: Point2\n  \
         field: x :: Integer = 0\n  \
         field: y :: Integer = 0\n  \
         x => 42\n  \
         withY: v => v\n  \
         class x: ax y: ay => nil\n",
    )
    .unwrap();

    let meta = extract_class_metadata(&file, "bt@stdlib@point2", &std::collections::HashMap::new())
        .unwrap();

    // User-defined "x" getter: not duplicated by the auto-getter.
    assert_eq!(meta.methods.iter().filter(|m| m.selector == "x").count(), 1);
    // User-defined "withY:" setter: not duplicated by the auto-updater.
    assert_eq!(
        meta.methods
            .iter()
            .filter(|m| m.selector == "withY:")
            .count(),
        1
    );
    // User-defined "x:y:" constructor: not duplicated by the auto-constructor.
    assert_eq!(
        meta.class_methods
            .iter()
            .filter(|m| m.selector == "x:y:")
            .count(),
        1
    );
    // The fields NOT covered by a user override still get their auto method.
    assert!(meta.methods.iter().any(|m| m.selector == "withX:"));
    assert!(meta.methods.iter().any(|m| m.selector == "y"));
}

// --- is_protocol_only_file ---

#[test]
fn test_is_protocol_only_file_true_for_protocol_only_source() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("printable.bt");
    fs::write(&file, "Protocol define: Printable\n  asString -> String\n").unwrap();
    assert!(is_protocol_only_file(&file).unwrap());
}

#[test]
fn test_is_protocol_only_file_false_for_class_source() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("Foo.bt");
    fs::write(&file, "Object subclass: Foo\n  noop => nil\n").unwrap();
    assert!(!is_protocol_only_file(&file).unwrap());
}

#[test]
fn test_is_protocol_only_file_false_on_parse_errors() {
    let (_temp, dir) = temp_utf8_dir();
    let file = dir.join("Broken.bt");
    // Conservative fallback: a file with parse errors is treated as a
    // normal class file so `extract_class_metadata` reports the real
    // error, rather than being silently misclassified as protocol-only.
    fs::write(&file, "Object subclass: ###!!!(((\n").unwrap();
    assert!(!is_protocol_only_file(&file).unwrap());
}

#[test]
fn test_is_protocol_only_file_errors_when_file_missing() {
    let (_temp, dir) = temp_utf8_dir();
    let missing = dir.join("Missing.bt");
    assert!(is_protocol_only_file(&missing).is_err());
}

// --- collect_stdlib_protocol_infos non-fatal read errors ---

#[test]
fn test_collect_stdlib_protocol_infos_skips_unreadable_file() {
    let (_temp, dir) = temp_utf8_dir();
    let missing = dir.join("Missing.bt");
    let real = dir.join("printable.bt");
    fs::write(&real, "Protocol define: Printable\n  asString -> String\n").unwrap();

    let (infos, defs, _sources) = collect_stdlib_protocol_infos(&[missing, real]);
    assert_eq!(infos.len(), 1);
    assert_eq!(infos[0].name.as_str(), "Printable");
    // `Printable` here declares no provided methods, so it contributes
    // nothing to the trait-flattening AST list — see this function's
    // own doc for why only provision-bearing protocols are collected.
    assert!(defs.is_empty());
}

/// The trait-flattening counterpart of the test above
/// (ADR 0127 §10a; BT-3591): a *provision-bearing* protocol's full AST
/// is collected, while a requirement-only protocol in the same source
/// set still isn't.
#[test]
fn test_collect_stdlib_protocol_infos_collects_provision_bearing_protocol_defs() {
    let (_temp, dir) = temp_utf8_dir();
    let requirement_only = dir.join("printable.bt");
    fs::write(
        &requirement_only,
        "Protocol define: Printable\n  asString -> String\n",
    )
    .unwrap();
    let with_provision = dir.join("comparable.bt");
    fs::write(
        &with_provision,
        "Protocol define: Comparable\n  \
         < other :: Self -> Boolean\n\n  \
         max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]\n",
    )
    .unwrap();

    let (infos, defs, _sources) =
        collect_stdlib_protocol_infos(&[requirement_only, with_provision]);
    assert_eq!(infos.len(), 2, "both protocols are tracked as ProtocolInfo");
    assert_eq!(
        defs.len(),
        1,
        "only the provision-bearing protocol contributes a full AST: {defs:?}"
    );
    assert_eq!(defs[0].name.name.as_str(), "Comparable");
}

// --- alias_source_texts_sorted_by_name ---

#[test]
fn test_alias_source_texts_sorted_by_name_sorts_by_alias_name_not_raw_text() {
    let (_temp, lib_dir) = temp_utf8_dir();
    let file = lib_dir.join("Fixture.bt");
    // Raw-text sort would put "internal type Zebra" before "type Alpha"
    // (`i` < `t`) — the opposite of sorting by alias *name*.
    fs::write(
        &file,
        "internal type Zebra = Integer\ntype Alpha = String\n\
         Object subclass: Fixture\n  noop => nil\n",
    )
    .unwrap();

    let alias_sources = collect_stdlib_alias_sources(&[file]).unwrap();
    assert_eq!(alias_sources.len(), 2);

    let sorted_texts = alias_source_texts_sorted_by_name(alias_sources);
    assert_eq!(sorted_texts[0], "type Alpha = String");
    assert_eq!(sorted_texts[1], "internal type Zebra = Integer");
}
