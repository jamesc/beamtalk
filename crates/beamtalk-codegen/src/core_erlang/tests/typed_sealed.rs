// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4069: sends to a receiver typed as a sealed Value class compile to a
//! guarded direct `Module:dispatch/3` call instead of a dynamic
//! `beamtalk_message_dispatch:send/3`.
//!
//! Positive tests (sealed receivers take the direct path) fail until the
//! `typed_sealed` handler lands; negative tests (open/untyped/union,
//! raw-representation builtins, exception kin, `Character` literals) lock in
//! today's dynamic behavior so the new handler cannot steal those sends.

use super::codegen;

// ---------------------------------------------------------------------------
// Positive: sealed receivers take the guarded direct path
// ---------------------------------------------------------------------------

#[test]
fn sealed_value_typed_param_sends_direct_with_tag_guard() {
    // NOTE: `generate_module` compiles only the module's first class — the
    // Caller (holding the send under test) comes first, the callee second.
    // The hierarchy and type inference still see both.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    // Direct call to the class module with the literal selector. The
    // module's own `dispatch/3` definition also mentions
    // `bt@sealed_widget':'dispatch'` (its `perform:` arm), but always with a
    // variable selector — the `('x'` literal pins this to the send site.
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call the class module. Got:\n{code}"
    );
    // Tag guard on '$beamtalk_class' with a dynamic-send fallback.
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "guard mismatch should fall back to the dynamic send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_object_typed_param_sends_direct() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: c :: SealedCounter -> Integer =>\n",
        "    c answer\n\n",
        "sealed Object subclass: SealedCounter\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_counter':'dispatch'('answer'"),
        "sealed Object-kind receiver should direct-call too (same tagged-map representation). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

// ---------------------------------------------------------------------------
// Negative: everything else stays dynamic
// ---------------------------------------------------------------------------

#[test]
fn sealed_value_keyword_send_binds_args_once() {
    // Keyword sends thread arguments into both the direct and fallback
    // branches from a single let-binding (no double evaluation).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w withX: 3\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('withX:', ["),
        "sealed keyword send should direct-call with args. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    // Exactly one `let` binding per operand: the single argument temp is
    // bound once and read three times (direct args list + both fallback
    // args lists), the receiver temp once and read five times (is_map,
    // maps:get, direct call, both fallback sends). Any inlining or double
    // binding changes these counts.
    assert_eq!(
        code.matches("TSealedArg").count(),
        4,
        "argument should be bound once and read three times. Got:\n{code}"
    );
    assert_eq!(
        code.matches("TSealedRecv").count(),
        6,
        "receiver should be bound once and read five times. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_typed_param_sends_direct_class_side() {
    // Class methods send to values too (every vs-elixir row is class-side):
    // instance `dispatch/3` on a value is pure, so no class-process hazard.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  class useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call from class side too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_typed_param_sends_direct_actor_side() {
    // Actor methods send to values too: a direct `dispatch/3` on a value
    // executes in-caller exactly like the dynamic send (values never hop
    // processes), and the emission must be verifier-clean — this test runs
    // with debug assertions on, so a `report_threaded_ir_verify_errors`
    // finding would panic rather than pass.
    let code = codegen(concat!(
        "Actor subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call from actor side too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn open_value_typed_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: OpenWidget -> Integer =>\n",
        "    w x\n\n",
        "Value subclass: OpenWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !code.contains("call 'bt@open_widget':'dispatch'('x'"),
        "open (subclassable) receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "open receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn untyped_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w =>\n",
        "    w ping\n",
    ));
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "untyped receiver send should stay dynamic. Got:\n{code}"
    );
}

#[test]
fn union_typed_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: Widget | Gadget =>\n",
        "    w ping\n\n",
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Gadget\n",
        "  ping => 1\n",
    ));
    assert!(
        !code.contains("'dispatch'('ping'"),
        "union receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "union receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn integer_typed_param_stays_dynamic() {
    // `Integer` instances are raw Erlang integers, never tagged maps: the
    // tag guard could never match, so emitting it would only pessimize.
    // (`frobnicate` is intentionally undefined — DNU only matters at
    // runtime; codegen shape is what this asserts.)
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: n :: Integer -> Object =>\n",
        "    n frobnicate\n",
    ));
    assert!(
        !code.contains("call 'bt@stdlib@integer':'dispatch'('frobnicate'"),
        "raw-term receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "raw-term receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn dictionary_typed_param_stays_dynamic() {
    // `Dictionary` instances are untagged maps: the tag guard could never
    // match. (`at:`/`at:put:` are dict intrinsics claimed before dispatch
    // selection, so this uses an unclaimed selector.)
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: d :: Dictionary(Symbol, Integer) -> Object =>\n",
        "    d frobnicate\n",
    ));
    assert!(
        !code.contains("call 'bt@stdlib@dictionary':'dispatch'('frobnicate'"),
        "untagged-map receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "untagged-map receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn sealed_exception_subclass_stays_dynamic() {
    // Dynamic sends to exceptions route via `bt@stdlib@exception:dispatch`,
    // not the leaf module — a direct leaf call would diverge.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: e :: SealedBoom -> Object =>\n",
        "    e frobnicate\n\n",
        "sealed typed Exception subclass: SealedBoom\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_boom':'dispatch'('frobnicate'"),
        "exception-kin receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "exception-kin send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn field_at_put_on_sealed_receiver_never_direct() {
    // `fieldAt:put:` is the one selector whose dynamic path diverges from
    // `dispatch/3` (semicolon vs em dash hint). The object-reflection
    // intrinsic claims it before dispatch selection today, so this pins the
    // end state rather than the handler's own guard: no direct call, ever.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w fieldAt: #x put: 1\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_widget':'dispatch'('fieldAt:put:'"),
        "fieldAt:put: must never take the direct path. Got:\n{code}"
    );
}

#[test]
fn binary_send_on_sealed_receiver_never_direct() {
    // Binary selectors never reach the dynamic fallthrough (it raises an
    // internal error for them); the direct path must not claim them either.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w + 1\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_widget':'dispatch'('+"),
        "binary selector must never take the direct path. Got:\n{code}"
    );
}

#[test]
fn erlang_module_typed_param_stays_dynamic() {
    // `ErlangModule` instances are tagged maps, but dynamic dispatch routes
    // them to the hand-written native proxy, not a compiled module — a
    // direct call would run the wrong code. Exclusion is correctness here.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: m :: ErlangModule -> Object =>\n",
        "    m frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "native-proxied receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "native-proxied receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn class_object_typed_param_stays_dynamic() {
    // `Class`/`Metaclass` instances are `#beamtalk_object{}` records, not
    // maps: the tag guard could never match.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: c :: Class -> Object =>\n",
        "    c frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "record receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "record receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn sealed_stdlib_result_sends_direct() {
    // A sealed stdlib Value converts too (vs-elixir row 9b's shape): the
    // guard matches the tag and the direct call hits `bt@stdlib@result`.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: r :: Result(Integer, Symbol) -> Object =>\n",
        "    r isOk\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@result':'dispatch'('isOk'"),
        "sealed stdlib receiver should direct-call. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn future_typed_param_stays_dynamic() {
    // `Future` is the runtime-only builtin: no compiled module, so no
    // `dispatch/3` to direct-call (its dynamic sends route via
    // `beamtalk_future`, not a class module at all).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: f :: Future -> Object =>\n",
        "    f frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "runtime-only receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "runtime-only receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

/// A sealed `Object`-kind class arriving via the ambient (live-image)
/// hierarchy — the shape a REPL inline class definition sees for a
/// `ClassBuilder` class after a live edit. `module_less` selects the
/// module-less (fun-backed, dispatches via `runtime_instance_method`)
/// flavor versus the compiled flavor.
fn ambient_sealed_object(
    module_less: bool,
    package: Option<&str>,
) -> beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
    ClassInfo {
        name: "AmbientWidget".into(),
        superclass: Some("Object".into()),
        is_sealed: true,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: package.map(Into::into),
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        surface_incomplete: false,
        is_module_less: module_less,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        state_kinds: std::collections::HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

fn codegen_with_ambient(
    src: &str,
    ambient: beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo,
) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_class_hierarchy(vec![ambient]),
    )
    .expect("codegen should succeed")
}

#[test]
fn module_less_ambient_sealed_class_stays_dynamic() {
    // A1: a sealed module-less (`ClassBuilder`) class seen through a REPL
    // inline definition's ambient hierarchy must NOT direct-call — there
    // is no `Module:dispatch/3` (dynamic dispatch runs the fun via
    // `runtime_instance_method`), so a direct call would `undef`.
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  useIt: w :: AmbientWidget -> Object =>\n",
            "    w frobnicate\n",
        ),
        ambient_sealed_object(true, None),
    );
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "module-less receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "module-less receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn module_ful_ambient_sealed_class_sends_direct() {
    // Positive control for the module-less test above: the same ambient
    // sealed class WITH a compiled module still takes the direct path, so
    // the negative cannot pass vacuously (e.g. by the checker failing to
    // resolve the pre-loaded annotation).
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  useIt: w :: AmbientWidget -> Object =>\n",
            "    w frobnicate\n",
        ),
        ambient_sealed_object(false, None),
    );
    assert!(
        code.contains("call 'bt@ambient_widget':'dispatch'('frobnicate'"),
        "module-ful ambient receiver should direct-call. Got:\n{code}"
    );
}

#[test]
fn packaged_ambient_sealed_class_resolves_qualified_module() {
    // The `package = Some` resolution branch: registry miss falls back to
    // the deterministic `bt@{package}@{snake}` composition.
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  useIt: w :: AmbientWidget -> Object =>\n",
            "    w frobnicate\n",
        ),
        ambient_sealed_object(false, Some("live")),
    );
    assert!(
        code.contains("call 'bt@live@ambient_widget':'dispatch'('frobnicate'"),
        "packaged ambient receiver should direct-call the qualified module. Got:\n{code}"
    );
}

#[test]
fn packaged_sealed_class_with_unqualified_index_entry_resolves_emitted_module() {
    // BT-4152: `beamtalk test` emits fixture (and test-file) modules
    // unqualified (`bt@<stem>`) while the file's own class carries
    // `package = Some(current_package)`. The direct-call target must be
    // the module this compilation unit actually emits (the unit index
    // entry), not the re-derived `bt@<package>@<class>` — a module that
    // is never emitted, so the direct call would `undef` at runtime
    // where the dynamic path resolves the loaded `bt@<stem>` module.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("AmbientWidget".to_string(), "bt@ambient_widget".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@caller_fixture")
            .with_class_hierarchy(vec![ambient_sealed_object(false, Some("buccaneer"))])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@ambient_widget':'dispatch'('frobnicate'"),
        "packaged receiver with an unqualified index entry should direct-call the emitted module. Got:\n{code}"
    );
    assert!(
        !code.contains("bt@buccaneer@ambient_widget"),
        "must not target the never-emitted qualified module. Got:\n{code}"
    );
}

#[test]
fn packaged_sealed_class_with_index_entry_keeps_package_mode_target() {
    // BT-4152 (no-change control): in package mode the unit's own
    // registry entry and the `info.package` re-derivation name the same
    // module, so resolving through the registry first must not change
    // the direct-call target there — including a subdirectory class the
    // closed-form `bt@{package}@{snake}` convention could never rebuild.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert(
        "AmbientWidget".to_string(),
        "bt@sicp@scheme@env".to_string(),
    );
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@sicp@main")
            .with_class_hierarchy(vec![ambient_sealed_object(false, Some("sicp"))])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@sicp@scheme@env':'dispatch'('frobnicate'"),
        "package-mode receiver should direct-call the indexed module. Got:\n{code}"
    );
}

/// BT-4069 (review F1): sealed Value/Object builtins whose instances are
/// `'$beamtalk_class'`-tagged maps with a compiled module — the complement of
/// [`NON_TAGGED_MAP_CLASSES`](crate::core_erlang::dispatch_codegen::NON_TAGGED_MAP_CLASSES)
/// over the sealed builtins. Every entry is `.bt`-defined (generated builtin:
/// a compiled `bt@stdlib@…` module exporting `dispatch/3` exists), so a
/// tag match routes to that same module dynamically and statically alike.
/// A newly added sealed builtin fails
/// `all_sealed_builtins_are_classified_for_static_dispatch` until its author
/// puts it here or in `NON_TAGGED_MAP_CLASSES` — with the routing reason —
/// so an `ErlangModule`-like (native proxy) or `Exception`-like (alternate
/// entry) addition cannot slip through unclassified.
const KNOWN_TAGGED_SEALED_BUILTINS: &[&str] = &[
    "ActorSpawned",
    "ActorStopped",
    "AnnouncementNavigation",
    "Array",
    "Beamtalk",
    "BindingChanged",
    "BindingsView",
    "ChangeEntry",
    "ChangeLog",
    "ClassLoaded",
    "ClassRemoved",
    "CompiledMethod",
    "Console",
    "DateTime",
    "Digest",
    "Duration",
    "Erlang",
    "File",
    "FileHandle",
    "FlushCompleted",
    "Inspector",
    "InspectorField",
    "Json",
    "Logger",
    "Node",
    "NodeDown",
    "NodeShapeSkew",
    "NodeUp",
    "OS",
    "ObjectStateChanged",
    "Package",
    "Parallel",
    "Program",
    "Protocol",
    "Random",
    "Regex",
    "Result",
    "Session",
    "Set",
    "Stream",
    "Subscription",
    "SubscriptionNode",
    "SupervisionChildAdded",
    "SupervisionChildCrashed",
    "SupervisorChild",
    "System",
    "SystemAnnouncer",
    "SystemNavigation",
    "Telemetry",
    "Time",
    "Timer",
    "Tracing",
    "Transcript",
    "Uuid",
    "Workspace",
];

#[test]
fn all_sealed_builtins_are_classified_for_static_dispatch() {
    use beamtalk_core::ast::ClassKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
    let module = beamtalk_core::ast::Module::new(
        Vec::new(),
        beamtalk_core::source_analysis::Span::new(0, 0),
    );
    let (hierarchy_result, _) = ClassHierarchy::build(&module);
    let hierarchy = hierarchy_result.expect("builtin hierarchy should build");
    for name in hierarchy.class_names() {
        let info = hierarchy
            .get_class(name)
            .unwrap_or_else(|| panic!("{name} should still resolve"));
        if !info.is_sealed {
            continue;
        }
        if !matches!(
            hierarchy.resolve_class_kind(name),
            ClassKind::Value | ClassKind::Object
        ) {
            continue;
        }
        if info.is_module_less {
            // Separately gated (dispatch condition 5, with its own negative
            // tests): only runtime-only `Future` and synthetic protocol roots
            // may be module-less here.
            assert!(
                name == "Future" || hierarchy.is_protocol_class(name),
                "{name} is newly module-less — confirm the dispatch gates exclude it"
            );
            continue;
        }
        assert!(
            crate::core_erlang::dispatch_codegen::NON_TAGGED_MAP_CLASSES.contains(&name.as_str())
                || KNOWN_TAGGED_SEALED_BUILTINS.contains(&name.as_str()),
            "{name} is a new sealed builtin — classify it: NON_TAGGED_MAP_CLASSES \
             (never a tagged map of its own name) or KNOWN_TAGGED_SEALED_BUILTINS \
             (.bt-defined: compiled module + tagged-map instances)"
        );
    }
}

#[test]
fn send_handlers_keep_priority_positions() {
    // BT-4069 (review F2): `HANDLERS` order is load-bearing — `typed_sealed`
    // must stay last (it converts only would-be-dynamic sends) and
    // `character_typed` first. A handler appended after `typed_sealed`
    // would silently never fire for sealed-typed receivers.
    let handlers = crate::core_erlang::dispatch_codegen::HANDLERS;
    assert_eq!(
        handlers.first().map(|(name, _)| *name),
        Some("character_typed"),
        "character_typed must stay first"
    );
    assert_eq!(
        handlers.last().map(|(name, _)| *name),
        Some("typed_sealed"),
        "typed_sealed must stay last"
    );
}

#[test]
fn non_tagged_exclusions_are_sealed_builtin_values() {
    // Every `NON_TAGGED_MAP_CLASSES` entry must still be a sealed builtin of
    // Value- or Object-kind: an entry that stopped being sealed is dead (the
    // sealed check excludes it first) and must be removed, not carried.
    use beamtalk_core::ast::ClassKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
    let module = beamtalk_core::ast::Module::new(
        Vec::new(),
        beamtalk_core::source_analysis::Span::new(0, 0),
    );
    let (hierarchy_result, _) = ClassHierarchy::build(&module);
    let hierarchy = hierarchy_result.expect("builtin hierarchy should build");
    for name in crate::core_erlang::dispatch_codegen::NON_TAGGED_MAP_CLASSES {
        let info = hierarchy
            .get_class(name)
            .unwrap_or_else(|| panic!("{name} should still be a builtin class"));
        assert!(
            info.is_sealed,
            "{name} is no longer sealed — remove it from NON_TAGGED_MAP_CLASSES"
        );
        assert!(
            matches!(
                hierarchy.resolve_class_kind(name),
                ClassKind::Value | ClassKind::Object
            ),
            "{name} is no longer Value/Object-kind — re-check its representation"
        );
    }
}

#[test]
fn character_literal_keeps_character_dispatch() {
    // `$A` recurses through `HANDLERS` (including the new trailing entry)
    // before falling back to `generate_character_typed_dispatch`: the new
    // handler must not steal it (`Character` is a raw integer at runtime).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  m -> Object =>\n",
        "    $A uppercase\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@character':'dispatch'('uppercase'"),
        "Character literal should still reach the Character module. Got:\n{code}"
    );
    assert!(
        !code.contains("beamtalk_message_dispatch':'send'("),
        "Character literal send should not go dynamic. Got:\n{code}"
    );
}

// ---------------------------------------------------------------------------
// BT-4200: hardening residuals
// ---------------------------------------------------------------------------

#[test]
fn own_class_redefined_into_new_module_targets_the_emitted_module() {
    // BT-4200 (1): the index is captured before the unit loads (REPL: the live
    // `build_class_module_index/0`), so a class compiled as `bt@pkg@pt` and
    // redefined inline into `bt@pt` is still indexed under the old module.
    // The unit's own class must resolve to the module the unit emits, or the
    // guarded direct call lands in the stale old module.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "sealed Value subclass: Pt\n",
        "  field: x :: Integer = 0\n",
        "  sameX: other :: Pt -> Integer =>\n",
        "    other x\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("Pt".to_string(), "bt@pkg@pt".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@pt").with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@pt':'dispatch'('x'"),
        "typed send must target the unit's own emitted module. Got:\n{code}"
    );
    assert!(
        !code.contains("bt@pkg@pt"),
        "must not target the stale module from the pre-load index. Got:\n{code}"
    );
}

#[test]
fn stale_compiled_metadata_for_module_less_redefinition_stays_dynamic() {
    // BT-4200 (2): a `ClassBuilder ... register` over an existing compiled
    // class never refreshes compiler-server metadata (`is_module_less` stays
    // `false`), while the live index now maps the class to its own name (the
    // module-less convention). The fresher index must win: no direct call.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("AmbientWidget".to_string(), "AmbientWidget".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test")
            .with_class_hierarchy(vec![ambient_sealed_object(false, None)])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "index says module-less: must stay dynamic despite stale metadata. Got:\n{code}"
    );
}

#[test]
fn stale_module_less_metadata_for_compiled_redefinition_goes_direct() {
    // BT-4200 (2), converse: metadata says module-less but the live index maps
    // the class to a compiled `bt@` module (a later compiled load). One
    // consistent source: the index decides.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("AmbientWidget".to_string(), "bt@ambient_widget".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test")
            .with_class_hierarchy(vec![ambient_sealed_object(true, None)])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@ambient_widget':'dispatch'('frobnicate'"),
        "index says compiled: should direct-call. Got:\n{code}"
    );
}

#[test]
fn abstract_value_new_stays_a_plain_builder_for_subclass_chaining() {
    // BT-4200 (4), documented-not-fixed: a direct `Mod:new()` skips the
    // gen_server abstract rejection, and the callee cannot add one — a Value
    // subclass's `new/0` chains through its superclass module's `new/0` to
    // collect inherited field defaults, so an abstract class's `new/0` must
    // stay a plain map builder. Pin both halves of that dependency so a future
    // "raise in abstract new/0" change cannot silently break subclass `new`.
    let parent = codegen(concat!(
        "abstract Value subclass: Shape\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        parent.contains("'new'/0 = fun () ->") && !parent.contains("instantiation_error"),
        "abstract Value new/0 must remain a plain builder. Got:\n{parent}"
    );
    let child = codegen(concat!(
        "Shape subclass: Circle\n",
        "  field: r :: Integer = 0\n\n",
        "abstract Value subclass: Shape\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        child.contains(":'new'() in"),
        "subclass new/0 must chain through the superclass module's new/0. Got:\n{child}"
    );
}

/// BT-4200 (3): the exclusion set mirrors the runtime's routing special arms
/// (`beamtalk_primitive:tagged_map_dispatch/1`), so both sides are checked
/// against one shared fixture. The `EUnit` half is
/// `beamtalk_tagged_map_routing_conformance_tests`; changing either side
/// alone fails one of the two.
#[test]
fn non_tagged_exclusions_match_shared_runtime_routing_fixture() {
    use crate::core_erlang::dispatch_codegen::{EXCEPTION_ROOT_CLASS, NON_TAGGED_MAP_CLASSES};
    use std::collections::BTreeSet;

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(
        "../../runtime/apps/beamtalk_runtime/test/fixtures/tagged_map_routing_conformance.json",
    );
    let raw = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    let fixture: serde_json::Value = serde_json::from_str(&raw).expect("fixture is JSON");

    let mut expected: Vec<String> = fixture["raw_term"]
        .as_array()
        .expect("raw_term list")
        .iter()
        .map(|e| e["class"].as_str().expect("raw_term.class").to_string())
        .collect();
    for key in ["untagged_map", "record", "proxy_routed"] {
        expected.extend(
            fixture[key]
                .as_array()
                .unwrap_or_else(|| panic!("{key} list"))
                .iter()
                .map(|e| e.as_str().expect("class name").to_string()),
        );
    }

    let expected_set: BTreeSet<&str> = expected.iter().map(String::as_str).collect();
    assert_eq!(
        expected_set.len(),
        expected.len(),
        "fixture lists a class under more than one routing kind"
    );
    let actual_set: BTreeSet<&str> = NON_TAGGED_MAP_CLASSES.iter().copied().collect();
    assert_eq!(
        actual_set.len(),
        NON_TAGGED_MAP_CLASSES.len(),
        "NON_TAGGED_MAP_CLASSES has duplicates"
    );
    assert_eq!(
        actual_set, expected_set,
        "NON_TAGGED_MAP_CLASSES drifted from the shared runtime-routing fixture \
         (update both it and beamtalk_primitive's tagged_map_dispatch/1 arms)"
    );
    assert_eq!(
        fixture["exception_root"].as_str(),
        Some(EXCEPTION_ROOT_CLASS),
        "exception-family root drifted from the shared runtime-routing fixture"
    );
}
