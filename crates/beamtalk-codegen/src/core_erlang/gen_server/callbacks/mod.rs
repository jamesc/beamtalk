// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP `gen_server` callback code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the standard OTP callbacks: `init/1`, `handle_continue/2`,
//! `handle_cast/2`, `handle_call/3`, `handle_info/2`, `code_change/3`,
//! and `terminate/2`.

use crate::core_erlang::sentinel_tags;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, leaf};

use ecow::EcoString;

/// A Block-layout `let var = value in`, with the rest of the sequence on the
/// next line.
fn bind(
    var: Document<'static>,
    value: Document<'static>,
    body: Document<'static>,
) -> Document<'static> {
    form::let_in(var, value, body, Layout::Block)
}

/// A Block-layout `case`: one clause per line between `of` and `end`.
fn case_block(
    scrutinee: Document<'static>,
    clauses: impl IntoIterator<Item = Document<'static>>,
) -> Document<'static> {
    form::case_of(scrutinee, clauses, Layout::Block)
}

/// A Block-layout clause with the always-true guard: `<patterns> when 'true' ->`
/// with the body nested beneath.
fn arm(
    patterns: impl IntoIterator<Item = Document<'static>>,
    body: Document<'static>,
) -> Document<'static> {
    form::clause(patterns, None, body, Layout::Block)
}

/// A one-line clause: `<patterns> when 'true' -> body`.
fn arm_inline(
    patterns: impl IntoIterator<Item = Document<'static>>,
    body: Document<'static>,
) -> Document<'static> {
    form::clause(patterns, None, body, Layout::Flat)
}

/// `call 'maps':'get'('__skip_initialize__', InitArgs, 'false')`: the flag a
/// child's `init/1` sets when it calls its parent's as a state-building helper.
fn skip_initialize_flag() -> Document<'static> {
    form::call(
        "maps",
        "get",
        [
            leaf::atom(sentinel_tags::SKIP_INITIALIZE),
            leaf::var("InitArgs"),
            leaf::atom("false"),
        ],
    )
}

/// The `case` on [`skip_initialize_flag`]: `on_helper` is its `'true'` clause
/// (a parent-helper call), `on_leaf` its `'false'` one.
fn skip_initialize_case(
    on_helper: Document<'static>,
    on_leaf: Document<'static>,
) -> Document<'static> {
    case_block(skip_initialize_flag(), [on_helper, on_leaf])
}

/// `{'noreply', state}`.
fn noreply(state: Document<'static>) -> Document<'static> {
    form::tuple([leaf::atom("noreply"), state])
}

/// ADR 0078: Identifies a single class's `initialize` method in the
/// auto-chained dispatch sequence emitted by `generate_handle_continue`.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct InitializeChainEntry {
    /// Compiled Erlang module name for this class (e.g. `"bt@caching_database_actor"`).
    /// The `safe_dispatch` call targets this module so dispatch starts at the
    /// class's own table rather than walking from the leaf.
    pub module_name: String,
}

/// ADR 0078: A typed-no-default state field annotated with the class
/// that declared it, so post-initialize diagnostics can name the owning class
/// rather than always the leaf.
///
/// Values are owned (rather than borrowed from the AST) because
/// cross-file ancestors produce synthetic entries from `ClassInfo` when the
/// defining class's AST is absent from the current compilation unit.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct InheritedTypedField {
    /// Beamtalk class name that declared this field.
    pub owning_class: String,
    /// Field (state variable) name.
    pub field_name: String,
    /// Human-readable type annotation (used verbatim in diagnostics).
    pub type_name: String,
}

/// An internal (non-field) key every compiled actor's state map carries.
#[derive(Debug, Clone, Copy)]
enum InternalStateKey {
    /// `'__class_mod__'`: the instance's compiled class module.
    ClassMod,
    /// `'__shape_version__'`: ADR 0123's hot-reload shape version.
    ShapeVersion,
}

impl InternalStateKey {
    fn name(self) -> &'static str {
        match self {
            Self::ClassMod => sentinel_tags::CLASS_MOD,
            Self::ShapeVersion => "__shape_version__",
        }
    }
}

/// The internal keys of an actor's state map, in the order `init/1` writes
/// them. The single source for both the initial state map
/// (`internal_state_prefix_doc`) and the committed-state guard
/// (`clean_state_let`, BT-4113).
const INTERNAL_STATE_KEYS: [InternalStateKey; 2] =
    [InternalStateKey::ClassMod, InternalStateKey::ShapeVersion];

/// BT-4113: the declared fields (the class's and its ancestors') a
/// committed state map of an actor holds when it carries no codegen-internal
/// `__local__` temporaries; with [`INTERNAL_STATE_KEYS`], its whole key set.
/// Built by `committed_state_keys`.
#[derive(Debug)]
struct CommittedStateKeys(Vec<EcoString>);

/// BT-4113: `let <dest> = <src with __local__ temporaries stripped> in`.
///
/// With a known committed key set (`committed_state_keys`), a new state that
/// holds every one of those keys **and** has exactly that many keys holds
/// nothing else, so no `__local__` key: it is kept as is. Anything else (a
/// temporary present, or a field set that differs from the compile-time one,
/// e.g. after hot reload or in a stale subclass of a redefined ancestor)
/// pays for `beamtalk_actor:strip_local_temps/1`'s scan of every key. The
/// check is one map pattern match plus `map_size/1`, with no key-name
/// inspection. Without a key set, it always strips.
///
/// Checking presence as well as size keeps the fast path sound whatever the
/// runtime field set is. A size check alone would let a `__local__` key
/// through whenever the live map had one declared field fewer than the
/// compile-time count, and a stale subclass can: redefining a superclass
/// migrates its loaded subclasses' live instances
/// (`beamtalk_repl_loader_activate:hot_reload_descendants/2`) and their `init/1`
/// calls the new superclass `init/1`, but their own modules are never
/// recompiled.
fn clean_state_let(
    dest: &'static str,
    src: &'static str,
    state_keys: Option<&CommittedStateKeys>,
    body: Document<'static>,
) -> Document<'static> {
    let strip = form::call("beamtalk_actor", "strip_local_temps", [leaf::var(src)]);
    let value = match state_keys {
        None => strip,
        Some(CommittedStateKeys(fields)) => {
            let keys = INTERNAL_STATE_KEYS
                .iter()
                .map(|k| k.name().to_owned())
                .chain(fields.iter().map(ToString::to_string));
            let mut pattern: Vec<Document<'static>> = Vec::new();
            let mut size: i64 = 0;
            for (i, key) in keys.enumerate() {
                if i > 0 {
                    pattern.push(Document::Str(", "));
                }
                pattern.push(leaf::atom(key));
                pattern.push(Document::Str(" := "));
                pattern.push(leaf::var(format!("_Key{i}")));
                size += 1;
            }
            case_block(
                leaf::var(src),
                [
                    form::clause(
                        [docvec!["~{", Document::Vec(pattern), "}~"]],
                        Some(form::call(
                            "erlang",
                            "=:=",
                            [
                                form::call("erlang", "map_size", [leaf::var(src)]),
                                leaf::int_lit(size),
                            ],
                        )),
                        leaf::var(src),
                        Layout::Flat,
                    ),
                    form::clause([leaf::var("_")], None, strip, Layout::Flat),
                ],
            )
        }
    };
    bind(leaf::var(dest), value, body)
}

/// The Core Erlang variable names of one pdict install / uninstall bracket
/// around a `safe_dispatch` (BT-4888): the `State` the install replaced, the
/// uninstall's result, and the call-stack erase's result. The re-entrant
/// self-send brackets use one fixed set; each `handle_continue` chain entry
/// needs its own, because its brackets nest in one function body.
struct PdictVars {
    old: String,
    restore: String,
    clear: String,
}

impl PdictVars {
    /// The single bracket of a `handle_call` / `handle_cast` / continuation
    /// dispatch.
    fn reentrant() -> Self {
        Self {
            old: "_OldState".to_string(),
            restore: "_RestoreOk".to_string(),
            clear: "_ClearStack".to_string(),
        }
    }

    /// The bracket around chain entry `idx`'s `initialize` in
    /// `generate_handle_continue`.
    fn chain_step(idx: &str) -> Self {
        Self {
            old: format!("_OldPdict{idx}"),
            restore: format!("_RestorePdict{idx}"),
            clear: format!("_ClearStack{idx}"),
        }
    }
}

/// The pieces that vary between the three "log the error, then stop" arms
/// of the generated `init/1` (BT-4887).
struct LogAndStop<'a> {
    /// Variable bound to `beamtalk_error:format_safe/1,2`'s message.
    msg_var: String,
    /// Variable bound to the class's `class_name/0`.
    class_name_var: String,
    /// The class module whose `class_name/0` is called.
    module_name: &'a str,
    /// The arguments of `format_safe` (one error, or `{Type, Reason}, Stack`).
    format_safe_args: Document<'static>,
    /// An extra `let <var> = beamtalk_error:format(<arg>) in` between the
    /// class-name and the logger call (the uninitialized-field arm stops with
    /// the formatted error rather than the raw one).
    formatted_var: Option<(String, Document<'static>)>,
    /// The logger's `'reason'` value.
    reason: Document<'static>,
    /// The logger's optional `'stacktrace'` value.
    stacktrace: Option<Document<'static>>,
    /// The first element after `'stop'`.
    stop_reason: Document<'static>,
    /// The state element of the `{'stop', Reason, State}` tuple.
    stop_state: Document<'static>,
}

/// BT-4887: the one "`format_safe` + `class_name` + `logger:error` + `{'stop', …}`"
/// arm shared by the generated `init/1`'s uninitialized-field check and its
/// two `{'error', …}` result clauses.
fn log_and_stop_arm(arm: LogAndStop<'_>) -> Document<'static> {
    let mut log_map = vec![
        Document::Str("~{'class' => "),
        leaf::var(arm.class_name_var.clone()),
        Document::Str(", 'reason' => "),
        arm.reason,
    ];
    if let Some(stack) = arm.stacktrace {
        log_map.push(Document::Str(", 'stacktrace' => "));
        log_map.push(stack);
    }
    log_map.push(Document::Str(", 'domain' => ['beamtalk'|['runtime'|[]]]}~"));
    let stop = form::tuple([leaf::atom("stop"), arm.stop_reason, arm.stop_state]);
    let log = bind(
        leaf::var("_"),
        form::call(
            "logger",
            "error",
            [leaf::var(arm.msg_var.clone()), Document::Vec(log_map)],
        ),
        stop,
    );
    let formatted = match arm.formatted_var {
        Some((var, format_arg)) => bind(
            leaf::var(var),
            form::call("beamtalk_error", "format", [format_arg]),
            log,
        ),
        None => log,
    };
    bind(
        leaf::var(arm.msg_var),
        form::call("beamtalk_error", "format_safe", [arm.format_safe_args]),
        bind(
            leaf::var(arm.class_name_var),
            form::call(arm.module_name, "class_name", []),
            formatted,
        ),
    )
}

mod handle_continue;
mod init;
mod lifecycle;
mod messages;

#[cfg(test)]
mod tests {
    use crate::core_erlang::CoreErlangGenerator;
    use beamtalk_core::ast::{
        ClassDefinition, Expression, Identifier, Literal, MessageSelector, MethodDefinition,
        Module, StateDeclaration, TypeAnnotation,
    };
    use beamtalk_core::source_analysis::Span;
    use beamtalk_core::test_helpers::test_support::empty_module;

    fn s() -> Span {
        Span::new(0, 0)
    }

    fn id(name: &str) -> Identifier {
        Identifier::new(name, s())
    }

    fn simple_ta(name: &str) -> TypeAnnotation {
        TypeAnnotation::Simple(id(name))
    }

    fn union_ta(types: Vec<TypeAnnotation>) -> TypeAnnotation {
        TypeAnnotation::Union { types, span: s() }
    }

    fn module_with_class(class: ClassDefinition) -> Module {
        Module {
            classes: vec![class],
            ..empty_module()
        }
    }

    // Nilability itself (ADR 0124 §7 A1) is now a `beamtalk-core` predicate —
    // `beamtalk_core::semantic_analysis::requires_definite_assignment` /
    // `is_nilable_type_name`, unit-tested directly in
    // `crates/beamtalk-core/src/semantic_analysis/definite_assignment.rs`
    // (plain non-nilable, `| Nil`, `| UndefinedObject`, alias-to-nilable,
    // alias-to-non-nilable, intersection, negation, `late`). What remains
    // here is codegen's own wiring: `inherited_typed_no_default_fields`
    // actually calls the shared predicate for both the AST path and the
    // cross-file `ClassInfo` path — see the `inherited_typed_no_default_fields`
    // tests below.

    // --- user_defined_initialize_chain (no-hierarchy fallback) ---

    fn class_with_initialize(name: &str) -> ClassDefinition {
        let init_method = MethodDefinition::new(
            MessageSelector::Unary("initialize".into()),
            Vec::new(),
            Vec::new(),
            s(),
        );
        ClassDefinition::new(id(name), id("Actor"), Vec::new(), vec![init_method], s())
    }

    fn class_without_initialize(name: &str) -> ClassDefinition {
        ClassDefinition::new(id(name), id("Actor"), Vec::new(), Vec::new(), s())
    }

    #[test]
    fn user_defined_initialize_chain_no_hierarchy_with_initialize() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_initialize("MyActor"));
        let chain = generator.user_defined_initialize_chain(&module, "MyActor");
        assert_eq!(chain.len(), 1);
        assert_eq!(
            chain[0].module_name,
            generator.compiled_module_name("MyActor")
        );
    }

    #[test]
    fn user_defined_initialize_chain_no_hierarchy_without_initialize() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_without_initialize("MyActor"));
        let chain = generator.user_defined_initialize_chain(&module, "MyActor");
        assert!(chain.is_empty());
    }

    #[test]
    fn user_defined_initialize_chain_no_hierarchy_unknown_leaf_class() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_initialize("MyActor"));
        let chain = generator.user_defined_initialize_chain(&module, "UnknownClass");
        assert!(chain.is_empty());
    }

    // --- class_or_ancestor_overrides_terminate (BT-3596, no-hierarchy fallback) ---

    fn class_with_terminate(name: &str) -> ClassDefinition {
        let terminate_method = MethodDefinition::new(
            MessageSelector::Keyword(vec![beamtalk_core::ast::KeywordPart::new(
                "terminate:",
                s(),
            )]),
            Vec::new(),
            Vec::new(),
            s(),
        );
        ClassDefinition::new(
            id(name),
            id("Actor"),
            Vec::new(),
            vec![terminate_method],
            s(),
        )
    }

    #[test]
    fn class_or_ancestor_overrides_terminate_no_hierarchy_with_override() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_terminate("MyActor"));
        assert!(generator.class_or_ancestor_overrides_terminate(&module, "MyActor"));
    }

    #[test]
    fn class_or_ancestor_overrides_terminate_no_hierarchy_without_override() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_without_initialize("MyActor"));
        assert!(!generator.class_or_ancestor_overrides_terminate(&module, "MyActor"));
    }

    #[test]
    fn class_or_ancestor_overrides_terminate_no_hierarchy_unknown_leaf_class() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_terminate("MyActor"));
        assert!(!generator.class_or_ancestor_overrides_terminate(&module, "UnknownClass"));
    }

    // --- inherited_typed_no_default_fields (no-hierarchy fallback) ---

    #[test]
    fn inherited_typed_no_default_fields_typed_no_default_returned() {
        let field = StateDeclaration::with_type(id("count"), simple_ta("Integer"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].field_name, "count");
        assert_eq!(fields[0].type_name, "Integer");
        assert_eq!(fields[0].owning_class, "MyActor");
    }

    #[test]
    fn inherited_typed_no_default_fields_typed_with_default_skipped() {
        let default_val = Expression::Literal(Literal::Integer(0), s());
        let field = StateDeclaration::with_type_and_default(
            id("count"),
            simple_ta("Integer"),
            default_val,
            s(),
        );
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_nilable_type_skipped() {
        let field = StateDeclaration::with_type(id("opt_val"), simple_ta("Nil"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_undefined_object_union_skipped() {
        // ADR 0124 §7 A1: the previous codegen-local predicate only matched
        // the literal `Nil` spelling, so `T | UndefinedObject` (the canonical
        // nil class) was wrongly treated as non-nilable. The shared
        // `beamtalk-core` predicate this now delegates to recognises both.
        let field = StateDeclaration::with_type(
            id("opt_val"),
            union_ta(vec![simple_ta("String"), simple_ta("UndefinedObject")]),
            s(),
        );
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_untyped_skipped() {
        let field = StateDeclaration::new(id("raw"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_unknown_class_returns_empty() {
        let field = StateDeclaration::with_type(id("count"), simple_ta("Integer"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "UnknownClass");
        assert!(fields.is_empty());
    }
}
