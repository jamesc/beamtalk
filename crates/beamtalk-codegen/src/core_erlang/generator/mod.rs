// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! [`CoreErlangGenerator`]: the code generator struct that coordinates
//! compilation of Beamtalk AST nodes to Core Erlang.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Holds the generator's own identity/config, field declarations, and
//! constructor; its methods are spread across sibling modules by kind:
//! - [`context`] — the `ReplContext`/`ClassContext`/`ValueTypeContext`/
//!   `SavedClassMethodCtx`/`DirectCallClassInfo` context structs
//! - [`branch_guard`] — [`BranchContextGuard`](branch_guard::BranchContextGuard)
//!   and the `with_branch_context`/`enter_branch_context` pair
//! - [`accessors`] — small field getters/setters and context-default wrappers
//! - [`version`] — `State{N}`/`ClassVars{N}`/`Self{N}` version-counter helpers

pub(in crate::core_erlang) mod accessors;
pub(in crate::core_erlang) mod branch_guard;
pub(in crate::core_erlang) mod context;
pub(in crate::core_erlang) mod version;

pub(in crate::core_erlang) use context::{
    ClassContext, DirectCallClassInfo, ReplContext, ValueTypeContext,
};

use crate::core_erlang::control_flow::LoopMode;
use crate::core_erlang::primitive_bindings::PrimitiveBindingTable;
use crate::core_erlang::sequencing;
use crate::core_erlang::threaded_ir::VersionCounter;
use crate::core_erlang::util::to_module_name;
use crate::core_erlang::variable_context::VariableContext;
use beamtalk_cerl_doc::{Document, INDENT, docvec, leaf, line, nest};
use beamtalk_core::source_analysis::Diagnostic;
use ecow::EcoString;

/// Code generation context (BT-213).
///
/// Determines how expressions are compiled based on the execution environment:
/// - **Actor**: Process-based with mutable state, async messaging
/// - **`ValueType`**: Plain maps with immutable semantics, sync function calls
/// - **Repl**: Interactive evaluation with bindings map
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// BT-3340: widened from `pub(crate)` — `beamtalk-repl` sets `context` to
// `CodeGenContext::Repl` on the generator it owns.
pub enum CodeGenContext {
    /// Generating code for an actor class (`gen_server` with async messaging).
    ///
    /// - Field access: `call 'maps':'get'('field', State)`
    /// - Method calls: Sync via `beamtalk_actor:sync_send` (ADR-0043)
    /// - State threading: Use State, State1, State2... for mutations
    Actor,

    /// Generating code for a value type class (plain Erlang functions).
    ///
    /// - Field access: `call 'maps':'get'('field', Self)`
    /// - Method calls: Synchronous function calls
    /// - No state threading: Value types are immutable
    ValueType,

    /// Generating code for REPL evaluation.
    ///
    /// - Variable access: `call 'maps':'get'('var', Bindings)`
    /// - Field access: Via maps:get from State (if in actor context)
    /// - Special handling for variable persistence across expressions
    Repl,
}

/// The generator delegates to specialized submodules:
/// - [`control_flow`] - Iteration and loop compilation
/// - [`dispatch_codegen`] - Message sending and dispatch
/// - [`expressions`] - Expression code generation
/// - [`gen_server`] - OTP `gen_server` scaffolding
/// - [`intrinsics`] - Compiler intrinsics (block, `ProtoObject`, `Object`, list iteration)
/// - [`operators`] - Binary operator code generation
///
/// # Context Structs (BT-1461)
///
/// Fields are organized into context-specific groups to reduce the cognitive
/// load of the god object:
/// - [`ReplContext`] — REPL-specific state (`is_repl_mode`, `workspace_mode`, etc.)
/// - [`ClassContext`] — Class/actor-specific state (`class_identity`, `class_var_*`, etc.)
/// - [`ValueTypeContext`] — Value-type-specific state (`self_version`, `current_nlr_token`)
///
/// Each context is `Option<T>` on the generator, set only when relevant.
/// Accessor methods provide safe defaults when the context is absent.
#[expect(
    clippy::struct_excessive_bools,
    reason = "Generator flags are context switches, not configuration"
)]
// BT-3340: widened from `pub(crate)` to `pub` (ADR 0117 Decision step 2),
// which brought this struct under the `missing_debug_implementations` lint
// (public-only). Not deriving `Debug`: several fields (e.g.
// `PrimitiveBindingTable`) are internal codegen state with no existing
// `Debug` impl, and this struct was never meant to be inspected/printed —
// only constructed and driven through its own methods.
#[allow(missing_debug_implementations)]
// BT-3340: widened from `pub(crate)` — the standalone `beamtalk-repl` crate
// (ADR 0117 Decision step 2) builds `CoreErlangGenerator` directly and reads
// its REPL-relevant state. Its many other fields stay module-private; only
// this struct and the specific members `beamtalk-repl` touches are `pub`.
pub struct CoreErlangGenerator {
    /// The module name being generated (ref-counted for O(1) clone).
    pub module_name: EcoString,
    /// Variable binding and scope management.
    pub(in crate::core_erlang) var_context: VariableContext,
    /// State threading for field assignments. BT-3131: `VersionCounter` is the
    /// single implementation shared with `ClassContext::class_var_version` and
    /// `ValueTypeContext::self_version` (formerly `StateThreading`).
    pub(in crate::core_erlang) state_threading: VersionCounter,
    /// BT-153: Whether we're inside a loop body (use `StateAcc` instead of `State`)
    pub(in crate::core_erlang) in_loop_body: bool,
    /// BT-3146 (ADR 0111 Addendum 5, §Branch-context version discipline):
    /// monotonic counter minting a fresh [`threaded_ir::FrameId`] per
    /// [`Self::enter_branch_context`] call — every `with_branch_context` arm
    /// (conditional branch, `on:do:`/`ensure:` body, loop body) gets its own
    /// distinct frame identity, never reused, so sibling arms that legitimately
    /// reach the same `State` version in disjoint scopes are modeled as
    /// distinct producer/consumer identities rather than colliding as a false
    /// [`threaded_ir::VerifyError::NonLinearVersion`]. `0` is reserved for
    /// [`threaded_ir::FrameId::ROOT`] (the method's own entry frame, never
    /// allocated by this counter); the first `enter_branch_context` call
    /// mints frame `1`. Never reset — frame identity must stay unique across
    /// an entire module compile, not just within one method.
    pub(in crate::core_erlang) branch_frame_counter: u32,
    /// The generator's loop-body context — the nine fields (hybrid/direct-params
    /// mode flags, `ClassVars`-threading side channels, pre-extracted field
    /// variable maps) that only have meaning while compiling a loop body,
    /// grouped into one `control_flow`-owned value. See [`LoopMode`].
    pub(in crate::core_erlang) loop_mode: LoopMode,
    /// BT-213: Code generation context (`Actor`, `ValueType`, or `Repl`).
    /// Determines variable naming and method dispatch strategy.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` sets this to
    // `CodeGenContext::Repl` around its own generation calls.
    pub context: CodeGenContext,
    /// BT-1475: Nesting depth of block (closure) bodies.
    /// When > 0, self-cast sends in Actor context must route through the
    /// actor mailbox (`beamtalk_message_dispatch:cast/3`) instead of calling
    /// `safe_dispatch` directly, because the block may execute in a different
    /// process (e.g. Timer callback, cross-actor callback).
    pub(in crate::core_erlang) block_depth: usize,
    /// BT-101: Original source text for extracting method source.
    pub(in crate::core_erlang) source_text: Option<String>,
    /// BT-295: Primitive binding table from compiled stdlib (ADR 0007).
    /// Used by `generate_primitive()` for method body compilation via static methods.
    #[allow(dead_code)] // stored for future call-site optimization with static typing
    pub(in crate::core_erlang) primitive_bindings: PrimitiveBindingTable,
    /// BT-295: Parameters of the current method being compiled (if any).
    /// Used by `Expression::Primitive` to generate dispatch argument lists.
    pub(in crate::core_erlang) current_method_params: Vec<String>,
    /// BT-2709: Declared types of the current method's parameters, keyed by
    /// **source** parameter name → simple type name (e.g. `"other" -> "Number"`).
    /// Used by the arithmetic fast-path classifier
    /// (`receiver_is_statically_numeric`) to drop the runtime `is_number` guard
    /// when a receiver is a `:: Integer/Float/Number`-annotated parameter. Only
    /// `Simple` annotations are recorded; absence falls back to the guard, which
    /// is always correct. Cleared at every method-body entry so a prior method's
    /// annotations never leak into the next.
    pub(in crate::core_erlang) current_method_param_types:
        std::collections::HashMap<String, String>,
    /// BT-2710 follow-up: maps an instance field's source name → its declared
    /// `Simple` type name, for the operator fast-path classifiers. Lets a
    /// `self.<field>` read with an explicit **non-primitive** (object) type be
    /// routed through the runtime guard so it dispatches (e.g. `self.lo < x`
    /// where `lo :: Money` reaches `Money>><`) instead of silently term-ordering
    /// the tagged map. Primitive-typed and *untyped* fields are absent from the
    /// guarding decision and stay on the bare BIF, preserving the counter /
    /// accumulator hot paths and their state-threading optimisations. Populated
    /// at value-type / actor class entry; cleared in extension bodies (which
    /// don't carry the target class's field types).
    pub(in crate::core_erlang) current_class_field_types: std::collections::HashMap<String, String>,
    /// ADR 0118 phase 1a (BT-3415): sub-expressions `threaded_expression`'s
    /// sequencing rule (`util.rs`) has already compiled — each one's value
    /// (a sequencing temp, or a state-effecting producer's pure result
    /// reference) keyed by the sub-expression's paren-unwrapped `Span`.
    /// `generate_expression` consults this FIRST, for every node, so when
    /// the enclosing parent is compiled through its ordinary AST-directed
    /// path it substitutes the already-sequenced value instead of
    /// compiling (and, for a self-send, dispatching) the child a second
    /// time. Entries are scoped by [`PrecompiledScope`]: registered by the
    /// sequencing helper, removed by `finish_precompiled_scope` right after
    /// the parent's compile, which also fails loudly if any entry was never
    /// consulted — the parent's compile path bypassed `generate_expression`
    /// for that child, so its prelude ran without its value being used
    /// (a double dispatch or a dropped operand, never silently).
    ///
    /// This started as the phase-1a substitution mechanism for the one
    /// parent kind the sequencing rule covered (message sends, incl. binary
    /// operators); ADR 0118 phase 2b (BT-3418) deleted the planner-driven
    /// consumers this used to run alongside (`hoisted_self_send_results`/
    /// `hoisted_field_reads`), so this is now the ONLY substitution
    /// mechanism a `threaded_expression`/`thread_ahead` caller relies on.
    pub(in crate::core_erlang) precompiled_subexprs: std::collections::HashMap<
        beamtalk_core::source_analysis::Span,
        sequencing::PrecompiledSubexpr,
    >,
    /// BT-845/BT-860: Source file path to embed as `beamtalk_source` module attribute.
    /// Set from `CodegenOptions::source_path` before generation begins.
    pub(in crate::core_erlang) source_path: Option<String>,
    /// BT-851: Tier 2 block parameters for the current method being compiled.
    ///
    /// When a method parameter name is in this set, `value:` / `value:value:` calls
    /// on that parameter use the stateful Tier 2 protocol:
    /// `apply _Fun(Args..., State) → {Result, NewState}`.
    pub(in crate::core_erlang) tier2_block_params: std::collections::HashSet<String>,
    /// BT-2797: Local variables in the current method/block body known to hold
    /// a Tier 2 block value — i.e. a `var := [block]` assignment where the
    /// block literal has captured-local or field mutations, *and* every later
    /// reference to `var` in the same body is a safe `value`/`value:`/etc.
    /// call (proven by `prescan_tier2_local_vars`, which runs once at the top
    /// of `lower_body_exprs_with_reply` before classification starts).
    /// `value:` / `value:value:` calls on a variable in this set use the
    /// stateful Tier 2 protocol: `apply _Fun(Args..., State) → {Result, NewState}`.
    /// A block whose safety can't be proven (returned, passed elsewhere,
    /// reassigned, ...) is deliberately left out — it keeps hitting the
    /// `generate_block`/`validate_stored_closure` compile-time diagnostic
    /// instead, since no known call site would thread state through it.
    pub(in crate::core_erlang) tier2_local_vars: std::collections::HashSet<String>,
    /// BT-2815: For each name in `tier2_local_vars` whose assigned block's
    /// only mutation is a captured outer local (not a field write), the
    /// names of those captured locals — mirrors what `captured_mutations_for_block`
    /// computes for an inline block literal, but keyed by variable name so a
    /// later `value(:...)` call site (which only has an identifier, not the
    /// block AST) can still find them. Populated alongside `tier2_local_vars`
    /// in `prescan_tier2_local_vars`; consulted by
    /// `get_inline_block_captured_mutations` to rebind the caller's own
    /// variable after the call, the same way it already does for an inline
    /// block literal receiver.
    pub(in crate::core_erlang) tier2_local_var_captured_mutations:
        std::collections::HashMap<String, Vec<String>>,
    /// BT-851: Pre-scanned Tier 2 block info for the current class.
    ///
    /// Maps method selector → list of parameter indices that receive Tier 2 blocks
    /// from self-sends within the same class. Populated by `scan_class_for_tier2_blocks`
    /// before method body generation.
    pub(in crate::core_erlang) tier2_method_info: std::collections::HashMap<String, Vec<usize>>,
    /// BT-855: Diagnostic warnings emitted during code generation.
    ///
    /// Collected during generation and returned to callers via
    /// [`generate_module_with_warnings`]. Examples include stateful blocks
    /// passed to Erlang call sites where mutations will be silently dropped.
    pub(crate) codegen_warnings: Vec<Diagnostic>,
    /// BT-1288: Pre-computed semantic facts from the pre-codegen analysis pass.
    /// Used for block profile lookups and dispatch classification.
    pub(in crate::core_erlang) semantic_facts: beamtalk_core::semantic_analysis::SemanticFacts,
    /// BT-1343: Whether codegen diagnostics are enabled (`BEAMTALK_CODEGEN_DIAGNOSTICS=1`).
    /// When true, emits `Diagnostic::hint` for calling convention choices, dynamic dispatch
    /// fallbacks, non-local returns, and other codegen decisions.
    pub(in crate::core_erlang) codegen_diagnostics_enabled: bool,
    /// BT-1343: Whether `StateAcc` fallback should be promoted to warning (`BEAMTALK_WARN_STATEACC=1`).
    pub(in crate::core_erlang) warn_stateacc: bool,
    /// BT-1435: Selector name of the method currently being compiled.
    /// Used by Logger intrinsics to inject `beamtalk_selector` metadata.
    pub(in crate::core_erlang) current_method_selector: Option<String>,
    /// ADR 0065 / BT-1457: Whether the current class is a Server subclass.
    /// When true, `generate_handle_info` dispatches to `handleInfo:` with
    /// log-and-continue error semantics instead of the default ignore-all stub.
    pub(in crate::core_erlang) is_server_subclass: bool,
    /// BT-1639: Pre-computed direct-call eligible class methods.
    ///
    /// Maps class name → `DirectCallClassInfo` for sealed classes whose class methods
    /// can be called directly (without `gen_server` dispatch). Computed from the class
    /// hierarchy in `generate_module_with_warnings`.
    pub(in crate::core_erlang) direct_call_eligible:
        std::collections::HashMap<String, DirectCallClassInfo>,
    /// BT-1461: REPL-specific codegen state. `Some` when in REPL mode.
    pub(in crate::core_erlang) repl_context: Option<ReplContext>,
    /// BT-1461: Class/actor-specific codegen state. `Some` when compiling a class.
    pub(in crate::core_erlang) class_context: Option<ClassContext>,
    /// BT-1461: Value-type-specific codegen state. `Some` when compiling value types.
    pub(in crate::core_erlang) value_type_context: Option<ValueTypeContext>,
    /// BT-1951: Snapshot of the class hierarchy for this generation (ADR 0078).
    ///
    /// Populated by `generate_module_with_warnings` before codegen begins. Used by
    /// actor `handle_continue` generation to walk the superclass chain and emit
    /// parent-first `initialize` dispatches, and by the post-initialize validation
    /// check to collect inherited typed-no-default fields.
    pub(in crate::core_erlang) class_hierarchy:
        Option<beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy>,
    /// ADR 0098 Phase 3: producing `BEAMTALK_VERSION`, baked into `__beamtalk_meta`.
    /// Supplied by the CLI; `None` for REPL/test codegen (key omitted).
    pub(in crate::core_erlang) beamtalk_version: Option<EcoString>,
    /// ADR 0098 Phase 3: producing compound OTP version (`<release>-<erts>`),
    /// baked into `__beamtalk_meta`. Supplied by the CLI; `None` omits the key.
    pub(in crate::core_erlang) otp_release: Option<EcoString>,
    /// BT-2932: cross-module-aware alias registry for this generation —
    /// this module's own `type_aliases` merged with any pre-loaded aliases
    /// from other modules in the same compilation unit
    /// (`CodegenOptions::pre_loaded_aliases`). Populated by
    /// `generate_module_with_warnings` before codegen begins; consumed by
    /// the `generate_class_specs`/`generate_method_spec`/
    /// `generate_type_alias`/`generate_alias_type_attrs` call sites in
    /// `actor_codegen.rs`, `value_type_codegen.rs`, `supervisor_codegen.rs`,
    /// and `gen_server/native_facade.rs` so an alias-typed annotation
    /// resolves to a `user_type` reference regardless of which module
    /// declared the alias. Empty (not `None`) when the module has no
    /// `type_aliases` and no pre-loaded aliases were supplied — mirrors
    /// `AliasRegistry::from_module_declarations`'s empty-registry default,
    /// so every downstream `Some(&self.alias_registry)` call site is a
    /// no-op for the common case.
    pub(in crate::core_erlang) alias_registry:
        beamtalk_core::semantic_analysis::alias_registry::AliasRegistry,
    /// BT-3217 (ADR 0115 Phase 2): per-expression inferred types (keyed by
    /// file-absolute `Span`), sourced from the driver's handed-off
    /// `AnalysisResult::type_map` when `CodegenOptions::with_analysis` was
    /// used, or from `infer_types_and_returns` in the self-sufficient path
    /// (`generate_module_with_warnings`, `mod.rs:944`). Consumed by
    /// `gen_server/methods.rs::build_method_xref_entry` to project each
    /// send's receiver type onto the xref `recv_type` field — see
    /// `docs/internal/adr-0115-phase1-spike-findings.md` §1 for why this
    /// field previously had no plumbing path into codegen. Empty (not
    /// populated) for codegen contexts that never ran type inference (rare
    /// unit-test-only paths); `recv_type` degrades safely to `dynamic` in
    /// that case, matching the runtime live-patch path's precedent.
    pub(in crate::core_erlang) type_map: beamtalk_core::semantic_analysis::TypeMap,
    /// BT-3249: keys of methods whose `return_type` was set by the return-type
    /// writeback pass (`apply_return_type_writeback_from_map`) rather than
    /// typed by the user — the same map used to build `module`/`module_owned`
    /// (whichever this generation's `generate_module_with_warnings` ended up
    /// using), populated once before codegen begins. Consulted by
    /// `gen_server/methods.rs::extract_method_source` so the image-resident
    /// `__source__` text it bakes never carries an inferred `-> Type`
    /// annotation the user never wrote, while the method's real
    /// `return_type` (used for `method_return_types` metadata, specs, etc.)
    /// stays untouched. Empty for codegen contexts that never ran writeback.
    pub(in crate::core_erlang) method_return_types_written_back: std::collections::HashMap<
        beamtalk_core::semantic_analysis::MethodReturnKey,
        beamtalk_core::semantic_analysis::InferredType,
    >,
}

impl CoreErlangGenerator {
    /// Creates a new code generator for the given module name.
    // BT-3340: widened from `pub(crate)` — `beamtalk-repl` constructs its
    // own generator.
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: EcoString::from(module_name),
            var_context: VariableContext::new(),
            state_threading: VersionCounter::new(),
            in_loop_body: false,
            branch_frame_counter: 0,
            loop_mode: LoopMode::new(),
            context: CodeGenContext::Actor, // Default to Actor for backward compatibility
            block_depth: 0,
            source_text: None,
            primitive_bindings: PrimitiveBindingTable::new(),
            current_method_params: Vec::new(),
            current_method_param_types: std::collections::HashMap::new(),
            current_class_field_types: std::collections::HashMap::new(),
            precompiled_subexprs: std::collections::HashMap::new(),
            source_path: None,
            tier2_block_params: std::collections::HashSet::new(),
            tier2_local_vars: std::collections::HashSet::new(),
            tier2_local_var_captured_mutations: std::collections::HashMap::new(),
            tier2_method_info: std::collections::HashMap::new(),
            codegen_warnings: Vec::new(),
            semantic_facts: beamtalk_core::semantic_analysis::SemanticFacts::default(),
            codegen_diagnostics_enabled: std::env::var("BEAMTALK_CODEGEN_DIAGNOSTICS")
                .is_ok_and(|v| v == "1"),
            warn_stateacc: std::env::var("BEAMTALK_WARN_STATEACC").is_ok_and(|v| v == "1"),
            current_method_selector: None,
            is_server_subclass: false,
            direct_call_eligible: std::collections::HashMap::new(),
            repl_context: Some(ReplContext::new()),
            class_context: Some(ClassContext::new()),
            value_type_context: Some(ValueTypeContext::new()),
            class_hierarchy: None,
            beamtalk_version: None,
            otp_release: None,
            alias_registry: beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::new(),
            type_map: beamtalk_core::semantic_analysis::TypeMap::new(),
            method_return_types_written_back: std::collections::HashMap::new(),
        }
    }

    /// Creates a new code generator with a primitive binding table.
    pub(in crate::core_erlang) fn with_bindings(
        module_name: &str,
        bindings: PrimitiveBindingTable,
    ) -> Self {
        let mut generator = Self::new(module_name);
        generator.primitive_bindings = bindings;
        generator
    }

    /// Generates the `start_link/1` function for supervised `gen_server` startup.
    ///
    /// This is the standard OTP entry point for starting a supervised `gen_server`.
    /// It calls `gen_server:start_link/3` directly with the provided init args.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'start_link'/1 = fun (InitArgs) ->
    ///     call 'gen_server':'start_link'('module_name', InitArgs, [])
    /// ```
    pub(in crate::core_erlang) fn generate_start_link_doc(&self) -> Document<'static> {
        docvec![
            "'start_link'/1 = fun (InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "call 'gen_server':'start_link'(",
                        leaf::atom(self.module_name.to_string()),
                        ", InitArgs, [])",
                    ],
                ]
            ),
            "\n\n",
        ]
    }

    /// Generates the `start_link/2` function for named `gen_server` startup.
    ///
    /// This is the OTP entry point for starting a supervised `gen_server` with
    /// a registered name (e.g. `{local, 'Transcript'}`). Used by workspace
    /// supervisors to start singleton actors under their binding name.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'start_link'/2 = fun (ServerName, InitArgs) ->
    ///     call 'gen_server':'start_link'(ServerName, 'module_name', InitArgs, [])
    /// ```
    pub(in crate::core_erlang) fn generate_start_link_named_doc(&self) -> Document<'static> {
        docvec![
            "'start_link'/2 = fun (ServerName, InitArgs) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "call 'gen_server':'start_link'(ServerName, ",
                        leaf::atom(self.module_name.to_string()),
                        ", InitArgs, [])",
                    ],
                ]
            ),
            "\n\n",
        ]
    }

    /// Generates the `dispatch/3` function that delegates to the actor's own Erlang module.
    ///
    /// For actor classes with `@primitive` methods, the compiled dispatch/4 calls
    /// `Module:dispatch(Selector, Args, Self)` (3-arity) for primitive method bodies.
    /// This function provides that 3-arity entry point, delegating to the actor's
    /// main Erlang module (e.g. `beamtalk_subprocess`) which exports `dispatch/3`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'dispatch'/3 = fun (Selector, Args, Self) ->
    ///     call 'beamtalk_subprocess':'dispatch'(Selector, Args, Self)
    /// ```
    pub(in crate::core_erlang) fn generate_primitive_dispatch_3_doc(&self) -> Document<'static> {
        // Call dispatch/3 on the actor's own Erlang module (same name as the backing
        // gen_server, e.g. `beamtalk_subprocess`). The actor module is responsible
        // for exporting `dispatch/3` to handle class-side @primitive methods.
        // If `to_module_name` already emits a `beamtalk_` prefix (e.g.
        // `BeamtalkInterface` → `beamtalk_interface`), use it as-is.
        let snake_name = to_module_name(&self.class_name());
        let actor_module_name = if snake_name.starts_with("beamtalk_") {
            snake_name
        } else {
            format!("beamtalk_{snake_name}")
        };
        docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec![
                        "call ",
                        leaf::atom(actor_module_name),
                        ":'dispatch'(Selector, Args, Self)",
                    ],
                ]
            ),
            "\n\n",
        ]
    }
}
