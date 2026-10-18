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
//! - [`version`] — `State{N}`/`Self{N}` version-counter helpers

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

/// Code generation context.
///
/// Determines how expressions are compiled based on the execution environment:
/// - **Actor**: Process-based with mutable state, async messaging
/// - **`ValueType`**: Plain maps with immutable semantics, sync function calls
/// - **Repl**: Interactive evaluation with bindings map
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// widened from `pub(crate)` — `beamtalk-repl` sets `context` to
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
/// # Context Structs
///
/// Fields are organized into context-specific groups to reduce the cognitive
/// load of the god object:
/// - [`ReplContext`] — REPL-specific state (`is_repl_mode`, etc.)
/// - [`ClassContext`] — Class/actor-specific state (`class_identity`, `class_var_*`, etc.)
/// - [`ValueTypeContext`] — Value-type-specific state (`self_version`, `current_nlr_token`)
///
/// Each context is `Option<T>` on the generator, set only when relevant.
/// Accessor methods provide safe defaults when the context is absent.
#[expect(
    clippy::struct_excessive_bools,
    reason = "Generator flags are context switches, not configuration"
)]
// widened from `pub(crate)` to `pub` (ADR 0117 Decision step 2),
// which brought this struct under the `missing_debug_implementations` lint
// (public-only). Not deriving `Debug`: several fields (e.g.
// `PrimitiveBindingTable`) are internal codegen state with no existing
// `Debug` impl, and this struct was never meant to be inspected/printed —
// only constructed and driven through its own methods.
#[allow(missing_debug_implementations)]
// widened from `pub(crate)` — the standalone `beamtalk-repl` crate
// (ADR 0117 Decision step 2) builds `CoreErlangGenerator` directly and reads
// its REPL-relevant state. Its many other fields stay module-private; only
// this struct and the specific members `beamtalk-repl` touches are `pub`.
pub struct CoreErlangGenerator {
    /// The module name being generated (ref-counted for O(1) clone).
    pub module_name: EcoString,
    /// Variable binding and scope management.
    pub(in crate::core_erlang) var_context: VariableContext,
    /// State threading for field assignments. `VersionCounter` is the
    /// single implementation shared with
    /// `ValueTypeContext::self_version` (formerly `StateThreading`).
    pub(in crate::core_erlang) state_threading: VersionCounter,
    /// Whether we're inside a loop body (use `StateAcc` instead of `State`)
    pub(in crate::core_erlang) in_loop_body: bool,
    /// ADR 0132 §2: whether the method body being lowered is a threaded
    /// entry's body (one `MethodBody` frame over the trailing `Acc` family).
    /// Read only through [`Self::threaded_body_kind`], which derives the
    /// body's kind from `context`/`in_class_method`. Never set in
    /// production: its only setter, the class-method threaded entry (ADR 0132
    /// Phase 1c), was removed by ADR 0133 Phase 2 (BT-3904), and BT-3905
    /// deletes this flag with the `Acc` family. A setter would have to scope
    /// it to the one body it lowers (set on entry, restored on exit,
    /// including on error), or the next method inherits `Acc`.
    ///
    /// **The first setter must generalise these single-family sites first.**
    /// While set, `eligible_families` adds `Acc` (so a class body's set is
    /// `[Acc]` and a value-type or actor body's has two entries), but these
    /// consumers assume at most one family or read a non-empty set as "this is
    /// a value-type/actor construct", and would silently miscompile (no
    /// `debug_assert!`: CLAUDE.md forbids one, and `verify()` cannot see a
    /// dropped slot):
    /// - read only `families.as_slice().first()`, dropping every later family
    ///   (`Acc` is last, so it is the one lost): the readers in
    ///   `control_flow/exception_handling.rs` (the `exception_family_slot`
    ///   family and the `on:do:`/`ensure:` cleanup baseline) and
    ///   `value_type_codegen.rs` (`extract_vt_loop_family_slot`);
    /// - gate on `families.as_slice().is_empty()`, which flips to "has a
    ///   family" for a class body that now carries `Acc`: the gates in
    ///   `control_flow/conditionals.rs` and in `value_type_codegen.rs`.
    ///
    /// Find them with `grep -n 'families.as_slice().first()'` and
    /// `grep -n 'families.as_slice().is_empty()'` over
    /// `crates/beamtalk-codegen/src/core_erlang`. One `is_empty()` gate is
    /// excluded: `FoldFamilySlots::new` (`control_flow/family_slots.rs`) was
    /// checked and handles multiple families.
    ///
    /// Tracked on BT-3871 (class methods) and BT-3872 (value-type methods).
    ///
    /// **BT-3871 (class methods) is the first setter and decided the
    /// generalisation this way:** `Acc` is a family of a *construct* only when
    /// that construct holds a use of a threading parameter
    /// (`CoreErlangGenerator::is_family_mutation` answers per expression, not
    /// "always"), so a loop, fold, conditional or `on:do:`/`ensure:` that does
    /// not invoke a threading parameter is lowered exactly as in a method with
    /// none, and the sites above never see an `Acc` family. A use inside such a
    /// construct is ADR 0132 Phase 3 (the predicate in `tier2_callee` makes the
    /// method non-threading), and is refused loudly: the single-family sites
    /// above report `VerifyError::ConstructFamilyDropped` for an `Acc` they
    /// cannot carry (`CoreErlangGenerator::reject_unthreadable_acc_family`), and
    /// the threading parameter's own identifier cannot be lowered outside a
    /// recognised use (`generate_identifier`). BT-3872 reuses the same rules
    /// for `[SelfVt, Acc]`.
    pub(in crate::core_erlang) in_threaded_body: bool,
    /// ADR 0132 §2: the per-body state of a threaded body (its threading
    /// parameters, the `Acc` version counter). Always empty since ADR 0133
    /// Phase 2 (BT-3904) removed the class-method threaded entries, the only
    /// threaded bodies; deleted with the `Acc` family by BT-3905.
    pub(in crate::core_erlang) threaded_body: crate::core_erlang::threaded_body::ThreadedBodyState,
    /// ADR 0111 Addendum 5, §Branch-context version discipline:
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
    /// The [`threaded_ir::FrameId`] of the innermost currently-active branch
    /// context — distinct from `branch_frame_counter`, which only mints
    /// fresh, never-reused frame identities and must NOT be read back as
    /// "the frame I am logically inside right now". A sibling branch that
    /// opens and closes (e.g. a second `ifTrue:` in the same loop body)
    /// bumps `branch_frame_counter` without restoring it, so reading it
    /// directly after such a sibling closes yields that now-closed
    /// sibling's frame instead of the enclosing scope's own frame (BT-3623).
    /// `enter_branch_context` saves/sets/restores this field the same way it
    /// does `state_version`/`self_version`; `0` means
    /// [`threaded_ir::FrameId::ROOT`] (no branch context currently active).
    pub(in crate::core_erlang) active_branch_frame: u32,
    /// The generator's loop-body context — the nine fields (hybrid/direct-params
    /// mode flags, pre-extracted field
    /// variable maps) that only have meaning while compiling a loop body,
    /// grouped into one `control_flow`-owned value. See [`LoopMode`].
    pub(in crate::core_erlang) loop_mode: LoopMode,
    /// Code generation context (`Actor`, `ValueType`, or `Repl`).
    /// Determines variable naming and method dispatch strategy.
    // widened from `pub(crate)` — `beamtalk-repl` sets this to
    // `CodeGenContext::Repl` around its own generation calls.
    pub context: CodeGenContext,
    /// Nesting depth of block (closure) bodies.
    /// When > 0, self-cast sends in Actor context must route through the
    /// actor mailbox (`beamtalk_message_dispatch:cast/3`) instead of calling
    /// `safe_dispatch` directly, because the block may execute in a different
    /// process (e.g. Timer callback, cross-actor callback).
    pub(in crate::core_erlang) block_depth: usize,
    /// ADR 0130 §5: the Core Erlang variable holding the class-variable
    /// capture of the innermost enclosing block literal that reads a class
    /// variable (`beamtalk_class_vars:capture/2` bound at its creation), or
    /// `None` at method level and inside blocks that read none. While `Some`,
    /// every class-variable read's miss path lowers to the 3-arity
    /// captured-fallback helper with this variable.
    pub(in crate::core_erlang) class_var_capture: Option<String>,
    /// BT-3719: the per-method class-variable key binding. While a
    /// class-method body is being lowered (`class_method_body_doc`) the `CVKey`
    /// variable is minted lazily at the first inlined class-variable access and
    /// bound once around the body, so every access reuses it instead of
    /// rebuilding `{'$bt_class_vars', element(2, ClassSelf)}`. Outside a
    /// class-method body an access inlines the key.
    pub(in crate::core_erlang) class_var_key_scope: super::class_var_keys::KeyScope,
    /// Original source text for extracting method source.
    pub(in crate::core_erlang) source_text: Option<String>,
    /// Source identity of each provision-bearing protocol (ADR 0127 §3);
    /// `MethodFrame::enter` swaps `source_text`/`source_path` to a flattened
    /// method's protocol's for the method's body.
    pub(in crate::core_erlang) protocol_sources:
        beamtalk_core::semantic_analysis::ProtocolSourceMap,
    /// Primitive binding table from compiled stdlib (ADR 0007).
    /// Used by `generate_primitive()` for method body compilation via static methods.
    #[allow(dead_code)] // stored for future call-site optimization with static typing
    pub(in crate::core_erlang) primitive_bindings: PrimitiveBindingTable,
    /// Parameters of the current method being compiled (if any).
    /// Used by `Expression::Primitive` to generate dispatch argument lists.
    pub(in crate::core_erlang) current_method_params: Vec<String>,
    /// Declared types of the current method's parameters, keyed by
    /// **source** parameter name → simple type name (e.g. `"other" -> "Number"`).
    /// Used by the arithmetic fast-path classifier
    /// (`receiver_is_statically_numeric`) to drop the runtime `is_number` guard
    /// when a receiver is a `:: Integer/Float/Number`-annotated parameter. Only
    /// `Simple` annotations are recorded; absence falls back to the guard, which
    /// is always correct. Cleared at every method-body entry so a prior method's
    /// annotations never leak into the next.
    pub(in crate::core_erlang) current_method_param_types:
        std::collections::HashMap<String, String>,
    /// Maps an instance field's source name → what codegen knows about its
    /// runtime type ([`FieldTypeFact`](crate::core_erlang::field_types::FieldTypeFact)),
    /// for the operator fast-path classifiers. A `self.<field>` read stays on
    /// the bare BIF only when its fact says so: a declared primitive type (a
    /// union counts when every non-`Nil` member is primitive), or an untyped
    /// field with numeric evidence (numeric-literal default, numeric in-class
    /// writes — keeps the `self.count := self.count + 1` counter bare). Any
    /// other field, and any name absent from the map, routes through the
    /// runtime guard so an object value dispatches (BT-3892). Populated at
    /// value-type / actor / supervisor class entry and for extension targets.
    pub(in crate::core_erlang) current_class_field_types:
        std::collections::HashMap<String, crate::core_erlang::field_types::FieldTypeFact>,
    /// The class-side counterpart of `current_class_field_types`: facts for
    /// the class's `classState:` variables, consulted for `self.<name>` reads
    /// inside class methods (BT-3892).
    pub(in crate::core_erlang) current_class_var_types:
        std::collections::HashMap<String, crate::core_erlang::field_types::FieldTypeFact>,
    /// ADR 0118 phase 1a: sub-expressions `threaded_expression`'s
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
    /// operators); ADR 0118 phase 2b deleted the planner-driven
    /// consumers this used to run alongside (`hoisted_self_send_results`/
    /// `hoisted_field_reads`), so this is now the ONLY substitution
    /// mechanism a `threaded_expression`/`thread_ahead` caller relies on.
    pub(in crate::core_erlang) precompiled_subexprs: std::collections::HashMap<
        beamtalk_core::source_analysis::Span,
        sequencing::PrecompiledSubexpr,
    >,
    /// ADR 0131 Phase 2 (BT-3749): the paren-unwrapped spans of the
    /// current method body's sequenced operand sites
    /// (`beamtalk_core`'s `sequenced_operand_sites` over each statement) —
    /// where a loop or fold is a local-threading producer whose prelude
    /// the method body splices. Set by the method-body lowerings
    /// (`with_local_producer_sites`) and empty everywhere else.
    pub(in crate::core_erlang) local_producer_sites:
        std::collections::HashSet<beamtalk_core::source_analysis::Span>,
    /// BT-3827: the body-block span of the one fold a value-type consumer is
    /// compiling right now AND will unpack the `Self` slot of
    /// (`with_fold_self_slot_consumer`); `None` everywhere else. A fold that
    /// writes a field ([`CoreErlangGenerator::fold_threads_value_self`]) hands
    /// its `Self` back as a trailing result-tuple slot, so lowering it where
    /// nothing reads that slot (a stored block, an evaluated block's body, a
    /// receiver) would drop the write: the fold emitters refuse it instead
    /// (`CoreErlangGenerator::value_self_fold_slot`).
    pub(in crate::core_erlang) fold_self_slot_consumer:
        Option<beamtalk_core::source_analysis::Span>,
    /// ADR 0131 Phase 2c (BT-3796): whether any loop or fold frame whose body
    /// is being lowered (`with_body_producer_sites`) is a fold's — the
    /// `outer_fold` input of `beamtalk_core`'s `LoopBodyContext` for the body
    /// of the frame nested in them. Deliberately crosses arm and closure
    /// boundaries between frames, the same way `local_producer_sites` is
    /// inherited by a nested body; the validator's `frame_sites` skip those
    /// boundaries when counting outer folds too, and a nested loop behind an
    /// arm or closure is judged by the rows there, not as a frame site. The
    /// agreement test's arm, closure and protected-body (`ensure:`, `on:do:`
    /// body and handler) two-level FRAMES pin that the two sides agree.
    pub(in crate::core_erlang) in_fold_frame: bool,
    /// ADR 0131 Phase 2b (BT-3793): the frame a local-threading producer's
    /// `LocalRebind`s are lowered by (§2's table), for every loop or fold
    /// body frame currently being lowered, keyed by that body's
    /// [`FrameId`](super::threaded_ir::FrameId). A body's statements are
    /// lowered before the loop node that encloses them exists, so the frame
    /// is registered here from the same `ThreadingPlan` the node is built
    /// from (`with_rebind_frame`) and the verifier's
    /// `LocalRebindModeMismatch` re-derives it from the finished node. The
    /// method root ([`FrameId::ROOT`](super::threaded_ir::FrameId::ROOT))
    /// is never registered: it is `RebindFrame::implicit_root`.
    pub(in crate::core_erlang) rebind_frames:
        std::collections::HashMap<super::threaded_ir::FrameId, super::threaded_ir::RebindFrame>,
    /// Source file path to embed as `beamtalk_source` module attribute.
    /// Set from `CodegenOptions::source_path` before generation begins.
    pub(in crate::core_erlang) source_path: Option<String>,
    /// Tier 2 block parameters for the current method being compiled.
    ///
    /// When a method parameter name is in this set, `value:` / `value:value:` calls
    /// on that parameter use the stateful Tier 2 protocol:
    /// `apply _Fun(Args..., State) → {Result, NewState}`.
    pub(in crate::core_erlang) tier2_block_params: std::collections::HashSet<String>,
    /// Local variables in the current method/block body known to hold
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
    /// For each name in `tier2_local_vars` whose assigned block's
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
    /// Pre-scanned Tier 2 block info for the current class.
    ///
    /// Maps method selector → list of parameter indices that receive Tier 2 blocks
    /// from self-sends within the same class. Populated by `scan_class_for_tier2_blocks`
    /// before method body generation.
    pub(in crate::core_erlang) tier2_method_info: std::collections::HashMap<String, Vec<usize>>,
    /// Diagnostic warnings emitted during code generation.
    ///
    /// Collected during generation and returned to callers via
    /// [`generate_module_with_warnings`]. Examples include stateful blocks
    /// passed to Erlang call sites where mutations will be silently dropped.
    pub(crate) codegen_warnings: Vec<Diagnostic>,
    /// Pre-computed semantic facts from the pre-codegen analysis pass.
    /// Used for block profile lookups and dispatch classification.
    pub(in crate::core_erlang) semantic_facts: beamtalk_core::semantic_analysis::SemanticFacts,
    /// Whether codegen diagnostics are enabled (`BEAMTALK_CODEGEN_DIAGNOSTICS=1`).
    /// When true, emits `Diagnostic::hint` for calling convention choices, dynamic dispatch
    /// fallbacks, non-local returns, and other codegen decisions.
    pub(in crate::core_erlang) codegen_diagnostics_enabled: bool,
    /// Whether `StateAcc` fallback should be promoted to warning (`BEAMTALK_WARN_STATEACC=1`).
    pub(in crate::core_erlang) warn_stateacc: bool,
    /// Selector name of the method currently being compiled.
    /// Used by Logger intrinsics to inject `beamtalk_selector` metadata.
    pub(in crate::core_erlang) current_method_selector: Option<String>,
    /// ADR 0065: Whether the current class is a Server subclass.
    /// When true, `generate_handle_info` dispatches to `handleInfo:` with
    /// log-and-continue error semantics instead of the default ignore-all stub.
    pub(in crate::core_erlang) is_server_subclass: bool,
    /// Pre-computed direct-call eligible class methods.
    ///
    /// Maps class name → `DirectCallClassInfo` for sealed classes whose class methods
    /// can be called directly (without `gen_server` dispatch). Computed from the class
    /// hierarchy in `generate_module_with_warnings`.
    pub(in crate::core_erlang) direct_call_eligible:
        std::collections::HashMap<String, DirectCallClassInfo>,
    /// ADR 0129 Phase 0b: the class being compiled's own direct-call-eligible
    /// class methods as sorted `(raw selector, safe fn name)` pairs — a view of
    /// `direct_call_eligible`, baked into `__beamtalk_meta/0`.
    pub(in crate::core_erlang) own_direct_class_methods: Vec<(String, String)>,
    /// REPL-specific codegen state. `Some` when in REPL mode.
    pub(in crate::core_erlang) repl_context: Option<ReplContext>,
    /// Class/actor-specific codegen state. `Some` when compiling a class.
    pub(in crate::core_erlang) class_context: Option<ClassContext>,
    /// Value-type-specific codegen state. `Some` when compiling value types.
    pub(in crate::core_erlang) value_type_context: Option<ValueTypeContext>,
    /// Snapshot of the class hierarchy for this generation (ADR 0078).
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
    /// cross-module-aware alias registry for this generation —
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
    /// ADR 0115 Phase 2: per-expression inferred types (keyed by
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
    /// keys of methods whose `return_type` was set by the return-type
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
    // widened from `pub(crate)` — `beamtalk-repl` constructs its
    // own generator.
    pub fn new(module_name: &str) -> Self {
        Self {
            module_name: EcoString::from(module_name),
            var_context: VariableContext::new(),
            state_threading: VersionCounter::new(),
            in_loop_body: false,
            in_threaded_body: false,
            threaded_body: crate::core_erlang::threaded_body::ThreadedBodyState::default(),
            branch_frame_counter: 0,
            active_branch_frame: 0,
            loop_mode: LoopMode::new(),
            context: CodeGenContext::Actor, // Default to Actor for backward compatibility
            block_depth: 0,
            class_var_capture: None,
            class_var_key_scope: super::class_var_keys::KeyScope::Unscoped,
            source_text: None,
            protocol_sources: std::collections::HashMap::new(),
            primitive_bindings: PrimitiveBindingTable::new(),
            current_method_params: Vec::new(),
            current_method_param_types: std::collections::HashMap::new(),
            current_class_field_types: std::collections::HashMap::new(),
            current_class_var_types: std::collections::HashMap::new(),
            precompiled_subexprs: std::collections::HashMap::new(),
            local_producer_sites: std::collections::HashSet::new(),
            fold_self_slot_consumer: None,
            in_fold_frame: false,
            rebind_frames: std::collections::HashMap::new(),
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
            own_direct_class_methods: Vec::new(),
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
        // `BeamtalkRuntime` → `beamtalk_runtime`), use it as-is.
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
