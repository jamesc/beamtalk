// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method body code generation and class registration.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates method dispatch case clauses, method body with state threading
//! and reply tuples, and the `register_class/0` on-load function.

use super::super::PrecompiledScope;
use super::super::selector_mangler::safe_class_method_fn_name;
use super::super::spec_codegen;
use super::super::value_type_codegen::has_opaque_native_representation;
use super::super::{
    CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis, threaded_ir,
};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::fname;
use beamtalk_cerl_doc::{Document, INDENT, join, leaf, line, nest};
use beamtalk_core::ast::{
    Block, CascadeMessage, ClassDefinition, ClassKind, Expression, Identifier, Literal, MapPair,
    MessageSelector, MethodDefinition, MethodKind, Module, ParameterDefinition,
    ProtocolMethodSignature, StateDeclaration, TypeAnnotation, TypeParamDecl, WellKnownSelector,
};
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use beamtalk_core::semantic_analysis::{InferredType, TypeProvenance};
use beamtalk_core::source_analysis::Span;
use beamtalk_core::unparse::{unparse_method_display_signature, unparse_type_annotation_display};
use ecow::EcoString;

/// Erlang's hard cap on atom size (ERTS source: `MAX_ATOM_CHARACTERS` / UTF-8
/// bytes).  Selectors or class names that exceed this limit cannot be emitted
/// as legal Core Erlang atoms and are dropped from xref / dispatch tables.
const MAX_ATOM_BYTES: usize = 255;

/// ADR 0098 Phase 3: producing-toolchain identity baked into a module's
/// `__beamtalk_meta/0` map so a *loaded* module is self-describing — consumers
/// (workspace attach, tooling) can detect staleness without re-reading the
/// on-disk stamp.
///
/// Both values are compile-time literals supplied by the CLI
/// (`beam_compiler.rs`), never a runtime `erlang:system_info/1` call in the
/// generated module: the latter would bake the bare OTP release (`"27"`) rather
/// than the compound key the stamp uses (`"27-15.0.1"`). `None` (REPL, tests, or
/// an older toolchain) omits the key entirely.
#[derive(Clone, Copy, Default)]
pub(crate) struct MetaProvenance<'a> {
    /// The producing `BEAMTALK_VERSION`, verbatim.
    pub beamtalk_version: Option<&'a str>,
    /// The producing compound OTP version (`<release>-<erts>`).
    pub otp_release: Option<&'a str>,
}

/// BT-3217 (ADR 0115 Phase 2): the xref `recv_type` write-path vocabulary a
/// message send's receiver `InferredType` projects onto — see
/// [`project_recv_type`] and `build_method_xref_entry`'s doc for the full
/// rule, and this PR's description for the `Meta{C}` decision.
enum RecvType {
    /// A concrete class-or-protocol name — `Known` resolving to exactly one
    /// name whose provenance the read path can trust (`Declared`,
    /// `Inferred`, or `Substituted`; ADR 0068's protocol names resolve
    /// through the same `Known` variant as classes).
    Name(EcoString),
    /// A class-object (metaclass) receiver (`InferredType::Meta{C}`, e.g.
    /// `Counter spawn`), rendered with the same `'<C> class'` convention as
    /// `beamtalk_class_registry:class_object_tag/1` rather than falling into
    /// the "otherwise unresolved" bucket by omission (spike §1e).
    ClassObject(EcoString),
    /// BT-3215: a `Union{members}` receiver where *every* member itself
    /// resolves to a single name or class-object tag (never `Dynamic`) —
    /// the already-rendered atoms, sorted and deduplicated. The read path's
    /// `is_relevant/3` treats this with OR-semantics: relevant iff *any*
    /// member is relevant, since the receiver could be any one of them at
    /// runtime.
    Union(Vec<EcoString>),
    /// BT-3215: same resolution rule as [`RecvType::Union`], for
    /// `Intersection{members}`. The read path uses AND-semantics: relevant
    /// only if *every* member is relevant, since the receiver must
    /// simultaneously satisfy all of them.
    Intersection(Vec<EcoString>),
    /// Everything else: `Negation`/`Dynamic`/`Never`, a `Union`/
    /// `Intersection` with at least one member that doesn't resolve to a
    /// single name (nested composed type, `Dynamic`, oversized atom, …),
    /// no `TypeMap` entry at all, or a `Known` whose provenance is
    /// `Extracted` (native/FFI, ADR 0075) or `Aliased` (ADR 0108) — neither
    /// of those two names has a `beamtalk_class_metadata` row, so coarsening
    /// them here (rather than deferring to the read path, which has no way
    /// to tell them apart from a genuine class) is the spike §4 fix.
    Dynamic,
}

/// BT-3217 (ADR 0115 Phase 2) write-path projection rule (spike §1e/§4):
/// projects one message send's receiver `InferredType` — looked up from the
/// type checker's `TypeMap` by the receiver's span — onto [`RecvType`].
fn project_recv_type(ty: &InferredType) -> RecvType {
    match ty {
        InferredType::Known {
            class_name,
            provenance,
            ..
        } => match provenance {
            // Neither an FFI/native type name nor an alias display name has
            // a `beamtalk_class_metadata` row the Phase 3 read path could
            // resolve — coarsen to `dynamic` at write time rather than let
            // the read path discover an unresolvable name (spike §4).
            TypeProvenance::Extracted | TypeProvenance::Aliased { .. } => RecvType::Dynamic,
            TypeProvenance::Declared(_)
            | TypeProvenance::Inferred(_)
            | TypeProvenance::Substituted(_) => RecvType::Name(class_name.clone()),
        },
        InferredType::Meta { class_name, .. } => RecvType::ClassObject(class_name.clone()),
        // BT-3215: project each member the same way a single-name receiver
        // would be projected; if every member resolves cleanly, key on the
        // member list instead of coarsening the whole composed type away.
        InferredType::Union { members, .. } => project_composed(members, RecvType::Union),
        InferredType::Intersection { members, .. } => {
            project_composed(members, RecvType::Intersection)
        }
        InferredType::Dynamic(_) | InferredType::Never | InferredType::Negation { .. } => {
            RecvType::Dynamic
        }
    }
}

/// BT-3215: shared `Union`/`Intersection` projection — resolves each member
/// to the same single atom [`recv_type_atom`] would render for it as a
/// standalone receiver, via [`project_recv_type`] recursively. A nested
/// composed type or anything else that isn't a clean single name (`Dynamic`,
/// an oversized atom, a native/alias-coarsened `Known`, …) makes any member
/// unresolvable — per Constraint 2 ("cannot be narrowed and must never be
/// excluded"), a *partial* member list would be unsound: dropping an
/// unresolvable member and keying on only the resolvable ones would make the
/// read path wrongly exclude a real dependent whose receiver happens to be
/// typed as that dropped member at runtime. So any unresolvable member
/// coarsens the *entire* composed type to `dynamic`, exactly like a
/// single-name receiver that doesn't resolve.
fn project_composed(members: &[InferredType], make: fn(Vec<EcoString>) -> RecvType) -> RecvType {
    let mut names: Vec<EcoString> = Vec::with_capacity(members.len());
    for member in members {
        let resolved = match project_recv_type(member) {
            RecvType::Name(name) if name.len() <= MAX_ATOM_BYTES => name,
            RecvType::ClassObject(name) if name.len() + " class".len() <= MAX_ATOM_BYTES => {
                EcoString::from(super::super::util::metaclass_tag(&name))
            }
            RecvType::Name(_)
            | RecvType::ClassObject(_)
            | RecvType::Union(_)
            | RecvType::Intersection(_)
            | RecvType::Dynamic => return RecvType::Dynamic,
        };
        names.push(resolved);
    }
    names.sort();
    names.dedup();
    make(names)
}

/// Renders a [`RecvType`] as the Core Erlang literal baked into a
/// `method_xref` send entry's `recv_type` field: a bare atom for
/// `Name`/`ClassObject`/`Dynamic`, or a `{'union' | 'intersection',
/// [Atom, ...]}` tuple for a composed type (BT-3215). Falls back to
/// `'dynamic'` for a name that would exceed the `MAX_ATOM_BYTES` cap —
/// `project_composed` already
/// enforces this per member, so `Union`/`Intersection` never reach here with
/// an oversized member atom.
fn recv_type_atom(recv_type: &RecvType) -> Document<'static> {
    match recv_type {
        RecvType::Name(name) if name.len() <= MAX_ATOM_BYTES => leaf::atom(name.to_string()),
        RecvType::ClassObject(name) if name.len() + " class".len() <= MAX_ATOM_BYTES => {
            leaf::atom(super::super::util::metaclass_tag(name))
        }
        RecvType::Union(names) => docvec!["{'union', ", recv_type_name_list_doc(names), "}"],
        RecvType::Intersection(names) => {
            docvec!["{'intersection', ", recv_type_name_list_doc(names), "}"]
        }
        RecvType::Name(_) | RecvType::ClassObject(_) | RecvType::Dynamic => leaf::atom("dynamic"),
    }
}

/// Renders a `[Atom, ...]` Core Erlang list of already-resolved member
/// names for a `Union`/`Intersection` `recv_type` (BT-3215) — the shared
/// bracket/comma-join helper the two `recv_type_atom` composed-type arms
/// use, mirroring `meta_type_repr_list_doc`'s bracket/join pattern.
fn recv_type_name_list_doc(names: &[EcoString]) -> Document<'static> {
    let parts: Vec<Document<'static>> = names.iter().map(|n| leaf::atom(n.to_string())).collect();
    docvec!["[", join(parts, &Document::Str(", ")), "]"]
}

/// BT-2734: Compiler-derived `__signature__` / `__doc__` selector-map entries
/// for a value class's auto-generated accessors, split by dispatch side.
///
/// Each `Vec` holds ready-to-embed `'selector' => <binary>` fragments (built by
/// [`CoreErlangGenerator::synthetic_selector_map_entry`]). Instance-side entries
/// feed the `methodSignatures` / `methodDocs` maps; class-side entries feed the
/// `classMethodSignatures` / `classMethodDocs` maps (the keyword constructor).
#[derive(Default)]
struct SyntheticAccessorMetadata {
    instance_sigs: Vec<Document<'static>>,
    instance_docs: Vec<Document<'static>>,
    class_sigs: Vec<Document<'static>>,
    class_docs: Vec<Document<'static>>,
}

/// BT-2734: one compiler-derived accessor's readable metadata:
/// `(selector, signature, doc)`. The pure, unit-testable intermediate produced
/// by [`CoreErlangGenerator::synthetic_value_accessor_entries`] before it is
/// rendered into Core Erlang `'selector' => <binary>` map fragments.
type SyntheticAccessorEntry = (String, String, String);

/// BT-2734: a value class's synthetic-accessor metadata, split by dispatch side.
/// `instance` holds slot getters and `with*:` setters; `class` holds the keyword
/// constructor.
#[derive(Default)]
struct SyntheticAccessorEntries {
    instance: Vec<SyntheticAccessorEntry>,
    class: Vec<SyntheticAccessorEntry>,
}

/// Collects the class names referenced by a type annotation into `out`
/// (ADR 0087 Phase 6, BT-2304).
///
/// Mirrors `collect_all_type_refs` in
/// [`beamtalk_core::method_source_walker`] — the walker hand-written-method
/// `references` rows use — so a synthetic accessor on a typed slot reports the
/// same referenced class names a hand-written accessor with the same type
/// signature would. `Singleton` / `Self` / `Self class` annotations carry no
/// class reference and are skipped.
fn collect_type_annotation_class_names(annotation: &TypeAnnotation, out: &mut Vec<String>) {
    match annotation {
        TypeAnnotation::Simple(id) => out.push(id.name.to_string()),
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            out.push(base.name.to_string());
            for param in parameters {
                collect_type_annotation_class_names(param, out);
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for ty in types {
                collect_type_annotation_class_names(ty, out);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_type_annotation_class_names(inner, out);
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            collect_type_annotation_class_names(base, out);
            collect_type_annotation_class_names(excluded, out);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            collect_type_annotation_class_names(left, out);
            collect_type_annotation_class_names(right, out);
        }
        TypeAnnotation::ClassOf { class_name, .. } => out.push(class_name.name.to_string()),
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

/// Extracts the package name from a BEAM module name following the
/// `bt@{package}@{class}` convention (ADR 0016/0070).
///
/// Returns `None` for module names that don't follow this convention
/// (e.g., stdlib modules like `beamtalk_integer` or REPL workspace modules).
///
/// # Examples
/// - `"bt@my_counter@counter"` → `Some("my_counter")`
/// - `"bt@stdlib@integer"` → `Some("stdlib")`
/// - `"beamtalk_integer"` → `None`
pub(crate) fn extract_package_from_module_name(module_name: &str) -> Option<String> {
    let parts: Vec<&str> = module_name.splitn(3, '@').collect();
    if parts.len() >= 3 && parts[0] == "bt" {
        Some(parts[1].to_string())
    } else {
        None
    }
}

/// Classification of how a method body expression should be handled for
/// state threading.  Produced by [`CoreErlangGenerator::classify_body_expr`]
/// and consumed by the unified [`CoreErlangGenerator::lower_body_exprs_with_reply`]
/// and [`CoreErlangGenerator::generate_conditional_branch_inline`].
pub(in crate::core_erlang) enum BodyExprKind {
    /// `^ value` — early return from method.
    EarlyReturn,
    /// `self fieldAt: name put: val` — reflective field mutation.
    SelfFieldAtPut,
    /// `self.field := value` — direct field assignment.
    FieldAssignment,
    /// `self.field := expr` where the RHS is control flow with mutations (BT-1477).
    FieldAssignmentControlFlow,
    /// `self fieldAt: name put: expr` where the RHS is control flow with field mutations.
    SelfFieldAtPutControlFlow,
    /// `{a, b} := expr` where the RHS is control flow with field mutations.
    DestructureAssignmentControlFlow,
    /// `var := expr` where the RHS is a Tier 2 `value:` call.
    LocalAssignTier2,
    /// `var := [block]` where the block literal itself needs Tier 2 (captured-local
    /// or field mutations) — BT-2797. Unlike `LocalAssignTier2` (RHS *invokes* a
    /// Tier 2 block), here the RHS *is* the block literal being stored for later
    /// invocation (e.g. `blk := [:x | self.total := self.total + x]`).
    LocalAssignTier2Block,
    /// `var := expr` where the RHS is control flow with field mutations.
    LocalAssignControlFlow,
    /// `var := self method` — local assignment where RHS is a dispatching self-send.
    LocalAssignSelfSend,
    /// `var := expr` — simple local assignment.
    LocalAssignPure,
    /// `{a, b} := expr` — destructure assignment.
    DestructureAssignment,
    /// `super method` — super message send.
    SuperSend,
    /// `self error: "..."` — never returns.
    ErrorSend,
    /// Tier 2 `value:` call — returns `{Result, NewState}`.
    Tier2ValueCall,
    /// Tier 2 self-send with stateful block arguments.
    Tier2SelfSend(Vec<(usize, Vec<String>)>),
    /// Control flow with field mutations — returns `{Result, State}`.
    ControlFlowWithMutations,
    /// `self userMethod` — dispatching self-send via `safe_dispatch` (BT-1420).
    DispatchingSelfSend,
    /// Regular expression with no special state-threading needs.
    Pure,
}

/// Representation of a type in runtime meta (`method_info` `return_type` / `param_types`).
///
/// ADR 0068: Generic classes emit `{type_param, Name, Index}` tagged tuples
/// for type parameters and `{generic, Base, [Params]}` for parameterised types,
/// rather than flat atom strings.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum MetaTypeRepr {
    /// No type annotation — rendered as `'none'`.
    None,
    /// A concrete named type — rendered as `'TypeName'`.
    Atom(String),
    /// A reference to a class-level type parameter — rendered as
    /// `{'type_param', 'Name', Index}`.
    TypeParam { name: String, index: i32 },
    /// A parameterised type — rendered as
    /// `{'generic', 'Base', [Param1, Param2, ...]}`.
    Generic {
        base: String,
        parameters: Vec<MetaTypeRepr>,
    },
    /// A union type (BT-3076) — rendered as `{'union', [Member1, Member2, ...]}`.
    Union(Vec<MetaTypeRepr>),
    /// A singleton/literal type (BT-3076), e.g. `#north` — rendered as
    /// `{'singleton', 'north'}` (the name, without the leading `#`).
    Singleton(String),
}

/// Tuple representing a method entry for `method_info` / `class_method_info` meta maps.
///
/// Fields: (`erlang_selector`, `arity`, `return_type`, `param_types`, `is_sealed`, `is_internal`)
pub(super) type MethodInfoEntry = (String, usize, MetaTypeRepr, Vec<MetaTypeRepr>, bool, bool);

impl CoreErlangGenerator {
    /// Generates dispatch case clauses for all methods in a class definition.
    pub(in crate::core_erlang) fn generate_class_method_dispatches(
        &mut self,
        class: &ClassDefinition,
        indent_level: isize,
    ) -> Result<Document<'static>> {
        // BT-851: Pre-scan for Tier 2 block parameters before generating method bodies
        self.scan_class_for_tier2_blocks(class);

        let mut docs = Vec::new();
        for method in &class.methods {
            // Only generate dispatch for primary methods for now
            if method.kind == MethodKind::Primary {
                docs.push(self.generate_method_dispatch(method, indent_level)?);
            }
        }
        Ok(Document::Vec(docs))
    }

    /// BT-295/BT-2709: Allocates a fresh variable for each method parameter,
    /// recording both the param var name (for dispatch argument lists) and its
    /// declared type (for the arithmetic fast-path classifier). Returns the
    /// fresh var names in declaration order.
    fn collect_method_param_vars(&mut self, method: &MethodDefinition) -> Vec<String> {
        method
            .parameters
            .iter()
            .map(|p| {
                let var_name = self.fresh_var(&p.name.name);
                self.current_method_params.push(var_name.clone());
                // BT-2709: Record declared type for the arithmetic fast path.
                self.record_method_param_type(&p.name.name, p.type_annotation.as_ref());
                var_name
            })
            .collect()
    }

    /// Generates a single method dispatch case clause.
    pub(in crate::core_erlang) fn generate_method_dispatch(
        &mut self,
        method: &MethodDefinition,
        indent_level: isize,
    ) -> Result<Document<'static>> {
        // Reset state version at the start of each method
        self.reset_state_version();

        // Push a new scope for this method's parameter bindings
        self.push_scope();
        // BT-295: Clear method params (will be populated below if present)
        self.current_method_params.clear();
        // BT-2709: Reset arithmetic fast-path parameter-type tracking.
        self.clear_method_param_types();

        let selector_name = method.selector.name();
        // BT-1435: Track current method selector for Logger intrinsic metadata.
        self.current_method_selector = Some(selector_name.to_string());

        // BT-295: Collect parameter variable names (mutates scope via fresh_var)
        let param_vars: Vec<String> = self.collect_method_param_vars(method);

        // BT-851: Populate tier2_block_params for this method from pre-scanned info
        self.tier2_block_params.clear();
        // BT-2797: Reset the same-method local-var tracking. The real
        // (re-)population happens inside lower_body_exprs_with_reply via
        // prescan_tier2_local_vars — this clear here is belt-and-suspenders
        // for the case where a caller inspects the field between the clear
        // and the body being generated.
        self.tier2_local_vars.clear();
        self.tier2_local_var_captured_mutations.clear();
        let selector_name_for_t2 = selector_name.to_string();
        if let Some(positions) = self.tier2_method_info.get(&selector_name_for_t2).cloned() {
            for pos in &positions {
                if *pos < method.parameters.len() {
                    self.tier2_block_params
                        .insert(method.parameters[*pos].name.name.to_string());
                }
            }
        }

        // BT-761: Detect whether any block argument in this method body contains ^.
        // If so, set up a non-local return token so ^ inside blocks can throw to escape
        // the closure and return from the enclosing actor method.
        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&method.span, &method.body);

        let method_body_doc = match self.generate_actor_method_body_with_nlr(method, needs_nlr) {
            Ok(doc) => doc,
            Err(e) => {
                self.pop_scope();
                self.current_method_selector = None;
                return Err(e);
            }
        };

        // Build method clause as Document tree
        let has_params = !param_vars.is_empty();
        let body_doc: Document = if has_params {
            docvec![
                "<",
                leaf::atom(selector_name.to_string()),
                "> when 'true' ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "case Args of",
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "<[",
                                join(
                                    param_vars.iter().map(|p| leaf::var(p.clone())),
                                    &Document::Str(", ")
                                ),
                                "]> when 'true' ->",
                                nest(INDENT, docvec![line(), method_body_doc,]),
                                line(),
                                "<_> when 'true' -> {'reply', {'error', 'bad_arity'}, State}",
                            ]
                        ),
                        line(),
                        "end",
                    ]
                ),
                "\n",
            ]
        } else {
            docvec![
                "<",
                leaf::atom(selector_name.to_string()),
                "> when 'true' ->",
                nest(INDENT, docvec![line(), method_body_doc,]),
                "\n",
            ]
        };

        // Render at correct indent level
        let indent_spaces = indent_level * INDENT;
        #[allow(clippy::cast_sign_loss)] // indent_spaces is always non-negative
        let indent_width = indent_spaces as usize;
        let result_doc = docvec![
            leaf::whitespace(indent_width),
            nest(indent_spaces, body_doc)
        ];

        // Pop the scope when done with this method
        self.pop_scope();
        self.current_method_selector = None;

        Ok(result_doc)
    }

    /// BT-3148 (ADR 0111 Addendum 4 task 2): lowering-only counterpart of the
    /// old `generate_method_definition_body_with_reply` (deleted BT-3171 once
    /// its last caller migrated off it), returning the raw `Vec<ThreadedStmt>`
    /// instead of rendering it. Used by [`Self::generate_method_dispatch`]
    /// and, since BT-3171, the other Actor-boundary NLR call sites (sealed
    /// methods, actor extension funs), all of which need the IR itself so
    /// they can prepend a real `NlrCatch` stmt (token already minted, in
    /// production's real mint position) before the single `verify()` +
    /// `render()` pass — rather than rendering the body first and wrapping
    /// the resulting `Document` afterward.
    pub(in crate::core_erlang) fn lower_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        let body = super::super::util::collect_body_exprs(&method.body);
        self.lower_body_exprs_with_reply(&body, true)
    }

    /// BT-3148 (ADR 0111 Addendum 4 task 2): mints the NLR token (if
    /// `needs_nlr`), lowers the method body, prepends a real `NlrCatch` stmt
    /// carrying that token, and verifies + renders the whole sequence —
    /// the token is minted BEFORE lowering, matching production's real mint
    /// order (§Gap 3). Callers still own `set_current_nlr_token(None)` /
    /// scope cleanup on error via the `?` this returns through.
    fn generate_actor_method_body_with_nlr(
        &mut self,
        method: &MethodDefinition,
        needs_nlr: bool,
    ) -> Result<Document<'static>> {
        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        let span = method
            .body
            .first()
            .map_or_else(|| method.span, |s| s.expression.span());
        let lowered = self.lower_method_definition_body_with_reply(method);
        self.set_current_nlr_token(None);
        let stmts = lowered?;

        // Case-arm context (dispatch clause): always needs the letrec frame
        // when NLR is present — see `prepend_nlr_catch_and_render`'s doc.
        Ok(self.prepend_nlr_catch_and_render(stmts, nlr_token_var.as_deref(), span, true))
    }

    /// BT-3171 (ADR 0111 Addendum 4/6): shared tail step for every
    /// Actor-boundary NLR call site — prepends a real `ThreadedStmt::NlrCatch`
    /// (when `nlr_token_var` is `Some`; the token is already minted, in
    /// production's real mint position, by the caller) to an already-lowered
    /// method body, then verifies and renders the whole sequence in one pass.
    /// Replaces the old two-step "render body, then
    /// `wrap_actor_body_with_nlr_catch` the rendered `Document`" shape.
    ///
    /// `needs_letrec` mirrors `wrap_actor_body_with_nlr_catch`'s own
    /// parameter of the same name: `true` when the try/catch would otherwise
    /// nest inside a `case` arm (dispatch clauses — BEAM validator
    /// `ambiguous_catch_try_state`), `false` for standalone functions (sealed
    /// methods, extension funs) that don't need the extra function frame. No
    /// letrec is emitted when `nlr_token_var` is `None` regardless of
    /// `needs_letrec` — there is no try/catch to isolate.
    pub(in crate::core_erlang) fn prepend_nlr_catch_and_render(
        &mut self,
        mut stmts: Vec<threaded_ir::ThreadedStmt>,
        nlr_token_var: Option<&str>,
        span: Span,
        needs_letrec: bool,
    ) -> Document<'static> {
        if let Some(token_var) = nlr_token_var {
            stmts.insert(
                0,
                threaded_ir::ThreadedStmt::NlrCatch {
                    boundary: super::super::NlrBoundary::ActorReply,
                    token: threaded_ir::TokenId::new(token_var.to_string()),
                    frame: threaded_ir::FrameId::ROOT,
                    span,
                },
            );
        }
        let rendered_body = self.verify_and_render_body_stmts(&stmts, span);

        // BT-761/BT-764: If NLR was detected, wrap the rendered try/catch in a
        // letrec function — letrec creates a genuine separate function frame,
        // avoiding BEAM validator ambiguous_catch_try_state errors that arise
        // when try/catch is nested inside case arms. `render`'s `NlrCatch` arm
        // already produced the try/catch itself; the letrec is a Document-level
        // wrapper around that, unchanged from `wrap_actor_body_with_nlr_catch`'s
        // own `needs_letrec` shape.
        if needs_letrec && nlr_token_var.is_some() {
            docvec![
                "letrec '__nlr_body'/0 = fun () ->\n",
                rendered_body,
                "\n",
                "in apply '__nlr_body'/0 ()",
            ]
        } else {
            rendered_body
        }
    }

    /// BT-3171 (ADR 0111 Addendum 4/6): lowering-only counterpart of the old
    /// `generate_method_body_with_reply` (deleted BT-3171 once its last
    /// caller migrated off it), returning the raw `Vec<ThreadedStmt>` instead
    /// of rendering it — the Block-based sibling of
    /// [`Self::lower_method_definition_body_with_reply`]. Used by
    /// `generate_legacy_method_clause` (top-level `name := [block]`
    /// workspace bindings), which needs the IR itself so it can prepend a
    /// real `NlrCatch` stmt before the single verify+render pass.
    pub(in crate::core_erlang) fn lower_method_body_with_reply(
        &mut self,
        block: &Block,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        let body = super::super::util::collect_body_exprs(&block.body);
        self.lower_body_exprs_with_reply(&body, false)
    }

    // ── BT-1422: Unified method body state-threading ──────────────────

    /// BT-2797: Pre-scans a method/block body for `var := [block]` assignments
    /// where the block itself needs Tier 2 (captured-local or field mutations),
    /// and populates `self.tier2_local_vars` with the ones that are *provably
    /// safe* to promote — i.e. every later reference to `var` in this same body
    /// is the receiver of a `value`/`value:`/`value:value:`/`value:value:value:`
    /// send, never a bare read (return, argument, reassignment, ...).
    ///
    /// Only considers `var := [block]` assignments that are themselves *flat
    /// top-level statements* of `body` — one that's nested inside e.g. an
    /// `ifTrue:`/`do:` block argument isn't a candidate here. Such a nested
    /// assignment still falls through to the existing
    /// `generate_block`/`validate_stored_closure` compile-time diagnostic,
    /// which is conservative but correct.
    ///
    /// **Safety invariant**: `scan_var_uses` marks *any* reference to `var`
    /// found inside a nested `Block` literal as unsafe, even a `value:` send
    /// that would otherwise qualify as safe. A nested block literal compiles
    /// through a completely separate path
    /// (`generate_block_body_slice`/`BlockExprKind` in `expressions.rs`, not
    /// `lower_body_exprs_with_reply`/`BodyExprKind` here) that has no
    /// Tier2-tuple-unpacking logic and never resets `tier2_local_vars` for
    /// its own body — so a "safe-looking" `value:` call on a promoted var
    /// found inside a nested block either leaks an unpacked
    /// `{Result, NewState}` tuple as the inner block's return value (a Tier 1
    /// inner block, which never resets `tier2_local_vars`) or calls a
    /// 2-arity Tier 2 fun with only 1 argument (a Tier 2 inner block, which
    /// resets `tier2_local_vars` for its own body and never re-adds `var`
    /// since it isn't assigned there) — `badarity` at runtime either way.
    /// Only a direct top-level method-body `var value:` statement is
    /// provably safe.
    ///
    /// This runs as a full pre-scan (like `tier2_block_params`'s class-level
    /// scan) rather than incrementally during codegen, specifically so that a
    /// block which *escapes* this method unsafely (returned, stored elsewhere,
    /// passed as an argument) is never promoted — it must keep hitting the
    /// `generate_block`/`validate_stored_closure` compile-time diagnostic,
    /// since no known call site would thread state through it correctly.
    fn prescan_tier2_local_vars(&mut self, body: &[&Expression]) {
        for (i, expr) in body.iter().enumerate() {
            let Expression::Assignment { target, value, .. } = expr else {
                continue;
            };
            let (Expression::Identifier(id), Expression::Block(block)) =
                (target.as_ref(), value.as_ref())
            else {
                continue;
            };
            let captured_mutations = Self::captured_mutations_for_block(block);
            let needs_tier2 = !captured_mutations.is_empty()
                || (self.context == CodeGenContext::Actor
                    && !block_analysis::analyze_block(block).field_writes.is_empty());
            if !needs_tier2 {
                continue;
            }
            let var_name = id.name.as_str();
            let (has_unsafe, has_safe) = body[i + 1..]
                .iter()
                .map(|stmt| Self::scan_var_uses(stmt, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
            // Require at least one safe use, not just the absence of unsafe
            // ones — otherwise a variable that's never referenced again (e.g.
            // `var := [block]` as the method's last statement, so the block
            // value implicitly escapes as the return value) would vacuously
            // pass "no unsafe use found" and be wrongly promoted.
            if has_safe && !has_unsafe {
                self.tier2_local_vars.insert(var_name.to_string());
                // BT-2815: record which outer locals this var's block captures
                // and mutates, so a later `value(:...)` call site — which only
                // has the variable name, not the block AST — can still rebind
                // them after the call (mirroring the inline-block-literal case).
                if !captured_mutations.is_empty() {
                    self.tier2_local_var_captured_mutations
                        .insert(var_name.to_string(), captured_mutations);
                }
            }
        }
    }

    /// BT-2808: Normalizes a `Cascade` into its true underlying receiver and the
    /// full ordered list of messages sent to it.
    ///
    /// The parser (`parse_cascade`) folds the cascade's *first* message into
    /// `Cascade.receiver` as a whole `MessageSend` — e.g. for `blk value: x;
    /// value: y`, `receiver` is `MessageSend(blk, value:, [x])` and `messages`
    /// holds only the remaining `value: y`. Every safety/codegen decision needs
    /// the TRUE receiver (`blk`) and ALL messages sent to it (both `value: x`
    /// and `value: y`), so this mirrors the same normalization
    /// `generate_cascade` (expressions.rs) already performs for ordinary
    /// (non-Tier-2) cascade codegen.
    fn normalize_cascade<'a>(
        receiver: &'a Expression,
        messages: &'a [CascadeMessage],
    ) -> (&'a Expression, Vec<(&'a MessageSelector, &'a [Expression])>) {
        if let Expression::MessageSend {
            receiver: inner,
            selector: first_selector,
            arguments: first_arguments,
            ..
        } = receiver
        {
            let mut all: Vec<(&MessageSelector, &[Expression])> =
                Vec::with_capacity(messages.len() + 1);
            all.push((first_selector, first_arguments.as_slice()));
            for msg in messages {
                all.push((&msg.selector, msg.arguments.as_slice()));
            }
            (inner.as_ref(), all)
        } else {
            let all: Vec<(&MessageSelector, &[Expression])> = messages
                .iter()
                .map(|msg| (&msg.selector, msg.arguments.as_slice()))
                .collect();
            (receiver, all)
        }
    }

    /// BT-2797/BT-2808: Returns true if `selector` is a `value`/`value:`/
    /// `value:value:`/`value:value:value:` send — the "safe" family that lets a
    /// Tier 2 block value be invoked without escaping to a call site that
    /// doesn't know to thread state through it.
    fn is_safe_value_family_selector(selector: &MessageSelector) -> bool {
        matches!(
            selector.well_known(),
            Some(
                WellKnownSelector::Value
                    | WellKnownSelector::ValueColon
                    | WellKnownSelector::ValueValue
                    | WellKnownSelector::ValueValueValue
            )
        )
    }

    /// BT-2797: Scans `expr` for references to `var_name`, returning
    /// `(has_unsafe_use, has_safe_use)`.
    ///
    /// A *safe* use is the receiver of a `value`/`value:`/`value:value:`/
    /// `value:value:value:` send. Any other reference — a bare return, an
    /// argument to another call, a reassignment, ... — is *unsafe*, since it
    /// would let a Tier 2 block value escape to a call site that doesn't know
    /// to thread state through it. A variable that's *never* referenced at
    /// all yields `(false, false)`, which the caller must treat as unsafe
    /// (not "no unsafe use found") — see `prescan_tier2_local_vars`.
    ///
    /// Deliberately conservative: exhaustively matches every `Expression`
    /// variant so a use hidden inside e.g. a map literal or string
    /// interpolation is never silently missed. A shadowing block parameter
    /// with the same name is *not* special-cased — that only makes this
    /// over-conservative (a missed promotion), never unsafe.
    #[expect(
        clippy::too_many_lines,
        reason = "exhaustive match over every Expression variant, kept as one function for locality with its single caller"
    )]
    fn scan_var_uses(expr: &Expression, var_name: &str) -> (bool, bool) {
        match expr {
            Expression::Identifier(id) => (id.name == var_name, false),
            Expression::Literal(..)
            | Expression::ClassReference { .. }
            | Expression::Super(_)
            | Expression::Primitive { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Error { .. } => (false, false),
            Expression::Spread { name, .. } => (name.name == var_name, false),
            Expression::FieldAccess { receiver, .. } => Self::scan_var_uses(receiver, var_name),
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } => {
                let is_safe_value_send = matches!(
                    receiver.as_ref(),
                    Expression::Identifier(id) if id.name == var_name
                ) && Self::is_safe_value_family_selector(selector);
                let (mut unsafe_, mut safe) = if is_safe_value_send {
                    (false, true)
                } else {
                    Self::scan_var_uses(receiver, var_name)
                };
                for arg in arguments {
                    let (u, s) = Self::scan_var_uses(arg, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                (unsafe_, safe)
            }
            Expression::Block(block) => {
                // Any reference to var_name inside a nested block literal is
                // unsafe — see the safety invariant note on
                // prescan_tier2_local_vars above (a nested block compiles
                // through a completely different path with no Tier2-tuple
                // unpacking and no tier2_local_vars reset of its own).
                let (any_unsafe, any_safe) = block
                    .body
                    .iter()
                    .map(|stmt| Self::scan_var_uses(&stmt.expression, var_name))
                    .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
                (any_unsafe || any_safe, false)
            }
            Expression::Assignment { target, value, .. } => {
                let (u1, s1) = Self::scan_var_uses(target, var_name);
                let (u2, s2) = Self::scan_var_uses(value, var_name);
                (u1 || u2, s1 || s2)
            }
            Expression::DestructureAssignment { value, .. } | Expression::Return { value, .. } => {
                Self::scan_var_uses(value, var_name)
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // BT-2808: when the cascade's true underlying receiver (see
                // `normalize_cascade`) *is* var_name itself (e.g. `blk value: x;
                // value: y`), the generic recursive scan would hit the plain
                // `Identifier` arm and unconditionally report it unsafe. Mirror the
                // `MessageSend` arm's `is_safe_value_send` check instead: if EVERY
                // message sent to that receiver (including the one folded into
                // `receiver` by the parser) is itself a safe
                // `value`/`value:`/`value:value:`/`value:value:value:` send, the
                // whole cascade is as safe as a single safe value send would be.
                let (underlying_receiver, all_messages) =
                    Self::normalize_cascade(receiver, messages);
                let receiver_is_var = matches!(
                    underlying_receiver,
                    Expression::Identifier(id) if id.name == var_name
                );
                let all_messages_safe_value_sends = receiver_is_var
                    && !all_messages.is_empty()
                    && all_messages
                        .iter()
                        .all(|(sel, _)| Self::is_safe_value_family_selector(sel));
                let (mut unsafe_, mut safe) = if all_messages_safe_value_sends {
                    (false, true)
                } else {
                    Self::scan_var_uses(underlying_receiver, var_name)
                };
                for (_, args) in &all_messages {
                    for arg in *args {
                        let (u, s) = Self::scan_var_uses(arg, var_name);
                        unsafe_ |= u;
                        safe |= s;
                    }
                }
                (unsafe_, safe)
            }
            Expression::Parenthesized { expression, .. } => {
                Self::scan_var_uses(expression, var_name)
            }
            Expression::Match { value, arms, .. } => {
                let (mut unsafe_, mut safe) = Self::scan_var_uses(value, var_name);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        let (u, s) = Self::scan_var_uses(guard, var_name);
                        unsafe_ |= u;
                        safe |= s;
                    }
                    let (u, s) = Self::scan_var_uses(&arm.body, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                (unsafe_, safe)
            }
            Expression::MapLiteral { pairs, .. } => pairs
                .iter()
                .map(|pair| {
                    let (u1, s1) = Self::scan_var_uses(&pair.key, var_name);
                    let (u2, s2) = Self::scan_var_uses(&pair.value, var_name);
                    (u1 || u2, s1 || s2)
                })
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
            Expression::ListLiteral { elements, tail, .. } => {
                let (mut unsafe_, mut safe) = elements
                    .iter()
                    .map(|e| Self::scan_var_uses(e, var_name))
                    .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2));
                if let Some(t) = tail {
                    let (u, s) = Self::scan_var_uses(t, var_name);
                    unsafe_ |= u;
                    safe |= s;
                }
                (unsafe_, safe)
            }
            Expression::ArrayLiteral { elements, .. } => elements
                .iter()
                .map(|e| Self::scan_var_uses(e, var_name))
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
            Expression::StringInterpolation { segments, .. } => segments
                .iter()
                .map(|seg| match seg {
                    beamtalk_core::ast::StringSegment::Interpolation(e) => {
                        Self::scan_var_uses(e, var_name)
                    }
                    beamtalk_core::ast::StringSegment::Literal(_) => (false, false),
                })
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        }
    }

    /// Classify a body expression for state-threading dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. field assignment)
    /// must come before general ones (e.g. pure expression).
    pub(in crate::core_erlang) fn classify_body_expr(&self, expr: &Expression) -> BodyExprKind {
        // Early return — `^ value`
        if matches!(expr, Expression::Return { .. }) {
            return BodyExprKind::EarlyReturn;
        }

        // self fieldAt: name put: val — sub-classify by RHS for control flow with mutations
        if self.is_self_field_at_put(expr) {
            if let Expression::MessageSend { arguments, .. } = expr {
                if arguments.len() >= 2 && self.control_flow_has_mutations(&arguments[1]) {
                    return BodyExprKind::SelfFieldAtPutControlFlow;
                }
            }
            return BodyExprKind::SelfFieldAtPut;
        }

        // self.field := value — sub-classify by RHS for control flow with mutations
        if Self::is_field_assignment(expr) {
            if let Expression::Assignment { value, .. } = expr {
                if self.control_flow_has_mutations(value) {
                    return BodyExprKind::FieldAssignmentControlFlow;
                }
            }
            return BodyExprKind::FieldAssignment;
        }

        // var := expr — sub-classify by RHS
        if Self::is_local_var_assignment(expr) {
            if let Expression::Assignment { target, value, .. } = expr {
                // BT-2797: var := [block] where the block itself needs Tier 2 —
                // stored for invocation later in this method (`blk value: x`),
                // not invoked here. Must be classified before is_tier2_value_call
                // (which detects the opposite shape: RHS *invoking* a Tier 2 block).
                //
                // Only takes this path if `tier2_local_vars` already proved (via
                // `prescan_tier2_local_vars`, run before classification starts) that
                // every later use of this variable in the method is a safe
                // `value`/`value:`/etc. call. Otherwise the block may escape (be
                // returned, passed elsewhere, reassigned) with no call site that
                // knows to thread state through it — fall through to the plain
                // `generate_block` path, which raises the compile-time
                // `FieldAssignmentInUnsupportedBlock` diagnostic for that case.
                if let (Expression::Identifier(id), Expression::Block(_)) =
                    (target.as_ref(), value.as_ref())
                {
                    if self.tier2_local_vars.contains(id.name.as_str()) {
                        return BodyExprKind::LocalAssignTier2Block;
                    }
                }
                if self.is_tier2_value_call(value) {
                    return BodyExprKind::LocalAssignTier2;
                }
                if self.control_flow_has_mutations(value) {
                    return BodyExprKind::LocalAssignControlFlow;
                }
                // BT-1421: var := self method — self-send as assignment RHS
                if self.is_dispatching_actor_self_send(value) {
                    return BodyExprKind::LocalAssignSelfSend;
                }
            }
            return BodyExprKind::LocalAssignPure;
        }

        // {a, b} := expr — sub-classify by RHS for control flow with mutations
        if let Expression::DestructureAssignment { value, .. } = expr {
            if self.control_flow_has_mutations(value) {
                return BodyExprKind::DestructureAssignmentControlFlow;
            }
            return BodyExprKind::DestructureAssignment;
        }

        // super send
        if Self::is_super_message_send(expr) {
            return BodyExprKind::SuperSend;
        }

        // self error: "..." — never returns
        if Self::is_error_message_send(expr) {
            return BodyExprKind::ErrorSend;
        }

        // Tier 2 value: call
        if self.is_tier2_value_call(expr) {
            return BodyExprKind::Tier2ValueCall;
        }

        // Tier 2 self-send with block args
        if let Some(tier2_args) = self.detect_tier2_self_send(expr) {
            return BodyExprKind::Tier2SelfSend(tier2_args);
        }

        // Control flow with field mutations
        if self.control_flow_has_mutations(expr) {
            return BodyExprKind::ControlFlowWithMutations;
        }

        // Dispatching self-send (BT-1420)
        if self.is_dispatching_actor_self_send(expr) {
            return BodyExprKind::DispatchingSelfSend;
        }

        BodyExprKind::Pure
    }

    /// BT-3148: verifies a lowered method-body IR once (via
    /// [`threaded_ir::verify_body_with_opaque_version_gaps`] — see its doc
    /// comment for what the opaque-statement backfill does and does not
    /// check) and renders it through [`threaded_ir::render`], the same
    /// renderer every other emission-input `ThreadedIr` producer uses.
    fn verify_and_render_body_stmts(
        &mut self,
        stmts: &[threaded_ir::ThreadedStmt],
        span: Span,
    ) -> Document<'static> {
        let errors = threaded_ir::verify_body_with_opaque_version_gaps(stmts);
        self.report_threaded_ir_verify_errors(
            &errors,
            "gen_server method-body ThreadedIr must be well-formed",
            span,
        );
        let mut ctx = threaded_ir::RenderCtx::new(self);
        threaded_ir::render(stmts, &mut ctx)
    }

    /// BT-3148: the shared two-hop `Bind` chain for a
    /// `self.field := <control-flow-with-mutations>` step (both the
    /// `BodyExprKind::FieldAssignmentControlFlow` arm and its `^`-return
    /// variant) — BT-3146's investigation-confirmed idiom for a mutation
    /// whose map source is a computed temp rather than the prior `State`
    /// version:
    ///
    /// 1. `Bind { target: Gensym(CfState), source: State(n), op:
    ///    Direct(Doc(element(2, CfTuple))) }` — the RHS construct's returned
    ///    state, bound to its pre-minted `_CfState{N}` temp;
    /// 2. `Bind { target: State(n+1), source: Gensym(CfState), op: Put {
    ///    field, CfVal } }` — the real field mutation, a genuine
    ///    [`threaded_ir::BindOp::Put`] whose `maps:put` rendering is
    ///    `render_bind`'s (`shadow_write` is `false`: actor `State` writes
    ///    never carry the ADR 0110 class-var obligation, so `class_tag` is
    ///    an unused placeholder).
    ///
    /// `prefix_doc` (the `CfTuple`/`CfVal` unpack) precedes the chain as an
    /// opaque `Statement`. Mint order is the caller's responsibility and
    /// matches the pre-BT-3148 emission exactly: `CfTuple`/`CfVal`/RHS doc/
    /// `CfState` are all minted before this is called; `next_state_var`
    /// advances here, after them.
    #[expect(
        clippy::too_many_arguments,
        reason = "two call sites share one mint-order-sensitive lowering step; a params struct would obscure the order the doc comment pins"
    )]
    fn lower_cf_field_assignment_binds(
        &mut self,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
        prefix_doc: Document<'static>,
        tuple_var: &str,
        rhs_state: &str,
        field_name: &str,
        val_var: &str,
        span: Span,
    ) {
        use threaded_ir::{BindOp, FrameId, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar};

        let source_version = self.state_version();
        stmts.push(ThreadedStmt::Statement(prefix_doc, span));
        let cf_state = VersionedVar::new(
            VersionPrefix::Gensym(rhs_state.to_string()),
            1,
            FrameId::ROOT,
        );
        stmts.push(ThreadedStmt::Bind {
            target: cf_state.clone(),
            source: VersionedVar::new(VersionPrefix::State, source_version, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(tuple_var.to_string()),
                ")",
            ])),
            shadow_write: false,
            span,
        });
        let _ = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, FrameId::ROOT),
            source: cf_state,
            op: BindOp::Put {
                field: field_name.to_string(),
                value: ValueRef::Var(val_var.to_string()),
                // Unused placeholder: only rendered when shadow_write is
                // true, which only class-var Puts ever set (ADR 0110).
                class_tag: ValueRef::Literal("'nil'"),
            },
            shadow_write: false,
            span,
        });
    }

    /// Lowers a method body to one straight-line `Vec<ThreadedStmt>` (ADR
    /// 0111 Addendum 4 / BT-3148 task 1): every `State`-version step this
    /// function itself emits is a real [`threaded_ir::ThreadedStmt::Bind`]
    /// (target/source versions read off the live counter); every ordinary
    /// AST-directed statement — dispatch, sends, Tier-2 calls, destructure
    /// bindings, reply epilogues — is an opaque
    /// [`threaded_ir::ThreadedStmt::Statement`] built by the SAME codegen
    /// call production ran before this migration (byte-identity: only the
    /// container changed, from `Vec<Document>` to `Vec<ThreadedStmt>`).
    /// Version steps hidden inside shared multi-module helpers
    /// (`generate_self_dispatch_open`, `emit_super_send_open`,
    /// `generate_tier2_self_send_open`, `generate_field_assignment_open`,
    /// `generate_self_field_at_put_open`) remain inside their `Statement`s —
    /// see `verify_body_with_opaque_version_gaps`'s backfill accounting.
    ///
    /// Classification happens exactly once (`classify_body_expr`, Phase 1);
    /// the mutating control-flow arms consume that decision via
    /// `emit_actor_threaded_last_stmts`/`emit_actor_threaded_assign_rhs_stmts`,
    /// which never decline — the pre-BT-3148 `verify_routing_invariant`
    /// call sites (and `VerifyError::RoutingMismatch`) are deleted because
    /// there is no second computation left to disagree with the first.
    ///
    /// `supports_early_return` controls whether `^ value` expressions are handled.
    /// Method definitions support it; block bodies do not (NLR uses throw/catch).
    #[expect(
        clippy::too_many_lines,
        reason = "unified handler for all method body expression types with state threading"
    )]
    fn lower_body_exprs_with_reply(
        &mut self,
        body: &[&Expression],
        supports_early_return: bool,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        use threaded_ir::ThreadedStmt;

        if body.is_empty() {
            let state = self.current_state_var();
            return Ok(vec![ThreadedStmt::Statement(
                docvec!["{'reply', Self, ", leaf::var(state), "}"],
                Span::default(),
            )]);
        }

        // BT-2797: (Re-)populate tier2_local_vars for *this* body before
        // classification reads it. Cleared here (not just in
        // generate_method_dispatch) because generate_legacy_method_clause
        // (top-level `name := [block]` workspace methods) calls into this
        // function without clearing it first.
        self.tier2_local_vars.clear();
        self.tier2_local_var_captured_mutations.clear();
        self.prescan_tier2_local_vars(body);

        // Phase 1: classify every expression upfront.  Classification is
        // stateless w.r.t. codegen state (state_version, variable bindings),
        // so pre-computing is safe and separates "what" from "how".
        let plan: Vec<BodyExprKind> = body
            .iter()
            .map(|expr| {
                let kind = self.classify_body_expr(expr);
                if matches!(&kind, BodyExprKind::EarlyReturn) && !supports_early_return {
                    BodyExprKind::Pure
                } else {
                    kind
                }
            })
            .collect();

        // Phase 2: lower each (expression, kind) pair to ThreadedStmts.
        let mut stmts: Vec<ThreadedStmt> = Vec::with_capacity(body.len());
        let body_len = body.len();

        for (i, (expr, kind)) in body.iter().zip(plan.into_iter()).enumerate() {
            let is_last = i == body_len - 1;
            let is_early_return = matches!(&kind, BodyExprKind::EarlyReturn);
            let span = expr.span();

            // Early return — always terminates generation regardless of position.
            // Classify the inner value to handle super/tier2/dispatch returns.
            if is_early_return && supports_early_return {
                if let Expression::Return { value, .. } = expr {
                    let value_kind = self.classify_body_expr(value);
                    match value_kind {
                        BodyExprKind::SuperSend => {
                            let expr_str = self.expression_doc(value)?;
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let _SuperTuple = ",
                                    expr_str,
                                    " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                                    " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                                    " in {'reply', _Result, _NewState}",
                                ],
                                span,
                            ));
                        }
                        BodyExprKind::Tier2ValueCall => {
                            let expr_str = self.generate_tier2_value_call_doc(value)?;
                            let reply = self.emit_tuple_unpack_reply("T2Tuple", expr_str);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        BodyExprKind::DispatchingSelfSend => {
                            // ADR 0118 phase 1a: `^self log: (self nextId)` — the
                            // producer sequences its own arguments; its prelude
                            // is this body's real `Statement` + `Bind` pair.
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            stmts.extend(tv.prelude);
                            let reply = self.threaded_value_reply_doc(&tv.value);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                            let tier2_args = tier2_args.clone();
                            let (doc, dispatch_var) =
                                self.generate_tier2_self_send_open(value, &tier2_args)?;
                            stmts.push(ThreadedStmt::Statement(doc, span));
                            let reply = self.dispatch_reply_doc(&dispatch_var);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        BodyExprKind::ControlFlowWithMutations => {
                            let expr_str = self.expression_doc(value)?;
                            let reply = self.emit_tuple_unpack_reply("Tuple", expr_str);
                            stmts.push(ThreadedStmt::Statement(reply, span));
                        }
                        // BT-1477: ^ self.field := <control-flow-with-mutations>
                        BodyExprKind::FieldAssignmentControlFlow => {
                            if let Expression::Assignment {
                                target, value: rhs, ..
                            } = &**value
                            {
                                if let Expression::FieldAccess { field, .. } = target.as_ref() {
                                    let tuple_var = self.fresh_temp_var("CfTuple");
                                    let val_var = self.fresh_temp_var("CfVal");
                                    let rhs_str = self.expression_doc(rhs)?;
                                    let rhs_state = self.fresh_temp_var("CfState");
                                    self.lower_cf_field_assignment_binds(
                                        &mut stmts,
                                        docvec![
                                            "let ",
                                            leaf::var(tuple_var.clone()),
                                            " = ",
                                            rhs_str,
                                            " in let ",
                                            leaf::var(val_var.clone()),
                                            " = call 'erlang':'element'(1, ",
                                            leaf::var(tuple_var.clone()),
                                            ") in ",
                                        ],
                                        &tuple_var,
                                        &rhs_state,
                                        field.name.as_str(),
                                        &val_var,
                                        span,
                                    );
                                    let field_state = self.current_state_var();
                                    stmts.push(ThreadedStmt::Statement(
                                        docvec![
                                            "{'reply', ",
                                            leaf::var(val_var),
                                            ", ",
                                            leaf::var(field_state),
                                            "}",
                                        ],
                                        span,
                                    ));
                                }
                            }
                        }
                        _ => {
                            // ADR 0118 phase 1a: `^ Array with: (self bump)`,
                            // `^ (items at: i) + (self bump)` — the value's
                            // state-effecting sub-expressions land in this
                            // body's IR as real `Bind`s (in source order, via
                            // the sequencing rule), then the reply carries the
                            // post-dispatch state.
                            //
                            // The reply's state is the one AFTER the prelude,
                            // not `current_state_var()`: the value's own compile
                            // may mint versions inside its closed document (a
                            // conditional receiver's dispatch chain) that are
                            // out of scope here — `state_var_after_prelude`.
                            let version_before = self.state_version();
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            let final_state =
                                self.state_var_after_prelude(&tv.prelude, version_before);
                            stmts.extend(tv.prelude);
                            let value_str = self.threaded_value_doc(&tv.value);
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let _ReturnValue = ",
                                    value_str,
                                    " in {'reply', _ReturnValue, ",
                                    leaf::var(final_state),
                                    "}",
                                ],
                                span,
                            ));
                        }
                    }
                    return Ok(stmts);
                }
            }

            match kind {
                // Mutation hidden inside a shared multi-module helper
                // (`generate_self_field_at_put_open` — also called from
                // conditionals.rs): stays an opaque Statement; the version
                // step it performs is accounted for by
                // `verify_body_with_opaque_version_gaps`'s backfill.
                BodyExprKind::SelfFieldAtPut => {
                    let (doc, val_var) = self.generate_self_field_at_put_open(expr)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                    if is_last {
                        let final_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "{'reply', ",
                                leaf::var(val_var),
                                ", ",
                                leaf::var(final_state),
                                "}",
                            ],
                            span,
                        ));
                    }
                }
                BodyExprKind::FieldAssignment => {
                    // ADR 0118 phase 1a: `self.log := self.log ++ #(self
                    // getValue)`, `self.count := self.count + (self bump)` —
                    // the RHS's state-effecting sub-expressions land in this
                    // body's IR as real `Bind`s BEFORE `source_version` is
                    // read (the exact snapshot BT-3382's reverted prototype
                    // desynced) and before the shared open helper mints its
                    // own step; the RHS compile below substitutes the
                    // already-sequenced value.
                    let rhs_scope = match expr {
                        Expression::Assignment { value, .. } => {
                            self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?
                        }
                        _ => PrecompiledScope::new(),
                    };
                    if is_last {
                        if let Expression::Assignment { target, value, .. } = expr {
                            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                                let val_var = self.fresh_temp_var("Val");
                                let source_version = self.state_version();
                                let value_str = self.generate_field_assignment_value_doc(value)?;
                                let new_state = self.next_state_var();
                                let target_version = self.state_version();
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "let ",
                                        leaf::var(val_var.clone()),
                                        " = ",
                                        value_str,
                                        " in ",
                                    ],
                                    span,
                                ));
                                // The real field mutation, as a real Bind —
                                // `render_bind`'s `BindOp::Put` arm is the
                                // single place the `maps:put` shape lives.
                                stmts.push(ThreadedStmt::Bind {
                                    target: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        target_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    source: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        source_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    op: threaded_ir::BindOp::Put {
                                        field: field.name.to_string(),
                                        value: threaded_ir::ValueRef::Var(val_var.clone()),
                                        // Unused placeholder — see
                                        // `lower_cf_field_assignment_binds`.
                                        class_tag: threaded_ir::ValueRef::Literal("'nil'"),
                                    },
                                    shadow_write: false,
                                    span,
                                });
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "{'reply', ",
                                        leaf::var(val_var),
                                        ", ",
                                        leaf::var(new_state),
                                        "}",
                                    ],
                                    span,
                                ));
                            }
                        }
                    } else {
                        // Shared helper (`generate_field_assignment_open`,
                        // also called from conditionals/exception_handling/
                        // list_ops/intrinsics): opaque Statement, version
                        // step backfilled at verify time.
                        let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                        stmts.push(ThreadedStmt::Statement(doc, span));
                    }
                    self.finish_precompiled_scope(rhs_scope)?;
                }
                // BT-1477: self.field := expr where RHS is control flow returning {Value, State}
                BodyExprKind::FieldAssignmentControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            // ADR 0118 phase 1a: `self.f := 1 to: (self bump)
                            // do: [..]` — the construct's own state-effecting
                            // operands thread ahead of it.
                            let rhs_scope =
                                self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?;
                            // ADR 0118 phase 4 (BT-3420): when the RHS is
                            // itself an inline-threaded control-flow
                            // construct, `thread_ahead` above already
                            // spliced its real prelude into `stmts` (via
                            // `subexpr_needs_prelude`/
                            // `inline_control_flow_needs_threading`
                            // recognizing this shape) and registered its
                            // ALREADY-UNWRAPPED value for substitution —
                            // `expression_doc` below returns that value
                            // directly, not a `{Value, State}` tuple, so no
                            // further `element/1` unwrap runs. Only the
                            // field's own `maps:put` version-bump remains.
                            // See `emit_actor_threaded_last_stmts`'s matching
                            // comment. Every other `FieldAssignmentControlFlow`
                            // shape (loops, list-ops) still returns a raw
                            // tuple `Document`, unpacked by the `tuple_var`/
                            // `lower_cf_field_assignment_binds` path below.
                            let val_var = self.fresh_temp_var("CfVal");
                            if self.inline_control_flow_needs_threading(value.unwrap_parens()) {
                                let value_str = self.expression_doc(value)?;
                                self.finish_precompiled_scope(rhs_scope)?;
                                let source_version = self.state_version();
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "let ",
                                        leaf::var(val_var.clone()),
                                        " = ",
                                        value_str,
                                        " in "
                                    ],
                                    span,
                                ));
                                let _ = self.next_state_var();
                                let target_version = self.state_version();
                                stmts.push(ThreadedStmt::Bind {
                                    target: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        target_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    source: threaded_ir::VersionedVar::new(
                                        threaded_ir::VersionPrefix::State,
                                        source_version,
                                        threaded_ir::FrameId::ROOT,
                                    ),
                                    op: threaded_ir::BindOp::Put {
                                        field: field.name.to_string(),
                                        value: threaded_ir::ValueRef::Var(val_var.clone()),
                                        class_tag: threaded_ir::ValueRef::Literal("'nil'"),
                                    },
                                    shadow_write: false,
                                    span,
                                });
                            } else {
                                // Evaluate the RHS (returns {Value, State} tuple)
                                let tuple_var = self.fresh_temp_var("CfTuple");
                                let value_str = self.expression_doc(value)?;
                                self.finish_precompiled_scope(rhs_scope)?;
                                // Unpack the tuple: element(1) is the value, element(2) is the state
                                let rhs_state = self.fresh_temp_var("CfState");
                                self.lower_cf_field_assignment_binds(
                                    &mut stmts,
                                    docvec![
                                        "let ",
                                        leaf::var(tuple_var.clone()),
                                        " = ",
                                        value_str,
                                        " in let ",
                                        leaf::var(val_var.clone()),
                                        " = call 'erlang':'element'(1, ",
                                        leaf::var(tuple_var.clone()),
                                        ") in ",
                                    ],
                                    &tuple_var,
                                    &rhs_state,
                                    field.name.as_str(),
                                    &val_var,
                                    span,
                                );
                            }
                            let field_state = self.current_state_var();
                            // Extract threaded locals from the control flow state
                            // (e.g. ifTrue: [y := 1. y + 1] threads y via __local__ keys)
                            let mut doc_parts: Vec<Document<'static>> = Vec::new();
                            if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value)
                            {
                                for var in &threaded_vars {
                                    let tv_core = self.lookup_var(var).map_or_else(
                                        || Self::to_core_erlang_var(var),
                                        String::clone,
                                    );
                                    doc_parts.push(docvec![
                                        "let ",
                                        leaf::var(tv_core),
                                        " = call 'maps':'get'(",
                                        leaf::atom(Self::local_state_key(var)),
                                        ", ",
                                        leaf::var(field_state.clone()),
                                        ") in ",
                                    ]);
                                }
                            }
                            if !doc_parts.is_empty() {
                                stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                            }
                            if is_last {
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "{'reply', ",
                                        leaf::var(val_var),
                                        ", ",
                                        leaf::var(field_state),
                                        "}",
                                    ],
                                    span,
                                ));
                            }
                        }
                    }
                }
                // BT-1479: self fieldAt: name put: expr where RHS is control flow returning {Value, State}
                //
                // The mutation is a real Bind: the map key is a dynamic
                // (computed) value, not a static field name, so it cannot
                // use `BindOp::Put` (whose `field` is a literal atom) —
                // BT-3146's investigation-established `Direct(ValueRef::Doc(...))`
                // idiom carries the whole `maps:put` expression opaquely
                // instead (ADR 0111 Addendum 4's type-level rule: a
                // version-mutating statement must be a `Bind`, never a
                // `Statement`, but `Bind`'s `op` may still be opaque).
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_temp_var("Name");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let val_var = self.fresh_temp_var("CfVal");
                        let val_code = self.expression_doc(&arguments[1])?;
                        let rhs_state = self.fresh_temp_var("CfState");
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                val_code,
                                " in let ",
                                leaf::var(val_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in let ",
                                leaf::var(rhs_state.clone()),
                                " = call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                target_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            source: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                source_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(docvec![
                                "call 'maps':'put'(",
                                leaf::var(name_var),
                                ", ",
                                leaf::var(val_var.clone()),
                                ", ",
                                leaf::var(rhs_state),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        let field_state = self.current_state_var();
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) =
                            self.get_control_flow_threaded_vars(&arguments[1])
                        {
                            for var in &threaded_vars {
                                let tv_core = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(tv_core),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(field_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                        if is_last {
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "{'reply', ",
                                    leaf::var(val_var),
                                    ", ",
                                    leaf::var(field_state),
                                    "}",
                                ],
                                span,
                            ));
                        }
                    }
                }
                // BT-1479: {a, b} := expr where RHS is control flow returning {Value, State}.
                // Element 2 of the RHS tuple becomes the next real `State` Bind
                // (Direct — the RHS is a computed map, not a static field Put);
                // the tuple unpack and threaded-local rebinds stay opaque Statements.
                BodyExprKind::DestructureAssignmentControlFlow => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // Evaluate RHS (returns {Value, State} tuple)
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let actual_val = self.fresh_temp_var("CfVal");
                        let value_str = self.expression_doc(value)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                value_str,
                                " in let ",
                                leaf::var(actual_val.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                target_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            source: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                source_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        let new_state = self.current_state_var();
                        // Extract threaded locals
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value) {
                            for var in &threaded_vars {
                                let tv_core = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(tv_core),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                        // Now destructure the unpacked value
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{'reply', 'nil', ", leaf::var(post_state), "}"],
                            span,
                        ));
                    }
                }
                // Real state Bind: the RHS's element 2 IS the next `State`
                // version (a computed map, not a static field — Direct, not Put).
                BodyExprKind::LocalAssignTier2 => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let tuple_var = self.fresh_temp_var("T2Tuple");
                            let value_str = self.generate_tier2_value_call_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            let source_version = self.state_version();
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(tuple_var.clone()),
                                    " = ",
                                    value_str,
                                    " in let ",
                                    leaf::var(core_var),
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var.clone()),
                                    ")\n in ",
                                ],
                                span,
                            ));
                            let _ = self.next_state_var();
                            let target_version = self.state_version();
                            stmts.push(ThreadedStmt::Bind {
                                target: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    target_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                source: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    source_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(
                                    docvec![
                                        "call 'erlang':'element'(2, ",
                                        leaf::var(tuple_var),
                                        ")",
                                    ],
                                )),
                                shadow_write: false,
                                span,
                            });
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::LocalAssignControlFlow => {
                    // BT-2378/BT-3148: route the actor `var := <control-flow-with-mutations>`
                    // assign-RHS through the shared `ThreadedExpr` emitter. The Actor boundary
                    // binds the target to element 1, advances the state version to element 2
                    // (the threaded gen_server `State`, via a real Bind) and rebinds
                    // `__local__`-threaded sibling outer-locals.
                    //
                    // ADR 0111 Addendum 4: `classify_body_expr` already decided this is
                    // `LocalAssignControlFlow` (Phase 1) — `emit_actor_threaded_assign_rhs_stmts`
                    // never declines, so there is no second computation left to disagree
                    // with, and `verify_routing_invariant`/`RoutingMismatch` are deleted.
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            // ADR 0118 phase 1a: `x := 1 to: (self bump) do:
                            // [..]` — the construct's own state-effecting
                            // operands thread ahead of it.
                            let rhs_scope =
                                self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?;
                            self.emit_actor_threaded_assign_rhs_stmts(&id.name, value, &mut stmts)?;
                            self.finish_precompiled_scope(rhs_scope)?;
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                // BT-1421: var := self method — dispatch RHS, bind result to var
                BodyExprKind::LocalAssignSelfSend => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            // ADR 0118 phase 1a: `v := self log: (self nextId)`
                            // — the producer's `Statement` + real `Bind`
                            // (its arguments sequenced first), then the
                            // local bound to its pure result reference.
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            stmts.extend(tv.prelude);
                            let value_str = self.threaded_value_doc(&tv.value);
                            self.bind_var(var_name, &core_var);
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                span,
                            ));
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                // BT-2797: var := [block needing Tier 2], where prescan_tier2_local_vars
                // already proved every later use of `var` in this body is a safe
                // `value`/`value:`/etc. call. Generate the block via
                // generate_block_stateful directly (bypassing generate_block's
                // "unsupported block" rejection, which is only needed when the
                // compiler can't prove the later invocation site is safe).
                BodyExprKind::LocalAssignTier2Block => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let (Expression::Identifier(id), Expression::Block(block)) =
                            (target.as_ref(), value.as_ref())
                        {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let captured_mutations = Self::captured_mutations_for_block(block);
                            let value_str =
                                self.generate_block_stateful(block, &captured_mutations)?;
                            self.bind_var(var_name, &core_var);
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                span,
                            ));
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::LocalAssignPure => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            // ADR 0118 phase 1a: `ok := (self recordOnce: x)
                            // and: [y]`, `total := items size + (self bump)` —
                            // the RHS's state-effecting sub-expressions land
                            // in this body's IR as real `Bind`s, in source
                            // order, via the sequencing rule.
                            let tv = self.threaded_expression(value, threaded_ir::FrameId::ROOT)?;
                            stmts.extend(tv.prelude);
                            let value_str = self.threaded_value_doc(&tv.value);
                            self.bind_var(var_name, &core_var);
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(core_var), " = ", value_str, " in "],
                                span,
                            ));
                        }
                    }
                    if is_last {
                        let reply = self.pure_reply_doc();
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // ADR 0118 phase 1b: `{a, b} := #(1, self bump)` —
                        // the RHS's state-effecting sub-expressions land in
                        // this body's IR as real `Bind`s before
                        // `generate_destructure_bindings`'s own
                        // `eval_rhs_to_temp_var` call (`expressions.rs`)
                        // compiles `value` via `expression_doc`, which
                        // substitutes the already-sequenced value.
                        let rhs_scope =
                            self.thread_ahead(value, &mut stmts, threaded_ir::FrameId::ROOT)?;
                        let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                        self.finish_precompiled_scope(rhs_scope)?;
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{'reply', 'nil', ", leaf::var(post_state), "}"],
                            span,
                        ));
                    }
                }
                BodyExprKind::SuperSend => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let _SuperTuple = ",
                                expr_str,
                                " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                                " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                                " in {'reply', _Result, _NewState}",
                            ],
                            span,
                        ));
                    } else {
                        let mut open_docs: Vec<Document<'static>> = Vec::new();
                        self.emit_super_send_open(expr, &mut open_docs)?;
                        stmts.push(ThreadedStmt::Statement(Document::Vec(open_docs), span));
                    }
                }
                BodyExprKind::ErrorSend => {
                    if is_last {
                        // Error send never returns — no reply tuple needed.
                        let expr_str = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(docvec![expr_str], span));
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        let expr_str = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in "],
                            span,
                        ));
                    }
                }
                BodyExprKind::Tier2ValueCall => {
                    if is_last {
                        let expr_str = self.generate_tier2_value_call_doc(expr)?;
                        let reply = self.emit_tuple_unpack_reply("T2Tuple", expr_str);
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    } else {
                        // Real state Bind: element 2 of the Tier-2 tuple IS the
                        // next `State` version (a computed map — Direct, not Put).
                        let tuple_var = self.fresh_temp_var("T2Tuple");
                        let discard_var = self.fresh_temp_var("T2Discard");
                        let expr_str = self.generate_tier2_value_call_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                expr_str,
                                " in let ",
                                leaf::var(discard_var),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ")\n in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                target_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            source: threaded_ir::VersionedVar::new(
                                threaded_ir::VersionPrefix::State,
                                source_version,
                                threaded_ir::FrameId::ROOT,
                            ),
                            op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        let new_state = self.current_state_var();

                        // BT-1213: Extract captured local mutations from NewState
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(mutations) = self.get_inline_block_captured_mutations(expr) {
                            for var in &mutations {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(core_var),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                    }
                }
                BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                    let (doc, dispatch_var) =
                        self.generate_tier2_self_send_open(expr, tier2_args)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                    if is_last {
                        let reply = self.dispatch_reply_doc(&dispatch_var);
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::ControlFlowWithMutations => {
                    if is_last {
                        // BT-2378/BT-3148: route the last-position actor control-flow
                        // construct through the shared `ThreadedExpr` emitter. The Actor
                        // boundary binds element 1 (Reply) and element 2 (the threaded
                        // gen_server `State`, via a real Bind) and returns
                        // `{'reply', Reply, NewState}`.
                        //
                        // ADR 0111 Addendum 4: `classify_body_expr` already decided this
                        // is `ControlFlowWithMutations` (Phase 1) —
                        // `emit_actor_threaded_last_stmts` never declines, so
                        // `verify_routing_invariant`/`RoutingMismatch` are deleted (no
                        // second computation left to disagree with).
                        //
                        // ADR 0118 phase 1a: `1 to: (self bump) do: [..]`,
                        // `(self bump) timesRepeat: [..]` — the construct's own
                        // state-effecting operands (bounds, receiver) thread
                        // ahead of it, so it starts from their post-dispatch
                        // state.
                        let scope =
                            self.thread_ahead(expr, &mut stmts, threaded_ir::FrameId::ROOT)?;
                        self.emit_actor_threaded_last_stmts(expr, &mut stmts)?;
                        self.finish_precompiled_scope(scope)?;
                    } else {
                        // Real state Bind: element 2 of the construct's tuple IS the
                        // next `State` version (a computed map — Direct, not Put).
                        let scope =
                            self.thread_ahead(expr, &mut stmts, threaded_ir::FrameId::ROOT)?;
                        // ADR 0118 phase 4 (BT-3420): when `expr` is itself an
                        // inline-threaded control-flow construct, the
                        // `thread_ahead` call just above (now that
                        // `subexpr_needs_prelude` recognizes this shape) already
                        // spliced its real prelude into `stmts` and registered
                        // its already-unwrapped value — see
                        // `emit_actor_threaded_last_stmts`'s matching comment.
                        // `expression_doc` below then returns that value
                        // directly, so no further `element/2` unwrap runs.
                        // Every other `ControlFlowWithMutations` shape (loops,
                        // list-ops) still returns a raw tuple `Document` here,
                        // unpacked by the manual `Tuple`/`Bind` pair below.
                        if self.inline_control_flow_needs_threading(expr.unwrap_parens()) {
                            let expr_str = self.expression_doc(expr)?;
                            self.finish_precompiled_scope(scope)?;
                            let seq_var = self.fresh_temp_var("seq");
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["let ", leaf::var(seq_var), " = ", expr_str, " in "],
                                span,
                            ));
                        } else {
                            let tuple_var = self.fresh_temp_var("Tuple");
                            let expr_str = self.expression_doc(expr)?;
                            self.finish_precompiled_scope(scope)?;
                            let source_version = self.state_version();
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(tuple_var.clone()),
                                    " = ",
                                    expr_str,
                                    " in "
                                ],
                                span,
                            ));
                            let _ = self.next_state_var();
                            let target_version = self.state_version();
                            stmts.push(ThreadedStmt::Bind {
                                target: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    target_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                source: threaded_ir::VersionedVar::new(
                                    threaded_ir::VersionPrefix::State,
                                    source_version,
                                    threaded_ir::FrameId::ROOT,
                                ),
                                op: threaded_ir::BindOp::Direct(threaded_ir::ValueRef::Doc(
                                    docvec![
                                        "call 'erlang':'element'(2, ",
                                        leaf::var(tuple_var),
                                        ")",
                                    ],
                                )),
                                shadow_write: false,
                                span,
                            });
                        }
                        let new_state = self.current_state_var();

                        // Extract threaded locals from the updated state
                        let mut doc_parts: Vec<Document<'static>> = Vec::new();
                        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(expr) {
                            for var in &threaded_vars {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                doc_parts.push(docvec![
                                    "let ",
                                    leaf::var(core_var),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state.clone()),
                                    ") in ",
                                ]);
                            }
                        }
                        if !doc_parts.is_empty() {
                            stmts.push(ThreadedStmt::Statement(Document::Vec(doc_parts), span));
                        }
                    }
                }
                BodyExprKind::DispatchingSelfSend => {
                    // ADR 0118 phase 1a: `self log: (self nextId)` — the
                    // producer (`generate_self_dispatch`) sequences its own
                    // arguments and yields the `Statement` + real `Bind` pair
                    // this body splices; the reply reads its pure result.
                    let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                    stmts.extend(tv.prelude);
                    if is_last {
                        let reply = self.threaded_value_reply_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(reply, span));
                    }
                }
                BodyExprKind::EarlyReturn => {
                    return Err(CodeGenError::Internal(
                        "EarlyReturn should be handled before match dispatch".to_string(),
                    ));
                }
                // BT-3396: the top-level counterpart of `conditionals.rs`'s
                // C12 catch-all — `Array with: (self bump)`, `(self
                // recordOnce: x) and: [y]`, `"{self next}"` as a method-body
                // statement of its own. Every order-safe nested self-send
                // is threaded as a real `Bind` ahead of the compile, so the
                // `post_state` read after it (and the next statement) see
                // the dispatch's `NewState`. No-op when there is nothing to
                // hoist.
                //
                // ADR 0118 phase 1a: `threaded_expression` replaces the planner
                // here — the statement's state-effecting sub-expressions land
                // in this body's IR as real `Bind`s in source order (the
                // sequencing rule temp-binds whatever precedes them), and the
                // `post_state` read after them (and the next statement) see
                // the dispatch's `NewState`. A pure statement costs one
                // `generate_expression` call, as before.
                BodyExprKind::Pure => {
                    if is_last {
                        let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                        stmts.extend(tv.prelude);
                        let expr_str = self.threaded_value_doc(&tv.value);
                        let post_state = self.current_state_var();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let _Result = ",
                                expr_str,
                                " in {'reply', _Result, ",
                                leaf::var(post_state),
                                "}",
                            ],
                            span,
                        ));
                    } else {
                        // Mint order: `seq` before the expression, as before.
                        let tmp_var = self.fresh_temp_var("seq");
                        let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                        stmts.extend(tv.prelude);
                        let expr_str = self.threaded_value_doc(&tv.value);
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in "],
                            span,
                        ));
                    }
                }
            }
        }

        Ok(stmts)
    }

    /// Emit a generic `{'reply', _Result, State}` close for the last expression
    /// when the expression itself has already been emitted as an open let chain.
    /// Used by local assignments and other open-chain handlers in last position.
    fn pure_reply_doc(&mut self) -> Document<'static> {
        let post_state = self.current_state_var();
        docvec!["{'reply', 'nil', ", leaf::var(post_state), "}"]
    }

    /// Emit the last-position reply for a dispatch open call (Tier 2 self-send
    /// or dispatching self-send).  Uses `current_state_var` and the
    /// explicitly-passed `dispatch_var`.
    fn dispatch_reply_doc(&mut self, dispatch_var: &str) -> Document<'static> {
        let final_state = self.current_state_var();
        docvec![
            "{'reply', call 'erlang':'element'(1, ",
            leaf::var(dispatch_var.to_string()),
            "), ",
            leaf::var(final_state),
            "}",
        ]
    }

    /// ADR 0118 phase 1a: the last-position reply for a spliced
    /// [`threaded_ir::ThreadedValue`] — `{'reply', <value>, StateN}` where
    /// `StateN` is the state after the value's prelude. Byte-identical to
    /// [`Self::dispatch_reply_doc`] for a self-send (whose value IS
    /// `element(1, _SD)`), which stays for the Tier 2 self-send arm.
    fn threaded_value_reply_doc(&mut self, value: &threaded_ir::ValueRef) -> Document<'static> {
        let value_doc = self.threaded_value_doc(value);
        let final_state = self.current_state_var();
        docvec!["{'reply', ", value_doc, ", ", leaf::var(final_state), "}"]
    }

    /// Emit the last-position reply for an expression that returns a
    /// `{Result, State}` tuple (Tier 2 value calls, control flow with
    /// mutations, early returns with mutations).
    fn emit_tuple_unpack_reply(
        &mut self,
        tuple_label: &str,
        expr_doc: Document<'static>,
    ) -> Document<'static> {
        let tuple_var = self.fresh_temp_var(tuple_label);
        docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            expr_doc,
            " in let _Result = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in let _NewState = call 'erlang':'element'(2, ",
            leaf::var(tuple_var),
            ") in {'reply', _Result, _NewState}",
        ]
    }

    /// Emit a super message send in non-last position, threading state.
    fn emit_super_send_open(
        &mut self,
        expr: &Expression,
        docs: &mut Vec<Document<'static>>,
    ) -> Result<()> {
        let super_result_var = self.fresh_temp_var("SuperReply");
        let current_state = self.current_state_var();
        let new_state = self.next_state_var();
        let class_name = self.class_name();

        if let Expression::MessageSend {
            selector,
            arguments,
            ..
        } = expr
        {
            let selector_atom = selector.name().to_string();
            let mut arg_docs: Vec<Document<'static>> = Vec::with_capacity(arguments.len());
            for (j, arg) in arguments.iter().enumerate() {
                if j > 0 {
                    arg_docs.push(Document::Str(", "));
                }
                arg_docs.push(self.expression_doc(arg)?);
            }
            docs.push(docvec![
                "let ",
                leaf::var(super_result_var.clone()),
                " = call 'beamtalk_dispatch':'super'(",
                leaf::atom(selector_atom),
                ", [",
                Document::Vec(arg_docs),
                "], Self, ",
                leaf::var(current_state),
                ", ",
                leaf::atom(class_name),
                ")",
            ]);
        }

        docs.push(docvec![
            " in let ",
            leaf::var(new_state),
            " = call 'erlang':'element'(3, ",
            leaf::var(super_result_var),
            ") in ",
        ]);
        Ok(())
    }

    /// BT-877: Detect the `new => self error: "..."` pattern that indicates a class
    /// is not constructible via `new`. Returns `true` if any method named `new` (unary)
    /// has a single-expression body that is `self error: <StringLiteral>`.
    fn has_raising_new(class: &ClassDefinition) -> bool {
        class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .filter(|m| m.selector == MessageSelector::Unary("new".into()))
            .any(|m| Self::is_self_error_body(&m.body))
    }

    /// BT-2998: whether the class declares a unary `new` of its own, on either
    /// side, and so keeps control of `new/0` (`Random`, `Queue`, `Announcer`).
    ///
    /// Mirrors the `has_explicit_new` / `has_explicit_class_new` test in
    /// `generate_value_type_module`, which is what decides whether the
    /// auto-generated — and now possibly raising — `new/0` is emitted at all.
    ///
    /// One case it deliberately does not mirror: a declared `new` whose body is
    /// `@intrinsic basicNew` routes back to the auto-generated constructor, so
    /// on a `native:` class it would raise despite being "declared". Only
    /// `value.bt`/`object.bt` write that body and neither is `native:`; if one
    /// ever were, the only cost is an omitted `isConstructible` key, which the
    /// runtime recomputes lazily from `new/0` anyway.
    fn declares_own_new(class: &ClassDefinition) -> bool {
        class
            .methods
            .iter()
            .chain(class.class_methods.iter())
            .filter(|m| m.kind == MethodKind::Primary)
            .any(|m| m.selector == MessageSelector::Unary("new".into()))
    }

    /// Check if a method body is a single `self error: <StringLiteral>` expression.
    fn is_self_error_body(body: &[beamtalk_core::ast::ExpressionStatement]) -> bool {
        if body.len() != 1 {
            return false;
        }
        // BT-2073: classify `error:` via the well-known enum so a future rename
        // forces this site to update too.
        matches!(
            &body[0].expression,
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                ..
            } if matches!(receiver.as_ref(), Expression::Identifier(id) if id.name == "self")
                && matches!(selector.well_known(), Some(WellKnownSelector::Error))
                && arguments.len() == 1
                && matches!(&arguments[0], Expression::Literal(Literal::String(_), _))
        )
    }

    /// Generates the `register_class/0` on-load function using the `ClassBuilder`
    /// protocol (ADR 0038 Phase 3 / BT-837).
    ///
    /// This function is called automatically via `-on_load` when the module loads.
    /// Instead of calling `beamtalk_object_class:start/2` directly, it builds a
    /// `ClassBuilder` state map and calls `beamtalk_class_builder:register/1`.
    /// This routes all compiled class registration through the `ClassBuilder`
    /// protocol, which handles both first registration and hot reload.
    ///
    /// If `beamtalk_class_builder:register/1` raises, the exception is re-raised
    /// via `primop 'raw_raise'` so the BEAM `-on_load` mechanism reports a visible
    /// load failure rather than silently succeeding with an unregistered class (BT-998).
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'register_class'/0 = fun () ->
    ///     try
    ///         let _BuilderState0 = ~{
    ///             'className' => 'Counter',
    ///             'superclassRef' => 'Actor',
    ///             'moduleName' => 'class_definition',
    ///             'methodSource' => ~{...}~,
    ///             'classMethodSource' => ~{...}~,
    ///             'methodSignatures' => ~{...}~,
    ///             'classMethodSignatures' => ~{}~,
    ///             'classState' => ~{}~,
    ///             'classDoc' => 'none',
    ///             'methodDocs' => ~{}~,
    ///             'classMethodDocs' => ~{}~,
    ///             'meta' => ~{...}~
    ///         }~
    ///         in let _Reg0 = case call 'beamtalk_class_builder':'register'(_BuilderState0) of
    ///             <{'ok', _Pid0}> when 'true' -> 'ok'
    ///             <{'error', _Err0}> when 'true' -> {'error', _Err0}
    ///         end
    ///         in _Reg0
    ///     of RegResult -> RegResult
    ///     catch <CatchType, CatchError, CatchStack> ->
    ///         primop 'raw_raise'(CatchType, CatchError, CatchStack)
    /// ```
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_register_class(
        &mut self,
        module: &Module,
        synthesize_supervision_spec: bool,
    ) -> Result<Document<'static>> {
        // BT-1610: Skip only if there are no class definitions AND no protocols
        // AND no foreign extension methods. Protocol-only files still need
        // register_class/0 for protocol registration; BT-2250: pure-extension
        // files (only `Target >> sel` with no host class) need it to register
        // their foreign extensions at load.
        if module.classes.is_empty()
            && module.protocols.is_empty()
            && !Self::has_foreign_extensions(module)
        {
            return Ok(Document::Nil);
        }

        // BT-1610 / BT-2250: Class-less module — generate register_class/0 with
        // only protocol registration and/or foreign extension registration
        // calls, no class builder chain.
        if module.classes.is_empty() {
            let ext_reg_doc = self.generate_foreign_extension_registrations(module)?;
            let protocol_reg_doc = self.generate_protocol_registrations(module);
            return Ok(docvec![
                "'register_class'/0 = fun () ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "try",
                        nest(
                            INDENT,
                            docvec![ext_reg_doc, protocol_reg_doc, line(), "'ok'", "\n",]
                        ),
                    ]
                ),
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "of _ProtoRegResult -> _ProtoRegResult",
                        line(),
                        "catch <CatchType, CatchError, CatchStack> -> primop \
                         'raw_raise'(CatchType, CatchError, CatchStack)",
                    ]
                ),
                "\n\n",
            ]);
        }

        let mut class_docs = Vec::new();

        for (i, class) in module.classes.iter().enumerate() {
            // Instance methods — used for methodSource, methodSignatures, and methodDocs
            let instance_methods: Vec<_> = class
                .methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .collect();

            // BT-101: Method source
            let method_source_doc = Self::build_selector_map(&instance_methods, |m| {
                let source_str = self.extract_method_source(class.name.name.as_str(), false, m);
                leaf::binary_lit(&source_str)
            });

            // BT-988: Method display signatures for :help command
            let method_sigs_doc = Self::build_selector_map(&instance_methods, |m| {
                let sig_str = unparse_method_display_signature(m);
                leaf::binary_lit(&sig_str)
            });

            // BT-990: Class-side method display signatures for :help command
            let class_methods_primary: Vec<_> = class
                .class_methods
                .iter()
                .filter(|m| m.kind == MethodKind::Primary)
                .collect();
            let class_method_sigs_doc = Self::build_selector_map(&class_methods_primary, |m| {
                let sig_str = unparse_method_display_signature(m);
                leaf::binary_lit(&sig_str)
            });

            // BT-2195: Class-side method source — mirrors method_source for the
            // instance side. Required by SystemNavigation `sendersOf:` /
            // `referencesTo:` / `methodsMatching:` to scan class-side bodies.
            let class_method_source_doc = Self::build_selector_map(&class_methods_primary, |m| {
                let source_str = self.extract_method_source(class.name.name.as_str(), true, m);
                leaf::binary_lit(&source_str)
            });

            // ADR 0087 Phase 2 (BT-2298): Per-method cross-reference index baked
            // into register_class/0. Forwarded to beamtalk_xref synchronously at
            // class-load time by beamtalk_object_class:init/1.
            let method_xref_doc =
                self.build_method_xref_list(class, &instance_methods, &class_methods_primary);

            // BT-412: Class variable initial values
            let class_vars_doc = self.build_class_var_map(&class.class_variables)?;

            // BT-771: Class-level doc comment
            let class_doc_value: Document<'static> = if let Some(ref doc) = class.doc_comment {
                leaf::binary_lit(doc)
            } else {
                Document::Str("'none'")
            };

            // BT-771: Method-level doc comments
            let method_docs_doc = Self::build_selector_map_filtered(&instance_methods, |m| {
                m.doc_comment.as_ref().map(|doc| leaf::binary_lit(doc))
            });

            // BT-1634: Class method doc comments
            let class_method_docs_doc =
                Self::build_selector_map_filtered(&class_methods_primary, |m| {
                    m.doc_comment.as_ref().map(|doc| leaf::binary_lit(doc))
                });

            // BT-2734: Value-type auto-accessors (slot getters, `with*:` copy-
            // setters, keyword constructor) are emitted by value_type_codegen with
            // no AST `MethodDefinition`, so the selector maps above have no entry
            // for them and their runtime `__doc__` / `__signature__` would be nil.
            // Inject compiler-derived doc + signature entries so every reflective
            // surface (System Browser read-only pane, `Beamtalk help:`, MCP docs)
            // shows them uniformly — reusing the BT-2714 resolver, no new read path.
            // A no-op for non-`Value` classes and value classes with no auto-
            // accessors (returns empty entry lists).
            let synth = Self::build_synthetic_value_accessor_metadata(class);
            let method_sigs_doc = Self::extend_selector_map_doc(
                method_sigs_doc,
                instance_methods.is_empty(),
                synth.instance_sigs,
            );
            let method_docs_doc = Self::extend_selector_map_doc(
                method_docs_doc,
                !instance_methods.iter().any(|m| m.doc_comment.is_some()),
                synth.instance_docs,
            );
            let class_method_sigs_doc = Self::extend_selector_map_doc(
                class_method_sigs_doc,
                class_methods_primary.is_empty(),
                synth.class_sigs,
            );
            let class_method_docs_doc = Self::extend_selector_map_doc(
                class_method_docs_doc,
                !class_methods_primary
                    .iter()
                    .any(|m| m.doc_comment.is_some()),
                synth.class_docs,
            );

            // BT-877: Detect non-constructible classes at compile time.
            // Emit `isConstructible = false` for: abstract classes, actors, and
            // classes with `new => self error: "..."`. For all others, omit the key
            // so the runtime can fall back to lazy computation — this is needed
            // because primitive classes (String, Integer, etc.) have raising new/0
            // in Erlang, not in Beamtalk AST.
            //
            // BT-2998: a `native:` class with no declared fields and no `new` of
            // its own now compiles a raising `new/0` too (see
            // `has_opaque_native_representation`). The runtime would reach the
            // same answer lazily by calling that `new/0` and catching, but
            // stating it up front keeps the registered metadata honest.
            let is_non_constructible = class.is_abstract
                || self.context == CodeGenContext::Actor
                || Self::has_raising_new(class)
                || (has_opaque_native_representation(class) && !Self::declares_own_new(class));

            // ADR 0050 Phase 5: BuilderState carries only module/source/signature/doc metadata.
            // Static fields (flags, fields, method signatures) are read from __beamtalk_meta/0
            // by beamtalk_object_class:init/1.
            // ADR 0070 Phase 4: Extract package name from module name
            let package_name = extract_package_from_module_name(&self.module_name);
            let meta_doc = Self::build_meta_map_doc(
                class,
                module,
                true,
                synthesize_supervision_spec,
                package_name.as_deref(),
                self.meta_provenance(),
            );
            let class_doc = Self::build_builder_state_doc(
                i,
                &class.name.name,
                class.superclass_name(),
                &self.module_name,
                method_source_doc,
                class_method_source_doc,
                method_sigs_doc,
                class_method_sigs_doc,
                method_xref_doc,
                class_vars_doc,
                class_doc_value,
                method_docs_doc,
                class_method_docs_doc,
                meta_doc,
                is_non_constructible,
                self.stdlib_mode(),
            );
            class_docs.push(class_doc);
        }

        // BT-738 / BT-749: Build a short-circuit chain so that the first
        // {error, ...} from register/1 propagates out of on_load, regardless
        // of which class position caused it.
        let class_chain = Self::build_short_circuit_chain(&class_docs);

        // BT-2250: Register foreign cross-class extension methods at load.
        // The `let _ExtN = ... in` fragments are prepended to the class
        // registration chain so extensions register before the chain's trailing
        // class-registration result is produced (extension registration always
        // succeeds — it just inserts into ETS — so it does not short-circuit).
        let ext_reg_doc = self.generate_foreign_extension_registrations(module)?;
        let try_body = docvec![ext_reg_doc, class_chain];

        // ADR 0068 Phase 2c: Generate protocol registration calls.
        // Protocol definitions in the module are registered with the runtime
        // protocol registry during on_load, after class registration succeeds.
        // The protocol registration is wrapped in a let/in chain that feeds
        // the class registration result through.
        let protocol_reg_doc = self.generate_protocol_registrations(module);

        let doc = if module.protocols.is_empty() {
            docvec![
                "'register_class'/0 = fun () ->",
                nest(
                    INDENT,
                    docvec![line(), "try", nest(INDENT, docvec![try_body, "\n",]),]
                ),
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "of _ClassRegResult -> _ClassRegResult",
                        line(),
                        "catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)",
                    ]
                ),
                "\n\n",
            ]
        } else {
            // ADR 0068 Phase 2c: After class registration succeeds, register
            // protocol definitions before returning the result.
            docvec![
                "'register_class'/0 = fun () ->",
                nest(
                    INDENT,
                    docvec![line(), "try", nest(INDENT, docvec![try_body, "\n",]),]
                ),
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "of _ClassRegResult ->",
                        nest(
                            INDENT,
                            docvec![protocol_reg_doc, line(), "_ClassRegResult",]
                        ),
                        line(),
                        "catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)",
                    ]
                ),
                "\n\n",
            ]
        };

        Ok(doc)
    }

    /// Builds a Core Erlang map document from methods, mapping each method's
    /// selector to a value produced by `value_fn`.
    ///
    /// Generates comma-separated `'selector' => value` entries suitable for
    /// embedding inside `~{ ... }~`.
    fn build_selector_map(
        methods: &[&MethodDefinition],
        mut value_fn: impl FnMut(&MethodDefinition) -> Document<'static>,
    ) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::with_capacity(methods.len());
        for (idx, method) in methods.iter().enumerate() {
            if idx > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(docvec![
                leaf::atom(method.selector.name()),
                " => ",
                value_fn(method),
            ]);
        }
        Document::Vec(parts)
    }

    /// Like [`Self::build_selector_map`], but only includes methods for which
    /// `value_fn` returns `Some(doc)`. Used for optional metadata like doc
    /// comments where not every method has an entry.
    fn build_selector_map_filtered(
        methods: &[&MethodDefinition],
        mut value_fn: impl FnMut(&MethodDefinition) -> Option<Document<'static>>,
    ) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for method in methods {
            if let Some(val) = value_fn(method) {
                if !parts.is_empty() {
                    parts.push(Document::Str(", "));
                }
                parts.push(docvec![leaf::atom(method.selector.name()), " => ", val,]);
            }
        }
        Document::Vec(parts)
    }

    /// BT-2734: Appends pre-built `'selector' => value` entries to an existing
    /// selector-map body document, inserting `, ` separators so the combined
    /// interior remains a valid comma-separated `~{ ... }~` map body.
    ///
    /// `base_is_empty` tells the caller's own builder result — not a re-derived
    /// count — whether `base` renders any entries, so the first appended entry
    /// knows whether it needs a leading separator. Returns `base` unchanged when
    /// there are no extras.
    fn extend_selector_map_doc(
        base: Document<'static>,
        base_is_empty: bool,
        extra: Vec<Document<'static>>,
    ) -> Document<'static> {
        if extra.is_empty() {
            return base;
        }
        let mut parts: Vec<Document<'static>> = vec![base];
        for (i, entry) in extra.into_iter().enumerate() {
            if !base_is_empty || i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(entry);
        }
        Document::Vec(parts)
    }

    /// BT-2734: Builds a single `'selector' => <binary>` selector-map entry for a
    /// compiler-derived signature or doc string. The value is a human-readable
    /// data string (not a Core Erlang structural fragment), so it is wrapped once
    /// in a `binary_lit` typed leaf — mirroring how the AST-driven maps embed
    /// `unparse_method_display_signature` / `doc_comment` strings.
    fn synthetic_selector_map_entry(selector: &str, value: &str) -> Document<'static> {
        docvec![
            leaf::atom(selector.to_string()),
            " => ",
            leaf::binary_lit(value),
        ]
    }

    /// BT-2734: Builds the four Core Erlang selector-map entry lists for a value
    /// class's auto-generated accessors, ready to inject into the
    /// `methodSignatures` / `methodDocs` (instance) and
    /// `classMethodSignatures` / `classMethodDocs` (class-side) maps.
    ///
    /// Value-type slot getters, `with*:` copy-setters, and the keyword constructor
    /// are emitted by `value_type_codegen` with no AST `MethodDefinition`, so they
    /// never reach those maps and their runtime `__doc__` / `__signature__` would
    /// be `nil`. Wrapping [`Self::synthetic_value_accessor_entries`], this renders
    /// each `(selector, signature, doc)` triple into `'selector' => <binary>`
    /// entries so the synthetics carry the same self-describing metadata every
    /// reflective surface reads (reusing the BT-2714 resolver — no new read path).
    fn build_synthetic_value_accessor_metadata(
        class: &ClassDefinition,
    ) -> SyntheticAccessorMetadata {
        let raw = Self::synthetic_value_accessor_entries(class);
        let mut md = SyntheticAccessorMetadata::default();
        for (selector, sig, doc) in &raw.instance {
            md.instance_sigs
                .push(Self::synthetic_selector_map_entry(selector, sig));
            md.instance_docs
                .push(Self::synthetic_selector_map_entry(selector, doc));
        }
        for (selector, sig, doc) in &raw.class {
            md.class_sigs
                .push(Self::synthetic_selector_map_entry(selector, sig));
            md.class_docs
                .push(Self::synthetic_selector_map_entry(selector, doc));
        }
        md
    }

    /// BT-2734: Computes the readable `(selector, signature, doc)` triples for a
    /// value class's compiler-generated accessors — the pure, unit-testable core
    /// of [`Self::build_synthetic_value_accessor_metadata`].
    ///
    /// The auto-accessor set and slot types come from the same sources
    /// [`Self::build_synthetic_accessor_xref_entries`] uses:
    /// [`compute_auto_slot_methods`] (which slots the user has *not* overridden)
    /// and each slot's `StateDeclaration` type annotation. `instance` holds the
    /// getters and `with*:` setters; `class` holds the keyword constructor.
    /// Returns all-empty for non-`Value` classes and for value classes with no
    /// auto-generated accessors.
    fn synthetic_value_accessor_entries(class: &ClassDefinition) -> SyntheticAccessorEntries {
        use super::super::value_type_codegen::{AutoSlotMethods, compute_auto_slot_methods};

        let mut entries = SyntheticAccessorEntries::default();
        let Some(auto) = compute_auto_slot_methods(class) else {
            return entries;
        };
        let class_name = class.name.name.as_str();

        // Getters: `field -> <SlotType>`. The return type is the slot's declared
        // type (falling back to `Object` for an untyped slot).
        for field in &auto.getters {
            let Some(slot) = class.state.iter().find(|s| s.name.name.as_str() == field) else {
                continue;
            };
            let slot_type = Self::synthetic_slot_type_display(slot);
            entries.instance.push((
                field.clone(),
                format!("{field} -> {slot_type}"),
                format!("Compiler-derived accessor. Returns the value of slot `{field}`."),
            ));
        }

        // Setters: `withField: aValue -> <ClassName>` (returns a copy).
        for field in &auto.setters {
            if !class.state.iter().any(|s| s.name.name.as_str() == field) {
                continue;
            }
            let with_sel = AutoSlotMethods::with_star_selector(field);
            entries.instance.push((
                with_sel.clone(),
                format!("{with_sel} aValue -> {class_name}"),
                format!(
                    "Compiler-derived copy-setter. Returns a copy with slot `{field}` replaced."
                ),
            ));
        }

        // Keyword constructor (class-side): `slot0: slot0 slot1: slot1 -> <ClassName>`.
        // The selector's keyword parts are the slot names in declaration order, so
        // the same names serve as the display parameter names.
        if let Some(kw_sel) = auto.keyword_constructor {
            let sig_parts: Vec<String> = class
                .state
                .iter()
                .map(|s| {
                    let n = s.name.name.as_str();
                    format!("{n}: {n}")
                })
                .collect();
            // BT-1408: the map *key* must be the same atom the runtime dispatch and
            // `__beamtalk_meta/0` entry use (`safe_class_method_selector` — hashed
            // once "class_" + selector would exceed Erlang's 255-char atom limit),
            // so a many-field Value class's keyword constructor doesn't blow the
            // atom limit here even though it already gets hashed for dispatch.
            // The signature/doc *text* keeps the full readable selector — it is a
            // binary literal, not an atom, so it carries no length limit.
            let safe_kw_sel = super::super::selector_mangler::safe_class_method_selector(&kw_sel);
            entries.class.push((
                safe_kw_sel,
                format!("{} -> {class_name}", sig_parts.join(" ")),
                format!(
                    "Compiler-derived keyword constructor. Returns a new {class_name} from the given slot values."
                ),
            ));
        }

        entries
    }

    /// BT-2734: Display form of a slot's declared type for a synthetic accessor
    /// signature, falling back to `Object` when the slot carries no annotation.
    fn synthetic_slot_type_display(slot: &StateDeclaration) -> String {
        slot.type_annotation
            .as_ref()
            .map_or_else(|| "Object".to_string(), unparse_type_annotation_display)
    }

    /// ADR 0087 Phase 2 (BT-2298): Builds the `method_xref` list document baked
    /// into `register_class/0`'s `ClassInfo` (via `BuilderState.methodXref`).
    ///
    /// One entry per primary method (instance- and class-side). Each entry
    /// records the method's defining line, the selectors it sends (with
    /// receiver kind), and the classes it references — the per-method rows
    /// `beamtalk_xref:register_class/2` fans out into the senders / references /
    /// methods ETS tables at class-load time.
    ///
    /// The send / reference data comes from the existing AST walkers
    /// ([`beamtalk_core::method_source_walker::find_all_sends_in_source`] and
    /// [`beamtalk_core::method_source_walker::find_all_references_in_source`]).
    /// Those operate on a plain `unparse_method(method)` of the method — *not*
    /// [`Self::extract_method_source`], which (BT-3249) strips any
    /// writeback-inferred `-> Type` annotation for the human-facing browsable
    /// source. xref/`referencesTo:` deliberately keeps such annotations (an
    /// inferred return type is still a real type reference), so this walk's
    /// source can differ in *content* (an extra `-> Type` token) from what
    /// `SystemNavigation`'s miss-policy fallback shows — but never in *line
    /// count* (the annotation is inline on the signature line), so baked line
    /// numbers stay method-relative and consistent with the fallback. No port
    /// round-trip; one in-process walk per method.
    ///
    /// Hand-written rows carry `source_status => indexed` and *omit* the
    /// optional `synthetic_origin` key (never emitted as a `null` sentinel).
    ///
    /// ADR 0087 Phase 6 (BT-2304): compiler-generated auto-accessors for
    /// `Value subclass:` classes (the `field/1` getters and `withField:/2`
    /// setters emitted by `value_type_codegen.rs`) have no user source text but
    /// are fully known to the compiler. They ride this same write path: their
    /// rows carry `source_status => synthetic` and a derived `synthetic_origin`
    /// line pointing at the generating slot declaration (or the class header).
    /// Included by default so `implementorsOf: #value` on an auto-accessor is
    /// non-empty — a documented parity exception, not a regression.
    pub(in crate::core_erlang::gen_server) fn build_method_xref_list(
        &self,
        class: &ClassDefinition,
        instance_methods: &[&MethodDefinition],
        class_methods: &[&MethodDefinition],
    ) -> Document<'static> {
        let mut entries: Vec<Document<'static>> = Vec::new();
        for method in instance_methods {
            entries.push(self.build_method_xref_entry(method, false));
        }
        for method in class_methods {
            entries.push(self.build_method_xref_entry(method, true));
        }
        // ADR 0087 Phase 6 (BT-2304): synthetic auto-accessor rows.
        entries.extend(self.build_synthetic_accessor_xref_entries(class));
        // BT-3073: actor class-side `new`/`new:`/`spawn`/`spawnWith:` no longer
        // get synthetic per-subclass rows here — BT-3071/BT-3072 lifted their
        // bodies into real, source-backed class methods on `Actor` itself
        // (`stdlib/src/actor.bt`), so a subclass genuinely *inherits* them
        // rather than *defining* them. `Actor`'s own compilation indexes them
        // through the normal `build_method_xref_entry` path above (real
        // `MethodDefinition`s, `source_status => indexed`); subclasses simply
        // have no row for them, which is the honest Smalltalk answer — see
        // BT-2614 (introduced the now-removed rows) and BT-3073 (retired them).
        docvec!["[", join(entries, &Document::Str(", ")), "]"]
    }

    /// ADR 0087 Phase 6 (BT-2304): Builds `method_xref` rows for the
    /// compiler-generated auto-accessors of a `Value subclass:` class.
    ///
    /// For each auto-generated slot getter (`field/1`) and `with*:` setter
    /// (`withField:/2`) — i.e. those the user did *not* hand-define — one row is
    /// emitted with:
    /// - `source_status => synthetic` (the parity-exception marker),
    /// - `synthetic_origin => N`, the 1-based source line of the generating
    ///   `field:` / `state:` slot declaration (falling back to the class header
    ///   line when the slot span cannot be resolved),
    /// - `line => N` mirroring the origin so LSP / System Browser navigation has
    ///   a target,
    /// - an empty `sends` list — accessors delegate to runtime map primitives
    ///   (`maps:get` / `maps:put`), not Beamtalk sends — and
    /// - a `references` list carrying the slot's declared type (e.g. a slot
    ///   `state: count :: Integer` yields a reference to `Integer` on both its
    ///   getter and its `withCount:` setter).
    ///
    /// Returns an empty vector for non-`Value` classes (only value types get
    /// auto-accessors) and for classes with no auto-generated accessors.
    fn build_synthetic_accessor_xref_entries(
        &self,
        class: &ClassDefinition,
    ) -> Vec<Document<'static>> {
        use super::super::value_type_codegen::{AutoSlotMethods, compute_auto_slot_methods};

        let Some(auto) = compute_auto_slot_methods(class) else {
            return Vec::new();
        };

        // Map field name -> its slot declaration so each accessor can derive its
        // origin line and type references from the generating declaration.
        let mut entries: Vec<Document<'static>> = Vec::new();

        for field in &auto.getters {
            if let Some(slot) = class.state.iter().find(|s| s.name.name.as_str() == field) {
                entries.push(self.build_synthetic_accessor_entry(field, slot, class));
            }
        }
        for field in &auto.setters {
            if let Some(slot) = class.state.iter().find(|s| s.name.name.as_str() == field) {
                let with_sel = AutoSlotMethods::with_star_selector(field);
                entries.push(self.build_synthetic_accessor_entry(&with_sel, slot, class));
            }
        }

        entries
    }

    /// Builds a single synthetic auto-accessor `method_xref` row
    /// (ADR 0087 Phase 6, BT-2304).
    ///
    /// `selector` is the accessor selector (`field` or `withField:`), `slot` the
    /// generating slot declaration that supplies the derived origin line and the
    /// referenced type.
    fn build_synthetic_accessor_entry(
        &self,
        selector: &str,
        slot: &StateDeclaration,
        class: &ClassDefinition,
    ) -> Document<'static> {
        // Derived location: the 1-based line of the generating slot declaration,
        // falling back to the class-header line when the slot span cannot be
        // resolved to a source line.
        let origin_line = self
            .span_to_line(slot.span)
            .or_else(|| self.span_to_line(class.span))
            .unwrap_or(1);

        // References: the slot's declared type names (e.g. `Integer`). Accessors
        // have no Beamtalk sends, but their type signature mentions the slot type
        // exactly like a hand-written `field :: Integer` accessor would.
        let mut ref_class_names: Vec<String> = Vec::new();
        if let Some(ref ann) = slot.type_annotation {
            collect_type_annotation_class_names(ann, &mut ref_class_names);
        }
        let refs_doc = {
            let ref_docs: Vec<Document<'static>> = ref_class_names
                .iter()
                .filter(|name| name.len() <= MAX_ATOM_BYTES)
                .map(|name| {
                    docvec![
                        "~{'class' => ",
                        leaf::atom(name.clone()),
                        ", 'line' => ",
                        leaf::int_lit(i64::from(origin_line)),
                        "}~",
                    ]
                })
                .collect();
            docvec!["[", join(ref_docs, &Document::Str(", ")), "]"]
        };

        docvec![
            "~{'class_side' => 'false', 'selector' => ",
            leaf::atom(selector.to_string()),
            ", 'line' => ",
            leaf::int_lit(i64::from(origin_line)),
            ", 'sends' => [], 'references' => ",
            refs_doc,
            ", 'source_status' => 'synthetic', 'synthetic_origin' => ",
            leaf::int_lit(i64::from(origin_line)),
            "}~",
        ]
    }

    /// Builds one `method_xref` entry map for a single method (ADR 0087 Phase 2).
    fn build_method_xref_entry(
        &self,
        method: &MethodDefinition,
        class_side: bool,
    ) -> Document<'static> {
        use beamtalk_core::method_source_walker::{
            ReceiverKind, collect_receiver_spans, find_all_references_in_source,
            find_all_sends_in_source,
        };

        // Erlang atoms cap at 255 bytes. A selector / class name longer than
        // that (e.g. a 20-keyword auto-constructor selector) can never exist as
        // a runtime dispatch atom, so a send / reference to it would never match
        // an xref query. Drop such entries rather than emitting an illegal atom
        // that fails `core_scan` at BEAM-compile time.

        // Unlike `extract_method_source` (used for the *browsable* `methodSource`/
        // `classMethodSource` maps, BT-3249), this xref walk deliberately keeps any
        // writeback-inferred `-> Type` annotation: `find_all_references_in_source`
        // explicitly walks `method.return_type` to record type references for
        // `referencesTo:`/xref queries, and an inferred-but-unannotated return type
        // is still a real reference the method's compiled behavior carries — only
        // the human-facing source text should hide it, not the xref data derived
        // from the full (annotated) AST.
        let source = beamtalk_core::unparse::unparse_method(method);

        // The method definition's line within its own (bare) source is line 1:
        // `unparse_method` emits the signature first (after any doc comment /
        // @expect lines the unparser prepends). The xref `line` field is the
        // method-relative definition line, so the first send/ref lines are
        // already in the same coordinate space.
        let def_line = Self::method_def_line(&source);

        let sends = find_all_sends_in_source(&source);

        // BT-3217 (ADR 0115 Phase 2): a second, span-carrying walk over the
        // *original* `method` (file-absolute spans, unlike `sends` above,
        // which comes from a re-unparsed/re-parsed synthetic copy — see the
        // ADR 0115 Phase 1 spike, docs/internal/adr-0115-phase1-spike-findings.md
        // §1c). Joined to `sends` **by pre-order ordinal**, before the
        // `MAX_ATOM_BYTES` filter below (a filter afterward would skew the
        // pairing) — the two walks are required to stay structurally
        // identical, verified by the corpus conformance test in
        // `source_analysis::method_span_corpus_tests`, not merely asserted
        // by this comment.
        let receiver_spans = collect_receiver_spans(method);
        // Defensive fallback for a divergence shape the corpus test doesn't
        // cover (see the comment above): a length mismatch means the
        // pre-order-ordinal pairing can't be trusted for *any* entry in this
        // method, so degrade the whole method to `dynamic` rather than risk
        // silently attributing a `recv_type` to the wrong selector.
        let spans_aligned = sends.len() == receiver_spans.len();
        let recv_types: Vec<RecvType> = sends
            .iter()
            .enumerate()
            .map(|(i, _hit)| {
                if !spans_aligned {
                    return RecvType::Dynamic;
                }
                receiver_spans
                    .get(i)
                    .and_then(|span_hit| self.type_map.get(span_hit.span))
                    .map_or(RecvType::Dynamic, project_recv_type)
            })
            .collect();

        let sends_doc = {
            let send_docs: Vec<Document<'static>> = sends
                .iter()
                .zip(recv_types.iter())
                .filter(|(hit, _)| hit.selector.len() <= MAX_ATOM_BYTES)
                .map(|(hit, recv_type)| {
                    let recv_kind = match hit.receiver {
                        ReceiverKind::SelfReceiver => "self_recv",
                        ReceiverKind::SuperReceiver => "super_recv",
                        ReceiverKind::ErlangFfi => "erlang_ffi",
                        ReceiverKind::Other => "other",
                    };
                    docvec![
                        "~{'selector' => ",
                        leaf::atom(hit.selector.clone()),
                        ", 'line' => ",
                        leaf::int_lit(i64::from(hit.line)),
                        ", 'recv_kind' => ",
                        leaf::atom(recv_kind),
                        ", 'recv_type' => ",
                        recv_type_atom(recv_type),
                        "}~",
                    ]
                })
                .collect();
            docvec!["[", join(send_docs, &Document::Str(", ")), "]"]
        };

        let references = find_all_references_in_source(&source);
        let refs_doc = {
            let ref_docs: Vec<Document<'static>> = references
                .iter()
                .filter(|hit| hit.class.len() <= MAX_ATOM_BYTES)
                .map(|hit| {
                    docvec![
                        "~{'class' => ",
                        leaf::atom(hit.class.clone()),
                        ", 'line' => ",
                        leaf::int_lit(i64::from(hit.line)),
                        "}~",
                    ]
                })
                .collect();
            docvec!["[", join(ref_docs, &Document::Str(", ")), "]"]
        };

        docvec![
            "~{'class_side' => ",
            if class_side { "'true'" } else { "'false'" },
            ", 'selector' => ",
            leaf::atom(method.selector.name().to_string()),
            ", 'line' => ",
            leaf::int_lit(i64::from(def_line)),
            ", 'sends' => ",
            sends_doc,
            ", 'references' => ",
            refs_doc,
            ", 'source_status' => 'indexed'}~",
        ]
    }

    /// Determine the method-relative definition line for an unparsed bare-method
    /// source: the first non-blank line that is not a leading doc comment
    /// (`///`), block/line comment, or `@expect`/`@`-directive line the unparser
    /// may prepend before the signature. Returns 1 if none is found.
    ///
    /// Multi-line block comments are tracked across lines so a continuation line
    /// (e.g. `   still inside the comment */`) is not mistaken for the signature.
    /// In practice the unparser emits `///`/`//` doc and line comments rather than
    /// `/* */` blocks before a signature, so this is defensive (per BT-2298 review).
    fn method_def_line(source: &str) -> u32 {
        let mut in_block_comment = false;
        for (idx, raw) in source.lines().enumerate() {
            let trimmed = raw.trim_start();
            if in_block_comment {
                if trimmed.contains("*/") {
                    in_block_comment = false;
                }
                continue;
            }
            if trimmed.starts_with("/*") {
                // A single-line `/* ... */` is fully consumed here; an unterminated
                // opener enters block-comment mode for subsequent lines.
                if !trimmed.contains("*/") {
                    in_block_comment = true;
                }
                continue;
            }
            if trimmed.is_empty()
                || trimmed.starts_with("///")
                || trimmed.starts_with("//")
                || trimmed.starts_with('@')
            {
                continue;
            }
            #[allow(clippy::cast_possible_truncation)]
            return (idx as u32) + 1;
        }
        1
    }

    /// Builds a Core Erlang map document for class variable initial values.
    ///
    /// Each variable maps `'name' => expression`, defaulting to `'nil'` when
    /// no default value is declared. Returns `Result` because evaluating
    /// default-value expressions is fallible.
    fn build_class_var_map(
        &mut self,
        class_variables: &[StateDeclaration],
    ) -> Result<Document<'static>> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for (idx, cv) in class_variables.iter().enumerate() {
            if idx > 0 {
                parts.push(Document::Str(", "));
            }
            let val = if let Some(ref default_value) = cv.default_value {
                self.expression_doc(default_value)?
            } else {
                Document::Str("'nil'")
            };
            parts.push(docvec![leaf::atom(cv.name.name.to_string()), " => ", val,]);
        }
        Ok(Document::Vec(parts))
    }

    /// Builds the `_BuilderState` map and register call block for a single class
    /// at position `idx` in the module.
    ///
    /// Generates the `let _BuilderStateN = ~{ ... }~ in let _RegN = case ... end`
    /// fragment that is later composed into the short-circuit chain by
    /// [`Self::build_short_circuit_chain`].
    ///
    /// # Parameters
    ///
    /// * `idx` — zero-based position of this class in the module (drives variable suffixes).
    /// * `class_name`, `superclass_name`, `module_name` — string identifiers for the class.
    /// * `method_source_doc` … `meta_doc` — pre-built map / value documents for each field.
    /// * `is_non_constructible` — emits `'isConstructible' => 'false'` when true.
    /// * `stdlib_mode` — emits `'stdlibMode' => 'true'` for stdlib compilations (BT-791).
    #[allow(clippy::too_many_arguments)]
    fn build_builder_state_doc(
        idx: usize,
        class_name: &str,
        superclass_name: &str,
        module_name: &str,
        method_source_doc: Document<'static>,
        class_method_source_doc: Document<'static>,
        method_sigs_doc: Document<'static>,
        class_method_sigs_doc: Document<'static>,
        method_xref_doc: Document<'static>,
        class_vars_doc: Document<'static>,
        class_doc_value: Document<'static>,
        method_docs_doc: Document<'static>,
        class_method_docs_doc: Document<'static>,
        meta_doc: Document<'static>,
        is_non_constructible: bool,
        stdlib_mode: bool,
    ) -> Document<'static> {
        docvec![
            line(),
            "let _BuilderState",
            idx,
            " = ~{",
            nest(
                INDENT,
                docvec![
                    line(),
                    docvec!["'className' => ", leaf::atom(class_name.to_string()), ","],
                    line(),
                    docvec![
                        "'superclassRef' => ",
                        leaf::atom(superclass_name.to_string()),
                        ","
                    ],
                    line(),
                    docvec!["'moduleName' => ", leaf::atom(module_name.to_string()), ","],
                    line(),
                    "'methodSource' => ~{",
                    method_source_doc,
                    "}~,",
                    line(),
                    "'classMethodSource' => ~{",
                    class_method_source_doc,
                    "}~,",
                    line(),
                    "'methodSignatures' => ~{",
                    method_sigs_doc,
                    "}~,",
                    line(),
                    "'classMethodSignatures' => ~{",
                    class_method_sigs_doc,
                    "}~,",
                    line(),
                    // ADR 0087 Phase 2 (BT-2298): per-method xref index. A list of
                    // maps, not a `~{ }~` map, so it is wrapped only by build_method_xref_list.
                    "'methodXref' => ",
                    method_xref_doc,
                    ",",
                    line(),
                    "'classState' => ~{",
                    class_vars_doc,
                    "}~,",
                    line(),
                    "'classDoc' => ",
                    class_doc_value,
                    ",",
                    line(),
                    "'methodDocs' => ~{",
                    method_docs_doc,
                    "}~,",
                    line(),
                    "'classMethodDocs' => ~{",
                    class_method_docs_doc,
                    "}~,",
                    // ADR 0050 Phase 5: Include meta map in BuilderState so that
                    // beamtalk_object_class:init/1 can access it during on_load.
                    // erlang:function_exported/3 returns false during on_load execution,
                    // making Module:'__beamtalk_meta'() unavailable at registration time.
                    line(),
                    "'meta' => ",
                    // include_standalone: true — standalone methods included in BuilderState.meta
                    // so that init/1 can register their return types during on_load.
                    meta_doc,
                    if is_non_constructible {
                        docvec![",", line(), "'isConstructible' => 'false'"]
                    } else {
                        Document::Nil
                    },
                    // BT-791: Emit stdlibMode flag for stdlib compilations so the
                    // runtime can bypass the sealed-superclass check in register/1.
                    // Character (extends sealed Integer) needs this to load correctly.
                    if stdlib_mode {
                        docvec![",", line(), "'stdlibMode' => 'true'"]
                    } else {
                        Document::Nil
                    },
                ]
            ),
            line(),
            "}~",
            line(),
            "in let _Reg",
            idx,
            " = case call 'beamtalk_class_builder':'register'(_BuilderState",
            idx,
            ") of",
            nest(
                INDENT,
                docvec![
                    line(),
                    "<{'ok', _Pid",
                    idx,
                    "}> when 'true' -> 'ok'",
                    line(),
                    "<{'error', _Err",
                    idx,
                    "}> when 'true' -> {'error', _Err",
                    idx,
                    "}",
                ]
            ),
            line(),
            "end",
        ]
    }

    /// Builds a short-circuit chain from per-class builder state blocks.
    ///
    /// For N classes, generates a nested let/case expression so that the first
    /// `{error, ...}` from `register/1` propagates out of `on_load` without
    /// processing remaining classes (BT-738 / BT-749).
    ///
    /// ```text
    ///   let _BuilderState0 = ... in let _Reg0 = case ... end
    ///   in case _Reg0 of
    ///     <{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}
    ///     <_> when 'true' ->
    ///       let _BuilderState1 = ... in _Reg1
    ///   end
    /// ```
    /// Generates Core Erlang calls to register protocol definitions with the
    /// runtime protocol registry (ADR 0068 Phase 2c).
    ///
    /// For each `ProtocolDefinition` in the module, emits a call to
    /// `beamtalk_protocol_registry:register_protocol/1` with a map containing
    /// the protocol's name, required methods, type parameters, extending clause,
    /// and the defining BEAM module.
    ///
    /// BT-2615: the `module` key records the module the protocol was defined in
    /// (e.g. `bt@stdlib@printable`) so the runtime — and the System Browser —
    /// can resolve a protocol class object's origin/source badge. The protocol
    /// class object itself is dispatched by the shared `beamtalk_protocol_object`
    /// module, which carries no package or source, so without this the browser
    /// cannot tell a stdlib protocol from a project one.
    ///
    /// BT-2957: each method requirement map also carries `param_types` (a list,
    /// one entry per parameter) and `return_type`, using the same Core Erlang
    /// abstract type representation `-spec`s use elsewhere
    /// (`spec_codegen::type_annotation_to_spec`) — including `user_type`
    /// references for cross-module alias-typed signatures. Protocol methods
    /// have no standalone function to attach a real `-spec`/Dialyzer contract
    /// to (they're pure metadata consumed by
    /// `beamtalk_protocol_registry:register_protocol/1`), so this is the
    /// closest equivalent: type-precise data for any consumer that wants it,
    /// rather than silently dropping alias-typed signatures to untyped
    /// `any()`. `self.alias_registry` must already have accumulated every
    /// alias this walk references into the module's `referenced_aliases` set
    /// — done by the pass in `actor_codegen.rs::generate_module` that runs
    /// before `generate_alias_type_attrs`, so the named `-type` this
    /// `user_type` reference points at is actually declared in the module
    /// header.
    ///
    /// Returns `Document::Nil` if the module has no protocol definitions.
    #[allow(clippy::too_many_lines)]
    fn generate_protocol_registrations(&self, module: &Module) -> Document<'static> {
        if module.protocols.is_empty() {
            return Document::Nil;
        }

        let mut parts: Vec<Document<'static>> = Vec::new();

        for protocol in &module.protocols {
            let name = protocol.name.name.to_string();

            // Helper: build a Core Erlang list of method requirement maps.
            // `referenced: None` on every `type_annotation_to_spec` call
            // below is intentional, not an oversight: the pre-pass in
            // `actor_codegen.rs::generate_module` already walked these same
            // signatures with `Some(&referenced_aliases)` before the module
            // header (and its named `-type` declarations) was assembled —
            // by the time this closure runs, there is nothing left to
            // record. Re-deriving the same Documents here (rather than
            // reusing the pre-pass's, which are discarded) is redundant
            // work, bounded by this protocol's own parameter/return-type
            // count, not worth threading a stashed value through for.
            let build_method_list = |sigs: &[ProtocolMethodSignature]| -> Document<'static> {
                let items: Vec<Document<'static>> = sigs
                    .iter()
                    .map(|sig| {
                        let selector = sig.selector.name().to_string();
                        let arity = sig.selector.arity();
                        let param_types_doc: Document<'static> = if sig.parameters.is_empty() {
                            Document::Str("[]")
                        } else {
                            let mut pt_parts: Vec<Document<'static>> = vec![Document::Str("[")];
                            for (i, param) in sig.parameters.iter().enumerate() {
                                if i > 0 {
                                    pt_parts.push(Document::Str(", "));
                                }
                                pt_parts.push(param.type_annotation.as_ref().map_or(
                                    Document::Str("{'type', 0, 'any', []}"),
                                    |ann| {
                                        spec_codegen::type_annotation_to_spec(
                                            ann,
                                            Some(&self.alias_registry),
                                            None,
                                        )
                                    },
                                ));
                            }
                            pt_parts.push(Document::Str("]"));
                            Document::Vec(pt_parts)
                        };
                        let return_type_doc: Document<'static> = sig.return_type.as_ref().map_or(
                            Document::Str("{'type', 0, 'any', []}"),
                            |ann| {
                                spec_codegen::type_annotation_to_spec(
                                    ann,
                                    Some(&self.alias_registry),
                                    None,
                                )
                            },
                        );
                        docvec![
                            "~{'selector' => ",
                            leaf::atom(selector),
                            ", 'arity' => ",
                            leaf::int_lit(i64::try_from(arity).unwrap_or(0)),
                            ", 'param_types' => ",
                            param_types_doc,
                            ", 'return_type' => ",
                            return_type_doc,
                            "}~"
                        ]
                    })
                    .collect();

                if items.is_empty() {
                    Document::Str("[]")
                } else {
                    let mut list_parts: Vec<Document<'static>> = Vec::new();
                    list_parts.push(Document::Str("["));
                    for (i, m) in items.into_iter().enumerate() {
                        if i > 0 {
                            list_parts.push(Document::Str(", "));
                        }
                        list_parts.push(m);
                    }
                    list_parts.push(Document::Str("]"));
                    Document::Vec(list_parts)
                }
            };

            // Build the required_methods and required_class_methods lists
            let methods_doc = build_method_list(&protocol.method_signatures);
            let class_methods_doc = build_method_list(&protocol.class_method_signatures);

            // Build type_params list
            let type_params: Vec<String> = protocol
                .type_params
                .iter()
                .map(|tp| tp.name.name.to_string())
                .collect();
            let type_params_doc = Self::meta_atom_list(&type_params);

            // Build extending value
            let extending_doc: Document<'static> = if let Some(ref ext) = protocol.extending {
                leaf::atom(ext.name.to_string())
            } else {
                Document::Str("'undefined'")
            };

            // Build doc value — propagate doc comments to runtime for protocol class objects
            let doc_doc: Document<'static> = if let Some(ref doc) = protocol.doc_comment {
                leaf::binary_lit(doc)
            } else {
                Document::Str("'none'")
            };

            parts.push(docvec![
                "\nlet <_ProtoReg_",
                leaf::var(name.clone()),
                "> = call 'beamtalk_protocol_registry':'register_protocol'(",
                "~{'name' => ",
                leaf::atom(name),
                ", 'module' => ",
                leaf::atom(self.module_name.to_string()),
                ", 'required_methods' => ",
                methods_doc,
                ", 'required_class_methods' => ",
                class_methods_doc,
                ", 'type_params' => ",
                type_params_doc,
                ", 'extending' => ",
                extending_doc,
                ", 'doc' => ",
                doc_doc,
                "}~) in",
            ]);
        }

        Document::Vec(parts)
    }

    fn build_short_circuit_chain(class_docs: &[Document<'static>]) -> Document<'static> {
        let last_i = class_docs.len() - 1;
        // Innermost: last class doc + final result variable
        let mut chain: Document<'static> =
            docvec![class_docs[last_i].clone(), "\n", line(), "in _Reg", last_i,];
        // Wrap from second-to-last down to first, adding short-circuit cases
        for i in (0..last_i).rev() {
            chain = docvec![
                class_docs[i].clone(),
                "\n",
                line(),
                "in case _Reg",
                i,
                " of",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "<{'error', _RegErr",
                        i,
                        "}> when 'true' -> {'error', _RegErr",
                        i,
                        "}",
                        line(),
                        "<_> when 'true' ->",
                        nest(INDENT, docvec![line(), chain]),
                    ]
                ),
                line(),
                "end",
            ];
        }
        chain
    }

    /// Generates standalone function bodies for class-side methods.
    ///
    /// Class methods are module-level functions with a `class_` prefix.
    /// They take `ClassSelf` as the first parameter (the class object),
    /// followed by any user-defined parameters.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'class_defaultValue'/1 = fun (ClassSelf) ->
    ///     42
    ///
    /// 'class_create'/1 = fun (ClassSelf) ->
    ///     let _Result = call 'beamtalk_object_class':'class_send'(
    ///         call 'erlang':'element'(4, ClassSelf), 'new:', [~{}~])
    ///     in _Result
    /// ```
    #[allow(clippy::too_many_lines)] // Error-path cleanup adds necessary lines
    pub(in crate::core_erlang) fn generate_class_method_functions(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        // BT-412: Populate class variable names for field access validation
        *self.class_var_names_mut() = class
            .class_variables
            .iter()
            .map(|cv| cv.name.name.to_string())
            .collect();

        // BT-412: Populate class method selectors for self-send routing
        *self.class_method_selectors_mut() = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| m.selector.name().to_string())
            .collect();

        // BT-3151: Populate the class-var-mutating selector set (transitive
        // closure over same-class self-sends) — see
        // `compute_class_var_mutating_selectors`'s doc comment. Depends on
        // `class_var_names` above, so must run after it; independent of
        // `class_method_selectors` above (recomputes its own local view).
        *self.class_var_mutating_selectors_mut() =
            crate::core_erlang::block_analysis::compute_class_var_mutating_selectors(
                class,
                self.class_var_names(),
            );

        // BT-996: Populate auto-generated keyword constructor selector for Value subclass: classes.
        // This allows `ClassName slot: value` inside a class method to route to the correct
        // class-side constructor instead of falling through to the instance-side getter.
        self.set_class_slot_constructor_selector(
            crate::core_erlang::value_type_codegen::compute_auto_slot_methods(class)
                .and_then(|auto| auto.keyword_constructor),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();

        for method in &class.class_methods {
            if method.kind != MethodKind::Primary {
                continue;
            }

            let selector_name = method.selector.name();
            // BT-412: +2 for ClassSelf and ClassVars parameters
            let arity = method.selector.arity() + 2;

            // Push scope for parameter bindings
            self.push_scope();
            self.current_method_params.clear();
            // BT-2709: Reset arithmetic fast-path parameter-type tracking.
            self.clear_method_param_types();
            self.reset_state_version();
            self.set_class_var_version(0);
            self.set_class_var_mutated(false);
            // BT-1435: Track current method selector for Logger intrinsic metadata.
            self.current_method_selector = Some(selector_name.to_string());

            // Bind ClassSelf as 'self' in scope
            self.bind_var("self", "ClassSelf");
            self.set_in_class_method(true);

            // Collect parameter names (mutates scope via fresh_var)
            let param_vars: Vec<String> = method
                .parameters
                .iter()
                .map(|p| {
                    let var_name = self.fresh_var(&p.name.name);
                    self.current_method_params.push(var_name.clone());
                    // BT-2709: Record declared type for the arithmetic fast path.
                    self.record_method_param_type(&p.name.name, p.type_annotation.as_ref());
                    var_name
                })
                .collect();

            // BT-1202: Detect if method body has ^ inside blocks (needs NLR).
            let needs_nlr = self
                .semantic_facts
                .has_block_nlr_or_walk(&method.span, &method.body);

            let nlr_token_var = if needs_nlr {
                let token_var = self.fresh_temp_var("NlrToken");
                self.set_current_nlr_token(Some(token_var.clone()));
                Some(token_var)
            } else {
                None
            };

            // ADR 0101 / BT-2720: On a `native:` Object, a class-side
            // `self delegate` body lowers through the unified FFI boundary,
            // omitting `self` from the arg list (class methods are not
            // instances). Gated to value-type codegen so native *actor* class
            // methods (compiled via the native facade with context=Actor) keep
            // their existing lowering.
            let native_class_delegate = matches!(self.context, CodeGenContext::ValueType)
                && method.is_self_delegate()
                && class.backing_module.is_some();

            // Generate body as Document and keep it in the Document pipeline (BT-875).
            let body_doc: Document<'static> = if native_class_delegate {
                self.set_current_nlr_token(None);
                let backing = class
                    .backing_module
                    .as_ref()
                    .expect("native_class_delegate implies backing_module is Some");
                Self::native_delegate_body_doc(
                    backing.name.as_str(),
                    class.name.name.as_str(),
                    &method.selector,
                    &param_vars,
                )
            } else if method.body.is_empty() {
                self.set_current_nlr_token(None);
                // Empty class method body returns self (ClassSelf)
                docvec!["ClassSelf"]
            } else {
                let mut body_stmts =
                    match self.lower_class_method_body(method, !class.class_variables.is_empty()) {
                        Ok(stmts) => stmts,
                        Err(e) => {
                            self.set_current_nlr_token(None);
                            self.pop_scope();
                            self.set_in_class_method(false);
                            self.current_method_selector = None;
                            return Err(e);
                        }
                    };
                self.set_current_nlr_token(None);
                // BT-1202: Use self.class_var_mutated (not just whether class vars are declared)
                // to preserve the {class_var_result, ...} contract. The normal path only wraps
                // in class_var_result when class vars were actually mutated; the NLR path must
                // match. class_var_mutated is set by lower_class_method_body when it sees a
                // class var assignment.
                let returns_class_var_result = self.class_var_mutated();
                // BT-1202/BT-3148/BT-3164 (ADR 0111 Addendum 4 task 2, closed out
                // for class methods by BT-3164): the token was already minted
                // above, before `lower_class_method_body` ran (production's real
                // mint order). Since BT-3164, `lower_class_method_body` returns a
                // real `Vec<ThreadedStmt>` (a real class-var `Bind` when the
                // body's last statement mutates one — see
                // `lower_class_method_last_class_var_bind`'s doc comment) rather
                // than one opaque `Statement` wrapping an already-rendered
                // `Document` — prepending a real `NlrCatch` here and verifying
                // the whole sequence in one `verify_and_render_body_stmts` call
                // is what lets `VerifyError::ShadowWriteMissing` see a real
                // class-var `Bind` jointly with this real `NlrCatch` for the
                // first time (ADR 0111 Addendum 6's closing note).
                if let Some(ref token_var) = nlr_token_var {
                    body_stmts.insert(
                        0,
                        threaded_ir::ThreadedStmt::NlrCatch {
                            boundary: super::super::NlrBoundary::ClassMethod {
                                has_class_vars: returns_class_var_result,
                            },
                            token: threaded_ir::TokenId::new(token_var.clone()),
                            frame: threaded_ir::FrameId::ROOT,
                            span: method.span,
                        },
                    );
                }
                self.verify_and_render_body_stmts(&body_stmts, method.span)
            };

            // Build function header with params (Document pieces, not format! —
            // Core Erlang fragments must use the Document API, BT-875).
            let doc = docvec![
                "\n",
                fname(safe_class_method_fn_name(&selector_name), arity),
                " = fun (ClassSelf, ClassVars",
                Self::class_method_params_suffix_doc(&param_vars),
                ") ->",
                nest(INDENT, docvec![line(), body_doc,]),
                "\n",
            ];
            docs.push(doc);

            self.pop_scope();
            self.set_in_class_method(false);
            self.current_method_selector = None;
        }
        self.class_var_names_mut().clear();
        self.class_method_selectors_mut().clear();
        self.set_class_slot_constructor_selector(None);
        Ok(Document::Vec(docs))
    }

    /// ADR 0084 / BT-2267: Lower the `classMethods:` argument of a programmatic
    /// `ClassBuilder` cascade — a map literal whose values are class-method block
    /// literals — into a Core Erlang map whose values are class-method funs.
    ///
    /// Each `#selector => [:self ... | body]` entry becomes
    /// `'selector' => fun (ClassSelf, ClassVars, A1..An) -> ... end`, matching the
    /// compiled `class_<sel>` calling convention so the runtime's fun-dispatch
    /// path (BT-2266) installs and invokes it identically. Non-block values, or
    /// blocks whose shape does not match the selector, fall through to ordinary
    /// expression lowering (a computed fun the user supplied).
    ///
    /// `class_var_names` are the keys of the cascade's `classVars:` map; they make
    /// `self.cvar` reads/writes lower as class-variable access (threaded through
    /// `{class_var_result, …}`). `class_name` keys the runtime self/`super`
    /// dispatch the funs emit (they have no module export to call).
    pub(in crate::core_erlang) fn generate_class_methods_map_arg(
        &mut self,
        pairs: &[MapPair],
        class_name: &str,
        class_var_names: &[String],
    ) -> Result<Document<'static>> {
        if pairs.is_empty() {
            return Ok(Document::Str("~{}~"));
        }

        // Establish the shared class-method context for every fun in the map,
        // saving/restoring any enclosing class context so a builder cascade
        // inside another class's method (or at the REPL top level) is unaffected.
        let saved = self.enter_builder_class_method_context(class_name, class_var_names);

        let mut parts: Vec<Document<'static>> = vec![Document::Str("~{ ")];
        let mut result: Result<()> = Ok(());
        for (i, pair) in pairs.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let key_doc = match self.expression_doc(&pair.key) {
                Ok(d) => d,
                Err(e) => {
                    result = Err(e);
                    break;
                }
            };
            let val_doc = match self.class_method_map_value_doc(&pair.key, &pair.value) {
                Ok(d) => d,
                Err(e) => {
                    result = Err(e);
                    break;
                }
            };
            parts.push(key_doc);
            parts.push(Document::Str(" => "));
            parts.push(val_doc);
        }
        parts.push(Document::Str(" }~"));

        self.exit_builder_class_method_context(saved);
        result?;
        Ok(Document::Vec(parts))
    }

    /// BT-2269: Lower the block argument of an incremental
    /// `addClassMethod: #sel body: [block]` setter into a class-method fun,
    /// mirroring how a `classMethods:` map value is lowered. The `key` is the
    /// selector-symbol argument (used to validate the block's parameter count).
    /// Enters/exits the builder class-method context around the single value so
    /// `self.cvar` access and self/`super` sends lower correctly, exactly as the
    /// map path does for each entry.
    pub(in crate::core_erlang) fn generate_class_method_single_arg(
        &mut self,
        key: &Expression,
        block: &Block,
        class_name: &str,
        class_var_names: &[String],
    ) -> Result<Document<'static>> {
        let value = Expression::Block(block.clone());
        let saved = self.enter_builder_class_method_context(class_name, class_var_names);
        let result = self.class_method_map_value_doc(key, &value);
        self.exit_builder_class_method_context(saved);
        result
    }

    /// Lowers a single `classMethods:` map value: a class-method fun for a literal
    /// block of the right shape, else ordinary expression lowering.
    ///
    /// BT-2276: A block literal whose parameter count does not match the
    /// selector (`self` plus one parameter per selector slot) is rejected at
    /// compile time with a [`CodeGenError::BlockArityError`]. Previously such a
    /// block fell through to ordinary expression lowering, producing a fun of the
    /// wrong arity that crashed with an opaque `error:undef` only when the class
    /// method was first called. A computed (non-block) value cannot have its
    /// arity checked at compile time, so it still falls through here and is
    /// validated at registration time (`beamtalk_class_builder:validate_class_method_arities/2`).
    fn class_method_map_value_doc(
        &mut self,
        key: &Expression,
        value: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Literal(Literal::Symbol(sym), _) = key {
            if let Some(selector) = super::super::class_builder_source::selector_from_symbol(sym) {
                if let Expression::Block(block) = value {
                    // A class-method block declares `self` plus one parameter per
                    // selector slot.
                    let expected = selector.arity() + 1;
                    if block.parameters.len() == expected {
                        return self.generate_class_method_fun_from_block(&selector, block);
                    }
                    return Err(CodeGenError::BlockArityError {
                        selector: format!("classMethods: {sym}"),
                        expected: expected.to_string(),
                        actual: block.parameters.len(),
                        hint: format!(
                            "Fix: A classMethods: block takes `self` plus one parameter per \
                             selector argument, so #{sym} needs {expected} parameter(s):\n\
                             \x20 classMethods: #{{ #{sym} => [:self{} | ...] }}",
                            Self::class_method_block_param_example(selector.arity())
                        ),
                    });
                }
            }
        }
        // Computed fun or non-conforming key: lower as an ordinary value. A
        // computed fun's arity is unknown until runtime and is validated at
        // registration time (BT-2276).
        self.expression_doc(value)
    }

    /// Builds the example trailing block parameters (`:a1 :a2 …`) for the
    /// `BlockArityError` hint shown when a `classMethods:` block has the wrong
    /// parameter count. Empty for a unary selector (just `:self`).
    fn class_method_block_param_example(selector_arity: usize) -> String {
        use std::fmt::Write as _;
        let mut out = String::new();
        for i in 1..=selector_arity {
            let _ = write!(out, " :a{i}");
        }
        out
    }

    /// Emits an anonymous class-method fun from a builder block literal.
    ///
    /// `fun (ClassSelf, ClassVars, P1..Pn) -> body` where the block's first
    /// parameter (the receiver) binds to `ClassSelf`, the remaining parameters to
    /// `P1..Pn`, and the body is lowered with the class-method machinery
    /// (`{class_var_result, …}` wrapping; self/`super` routed to runtime dispatch
    /// because there is no `class_<sel>` export). Assumes the caller has already
    /// entered the builder class-method context.
    fn generate_class_method_fun_from_block(
        &mut self,
        selector: &MessageSelector,
        block: &Block,
    ) -> Result<Document<'static>> {
        self.push_scope();
        self.current_method_params.clear();
        // BT-2709: Reset arithmetic fast-path parameter-type tracking.
        self.clear_method_param_types();
        self.reset_state_version();
        self.set_class_var_version(0);
        self.set_class_var_mutated(false);
        // ADR 0110 (BT-3037): the fun body executes at runtime as a class
        // method's own top frame, even when the builder cascade lexically sits
        // inside a block (`block_depth > 0` at the cascade's position). Reset
        // `block_depth` so `generate_field_assignment`'s shadow-write gate
        // (`block_depth == 0`) uniformly means "the method's own top frame"
        // across compiled methods and ClassBuilder funs alike; restored on
        // every exit path below.
        let saved_block_depth = self.block_depth;
        self.block_depth = 0;

        // The class is reachable via the conventional literal `self` (so
        // `self.cvar` access and self-sends lower correctly — both key on the
        // `self` identifier) and also via the block's receiver parameter under
        // whatever name it was declared.
        self.bind_var("self", "ClassSelf");
        if let Some(receiver_param) = block.parameters.first() {
            self.bind_var(&receiver_param.name, "ClassSelf");
        }
        // Remaining parameters become the fun's user parameters P1..Pn.
        let param_vars: Vec<String> = block.parameters[1..]
            .iter()
            .map(|bp| {
                let var_name = self.fresh_var(&bp.name);
                self.current_method_params.push(var_name.clone());
                var_name
            })
            .collect();

        // Synthesize a MethodDefinition so the shared class-method body lowering
        // applies unchanged.
        let params: Vec<ParameterDefinition> = block.parameters[1..]
            .iter()
            .map(|bp| ParameterDefinition::new(Identifier::new(bp.name.clone(), bp.span)))
            .collect();
        let method =
            MethodDefinition::new(selector.clone(), params, block.body.clone(), block.span);

        let needs_nlr = self
            .semantic_facts
            .has_block_nlr_or_walk(&block.span, &block.body);
        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        let has_class_vars = !self.class_var_names().is_empty();
        let body_doc: Document<'static> = if method.body.is_empty() {
            self.set_current_nlr_token(None);
            docvec!["ClassSelf"]
        } else {
            let mut body_stmts = match self.lower_class_method_body(&method, has_class_vars) {
                Ok(stmts) => stmts,
                Err(e) => {
                    self.set_current_nlr_token(None);
                    self.block_depth = saved_block_depth;
                    self.pop_scope();
                    return Err(e);
                }
            };
            self.set_current_nlr_token(None);
            let returns_class_var_result = self.class_var_mutated();
            // BT-3164: same real-`NlrCatch`-prepend pattern as
            // `generate_class_method_functions` — see that call site's
            // comment for why this replaces the old
            // `wrap_class_method_body_with_nlr_catch` Document-wrap.
            if let Some(ref token_var) = nlr_token_var {
                body_stmts.insert(
                    0,
                    threaded_ir::ThreadedStmt::NlrCatch {
                        boundary: super::super::NlrBoundary::ClassMethod {
                            has_class_vars: returns_class_var_result,
                        },
                        token: threaded_ir::TokenId::new(token_var.clone()),
                        frame: threaded_ir::FrameId::ROOT,
                        span: method.span,
                    },
                );
            }
            self.verify_and_render_body_stmts(&body_stmts, method.span)
        };

        let doc = docvec![
            "fun (ClassSelf, ClassVars",
            Self::class_method_params_suffix_doc(&param_vars),
            ") ->",
            nest(INDENT, docvec![line(), body_doc]),
        ];

        self.block_depth = saved_block_depth;
        self.pop_scope();
        Ok(doc)
    }

    /// Builds the trailing fun parameter list `, P1, P2, …` as `Document` pieces
    /// (never `format!` — Core Erlang fragments must use the Document API,
    /// BT-875). Empty when there are no user parameters.
    fn class_method_params_suffix_doc(param_vars: &[String]) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for var in param_vars {
            parts.push(Document::Str(", "));
            parts.push(leaf::var(var.clone()));
        }
        Document::Vec(parts)
    }

    /// Lowers the body of a class-side method to one straight-line
    /// `Vec<ThreadedStmt>` (BT-3164, ADR 0111 Addendum 4/6: mirrors
    /// `lower_body_exprs_with_reply`'s pattern for the Actor pipeline, for
    /// the class-method body pipeline BT-3148 explicitly left as a
    /// hand-written `Document` builder).
    ///
    /// Unlike instance methods, class methods have no `State` threading —
    /// the only version-mutating construct a class method's own body can
    /// directly produce is a class-var write (`self.classVar := value`, ADR
    /// 0110's `ClassVars` counter). Every other body statement — local-var
    /// bindings, destructuring, `^`-returns, class-method self-sends (whose
    /// own class-var rebind, if any, is produced by the shared
    /// `emit_class_var_result_unwrap` helper and stays opaque here — the
    /// same "mutation hidden inside a shared multi-module helper" treatment
    /// BT-3148 gave `generate_self_dispatch_open` et al. in the Actor
    /// pipeline) — is an opaque [`threaded_ir::ThreadedStmt::Statement`]
    /// built by the SAME `generate_class_method_*` codegen calls production
    /// used before this migration (byte-identity: only the container
    /// changed, from `Vec<Document>` to `Vec<ThreadedStmt>`).
    ///
    /// The ONE case promoted to a real [`threaded_ir::ThreadedStmt::Bind`]
    /// is a class method's own direct `self.classVar := value` when it is
    /// the body's *last* statement (implicit return) — mirroring exactly
    /// how BT-3148's Actor pipeline only promotes
    /// `BodyExprKind::FieldAssignment` to a real `Bind` in its `is_last`
    /// arm (`lower_body_exprs_with_reply`), leaving every other position's
    /// field mutation inside a shared helper's opaque `Statement`. This is
    /// the ADR 0110 joint-visibility case this issue exists to close: once
    /// the caller (`generate_class_method_functions`/
    /// `generate_class_method_fun_from_block`) prepends a real `NlrCatch`,
    /// this `Bind` and that `NlrCatch` are visible to the SAME `verify()`
    /// call for the first time — see `lower_class_method_last_class_var_bind`'s
    /// own doc comment for why the pre-existing isolated
    /// `construct_and_verify_class_var_bind` check stays alongside the new
    /// joint one rather than being replaced by it.
    ///
    /// BT-412: when a class-var write happened anywhere in the body
    /// (`class_var_mutated()`), the caller wraps the final result in
    /// `{class_var_result, Result, ClassVarsN}` — unchanged, decided after
    /// this function returns, exactly as before.
    fn lower_class_method_body(
        &mut self,
        method: &MethodDefinition,
        has_class_vars: bool,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        use threaded_ir::ThreadedStmt;

        let mut stmts: Vec<ThreadedStmt> = Vec::new();

        // Filter out @expect directives (compile-time only, no runtime representation).
        let body = super::super::util::collect_body_exprs(&method.body);
        let body_len = body.len();
        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body_len - 1;
            let span = expr.span();

            if let Expression::Return { value, .. } = expr {
                let doc = self.generate_class_method_return(value, has_class_vars)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
                return Ok(stmts);
            }

            if is_last && has_class_vars && self.is_class_var_assignment(expr) {
                self.lower_class_method_last_class_var_bind(&mut stmts, expr, span)?;
            } else if is_last {
                let doc = self.generate_class_method_last_expr(expr, has_class_vars)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
            } else if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
                // ADR 0118 phase 5a (BT-3421): splice the real `ClassVars`
                // prelude instead of wrapping one opaque `Statement` around
                // an already-rendered open-Document (`generate_class_method_non_last_expr`'s
                // old branch for this same condition) — every non-last
                // class-var mutation is now a genuine, verified `Bind` in
                // this body's own IR, closing the ADR 0111 Addendum 6 gap
                // `verify_body_with_opaque_version_gaps`'s `ClassVars`
                // backfill used to paper over for class methods. The
                // statement's own value is discarded (matching the old
                // behaviour: nothing here ever referenced it), so only the
                // prelude is spliced.
                let tv = self.threaded_expression(expr, threaded_ir::FrameId::ROOT)?;
                stmts.extend(tv.prelude);
            } else {
                let doc = self.generate_class_method_non_last_expr(expr)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
            }
        }
        Ok(stmts)
    }

    /// BT-3164: constructs the real `ThreadedStmt::Bind` for a class
    /// method's own `self.classVar := value` when it is the body's last
    /// statement — the shape `lower_class_method_body` promotes out of the
    /// generic `generate_class_method_last_expr_with_class_vars` path.
    /// Delegates the actual `Bind` construction to the shared
    /// [`Self::lower_class_var_field_assignment_bind`] (`expressions.rs`;
    /// same struct, `impl` block in a different file — the identical
    /// sequence `expressions.rs::generate_class_var_field_assignment`
    /// builds for every OTHER position, not hand-rolled a second time here,
    /// CLAUDE.md's no-duplicate-implementations rule), but — unlike that
    /// call site, which still renders its `Bind` immediately and keeps it
    /// inside an opaque `Statement` — pushes the returned `Bind` into
    /// `stmts` as a real IR node, so the method's real `NlrCatch`
    /// (prepended by the caller after this function returns) and this
    /// `Bind` are both visible to the single `verify_and_render_body_stmts`
    /// call over the whole body, closing the ADR 0110 joint-visibility gap
    /// ADR 0111 Addendum 6 left open for class methods.
    ///
    /// The isolated, synthetic-marker `ShadowWriteMissing` check the shared
    /// helper runs internally is deliberately still reported (not dropped
    /// in favor of the new joint check): it is the ONLY check that still
    /// fires for a method with no literal `^` at all (`needs_nlr: false`,
    /// so no real `NlrCatch` in the body at all) — the exact ADR 0110
    /// `CollectionDriver countedRun:over:` repro shape (the mutation must
    /// still be shadow-written even though this specific method never
    /// mints a local NLR catch, because the relay can happen one layer out
    /// via a caller-supplied block) — so dropping it would regress
    /// coverage the joint check cannot replace. The two checks are
    /// complementary, not redundant: the isolated one always assumes the
    /// worst case; the joint one is precise when a real `NlrCatch` is
    /// actually present.
    fn lower_class_method_last_class_var_bind(
        &mut self,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
        expr: &Expression,
        span: Span,
    ) -> Result<()> {
        let (field_name, value) = match expr {
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::FieldAccess { field, .. } => (field.name.to_string(), value.as_ref()),
                _ => unreachable!(
                    "is_class_var_assignment guarantees an Assignment with a FieldAccess target"
                ),
            },
            _ => unreachable!("is_class_var_assignment guarantees an Assignment"),
        };

        let (preamble_doc, bind, val_var) = self.lower_class_var_field_assignment_bind(
            &field_name,
            value,
            threaded_ir::FrameId::ROOT,
        )?;

        let final_cv = self.current_class_var();
        stmts.push(threaded_ir::ThreadedStmt::Statement(preamble_doc, span));
        stmts.push(bind);
        stmts.push(threaded_ir::ThreadedStmt::Statement(
            docvec![
                "{'class_var_result', ",
                leaf::var(val_var),
                ", ",
                leaf::var(final_cv),
                "}",
            ],
            span,
        ));
        Ok(())
    }

    /// Generates code for an explicit `^` return in a class method.
    fn generate_class_method_return(
        &mut self,
        value: &Expression,
        has_class_vars: bool,
    ) -> Result<Document<'static>> {
        // BT-2358: An explicit `^` return of a value-type threading construct
        // (counted/while loop, foldl list-op, or read+write conditional) must
        // unwrap the construct's logical value rather than leak the raw
        // `{value, StateAcc}` tuple (or crash dispatching a read+write
        // conditional's stateful block at the wrong arity). This mirrors the
        // implicit last-expression path (`generate_class_method_last_expr`);
        // the shared helper applies the `{class_var_result, …}` wrapping based on
        // `class_var_mutated()`, identical to the wrapping below — so it is
        // correct for both the class-vars and no-class-vars cases.
        if let Some(doc) = self.try_generate_class_method_threaded_last(
            value,
            super::super::threaded_expr::ThreadingPosition::Return,
        )? {
            return Ok(doc);
        }
        // ADR 0118 phase 5b (BT-3422): mirrors
        // `generate_class_method_last_expr_with_class_vars` — when `value`
        // is itself a recognized producer (a class-var assignment, or a
        // *locally declared* class-method self-send per
        // `is_class_method_self_send`'s `class_method_selectors()` check),
        // `threaded_expression` gives it a real prelude whose rebound
        // `ClassVarsN` stays lexically visible here, so `current_class_var()`
        // below is safe to read directly. Otherwise `value` may still
        // dispatch a class-var-mutating self-send that the compile below
        // reaches opaquely and closes (e.g. BT-2007 inherited dispatch,
        // deliberately excluded by that same check) — closing loses the
        // mutated name's LEXICAL visibility, but not the mutation itself,
        // so `refresh_class_var_after_opaque_scope` recovers the live value
        // via the ADR 0110 shadow write instead of relying on lexical scope.
        if has_class_vars {
            if self.is_class_var_assignment(value) || self.is_class_method_self_send(value) {
                let result_var = self.fresh_temp_var("Ret");
                let frame = self.current_frame();
                let tv = self.threaded_expression(value, frame)?;
                let preamble = self.threaded_prelude_doc(&tv.prelude);
                let value_doc = self.threaded_value_doc(&tv.value);
                if self.class_var_mutated() {
                    let final_cv = self.current_class_var();
                    Ok(docvec![
                        preamble,
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        value_doc,
                        " in {'class_var_result', ",
                        leaf::var(result_var),
                        ", ",
                        leaf::var(final_cv),
                        "}",
                    ])
                } else {
                    Ok(docvec![
                        preamble,
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        value_doc,
                        " in ",
                        leaf::var(result_var),
                    ])
                }
            } else {
                let result_var = self.fresh_temp_var("Ret");
                let cv_version_before = self.class_var_version();
                let expr_doc = self.expression_doc(value)?;
                let refresh = self.refresh_class_var_after_opaque_scope(cv_version_before);
                if self.class_var_mutated() {
                    let final_cv = self.current_class_var();
                    Ok(docvec![
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        expr_doc,
                        " in ",
                        refresh.unwrap_or(Document::Nil),
                        "{'class_var_result', ",
                        leaf::var(result_var),
                        ", ",
                        leaf::var(final_cv),
                        "}",
                    ])
                } else {
                    Ok(docvec![
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        expr_doc,
                        " in ",
                        leaf::var(result_var),
                    ])
                }
            }
        } else {
            // ADR 0118 phase 5b (BT-3422): same treatment for the
            // no-class-vars path — no `class_var_mutated()`/`current_class_var()`
            // read follows, so the prelude and value simply concatenate.
            let frame = self.current_frame();
            self.threaded_expression_doc(value, frame)
        }
    }

    /// Generates code for the last expression in a class method body.
    fn generate_class_method_last_expr(
        &mut self,
        expr: &Expression,
        has_class_vars: bool,
    ) -> Result<Document<'static>> {
        // BT-2349: A last-position threading construct (counted/while loop or foldl list-op
        // yielding a `{value, StateAcc}` tuple) or a read+write conditional must unwrap the
        // construct's logical value rather than leak the raw tuple (or crash on the 0-arg
        // stateful-block dispatch). Handled here because the `{class_var_result, ...}` wrapping
        // is identical whether or not the class declares class vars — threading constructs
        // mutate *locals*, not class vars, so the wrapping is driven solely by whether an
        // earlier statement mutated a class var (`class_var_mutated()`).
        if let Some(doc) = self.try_generate_class_method_threaded_last(
            expr,
            super::super::threaded_expr::ThreadingPosition::Last,
        )? {
            return Ok(doc);
        }
        if has_class_vars {
            self.generate_class_method_last_expr_with_class_vars(expr)
        } else {
            self.generate_class_method_last_expr_no_class_vars(expr)
        }
    }

    /// BT-2349: Handles a class method's last expression when it is a value-type threading
    /// construct (counted/while loop or foldl list-op) or a read+write conditional.
    ///
    /// Returns `None` when `expr` is neither, so the caller falls back to the standard
    /// last-expression paths.
    ///
    /// Both shapes produce a logical value bound to a fresh result var (via the shared BT-2342
    /// value-type primitives), which is then wrapped in `{class_var_result, Result, ClassVarsN}`
    /// when an earlier statement mutated a class var, or returned bare otherwise.
    fn try_generate_class_method_threaded_last(
        &mut self,
        expr: &Expression,
        position: super::super::threaded_expr::ThreadingPosition,
    ) -> Result<Option<Document<'static>>> {
        // BT-2361: route through the shared `ThreadedExpr` transform + boundary emitter.
        // BT-2358: it peels redundant parentheses (e.g. `^(items collect: …)` or
        // `(flag ifTrue: [...])`) so the threading construct inside is unwrapped to its
        // logical value rather than leaking its raw `{value, StateAcc}` tuple. Applies to
        // both the explicit `^`-return and the implicit last-expression callers.
        let mut parts: Vec<Document<'static>> = Vec::new();
        if self.emit_threaded_last(
            expr,
            position,
            super::super::threaded_expr::ThreadingBoundary::ClassMethod,
            &mut parts,
        )? {
            Ok(Some(Document::Vec(parts)))
        } else {
            Ok(None)
        }
    }

    /// Last expression with class vars: may need `{class_var_result, ...}` wrapping.
    fn generate_class_method_last_expr_with_class_vars(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let frame = self.current_frame();
        if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
            // ADR 0118 phase 5b (BT-3422): `expr` is itself a producer at
            // its own top level, so `threaded_expression` always gives it a
            // real value (never the do:-in-direct-params-loop `'nil'` case
            // — a class-var assignment/self-send never produces that).
            // `final_cv` is read AFTER threading so it reflects the rebind.
            let tv = self.threaded_expression(expr, frame)?;
            let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
            let value_doc = self.threaded_value_doc(&tv.value);
            let final_cv = self.current_class_var();
            Ok(docvec![
                prelude_doc,
                "{'class_var_result', ",
                value_doc,
                ", ",
                leaf::var(final_cv),
                "}",
            ])
        } else {
            // `expr` is not ITSELF a recognized producer at this level, but
            // may still dispatch one that the compile below reaches
            // opaquely and closes (e.g. a same-class self-send NOT declared
            // locally — BT-2007 inherited dispatch — which
            // `is_class_method_self_send`'s `class_method_selectors()`
            // check deliberately excludes, per its own doc comment, since
            // `try_handle_class_method_self_send`'s real reach is any
            // `self`-receiver send regardless of selector). Closing loses
            // the mutated `ClassVarsN` name's LEXICAL visibility, but not
            // the mutation itself — `refresh_class_var_after_opaque_scope`
            // recovers the live value via the ADR 0110 shadow write rather
            // than relying on lexical scope, so this is robust to whatever
            // depth/shape the opaque compile below reaches.
            let result_var = self.fresh_temp_var("Ret");
            let cv_version_before = self.class_var_version();
            let expr_doc = self.expression_doc(expr)?;
            let refresh = self.refresh_class_var_after_opaque_scope(cv_version_before);
            if self.class_var_mutated() {
                let final_cv = self.current_class_var();
                Ok(docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    expr_doc,
                    " in ",
                    refresh.unwrap_or(Document::Nil),
                    "{'class_var_result', ",
                    leaf::var(result_var),
                    ", ",
                    leaf::var(final_cv),
                    "}",
                ])
            } else {
                Ok(docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    expr_doc,
                    " in ",
                    leaf::var(result_var),
                ])
            }
        }
    }

    /// Last expression without class vars: simpler wrapping.
    fn generate_class_method_last_expr_no_class_vars(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        // ADR 0118 phase 5b (BT-3422): no `class_var_result` wrapping and no
        // later read of `current_class_var()` follows either branch below,
        // so both the bare self-send case (BT-891) and the general case
        // (BT-1201) collapse to the same plain threaded compile.
        let frame = self.current_frame();
        self.threaded_expression_doc(expr, frame)
    }

    /// Generates code for a non-last expression in a class method body.
    ///
    /// ADR 0118 phase 5a (BT-3421): a class-var assignment or class-method
    /// self-send is intercepted one level up, in `lower_class_method_body`,
    /// which splices its real `ClassVars` prelude directly instead of
    /// calling this function — so this function's own callers never reach
    /// it with either of those shapes any more.
    fn generate_class_method_non_last_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if Self::is_local_var_assignment(expr) {
            self.generate_class_method_local_var_binding(expr)
        } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let binding_docs = self.generate_destructure_bindings(pattern, value)?;
            Ok(Document::Vec(binding_docs))
        } else if self.is_do_with_vt_local_threading(expr) {
            // BT-1414: Non-last `do:` loop that mutates captured outer locals.
            self.generate_value_type_do_open(expr)
        } else if self.is_counted_loop_with_vt_local_threading(expr) {
            // BT-2349: Non-last counted loop (to:do:/to:by:do:/timesRepeat:) that
            // mutates captured outer locals. Extracts the threaded locals from the
            // `{'nil', StateAcc}` tuple so subsequent statements see the updates.
            self.generate_vt_counted_loop_open(expr)
        } else if self.is_while_with_vt_local_threading(expr) {
            // BT-2349: Non-last whileTrue:/whileFalse: that mutates captured outer locals.
            self.generate_vt_while_open(expr)
        } else if self.is_foldl_list_op_with_vt_local_threading(expr) {
            // BT-2349: Non-last collect:/select:/reject:/inject:into: that mutates captured
            // outer locals. Extracts the threaded locals from the `{value, StateAcc}` tuple
            // (the logical value is discarded in non-last position).
            self.generate_vt_foldl_list_op_open(expr)
        } else if self.is_conditional_with_vt_local_threading(expr) {
            // BT-1392: Non-last conditional that mutates captured outer locals.
            self.generate_vt_conditional_open(expr)
        } else if self.is_exception_construct_with_vt_local_threading(expr) {
            // BT-3177: Non-last on:do:/ensure: that mutates captured outer
            // locals. Extracts the threaded locals from the returned
            // `{Result, StateAcc}` tuple, same idiom as the loop/conditional
            // arms above.
            self.generate_vt_exception_construct_open(expr)
        } else {
            // `expr` may dispatch a class-method self-send (locally
            // declared or, per BT-2007, inherited) that rebinds `ClassVarsN`
            // opaquely, closed by the time this call returns —
            // `refresh_class_var_after_opaque_scope` recovers the live
            // value via the ADR 0110 shadow write (rather than relying on
            // lexical scope) so the NEXT statement in this same body — which
            // reads `current_class_var()` when it builds its own call —
            // sees it regardless of nesting depth. Bind the result to the
            // seq temp so subsequent code can sequence after it.
            let tmp_var = self.fresh_temp_var("seq");
            let cv_version_before = self.class_var_version();
            let expr_doc = self.expression_doc(expr)?;
            let refresh = self
                .refresh_class_var_after_opaque_scope(cv_version_before)
                .unwrap_or(Document::Nil);
            Ok(docvec![
                "let ",
                leaf::var(tmp_var),
                " = ",
                expr_doc,
                " in ",
                refresh,
            ])
        }
    }

    /// BT-741: Local variable assignment in class method — create a proper `let` binding.
    fn generate_class_method_local_var_binding(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                // BT-2349/BT-2371: When the RHS is a threading construct (value-type loop /
                // foldl list-op yielding `{value, StateAcc}`) or a read+write conditional
                // (each branch emits `{LogicalValue, Mut1..MutN}`), bind the target to the
                // logical value and rebind the threaded siblings — rather than binding the
                // target to the raw tuple. BT-2361: shared with the value-type instance-method
                // body sequencer via `emit_threaded_assign_rhs`.
                let mut parts: Vec<Document<'static>> = Vec::new();
                if self
                    .emit_threaded_assign_rhs(&id.name, value, &mut parts)?
                    .is_some()
                {
                    return Ok(Document::Vec(parts));
                }
                let var_name = &id.name;
                let core_var = self
                    .lookup_var(var_name)
                    .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                // BT-3169: captured before generating `value` — a
                // class-method self-send inside it (locally declared or,
                // per BT-2007, inherited — `is_class_method_self_send`'s
                // `class_method_selectors()` check only recognizes the
                // former) may rebind `ClassVarsN` opaquely, closed by the
                // time this call returns; `refresh_class_var_after_opaque_scope`
                // recovers the live value via the ADR 0110 shadow write
                // rather than relying on lexical scope, so this is robust
                // to whatever depth/shape the compile below reaches.
                let cv_version_before = self.class_var_version();
                let val_doc = self.expression_doc(value)?;
                self.bind_var(var_name, &core_var);
                let refresh = self
                    .refresh_class_var_after_opaque_scope(cv_version_before)
                    .unwrap_or(Document::Nil);
                return Ok(docvec![
                    "let ",
                    leaf::var(core_var),
                    " = ",
                    val_doc,
                    " in ",
                    refresh,
                ]);
            }
        }
        Ok(Document::Nil)
    }

    /// Extracts source text for a method using the AST unparser (BT-977).
    ///
    /// The unparser produces complete, comment-inclusive source for all methods,
    /// whether parsed from a `.bt` file or constructed programmatically by a live
    /// tool (synthesized methods have no source text but still produce valid output).
    ///
    /// Previously this used raw byte-range slicing (`source[span.start..span.end]`),
    /// which silently dropped leading comments (they appear before `method.span.start()`)
    /// and fell back to the selector name for synthesized methods. The unparser fixes
    /// both deficiencies — see ADR 0044 Phase 4.
    ///
    /// BT-3249: `class_name`/`is_class_method` identify `method` within
    /// `self.method_return_types_written_back` (keyed by
    /// `(class_name, selector, is_class_method)` — the caller's own
    /// knowledge of which method list `method` came from, rather than
    /// `method.is_class_method`, which a standalone extension method
    /// (`Target class >> sel`) does not reliably carry). When present, the
    /// return-type writeback pass — not the user — wrote `method.return_type`,
    /// so this unparses a throwaway clone with `return_type` reset to `None`
    /// instead of `method` itself. Without this, the image-resident
    /// `__source__` text this feeds (used by the System Browser / cockpit /
    /// `SystemNavigation` scanners) carries an inferred `-> Type` annotation
    /// the `ChangeLog`'s canonical `source_ref` (unparsed pre-writeback) never
    /// had — the divergence that produces a spurious revert-then-resave
    /// `ChangeLog` entry. `method`'s own `return_type` is never mutated:
    /// codegen elsewhere (specs, `method_return_types` metadata) still needs
    /// the inferred type.
    pub(super) fn extract_method_source(
        &self,
        class_name: &str,
        is_class_method: bool,
        method: &MethodDefinition,
    ) -> String {
        let key: beamtalk_core::semantic_analysis::MethodReturnKey = (
            EcoString::from(class_name),
            method.selector.name(),
            is_class_method,
        );
        if method.return_type.is_some() && self.method_return_types_written_back.contains_key(&key)
        {
            let mut stripped = method.clone();
            beamtalk_core::semantic_analysis::clear_return_type_writeback_for_key(
                &mut stripped,
                &key,
                &self.method_return_types_written_back,
            );
            beamtalk_core::unparse::unparse_method(&stripped)
        } else {
            beamtalk_core::unparse::unparse_method(method)
        }
    }

    /// BT-851: Checks if an expression is a `value:` call on a Tier 2 block parameter.
    ///
    /// When true, the expression will generate a `{Result, NewState}` tuple via
    /// `generate_block_value_call_stateful()` and must be unpacked by the caller.
    pub(in crate::core_erlang) fn is_tier2_value_call(&self, expr: &Expression) -> bool {
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            let (is_positional_value_selector, is_value_with_arguments) = match selector {
                beamtalk_core::ast::MessageSelector::Unary(name) => (name == "value", false),
                beamtalk_core::ast::MessageSelector::Keyword(parts) => {
                    let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                    (
                        matches!(
                            selector_name.as_str(),
                            "value:" | "value:value:" | "value:value:value:"
                        ),
                        selector_name == "valueWithArguments:",
                    )
                }
                beamtalk_core::ast::MessageSelector::Binary(_) => (false, false),
            };
            if is_positional_value_selector || is_value_with_arguments {
                // BT-851: Tier 2 block parameter (variable holding a stateful block)
                // BT-2797: or a local variable this method itself assigned a Tier 2
                // block literal to earlier in its own body (tier2_local_vars).
                if let Expression::Identifier(id) = receiver.as_ref() {
                    if self.tier2_block_params.contains(id.name.as_str())
                        || self.tier2_local_vars.contains(id.name.as_str())
                    {
                        return true;
                    }
                }
                // BT-2797: `self.field value(:...)` — the field may hold a Tier 2
                // block assigned from a different method than this call site, so
                // it needs the runtime is_function/2 discrimination generated by
                // generate_block_value_call_runtime_discriminated, which always
                // returns a {Result, NewState} tuple that this call site must
                // unpack (same as the statically-known-Tier-2 cases above).
                // BT-2803: `valueWithArguments:` gets the same treatment via
                // generate_block_value_with_arguments_call_runtime_discriminated.
                if self.context == super::super::CodeGenContext::Actor
                    && Self::is_self_field_access(receiver)
                {
                    return true;
                }
            }
            // BT-2803: literal-block-with-mutations receivers stay scoped to the
            // positional value/value:/... selectors — generate_block_value_inline_with_mutations
            // binds `arguments` directly to the block's own parameters, which
            // doesn't hold for valueWithArguments: (a single runtime list, not
            // per-parameter positional args). Not a motivating shape for BT-2803.
            if is_positional_value_selector {
                // BT-1213: Inline block literal with captured mutations
                // (e.g. [errors := errors add: #foo] value)
                // Only in Actor/REPL context — ValueType inlines as plain value (no tuple).
                if let Expression::Block(block) = receiver.as_ref() {
                    if self.context != super::super::CodeGenContext::ValueType
                        && !Self::captured_mutations_for_block(block).is_empty()
                    {
                        return true;
                    }
                    // BT-1481: Block literal with field mutations (actor state threading)
                    if self.context != super::super::CodeGenContext::ValueType {
                        let analysis = super::super::block_analysis::analyze_block(block);
                        if self.needs_mutation_threading(&analysis) {
                            return true;
                        }
                    }
                }
            }
        }
        // BT-2808: `blk value: x; value: y` — a cascade where every message
        // (including the one the parser folds into `receiver` — see
        // `normalize_cascade`) is itself a safe value-family send on a receiver
        // that (by the same rules as the single-send case above) may hold a
        // Tier 2 block. Each message needs the same tuple-unpacking treatment
        // as a single Tier2ValueCall, sequenced through
        // `generate_tier2_cascade_doc`.
        if let Expression::Cascade {
            receiver, messages, ..
        } = expr
        {
            let (underlying_receiver, all_messages) = Self::normalize_cascade(receiver, messages);
            let all_safe_value_sends = !all_messages.is_empty()
                && all_messages
                    .iter()
                    .all(|(sel, _)| Self::is_safe_value_family_selector(sel));
            if all_safe_value_sends {
                if let Expression::Identifier(id) = underlying_receiver {
                    if self.tier2_block_params.contains(id.name.as_str())
                        || self.tier2_local_vars.contains(id.name.as_str())
                    {
                        return true;
                    }
                }
                if self.context == super::super::CodeGenContext::Actor
                    && Self::is_self_field_access(underlying_receiver)
                {
                    return true;
                }
            }
        }
        false
    }

    /// BT-2880: `true` when a `match:` needs actor state threading — i.e. it
    /// runs in Actor context and at least one arm's body either is a Tier 2
    /// value-call (most commonly a state-mutating `[...] value` block) or is
    /// itself a nested control-flow-with-mutations construct (`ifTrue:`/
    /// `ifFalse:`/a nested `match:`/etc. with no `[...] value` wrapper, e.g.
    /// `nil -> flag ifTrue: [self.x := 1]`). `generate_match` checks this once
    /// per `match:` and, when true, compiles every arm's body to a uniform
    /// `{Value, State}` shape so the whole expression can be unwrapped by the
    /// same machinery as `ifTrue:`/`ifFalse:` mutations
    /// (`control_flow_has_mutations`'s `Expression::Match` branch above — this
    /// function is mutually recursive with it, which is what lets a nested
    /// `match:` arm body be detected too).
    pub(in crate::core_erlang) fn match_needs_mutation_threading(
        &self,
        arms: &[beamtalk_core::ast::MatchArm],
    ) -> bool {
        self.context == super::super::CodeGenContext::Actor
            && arms.iter().any(|arm| {
                self.is_tier2_value_call(&arm.body)
                    || self.control_flow_has_mutations(&arm.body)
                    // BT-3420 (ADR 0118 phase 4): an arm body that is
                    // neither a Tier 2 block-value call nor itself a nested
                    // control-flow-with-mutations construct, but DOES
                    // contain a (possibly nested, hoistable) actor
                    // self-send — `1 -> 1 + (self bumpCount)` — still needs
                    // this `match:` threaded, so `generate_match_arm_body`'s
                    // plain-wrap arm gets a chance to hoist it instead of
                    // silently dropping the mutation via a bare
                    // `expression_doc` compile.
                    || self.conditional_receiver_needs_threading(&arm.body)
            })
    }

    /// BT-1213/BT-2815: Returns captured mutation variable names for a Tier 2
    /// value-call statement (`expr` already classified/proven
    /// `BodyExprKind::Tier2ValueCall` by `is_tier2_value_call`) whose receiver
    /// mutates outer locals, so the caller can rebind them after the call.
    ///
    /// Handles both:
    /// - An inline block literal receiver (`[block] value`/`value:`/... —
    ///   the original BT-1213 scope), via `captured_mutations_for_block` on
    ///   the literal directly.
    /// - BT-2815: a NAMED `tier2_local_vars` identifier receiver (`blk value:
    ///   x`) whose block literal was assigned earlier in the same method —
    ///   the call site only has the identifier, not the block AST, so this
    ///   looks up the mutations `prescan_tier2_local_vars` already recorded
    ///   for that variable name in `tier2_local_var_captured_mutations`.
    ///
    /// Also handles a `Cascade` expression (`blk value: x; value: x`) by
    /// normalizing to its true underlying receiver first — the same
    /// receiver-shape checks then apply.
    //
    // BT-2797 (PR #2899 review fix): widened from private to
    // `pub(in crate::core_erlang)` so `control_flow/conditionals.rs`
    // can rebind captured local-var mutations for a bare `Tier2ValueCall`
    // statement inside a conditional branch, mirroring this file's own
    // `Tier2ValueCall` handling.
    pub(in crate::core_erlang) fn get_inline_block_captured_mutations(
        &self,
        expr: &Expression,
    ) -> Option<Vec<String>> {
        let receiver = match expr {
            Expression::MessageSend { receiver, .. } => receiver.as_ref(),
            Expression::Cascade {
                receiver, messages, ..
            } => Self::normalize_cascade(receiver, messages).0,
            _ => return None,
        };
        if let Expression::Identifier(id) = receiver {
            if let Some(mutations) = self
                .tier2_local_var_captured_mutations
                .get(id.name.as_str())
            {
                return Some(mutations.clone());
            }
        }
        Self::inline_block_captured_mutations(expr)
    }

    /// BT-2797 (PR #2899 review fix): generates the `Document` for an
    /// expression already classified as `BodyExprKind::Tier2ValueCall` or the
    /// RHS of `BodyExprKind::LocalAssignTier2` — i.e. a `value`/`value:`/etc.
    /// send that `is_tier2_value_call` proved needs Tier 2 tuple-unpacking
    /// treatment.
    ///
    /// When the receiver is a `self.field` access, this calls
    /// `generate_block_value_call_runtime_discriminated` directly instead of
    /// going through the generic `expression_doc` dispatch. That function is
    /// deliberately NOT reachable from `expression_doc` (see the matching
    /// comment on it and in `intrinsics.rs`'s `try_generate_block_value_unary`/
    /// `try_generate_block_value_keyword`): every call site of *this* helper
    /// unpacks the `{Result, NewState}` tuple it returns, but an arbitrary
    /// sub-expression reached via plain `expression_doc` would not, silently
    /// handing the raw tuple to code expecting a plain value.
    ///
    /// For every other `Tier2ValueCall` shape (a `tier2_block_params`/
    /// `tier2_local_vars` identifier receiver, or an inline literal block with
    /// captured/field mutations), falls through to `expression_doc`, which
    /// already handles those correctly.
    ///
    /// Also called from `control_flow/mod.rs`'s
    /// `generate_local_var_assignment_in_loop` (the `is_tier2_value_call`
    /// branch there — BT-912) for the same reason: it unpacks a
    /// `{Result, NewState}` tuple, so it must reach the same
    /// runtime-discriminated codegen for a `self.field` receiver.
    ///
    /// BT-2803: `valueWithArguments:` has no compile-time-known-Tier-2
    /// "stateful" fast path the way `value`/`value:` do
    /// (`generate_block_value_call_stateful`) — `is_tier2_value_call` only
    /// ever proves a `valueWithArguments:` send needs Tier 2 handling at
    /// all, never which arity branch statically applies, so every match
    /// (`self.field`, `tier2_block_params`, `tier2_local_vars`) routes
    /// through the same runtime-discriminated codegen here, unconditionally
    /// — unlike the positional selectors' `self.field`-only special case
    /// below.
    pub(in crate::core_erlang) fn generate_tier2_value_call_doc(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if selector.name() == "valueWithArguments:" {
                // The parser always gives a keyword message at least one
                // argument, so `arguments` is never empty here — but fail
                // loudly instead of silently falling through to the
                // `self.field` positional-value: branch below, which would
                // emit a malformed 0-arity value call for this selector.
                let args_expr = arguments.first().ok_or_else(|| {
                    CodeGenError::Internal(
                        "valueWithArguments: with no argument expression".to_string(),
                    )
                })?;
                return self.generate_block_value_with_arguments_call_runtime_discriminated(
                    receiver, args_expr,
                );
            }
            if self.context == CodeGenContext::Actor && Self::is_self_field_access(receiver) {
                return self.generate_block_value_call_runtime_discriminated(
                    receiver,
                    arguments,
                    &selector.name(),
                );
            }
            // BT-2814: `tier2_local_vars`/`tier2_block_params` receivers must
            // also bypass `expression_doc` below — since `try_generate_block_value_unary`/
            // `try_generate_block_value_keyword` now intercept this same
            // receiver shape from the generic sub-expression dispatch and
            // (correctly, for THAT position) close the tuple down to just
            // `Result`. Calling `generate_block_value_call_stateful` directly
            // here, exactly like the `self.field` case above, keeps this
            // TOP-LEVEL statement path getting the raw `{Result, NewState}`
            // tuple its callers (`BodyExprKind::Tier2ValueCall` handling in
            // this file, `conditionals.rs`, `control_flow/mod.rs`) unpack.
            if let Expression::Identifier(id) = receiver.as_ref() {
                if self.tier2_block_params.contains(id.name.as_str())
                    || self.tier2_local_vars.contains(id.name.as_str())
                {
                    return self.generate_block_value_call_stateful(receiver, arguments);
                }
            }
        }
        // BT-2808: `blk value: x; value: y` — proved safe by `is_tier2_value_call`'s
        // Cascade branch. Unlike the single-send case, `expression_doc` has no
        // generic Tier 2 cascade handling to fall through to, so this must be
        // generated directly regardless of receiver kind.
        if let Expression::Cascade {
            receiver, messages, ..
        } = expr
        {
            let (underlying_receiver, all_messages) = Self::normalize_cascade(receiver, messages);
            return self.generate_tier2_cascade_doc(underlying_receiver, &all_messages);
        }
        self.expression_doc(expr)
    }

    /// BT-2808: Generates a sequential Tier 2 tuple-unpacking cascade
    /// (`blk value: x; value: y`), reached only for a `Cascade` expression that
    /// `is_tier2_value_call` already proved is entirely safe `value`-family
    /// sends on a receiver that may hold a Tier 2 block.
    ///
    /// The receiver is evaluated once per message rather than hoisted into a
    /// single shared binding — harmless here since a `tier2_block_params`/
    /// `tier2_local_vars`/`self.field` receiver is always a pure variable or
    /// field read, never an expression with side effects. Each message is
    /// generated with the *current* threaded state (via
    /// `generate_block_value_call_stateful`/`generate_block_value_call_runtime_discriminated`,
    /// both of which read `self.current_state_var()` internally), then its
    /// returned `{Result, NewState}` tuple is unpacked and `NewState` becomes
    /// the current state for the next message — mirroring the sequencing the
    /// bare (non-cascade) `Tier2ValueCall`/`LocalAssignTier2` call sites already
    /// use between statements.
    ///
    /// Matching ordinary (non-Tier-2) cascade semantics, the overall value is
    /// the result of the LAST message. Returns a `{Result, NewState}` tuple
    /// with that contract — callers unpack it exactly like a single
    /// `Tier2ValueCall` (see `generate_tier2_value_call_doc`'s own callers).
    fn generate_tier2_cascade_doc(
        &mut self,
        receiver: &Expression,
        all_messages: &[(&MessageSelector, &[Expression])],
    ) -> Result<Document<'static>> {
        let use_runtime_discrimination =
            self.context == CodeGenContext::Actor && Self::is_self_field_access(receiver);

        let mut parts: Vec<Document<'static>> = Vec::with_capacity(all_messages.len() + 1);
        let mut result_var: Option<String> = None;
        let mut state_var: Option<String> = None;
        for (selector, args) in all_messages {
            let call_doc = if use_runtime_discrimination {
                self.generate_block_value_call_runtime_discriminated(
                    receiver,
                    args,
                    &selector.name(),
                )?
            } else {
                self.generate_block_value_call_stateful(receiver, args)?
            };
            let tuple_var = self.fresh_temp_var("CascTuple");
            let this_result = self.fresh_temp_var("CascResult");
            let this_state = self.next_state_var();
            parts.push(docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                call_doc,
                " in let ",
                leaf::var(this_result.clone()),
                " = call 'erlang':'element'(1, ",
                leaf::var(tuple_var.clone()),
                ") in let ",
                leaf::var(this_state.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ") in ",
            ]);
            result_var = Some(this_result);
            state_var = Some(this_state);
        }
        // is_tier2_value_call requires a non-empty message list before ever
        // routing here, so both are always populated by the loop above.
        let result_var = result_var.expect("BT-2808: cascade must have at least one message");
        let state_var = state_var.expect("BT-2808: cascade must have at least one message");
        parts.push(docvec![
            "{",
            leaf::var(result_var),
            ", ",
            leaf::var(state_var),
            "}"
        ]);
        Ok(Document::Vec(parts))
    }

    /// Checks if a control flow expression actually threads state through mutations.
    ///
    /// This goes beyond mere selector-based classification by analysing whether
    /// the block argument(s) contain mutations that require state threading.
    ///
    /// Returns `true` only if:
    /// 1. The expression is a `ControlFlow` dispatch (from pre-computed `dispatch_kinds`),
    ///    or — when semantic facts are unavailable — the selector matches a known
    ///    exception/conditional selector as a fallback.
    /// 2. The relevant block argument(s) need state threading in the current context
    ///    (checked via `needs_mutation_threading` on pre-computed `block_profiles`).
    ///
    /// Using pre-computed `dispatch_kinds` and `block_profiles` avoids the repeated
    /// selector-based re-classification and `analyze_block` calls that the original
    /// implementation performed (BT-1309).
    pub(in crate::core_erlang) fn control_flow_has_mutations(&self, expr: &Expression) -> bool {
        // BT-2355: see through parentheses so `_r := (1 to: 5 do: [...])` is still
        // classified as control flow with mutations (and thus unpacked + threaded)
        // rather than falling through to a plain pure local assignment.
        let expr = expr.unwrap_parens();

        // BT-2880: `match:` is a dedicated `Expression::Match` node, not a
        // `MessageSend`, so it's otherwise invisible to this classifier — a
        // `match:` arm body that is a state-mutating `[...] value` block would
        // fall through to `BodyExprKind::Pure` and leak its raw `{Result,
        // NewState}` tuple as the match's value. `generate_match` threads state
        // through every arm (see `match_needs_mutation_threading`) whenever any
        // arm needs it, so route it through the same tuple-unwrap machinery as
        // `ifTrue:`/`ifFalse:`.
        if let Expression::Match { arms, .. } = expr {
            return self.match_needs_mutation_threading(arms);
        }

        let Expression::MessageSend {
            receiver,
            arguments,
            selector: beamtalk_core::ast::MessageSelector::Keyword(parts),
            span,
            ..
        } = expr
        else {
            return false;
        };

        // Use pre-computed dispatch classification instead of re-deriving it.
        // When semantic_facts is empty (e.g. in unit tests constructed via
        // `CoreErlangGenerator::new`), `dispatch_kind` returns `Unknown`.
        // In that case fall back to local selector-based classification so the
        // function still returns the correct result for known control-flow
        // selectors rather than silently returning `false` for all of them.
        let dispatch_kind = self.semantic_facts.dispatch_kind(span);
        let sel_str: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let is_control_flow = match dispatch_kind {
            beamtalk_core::semantic_analysis::DispatchKind::ControlFlow => true,
            beamtalk_core::semantic_analysis::DispatchKind::Unknown => {
                beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str())
                    || beamtalk_core::state_threading_selectors::is_conditional_selector(
                        sel_str.as_str(),
                    )
            }
            _ => false,
        };
        if !is_control_flow {
            return false;
        }

        // ADR 0118 phase 3 (BT-3419): `whileTrue:`/`whileFalse:`'s RECEIVER
        // is the condition block — like `ensure:`/`on:do:`'s try-body
        // receiver just below, and the conditional-selector receiver
        // further down, it may itself have state effects (a self-send, or
        // an `and:`/`or:` that carries one) even when the BODY argument has
        // none, and neither `is_exception_selector` nor
        // `is_conditional_selector` cover it — the "standard check" below
        // only ever walks `arguments` (the body), never `receiver`. Shares
        // `while_loops.rs`'s own gate (`condition_has_state_effects`)
        // rather than re-deriving it, so the two decisions — "does this
        // statement need `ControlFlowWithMutations` classification" here,
        // "does this loop's own condition need threading" there — cannot
        // disagree.
        if matches!(sel_str.as_str(), "whileTrue:" | "whileFalse:")
            && super::super::control_flow::condition_has_state_effects(receiver)
        {
            return true;
        }

        // BT-410: For on:do: and ensure:, the receiver (try body) is also
        // a block that may contain field mutations.
        if beamtalk_core::state_threading_selectors::is_exception_selector(sel_str.as_str()) {
            if let Expression::Block(block) = receiver.as_ref() {
                // BT-3173: also covers a nested list-op/counted-loop inside the
                // try body mutating an outer local even when the try body's own
                // top-level analysis sees no direct mutation (`analyze_block`
                // does not propagate writes out of a nested, non-conditional
                // block — same gap BT-2356/BT-1329 already close for
                // conditionals and ordinary block arguments below). Without
                // this, `[nested-loop] ensure: [...]`/`on:do:` would be
                // classified as pure here even though the nested loop's own
                // cross-scope collector (this call site's sibling,
                // `compute_threaded_locals_for_loop`) correctly detects the
                // mutation — the same "two decision points disagree" shape as
                // the do:/collect: self-classification gap this issue fixes.
                if self.block_arg_needs_threading(block) {
                    return true;
                }
            }
        }

        // BT-915: For Boolean conditionals, any block argument may contain mutations.
        // BT-1226: ifNotNil: also needs per-block mutation detection.
        if beamtalk_core::state_threading_selectors::is_conditional_selector(sel_str.as_str()) {
            // BT-3382: the conditional's own RECEIVER may be an actor
            // self-send (`(self recordOnce: x) ifTrue:ifFalse:`) whose own
            // state mutation must be threaded, even when neither block
            // argument mutates anything itself. Must stay in sync with
            // `intrinsics.rs`'s `try_generate_boolean_protocol`'s matching
            // check — the two are independently-computed decisions that
            // must agree (see this file's own commentary on that class of
            // bug, e.g. BT-2356's "two decision points disagree" note).
            // BT-3396: widened to any self-send needing threading in the
            // receiver's sub-tree (`((self recordOnce: x) and: [y])
            // ifTrue: [...]`) — the same probe `compile_conditional_receiver`
            // threads ahead with, so the two cannot disagree.
            if self.conditional_receiver_needs_threading(receiver) {
                return true;
            }
            for arg in arguments {
                // BT-2356: `block_arg_needs_threading` also catches a nested list
                // op inside a branch mutating an outer local even when the
                // branch block itself has no direct mutation (`analyze_block`
                // does not propagate writes out of nested blocks) — e.g.
                // `flag ifTrue: [ items do: [:x | sum := sum + x] ]`.
                if let Expression::Block(block) = arg {
                    if self.block_arg_needs_threading(block) {
                        return true;
                    }
                }
            }
            return false;
        }

        // Standard check: analyse argument blocks for mutations.
        // BT-1486: Check ALL block arguments, not just the last one.
        // For selectors like `detect:ifNone:`, the mutation-bearing block is the
        // first argument (predicate), not the last (ifNone handler).
        // BT-1329: `block_arg_needs_threading` also catches nested list ops with
        // cross-scope mutations that `analyze_block` alone can't see.
        for arg in arguments {
            if let Expression::Block(block) = arg {
                if self.block_arg_needs_threading(block) {
                    return true;
                }
            }
        }

        false
    }

    /// Generates the `__beamtalk_meta/0` function (BT-942).
    ///
    /// Embeds static reflection metadata directly in the compiled BEAM module.
    /// This enables zero-process reflection queries for structural data:
    /// class name, superclass, fields, instance methods, and class methods.
    ///
    /// Dynamic classes created via `beamtalk_class_builder` do not have this function;
    /// the runtime falls back to `gen_server` calls when `erlang:function_exported/3` (BIF)
    /// returns false.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// '__beamtalk_meta'/0 = fun () ->
    ///     ~{'class' => 'Counter',
    ///       'superclass' => 'Actor',
    ///       'fields' => ['value'],
    ///       'class_fields' => ['total'],
    ///       'methods' => [{'increment', 0}, {'decrement', 0}, {'getValue', 0}],
    ///       'class_methods' => [{'new', 0}]
    ///     }~
    /// ```
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_meta_function(
        &self,
        module: &Module,
        synthesize_supervision_spec: bool,
    ) -> Result<Document<'static>> {
        let Some(class) = module.classes.first() else {
            return Ok(Document::Nil);
        };

        // ADR 0070 Phase 4: Extract package name from BEAM module name (bt@{package}@{class})
        let package_name = extract_package_from_module_name(&self.module_name);

        Ok(docvec![
            "'__beamtalk_meta'/0 = fun () ->\n",
            "    ",
            // include_standalone: false — standalone methods are runtime-patched, not static
            Self::build_meta_map_doc(
                class,
                module,
                false,
                synthesize_supervision_spec,
                package_name.as_deref(),
                self.meta_provenance(),
            ),
            "\n\n",
        ])
    }

    /// Builds the Core Erlang map document for the static class metadata.
    ///
    /// Used by both `generate_meta_function` (for `__beamtalk_meta/0`) and
    /// `generate_register_class` (for the `'meta'` key in `BuilderState`).
    ///
    /// ADR 0050 Phase 5: `erlang:function_exported/3` returns `false` during `on_load`,
    /// so `__beamtalk_meta/0` cannot be called from within the `on_load` callback chain.
    /// Including this map literal in `BuilderState` makes the data available during `init/1`.
    ///
    /// When `include_standalone` is `false` (used for `__beamtalk_meta/0`), standalone
    /// Tonel-style methods (`module.method_definitions`) are excluded — they are
    /// runtime-patched and deliberately absent from the static meta. When `true`
    /// (used for `BuilderState.meta`), standalone methods are included so that
    /// return-type information is available to `init/1` during `on_load`.
    pub(super) fn build_meta_map_doc(
        class: &ClassDefinition,
        module: &Module,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
        package_name: Option<&str>,
        provenance: MetaProvenance<'_>,
    ) -> Document<'static> {
        Self::build_meta_map_doc_with_extra(
            class,
            module,
            include_standalone,
            synthesize_supervision_spec,
            Document::Nil,
            package_name,
            provenance,
        )
    }

    /// Like `build_meta_map_doc` but appends extra map entries before closing the map.
    ///
    /// Used by native facade codegen to add `'native'` and `'backing_module'` keys
    /// while reusing the standard meta map structure.
    #[allow(clippy::too_many_lines)] // one contiguous map literal; splitting hurts readability
    pub(super) fn build_meta_map_doc_with_extra(
        class: &ClassDefinition,
        module: &Module,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
        extra_entries: Document<'static>,
        package_name: Option<&str>,
        provenance: MetaProvenance<'_>,
    ) -> Document<'static> {
        let class_name = class.name.name.to_string();
        let superclass_name = class
            .superclass
            .as_ref()
            .map_or_else(|| "nil".to_string(), |s| s.name.to_string());

        // Build fields list from instance state declarations
        let fields: Vec<String> = class
            .state
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();

        let fields_doc = Self::meta_atom_list(&fields);

        // BT-2238: Build class-side field list from `classState:` declarations so
        // class-side slots are reflectable (`Behaviour>>classVarNames` /
        // `allClassVarNames`). The instance `fields` key above carries instance
        // state only.
        let class_fields: Vec<String> = class
            .class_variables
            .iter()
            .map(|s| s.name.name.to_string())
            .collect();

        let class_fields_doc = Self::meta_atom_list(&class_fields);

        // Boolean flags
        let is_sealed_doc = Self::meta_bool(class.is_sealed);
        let is_abstract_doc = Self::meta_bool(class.is_abstract);
        let is_value_doc = Self::meta_bool(class.class_kind == ClassKind::Value);
        let is_typed_doc = Self::meta_bool(class.is_typed);
        let is_internal_doc = Self::meta_bool(class.is_internal);

        // ADR 0071 Phase 4: Emit class-level visibility
        let visibility_doc: Document<'static> = if class.is_internal {
            Document::Str("'internal'")
        } else {
            Document::Str("'public'")
        };

        // field_types: map of field name → declared type atom or 'none'
        let field_types_doc = Self::meta_field_types_map(&class.state);

        // BT-1976: field_has_default — map of field name → 'true' | 'false'.
        // Cross-file consumers use this to identify typed-no-default fields
        // without the AST (post-initialize validation in gen_server codegen).
        let field_has_default_doc = Self::meta_field_has_default_map(&class.state);

        // Compute auto-slot methods once and share across method_info / class_method_info
        let auto = crate::core_erlang::value_type_codegen::compute_auto_slot_methods(class);
        let method_info_doc = Self::meta_method_info_map(&Self::meta_instance_method_entries(
            class,
            module,
            auto.as_ref(),
            include_standalone,
        ));
        let class_method_info_doc = Self::meta_method_info_map(&Self::meta_class_method_entries(
            class,
            module,
            auto.as_ref(),
            include_standalone,
            synthesize_supervision_spec,
        ));

        // ADR 0068: Emit type_params list for generic classes
        let type_params_doc = Self::meta_atom_list(
            &class
                .type_params
                .iter()
                .map(|tp| tp.name.name.to_string())
                .collect::<Vec<_>>(),
        );

        // ADR 0070 Phase 4: Emit package name as compile-time constant
        let package_doc: Document<'static> = match package_name {
            Some(pkg) => leaf::atom(pkg.to_string()),
            None => Document::Str("'none'"),
        };

        // ADR 0070 Phase 4: Emit ClassKind as atom (object | value | actor)
        let kind_doc: Document<'static> = match class.class_kind {
            ClassKind::Object => Document::Str("'object'"),
            ClassKind::Value => Document::Str("'value'"),
            ClassKind::Actor => Document::Str("'actor'"),
        };

        // ADR 0103: emit the declared sendability handle scope as an atom, only
        // when present — keeps meta output stable for the vast majority of
        // classes that declare none (mirrors the provenance keys' pattern).
        let handle_scope_doc: Document<'static> = match &class.handle_scope {
            Some(sym) => docvec![
                ",\n      'handle_scope' => ",
                leaf::atom(sym.name.to_string()),
            ],
            None => Document::Nil,
        };

        docvec![
            "~{'class' => ",
            leaf::atom(class_name),
            ",\n      'superclass' => ",
            leaf::atom(superclass_name),
            ",\n      'package' => ",
            package_doc,
            ",\n      'kind' => ",
            kind_doc,
            ",\n      'fields' => ",
            fields_doc,
            ",\n      'class_fields' => ",
            class_fields_doc,
            ",\n      'is_sealed' => ",
            is_sealed_doc,
            ",\n      'is_abstract' => ",
            is_abstract_doc,
            ",\n      'is_value' => ",
            is_value_doc,
            ",\n      'is_typed' => ",
            is_typed_doc,
            ",\n      'is_internal' => ",
            is_internal_doc,
            ",\n      'visibility' => ",
            visibility_doc,
            ",\n      'type_params' => ",
            type_params_doc,
            ",\n      'field_types' => ",
            field_types_doc,
            ",\n      'field_has_default' => ",
            field_has_default_doc,
            ",\n      'method_info' => ",
            method_info_doc,
            ",\n      'class_method_info' => ",
            class_method_info_doc,
            // ADR 0103: sendability handle scope (omitted when undeclared).
            handle_scope_doc,
            // ADR 0098 Phase 3: producing-toolchain identity (omitted when unknown).
            Self::meta_provenance_entries(provenance),
            extra_entries,
            "\n    }~",
        ]
    }

    /// ADR 0098 Phase 3: emit the `beamtalk_version` / `otp_release` provenance
    /// keys for `__beamtalk_meta`, as binary string literals.
    ///
    /// Each key is emitted only when known: an older toolchain (and REPL/test
    /// codegen) leaves them absent, which `__beamtalk_meta` readers treat as a
    /// provenance miss (stale → recompile), never an error. Both values are
    /// compile-time literals from the CLI — never a runtime `erlang:system_info/1`
    /// call, which would bake the bare OTP release rather than the compound key.
    fn meta_provenance_entries(provenance: MetaProvenance<'_>) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        if let Some(version) = provenance.beamtalk_version {
            parts.push(Document::Str(",\n      'beamtalk_version' => "));
            parts.push(leaf::binary_lit(version));
        }
        if let Some(otp_release) = provenance.otp_release {
            parts.push(Document::Str(",\n      'otp_release' => "));
            parts.push(leaf::binary_lit(otp_release));
        }
        Document::Vec(parts)
    }

    /// Builds a Core Erlang atom list document from a slice of string names.
    ///
    /// Example: `["field1", "field2"]` → `['field1', 'field2']`
    /// Empty slice → `[]`
    pub(super) fn meta_atom_list(names: &[String]) -> Document<'static> {
        if names.is_empty() {
            return Document::Str("[]");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("["));
        for (i, name) in names.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(leaf::atom(name.clone()));
        }
        parts.push(Document::Str("]"));
        Document::Vec(parts)
    }

    /// Produces the Core Erlang atom for a boolean value.
    pub(super) fn meta_bool(b: bool) -> Document<'static> {
        if b {
            Document::Str("'true'")
        } else {
            Document::Str("'false'")
        }
    }

    /// BT-1976: Builds a field-has-default map for `__beamtalk_meta/0`.
    ///
    /// Example: `[StateDecl{name: "count", default: Some(0)}]` → `~{'count' => 'true'}~`
    /// Empty slice → `~{}~`
    ///
    /// Cross-file consumers read this to identify typed-no-default fields when
    /// the class's AST is not in the current compilation unit.
    pub(super) fn meta_field_has_default_map(state: &[StateDeclaration]) -> Document<'static> {
        if state.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, s) in state.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let flag = if s.default_value.is_some() {
                Document::Str("'true'")
            } else {
                Document::Str("'false'")
            };
            parts.push(docvec![leaf::atom(s.name.name.to_string()), " => ", flag,]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }

    /// Builds a Core Erlang map of field name → declared type atom or `'none'`.
    ///
    /// Example: `[StateDecl{name: "value", type: Integer}]` → `~{'value' => 'Integer'}~`
    /// Empty slice → `~{}~`
    pub(super) fn meta_field_types_map(state: &[StateDeclaration]) -> Document<'static> {
        if state.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, s) in state.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let type_doc = match &s.type_annotation {
                Some(ta) => leaf::atom(ta.type_name().to_string()),
                None => Document::Str("'none'"),
            };
            parts.push(docvec![
                leaf::atom(s.name.name.to_string()),
                " => ",
                type_doc,
            ]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }

    pub(super) fn meta_instance_method_entries(
        class: &ClassDefinition,
        module: &Module,
        auto: Option<&crate::core_erlang::value_type_codegen::AutoSlotMethods>,
        include_standalone: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let type_params = &class.type_params;
        let mut entries: Vec<MethodInfoEntry> = class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, type_params))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && !m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(&standalone.method, type_params));
            }
        }
        if let Some(auto) = auto {
            use crate::core_erlang::value_type_codegen::AutoSlotMethods;
            for field in &auto.getters {
                entries.push((field.clone(), 0, MetaTypeRepr::None, vec![], sealed, false));
            }
            for field in &auto.setters {
                entries.push((
                    AutoSlotMethods::with_star_selector(field),
                    1,
                    MetaTypeRepr::None,
                    vec![MetaTypeRepr::None],
                    sealed,
                    false,
                ));
            }
        }
        entries
    }

    /// Collects `MethodInfoEntry` tuples for all primary class methods of `class`,
    /// including the auto-generated keyword constructor for Value subclasses.
    pub(super) fn meta_class_method_entries(
        class: &ClassDefinition,
        module: &Module,
        auto: Option<&crate::core_erlang::value_type_codegen::AutoSlotMethods>,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let type_params = &class.type_params;
        let mut entries: Vec<MethodInfoEntry> = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, type_params))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(&standalone.method, type_params));
            }
        }
        if let Some(auto) = auto {
            if let Some(kw_sel) = &auto.keyword_constructor {
                let arity = class.state.len();
                // BT-1408: Hash long keyword constructor selectors to stay within
                // Erlang's 255-char atom limit. The meta selector must match what
                // class_send emits so runtime dispatch finds the method.
                let safe_sel =
                    crate::core_erlang::selector_mangler::safe_class_method_selector(kw_sel);
                entries.push((
                    safe_sel,
                    arity,
                    MetaTypeRepr::None,
                    vec![MetaTypeRepr::None; arity],
                    sealed,
                    false,
                ));
            }
        }
        // BT-1218: Register the synthesized supervisionSpec so class dispatch finds it locally
        // rather than walking the chain to Actor's version (which always returns #temporary).
        if synthesize_supervision_spec {
            entries.push((
                "supervisionSpec".to_string(),
                0,
                MetaTypeRepr::Atom("SupervisionSpec".to_string()),
                vec![],
                sealed,
                false,
            ));
        }
        entries
    }

    /// Converts a `MethodDefinition` into a `MethodInfoEntry`.
    ///
    /// ADR 0068: When `class_type_params` is non-empty, type annotations that reference
    /// a class-level type parameter emit `MetaTypeRepr::TypeParam` instead of a flat atom.
    ///
    /// BT-3367: the serialized `is_sealed` bit is `m.is_sealed` alone — the same
    /// per-method flag `ClassInfo::from_class_definition` (`semantic_analysis/
    /// class_hierarchy/class_info.rs`) records for a fresh-AST compile, with no OR
    /// against the class-level `sealed` flag. `is_sealed` here means two different
    /// things to two different readers: `compute_direct_call_eligible`'s Gate 5
    /// (`codegen/core_erlang/mod.rs`) treats it as "this method body never references
    /// self/its own class for construction, so it's safe to call directly with a
    /// literal `nil` `ClassSelf`" — true only for a method individually declared
    /// `class sealed`, per that gate's own doc comment. A class being sealed only
    /// means "cannot be subclassed" (`can_be_subclassed`/`check_sealed_superclass`);
    /// it says nothing about whether an *individual* method's body is self-free.
    /// OR'ing in `class_is_sealed` previously made this producer disagree with the
    /// fresh-AST one — a class-side factory method of a sealed class (e.g. `class
    /// ok: a details: b => Self checkName: a details: b`) that itself constructs a
    /// new instance would round-trip through this BEAM-metadata path (the one the
    /// REPL uses to see an already-loaded project class) as `is_sealed = true` even
    /// though the method was never individually sealed, wrongly clearing Gate 5 for
    /// it. The generated direct call then hard-codes `ClassSelf = 'nil'`
    /// (`generate_direct_class_method_call`, `dispatch_codegen.rs`), and the method's
    /// own `self`/class-name construction dereferences that `nil` as a tuple —
    /// `erlang:element(2, 'nil')` — raising exactly the reported `badarg` ("invalid
    /// argument"). See BT-3367.
    fn meta_method_entry(
        m: &MethodDefinition,
        class_type_params: &[TypeParamDecl],
    ) -> MethodInfoEntry {
        let return_type = m.return_type.as_ref().map_or(MetaTypeRepr::None, |rt| {
            Self::type_annotation_to_meta_repr(rt, class_type_params)
        });
        let param_types: Vec<MetaTypeRepr> = m
            .parameters
            .iter()
            .map(|p| {
                p.type_annotation.as_ref().map_or(MetaTypeRepr::None, |ta| {
                    Self::type_annotation_to_meta_repr(ta, class_type_params)
                })
            })
            .collect();
        (
            m.selector.name().to_string(),
            m.selector.arity(),
            return_type,
            param_types,
            m.is_sealed,
            m.is_internal,
        )
    }

    /// Converts a `TypeAnnotation` into a `MetaTypeRepr`.
    ///
    /// Thin wrapper (BT-3076) around [`Self::declared_type_to_meta_repr`] —
    /// converts to the span-free [`DeclaredType`] first and delegates, so the
    /// AST and the structured `MethodInfo`/generator paths share one
    /// conversion. See that function's doc for the per-variant rules.
    fn type_annotation_to_meta_repr(
        ta: &beamtalk_core::ast::TypeAnnotation,
        class_type_params: &[TypeParamDecl],
    ) -> MetaTypeRepr {
        Self::declared_type_to_meta_repr(&DeclaredType::from(ta), class_type_params)
    }

    /// Converts a [`DeclaredType`] into a `MetaTypeRepr` (BT-3076).
    ///
    /// ADR 0068: If a bare `Simple` name matches one of the class-level type
    /// parameters, it becomes a `TypeParam { name, index }`. A single
    /// uppercase-letter `Simple` name not among `class_type_params` becomes a
    /// method-local `TypeParam` (index `-1`). `Generic` types with
    /// parameters become `Generic { base, parameters }`, recursively.
    ///
    /// BT-3076: `Union` and `Singleton` now convert structurally too
    /// (`MetaTypeRepr::Union` / `MetaTypeRepr::Singleton`), rather than
    /// degrading to a flat atom of the rendered string — the wire-format
    /// extension this stage adds. `FalseOr`, `Difference`, `Intersection`,
    /// `SelfType`, `SelfClass`, and `ClassOf` are rare in method signatures
    /// and still fall back to a flat `Atom` of the rendered string (old
    /// readers of a new artifact degrade gracefully; the format is internal
    /// — see this module's `MetaTypeRepr` doc). The self-type renderings
    /// (`'Self'`, `'Self class'`, `'<Name> class'`) are recognised by
    /// `DeclaredType::parse` on the reader side, so they round-trip
    /// structurally despite the flat encoding (compiler-port's
    /// `self_type_return_survives_etf_meta`).
    fn declared_type_to_meta_repr(
        dt: &DeclaredType,
        class_type_params: &[TypeParamDecl],
    ) -> MetaTypeRepr {
        match dt {
            DeclaredType::Simple(name) => {
                // Check if this is a class-level type parameter
                if let Some(index) = class_type_params
                    .iter()
                    .position(|tp| tp.name.name == *name)
                {
                    MetaTypeRepr::TypeParam {
                        name: name.to_string(),
                        index: i32::try_from(index).unwrap_or(0),
                    }
                } else if name.len() == 1
                    && name.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                {
                    // Single uppercase letter not in class type params → method-local type param
                    MetaTypeRepr::TypeParam {
                        name: name.to_string(),
                        index: -1,
                    }
                } else {
                    MetaTypeRepr::Atom(name.to_string())
                }
            }
            DeclaredType::Generic { base, parameters } => {
                let params: Vec<MetaTypeRepr> = parameters
                    .iter()
                    .map(|p| Self::declared_type_to_meta_repr(p, class_type_params))
                    .collect();
                MetaTypeRepr::Generic {
                    base: base.to_string(),
                    parameters: params,
                }
            }
            DeclaredType::Union(members) => MetaTypeRepr::Union(
                members
                    .iter()
                    .map(|m| Self::declared_type_to_meta_repr(m, class_type_params))
                    .collect(),
            ),
            DeclaredType::Singleton(name) => MetaTypeRepr::Singleton(name.to_string()),
            // FalseOr, Difference, Intersection, SelfType, SelfClass, ClassOf
            // → fall back to flat atom string (see doc above).
            _ => MetaTypeRepr::Atom(dt.to_string()),
        }
    }

    /// Renders a `MetaTypeRepr` as a Core Erlang document.
    ///
    /// - `None` → `'none'`
    /// - `Atom("T")` → `'T'`
    /// - `TypeParam { name: "T", index: 0 }` → `{'type_param', 'T', 0}`
    /// - `Generic { base: "Result", params: [TypeParam T, Atom E] }` →
    ///   `{'generic', 'Result', [{'type_param', 'T', 0}, 'E']}`
    /// - `Union([Atom A, Atom B])` → `{'union', ['A', 'B']}` (BT-3076)
    /// - `Singleton("north")` → `{'singleton', 'north'}` (BT-3076)
    pub(super) fn meta_type_repr_doc(repr: &MetaTypeRepr) -> Document<'static> {
        match repr {
            MetaTypeRepr::None => Document::Str("'none'"),
            MetaTypeRepr::Atom(name) => leaf::atom(name.clone()),
            MetaTypeRepr::TypeParam { name, index } => docvec![
                "{'type_param', ",
                leaf::atom(name.clone()),
                ", ",
                leaf::int_lit(i64::from(*index)),
                "}"
            ],
            MetaTypeRepr::Generic { base, parameters } => {
                docvec![
                    "{'generic', ",
                    leaf::atom(base.clone()),
                    ", ",
                    Self::meta_type_repr_list_doc(parameters),
                    "}"
                ]
            }
            MetaTypeRepr::Union(members) => {
                docvec!["{'union', ", Self::meta_type_repr_list_doc(members), "}"]
            }
            MetaTypeRepr::Singleton(name) => {
                docvec!["{'singleton', ", leaf::atom(name.clone()), "}"]
            }
        }
    }

    /// Renders a `[MetaTypeRepr, ...]` Core Erlang list — the shared
    /// bracket/comma-join helper `Generic` and `Union` (BT-3076) both use.
    fn meta_type_repr_list_doc(items: &[MetaTypeRepr]) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("["));
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            parts.push(Self::meta_type_repr_doc(item));
        }
        parts.push(Document::Str("]"));
        Document::Vec(parts)
    }

    /// Builds a Core Erlang map of selector → method info map.
    ///
    /// Each entry: `'selector' => ~{'arity' => N, 'param_types' => [...], 'return_type' => ...}~`
    /// Empty slice → `~{}~`
    ///
    /// ADR 0068: `return_type` and `param_types` can now be tagged tuples for generic types.
    pub(super) fn meta_method_info_map(methods: &[MethodInfoEntry]) -> Document<'static> {
        if methods.is_empty() {
            return Document::Str("~{}~");
        }
        let mut parts: Vec<Document<'static>> = Vec::new();
        parts.push(Document::Str("~{"));
        for (i, (sel, arity, return_type, param_types, is_sealed, is_internal)) in
            methods.iter().enumerate()
        {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let param_types_doc = if param_types.is_empty() {
                Document::Str("[]")
            } else {
                let mut pts: Vec<Document<'static>> = Vec::new();
                pts.push(Document::Str("["));
                for (j, pt) in param_types.iter().enumerate() {
                    if j > 0 {
                        pts.push(Document::Str(", "));
                    }
                    pts.push(Self::meta_type_repr_doc(pt));
                }
                pts.push(Document::Str("]"));
                Document::Vec(pts)
            };
            let return_type_doc = Self::meta_type_repr_doc(return_type);
            let is_sealed_doc: Document<'static> = Self::meta_bool(*is_sealed);
            let visibility_doc: Document<'static> = if *is_internal {
                Document::Str("'internal'")
            } else {
                Document::Str("'public'")
            };
            parts.push(docvec![
                leaf::atom(sel.clone()),
                " => ~{'arity' => ",
                leaf::int_lit(i64::try_from(*arity).unwrap_or(0)),
                ", 'param_types' => ",
                param_types_doc,
                ", 'return_type' => ",
                return_type_doc,
                ", 'is_sealed' => ",
                is_sealed_doc,
                ", 'visibility' => ",
                visibility_doc,
                "}~",
            ]);
        }
        parts.push(Document::Str("}~"));
        Document::Vec(parts)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        DeclaredType, MetaProvenance, MetaTypeRepr, RecvType, extract_package_from_module_name,
        project_recv_type,
    };
    use crate::core_erlang::CoreErlangGenerator;
    use beamtalk_core::ast::{
        ClassDefinition, ClassKind, Expression, ExpressionStatement, Identifier, KeywordPart,
        Literal, MessageSelector, MethodDefinition, Module, ParameterDefinition, TypeAnnotation,
        TypeParamDecl,
    };
    use beamtalk_core::semantic_analysis::{
        DynamicReason, InferredType, MethodReturnKey, TypeProvenance,
    };
    use beamtalk_core::source_analysis::Span;
    use beamtalk_core::test_helpers::test_support::make_actor_class;
    use ecow::EcoString;

    fn s() -> Span {
        Span::new(0, 0)
    }

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    fn simple_unary_method(selector: &str) -> MethodDefinition {
        MethodDefinition::new(
            MessageSelector::Unary(selector.into()),
            vec![],
            vec![bare(Expression::Literal(Literal::Integer(42), s()))],
            s(),
        )
    }

    // -----------------------------------------------------------------------
    // BT-3249: `extract_method_source` must not leak an inference-written
    // `-> Type` return-type annotation into the image-resident `__source__`
    // text, while still round-tripping a genuine user-written annotation
    // untouched. See `clear_return_type_writeback_for_key`'s doc for the
    // full root-cause story (ChangeLog's canonical `source_ref` is unparsed
    // pre-writeback; without this, browsed source was unparsed
    // post-writeback, so a save -> revert -> re-save of an unchanged buffer
    // recorded a spurious annotation-only ChangeLog diff).
    // -----------------------------------------------------------------------

    #[test]
    fn extract_method_source_strips_inferred_return_type_annotation() {
        let mut method = simple_unary_method("greeting");
        method.return_type = Some(TypeAnnotation::simple(EcoString::from("Hello"), s()));

        let mut generator = CoreErlangGenerator::new("test");
        let key: MethodReturnKey = (EcoString::from("Hello"), EcoString::from("greeting"), false);
        generator.method_return_types_written_back.insert(
            key,
            InferredType::Known {
                class_name: EcoString::from("Hello"),
                type_args: vec![],
                provenance: TypeProvenance::Inferred(s()),
            },
        );

        let source = generator.extract_method_source("Hello", false, &method);
        assert!(
            !source.contains("->"),
            "inferred return-type annotation leaked into extracted source: {source:?}"
        );
        // `method`'s own AST is left untouched — codegen elsewhere (specs,
        // `method_return_types` metadata) still needs the inferred type.
        assert!(
            method.return_type.is_some(),
            "extract_method_source must not mutate the method it was given"
        );
    }

    #[test]
    fn extract_method_source_preserves_explicit_return_type_annotation() {
        let mut method = simple_unary_method("greeting");
        method.return_type = Some(TypeAnnotation::simple(EcoString::from("Hello"), s()));

        // No entry in `method_return_types_written_back`: this key was never
        // written by inference, so the annotation is the user's own.
        let generator = CoreErlangGenerator::new("test");
        let source = generator.extract_method_source("Hello", false, &method);
        assert!(
            source.contains("-> Hello"),
            "explicit user-written return-type annotation was stripped: {source:?}"
        );
    }

    // -----------------------------------------------------------------------
    // BT-3217 (ADR 0115 Phase 2): `project_recv_type` unit coverage.
    //
    // The codegen fixture matrix (`codegen/core_erlang/tests/recv_type.rs`)
    // exercises this rule end-to-end through real `.bt` source for every case
    // reachable from actual inference (typed/protocol/dynamic/union/native/
    // alias locals, `Meta{C}`, self-send, FFI receiver). `Intersection` and
    // `Negation` are not reachable that way without substantially more
    // fixture machinery (ADR 0068 protocol composition, ADR 0102 negation
    // narrowing) for a result this rule treats identically to `Union` —
    // tested directly here instead, alongside every other variant, as a
    // complete case-by-case pin of the write-path projection rule.
    // -----------------------------------------------------------------------

    fn known(class_name: &str, provenance: TypeProvenance) -> InferredType {
        InferredType::Known {
            class_name: class_name.into(),
            type_args: vec![],
            provenance,
        }
    }

    #[test]
    fn project_recv_type_known_declared_yields_name() {
        let ty = known("Counter", TypeProvenance::Declared(s()));
        assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Counter"));
    }

    #[test]
    fn project_recv_type_known_inferred_yields_name() {
        let ty = known("Counter", TypeProvenance::Inferred(s()));
        assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Counter"));
    }

    #[test]
    fn project_recv_type_known_substituted_yields_name() {
        let ty = known("Counter", TypeProvenance::Substituted(s()));
        assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Counter"));
    }

    #[test]
    fn project_recv_type_known_with_type_args_drops_them() {
        // `Collection(Integer)` still keys `recv_type: 'Collection'` — the
        // generic parameter doesn't change which class/protocol a reader
        // needs to reason about (ADR 0115 §Write path).
        let ty = InferredType::Known {
            class_name: "Collection".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(project_recv_type(&ty), RecvType::Name(n) if n == "Collection"));
    }

    #[test]
    fn project_recv_type_known_extracted_native_type_coarsens_to_dynamic() {
        // ADR 0075 native/FFI type name — no `beamtalk_class_metadata` row.
        let ty = known("List", TypeProvenance::Extracted);
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn project_recv_type_known_aliased_coarsens_to_dynamic() {
        // ADR 0108 alias display name — no `beamtalk_class_metadata` row.
        let ty = known(
            "RestartStrategy",
            TypeProvenance::Aliased {
                name: "RestartStrategy".into(),
                span: s(),
            },
        );
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn project_recv_type_meta_yields_class_object() {
        let ty = InferredType::Meta {
            class_name: "Counter".into(),
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(project_recv_type(&ty), RecvType::ClassObject(n) if n == "Counter"));
    }

    #[test]
    fn project_recv_type_dynamic_coarsens_to_dynamic() {
        let ty = InferredType::Dynamic(DynamicReason::Unknown);
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn project_recv_type_never_coarsens_to_dynamic() {
        assert!(matches!(
            project_recv_type(&InferredType::Never),
            RecvType::Dynamic
        ));
    }

    #[test]
    fn project_recv_type_union_of_resolvable_members_yields_union() {
        // BT-3215: every member resolves to a clean single name, so the
        // whole union keys precisely instead of coarsening to `dynamic`.
        let ty = InferredType::simple_union(&["String", "Integer"]);
        assert!(matches!(
            project_recv_type(&ty),
            RecvType::Union(names) if names == vec![EcoString::from("Integer"), EcoString::from("String")]
        ));
    }

    #[test]
    fn project_recv_type_union_dedupes_members() {
        // Two members that resolve to the same name (e.g. distinct
        // provenance for the same class) must not double up in the list.
        let ty = InferredType::Union {
            members: vec![
                known("Foo", TypeProvenance::Inferred(s())),
                known("Foo", TypeProvenance::Declared(s())),
            ],
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(
            project_recv_type(&ty),
            RecvType::Union(names) if names == vec![EcoString::from("Foo")]
        ));
    }

    #[test]
    fn project_recv_type_union_with_unresolvable_member_coarsens_to_dynamic() {
        // BT-3215: a partial member list would be unsound (Constraint 2) —
        // one member that can't resolve to a clean name (here, `Dynamic`)
        // must coarsen the *whole* union, not just drop that member.
        let ty = InferredType::Union {
            members: vec![
                known("Foo", TypeProvenance::Inferred(s())),
                InferredType::Dynamic(DynamicReason::Unknown),
            ],
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn project_recv_type_union_with_nested_composed_member_coarsens_to_dynamic() {
        // A member that is itself a `Union`/`Intersection` never resolves
        // to a single name (`project_composed` only builds one level of
        // member list), so it coarsens the outer union too.
        let ty = InferredType::Union {
            members: vec![
                known("Foo", TypeProvenance::Inferred(s())),
                InferredType::simple_union(&["Bar", "Baz"]),
            ],
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn project_recv_type_intersection_of_resolvable_members_yields_intersection() {
        // BT-3215: ADR 0068 protocol composition
        // (`Collection(Object) & Comparable`) now keys precisely instead of
        // deferring to `dynamic`.
        let ty = InferredType::Intersection {
            members: vec![
                known("Printable", TypeProvenance::Inferred(s())),
                known("Comparable", TypeProvenance::Inferred(s())),
            ],
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(
            project_recv_type(&ty),
            RecvType::Intersection(names)
                if names == vec![EcoString::from("Comparable"), EcoString::from("Printable")]
        ));
    }

    #[test]
    fn project_recv_type_intersection_with_unresolvable_member_coarsens_to_dynamic() {
        let ty = InferredType::Intersection {
            members: vec![
                known("Comparable", TypeProvenance::Inferred(s())),
                known("List", TypeProvenance::Extracted),
            ],
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn project_recv_type_negation_coarsens_to_dynamic() {
        let ty = InferredType::Negation {
            base: Box::new(known("Symbol", TypeProvenance::Inferred(s()))),
            excluded: Box::new(known("#foo", TypeProvenance::Inferred(s()))),
            provenance: TypeProvenance::Inferred(s()),
        };
        assert!(matches!(project_recv_type(&ty), RecvType::Dynamic));
    }

    #[test]
    fn test_generate_register_class_empty_module_renders_empty() {
        let mut generator = CoreErlangGenerator::new("test");
        let module = Module {
            classes: vec![],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = generator.generate_register_class(&module, false).unwrap();
        assert_eq!(
            doc.to_pretty_string(),
            "",
            "empty module should produce empty doc"
        );
    }

    #[test]
    fn test_generate_register_class_includes_class_name() {
        let mut generator = CoreErlangGenerator::new("test");
        let module = Module {
            classes: vec![make_actor_class("Counter")],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = generator.generate_register_class(&module, false).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'Counter'"),
            "register_class should include class name atom. Got: {output}"
        );
        assert!(
            output.contains("register_class"),
            "register_class should define register_class/0. Got: {output}"
        );
    }

    #[test]
    fn test_generate_method_dispatch_unary_includes_selector() {
        let mut generator = CoreErlangGenerator::new("test");
        let method = simple_unary_method("increment");
        let doc = generator.generate_method_dispatch(&method, 2).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'increment'"),
            "method dispatch should include selector atom. Got: {output}"
        );
    }

    #[test]
    fn test_generate_class_method_dispatches_empty_class() {
        let mut generator = CoreErlangGenerator::new("test");
        let class = make_actor_class("Counter");
        let doc = generator
            .generate_class_method_dispatches(&class, 2)
            .unwrap();
        assert_eq!(
            doc.to_pretty_string(),
            "",
            "class with no methods should produce empty dispatch doc"
        );
    }

    #[test]
    fn test_generate_class_method_functions_empty_class() {
        let mut generator = CoreErlangGenerator::new("test");
        let class = make_actor_class("Counter");
        let doc = generator.generate_class_method_functions(&class).unwrap();
        assert_eq!(
            doc.to_pretty_string(),
            "",
            "class with no class methods should produce empty doc"
        );
    }

    // ── ADR 0068: MetaTypeRepr tests ──

    #[test]
    fn test_meta_type_repr_none_renders_none_atom() {
        let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::None);
        assert_eq!(doc.to_pretty_string(), "'none'");
    }

    #[test]
    fn test_meta_type_repr_atom_renders_quoted() {
        let doc =
            CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Atom("Integer".to_string()));
        assert_eq!(doc.to_pretty_string(), "'Integer'");
    }

    #[test]
    fn test_meta_type_repr_type_param_renders_tagged_tuple() {
        let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::TypeParam {
            name: "T".to_string(),
            index: 0,
        });
        assert_eq!(doc.to_pretty_string(), "{'type_param', 'T', 0}");
    }

    #[test]
    fn test_meta_type_repr_type_param_method_local() {
        let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::TypeParam {
            name: "R".to_string(),
            index: -1,
        });
        assert_eq!(doc.to_pretty_string(), "{'type_param', 'R', -1}");
    }

    #[test]
    fn test_meta_type_repr_generic_renders_tagged_tuple() {
        let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Generic {
            base: "Result".to_string(),
            parameters: vec![
                MetaTypeRepr::TypeParam {
                    name: "T".to_string(),
                    index: 0,
                },
                MetaTypeRepr::TypeParam {
                    name: "E".to_string(),
                    index: 1,
                },
            ],
        });
        assert_eq!(
            doc.to_pretty_string(),
            "{'generic', 'Result', [{'type_param', 'T', 0}, {'type_param', 'E', 1}]}"
        );
    }

    #[test]
    fn test_meta_type_repr_union_renders_tagged_tuple() {
        // BT-3076: `Integer | String` → `{'union', ['Integer', 'String']}`.
        let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Union(vec![
            MetaTypeRepr::Atom("Integer".to_string()),
            MetaTypeRepr::Atom("String".to_string()),
        ]));
        assert_eq!(doc.to_pretty_string(), "{'union', ['Integer', 'String']}");
    }

    #[test]
    fn test_meta_type_repr_singleton_renders_tagged_tuple() {
        // BT-3076: `#north` → `{'singleton', 'north'}`.
        let doc =
            CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Singleton("north".to_string()));
        assert_eq!(doc.to_pretty_string(), "{'singleton', 'north'}");
    }

    #[test]
    fn test_meta_type_repr_generic_of_union_nests_tagged_tuples() {
        // BT-3076: `Result(Integer | String, Error)` — Union nested inside
        // Generic, exercising the shared `meta_type_repr_list_doc` helper.
        let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Generic {
            base: "Result".to_string(),
            parameters: vec![
                MetaTypeRepr::Union(vec![
                    MetaTypeRepr::Atom("Integer".to_string()),
                    MetaTypeRepr::Atom("String".to_string()),
                ]),
                MetaTypeRepr::Atom("Error".to_string()),
            ],
        });
        assert_eq!(
            doc.to_pretty_string(),
            "{'generic', 'Result', [{'union', ['Integer', 'String']}, 'Error']}"
        );
    }

    #[test]
    fn test_declared_type_to_meta_repr_union_converts_structurally() {
        // BT-3076: `declared_type_to_meta_repr` — not the pre-existing
        // atom-of-rendered-string fallback — handles `Union` structurally.
        let dt = DeclaredType::Union(vec![
            DeclaredType::simple("Integer"),
            DeclaredType::simple("String"),
        ]);
        let repr = CoreErlangGenerator::declared_type_to_meta_repr(&dt, &[]);
        assert_eq!(
            repr,
            MetaTypeRepr::Union(vec![
                MetaTypeRepr::Atom("Integer".to_string()),
                MetaTypeRepr::Atom("String".to_string()),
            ])
        );
    }

    #[test]
    fn test_declared_type_to_meta_repr_singleton_converts_structurally() {
        let dt = DeclaredType::singleton("north");
        let repr = CoreErlangGenerator::declared_type_to_meta_repr(&dt, &[]);
        assert_eq!(repr, MetaTypeRepr::Singleton("north".to_string()));
    }

    #[test]
    fn test_type_annotation_to_meta_repr_simple_concrete() {
        let ta = TypeAnnotation::simple("Integer", s());
        let class_tp = vec![];
        let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
        assert_eq!(repr, MetaTypeRepr::Atom("Integer".to_string()));
    }

    #[test]
    fn test_type_annotation_to_meta_repr_simple_type_param() {
        let ta = TypeAnnotation::simple("T", s());
        let class_tp = vec![
            TypeParamDecl::unbounded(Identifier::new("T", s())),
            TypeParamDecl::unbounded(Identifier::new("E", s())),
        ];
        let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
        assert_eq!(
            repr,
            MetaTypeRepr::TypeParam {
                name: "T".to_string(),
                index: 0,
            }
        );
    }

    #[test]
    fn test_type_annotation_to_meta_repr_method_local_type_param() {
        // 'R' is a single uppercase letter not in class type_params → method-local
        let ta = TypeAnnotation::simple("R", s());
        let class_tp = vec![TypeParamDecl::unbounded(Identifier::new("T", s()))];
        let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
        assert_eq!(
            repr,
            MetaTypeRepr::TypeParam {
                name: "R".to_string(),
                index: -1,
            }
        );
    }

    #[test]
    fn test_type_annotation_to_meta_repr_generic_with_type_params() {
        // Result(R, E) where class has T, E → R is method-local (-1), E is class param (1)
        let ta = TypeAnnotation::generic(
            Identifier::new("Result", s()),
            vec![
                TypeAnnotation::simple("R", s()),
                TypeAnnotation::simple("E", s()),
            ],
            s(),
        );
        let class_tp = vec![
            TypeParamDecl::unbounded(Identifier::new("T", s())),
            TypeParamDecl::unbounded(Identifier::new("E", s())),
        ];
        let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
        assert_eq!(
            repr,
            MetaTypeRepr::Generic {
                base: "Result".to_string(),
                parameters: vec![
                    MetaTypeRepr::TypeParam {
                        name: "R".to_string(),
                        index: -1,
                    },
                    MetaTypeRepr::TypeParam {
                        name: "E".to_string(),
                        index: 1,
                    },
                ],
            }
        );
    }

    #[test]
    fn test_meta_method_info_map_with_type_params() {
        let entries: Vec<super::MethodInfoEntry> = vec![(
            "unwrap".to_string(),
            0,
            MetaTypeRepr::TypeParam {
                name: "T".to_string(),
                index: 0,
            },
            vec![],
            true,
            false,
        )];
        let doc = CoreErlangGenerator::meta_method_info_map(&entries);
        let output = doc.to_pretty_string();
        assert!(
            output.contains("{'type_param', 'T', 0}"),
            "method_info map should contain type_param tagged tuple. Got: {output}"
        );
    }

    #[test]
    fn test_meta_type_params_in_meta_map() {
        // Build a generic class and verify type_params appears in meta map
        let mut class = make_actor_class("Container");
        class.type_params = vec![
            TypeParamDecl::unbounded(Identifier::new("T", s())),
            TypeParamDecl::unbounded(Identifier::new("E", s())),
        ];
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'type_params' => ['T', 'E']"),
            "meta map should include type_params list. Got: {output}"
        );
    }

    /// Helper: build a single-class module from an actor class (ADR 0098 tests).
    fn module_with(class: ClassDefinition) -> Module {
        Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        }
    }

    #[test]
    fn test_meta_provenance_keys_emitted_when_supplied() {
        // ADR 0098 Phase 3: a known toolchain bakes beamtalk_version + otp_release
        // into __beamtalk_meta as binary string literals (the same compound OTP key
        // the stamp uses — never a runtime system_info call).
        let module = module_with(make_actor_class("Counter"));
        let provenance = MetaProvenance {
            beamtalk_version: Some("0.4.0-dev+abc123"),
            otp_release: Some("28-16.4"),
        };
        let output = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            provenance,
        )
        .to_pretty_string();

        assert!(
            output.contains("'beamtalk_version' => "),
            "meta map should include beamtalk_version key. Got: {output}"
        );
        assert!(
            output.contains("'otp_release' => "),
            "meta map should include otp_release key. Got: {output}"
        );
        // Values are baked verbatim as binary literals.
        assert!(
            output.contains(&beamtalk_cerl_doc::binary::binary_string_literal(
                "0.4.0-dev+abc123"
            )),
            "beamtalk_version value not baked correctly. Got: {output}"
        );
        assert!(
            output.contains(&beamtalk_cerl_doc::binary::binary_string_literal("28-16.4")),
            "otp_release value not baked correctly. Got: {output}"
        );
    }

    #[test]
    fn test_meta_provenance_keys_absent_by_default() {
        // REPL / test / older-toolchain codegen supplies no provenance; the keys
        // must be omitted entirely (readers treat absence as a stale module).
        let module = module_with(make_actor_class("Counter"));
        let output = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        )
        .to_pretty_string();

        assert!(
            !output.contains("beamtalk_version"),
            "meta map must omit beamtalk_version when unknown. Got: {output}"
        );
        assert!(
            !output.contains("otp_release"),
            "meta map must omit otp_release when unknown. Got: {output}"
        );
    }

    #[test]
    fn test_meta_provenance_version_only_when_otp_unknown() {
        // OTP probe failed but the version is known: emit beamtalk_version alone.
        let module = module_with(make_actor_class("Counter"));
        let provenance = MetaProvenance {
            beamtalk_version: Some("1.2.3"),
            otp_release: None,
        };
        let output = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            provenance,
        )
        .to_pretty_string();

        assert!(
            output.contains("'beamtalk_version' => "),
            "beamtalk_version should be present. Got: {output}"
        );
        assert!(
            !output.contains("otp_release"),
            "otp_release must be omitted when OTP is unknown. Got: {output}"
        );
    }

    #[test]
    fn test_meta_type_params_empty_for_non_generic() {
        let class = make_actor_class("Counter");
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'type_params' => []"),
            "non-generic class should have empty type_params. Got: {output}"
        );
    }

    #[test]
    fn test_meta_map_includes_package_name() {
        let class = make_actor_class("Counter");
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            Some("my_counter"),
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'package' => 'my_counter'"),
            "meta map should include package name. Got: {output}"
        );
    }

    #[test]
    fn test_meta_map_package_none_without_package() {
        let class = make_actor_class("Counter");
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'package' => 'none'"),
            "meta map should have 'none' package when no package. Got: {output}"
        );
    }

    #[test]
    fn test_meta_map_includes_kind_actor() {
        let class = make_actor_class("Counter");
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'kind' => 'actor'"),
            "actor class meta should have kind 'actor'. Got: {output}"
        );
    }

    #[test]
    fn test_meta_map_includes_kind_value() {
        let mut class = make_actor_class("Point");
        class.class_kind = ClassKind::Value;
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'kind' => 'value'"),
            "value class meta should have kind 'value'. Got: {output}"
        );
    }

    #[test]
    fn test_extract_package_from_module_name() {
        assert_eq!(
            extract_package_from_module_name("bt@my_counter@counter"),
            Some("my_counter".to_string())
        );
        assert_eq!(
            extract_package_from_module_name("bt@stdlib@integer"),
            Some("stdlib".to_string())
        );
        assert_eq!(extract_package_from_module_name("beamtalk_integer"), None);
        assert_eq!(extract_package_from_module_name("bt@"), None);
        assert_eq!(
            extract_package_from_module_name("bt@pkg@sub@dir@class"),
            Some("pkg".to_string())
        );
    }

    #[test]
    fn test_meta_map_visibility_public_by_default() {
        let class = make_actor_class("Counter");
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'visibility' => 'public'"),
            "non-internal class should have visibility 'public'. Got: {output}"
        );
    }

    #[test]
    fn test_meta_map_visibility_internal() {
        let mut class = make_actor_class("Helper");
        class.is_internal = true;
        let module = Module {
            classes: vec![class],
            method_definitions: Vec::new(),
            protocols: vec![],
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: vec![],
            file_trailing_comments: Vec::new(),
        };
        let doc = CoreErlangGenerator::build_meta_map_doc(
            module.classes.first().unwrap(),
            &module,
            false,
            false,
            None,
            MetaProvenance::default(),
        );
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'visibility' => 'internal'"),
            "internal class should have visibility 'internal'. Got: {output}"
        );
    }

    #[test]
    fn test_method_info_visibility_public_by_default() {
        let entries: Vec<super::MethodInfoEntry> = vec![(
            "getValue".to_string(),
            0,
            MetaTypeRepr::None,
            vec![],
            false,
            false,
        )];
        let doc = CoreErlangGenerator::meta_method_info_map(&entries);
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'visibility' => 'public'"),
            "non-internal method should have visibility 'public'. Got: {output}"
        );
    }

    #[test]
    fn test_method_info_visibility_internal() {
        let entries: Vec<super::MethodInfoEntry> = vec![(
            "helperMethod".to_string(),
            0,
            MetaTypeRepr::None,
            vec![],
            false,
            true,
        )];
        let doc = CoreErlangGenerator::meta_method_info_map(&entries);
        let output = doc.to_pretty_string();
        assert!(
            output.contains("'visibility' => 'internal'"),
            "internal method should have visibility 'internal'. Got: {output}"
        );
    }

    // ── BT-2734: synthetic value-accessor doc / signature metadata ──

    use beamtalk_core::ast::{DeclaredKeyword, StateDeclaration};

    fn slot(name: &str, ty: Option<&str>) -> StateDeclaration {
        StateDeclaration {
            name: Identifier::new(name, s()),
            type_annotation: ty.map(|t| TypeAnnotation::Simple(Identifier::new(t, s()))),
            default_value: None,
            expect: None,
            comments: beamtalk_core::ast::CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: s(),
        }
    }

    fn value_class(name: &str, slots: Vec<StateDeclaration>) -> ClassDefinition {
        ClassDefinition::new(
            Identifier::new(name, s()),
            Identifier::new("Value", s()),
            slots,
            vec![],
            s(),
        )
    }

    fn find_entry<'a>(
        entries: &'a [super::SyntheticAccessorEntry],
        selector: &str,
    ) -> &'a super::SyntheticAccessorEntry {
        entries
            .iter()
            .find(|(sel, _, _)| sel == selector)
            .unwrap_or_else(|| panic!("no synthetic entry for {selector}"))
    }

    #[test]
    fn test_synthetic_getter_signature_and_doc() {
        let class = value_class("Point", vec![slot("x", Some("Integer"))]);
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        let (_, sig, doc) = find_entry(&entries.instance, "x");
        assert_eq!(sig, "x -> Integer");
        assert_eq!(
            doc,
            "Compiler-derived accessor. Returns the value of slot `x`."
        );
    }

    #[test]
    fn test_synthetic_setter_signature_and_doc() {
        let class = value_class("Point", vec![slot("x", Some("Integer"))]);
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        let (_, sig, doc) = find_entry(&entries.instance, "withX:");
        assert_eq!(sig, "withX: aValue -> Point");
        assert_eq!(
            doc,
            "Compiler-derived copy-setter. Returns a copy with slot `x` replaced."
        );
    }

    #[test]
    fn test_synthetic_keyword_constructor_is_class_side() {
        let class = value_class(
            "Point",
            vec![slot("x", Some("Integer")), slot("y", Some("Integer"))],
        );
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        assert_eq!(entries.class.len(), 1, "one keyword constructor entry");
        let (sel, sig, doc) = &entries.class[0];
        assert_eq!(sel, "x:y:");
        assert_eq!(sig, "x: x y: y -> Point");
        assert_eq!(
            doc,
            "Compiler-derived keyword constructor. Returns a new Point from the given slot values."
        );
    }

    #[test]
    fn test_synthetic_keyword_constructor_long_selector_is_hashed() {
        // Regression guard alongside BT-1408's `value_many_fields.bt` fixture: a
        // Value class with enough long field names that the raw keyword-
        // constructor selector exceeds Erlang's 255-char atom limit must not
        // surface that raw selector as a `classMethodSignatures`/
        // `classMethodDocs` map *key* atom (it would blow the limit exactly like
        // the dispatch function name did before BT-1408). The key must match
        // `safe_class_method_selector`, the same hash the runtime meta entry and
        // dispatch already use for this selector — the doc/signature *text* still
        // carries the full readable field names since it is a binary, not atom.
        let long_field = "a".repeat(60);
        let field_names: Vec<String> = (0..5).map(|i| format!("{long_field}{i}")).collect();
        let slots: Vec<StateDeclaration> = field_names
            .iter()
            .map(|n| slot(n, Some("Integer")))
            .collect();
        let class = value_class("Big", slots);
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        assert_eq!(entries.class.len(), 1, "one keyword constructor entry");
        let (sel, sig, _doc) = &entries.class[0];
        assert!(
            sel.len() <= 255,
            "keyword constructor map key must stay within the atom limit, got {} bytes",
            sel.len()
        );
        let raw_kw_sel = beamtalk_core::synthetic_selectors::keyword_constructor_selector(
            field_names.iter().map(String::as_str),
        );
        assert_eq!(
            *sel,
            crate::core_erlang::selector_mangler::safe_class_method_selector(&raw_kw_sel),
            "map key must match the same hash the runtime meta entry/dispatch use"
        );
        // The signature text keeps the full readable field names (a binary, not
        // an atom, so it carries no length limit).
        assert!(sig.contains(&format!("{long_field}0")));
    }

    #[test]
    fn test_synthetic_untyped_slot_falls_back_to_object() {
        let class = value_class("Box", vec![slot("v", None)]);
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        let (_, sig, _) = find_entry(&entries.instance, "v");
        assert_eq!(sig, "v -> Object");
    }

    #[test]
    fn test_synthetic_entries_empty_for_non_value_class() {
        let class = make_actor_class("Counter");
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        assert!(
            entries.instance.is_empty() && entries.class.is_empty(),
            "actor classes get no synthetic value accessors"
        );
    }

    #[test]
    fn test_synthetic_skips_user_overridden_getter() {
        // A user-defined `x` getter shadows the auto getter, but the auto
        // `withX:` copy-setter is still synthesized.
        let mut class = value_class("Point", vec![slot("x", Some("Integer"))]);
        class.methods.push(simple_unary_method("x"));
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        assert!(
            entries.instance.iter().all(|(sel, _, _)| sel != "x"),
            "user-defined getter must not be re-synthesized"
        );
        assert!(
            entries.instance.iter().any(|(sel, _, _)| sel == "withX:"),
            "the copy-setter is still auto-generated"
        );
    }

    #[test]
    fn test_synthetic_skips_user_overridden_keyword_constructor() {
        // A user-defined `x:y:` class-side method shadows the auto keyword
        // constructor, so no synthetic class-side entry is injected.
        let mut class = value_class(
            "Point",
            vec![slot("x", Some("Integer")), slot("y", Some("Integer"))],
        );
        class.class_methods.push({
            let mut m = MethodDefinition::new(
                MessageSelector::Keyword(vec![
                    KeywordPart::new("x:", s()),
                    KeywordPart::new("y:", s()),
                ]),
                vec![
                    ParameterDefinition::new(Identifier::new("x", s())),
                    ParameterDefinition::new(Identifier::new("y", s())),
                ],
                vec![bare(Expression::Literal(Literal::Integer(42), s()))],
                s(),
            );
            m.is_class_method = true;
            m
        });
        let entries = CoreErlangGenerator::synthetic_value_accessor_entries(&class);
        assert!(
            entries.class.is_empty(),
            "user-defined keyword constructor must suppress the synthetic class-side entry"
        );
    }
}
