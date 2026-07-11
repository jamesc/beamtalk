// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method body code generation and class registration.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates method dispatch case clauses, method body with state threading
//! and reply tuples, and the `register_class/0` on-load function.

use super::super::document::leaf::fname;
use super::super::document::{Document, INDENT, join, leaf, line, nest};
use super::super::selector_mangler::safe_class_method_fn_name;
use super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use crate::ast::{
    Block, CascadeMessage, ClassDefinition, ClassKind, Expression, Identifier, Literal, MapPair,
    MessageSelector, MethodDefinition, MethodKind, Module, ParameterDefinition, StateDeclaration,
    TypeAnnotation, TypeParamDecl, WellKnownSelector,
};
use crate::docvec;
use crate::unparse::{unparse_method_display_signature, unparse_type_annotation_display};

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
/// [`crate::queries::references_to_query`] — the walker hand-written-method
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
/// and consumed by the unified [`CoreErlangGenerator::generate_body_exprs_with_reply`]
/// and [`CoreErlangGenerator::generate_conditional_branch_inline`].
pub(in crate::codegen::core_erlang) enum BodyExprKind {
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
}

/// Tuple representing a method entry for `method_info` / `class_method_info` meta maps.
///
/// Fields: (`erlang_selector`, `arity`, `return_type`, `param_types`, `is_sealed`, `is_internal`)
pub(super) type MethodInfoEntry = (String, usize, MetaTypeRepr, Vec<MetaTypeRepr>, bool, bool);

impl CoreErlangGenerator {
    /// Generates dispatch case clauses for all methods in a class definition.
    pub(in crate::codegen::core_erlang) fn generate_class_method_dispatches(
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
    pub(in crate::codegen::core_erlang) fn generate_method_dispatch(
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
        // (re-)population happens inside generate_body_exprs_with_reply via
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

        let nlr_token_var = if needs_nlr {
            let token_var = self.fresh_temp_var("NlrToken");
            self.set_current_nlr_token(Some(token_var.clone()));
            Some(token_var)
        } else {
            None
        };

        // Generate body as Document
        let method_body_doc = match self.generate_method_definition_body_with_reply(method) {
            Ok(doc) => doc,
            Err(e) => {
                self.set_current_nlr_token(None);
                self.pop_scope();
                self.current_method_selector = None;
                return Err(e);
            }
        };

        self.set_current_nlr_token(None);

        // BT-761/BT-764: If NLR was detected, wrap body in a letrec function with
        // try/catch via the shared helper. letrec creates a genuine separate
        // function frame, avoiding BEAM validator ambiguous_catch_try_state
        // errors that arise when try/catch is nested inside case arms.
        // BT-774: Compose at Document level without intermediate string rendering.
        let method_body_doc = if let Some(ref token_var) = nlr_token_var {
            self.wrap_actor_body_with_nlr_catch(method_body_doc, token_var, true)
        } else {
            method_body_doc
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

    /// Generates a method definition body wrapped in a reply tuple.
    ///
    /// For `MethodDefinition` nodes with explicit body expressions.
    /// Delegates to [`Self::generate_body_exprs_with_reply`].
    pub(in crate::codegen::core_erlang) fn generate_method_definition_body_with_reply(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<Document<'static>> {
        let body = super::super::util::collect_body_exprs(&method.body);
        self.generate_body_exprs_with_reply(&body, true)
    }

    /// Generates a block-based method body with state threading and reply tuple.
    ///
    /// For methods using block syntax with implicit returns.
    /// Delegates to [`Self::generate_body_exprs_with_reply`].
    pub(in crate::codegen::core_erlang) fn generate_method_body_with_reply(
        &mut self,
        block: &Block,
    ) -> Result<Document<'static>> {
        let body = super::super::util::collect_body_exprs(&block.body);
        self.generate_body_exprs_with_reply(&body, false)
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
    /// `generate_body_exprs_with_reply`/`BodyExprKind` here) that has no
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
                    crate::ast::StringSegment::Interpolation(e) => Self::scan_var_uses(e, var_name),
                    crate::ast::StringSegment::Literal(_) => (false, false),
                })
                .fold((false, false), |(u, s), (u2, s2)| (u || u2, s || s2)),
        }
    }

    /// Classify a body expression for state-threading dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. field assignment)
    /// must come before general ones (e.g. pure expression).
    pub(in crate::codegen::core_erlang) fn classify_body_expr(
        &self,
        expr: &Expression,
    ) -> BodyExprKind {
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

    /// Unified method body code generation with state threading and reply tuple.
    ///
    /// Both `generate_method_definition_body_with_reply` (for `MethodDefinition`)
    /// and `generate_method_body_with_reply` (for `Block`) delegate here.
    ///
    /// `supports_early_return` controls whether `^ value` expressions are handled.
    /// Method definitions support it; block bodies do not (NLR uses throw/catch).
    #[expect(
        clippy::too_many_lines,
        reason = "unified handler for all method body expression types with state threading"
    )]
    fn generate_body_exprs_with_reply(
        &mut self,
        body: &[&Expression],
        supports_early_return: bool,
    ) -> Result<Document<'static>> {
        if body.is_empty() {
            let state = self.current_state_var();
            return Ok(docvec!["{'reply', Self, ", leaf::var(state), "}"]);
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

        // Phase 2: emit code for each (expression, kind) pair.
        let mut docs: Vec<Document<'static>> = Vec::with_capacity(body.len());
        let body_len = body.len();

        for (i, (expr, kind)) in body.iter().zip(plan.into_iter()).enumerate() {
            let is_last = i == body_len - 1;
            let is_early_return = matches!(&kind, BodyExprKind::EarlyReturn);

            // Early return — always terminates generation regardless of position.
            // Classify the inner value to handle super/tier2/dispatch returns.
            if is_early_return && supports_early_return {
                if let Expression::Return { value, .. } = expr {
                    let value_kind = self.classify_body_expr(value);
                    match value_kind {
                        BodyExprKind::SuperSend => {
                            let expr_str = self.expression_doc(value)?;
                            docs.push(docvec![
                                "let _SuperTuple = ",
                                expr_str,
                                " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                                " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                                " in {'reply', _Result, _NewState}",
                            ]);
                        }
                        BodyExprKind::Tier2ValueCall => {
                            let expr_str = self.generate_tier2_value_call_doc(value)?;
                            docs.push(self.emit_tuple_unpack_reply("T2Tuple", expr_str));
                        }
                        BodyExprKind::DispatchingSelfSend => {
                            let (doc, dispatch_var) = self.generate_self_dispatch_open(value)?;
                            docs.push(doc);
                            self.emit_dispatch_reply(&mut docs, &dispatch_var);
                        }
                        BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                            let tier2_args = tier2_args.clone();
                            let (doc, dispatch_var) =
                                self.generate_tier2_self_send_open(value, &tier2_args)?;
                            docs.push(doc);
                            self.emit_dispatch_reply(&mut docs, &dispatch_var);
                        }
                        BodyExprKind::ControlFlowWithMutations => {
                            let expr_str = self.expression_doc(value)?;
                            docs.push(self.emit_tuple_unpack_reply("Tuple", expr_str));
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
                                    let field_state = self.next_state_var();
                                    docs.push(docvec![
                                        "let ",
                                        leaf::var(tuple_var.clone()),
                                        " = ",
                                        rhs_str,
                                        " in let ",
                                        leaf::var(val_var.clone()),
                                        " = call 'erlang':'element'(1, ",
                                        leaf::var(tuple_var.clone()),
                                        ") in let ",
                                        leaf::var(rhs_state.clone()),
                                        " = call 'erlang':'element'(2, ",
                                        leaf::var(tuple_var),
                                        ") in let ",
                                        leaf::var(field_state.clone()),
                                        " = call 'maps':'put'(",
                                        leaf::atom(field.name.to_string()),
                                        ", ",
                                        leaf::var(val_var.clone()),
                                        ", ",
                                        leaf::var(rhs_state),
                                        ") in {'reply', ",
                                        leaf::var(val_var),
                                        ", ",
                                        leaf::var(field_state),
                                        "}",
                                    ]);
                                }
                            }
                        }
                        _ => {
                            let final_state = self.current_state_var();
                            let value_str = self.expression_doc(value)?;
                            docs.push(docvec![
                                "let _ReturnValue = ",
                                value_str,
                                " in {'reply', _ReturnValue, ",
                                leaf::var(final_state),
                                "}",
                            ]);
                        }
                    }
                    return Ok(Document::Vec(docs));
                }
            }

            match kind {
                BodyExprKind::SelfFieldAtPut => {
                    let (doc, val_var) = self.generate_self_field_at_put_open(expr)?;
                    docs.push(doc);
                    if is_last {
                        let final_state = self.current_state_var();
                        docs.push(docvec![
                            "{'reply', ",
                            leaf::var(val_var),
                            ", ",
                            leaf::var(final_state),
                            "}",
                        ]);
                    }
                }
                BodyExprKind::FieldAssignment => {
                    if is_last {
                        if let Expression::Assignment { target, value, .. } = expr {
                            if let Expression::FieldAccess { field, .. } = target.as_ref() {
                                let val_var = self.fresh_temp_var("Val");
                                let current_state = self.current_state_var();
                                let value_str = self.generate_field_assignment_value_doc(value)?;
                                let new_state = self.next_state_var();
                                docs.push(docvec![
                                    "let ",
                                    leaf::var(val_var.clone()),
                                    " = ",
                                    value_str,
                                    " in let ",
                                    leaf::var(new_state.clone()),
                                    " = call 'maps':'put'(",
                                    leaf::atom(field.name.to_string()),
                                    ", ",
                                    leaf::var(val_var.clone()),
                                    ", ",
                                    leaf::var(current_state),
                                    ") in {'reply', ",
                                    leaf::var(val_var),
                                    ", ",
                                    leaf::var(new_state),
                                    "}",
                                ]);
                            }
                        }
                    } else {
                        let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                        docs.push(doc);
                    }
                }
                // BT-1477: self.field := expr where RHS is control flow returning {Value, State}
                BodyExprKind::FieldAssignmentControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            // Evaluate the RHS (returns {Value, State} tuple)
                            let tuple_var = self.fresh_temp_var("CfTuple");
                            let val_var = self.fresh_temp_var("CfVal");
                            let value_str = self.expression_doc(value)?;
                            // Unpack the tuple: element(1) is the value, element(2) is the state
                            let rhs_state = self.fresh_temp_var("CfState");
                            let field_state = self.next_state_var();
                            let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                value_str,
                                " in let ",
                                leaf::var(val_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in let ",
                                leaf::var(rhs_state.clone()),
                                " = call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ") in let ",
                                leaf::var(field_state.clone()),
                                " = call 'maps':'put'(",
                                leaf::atom(field.name.to_string()),
                                ", ",
                                leaf::var(val_var.clone()),
                                ", ",
                                leaf::var(rhs_state),
                                ") in ",
                            ]];
                            // Extract threaded locals from the control flow state
                            // (e.g. ifTrue: [y := 1. y + 1] threads y via __local__ keys)
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
                            docs.push(Document::Vec(doc_parts));
                            if is_last {
                                docs.push(docvec![
                                    "{'reply', ",
                                    leaf::var(val_var),
                                    ", ",
                                    leaf::var(field_state),
                                    "}",
                                ]);
                            }
                        }
                    }
                }
                // BT-1479: self fieldAt: name put: expr where RHS is control flow returning {Value, State}
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_temp_var("Name");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let val_var = self.fresh_temp_var("CfVal");
                        let val_code = self.expression_doc(&arguments[1])?;
                        let rhs_state = self.fresh_temp_var("CfState");
                        let field_state = self.next_state_var();
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
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
                            ") in let ",
                            leaf::var(field_state.clone()),
                            " = call 'maps':'put'(",
                            leaf::var(name_var),
                            ", ",
                            leaf::var(val_var.clone()),
                            ", ",
                            leaf::var(rhs_state),
                            ") in ",
                        ]];
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
                        docs.push(Document::Vec(doc_parts));
                        if is_last {
                            docs.push(docvec![
                                "{'reply', ",
                                leaf::var(val_var),
                                ", ",
                                leaf::var(field_state),
                                "}",
                            ]);
                        }
                    }
                }
                // BT-1479: {a, b} := expr where RHS is control flow returning {Value, State}
                BodyExprKind::DestructureAssignmentControlFlow => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        // Evaluate RHS (returns {Value, State} tuple)
                        let tuple_var = self.fresh_temp_var("CfTuple");
                        let actual_val = self.fresh_temp_var("CfVal");
                        let value_str = self.expression_doc(value)?;
                        let new_state = self.peek_next_state_var();
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                            "let ",
                            leaf::var(tuple_var.clone()),
                            " = ",
                            value_str,
                            " in let ",
                            leaf::var(actual_val.clone()),
                            " = call 'erlang':'element'(1, ",
                            leaf::var(tuple_var.clone()),
                            ") in let ",
                            leaf::var(new_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            leaf::var(tuple_var),
                            ") in ",
                        ]];
                        let _ = self.next_state_var();
                        // Extract threaded locals
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
                        docs.push(Document::Vec(doc_parts));
                        // Now destructure the unpacked value
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            docs.push(d);
                        }
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        docs.push(docvec!["{'reply', 'nil', ", leaf::var(post_state), "}",]);
                    }
                }
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
                            let new_state = self.next_state_var();
                            docs.push(docvec![
                                "let ",
                                leaf::var(tuple_var.clone()),
                                " = ",
                                value_str,
                                " in let ",
                                leaf::var(core_var),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ")\n in let ",
                                leaf::var(new_state),
                                " = call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ") in ",
                            ]);
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
                    }
                }
                BodyExprKind::LocalAssignControlFlow => {
                    // BT-2378: route the actor `var := <control-flow-with-mutations>` assign-RHS
                    // through the shared `ThreadedExpr` emitter. The Actor boundary binds the
                    // target to element 1, advances the state version to element 2 (the threaded
                    // gen_server `State`), and rebinds `__local__`-threaded sibling outer-locals.
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let routed = self
                                .emit_threaded_assign_rhs(
                                    &id.name,
                                    value,
                                    super::super::threaded_expr::ThreadingBoundary::Actor,
                                    &mut docs,
                                )?
                                .is_some();
                            debug_assert!(
                                routed,
                                "LocalAssignControlFlow must route through the Actor threaded emitter"
                            );
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
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
                            // Dispatch the self-send (threads state, returns dispatch var)
                            let (dispatch_doc, dispatch_var) =
                                self.generate_self_dispatch_open(value)?;
                            docs.push(dispatch_doc);
                            self.bind_var(var_name, &core_var);
                            docs.push(docvec![
                                "let ",
                                leaf::var(core_var),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(dispatch_var),
                                ") in ",
                            ]);
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
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
                            docs.push(docvec![
                                "let ",
                                leaf::var(core_var),
                                " = ",
                                value_str,
                                " in ",
                            ]);
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
                    }
                }
                BodyExprKind::LocalAssignPure => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::Identifier(id) = target.as_ref() {
                            let var_name = &id.name;
                            let core_var = self
                                .lookup_var(var_name)
                                .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                            let value_str = self.expression_doc(value)?;
                            self.bind_var(var_name, &core_var);
                            docs.push(docvec![
                                "let ",
                                leaf::var(core_var),
                                " = ",
                                value_str,
                                " in ",
                            ]);
                        }
                    }
                    if is_last {
                        self.emit_pure_reply(&mut docs);
                    }
                }
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                        for d in binding_docs {
                            docs.push(d);
                        }
                    }
                    if is_last {
                        let post_state = self.current_state_var();
                        docs.push(docvec!["{'reply', 'nil', ", leaf::var(post_state), "}",]);
                    }
                }
                BodyExprKind::SuperSend => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec![
                            "let _SuperTuple = ",
                            expr_str,
                            " in let _Result = call 'erlang':'element'(2, _SuperTuple)",
                            " in let _NewState = call 'erlang':'element'(3, _SuperTuple)",
                            " in {'reply', _Result, _NewState}",
                        ]);
                    } else {
                        self.emit_super_send_open(expr, &mut docs)?;
                    }
                }
                BodyExprKind::ErrorSend => {
                    if is_last {
                        // Error send never returns — no reply tuple needed.
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec![expr_str]);
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in ",]);
                    }
                }
                BodyExprKind::Tier2ValueCall => {
                    if is_last {
                        let expr_str = self.generate_tier2_value_call_doc(expr)?;
                        docs.push(self.emit_tuple_unpack_reply("T2Tuple", expr_str));
                    } else {
                        let tuple_var = self.fresh_temp_var("T2Tuple");
                        let discard_var = self.fresh_temp_var("T2Discard");
                        let expr_str = self.generate_tier2_value_call_doc(expr)?;
                        let new_state = self.next_state_var();
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                            "let ",
                            leaf::var(tuple_var.clone()),
                            " = ",
                            expr_str,
                            " in let ",
                            leaf::var(discard_var),
                            " = call 'erlang':'element'(1, ",
                            leaf::var(tuple_var.clone()),
                            ")\n in let ",
                            leaf::var(new_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            leaf::var(tuple_var),
                            ") in ",
                        ]];

                        // BT-1213: Extract captured local mutations from NewState
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
                        docs.push(Document::Vec(doc_parts));
                    }
                }
                BodyExprKind::Tier2SelfSend(ref tier2_args) => {
                    let (doc, dispatch_var) =
                        self.generate_tier2_self_send_open(expr, tier2_args)?;
                    docs.push(doc);
                    if is_last {
                        self.emit_dispatch_reply(&mut docs, &dispatch_var);
                    }
                }
                BodyExprKind::ControlFlowWithMutations => {
                    if is_last {
                        // BT-2378: route the last-position actor control-flow construct through
                        // the shared `ThreadedExpr` emitter. The Actor boundary binds element 1
                        // (Reply) and element 2 (the threaded gen_server `State`) and returns
                        // `{'reply', Reply, NewState}`.
                        let routed = self.emit_threaded_last(
                            expr,
                            super::super::threaded_expr::ThreadingPosition::Last,
                            super::super::threaded_expr::ThreadingBoundary::Actor,
                            &mut docs,
                        )?;
                        debug_assert!(
                            routed,
                            "ControlFlowWithMutations must route through the Actor threaded emitter"
                        );
                    } else {
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let new_state = self.peek_next_state_var();
                        let expr_str = self.expression_doc(expr)?;
                        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
                            "let ",
                            leaf::var(tuple_var.clone()),
                            " = ",
                            expr_str,
                            " in let ",
                            leaf::var(new_state.clone()),
                            " = call 'erlang':'element'(2, ",
                            leaf::var(tuple_var),
                            ") in ",
                        ]];
                        let _ = self.next_state_var();

                        // Extract threaded locals from the updated state
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
                        docs.push(Document::Vec(doc_parts));
                    }
                }
                BodyExprKind::DispatchingSelfSend => {
                    let (doc, dispatch_var) = self.generate_self_dispatch_open(expr)?;
                    docs.push(doc);
                    if is_last {
                        self.emit_dispatch_reply(&mut docs, &dispatch_var);
                    }
                }
                BodyExprKind::EarlyReturn => {
                    return Err(CodeGenError::Internal(
                        "EarlyReturn should be handled before match dispatch".to_string(),
                    ));
                }
                BodyExprKind::Pure => {
                    if is_last {
                        let expr_str = self.expression_doc(expr)?;
                        let post_state = self.current_state_var();
                        docs.push(docvec![
                            "let _Result = ",
                            expr_str,
                            " in {'reply', _Result, ",
                            leaf::var(post_state),
                            "}",
                        ]);
                    } else {
                        let tmp_var = self.fresh_temp_var("seq");
                        let expr_str = self.expression_doc(expr)?;
                        docs.push(docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in ",]);
                    }
                }
            }
        }

        Ok(Document::Vec(docs))
    }

    /// Emit a generic `{'reply', _Result, State}` close for the last expression
    /// when the expression itself has already been emitted as an open let chain.
    /// Used by local assignments and other open-chain handlers in last position.
    fn emit_pure_reply(&mut self, docs: &mut Vec<Document<'static>>) {
        let post_state = self.current_state_var();
        docs.push(docvec!["{'reply', 'nil', ", leaf::var(post_state), "}",]);
    }

    /// Emit the last-position reply for a dispatch open call (Tier 2 self-send
    /// or dispatching self-send).  Uses the explicitly-passed `dispatch_var` and `current_state_var`.
    fn emit_dispatch_reply(&mut self, docs: &mut Vec<Document<'static>>, dispatch_var: &str) {
        let final_state = self.current_state_var();
        docs.push(docvec![
            "{'reply', call 'erlang':'element'(1, ",
            leaf::var(dispatch_var.to_string()),
            "), ",
            leaf::var(final_state),
            "}",
        ]);
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
            let selector_atom = selector.to_erlang_atom();
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

    /// Check if a method body is a single `self error: <StringLiteral>` expression.
    fn is_self_error_body(body: &[crate::ast::ExpressionStatement]) -> bool {
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
    pub(in crate::codegen::core_erlang) fn generate_register_class(
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
                let source_str = self.extract_method_source(m);
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
                let source_str = self.extract_method_source(m);
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
                instance_methods.len(),
                synth.instance_sigs,
            );
            let method_docs_doc = Self::extend_selector_map_doc(
                method_docs_doc,
                instance_methods
                    .iter()
                    .filter(|m| m.doc_comment.is_some())
                    .count(),
                synth.instance_docs,
            );
            let class_method_sigs_doc = Self::extend_selector_map_doc(
                class_method_sigs_doc,
                class_methods_primary.len(),
                synth.class_sigs,
            );
            let class_method_docs_doc = Self::extend_selector_map_doc(
                class_method_docs_doc,
                class_methods_primary
                    .iter()
                    .filter(|m| m.doc_comment.is_some())
                    .count(),
                synth.class_docs,
            );

            // BT-877: Detect non-constructible classes at compile time.
            // Emit `isConstructible = false` for: abstract classes, actors, and
            // classes with `new => self error: "..."`. For all others, omit the key
            // so the runtime can fall back to lazy computation — this is needed
            // because primitive classes (String, Integer, etc.) have raising new/0
            // in Erlang, not in Beamtalk AST.
            let is_non_constructible = class.is_abstract
                || self.context == CodeGenContext::Actor
                || Self::has_raising_new(class);

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
    /// `base_len` is the number of entries already present in `base` (0 when it
    /// is empty) — used solely to decide whether the first appended entry needs a
    /// leading separator. Returns `base` unchanged when there are no extras.
    fn extend_selector_map_doc(
        base: Document<'static>,
        base_len: usize,
        extra: Vec<Document<'static>>,
    ) -> Document<'static> {
        if extra.is_empty() {
            return base;
        }
        let mut parts: Vec<Document<'static>> = vec![base];
        for (i, entry) in extra.into_iter().enumerate() {
            if base_len > 0 || i > 0 {
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
    /// ([`crate::queries::all_sends_query::find_all_sends_in_source`] and
    /// [`crate::queries::references_to_query::find_all_references_in_source`]).
    /// Those operate on the unparsed bare-method source produced by
    /// [`Self::extract_method_source`] — the *same* source channel and walkers
    /// the `SystemNavigation` miss-policy fallback uses (ADR 0087 §Read path), so
    /// the baked line numbers are method-relative and byte-for-byte consistent
    /// with the fallback. No port round-trip; one in-process walk per method.
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
    pub(in crate::codegen::core_erlang::gen_server) fn build_method_xref_list(
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
        // BT-2614: synthetic compiler-injected class-side constructors
        // (`new`/`new:`/`spawn`/`spawn:` on actors) so the System Browser's
        // xref-backed protocol list matches runtime `allMethods` reflection.
        entries.extend(self.build_synthetic_class_method_xref_entries(class));
        docvec!["[", join(entries, &Document::Str(", ")), "]"]
    }

    /// BT-2614: Builds synthetic `method_xref` rows for the compiler-injected,
    /// sourceless **class-side** entry points of an actor class — `new`/`new:`
    /// (the no-direct-instantiation error stubs) and `spawn`/`spawn:` (the actor
    /// spawn functions). These are emitted as real exported functions by
    /// `actor_codegen.rs` (`generate_actor_new_error_method`,
    /// `generate_spawn_function`, and their `:`-arity / abstract variants) yet
    /// carry no user source and no AST `MethodDefinition`, so they never reach
    /// `build_method_xref_entry`. Without these rows the runtime *has* the
    /// methods (they show up in `SomeActor class allMethods`) but the System
    /// Browser's `defined_selectors/2` omits them (BT-2614).
    ///
    /// The rows mirror the synthetic auto-accessor shape (BT-2304):
    /// `source_status => synthetic`, a `synthetic_origin` line pointing at the
    /// class header (these methods have no slot/method declaration of their own),
    /// empty `sends` / `references`, and `class_side => true`. The browser badges
    /// them read-only (no openable source) and buckets them under
    /// "instance creation".
    ///
    /// Gated to `CodeGenContext::Actor`: value types auto-generate their own
    /// `new`/`new:` constructors with different semantics (a real constructor, not
    /// an error stub), so their xref parity is deliberately out of scope here and
    /// tracked separately. Both abstract and concrete actors inject all four
    /// selectors (the spawn *body* differs by abstractness, the selector set does
    /// not), so no `is_abstract` gating is needed.
    fn build_synthetic_class_method_xref_entries(
        &self,
        class: &ClassDefinition,
    ) -> Vec<Document<'static>> {
        if self.context != CodeGenContext::Actor {
            return Vec::new();
        }

        // Derived location: the 1-based class-header line. These injected methods
        // have no declaration of their own, so the class header is the only
        // meaningful origin (the browser uses it solely as a non-null marker).
        let origin_line = self.span_to_line(class.span).unwrap_or(1);

        ["new", "new:", "spawn", "spawn:"]
            .into_iter()
            .map(|selector| Self::build_synthetic_class_method_entry(selector, origin_line))
            .collect()
    }

    /// Builds a single synthetic class-side `method_xref` row (BT-2614). Mirrors
    /// `build_synthetic_accessor_entry` but is class-side (`class_side => true`)
    /// and has no type references (a constructor / spawn stub references no slot
    /// type). `origin_line` is the class-header line.
    fn build_synthetic_class_method_entry(selector: &str, origin_line: u32) -> Document<'static> {
        docvec![
            "~{'class_side' => 'true', 'selector' => ",
            leaf::atom(selector.to_string()),
            ", 'line' => ",
            leaf::int_lit(i64::from(origin_line)),
            ", 'sends' => [], 'references' => [], 'source_status' => 'synthetic', \
             'synthetic_origin' => ",
            leaf::int_lit(i64::from(origin_line)),
            "}~",
        ]
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
        const MAX_ATOM_BYTES: usize = 255;

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
        use crate::queries::all_sends_query::{ReceiverKind, find_all_sends_in_source};
        use crate::queries::references_to_query::find_all_references_in_source;

        // Erlang atoms cap at 255 bytes. A selector / class name longer than
        // that (e.g. a 20-keyword auto-constructor selector) can never exist as
        // a runtime dispatch atom, so a send / reference to it would never match
        // an xref query. Drop such entries rather than emitting an illegal atom
        // that fails `core_scan` at BEAM-compile time.
        const MAX_ATOM_BYTES: usize = 255;

        let source = self.extract_method_source(method);

        // The method definition's line within its own (bare) source is line 1:
        // `extract_method_source` emits the signature first (after any doc
        // comment / @expect lines the unparser prepends). The xref `line` field
        // is the method-relative definition line, so the first send/ref lines
        // are already in the same coordinate space.
        let def_line = Self::method_def_line(&source);

        let sends = find_all_sends_in_source(&source);
        let sends_doc = {
            let send_docs: Vec<Document<'static>> = sends
                .iter()
                .filter(|hit| hit.selector.len() <= MAX_ATOM_BYTES)
                .map(|hit| {
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
    /// Returns `Document::Nil` if the module has no protocol definitions.
    fn generate_protocol_registrations(&self, module: &Module) -> Document<'static> {
        if module.protocols.is_empty() {
            return Document::Nil;
        }

        let mut parts: Vec<Document<'static>> = Vec::new();

        for protocol in &module.protocols {
            let name = protocol.name.name.to_string();

            // Helper: build a Core Erlang list of method requirement maps
            let build_method_list =
                |sigs: &[crate::ast::ProtocolMethodSignature]| -> Document<'static> {
                    let items: Vec<Document<'static>> = sigs
                        .iter()
                        .map(|sig| {
                            let selector = sig.selector.name().to_string();
                            let arity = sig.selector.arity();
                            docvec![
                                "~{'selector' => ",
                                leaf::atom(selector),
                                ", 'arity' => ",
                                leaf::int_lit(i64::try_from(arity).unwrap_or(0)),
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
    pub(in crate::codegen::core_erlang) fn generate_class_method_functions(
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
            .map(|m| m.selector.to_erlang_atom())
            .collect();

        // BT-996: Populate auto-generated keyword constructor selector for Value subclass: classes.
        // This allows `ClassName slot: value` inside a class method to route to the correct
        // class-side constructor instead of falling through to the instance-side getter.
        self.set_class_slot_constructor_selector(
            crate::codegen::core_erlang::value_type_codegen::compute_auto_slot_methods(class)
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
                let inner_doc = match self
                    .generate_class_method_body(method, !class.class_variables.is_empty())
                {
                    Ok(doc) => doc,
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
                // match. class_var_mutated is set by generate_class_method_body when it sees a
                // class var assignment.
                let returns_class_var_result = self.class_var_mutated();
                if let Some(ref token_var) = nlr_token_var {
                    // BT-1202: Wrap body in try/catch to catch NLR throws from ^ inside blocks.
                    self.wrap_class_method_body_with_nlr_catch(
                        inner_doc,
                        token_var,
                        returns_class_var_result,
                    )
                } else {
                    inner_doc
                }
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
    pub(in crate::codegen::core_erlang) fn generate_class_methods_map_arg(
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
    pub(in crate::codegen::core_erlang) fn generate_class_method_single_arg(
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
            let inner_doc = match self.generate_class_method_body(&method, has_class_vars) {
                Ok(doc) => doc,
                Err(e) => {
                    self.set_current_nlr_token(None);
                    self.pop_scope();
                    return Err(e);
                }
            };
            self.set_current_nlr_token(None);
            let returns_class_var_result = self.class_var_mutated();
            if let Some(ref token_var) = nlr_token_var {
                self.wrap_class_method_body_with_nlr_catch(
                    inner_doc,
                    token_var,
                    returns_class_var_result,
                )
            } else {
                inner_doc
            }
        };

        let doc = docvec![
            "fun (ClassSelf, ClassVars",
            Self::class_method_params_suffix_doc(&param_vars),
            ") ->",
            nest(INDENT, docvec![line(), body_doc]),
        ];

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

    /// Generates the body of a class-side method.
    ///
    /// Unlike instance methods, class methods have no state threading.
    /// They simply evaluate expressions and return the last value.
    /// BT-412: If class variables were mutated, wraps the final result
    /// in `{class_var_result, Result, ClassVarsN}`.
    fn generate_class_method_body(
        &mut self,
        method: &MethodDefinition,
        has_class_vars: bool,
    ) -> Result<Document<'static>> {
        let mut docs: Vec<Document<'static>> = Vec::new();

        // Filter out @expect directives (compile-time only, no runtime representation).
        let body = super::super::util::collect_body_exprs(&method.body);
        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;

            if let Expression::Return { value, .. } = expr {
                let doc = self.generate_class_method_return(value, has_class_vars)?;
                docs.push(doc);
                return Ok(Document::Vec(docs));
            }

            if is_last {
                let doc = self.generate_class_method_last_expr(expr, has_class_vars)?;
                docs.push(doc);
            } else {
                let doc = self.generate_class_method_non_last_expr(expr)?;
                docs.push(doc);
            }
        }
        Ok(Document::Vec(docs))
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
        // BT-1942: Use `expression_doc_with_open_scope` so an explicit
        // `^ expr` where `expr` produces an open let-chain (e.g.
        // `(self tick) class` or any ProtoObject/Object intrinsic fed a
        // class method self-send) is emitted correctly. The open chain
        // must be emitted as a prefix; the value is the returned result
        // variable, not the chain wrapped in another let binding (which
        // would produce invalid double-`in` Core Erlang).
        if has_class_vars {
            let result_var = self.fresh_temp_var("Ret");
            let (value_str, open_scope) = self.expression_doc_with_open_scope(value)?;
            let (preamble, value_doc) = if let Some(open_scope_result) = open_scope {
                (value_str, leaf::var(open_scope_result))
            } else {
                (Document::Nil, value_str)
            };
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
            // BT-1942: Same treatment for the no-class-vars path.
            let (value_str, open_scope) = self.expression_doc_with_open_scope(value)?;
            if let Some(open_scope_result) = open_scope {
                Ok(docvec![value_str, leaf::var(open_scope_result)])
            } else {
                Ok(value_str)
            }
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
        if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
            let (expr_str, open_scope) = self.expression_doc_with_open_scope(expr)?;
            let final_cv = self.current_class_var();
            if let Some(result_var) = open_scope {
                Ok(docvec![
                    expr_str,
                    "{'class_var_result', ",
                    leaf::var(result_var),
                    ", ",
                    leaf::var(final_cv),
                    "}",
                ])
            } else {
                Ok(docvec![
                    expr_str,
                    "{'class_var_result', 'nil', ",
                    leaf::var(final_cv),
                    "}",
                ])
            }
        } else {
            let result_var = self.fresh_temp_var("Ret");
            // BT-1201: Use expression_doc_with_open_scope to detect open-scope results
            // produced by THIS expression, not by a previous field assignment.
            let (expr_str, open_scope) = self.expression_doc_with_open_scope(expr)?;
            if let Some(open_scope_result) = open_scope {
                if self.class_var_mutated() {
                    let final_cv = self.current_class_var();
                    Ok(docvec![
                        expr_str,
                        "let ",
                        leaf::var(result_var.clone()),
                        " = ",
                        leaf::var(open_scope_result),
                        " in {'class_var_result', ",
                        leaf::var(result_var),
                        ", ",
                        leaf::var(final_cv),
                        "}",
                    ])
                } else {
                    Ok(docvec![expr_str, leaf::var(open_scope_result)])
                }
            } else if self.class_var_mutated() {
                let final_cv = self.current_class_var();
                Ok(docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    expr_str,
                    " in {'class_var_result', ",
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
                    expr_str,
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
        if self.is_class_method_self_send(expr) {
            // BT-891: Class method self-send as last expression with no class vars.
            let (expr_str, open_scope) = self.expression_doc_with_open_scope(expr)?;
            if let Some(result_var) = open_scope {
                Ok(docvec![expr_str, leaf::var(result_var)])
            } else {
                Ok(expr_str)
            }
        } else {
            // BT-1201: Use expression_doc_with_open_scope to detect open-scope results.
            let (expr_str, open_scope) = self.expression_doc_with_open_scope(expr)?;
            if let Some(open_scope_result) = open_scope {
                Ok(docvec![expr_str, leaf::var(open_scope_result)])
            } else {
                Ok(expr_str)
            }
        }
    }

    /// Generates code for a non-last expression in a class method body.
    fn generate_class_method_non_last_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if self.is_class_var_assignment(expr) || self.is_class_method_self_send(expr) {
            // Class var assignment or class method self-send: the generated code
            // ends with `in ` (open scope) so ClassVarsN stays visible.
            self.generate_expression(expr)
        } else if Self::is_local_var_assignment(expr) {
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
        } else {
            // BT-1942: Detect open-scope results produced by the expression
            // itself (e.g. a ProtoObject/Object intrinsic whose receiver is a
            // class method self-send). Emit the open chain as-is and bind the
            // result var to the seq temp so subsequent code can sequence
            // after it while keeping ClassVarsN in scope.
            let tmp_var = self.fresh_temp_var("seq");
            let (expr_str, open_scope) = self.expression_doc_with_open_scope(expr)?;
            if let Some(result_var) = open_scope {
                Ok(docvec![
                    expr_str,
                    "let ",
                    leaf::var(tmp_var),
                    " = ",
                    leaf::var(result_var),
                    " in "
                ])
            } else {
                Ok(docvec!["let ", leaf::var(tmp_var), " = ", expr_str, " in "])
            }
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
                    .emit_threaded_assign_rhs(
                        &id.name,
                        value,
                        super::super::threaded_expr::ThreadingBoundary::ClassMethod,
                        &mut parts,
                    )?
                    .is_some()
                {
                    return Ok(Document::Vec(parts));
                }
                let var_name = &id.name;
                let core_var = self
                    .lookup_var(var_name)
                    .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                // BT-1201: Use expression_doc_with_open_scope to detect open-scope results.
                let (val_doc, open_scope) = self.expression_doc_with_open_scope(value)?;
                self.bind_var(var_name, &core_var);
                if let Some(open_scope_result) = open_scope {
                    return Ok(docvec![
                        val_doc,
                        "let ",
                        leaf::var(core_var),
                        " = ",
                        leaf::var(open_scope_result),
                        " in "
                    ]);
                }
                return Ok(docvec!["let ", leaf::var(core_var), " = ", val_doc, " in "]);
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
    #[allow(clippy::unused_self)]
    pub(super) fn extract_method_source(&self, method: &MethodDefinition) -> String {
        crate::unparse::unparse_method(method)
    }

    /// BT-851: Checks if an expression is a `value:` call on a Tier 2 block parameter.
    ///
    /// When true, the expression will generate a `{Result, NewState}` tuple via
    /// `generate_block_value_call_stateful()` and must be unpacked by the caller.
    pub(in crate::codegen::core_erlang) fn is_tier2_value_call(&self, expr: &Expression) -> bool {
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            let (is_positional_value_selector, is_value_with_arguments) = match selector {
                crate::ast::MessageSelector::Unary(name) => (name == "value", false),
                crate::ast::MessageSelector::Keyword(parts) => {
                    let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                    (
                        matches!(
                            selector_name.as_str(),
                            "value:" | "value:value:" | "value:value:value:"
                        ),
                        selector_name == "valueWithArguments:",
                    )
                }
                crate::ast::MessageSelector::Binary(_) => (false, false),
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
    // `pub(in crate::codegen::core_erlang)` so `control_flow/conditionals.rs`
    // can rebind captured local-var mutations for a bare `Tier2ValueCall`
    // statement inside a conditional branch, mirroring this file's own
    // `Tier2ValueCall` handling.
    pub(in crate::codegen::core_erlang) fn get_inline_block_captured_mutations(
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
    pub(in crate::codegen::core_erlang) fn generate_tier2_value_call_doc(
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
    pub(in crate::codegen::core_erlang) fn control_flow_has_mutations(
        &self,
        expr: &Expression,
    ) -> bool {
        // BT-2355: see through parentheses so `_r := (1 to: 5 do: [...])` is still
        // classified as control flow with mutations (and thus unpacked + threaded)
        // rather than falling through to a plain pure local assignment.
        let expr = Self::peel_parens(expr);
        let Expression::MessageSend {
            receiver,
            arguments,
            selector: crate::ast::MessageSelector::Keyword(parts),
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
            crate::semantic_analysis::DispatchKind::ControlFlow => true,
            crate::semantic_analysis::DispatchKind::Unknown => {
                crate::state_threading_selectors::is_exception_selector(sel_str.as_str())
                    || crate::state_threading_selectors::is_conditional_selector(sel_str.as_str())
            }
            _ => false,
        };
        if !is_control_flow {
            return false;
        }

        // BT-410: For on:do: and ensure:, the receiver (try body) is also
        // a block that may contain field mutations.
        if crate::state_threading_selectors::is_exception_selector(sel_str.as_str()) {
            if let Expression::Block(block) = receiver.as_ref() {
                // Use pre-computed block profile when available.
                let analysis = self
                    .semantic_facts
                    .block_profile(&block.span)
                    .cloned()
                    .unwrap_or_else(|| block_analysis::analyze_block(block));
                if self.needs_mutation_threading(&analysis) {
                    return true;
                }
            }
        }

        // BT-915: For Boolean conditionals, any block argument may contain mutations.
        // BT-1226: ifNotNil: also needs per-block mutation detection.
        if crate::state_threading_selectors::is_conditional_selector(sel_str.as_str()) {
            for arg in arguments {
                if let Expression::Block(block) = arg {
                    let analysis = self
                        .semantic_facts
                        .block_profile(&block.span)
                        .cloned()
                        .unwrap_or_else(|| block_analysis::analyze_block(block));
                    if self.needs_mutation_threading(&analysis) {
                        return true;
                    }
                    // BT-2356: a nested list op inside a branch may mutate an outer
                    // local even when the branch block itself has no direct mutation
                    // (`analyze_block` does not propagate writes out of nested blocks).
                    // Without this the conditional is classified as pure and the
                    // nested op's mutation is dropped — e.g.
                    // `flag ifTrue: [ items do: [:x | sum := sum + x] ]`.
                    if self.body_has_list_op_cross_scope_mutations(block) {
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
        for arg in arguments {
            if let Expression::Block(block) = arg {
                let analysis = self
                    .semantic_facts
                    .block_profile(&block.span)
                    .cloned()
                    .unwrap_or_else(|| block_analysis::analyze_block(block));
                if self.needs_mutation_threading(&analysis) {
                    return true;
                }
                // BT-1329: Also check for nested list ops with cross-scope mutations.
                // `analyze_block` doesn't propagate local_writes from nested blocks,
                // so variables mutated inside do:/collect:/inject:/select:/reject: blocks
                // are invisible to the standard `needs_mutation_threading` check.
                if self.body_has_list_op_cross_scope_mutations(block) {
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
    pub(in crate::codegen::core_erlang) fn generate_meta_function(
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
        let auto =
            crate::codegen::core_erlang::value_type_codegen::compute_auto_slot_methods(class);
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
        auto: Option<&crate::codegen::core_erlang::value_type_codegen::AutoSlotMethods>,
        include_standalone: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let type_params = &class.type_params;
        let mut entries: Vec<MethodInfoEntry> = class
            .methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, sealed, type_params))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && !m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(
                    &standalone.method,
                    sealed,
                    type_params,
                ));
            }
        }
        if let Some(auto) = auto {
            use crate::codegen::core_erlang::value_type_codegen::AutoSlotMethods;
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
        auto: Option<&crate::codegen::core_erlang::value_type_codegen::AutoSlotMethods>,
        include_standalone: bool,
        synthesize_supervision_spec: bool,
    ) -> Vec<MethodInfoEntry> {
        let sealed = class.is_sealed;
        let type_params = &class.type_params;
        let mut entries: Vec<MethodInfoEntry> = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| Self::meta_method_entry(m, sealed, type_params))
            .collect();
        // BT-1005: Standalone methods are excluded from __beamtalk_meta/0 (runtime-patched)
        // but included in BuilderState.meta so init/1 can register their return types.
        if include_standalone {
            for standalone in module.method_definitions.iter().filter(|m| {
                m.class_name.name == class.name.name
                    && m.is_class_method
                    && m.method.kind == MethodKind::Primary
            }) {
                entries.push(Self::meta_method_entry(
                    &standalone.method,
                    sealed,
                    type_params,
                ));
            }
        }
        if let Some(auto) = auto {
            if let Some(kw_sel) = &auto.keyword_constructor {
                let arity = class.state.len();
                // BT-1408: Hash long keyword constructor selectors to stay within
                // Erlang's 255-char atom limit. The meta selector must match what
                // class_send emits so runtime dispatch finds the method.
                let safe_sel =
                    crate::codegen::core_erlang::selector_mangler::safe_class_method_selector(
                        kw_sel,
                    );
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
    fn meta_method_entry(
        m: &MethodDefinition,
        class_is_sealed: bool,
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
            m.selector.to_erlang_atom(),
            m.selector.arity(),
            return_type,
            param_types,
            m.is_sealed || class_is_sealed,
            m.is_internal,
        )
    }

    /// Converts a `TypeAnnotation` into a `MetaTypeRepr`.
    ///
    /// ADR 0068: If the type name matches one of the class-level type parameters,
    /// it becomes a `TypeParam { name, index }`. Generic types with parameters
    /// become `Generic { base, parameters }`. Method-local type params (not in
    /// `class_type_params`) get index `-1`.
    fn type_annotation_to_meta_repr(
        ta: &crate::ast::TypeAnnotation,
        class_type_params: &[TypeParamDecl],
    ) -> MetaTypeRepr {
        use crate::ast::TypeAnnotation;
        match ta {
            TypeAnnotation::Simple(id) => {
                // Check if this is a class-level type parameter
                if let Some(index) = class_type_params
                    .iter()
                    .position(|tp| tp.name.name == id.name)
                {
                    MetaTypeRepr::TypeParam {
                        name: id.name.to_string(),
                        index: i32::try_from(index).unwrap_or(0),
                    }
                } else if id.name.len() == 1
                    && id
                        .name
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_uppercase())
                {
                    // Single uppercase letter not in class type params → method-local type param
                    MetaTypeRepr::TypeParam {
                        name: id.name.to_string(),
                        index: -1,
                    }
                } else {
                    MetaTypeRepr::Atom(id.name.to_string())
                }
            }
            TypeAnnotation::Generic {
                base, parameters, ..
            } => {
                let params: Vec<MetaTypeRepr> = parameters
                    .iter()
                    .map(|p| Self::type_annotation_to_meta_repr(p, class_type_params))
                    .collect();
                MetaTypeRepr::Generic {
                    base: base.name.to_string(),
                    parameters: params,
                }
            }
            // Union, FalseOr, Singleton, SelfType → fall back to flat atom string
            _ => MetaTypeRepr::Atom(ta.type_name().to_string()),
        }
    }

    /// Renders a `MetaTypeRepr` as a Core Erlang document.
    ///
    /// - `None` → `'none'`
    /// - `Atom("T")` → `'T'`
    /// - `TypeParam { name: "T", index: 0 }` → `{'type_param', 'T', 0}`
    /// - `Generic { base: "Result", params: [TypeParam T, Atom E] }` →
    ///   `{'generic', 'Result', [{'type_param', 'T', 0}, 'E']}`
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
                let mut params: Vec<Document<'static>> = Vec::new();
                params.push(Document::Str("["));
                for (i, p) in parameters.iter().enumerate() {
                    if i > 0 {
                        params.push(Document::Str(", "));
                    }
                    params.push(Self::meta_type_repr_doc(p));
                }
                params.push(Document::Str("]"));
                docvec![
                    "{'generic', ",
                    leaf::atom(base.clone()),
                    ", ",
                    Document::Vec(params),
                    "}"
                ]
            }
        }
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
    use super::{MetaProvenance, MetaTypeRepr, extract_package_from_module_name};
    use crate::ast::{
        ClassDefinition, ClassKind, Expression, ExpressionStatement, Identifier, Literal,
        MessageSelector, MethodDefinition, Module, TypeParamDecl,
    };
    use crate::codegen::core_erlang::CoreErlangGenerator;
    use crate::source_analysis::Span;
    use crate::test_helpers::test_support::make_actor_class;

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

    #[test]
    fn test_generate_register_class_empty_module_renders_empty() {
        let mut generator = CoreErlangGenerator::new("test");
        let module = Module {
            classes: vec![],
            method_definitions: Vec::new(),
            protocols: Vec::new(),
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

    use crate::ast::TypeAnnotation;

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
            output.contains(&CoreErlangGenerator::binary_string_literal(
                "0.4.0-dev+abc123"
            )),
            "beamtalk_version value not baked correctly. Got: {output}"
        );
        assert!(
            output.contains(&CoreErlangGenerator::binary_string_literal("28-16.4")),
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

    use crate::ast::{DeclaredKeyword, StateDeclaration};

    fn slot(name: &str, ty: Option<&str>) -> StateDeclaration {
        StateDeclaration {
            name: Identifier::new(name, s()),
            type_annotation: ty.map(|t| TypeAnnotation::Simple(Identifier::new(t, s()))),
            default_value: None,
            expect: None,
            comments: crate::ast::CommentAttachment::default(),
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
        let raw_kw_sel = crate::synthetic_selectors::keyword_constructor_selector(
            field_names.iter().map(String::as_str),
        );
        assert_eq!(
            *sel,
            crate::codegen::core_erlang::selector_mangler::safe_class_method_selector(&raw_kw_sel),
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
}
