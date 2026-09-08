// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `register_class/0` construction: `ClassBuilder` state, selector maps, and
//! protocol registration (ADR 0038 Phase 3).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Builds the on-load `register_class/0` function that hands a `ClassBuilder`
//! state map to `beamtalk_class_builder:register/1` — method/doc/signature
//! selector maps, class-variable initial values, the `method_xref`/
//! `state_var_xref` indexes ([`super::xref`]), the static `__beamtalk_meta`
//! map ([`super::class_meta`]), and (ADR 0068 Phase 2c) protocol
//! registration.
//!
//! Kind-agnostic: called for actor, value-type, and native-facade classes
//! alike — split out of `gen_server/methods.rs`, which was actor-specific
//! in name but carried this shared logic.

use super::gen_server::extract_package_from_module_name;
use super::value_accessors::has_opaque_native_representation;
use super::{CodeGenContext, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::{
    ClassDefinition, Expression, Literal, MessageSelector, MethodDefinition, MethodKind, Module,
    ProtocolMethodSignature, StateDeclaration, WellKnownSelector,
};
use beamtalk_core::unparse::unparse_method_display_signature;

impl CoreErlangGenerator {
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

            // BT-3439: Per-instance-variable declaration-line index, the
            // state-var analogue of `method_xref_doc` above.
            let state_var_xref_doc = self.build_state_var_xref_list(class);

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
                state_var_xref_doc,
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
        state_var_xref_doc: Document<'static>,
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
                    // BT-3439: per-instance-variable declaration-line index,
                    // the state-var analogue of 'methodXref' above. A list of
                    // maps (like methodXref), not a `~{ }~` map.
                    "'stateVarXref' => ",
                    state_var_xref_doc,
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
                                        super::spec_codegen::type_annotation_to_spec(
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
                                super::spec_codegen::type_annotation_to_spec(
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
}

#[cfg(test)]
mod tests;
