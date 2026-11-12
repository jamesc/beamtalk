// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-method lowering.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the standalone `class_*` functions for class-side methods, the
//! class-method map entries registered with the class process, and the
//! class-method body lowering (local-var bindings, destructuring, `^`-returns
//! and threaded last expressions). Shared by actor and value-type classes, so
//! it lives outside `gen_server/`.

use super::method_frame::{CLASS_SELF_VAR, MethodBoundary, MethodFrame};
use super::selector_mangler::safe_class_method_fn_name;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, threaded_ir};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf::fname;
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::{
    Block, ClassDefinition, Expression, Identifier, Literal, MapPair, MessageSelector,
    MethodDefinition, MethodKind, ParameterDefinition,
};

impl CoreErlangGenerator {
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
        // Populate class variable names for field access validation. These are
        // the class's own `classState:` declarations (BT-3762): the AST class
        // definition is the same own-declaration set that
        // `ClassHierarchy::own_class_variable_names` answers for the
        // hierarchy-sourced callers, so an ancestor's variable is rejected.
        *self.class_var_names_mut() = class
            .class_variables
            .iter()
            .map(|cv| cv.name.name.to_string())
            .collect();

        // Populate class method selectors for self-send routing
        *self.class_method_selectors_mut() = class
            .class_methods
            .iter()
            .filter(|m| m.kind == MethodKind::Primary)
            .map(|m| m.selector.name().to_string())
            .collect();

        // Populate auto-generated keyword constructor selector for Value subclass: classes.
        // This allows `ClassName slot: value` inside a class method to route to the correct
        // class-side constructor instead of falling through to the instance-side getter.
        self.set_class_slot_constructor_selector(
            crate::core_erlang::value_accessors::compute_auto_slot_methods(class)
                .and_then(|auto| auto.keyword_constructor),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();

        for method in &class.class_methods {
            if method.kind != MethodKind::Primary {
                continue;
            }

            let selector_name = method.selector.name();
            // +1 for the ClassSelf parameter (ADR 0130 §3: class variables
            // are neither passed in nor returned)
            let arity = method.selector.arity() + 1;

            let (mut frame, param_vars) =
                MethodFrame::enter(self, method, MethodBoundary::ClassMethod);

            // Detect if method body has ^ inside blocks (needs NLR).
            let needs_nlr = frame
                .semantic_facts
                .has_block_nlr_or_walk(&method.span, &method.body);

            // ADR 0101: On a `native:` Object, a class-side
            // `self delegate` body lowers through the unified FFI boundary,
            // omitting `self` from the arg list (class methods are not
            // instances). Gated to value-type codegen so native *actor* class
            // methods (compiled via the native facade with context=Actor) keep
            // their existing lowering.
            let native_class_delegate = matches!(frame.context, CodeGenContext::ValueType)
                && method.is_self_delegate()
                && class.backing_module.is_some();

            // Generate body as Document and keep it in the Document pipeline (ADR 0089).
            let body_doc: Document<'static> = if native_class_delegate {
                // The delegate body has no NLR; run it through the shared
                // scope anyway so the current token is cleared on this path
                // exactly as on every other class-method body.
                let ((), _) = frame.with_nlr_scope(needs_nlr, |_| ());
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
            } else {
                // `frame`'s `Drop` (pop scope, clear `in_class_method`, restore
                // the selector) runs on the `?` error path as on success.
                frame.class_method_body_doc(method, needs_nlr)?
            };

            // BT-4301: every caller passes the real class object as
            // `ClassSelf` (static guarded direct calls, the in-caller dynamic
            // path, the class process), so the parameter is `ClassSelf` itself
            // and nothing resolves a `nil` (the BT-4029 prologue is gone).
            let doc = docvec![
                "\n",
                fname(safe_class_method_fn_name(&selector_name), arity),
                " = fun (",
                leaf::var(CLASS_SELF_VAR),
                Self::class_method_params_suffix_doc(&param_vars),
                ") ->",
                nest(INDENT, docvec![line(), body_doc,]),
                "\n",
            ];
            docs.push(doc);

            // `frame` drops here, popping the scope, clearing
            // `in_class_method`, and restoring the selector.
        }
        self.class_var_names_mut().clear();
        self.class_method_selectors_mut().clear();
        self.set_class_slot_constructor_selector(None);
        Ok(Document::Vec(docs))
    }

    /// ADR 0084: Lower the `classMethods:` argument of a programmatic
    /// `ClassBuilder` cascade — a map literal whose values are class-method block
    /// literals — into a Core Erlang map whose values are class-method funs.
    ///
    /// Each `#selector => [:self ... | body]` entry becomes
    /// `'selector' => fun (ClassSelf, A1..An) -> ... end`, matching the
    /// compiled `class_<sel>` calling convention so the runtime's fun-dispatch
    /// path installs and invokes it identically. Non-block values, or
    /// blocks whose shape does not match the selector, fall through to ordinary
    /// expression lowering (a computed fun the user supplied).
    ///
    /// `class_var_names` are the keys of the cascade's `classVars:` map; they make
    /// `self.cvar` reads/writes lower as class-variable access (in place, in
    /// the class process — ADR 0130). `class_name` keys the runtime self/`super`
    /// dispatch the funs emit (they have no module export to call).
    pub(in crate::core_erlang) fn generate_class_methods_map_arg(
        &mut self,
        pairs: &[MapPair],
        class_name: &str,
        class_var_names: &[String],
        docs: &mut Vec<super::threaded_ir::ThreadedStmt>,
    ) -> Result<Document<'static>> {
        if pairs.is_empty() {
            return Ok(Document::Str("~{}~"));
        }

        // BT-3777: the builder class-method context is entered per literal-block
        // value only (inside `class_method_map_value_doc`). Keys and computed
        // values are ordinary expressions evaluated by the *enclosing* method,
        // so they lower in its context — `self` there is the enclosing
        // receiver, not the builder class's unbound `ClassSelf`.
        //
        // BT-3791: a computed value can change actor state (a field
        // assignment, a state-threading self-send), so it is split exactly like
        // any other cascade argument and its prelude hoisted into the cascade's
        // `docs`; otherwise its `StateN` rebind is trapped inside the map
        // literal and later cascade messages see an unbound/stale `State`.
        // BT-3802: a computed KEY can change state too (`#{(self bump) => 1}`),
        // and `cascade_operands_need_prelude` counts keys as operands (the
        // literal-container rule), so keys are split exactly like values and
        // interleaved key-then-value in source order: the all-or-nothing hoist
        // then runs every prelude in evaluation order, never value preludes
        // ahead of a later key's. A literal key is pure and stays inline.
        let mut splits = Vec::with_capacity(pairs.len() * 2);
        // Per pair: the key's inline doc (literal) or `None` when it was split.
        let mut inline_keys: Vec<Option<Document<'static>>> = Vec::with_capacity(pairs.len());
        for pair in pairs {
            if matches!(pair.key, Expression::Literal(..)) {
                inline_keys.push(Some(self.expression_doc(&pair.key)?));
            } else {
                inline_keys.push(None);
                splits.push(self.split_cascade_arg(&pair.key)?);
            }
            if matches!(pair.value, Expression::Block(_)) {
                let fun_doc = self.class_method_map_value_doc(
                    &pair.key,
                    &pair.value,
                    class_name,
                    class_var_names,
                )?;
                splits.push((Vec::new(), fun_doc));
            } else {
                splits.push(self.split_cascade_arg(&pair.value)?);
            }
        }
        let span = pairs.first().map_or_else(
            || beamtalk_core::source_analysis::Span::new(0, 0),
            |p| p.value.span(),
        );
        // BT-4296: the map literal evaluates its last pair first, so two
        // computed operands that may run code are hoisted in source order.
        let operands: Vec<&Expression> = pairs.iter().flat_map(|p| [&p.key, &p.value]).collect();
        let racing = self.last_racing_operand(&operands).is_some();
        let mut hoisted = self
            .hoist_cascade_splits(splits, docs, span, racing)
            .into_iter();
        let missing_split =
            || CodeGenError::Internal("classMethods: map entry lost a hoisted operand".to_string());

        let mut parts: Vec<Document<'static>> = vec![Document::Str("~{ ")];
        for (i, inline_key) in inline_keys.into_iter().enumerate() {
            if i > 0 {
                parts.push(Document::Str(", "));
            }
            let key_doc = match inline_key {
                Some(doc) => doc,
                None => hoisted.next().ok_or_else(missing_split)?,
            };
            let val_doc = hoisted.next().ok_or_else(missing_split)?;
            parts.push(key_doc);
            parts.push(Document::Str(" => "));
            parts.push(val_doc);
        }
        parts.push(Document::Str(" }~"));
        Ok(Document::Vec(parts))
    }

    /// Lower the block argument of an incremental
    /// `addClassMethod: #sel body: [block]` setter into a class-method fun,
    /// mirroring how a `classMethods:` map value is lowered. The `key` is the
    /// selector-symbol argument (used to validate the block's parameter count).
    /// The builder class-method context is entered around the class-method fun
    /// only, exactly as the map path does for each literal-block entry. (A
    /// non-block `body:` argument never reaches here — the cascade lowers it as
    /// an ordinary argument in the enclosing method's context.)
    pub(in crate::core_erlang) fn generate_class_method_single_arg(
        &mut self,
        key: &Expression,
        block: &Block,
        class_name: &str,
        class_var_names: &[String],
    ) -> Result<Document<'static>> {
        let value = Expression::Block(block.clone());
        self.class_method_map_value_doc(key, &value, class_name, class_var_names)
    }

    /// Lowers a single `classMethods:` map value: a class-method fun for a literal
    /// block of the right shape, else ordinary expression lowering.
    ///
    /// A block literal whose parameter count does not match the
    /// selector (`self` plus one parameter per selector slot) is rejected at
    /// compile time with a [`CodeGenError::BlockArityError`]. Previously such a
    /// block fell through to ordinary expression lowering, producing a fun of the
    /// wrong arity that crashed with an opaque `error:undef` only when the class
    /// method was first called. A computed (non-block) value cannot have its
    /// arity checked at compile time, so it still falls through here and is
    /// validated at registration time (`beamtalk_class_builder:validate_class_method_arities/2`).
    ///
    /// Only the class-method fun is lowered inside the builder class-method
    /// context (entered and exited here, saving/restoring any enclosing class
    /// context). A computed value is an ordinary expression evaluated by the
    /// enclosing method, so it lowers in that method's context (BT-3777) — a
    /// `self` send in it targets the enclosing receiver, not `ClassSelf`.
    fn class_method_map_value_doc(
        &mut self,
        key: &Expression,
        value: &Expression,
        class_name: &str,
        class_var_names: &[String],
    ) -> Result<Document<'static>> {
        if let Expression::Literal(Literal::Symbol(sym), _) = key {
            if let Some(selector) = super::class_builder_source::selector_from_symbol(sym) {
                if let Expression::Block(block) = value {
                    // A class-method block declares `self` plus one parameter per
                    // selector slot.
                    let expected = selector.arity() + 1;
                    if block.parameters.len() == expected {
                        let saved =
                            self.enter_builder_class_method_context(class_name, class_var_names);
                        let result = self.generate_class_method_fun_from_block(&selector, block);
                        self.exit_builder_class_method_context(saved);
                        return result;
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
        // registration time.
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
    /// `fun (ClassSelf, P1..Pn) -> body` where the block's first
    /// parameter (the receiver) binds to `ClassSelf`, the remaining parameters to
    /// `P1..Pn`, and the body is lowered with the class-method machinery
    /// (self/`super` routed to runtime dispatch
    /// because there is no `class_<sel>` export). Assumes the caller has already
    /// entered the builder class-method context.
    fn generate_class_method_fun_from_block(
        &mut self,
        selector: &MessageSelector,
        block: &Block,
    ) -> Result<Document<'static>> {
        // The fun body executes at runtime as a class method's own top frame,
        // even when the builder cascade lexically sits inside a block or a
        // loop body: it inherits nothing of the enclosing body's per-body
        // state (ADR 0133 Phase 3, BT-3911: the block depth, the class-variable
        // capture of ADR 0130 §5, the loop and branch context, the producer
        // sites and frames), and the enclosing body gets it back on every
        // exit path.
        let mut scope = self.enter_body_scope();
        scope.generate_class_method_fun_in_body_scope(selector, block)
    }

    /// [`Self::generate_class_method_fun_from_block`] once the nested body's
    /// [`BodyScope`](crate::core_erlang::generator::body_state::BodyScope) is
    /// entered.
    fn generate_class_method_fun_in_body_scope(
        &mut self,
        selector: &MessageSelector,
        block: &Block,
    ) -> Result<Document<'static>> {
        self.push_scope();
        self.current_method_params.clear();
        // Reset arithmetic fast-path parameter-type tracking.
        self.clear_method_param_types();
        self.reset_state_version();

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
        let body_doc: Document<'static> = match self.class_method_body_doc(&method, needs_nlr) {
            Ok(doc) => doc,
            Err(e) => {
                self.pop_scope();
                return Err(e);
            }
        };

        let doc = docvec![
            "fun (ClassSelf",
            Self::class_method_params_suffix_doc(&param_vars),
            ") ->",
            nest(INDENT, docvec![line(), body_doc]),
        ];

        self.pop_scope();
        Ok(doc)
    }

    /// The body of a class-side method, shared by compiled class methods, `ClassBuilder`
    /// funs and class-side extension funs (ADR 0130 §3): the bare result, with a
    /// real `NlrCatch` prepended when `needs_nlr` (the token is minted by
    /// [`Self::with_nlr_scope`] before the body lowers, production's real mint
    /// order) and the whole sequence verified and rendered once. Class variables
    /// are read and written in place, so nothing is wrapped or returned beside
    /// the result. An empty body returns `ClassSelf`. Clears the current NLR
    /// token on every path.
    pub(in crate::core_erlang) fn class_method_body_doc(
        &mut self,
        method: &MethodDefinition,
        needs_nlr: bool,
    ) -> Result<Document<'static>> {
        if method.body.is_empty() {
            let ((), _) = self.with_nlr_scope(needs_nlr, |_| ());
            return Ok(docvec!["ClassSelf"]);
        }
        // BT-3719: the class key is bound once around the whole body.
        self.with_class_var_key_binding(|this| {
            let (body_stmts_result, nlr_token_var) =
                this.with_nlr_scope(needs_nlr, |g| g.lower_class_method_body(method));
            let mut body_stmts = body_stmts_result?;
            if let Some(token_var) = nlr_token_var {
                body_stmts.insert(
                    0,
                    threaded_ir::ThreadedStmt::NlrCatch {
                        boundary: super::NlrBoundary::ClassMethod,
                        token: threaded_ir::TokenId::new(token_var),
                        frame: threaded_ir::FrameId::ROOT,
                        span: method.span,
                    },
                );
            }
            Ok(this.verify_and_render_body_stmts(&body_stmts, method.span))
        })
    }

    /// Builds the trailing fun parameter list `, P1, P2, …` as `Document` pieces
    /// (never `format!` — Core Erlang fragments must use the Document API,
    /// ADR 0089). Empty when there are no user parameters.
    fn class_method_params_suffix_doc(param_vars: &[String]) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for var in param_vars {
            parts.push(Document::Str(", "));
            parts.push(leaf::var(var.clone()));
        }
        Document::Vec(parts)
    }

    /// Lowers the body of a class-side method to one straight-line
    /// `Vec<ThreadedStmt>` (ADR 0111 Addendum 4/6: mirrors
    /// `lower_body_exprs_with_reply`'s pattern for the Actor pipeline, for
    /// the class-method body pipeline the Actor-pipeline migration explicitly
    /// left as a hand-written `Document` builder).
    ///
    /// Class methods have no state threading at all: class variables are
    /// read and written in place in the class process (ADR 0130 §3), so no
    /// body statement produces a version `Bind`. Every body statement —
    /// local-var bindings, destructuring, `^`-returns, class-method
    /// self-sends — is an opaque [`threaded_ir::ThreadedStmt::Statement`]
    /// built by the `generate_class_method_*` codegen calls below.
    fn lower_class_method_body(
        &mut self,
        method: &MethodDefinition,
    ) -> Result<Vec<threaded_ir::ThreadedStmt>> {
        use threaded_ir::ThreadedStmt;

        // Filter out @expect directives (compile-time only, no runtime representation).
        let body = super::util::collect_body_exprs(&method.body);
        self.with_local_producer_sites(&body, |this| {
            let mut stmts: Vec<ThreadedStmt> = Vec::new();
            let body_len = body.len();
            for (i, expr) in body.iter().enumerate() {
                let is_last = i == body_len - 1;
                let span = expr.span();

                if let Expression::Return { value, .. } = expr {
                    let doc = this.generate_class_method_return(value)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                    return Ok(stmts);
                }

                // ADR 0131 Phase 2: a local-threading producer in this
                // statement rebinds its locals at the method root.
                if let Some(result_var) =
                    this.lower_root_statement_with_producer(expr, &mut stmts)?
                {
                    if is_last {
                        stmts.push(ThreadedStmt::Statement(leaf::var(result_var), span));
                    }
                    continue;
                }

                if is_last {
                    let doc = this.generate_class_method_last_expr(expr)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                } else {
                    let doc = this.generate_class_method_non_last_expr(expr)?;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                }
            }
            Ok(stmts)
        })
    }

    /// ADR 0131 Phase 2: lowers a method-body statement of a class or
    /// value-type method that holds a local-threading producer — `x :=
    /// <loop or fold>`, or any statement with one at a sequenced operand
    /// site — through [`Self::threaded_expression`], splicing its prelude
    /// (the construct tuple and the `LocalRebind`s) into `stmts` so the
    /// rebinds are visible to the statements after it. Returns the variable
    /// bound to the statement's value, or `Ok(None)`, with nothing pushed,
    /// for any other statement.
    ///
    /// **Invariant** (BT-4014, every method-root scope this lowers: a class
    /// method, a value-type method, and so the `TestCase` and plain-object
    /// instance methods compiled as value types): each local-threading
    /// producer at a sequenced operand site of `expr` (`sequenced_operand_sites`,
    /// the set [`Self::with_local_producer_sites`] registers; a value-position
    /// local assignment there too, BT-3910) has its
    /// [`ThreadedStmt::ConstructTuple`], threading every local it writes, at
    /// the top level of the prelude spliced into `stmts`, so its
    /// `LocalRebind`s are in scope for every later statement of the method.
    /// A producer compiled inside an operand the statement's own lowering
    /// closes into a `Document` (a class-side self-send's argument list) is
    /// scoped inside it and the next read of the local is unbound. Checked
    /// here as [`VerifyError::ProducerSiteNotThreaded`], the BT-3939 rule a
    /// Tier 2 block body and the arm and loop frames apply to their own
    /// statements.
    ///
    /// [`VerifyError::ProducerSiteNotThreaded`]: threaded_ir::VerifyError::ProducerSiteNotThreaded
    pub(in crate::core_erlang) fn lower_root_statement_with_producer(
        &mut self,
        expr: &Expression,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
    ) -> Result<Option<String>> {
        // Read before the statement is lowered, by the one rule every frame
        // reads (`producer_site_locals`).
        let sites: Vec<(beamtalk_core::source_analysis::Span, Vec<String>)> = self
            .root_producer_site_exprs(expr)
            .into_iter()
            .filter_map(|site| {
                self.producer_site_locals(site)
                    .map(|locals| (site.span(), locals))
            })
            .collect();
        let start = stmts.len();
        let result = self.lower_root_statement_with_producer_in_place(expr, stmts)?;
        // Checked whether or not the statement took the producer path: a
        // statement whose sites hold a producer but which the gate above sends
        // to the opaque lowering (`None`, nothing pushed) is the same trapped
        // rebind, reported rather than left to `erlc`.
        let errors = threaded_ir::verify_producer_sites_threaded(&stmts[start..], &sites);
        self.report_threaded_ir_verify_errors(
            &errors,
            "method-root statement's local-threading producer whose rebinds are not \
             spliced into the method body",
            expr.span(),
        );
        Ok(result)
    }

    /// [`Self::lower_root_statement_with_producer`] before its check.
    fn lower_root_statement_with_producer_in_place(
        &mut self,
        expr: &Expression,
        stmts: &mut Vec<threaded_ir::ThreadedStmt>,
    ) -> Result<Option<String>> {
        use threaded_ir::{FrameId, ThreadedStmt};
        let span = expr.span();
        let assign = match expr {
            Expression::Assignment { target, value, .. } => match target.as_ref() {
                Expression::Identifier(id) => Some((id.name.as_str(), value.as_ref())),
                _ => None,
            },
            _ => None,
        };
        let direct = assign
            .is_some_and(|(_, value)| self.construct_tuple_locals(value.unwrap_parens()).is_some());
        // ADR 0131 Phase 3 (BT-3738): a method-body statement that is itself a
        // conditional producer (`c ifTrue: [t := t + 1]`, `x ifNil: [...]`,
        // `c and: [...]`) is lowered through the producer too, in a class or
        // value-type method, so its locals come back through `LocalRebind`s.
        // ADR 0131 Phase 3b (BT-3817): so is an `on:do:`/`ensure:` statement.
        let conditional_statement = assign.is_none()
            && !matches!(expr, Expression::Return { .. })
            && (self
                .conditional_producer_locals(expr.unwrap_parens())
                .is_some()
                || self.exception_producer_in_frame(expr));
        if !direct
            && !conditional_statement
            && !self.statement_has_local_producer(expr)
            && !self.statement_has_value_type_producer(expr)
        {
            return Ok(None);
        }
        let Some((var_name, value)) = assign else {
            let tmp_var = self.fresh_temp_var("seq");
            let tv = self.threaded_expression(expr, FrameId::ROOT)?;
            stmts.extend(tv.prelude);
            let value_doc = self.threaded_value_doc(&tv.value);
            stmts.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(tmp_var.clone()), " = ", value_doc, " in "],
                span,
            ));
            return Ok(Some(tmp_var));
        };
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
        let tv = if direct {
            self.threaded_construct_value(value, FrameId::ROOT)?
                .ok_or_else(|| {
                    CodeGenError::Internal(
                        "construct_tuple_locals accepted a non-construct".to_string(),
                    )
                })?
        } else {
            self.threaded_expression(value, FrameId::ROOT)?
        };
        stmts.extend(tv.prelude);
        let value_doc = self.threaded_value_doc(&tv.value);
        self.bind_var(var_name, &core_var);
        stmts.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(core_var.clone()),
                " = ",
                value_doc,
                " in "
            ],
            span,
        ));
        Ok(Some(core_var))
    }

    /// Generates code for an explicit `^` return in a class method.
    fn generate_class_method_return(&mut self, value: &Expression) -> Result<Document<'static>> {
        // An explicit `^` return of a value-type threading construct
        // (counted/while loop, foldl list-op, or read+write conditional) must
        // unwrap the construct's logical value rather than leak the raw
        // `{value, StateAcc}` tuple (or crash dispatching a read+write
        // conditional's stateful block at the wrong arity). This mirrors the
        // implicit last-expression path (`generate_class_method_last_expr`).
        if let Some(doc) = self.try_generate_class_method_threaded_last(value)? {
            return Ok(doc);
        }
        // ADR 0118 phase 5b: the prelude and value simply concatenate.
        let frame = self.current_frame();
        self.threaded_expression_doc(value, frame)
    }

    /// Generates code for the last expression in a class method body.
    fn generate_class_method_last_expr(&mut self, expr: &Expression) -> Result<Document<'static>> {
        // BT-3889: a destructuring assignment evaluates to its right-hand side.
        if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let (mut docs, rhs_var) =
                self.generate_destructure_bindings_with_value(pattern, value)?;
            docs.push(leaf::var(rhs_var));
            return Ok(Document::Vec(docs));
        }
        // A last-position threading construct (counted/while loop or foldl list-op
        // yielding a `{value, StateAcc}` tuple) or a read+write conditional must unwrap the
        // construct's logical value rather than leak the raw tuple (or crash on the 0-arg
        // stateful-block dispatch).
        if let Some(doc) = self.try_generate_class_method_threaded_last(expr)? {
            return Ok(doc);
        }
        // ADR 0118 phase 5b: the prelude and value simply concatenate.
        let frame = self.current_frame();
        self.threaded_expression_doc(expr, frame)
    }

    /// Handles a class method's last expression when it is a value-type threading
    /// construct (counted/while loop or foldl list-op) or a read+write conditional.
    ///
    /// Returns `None` when `expr` is neither, so the caller falls back to the standard
    /// last-expression paths.
    ///
    /// Both shapes produce a logical value bound to a fresh result var (via the shared
    /// value-type primitives), which is returned bare.
    fn try_generate_class_method_threaded_last(
        &mut self,
        expr: &Expression,
    ) -> Result<Option<Document<'static>>> {
        // Route through the shared `ThreadedExpr` transform + boundary emitter.
        // It peels redundant parentheses (e.g. `^(items collect: …)` or
        // `(flag ifTrue: [...])`) so the threading construct inside is unwrapped to its
        // logical value rather than leaking its raw `{value, StateAcc}` tuple. Applies to
        // both the explicit `^`-return and the implicit last-expression callers.
        let mut parts: Vec<Document<'static>> = Vec::new();
        if self.emit_threaded_last(
            expr,
            super::threaded_expr::ThreadingBoundary::ClassMethod,
            &mut parts,
        )? {
            Ok(Some(Document::Vec(parts)))
        } else {
            Ok(None)
        }
    }

    /// Generates code for a non-last expression in a class method body.
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
            // Non-last `do:` loop that mutates captured outer locals.
            self.generate_value_type_do_open(expr)
        } else if self.is_counted_loop_with_vt_local_threading(expr) {
            // Non-last counted loop (to:do:/to:by:do:/timesRepeat:) that
            // mutates captured outer locals. Extracts the threaded locals from the
            // `{'nil', StateAcc}` tuple so subsequent statements see the updates.
            self.generate_vt_counted_loop_open(expr)
        } else if self.is_while_with_vt_local_threading(expr) {
            // Non-last whileTrue:/whileFalse: that mutates captured outer locals.
            self.generate_vt_while_open(expr)
        } else if self.is_foldl_list_op_with_vt_local_threading(expr) {
            // Non-last collect:/select:/reject:/inject:into: that mutates captured
            // outer locals. Extracts the threaded locals from the `{value, StateAcc}` tuple
            // (the logical value is discarded in non-last position).
            self.generate_vt_foldl_list_op_open(expr)
        } else if self.is_conditional_with_vt_local_threading(expr) {
            // Non-last conditional that mutates captured outer locals.
            self.generate_vt_conditional_open(expr)
        } else if self.is_exception_construct_with_vt_local_threading(expr) {
            // Non-last on:do:/ensure: that mutates captured outer
            // locals. Extracts the threaded locals from the returned
            // `{Result, StateAcc}` tuple, same idiom as the loop/conditional
            // arms above.
            self.generate_vt_exception_construct_open(expr)
        } else {
            // Bind the result to the seq temp so subsequent code can
            // sequence after it.
            let tmp_var = self.fresh_temp_var("seq");
            let expr_doc = self.expression_doc(expr)?;
            Ok(docvec!["let ", leaf::var(tmp_var), " = ", expr_doc, " in ",])
        }
    }

    /// Local variable assignment in class method — create a proper `let` binding.
    fn generate_class_method_local_var_binding(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let var_name = &id.name;
                let core_var = self
                    .lookup_var(var_name)
                    .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
                let val_doc = self.expression_doc(value)?;
                self.bind_var(var_name, &core_var);
                return Ok(docvec!["let ", leaf::var(core_var), " = ", val_doc, " in ",]);
            }
        }
        Ok(Document::Nil)
    }
}
