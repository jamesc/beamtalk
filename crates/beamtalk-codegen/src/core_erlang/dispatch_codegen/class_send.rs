// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-side send lowering.
//!
//! Builds the class-method call forms: runtime lookup, guarded direct calls to
//! class methods and constants, REPL-bound class sends, and the dynamic
//! class-send fallback.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::threaded_ir::ThreadedStmt;
use crate::core_erlang::{CodeGenError, CoreErlangGenerator, Result};

use super::ClassSendArgs;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};

// NOTE: class_method_module_name and related helpers (is_primitive_stdlib_class,
// is_bt_stdlib_class, is_erlang_stdlib_module) have been removed.
// Class dispatch now goes through runtime class_send/3 instead of
// compile-time module name resolution.

impl CoreErlangGenerator {
    /// Generates a method lookup via `>>` operator.
    ///
    /// `Counter >> #increment` compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'('Counter', 'increment')
    /// ```
    ///
    /// Returns a `CompiledMethod` map with selector, source, and arity metadata.
    pub(super) fn generate_method_lookup(
        &mut self,
        class_name: &str,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        let arg_doc = self.expression_doc(&arguments[0])?;
        let doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            leaf::atom(class_name.to_string()),
            ", ",
            arg_doc,
            ")"
        ];
        Ok(doc)
    }

    /// Generates a runtime method resolution via `>>` for non-class-literal receivers.
    ///
    /// `cls >> #increment` (where cls holds a class object) compiles to:
    /// ```erlang
    /// call 'beamtalk_method_resolver':'resolve'(cls, 'increment')
    /// ```
    ///
    /// The `MethodResolver` domain service accepts pids, atoms, and class object tuples.
    pub(super) fn generate_runtime_method_lookup(
        &mut self,
        receiver: &Expression,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        if arguments.len() != 1 {
            return Err(CodeGenError::Internal(format!(
                ">> operator requires exactly one argument, got {}",
                arguments.len()
            )));
        }
        // Capture receiver + arg as one ordered sequence so
        // left-to-right evaluation order is preserved.
        let exprs: [&Expression; 2] = [receiver, &arguments[0]];
        let (preamble, mut docs) = self.thread_subexprs(&exprs, "Lookup")?;
        let arg_doc = docs.pop().expect("arg");
        let actual_receiver = docs.pop().expect("receiver");

        let call_doc = docvec![
            "call 'beamtalk_method_resolver':'resolve'(",
            actual_receiver,
            ", ",
            arg_doc,
            ")"
        ];
        Ok(self.close_prelude(&preamble, call_doc, "MethodLookup"))
    }

    /// Generates a class-side send at REPL top level, where the receiver name may
    /// be a session binding (`Workspace bind:as:`) rather than a class
    /// (ADR 0129 §7: session locals, then `bind:as:` entries, then the class
    /// registry).
    ///
    /// Only reached for [`CodeGenContext::Repl`]; methods and batch-compiled
    /// code never consult the session map and go straight through
    /// [`Self::generate_class_method_call`]. There is no singleton step: the
    /// system facades are ordinary classes (ADR 0129).
    ///
    /// ```erlang
    /// let Lookup = call 'maps':'find'('Name', State) in
    /// case Lookup of
    ///   <{'ok', Bound}> -> call 'beamtalk_message_dispatch':'send'(Bound, Sel, Args)
    ///   <'error'>       -> %% direct call or class_send, as generate_class_method_call
    /// end
    /// ```
    pub(super) fn generate_repl_bound_class_send(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Document<'static>> {
        // The bound-value branch dispatches to an instance, so it uses the raw
        // selector; the class fallback uses the class-method mangled selector.
        let raw = selector.name().to_string();
        let instance_selector = crate::core_erlang::selector_mangler::safe_atom_name(&raw);
        let bound_var = self.fresh_var("BindingVal");
        let state_var = self.current_state_var();
        let lookup_var = self.fresh_temp_var("Lookup");

        // Receiver first, then args: bind the session lookup before the arg
        // preamble, and bind args to temps so each is evaluated exactly once.
        let (arg_prelude, arg_refs) = self.thread_args_bound(arguments, "BindArg")?;
        let args_doc = Self::join_docs_with_commas(arg_refs);

        let class_fallback: Document<'static> =
            if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw) {
                self.guarded_class_direct_call_doc(
                    class_name,
                    module_name,
                    &raw,
                    args_doc.clone(),
                    arguments.len(),
                )
            } else {
                self.generate_class_send_fallback(class_name, &raw, args_doc.clone())
            };

        let lookup_binding = docvec![
            "let ",
            leaf::var(lookup_var.clone()),
            " = call 'maps':'find'(",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::var(state_var),
            ") in ",
        ];
        let case_doc = docvec![
            "case ",
            leaf::var(lookup_var),
            " of ",
            "<{'ok', ",
            leaf::var(bound_var.clone()),
            "}> when 'true' -> ",
            "call 'beamtalk_message_dispatch':'send'(",
            leaf::var(bound_var),
            ", ",
            leaf::atom(instance_selector),
            ", [",
            args_doc,
            "]) ",
            "<'error'> when 'true' -> ",
            class_fallback,
            " end"
        ];

        // ADR 0118 phase 5b: thread the lookup ahead of the arg prelude, then
        // close (this returns a bare `Document`).
        let mut prelude = vec![ThreadedStmt::Statement(
            lookup_binding,
            beamtalk_core::source_analysis::Span::default(),
        )];
        prelude.extend(arg_prelude);
        Ok(self.close_prelude(&prelude, case_doc, "BindClassRes"))
    }

    /// Generates a class-level method call to a literal class receiver.
    ///
    /// ADR 0142 §2 (BT-4301): when the class itself defines the selector and
    /// may run it in the caller ([`Self::direct_call_eligible_module`], the
    /// one `ClassHierarchy::is_direct_call_eligible` rule), the send compiles
    /// to the guarded direct call of [`Self::guarded_class_direct_call_doc`].
    /// Everything else, an inherited class method included, takes the dynamic
    /// path (registered-name `whereis` + `class_send`), which itself runs a stateless
    /// class method in the caller (BT-4300). A send that resolves to one of
    /// `Exception`'s caller-safe signal primitives, own or inherited
    /// (`Error signal: m`), raises in the caller behind its own key instead
    /// ([`Self::guarded_caller_safe_raise_doc`], BT-4115, BT-4270).
    ///
    /// # Generated Code (dynamic path)
    ///
    /// ```erlang
    /// let ClassPid = call 'erlang':'whereis'('beamtalk_class_Tracing') in
    /// call 'beamtalk_object_class':'class_send'(ClassPid, 'setContext:', [Ctx])
    /// ```
    ///
    /// `arguments` may be already bound ([`ClassSendArgs::Bound`]): the
    /// explicit own-class send of [`Self::generate_own_class_literal_send`]
    /// splices this lowering into one arm of its receiver-tag `case`.
    pub(super) fn generate_class_method_call(
        &mut self,
        class_name: &str,
        selector: &MessageSelector,
        arguments: ClassSendArgs<'_>,
    ) -> Result<Document<'static>> {
        let raw_selector = selector.name().to_string();

        // nullary `new` on an eligible Value builds via a direct
        // `Module:'new'()` call, behind the same static call key as every
        // other guarded class send (BT-4409). (`new:` stays on `class_send`:
        // its chain verdict is decided at run time, on the dynamic path.)
        if raw_selector == "new" && arguments.is_empty() {
            if let Some(module_name) = self.direct_new0_module(class_name) {
                return Ok(self.guarded_direct_new_doc(class_name, module_name));
            }
        }

        if let Some((module_name, primitive)) =
            self.caller_safe_signal_target(class_name, &raw_selector)
        {
            let (args_preamble, arg_refs) = self.thread_class_send_args_bound(arguments)?;
            let call_doc = self.guarded_caller_safe_raise_doc(
                class_name,
                module_name,
                &raw_selector,
                &primitive,
                arg_refs,
            )?;
            return Ok(self.close_prelude(&args_preamble, call_doc, "SignalCall"));
        }

        if let Some(module_name) = self.direct_call_eligible_module(class_name, &raw_selector) {
            // Bind every argument once: the doc is spliced into both arms of
            // the guard (an inline block argument holding further guarded
            // sends would otherwise double per nesting level).
            let (args_preamble, arg_refs) = self.thread_class_send_args_bound(arguments)?;
            let args_doc = Self::join_docs_with_commas(arg_refs);
            let call_doc = self.guarded_class_direct_call_doc(
                class_name,
                module_name,
                &raw_selector,
                args_doc,
                arguments.len(),
            );
            return Ok(self.close_prelude(&args_preamble, call_doc, "DirectCall"));
        }

        // Fallback: gen_server dispatch via class_send
        // Hash long selector atoms (e.g. keyword constructors with many
        // fields) to stay within Erlang's 255-char atom limit.
        // Hoist preambles from sub-expression class var mutations.
        let selector_atom =
            crate::core_erlang::selector_mangler::safe_class_method_selector(&raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        let (args_preamble, args_doc) = self.thread_class_send_args(arguments)?;

        let call_doc = docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = ",
            self.registered_class_pid_or_name_doc(class_name),
            " in ",
            self.class_send_call_doc(&class_pid_var, selector_atom, args_doc),
        ];

        Ok(self.close_prelude(&args_preamble, call_doc, "ClassCall"))
    }

    /// ADR 0142 §2 (BT-4301, BT-4316): the guarded direct call of a static
    /// send of `raw_selector`, which `class_name` itself defines, to the
    /// literal class `class_name` compiled to `module_name`. `args_doc` must
    /// be variables or literals: it is spliced into both arms.
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'(
    ///         'Point class static calls', ~{}~) of
    ///     <~{{'bt@point', 'class_origin', 'Point class'} := StaticRef}~> when 'true' ->
    ///       StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(
    ///           {'bt@point', 'class_origin', 'Point class'}, 'Point', 'origin', 1)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' ->
    ///     let ClassPid = call 'erlang':'whereis'('beamtalk_class_Point') in
    ///     call 'beamtalk_object_class':'class_send'(ClassPid, 'origin', [])
    ///   <_> when 'true' -> call 'bt@point':'class_origin'(StaticSelf)
    /// end
    /// ```
    ///
    /// The guard is the contract (ADR 0025 amendment). The hit path is one
    /// `persistent_term` read of the class's static call map (BT-4442: one
    /// key per class, `'<Name> class static calls'`, so a metadata write
    /// erases one key per class, not one per warm target) and a map match of
    /// the call target's key (BT-4316), whose value is the class reference
    /// the callee gets as `ClassSelf`. The runtime installs the key
    /// (`beamtalk_class_shadow_flags:static_call_target/4`) only while
    /// `direct_call_ok/2` holds (nothing shadows the compiled method: no
    /// class-side extension, no ADR 0084 runtime fun; neither the class nor
    /// an ancestor declares class variables), the dynamic path would run
    /// exactly this function in the caller, the function is exported and the
    /// class process is alive, and erases it on every class-metadata write
    /// that could change one of those answers. Otherwise the fallback is the
    /// dynamic `class_send`, byte for byte, so a removed method answers what a
    /// dynamic send answers instead of `undef`. The direct call stays in tail
    /// position: tail recursion through a literal class reference runs in
    /// constant stack.
    ///
    /// The target's map key is the call-site literal `{Module, FunName, Tag}`,
    /// passed whole to the runtime, which never spells it. The metaclass tag keeps a call
    /// site naming one class from hitting a key another class installed for
    /// the same module (after `renameTo:`, or a removed class's module name
    /// reused): such a site misses, and its own miss path answers for the
    /// class it names (`class_not_found` once that class is gone). The callee gets the real class
    /// reference (its tag, its own module and its registered pid, the value a
    /// class literal evaluates to), never `nil` (BT-4037); the pid is never
    /// baked: the key is erased when the class process goes down, so a
    /// restarted class process is found.
    pub(in crate::core_erlang) fn guarded_class_direct_call_doc(
        &mut self,
        class_name: &str,
        module_name: String,
        raw_selector: &str,
        args_doc: Document<'static>,
        arg_count: usize,
    ) -> Document<'static> {
        if let Some(value) = self.constant_class_method_value(class_name, raw_selector) {
            return self.guarded_constant_doc(
                class_name,
                module_name,
                raw_selector,
                &value,
                args_doc,
                arg_count,
            );
        }
        let safe_fn = crate::core_erlang::selector_mangler::safe_class_method_fn_name(raw_selector);
        let class_self_var = self.fresh_temp_var("StaticSelf");
        let comma = if arg_count == 0 { "" } else { ", " };
        let key_doc = docvec![
            "{",
            leaf::atom(module_name.clone()),
            ", ",
            leaf::atom(safe_fn.clone()),
            ", ",
            leaf::atom(crate::core_erlang::util::metaclass_tag(class_name)),
            "}"
        ];
        // BT-4366: the direct call sits inside the guard `case`, so it carries
        // the send's line itself.
        let direct_call = self.annotate_current_send_call(docvec![
            "call ",
            leaf::atom(module_name),
            ":",
            leaf::atom(safe_fn),
            "(",
            leaf::var(class_self_var.clone()),
            comma,
            args_doc.clone(),
            ")"
        ]);
        self.keyed_static_send_doc(
            class_name,
            raw_selector,
            &key_doc,
            &class_self_var,
            args_doc,
            arg_count + 1,
            direct_call,
        )
    }

    /// The literal that `class_name`'s own class-side method `raw_selector`
    /// is, when its body is exactly that scalar literal
    /// (`MethodInfo::constant_value`, BT-4363; every kind of
    /// [`ConstantValue`](beamtalk_core::ast::ConstantValue) since BT-4504).
    /// Only a compile-time candidate: the runtime key of
    /// [`Self::guarded_constant_doc`] decides each send.
    fn constant_class_method_value(
        &self,
        class_name: &str,
        raw_selector: &str,
    ) -> Option<beamtalk_core::ast::ConstantValue> {
        self.class_hierarchy
            .as_ref()?
            .get_class(class_name)?
            .class_methods
            .iter()
            .find(|m| m.selector == raw_selector)?
            .constant_value
            .clone()
    }

    /// BT-4363: a static send of `raw_selector` to the literal class
    /// `class_name`, whose own class-side method compiled to `module_name`
    /// has the scalar literal `value` as its whole body
    /// ([`Self::constant_class_method_value`]), answers `value` in place,
    /// behind the keyed shape of [`Self::guarded_class_direct_call_doc`]:
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'('Errs class static calls', ~{}~) of
    ///     <~{{'constant_value', 1, 'bt@errs', 'class_protocolError', 'Errs class'}
    ///         := StaticRef}~> when 'true' -> StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(
    ///           <same key>, 'Errs', 'protocolError', 1)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' -> <the dynamic class_send to 'Errs'>
    ///   <_> when 'true' -> 1
    /// end
    /// ```
    ///
    /// The guard is the contract (ADR 0025 amendment): the runtime installs
    /// the key only while every condition of a direct-call key holds (the
    /// dynamic path would run exactly `module_name`'s compiled function in
    /// the caller) and the class's published metadata row maps the selector
    /// to this same value (`constant_class_methods`, published from the
    /// module's `__beamtalk_meta/0` on registration and reload;
    /// `beamtalk_class_shadow_flags`, `key_body_holds/4`). A class-side
    /// extension, a runtime fun (`put_class_method`), `removeSelector:` or a
    /// reload that changes the body erases or declines the key, and the send
    /// then answers what the dynamic send answers. Sealing plays no part: the
    /// guard already proves the target (ADR 0025 amendment §3).
    ///
    /// The key and the hit arm carry `value` as the term the method body
    /// compiles to ([`Self::constant_value_doc`], BT-4504): a string is the
    /// tagged `{'beamtalk_string', #{…}#}`, a symbol, `nil`, `true` or
    /// `false` an atom, a character its code point. Map-key matching and the
    /// runtime certification are both exact (`=:=`), so `1` never answers
    /// for `1.0`.
    ///
    /// `args_doc` must be variables or literals (bound by the caller, so a
    /// side-effecting argument still runs once); the hit arm ignores them.
    fn guarded_constant_doc(
        &mut self,
        class_name: &str,
        module_name: String,
        raw_selector: &str,
        value: &beamtalk_core::ast::ConstantValue,
        args_doc: Document<'static>,
        arg_count: usize,
    ) -> Document<'static> {
        let value_doc = Self::constant_value_doc(value);
        let key_doc = docvec![
            "{'constant_value', ",
            value_doc.clone(),
            ", ",
            leaf::atom(module_name),
            ", ",
            leaf::atom(
                crate::core_erlang::selector_mangler::safe_class_method_fn_name(raw_selector)
            ),
            ", ",
            leaf::atom(crate::core_erlang::util::metaclass_tag(class_name)),
            "}"
        ];
        let class_self_var = self.fresh_temp_var("StaticSelf");
        self.keyed_static_send_doc(
            class_name,
            raw_selector,
            &key_doc,
            &class_self_var,
            args_doc,
            arg_count + 1,
            value_doc,
        )
    }

    /// BT-4409 (ADR 0142 rule 3): a static nullary `new` sent to the literal
    /// class `class_name`, whose synthesized `new/0` in `module_name` may
    /// build in the caller ([`Self::direct_new0_module`]), compiles to the
    /// keyed shape of [`Self::guarded_class_direct_call_doc`]:
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'(
    ///         'Point class static calls', ~{}~) of
    ///     <~{{'bt@point', 'new', 'Point class'} := StaticRef}~> when 'true' -> StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(
    ///           {'bt@point', 'new', 'Point class'}, 'Point', 'new', 0)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' ->
    ///     let ClassPid = call 'erlang':'whereis'('beamtalk_class_Point') in
    ///     call 'beamtalk_object_class':'class_send'(ClassPid, 'new', [])
    ///   <_> when 'true' -> call 'bt@point':'new'()
    /// end
    /// ```
    ///
    /// `new/0` takes no `ClassSelf`, so the key's class reference is only the
    /// "installed" witness. The runtime installs the key only while the
    /// dynamic path would build in the caller too
    /// (`beamtalk_class_dispatch:lookup_direct_constructor/3`, which needs
    /// `direct_call_ok/2`: no class-side extension, no ADR 0084 runtime fun,
    /// no class variables in the chain), so a later extension or
    /// `put_class_method` of `new` is honoured through the fallback.
    fn guarded_direct_new_doc(
        &mut self,
        class_name: &str,
        module_name: String,
    ) -> Document<'static> {
        let class_self_var = self.fresh_temp_var("StaticSelf");
        let key_doc = docvec![
            "{",
            leaf::atom(module_name.clone()),
            ", ",
            leaf::atom("new".to_string()),
            ", ",
            leaf::atom(crate::core_erlang::util::metaclass_tag(class_name)),
            "}"
        ];
        let direct_call = self.annotate_current_send_call(docvec![
            "call ",
            leaf::atom(module_name),
            ":",
            leaf::atom("new".to_string()),
            "()"
        ]);
        self.keyed_static_send_doc(
            class_name,
            "new",
            &key_doc,
            &class_self_var,
            Document::Nil,
            0,
            direct_call,
        )
    }

    /// BT-4270: a class-literal send that resolves to one of `Exception`'s
    /// caller-safe signal primitives ([`Self::caller_safe_signal_target`])
    /// raises in the caller, as BT-4115 made it, but only behind a static
    /// call key, so a later `class signal:` override, class-side extension,
    /// runtime fun or reload is honoured:
    ///
    /// ```erlang
    /// let StaticSelf = case call 'persistent_term':'get'(
    ///         'Error class static calls', ~{}~) of
    ///     <~{{'caller_safe_raise', 'classSignal:', 'bt@stdlib@exception', 'class_signal:',
    ///         'Error class'} := StaticRef}~> when 'true' -> StaticRef
    ///     <_> when 'true' ->
    ///       call 'beamtalk_class_shadow_flags':'static_call_target'(<same key>, 'Error', 'signal:', 2)
    ///   end
    /// in case StaticSelf of
    ///   <'false'> when 'true' -> <the dynamic class_send to 'Error'>
    ///   <_> when 'true' ->
    ///     call 'beamtalk_exception_handler':'class_signal_message'(Arg1, 'Error')
    /// end
    /// ```
    ///
    /// The key names the defining class's compiled method and the receiving
    /// class's tag; the runtime installs it only while every condition of a
    /// direct-call key holds for that method (the dynamic path would run it in
    /// the caller for this receiver) and the defining module's metadata maps
    /// the selector to the key's primitive, i.e. its body is the raise the hit
    /// arm performs (`beamtalk_class_shadow_flags`, `key_body_holds/3`).
    /// The hit arm raises exactly what that body raises: the primitive with
    /// the receiving class's name in place of its `ClassSelf`. `arg_refs` must
    /// be variables or literals: they are spliced into both arms.
    fn guarded_caller_safe_raise_doc(
        &mut self,
        class_name: &str,
        module_name: String,
        raw_selector: &str,
        primitive: &str,
        arg_refs: Vec<Document<'static>>,
    ) -> Result<Document<'static>> {
        let arg_count = arg_refs.len();
        let raise = crate::core_erlang::primitives::generate_class_signal_bif(
            primitive,
            &arg_refs,
            leaf::atom(class_name.to_string()),
        )
        .ok_or_else(|| {
            CodeGenError::Internal(format!(
                "caller-safe class primitive `{primitive}` did not lower for `{class_name} {raw_selector}`"
            ))
        })?;
        let raise = self.annotate_current_send_call(raise);
        let key_doc = docvec![
            "{'caller_safe_raise', ",
            leaf::atom(primitive.to_string()),
            ", ",
            leaf::atom(module_name),
            ", ",
            leaf::atom(
                crate::core_erlang::selector_mangler::safe_class_method_fn_name(raw_selector)
            ),
            ", ",
            leaf::atom(crate::core_erlang::util::metaclass_tag(class_name)),
            "}"
        ];
        let class_self_var = self.fresh_temp_var("StaticSelf");
        Ok(self.keyed_static_send_doc(
            class_name,
            raw_selector,
            &key_doc,
            &class_self_var,
            Self::join_docs_with_commas(arg_refs),
            arg_count + 1,
            raise,
        ))
    }

    /// The shared shape of a keyed static class send (BT-4316): read the
    /// class's static call map (`'<Name> class static calls'`, BT-4442),
    /// match the call target's key in it, install it on a miss
    /// (`beamtalk_class_shadow_flags:static_call_target/4`), and run `hit_doc`
    /// (which may read `class_self_var`, the key's class reference) when it
    /// is present, else the dynamic `class_send` fallback. `fun_arity` is the
    /// arity of the function the key names (`ClassSelf` included, except for
    /// a constructor key's `new/0`, BT-4409).
    #[allow(clippy::too_many_arguments)] // One shape, three hit kinds: the parts are all distinct.
    fn keyed_static_send_doc(
        &mut self,
        class_name: &str,
        raw_selector: &str,
        key_doc: &Document<'static>,
        class_self_var: &str,
        args_doc: Document<'static>,
        fun_arity: usize,
        hit_doc: Document<'static>,
    ) -> Document<'static> {
        let static_ref_var = self.fresh_temp_var("StaticRef");
        let selector_atom =
            crate::core_erlang::selector_mangler::safe_class_method_selector(raw_selector);
        let fallback = self.generate_class_send_fallback(class_name, raw_selector, args_doc);
        // BT-4442: the class's one static call key (`util::static_calls_key`).
        let class_key_doc = leaf::atom(crate::core_erlang::util::static_calls_key(class_name));
        docvec![
            "let ",
            leaf::var(class_self_var.to_string()),
            " = case call 'persistent_term':'get'(",
            class_key_doc,
            ", ~{}~) of <~{",
            key_doc.clone(),
            " := ",
            leaf::var(static_ref_var.clone()),
            "}~> when 'true' -> ",
            leaf::var(static_ref_var),
            " <_> when 'true' -> ",
            "call 'beamtalk_class_shadow_flags':'static_call_target'(",
            key_doc.clone(),
            ", ",
            leaf::atom(class_name.to_string()),
            ", ",
            leaf::atom(selector_atom),
            ", ",
            leaf::int_lit(i64::try_from(fun_arity).unwrap_or(i64::MAX)),
            ") end in case ",
            leaf::var(class_self_var.to_string()),
            " of <'false'> when 'true' -> ",
            fallback,
            " <_> when 'true' -> ",
            hit_doc,
            " end"
        ]
    }

    /// BT-4270: the defining module and primitive of the class-side method a
    /// class-literal send of `raw_selector` to `class_name` resolves to, when
    /// that method (own or inherited) is one of `Exception`'s caller-safe
    /// signal primitives that compiles to that raise in its defining class
    /// ([`crate::core_erlang::primitives::lowers_to_caller_safe_raise`], the primitive read
    /// from its source, `MethodInfo::primitive`) and its defining class
    /// may run it in the caller ([`Self::direct_call_eligible_module`]). The
    /// send then compiles to [`Self::guarded_caller_safe_raise_doc`]: this is
    /// only the compile-time candidate; the runtime key decides each send.
    fn caller_safe_signal_target(
        &self,
        class_name: &str,
        raw_selector: &str,
    ) -> Option<(String, String)> {
        if self.is_shadowed_class(class_name) {
            return None;
        }
        let method = self
            .class_hierarchy
            .as_ref()?
            .find_class_method(class_name, raw_selector)?;
        let primitive = method
            .primitive
            .as_deref()
            .filter(|p| {
                crate::core_erlang::primitives::lowers_to_caller_safe_raise(
                    method.defined_in.as_str(),
                    p,
                )
            })?
            .to_string();
        let module = self.direct_call_eligible_module(method.defined_in.as_str(), raw_selector)?;
        Some((module, primitive))
    }

    /// Generates the `gen_server` `class_send` fallback for binding-aware dispatch.
    ///
    /// Used when a class method is not eligible for direct call optimization.
    fn generate_class_send_fallback(
        &mut self,
        class_name: &str,
        raw_selector: &str,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        let class_selector =
            crate::core_erlang::selector_mangler::safe_class_method_selector(raw_selector);
        let class_pid_var = self.fresh_var("ClassPid");
        docvec![
            "let ",
            leaf::var(class_pid_var.clone()),
            " = ",
            self.registered_class_pid_or_name_doc(class_name),
            " in ",
            self.class_send_call_doc(&class_pid_var, class_selector, args_doc),
        ]
    }

    /// Builds the shared `call
    /// 'beamtalk_object_class':'class_send'(ClassPid, Selector, [Args])`
    /// fragment — the runtime `gen_server` dispatch every class-send fallback
    /// eventually reaches, once a live `ClassPid` is in hand.
    ///
    /// BT-4366: every caller nests this call inside a `let` (the `ClassPid`
    /// binding) or `case` (the direct-call guard), where the send's
    /// whole-document line annotation never reaches the call node — so the
    /// node carries the line itself.
    fn class_send_call_doc(
        &self,
        class_pid_var: &str,
        selector_atom: String,
        args_doc: Document<'static>,
    ) -> Document<'static> {
        self.annotate_current_send_call(docvec![
            "call 'beamtalk_object_class':'class_send'(",
            leaf::var(class_pid_var.to_string()),
            ", ",
            leaf::atom(selector_atom),
            ", [",
            args_doc,
            "])"
        ])
    }

    /// The `direct_call_eligible` entry a send to the class *reference*
    /// `class_name` may pin, or `None` when the name is shadowed
    /// ([`Self::is_shadowed_class`], BT-4271): a shadowed name is one
    /// last-wins runtime class, so the compile-time package's module is not
    /// necessarily the one a dynamic send reaches. The single gate behind
    /// [`Self::direct_call_eligible_module`] and [`Self::direct_new0_module`].
    fn class_reference_direct_call_info(
        &self,
        class_name: &str,
    ) -> Option<std::rc::Rc<crate::core_erlang::generator::DirectCallClassInfo>> {
        if self.is_shadowed_class(class_name) {
            return None;
        }
        self.direct_call_info(class_name)
    }

    /// Shared `direct_call_eligible` gate — returns the target module name
    /// when a send of `raw_selector` to the class reference `class_name`
    /// compiles to the guarded direct call (ADR 0142 §2: the class defines the
    /// method, no class variables in its chain, not process-bound; name not
    /// shadowed).
    /// Used by every class-reference direct-call gate check
    /// (`generate_repl_bound_class_send`, `generate_class_method_call`)
    /// instead of each repeating the same lookup.
    fn direct_call_eligible_module(&self, class_name: &str, raw_selector: &str) -> Option<String> {
        self.class_reference_direct_call_info(class_name)
            .and_then(|info| {
                info.selectors
                    .contains(raw_selector)
                    .then(|| info.module_name.to_string())
            })
    }

    /// Module for a direct nullary-`new` call, if eligible.
    ///
    /// Companion to [`direct_call_eligible_module`](Self::direct_call_eligible_module)
    /// for the `new` constructor, which cannot live in `selectors` (its
    /// direct form takes no `ClassSelf` argument).
    fn direct_new0_module(&self, class_name: &str) -> Option<String> {
        self.class_reference_direct_call_info(class_name)
            .and_then(|info| info.direct_new0.then(|| info.module_name.to_string()))
    }
}
