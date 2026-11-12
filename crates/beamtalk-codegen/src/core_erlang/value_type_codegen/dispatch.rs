// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type `dispatch/3`, `dispatch/4`, reflection arms, and `has_method`
//! generation.
//!
//! **DDD Context:** Code Generation

use super::{DISPATCH_PREFIX_SELECTORS, OwnDispatchArm};

use super::super::dispatch_spec::{self, DispatchSpec, SuperclassDelegation};
use super::super::value_accessors::{AutoSlotMethods, compute_auto_slot_methods};
use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::{ClassDefinition, ClassKind};

impl CoreErlangGenerator {
    /// Encode a string as a Core Erlang binary literal.
    /// Generates the `dispatch/3` function for a value type.
    ///
    /// This routes selectors to individual method functions, provides reflection
    /// methods (class, respondsTo:, fieldNames, fieldAt:, fieldAt:put:,
    /// hasField:, clearField:, perform:, perform:withArguments:), checks the
    /// extension registry for unknown selectors, and delegates to superclass
    /// dispatch/3 for inherited methods.
    /// Only raises `does_not_understand` at the hierarchy root (`ProtoObject`).
    ///
    /// For classes with zero instance methods (e.g., File), generates a
    /// minimal stub that handles only `class` and `respondsTo:`, delegating
    /// everything else to the superclass.
    #[allow(clippy::too_many_lines)] // DNU catch-all logic adds essential branches
    pub(super) fn generate_primitive_dispatch(
        &mut self,
        class: &ClassDefinition,
        auto_methods: Option<&AutoSlotMethods>,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let superclass_mod = self.superclass_module_name(class.superclass_name());

        // Class-methods-only classes skip protocol boilerplate —
        // but only when there are no auto-generated slot methods either.
        let has_auto_instance_methods =
            auto_methods.is_some_and(|a| !a.getters.is_empty() || !a.setters.is_empty());
        if class.methods.is_empty() && !has_auto_instance_methods && superclass_mod.is_some() {
            return self.generate_minimal_dispatch(class);
        }

        // User-defined value objects (ClassKind::Value) store slots in the
        // underlying map and support read-only reflection via fieldAt: and fieldNames.
        // Stdlib primitive types (ClassKind::Object) have no map slots — block both.
        let is_value_class = class.class_kind == ClassKind::Value;
        let (field_names_branch, field_at_branch) =
            Self::generate_dispatch_reflection_branches(&class_name, is_value_class);
        let has_field_branch = Self::generate_dispatch_has_field_branch();

        // fieldAt:put: hint — suggest with*: for value objects, assignment for primitives
        let immutable_hint = if is_value_class {
            super::super::errors::VALUE_IMMUTABLE_SLOT_HINT.to_string()
        } else {
            format!("{class_name}s are immutable. Use assignment (x := newValue) instead.")
        };

        // Route each class-defined method to its individual function
        let method_branches = Self::generate_dispatch_method_branches(class, &mod_name);

        // Check whether this class has a catch-all DNU handler.
        let has_catch_all_dnu = dispatch_spec::class_has_catch_all_dnu(class);

        // Default case: extension fallback, then superclass delegation (or DNU)
        let default_arm: Document<'static> = if has_catch_all_dnu {
            // Class defines DNU — route unknown selectors through it
            Self::value_extension_default_arm(
                &class_name,
                docvec![
                    "                    call ",
                    leaf::atom(mod_name.to_string()),
                    ":'doesNotUnderstand:args:'(Self, Selector, Args)\n",
                ],
            )
        } else if let Some(ref super_mod) = superclass_mod {
            self.value_inherited_default_arm(class, &class_name, super_mod)
        } else {
            // Root of hierarchy — raise does_not_understand
            Self::value_extension_default_arm(
                &class_name,
                docvec![
                    "                    let <DnuClass> = call 'beamtalk_primitive':'class_of'(Self) in\n",
                    "                    let <DnuErr0> = call 'beamtalk_error':'new'('does_not_understand', DnuClass) in\n",
                    "                    let <DnuErr1> = call 'beamtalk_error':'with_selector'(DnuErr0, Selector) in\n",
                    "                    let <DnuErr2> = call 'beamtalk_error':'with_hint'(DnuErr1, ",
                    leaf::binary_lit("Check spelling or use 'respondsTo:' to verify method exists"),
                    ") in\n",
                    "                    call 'beamtalk_error':'raise'(DnuErr2)\n",
                ],
            )
        };

        let doc = docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->\n",
            "    case Selector of\n",
            // class
            "        <'class'> when 'true' ->\n",
            "            ",
            leaf::atom(class_name.clone()),
            "\n",
            // respondsTo:
            "        <'respondsTo:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[RtSelector | _]> when 'true' -> call ",
            leaf::atom(mod_name.to_string()),
            ":'has_method'(RtSelector)\n",
            "                <_> when 'true' -> 'false'\n",
            "            end\n",
            // fieldNames — delegates to beamtalk_reflection for value objects
            field_names_branch,
            // fieldAt: — reads from map for value objects, error for primitives
            field_at_branch,
            // hasField: — presence test, never raises (ADR 0124 §1/§9/B4)
            has_field_branch,
            // fieldAt:put:
            "        <'fieldAt:put:'> when 'true' ->\n",
            "            let <ImmErr0> = call 'beamtalk_error':'new'('immutable_value', ",
            leaf::atom(class_name.clone()),
            ") in\n",
            "            let <ImmErr1> = call 'beamtalk_error':'with_selector'(ImmErr0, 'fieldAt:put:') in\n",
            "            let <ImmErr2> = call 'beamtalk_error':'with_hint'(ImmErr1, ",
            leaf::binary_lit(immutable_hint.clone()),
            ") in\n",
            "            call 'beamtalk_error':'raise'(ImmErr2)\n",
            // clearField: — always the existing "Cannot modify slot on
            // value type" error (ADR 0124 §1/B4): a Value can never declare
            // a `late field:` (§5), so there is never a late slot to clear.
            "        <'clearField:'> when 'true' ->\n",
            "            let <ClrErr0> = call 'beamtalk_error':'new'('immutable_value', ",
            leaf::atom(class_name.clone()),
            ") in\n",
            "            let <ClrErr1> = call 'beamtalk_error':'with_selector'(ClrErr0, 'clearField:') in\n",
            "            let <ClrErr2> = call 'beamtalk_error':'with_hint'(ClrErr1, ",
            leaf::binary_lit(immutable_hint),
            ") in\n",
            "            call 'beamtalk_error':'raise'(ClrErr2)\n",
            // perform:
            "        <'perform:'> when 'true' ->\n",
            "            let <PerfSel> = call 'erlang':'hd'(Args) in\n",
            "            call ",
            leaf::atom(mod_name.to_string()),
            ":'dispatch'(PerfSel, [], Self)\n",
            // perform:withArguments:
            "        <'perform:withArguments:'> when 'true' ->\n",
            "            let <PwaSel> = call 'erlang':'hd'(Args) in\n",
            "            let <PwaArgs> = call 'erlang':'hd'(call 'erlang':'tl'(Args)) in\n",
            "            call ",
            leaf::atom(mod_name.to_string()),
            ":'dispatch'(PwaSel, PwaArgs, Self)\n",
            // Class-defined method branches
            Document::Vec(method_branches),
            // Auto-generated getter and with*: dispatch arms
            Document::Vec(
                auto_methods
                    .map(|a| Self::generate_auto_slot_dispatch_arms(&mod_name, a))
                    .unwrap_or_default(),
            ),
            // Default case
            default_arm,
            "    end\n",
            "\n",
        ];

        Ok(doc)
    }

    /// The `dispatch/3` default arm of a value type that has no inherited
    /// route: the extension registry, then `not_found` (a catch-all
    /// `doesNotUnderstand:args:` handler, or the root's DNU raise).
    fn value_extension_default_arm(
        class_name: &str,
        not_found: Document<'static>,
    ) -> Document<'static> {
        docvec![
            "        <_Other> when 'true' ->\n",
            "            case call 'beamtalk_extensions':'lookup'(",
            leaf::atom(class_name.to_string()),
            ", Selector) of\n",
            "                <{'ok', ExtFun, _ExtOwner}> when 'true' ->\n",
            "                    apply ExtFun(Args, Self)\n",
            "                <'not_found'> when 'true' ->\n",
            not_found,
            "            end\n",
        ]
    }

    /// The `dispatch/3` default arm of a value type with a superclass.
    ///
    /// BT-4260: it first asks the memoised `beamtalk_dispatch:inherited_target/2`
    /// verdict (BT-4280's, one derived-cache key for actors and values). On
    /// `{'value', Owner, Module}` the inherited method is reached by calling
    /// the owner's `dispatch/3` directly, skipping the superclass-by-superclass
    /// delegation and its extension lookup per level. Anything else (`slow`,
    /// or an actor-convention verdict a value chain cannot produce) takes the
    /// unchanged route: this class's extensions, then the superclass's
    /// `dispatch/3`.
    ///
    /// Both routes answer the same, so which one a selector takes is only a
    /// cost choice. A selector the superclass defines itself is one hop away:
    /// the unchanged route (one extension lookup and one call) is cheaper than
    /// the verdict lookup, so such selectors skip the verdict. The list comes
    /// from the class hierarchy at compile time; if the superclass later
    /// changes, a selector only takes the slower of two equivalent routes.
    fn value_inherited_default_arm(
        &self,
        class: &ClassDefinition,
        class_name: &str,
        super_mod: &str,
    ) -> Document<'static> {
        let ask = docvec![
            "call 'beamtalk_dispatch':'inherited_target'(",
            leaf::atom(class_name.to_string()),
            ", Selector)",
        ];
        let one_hop = self.one_hop_selectors(class);
        let verdict: Document<'static> = if one_hop.is_empty() {
            ask
        } else {
            let arms: Vec<Document<'static>> = one_hop
                .into_iter()
                .map(|sel| {
                    docvec![
                        "                <",
                        leaf::atom(sel),
                        "> when 'true' -> 'slow'\n",
                    ]
                })
                .collect();
            docvec![
                "case Selector of\n",
                Document::Vec(arms),
                "                <_> when 'true' -> ",
                ask,
                "\n",
                "            end",
            ]
        };
        docvec![
            "        <_Other> when 'true' ->\n",
            "            %% BT-4260: a memoised verdict picks the direct inherited call.\n",
            "            case ",
            verdict,
            " of\n",
            "                <{'value', _InhOwner, InhModule}> when 'true' ->\n",
            "                    call InhModule:'dispatch'(Selector, Args, Self)\n",
            "                <_InhSlow> when 'true' ->\n",
            "                    case call 'beamtalk_extensions':'lookup'(",
            leaf::atom(class_name.to_string()),
            ", Selector) of\n",
            "                        <{'ok', ExtFun, _ExtOwner}> when 'true' ->\n",
            "                            apply ExtFun(Args, Self)\n",
            "                        <'not_found'> when 'true' ->\n",
            "                            call ",
            leaf::atom(super_mod.to_string()),
            ":'dispatch'(Selector, Args, Self)\n",
            "                    end\n",
            "            end\n",
        ]
    }

    /// The instance selectors `class`'s superclass defines itself, per the
    /// class hierarchy, less those `class` defines (its own `dispatch/3` arms
    /// answer them before the default arm); sorted and deduplicated, and
    /// empty when no hierarchy is available.
    fn one_hop_selectors(&self, class: &ClassDefinition) -> Vec<String> {
        let superclass = class.superclass_name();
        let Some(info) = self
            .class_hierarchy
            .as_ref()
            .and_then(|h| h.get_class(superclass))
        else {
            return Vec::new();
        };
        info.methods
            .iter()
            .filter(|m| m.defined_in == superclass)
            .filter(|m| {
                !class
                    .methods
                    .iter()
                    .any(|own| own.selector.name() == m.selector)
            })
            .map(|m| m.selector.to_string())
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect()
    }

    /// Generates the `fieldNames` and `fieldAt:` dispatch arms.
    ///
    /// Value objects (`ClassKind::Value`) support reflective field access via
    /// `beamtalk_reflection`. Primitive types raise an immutable-value error.
    fn generate_dispatch_reflection_branches(
        class_name: &str,
        is_value_class: bool,
    ) -> (Document<'static>, Document<'static>) {
        let field_names_branch: Document<'static> = if is_value_class {
            docvec![
                "        <'fieldNames'> when 'true' ->\n",
                "            ",
                super::super::primitives::field_names_doc("Self"),
                "\n",
            ]
        } else {
            docvec![
                "        <'fieldNames'> when 'true' ->\n",
                "            []\n",
            ]
        };

        let field_at_branch: Document<'static> = if is_value_class {
            docvec![
                "        <'fieldAt:'> when 'true' ->\n",
                "            let <FaName> = call 'erlang':'hd'(Args) in\n",
                "            call 'beamtalk_reflection':'read_field'(FaName, Self)\n",
            ]
        } else {
            let iva_hint = format!("{class_name}s are immutable and have no fields.");
            docvec![
                "        <'fieldAt:'> when 'true' ->\n",
                "            let <IvaErr0> = call 'beamtalk_error':'new'('immutable_value', ",
                leaf::atom(class_name.to_string()),
                ") in\n",
                "            let <IvaErr1> = call 'beamtalk_error':'with_selector'(IvaErr0, 'fieldAt:') in\n",
                "            let <IvaErr2> = call 'beamtalk_error':'with_hint'(IvaErr1, ",
                leaf::binary_lit(iva_hint),
                ") in\n",
                "            call 'beamtalk_error':'raise'(IvaErr2)\n",
            ]
        };

        (field_names_branch, field_at_branch)
    }

    /// Generates the `hasField:` dispatch arm (ADR 0124 §1/§9/B4).
    ///
    /// A presence test that never raises — the one difference from
    /// [`Self::generate_dispatch_reflection_branches`]'s `fieldAt:` sibling,
    /// whose primitive-type branch raises `immutable_value` instead. A
    /// primitive type has no fields at all, so `hasField:` on one always
    /// answers `'false'`, exactly like `Object>>fieldNames` already answers
    /// `#()` for a primitive rather than erroring.
    ///
    /// Guards with a **runtime** `is_map(Self)` check rather than trusting
    /// the compile-time `ClassKind::Value` classification every OTHER
    /// reflection arm in this function keys on: several stdlib `Value`
    /// subclasses (`Integer`, `Float`, `String`, `Character`, `Boolean`,
    /// `UndefinedObject`, `Block`, …, via `Number`/`Value`) are
    /// `ClassKind::Value` for type-checking purposes but represent their
    /// instances as bare Erlang terms, never a tagged map — so
    /// `beamtalk_reflection:has_field/2`'s `is_map(State)` guard would
    /// `function_clause`-crash on `Self` for any of them. `fieldAt:`'s own
    /// arm has the identical latent gap but is masked in practice: every
    /// existing call site reaches it only through the intrinsic's own
    /// call-site `is_map`/`is_tuple` dance (`intrinsics.rs`), which never
    /// forwards a non-map primitive into `dispatch/3` at all. `hasField:`
    /// (and `clearField:`'s non-self path) instead delegate the general
    /// case to `beamtalk_message_dispatch:send/3` (ADR 0124 §4i's
    /// class-object-receiver fix), which — correctly — does route a bare
    /// primitive here, so this arm must be safe for one on its own rather
    /// than relying on a caller-side gate it no longer has.
    fn generate_dispatch_has_field_branch() -> Document<'static> {
        docvec![
            "        <'hasField:'> when 'true' ->\n",
            "            let <HfName> = call 'erlang':'hd'(Args) in\n",
            "            case call 'erlang':'is_map'(Self) of\n",
            "                <'true'> when 'true' -> call 'beamtalk_reflection':'has_field'(HfName, Self)\n",
            "                <'false'> when 'true' -> 'false'\n",
            "            end\n",
        ]
    }

    /// The class's auto-generated slot accessors
    /// ([`compute_auto_slot_methods`]), after recording (BT-4473)
    /// [`Self::own_dispatch_arms`] in the class context — before any method
    /// body is generated, so a typed send to an own-class instance inside one
    /// can call the arm's function directly.
    pub(super) fn auto_slot_methods_and_own_arms(
        &mut self,
        class: &ClassDefinition,
    ) -> Option<AutoSlotMethods> {
        let auto = compute_auto_slot_methods(class);
        let arms = Self::own_dispatch_arms(class, auto.as_ref());
        self.class_context_mut().own_dispatch_arms = arms;
        auto
    }

    /// BT-4473: the selectors `class`'s `dispatch/3` answers from a method
    /// arm — the arms [`Self::generate_dispatch_method_branches`] and
    /// `generate_auto_slot_dispatch_arms` emit — mapped to how the arm calls
    /// the method's function. A selector that one of the arms ahead of them
    /// answers first ([`DISPATCH_PREFIX_SELECTORS`]) is left out, and a
    /// selector defined twice keeps its first arm, as the `case` does.
    fn own_dispatch_arms(
        class: &ClassDefinition,
        auto: Option<&AutoSlotMethods>,
    ) -> std::collections::HashMap<String, OwnDispatchArm> {
        let mut arms = std::collections::HashMap::new();
        for method in &class.methods {
            arms.entry(method.selector.name().to_string())
                .or_insert(OwnDispatchArm {
                    arity: method.parameters.len(),
                });
        }
        if let Some(auto) = auto {
            for getter in &auto.getters {
                arms.entry(getter.clone())
                    .or_insert(OwnDispatchArm { arity: 0 });
            }
            for field in &auto.setters {
                arms.entry(AutoSlotMethods::with_star_selector(field))
                    .or_insert(OwnDispatchArm { arity: 1 });
            }
        }
        arms.retain(|selector, _| !DISPATCH_PREFIX_SELECTORS.contains(&selector.as_str()));
        arms
    }

    /// Generates dispatch case arms for all class-defined instance methods.
    ///
    /// Each arm calls the module-level function for that method and answers
    /// its return (BT-4503: the function answers the send's result itself).
    pub(in crate::core_erlang) fn generate_dispatch_method_branches(
        class: &ClassDefinition,
        mod_name: &str,
    ) -> Vec<Document<'static>> {
        let mut method_branches: Vec<Document<'static>> = Vec::new();
        for method in &class.methods {
            let mangled = method.selector.name().to_string();
            method_branches.push(docvec![
                "        <",
                leaf::atom(mangled.clone()),
                "> when 'true' ->\n",
            ]);

            // Build the method call arguments: (Self) or (Self, DispArg0, DispArg1, ...)
            if !method.parameters.is_empty() {
                // Extract args from Args list: hd(Args), hd(tl(Args)), ...
                for (i, _param) in method.parameters.iter().enumerate() {
                    let arg_var = format!("DispArg{i}");
                    let mut access: Document<'static> = Document::Str("Args");
                    for _ in 0..i {
                        access = docvec!["call 'erlang':'tl'(", access, ")",];
                    }
                    method_branches.push(docvec![
                        "            let <",
                        leaf::var(arg_var),
                        "> = call 'erlang':'hd'(",
                        access,
                        ") in\n",
                    ]);
                }
            }

            // Build the call expression: call 'mod':'method'(Self, DispArg0, ...)
            let mut call_args: Vec<Document<'static>> = vec![Document::Str("Self")];
            for i in 0..method.parameters.len() {
                call_args.push(docvec![
                    ", DispArg",
                    leaf::int_lit(i64::try_from(i).unwrap_or(i64::MAX)),
                ]);
            }
            let call_doc = docvec![
                "call ",
                leaf::atom(mod_name.to_string()),
                ":",
                leaf::atom(mangled.clone()),
                "(",
                Document::Vec(call_args),
                ")",
            ];

            method_branches.push(docvec!["            ", call_doc, "\n"]);
        }
        method_branches
    }

    /// Generates the `dispatch/4` function for a value type.
    ///
    /// The dispatch service (`beamtalk_dispatch:invoke_method/6`) only calls
    /// modules that export `dispatch/4`. Without this, compiled value type
    /// modules are skipped during the actor hierarchy walk, preventing actors
    /// from inheriting Object-level methods.
    ///
    /// For Object specifically, this generates state-aware implementations
    /// for reflection primitives (fieldAt:, printString, etc.) that need
    /// access to the actor's state map. For all other value types, this is
    /// a simple wrapper that delegates to dispatch/3.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(super) fn generate_dispatch_4(
        &mut self,
        _class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();

        let header: Document<'static> =
            Document::Str("'dispatch'/4 = fun (Selector, Args, Self, State) ->\n");

        let body = if class_name == "Object" {
            self.generate_object_dispatch_4(mod_name.as_str())
        } else {
            Self::generate_dispatch_4_wrapper(mod_name.as_str())
        };

        Ok(docvec![header, body, "\n"])
    }

    /// Generates Object's dispatch/4 with state-aware reflection primitives.
    ///
    /// Actor instances inherit Object methods via the hierarchy walk. These
    /// methods need access to the actor's State map for proper reflection
    /// (field access, class name, etc.).
    #[allow(clippy::too_many_lines)] // Object dispatch with reflection primitives
    pub(super) fn generate_object_dispatch_4(&self, mod_name: &str) -> Document<'static> {
        // fieldAt: arity error
        let inst_var_at_err = self.arity_error_fragment("'fieldAt:'", 1, "                ");
        // fieldAt:put: arity error (used twice)
        let inst_var_at_put_err =
            self.arity_error_fragment("'fieldAt:put:'", 2, "                    ");
        let inst_var_at_put_err2 =
            self.arity_error_fragment("'fieldAt:put:'", 2, "                ");
        // hasField:/clearField: arity errors (ADR 0124 §1/§9/B4)
        let has_field_err = self.arity_error_fragment("'hasField:'", 1, "                ");
        let clear_field_err = self.arity_error_fragment("'clearField:'", 1, "                ");
        // perform: type error
        let perf_type_err =
            self.type_error_fragment("'perform:'", "selector must be an atom", "                ");
        // perform:withArguments: type errors
        let pwa_type_err_list = self.type_error_fragment(
            "'perform:withArguments:'",
            "arguments must be a list",
            "                        ",
        );
        let pwa_type_err_sel = self.type_error_fragment(
            "'perform:withArguments:'",
            "selector must be an atom",
            "                ",
        );

        docvec![
            "    case Selector of\n",
            // --- class ---
            "        <'class'> when 'true' ->\n",
            "            {'reply', call 'beamtalk_tagged_map':'class_of'(State), State}\n",
            // --- respondsTo: ---
            "        <'respondsTo:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[RtSel4|_]> when call 'erlang':'is_atom'(RtSel4) ->\n",
            "                    let <RtClass4> = call 'beamtalk_tagged_map':'class_of'(State) in\n",
            "                    {'reply', call 'beamtalk_dispatch':'responds_to'(RtSel4, RtClass4), State}\n",
            "                <_RtBadArgs> when 'true' -> {'reply', 'false', State}\n",
            "            end\n",
            // --- fieldNames ---
            "        <'fieldNames'> when 'true' ->\n",
            "            {'reply', call 'beamtalk_reflection':'field_names'(State), State}\n",
            // --- fieldAt: ---
            "        <'fieldAt:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[IvaName4|_]> when 'true' ->\n",
            "                    {'reply', call 'beamtalk_reflection':'read_field'(IvaName4, State), State}\n",
            "                <_IvaBadArgs> when 'true' ->\n",
            inst_var_at_err,
            "\n",
            "            end\n",
            // --- fieldAt:put: ---
            "        <'fieldAt:put:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[IvapName4|IvapRest4]> when 'true' ->\n",
            "                    case IvapRest4 of\n",
            "                        <[IvapVal4|_]> when 'true' ->\n",
            "                            let <IvapTuple4> = call 'beamtalk_reflection':'write_field'(IvapName4, IvapVal4, State) in\n",
            "                            let <IvapResult4> = call 'erlang':'element'(1, IvapTuple4) in\n",
            "                            let <IvapNewState4> = call 'erlang':'element'(2, IvapTuple4) in\n",
            "                            {'reply', IvapResult4, IvapNewState4}\n",
            "                        <_IvapBad2> when 'true' ->\n",
            inst_var_at_put_err,
            "\n",
            "                    end\n",
            "                <_IvapBad1> when 'true' ->\n",
            inst_var_at_put_err2,
            "\n",
            "            end\n",
            // --- hasField: (ADR 0124 §1/§9/B4) — never raises ---
            "        <'hasField:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[HfName4|_]> when 'true' ->\n",
            "                    {'reply', call 'beamtalk_reflection':'has_field'(HfName4, State), State}\n",
            "                <_HfBadArgs> when 'true' ->\n",
            has_field_err,
            "\n",
            "            end\n",
            // --- clearField: (ADR 0124 §1/§4f/§9/B4) — a write ---
            "        <'clearField:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[ClfName4|_]> when 'true' ->\n",
            "                    let <ClfNewState4> = call 'beamtalk_reflection':'clear_field'(ClfName4, State) in\n",
            "                    {'reply', Self, ClfNewState4}\n",
            "                <_ClfBadArgs> when 'true' ->\n",
            clear_field_err,
            "\n",
            "            end\n",
            // --- printString ---
            "        <'printString'> when 'true' ->\n",
            "            let <PsClass4> = call 'beamtalk_tagged_map':'class_of'(State, 'Object') in\n",
            "            let <PsStr4> = call 'erlang':'atom_to_binary'(PsClass4, 'utf8') in\n",
            "            {'reply', PsStr4, State}\n",
            // --- perform: ---
            "        <'perform:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[PerfSel4|_]> when call 'erlang':'is_atom'(PerfSel4) ->\n",
            "                    let <PerfClass4> = call 'beamtalk_tagged_map':'class_of'(State) in\n",
            "                    let <PerfResult4> = call 'beamtalk_dispatch':'lookup'(PerfSel4, [], Self, State, PerfClass4) in\n",
            "                    case PerfResult4 of\n",
            // BT-4593: `perform:` itself was invoked, so its target's lookup
            // failure is a raise (a wrapped map), not a miss of `perform:`.
            "                        <{'error', PerfErr4}> when 'true' -> {'error', call 'beamtalk_exception_handler':'ensure_wrapped'(PerfErr4), State}\n",
            "                        <PerfOther4> when 'true' -> PerfOther4\n",
            "                    end\n",
            "                <_PerfBadArgs> when 'true' ->\n",
            perf_type_err,
            "\n",
            "            end\n",
            // --- perform:withArguments: ---
            "        <'perform:withArguments:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[PwaSel4|PwaRest4]> when call 'erlang':'is_atom'(PwaSel4) ->\n",
            "                    case PwaRest4 of\n",
            "                        <[PwaArgs4|_]> when call 'erlang':'is_list'(PwaArgs4) ->\n",
            "                            let <PwaClass4> = call 'beamtalk_tagged_map':'class_of'(State) in\n",
            "                            let <PwaResult4> = call 'beamtalk_dispatch':'lookup'(PwaSel4, PwaArgs4, Self, State, PwaClass4) in\n",
            "                            case PwaResult4 of\n",
            // BT-4593: `perform:` itself was invoked, so its target's lookup
            // failure is a raise (a wrapped map), not a miss of `perform:`.
            "                                <{'error', PwaErr4}> when 'true' -> {'error', call 'beamtalk_exception_handler':'ensure_wrapped'(PwaErr4), State}\n",
            "                                <PwaOther4> when 'true' -> PwaOther4\n",
            "                            end\n",
            "                        <_PwaBadArgList> when 'true' ->\n",
            pwa_type_err_list,
            "\n",
            "                    end\n",
            "                <_PwaBadSel> when 'true' ->\n",
            pwa_type_err_sel,
            "\n",
            "            end\n",
            // --- Default: delegate to dispatch/3 ---
            "        <_OtherSel4> when 'true' ->\n",
            Self::dispatch_3_adapter(mod_name, "            "),
            "    end\n",
        ]
    }

    /// Generates a simple dispatch/4 wrapper that delegates to dispatch/3.
    pub(in crate::core_erlang) fn generate_dispatch_4_wrapper(mod_name: &str) -> Document<'static> {
        Self::dispatch_3_adapter(mod_name, "    ")
    }

    /// The `dispatch/4` → `dispatch/3` adapter: calls the state-less
    /// `dispatch/3` and answers `{'reply', Result, State}`, or, for an
    /// `error` the method raised, `{'error', Wrapped, State}` with the raised
    /// term wrapped (with its stacktrace, unless it already carries one) as an
    /// exception map — so the walk
    /// reads it as "the invoked method raised", never as a lookup miss, even
    /// when it is a `does_not_understand` from inside the method body
    /// (BT-4593).
    ///
    /// Only `error` is caught. A `throw` (a `^` non-local return in flight,
    /// ADR 0041, or a `Program exit:`, ADR 0099 §3) and an `exit` are
    /// re-raised unchanged, so a `^` out of a block reaching an inherited
    /// value-class method keeps its semantics instead of turning into an
    /// error result.
    ///
    /// `indent` is the leading indentation of the `try` line.
    fn dispatch_3_adapter(mod_name: &str, indent: &'static str) -> Document<'static> {
        docvec![
            indent,
            "try call ",
            leaf::atom(mod_name.to_string()),
            ":'dispatch'(Selector, Args, Self)\n",
            indent,
            "of <D4Result> -> {'reply', D4Result, State}\n",
            indent,
            "catch <D4Type, D4Error, D4Stack> ->\n",
            indent,
            "    case D4Type of\n",
            indent,
            "        <'error'> when 'true' ->\n",
            indent,
            "            let <D4Built> = primop 'build_stacktrace'(D4Stack) in\n",
            indent,
            // `ensure_wrapped/4` (empty context): an already-wrapped
            // exception keeps its innermost `raw_stacktrace`, as in the
            // walk's own `raised/4`.
            "            {'error', call 'beamtalk_exception_handler':'ensure_wrapped'('error', D4Error, D4Built, ~{}~), State}\n",
            indent,
            "        <_D4Other> when 'true' -> primop 'raw_raise'(D4Type, D4Error, D4Stack)\n",
            indent,
            "    end\n",
        ]
    }

    /// Generates a structured `arity_mismatch` error for dispatch/4 argument validation.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    fn arity_error_fragment(
        &self,
        selector: &str,
        expected_arity: u32,
        indent: &str,
    ) -> Document<'static> {
        let hint = format!("Expected {expected_arity} argument(s) for {selector}");
        let sep = docvec!["\n", leaf::var(indent.to_string())];
        docvec![
            leaf::var(indent.to_string()),
            super::super::errors::beamtalk_error_doc(
                Document::Str("<ArErr0>"),
                Document::Str("ArErr0"),
                Document::Str("<ArErr1>"),
                Document::Str("ArErr1"),
                Document::Str("<ArErr2>"),
                "arity_mismatch",
                Document::Str("call 'beamtalk_tagged_map':'class_of'(State, 'Object')"),
                leaf::var(selector.to_string()),
                leaf::binary_lit(hint),
                sep,
            ),
            "{'error', ArErr2, State}",
        ]
    }

    /// Generates a structured `type_error` for dispatch/4 argument validation.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    fn type_error_fragment(
        &self,
        selector: &str,
        hint_msg: &str,
        indent: &str,
    ) -> Document<'static> {
        let sep = docvec!["\n", leaf::var(indent.to_string())];
        docvec![
            leaf::var(indent.to_string()),
            super::super::errors::beamtalk_error_doc(
                Document::Str("<TyErr0>"),
                Document::Str("TyErr0"),
                Document::Str("<TyErr1>"),
                Document::Str("TyErr1"),
                Document::Str("<TyErr2>"),
                "type_error",
                Document::Str("call 'beamtalk_tagged_map':'class_of'(State, 'Object')"),
                leaf::var(selector.to_string()),
                leaf::binary_lit(hint_msg),
                sep,
            ),
            "{'error', TyErr2, State}",
        ]
    }

    /// Generates the `has_method/1` function for a value type.
    ///
    /// Returns `true` for all known selectors (class-defined + reflection +
    /// extensions + superclass methods via delegation).
    pub(super) fn generate_primitive_has_method(
        &mut self,
        class: &ClassDefinition,
        auto_methods: Option<&AutoSlotMethods>,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let superclass_mod = self.superclass_module_name(class.superclass_name());

        // If the class has a catch-all DNU handler, it accepts any
        // selector — return true unconditionally.
        let has_catch_all_dnu = dispatch_spec::class_has_catch_all_dnu(class);
        if has_catch_all_dnu {
            return Ok(dispatch_spec::generate_has_method_from_spec(
                &[],
                &DispatchSpec {
                    reflection: &[],
                    class_name: &class_name,
                    superclass: None,
                    dnu: true,
                    auto_slots: None,
                    // BT-4260: the strictly-local probe lets
                    // `beamtalk_dispatch:resolve_owner/2` stop exactly where
                    // the `dispatch/3` superclass chain would (a catch-all
                    // DNU class handles every selector itself).
                    emit_local_probe: true,
                },
            ));
        }

        // Class-methods-only classes delegate directly to superclass —
        // but only when there are no auto-generated slot methods either.
        let has_auto_instance_methods =
            auto_methods.is_some_and(|a| !a.getters.is_empty() || !a.setters.is_empty());
        if class.methods.is_empty() && !has_auto_instance_methods && superclass_mod.is_some() {
            return self.generate_minimal_has_method(class);
        }

        // Build the reflection selector list — the nine hard-coded
        // dispatch/3 arms (ADR 0124 §1/§9/B4 added `hasField:`/`clearField:`
        // to the original seven).
        let reflection: Vec<&'static str> = vec![
            "class",
            "respondsTo:",
            "fieldNames",
            "fieldAt:",
            "fieldAt:put:",
            "hasField:",
            "clearField:",
            "perform:",
            "perform:withArguments:",
        ];
        let own_methods: Vec<String> = class
            .methods
            .iter()
            .map(|m| m.selector.name().to_string())
            .collect();

        Ok(dispatch_spec::generate_has_method_from_spec(
            &own_methods,
            &DispatchSpec {
                reflection: &reflection,
                class_name: &class_name,
                superclass: superclass_mod.as_deref().map(SuperclassDelegation::Static),
                dnu: false,
                auto_slots: auto_methods,
                // BT-4260: see the catch-all-DNU spec above.
                emit_local_probe: true,
            },
        ))
    }

    /// Generates a minimal `dispatch/3` for classes with no instance methods.
    ///
    /// Handles `class`, `respondsTo:`, `perform:`, and `perform:withArguments:`
    /// locally, then checks extensions and delegates everything else to the
    /// superclass. `perform:` must re-dispatch through this module to preserve
    /// extension lookup and correct error attribution. Skips fieldNames,
    /// fieldAt:, fieldAt:put: since the superclass already handles them.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(super) fn generate_minimal_dispatch(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let mod_name = self.module_name.clone();
        let super_mod = self
            .superclass_module_name(class.superclass_name())
            .expect("minimal dispatch requires superclass");

        let doc = docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->\n",
            "    case Selector of\n",
            // class
            "        <'class'> when 'true' ->\n",
            "            ",
            leaf::atom(class_name.clone()),
            "\n",
            // respondsTo:
            "        <'respondsTo:'> when 'true' ->\n",
            "            case Args of\n",
            "                <[RtSelector | _]> when 'true' -> call ",
            leaf::atom(mod_name.clone()),
            ":'has_method'(RtSelector)\n",
            "                <_> when 'true' -> 'false'\n",
            "            end\n",
            // perform:
            "        <'perform:'> when 'true' ->\n",
            "            let <PerfSel> = call 'erlang':'hd'(Args) in\n",
            "            call ",
            leaf::atom(mod_name.clone()),
            ":'dispatch'(PerfSel, [], Self)\n",
            // perform:withArguments:
            "        <'perform:withArguments:'> when 'true' ->\n",
            "            let <PwaSel> = call 'erlang':'hd'(Args) in\n",
            "            let <PwaArgs> = call 'erlang':'hd'(call 'erlang':'tl'(Args)) in\n",
            "            call ",
            leaf::atom(mod_name),
            ":'dispatch'(PwaSel, PwaArgs, Self)\n",
            // Default: the inherited verdict, then extension check and
            // superclass delegation
            self.value_inherited_default_arm(class, &class_name, &super_mod),
            "    end\n",
            "\n",
        ];

        Ok(doc)
    }

    /// Generates a minimal `has_method/1` for classes with no instance methods.
    ///
    /// Checks `class`, `respondsTo:`, `perform:`, `perform:withArguments:`,
    /// then extensions, then delegates to superclass.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(super) fn generate_minimal_has_method(
        &mut self,
        class: &ClassDefinition,
    ) -> Result<Document<'static>> {
        let class_name = self.class_name().clone();
        let super_mod = self
            .superclass_module_name(class.superclass_name())
            .expect("minimal has_method requires superclass");

        Ok(dispatch_spec::generate_has_method_from_spec(
            &[],
            &DispatchSpec {
                reflection: &["class", "respondsTo:", "perform:", "perform:withArguments:"],
                class_name: &class_name,
                superclass: Some(SuperclassDelegation::Static(&super_mod)),
                dnu: false,
                auto_slots: None,
                // BT-4260: see `generate_primitive_has_method`.
                emit_local_probe: true,
            },
        ))
    }
}
