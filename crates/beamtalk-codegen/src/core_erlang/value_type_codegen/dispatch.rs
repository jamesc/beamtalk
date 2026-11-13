// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type `dispatch/3`, `dispatch/4`, reflection arms, and `has_method`
//! generation.
//!
//! **DDD Context:** Code Generation

use super::{DISPATCH_PREFIX_SELECTORS, OwnDispatchArm};

use super::super::dispatch_spec::{self, DispatchSpec, SuperclassDelegation};
use super::super::exception_forms::{CatchVars, wrap_and_reraise_doc};
use super::super::value_accessors::{AutoSlotMethods, compute_auto_slot_methods};
use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout, case_of, clause, let_in, try_of_catch};
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::{ClassDefinition, ClassKind};

/// A Core Erlang variable document.
fn var(name: &str) -> Document<'static> {
    leaf::var(name.to_string())
}

/// `{'reply', value, State}` — a `dispatch/4` answer that leaves the state as is.
fn reply(value: Document<'static>) -> Document<'static> {
    docvec!["{", leaf::atom("reply"), ", ", value, ", State}"]
}

/// The nine hard-coded `dispatch/3` reflection arms of a primitive value type,
/// as `has_method/1` lists them (ADR 0124 §1/§9/B4 added `hasField:` and
/// `clearField:` to the original seven).
///
/// [`DISPATCH_PREFIX_SELECTORS`] holds the same nine selectors in the order
/// `dispatch/3` emits them; this list keeps the order the generated
/// `has_method/1` has always had, so its output does not change.
/// `primitive_has_method_reflection_matches_dispatch_arms` pins the two to the
/// same set.
const PRIMITIVE_HAS_METHOD_REFLECTION: &[&str] = &[
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

        let (reflection_head, reflection_perform) =
            Self::split_value_reflection_arms(&class_name, &mod_name);

        let doc = docvec![
            "'dispatch'/3 = fun (Selector, Args, Self) ->\n",
            "    case Selector of\n",
            // class, respondsTo:
            Document::Vec(reflection_head),
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
            // perform:, perform:withArguments:
            Document::Vec(reflection_perform),
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

    /// [`dispatch_spec::value_core_reflection_arms`] split around the field
    /// reflection arms the primitive dispatch puts between them: `class` and
    /// `respondsTo:`, then `perform:` and `perform:withArguments:`.
    fn split_value_reflection_arms(
        class_name: &str,
        mod_name: &str,
    ) -> (Vec<Document<'static>>, Vec<Document<'static>>) {
        let mut arms = dispatch_spec::value_core_reflection_arms(class_name, mod_name);
        let split = arms
            .iter()
            .position(|(selector, _)| *selector == "perform:")
            .expect("the core reflection arms include perform:");
        let perform = arms.split_off(split);
        let docs = |arms: Vec<(&'static str, Document<'static>)>| {
            arms.into_iter().map(|(_, arm)| arm).collect::<Vec<_>>()
        };
        (docs(arms), docs(perform))
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
        dispatch_spec::inherited_default_arm(
            class_name,
            dispatch_spec::DefaultArmFlavour::Value {
                one_hop: &self.one_hop_selectors(class),
                super_module: super_mod,
            },
        )
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
            Document::Str("'dispatch'/4 = fun (Selector, Args, Self, State) ->");

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
    ///
    /// The result is the function body, starting on its own line: the caller
    /// writes the `'dispatch'/4 = fun (...) ->` header without a trailing
    /// newline. Built with the structural constructors (BT-4685), so every
    /// indent comes from `nest`.
    #[allow(clippy::too_many_lines)] // Object dispatch with reflection primitives
    pub(super) fn generate_object_dispatch_4(&self, mod_name: &str) -> Document<'static> {
        let state_class = || form::call("beamtalk_tagged_map", "class_of", [var("State")]);
        let arm = |selector: &str, body: Document<'static>| {
            clause([leaf::atom(selector)], None, body, Layout::Block)
        };
        // A `case Args of` whose arms are the argument-shape alternatives.
        let args_case = |arms: Vec<Document<'static>>| case_of(var("Args"), arms, Layout::Block);
        let pat = |text: &'static str| Document::Str(text);
        let bad_args =
            |name: &str, body: Document<'static>| clause([var(name)], None, body, Layout::Block);
        let single_arg = |head: &'static str, guard: Option<Document<'static>>, body| {
            clause([pat(head)], guard, body, Layout::Block)
        };
        let let_ = |name: &str, value: Document<'static>, body: Document<'static>| {
            let_in(var(name), value, body, Layout::Block)
        };
        let lookup_arm = |sel: &'static str,
                          args: Document<'static>,
                          class_v: &'static str,
                          result_v: &'static str,
                          err_v: &'static str,
                          other_v: &'static str| {
            // BT-4593: `perform:` itself was invoked, so its target's lookup
            // failure is a raise (a wrapped map), not a miss of `perform:`.
            let_(
                class_v,
                state_class(),
                let_(
                    result_v,
                    form::call(
                        "beamtalk_dispatch",
                        "lookup",
                        [var(sel), args, var("Self"), var("State"), var(class_v)],
                    ),
                    case_of(
                        var(result_v),
                        [
                            clause(
                                [docvec!["{", leaf::atom("error"), ", ", var(err_v), "}"]],
                                None,
                                docvec![
                                    "{",
                                    leaf::atom("error"),
                                    ", ",
                                    form::call(
                                        "beamtalk_exception_handler",
                                        "ensure_wrapped",
                                        [var(err_v)]
                                    ),
                                    ", State}",
                                ],
                                Layout::Block,
                            ),
                            clause([var(other_v)], None, var(other_v), Layout::Block),
                        ],
                        Layout::Block,
                    ),
                ),
            )
        };

        let arms = vec![
            // --- class ---
            arm("class", reply(state_class())),
            // --- respondsTo: ---
            arm(
                "respondsTo:",
                args_case(vec![
                    single_arg(
                        "[RtSel4|_]",
                        Some(form::call("erlang", "is_atom", [var("RtSel4")])),
                        let_(
                            "RtClass4",
                            state_class(),
                            reply(form::call(
                                "beamtalk_dispatch",
                                "responds_to",
                                [var("RtSel4"), var("RtClass4")],
                            )),
                        ),
                    ),
                    bad_args("_RtBadArgs", reply(leaf::atom("false"))),
                ]),
            ),
            // --- fieldNames ---
            arm(
                "fieldNames",
                reply(form::call(
                    "beamtalk_reflection",
                    "field_names",
                    [var("State")],
                )),
            ),
            // --- fieldAt: ---
            arm(
                "fieldAt:",
                args_case(vec![
                    single_arg(
                        "[IvaName4|_]",
                        None,
                        reply(form::call(
                            "beamtalk_reflection",
                            "read_field",
                            [var("IvaName4"), var("State")],
                        )),
                    ),
                    bad_args("_IvaBadArgs", self.arity_error_fragment("fieldAt:", 1)),
                ]),
            ),
            // --- fieldAt:put: ---
            arm(
                "fieldAt:put:",
                args_case(vec![
                    single_arg(
                        "[IvapName4|IvapRest4]",
                        None,
                        case_of(
                            var("IvapRest4"),
                            [
                                single_arg(
                                    "[IvapVal4|_]",
                                    None,
                                    let_(
                                        "IvapTuple4",
                                        form::call(
                                            "beamtalk_reflection",
                                            "write_field",
                                            [var("IvapName4"), var("IvapVal4"), var("State")],
                                        ),
                                        let_(
                                            "IvapResult4",
                                            form::call(
                                                "erlang",
                                                "element",
                                                [leaf::int_lit(1), var("IvapTuple4")],
                                            ),
                                            let_(
                                                "IvapNewState4",
                                                form::call(
                                                    "erlang",
                                                    "element",
                                                    [leaf::int_lit(2), var("IvapTuple4")],
                                                ),
                                                docvec![
                                                    "{",
                                                    leaf::atom("reply"),
                                                    ", IvapResult4, IvapNewState4}",
                                                ],
                                            ),
                                        ),
                                    ),
                                ),
                                bad_args("_IvapBad2", self.arity_error_fragment("fieldAt:put:", 2)),
                            ],
                            Layout::Block,
                        ),
                    ),
                    bad_args("_IvapBad1", self.arity_error_fragment("fieldAt:put:", 2)),
                ]),
            ),
            // --- hasField: (ADR 0124 §1/§9/B4) — never raises ---
            arm(
                "hasField:",
                args_case(vec![
                    single_arg(
                        "[HfName4|_]",
                        None,
                        reply(form::call(
                            "beamtalk_reflection",
                            "has_field",
                            [var("HfName4"), var("State")],
                        )),
                    ),
                    bad_args("_HfBadArgs", self.arity_error_fragment("hasField:", 1)),
                ]),
            ),
            // --- clearField: (ADR 0124 §1/§4f/§9/B4) — a write ---
            arm(
                "clearField:",
                args_case(vec![
                    single_arg(
                        "[ClfName4|_]",
                        None,
                        let_(
                            "ClfNewState4",
                            form::call(
                                "beamtalk_reflection",
                                "clear_field",
                                [var("ClfName4"), var("State")],
                            ),
                            docvec!["{", leaf::atom("reply"), ", Self, ClfNewState4}"],
                        ),
                    ),
                    bad_args("_ClfBadArgs", self.arity_error_fragment("clearField:", 1)),
                ]),
            ),
            // --- printString ---
            arm(
                "printString",
                let_(
                    "PsClass4",
                    form::call(
                        "beamtalk_tagged_map",
                        "class_of",
                        [var("State"), leaf::atom("Object")],
                    ),
                    let_(
                        "PsStr4",
                        form::call(
                            "erlang",
                            "atom_to_binary",
                            [var("PsClass4"), leaf::atom("utf8")],
                        ),
                        reply(var("PsStr4")),
                    ),
                ),
            ),
            // --- perform: ---
            arm(
                "perform:",
                args_case(vec![
                    single_arg(
                        "[PerfSel4|_]",
                        Some(form::call("erlang", "is_atom", [var("PerfSel4")])),
                        lookup_arm(
                            "PerfSel4",
                            Document::Str("[]"),
                            "PerfClass4",
                            "PerfResult4",
                            "PerfErr4",
                            "PerfOther4",
                        ),
                    ),
                    bad_args(
                        "_PerfBadArgs",
                        self.type_error_fragment("perform:", "selector must be an atom"),
                    ),
                ]),
            ),
            // --- perform:withArguments: ---
            arm(
                "perform:withArguments:",
                args_case(vec![
                    single_arg(
                        "[PwaSel4|PwaRest4]",
                        Some(form::call("erlang", "is_atom", [var("PwaSel4")])),
                        case_of(
                            var("PwaRest4"),
                            [
                                single_arg(
                                    "[PwaArgs4|_]",
                                    Some(form::call("erlang", "is_list", [var("PwaArgs4")])),
                                    lookup_arm(
                                        "PwaSel4",
                                        var("PwaArgs4"),
                                        "PwaClass4",
                                        "PwaResult4",
                                        "PwaErr4",
                                        "PwaOther4",
                                    ),
                                ),
                                bad_args(
                                    "_PwaBadArgList",
                                    self.type_error_fragment(
                                        "perform:withArguments:",
                                        "arguments must be a list",
                                    ),
                                ),
                            ],
                            Layout::Block,
                        ),
                    ),
                    bad_args(
                        "_PwaBadSel",
                        self.type_error_fragment(
                            "perform:withArguments:",
                            "selector must be an atom",
                        ),
                    ),
                ]),
            ),
            // --- Default: delegate to dispatch/3 ---
            bad_args("_OtherSel4", Self::dispatch_3_adapter(mod_name)),
        ];

        nest(
            INDENT,
            docvec![line(), case_of(var("Selector"), arms, Layout::Block)],
        )
    }

    /// Generates a simple dispatch/4 wrapper that delegates to dispatch/3.
    ///
    /// The result is the function body, starting on its own line: callers
    /// write the `'dispatch'/4 = fun (...) ->` header without a trailing
    /// newline.
    pub(in crate::core_erlang) fn generate_dispatch_4_wrapper(mod_name: &str) -> Document<'static> {
        nest(INDENT, docvec![line(), Self::dispatch_3_adapter(mod_name)])
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
    fn dispatch_3_adapter(mod_name: &str) -> Document<'static> {
        let caught = CatchVars {
            class: var("D4Type"),
            reason: var("D4Error"),
            stack: var("D4Stack"),
        };
        // `ensure_wrapped/4` (empty context): an already-wrapped exception
        // keeps its innermost `raw_stacktrace`, as in the walk's own
        // `raised/4`.
        let wrap_error = wrap_and_reraise_doc(
            &CatchVars {
                class: leaf::atom("error"),
                reason: var("D4Error"),
                stack: var("D4Stack"),
            },
            var("D4Built"),
            var("D4Wrapped"),
            Some(Document::Str("~{}~")),
            docvec!["{", leaf::atom("error"), ", D4Wrapped, State}"],
            Layout::Block,
        );
        let handler = case_of(
            var("D4Type"),
            [
                clause([leaf::atom("error")], None, wrap_error, Layout::Block),
                clause(
                    [var("_D4Other")],
                    None,
                    form::primop(
                        "raw_raise",
                        [
                            caught.class.clone(),
                            caught.reason.clone(),
                            caught.stack.clone(),
                        ],
                    ),
                    Layout::Block,
                ),
            ],
            Layout::Block,
        );
        try_of_catch(
            form::call(
                mod_name.to_string(),
                "dispatch",
                [var("Selector"), var("Args"), var("Self")],
            ),
            [var("D4Result")],
            reply(var("D4Result")),
            [caught.class, caught.reason, caught.stack],
            handler,
            Layout::Block,
        )
    }

    /// Generates a structured `arity_mismatch` error for dispatch/4 argument
    /// validation, answered as `{'error', Err, State}`. `selector` is the bare
    /// selector name.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    fn arity_error_fragment(&self, selector: &str, expected_arity: u32) -> Document<'static> {
        let hint = format!("Expected {expected_arity} argument(s) for {selector}");
        docvec![
            super::super::errors::beamtalk_error_doc(
                Document::Str("<ArErr0>"),
                Document::Str("ArErr0"),
                Document::Str("<ArErr1>"),
                Document::Str("ArErr1"),
                Document::Str("<ArErr2>"),
                "arity_mismatch",
                form::call(
                    "beamtalk_tagged_map",
                    "class_of",
                    [var("State"), leaf::atom("Object")]
                ),
                leaf::atom(selector.to_string()),
                leaf::binary_lit(hint),
                line(),
            ),
            "{'error', ArErr2, State}",
        ]
    }

    /// Generates a structured `type_error` for dispatch/4 argument validation,
    /// answered as `{'error', Err, State}`. `selector` is the bare selector
    /// name.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    fn type_error_fragment(&self, selector: &str, hint_msg: &str) -> Document<'static> {
        docvec![
            super::super::errors::beamtalk_error_doc(
                Document::Str("<TyErr0>"),
                Document::Str("TyErr0"),
                Document::Str("<TyErr1>"),
                Document::Str("TyErr1"),
                Document::Str("<TyErr2>"),
                "type_error",
                form::call(
                    "beamtalk_tagged_map",
                    "class_of",
                    [var("State"), leaf::atom("Object")]
                ),
                leaf::atom(selector.to_string()),
                leaf::binary_lit(hint_msg),
                line(),
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

        let own_methods: Vec<String> = class
            .methods
            .iter()
            .map(|m| m.selector.name().to_string())
            .collect();

        Ok(dispatch_spec::generate_has_method_from_spec(
            &own_methods,
            &DispatchSpec {
                reflection: PRIMITIVE_HAS_METHOD_REFLECTION,
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
            // class, respondsTo:, perform:, perform:withArguments:
            Document::Vec(
                dispatch_spec::value_core_reflection_arms(&class_name, &mod_name)
                    .into_iter()
                    .map(|(_, arm)| arm)
                    .collect()
            ),
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
                reflection: &dispatch_spec::value_core_reflection_selectors(),
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

#[cfg(test)]
mod reflection_tests {
    use super::{DISPATCH_PREFIX_SELECTORS, PRIMITIVE_HAS_METHOD_REFLECTION};
    use crate::core_erlang::dispatch_spec;

    #[test]
    fn primitive_has_method_reflection_matches_dispatch_arms() {
        let mut listed = PRIMITIVE_HAS_METHOD_REFLECTION.to_vec();
        let mut emitted = DISPATCH_PREFIX_SELECTORS.to_vec();
        listed.sort_unstable();
        emitted.sort_unstable();
        assert_eq!(listed, emitted);
    }

    #[test]
    fn value_core_reflection_selectors_are_the_emitted_arm_heads() {
        let arms = dispatch_spec::value_core_reflection_arms("Cls", "bt@cls");
        for (selector, arm) in &arms {
            let text = arm.to_pretty_string();
            assert!(
                text.starts_with(&format!("        <'{selector}'> when 'true' ->\n")),
                "arm for {selector} does not start with its own head: {text}"
            );
        }
        let selectors: Vec<_> = arms.iter().map(|(selector, _)| *selector).collect();
        assert_eq!(selectors, dispatch_spec::value_core_reflection_selectors());
    }
}
