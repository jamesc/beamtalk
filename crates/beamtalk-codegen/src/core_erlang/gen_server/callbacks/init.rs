// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor `gen_server` callbacks: `init/1` and its post-initialize checks (split from `callbacks`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::errors::{BeamtalkErrorChain, ErrorBinding};

use crate::core_erlang::sentinel_tags;
use crate::core_erlang::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::{ClassDefinition, Expression, Module, SlotKind};

use super::{
    CommittedStateKeys, INTERNAL_STATE_KEYS, InheritedTypedField, InitializeChainEntry,
    InternalStateKey, LogAndStop, arm, arm_inline, bind, case_block, clean_state_let,
    log_and_stop_arm, noreply, skip_initialize_case,
};

impl CoreErlangGenerator {
    /// Generates the `init/1` callback for `gen_server`.
    ///
    /// For classes with non-Actor superclasses, the init function:
    /// 1. Calls parent's `init(InitArgs)` to get inherited state
    /// 2. Creates a map with this class's metadata and fields
    /// 3. Merges: parent defaults → child defaults → user `InitArgs` (user wins)
    /// 4. Returns `{ok, FinalState}` or propagates parent init errors
    ///
    /// For base classes (extending Actor), it generates a simple init:
    /// 1. Creates a default state map with `__class_mod__` and field values
    /// 2. Merges `InitArgs` into the default state (`InitArgs` values override defaults)
    /// 3. Returns `{ok, FinalState}`
    ///
    /// # Generated Code (with inheritance)
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     case call 'counter':'init'(InitArgs) of
    ///         <{'ok', ParentState}> when 'true' ->
    ///             let ChildFields = ~{
    ///                 '__class_mod__' => 'logging_counter',
    ///                 '__shape_version__' => 1,
    ///                 'logCount' => 0
    ///             }~
    ///             in let MergedState = call 'maps':'merge'(ParentState, ChildFields)
    ///             in let CheckedArgs = <BT-4030 unknown-key check, leaf call only>
    ///             in let FinalState = call 'maps':'merge'(MergedState, CheckedArgs)
    ///             in {'ok', FinalState}
    ///         <{'error', Reason}> when 'true' ->
    ///             %% Propagate parent init error
    ///             {'error', Reason}
    ///     end
    /// ```
    ///
    /// # Generated Code (base class)
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     let DefaultState = ~{
    ///         '__class_mod__' => 'counter',
    ///         '__shape_version__' => 1,
    ///         'value' => 0
    ///     }~
    ///     in let CheckedArgs = <BT-4030 unknown-key check>
    ///     in let FinalState = call 'maps':'merge'(DefaultState, CheckedArgs)
    ///        in {'ok', FinalState}
    /// ```
    #[allow(clippy::too_many_lines)] // inheritance-aware init with parent state merge
    pub(in crate::core_erlang) fn generate_init_function(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Find the current class to check for superclass
        // NOTE: This requires the .bt file to have an explicit class definition
        // like "Counter subclass: LoggingCounter" (see tests/fixtures/logging_counter.bt).
        // Module-level expressions without a class definition take the base class path below.
        let current_class = self.current_class(module);

        // Check if we have a superclass that's not Actor (base class)
        // When true, we'll call the parent's init to inherit state fields
        let has_parent_init = current_class.is_some_and(Self::has_parent_actor_module);

        // Check if the class defines an initialize method.
        // If so, dispatch it at the end of init/1 — but only when not called
        // as a parent state-building helper. The __skip_initialize__ flag in
        // InitArgs suppresses dispatch when a child's init calls us as a helper.
        //
        // ADR 0078: Also defer to handle_continue when any *ancestor*
        // class defines initialize — handle_continue auto-chains each ancestor's
        // initialize parent-first. And defer when any class in the chain has
        // typed-no-default fields so the post-initialize validation runs.
        let chain_has_initialize = current_class.is_some_and(|c| {
            !self
                .user_defined_initialize_chain(module, &c.name.name)
                .is_empty()
        });
        let chain_has_typed_no_default = current_class.is_some_and(|c| {
            !self
                .inherited_typed_no_default_fields(module, &c.name.name)
                .is_empty()
        });
        let has_initialize = chain_has_initialize || chain_has_typed_no_default;

        // BT-3596: trap exits only for classes whose effective `terminate:`
        // is overridden somewhere in the hierarchy — see
        // `class_or_ancestor_overrides_terminate` doc comment. Actors that
        // never override `terminate:` keep today's plain-link semantics and
        // this leaves their `init/1` byte-identical.
        // BT-4052: a Server that overrides `onLinkedExit:reason:` also traps
        // (the hook alone suffices — no dummy `terminate:` needed).
        let traps_exit = current_class.is_some_and(|c| {
            self.class_or_ancestor_overrides_terminate(module, &c.name.name)
                || (self.is_server_subclass
                    && self.class_or_ancestor_defines_linked_exit_hook(module, &c.name.name))
        });

        let module_name = self.module_name.clone();

        // ADR 0123 §1: `'__shape_version__'` is the leaf class's own
        // declared `shapeVersion:` (default 1) — never inherited, since
        // shape versions and migration chains are per concrete class
        // (BT-3534 added the internal-fields plumbing; this is what makes
        // the written value non-default).
        let shape_version = current_class.map_or(1, ClassDefinition::effective_shape_version);

        // Use the clean Beamtalk class name (e.g., "EventStore") for
        // lifecycle telemetry metadata instead of the compiled Erlang module name
        // (e.g., "bt@exdura@event_store"). This matches how dispatch traces
        // report class names via lookup_class/1.
        let class_name = current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());

        // Generate the init return — either plain {ok, State} or
        // dispatch initialize first, then return {ok, NewState} / {stop, Error}.
        // When has_initialize is true, we wrap the dispatch in a guard that
        // checks __skip_initialize__ in InitArgs, so parent helpers skip it.
        // Lifecycle start telemetry is emitted here, guarded by
        // __skip_initialize__ so parent helper calls don't double-fire.
        let init_return = if has_initialize {
            Self::init_initialize_guarded_doc(&class_name)
        } else {
            Self::init_plain_return_doc(&class_name)
        };

        if has_parent_init {
            // Call parent's init to get inherited state, then merge with our state
            // SAFETY: has_parent_init is true only when current_class.is_some(),
            // so this expect cannot fail unless there's a logic error
            let class = current_class.expect("has_parent_init implies current_class is Some");
            let parent_module = {
                // ADR 0016: Use the same module naming logic as
                // superclass_module_name() which handles stdlib (bt@stdlib@*),
                // package (bt@{pkg}@*), and legacy (bt@*) modules
                self.compiled_module_name(class.superclass_name())
            };

            // Get this class's own state fields
            let own_state_fields = self.generate_own_state_fields(module)?;
            let (own_prelude, own_state_fields) =
                (own_state_fields.prelude, own_state_fields.fields);

            let doc = docvec![
                form::fun_def(
                    "init",
                    [leaf::var("InitArgs")],
                    // ADR 0079: set the `$beamtalk_actor` marker
                    // so `class named:` / `allRegistered` can filter this
                    // process as a Beamtalk actor. Guarded by
                    // `__skip_initialize__` so parent helpers don't overwrite
                    // the leaf class's marker.
                    Self::beamtalk_actor_marker_doc(
                        &class_name,
                        traps_exit,
                        docvec![
                            "%% Call parent init to get inherited state fields",
                            line(),
                            // Pass __skip_initialize__ to parent so it
                            // doesn't dispatch initialize — only the leaf should.
                            bind(
                                leaf::var("_ParentArgs"),
                                form::call(
                                    "maps",
                                    "put",
                                    [
                                        leaf::atom(sentinel_tags::SKIP_INITIALIZE),
                                        leaf::atom("true"),
                                        leaf::var("InitArgs"),
                                    ],
                                ),
                                case_block(
                                    form::call(parent_module, "init", [leaf::var("_ParentArgs")]),
                                    [
                                        arm(
                                            [form::tuple([
                                                leaf::atom("ok"),
                                                leaf::var("ParentState"),
                                            ])],
                                            docvec![
                                                "%% Merge parent state with the fields declared by this class",
                                                line(),
                                                own_prelude,
                                                // The `in let` chain keeps `in` at the start of each
                                                // line, which no `form::let_in` layout produces.
                                                "let ChildFields = ~{",
                                                nest(
                                                    INDENT,
                                                    docvec![
                                                        line(),
                                                        Self::internal_state_prefix_doc(
                                                            &module_name,
                                                            shape_version
                                                        ),
                                                        Document::Vec(own_state_fields),
                                                    ]
                                                ),
                                                line(),
                                                "}~",
                                                line(),
                                                "in let MergedState = ",
                                                form::call(
                                                    "maps",
                                                    "merge",
                                                    [
                                                        leaf::var("ParentState"),
                                                        leaf::var("ChildFields"),
                                                    ],
                                                ),
                                                line(),
                                                // Merge InitArgs last so user-provided values override defaults
                                                // Order: parent defaults → child defaults → user overrides
                                                // `self.class_name()` (not the AST-or-module
                                                // `class_name` above): identical when a class is
                                                // present, and consistent with the spawn arms
                                                // for expression modules with no class.
                                                Self::check_init_args_guarded_doc(
                                                    &self.class_name(),
                                                    "MergedState",
                                                ),
                                                line(),
                                                "in let FinalState = ",
                                                form::call(
                                                    "maps",
                                                    "merge",
                                                    [
                                                        leaf::var("MergedState"),
                                                        leaf::var("CheckedArgs"),
                                                    ],
                                                ),
                                                line(),
                                                init_return.clone(),
                                            ]
                                        ),
                                        arm(
                                            [form::tuple([
                                                leaf::atom("error"),
                                                leaf::var("Reason"),
                                            ])],
                                            docvec![
                                                "%% Propagate parent init error",
                                                line(),
                                                form::tuple([
                                                    leaf::atom("error"),
                                                    leaf::var("Reason")
                                                ]),
                                            ]
                                        ),
                                    ],
                                ),
                            ),
                        ],
                    ),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        } else {
            // No parent, or parent is Actor base class - generate normal init
            let initial_state_fields = self.generate_initial_state_fields(module)?;
            let (initial_prelude, initial_state_fields) =
                (initial_state_fields.prelude, initial_state_fields.fields);

            let doc = docvec![
                form::fun_def(
                    "init",
                    [leaf::var("InitArgs")],
                    // ADR 0079: set the `$beamtalk_actor` marker
                    // so `class named:` / `allRegistered` can filter this
                    // process as a Beamtalk actor. Guarded by
                    // `__skip_initialize__` so parent helpers don't overwrite
                    // the leaf class's marker.
                    Self::beamtalk_actor_marker_doc(
                        &class_name,
                        traps_exit,
                        docvec![
                            initial_prelude,
                            // The `in let` chain keeps `in` at the start of each
                            // line, which no `form::let_in` layout produces.
                            "let DefaultState = ~{",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    Self::internal_state_prefix_doc(&module_name, shape_version),
                                    Document::Vec(initial_state_fields),
                                ]
                            ),
                            line(),
                            "}~",
                            line(),
                            // Merge InitArgs into DefaultState - InitArgs values override defaults
                            // `self.class_name()`: see the MergedState path above.
                            Self::check_init_args_guarded_doc(&self.class_name(), "DefaultState"),
                            line(),
                            "in let FinalState = ",
                            form::call(
                                "maps",
                                "merge",
                                [leaf::var("DefaultState"), leaf::var("CheckedArgs")],
                            ),
                            line(),
                            init_return,
                        ],
                    ),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        }
    }

    /// BT-4030: the leaf-only `spawnWith:` unknown-key check.
    ///
    /// Binds `CheckedArgs` — `InitArgs` validated via `check_init_args/5`
    /// against the class's full declared field set (own, inherited, and
    /// `late`, resolved from metadata at runtime), raising `#unknown_field`
    /// for undeclared keys. Guarded by `__skip_initialize__` like the
    /// marker and `init_return` below: a parent-helper call carries the
    /// leaf's own keys and skips, leaving validation to the leaf. The
    /// `'$bt_init_ack'` protocol key rides in `ExtraAllowed` since the
    /// spawner adds it to every map's start args.
    ///
    /// Let-chain convention: opens with `in` (closing the base-state
    /// binding) and ends with a bare `end` (the following `in let
    /// FinalState` closes this one) — mirroring the neighbouring
    /// `in let MergedState` / `in let FinalState` items.
    fn check_init_args_guarded_doc(
        class_name: &str,
        base_state_var: &'static str,
    ) -> Document<'static> {
        docvec![
            "in let CheckedArgs = ",
            skip_initialize_case(
                arm_inline([leaf::atom("true")], leaf::var("InitArgs")),
                arm(
                    [leaf::atom("false")],
                    crate::core_erlang::errors::check_init_args_call_doc(
                        leaf::atom(class_name.to_owned()),
                        "spawnWith:",
                        Document::Str(base_state_var),
                        Document::Str("InitArgs"),
                        &[sentinel_tags::INIT_ACK],
                    ),
                ),
            ),
        ]
    }

    /// The `'__class_mod__'` and `'__shape_version__' => N` internal-key
    /// pair, as map-literal entries (each after the first needs its own
    /// leading `, `) — the prefix shared, byte for byte, by both `init/1`
    /// code paths: the base-class `DefaultState` map and the subclass
    /// `ChildFields` map both start every internal-key block with these
    /// same two entries against the same `module_name`.
    ///
    /// `'__shape_version__'` is ADR 0123 Phase 0's (BT-3534) hot-reload
    /// shape version key; `shape_version` is this leaf class's own
    /// effective `shapeVersion:` (ADR 0123 §1, default `1`) — never the
    /// superclass's, since shape versions are per concrete class.
    ///
    /// The entries come from [`INTERNAL_STATE_KEYS`], the same table the
    /// committed-state guard ([`clean_state_let`]) matches, so the guard
    /// can never disagree with the map `init/1` builds.
    fn internal_state_prefix_doc(module_name: &str, shape_version: u32) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for (i, key) in INTERNAL_STATE_KEYS.iter().enumerate() {
            if i > 0 {
                parts.push(line());
                parts.push(Document::Str(", "));
            }
            parts.push(leaf::atom(key.name()));
            parts.push(Document::Str(" => "));
            parts.push(match key {
                InternalStateKey::ClassMod => leaf::atom(module_name.to_owned()),
                InternalStateKey::ShapeVersion => leaf::int_lit(i64::from(shape_version)),
            });
        }
        Document::Vec(parts)
    }

    /// BT-4113: every declared field of the class and its ancestors: with
    /// [`INTERNAL_STATE_KEYS`], the key set a committed state map of this
    /// actor holds when it carries no codegen-internal `__local__`
    /// temporaries. The generated callbacks skip the `strip_local_temps/1`
    /// key scan when the new state holds all of these keys and no others
    /// (see [`clean_state_let`]).
    ///
    /// The guard checks presence as well as count, so it stays sound
    /// whatever the runtime field set turns out to be: an ancestor missing
    /// from the compile-time hierarchy, a superclass redefined without this
    /// module being recompiled, or a hot-reload shape migration only make it
    /// fall back to the scan. This answers `None` (always strip) where the
    /// key set is unknown or the fast path could rarely match: no class
    /// identity or hierarchy, a script module seeding extra keys from
    /// top-level assignments, or a `late` slot anywhere in the chain (a
    /// defaultless one is absent until first assigned).
    pub(super) fn committed_state_keys(&self, module: &Module) -> Option<CommittedStateKeys> {
        if module
            .expressions
            .iter()
            .any(|stmt| matches!(stmt.expression, Expression::Assignment { .. }))
        {
            return None;
        }
        let class = self.current_class(module)?;
        let hierarchy = self.class_hierarchy.as_ref()?;
        let class_name = class.name.name.as_str();
        hierarchy.get_class(class_name)?;
        let fields = hierarchy.all_state(class_name);
        if fields
            .iter()
            .any(|f| hierarchy.state_field_kind(class_name, f) == SlotKind::Late)
        {
            return None;
        }
        Some(CommittedStateKeys(fields))
    }

    /// ADR 0079: Generate the `$beamtalk_actor` process-dictionary
    /// marker call. Every Beamtalk actor init/1 sets this so `all_registered/0`,
    /// `class named:`, and future tooling can distinguish compiled Beamtalk
    /// actors from raw OTP processes via `erlang:process_info(Pid, dictionary)`.
    ///
    /// Guarded by `__skip_initialize__` so that when a subclass's init/1 calls
    /// the parent as a state-building helper, the parent does not overwrite
    /// the subclass's marker — the leaf class name is what downstream lookups
    /// need.
    ///
    /// BT-4937: the same branch also writes `'$beamtalk_actor_started_at'`
    /// (`erlang:system_time(microsecond)`), the age `resolve_global_conflict/3`
    /// compares. It is the writer for compiled actors.
    ///
    /// BT-3596: when `traps_exit` is true (this class or an ancestor
    /// overrides `terminate:`, see `class_or_ancestor_overrides_terminate`),
    /// also sets `trap_exit` on this actor's own `gen_server` process — in the
    /// same `__skip_initialize__`-guarded, non-helper branch, so it fires
    /// exactly once per spawned process and not on every parent-helper
    /// recursion. Without this, a supervisor's `exit(Pid, shutdown)` (used by
    /// both `terminate_child/2` and `rest_for_one`/`one_for_all` cascades)
    /// kills the process immediately and `terminate/2` never runs. When
    /// `traps_exit` is false, this is byte-identical to the pre-BT-3596
    /// output — actors that never override `terminate:` keep today's plain
    /// link semantics.
    fn beamtalk_actor_marker_doc(
        class_name: &str,
        traps_exit: bool,
        body: Document<'static>,
    ) -> Document<'static> {
        let put_marker = form::call(
            "erlang",
            "put",
            [
                leaf::atom("$beamtalk_actor"),
                leaf::atom(class_name.to_owned()),
            ],
        );
        // BT-4937 / ADR 0126 §4: the start-time marker
        // `beamtalk_actor_registry:resolve_global_conflict/3` reads (via
        // `actor_started_at/1`, possibly from another node) to keep the older
        // registrant. Written once per spawned process, in the same
        // `__skip_initialize__`-guarded branch as the class marker.
        let put_started_at = form::call(
            "erlang",
            "put",
            [
                leaf::atom("$beamtalk_actor_started_at"),
                form::call("erlang", "system_time", [leaf::atom("microsecond")]),
            ],
        );
        let put_markers = bind(leaf::var("_ClassMarker"), put_marker, put_started_at);
        let false_arm_body = if traps_exit {
            bind(
                leaf::var("_TrapExit"),
                form::call(
                    "erlang",
                    "process_flag",
                    [leaf::atom("trap_exit"), leaf::atom("true")],
                ),
                put_markers,
            )
        } else {
            put_markers
        };
        bind(
            leaf::var("_Marker"),
            skip_initialize_case(
                arm_inline([leaf::atom("true")], leaf::atom("skipped")),
                arm([leaf::atom("false")], false_arm_body),
            ),
            body,
        )
    }

    /// Generate the lifecycle call for `init/1` (`start`) and `terminate/2` (`stop`).
    ///
    /// Emits `call 'beamtalk_actor':'lifecycle_started'(Class)` or
    /// `'lifecycle_stopped'(Class, Reason)` (BT-4997). The runtime function
    /// announces `ActorSpawned` / `ActorStopped`, records the trace-store
    /// aggregate, and builds and emits the `[beamtalk, actor, lifecycle, _]`
    /// telemetry event only while lifecycle events are enabled, so the generated
    /// code builds no event metadata itself. Takes the clean Beamtalk class name
    /// (e.g., `EventStore`), not the compiled module name, so lifecycle traces
    /// match dispatch trace format.
    pub(super) fn lifecycle_call_let(
        function: &'static str,
        telemetry_var: &'static str,
        class_name: &str,
        extra_args: Vec<Document<'static>>,
        body: Document<'static>,
    ) -> Document<'static> {
        let mut args = vec![leaf::atom(class_name.to_owned())];
        args.extend(extra_args);
        bind(
            leaf::var(telemetry_var),
            form::call("beamtalk_actor", function, args),
            body,
        )
    }

    /// Generate a plain init return with lifecycle telemetry.
    ///
    /// For classes WITHOUT `initialize`, emits lifecycle start telemetry
    /// guarded by `__skip_initialize__` so parent helper calls don't double-fire.
    /// When called as a parent helper (flag is 'true'), skips telemetry and
    /// returns `{ok, State}` directly.
    fn init_plain_return_doc(class_name: &str) -> Document<'static> {
        docvec![
            // Guard telemetry behind __skip_initialize__ to avoid
            // double-fire when called as parent state-building helper
            "in ",
            skip_initialize_case(
                arm(
                    [leaf::atom("true")],
                    bind(
                        leaf::var("CleanState"),
                        form::call(
                            "maps",
                            "remove",
                            [
                                leaf::atom(sentinel_tags::SKIP_INITIALIZE),
                                leaf::var("FinalState")
                            ],
                        ),
                        form::tuple([leaf::atom("ok"), leaf::var("CleanState")]),
                    ),
                ),
                arm(
                    [leaf::atom("false")],
                    Self::lifecycle_call_let(
                        "lifecycle_started",
                        "_TelStart",
                        class_name,
                        Vec::new(),
                        // BT-3992: strip the starter's initialize-ack
                        // target and ack at once — no `initialize` step
                        // follows, so startup ends here.
                        bind(
                            leaf::var("AckState"),
                            form::call(
                                "beamtalk_actor",
                                "take_init_ack",
                                [leaf::var("FinalState"), leaf::atom("initialized")],
                            ),
                            form::tuple([leaf::atom("ok"), leaf::var("AckState")]),
                        ),
                    ),
                ),
            ),
        ]
    }

    /// Generate the guarded initialize dispatch block for init/1.
    ///
    /// Checks `__skip_initialize__` in `InitArgs` — when a child's init calls
    /// us as a parent state helper, it sets this flag to prevent double dispatch.
    /// When called as the outermost init (by OTP), the flag is absent and
    /// initialize is deferred to `handle_continue`.
    ///
    /// Uses OTP's `handle_continue` pattern instead of dispatching
    /// initialize inline. This ensures the `gen_server` message loop is running
    /// when initialize executes, so self-sends don't deadlock. OTP guarantees
    /// no messages arrive before `handle_continue` runs.
    ///
    /// Also emits lifecycle start telemetry in the non-helper branch.
    fn init_initialize_guarded_doc(class_name: &str) -> Document<'static> {
        docvec![
            "%% BT-1417/BT-1541: Defer initialize to handle_continue unless called as a parent helper",
            line(),
            "in ",
            skip_initialize_case(
                arm(
                    [leaf::atom("true")],
                    // Strip the flag from state before returning
                    bind(
                        leaf::var("CleanState"),
                        form::call(
                            "maps",
                            "remove",
                            [
                                leaf::atom(sentinel_tags::SKIP_INITIALIZE),
                                leaf::var("FinalState")
                            ],
                        ),
                        form::tuple([leaf::atom("ok"), leaf::var("CleanState")]),
                    ),
                ),
                arm(
                    [leaf::atom("false")],
                    // Strip the flag from state before returning
                    bind(
                        leaf::var("CleanState1"),
                        form::call(
                            "maps",
                            "remove",
                            [
                                leaf::atom(sentinel_tags::SKIP_INITIALIZE),
                                leaf::var("FinalState")
                            ],
                        ),
                        // Emit lifecycle start telemetry before returning
                        Self::lifecycle_call_let(
                            "lifecycle_started",
                            "_TelStart",
                            class_name,
                            Vec::new(),
                            // BT-3992: strip the starter's initialize-ack
                            // target and keep it for handle_continue's
                            // `initialize` step, which acks once it succeeds.
                            bind(
                                leaf::var("AckState"),
                                form::call(
                                    "beamtalk_actor",
                                    "take_init_ack",
                                    [leaf::var("CleanState1"), leaf::atom("initializing")],
                                ),
                                // Return {ok, State, {continue, initialize}} to defer
                                // initialize dispatch to handle_continue where the loop is running
                                form::tuple([
                                    leaf::atom("ok"),
                                    leaf::var("AckState"),
                                    form::tuple([leaf::atom("continue"), leaf::atom("initialize")]),
                                ]),
                            ),
                        ),
                    ),
                ),
            ),
        ]
    }

    /// Generates the success body for `handle_continue` after
    /// the auto-chained initialize sequence.
    ///
    /// Collects typed-no-default state fields from the full inheritance chain
    /// (ADR 0078). For each such field that is still `nil` after the chain
    /// completes, raises `UninitializedStateError` with the owning class name,
    /// field name, and expected type. If no such fields exist, returns a plain
    /// `{'noreply', InitNewState}`.
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_post_initialize_check(
        &self,
        current_class: Option<&ClassDefinition>,
        module: &Module,
        state_keys: Option<&CommittedStateKeys>,
        // Reserved for future log-metadata enrichment that names the leaf
        // spawning class in addition to the field's owning class.
        _leaf_class_name: &ecow::EcoString,
    ) -> Document<'static> {
        // Collect typed-no-default fields from the full inheritance
        // chain (leaf + ancestors), not just the current class.
        //
        // Ancestors defined outside this module fall back to
        // `ClassInfo` metadata so cross-file typed-no-default fields are
        // validated too.
        let typed_no_default: Vec<InheritedTypedField> = current_class
            .map(|c| self.inherited_typed_no_default_fields(module, &c.name.name))
            .unwrap_or_default();

        if typed_no_default.is_empty() {
            // strip codegen-internal `__local__` threading temps from the
            // post-initialize committed state, the same outermost-boundary clean-up
            // handle_call/handle_cast apply — an `initialize` that threads an outer
            // local must not persist the temporary into the actor's first state.
            return clean_state_let(
                "InitCleanState",
                "InitNewState",
                state_keys,
                noreply(leaf::var("InitCleanState")),
            );
        }

        // Build nested checks: each field gets a case that either stops or continues.
        // We build from the inside out — the innermost is the cleaned {'noreply', …}
        // (__local__ threading temps stripped from the committed state).
        let mut body: Document<'static> = noreply(leaf::var("InitCleanState"));

        for (i, field) in typed_no_default.iter().enumerate().rev() {
            let field_name = field.field_name.clone();
            let type_name = field.type_name.clone();
            // Use the owning class name (which may be an ancestor)
            // in the diagnostic, while keeping the leaf class in logger metadata
            // so operators see which actor failed to start.
            let owning_class = field.owning_class.as_str();
            let idx = i.to_string();
            let check_var = format!("_ChkVal{idx}");
            let err_var0 = format!("_UErr{idx}a");
            let err_var1 = format!("_UErr{idx}b");
            let err_msg_var = format!("_UErrMsg{idx}");
            let class_name_var = format!("_UErrClass{idx}");
            let fmt_var = format!("_UFmt{idx}");
            let hint_msg = format!(
                "{owning_class} field '{field_name}' (:: {type_name}) was not initialized",
            );

            body = bind(
                leaf::var(check_var.clone()),
                form::call(
                    "maps",
                    "get",
                    [leaf::atom(field_name), leaf::var("InitNewState")],
                ),
                case_block(
                    leaf::var(check_var),
                    [
                        arm(
                            [leaf::atom("nil")],
                            docvec![
                                BeamtalkErrorChain::new(
                                    "uninitialized_state_error",
                                    leaf::atom(owning_class.to_string())
                                )
                                .hint(leaf::binary_lit(&hint_msg))
                                .build(
                                    vec![
                                        ErrorBinding::plain(err_var0),
                                        ErrorBinding::plain(err_var1.clone()),
                                    ],
                                    line(),
                                ),
                                // terminating path — the state flows to
                                // terminate/2, not persistence, so no clean-up contract
                                // applies; use the cleaned binding for consistency with
                                // the success arm.
                                log_and_stop_arm(LogAndStop {
                                    msg_var: err_msg_var,
                                    class_name_var,
                                    module_name: &self.module_name,
                                    format_safe_args: leaf::var(err_var1.clone()),
                                    formatted_var: Some((
                                        fmt_var.clone(),
                                        leaf::var(err_var1.clone())
                                    )),
                                    reason: leaf::var(err_var1),
                                    stacktrace: None,
                                    stop_reason: leaf::var(fmt_var),
                                    stop_state: Document::Str("InitCleanState"),
                                }),
                            ],
                        ),
                        arm([leaf::var("_")], body),
                    ],
                ),
            );
        }

        // clean the committed state once before the field checks; the
        // checks read user fields (never `__local__`) so they are unaffected.
        clean_state_let(
            "InitCleanState",
            "InitNewState",
            state_keys,
            docvec![
                "%% BT-1949/BT-1951: Verify typed-no-default fields (including inherited) were initialized",
                line(),
                body,
            ],
        )
    }

    /// ADR 0078: Returns the list of user-defined initializers to run
    /// for a class, parent-first.
    ///
    /// Each entry identifies a class in the inheritance chain (leaf last) that
    /// defines its own `initialize` method. Root classes (`Actor`, `Object`,
    /// `ProtoObject`) are excluded because their `initialize` is a no-op default
    /// — running them would generate unnecessary dispatch calls.
    ///
    /// Uses the compile-time `ClassHierarchy` snapshot so cross-file ancestors
    /// are visible. When the hierarchy is unavailable (e.g. generator
    /// constructed standalone without `generate_module_with_warnings`), falls
    /// back to the leaf class's own `initialize` method if the AST defines one
    /// so single-class initialization still runs.
    pub(in crate::core_erlang) fn user_defined_initialize_chain(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> Vec<InitializeChainEntry> {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            // Fallback: AST only, leaf class initialize.
            return module
                .classes
                .iter()
                .find(|c| c.name.name == leaf_class)
                .filter(|c| c.methods.iter().any(|m| m.selector.name() == "initialize"))
                .map(|c| {
                    vec![InitializeChainEntry {
                        module_name: self.compiled_module_name(&c.name.name),
                    }]
                })
                .unwrap_or_default();
        };

        // Build the chain parent-first: leaf's superclass_chain returns
        // immediate ancestors first (Actor, Object, ProtoObject at the end
        // after user classes). Reverse to put the root ancestor first, then
        // append the leaf.
        let mut ordered: Vec<ecow::EcoString> = hierarchy
            .superclass_chain(leaf_class)
            .into_iter()
            .rev()
            .collect();
        ordered.push(ecow::EcoString::from(leaf_class));

        let mut out = Vec::new();
        for name in ordered {
            // Skip root no-op initialize owners — Actor's `initialize` is a
            // no-op per ADR 0078 and we don't generate a dispatch for it.
            if matches!(name.as_str(), "Actor" | "Object" | "ProtoObject") {
                continue;
            }
            let defines_initialize = hierarchy
                .get_class(name.as_str())
                .is_some_and(|info| info.methods.iter().any(|m| m.selector == "initialize"));
            if !defines_initialize {
                continue;
            }
            let module_name = self.compiled_module_name(name.as_str());
            out.push(InitializeChainEntry { module_name });
        }
        out
    }

    /// BT-3596: Returns true when `leaf_class`, or an ancestor other than
    /// `Actor`/`Object`/`ProtoObject`, defines its own `terminate:` method —
    /// i.e. overrides `Actor`'s no-op default `terminate:`.
    ///
    /// Drives the `trap_exit` gating in `generate_init_function`: an actor
    /// whose effective `terminate:` is never overridden keeps today's plain
    /// link semantics (killed immediately by a linked exit, `terminate/2`
    /// never runs), while one that does override it traps exits so OTP
    /// delivers a supervisor's `exit(Pid, shutdown)` — used by both
    /// `terminate_child/2` and `rest_for_one`/`one_for_all` cascades — as a
    /// message instead of killing the process outright, letting the normal
    /// parent-EXIT path in the generated `gen_server` loop call
    /// `terminate/2` before exiting.
    ///
    /// Same AST-only fallback as `user_defined_initialize_chain`: when the
    /// compile-time `ClassHierarchy` snapshot is unavailable, falls back to
    /// the leaf class's own AST methods so single-class compilation (e.g.
    /// the REPL, or a generator built without `generate_module_with_warnings`)
    /// still detects an override — it just can't see ancestors defined in
    /// other files.
    pub(in crate::core_erlang) fn class_or_ancestor_overrides_terminate(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> bool {
        self.class_or_ancestor_defines_method(
            module,
            leaf_class,
            "terminate:",
            &["Actor", "Object", "ProtoObject"],
        )
    }

    /// BT-4052: Returns true when `leaf_class`, or an ancestor other than
    /// `Server`/`Actor`/`Object`/`ProtoObject`, defines its own
    /// `onLinkedExit:reason:` method — i.e. overrides `Server`'s no-op default.
    ///
    /// Drives both the `trap_exit` gating in `generate_init_function` (for
    /// Server subclasses) and the baked flag passed to
    /// `beamtalk_actor:handle_linked_exit/3` in `generate_handle_info`. Same
    /// AST-only no-hierarchy fallback as `class_or_ancestor_overrides_terminate`.
    pub(in crate::core_erlang) fn class_or_ancestor_defines_linked_exit_hook(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> bool {
        self.class_or_ancestor_defines_method(
            module,
            leaf_class,
            "onLinkedExit:reason:",
            &["Server", "Actor", "Object", "ProtoObject"],
        )
    }

    /// Shared walk for the override predicates above: does `leaf_class` or an
    /// ancestor not in `excluded_defaults` define `selector`?
    fn class_or_ancestor_defines_method(
        &self,
        module: &Module,
        leaf_class: &str,
        selector: &str,
        excluded_defaults: &[&str],
    ) -> bool {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            return module
                .classes
                .iter()
                .find(|c| c.name.name == leaf_class)
                .is_some_and(|c| c.methods.iter().any(|m| m.selector.name() == selector));
        };

        let mut ordered: Vec<ecow::EcoString> = hierarchy
            .superclass_chain(leaf_class)
            .into_iter()
            .rev()
            .collect();
        ordered.push(ecow::EcoString::from(leaf_class));

        ordered.iter().any(|name| {
            if excluded_defaults.contains(&name.as_str()) {
                return false;
            }
            hierarchy
                .get_class(name.as_str())
                .is_some_and(|info| info.methods.iter().any(|m| m.selector == selector))
        })
    }

    /// ADR 0078: Collect typed-no-default state fields across the
    /// inheritance chain for post-initialize validation.
    ///
    /// Returns fields in parent-first order (ancestors first, then the leaf's
    /// own fields), annotated with the owning class name so diagnostics can
    /// point at the class that declared the field.
    ///
    /// When an ancestor's AST is not present in the current `Module`
    /// (cross-file parent), the function falls back to `ClassInfo` metadata
    /// loaded from that ancestor's `__beamtalk_meta/0` (see `state_types` /
    /// `state_has_default`). This closes the cross-file inherited-field
    /// validation gap.
    pub(in crate::core_erlang) fn inherited_typed_no_default_fields(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> Vec<InheritedTypedField> {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            // Fallback: AST only, leaf class fields.
            return module
                .classes
                .iter()
                .find(|c| c.name.name == leaf_class)
                .map(|c| {
                    c.state
                        .iter()
                        .filter(|s| {
                            beamtalk_core::semantic_analysis::requires_definite_assignment(
                                s,
                                &self.alias_registry,
                            )
                        })
                        .map(|s| InheritedTypedField {
                            owning_class: c.name.name.to_string(),
                            field_name: s.name.name.to_string(),
                            type_name: s.type_annotation.as_ref().map_or_else(
                                || "Unknown".to_string(),
                                |ta| ta.type_name().to_string(),
                            ),
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
        };

        let mut ordered: Vec<ecow::EcoString> = hierarchy
            .superclass_chain(leaf_class)
            .into_iter()
            .rev()
            .collect();
        ordered.push(ecow::EcoString::from(leaf_class));

        let mut out = Vec::new();
        for name in ordered {
            if matches!(name.as_str(), "Actor" | "Object" | "ProtoObject") {
                continue;
            }
            if let Some(class) = module.classes.iter().find(|c| c.name.name == name) {
                for s in &class.state {
                    if beamtalk_core::semantic_analysis::requires_definite_assignment(
                        s,
                        &self.alias_registry,
                    ) {
                        out.push(InheritedTypedField {
                            owning_class: class.name.name.to_string(),
                            field_name: s.name.name.to_string(),
                            type_name: s.type_annotation.as_ref().map_or_else(
                                || "Unknown".to_string(),
                                |ta| ta.type_name().to_string(),
                            ),
                        });
                    }
                }
            } else if let Some(info) = hierarchy.get_class(&name) {
                // Cross-file ancestor — use ClassInfo metadata.
                // Preserve the declaration order by iterating `info.state`.
                for field_name in &info.state {
                    let Some(type_name) = info.state_types.get(field_name) else {
                        continue; // untyped field — not a typed-no-default
                    };
                    // `ClassHierarchy::state_field_has_default` conservatively
                    // treats a missing default-info entry (e.g., older BEAM
                    // artifact) as "has default" so we don't raise a spurious
                    // UninitializedStateError on stale metadata — see its doc
                    // comment for the shared degradation rule this and
                    // `TypeChecker::check_value_construction_definite_assignment`
                    // both rely on.
                    if hierarchy.state_field_has_default(&name, field_name) {
                        continue;
                    }
                    // ADR 0124 §1/B2: a `late` slot opts out of definite
                    // assignment exactly like the AST path's
                    // `requires_definite_assignment` (`SlotKind::Late` short-
                    // circuits before nilability is even considered) —
                    // mirrored here via `state_kinds` since a cross-file
                    // ancestor has no `StateDeclaration` to hand that
                    // predicate directly. Missing metadata (an older
                    // `__beamtalk_meta/0` predating `state_kinds`) degrades to
                    // `SlotKind::Eager`, matching `state_field_kind`'s own
                    // fallback.
                    let kind = info
                        .state_kinds
                        .get(field_name)
                        .copied()
                        .unwrap_or(SlotKind::Eager);
                    if kind == SlotKind::Late {
                        continue;
                    }
                    let type_name = type_name.to_string();
                    if beamtalk_core::semantic_analysis::is_nilable_type_name(
                        &type_name,
                        &self.alias_registry,
                    ) {
                        continue;
                    }
                    out.push(InheritedTypedField {
                        owning_class: info.name.to_string(),
                        field_name: field_name.to_string(),
                        type_name,
                    });
                }
            }
        }
        out
    }
}
