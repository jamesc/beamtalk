%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_spec).

%%% **DDD Context:** Actor System Context

-moduledoc """
Child-spec building for Beamtalk `Supervisor` and `DynamicSupervisor`.

Translates the Beamtalk `SupervisionSpec childSpec` dictionary into an OTP
child-spec map: the start MFA (`spec_to_otp/2`), the strategy and shutdown
values (`to_otp_strategy/1`, `to_otp_shutdown/3`) and the nested-supervisor
start trampoline selection. Split out of `beamtalk_supervisor` (BT-4705); a
pure move with no behaviour change.

The start MFAs it emits name `beamtalk_supervisor` functions
(`start_actor_child`, `start_dynamic_child`, `start_child_via_class_method`,
`start_supervisor_child`, ...) by module name. Those stay exported from
`beamtalk_supervisor`: they are stored in live OTP child specs, so they must
keep their module and name.

## References

- ADR 0059: Supervision Tree Syntax
- ADR 0135 §3: nested supervisor children
- runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl
""".

-export([
    build_child_spec/2,
    spec_to_otp/1,
    spec_to_otp/2,
    to_otp_strategy/1,
    to_otp_shutdown/3,
    check_child_shutdown/2,
    apply_child_shutdown/2
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% BT-3965: largest Integer shutdown timeout. OTP's own child-spec check has
%% no upper bound, but a static supervisor waits with `receive ... after`,
%% whose timeout is limited to 16#FFFFFFFF ms; a larger value would crash the
%% supervisor mid-shutdown. `#infinity` covers anything longer.
-define(MAX_SHUTDOWN_MS, 16#FFFFFFFF).

-doc "Build a single OTP child spec from a class object or SupervisionSpec map.".
-spec build_child_spec(term(), static | dynamic) -> map().
build_child_spec(ClassObj, Mode) when
    is_tuple(ClassObj), element(1, ClassObj) =:= beamtalk_object
->
    case beamtalk_class_registry:is_class_object(ClassObj) of
        true ->
            %% A bare class, actor or supervisor alike (ADR 0135 §3): its
            %% `supervisionSpec`, then `childSpec`. A bare nested supervisor
            %% is therefore an anonymous child, like any other.
            ChildClassPid = element(4, ClassObj),
            BtSpec = beamtalk_object_class:class_send(ChildClassPid, 'supervisionSpec', []),
            spec_to_otp(beamtalk_message_dispatch:send(BtSpec, 'childSpec', []), Mode);
        false ->
            %% Actor instance passed as spec — treat as SupervisionSpec-like value
            spec_to_otp(beamtalk_message_dispatch:send(ClassObj, 'childSpec', []), Mode)
    end;
build_child_spec(Spec, Mode) when is_map(Spec) ->
    %% SupervisionSpec value (tagged map): call childSpec directly
    spec_to_otp(beamtalk_message_dispatch:send(Spec, 'childSpec', []), Mode).

-doc """
Translate Beamtalk strategy symbol to OTP supervisor strategy atom.

OTP expects snake_case atoms (one_for_one), while Beamtalk uses camelCase
symbols (#oneForOne). Unknown strategies pass through unchanged so OTP
can report the error with context.
""".
-spec to_otp_strategy(atom()) -> atom().
to_otp_strategy(oneForOne) -> one_for_one;
to_otp_strategy(oneForAll) -> one_for_all;
to_otp_strategy(restForOne) -> rest_for_one;
to_otp_strategy(S) -> S.

-doc """
Convert a Beamtalk child spec dict to an OTP-compatible child spec map.

The Beamtalk dict from `SupervisionSpec childSpec` has keys:
  id, start ([ClassObj, FnAtom, ArgsList]), restart, shutdown, type
The `start` value is a Beamtalk Array [ClassObj, #spawn, #()] that must be
converted to the OTP MFA tuple {Module, Function, Args}.

For nested supervisor children (Supervisor/DynamicSupervisor subclasses), the
start goes through the `start_supervisor_child` trampoline (ADR 0135 §3; see
`supervisor_child_start/4`), so the child is anonymous, gets the spec's args
and stays linked into the tree.

For actor (worker) children other than `withClassMethod:` factories, the
start goes through `start_actor_child/2,3` (BT-4033): an awaited, linked
start that answers `{ok, Pid}` once the child's `initialize` has returned,
and `{error, Reason}` when it failed — so a failing `initialize` fails the
parent's start, as OTP `init/1` returning `{stop, Reason}` does.

`Mode` selects between a static Supervisor's immediate-start semantics and
a DynamicSupervisor's extensible `simple_one_for_one` template — see the
`spawn` case below.
""".
-spec spec_to_otp(map()) -> map().
spec_to_otp(BtSpec) ->
    spec_to_otp(BtSpec, static).

-spec spec_to_otp(map(), static | dynamic) -> map().
spec_to_otp(BtSpec, Mode) ->
    %% `start` is a Beamtalk Array #[ClassObj, StartFn, StartArgs]. Beamtalk
    %% Arrays are tagged maps with a canonical index→value `'data'` map (ADR
    %% 0090); read the elements as an ordered list via beamtalk_tagged_map.
    StartBtArray = maps:get(start, BtSpec),
    StartElems = beamtalk_tagged_map:array_to_list(StartBtArray),
    ClassObj = lists:nth(1, StartElems),
    ChildModule = element(3, ClassObj),
    ChildClassPid = element(4, ClassObj),
    ChildClass = beamtalk_object_class:class_name_safe(ChildClassPid),
    OtpStart =
        case beamtalk_supervisor:is_supervisor(ChildClass) of
            true ->
                supervisor_child_start(ChildClass, ChildModule, StartElems, Mode);
            false ->
                %% Worker: start through start_actor_child/2,3 (BT-4033), the
                %% awaited linked start answering {ok, Pid} | {error, Reason}.
                StartFn = lists:nth(2, StartElems),
                case StartFn of
                    spawn when Mode =:= dynamic ->
                        %% A DynamicSupervisor's simple_one_for_one template
                        %% must NOT bake a static arg here. OTP appends whatever extra
                        %% args supervisor:start_child/2 was given to this MFA's args —
                        %% baking `[#{}]` meant `startChild: args` landed as
                        %% `ChildModule:start_link(#{}, Args)`, a 2-arity call that
                        %% doesn't exist as start_link/1 and gets misread by OTP as the
                        %% 4-arity named-registration form, crashing with badarg.
                        %% Using a zero-static-arg indirection through
                        %% start_dynamic_child/2,3 instead lets both the no-arg
                        %% `startChild` (arity 2: DefaultArgs) and `startChild: args`
                        %% (arity 3: caller's Args wins) resolve to the single correct
                        %% argument for start_actor_child/2. See
                        %% start_dynamic_child/2,3's docs for how each restarts —
                        %% OTP's own simple_one_for_one bookkeeping replays each
                        %% child's own original start args, so this is unaffected
                        %% either way.
                        {beamtalk_supervisor, start_dynamic_child, [ChildModule, #{}]};
                    spawn ->
                        %% Static Supervisor: no init args — start with empty state map.
                        %% Static children start once at supervisor-init time and never
                        %% have extra args appended, so a single baked arg is correct.
                        %% BT-4033: awaited start, see start_actor_child/2.
                        {beamtalk_supervisor, start_actor_child, [ChildModule, #{}]};
                    'spawnWith:' when Mode =:= dynamic ->
                        %% A DynamicSupervisor childClass
                        %% can override `class supervisionSpec` to bake default args via
                        %% `withArgs:` (supervision_spec.bt childSpec), which also compiles
                        %% to startFn #spawnWith:. That hits the exact same arity-mismatch
                        %% badarg as the plain #spawn case once `startChild: args` appends
                        %% its own args on top of the baked ones — so it needs the same
                        %% zero-static-arg indirection, using the baked map as DefaultArgs
                        %% instead of #{}.
                        [InitArgsMap] = lists:nth(3, StartElems),
                        {beamtalk_supervisor, start_dynamic_child, [ChildModule, InitArgsMap]};
                    'spawnWith:' ->
                        %% #(self args) uses list syntax (#(...)) so it compiles to an
                        %% Erlang list [ArgsMap]. BT-4033: awaited start, see
                        %% start_actor_child/2.
                        [InitArgs] = lists:nth(3, StartElems),
                        {beamtalk_supervisor, start_actor_child, [ChildModule, InitArgs]};
                    'spawnAs:' ->
                        %% ADR 0079: SupervisionSpec withName: routes
                        %% named children through `beamtalk_actor_registry:spawnAs/3`
                        %% (via start_actor_child/3, BT-4033) so the child registers
                        %% under `Name` atomically inside
                        %% gen_server:start_link({local, Name}, ...). The startArgs
                        %% in `SupervisionSpec childSpec` carry just the Name as
                        %% `#(name)` — i.e. an Erlang list `[Name]`.
                        SpawnAsArgs = lists:nth(3, StartElems),
                        [Name] = SpawnAsArgs,
                        {beamtalk_supervisor, start_actor_child, [ChildModule, #{}, {local, Name}]};
                    'spawnWith:as:' ->
                        %% ADR 0079: spawn-with-args under a registered
                        %% name. startArgs are `#(args, name)` → Erlang `[Args, Name]`.
                        %% Goes through `beamtalk_actor_registry:spawnAs/3` (via
                        %% start_actor_child/3) so the child is registered
                        %% atomically with init args at start time.
                        SpawnAsArgs2 = lists:nth(3, StartElems),
                        [InitArgsMap, NameAtom] = SpawnAsArgs2,
                        {beamtalk_supervisor, start_actor_child, [
                            ChildModule, InitArgsMap, {local, NameAtom}
                        ]};
                    'spawnAs:scope:' ->
                        %% ADR 0126 §4, Phase 5: `SupervisionSpec withName:
                        %% scope: #global` — same shape as `spawnAs:` above
                        %% (startArgs carries just the Name, `#(name)` →
                        %% Erlang `[Name]`), but routes through
                        %% `beamtalk_actor_registry:'spawnAsGlobal'/3` (via
                        %% start_actor_child/3) so the child
                        %% registers with OTP `global` instead of the local
                        %% registry. `childSpec` only ever emits this
                        %% startFn when `scope` is `#global` — `#local`
                        %% still emits plain `#spawnAs:` above.
                        SpawnAsGlobalArgs = lists:nth(3, StartElems),
                        [GlobalName] = SpawnAsGlobalArgs,
                        {beamtalk_supervisor, start_actor_child, [
                            ChildModule, #{}, {global, GlobalName}
                        ]};
                    'spawnWith:as:scope:' ->
                        %% ADR 0126 §4, Phase 5: cluster-global spawn-with-args
                        %% sibling of `spawnWith:as:` above — same startArgs
                        %% shape (`#(args, name)` → Erlang `[Args, Name]`),
                        %% routed through `beamtalk_actor_registry:'spawnAsGlobal'/3`.
                        SpawnAsGlobalArgs2 = lists:nth(3, StartElems),
                        [GlobalInitArgsMap, GlobalNameAtom] = SpawnAsGlobalArgs2,
                        {beamtalk_supervisor, start_actor_child, [
                            ChildModule, GlobalInitArgsMap, {global, GlobalNameAtom}
                        ]};
                    classMethod ->
                        %% Route through the actor's keyword class method.
                        %% StartArgs is #(selector, argsList) — compiled as an
                        %% Erlang list [Selector, ArgsList]. The selector is stored
                        %% in source form (e.g., 'create:value:'); we prepend 'class_'
                        %% to get the compiled function name for call_class_method_direct.
                        [Selector, ArgsList] = lists:nth(3, StartElems),
                        % elp:fixme W0023 intentional atom creation
                        CompiledSelector = list_to_atom(
                            "class_" ++ atom_to_list(Selector)
                        ),
                        {beamtalk_supervisor, start_child_via_class_method, [
                            ChildClass, ChildModule, CompiledSelector, ArgsList
                        ]};
                    Other ->
                        %% Unknown start function — raise structured error rather than crashing.
                        %% Tag as 'SupervisionSpec'/'childSpec' to reflect where the spec
                        %% originated (SupervisionSpec>>childSpec), not the supervisor itself.
                        Error = beamtalk_error:new_fmt(
                            runtime_error,
                            'SupervisionSpec',
                            childSpec,
                            "unsupported child start function: ~tp "
                            "(expected spawn, spawnWith:, spawnAs:, "
                            "spawnWith:as:, spawnAs:scope:, "
                            "spawnWith:as:scope:, or classMethod)",
                            [Other]
                        ),
                        beamtalk_exception_handler:reraise(Error)
                end
        end,
    #{
        id => maps:get(id, BtSpec),
        start => OtpStart,
        restart => maps:get(restart, BtSpec),
        shutdown => to_otp_shutdown('SupervisionSpec', shutdown, maps:get(shutdown, BtSpec)),
        type => maps:get(type, BtSpec),
        modules => [ChildModule]
    }.

-doc """
The start MFA for a nested supervisor child (ADR 0135 §3), from the
`#[Class, StartFn, StartArgs]` that `SupervisionSpec childSpec` built.

A static parent's child starts through `start_supervisor_child/2,3` with the
spec's args (`nil` without `withArgs:`) and, for `withName:`, its name. A
DynamicSupervisor's `simple_one_for_one` template is
`start_dynamic_supervisor_child` with the spec's args as the default, so
`startChild` / `startChild:` / `startChild:name:` each start a new child.
`withClassMethod:` (a class-method factory would run the start in the class
process and link the child there) and `scope: #global` are not supported for
supervisor children: they raise a structured `type_error`, which fails the
parent's start.
""".
-spec supervisor_child_start(atom(), module(), [term()], static | dynamic) ->
    {module(), atom(), [term()]}.
supervisor_child_start(ChildClass, ChildModule, StartElems, Mode) ->
    StartFn = lists:nth(2, StartElems),
    StartArgs = lists:nth(3, StartElems),
    {Args, Name} =
        case {StartFn, StartArgs} of
            {spawn, []} -> {nil, none};
            {'spawnWith:', [A]} -> {A, none};
            {'spawnAs:', [N]} -> {nil, N};
            {'spawnWith:as:', [A, N]} -> {A, N};
            _ -> unsupported_supervisor_child(ChildClass, StartFn)
        end,
    case {Mode, Name} of
        {static, none} ->
            {beamtalk_supervisor, start_supervisor_child, [ChildModule, Args]};
        {static, _} ->
            {beamtalk_supervisor, start_supervisor_child, [ChildModule, Args, Name]};
        {dynamic, none} ->
            {beamtalk_supervisor, start_dynamic_supervisor_child, [ChildModule, Args]};
        {dynamic, _} ->
            %% A pool's single template cannot carry one name for every
            %% child; `startChild:name:` names each child instead.
            unsupported_supervisor_child(ChildClass, StartFn)
    end.

-spec unsupported_supervisor_child(atom(), term()) -> no_return().
unsupported_supervisor_child(ChildClass, StartFn) ->
    Hint = beamtalk_error:chars_to_binary(
        io_lib:format(
            "~ts is a supervisor: its supervisionSpec supports withId:, withArgs:, "
            "withRestart:, withShutdown: and withName: (a static child), not ~ts",
            [ChildClass, unsupported_field(StartFn)]
        )
    ),
    Error0 = beamtalk_error:new(type_error, ChildClass, supervisionSpec, Hint),
    beamtalk_exception_handler:reraise(
        beamtalk_error:with_details(Error0, #{start => StartFn})
    ).

-spec unsupported_field(term()) -> string().
unsupported_field(classMethod) -> "withClassMethod:";
unsupported_field('spawnAs:scope:') -> "scope: #global";
unsupported_field('spawnWith:as:scope:') -> "scope: #global";
unsupported_field('spawnAs:') -> "withName: on a DynamicSupervisor's child class";
unsupported_field('spawnWith:as:') -> "withName: on a DynamicSupervisor's child class";
unsupported_field(Other) -> io_lib:format("~p", [Other]).

-doc """
Translate a Beamtalk shutdown value into the OTP child-spec `shutdown` term
(BT-3965).

Accepts an Integer from 0 to `?MAX_SHUTDOWN_MS` (milliseconds), `infinity`
(Beamtalk `#infinity`) or `brutalKill` (Beamtalk `#brutalKill`, emitted as
OTP `brutal_kill`); anything else raises a `type_error` attributed to
`Class`/`Selector`, with the offending value in `details`. The single place
the `ShutdownTimeout` rule is checked: both `SupervisionSpec childSpec`
(via `spec_to_otp/2`) and `DynamicSupervisor childShutdown` go
through it.
""".
-spec to_otp_shutdown(atom(), atom(), term()) -> brutal_kill | infinity | non_neg_integer().
to_otp_shutdown(Class, Selector, Value) ->
    case check_shutdown(Class, Selector, Value) of
        {ok, Shutdown} -> Shutdown;
        {error, Error} -> beamtalk_error:raise(Error)
    end.

-spec check_shutdown(atom(), atom(), term()) ->
    {ok, brutal_kill | infinity | non_neg_integer()} | {error, #beamtalk_error{}}.
check_shutdown(_Class, _Selector, infinity) ->
    {ok, infinity};
check_shutdown(_Class, _Selector, brutalKill) ->
    {ok, brutal_kill};
check_shutdown(_Class, _Selector, Ms) when is_integer(Ms), Ms >= 0, Ms =< ?MAX_SHUTDOWN_MS ->
    {ok, Ms};
check_shutdown(Class, Selector, Other) ->
    {error,
        beamtalk_error:bad_value(
            Class,
            Selector,
            "~ts must be an Integer from 0 to ~b (ms), #infinity or #brutalKill, got ~tp",
            [Selector, ?MAX_SHUTDOWN_MS, Other],
            Other
        )}.

-doc """
Check a DynamicSupervisor's `childShutdown` (BT-3965): `nil` (use the
child class's own spec value) or any value `check_shutdown/3` accepts.
""".
-spec check_child_shutdown(atom(), term()) ->
    {ok, nil | brutal_kill | infinity | non_neg_integer()} | {error, #beamtalk_error{}}.
check_child_shutdown(_ClassName, nil) ->
    {ok, nil};
check_child_shutdown(ClassName, Value) ->
    check_shutdown(ClassName, childShutdown, Value).

-spec apply_child_shutdown(map(), nil | brutal_kill | infinity | non_neg_integer()) -> map().
apply_child_shutdown(Spec, nil) ->
    Spec;
apply_child_shutdown(#{type := supervisor, id := Id} = Spec, Shutdown) when
    Shutdown =/= infinity
->
    %% OTP allows it, but a supervisor child with a finite shutdown can be
    %% killed while it is still stopping its own children, which then exit
    %% with `killed` instead of their own shutdown. Applied as configured.
    ?LOG_WARNING("DynamicSupervisor childShutdown is not #infinity for a supervisor child", #{
        child => Id,
        child_shutdown => Shutdown,
        domain => [beamtalk, runtime]
    }),
    Spec#{shutdown => Shutdown};
apply_child_shutdown(Spec, Shutdown) ->
    Spec#{shutdown => Shutdown}.
