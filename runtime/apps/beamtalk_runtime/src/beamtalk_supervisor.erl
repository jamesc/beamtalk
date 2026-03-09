%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Erlang runtime glue for Beamtalk Supervisor and DynamicSupervisor.
%%%
%%% **DDD Context:** Actor System Context
%%%
%%% This module provides the BEAM interop entry points called from the
%%% Supervisor and DynamicSupervisor stdlib methods via Erlang FFI.
%%% Generated `init/1` callbacks delegate to `static_init/2` and `dynamic_init/2`
%%% (Phase 3 codegen) to avoid gen_server deadlocks. `is_supervisor/1` is used
%%% for compile-time routing and child spec construction.
%%%
%%% ## Design (ADR 0059 Phase 2)
%%%
%%% Supervisor instances are represented as:
%%%   `{beamtalk_supervisor, ClassName, Module, Pid}`
%%%
%%% This distinct tuple tag allows `beamtalk_message_dispatch` to route
%%% messages directly to `Module:'method'(Self)` (Phase 3) or via the
%%% stdlib hierarchy walk (Phase 2) without going through gen_server.
%%%
%%% OTP supervisor behaviour handles `handle_call/3` internally, so inspection
%%% methods (`children`, `which:`, etc.) are implemented as exported module
%%% functions that call OTP APIs from the caller's process context.
%%%
%%% ## References
%%%
%%% - ADR 0059: Supervision Tree Syntax, Phase 2
%%% - stdlib/src/Supervisor.bt, DynamicSupervisor.bt
%%% - runtime/apps/beamtalk_runtime/src/beamtalk_message_dispatch.erl (routing)

-module(beamtalk_supervisor).

-export([
    startLink/1,
    current/1,
    static_init/2,
    dynamic_init/2,
    whichChildren/1,
    whichChild/2,
    terminateChild/2,
    startChild/1,
    startChild/2,
    countChildren/1,
    stop/1,
    build_child_specs/1,
    is_supervisor/1,
    register_root/1,
    get_root/0
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% ETS table name for the OTP application root supervisor registry (BT-1191).
%% Stores `{root, SupervisorTuple}` where SupervisorTuple is a
%% `{beamtalk_supervisor, ClassName, Module, Pid}` value.
-define(ROOT_SUPERVISOR_TABLE, beamtalk_root_supervisor).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Start (or return) the running supervisor for the given class.
%%
%% Called from `class supervise` on Supervisor and DynamicSupervisor subclasses.
%% Self is the class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
%%
%% Returns `{beamtalk_supervisor, ClassName, Module, Pid}`.
%% Idempotent: if the supervisor is already running, returns the existing instance.
-spec startLink(tuple()) -> tuple().
startLink(Self) ->
    ClassPid = element(4, Self),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    Module = beamtalk_object_class:module_name(ClassPid),
    case Module:start_link() of
        {ok, Pid} ->
            {beamtalk_supervisor, ClassName, Module, Pid};
        {error, {already_started, Pid}} ->
            {beamtalk_supervisor, ClassName, Module, Pid};
        {error, Reason} ->
            Error = beamtalk_error:new(
                runtime_error,
                ClassName,
                supervise,
                iolist_to_binary(io_lib:format("supervisor start_link failed: ~p", [Reason]))
            ),
            error(beamtalk_exception_handler:ensure_wrapped(Error))
    end.

%% @doc Initialize a static supervisor without calling through the class gen_server.
%%
%% Called from the generated `init/1` callback of Supervisor subclasses.
%%
%% The problem with calling `beamtalk_object_class:class_send/3` from `init/1`:
%% OTP spawns a new process and calls `Module:init([])`. Inside `init/1`, calling
%% `class_send(ClassPid, ...)` sends a `gen_server:call` to ClassPid — which is
%% blocked inside `startLink/1` waiting for `supervisor:start_link` to return.
%% This is a deadlock.
%%
%% Solution: call class module functions directly (bypassing the gen_server).
%% We use ETS for the class hierarchy walk (no gen_server needed for lookup).
-spec static_init(module(), atom()) -> {ok, {map(), [map()]}}.
static_init(Module, ClassName) ->
    ClassSelf = make_init_class_self(ClassName, Module),
    ClassVars = #{},
    Children = call_class_method_direct(ClassName, Module, class_children, ClassSelf, ClassVars),
    BtStrategy = call_class_method_direct(ClassName, Module, class_strategy, ClassSelf, ClassVars),
    MaxR = call_class_method_direct(ClassName, Module, class_maxRestarts, ClassSelf, ClassVars),
    MaxT = call_class_method_direct(ClassName, Module, class_restartWindow, ClassSelf, ClassVars),
    SupFlags = #{strategy => to_otp_strategy(BtStrategy), intensity => MaxR, period => MaxT},
    Specs = build_child_specs(Children),
    {ok, {SupFlags, Specs}}.

%% @doc Initialize a dynamic supervisor without calling through the class gen_server.
%%
%% Called from the generated `init/1` callback of DynamicSupervisor subclasses.
%% Same deadlock avoidance rationale as `static_init/2`.
-spec dynamic_init(module(), atom()) -> {ok, {map(), [map()]}}.
dynamic_init(Module, ClassName) ->
    ClassSelf = make_init_class_self(ClassName, Module),
    ClassVars = #{},
    ChildClass = call_class_method_direct(
        ClassName, Module, class_childClass, ClassSelf, ClassVars
    ),
    MaxR = call_class_method_direct(ClassName, Module, class_maxRestarts, ClassSelf, ClassVars),
    MaxT = call_class_method_direct(ClassName, Module, class_restartWindow, ClassSelf, ClassVars),
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxR, period => MaxT},
    Specs = build_child_specs([ChildClass]),
    {ok, {SupFlags, Specs}}.

%% @doc Return the running supervisor instance, or nil if not started.
%%
%% Called from `class current` on Supervisor and DynamicSupervisor subclasses.
%% Self is the class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
-spec current(tuple()) -> tuple() | nil.
current(Self) ->
    ClassPid = element(4, Self),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    Module = beamtalk_object_class:module_name(ClassPid),
    case whereis(Module) of
        undefined ->
            nil;
        Pid ->
            {beamtalk_supervisor, ClassName, Module, Pid}
    end.

%% @doc Return the child ids of currently-running children.
%%
%% Called from `children` on Supervisor instances.
%% Returns a list of child id atoms (class name atoms by default, or custom
%% ids when `withId:` was used in `SupervisionSpec`).
%% Dead or restarting children are excluded.
-spec whichChildren(tuple()) -> [atom()].
whichChildren(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(ClassName, children, fun() ->
        [
            Id
         || {Id, ChildPid, _, _} <- supervisor:which_children(Pid),
            Id =/= undefined,
            is_pid(ChildPid)
        ]
    end).

%% @doc Return the running child object for the given class, or nil.
%%
%% Called from `which: aClass` on Supervisor instances.
%% ClassArg is a class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
%% Matches by child module (position 4 in which_children tuples) rather than child id
%% so that custom ids set via `withId:` in SupervisionSpec still resolve correctly.
%% Returns {beamtalk_supervisor, ...} for supervisor subclasses, {beamtalk_object, ...} otherwise.
-spec whichChild(tuple(), tuple()) -> tuple() | nil.
whichChild(Self, ClassArg) ->
    SupPid = element(4, Self),
    ClassName = element(2, Self),
    ChildModule = element(3, ClassArg),
    ChildClassPid = element(4, ClassArg),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    with_live_supervisor(ClassName, 'which:', fun() ->
        Children = supervisor:which_children(SupPid),
        case
            lists:search(
                fun({_Id, CPid, _, Mods}) ->
                    is_pid(CPid) andalso lists:member(ChildModule, Mods)
                end,
                Children
            )
        of
            {value, {_Id, ChildPid, _, _}} ->
                wrap_child(ChildClass, ChildModule, ChildPid);
            false ->
                nil
        end
    end).

%% @doc Terminate a supervised child.
%%
%% For Supervisor (static): Arg is a class object — terminates child by its
%% class name, which is the OTP child id. Returns nil.
%%
%% For DynamicSupervisor (dynamic): Arg is an actor or supervisor instance —
%% terminates child by its process pid (simple_one_for_one semantics). Returns nil.
%%
%% Raises a runtime_error if the child is not found or cannot be terminated.
-spec terminateChild(tuple(), tuple()) -> nil.
terminateChild(Self, Arg) ->
    SupPid = element(4, Self),
    SupClass = element(2, Self),
    with_live_supervisor(SupClass, 'terminateChild:', fun() ->
        case beamtalk_class_registry:is_class_object(Arg) of
            true ->
                %% Supervisor case: terminate by class name (the default child id)
                ChildClassPid = element(4, Arg),
                ChildId = beamtalk_object_class:class_name(ChildClassPid),
                case supervisor:terminate_child(SupPid, ChildId) of
                    ok ->
                        nil;
                    {error, Reason} ->
                        Error = beamtalk_error:new(
                            runtime_error,
                            SupClass,
                            'terminateChild:',
                            iolist_to_binary(io_lib:format("~p", [Reason]))
                        ),
                        error(beamtalk_exception_handler:ensure_wrapped(Error))
                end;
            false ->
                %% DynamicSupervisor case: terminate by child process pid
                ChildPid = element(4, Arg),
                case supervisor:terminate_child(SupPid, ChildPid) of
                    ok ->
                        nil;
                    {error, not_found} ->
                        Error = beamtalk_error:new(
                            runtime_error,
                            SupClass,
                            'terminateChild:',
                            <<"Child not found — it may have already terminated">>
                        ),
                        error(beamtalk_exception_handler:ensure_wrapped(Error));
                    {error, Reason} ->
                        Error = beamtalk_error:new(
                            runtime_error,
                            SupClass,
                            'terminateChild:',
                            iolist_to_binary(io_lib:format("~p", [Reason]))
                        ),
                        error(beamtalk_exception_handler:ensure_wrapped(Error))
                end
        end
    end).

%% @doc Start a new child with default args under a DynamicSupervisor.
%%
%% Called from `startChild` on DynamicSupervisor instances.
%% Calls `Module:'childClass'()` to determine the child class and module,
%% then starts the child via OTP simple_one_for_one.
%% Returns {beamtalk_supervisor, ChildClass, ChildModule, ChildPid} for supervisor
%% subclasses, or {beamtalk_object, ChildClass, ChildModule, ChildPid} for workers.
-spec startChild(tuple()) -> tuple().
startChild(Self) ->
    SupPid = element(4, Self),
    SupMod = element(3, Self),
    SupClass = element(2, Self),
    ChildClassObj = SupMod:'childClass'(),
    ChildClassPid = element(4, ChildClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    ChildModule = element(3, ChildClassObj),
    with_live_supervisor(SupClass, startChild, fun() ->
        case supervisor:start_child(SupPid, []) of
            {ok, ChildPid} ->
                wrap_child(ChildClass, ChildModule, ChildPid);
            {error, Reason} ->
                Error = beamtalk_error:new(
                    runtime_error,
                    SupClass,
                    startChild,
                    iolist_to_binary(io_lib:format("~p", [Reason]))
                ),
                error(beamtalk_exception_handler:ensure_wrapped(Error))
        end
    end).

%% @doc Start a new child with args under a DynamicSupervisor.
%%
%% Called from `startChild: args` on DynamicSupervisor instances.
%% Args is passed as the extra argument to OTP simple_one_for_one,
%% which appends it to the child start function's argument list.
%% Returns {beamtalk_supervisor, ChildClass, ChildModule, ChildPid} for supervisor
%% subclasses, or {beamtalk_object, ChildClass, ChildModule, ChildPid} for workers.
-spec startChild(tuple(), term()) -> tuple().
startChild(Self, Args) ->
    SupPid = element(4, Self),
    SupMod = element(3, Self),
    SupClass = element(2, Self),
    ChildClassObj = SupMod:'childClass'(),
    ChildClassPid = element(4, ChildClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    ChildModule = element(3, ChildClassObj),
    with_live_supervisor(SupClass, 'startChild:', fun() ->
        case supervisor:start_child(SupPid, [Args]) of
            {ok, ChildPid} ->
                wrap_child(ChildClass, ChildModule, ChildPid);
            {error, Reason} ->
                Error = beamtalk_error:new(
                    runtime_error,
                    SupClass,
                    'startChild:',
                    iolist_to_binary(io_lib:format("~p", [Reason]))
                ),
                error(beamtalk_exception_handler:ensure_wrapped(Error))
        end
    end).

%% @doc Return the count of active children.
%%
%% Called from `count` on Supervisor and DynamicSupervisor instances.
%% Uses `supervisor:count_children/1` which returns a proplist with
%% `active` (running), `workers`, `supervisors`, `specs` counts.
-spec countChildren(tuple()) -> non_neg_integer().
countChildren(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(ClassName, count, fun() ->
        Counts = supervisor:count_children(Pid),
        proplists:get_value(active, Counts, 0)
    end).

%% @doc Stop the supervisor and all its children.
%%
%% Called from `stop` on Supervisor and DynamicSupervisor instances.
%% Uses gen_server:stop/1 since supervisors are OTP gen_servers.
%% Returns nil.
-spec stop(tuple()) -> nil.
stop(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(ClassName, stop, fun() ->
        gen_server:stop(Pid),
        nil
    end).

%% @doc Build OTP child specs from a list of class objects or SupervisionSpec values.
%%
%% Called from the generated `init/1` of Supervisor subclasses (Phase 3 codegen).
%% For each element:
%% - Class object: calls `supervisionSpec` on the class to get a SupervisionSpec,
%%   then calls `childSpec` on the spec to get the Beamtalk dict.
%% - SupervisionSpec map: calls `childSpec` directly.
%% Converts Beamtalk child spec dicts to OTP-compatible maps.
-spec build_child_specs([term()]) -> [map()].
build_child_specs(Children) ->
    [build_child_spec(C) || C <- Children].

%% @doc Check if a class name is a Supervisor or DynamicSupervisor subclass.
%%
%% Used by codegen (Phase 3) to determine routing at compile time and by
%% `SupervisionSpec childSpec` to determine child `type` and `shutdown`.
%% ClassName must be the base class name atom (e.g., 'WebApp', not 'WebApp class').
-spec is_supervisor(atom()) -> boolean().
is_supervisor(ClassName) ->
    beamtalk_class_registry:inherits_from(ClassName, 'Supervisor') orelse
        beamtalk_class_registry:inherits_from(ClassName, 'DynamicSupervisor').

%% @doc Register the OTP application root supervisor (BT-1191).
%%
%% Called from the generated `beamtalk_{appname}_app:start/2` callback after
%% the root supervisor has started. SupervisorTuple must be a
%% `{beamtalk_supervisor, ClassName, Module, Pid}` value as returned by
%% `startLink/1`. Creates the ETS table if it does not already exist.
-spec register_root(tuple()) -> ok.
register_root(SupervisorTuple) ->
    ensure_root_table(),
    ets:insert(?ROOT_SUPERVISOR_TABLE, {root, SupervisorTuple}),
    ok.

%% @doc Return the registered OTP application root supervisor, or `nil`.
%%
%% Called by `Workspace supervisor` via the workspace interface primitives.
%% Returns the `{beamtalk_supervisor, ClassName, Module, Pid}` tuple registered
%% by `register_root/1`, or the Beamtalk `nil` atom if no root supervisor has
%% been registered (e.g. no `[application]` section in `beamtalk.toml`).
-spec get_root() -> tuple() | nil.
get_root() ->
    ensure_root_table(),
    case ets:lookup(?ROOT_SUPERVISOR_TABLE, root) of
        [{root, Sup}] -> Sup;
        [] -> nil
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private
%% Build a ClassSelf tuple for use in direct class method calls during supervisor init.
%% The pid field is set to the class gen_server pid (may be blocked, but ClassSelf is
%% used only as a value object — pure class methods do not send messages to self).
-spec make_init_class_self(atom(), module()) -> tuple().
make_init_class_self(ClassName, Module) ->
    ClassPid = beamtalk_class_registry:whereis_class(ClassName),
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    {beamtalk_object, ClassTag, Module, ClassPid}.

%% @private
%% Call a class method directly by invoking the module function, bypassing the class
%% gen_server. Tries the subclass module first, then walks the class hierarchy via
%% ETS until the method is found in an ancestor's module.
-spec call_class_method_direct(atom(), module(), atom(), tuple(), map()) -> term().
call_class_method_direct(ClassName, Module, FunName, ClassSelf, ClassVars) ->
    case erlang:function_exported(Module, FunName, 2) of
        true ->
            erlang:apply(Module, FunName, [ClassSelf, ClassVars]);
        false ->
            call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ClassVars, 0)
    end.

-spec call_inherited_class_method_direct(atom(), atom(), tuple(), map(), non_neg_integer()) ->
    term().
call_inherited_class_method_direct(_ClassName, FunName, _ClassSelf, _ClassVars, Depth) when
    Depth > 30
->
    error({supervisor_init_method_not_found, FunName});
call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ClassVars, Depth) ->
    case beamtalk_class_hierarchy_table:lookup(ClassName) of
        not_found ->
            error({supervisor_init_method_not_found, FunName});
        {ok, SuperclassName} ->
            SuperPid = beamtalk_class_registry:whereis_class(SuperclassName),
            case SuperPid of
                undefined ->
                    error({supervisor_init_class_not_found, SuperclassName});
                _ ->
                    %% module_name/1 uses process dict for self-calls; gen_server:call
                    %% for ancestors (which are not blocked, so this is safe).
                    SuperModule = beamtalk_object_class:module_name(SuperPid),
                    case SuperModule of
                        undefined ->
                            %% Module not yet set (class still initialising) — skip upward.
                            call_inherited_class_method_direct(
                                SuperclassName, FunName, ClassSelf, ClassVars, Depth + 1
                            );
                        _ ->
                            case erlang:function_exported(SuperModule, FunName, 2) of
                                true ->
                                    erlang:apply(SuperModule, FunName, [ClassSelf, ClassVars]);
                                false ->
                                    call_inherited_class_method_direct(
                                        SuperclassName, FunName, ClassSelf, ClassVars, Depth + 1
                                    )
                            end
                    end
            end
    end.

%% @private
%% Build a single OTP child spec from a class object or SupervisionSpec map.
-spec build_child_spec(term()) -> map().
build_child_spec(ClassObj) when is_tuple(ClassObj), element(1, ClassObj) =:= beamtalk_object ->
    case beamtalk_class_registry:is_class_object(ClassObj) of
        true ->
            ChildClassPid = element(4, ClassObj),
            ChildClass = beamtalk_object_class:class_name(ChildClassPid),
            ChildModule = element(3, ClassObj),
            case is_supervisor(ChildClass) of
                true ->
                    %% Nested supervisor child — build OTP spec directly.
                    %% Supervisor subclasses don't have supervisionSpec (that is on Actor).
                    %% OTP requires supervisor children to use start_link/0.
                    #{
                        id => ChildClass,
                        start => {ChildModule, start_link, []},
                        restart => permanent,
                        shutdown => infinity,
                        type => supervisor,
                        modules => [ChildModule]
                    };
                false ->
                    %% Worker child: call supervisionSpec then childSpec.
                    BtSpec = beamtalk_object_class:class_send(ChildClassPid, 'supervisionSpec', []),
                    spec_to_otp(beamtalk_message_dispatch:send(BtSpec, 'childSpec', []))
            end;
        false ->
            %% Actor instance passed as spec — treat as SupervisionSpec-like value
            spec_to_otp(beamtalk_message_dispatch:send(ClassObj, 'childSpec', []))
    end;
build_child_spec(Spec) when is_map(Spec) ->
    %% SupervisionSpec value (tagged map): call childSpec directly
    spec_to_otp(beamtalk_message_dispatch:send(Spec, 'childSpec', [])).

%% @private
%% Translate Beamtalk strategy symbol to OTP supervisor strategy atom.
%%
%% OTP expects snake_case atoms (one_for_one), while Beamtalk uses camelCase
%% symbols (#oneForOne). Unknown strategies pass through unchanged so OTP
%% can report the error with context.
-spec to_otp_strategy(atom()) -> atom().
to_otp_strategy(oneForOne) -> one_for_one;
to_otp_strategy(oneForAll) -> one_for_all;
to_otp_strategy(restForOne) -> rest_for_one;
to_otp_strategy(S) -> S.

%% @private
%% Convert a Beamtalk child spec dict to an OTP-compatible child spec map.
%%
%% The Beamtalk dict from `SupervisionSpec childSpec` has keys:
%%   id, start ([ClassObj, FnAtom, ArgsList]), restart, shutdown, type
%% The `start` value is a Beamtalk Array [ClassObj, #spawn, #()] that must be
%% converted to the OTP MFA tuple {Module, Function, Args}.
%%
%% For nested supervisor children (Supervisor/DynamicSupervisor subclasses), the
%% Beamtalk IR uses #spawn as the start function, but OTP requires start_link/0
%% so the supervisor process is linked into the tree. This is translated here.
%%
%% For worker children, Beamtalk's spawn/0 returns {beamtalk_object,...} which
%% OTP supervisor does not accept (it expects {ok, Pid}). The generated
%% start_link/1 returns {ok, Pid} directly from gen_server:start_link, so
%% worker children use start_link/1 with an init-args map instead of spawn/0.
-spec spec_to_otp(map()) -> map().
spec_to_otp(BtSpec) ->
    %% `start` is a Beamtalk Array #[ClassObj, StartFn, StartArgs].
    %% Beamtalk Arrays are tagged maps: #{'$beamtalk_class' => 'Array', 'data' => ErlangArray}.
    %% Use array:get/2 (0-based) to access elements.
    StartBtArray = maps:get(start, BtSpec),
    StartErlArray = maps:get(data, StartBtArray),
    ClassObj = array:get(0, StartErlArray),
    ChildModule = element(3, ClassObj),
    ChildClassPid = element(4, ClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    OtpStart =
        case is_supervisor(ChildClass) of
            true ->
                %% Nested supervisor: OTP expects start_link/0 to link the child supervisor.
                {ChildModule, start_link, []};
            false ->
                %% Worker: use start_link/1 (returns {ok, Pid}) for OTP compatibility.
                %% spawn/0 wraps the pid in {beamtalk_object,...} which OTP rejects.
                StartFn = array:get(1, StartErlArray),
                InitArgs =
                    case StartFn of
                        spawn ->
                            %% No init args — start with empty state map.
                            [#{}];
                        'spawnWith:' ->
                            %% #(self args) uses list syntax (#(...)) so it compiles to an
                            %% Erlang list [ArgsMap] — use it directly as the start_link arg.
                            array:get(2, StartErlArray);
                        Other ->
                            %% Unknown start function — raise structured error rather than crashing.
                            %% Tag as 'SupervisionSpec'/'childSpec' to reflect where the spec
                            %% originated (SupervisionSpec>>childSpec), not the supervisor itself.
                            Error = beamtalk_error:new(
                                runtime_error,
                                'SupervisionSpec',
                                childSpec,
                                iolist_to_binary(
                                    io_lib:format(
                                        "unsupported child start function: ~p (expected spawn or spawnWith:)",
                                        [Other]
                                    )
                                )
                            ),
                            error(beamtalk_exception_handler:ensure_wrapped(Error))
                    end,
                {ChildModule, start_link, InitArgs}
        end,
    #{
        id => maps:get(id, BtSpec),
        start => OtpStart,
        restart => maps:get(restart, BtSpec),
        shutdown => maps:get(shutdown, BtSpec),
        type => maps:get(type, BtSpec),
        modules => [ChildModule]
    }.

%% @private
%% Wrap a child pid with the correct Beamtalk tuple tag.
%% Supervisor subclasses use {beamtalk_supervisor, ...} so follow-up sends
%% use the supervisor dispatch path rather than the actor/gen_server path.
-spec wrap_child(atom(), module(), pid()) -> tuple().
wrap_child(ChildClass, ChildModule, ChildPid) ->
    case is_supervisor(ChildClass) of
        true -> {beamtalk_supervisor, ChildClass, ChildModule, ChildPid};
        false -> {beamtalk_object, ChildClass, ChildModule, ChildPid}
    end.

%% @private
%% Execute Fun(), catching raw OTP process exits that indicate a stale
%% supervisor handle ({noproc, _} when the process is dead) and converting
%% them to a structured runtime_error instead of letting the raw exit leak
%% across the public API boundary.
%%
%% Both `supervisor:*` and `gen_server:stop` raise `exit:{noproc, MFA}` when
%% the target process is not alive.
-spec with_live_supervisor(atom(), atom(), fun(() -> term())) -> term().
with_live_supervisor(ClassName, Selector, Fun) ->
    try
        Fun()
    catch
        exit:{noproc, _} ->
            %% supervisor:* calls use gen_server:call internally, which exits
            %% with {noproc, MFA} when the target process is dead.
            Error = beamtalk_error:new(
                runtime_error,
                ClassName,
                Selector,
                <<"supervisor is not running — the handle is stale">>
            ),
            error(beamtalk_exception_handler:ensure_wrapped(Error));
        exit:noproc ->
            %% gen_server:stop/1 exits with the bare atom noproc (no MFA wrapper).
            Error = beamtalk_error:new(
                runtime_error,
                ClassName,
                Selector,
                <<"supervisor is not running — the handle is stale">>
            ),
            error(beamtalk_exception_handler:ensure_wrapped(Error))
    end.

%% @private
%% Ensure the root supervisor ETS table exists.
%% Uses `public` access so the generated app callback and workspace primitives
%% can both read/write without process ownership constraints.
-spec ensure_root_table() -> ok.
ensure_root_table() ->
    case ets:info(?ROOT_SUPERVISOR_TABLE, id) of
        undefined ->
            try
                ets:new(?ROOT_SUPERVISOR_TABLE, [named_table, public, set])
            catch
                error:badarg ->
                    %% Another process created the table concurrently — that's fine
                    ok
            end,
            ok;
        _ ->
            ok
    end.
