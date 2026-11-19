%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_pool).

%%% **DDD Context:** Runtime Context

-moduledoc """
Pool-child helpers: reading a DynamicSupervisor's child class and capacity
configuration and the slow path that resolves a pool child from its children.

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    stale_handle_error/2,
    with_pool_child/3,
    with_pool_child_class/3
]).

-doc """
Run `Fun(ChildClass, ChildModule)` for a live DynamicSupervisor, with its
child class resolved by `pool_child_class/1`. A dead supervisor is
`stale_handle`; an unresolvable child class is returned as its error.
""".
-spec with_pool_child_class(tuple(), atom(), fun((atom(), module()) -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_pool_child_class(Self, Selector, Fun) ->
    beamtalk_supervisor:with_live_supervisor(Self, Selector, fun() ->
        case pool_child_class(Self) of
            {ok, {ChildClass, ChildModule}} -> Fun(ChildClass, ChildModule);
            {error, #beamtalk_error{} = Error} -> {error, Error#beamtalk_error{selector = Selector}}
        end
    end).

-doc """
Like `with_pool_child_class/3` for the start path: `Fun(ChildClass, ChildModule,
Kind, Max)`, where `Kind` is the cached spawn kind (or `unknown`) and `Max` the
`maxChildren` limit. For a local supervisor both dictionary entries come from a
single `process_info/2` (BT-4502); a remote or self supervisor falls back to
`pool_child_class/1` and `max_children/1`.
""".
-spec with_pool_child(tuple(), atom(), fun((atom(), module(), atom(), term()) -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_pool_child({beamtalk_supervisor, _SupClass, _Module, SupPid} = Self, Selector, Fun) ->
    beamtalk_supervisor:with_live_supervisor(Self, Selector, fun() ->
        case pool_child_config(Self) of
            {ok, {ChildClass, ChildModule, Kind}, Max} ->
                Fun(ChildClass, ChildModule, Kind, Max);
            {slow, _} ->
                slow_pool_child(Self, SupPid, Selector, Fun)
        end
    end).

-spec slow_pool_child(tuple(), pid(), atom(), fun((atom(), module(), atom(), term()) -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().

slow_pool_child(Self, SupPid, Selector, Fun) ->
    case pool_child_class(Self) of
        {ok, {ChildClass, ChildModule}} ->
            Fun(
                ChildClass, ChildModule, unknown, beamtalk_supervisor_capacity:max_children(SupPid)
            );
        {error, #beamtalk_error{} = Error} ->
            {error, Error#beamtalk_error{selector = Selector}}
    end.

-spec pool_child_config(tuple()) ->
    {ok, {atom(), module(), atom()}, non_neg_integer() | infinity} | {slow, term()}.

pool_child_config({beamtalk_supervisor, _SupClass, _Module, SupPid}) when
    is_pid(SupPid), SupPid =/= self(), node(SupPid) =:= node()
->
    case
        erlang:process_info(SupPid, [
            {dictionary, ?CHILD_CLASS_KEY}, {dictionary, ?MAX_CHILDREN_KEY}
        ])
    of
        [
            {{dictionary, ?CHILD_CLASS_KEY}, {ChildClass, ChildModule, Kind}},
            {{dictionary, ?MAX_CHILDREN_KEY}, Max}
        ] when is_atom(ChildClass), is_atom(ChildModule), is_atom(Kind) ->
            {ok, {ChildClass, ChildModule, Kind}, normalise_max(Max)};
        [
            {{dictionary, ?CHILD_CLASS_KEY}, {ChildClass, ChildModule}},
            {{dictionary, ?MAX_CHILDREN_KEY}, Max}
        ] when is_atom(ChildClass), is_atom(ChildModule) ->
            {ok, {ChildClass, ChildModule, unknown}, normalise_max(Max)};
        Other ->
            {slow, Other}
    end;
pool_child_config(_Self) ->
    {slow, remote_or_self}.

-spec normalise_max(term()) -> non_neg_integer() | infinity.

normalise_max(Max) when is_integer(Max) -> Max;
normalise_max(_) -> infinity.

-doc """
Resolve a DynamicSupervisor's child class as `{ChildClassName, ChildModule}`
(ADR 0135 §2).

`dynamic_init/3` stores the value the pool instance's `childClass` returned
in the pool's process dictionary (`?CHILD_CLASS_KEY`); it is read from there,
locally or via `erpc` for a remote pool. A pool without the entry (started
before ADR 0135) falls back to the module of a running child, from
`supervisor:which_children/1`; with no running child the result is a
structured `child_class_unknown` error. The class-side `childClass` is never
consulted: configuration is per instance.
""".
-spec pool_child_class(tuple()) -> {ok, {atom(), module()}} | {error, #beamtalk_error{}}.
pool_child_class({beamtalk_supervisor, SupClass, _Module, SupPid} = Self) ->
    case beamtalk_supervisor:sup_dictionary_value(SupPid, ?CHILD_CLASS_KEY) of
        {ok, {ChildClass, ChildModule}} ->
            {ok, {ChildClass, ChildModule}};
        {ok, {ChildClass, ChildModule, _Kind}} ->
            {ok, {ChildClass, ChildModule}};
        undefined ->
            child_class_from_children(SupClass, SupPid);
        {ok, _Malformed} ->
            child_class_from_children(SupClass, SupPid);
        Unavailable ->
            beamtalk_supervisor_errors:supervisor_unavailable_error(Self, childClass, Unavailable)
    end.

-spec child_class_from_children(atom(), pid()) ->
    {ok, {atom(), module()}} | {error, #beamtalk_error{}}.

child_class_from_children(SupClass, SupPid) ->
    Modules = [
        M
     || {_Id, Pid, _Type, [M | _]} <- supervisor:which_children(SupPid),
        is_pid(Pid),
        is_atom(M)
    ],
    %% One read of the memoised map for all children, not one row read each.
    ModClasses = beamtalk_class_registry:module_to_class_map(),
    Resolved = [
        {ClassName, M}
     || M <- Modules,
        {ok, ClassName} <- [maps:find(M, ModClasses)]
    ],
    case Resolved of
        [Entry | _] ->
            {ok, Entry};
        [] ->
            {error,
                beamtalk_error:new(
                    child_class_unknown,
                    SupClass,
                    childClass,
                    <<
                        "cannot determine this pool's child class: it was started without one "
                        "recorded (before ADR 0135) and has no running child; restart the pool"
                    >>
                )}
    end.

-doc """
Build the `{error, #beamtalk_error{kind = stale_handle}}` return for
`with_live_supervisor/3`. Logs a warning and constructs the Beamtalk
error with class and selector context.
""".
-spec stale_handle_error(atom(), atom()) -> {error, #beamtalk_error{}}.
stale_handle_error(ClassName, Selector) ->
    ?LOG_WARNING("Supervisor stale handle", #{
        supervisor => ClassName,
        selector => Selector,
        domain => [beamtalk, runtime]
    }),
    Error = beamtalk_error:stale_handle(
        ClassName, Selector, <<"supervisor is not running; the handle is stale">>
    ),
    {error, Error}.
