%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Instance creation logic for Beamtalk classes.
%%%
%%% **DDD Context:** Runtime — Class System
%%%
%%% Handles the spawn and new protocols for creating class instances.
%%% Contains the logic for compiled class instantiation, abstract
%%% class validation, and constructibility checks.
%%%
%%% Extracted from `beamtalk_object_class` (BT-576) for single-responsibility.
%%% Called by `beamtalk_object_class` gen_server handle_call clauses.
%%%
%%% ## Responsibilities
%%%
%%% - Actor spawn protocol (spawn/spawnWith:)
%%% - Value type instantiation protocol (new/new:)
%%% - Constructibility checking (is_constructible)
%%% - Abstract class error construction
-module(beamtalk_class_instantiation).

-include("beamtalk.hrl").

-export([
    handle_spawn/4,
    handle_new/4,
    class_self_new/3,
    class_self_spawn/3,
    ensure_is_constructible/3,
    compute_is_constructible/2,
    abstract_class_error/2
]).

-type class_name() :: atom().

%%====================================================================
%% Spawn Protocol
%%====================================================================

%% @doc Handle the spawn protocol for actor creation.
%%
%% BT-246: Actor spawn via dynamic class dispatch.
%% Routes spawn/spawnWith: through class_send → {spawn, Args} protocol.
%% Returns gen_server reply tuple.
-spec handle_spawn(list(), class_name(), atom(), boolean()) ->
    {ok, #beamtalk_object{}} | {error, term()}.
handle_spawn(Args, ClassName, _Module, true) ->
    Selector =
        case Args of
            [] -> spawn;
            _ -> 'spawnWith:'
        end,
    {error, abstract_class_error(ClassName, Selector)};
handle_spawn(Args, ClassName, Module, false) ->
    try
        SpawnResult =
            case Args of
                [] ->
                    erlang:apply(Module, spawn, []);
                [InitArgs] ->
                    erlang:apply(Module, spawn, [InitArgs]);
                _ ->
                    Error0 = beamtalk_error:new(type_error, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, 'spawnWith:'),
                    Error2 = beamtalk_error:with_hint(
                        Error1, <<"spawnWith: expects a Dictionary argument">>
                    ),
                    beamtalk_error:raise(Error2)
            end,
        case SpawnResult of
            {beamtalk_object, _, _, _} = Obj ->
                {ok, Obj};
            {ok, Pid} ->
                Obj = #beamtalk_object{
                    class = ClassName,
                    class_mod = Module,
                    pid = Pid
                },
                {ok, Obj};
            SpawnError ->
                SpawnError
        end
    catch
        error:CaughtError ->
            {error, CaughtError}
    end.

%%====================================================================
%% New Protocol
%%====================================================================

%% @doc Handle the new protocol for value type instantiation.
%%
%% BT-246 / ADR 0013: Value types support new/new:. Actor classes should use
%% {spawn, Args} protocol via class_send.
%%
%% BT-873: Dynamic class path (BT-838) removed. All classes go through
%% the compiled path.
-spec handle_new(list(), class_name(), atom(), boolean() | undefined) ->
    {ok, term(), boolean() | undefined} | {error, term(), boolean() | undefined}.
handle_new(Args, ClassName, Module, IsConstructible0) ->
    handle_new_compiled(Args, ClassName, Module, IsConstructible0).

%% @private
handle_new_compiled(Args, ClassName, Module, IsConstructible0) ->
    IsConstructible =
        case IsConstructible0 of
            undefined -> compute_is_constructible(Module, false);
            C -> C
        end,
    try
        Result =
            case Args of
                [] ->
                    erlang:apply(Module, new, []);
                [InitMap] when is_map(InitMap) ->
                    case erlang:function_exported(Module, new, 1) of
                        true ->
                            erlang:apply(Module, new, [InitMap]);
                        false ->
                            erlang:apply(Module, new, [])
                    end;
                _ ->
                    case IsConstructible of
                        false ->
                            erlang:apply(Module, new, []);
                        true ->
                            Error0 = beamtalk_error:new(type_error, ClassName),
                            Error1 = beamtalk_error:with_selector(Error0, 'new:'),
                            Error2 = beamtalk_error:with_hint(
                                Error1, <<"new: expects a Dictionary argument">>
                            ),
                            beamtalk_error:raise(Error2)
                    end
            end,
        {ok, Result, IsConstructible}
    catch
        error:Error ->
            {error, Error, IsConstructible}
    end.

%%====================================================================
%% Class Method Self-Send (BT-893)
%%====================================================================

%% @doc Create a new instance from within a class method (bypasses gen_server).
%%
%% BT-893: When a class method calls `self new` or `self new:`, the codegen
%% routes here instead of through `module:new()` → `gen_server:call()` which
%% would deadlock since the class method is already executing inside handle_call.
-spec class_self_new(class_name(), atom(), list()) -> term().
class_self_new(ClassName, Module, Args) ->
    case handle_new(Args, ClassName, Module, undefined) of
        {ok, Result, _IsConstructible} ->
            Result;
        {error, Error, _IsConstructible} ->
            Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
            error(Wrapped)
    end.

%% @doc Spawn an actor from within a class method (bypasses gen_server).
%%
%% BT-893: When a class method calls `self spawn` or `self spawnWith:`, the
%% codegen routes here instead of through `module:spawn()` → `gen_server:call()`
%% which would deadlock.
-spec class_self_spawn(class_name(), atom(), list()) -> term().
class_self_spawn(ClassName, Module, Args) ->
    case handle_spawn(Args, ClassName, Module, false) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
            error(Wrapped)
    end.

%%====================================================================
%% Constructibility
%%====================================================================

%% @doc Ensure is_constructible is computed and cached (BT-474).
%%
%% Lazily computes whether a class can be instantiated via new/new: on first
%% access. Cannot be computed during init because the module isn't fully
%% available during on_load.
-spec ensure_is_constructible(boolean() | undefined, atom(), boolean()) -> boolean().
ensure_is_constructible(C, _Module, _IsAbstract) when C =/= undefined ->
    C;
ensure_is_constructible(undefined, Module, IsAbstract) ->
    compute_is_constructible(Module, IsAbstract).

%% @doc Compute whether a class is constructible via new/new:.
%%
%% Returns false for:
%% - Abstract classes (cannot be instantiated at all)
%% - Actors (have spawn/0 — must use spawn/spawnWith:)
%% - Non-instantiable primitives (Integer, String, etc. — new/0 raises)
-spec compute_is_constructible(atom(), boolean()) -> boolean().
compute_is_constructible(_Module, true) ->
    false;
compute_is_constructible(Module, false) ->
    case erlang:function_exported(Module, spawn, 0) of
        true ->
            false;
        false ->
            case erlang:function_exported(Module, new, 0) of
                false ->
                    false;
                true ->
                    try erlang:apply(Module, new, []) of
                        _ -> true
                    catch
                        error:_ -> false
                    end
            end
    end.

%%====================================================================
%% Error Construction
%%====================================================================

%% @doc Build a structured instantiation_error for abstract classes.
-spec abstract_class_error(atom(), atom()) -> #beamtalk_error{}.
abstract_class_error(ClassName, Selector) ->
    Error0 = beamtalk_error:new(instantiation_error, ClassName),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:with_hint(
        Error1, <<"Abstract classes cannot be instantiated. Subclass it first.">>
    ).
