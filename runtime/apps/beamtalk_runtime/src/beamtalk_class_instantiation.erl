%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_instantiation).

%%% **DDD Context:** Object System Context

-moduledoc """
Instance creation logic for Beamtalk classes.

Handles the spawn and new protocols for creating class instances.
Contains the logic for compiled class instantiation, abstract
class validation, and constructibility checks.

Extracted from `beamtalk_object_class` (BT-576) for single-responsibility.
Called by `beamtalk_object_class` gen_server handle_call clauses.

## Responsibilities

- Actor spawn protocol (spawn/spawnWith:)
- Value type instantiation protocol (new/new:)
- Constructibility checking (is_constructible)
- Abstract class error construction
""".

-include("beamtalk.hrl").

-export([
    handle_spawn/4,
    handle_new/4,
    class_self_new/3,
    class_self_spawn/3,
    class_self_spawn/4,
    class_self_spawn_as/4,
    class_self_spawn_with/5,
    ensure_is_constructible/3,
    compute_is_constructible/2,
    abstract_class_error/2
]).

-type class_name() :: atom().

%%====================================================================
%% Spawn Protocol
%%====================================================================

-doc """
Handle the spawn protocol for actor creation.

BT-246: Actor spawn via dynamic class dispatch.
Routes spawn/spawnWith: through class_send → {spawn, Args} protocol.
Returns gen_server reply tuple.
""".
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
            #beamtalk_object{} = Obj ->
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

-doc """
Handle the new protocol for value type instantiation.

BT-246 / ADR 0013: Value types support new/new:. Actor classes should use
{spawn, Args} protocol via class_send.

BT-873: Dynamic class path (BT-838) removed. All classes go through
the compiled path.
""".
-spec handle_new(list(), class_name(), atom(), boolean() | undefined) ->
    {ok, term(), boolean() | undefined} | {error, term(), boolean() | undefined}.
handle_new(Args, ClassName, Module, IsConstructible0) ->
    handle_new_compiled(Args, ClassName, Module, IsConstructible0).

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

-doc """
Create a new instance from within a class method (bypasses gen_server).

BT-893: When a class method calls `self new` or `self new:`, the codegen
routes here instead of through `module:new()` → `gen_server:call()` which
would deadlock since the class method is already executing inside handle_call.
""".
-spec class_self_new(class_name(), atom(), list()) -> term().
class_self_new(ClassName, Module, Args) ->
    case handle_new(Args, ClassName, Module, undefined) of
        {ok, Result, _IsConstructible} ->
            Result;
        {error, Error, _IsConstructible} ->
            Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
            error(Wrapped)
    end.

-doc """
Spawn an actor from within a class method (bypasses gen_server).

BT-893: When a class method calls `self spawn` or `self spawnWith:`, the
codegen routes here instead of through `module:spawn()` → `gen_server:call()`
which would deadlock.
""".
-spec class_self_spawn(class_name(), atom(), list()) -> term().
class_self_spawn(ClassName, Module, Args) ->
    class_self_spawn(ClassName, Module, false, Args).

-doc """
Spawn with explicit abstract class flag (used by runtime self-instantiation).

BT-908: The IsAbstract parameter may come from erlang:get(beamtalk_class_is_abstract)
in codegen-generated class methods. Normalize to boolean defensively — undefined or
any non-true value is treated as false (non-abstract).
""".
-spec class_self_spawn(class_name(), atom(), boolean() | term(), list()) -> term().
class_self_spawn(ClassName, Module, IsAbstract0, Args) ->
    IsAbstract = IsAbstract0 =:= true,
    case handle_spawn(Args, ClassName, Module, IsAbstract) of
        {ok, Result} ->
            Result;
        {error, Error} ->
            Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
            error(Wrapped)
    end.

-doc """
Spawn a named actor from within a class method (bypasses gen_server).

BT-2004: When a class method calls `self spawnAs: name`, the codegen routes
here so that ClassName/Module come from the process dictionary rather than a
gen_server:call on the current class — which would deadlock.

Returns a Beamtalk `Result` directly (matching `Actor>>spawnAs:` which has
return type `Result(Self, Error)`) so `unwrap` / `onError:` chaining works
the same as for external `ClassName spawnAs:` calls.
""".
-spec class_self_spawn_as(class_name(), atom(), boolean() | term(), term()) ->
    beamtalk_result:t().
class_self_spawn_as(ClassName, Module, IsAbstract0, Name) ->
    do_class_self_named_spawn(ClassName, Module, IsAbstract0, #{}, Name, 'spawnAs:').

-doc """
Spawn a named actor with init args from within a class method.

BT-2004: `self spawnWith: InitArgs as: Name` equivalent. See
`class_self_spawn_as/4` for the deadlock-avoidance rationale.
""".
-spec class_self_spawn_with(class_name(), atom(), boolean() | term(), term(), term()) ->
    beamtalk_result:t().
class_self_spawn_with(ClassName, Module, IsAbstract0, InitArgs, Name) ->
    do_class_self_named_spawn(ClassName, Module, IsAbstract0, InitArgs, Name, 'spawnWith:as:').

-spec do_class_self_named_spawn(
    class_name(), atom(), boolean() | term(), term(), term(), atom()
) -> beamtalk_result:t().
do_class_self_named_spawn(ClassName, Module, IsAbstract0, InitArgs, Name, Selector) ->
    IsAbstract = IsAbstract0 =:= true,
    case IsAbstract of
        true ->
            beamtalk_result:from_tagged_tuple(
                {error, abstract_class_error(ClassName, Selector)}
            );
        false ->
            case beamtalk_actor:'spawnAs'(Name, Module, InitArgs) of
                {ok, Pid} ->
                    beamtalk_result:from_tagged_tuple(
                        {ok, #beamtalk_object{
                            class = ClassName,
                            class_mod = Module,
                            pid = Pid
                        }}
                    );
                {error, #beamtalk_error{} = Err} ->
                    beamtalk_result:from_tagged_tuple(
                        {error, Err#beamtalk_error{
                            class = ClassName,
                            selector = Selector
                        }}
                    );
                {error, Reason} ->
                    beamtalk_result:from_tagged_tuple(
                        {error, generic_spawn_error(ClassName, Selector, Reason)}
                    )
            end
    end.

-spec generic_spawn_error(atom(), atom(), term()) -> #beamtalk_error{}.
generic_spawn_error(ClassName, Selector, Reason) ->
    beamtalk_error:with_hint(
        beamtalk_error:with_selector(
            beamtalk_error:new(instantiation_error, ClassName),
            Selector
        ),
        iolist_to_binary(io_lib:format("spawn failed: ~tp", [Reason]))
    ).

%%====================================================================
%% Constructibility
%%====================================================================

-doc """
Ensure is_constructible is computed and cached (BT-474).

Lazily computes whether a class can be instantiated via new/new: on first
access. Cannot be computed during init because the module isn't fully
available during on_load.
""".
-spec ensure_is_constructible(boolean() | undefined, atom(), boolean()) -> boolean().
ensure_is_constructible(C, _Module, _IsAbstract) when C =/= undefined ->
    C;
ensure_is_constructible(undefined, Module, IsAbstract) ->
    compute_is_constructible(Module, IsAbstract).

-doc """
Compute whether a class is constructible via new/new:.

BT-877: The compiler now infers is_constructible at compile time by detecting
the `new => self error: "..."` pattern, and ClassBuilder inherits the flag
from superclasses. This function is only called for bootstrap/legacy classes
that weren't registered with an explicit is_constructible flag.

Returns false for:
- Abstract classes (cannot be instantiated at all)
- Actors (have spawn/0 — must use spawn/spawnWith:)
- Classes without new/0
- Classes whose new/0 raises an error (e.g., primitive types)

Returns true for:
- Classes that export new/0 but not spawn/0 AND new/0 succeeds (value types)
""".
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

-doc "Build a structured instantiation_error for abstract classes.".
-spec abstract_class_error(atom(), atom()) -> #beamtalk_error{}.
abstract_class_error(ClassName, Selector) ->
    Error0 = beamtalk_error:new(instantiation_error, ClassName),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:with_hint(
        Error1, <<"Abstract classes cannot be instantiated. Subclass it first.">>
    ).
