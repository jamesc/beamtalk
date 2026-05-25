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
-include_lib("kernel/include/logger.hrl").

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

BT-873: Compiled classes go through the compiled path (`Module:new/0,1`).

BT-2275 / ADR 0082: A class built purely programmatically via `ClassBuilder`
has a *module name* (defaulting to the class name) but **no loaded BEAM module**
behind it — compilation is "persist + optimise", never a prerequisite for using
a class. When the module is not loaded, build a generic, module-free instance: a
tagged map `#{'$beamtalk_class' => ClassName, Field => Default, ...}`, identical
in shape to the compiled value-type representation, so behaviour is unchanged
across `flush`+reload (no visible change on flush). The same path serves both
the external `X new` (gen_server `{new, _}`) and the `self new`
(`class_self_new`) cases — both call this function from inside the class
gen_server process, where the field-default cache lives in the process
dictionary.
""".
-spec handle_new(list(), class_name(), atom(), boolean() | undefined) ->
    {ok, term(), boolean() | undefined} | {error, term(), boolean() | undefined}.
handle_new(Args, ClassName, Module, IsConstructible0) ->
    case is_module_loaded_with_new(Module) of
        true ->
            handle_new_compiled(Args, ClassName, Module, IsConstructible0);
        false ->
            handle_new_generic(Args, ClassName)
    end.

-doc """
Returns true when `Module` is a loaded BEAM module exporting `new/0`.

This discriminates the compiled instantiation path from the generic
module-free path (BT-2275). `code:ensure_loaded/1` triggers lazy load so
`function_exported/3` sees a freshly-loaded module; a module-less builder class
(module name with no `.beam` behind it) fails to load and falls through to the
generic path.
""".
-spec is_module_loaded_with_new(atom()) -> boolean().
is_module_loaded_with_new(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            erlang:function_exported(Module, new, 0);
        _ ->
            false
    end;
is_module_loaded_with_new(_) ->
    false.

-doc """
Build a generic, module-free instance for a class with no loaded module.

The instance is a tagged map carrying `$beamtalk_class` plus every declared
field set to its default value (inherited fields included), with any `new:`
initialiser map merged on top. This is the same shape a compiled value type's
`new/0`/`new/1` produces, so a builder-built class behaves identically before
and after it is flushed to a compiled module (ADR 0006 unified dispatch,
ADR 0082 memory-first model).

`IsConstructible` is reported as `true`: a module-less builder class with no
abstract modifier is, by construction, instantiable — there is no `new/0` to
probe for the legacy non-constructible pattern.

Abstract classes are screened twice: the external `X new` path is rejected by
the gen_server `{new, _}` handler before reaching here, and the `self new` path
(`class_self_new`, which does not pre-screen) is caught here via the
`beamtalk_class_is_abstract` process-dictionary flag set in
`beamtalk_object_class:init/1`. Both produce the same `instantiation_error`.
""".
-spec handle_new_generic(list(), class_name()) ->
    {ok, map(), boolean()} | {error, term(), boolean()}.
handle_new_generic(Args, ClassName) ->
    case get(beamtalk_class_is_abstract) of
        true ->
            Selector =
                case Args of
                    [] -> 'new';
                    _ -> 'new:'
                end,
            {error, abstract_class_error(ClassName, Selector), false};
        _ ->
            Defaults = generic_field_defaults(ClassName),
            try
                Result =
                    case Args of
                        [] ->
                            build_generic_instance(ClassName, Defaults, #{});
                        [InitMap] when is_map(InitMap) ->
                            build_generic_instance(ClassName, Defaults, InitMap);
                        _ ->
                            Error0 = beamtalk_error:new(type_error, ClassName),
                            Error1 = beamtalk_error:with_selector(Error0, 'new:'),
                            Error2 = beamtalk_error:with_hint(
                                Error1, <<"new: expects a Dictionary argument">>
                            ),
                            beamtalk_error:raise(Error2)
                    end,
                {ok, Result, true}
            catch
                error:Error ->
                    {error, Error, true}
            end
    end.

-doc """
Construct the generic instance map: class tag + defaults + new: overrides.

Only keys already declared as fields (present in `Defaults`) are accepted from
the `new:` initialiser; unknown keys are silently ignored, matching the
compiled `new/1` which merges over a fixed default map and never introduces new
slots. The `$beamtalk_class` tag always wins so it cannot be overwritten by a
stray initialiser key.
""".
-spec build_generic_instance(class_name(), map(), map()) -> map().
build_generic_instance(ClassName, Defaults, InitMap) ->
    Filtered = maps:with(maps:keys(Defaults), InitMap),
    Merged = maps:merge(Defaults, Filtered),
    Merged#{beamtalk_tagged_map:class_key() => ClassName}.

-doc """
Collect field defaults for a module-less class, including inherited fields.

The current class's own defaults come from the process dictionary cache set in
`beamtalk_object_class:init/1` (`handle_new` always runs inside the class
gen_server process, so this read is local and deadlock-free). Inherited fields
are gathered by walking the superclass chain via the registry: a compiled
ancestor with `new/0` contributes its instance map's user fields; a module-less
ancestor contributes its own cached field defaults. Own fields shadow inherited
ones, mirroring the compiled value-type `maps:merge(ParentState, Own)` order.
""".
-spec generic_field_defaults(class_name()) -> map().
generic_field_defaults(ClassName) ->
    Own =
        case get(beamtalk_class_field_defaults) of
            M when is_map(M) -> M;
            _ -> #{}
        end,
    Inherited = inherited_field_defaults(ClassName),
    maps:merge(Inherited, Own).

-doc "Gather inherited field defaults by walking the superclass chain.".
-spec inherited_field_defaults(class_name()) -> map().
inherited_field_defaults(ClassName) ->
    SuperName =
        case beamtalk_class_metadata:lookup_superclass(ClassName) of
            {ok, S} -> S;
            _ -> none
        end,
    collect_ancestor_defaults(SuperName, #{}, 0).

-spec collect_ancestor_defaults(class_name() | none, map(), non_neg_integer()) -> map().
collect_ancestor_defaults(none, Acc, _Depth) ->
    Acc;
collect_ancestor_defaults(_Name, Acc, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    Acc;
collect_ancestor_defaults(Name, Acc, Depth) ->
    Module =
        case beamtalk_class_metadata:lookup_module(Name) of
            {ok, M} -> M;
            _ -> Name
        end,
    AncestorDefaults =
        case is_module_loaded_with_new(Module) of
            true ->
                ancestor_compiled_defaults(Module);
            false ->
                ancestor_builder_defaults(Name)
        end,
    %% Closer ancestors shadow farther ones: keep what we already have (Acc),
    %% only filling in fields the ancestor introduces.
    NextAcc = maps:merge(AncestorDefaults, Acc),
    Super =
        case beamtalk_class_metadata:lookup_superclass(Name) of
            {ok, S} -> S;
            _ -> none
        end,
    collect_ancestor_defaults(Super, NextAcc, Depth + 1).

-doc "Read a compiled ancestor's default field values from its `new/0` map.".
-spec ancestor_compiled_defaults(atom()) -> map().
ancestor_compiled_defaults(Module) ->
    try erlang:apply(Module, new, []) of
        Instance when is_map(Instance) ->
            maps:without(beamtalk_tagged_map:internal_fields(), Instance);
        _ ->
            #{}
    catch
        Kind:Reason:ST ->
            ?LOG_DEBUG(
                "ancestor ~p:new/0 failed while collecting field defaults: ~p:~p",
                [Module, Kind, Reason],
                #{stacktrace => ST, domain => [beamtalk, runtime]}
            ),
            #{}
    end.

-doc "Read a module-less ancestor's cached field defaults via the registry.".
-spec ancestor_builder_defaults(class_name()) -> map().
ancestor_builder_defaults(Name) ->
    case beamtalk_class_registry:whereis_class(Name) of
        Pid when is_pid(Pid), Pid =/= self() ->
            try
                gen_server:call(Pid, field_defaults, 5000)
            catch
                exit:{timeout, _} -> #{};
                exit:{noproc, _} -> #{}
            end;
        _ ->
            #{}
    end.

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
