%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Instance creation logic for Beamtalk classes.
%%%
%%% **DDD Context:** Runtime — Class System
%%%
%%% Handles the spawn and new protocols for creating class instances.
%%% Contains the logic for both compiled and dynamic classes, abstract
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
%%% - Dynamic class instantiation
%%% - Method closure validation for dynamic classes
%%% - Abstract class error construction
-module(beamtalk_class_instantiation).

-include("beamtalk.hrl").

-export([
    handle_spawn/4,
    handle_new/8,
    ensure_is_constructible/3,
    compute_is_constructible/2,
    abstract_class_error/2,
    convert_methods_to_info/1,
    create_subclass/3
]).

-type class_name() :: atom().
-type selector() :: atom().

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

%% @doc Handle the new protocol for value type and dynamic class instantiation.
%%
%% BT-246 / ADR 0013: Value types support new/new:. Actor classes should use
%% {spawn, Args} protocol via class_send. Dynamic classes use
%% beamtalk_dynamic_object.
%%
%% BT-838: Added FieldDefaults parameter for dynamic class field defaults.
-spec handle_new(
    list(),
    class_name(),
    atom(),
    map(),
    [atom()],
    map(),
    boolean() | undefined,
    pid()
) ->
    {ok, term(), boolean() | undefined} | {error, term(), boolean() | undefined}.
handle_new(
    Args,
    ClassName,
    beamtalk_dynamic_object,
    DynamicMethods,
    InstanceVars,
    FieldDefaults,
    IsConstructible,
    ClassPid
) ->
    handle_new_dynamic(
        Args,
        ClassName,
        DynamicMethods,
        InstanceVars,
        FieldDefaults,
        IsConstructible,
        ClassPid
    );
handle_new(
    Args,
    ClassName,
    Module,
    _DynamicMethods,
    _InstanceVars,
    _FieldDefaults,
    IsConstructible0,
    _ClassPid
) ->
    handle_new_compiled(Args, ClassName, Module, IsConstructible0).

%% @private
%% BT-838: Added FieldDefaults parameter for field default values.
handle_new_dynamic(
    Args,
    ClassName,
    DynamicMethods,
    InstanceVars,
    FieldDefaults,
    IsConstructible,
    ClassPid
) ->
    try
        InitState = #{
            '$beamtalk_class' => ClassName,
            '__class_pid__' => ClassPid,
            '__methods__' => DynamicMethods
        },
        %% BT-838: Apply field defaults first, then overlay user-provided fields.
        %% This ensures partial initialization (new: #{x => 5}) still gets
        %% defaults for unspecified fields.
        InitStateWithDefaults = lists:foldl(
            fun(Var, Acc) ->
                Default = maps:get(Var, FieldDefaults, nil),
                maps:put(Var, Default, Acc)
            end,
            InitState,
            InstanceVars
        ),
        InitStateWithFields =
            case Args of
                [FieldMap] when is_map(FieldMap) ->
                    maps:merge(InitStateWithDefaults, FieldMap);
                [] ->
                    InitStateWithDefaults;
                _ ->
                    DynErr0 = beamtalk_error:new(type_error, ClassName),
                    DynErr1 = beamtalk_error:with_selector(DynErr0, 'new:'),
                    DynErr2 = beamtalk_error:with_hint(
                        DynErr1, <<"new: expects a Dictionary argument">>
                    ),
                    error(DynErr2)
            end,
        case beamtalk_dynamic_object:start_link(ClassName, InitStateWithFields) of
            {ok, Pid} ->
                Obj = #beamtalk_object{
                    class = ClassName,
                    class_mod = beamtalk_dynamic_object,
                    pid = Pid
                },
                {ok, Obj, IsConstructible};
            DynError ->
                {error, DynError, IsConstructible}
        end
    catch
        error:DynCaughtError ->
            {error, DynCaughtError, IsConstructible}
    end.

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
compute_is_constructible(beamtalk_dynamic_object, false) ->
    true;
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

%%====================================================================
%% Dynamic Method Validation
%%====================================================================

%% @doc Convert dynamic method closures to method_info maps.
%% Validates that dynamic methods have correct arity (3: Self, Args, State).
-spec convert_methods_to_info(#{selector() => fun()}) -> #{selector() => map()}.
convert_methods_to_info(Methods) ->
    maps:map(
        fun(Selector, Fun) ->
            {arity, Arity} = erlang:fun_info(Fun, arity),
            case Arity of
                3 ->
                    ok;
                _ ->
                    Error0 = beamtalk_error:new(arity_mismatch, 'DynamicClass'),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_hint(
                        Error1, <<"Dynamic methods must be arity 3 (Self, Args, State)">>
                    ),
                    Error3 = beamtalk_error:with_details(Error2, #{
                        actual_arity => Arity, expected_arity => 3
                    }),
                    beamtalk_error:raise(Error3)
            end,
            #{
                arity => Arity,
                block => Fun
            }
        end,
        Methods
    ).

%%====================================================================
%% Dynamic Subclass Creation
%%====================================================================

%% @doc Create a dynamic subclass at runtime.
%%
%% Phase 1 implementation using interpreter-based dynamic classes.
%% Methods are stored as closures and dispatch via apply/2.
%%
%% BT-704: Extracted from beamtalk_object_class.
-spec create_subclass(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
create_subclass(SuperclassName, ClassName, ClassSpec) ->
    case beamtalk_class_registry:whereis_class(SuperclassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, SuperclassName),
            Error = beamtalk_error:with_hint(
                Error0, <<"Superclass must be registered before creating subclass">>
            ),
            {error, Error};
        _SuperclassPid ->
            InstanceVars = maps:get(fields, ClassSpec, []),
            InstanceMethods = maps:get(instance_methods, ClassSpec, #{}),
            try convert_methods_to_info(InstanceMethods) of
                MethodInfo ->
                    ClassInfo = #{
                        name => ClassName,
                        module => beamtalk_dynamic_object,
                        superclass => SuperclassName,
                        instance_methods => MethodInfo,
                        fields => InstanceVars,
                        class_methods => #{},
                        dynamic_methods => InstanceMethods
                    },
                    case beamtalk_object_class:start_link(ClassName, ClassInfo) of
                        {ok, ClassPid} ->
                            {ok, ClassPid};
                        {error, {already_started, _Pid}} ->
                            Error0 = beamtalk_error:new(class_already_exists, ClassName),
                            {error, Error0};
                        {error, Reason} ->
                            case Reason of
                                #beamtalk_error{} ->
                                    {error, Reason};
                                _ ->
                                    Err0 = beamtalk_error:new(class_creation_failed, ClassName),
                                    Err1 = beamtalk_error:with_hint(
                                        Err0, <<"Failed to start subclass">>
                                    ),
                                    {error, beamtalk_error:with_details(Err1, #{reason => Reason})}
                            end
                    end
            catch
                error:ErrorReason ->
                    case ErrorReason of
                        #beamtalk_error{} ->
                            {error, ErrorReason};
                        _ ->
                            Err0 = beamtalk_error:new(class_creation_failed, ClassName),
                            Err1 = beamtalk_error:with_hint(Err0, <<"Failed to start subclass">>),
                            {error, beamtalk_error:with_details(Err1, #{reason => ErrorReason})}
                    end
            end
    end.
