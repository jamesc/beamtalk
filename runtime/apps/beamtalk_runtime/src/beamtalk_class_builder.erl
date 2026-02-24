%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc ClassBuilder runtime backing for ADR 0038 (ClassBuilder Protocol Phase 1).
%%%
%%% **DDD Context:** Object System
%%%
%%% This module implements the `register/1` function that is the Erlang backing
%%% for the `@intrinsic classBuilderRegister` intrinsic. It accepts a builder
%%% state map accumulated by the ClassBuilder gen_server, validates it, and
%%% delegates to `beamtalk_object_class:start/2` for first registration or
%%% `beamtalk_object_class:update_class/2` for hot reload.
%%%
%%% ## Phase 1 (BT-835): Runtime Backing and Bootstrap
%%%
%%% Provides the `register/1` function used by the ClassBuilder protocol. The
%%% ClassBuilder gen_server itself (Phase 2) will call this function as the
%%% terminal operation of the fluent builder pattern.
%%%
%%% ## Hot Reload
%%%
%%% When a class is already registered (module on_load runs twice or during
%%% interactive redefinition), `start/2` returns `{error, {already_started, _}}`.
%%% `register/1` detects this and falls back to `update_class/2`, keeping the
%%% existing gen_server process but refreshing its metadata.
%%%
%%% ## Builder Lifecycle
%%%
%%% After successful registration, `register/1` stops the builder gen_server
%%% (if `builderPid` is in the state map) via `gen_server:stop/1`. The builder
%%% is single-use: create, configure, register, done.
%%%
%%% ## References
%%%
%%% * ADR 0038: `docs/ADR/0038-subclass-classbuilder-protocol.md` (Phase 1)
%%% * Pattern: `beamtalk_class_bt.erl`, `beamtalk_metaclass_bt.erl`
%%% * Existing registration: `beamtalk_object_class:start/2`, `update_class/2`

-module(beamtalk_class_builder).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([register/1]).

%%% ============================================================================
%%% API
%%% ============================================================================

%% @doc Register a class from a builder state map.
%%
%% Accepts a map with the following keys:
%%   - `className`    — atom (required): name of the class to register
%%   - `superclassRef` — atom | pid (required, not nil): superclass reference
%%   - `fieldSpecs`   — map (optional): field_name => default_value
%%   - `methodSpecs`  — map (optional): selector => fun()
%%   - `modifiers`    — list (optional): [:abstract, :sealed, ...]
%%   - `builderPid`   — pid (optional): builder process to stop after registration
%%
%% On success: registers the class with the runtime and returns `{ok, ClassPid}`.
%% Hot reload: if the class already exists, updates it and returns `{ok, ClassPid}`.
%% On failure: returns `{error, #beamtalk_error{}}` without stopping any process.
%%
%% ADR 0038 Phase 1: Backing for `@intrinsic classBuilderRegister`.
-spec register(map()) -> {ok, pid()} | {error, #beamtalk_error{}}.
register(BuilderState) when is_map(BuilderState) ->
    ClassName = maps:get(className, BuilderState, nil),
    SuperclassRef = maps:get(superclassRef, BuilderState, nil),
    FieldSpecs = maps:get(fieldSpecs, BuilderState, #{}),
    MethodSpecs = maps:get(methodSpecs, BuilderState, #{}),
    Modifiers = maps:get(modifiers, BuilderState, []),
    BuilderPid = maps:get(builderPid, BuilderState, undefined),
    case validate(ClassName, SuperclassRef) of
        {error, _} = Err ->
            Err;
        ok ->
            SuperclassName = resolve_superclass_name(SuperclassRef),
            ClassInfo = build_class_info(
                ClassName, SuperclassName, FieldSpecs, MethodSpecs, Modifiers
            ),
            case do_register(ClassName, ClassInfo) of
                {ok, ClassPid} ->
                    maybe_stop_builder(BuilderPid),
                    {ok, ClassPid};
                {error, _} = Err ->
                    Err
            end
    end.

%%% ============================================================================
%%% Internal functions
%%% ============================================================================

%% @private
%% @doc Attempt to start or update the class gen_server.
-spec do_register(atom(), map()) -> {ok, pid()} | {error, #beamtalk_error{}}.
do_register(ClassName, ClassInfo) ->
    case beamtalk_object_class:start(ClassName, ClassInfo) of
        {ok, Pid} ->
            ?LOG_INFO("Registered class via ClassBuilder", #{
                class => ClassName, module => ?MODULE
            }),
            {ok, Pid};
        {error, {already_started, _}} ->
            %% Hot reload path: class already exists, update its metadata.
            case beamtalk_object_class:update_class(ClassName, ClassInfo) of
                {ok, _IVars} ->
                    ?LOG_INFO("Updated class via ClassBuilder (hot reload)", #{
                        class => ClassName, module => ?MODULE
                    }),
                    {ok, beamtalk_class_registry:whereis_class(ClassName)};
                {error, Reason} ->
                    ?LOG_WARNING("ClassBuilder update_class failed", #{
                        class => ClassName, reason => Reason, module => ?MODULE
                    }),
                    Error0 = beamtalk_error:new(internal_error, 'ClassBuilder'),
                    Error1 = beamtalk_error:with_selector(Error0, register),
                    Error = beamtalk_error:with_hint(
                        Error1,
                        iolist_to_binary(io_lib:format("update_class failed: ~p", [Reason]))
                    ),
                    {error, Error}
            end;
        {error, Reason} ->
            Error0 = beamtalk_error:new(internal_error, 'ClassBuilder'),
            Error1 = beamtalk_error:with_selector(Error0, register),
            Error = beamtalk_error:with_hint(
                Error1,
                iolist_to_binary(io_lib:format("start failed: ~p", [Reason]))
            ),
            {error, Error}
    end.

%% @private
%% @doc Validate the builder state before registration.
%%
%% Returns ok if valid, {error, #beamtalk_error{}} otherwise.
%%
%% Checks:
%%   1. className is an atom (Symbol in Beamtalk terms)
%%   2. superclassRef is set (not nil)
%%   3. superclass is not sealed (if it is already registered)
-spec validate(term(), term()) -> ok | {error, #beamtalk_error{}}.
validate(nil, _) ->
    Error0 = beamtalk_error:new(missing_parameter, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'name:'),
    {error, beamtalk_error:with_hint(Error1, <<"className must be a Symbol (atom)">>)};
validate(ClassName, _) when not is_atom(ClassName) ->
    Error0 = beamtalk_error:new(type_error, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'name:'),
    {error, beamtalk_error:with_hint(Error1, <<"className must be a Symbol (atom)">>)};
validate(_ClassName, nil) ->
    Error0 = beamtalk_error:new(no_superclass, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'superclass:'),
    {error,
        beamtalk_error:with_hint(Error1, <<"superclassRef must be set before calling register">>)};
validate(_ClassName, SuperclassRef) ->
    validate_superclass_not_sealed(SuperclassRef).

%% @private
-spec validate_superclass_not_sealed(term()) -> ok | {error, #beamtalk_error{}}.
validate_superclass_not_sealed(SuperclassRef) ->
    SuperclassName = resolve_superclass_name(SuperclassRef),
    case beamtalk_class_registry:whereis_class(SuperclassName) of
        undefined ->
            %% Superclass not yet registered — acceptable during bootstrap ordering.
            ok;
        Pid ->
            case beamtalk_object_class:is_sealed(Pid) of
                true ->
                    Error0 = beamtalk_error:new(instantiation_error, SuperclassName),
                    Error1 = beamtalk_error:with_selector(Error0, register),
                    {error,
                        beamtalk_error:with_hint(
                            Error1,
                            <<"Cannot subclass a sealed class">>
                        )};
                false ->
                    ok
            end
    end.

%% @private
%% @doc Resolve a superclass reference to a class name atom.
%%
%% Accepts:
%%   - atom: returned as-is
%%   - pid: class gen_server pid → resolved via class_name/1
%%   - #beamtalk_object{}: extracts class name from the object record
-spec resolve_superclass_name(atom() | pid() | #beamtalk_object{}) -> atom().
resolve_superclass_name(Name) when is_atom(Name) ->
    Name;
resolve_superclass_name(Pid) when is_pid(Pid) ->
    beamtalk_object_class:class_name(Pid);
resolve_superclass_name(#beamtalk_object{class = ClassName}) ->
    ClassName.

%% @private
%% @doc Build a ClassInfo map for beamtalk_object_class:start/2.
-spec build_class_info(atom(), atom(), map(), map(), list()) -> map().
build_class_info(ClassName, SuperclassName, FieldSpecs, MethodSpecs, Modifiers) ->
    IsSealed = lists:member(sealed, Modifiers),
    IsAbstract = lists:member(abstract, Modifiers),
    Fields = maps:keys(FieldSpecs),
    InstanceMethods = build_method_map(MethodSpecs),
    #{
        name => ClassName,
        superclass => SuperclassName,
        module => ClassName,
        fields => Fields,
        instance_methods => InstanceMethods,
        class_methods => #{},
        is_sealed => IsSealed,
        is_abstract => IsAbstract
    }.

%% @private
%% @doc Convert a methodSpecs map to the instance_methods format expected by the class gen_server.
%%
%% Each entry in methodSpecs is either:
%%   - `selector => fun()`: dynamic method (closure-based, Phase 2)
%%   - `selector => #{arity => N}`: compiled method reference (already in the right format)
-spec build_method_map(map()) -> map().
build_method_map(MethodSpecs) when is_map(MethodSpecs) ->
    maps:fold(
        fun
            (Selector, Fun, Acc) when is_function(Fun) ->
                {arity, Arity} = erlang:fun_info(Fun, arity),
                maps:put(Selector, #{block => Fun, arity => Arity}, Acc);
            (Selector, MethodInfo, Acc) when is_map(MethodInfo) ->
                maps:put(Selector, MethodInfo, Acc);
            (_Selector, _Other, Acc) ->
                Acc
        end,
        #{},
        MethodSpecs
    ).

%% @private
%% @doc Stop the builder gen_server process after successful registration.
%%
%% Only called if builderPid is present in the builder state map.
%% Uses gen_server:stop/1 which waits for the process to terminate.
%%
%% Not called in error cases — the caller may wish to retry or inspect the builder.
-spec maybe_stop_builder(pid() | undefined) -> ok.
maybe_stop_builder(undefined) ->
    ok;
maybe_stop_builder(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(Pid, normal, 5000);
        false ->
            ok
    end.
