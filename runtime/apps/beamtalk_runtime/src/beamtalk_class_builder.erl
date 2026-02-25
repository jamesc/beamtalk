%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc ClassBuilder runtime backing for ADR 0038 (ClassBuilder Protocol).
%%%
%% **DDD Context:** Object System
%%%
%%% This module implements the `register/1` function that is the Erlang backing
%%% for the `@intrinsic classBuilderRegister` intrinsic. It accepts a builder
%%% state map accumulated by the ClassBuilder gen_server, validates it, and
%%% delegates to `beamtalk_object_class:start/2` for first registration or
%%% `beamtalk_object_class:update_class/2` for hot reload.
%%%
%%% ## Single Path (BT-873)
%%%
%%% All class creation goes through the compiled path (Path 1). The dynamic
%%% closure-based path (BT-838 / ADR 0038 Path 2) has been removed. Every
%%% class registration requires a `moduleName` key (or defaults to className)
%%% and uses compiled BEAM module dispatch.
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
%%% (if `builderPid` is in the state map) via `gen_server:stop/3` with a 5-second
%%% timeout. The builder is single-use: create, configure, register, done.
%%%
%%% ## References
%%%
%%% * ADR 0038: `docs/ADR/0038-subclass-classbuilder-protocol.md`
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
%% Additional keys for compiled classes (BT-837 / ADR 0038 Phase 3):
%%   - `moduleName`   — atom: compiled Erlang module name (default: className)
%%   - `classMethods` — map: class method specs (default: #{})
%%   - `methodSource`  — map: selector => binary source text (default: #{})
%%   - `classState`   — map: class variable defaults (default: #{})
%%   - `classDoc`     — binary | none: class doc comment (default: none)
%%   - `methodDocs`   — map: selector => binary doc text (default: #{})
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
        ok when is_map(FieldSpecs), is_map(MethodSpecs) ->
            SuperclassName = resolve_superclass_name(SuperclassRef),
            ClassInfo = build_class_info(
                ClassName,
                SuperclassName,
                FieldSpecs,
                MethodSpecs,
                Modifiers,
                BuilderState
            ),
            case do_register(ClassName, ClassInfo) of
                {ok, ClassPid} ->
                    maybe_stop_builder(BuilderPid),
                    {ok, ClassPid};
                {error, _} = Err ->
                    Err
            end;
        ok ->
            Error0 = beamtalk_error:new(type_error, 'ClassBuilder'),
            Error1 = beamtalk_error:with_selector(Error0, register),
            {error, beamtalk_error:with_hint(Error1, <<"fieldSpecs and methodSpecs must be maps">>)}
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
%%   3. superclassRef is a valid type (atom | pid | #beamtalk_object{})
%%   4. superclass is not sealed (if it is already registered)
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
validate(_ClassName, SuperclassRef) when
    is_atom(SuperclassRef); is_pid(SuperclassRef); is_record(SuperclassRef, beamtalk_object)
->
    validate_superclass_not_sealed(SuperclassRef);
validate(_ClassName, SuperclassRef) ->
    Error0 = beamtalk_error:new(type_error, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'superclass:'),
    {error,
        beamtalk_error:with_hint(
            Error1,
            iolist_to_binary(
                io_lib:format(
                    "superclassRef must be an atom, pid, or class object, got: ~p",
                    [SuperclassRef]
                )
            )
        )}.

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
resolve_superclass_name(#beamtalk_object{pid = Pid}) when is_pid(Pid) ->
    beamtalk_object_class:class_name(Pid).

%% @private
%% @doc Build a ClassInfo map for beamtalk_object_class:start/2.
%%
%% BT-837: Accepts the full builder state as the last argument to extract
%% additional metadata for compiled classes (moduleName, classMethods,
%% methodSource, classState, classDoc, methodDocs).
%%
%% BT-873: Dynamic path (BT-838) removed. All classes go through the
%% compiled path regardless of whether moduleName is present.
-spec build_class_info(atom(), atom(), map(), map(), list(), map()) -> map().
build_class_info(ClassName, SuperclassName, FieldSpecs, MethodSpecs, Modifiers, BuilderState) ->
    IsSealed = lists:member(sealed, Modifiers),
    IsAbstract = lists:member(abstract, Modifiers),
    Fields = maps:keys(FieldSpecs),
    build_compiled_class_info(
        ClassName,
        SuperclassName,
        Fields,
        MethodSpecs,
        Modifiers,
        IsSealed,
        IsAbstract,
        BuilderState
    ).

%% @private
%% @doc Build ClassInfo for a compiled class (Path 1).
-spec build_compiled_class_info(
    atom(),
    atom(),
    [atom()],
    map(),
    list(),
    boolean(),
    boolean(),
    map()
) -> map().
build_compiled_class_info(
    ClassName,
    SuperclassName,
    Fields,
    MethodSpecs,
    _Modifiers,
    IsSealed,
    IsAbstract,
    BuilderState
) ->
    InstanceMethods = build_method_map(MethodSpecs),
    Module = maps:get(moduleName, BuilderState, ClassName),
    ClassMethods = maps:get(classMethods, BuilderState, #{}),
    Base = #{
        name => ClassName,
        superclass => SuperclassName,
        module => Module,
        fields => Fields,
        instance_methods => InstanceMethods,
        class_methods => ClassMethods,
        is_sealed => IsSealed,
        is_abstract => IsAbstract
    },
    %% BT-837: Pass through optional compiler metadata if present
    maybe_put(
        method_source,
        maps:get(methodSource, BuilderState, undefined),
        maybe_put(
            class_state,
            maps:get(classState, BuilderState, undefined),
            maybe_put(
                doc,
                maps:get(classDoc, BuilderState, undefined),
                maybe_put(
                    method_docs,
                    maps:get(methodDocs, BuilderState, undefined),
                    Base
                )
            )
        )
    ).

%% @private
%% @doc Conditionally add a key to a map (skips undefined values).
-spec maybe_put(atom(), term(), map()) -> map().
maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) ->
    maps:put(Key, Value, Map).

%% @private
%% @doc Convert a methodSpecs map to the instance_methods format expected by the class gen_server.
%%
%% Each entry in methodSpecs is either:
%%   - `selector => fun()`: closure (converted to #{block, arity} format)
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
%% Uses gen_server:stop/3 with a 5-second timeout. Catches exit signals for
%% the case where the builder exits between the call and the stop (TOCTOU).
%%
%% Not called in error cases — the caller may wish to retry or inspect the builder.
-spec maybe_stop_builder(pid() | undefined) -> ok.
maybe_stop_builder(undefined) ->
    ok;
maybe_stop_builder(Pid) when is_pid(Pid) ->
    case Pid =:= self() of
        true ->
            %% BT-838: Builder calling register from its own gen_server handler.
            %% Cannot stop self synchronously — the process will be cleaned up
            %% when the caller drops the reference.
            ok;
        false ->
            try
                gen_server:stop(Pid, normal, 5000)
            catch
                exit:noproc -> ok;
                exit:normal -> ok;
                exit:timeout -> ok;
                exit:{noproc, _} -> ok
            end
    end.
