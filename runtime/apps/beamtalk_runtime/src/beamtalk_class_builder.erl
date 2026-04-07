%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_builder).

%%% **DDD Context:** Object System Context

-moduledoc """
ClassBuilder runtime backing for ADR 0038 (ClassBuilder Protocol).

This module implements the `register/1` function that is the Erlang backing
for the `@intrinsic classBuilderRegister` intrinsic. It accepts a builder
state map accumulated by the ClassBuilder gen_server, validates it, and
delegates to `beamtalk_object_class:start/2` for first registration or
`beamtalk_object_class:update_class/2` for hot reload.

All class registration requires a `moduleName` key (or defaults to className)
and uses compiled BEAM module dispatch.

## Hot Reload

When a class is already registered (module on_load runs twice or during
interactive redefinition), `start/2` returns `{error, {already_started, _}}`.
`register/1` detects this and falls back to `update_class/2`, keeping the
existing gen_server process but refreshing its metadata.

## Builder Lifecycle

After successful registration, `register/1` stops the builder gen_server
(if `builderPid` is in the state map) via `gen_server:stop/3` with a 5-second
timeout. The builder is single-use: create, configure, register, done.

## References

* ADR 0038: `docs/ADR/0038-subclass-classbuilder-protocol.md`
* Pattern: `beamtalk_class_bt.erl`, `beamtalk_metaclass_bt.erl`
* Existing registration: `beamtalk_object_class:start/2`, `update_class/2`
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([register/1]).

%%% ============================================================================
%%% API
%%% ============================================================================

-doc """
Register a class from a builder state map.

Accepts a map with the following keys:
  - `className`    — atom (required): name of the class to register
  - `superclassRef` — atom | pid (required, not nil): superclass reference
  - `fieldSpecs`   — map (optional): field_name => default_value
  - `methodSpecs`  — map (optional): selector => fun()
  - `modifiers`    — list (optional): [:abstract, :sealed, ...]
  - `builderPid`   — pid (optional): builder process to stop after registration

Additional keys for compiled classes (BT-837 / ADR 0038 Phase 3):
  - `moduleName`   — atom: compiled Erlang module name (default: className)
  - `classMethods` — map: class method specs (default: #{})
  - `methodSource`  — map: selector => binary source text (default: #{})
  - `classState`   — map: class variable defaults (default: #{})
  - `classDoc`     — binary | none: class doc comment (default: none)
  - `methodDocs`   — map: selector => binary doc text (default: #{})

On success: registers the class with the runtime and returns `{ok, ClassPid}`.
Hot reload: if the class already exists, updates it and returns `{ok, ClassPid}`.
On failure: returns `{error, #beamtalk_error{}}` without stopping any process.

ADR 0038 Phase 1: Backing for `@intrinsic classBuilderRegister`.
""".
-spec register(map()) -> {ok, pid()} | {error, #beamtalk_error{}}.
register(BuilderState) when is_map(BuilderState) ->
    ClassName = maps:get(className, BuilderState, nil),
    SuperclassRef = maps:get(superclassRef, BuilderState, nil),
    FieldSpecs = maps:get(fieldSpecs, BuilderState, #{}),
    MethodSpecs = maps:get(methodSpecs, BuilderState, #{}),
    Modifiers = maps:get(modifiers, BuilderState, []),
    BuilderPid = maps:get(builderPid, BuilderState, undefined),
    %% BT-791: stdlib_mode bypasses sealed-superclass check so stdlib classes
    %% like Character (extends sealed Integer) can load via on_load hooks.
    StdlibMode = maps:get(stdlibMode, BuilderState, false),
    case validate(ClassName, SuperclassRef, StdlibMode) of
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

-doc "Attempt to start or update the class gen_server.".
-spec do_register(atom(), map()) -> {ok, pid()} | {error, #beamtalk_error{}}.
do_register(ClassName, ClassInfo) ->
    case beamtalk_object_class:start(ClassName, ClassInfo) of
        {ok, Pid} ->
            ?LOG_DEBUG("Registered class via ClassBuilder", #{
                class => ClassName, module => ?MODULE, domain => [beamtalk, runtime]
            }),
            notify_class_loaded(ClassName),
            {ok, Pid};
        {error, {already_started, _}} ->
            %% Hot reload path: class already exists, update its metadata.
            case beamtalk_object_class:update_class(ClassName, ClassInfo) of
                {ok, _IVars} ->
                    notify_class_loaded(ClassName),
                    {ok, beamtalk_class_registry:whereis_class(ClassName)};
                {error, Reason} ->
                    ?LOG_WARNING("ClassBuilder update_class failed", #{
                        class => ClassName,
                        reason => Reason,
                        module => ?MODULE,
                        domain => [beamtalk, runtime]
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

-doc """
Validate the builder state before registration.

Returns ok if valid, {error, #beamtalk_error{}} otherwise.

Checks:
  1. className is an atom (Symbol in Beamtalk terms)
  2. superclassRef is set (not nil)
  3. superclassRef is a valid type (atom | pid | #beamtalk_object{})
  4. superclass is not sealed (unless StdlibMode = true)

Note: sealed-superclass enforcement is bypassed when StdlibMode = true.
The Beamtalk compiler (semantic_analysis) enforces it at compile time,
and stdlib classes compiled with stdlib_mode are explicitly permitted to
subclass sealed classes (e.g. Character extends Integer). Enforcing it
again at runtime causes stdlib on_load hooks to fail when topo-sorted
stdlib loading registers Integer (sealed) before Character (BT-791).
""".
-spec validate(term(), term(), boolean()) -> ok | {error, #beamtalk_error{}}.
validate(nil, _, _) ->
    Error0 = beamtalk_error:new(missing_parameter, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'name:'),
    {error, beamtalk_error:with_hint(Error1, <<"className must be a Symbol (atom)">>)};
validate(ClassName, _, _) when not is_atom(ClassName) ->
    Error0 = beamtalk_error:new(type_error, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'name:'),
    {error, beamtalk_error:with_hint(Error1, <<"className must be a Symbol (atom)">>)};
validate(_ClassName, nil, _) ->
    Error0 = beamtalk_error:new(no_superclass, 'ClassBuilder'),
    Error1 = beamtalk_error:with_selector(Error0, 'superclass:'),
    {error,
        beamtalk_error:with_hint(Error1, <<"superclassRef must be set before calling register">>)};
validate(_ClassName, SuperclassRef, _) when
    not is_atom(SuperclassRef),
    not is_pid(SuperclassRef),
    not is_record(SuperclassRef, beamtalk_object)
->
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
        )};
validate(_ClassName, SuperclassRef, false) ->
    validate_superclass_not_sealed(SuperclassRef);
validate(_ClassName, _SuperclassRef, true) ->
    %% BT-791: stdlib mode — skip sealed check so stdlib on_load hooks succeed.
    ok.

-doc """
Check that the superclass is not sealed (user code enforcement).

Looks up the superclass class process and checks `is_sealed`. Only called
when StdlibMode = false (user-invoked classBuilder). Stdlib on_load hooks
pass StdlibMode = true and skip this check entirely (BT-791).
""".
-spec validate_superclass_not_sealed(atom() | pid() | #beamtalk_object{}) ->
    ok | {error, #beamtalk_error{}}.
validate_superclass_not_sealed(SuperclassRef) ->
    SuperclassPid =
        case SuperclassRef of
            Name when is_atom(Name) -> beamtalk_class_registry:whereis_class(Name);
            Pid when is_pid(Pid) -> Pid;
            #beamtalk_object{pid = Pid} -> Pid
        end,
    case SuperclassPid of
        undefined ->
            %% Unknown superclass — allow registration; name resolution will fail later
            ok;
        ClassPid when is_pid(ClassPid) ->
            case beamtalk_object_class:is_sealed(ClassPid) of
                true ->
                    SuperclassName = beamtalk_object_class:class_name(ClassPid),
                    Error0 = beamtalk_error:new(instantiation_error, 'ClassBuilder'),
                    Error1 = beamtalk_error:with_selector(Error0, 'superclass:'),
                    {error,
                        beamtalk_error:with_hint(
                            Error1,
                            iolist_to_binary(
                                io_lib:format(
                                    "cannot subclass sealed class ~p",
                                    [SuperclassName]
                                )
                            )
                        )};
                false ->
                    ok
            end
    end.

-doc """
Resolve a superclass reference to a class name atom.

Accepts:
  - atom: returned as-is
  - pid: class gen_server pid → resolved via class_name/1
  - #beamtalk_object{}: extracts class name from the object record
""".
-spec resolve_superclass_name(atom() | pid() | #beamtalk_object{}) -> atom().
resolve_superclass_name(Name) when is_atom(Name) ->
    Name;
resolve_superclass_name(Pid) when is_pid(Pid) ->
    beamtalk_object_class:class_name(Pid);
resolve_superclass_name(#beamtalk_object{pid = Pid}) when is_pid(Pid) ->
    beamtalk_object_class:class_name(Pid).

-doc """
Build a ClassInfo map for beamtalk_object_class:start/2.

BT-837: Accepts the full builder state as the last argument to extract
additional metadata for compiled classes (moduleName, classMethods,
methodSource, classState, classDoc, methodDocs).

BT-873: Dynamic path (BT-838) removed. All classes go through the
compiled path regardless of whether moduleName is present.
""".
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

-doc "Build ClassInfo for a compiled class.".
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
    %% BT-877: Read is_constructible from compiler inference.
    %% The compiler emits isConstructible based on the `new => self error:` pattern.
    %% Superclass inheritance is handled at init/query time via the class hierarchy.
    IsConstructible = maps:get(isConstructible, BuilderState, undefined),
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
        is_constructible,
        IsConstructible,
        maybe_put(
            method_source,
            maps:get(methodSource, BuilderState, undefined),
            maybe_put(
                method_signatures,
                maps:get(methodSignatures, BuilderState, undefined),
                maybe_put(
                    class_method_signatures,
                    maps:get(classMethodSignatures, BuilderState, undefined),
                    maybe_put(
                        method_return_types,
                        maps:get(methodReturnTypes, BuilderState, undefined),
                        maybe_put(
                            class_method_return_types,
                            maps:get(classMethodReturnTypes, BuilderState, undefined),
                            maybe_put(
                                class_state,
                                maps:get(classState, BuilderState, undefined),
                                maybe_put(
                                    doc,
                                    maps:get(classDoc, BuilderState, undefined),
                                    maybe_put(
                                        method_docs,
                                        maps:get(methodDocs, BuilderState, undefined),
                                        maybe_put(
                                            class_method_docs,
                                            maps:get(classMethodDocs, BuilderState, undefined),
                                            %% ADR 0050 Phase 5: Pass meta map from BuilderState so
                                            %% beamtalk_object_class:init/1 can use it during on_load
                                            %% (erlang:function_exported/3 returns false at that time).
                                            maybe_put(
                                                meta,
                                                maps:get(meta, BuilderState, undefined),
                                                Base
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ).

-doc "Conditionally add a key to a map (skips undefined values).".
-spec maybe_put(atom(), term(), map()) -> map().
maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) ->
    maps:put(Key, Value, Map).

-doc """
Convert a methodSpecs map to the instance_methods format expected by the class gen_server.

Each entry in methodSpecs is either:
  - `selector => fun()`: closure (converted to #{block, arity} format)
  - `selector => #{arity => N}`: compiled method reference (already in the right format)
""".
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

-doc """
Stop the builder gen_server process after successful registration.

Only called if builderPid is present in the builder state map.
Uses gen_server:stop/3 with a 5-second timeout. Catches exit signals for
the case where the builder exits between the call and the stop (TOCTOU).

Not called in error cases — the caller may wish to retry or inspect the builder.
""".
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

-doc """
Notify class_load_callback module that a class was loaded (BT-1020).
Checks application env `class_load_callback` — same pattern as `actor_spawn_callback`.
Safe to call if the callback module is not running.
""".
-spec notify_class_loaded(atom()) -> ok.
notify_class_loaded(ClassName) ->
    case application:get_env(beamtalk_runtime, class_load_callback) of
        {ok, Mod} ->
            try
                Mod:on_class_loaded(ClassName)
            catch
                error:undef ->
                    %% Callback module doesn't implement on_class_loaded/1
                    ?LOG_WARNING("Class load callback not implemented", #{
                        callback => Mod,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    });
                Kind:Reason:Stacktrace ->
                    %% Unexpected failure in callback — log but don't fail class load
                    ?LOG_WARNING("Class load callback failed", #{
                        callback => Mod,
                        class => ClassName,
                        kind => Kind,
                        reason => Reason,
                        stacktrace => Stacktrace,
                        domain => [beamtalk, runtime]
                    })
            end;
        undefined ->
            ok
    end.
