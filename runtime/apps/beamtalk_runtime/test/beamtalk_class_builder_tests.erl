%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_builder_tests).

-moduledoc """
EUnit tests for beamtalk_class_builder:register/1 (ADR 0038 Phase 1, BT-835).

Tests:
  - happy path: successful class registration
  - hot reload: registering an existing class updates it
  - missing superclass error: superclassRef = nil
  - sealed superclass rejected (stdlibMode bypasses this for stdlib loading, BT-791)
  - bad name error: className is not an atom (nil or non-atom)
  - bootstrap assertion: Class respondsTo: #classBuilder
  - BT-1967: missing className key defaults to nil error
  - BT-1967: missing superclassRef key defaults to nil error
  - BT-1967: integer className rejected
  - BT-1967: non-map methodSpecs rejected
  - BT-1967: builder pid stopped after successful registration
  - BT-1967: builder pid = self() skips stop
  - BT-1967: compiled class metadata passthrough
  - BT-1967: method specs with compiled method info map
  - BT-1967: sealed and abstract modifiers applied
  - BT-1967: hot reload updates methods
  - BT-1967: superclass resolution via pid reference
  - BT-1967: class_load_callback notification
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / Teardown
%%====================================================================

setup() ->
    %% Ensure pg is started (required by class registry)
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    %% Ensure the ETS hierarchy table exists
    beamtalk_class_registry:ensure_hierarchy_table(),
    %% Track class names created during each test for cleanup
    [].

teardown(ClassNames) ->
    %% Stop and unregister any classes created during the test
    lists:foreach(
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
            end
        end,
        ClassNames
    ).

%%====================================================================
%% Happy Path
%%====================================================================

register_happy_path_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835TestClassA',
                    superclassRef => 'Object',
                    fieldSpecs => #{x => 0, y => 0},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({ok, _Pid}, Result),
                {ok, Pid} = Result,
                ?assert(is_pid(Pid)),
                ?assert(is_process_alive(Pid)),
                %% Verify class is registered
                ?assertEqual(Pid, beamtalk_class_registry:whereis_class('BT835TestClassA')),
                %% Verify superclass recorded
                ?assertEqual('Object', beamtalk_object_class:superclass(Pid)),
                %% Cleanup
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

register_with_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(Self, _Args, State) ->
                    {reply, maps:get(val, State, 0), State, Self}
                end,
                State = #{
                    className => 'BT835TestClassB',
                    superclassRef => 'Object',
                    fieldSpecs => #{val => 0},
                    methodSpecs => #{'getValue' => MethodFun}
                },
                {ok, Pid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(Pid)),
                %% Verify method is registered
                ?assert(beamtalk_object_class:has_method(Pid, 'getValue')),
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% Hot Reload
%%====================================================================

register_hot_reload_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State1 = #{
                    className => 'BT835HotReloadClass',
                    superclassRef => 'Object',
                    fieldSpecs => #{a => 0},
                    methodSpecs => #{}
                },
                {ok, Pid1} = beamtalk_class_builder:register(State1),
                ?assert(is_process_alive(Pid1)),

                %% Register again (hot reload): should update, not crash
                State2 = #{
                    className => 'BT835HotReloadClass',
                    superclassRef => 'Object',
                    fieldSpecs => #{a => 0, b => 1},
                    methodSpecs => #{}
                },
                Result2 = beamtalk_class_builder:register(State2),
                ?assertMatch({ok, _}, Result2),

                %% The class gen_server should still be alive (same or new pid)
                CurrentPid = beamtalk_class_registry:whereis_class('BT835HotReloadClass'),
                ?assert(is_pid(CurrentPid)),
                ?assert(is_process_alive(CurrentPid)),

                gen_server:stop(CurrentPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% Validation Errors
%%====================================================================

register_missing_superclass_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835NoSuperclass',
                    superclassRef => nil,
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = no_superclass}}, Result)
            end)
        ]
    end}.

%% Sealed-superclass enforcement is done at runtime for user code.
%% StdlibMode = true bypasses this for stdlib on_load hooks (BT-791).
register_sealed_superclass_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register a sealed class first
                SealedClassInfo = #{
                    name => 'BT835SealedClass',
                    superclass => 'Object',
                    module => beamtalk_class_builder_bt,
                    fields => [],
                    class_methods => #{},
                    instance_methods => #{},
                    is_sealed => true
                },
                {ok, SealedPid} = beamtalk_object_class:start('BT835SealedClass', SealedClassInfo),

                %% Subclassing a sealed class fails for user code (no stdlibMode)
                State = #{
                    className => 'BT835SealedSubclass',
                    superclassRef => 'BT835SealedClass',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = instantiation_error}}, Result),

                %% Subclassing succeeds when stdlibMode = true
                StdlibState = State#{stdlibMode => true},
                StdlibResult = beamtalk_class_builder:register(StdlibState),
                ?assertMatch({ok, _Pid}, StdlibResult),
                {ok, SubPid} = StdlibResult,

                %% Cleanup
                gen_server:stop(SubPid, normal, 5000),
                gen_server:stop(SealedPid, normal, 5000)
            end)
        ]
    end}.

register_bad_name_nil_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => nil,
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = missing_parameter}}, Result)
            end)
        ]
    end}.

register_bad_name_non_atom_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => <<"NotAnAtom">>,
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

register_bad_superclass_ref_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835BadSuperRef',
                    superclassRef => <<"NotValidRef">>,
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

register_bad_specs_type_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835BadSpecs',
                    superclassRef => 'Object',
                    fieldSpecs => not_a_map,
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

%%====================================================================
%% Bootstrap Assertion: Class respondsTo: #classBuilder
%%====================================================================

class_responds_to_classbuilder_test_() ->
    {setup,
        fun() ->
            %% Need a running bootstrap to have the Class stub registered
            case whereis(pg) of
                undefined -> {ok, _} = pg:start_link();
                _ -> ok
            end,
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_bt:register_class(),
            []
        end,
        fun teardown/1, fun(_) ->
            [
                ?_test(begin
                    ClassPid = beamtalk_class_registry:whereis_class('Class'),
                    ?assert(is_pid(ClassPid)),
                    %% Post-bootstrap assertion: Class respondsTo: #classBuilder
                    %% (ADR 0038 Phase 1 acceptance criterion)
                    ?assert(beamtalk_object_class:has_method(ClassPid, 'classBuilder'))
                end)
            ]
        end}.

%%====================================================================
%% Bootstrap Sequence: ClassBuilder is registered after Metaclass
%%====================================================================

classbuilder_registered_in_bootstrap_test_() ->
    {setup,
        fun() ->
            case whereis(pg) of
                undefined -> {ok, _} = pg:start_link();
                _ -> ok
            end,
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_bt:register_class(),
            beamtalk_metaclass_bt:register_class(),
            beamtalk_class_builder_bt:register_class(),
            []
        end,
        fun teardown/1, fun(_) ->
            [
                ?_test(begin
                    ClassBuilderPid = beamtalk_class_registry:whereis_class('ClassBuilder'),
                    ?assert(is_pid(ClassBuilderPid)),
                    ?assert(is_process_alive(ClassBuilderPid)),
                    %% ClassBuilder has superclass 'Actor'
                    ?assertEqual('Actor', beamtalk_object_class:superclass(ClassBuilderPid))
                end)
            ]
        end}.

%%====================================================================
%% BT-1967: Missing keys default to nil → error
%%====================================================================

register_missing_classname_key_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% className key absent entirely — defaults to nil via maps:get/3
                State = #{
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = missing_parameter}}, Result)
            end)
        ]
    end}.

register_missing_superclass_key_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% superclassRef key absent entirely — defaults to nil
                State = #{
                    className => 'BT1967NoSuperKey',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = no_superclass}}, Result)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Integer className rejected
%%====================================================================

register_integer_classname_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 42,
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Non-map methodSpecs rejected
%%====================================================================

register_bad_method_specs_type_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT1967BadMethodSpecs',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => not_a_map
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Builder pid stopped after successful registration
%%====================================================================

register_stops_builder_pid_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Spawn a dummy gen_server to act as the builder pid
                {ok, BuilderPid} = gen_server:start(
                    {local, bt1967_dummy_builder},
                    beamtalk_test_gen_server,
                    [],
                    []
                ),
                ?assert(is_process_alive(BuilderPid)),

                State = #{
                    className => 'BT1967BuilderStop',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{},
                    builderPid => BuilderPid
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),

                %% Builder should have been stopped
                timer:sleep(100),
                ?assertNot(is_process_alive(BuilderPid)),

                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Builder pid = self skips stop (no deadlock)
%%====================================================================

register_builder_pid_self_noop_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% When builderPid is self(), the stop should be skipped
                %% (can't stop yourself synchronously).
                %% We pass self() as the builderPid and verify we're still alive after.
                State = #{
                    className => 'BT1967SelfBuilder',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{},
                    builderPid => self()
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),
                ?assert(is_process_alive(self())),
                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Compiled class metadata passthrough
%%====================================================================

register_compiled_class_metadata_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT1967CompiledMeta',
                    superclassRef => 'Object',
                    fieldSpecs => #{x => 0},
                    methodSpecs => #{},
                    moduleName => 'bt1967_compiled_meta_bt',
                    classMethods => #{'factory' => #{arity => 0}},
                    classDoc => <<"A test class for metadata passthrough">>,
                    methodDocs => #{'x' => <<"accessor for x">>}
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),

                %% Verify module name was passed through
                ?assertEqual(
                    'bt1967_compiled_meta_bt',
                    beamtalk_object_class:module_name(ClassPid)
                ),

                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Method specs with compiled method info (map form)
%%====================================================================

register_compiled_method_specs_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Method specs as maps (compiled method references) rather than funs
                State = #{
                    className => 'BT1967CompiledMethods',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{
                        'getValue' => #{arity => 0},
                        'setValue:' => #{arity => 1}
                    }
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),
                ?assert(beamtalk_object_class:has_method(ClassPid, 'getValue')),
                ?assert(beamtalk_object_class:has_method(ClassPid, 'setValue:')),
                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Sealed and abstract modifiers applied
%%====================================================================

register_sealed_modifier_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT1967SealedClass',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{},
                    modifiers => [sealed]
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(beamtalk_object_class:is_sealed(ClassPid)),
                ?assertNot(beamtalk_object_class:is_abstract(ClassPid)),
                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.

register_abstract_modifier_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT1967AbstractClass',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{},
                    modifiers => [abstract]
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(beamtalk_object_class:is_abstract(ClassPid)),
                ?assertNot(beamtalk_object_class:is_sealed(ClassPid)),
                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Hot reload updates methods
%%====================================================================

register_hot_reload_updates_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Initial registration with one method
                State1 = #{
                    className => 'BT1967HotReloadMethods',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{'alpha' => #{arity => 0}}
                },
                {ok, _Pid1} = beamtalk_class_builder:register(State1),
                Pid = beamtalk_class_registry:whereis_class('BT1967HotReloadMethods'),
                ?assert(beamtalk_object_class:has_method(Pid, 'alpha')),

                %% Hot reload with different method
                State2 = #{
                    className => 'BT1967HotReloadMethods',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{'beta' => #{arity => 0}}
                },
                {ok, _Pid2} = beamtalk_class_builder:register(State2),
                ReloadPid = beamtalk_class_registry:whereis_class('BT1967HotReloadMethods'),
                ?assert(beamtalk_object_class:has_method(ReloadPid, 'beta')),

                gen_server:stop(ReloadPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: Superclass resolution via pid reference
%%====================================================================

register_superclass_as_pid_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register a parent class first
                ParentInfo = #{
                    name => 'BT1967Parent',
                    superclass => 'Object',
                    module => beamtalk_class_builder_bt,
                    fields => [],
                    class_methods => #{},
                    instance_methods => #{},
                    is_sealed => false
                },
                {ok, ParentPid} = beamtalk_object_class:start('BT1967Parent', ParentInfo),

                %% Register child using pid as superclass reference
                State = #{
                    className => 'BT1967Child',
                    superclassRef => ParentPid,
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                {ok, ChildPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ChildPid)),

                %% Superclass should resolve to the parent's name
                ?assertEqual('BT1967Parent', beamtalk_object_class:superclass(ChildPid)),

                gen_server:stop(ChildPid, normal, 5000),
                gen_server:stop(ParentPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% BT-1967: class_load_callback notification
%%====================================================================

register_class_load_callback_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register a nonexistent callback module in application env.
                %% The undef error should be caught gracefully by notify_class_loaded/1.
                OldVal = application:get_env(beamtalk_runtime, class_load_callback),
                application:set_env(
                    beamtalk_runtime,
                    class_load_callback,
                    beamtalk_test_class_load_callback
                ),
                State = #{
                    className => 'BT1967CallbackTest',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                %% Registration should succeed even if callback module doesn't exist
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),

                %% Restore original env
                case OldVal of
                    undefined -> application:unset_env(beamtalk_runtime, class_load_callback);
                    {ok, V} -> application:set_env(beamtalk_runtime, class_load_callback, V)
                end,

                gen_server:stop(ClassPid, normal, 5000)
            end),

            %% BT-1982: Register passing a #beamtalk_object{} record as
            %% superclassRef — exercises the object-clause of
            %% resolve_superclass_name/1 and validate_superclass_not_sealed/1.
            ?_test(begin
                %% First register a parent class.
                ParentState = #{
                    className => 'BT1982ObjSuperParent',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                {ok, ParentPid} = beamtalk_class_builder:register(ParentState),
                ParentModule = beamtalk_object_class:module_name(ParentPid),
                ParentObj = #beamtalk_object{
                    class = 'BT1982ObjSuperParent class',
                    class_mod = ParentModule,
                    pid = ParentPid
                },
                ChildState = #{
                    className => 'BT1982ObjSuperChild',
                    superclassRef => ParentObj,
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                {ok, ChildPid} = beamtalk_class_builder:register(ChildState),
                ?assertEqual(
                    'BT1982ObjSuperParent', beamtalk_object_class:superclass(ChildPid)
                ),
                gen_server:stop(ChildPid, normal, 5000),
                gen_server:stop(ParentPid, normal, 5000)
            end),

            %% BT-1982: maybe_stop_builder/1 stops an external builder pid
            %% synchronously — covers the gen_server:stop branch (lines 436-439).
            ?_test(begin
                %% Spawn a gen_server-like process that will just exit on stop.
                BuilderPid = spawn(fun() ->
                    receive
                        {'$gen_call', _, _} -> ok;
                        _ -> ok
                    after 5000 -> ok
                    end
                end),
                State = #{
                    className => 'BT1982StopsBuilder',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{},
                    builderPid => BuilderPid
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),
                %% BuilderPid might be stopped via gen_server:stop (which might
                %% timeout since it's not a real gen_server) or killed — either
                %% exercises the code path. Catch timeouts from the fake builder.
                try
                    gen_server:stop(ClassPid, normal, 5000)
                catch
                    exit:_ -> ok
                end,
                %% Cleanup builder
                case is_process_alive(BuilderPid) of
                    true -> exit(BuilderPid, kill);
                    false -> ok
                end
            end),

            %% BT-1982: build_method_map/1 skips entries with unexpected shapes
            %% (neither function nor map) — covers the catch-all fold branch.
            ?_test(begin
                State = #{
                    className => 'BT1982BuildMapWeird',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{
                        %% Both valid and invalid entries — the invalid one hits
                        %% the fold catch-all (line 407).
                        'good' => #{arity => 0},
                        'bad' => weird_atom
                    }
                },
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                Methods = beamtalk_object_class:methods(ClassPid),
                ?assert(lists:member('good', Methods)),
                ?assertNot(lists:member('bad', Methods)),
                gen_server:stop(ClassPid, normal, 5000)
            end),

            %% BT-1982: Callback module that exists but raises a non-undef error
            %% exercises the generic `Kind:Reason:Stacktrace` branch of
            %% notify_class_loaded/1. We dynamically compile a tiny module whose
            %% on_class_loaded/1 raises.
            ?_test(begin
                OldVal = application:get_env(beamtalk_runtime, class_load_callback),
                Forms = [
                    {attribute, 1, module, bt1982_callback_raiser},
                    {attribute, 2, export, [{on_class_loaded, 1}]},
                    {function, 3, on_class_loaded, 1, [
                        {clause, 3, [{var, 3, '_'}], [], [
                            {call, 3, {atom, 3, error}, [{atom, 3, deliberate_failure}]}
                        ]}
                    ]}
                ],
                {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
                {module, Mod} = code:load_binary(
                    Mod, "bt1982_callback_raiser.erl", Bin
                ),
                application:set_env(
                    beamtalk_runtime, class_load_callback, Mod
                ),
                State = #{
                    className => 'BT1982CallbackRaises',
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                %% Registration still succeeds — the generic catch-all eats the error.
                {ok, ClassPid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(ClassPid)),

                %% Restore original env
                case OldVal of
                    undefined -> application:unset_env(beamtalk_runtime, class_load_callback);
                    {ok, V} -> application:set_env(beamtalk_runtime, class_load_callback, V)
                end,
                gen_server:stop(ClassPid, normal, 5000)
            end)
        ]
    end}.
