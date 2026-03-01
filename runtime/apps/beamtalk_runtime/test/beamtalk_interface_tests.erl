%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_interface module
%%%
%%% Tests the BeamtalkInterface actor functionality:
%%% - Basic lifecycle (start_link, init)
%%% - allClasses method
%%% - classNamed: method
%%% - globals method
%%% - version method
%%% - has_method/1 export
%%% - Error cases

-module(beamtalk_interface_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Start pg server if not already running
    case whereis(pg) of
        undefined ->
            {ok, _Pid} = pg:start_link(),
            ok;
        _Pid ->
            ok
    end,

    %% Register a few test classes for introspection
    register_test_classes().

teardown(_) ->
    %% Trap exits to prevent linked class process kills from
    %% terminating the EUnit fixture process.
    OldTrapExit = process_flag(trap_exit, true),
    cleanup_test_classes(),
    process_flag(trap_exit, OldTrapExit).

register_test_classes() ->
    %% Register Object class (base class)
    ObjectInfo = #{
        name => 'Object',
        module => beamtalk_object,
        superclass => none,
        instance_methods => #{},
        instance_variables => []
    },
    ensure_class_started('Object', ObjectInfo),

    %% Register Integer class
    IntegerInfo = #{
        name => 'Integer',
        module => 'bt@stdlib@integer',
        superclass => 'Object',
        instance_methods => #{
            '+' => #{arity => 1},
            '-' => #{arity => 1}
        },
        instance_variables => []
    },
    ensure_class_started('Integer', IntegerInfo),

    %% Register Actor class (superclass of BeamtalkInterface)
    ActorInfo = #{
        name => 'Actor',
        module => 'bt@stdlib@actor',
        superclass => 'Object',
        instance_methods => #{},
        instance_variables => []
    },
    ensure_class_started('Actor', ActorInfo),

    %% Register BeamtalkInterface class
    ensure_class_started('BeamtalkInterface', beamtalk_interface:class_info()),

    %% Register Counter class (example user class)
    CounterInfo = #{
        name => 'Counter',
        module => counter,
        superclass => 'Object',
        instance_methods => #{
            increment => #{arity => 0},
            getValue => #{arity => 0}
        },
        instance_variables => [value]
    },
    ensure_class_started('Counter', CounterInfo),
    ok.

%% Start a class, tolerating already-started (from other test modules).
ensure_class_started(ClassName, ClassInfo) ->
    case beamtalk_object_class:start_link(ClassName, ClassInfo) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

cleanup_test_classes() ->
    %% Get all class processes from pg group
    Members =
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end,

    %% Terminate each class process
    lists:foreach(
        fun
            (Pid) when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        MRef = monitor(process, Pid),
                        exit(Pid, kill),
                        receive
                            {'DOWN', MRef, process, Pid, _} -> ok
                        after 1000 ->
                            ok
                        end;
                    false ->
                        ok
                end;
            (_) ->
                ok
        end,
        Members
    ),

    %% Force unregister any stragglers
    lists:foreach(
        fun(Name) ->
            try
                unregister(Name)
            catch
                _:_ -> ok
            end
        end,
        [beamtalk_class_Object, beamtalk_class_Integer, beamtalk_class_Counter]
    ),
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_link_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                ?assert(is_pid(Pid)),
                ?assert(is_process_alive(Pid)),
                gen_server:stop(Pid)
            end)
        ]
    end}.

start_link_with_options_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Opts = [{version, <<"1.2.3">>}, {workspace, test_workspace}],
                {ok, Pid} = beamtalk_interface:start_link(Opts),
                ?assert(is_pid(Pid)),
                ?assert(is_process_alive(Pid)),

                %% Verify version was set
                Version = gen_server:call(Pid, {version, []}),
                ?assertEqual(<<"1.2.3">>, Version),

                gen_server:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% has_method/1 Tests
%%====================================================================

has_method_supported_test() ->
    %% Verify has_method returns true for supported selectors
    ?assertEqual(true, beamtalk_interface:has_method(allClasses)),
    ?assertEqual(true, beamtalk_interface:has_method('classNamed:')),
    ?assertEqual(true, beamtalk_interface:has_method(globals)),
    ?assertEqual(true, beamtalk_interface:has_method(version)).

has_method_unsupported_test() ->
    %% Verify has_method returns false for unsupported selectors
    ?assertEqual(false, beamtalk_interface:has_method(unknown)),
    ?assertEqual(false, beamtalk_interface:has_method(spawn)),
    ?assertEqual(false, beamtalk_interface:has_method('show:')).

%%====================================================================
%% allClasses Tests
%%====================================================================

all_classes_returns_list_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Classes = gen_server:call(Pid, {allClasses, []}),
                ?assert(is_list(Classes)),
                ?assert(length(Classes) > 0),
                gen_server:stop(Pid)
            end)
        ]
    end}.

all_classes_includes_core_classes_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Classes = gen_server:call(Pid, {allClasses, []}),

                %% Verify core classes are included
                ?assert(lists:member('Object', Classes)),
                ?assert(lists:member('Integer', Classes)),
                ?assert(lists:member('Counter', Classes)),

                gen_server:stop(Pid)
            end)
        ]
    end}.

all_classes_returns_atoms_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Classes = gen_server:call(Pid, {allClasses, []}),

                %% Verify all elements are atoms
                lists:foreach(
                    fun(Class) ->
                        ?assert(is_atom(Class))
                    end,
                    Classes
                ),

                gen_server:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% classNamed: Tests
%%====================================================================

class_named_known_class_atom_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, {'classNamed:', ['Counter']}),

                %% BT-374: Returns beamtalk_object tuple wrapping the class
                ?assertMatch({beamtalk_object, 'Counter class', _, _}, Result),
                {beamtalk_object, _, _, ClassPid} = Result,
                ?assert(is_pid(ClassPid)),
                ?assert(is_process_alive(ClassPid)),

                %% Verify it's the Counter class
                ClassName = beamtalk_object_class:class_name(ClassPid),
                ?assertEqual('Counter', ClassName),

                gen_server:stop(Pid)
            end)
        ]
    end}.

class_named_known_class_binary_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, {'classNamed:', [<<"Counter">>]}),

                %% BT-374: Returns beamtalk_object tuple wrapping the class
                ?assertMatch({beamtalk_object, 'Counter class', _, _}, Result),
                {beamtalk_object, _, _, ClassPid} = Result,
                ?assert(is_pid(ClassPid)),
                ?assert(is_process_alive(ClassPid)),

                %% Verify it's the Counter class
                ClassName = beamtalk_object_class:class_name(ClassPid),
                ?assertEqual('Counter', ClassName),

                gen_server:stop(Pid)
            end)
        ]
    end}.

class_named_unknown_class_atom_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, {'classNamed:', ['UnknownClass']}),

                %% BT-374: Returns nil for unknown classes
                ?assertEqual(nil, Result),

                gen_server:stop(Pid)
            end)
        ]
    end}.

class_named_unknown_class_binary_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, {'classNamed:', [<<"UnknownClass">>]}),

                %% BT-374: Returns nil for unknown classes (binary doesn't exist as atom)
                ?assertEqual(nil, Result),

                gen_server:stop(Pid)
            end)
        ]
    end}.

class_named_invalid_type_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, {'classNamed:', [42]}),

                %% Should return a type error for non-atom/binary input
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result),

                gen_server:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% globals Tests
%%====================================================================

globals_returns_map_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Globals = gen_server:call(Pid, {globals, []}),

                %% Should return a non-empty map (class registry snapshot)
                ?assert(is_map(Globals)),
                ?assert(map_size(Globals) > 0),

                %% Values should be beamtalk_object tuples with Symbol (atom) keys
                maps:foreach(
                    fun(Key, Val) ->
                        ?assert(is_atom(Key)),
                        ?assertMatch({beamtalk_object, _, _, _}, Val)
                    end,
                    Globals
                ),

                gen_server:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% version Tests
%%====================================================================

version_returns_binary_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Version = gen_server:call(Pid, {version, []}),

                %% Should return a binary string
                ?assert(is_binary(Version)),
                ?assert(byte_size(Version) > 0),

                gen_server:stop(Pid)
            end)
        ]
    end}.

version_default_value_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Version = gen_server:call(Pid, {version, []}),

                %% Default version comes from OTP app vsn
                ?assert(is_binary(Version)),
                ?assert(byte_size(Version) > 0),

                gen_server:stop(Pid)
            end)
        ]
    end}.

version_custom_value_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link([{version, <<"2.0.0">>}]),
                Version = gen_server:call(Pid, {version, []}),

                %% Custom version should be returned
                ?assertEqual(<<"2.0.0">>, Version),

                gen_server:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

unknown_selector_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, {unknownMethod, []}),

                %% Should return a structured error
                ?assertMatch(
                    {error, #beamtalk_error{kind = does_not_understand, selector = unknownMethod}},
                    Result
                ),

                gen_server:stop(Pid)
            end)
        ]
    end}.

malformed_call_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),
                Result = gen_server:call(Pid, malformed_message),

                %% Should return a structured error
                ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),

                gen_server:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% Cast/Info Handling Tests
%%====================================================================

unexpected_cast_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),

                %% Send a cast (should be logged but not crash)
                ok = gen_server:cast(Pid, unexpected_cast_message),

                %% Use a synchronous call as barrier to ensure cast was processed
                _ = gen_server:call(Pid, {version, []}),
                ?assert(is_process_alive(Pid)),

                gen_server:stop(Pid)
            end)
        ]
    end}.

info_message_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_interface:start_link(),

                %% Send an info message (should be logged but not crash)
                Pid ! unexpected_info_message,

                %% Use a synchronous call as barrier to ensure info was processed
                _ = gen_server:call(Pid, {version, []}),
                ?assert(is_process_alive(Pid)),

                gen_server:stop(Pid)
            end)
        ]
    end}.
