%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_system_dictionary module
%%%
%%% Tests the SystemDictionary actor functionality:
%%% - Basic lifecycle (start_link, init)
%%% - allClasses method
%%% - classNamed: method
%%% - globals method
%%% - version method
%%% - has_method/1 export
%%% - Error cases

-module(beamtalk_system_dictionary_tests).
-include_lib("eunit/include/eunit.hrl").

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
    %% Clean up test classes
    cleanup_test_classes().

register_test_classes() ->
    %% Register Object class (base class)
    ObjectInfo = #{
        name => 'Object',
        module => beamtalk_object,
        superclass => none,
        instance_methods => #{},
        instance_variables => []
    },
    {ok, _} = beamtalk_object_class:start_link('Object', ObjectInfo),
    
    %% Register Integer class
    IntegerInfo = #{
        name => 'Integer',
        module => beamtalk_integer,
        superclass => 'Object',
        instance_methods => #{
            '+' => #{arity => 1},
            '-' => #{arity => 1}
        },
        instance_variables => []
    },
    {ok, _} = beamtalk_object_class:start_link('Integer', IntegerInfo),
    
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
    {ok, _} = beamtalk_object_class:start_link('Counter', CounterInfo),
    ok.

cleanup_test_classes() ->
    %% Get all class processes from pg group
    Members = try
        pg:get_members(beamtalk_classes)
    catch
        _:_ -> []
    end,
    
    %% Terminate each class process
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
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
            try unregister(Name) catch _:_ -> ok end
        end,
        [beamtalk_class_Object, beamtalk_class_Integer, beamtalk_class_Counter]
    ),
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_link_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     ?assert(is_pid(Pid)),
                     ?assert(is_process_alive(Pid)),
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

start_link_with_options_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     Opts = [{version, <<"1.2.3">>}, {workspace, test_workspace}],
                     {ok, Pid} = beamtalk_system_dictionary:start_link(Opts),
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
    ?assertEqual(true, beamtalk_system_dictionary:has_method(allClasses)),
    ?assertEqual(true, beamtalk_system_dictionary:has_method('classNamed:')),
    ?assertEqual(true, beamtalk_system_dictionary:has_method(globals)),
    ?assertEqual(true, beamtalk_system_dictionary:has_method(version)).

has_method_unsupported_test() ->
    %% Verify has_method returns false for unsupported selectors
    ?assertEqual(false, beamtalk_system_dictionary:has_method(unknown)),
    ?assertEqual(false, beamtalk_system_dictionary:has_method(spawn)),
    ?assertEqual(false, beamtalk_system_dictionary:has_method('show:')).

%%====================================================================
%% allClasses Tests
%%====================================================================

all_classes_returns_list_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Classes = gen_server:call(Pid, {allClasses, []}),
                     ?assert(is_list(Classes)),
                     ?assert(length(Classes) > 0),
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

all_classes_includes_core_classes_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
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
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
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
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Result = gen_server:call(Pid, {'classNamed:', ['Counter']}),
                     
                     %% Should return a pid
                     ?assert(is_pid(Result)),
                     ?assert(is_process_alive(Result)),
                     
                     %% Verify it's the Counter class
                     ClassName = beamtalk_object_class:class_name(Result),
                     ?assertEqual('Counter', ClassName),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

class_named_known_class_binary_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Result = gen_server:call(Pid, {'classNamed:', [<<"Counter">>]}),
                     
                     %% Should return a pid
                     ?assert(is_pid(Result)),
                     ?assert(is_process_alive(Result)),
                     
                     %% Verify it's the Counter class
                     ClassName = beamtalk_object_class:class_name(Result),
                     ?assertEqual('Counter', ClassName),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

class_named_unknown_class_atom_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Result = gen_server:call(Pid, {'classNamed:', ['UnknownClass']}),
                     
                     %% Should return an error
                     ?assertMatch({error, {class_not_found, 'UnknownClass'}}, Result),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

class_named_unknown_class_binary_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Result = gen_server:call(Pid, {'classNamed:', [<<"UnknownClass">>]}),
                     
                     %% Should return an error (binary doesn't exist as atom)
                     ?assertMatch({error, {class_not_found, <<"UnknownClass">>}}, Result),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

%%====================================================================
%% globals Tests
%%====================================================================

globals_returns_map_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Globals = gen_server:call(Pid, {globals, []}),
                     
                     %% Should return a map (empty in Phase 1)
                     ?assert(is_map(Globals)),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

%%====================================================================
%% version Tests
%%====================================================================

version_returns_binary_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Version = gen_server:call(Pid, {version, []}),
                     
                     %% Should return a binary string
                     ?assert(is_binary(Version)),
                     ?assert(byte_size(Version) > 0),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

version_default_value_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Version = gen_server:call(Pid, {version, []}),
                     
                     %% Default version should be 0.1.0-dev
                     ?assertEqual(<<"0.1.0-dev">>, Version),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

version_custom_value_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link([{version, <<"2.0.0">>}]),
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
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Result = gen_server:call(Pid, {unknownMethod, []}),
                     
                     %% Should return an error
                     ?assertMatch({error, {unknown_selector, unknownMethod}}, Result),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

malformed_call_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     Result = gen_server:call(Pid, malformed_message),
                     
                     %% Should return an error
                     ?assertMatch({error, {unknown_call_format, malformed_message}}, Result),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

%%====================================================================
%% Cast/Info Handling Tests
%%====================================================================

unexpected_cast_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     
                     %% Send a cast (should be logged but not crash)
                     ok = gen_server:cast(Pid, unexpected_cast_message),
                     
                     %% Process should still be alive
                     timer:sleep(50),  %% Give it time to process
                     ?assert(is_process_alive(Pid)),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.

info_message_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, Pid} = beamtalk_system_dictionary:start_link(),
                     
                     %% Send an info message (should be logged but not crash)
                     Pid ! unexpected_info_message,
                     
                     %% Process should still be alive
                     timer:sleep(50),  %% Give it time to process
                     ?assert(is_process_alive(Pid)),
                     
                     gen_server:stop(Pid)
                 end)
         ]
     end}.
