%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for dynamic class creation (BT-219 Phase 1)

-module(beamtalk_dynamic_class_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    %% Ensure beamtalk_runtime application is started
    application:ensure_all_started(beamtalk_runtime),
    
    %% Ensure bootstrap has run (base classes are registered)
    case whereis(beamtalk_bootstrap) of
        undefined ->
            %% Start bootstrap manually
            case beamtalk_bootstrap:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _ ->
            ok
    end,
    
    %% Wait for Actor class to be registered (may take a moment)
    wait_for_actor_class(100).

wait_for_actor_class(0) ->
    error(actor_class_not_registered_after_timeout);
wait_for_actor_class(N) ->
    case beamtalk_class:whereis_class('Actor') of
        undefined ->
            timer:sleep(50),
            wait_for_actor_class(N - 1);
        _Pid ->
            ok
    end.

%%====================================================================
%% Tests
%%====================================================================

%% Test creating a dynamic class and verifying registration
create_dynamic_class_test() ->
    setup(),
    
    %% Create a simple dynamic class with one method
    Result = beamtalk_class:create_subclass('Actor', 'TestDynamicClass', #{
        instance_variables => [count],
        instance_methods => #{
            increment => fun(_Self, [], State) ->
                Count = maps:get(count, State, 0),
                NewCount = Count + 1,
                {reply, NewCount, maps:put(count, NewCount, State)}
            end
        }
    }),
    
    ?assertMatch({ok, _Pid}, Result),
    {ok, ClassPid} = Result,
    
    %% Verify class is registered
    ?assertEqual(ClassPid, beamtalk_class:whereis_class('TestDynamicClass')),
    
    %% Verify class metadata
    ?assertEqual('TestDynamicClass', beamtalk_class:class_name(ClassPid)),
    ?assertEqual('Actor', beamtalk_class:superclass(ClassPid)),
    ?assertEqual([count], beamtalk_class:instance_variables(ClassPid)),
    ?assertEqual([increment], beamtalk_class:methods(ClassPid)).

%% Test spawning a dynamic object instance and calling methods
spawn_dynamic_instance_test() ->
    setup(),
    
    %% Create dynamic class
    {ok, ClassPid} = beamtalk_class:create_subclass('Actor', 'Counter', #{
        instance_variables => [value],
        instance_methods => #{
            increment => fun(_Self, [], State) ->
                Value = maps:get(value, State, 0),
                NewValue = Value + 1,
                {reply, NewValue, maps:put(value, NewValue, State)}
            end,
            getValue => fun(_Self, [], State) ->
                Value = maps:get(value, State, 0),
                {reply, Value, State}
            end
        }
    }),
    
    %% Spawn instance with initial state
    {ok, #beamtalk_object{pid = InstancePid}} = beamtalk_class:new(ClassPid, [#{value => 5}]),
    
    ?assert(is_pid(InstancePid)),
    ?assert(erlang:is_process_alive(InstancePid)),
    
    %% Call getValue - should return 5
    ?assertEqual(5, gen_server:call(InstancePid, {getValue, []})),
    
    %% Call increment - should return 6 and update state
    ?assertEqual(6, gen_server:call(InstancePid, {increment, []})),
    
    %% Call getValue again - should return 6
    ?assertEqual(6, gen_server:call(InstancePid, {getValue, []})).

%% Test method not found error handling
method_not_found_test() ->
    setup(),
    
    %% Create class with one method
    {ok, ClassPid} = beamtalk_class:create_subclass('Actor', 'Simple', #{
        instance_variables => [],
        instance_methods => #{
            foo => fun(_Self, [], State) ->
                {reply, ok, State}
            end
        }
    }),
    
    %% Spawn instance
    {ok, #beamtalk_object{pid = InstancePid}} = beamtalk_class:new(ClassPid, [#{}]),
    
    %% Call non-existent method
    Result = gen_server:call(InstancePid, {bar, []}),
    ?assertMatch({error, {method_not_found, bar}}, Result).

%% Test field initialization
field_initialization_test() ->
    setup(),
    
    %% Create class with multiple instance variables
    {ok, ClassPid} = beamtalk_class:create_subclass('Actor', 'Point', #{
        instance_variables => [x, y],
        instance_methods => #{
            getX => fun(_Self, [], State) ->
                {reply, maps:get(x, State, 0), State}
            end,
            getY => fun(_Self, [], State) ->
                {reply, maps:get(y, State, 0), State}
            end
        }
    }),
    
    %% Spawn with initial values
    {ok, #beamtalk_object{pid = InstancePid}} = beamtalk_class:new(ClassPid, [#{x => 10, y => 20}]),
    
    %% Verify field values
    ?assertEqual(10, gen_server:call(InstancePid, {getX, []})),
    ?assertEqual(20, gen_server:call(InstancePid, {getY, []})).

%% Test dynamic subclass inherits superclass methods (via class hierarchy)
%% Note: Method inheritance is handled by beamtalk_class lookup, not tested here
%% This test verifies the class hierarchy is correctly set up
dynamic_subclass_hierarchy_test() ->
    setup(),
    
    %% Create parent dynamic class
    {ok, ParentPid} = beamtalk_class:create_subclass('Actor', 'Parent', #{
        instance_variables => [],
        instance_methods => #{
            parentMethod => fun(_Self, [], State) ->
                {reply, from_parent, State}
            end
        }
    }),
    
    %% Create child dynamic class
    {ok, ChildPid} = beamtalk_class:create_subclass('Parent', 'Child', #{
        instance_variables => [],
        instance_methods => #{
            childMethod => fun(_Self, [], State) ->
                {reply, from_child, State}
            end
        }
    }),
    
    %% Verify hierarchy
    ?assertEqual('Actor', beamtalk_class:superclass(ParentPid)),
    ?assertEqual('Parent', beamtalk_class:superclass(ChildPid)),
    
    %% Verify class names
    ?assertEqual('Parent', beamtalk_class:class_name(ParentPid)),
    ?assertEqual('Child', beamtalk_class:class_name(ChildPid)).

%% Test error when superclass doesn't exist
superclass_not_found_test() ->
    setup(),
    
    Result = beamtalk_class:create_subclass('NonExistentClass', 'MyClass', #{
        instance_variables => [],
        instance_methods => #{}
    }),
    
    ?assertMatch({error, {superclass_not_found, 'NonExistentClass'}}, Result).

%% Test creating class with no instance variables
no_instance_variables_test() ->
    setup(),
    
    {ok, ClassPid} = beamtalk_class:create_subclass('Actor', 'Stateless', #{
        instance_methods => #{
            hello => fun(_Self, [], State) ->
                {reply, world, State}
            end
        }
    }),
    
    ?assertEqual([], beamtalk_class:instance_variables(ClassPid)),
    
    %% Spawn and call method
    {ok, #beamtalk_object{pid = InstancePid}} = beamtalk_class:new(ClassPid, [#{}]),
    ?assertEqual(world, gen_server:call(InstancePid, {hello, []})).

%% Test async message send with futures
async_message_test() ->
    setup(),
    
    %% Create class
    {ok, ClassPid} = beamtalk_class:create_subclass('Actor', 'AsyncTest', #{
        instance_variables => [value],
        instance_methods => #{
            compute => fun(_Self, [X], State) ->
                %% Simulate async computation
                Result = X * 2,
                {reply, Result, State}
            end
        }
    }),
    
    %% Spawn instance
    {ok, #beamtalk_object{pid = InstancePid}} = beamtalk_class:new(ClassPid, [#{value => 0}]),
    
    %% Send async message
    FuturePid = beamtalk_future:new(),
    gen_server:cast(InstancePid, {compute, [5], FuturePid}),
    
    %% Await result
    Result = beamtalk_future:await(FuturePid),
    ?assertEqual(10, Result).

%% Test REPL-style usage (documenting the API for interactive use)
repl_style_usage_test() ->
    setup(),
    
    %% This test demonstrates how to use dynamic classes from the REPL
    %% Users would type these expressions interactively
    
    %% Step 1: Create a dynamic class (use unique name to avoid collisions)
    ClassPid = element(2, beamtalk_class:create_subclass('Actor', 'REPLCounter', #{
        instance_variables => [count],
        instance_methods => #{
            increment => fun(_Self, [], State) ->
                Count = maps:get(count, State, 0),
                {reply, Count + 1, maps:put(count, Count + 1, State)}
            end,
            getValue => fun(_Self, [], State) ->
                Count = maps:get(count, State, 0),
                {reply, Count, State}
            end
        }
    })),
    
    %% Step 2: Spawn an instance
    CounterObj = element(2, beamtalk_class:new(ClassPid, [#{count => 0}])),
    CounterPid = CounterObj#beamtalk_object.pid,
    
    %% Verify class was registered correctly
    ?assertEqual([getValue, increment], lists:sort(beamtalk_class:methods(ClassPid))),
    
    %% Step 3: Send messages to the instance
    ?assertEqual(0, gen_server:call(CounterPid, {getValue, []})),
    ?assertEqual(1, gen_server:call(CounterPid, {increment, []})),
    ?assertEqual(1, gen_server:call(CounterPid, {getValue, []})),
    ?assertEqual(2, gen_server:call(CounterPid, {increment, []})),
    ?assertEqual(2, gen_server:call(CounterPid, {getValue, []})).
    
%% Test dynamic class lookup and introspection
class_introspection_test() ->
    setup(),
    
    %% Create a dynamic class
    {ok, ClassPid} = beamtalk_class:create_subclass('Actor', 'TestClass', #{
        instance_variables => [x, y, z],
        instance_methods => #{
            foo => fun(_Self, [], State) -> {reply, bar, State} end,
            baz => fun(_Self, [N], State) -> {reply, N * 2, State} end
        }
    }),
    
    %% Test class lookup
    ?assertEqual(ClassPid, beamtalk_class:whereis_class('TestClass')),
    
    %% Test class introspection
    ?assertEqual('TestClass', beamtalk_class:class_name(ClassPid)),
    ?assertEqual('Actor', beamtalk_class:superclass(ClassPid)),
    ?assertEqual([x, y, z], beamtalk_class:instance_variables(ClassPid)),
    
    %% Test method enumeration (order may vary)
    Methods = beamtalk_class:methods(ClassPid),
    ?assert(lists:member(foo, Methods)),
    ?assert(lists:member(baz, Methods)),
    ?assertEqual(2, length(Methods)).
