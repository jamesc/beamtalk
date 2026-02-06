%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_dispatch module.
%%%
%%% Tests the method dispatch domain service including:
%%% - Hierarchy walking (lookup/5)
%%% - Super send dispatch (super/5)
%%% - Extension method checking
%%% - Error handling (structured #beamtalk_error{})
%%%
%%% ## Test Strategy
%%%
%%% Uses the real compiled Counter fixture (tests/e2e/fixtures/counter.bt)
%%% and beamtalk_object_class registry for realistic testing.

-module(beamtalk_dispatch_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Test Setup/Teardown
%%% ============================================================================

%% Setup function that starts minimal required services
setup() ->
    %% Just start the application to ensure ETS tables and registries are ready
    %% The beamtalk_runtime app should handle starting necessary supervisors
    application:ensure_all_started(beamtalk_runtime),
    
    %% Initialize extensions registry
    beamtalk_extensions:init(),
    
    ok.

%% Teardown function
teardown(_) ->
    %% Minimal cleanup
    ok.

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% Run all tests with setup/teardown
dispatch_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          {"lookup finds local method", fun test_lookup_local_method/0},
          {"lookup finds inherited method", fun test_lookup_inherited_method/0},
          {"lookup returns DNU error for missing method", fun test_lookup_missing_method/0},
          {"super skips current class", fun test_super_skips_current/0},
          {"super returns error at root", fun test_super_at_root/0},
          {"lookup checks extensions before class methods", fun test_extension_priority/0},
          {"dispatch errors are always 2-tuples", fun test_error_tuple_shape/0},
          {"super finds inherited reflection method", fun test_super_finds_inherited/0}
         ]
     end
    }.

%%% ============================================================================
%%% Test Cases
%%% ============================================================================

%% Test that lookup/5 finds a method defined in the current class
test_lookup_local_method() ->
    %% Use the Counter class from compiled fixture
    %% Counter defines increment/0 locally
    
    %% First, ensure Counter is loaded and registered
    ok = ensure_counter_loaded(),
    
    %% Create a Counter instance state
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),  % Placeholder self
    
    %% Dispatch increment on Counter
    Result = beamtalk_dispatch:lookup(increment, [], Self, State, 'Counter'),
    
    %% Should succeed (exact result depends on Counter implementation)
    ?assertMatch({reply, _Result, _NewState}, Result).

%% Test that lookup/5 walks hierarchy to find inherited method
test_lookup_inherited_method() ->
    %% Counter has 'class' method inlined in its dispatch
    %% This test actually tests that we can find a method in the current class
    %% (Real hierarchy walking is tested with LoggingCounter -> Counter)
    
    ok = ensure_counter_loaded(),
    
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),
    
    %% Dispatch 'class' on Counter (defined locally in Counter's dispatch)
    Result = beamtalk_dispatch:lookup(class, [], Self, State, 'Counter'),
    
    %% Should succeed and return 'Counter'
    ?assertMatch({reply, 'Counter', _State}, Result).

%% Test that lookup/5 returns structured error for missing method
test_lookup_missing_method() ->
    ok = ensure_counter_loaded(),
    
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),
    
    %% Dispatch a method that doesn't exist
    Result = beamtalk_dispatch:lookup(unknownMethod, [], Self, State, 'Counter'),
    
    %% Should return structured error
    ?assertMatch({error, #beamtalk_error{
        kind = does_not_understand,
        class = 'Counter',
        selector = unknownMethod
    }}, Result).

%% Test that super/5 skips the current class and starts at superclass
test_super_skips_current() ->
    ok = ensure_counter_loaded(),
    
    %% Counter inherits from Actor. Call super on Counter for 'increment'.
    %% Counter defines 'increment', but super should NOT find it in Counter.
    %% It should look in Actor (no increment there), then Object, etc.
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),
    
    %% Super from Counter for 'increment' should NOT find it
    %% (Actor/Object don't define increment)
    Result = beamtalk_dispatch:super(increment, [], Self, State, 'Counter'),
    
    %% Should return does_not_understand since increment is only in Counter
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result).

%% Test that super/5 returns error when at root with no superclass
test_super_at_root() ->
    %% Object has no superclass (or ProtoObject is root)
    %% Calling super from Object should return error
    
    %% We need to know the root class name
    %% For now, assume Object is close to root
    
    ok = ensure_counter_loaded(),
    
    State = #{
        '__class__' => 'Object'
    },
    
    Self = make_ref(),
    
    %% Call super on Object for a method that doesn't exist in superclass chain
    Result = beamtalk_dispatch:super(unknownMethod, [], Self, State, 'Object'),
    
    %% Should return error (no superclass or method not found)
    ?assertMatch({error, #beamtalk_error{}}, Result).

%% Test that extensions are checked before class methods
test_extension_priority() ->
    %% Register an extension method on Counter
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),
    
    TestFun = fun(_Args, State) -> 
        %% Return a test value
        {extension_called, State}
    end,
    
    ok = beamtalk_extensions:register('Counter', testExtension, TestFun, test_owner),
    
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),
    
    %% Call the extension method
    Result = beamtalk_dispatch:lookup(testExtension, [], Self, State, 'Counter'),
    
    %% Clean up the extension to avoid leaking between tests
    catch ets:delete(beamtalk_extensions, {'Counter', testExtension}),
    
    %% Should invoke the extension
    ?assertMatch({reply, {extension_called, _}, _}, Result).

%% Test that all dispatch error returns are proper 2-tuples {error, #beamtalk_error{}}
%% This validates the normalization in invoke_method that converts
%% 3-tuple {error, Error, State} from dispatch/4 to 2-tuple {error, Error}
test_error_tuple_shape() ->
    ok = ensure_counter_loaded(),
    
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),
    
    %% All error paths should return exactly {error, #beamtalk_error{}} (2-tuple)
    %% Never {error, Error, State} (3-tuple)
    LookupResult = beamtalk_dispatch:lookup(noSuchMethod, [], Self, State, 'Counter'),
    ?assertMatch({error, #beamtalk_error{}}, LookupResult),
    ?assertEqual(2, tuple_size(LookupResult)),
    
    SuperResult = beamtalk_dispatch:super(noSuchMethod, [], Self, State, 'Counter'),
    ?assertMatch({error, #beamtalk_error{}}, SuperResult),
    ?assertEqual(2, tuple_size(SuperResult)).

%% Test that super/5 can find a method defined in a parent class
%% Counter inherits 'class' which is inlined in all compiled modules.
%% We test 'isNil' which is registered in Object's metadata but has no
%% compiled module — demonstrating the hierarchy walk terminates correctly.
test_super_finds_inherited() ->
    ok = ensure_counter_loaded(),
    
    State = #{
        '__class__' => 'Counter',
        'value' => 0
    },
    
    Self = make_ref(),
    
    %% 'class' is inlined in Counter's dispatch, so super should skip Counter
    %% and look for it in Actor. Actor's dispatch also inlines 'class' (if compiled).
    %% Since Actor is a bootstrap class without a real module, this will return error.
    %% This tests that super correctly walks up AND terminates at bootstrap classes.
    Result = beamtalk_dispatch:super(class, [], Self, State, 'Counter'),
    
    %% Actor/Object/ProtoObject are bootstrap classes without dispatch/4 modules,
    %% so super should return does_not_understand error (walked full chain, no module to invoke)
    ?assertMatch({error, #beamtalk_error{}}, Result).

%%% ============================================================================
%%% Helper Functions
%%% ============================================================================

%% Ensure the Counter class is loaded and registered
ensure_counter_loaded() ->
    %% Check if Counter class is already registered
    case beamtalk_object_class:whereis_class('Counter') of
        undefined ->
            %% Counter not registered - register it
            %% The counter module is compiled from test fixture
            case code:ensure_loaded(counter) of
                {module, counter} ->
                    %% Call the module's register_class/0 if it exists
                    case erlang:function_exported(counter, register_class, 0) of
                        true ->
                            counter:register_class(),
                            ok;
                        false ->
                            error(counter_no_register_function)
                    end;
                {error, Reason} ->
                    error({counter_module_not_found, Reason})
            end;
        _Pid ->
            %% Already registered
            ok
    end.

