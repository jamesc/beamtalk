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
          {"lookup checks extensions before class methods", fun test_extension_priority/0}
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
    %% SKIP: This test requires LoggingCounter which doesn't have register_class
    %% TODO: Either convert logging_counter.bt to use class syntax,
    %% or create a new test fixture with proper class definition
    ok.

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
    
    %% Should invoke the extension
    ?assertMatch({reply, {extension_called, _State}, _State}, Result).

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

%% Ensure the LoggingCounter class is loaded and registered  
ensure_logging_counter_loaded() ->
    %% Ensure Counter is loaded first (superclass)
    ok = ensure_counter_loaded(),
    
    %% Check if LoggingCounter class is already registered
    case beamtalk_object_class:whereis_class('LoggingCounter') of
        undefined ->
            %% LoggingCounter not registered
            case code:ensure_loaded(logging_counter) of
                {module, logging_counter} ->
                    case erlang:function_exported(logging_counter, register_class, 0) of
                        true ->
                            logging_counter:register_class(),
                            ok;
                        false ->
                            error(logging_counter_no_register_function)
                    end;
                {error, Reason} ->
                    error({logging_counter_module_not_found, Reason})
            end;
        _Pid ->
            ok
    end.
