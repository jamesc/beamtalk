%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_dynamic_object module (BT-346).
%%%
%%% Tests dynamic object dispatch, state management, and error handling.

-module(beamtalk_dynamic_object_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

%% Build an init state map with methods and optional fields
test_init_state(ClassName, Methods) ->
    test_init_state(ClassName, Methods, #{}).

test_init_state(ClassName, Methods, ExtraFields) ->
    %% __class_pid__ requires a real pid — use self() as placeholder
    Base = #{
        '__class__' => ClassName,
        '__methods__' => Methods,
        '__class_pid__' => self()
    },
    maps:merge(Base, ExtraFields).

%%====================================================================
%% init tests
%%====================================================================

init_sets_class_name_test() ->
    InitState = test_init_state('TestClass', #{}),
    {ok, _State} = beamtalk_dynamic_object:init({'TestClass', InitState}).

%%====================================================================
%% dispatch/4 tests (direct function calls, no gen_server)
%%====================================================================

dispatch_calls_method_closure_test() ->
    Methods = #{
        'getValue' => fun(_Self, [], State) ->
            {reply, maps:get(value, State, 0), State}
        end
    },
    Fields = test_init_state('TestClass', Methods, #{value => 42}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    ?assertEqual({reply, 42, Fields}, beamtalk_dynamic_object:dispatch('getValue', [], Self, Fields)).

dispatch_updates_state_test() ->
    Methods = #{
        'increment' => fun(_Self, [], State) ->
            V = maps:get(value, State, 0),
            NewState = maps:put(value, V + 1, State),
            {reply, V + 1, NewState}
        end
    },
    Fields = test_init_state('TestClass', Methods, #{value => 10}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {reply, 11, NewFields} = beamtalk_dynamic_object:dispatch('increment', [], Self, Fields),
    ?assertEqual(11, maps:get(value, NewFields)).

dispatch_with_args_test() ->
    Methods = #{
        'add:' => fun(_Self, [X], State) ->
            V = maps:get(value, State, 0),
            NewState = maps:put(value, V + X, State),
            {reply, V + X, NewState}
        end
    },
    Fields = test_init_state('TestClass', Methods, #{value => 5}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {reply, 8, _NewFields} = beamtalk_dynamic_object:dispatch('add:', [3], Self, Fields),
    ok.

dispatch_noreply_method_test() ->
    Methods = #{
        'reset' => fun(_Self, [], State) ->
            {noreply, maps:put(value, 0, State)}
        end
    },
    Fields = test_init_state('TestClass', Methods, #{value => 99}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {noreply, NewFields} = beamtalk_dynamic_object:dispatch('reset', [], Self, Fields),
    ?assertEqual(0, maps:get(value, NewFields)).

%%====================================================================
%% Method not found / doesNotUnderstand tests
%%====================================================================

dispatch_unknown_method_returns_error_test() ->
    Fields = test_init_state('TestClass', #{}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {error, Error} = beamtalk_dynamic_object:dispatch('unknown', [], Self, Fields),
    ?assertMatch(#beamtalk_error{kind = does_not_understand, class = 'TestClass'}, Error).

dispatch_unknown_method_error_has_selector_test() ->
    Fields = test_init_state('TestClass', #{}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {error, #beamtalk_error{selector = Sel}} = beamtalk_dynamic_object:dispatch('missing', [], Self, Fields),
    ?assertEqual('missing', Sel).

dispatch_unknown_method_error_has_hint_test() ->
    Fields = test_init_state('TestClass', #{}),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {error, #beamtalk_error{hint = Hint}} = beamtalk_dynamic_object:dispatch('missing', [], Self, Fields),
    ?assert(is_binary(Hint)).

dispatch_does_not_understand_handler_test() ->
    Methods = #{
        'doesNotUnderstand:args:' => fun(_Self, [Sel, _Args], State) ->
            {reply, {caught, Sel}, State}
        end
    },
    Fields = test_init_state('TestClass', Methods),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    ?assertEqual({reply, {caught, 'foo'}, Fields},
                 beamtalk_dynamic_object:dispatch('foo', [], Self, Fields)).

%%====================================================================
%% Method exception handling tests
%%====================================================================

dispatch_method_exception_returns_error_test() ->
    Methods = #{
        'crasher' => fun(_Self, [], _State) ->
            error(intentional_crash)
        end
    },
    Fields = test_init_state('TestClass', Methods),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {error, Error} = beamtalk_dynamic_object:dispatch('crasher', [], Self, Fields),
    ?assertMatch(#beamtalk_error{kind = type_error, class = 'TestClass'}, Error).

dispatch_dnu_handler_exception_returns_error_test() ->
    Methods = #{
        'doesNotUnderstand:args:' => fun(_Self, _MsgArgs, _State) ->
            error(dnu_crash)
        end
    },
    Fields = test_init_state('TestClass', Methods),
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {error, Error} = beamtalk_dynamic_object:dispatch('missingMethod', [], Self, Fields),
    ?assertMatch(#beamtalk_error{kind = type_error}, Error).

%%====================================================================
%% gen_server callback tests (via running process)
%%====================================================================

handle_call_dispatches_method_test() ->
    Methods = #{
        'getValue' => fun(_Self, [], State) ->
            {reply, maps:get(value, State, 0), State}
        end
    },
    InitState = test_init_state('TestClass', Methods, #{value => 42}),
    {ok, Pid} = beamtalk_dynamic_object:start_link('TestClass', InitState),
    Result = gen_server:call(Pid, {'getValue', []}),
    ?assertEqual(42, Result),
    gen_server:stop(Pid).

handle_call_unknown_message_test() ->
    InitState = test_init_state('TestClass', #{}),
    {ok, Pid} = beamtalk_dynamic_object:start_link('TestClass', InitState),
    Result = gen_server:call(Pid, unexpected_message),
    ?assertEqual({error, unknown_message}, Result),
    gen_server:stop(Pid).

handle_call_method_not_found_test() ->
    InitState = test_init_state('TestClass', #{}),
    {ok, Pid} = beamtalk_dynamic_object:start_link('TestClass', InitState),
    Result = gen_server:call(Pid, {'nonexistent', []}),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),
    gen_server:stop(Pid).

handle_cast_unknown_message_test() ->
    InitState = test_init_state('TestClass', #{}),
    {ok, Pid} = beamtalk_dynamic_object:start_link('TestClass', InitState),
    gen_server:cast(Pid, unknown_cast),
    timer:sleep(20),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

handle_info_unknown_message_test() ->
    InitState = test_init_state('TestClass', #{}),
    {ok, Pid} = beamtalk_dynamic_object:start_link('TestClass', InitState),
    Pid ! random_info,
    timer:sleep(20),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

terminate_returns_ok_test() ->
    InitState = test_init_state('TestClass', #{}),
    {ok, Pid} = beamtalk_dynamic_object:start_link('TestClass', InitState),
    gen_server:stop(Pid).

%%====================================================================
%% State field access tests
%%====================================================================

dispatch_empty_methods_map_test() ->
    %% When __methods__ key is missing, should default to empty map
    Fields = #{
        '__class__' => 'TestClass',
        '__class_pid__' => self()
    },
    Self = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_dynamic_object, pid = self()},
    {error, _} = beamtalk_dynamic_object:dispatch('anything', [], Self, Fields).
