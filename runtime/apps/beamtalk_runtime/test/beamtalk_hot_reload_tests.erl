%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hot_reload_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for beamtalk_hot_reload domain service
%%====================================================================

%% Basic behavior tests
code_change_returns_ok_tuple_test() ->
    State = #{foo => bar},
    Result = beamtalk_hot_reload:code_change(old_version, State, extra),
    ?assertMatch({ok, _}, Result).

code_change_preserves_state_test() ->
    State = #{field1 => value1, field2 => 42},
    {ok, NewState} = beamtalk_hot_reload:code_change(old_version, State, extra),
    ?assertEqual(State, NewState).

%% Test with different OldVsn formats
code_change_handles_atom_version_test() ->
    State = #{data => <<"test">>},
    {ok, NewState} = beamtalk_hot_reload:code_change('1.0', State, extra),
    ?assertEqual(State, NewState).

code_change_handles_down_tuple_version_test() ->
    State = #{count => 0},
    {ok, NewState} = beamtalk_hot_reload:code_change({down, '2.0'}, State, extra),
    ?assertEqual(State, NewState).

code_change_handles_undefined_version_test() ->
    State = #{},
    {ok, NewState} = beamtalk_hot_reload:code_change(undefined, State, extra),
    ?assertEqual(State, NewState).

%% Test with different state types
code_change_handles_empty_map_test() ->
    State = #{},
    {ok, NewState} = beamtalk_hot_reload:code_change(v1, State, []),
    ?assertEqual(State, NewState).

code_change_handles_complex_state_test() ->
    State = #{
        '$beamtalk_class' => 'Counter',
        '__methods__' => #{increment => fun() -> ok end},
        value => 42,
        nested => #{deep => #{value => <<"test">>}}
    },
    {ok, NewState} = beamtalk_hot_reload:code_change(v1, State, extra),
    ?assertEqual(State, NewState).

code_change_handles_list_state_test() ->
    %% Some gen_servers might use list states
    State = [1, 2, 3],
    {ok, NewState} = beamtalk_hot_reload:code_change(v1, State, extra),
    ?assertEqual(State, NewState).

code_change_handles_tuple_state_test() ->
    %% Some gen_servers might use tuple states
    State = {state, 42, <<"data">>},
    {ok, NewState} = beamtalk_hot_reload:code_change(v1, State, extra),
    ?assertEqual(State, NewState).

%% Test with different Extra values
code_change_handles_various_extra_test() ->
    State = #{test => true},
    ?assertMatch({ok, State}, beamtalk_hot_reload:code_change(v1, State, undefined)),
    ?assertMatch({ok, State}, beamtalk_hot_reload:code_change(v1, State, [])),
    ?assertMatch({ok, State}, beamtalk_hot_reload:code_change(v1, State, #{config => value})),
    ?assertMatch({ok, State}, beamtalk_hot_reload:code_change(v1, State, {migration, data})).

%%====================================================================
%% Tests for trigger_code_change/2
%%====================================================================

trigger_code_change_empty_pids_test() ->
    {ok, Upgraded, Failures} = beamtalk_hot_reload:trigger_code_change(test_module, []),
    ?assertEqual(0, Upgraded),
    ?assertEqual([], Failures).

trigger_code_change_dead_pid_test() ->
    %% A dead PID should result in a failure (noproc)
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    {ok, 0, Failures} = beamtalk_hot_reload:trigger_code_change(test_module, [DeadPid]),
    ?assertEqual(1, length(Failures)),
    [{DeadPid, _Reason}] = Failures.
