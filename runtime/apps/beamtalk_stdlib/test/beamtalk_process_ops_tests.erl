%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_process_ops_tests).

%%% **DDD Context:** Runtime Context

-moduledoc "EUnit tests for beamtalk_process_ops (BT-4049).".

-include_lib("eunit/include/eunit.hrl").

send_delivers_raw_term_and_answers_nil_test() ->
    ?assertEqual(nil, beamtalk_process_ops:send(self(), {ok, 1})),
    receive
        Msg -> ?assertEqual({ok, 1}, Msg)
    after 1000 -> ?assert(false)
    end.

send_to_dead_pid_is_noop_test() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end,
    ?assertEqual(nil, beamtalk_process_ops:send(Pid, hello)).

make_ref_is_unique_test() ->
    A = beamtalk_process_ops:makeRef(),
    B = beamtalk_process_ops:makeRef(),
    ?assert(is_reference(A)),
    ?assertNotEqual(A, B).

label_round_trip_test() ->
    Parent = self(),
    Pid = spawn(fun() ->
        nil = beamtalk_process_ops:setLabel({ok, 1}),
        Parent ! labelled,
        receive
            stop -> ok
        end
    end),
    receive
        labelled -> ok
    end,
    ?assertEqual({ok, 1}, beamtalk_process_ops:labelOf(Pid)),
    Pid ! stop.

unlabelled_process_answers_nil_test() ->
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual(nil, beamtalk_process_ops:labelOf(Pid)),
    Pid ! stop.

raw_results_marker_test() ->
    ?assert(beamtalk_process_ops:beamtalk_ffi_raw_results()).
