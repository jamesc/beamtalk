%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_script_harness_tests).

-moduledoc """
Tests for the stop-request flag of `beamtalk_script_harness` (BT-3634).

An actor's `Program exit:` requests a graceful stop from a process off the
entry's causal call chain. The entry may then complete normally; its implicit
`halt/1` must defer to the requested stop rather than win the race. `init:stop/1`
cannot run under EUnit, so these tests set the flag directly.
""".

-include_lib("eunit/include/eunit.hrl").

setup() ->
    beamtalk_script_harness:clear_stop_requested().

teardown(_) ->
    beamtalk_script_harness:clear_stop_requested().

stop_requested_defaults_to_none_test() ->
    setup(),
    ?assertEqual(none, beamtalk_script_harness:stop_requested()).

first_stop_request_wins_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_assertEqual(ok, beamtalk_script_harness:mark_stop_requested(3)),
            ?_assertEqual(ok, beamtalk_script_harness:mark_stop_requested(9)),
            ?_assertEqual({ok, 3}, beamtalk_script_harness:stop_requested())
        ]
    end}.

%% The decoupled case: a stop is requested from a process that is not on the
%% entry's call chain, then the entry completes normally (implicit status 0). The
%% harness must defer (block) instead of halting the VM with 0.
entry_completion_defers_to_requested_stop_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            Parent = self(),
            %% "Actor": a separate process requests the stop synchronously.
            spawn_link(fun() ->
                beamtalk_script_harness:mark_stop_requested(3),
                Parent ! requested
            end),
            receive
                requested -> ok
            after 1000 -> ?assert(false)
            end,
            %% "Entry": completes normally and reaches its implicit halt.
            Entry = spawn(fun() -> beamtalk_script_harness:halt_unless_stopping(0) end),
            Ref = erlang:monitor(process, Entry),
            receive
                {'DOWN', Ref, process, Entry, _} -> ?assert(false)
            after 200 -> ok
            end,
            ?assert(is_process_alive(Entry)),
            ?assertEqual({ok, 3}, beamtalk_script_harness:stop_requested()),
            exit(Entry, kill)
        end
    end}.
