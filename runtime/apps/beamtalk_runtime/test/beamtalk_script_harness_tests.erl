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

%% flush_loggers/0 is exported and can be called directly in EUnit. The default
%% OTP logger configuration includes at least the 'default' handler (logger_std_h),
%% so the first arm fires and calls logger_std_h:filesync/1. The second arm
%% fires when a handler with a different module is present (e.g. the test-only
%% beamtalk_noop_log_handler below).
flush_loggers_returns_ok_test() ->
    ?assertEqual(ok, beamtalk_script_harness:flush_loggers()).

flush_loggers_skips_non_std_handler_test_() ->
    HandlerId = beamtalk_flush_loggers_noop_test_handler,
    {setup,
        fun() ->
            ok = logger:add_handler(HandlerId, beamtalk_noop_log_handler, #{}),
            HandlerId
        end,
        fun(Id) ->
            _ = logger:remove_handler(Id)
        end,
        fun(_Id) ->
            [?_assertEqual(ok, beamtalk_script_harness:flush_loggers())]
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

%%% BT-4821: `--raw-mfa` lists every frame with its raw MFA.
raw_trace_lists_every_frame_with_raw_mfa_test() ->
    Stack = [
        {'bt@user@A', m1, 0, [{file, "a.bt"}, {line, 1}]},
        {gen_server, loop, 7, [{file, "gen_server.erl"}, {line, 2}]}
    ],
    Out = iolist_to_binary(beamtalk_script_harness:raw_trace(#{raw_mfa => true}, boom, Stack)),
    ?assertEqual(2, length(binary:matches(Out, <<"  at ">>))),
    ?assertNotEqual(nomatch, binary:match(Out, <<"[bt@user@A:m1/0]">>)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"[gen_server:loop/7]">>)).

%%% BT-4835: the default lists the visible frames and names what it left out.
raw_trace_default_lists_visible_frames_and_notice_test() ->
    Stack = [
        {maps, get, 2, [{file, "maps.erl"}, {line, 1}]},
        {gen_server, loop, 7, [{file, "gen_server.erl"}, {line, 2}]}
    ],
    Out = iolist_to_binary(
        beamtalk_script_harness:raw_trace(#{raw_mfa => false}, boom, Stack)
    ),
    ?assertNotEqual(nomatch, binary:match(Out, <<"  at maps:get/2">>)),
    ?assertEqual(nomatch, binary:match(Out, <<"gen_server">>)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"(1 frame hidden; --raw-mfa shows all)">>)).

%%% BT-4835: a caller with no `--raw-mfa` switch (a built escript calls dispatch/3)
%%% gets the frames but not a hint pointing at it.
raw_trace_without_the_option_has_no_hint_test() ->
    Stack = [
        {maps, get, 2, [{file, "maps.erl"}, {line, 1}]},
        {gen_server, loop, 7, [{file, "gen_server.erl"}, {line, 2}]}
    ],
    Out = iolist_to_binary(beamtalk_script_harness:raw_trace(#{}, boom, Stack)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"  at maps:get/2">>)),
    ?assertEqual(nomatch, binary:match(Out, <<"--raw-mfa">>)).

%%% BT-4835: a wrapped exception's own (raise-site) frames win over the catch stack.
raw_trace_prefers_the_exceptions_own_frames_test() ->
    Raise = [{maps, get, 2, [{file, "maps.erl"}, {line, 1}]}],
    Catch = [{beamtalk_script_harness, dispatch, 4, []}],
    Exc = (beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'Object')))#{
        raw_stacktrace => Raise
    },
    Out = iolist_to_binary(beamtalk_script_harness:raw_trace(#{raw_mfa => true}, Exc, Catch)),
    ?assertNotEqual(nomatch, binary:match(Out, <<"[maps:get/2]">>)),
    ?assertEqual(nomatch, binary:match(Out, <<"dispatch/4">>)).
