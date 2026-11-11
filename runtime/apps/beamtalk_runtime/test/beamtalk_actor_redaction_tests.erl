%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_actor_redaction_tests).
-moduledoc """
BT-4567: `beamtalk_actor_redaction`, unit-level and through OTP.

This module doubles as a throwaway `gen_server` that speaks the compiled
actor's message shapes and whose `format_status/1` is the compiled actor's
(`beamtalk_actor_redaction:format_status(?MODULE, Status)`), with
`'__beamtalk_secrets'/0` declaring slot `token` and selector `login:`. It
also serves as a logger handler that forwards events to a test process, to
capture crash reports.
""".

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, format_status/1]).
-export(['__beamtalk_secrets'/0]).
%% logger handler callback
-export([log/2]).

-define(R, <<"[redacted]">>).

%%% ----------------------------------------------------------------------
%%% The throwaway actor
%%% ----------------------------------------------------------------------

'__beamtalk_secrets'() ->
    beamtalk_actor_redaction:merge_secrets({[token], []}, [], ['login:']).

format_status(Status) ->
    beamtalk_actor_redaction:format_status(?MODULE, Status).

init(State) ->
    {ok, State}.

handle_call({'login:', [Pw], _Ctx}, _From, State) ->
    {reply, {ok, <<"session-for-", Pw/binary>>}, State#{token := Pw}};
handle_call({count, [], _Ctx}, _From, #{count := N} = State) ->
    {reply, {ok, N}, State}.

handle_cast({cast, 'login:', [_Pw], _Ctx}, _State) ->
    error(boom);
handle_cast({cast, bump, [], _Ctx}, #{count := N} = State) ->
    {noreply, State#{count := N + 1}}.

handle_info(_Msg, State) ->
    {noreply, State}.

log(Event, #{config := #{pid := Pid}}) ->
    Pid ! {captured_log, Event},
    ok.

start() ->
    {ok, Pid} = gen_server:start(?MODULE, #{token => <<"tok-s3cret">>, count => 7}, []),
    Pid.

%%% ----------------------------------------------------------------------
%%% Unit: format_status/2
%%% ----------------------------------------------------------------------

state_secret_slots_redacted_test() ->
    #{state := State} = beamtalk_actor_redaction:format_status(
        ?MODULE, #{state => #{token => <<"x">>, count => 1}}
    ),
    ?assertEqual(#{token => ?R, count => 1}, State).

message_shapes_test_() ->
    Ctx = #{},
    Fut = self(),
    Cases = [
        {{'login:', [pw], Ctx}, {'login:', ?R, Ctx}},
        {{'login:', [pw]}, {'login:', ?R}},
        {{cast, 'login:', [pw], Ctx}, {cast, 'login:', ?R, Ctx}},
        {{cast, 'login:', [pw]}, {cast, 'login:', ?R}},
        {{'login:', [pw], Fut, Ctx}, {'login:', ?R, Fut, Ctx}},
        {
            {'$beamtalk_wire', 1, call, 'login:', w, Ctx},
            {'$beamtalk_wire', 1, call, 'login:', ?R, Ctx}
        },
        {
            {'$beamtalk_wire', 1, cast, 'login:', w, Ctx},
            {'$beamtalk_wire', 1, cast, 'login:', ?R, Ctx}
        },
        {
            {'$beamtalk_wire', 1, async, 'login:', w, Fut, Ctx},
            {'$beamtalk_wire', 1, async, 'login:', ?R, Fut, Ctx}
        },
        {{'$gen_cast', {cast, 'login:', [pw], Ctx}}, {'$gen_cast', {cast, 'login:', ?R, Ctx}}},
        {
            {continue, {?BT_CONTINUE_TAG, [{'login:', [pw]}, {count, []}]}},
            {continue, {?BT_CONTINUE_TAG, [{'login:', ?R}, {count, []}]}}
        },
        {{'perform:withArguments:', ['login:', [pw]], Ctx}, {'perform:withArguments:', ?R, Ctx}},
        {
            {'perform:withArguments:', ['perform:withArguments:', ['login:', [pw]]], Ctx},
            {'perform:withArguments:', ?R, Ctx}
        },
        %% non-secret sends and foreign messages pass through
        {{count, [], Ctx}, {count, [], Ctx}},
        {{'perform:', [count], Ctx}, {'perform:', [count], Ctx}},
        {{'EXIT', self(), normal}, {'EXIT', self(), normal}}
    ],
    [
        ?_assertEqual(
            #{message => Want},
            beamtalk_actor_redaction:format_status(?MODULE, #{message => In})
        )
     || {In, Want} <- Cases
    ].

log_reply_to_secret_call_redacted_test() ->
    F1 = {self(), tag1},
    F2 = {self(), tag2},
    Log = [
        {in, {'$gen_call', F1, {'login:', [pw], #{}}}},
        {out, {ok, session}, F1, #{token => pw}},
        {in, {'$gen_call', F2, {count, [], #{}}}},
        {out, {ok, 3}, F2, #{token => pw}},
        {noreply, #{token => pw}}
    ],
    #{log := Out} = beamtalk_actor_redaction:format_status(?MODULE, #{log => Log}),
    ?assertEqual(
        [
            {in, {'$gen_call', F1, {'login:', ?R, #{}}}},
            {out, ?R, F1, #{token => ?R}},
            {in, {'$gen_call', F2, {count, [], #{}}}},
            {out, {ok, 3}, F2, #{token => ?R}},
            {noreply, #{token => ?R}}
        ],
        Out
    ).

log_reply_without_its_call_redacted_test() ->
    %% The call dropped out of the bounded log: the reply may be secret.
    Log = [{out, {ok, session}, {self(), gone}, #{}}],
    #{log := Out} = beamtalk_actor_redaction:format_status(?MODULE, #{log => Log}),
    ?assertEqual([{out, ?R, {self(), gone}, #{}}], Out).

reason_untouched_test() ->
    Status = #{reason => {secret, in, reason}},
    ?assertEqual(Status, beamtalk_actor_redaction:format_status(?MODULE, Status)).

fails_closed_when_metadata_unreadable_test() ->
    %% `missing_actor_mod` is not loaded, so its metadata cannot be read.
    Status = #{state => #{token => x}, message => m, log => [e], reason => r},
    ?assertEqual(
        #{state => ?R, message => ?R, log => [], reason => r},
        beamtalk_actor_redaction:format_status(missing_actor_mod, Status)
    ).

%%% ----------------------------------------------------------------------
%%% Through OTP
%%% ----------------------------------------------------------------------

get_status_redacts_state_and_log_test() ->
    Pid = start(),
    try
        ok = sys:log(Pid, true),
        {ok, <<"session-for-hunter2">>} = gen_server:call(Pid, {'login:', [<<"hunter2">>], #{}}),
        {ok, 7} = gen_server:call(Pid, {count, [], #{}}),
        %% Only the formatted part (`Misc`) goes through format_status/1:
        %% OTP hands back the raw `Debug` term, which holds the raw sys log,
        %% alongside it (see the beamtalk_actor_redaction moduledoc).
        {status, Pid, {module, gen_server}, [_PDict, _SysState, _Parent, _Debug, Misc]} =
            sys:get_status(Pid),
        Text = lists:flatten(io_lib:format("~p", [Misc])),
        ?assertEqual(nomatch, string:find(Text, "hunter2")),
        ?assertEqual(nomatch, string:find(Text, "tok-s3cret")),
        %% the selector, the other slot and the non-secret reply are kept
        ?assertNotEqual(nomatch, string:find(Text, "login:")),
        ?assertNotEqual(nomatch, string:find(Text, "count")),
        ?assertNotEqual(nomatch, string:find(Text, "{ok,7}"))
    after
        exit(Pid, kill)
    end.

crash_report_redacts_message_and_state_test() ->
    HandlerId = bt4567_capture,
    ok = logger:add_handler(HandlerId, ?MODULE, #{config => #{pid => self()}}),
    Pid = start(),
    MRef = monitor(process, Pid),
    try
        gen_server:cast(Pid, {cast, 'login:', [<<"hunter2">>], #{}}),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 5000 -> error(no_crash)
        end,
        Report = await_terminate_report(Pid),
        #{last_message := LastMsg, state := State} = Report,
        ?assertEqual({'$gen_cast', {cast, 'login:', ?R, #{}}}, LastMsg),
        ?assertEqual(#{token => ?R, count => 7}, State)
    after
        logger:remove_handler(HandlerId)
    end.

await_terminate_report(Pid) ->
    receive
        {captured_log, #{msg := {report, #{label := {gen_server, terminate}} = Report}}} ->
            case Report of
                #{name := Pid} -> Report;
                _ -> await_terminate_report(Pid)
            end;
        {captured_log, _Other} ->
            await_terminate_report(Pid)
    after 5000 -> error(no_terminate_report)
    end.

%%% ----------------------------------------------------------------------
%%% A compiled actor (test_fixtures/secret_vault_actor.bt)
%%% ----------------------------------------------------------------------

compiled_secrets_metadata_test() ->
    ?assertEqual(
        {[verifier], ['unlock:', 'explode:']},
        'bt@secret_vault_actor':'__beamtalk_secrets'()
    ).

compiled_actor_get_status_redacts_test() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    Pid = element(4, 'bt@secret_vault_actor':spawn()),
    try
        ok = sys:log(Pid, true),
        ?assertEqual({ok, 62674}, gen_server:call(Pid, {'unlock:', [31337], #{}})),
        ?assertEqual({ok, 7}, gen_server:call(Pid, {count, [], #{}})),
        {status, Pid, {module, gen_server}, [_PDict, _SysState, _Parent, _Debug, Misc]} =
            sys:get_status(Pid),
        Text = lists:flatten(io_lib:format("~p", [Misc])),
        ?assertEqual(nomatch, string:find(Text, "31337")),
        ?assertEqual(nomatch, string:find(Text, "62674")),
        ?assertEqual(nomatch, string:find(Text, "vault-s3cret")),
        ?assertNotEqual(nomatch, string:find(Text, "unlock:")),
        ?assertNotEqual(nomatch, string:find(Text, "{ok,7}"))
    after
        gen_server:stop(Pid)
    end.

compiled_actor_crash_report_redacts_test() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    HandlerId = bt4567_compiled_capture,
    ok = logger:add_handler(HandlerId, ?MODULE, #{config => #{pid => self()}}),
    Pid = element(4, 'bt@secret_vault_actor':spawn()),
    MRef = monitor(process, Pid),
    try
        %% A fire-and-forget cast that raises crashes the actor (ADR 0134).
        gen_server:cast(Pid, {cast, 'explode:', [<<"hunter2">>], #{}}),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 5000 -> error(no_crash)
        end,
        #{last_message := LastMsg, state := State} = await_terminate_report(Pid),
        ?assertEqual({'$gen_cast', {cast, 'explode:', ?R, #{}}}, LastMsg),
        ?assertMatch(#{verifier := ?R, count := 7}, State)
    after
        logger:remove_handler(HandlerId)
    end.

%%% ----------------------------------------------------------------------
%%% proc_lib crash report filter (BT-4666)
%%% ----------------------------------------------------------------------

%% A `{proc_lib, crash}` log event as proc_lib:crash_report/4 builds it.
crash_event(InitialCall, Messages, Dict) ->
    Info = [
        {initial_call, InitialCall},
        {pid, self()},
        {error_info, {error, boom, []}},
        {messages, Messages},
        {dictionary, Dict}
    ],
    #{
        level => error,
        msg => {report, #{label => {proc_lib, crash}, report => [Info, []]}},
        meta => #{}
    }.

crash_info(#{msg := {report, #{report := [Info, _Linked]}}}) ->
    Info.

crash_filter_redacts_queued_secret_sends_test() ->
    Call = {'$gen_call', {self(), tag}, {'login:', [pw], #{}}},
    Other = {'$gen_call', {self(), tag2}, {count, [], #{}}},
    Event = crash_event({?MODULE, init, ['Argument__1']}, [Call, Other, stray], []),
    Info = crash_info(beamtalk_actor_redaction:crash_report_filter(Event, [])),
    ?assertEqual(
        [{'$gen_call', {self(), tag}, {'login:', ?R, #{}}}, Other, stray],
        proplists:get_value(messages, Info)
    ).

crash_filter_redacts_pending_continuations_test() ->
    Pending = {'$bt_pending_continue', [{'login:', [pw]}, {count, []}]},
    Event = crash_event({?MODULE, init, ['Argument__1']}, [], [Pending, {other, kept}]),
    Info = crash_info(beamtalk_actor_redaction:crash_report_filter(Event, [])),
    ?assertEqual(
        [{'$bt_pending_continue', [{'login:', ?R}, {count, []}]}, {other, kept}],
        proplists:get_value(dictionary, Info)
    ).

crash_filter_ignores_non_actor_processes_test() ->
    Event = crash_event({some_server, init, ['Argument__1']}, [{'login:', [pw], #{}}], []),
    ?assertEqual(ignore, beamtalk_actor_redaction:crash_report_filter(Event, [])).

crash_filter_ignores_other_events_test() ->
    Event = #{level => error, msg => {string, "hello"}, meta => #{}},
    ?assertEqual(ignore, beamtalk_actor_redaction:crash_report_filter(Event, [])).

crash_filter_fails_closed_test() ->
    ok = meck:new(bt4666_broken_actor, [non_strict]),
    try
        ok = meck:expect(bt4666_broken_actor, '__beamtalk_secrets', fun() -> error(boom) end),
        Event = crash_event({bt4666_broken_actor, init, ['Argument__1']}, [m], [{k, v}]),
        Info = crash_info(beamtalk_actor_redaction:crash_report_filter(Event, [])),
        ?assertEqual(?R, proplists:get_value(messages, Info)),
        ?assertEqual(?R, proplists:get_value(dictionary, Info)),
        ?assertMatch({error, boom, _}, proplists:get_value(error_info, Info))
    after
        meck:unload(bt4666_broken_actor)
    end.

compiled_actor_proc_lib_report_redacts_queued_send_test() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    HandlerId = bt4666_compiled_capture,
    ok = logger:add_handler(HandlerId, ?MODULE, #{config => #{pid => self()}}),
    Pid = element(4, 'bt@secret_vault_actor':spawn()),
    MRef = monitor(process, Pid),
    try
        %% Queue a secret call behind the cast that crashes the actor, so it
        %% is still in the mailbox when proc_lib writes its crash report.
        true = erlang:suspend_process(Pid),
        gen_server:cast(Pid, {cast, 'explode:', [1], #{}}),
        Pid ! {'$gen_call', {self(), make_ref()}, {'unlock:', [31337], #{}}},
        true = erlang:resume_process(Pid),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 5000 -> error(no_crash)
        end,
        Info = await_proc_lib_report(Pid),
        Messages = proplists:get_value(messages, Info),
        ?assertMatch([{'$gen_call', _, {'unlock:', ?R, _}}], Messages),
        ?assertEqual(nomatch, string:find(lists:flatten(io_lib:format("~p", [Info])), "31337"))
    after
        logger:remove_handler(HandlerId)
    end.

await_proc_lib_report(Pid) ->
    receive
        {captured_log, #{msg := {report, #{label := {proc_lib, crash}, report := [Info, _]}}}} ->
            case proplists:get_value(pid, Info) of
                Pid -> Info;
                _ -> await_proc_lib_report(Pid)
            end;
        {captured_log, _Other} ->
            await_proc_lib_report(Pid)
    after 5000 -> error(no_proc_lib_report)
    end.
