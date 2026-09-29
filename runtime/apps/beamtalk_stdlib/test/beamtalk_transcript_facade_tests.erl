%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_transcript_facade_tests).

-moduledoc """
Unit tests for `beamtalk_transcript_facade` (ADR 0129 §5, BT-3642).

Covers the two routes (registered `TranscriptStream` vs the Logger fallback),
the guard that a user process registered as `'Transcript'` cannot capture the
output, the `no_workspace` refusal of `recent`/`clear`, and the runtime's
`beamtalk_transcript_log` handler + default-handler domain filter.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% logger handler callback (the capture handler below)
-export([log/2]).

-define(CAPTURE, beamtalk_transcript_facade_capture).

%%% ============================================================================
%%% Fixtures
%%% ============================================================================

facade_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        t("show: logs one notice event", fun show_logs_notice/0),
        t("show: renders non-strings", fun show_renders_printstring/0),
        t("showCr: logs one event", fun show_cr_logs_one_event/0),
        t("cr is a no-op on the Logger route", fun cr_is_noop_without_stream/0),
        t("recent raises no_workspace", fun recent_raises_no_workspace/0),
        t("clear raises no_workspace", fun clear_raises_no_workspace/0),
        t("a user process named Transcript cannot capture output", fun user_process_not_captured/0),
        t("routes to the registered TranscriptStream", fun routes_to_stream/0),
        t("recent and clear use the stream", fun recent_and_clear_use_stream/0),
        t("dead stream falls back to Logger", fun dead_stream_falls_back/0)
    ]}.

%% eunit runs the test body in a different process from `setup/0`, so the
%% capture handler delivers to whichever process called `capture/0` last.
t(Name, Fun) ->
    fun(_) ->
        {Name, fun() ->
            capture(self()),
            Fun()
        end}
    end.

capture(Pid) ->
    persistent_term:put(?CAPTURE, Pid).

handler_test_() ->
    {setup, fun() -> beamtalk_logging_config:install_transcript_handler() end, fun(_) -> ok end, [
        {"transcript handler is installed with the domain filter", fun handler_installed/0},
        {"default handler stops the transcript domain", fun default_handler_filters_domain/0},
        {"install is idempotent", fun install_idempotent/0},
        {"flush_transcript returns ok", fun flush_returns_ok/0}
    ]}.

setup() ->
    ok = beamtalk_logging_config:install_transcript_handler(),
    %% Other suites may have raised the primary level; notice must pass.
    #{level := OldLevel} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, notice),
    ok = logger:add_handler(?CAPTURE, ?MODULE, #{
        level => all,
        filter_default => stop,
        filters => [
            {domain, {
                fun logger_filters:domain/2, {log, sub, beamtalk_logging_config:transcript_domain()}
            }}
        ],
        config => #{}
    }),
    unregister_transcript(),
    OldLevel.

cleanup(OldLevel) ->
    unregister_transcript(),
    _ = logger:remove_handler(?CAPTURE),
    _ = persistent_term:erase(?CAPTURE),
    ok = logger:set_primary_config(level, OldLevel),
    ok.

unregister_transcript() ->
    case whereis('Transcript') of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            exit(Pid, kill),
            wait_gone('Transcript')
    end.

wait_gone(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        _ ->
            timer:sleep(10),
            wait_gone(Name)
    end.

%%% logger handler callback

log(#{msg := Msg, level := Level, meta := Meta}, _Config) ->
    case persistent_term:get(?CAPTURE, undefined) of
        undefined -> ok;
        Pid -> Pid ! {transcript_event, Level, Meta, format(Msg)}
    end,
    ok.

format({string, S}) -> iolist_to_binary(S);
format({Fmt, Args}) -> iolist_to_binary(io_lib:format(Fmt, Args));
format(Other) -> Other.

next_event() ->
    receive
        {transcript_event, Level, Meta, Text} -> {Level, Meta, Text}
    after 1000 -> timeout
    end.

no_event() ->
    receive
        {transcript_event, _, _, _} = Ev -> erlang:error({unexpected_event, Ev})
    after 100 -> ok
    end.

%%% ============================================================================
%%% Logger route
%%% ============================================================================

show_logs_notice() ->
    ?assertEqual(nil, beamtalk_transcript_facade:show(<<"hello">>)),
    {Level, Meta, Text} = next_event(),
    ?assertEqual(notice, Level),
    ?assertEqual([beamtalk, user, transcript], maps:get(domain, Meta)),
    ?assertEqual(<<"hello">>, Text),
    ok = no_event().

show_renders_printstring() ->
    beamtalk_transcript_facade:show(42),
    ?assertMatch({notice, _, <<"42">>}, next_event()).

show_cr_logs_one_event() ->
    ?assertEqual(nil, beamtalk_transcript_facade:showCr(<<"line">>)),
    ?assertMatch({notice, _, <<"line">>}, next_event()),
    ok = no_event().

cr_is_noop_without_stream() ->
    ?assertEqual(nil, beamtalk_transcript_facade:cr()),
    ok = no_event().

recent_raises_no_workspace() ->
    ?assertError(
        #{error := #beamtalk_error{kind = no_workspace, class = 'Transcript', selector = recent}},
        beamtalk_transcript_facade:recent()
    ).

clear_raises_no_workspace() ->
    ?assertError(
        #{error := #beamtalk_error{kind = no_workspace, class = 'Transcript', selector = clear}},
        beamtalk_transcript_facade:clear()
    ).

%% A user actor registered as #Transcript must not capture the output.
user_process_not_captured() ->
    Self = self(),
    Impostor = spawn(fun() ->
        register('Transcript', self()),
        Self ! registered,
        receive
            Msg -> Self ! {impostor_got, Msg}
        after 500 -> ok
        end
    end),
    receive
        registered -> ok
    after 1000 -> erlang:error(impostor_not_registered)
    end,
    beamtalk_transcript_facade:show(<<"secret">>),
    ?assertMatch({notice, _, <<"secret">>}, next_event()),
    ?assertError(
        #{error := #beamtalk_error{kind = no_workspace}}, beamtalk_transcript_facade:recent()
    ),
    receive
        {impostor_got, _} -> erlang:error(impostor_captured_output)
    after 100 -> ok
    end,
    exit(Impostor, kill).

%%% ============================================================================
%%% Stream route
%%% ============================================================================

routes_to_stream() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 100),
    try
        ?assertEqual(nil, beamtalk_transcript_facade:show(<<"to stream">>)),
        ?assertEqual(nil, beamtalk_transcript_facade:cr()),
        ok = no_event(),
        ?assertEqual([<<"to stream">>, <<"\n">>], gen_server:call(Pid, recent))
    after
        unlink(Pid),
        gen_server:stop(Pid)
    end.

recent_and_clear_use_stream() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 100),
    try
        beamtalk_transcript_facade:showCr(<<"a">>),
        ?assertEqual([<<"a">>, <<"\n">>], beamtalk_transcript_facade:recent()),
        ?assertEqual(nil, beamtalk_transcript_facade:clear()),
        ?assertEqual([], beamtalk_transcript_facade:recent()),
        ok = no_event()
    after
        unlink(Pid),
        gen_server:stop(Pid)
    end.

dead_stream_falls_back() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 100),
    unlink(Pid),
    gen_server:stop(Pid),
    beamtalk_transcript_facade:show(<<"after">>),
    ?assertMatch({notice, _, <<"after">>}, next_event()).

%%% ============================================================================
%%% Runtime handler
%%% ============================================================================

handler_installed() ->
    {ok, Config} = logger:get_handler_config(beamtalk_transcript_log),
    ?assertEqual(logger_std_h, maps:get(module, Config)),
    ?assertEqual(#{type => standard_io}, maps:with([type], maps:get(config, Config))),
    ?assertEqual(stop, maps:get(filter_default, Config)),
    ?assertMatch(
        {logger_formatter, #{template := [msg, "\n"]}}, maps:get(formatter, Config)
    ),
    ?assertMatch([{_, _}], maps:get(filters, Config)).

default_handler_filters_domain() ->
    case logger:get_handler_config(default) of
        {ok, #{filters := Filters}} ->
            ?assert(lists:keymember(beamtalk_transcript_domain, 1, Filters));
        {error, _} ->
            ok
    end.

install_idempotent() ->
    ?assertEqual(ok, beamtalk_logging_config:install_transcript_handler()),
    ?assertEqual(ok, beamtalk_logging_config:install_transcript_handler()).

flush_returns_ok() ->
    beamtalk_transcript_facade:show(<<"flushed">>),
    ?assertEqual(ok, beamtalk_logging_config:flush_transcript()),
    %% The harness flushes every std handler, including the transcript one,
    %% before it halts the node.
    ?assertEqual(ok, beamtalk_script_harness:flush_loggers()).
