%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_crash_report_filter_tests).
-moduledoc """
BT-4820: `beamtalk_crash_report_filter` against real OTP crash reports.

A throwaway module named like a compiled class module
(`bt@bt4820pkg@bt4820_crasher`, registered as class `Bt4820Crasher`) crashes in
a `handleInfo:` handler under `gen_server`, under `proc_lib`, and under a
supervisor. The reports are captured with a logger handler, run through the
filter installed as in the runtime, and formatted with `logger_formatter`.
Reports are built by the OTP version running the tests, so CI covers every
OTP in the supported window.
""".

-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

-define(MOD, 'bt@bt4820pkg@bt4820_crasher').
-define(HANDLER, bt4820_capture).
-define(RAW, <<"bt@bt4820pkg@bt4820_crasher:">>).

crash_report_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"gen_server terminate names Class>>selector", fun gen_server_terminate/0},
            {"proc_lib keyword-selector frame (quoted atom) is renamed",
                fun proc_lib_keyword_selector/0},
            {"proc_lib crash renders initial_call and stacktrace", fun proc_lib_crash/0},
            {"supervisor child report renders mfargs", fun supervisor_child/0},
            {"raw MFA stays in the report term", fun raw_stays_in_report/0},
            {"unrecognised reports pass through", fun passthrough/0},
            {"malformed report: warn, untranslated, hook still installed", fun malformed_report/0}
        ]
    end}.

%%% Setup

setup() ->
    beamtalk_test_boot:registry(),
    load_crasher(),
    _ = beamtalk_test_class:start('Bt4820Crasher', #{
        superclass => none, methods => #{}, class_methods => #{}, module => ?MOD
    }),
    ok = logger:add_handler(?HANDLER, ?MODULE, #{level => all}),
    ok = beamtalk_crash_report_filter:install(),
    process_flag(trap_exit, true),
    ok.

cleanup(_) ->
    _ = logger:remove_primary_filter(beamtalk_crash_report_filter),
    _ = logger:remove_handler(?HANDLER),
    beamtalk_test_class:stop_graceful('Bt4820Crasher'),
    code:purge(?MOD),
    code:delete(?MOD),
    code:purge(?MOD),
    ok.

%% EUnit runs `setup` in another process than the tests: each test claims
%% the capture by registering itself (see drain/0).
log(Event, _Config) ->
    case whereis(bt4820_test) of
        undefined -> ok;
        Pid -> Pid ! {captured, Event}
    end,
    ok.

%% gen_server / supervisor / proc_lib callbacks of the "actor".
load_crasher() ->
    Src =
        "-module('bt@bt4820pkg@bt4820_crasher').\n"
        "-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 'handleInfo:'/2,\n"
        " start_link/0, crash/0, 'at:put:'/0]).\n"
        "init(S) -> {ok, S}.\n"
        "handle_call(_, _, S) -> {reply, ok, S}.\n"
        "handle_cast(_, S) -> {noreply, S}.\n"
        "handle_info(M, S) -> 'handleInfo:'(M, S).\n"
        "'handleInfo:'(_M, _S) -> erlang:error(boom).\n"
        "start_link() -> gen_server:start_link('bt@bt4820pkg@bt4820_crasher', #{}, []).\n"
        "crash() -> erlang:error(boom).\n"
        "'at:put:'() -> erlang:error(boom).\n",
    {ok, Toks, _} = erl_scan:string(Src),
    Forms = parse_forms(Toks, []),
    {ok, ?MOD, Bin} = compile:forms(Forms, [binary]),
    {module, ?MOD} = code:load_binary(?MOD, "bt4820.beam", Bin),
    ok.

parse_forms([], Acc) ->
    lists:reverse(Acc);
parse_forms(Toks, Acc) ->
    {Form, Rest} = lists:splitwith(fun(T) -> element(1, T) =/= dot end, Toks),
    {ok, F} = erl_parse:parse_form(Form ++ [hd(Rest)]),
    parse_forms(tl(Rest), [F | Acc]).

%%% Cases

gen_server_terminate() ->
    drain(),
    {ok, Pid} = gen_server:start(?MOD, #{}, []),
    Pid ! trigger,
    Event = await(fun
        ({gen_server, terminate}) -> true;
        (_) -> false
    end),
    Text = format(Event),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Bt4820Crasher>>handleInfo:">>)),
    ?assertEqual(nomatch, binary:match(Text, ?RAW)).

proc_lib_keyword_selector() ->
    drain(),
    _ = proc_lib:spawn(?MOD, 'at:put:', []),
    Event = await(fun
        ({proc_lib, crash}) -> true;
        (_) -> false
    end),
    Text = format(Event),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Bt4820Crasher>>at:put:">>)),
    ?assertEqual(nomatch, binary:match(Text, ?RAW)).

proc_lib_crash() ->
    drain(),
    _ = proc_lib:spawn(?MOD, crash, []),
    Event = await(fun
        ({proc_lib, crash}) -> true;
        (_) -> false
    end),
    Text = format(Event),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Bt4820Crasher>>crash">>)),
    ?assertEqual(nomatch, binary:match(Text, ?RAW)).

supervisor_child() ->
    drain(),
    {ok, Sup} = supervisor:start_link(
        {local, bt4820_sup},
        bt4820_sup_cb,
        []
    ),
    unlink(Sup),
    [{_, Child, _, _}] = supervisor:which_children(Sup),
    Child ! trigger,
    Event = await(fun
        ({supervisor, child_terminated}) -> true;
        (_) -> false
    end),
    Text = format(Event),
    ?assertNotEqual(
        nomatch, binary:match(Text, <<"{mfargs,{<<\"Bt4820Crasher>>start_link\">>,[]}}">>)
    ),
    ?assertEqual(nomatch, binary:match(Text, ?RAW)),
    exit(Sup, shutdown).

raw_stays_in_report() ->
    drain(),
    {ok, Pid} = gen_server:start(?MOD, #{}, []),
    Pid ! trigger,
    #{msg := {report, Report}} = await(fun
        ({gen_server, terminate}) -> true;
        (_) -> false
    end),
    Raw = iolist_to_binary(io_lib:format("~tp", [maps:get(reason, Report)])),
    ?assertNotEqual(nomatch, binary:match(Raw, <<"bt@bt4820pkg@bt4820_crasher">>)).

passthrough() ->
    Cb = fun(R) -> {"~tp", [R]} end,
    Meta = #{report_cb => Cb},
    Unknown = #{msg => {report, #{label => {other, thing}}}, meta => Meta},
    ?assertEqual(ignore, beamtalk_crash_report_filter:filter(Unknown, [])),
    NoBt = #{
        msg =>
            {report, #{label => {gen_server, terminate}, reason => {oops, [{lists, foo, 1, []}]}}},
        meta => Meta
    },
    ?assertEqual(ignore, beamtalk_crash_report_filter:filter(NoBt, [])),
    ?assertEqual(
        ignore, beamtalk_crash_report_filter:filter(#{msg => {string, "x"}, meta => #{}}, [])
    ),
    ?assertEqual(ignore, beamtalk_crash_report_filter:filter(garbage, [])).

malformed_report() ->
    drain(),
    Bad = #{
        msg =>
            {report, #{label => {supervisor, child_terminated}, report => [{offender, [a | b]}]}},
        meta => #{report_cb => fun(R) -> {"~tp", [R]} end}
    },
    ?assertEqual(ignore, beamtalk_crash_report_filter:filter(Bad, [])),
    receive
        {captured, #{msg := {report, #{label := {beamtalk, crash_report_translation_failed}}}}} ->
            ok
    after 2000 -> ?assert(false)
    end,
    #{filters := Filters} = logger:get_primary_config(),
    ?assert(lists:keymember(beamtalk_crash_report_filter, 1, Filters)).

%%% Helpers

format(Event) ->
    unicode:characters_to_binary(logger_formatter:format(Event, #{single_line => false})).

await(Match) ->
    receive
        {captured, #{msg := {report, #{label := L}}} = E} ->
            case Match(L) of
                true -> E;
                false -> await(Match)
            end;
        {captured, _} ->
            await(Match)
    after 5000 -> error(no_report)
    end.

drain() ->
    try
        unregister(bt4820_test)
    catch
        error:badarg -> ok
    end,
    true = register(bt4820_test, self()),
    process_flag(trap_exit, true),
    drain_mailbox().

drain_mailbox() ->
    receive
        {captured, _} -> drain_mailbox()
    after 0 -> ok
    end.
