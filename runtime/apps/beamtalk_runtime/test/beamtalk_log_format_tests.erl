%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_log_format_tests).

%%% **DDD Context:** Runtime Context — shared leaf

-moduledoc """
EUnit tests for the log-event helpers shared by the JSON formatter, the
WebSocket log handler and the REPL watch op (BT-4902).
""".

-include_lib("eunit/include/eunit.hrl").

format_domain_joins_atoms_test() ->
    ?assertEqual(
        <<"beamtalk.runtime">>, beamtalk_log_format:format_domain(#{domain => [beamtalk, runtime]})
    ),
    ?assertEqual(<<>>, beamtalk_log_format:format_domain(#{domain => []})).

format_domain_absent_or_malformed_test() ->
    ?assertEqual(undefined, beamtalk_log_format:format_domain(#{})),
    ?assertEqual(undefined, beamtalk_log_format:format_domain(#{domain => not_a_list})).

format_mfa_test() ->
    ?assertEqual(<<"lists:map/2">>, beamtalk_log_format:format_mfa(#{mfa => {lists, map, 2}})),
    ?assertEqual(undefined, beamtalk_log_format:format_mfa(#{})),
    ?assertEqual(undefined, beamtalk_log_format:format_mfa(#{mfa => {lists, map, two}})),
    ?assertEqual(undefined, beamtalk_log_format:format_mfa(#{mfa => bogus})).

format_mfa_or_default_test() ->
    ?assertEqual(<<"lists:map/2">>, beamtalk_log_format:format_mfa_or({lists, map, 2}, nil)),
    ?assertEqual(nil, beamtalk_log_format:format_mfa_or(undefined, nil)),
    ?assertEqual(nil, beamtalk_log_format:format_mfa_or({erlang, apply, x}, nil)).

report_chardata_without_callback_test() ->
    ?assertEqual(
        "#{a => 1}", lists:flatten(beamtalk_log_format:report_chardata(#{a => 1}, #{}))
    ).

report_chardata_arity_1_callback_test() ->
    Cb = fun(#{a := A}) -> {"a=~p", [A]} end,
    ?assertEqual(
        "a=1",
        lists:flatten(beamtalk_log_format:report_chardata(#{a => 1}, #{report_cb => Cb}))
    ).

report_chardata_arity_2_callback_test() ->
    Cb = fun(#{a := A}, #{single_line := true}) -> io_lib:format("one-line ~p", [A]) end,
    ?assertEqual(
        "one-line 1",
        lists:flatten(beamtalk_log_format:report_chardata(#{a => 1}, #{report_cb => Cb}))
    ).

report_chardata_non_map_report_test() ->
    ?assertEqual(
        "[{a,1}]", lists:flatten(beamtalk_log_format:report_chardata([{a, 1}], #{}))
    ).
