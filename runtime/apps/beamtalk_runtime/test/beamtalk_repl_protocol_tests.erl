%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tests for beamtalk_repl_protocol module

-module(beamtalk_repl_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Decode tests

decode_new_format_eval_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"eval\", \"id\": \"msg-001\", \"session\": \"alice\", \"code\": \"1 + 2\"}">>),
    ?assertEqual(<<"eval">>, element(2, Msg)),
    ?assertEqual(<<"msg-001">>, element(3, Msg)),
    ?assertEqual(<<"alice">>, element(4, Msg)),
    ?assertEqual(#{<<"code">> => <<"1 + 2">>}, element(5, Msg)),
    ?assertEqual(false, beamtalk_repl_protocol:is_legacy(Msg)).

decode_new_format_clear_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"clear\", \"id\": \"msg-002\"}">>),
    ?assertEqual(<<"clear">>, element(2, Msg)),
    ?assertEqual(<<"msg-002">>, element(3, Msg)),
    ?assertEqual(false, beamtalk_repl_protocol:is_legacy(Msg)).

decode_new_format_load_file_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"load-file\", \"id\": \"msg-003\", \"path\": \"examples/counter.bt\"}">>),
    ?assertEqual(<<"load-file">>, element(2, Msg)),
    ?assertEqual(#{<<"path">> => <<"examples/counter.bt">>}, element(5, Msg)).

decode_new_format_bindings_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"bindings\", \"id\": \"msg-004\"}">>),
    ?assertEqual(<<"bindings">>, element(2, Msg)).

decode_new_format_actors_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"actors\"}">>),
    ?assertEqual(<<"actors">>, element(2, Msg)),
    ?assertEqual(undefined, element(3, Msg)).

decode_new_format_sessions_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"sessions\", \"id\": \"msg-005\"}">>),
    ?assertEqual(<<"sessions">>, element(2, Msg)).

decode_new_format_inspect_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"inspect\", \"id\": \"msg-006\", \"actor\": \"<0.123.0>\"}">>),
    ?assertEqual(<<"inspect">>, element(2, Msg)),
    ?assertEqual(#{<<"actor">> => <<"<0.123.0>">>}, element(5, Msg)).

decode_new_format_complete_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"complete\", \"id\": \"msg-007\", \"code\": \"Coun\"}">>),
    ?assertEqual(<<"complete">>, element(2, Msg)),
    ?assertEqual(#{<<"code">> => <<"Coun">>}, element(5, Msg)).

decode_new_format_info_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"info\", \"id\": \"msg-008\", \"symbol\": \"Counter\"}">>),
    ?assertEqual(<<"info">>, element(2, Msg)),
    ?assertEqual(#{<<"symbol">> => <<"Counter">>}, element(5, Msg)).

%%% Legacy format decode tests

decode_legacy_eval_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"eval\", \"expression\": \"1 + 2\"}">>),
    ?assertEqual(<<"eval">>, element(2, Msg)),
    ?assertEqual(#{<<"code">> => <<"1 + 2">>}, element(5, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_legacy_clear_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"clear\"}">>),
    ?assertEqual(<<"clear">>, element(2, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_legacy_load_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"load\", \"path\": \"examples/counter.bt\"}">>),
    ?assertEqual(<<"load-file">>, element(2, Msg)),
    ?assertEqual(#{<<"path">> => <<"examples/counter.bt">>}, element(5, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_legacy_bindings_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"bindings\"}">>),
    ?assertEqual(<<"bindings">>, element(2, Msg)).

decode_legacy_actors_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"actors\"}">>),
    ?assertEqual(<<"actors">>, element(2, Msg)).

decode_legacy_modules_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"modules\"}">>),
    ?assertEqual(<<"modules">>, element(2, Msg)).

decode_legacy_kill_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"kill\", \"pid\": \"<0.123.0>\"}">>),
    ?assertEqual(<<"kill">>, element(2, Msg)),
    ?assertEqual(#{<<"actor">> => <<"<0.123.0>">>}, element(5, Msg)).

decode_legacy_unload_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"unload\", \"module\": \"Counter\"}">>),
    ?assertEqual(<<"unload">>, element(2, Msg)),
    ?assertEqual(#{<<"module">> => <<"Counter">>}, element(5, Msg)).

%%% Raw expression decode

decode_raw_expression_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(<<"1 + 2">>),
    ?assertEqual(<<"eval">>, element(2, Msg)),
    ?assertEqual(#{<<"code">> => <<"1 + 2">>}, element(5, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_empty_expression_test() ->
    ?assertEqual({error, empty_expression}, beamtalk_repl_protocol:decode(<<>>)).

decode_whitespace_only_test() ->
    ?assertEqual({error, empty_expression}, beamtalk_repl_protocol:decode(<<"   ">>)).

%%% Encode tests (new format)

encode_result_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-001">>, <<"alice">>, false),
    Result = beamtalk_repl_protocol:encode_result(42, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-001">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"alice">>, maps:get(<<"session">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_error_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-002">>, <<"bob">>, false),
    Result = beamtalk_repl_protocol:encode_error(some_error, Msg, fun format_err/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-002">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"bob">>, maps:get(<<"session">>, Decoded)),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

encode_status_new_format_test() ->
    Msg = make_msg(<<"clear">>, <<"msg-003">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_status(ok, Msg, fun atom_to_binary/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-003">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"ok">>, maps:get(<<"value">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    ?assertEqual(error, maps:find(<<"session">>, Decoded)).

encode_bindings_new_format_test() ->
    Msg = make_msg(<<"bindings">>, <<"msg-004">>, undefined, false),
    Bindings = #{x => 42, y => <<"hello">>},
    Result = beamtalk_repl_protocol:encode_bindings(Bindings, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    BindingsMap = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"x">>, BindingsMap)),
    ?assertEqual(<<"hello">>, maps:get(<<"y">>, BindingsMap)).

encode_loaded_new_format_test() ->
    Msg = make_msg(<<"load-file">>, <<"msg-005">>, undefined, false),
    Classes = [#{name => "Counter"}, #{name => "Logger"}],
    Result = beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    ?assertEqual([<<"Counter">>, <<"Logger">>], maps:get(<<"classes">>, Decoded)).

encode_sessions_new_format_test() ->
    Msg = make_msg(<<"sessions">>, <<"msg-006">>, undefined, false),
    Sessions = [#{id => <<"s1">>}, #{id => <<"s2">>, created_at => 12345}],
    Result = beamtalk_repl_protocol:encode_sessions(Sessions, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    SessionList = maps:get(<<"sessions">>, Decoded),
    ?assertEqual(2, length(SessionList)).

%%% Encode tests (legacy format)

encode_result_legacy_format_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_result(42, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual(error, maps:find(<<"status">>, Decoded)).

encode_error_legacy_format_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_error(some_error, Msg, fun format_err/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))),
    ?assertEqual(error, maps:find(<<"status">>, Decoded)).

encode_bindings_legacy_format_test() ->
    Msg = make_msg(<<"bindings">>, undefined, undefined, true),
    Bindings = #{x => 42},
    Result = beamtalk_repl_protocol:encode_bindings(Bindings, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"bindings">>, maps:get(<<"type">>, Decoded)).

encode_loaded_legacy_format_test() ->
    Msg = make_msg(<<"load-file">>, undefined, undefined, true),
    Classes = [#{name => "Counter"}],
    Result = beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"loaded">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([<<"Counter">>], maps:get(<<"classes">>, Decoded)).

encode_actors_legacy_format_test() ->
    Msg = make_msg(<<"actors">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_actors([], Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"actors">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"actors">>, Decoded)).

encode_modules_legacy_format_test() ->
    Msg = make_msg(<<"modules">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_modules([], Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"modules">>, Decoded)).

%%% Encode tests (with output field)

encode_result_with_output_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-010">>, <<"alice">>, false),
    Result = beamtalk_repl_protocol:encode_result(42, Msg, fun identity/1, <<"Hello\n">>),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-010">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual(<<"Hello\n">>, maps:get(<<"output">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_result_with_empty_output_omitted_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-011">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_result(42, Msg, fun identity/1, <<>>),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    %% Empty output should NOT appear in response
    ?assertEqual(error, maps:find(<<"output">>, Decoded)).

encode_result_with_output_legacy_format_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_result(<<"ok">>, Msg, fun identity/1, <<"World">>),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"ok">>, maps:get(<<"value">>, Decoded)),
    ?assertEqual(<<"World">>, maps:get(<<"output">>, Decoded)).

%%% Roundtrip tests

roundtrip_new_format_test() ->
    Input = <<"{\"op\": \"eval\", \"id\": \"rt-1\", \"session\": \"s1\", \"code\": \"42\"}">>,
    {ok, Msg} = beamtalk_repl_protocol:decode(Input),
    Result = beamtalk_repl_protocol:encode_result(42, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    %% Response should echo id and session
    ?assertEqual(<<"rt-1">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"s1">>, maps:get(<<"session">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

roundtrip_legacy_format_test() ->
    Input = <<"{\"type\": \"eval\", \"expression\": \"42\"}">>,
    {ok, Msg} = beamtalk_repl_protocol:decode(Input),
    Result = beamtalk_repl_protocol:encode_result(42, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    %% Response should use legacy format
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(error, maps:find(<<"status">>, Decoded)).

%%% Helpers

make_msg(Op, Id, Session, Legacy) ->
    {protocol_msg, Op, Id, Session, #{}, Legacy}.

identity(X) -> X.

format_err(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

atom_to_binary(A) ->
    erlang:atom_to_binary(A, utf8).

%%% R7: Non-map JSON decode tests

decode_json_array_returns_error_test() ->
    Result = beamtalk_repl_protocol:decode(<<"[1, 2, 3]">>),
    ?assertMatch({error, {invalid_request, non_object_json}}, Result).

decode_json_string_returns_error_test() ->
    Result = beamtalk_repl_protocol:decode(<<"\"hello\"">>),
    ?assertMatch({error, {invalid_request, non_object_json}}, Result).

decode_json_number_returns_error_test() ->
    Result = beamtalk_repl_protocol:decode(<<"42">>),
    ?assertMatch({error, {invalid_request, non_object_json}}, Result).
