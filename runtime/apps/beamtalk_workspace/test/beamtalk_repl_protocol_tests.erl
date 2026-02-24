%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tests for beamtalk_repl_protocol module

-module(beamtalk_repl_protocol_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Decode tests

decode_new_format_eval_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"eval\", \"id\": \"msg-001\", \"session\": \"alice\", \"code\": \"1 + 2\"}">>
    ),
    ?assertEqual(<<"eval">>, element(2, Msg)),
    ?assertEqual(<<"msg-001">>, element(3, Msg)),
    ?assertEqual(<<"alice">>, element(4, Msg)),
    ?assertEqual(#{<<"code">> => <<"1 + 2">>}, element(5, Msg)),
    ?assertEqual(false, beamtalk_repl_protocol:is_legacy(Msg)).

decode_new_format_clear_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"clear\", \"id\": \"msg-002\"}">>
    ),
    ?assertEqual(<<"clear">>, element(2, Msg)),
    ?assertEqual(<<"msg-002">>, element(3, Msg)),
    ?assertEqual(false, beamtalk_repl_protocol:is_legacy(Msg)).

decode_new_format_load_file_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"load-file\", \"id\": \"msg-003\", \"path\": \"examples/counter.bt\"}">>
    ),
    ?assertEqual(<<"load-file">>, element(2, Msg)),
    ?assertEqual(#{<<"path">> => <<"examples/counter.bt">>}, element(5, Msg)).

decode_new_format_bindings_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"bindings\", \"id\": \"msg-004\"}">>
    ),
    ?assertEqual(<<"bindings">>, element(2, Msg)).

decode_new_format_actors_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"actors\"}">>
    ),
    ?assertEqual(<<"actors">>, element(2, Msg)),
    ?assertEqual(undefined, element(3, Msg)).

decode_new_format_sessions_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"sessions\", \"id\": \"msg-005\"}">>
    ),
    ?assertEqual(<<"sessions">>, element(2, Msg)).

decode_new_format_inspect_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"inspect\", \"id\": \"msg-006\", \"actor\": \"<0.123.0>\"}">>
    ),
    ?assertEqual(<<"inspect">>, element(2, Msg)),
    ?assertEqual(#{<<"actor">> => <<"<0.123.0>">>}, element(5, Msg)).

decode_new_format_complete_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"complete\", \"id\": \"msg-007\", \"code\": \"Coun\"}">>
    ),
    ?assertEqual(<<"complete">>, element(2, Msg)),
    ?assertEqual(#{<<"code">> => <<"Coun">>}, element(5, Msg)).

decode_new_format_info_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"info\", \"id\": \"msg-008\", \"symbol\": \"Counter\"}">>
    ),
    ?assertEqual(<<"info">>, element(2, Msg)),
    ?assertEqual(#{<<"symbol">> => <<"Counter">>}, element(5, Msg)).

decode_new_format_health_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"health\"}">>
    ),
    ?assertEqual(<<"health">>, beamtalk_repl_protocol:get_op(Msg)),
    ?assertEqual(#{}, beamtalk_repl_protocol:get_params(Msg)).

decode_new_format_shutdown_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"shutdown\", \"cookie\": \"secret123\"}">>
    ),
    ?assertEqual(<<"shutdown">>, beamtalk_repl_protocol:get_op(Msg)),
    ?assertEqual(#{<<"cookie">> => <<"secret123">>}, beamtalk_repl_protocol:get_params(Msg)).

%%% Legacy format decode tests

decode_legacy_eval_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"eval\", \"expression\": \"1 + 2\"}">>
    ),
    ?assertEqual(<<"eval">>, element(2, Msg)),
    ?assertEqual(#{<<"code">> => <<"1 + 2">>}, element(5, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_legacy_clear_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"clear\"}">>
    ),
    ?assertEqual(<<"clear">>, element(2, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_legacy_load_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"load\", \"path\": \"examples/counter.bt\"}">>
    ),
    ?assertEqual(<<"load-file">>, element(2, Msg)),
    ?assertEqual(#{<<"path">> => <<"examples/counter.bt">>}, element(5, Msg)),
    ?assertEqual(true, beamtalk_repl_protocol:is_legacy(Msg)).

decode_legacy_bindings_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"bindings\"}">>
    ),
    ?assertEqual(<<"bindings">>, element(2, Msg)).

decode_legacy_actors_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"actors\"}">>
    ),
    ?assertEqual(<<"actors">>, element(2, Msg)).

decode_legacy_modules_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"modules\"}">>
    ),
    ?assertEqual(<<"modules">>, element(2, Msg)).

decode_legacy_kill_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"kill\", \"pid\": \"<0.123.0>\"}">>
    ),
    ?assertEqual(<<"kill">>, element(2, Msg)),
    ?assertEqual(#{<<"actor">> => <<"<0.123.0>">>}, element(5, Msg)).

decode_legacy_unload_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"unload\", \"module\": \"Counter\"}">>
    ),
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

encode_error_with_output_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-012">>, <<"alice">>, false),
    Result = beamtalk_repl_protocol:encode_error(
        some_error, Msg, fun(R) -> list_to_binary(io_lib:format("~p", [R])) end, <<"Captured\n">>
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-012">>, maps:get(<<"id">>, Decoded)),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assertEqual(<<"Captured\n">>, maps:get(<<"output">>, Decoded)),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

encode_error_with_empty_output_omitted_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-013">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_error(
        some_error, Msg, fun(R) -> list_to_binary(io_lib:format("~p", [R])) end, <<>>
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(error, maps:find(<<"output">>, Decoded)).

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

%%% Accessor tests

get_id_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"eval\", \"id\": \"my-id\", \"code\": \"1\"}">>
    ),
    ?assertEqual(<<"my-id">>, beamtalk_repl_protocol:get_id(Msg)).

get_id_undefined_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"eval\", \"code\": \"1\"}">>
    ),
    ?assertEqual(undefined, beamtalk_repl_protocol:get_id(Msg)).

get_session_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"eval\", \"session\": \"s99\", \"code\": \"1\"}">>
    ),
    ?assertEqual(<<"s99">>, beamtalk_repl_protocol:get_session(Msg)).

get_session_undefined_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\": \"actors\"}">>
    ),
    ?assertEqual(undefined, beamtalk_repl_protocol:get_session(Msg)).

%%% Encode result/error with warnings

encode_result_with_warnings_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-w1">>, <<"alice">>, false),
    Result = beamtalk_repl_protocol:encode_result(
        42, Msg, fun identity/1, <<"out">>, [<<"warn1">>, <<"warn2">>]
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual(<<"out">>, maps:get(<<"output">>, Decoded)),
    ?assertEqual([<<"warn1">>, <<"warn2">>], maps:get(<<"warnings">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_result_with_warnings_legacy_format_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_result(
        99, Msg, fun identity/1, <<>>, [<<"w1">>]
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(99, maps:get(<<"value">>, Decoded)),
    ?assertEqual([<<"w1">>], maps:get(<<"warnings">>, Decoded)),
    ?assertEqual(error, maps:find(<<"output">>, Decoded)).

encode_result_with_empty_warnings_omitted_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-w2">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_result(1, Msg, fun identity/1, <<>>, []),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

encode_error_with_warnings_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-e1">>, <<"bob">>, false),
    Result = beamtalk_repl_protocol:encode_error(
        bad, Msg, fun format_err/1, <<"captured">>, [<<"w1">>]
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    ?assertEqual(<<"captured">>, maps:get(<<"output">>, Decoded)),
    ?assertEqual([<<"w1">>], maps:get(<<"warnings">>, Decoded)),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

encode_error_with_warnings_legacy_format_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_error(
        oops, Msg, fun format_err/1, <<"out">>, [<<"w">>]
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"out">>, maps:get(<<"output">>, Decoded)),
    ?assertEqual([<<"w">>], maps:get(<<"warnings">>, Decoded)).

encode_error_with_empty_warnings_omitted_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-e2">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_error(err, Msg, fun format_err/1, <<>>, []),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

%%% encode_inspect/2 (binary version)

encode_inspect_binary_new_format_test() ->
    Msg = make_msg(<<"inspect">>, <<"msg-i1">>, <<"s1">>, false),
    Result = beamtalk_repl_protocol:encode_inspect(<<"state_str">>, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-i1">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"state_str">>, maps:get(<<"state">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_inspect_binary_legacy_format_test() ->
    Msg = make_msg(<<"inspect">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_inspect(<<"state_str">>, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"inspect">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"state_str">>, maps:get(<<"state">>, Decoded)).

%%% encode_inspect/3 (map version)

encode_inspect_map_new_format_test() ->
    Msg = make_msg(<<"inspect">>, <<"msg-i2">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_inspect(
        #{count => 42, name => <<"foo">>}, Msg, fun identity/1
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    StateMap = maps:get(<<"state">>, Decoded),
    ?assertEqual(42, maps:get(<<"count">>, StateMap)),
    ?assertEqual(<<"foo">>, maps:get(<<"name">>, StateMap)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_inspect_map_legacy_format_test() ->
    Msg = make_msg(<<"inspect">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_inspect(
        #{x => 1}, Msg, fun identity/1
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"inspect">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(1, maps:get(<<"x">>, maps:get(<<"state">>, Decoded))).

%%% encode_docs/2

encode_docs_new_format_test() ->
    Msg = make_msg(<<"docs">>, <<"msg-d1">>, <<"s1">>, false),
    Result = beamtalk_repl_protocol:encode_docs(<<"Integer: A whole number">>, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-d1">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"Integer: A whole number">>, maps:get(<<"docs">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

encode_docs_legacy_format_test() ->
    Msg = make_msg(<<"docs">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_docs(<<"Some docs">>, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"docs">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Some docs">>, maps:get(<<"docs">>, Decoded)).

%%% encode_actors/3 new format

encode_actors_new_format_test() ->
    Msg = make_msg(<<"actors">>, <<"msg-a1">>, undefined, false),
    Actors = [#{pid => self(), class => 'Counter', module => counter, spawned_at => 12345}],
    Result = beamtalk_repl_protocol:encode_actors(Actors, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    ActorList = maps:get(<<"actors">>, Decoded),
    ?assertEqual(1, length(ActorList)),
    [A] = ActorList,
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, A)),
    ?assertEqual(<<"counter">>, maps:get(<<"module">>, A)).

%%% encode_modules/3 new format

encode_modules_new_format_test() ->
    Msg = make_msg(<<"modules">>, <<"msg-m1">>, undefined, false),
    Modules = [
        {counter, #{
            name => <<"Counter">>,
            source_file => "/tmp/c.bt",
            actor_count => 2,
            load_time => 99,
            time_ago => "5s ago"
        }}
    ],
    Result = beamtalk_repl_protocol:encode_modules(Modules, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    [M] = maps:get(<<"modules">>, Decoded),
    ?assertEqual(<<"Counter">>, maps:get(<<"name">>, M)),
    ?assertEqual(<<"/tmp/c.bt">>, maps:get(<<"source_file">>, M)).

%%% encode_sessions/3 legacy format

encode_sessions_legacy_format_test() ->
    Msg = make_msg(<<"sessions">>, undefined, undefined, true),
    Sessions = [#{id => <<"s1">>}],
    Result = beamtalk_repl_protocol:encode_sessions(Sessions, Msg, fun identity/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"sessions">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(1, length(maps:get(<<"sessions">>, Decoded))).

%%% base_response edge cases

base_response_with_both_id_and_session_test() ->
    Msg = make_msg(<<"eval">>, <<"id1">>, <<"sess1">>, false),
    Base = beamtalk_repl_protocol:base_response(Msg),
    ?assertEqual(<<"id1">>, maps:get(<<"id">>, Base)),
    ?assertEqual(<<"sess1">>, maps:get(<<"session">>, Base)).

base_response_with_no_id_no_session_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, false),
    Base = beamtalk_repl_protocol:base_response(Msg),
    ?assertEqual(error, maps:find(<<"id">>, Base)),
    ?assertEqual(error, maps:find(<<"session">>, Base)).

%%% decode edge cases

decode_missing_op_and_type_test() ->
    Result = beamtalk_repl_protocol:decode(<<"{\"foo\": \"bar\"}">>),
    ?assertMatch({error, {invalid_request, missing_op_or_type}}, Result).

decode_legacy_unknown_type_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"type\": \"unknown_op\"}">>
    ),
    ?assertEqual(<<"unknown_op">>, beamtalk_repl_protocol:get_op(Msg)),
    ?assertEqual(#{}, beamtalk_repl_protocol:get_params(Msg)).

%%% encode_status legacy format

encode_status_legacy_format_test() ->
    Msg = make_msg(<<"clear">>, undefined, undefined, true),
    Result = beamtalk_repl_protocol:encode_status(ok, Msg, fun atom_to_binary/1),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"ok">>, maps:get(<<"value">>, Decoded)).

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

%%% encode_describe tests

encode_describe_new_format_test() ->
    Msg = make_msg(<<"describe">>, <<"msg-040">>, undefined, false),
    Ops = #{
        <<"eval">> => #{<<"params">> => [<<"code">>]},
        <<"describe">> => #{<<"params">> => []}
    },
    Versions = #{<<"protocol">> => <<"1.0">>, <<"beamtalk">> => <<"0.1.0">>},
    Result = beamtalk_repl_protocol:encode_describe(Ops, Versions, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-040">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    ?assert(is_map(maps:get(<<"ops">>, Decoded))),
    ?assertEqual(
        #{<<"params">> => [<<"code">>]},
        maps:get(<<"eval">>, maps:get(<<"ops">>, Decoded))
    ),
    ?assertEqual(
        #{<<"protocol">> => <<"1.0">>, <<"beamtalk">> => <<"0.1.0">>},
        maps:get(<<"versions">>, Decoded)
    ).

encode_describe_legacy_format_test() ->
    Msg = make_msg(<<"describe">>, undefined, undefined, true),
    Ops = #{<<"health">> => #{<<"params">> => []}},
    Versions = #{<<"protocol">> => <<"1.0">>, <<"beamtalk">> => <<"0.1.0">>},
    Result = beamtalk_repl_protocol:encode_describe(Ops, Versions, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"describe">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_map(maps:get(<<"ops">>, Decoded))),
    ?assertEqual(
        #{<<"protocol">> => <<"1.0">>, <<"beamtalk">> => <<"0.1.0">>},
        maps:get(<<"versions">>, Decoded)
    ).

%%% encode_need_input tests (BT-698)

encode_need_input_new_format_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-050">>, <<"sess1">>, false),
    Result = beamtalk_repl_protocol:encode_need_input(<<"Name: ">>, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-050">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"sess1">>, maps:get(<<"session">>, Decoded)),
    ?assertEqual([<<"need-input">>], maps:get(<<"status">>, Decoded)),
    ?assertEqual(<<"Name: ">>, maps:get(<<"prompt">>, Decoded)).

encode_need_input_no_session_test() ->
    Msg = make_msg(<<"eval">>, <<"msg-051">>, undefined, false),
    Result = beamtalk_repl_protocol:encode_need_input(<<"? ">>, Msg),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"msg-051">>, maps:get(<<"id">>, Decoded)),
    ?assertEqual([<<"need-input">>], maps:get(<<"status">>, Decoded)),
    ?assertEqual(<<"? ">>, maps:get(<<"prompt">>, Decoded)),
    ?assertEqual(error, maps:find(<<"session">>, Decoded)).
