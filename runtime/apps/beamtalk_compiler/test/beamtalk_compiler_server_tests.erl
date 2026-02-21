%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_compiler_server internal functions.
%%
%% Tests response handlers, compile_core_erlang, flush_port_messages,
%% and gen_server edge cases (unknown call, cast, info).
%% Integration tests (compile_expression, compile, diagnostics, version)
%% are covered in beamtalk_compiler_tests.erl.

-module(beamtalk_compiler_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ---------------------------------------------------------------
%%% compile_core_erlang/1 — direct coverage of the server's function
%%% ---------------------------------------------------------------

compile_core_erlang_valid_test() ->
    CoreErlang = valid_core_erlang(),
    {ok, test_server_mod, Binary} = beamtalk_compiler_server:compile_core_erlang(CoreErlang),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

compile_core_erlang_with_warnings_test() ->
    %% A valid module should compile even if compile:forms returns warnings
    CoreErlang = valid_core_erlang(),
    Result = beamtalk_compiler_server:compile_core_erlang(CoreErlang),
    ?assertMatch({ok, _, _}, Result).

compile_core_erlang_scan_error_test() ->
    %% Use a character that core_scan cannot tokenize
    Result = beamtalk_compiler_server:compile_core_erlang(<<"\x00\x01\x02">>),
    ?assertMatch({error, _}, Result).

compile_core_erlang_parse_error_test() ->
    Result = beamtalk_compiler_server:compile_core_erlang(<<"module 'x' []\n">>),
    ?assertMatch({error, {core_parse_error, _}}, Result).

compile_core_erlang_empty_test() ->
    Result = beamtalk_compiler_server:compile_core_erlang(<<>>),
    ?assertMatch({error, _}, Result).

%%% ---------------------------------------------------------------
%%% handle_compile_response/1 — internal response handler
%%% ---------------------------------------------------------------

handle_compile_response_ok_test() ->
    Response = #{
        status => ok,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        classes => [<<"Foo">>],
        warnings => []
    },
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch(
        {ok, #{
            core_erlang := <<"core">>,
            module_name := <<"mod">>,
            classes := [<<"Foo">>],
            warnings := []
        }},
        Result
    ).

handle_compile_response_error_test() ->
    Response = #{status => error, diagnostics => [<<"parse error">>]},
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertEqual({error, [<<"parse error">>]}, Result).

handle_compile_response_unexpected_test() ->
    Response = #{status => unknown_status, data => <<"garbage">>},
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch({error, [<<"Unexpected compiler response">>]}, Result).

%%% ---------------------------------------------------------------
%%% handle_diagnostics_response/1 — internal response handler
%%% ---------------------------------------------------------------

handle_diagnostics_response_ok_test() ->
    Response = #{status => ok, diagnostics => [#{message => <<"warn">>}]},
    Result = beamtalk_compiler_server:handle_diagnostics_response(Response),
    ?assertEqual({ok, [#{message => <<"warn">>}]}, Result).

handle_diagnostics_response_error_test() ->
    Response = #{status => error, diagnostics => [<<"syntax error">>]},
    Result = beamtalk_compiler_server:handle_diagnostics_response(Response),
    ?assertEqual({error, [<<"syntax error">>]}, Result).

handle_diagnostics_response_unexpected_test() ->
    Response = #{bogus => true},
    Result = beamtalk_compiler_server:handle_diagnostics_response(Response),
    ?assertMatch({error, [<<"Unexpected compiler response">>]}, Result).

%%% ---------------------------------------------------------------
%%% handle_version_response/1 — internal response handler
%%% ---------------------------------------------------------------

handle_version_response_ok_test() ->
    Response = #{status => ok, version => <<"1.0.0">>},
    Result = beamtalk_compiler_server:handle_version_response(Response),
    ?assertEqual({ok, <<"1.0.0">>}, Result).

handle_version_response_unexpected_test() ->
    Response = #{status => error, message => <<"fail">>},
    Result = beamtalk_compiler_server:handle_version_response(Response),
    ?assertEqual({error, unexpected_response}, Result).

%%% ---------------------------------------------------------------
%%% flush_port_messages/1 — drains stale port messages
%%% ---------------------------------------------------------------

flush_port_messages_empty_test() ->
    %% No messages in mailbox — should return ok immediately
    FakePort = make_ref(),
    ok = beamtalk_compiler_server:flush_port_messages(FakePort).

flush_port_messages_drains_data_test() ->
    %% Simulate stale data messages
    FakePort = make_ref(),
    self() ! {FakePort, {data, <<"stale1">>}},
    self() ! {FakePort, {data, <<"stale2">>}},
    ok = beamtalk_compiler_server:flush_port_messages(FakePort),
    %% Mailbox should be empty now
    receive
        {FakePort, _} -> ?assert(false)
    after 0 ->
        ok
    end.

flush_port_messages_stops_at_exit_test() ->
    FakePort = make_ref(),
    self() ! {FakePort, {data, <<"stale">>}},
    self() ! {FakePort, {exit_status, 0}},
    ok = beamtalk_compiler_server:flush_port_messages(FakePort).

%%% ---------------------------------------------------------------
%%% gen_server edge cases (via running server)
%%% ---------------------------------------------------------------

gen_server_edge_test_() ->
    {setup, fun start_compiler/0, fun stop_compiler/1, [
        {"unknown call returns error", fun unknown_call/0},
        {"cast is ignored gracefully", fun cast_ignored/0},
        {"unknown info is ignored gracefully", fun unknown_info/0}
    ]}.

unknown_call() ->
    Result = gen_server:call(beamtalk_compiler_server, {bogus_request, 42}, 5000),
    ?assertEqual({error, unknown_request}, Result).

cast_ignored() ->
    %% cast should not crash the server
    gen_server:cast(beamtalk_compiler_server, {bogus_cast}),
    timer:sleep(50),
    %% Server is still alive
    ?assert(is_pid(whereis(beamtalk_compiler_server))).

unknown_info() ->
    %% Sending random message should not crash the server
    beamtalk_compiler_server ! {unexpected_message, 123},
    timer:sleep(50),
    ?assert(is_pid(whereis(beamtalk_compiler_server))).

%%% ---------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------

start_compiler() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

stop_compiler(_) ->
    ok.

valid_core_erlang() ->
    <<
        "module 'test_server_mod' ['hello'/0]\n"
        "  attributes []\n"
        "  'hello'/0 = fun () -> 'world'\n"
        "end\n"
    >>.
