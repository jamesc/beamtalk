%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** REPL Session Context

%%% @doc Unit tests for beamtalk_repl_server module
%%%
%%% Tests TCP client handling, JSON protocol parsing, and server-side
%%% request/response behaviour. Formatting tests live in
%%% beamtalk_repl_json_tests.

-module(beamtalk_repl_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Request parsing tests

parse_request_eval_test() ->
    Request = <<"{\"type\": \"eval\", \"expression\": \"1 + 2\"}">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_clear_test() ->
    Request = <<"{\"type\": \"clear\"}">>,
    ?assertEqual({clear_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_bindings_test() ->
    Request = <<"{\"type\": \"bindings\"}">>,
    ?assertEqual({get_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_load_test() ->
    Request = <<"{\"type\": \"load\", \"path\": \"examples/counter.bt\"}">>,
    ?assertEqual({load_file, "examples/counter.bt"}, beamtalk_repl_server:parse_request(Request)).

parse_request_load_source_test() ->
    Request = <<"{\"op\": \"load-source\", \"source\": \"Object subclass: Foo\"}">>,
    ?assertEqual(
        {load_source, <<"Object subclass: Foo">>}, beamtalk_repl_server:parse_request(Request)
    ).

parse_request_with_newline_test() ->
    Request = <<"{\"type\": \"eval\", \"expression\": \"1 + 2\"}\n">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_raw_expression_test() ->
    %% Backwards compatibility: non-JSON is treated as eval
    Request = <<"1 + 2">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_empty_test() ->
    Request = <<"">>,
    ?assertMatch({error, empty_expression}, beamtalk_repl_server:parse_request(Request)).

parse_request_unknown_type_test() ->
    Request = <<"{\"type\": \"unknown\"}">>,
    ?assertMatch(
        {error, {invalid_request, unknown_type}}, beamtalk_repl_server:parse_request(Request)
    ).

parse_request_missing_expression_test() ->
    Request = <<"{\"type\": \"eval\"}">>,
    %% Missing expression field - parser can't extract the expression value
    %% so it returns an invalid_request error
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {invalid_request, _}}, Result).

%%% New protocol format (op) parsing tests

parse_request_op_eval_test() ->
    Request = <<"{\"op\": \"eval\", \"id\": \"msg-001\", \"code\": \"1 + 2\"}">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_clear_test() ->
    Request = <<"{\"op\": \"clear\", \"id\": \"msg-002\"}">>,
    ?assertEqual({clear_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_bindings_test() ->
    Request = <<"{\"op\": \"bindings\"}">>,
    ?assertEqual({get_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_load_file_test() ->
    Request = <<"{\"op\": \"load-file\", \"path\": \"examples/counter.bt\"}">>,
    ?assertEqual({load_file, "examples/counter.bt"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_actors_test() ->
    Request = <<"{\"op\": \"actors\"}">>,
    ?assertEqual({list_actors}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_modules_test() ->
    Request = <<"{\"op\": \"modules\"}">>,
    ?assertEqual({list_modules}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_kill_test() ->
    Request = <<"{\"op\": \"kill\", \"actor\": \"<0.123.0>\"}">>,
    ?assertEqual({kill_actor, "<0.123.0>"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_unload_test() ->
    %% BT-1239: unload op restored — parses module name for full class removal.
    Request = <<"{\"op\": \"unload\", \"module\": \"Counter\"}">>,
    ?assertMatch({unload, <<"Counter">>}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_unknown_test() ->
    Request = <<"{\"op\": \"foobar\"}">>,
    ?assertMatch({error, {unknown_op, <<"foobar">>}}, beamtalk_repl_server:parse_request(Request)).

%%% Health and shutdown protocol tests (BT-611)

parse_request_op_health_test() ->
    Request = <<"{\"op\": \"health\"}">>,
    ?assertEqual({health}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_shutdown_test() ->
    Request = <<"{\"op\": \"shutdown\", \"cookie\": \"my_secret_cookie\"}">>,
    ?assertEqual({shutdown, "my_secret_cookie"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_shutdown_no_cookie_test() ->
    Request = <<"{\"op\": \"shutdown\"}">>,
    ?assertEqual({shutdown, ""}, beamtalk_repl_server:parse_request(Request)).

%%% Nonce generation tests (BT-611)

generate_nonce_returns_binary_test() ->
    Nonce = beamtalk_repl_server:generate_nonce(),
    ?assert(is_binary(Nonce)),
    %% 8 random bytes → 16 hex chars
    ?assertEqual(16, byte_size(Nonce)).

generate_nonce_is_unique_test() ->
    Nonce1 = beamtalk_repl_server:generate_nonce(),
    Nonce2 = beamtalk_repl_server:generate_nonce(),
    ?assertNotEqual(Nonce1, Nonce2).

generate_nonce_is_hex_test() ->
    Nonce = beamtalk_repl_server:generate_nonce(),
    %% Verify all characters are hex digits
    IsHex = lists:all(
        fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
        binary_to_list(Nonce)
    ),
    ?assert(IsHex).

%%% JSON parsing tests (internal parser)

parse_json_not_json_test() ->
    %% Non-JSON should be treated as raw expression
    NotJson = <<"not json at all">>,
    ?assertEqual({eval, "not json at all"}, beamtalk_repl_server:parse_request(NotJson)).

%%% Request parsing edge cases

parse_request_whitespace_only_test() ->
    Request = <<"   \n\t  ">>,
    Result = beamtalk_repl_server:parse_request(Request),
    %% Whitespace-only is treated as empty after stripping
    ?assertMatch({error, empty_expression}, Result).

parse_request_json_with_extra_fields_test() ->
    %% JSON with extra fields should be ignored
    Request = <<"{\"type\": \"eval\", \"expression\": \"1 + 1\", \"extra\": \"ignored\"}">>,
    ?assertEqual({eval, "1 + 1"}, beamtalk_repl_server:parse_request(Request)).

parse_request_load_with_spaces_in_path_test() ->
    Request = <<"{\"type\": \"load\", \"path\": \"/path/with spaces/file.bt\"}">>,
    ?assertEqual(
        {load_file, "/path/with spaces/file.bt"}, beamtalk_repl_server:parse_request(Request)
    ).

parse_request_string_literal_with_escaped_quotes_test() ->
    %% Test parsing string literal with escaped quotes (BT-227 regression test)
    %% The expression "\"hello\"" contains escaped quotes that must be parsed correctly
    Request = <<"{\"type\": \"eval\", \"expression\": \"\\\"hello\\\"\"}">>,
    ?assertEqual({eval, "\"hello\""}, beamtalk_repl_server:parse_request(Request)).

parse_request_malformed_json_test() ->
    %% Malformed JSON should fall back to raw expression (backwards compatibility)
    Request = <<"{\"type\": incomplete">>,
    ?assertEqual({eval, "{\"type\": incomplete"}, beamtalk_repl_server:parse_request(Request)).

parse_request_unicode_test() ->
    %% Test with unicode characters
    Request = <<"{\"type\": \"eval\", \"expression\": \"greeting := \\\"你好\\\"\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({eval, _}, Result).

%%% BT-520: safe_to_existing_atom tests

safe_to_existing_atom_existing_test() ->
    %% 'ok' is a well-known existing atom
    ?assertEqual({ok, ok}, beamtalk_repl_server:safe_to_existing_atom(<<"ok">>)).

safe_to_existing_atom_nonexistent_test() ->
    %% A random string should fail
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_server:safe_to_existing_atom(<<"xyzzy_nonexistent_atom_9999">>)
    ).

safe_to_existing_atom_empty_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_server:safe_to_existing_atom(<<>>)).

safe_to_existing_atom_non_binary_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_server:safe_to_existing_atom(123)).

safe_to_existing_atom_integer_atom_test() ->
    %% 'Integer' is a well-known existing atom in beamtalk
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_server:safe_to_existing_atom(<<"Integer">>)).

%%% BT-520: generate_session_id tests (via parse_request with session operations)

%% Test that parse_request handles op "sessions"
parse_request_op_sessions_test() ->
    Request = <<"{\"op\": \"sessions\"}">>,
    %% sessions doesn't map via op_to_request - it's handled in handle_op
    %% The legacy parse doesn't have a sessions type, so this goes through op route
    Result = beamtalk_repl_server:parse_request(Request),
    %% sessions isn't in op_to_request, so it's an unknown_op
    ?assertMatch({error, {unknown_op, <<"sessions">>}}, Result).

%% Test that parse_request handles op "clone"
parse_request_op_clone_test() ->
    Request = <<"{\"op\": \"clone\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"clone">>}}, Result).

%% Test that parse_request handles op "close"
parse_request_op_close_test() ->
    Request = <<"{\"op\": \"close\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"close">>}}, Result).

%% Test that parse_request handles op "inspect"
parse_request_op_inspect_test() ->
    Request = <<"{\"op\": \"inspect\", \"actor\": \"<0.1.0>\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"inspect">>}}, Result).

%% Test that parse_request handles op "reload"
parse_request_op_reload_test() ->
    Request = <<"{\"op\": \"reload\", \"module\": \"Counter\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"reload">>}}, Result).

%% Test that parse_request handles op "complete"
parse_request_op_complete_test() ->
    Request = <<"{\"op\": \"complete\", \"code\": \"Cou\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"complete">>}}, Result).

%% Test that parse_request handles op "docs"
parse_request_op_docs_test() ->
    Request = <<"{\"op\": \"docs\", \"class\": \"Integer\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({get_docs, <<"Integer">>, undefined}, Result).

parse_request_op_docs_with_selector_test() ->
    Request = <<"{\"op\": \"docs\", \"class\": \"Integer\", \"selector\": \"+\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({get_docs, <<"Integer">>, <<"+">>}, Result).

%%% BT-520: Additional parse_request edge cases

parse_request_nested_json_expression_test() ->
    Request = <<"{\"type\": \"eval\", \"expression\": \"#{x => 1, y => 2}\"}">>,
    ?assertMatch({eval, _}, beamtalk_repl_server:parse_request(Request)).

parse_request_very_long_expression_test() ->
    LongExpr = list_to_binary(lists:duplicate(1000, $a)),
    Request = jsx:encode(#{<<"type">> => <<"eval">>, <<"expression">> => LongExpr}),
    ?assertMatch({eval, _}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_with_id_and_session_test() ->
    Request = <<"{\"op\": \"eval\", \"id\": \"msg-123\", \"session\": \"s1\", \"code\": \"42\"}">>,
    ?assertEqual({eval, "42"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_load_file_missing_path_test() ->
    Request = <<"{\"op\": \"load-file\"}">>,
    ?assertEqual({load_file, ""}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_unload_missing_module_test() ->
    %% BT-1239: unload op restored — missing module defaults to empty binary.
    Request = <<"{\"op\": \"unload\"}">>,
    ?assertMatch({unload, <<>>}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_kill_with_pid_field_test() ->
    Request = <<"{\"op\": \"kill\", \"pid\": \"<0.99.0>\"}">>,
    ?assertEqual({kill_actor, "<0.99.0>"}, beamtalk_repl_server:parse_request(Request)).

%%% BT-520: Legacy type-format parse_request tests (covering lines 499-506)

parse_request_type_actors_test() ->
    Request = <<"{\"type\": \"actors\"}">>,
    ?assertEqual({list_actors}, beamtalk_repl_server:parse_request(Request)).

parse_request_type_modules_test() ->
    Request = <<"{\"type\": \"modules\"}">>,
    ?assertEqual({list_modules}, beamtalk_repl_server:parse_request(Request)).

parse_request_type_unload_test() ->
    %% BT-785: :unload removed — legacy type "unload" no longer recognized
    Request = <<"{\"type\": \"unload\", \"module\": \"Counter\"}">>,
    ?assertEqual(
        {error, {invalid_request, unknown_type}}, beamtalk_repl_server:parse_request(Request)
    ).

parse_request_type_kill_test() ->
    Request = <<"{\"type\": \"kill\", \"pid\": \"<0.1.0>\"}">>,
    ?assertEqual({kill_actor, "<0.1.0>"}, beamtalk_repl_server:parse_request(Request)).

%%% BT-520: parse_request catching exceptions (covering line 519)

parse_request_binary_causes_crash_test() ->
    %% Test that parse_request doesn't crash on truly bizarre input
    %% Even if something throws during parsing, it should return error
    Result = beamtalk_repl_server:parse_request(<<0>>),
    %% Should either parse as raw expression or return error
    ?assert(element(1, Result) =:= eval orelse element(1, Result) =:= error).

%% Generator: runs all TCP integration tests within a single workspace instance
tcp_integration_test_() ->
    {setup, fun tcp_setup/0, fun tcp_cleanup/1, fun({Port, _SupPid}) ->
        [
            {"clear op", fun() -> tcp_clear_test(Port) end},
            {"bindings op (empty)", fun() -> tcp_bindings_empty_test(Port) end},
            {"actors op (empty)", fun() -> tcp_actors_empty_test(Port) end},
            {"sessions op", fun() -> tcp_sessions_test(Port) end},
            {"close op", fun() -> tcp_close_test(Port) end},
            {"complete op (empty prefix)", fun() -> tcp_complete_empty_test(Port) end},
            {"complete op (with prefix)", fun() -> tcp_complete_prefix_test(Port) end},
            {"unknown op", fun() -> tcp_unknown_op_test(Port) end},
            {"empty eval op", fun() -> tcp_eval_empty_test(Port) end},
            {"simple eval op", fun() -> tcp_eval_simple_test(Port) end},
            {"unload nonexistent", fun() -> tcp_unload_nonexistent_test(Port) end},
            {"unload empty module", fun() -> tcp_unload_empty_test(Port) end},
            {"unload in-use module", fun() -> tcp_unload_in_use_test(Port) end},
            {"inspect invalid pid", fun() -> tcp_inspect_invalid_pid_test(Port) end},
            {"kill invalid pid", fun() -> tcp_kill_invalid_pid_test(Port) end},
            {"reload module not loaded", fun() -> tcp_reload_module_not_loaded_test(Port) end},
            {"docs unknown class", fun() -> tcp_docs_unknown_class_test(Port) end},
            {"modules op", fun() -> tcp_modules_test(Port) end},
            {"clone op", fun() -> tcp_clone_test(Port) end},
            {"inspect dead actor", fun() -> tcp_inspect_dead_actor_test(Port) end},
            {"malformed json", fun() -> tcp_malformed_json_test(Port) end},
            {"raw expression", fun() -> tcp_raw_expression_test(Port) end},
            %% BT-523: TCP connection lifecycle tests
            {"multiple sequential connects", fun() -> tcp_multiple_connects_test(Port) end},
            {"concurrent clients", fun() -> tcp_concurrent_clients_test(Port) end},
            {"client disconnect", fun() -> tcp_client_disconnect_test(Port) end},
            {"multi request same connection", fun() -> tcp_multi_request_same_conn_test(Port) end},
            %% BT-523: Connection error handling tests
            {"empty line", fun() -> tcp_empty_line_test(Port) end},
            {"binary garbage", fun() -> tcp_binary_garbage_test(Port) end},
            {"reload empty module", fun() -> tcp_reload_empty_module_test(Port) end},
            {"reload with path", fun() -> tcp_reload_with_path_test(Port) end},
            {"docs with selector unknown class", fun() ->
                tcp_docs_with_selector_unknown_test(Port)
            end},
            {"inspect unknown actor", fun() -> tcp_inspect_unknown_actor_test(Port) end},
            {"kill unknown actor", fun() -> tcp_kill_unknown_actor_test(Port) end},
            %% BT-523: gen_server callback tests
            {"get port", fun() -> tcp_get_port_test(Port) end},
            {"get nonce", fun() -> tcp_get_nonce_test(Port) end},
            {"unknown gen_server call", fun() -> tcp_unknown_call_check() end},
            {"gen_server cast", fun() -> tcp_cast_check() end},
            {"gen_server info unknown", fun() -> tcp_info_unknown_check2() end},
            {"health op", fun() -> tcp_health_op_test(Port) end},
            {"start_link integer port", fun() -> tcp_start_link_integer_test() end},
            %% BT-523: session ID uniqueness test
            {"clone uniqueness", fun() -> tcp_clone_uniqueness_test(Port) end},
            %% BT-666: interrupt operation tests
            {"interrupt no eval", fun() -> tcp_interrupt_no_eval_test(Port) end},
            {"interrupt unknown session", fun() -> tcp_interrupt_unknown_session_test(Port) end},
            %% BT-686: browser page tests
            {"GET / returns HTML page", fun() -> http_index_page_test(Port) end},
            {"HTML contains auth panel", fun() -> http_index_has_auth_panel_test(Port) end},
            {"HTML contains transcript", fun() -> http_index_has_transcript_test(Port) end},
            {"HTML contains eval panel", fun() -> http_index_has_eval_panel_test(Port) end},
            {"HTML serves workspace page with static JS", fun() ->
                http_index_has_websocket_js_test(Port)
            end},
            {"GET /ws without upgrade returns error", fun() -> http_ws_no_upgrade_test(Port) end},
            %% bind:as: / unbind: session refresh
            {"bind:as: appears in bindings immediately", fun() ->
                tcp_bind_as_updates_bindings_test(Port)
            end},
            {"unbind: removes from bindings immediately", fun() ->
                tcp_unbind_removes_from_bindings_test(Port)
            end},
            {"bind:as: value available in next eval", fun() ->
                tcp_bind_as_available_in_next_eval_test(Port)
            end},
            {"unbind: makes binding unavailable in next eval", fun() ->
                tcp_unbind_unavailable_in_next_eval_test(Port)
            end}
        ]
    end}.

tcp_setup() ->
    process_flag(trap_exit, true),
    application:ensure_all_started(beamtalk_workspace),
    %% Find a free port and start workspace, with retry on port conflict
    {Port, SupPid} = tcp_start_workspace(3),
    timer:sleep(100),
    {Port, SupPid}.

tcp_start_workspace(0) ->
    error(failed_to_start_workspace);
tcp_start_workspace(Retries) ->
    {ok, LSock} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    gen_tcp:close(LSock),
    timer:sleep(50),
    Config = #{
        workspace_id => <<"tcp_test_ws">>,
        project_path => <<"/tmp/bt_tcp_test">>,
        tcp_port => Port,
        auto_cleanup => false
    },
    case beamtalk_workspace_sup:start_link(Config) of
        {ok, Pid} -> {Port, Pid};
        {error, {already_started, Pid}} -> {Port, Pid};
        {error, {listen_failed, eaddrinuse}} -> tcp_start_workspace(Retries - 1);
        {error, Reason} -> error({workspace_start_failed, Reason})
    end.

tcp_cleanup({_Port, SupPid}) ->
    %% Stop cowboy listener before killing supervisor (ADR 0020)
    _ = cowboy:stop_listener(beamtalk_repl_ws),
    case is_pid(SupPid) andalso is_process_alive(SupPid) of
        true ->
            unlink(SupPid),
            exit(SupPid, shutdown),
            timer:sleep(200);
        false ->
            ok
    end.

%% Helper: connect, send a JSON op, receive response via WebSocket
tcp_send_op(Port, OpJson) ->
    {Ws, _Welcome} = ws_connect(Port),
    ws_send(Ws, OpJson),
    {ok, Data} = ws_recv(Ws),
    ws_close(Ws),
    jsx:decode(Data, [return_maps]).

%% Minimal WebSocket client for tests.
%% Performs HTTP upgrade, cookie auth, then supports text frame send/recv.

ws_connect(Port) ->
    {ok, Sock} = gen_tcp:connect(
        {127, 0, 0, 1},
        Port,
        [binary, {active, false}, {packet, raw}],
        5000
    ),
    %% WebSocket upgrade request
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Req = [
        <<"GET /ws HTTP/1.1\r\n">>,
        <<"Host: 127.0.0.1:">>,
        integer_to_binary(Port),
        <<"\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Key: ">>,
        Key,
        <<"\r\n">>,
        <<"Sec-WebSocket-Version: 13\r\n">>,
        <<"\r\n">>
    ],
    ok = gen_tcp:send(Sock, Req),
    %% Read HTTP upgrade response, return any leftover bytes after \r\n\r\n
    {ok, Rest} = ws_consume_http_response(Sock),
    %% Read auth-required message (may already be in Rest buffer)
    {ok, _AuthRequired} = ws_recv_with_buf(Sock, Rest),
    %% Send cookie auth
    Cookie = atom_to_binary(erlang:get_cookie(), utf8),
    AuthMsg = jsx:encode(#{<<"type">> => <<"auth">>, <<"cookie">> => Cookie}),
    ws_send(Sock, AuthMsg),
    %% Read auth_ok response
    {ok, _AuthOk} = ws_recv(Sock),
    %% Read session-started message (created after auth)
    {ok, Welcome} = ws_recv(Sock),
    {Sock, Welcome}.

ws_consume_http_response(Sock) ->
    ws_consume_http_response(Sock, <<>>).

ws_consume_http_response(Sock, Buf) ->
    case binary:match(Buf, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, More} = gen_tcp:recv(Sock, 0, 5000),
            ws_consume_http_response(Sock, <<Buf/binary, More/binary>>);
        {Pos, Len} ->
            %% Return bytes after the \r\n\r\n delimiter
            RestStart = Pos + Len,
            Rest = binary:part(Buf, RestStart, byte_size(Buf) - RestStart),
            {ok, Rest}
    end.

%% Send a masked text frame (clients must mask per RFC 6455)
ws_send(Sock, Data) when is_binary(Data) ->
    Len = byte_size(Data),
    MaskKey = crypto:strong_rand_bytes(4),
    Header =
        if
            Len < 126 ->
                % FIN=1, opcode=1 (text), MASK=1
                <<1:1, 0:3, 1:4, 1:1, Len:7>>;
            Len < 65536 ->
                <<1:1, 0:3, 1:4, 1:1, 126:7, Len:16>>;
            true ->
                <<1:1, 0:3, 1:4, 1:1, 127:7, Len:64>>
        end,
    Masked = ws_mask(Data, MaskKey),
    ok = gen_tcp:send(Sock, [Header, MaskKey, Masked]);
ws_send(Sock, Data) when is_list(Data) ->
    ws_send(Sock, iolist_to_binary(Data)).

%% Receive a text frame (server frames are unmasked).
%% Uses exact-byte reads to avoid consuming bytes from the next frame.
ws_recv(Sock) ->
    ws_recv_with_buf(Sock, <<>>).

%% Receive a text frame, with optional pre-buffered data from HTTP upgrade.
%% Buf may contain leftover bytes from ws_consume_http_response.
ws_recv_with_buf(Sock, Buf) ->
    %% Read 2-byte frame header (may already be in Buf)
    {<<_FIN:1, _RSV:3, Opcode:4, Mask:1, Len0:7>>, Rest0} = ws_read_exact(Sock, 2, Buf),
    {PayloadLen, Rest1} =
        case Len0 of
            126 ->
                {<<L:16>>, R} = ws_read_exact(Sock, 2, Rest0),
                {L, R};
            127 ->
                {<<L:64>>, R} = ws_read_exact(Sock, 8, Rest0),
                {L, R};
            L ->
                {L, Rest0}
        end,
    {MaskKey, Rest2} =
        case Mask of
            1 ->
                {MK, R2} = ws_read_exact(Sock, 4, Rest1),
                {MK, R2};
            0 ->
                {<<0, 0, 0, 0>>, Rest1}
        end,
    {Payload, Rest3} = ws_read_exact(Sock, PayloadLen, Rest2),
    Unmasked =
        case Mask of
            1 -> ws_mask(Payload, MaskKey);
            0 -> Payload
        end,
    case Opcode of
        % text
        1 ->
            {ok, Unmasked};
        % close
        8 ->
            {close, Unmasked};
        9 ->
            %% Ping — respond with masked pong (clients must mask all frames per RFC 6455)
            PongMaskKey = crypto:strong_rand_bytes(4),
            Pong = <<1:1, 0:3, 10:4, 1:1, 0:7>>,
            ok = gen_tcp:send(Sock, [Pong, PongMaskKey]),
            ws_recv_with_buf(Sock, Rest3);
        % skip other frames
        _ ->
            ws_recv_with_buf(Sock, Rest3)
    end.

%% Receive a text frame, skipping any server-initiated push events.
%% Use when you want the response to a specific request and push events may interleave.
ws_recv_response(Sock) ->
    {ok, Data} = ws_recv(Sock),
    case jsx:decode(Data, [return_maps]) of
        #{<<"type">> := <<"push">>} -> ws_recv_response(Sock);
        Msg -> {ok, jsx:encode(Msg)}
    end.

%% Read exactly N bytes: first from Buf, then from socket if needed.
%% Returns {Bytes, Rest} where Bytes is exactly N bytes and Rest is leftovers.
ws_read_exact(_Sock, N, Buf) when byte_size(Buf) >= N ->
    <<Bytes:N/binary, Rest/binary>> = Buf,
    {Bytes, Rest};
ws_read_exact(Sock, N, Buf) ->
    Need = N - byte_size(Buf),
    {ok, More} = gen_tcp:recv(Sock, Need, 5000),
    Combined = <<Buf/binary, More/binary>>,
    <<Bytes:N/binary, Rest/binary>> = Combined,
    {Bytes, Rest}.

ws_close(Sock) ->
    %% Send close frame
    CloseFrame = <<1:1, 0:3, 8:4, 1:1, 0:7, 0:32>>,
    _ = gen_tcp:send(Sock, CloseFrame),
    gen_tcp:close(Sock).

ws_mask(Data, <<M1, M2, M3, M4>>) ->
    ws_mask(Data, <<M1, M2, M3, M4>>, 0, <<>>).

ws_mask(<<>>, _Key, _I, Acc) ->
    Acc;
ws_mask(<<B, Rest/binary>>, Key = <<K1, K2, K3, K4>>, I, Acc) ->
    M =
        case I rem 4 of
            0 -> B bxor K1;
            1 -> B bxor K2;
            2 -> B bxor K3;
            3 -> B bxor K4
        end,
    ws_mask(Rest, Key, I + 1, <<Acc/binary, M>>).

%% Test: clear op returns ok status
tcp_clear_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"t1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t1">>}, Resp),
    ?assert(lists:member(<<"done">>, maps:get(<<"status">>, Resp))).

%% Test: bindings op returns bindings list
tcp_bindings_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"t2">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t2">>}, Resp),
    ?assert(maps:is_key(<<"bindings">>, Resp)).

%% Test: actors op returns empty actor list
tcp_actors_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"actors">>, <<"id">> => <<"t3">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t3">>}, Resp),
    ?assertEqual([], maps:get(<<"actors">>, Resp)).

%% Test: sessions op returns sessions list
tcp_sessions_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"sessions">>, <<"id">> => <<"t4">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t4">>}, Resp),
    ?assert(maps:is_key(<<"sessions">>, Resp)).

%% Test: close op returns ok status
tcp_close_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"close">>, <<"id">> => <<"t5">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t5">>}, Resp),
    ?assert(lists:member(<<"done">>, maps:get(<<"status">>, Resp))).

%% Test: complete with empty prefix returns empty
tcp_complete_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"complete">>, <<"id">> => <<"t6">>, <<"code">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t6">>}, Resp),
    ?assertEqual([], maps:get(<<"completions">>, Resp)).

%% Test: complete with a non-empty prefix
tcp_complete_prefix_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"complete">>, <<"id">> => <<"t6b">>, <<"code">> => <<"sel">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t6b">>}, Resp),
    Completions = maps:get(<<"completions">>, Resp),
    %% "self" starts with "sel" and is a built-in keyword
    ?assert(lists:member(<<"self">>, Completions)).

%% Test: unknown op returns error
tcp_unknown_op_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"foobar_nonexistent">>, <<"id">> => <<"t8">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t8">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: eval with empty code returns error
tcp_eval_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"eval">>, <<"id">> => <<"t9">>, <<"code">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t9">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: eval with simple expression
tcp_eval_simple_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"eval">>, <<"id">> => <<"t10">>, <<"code">> => <<"1 + 2">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t10">>}, Resp),
    %% Result could be error (compiler not running) or success
    ?assert(maps:is_key(<<"value">>, Resp) orelse maps:is_key(<<"error">>, Resp)).

%% Test: unload nonexistent module
tcp_unload_nonexistent_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"unload">>, <<"id">> => <<"t11">>, <<"module">> => <<"XyzzyNotLoaded">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t11">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: unload with empty module name
tcp_unload_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"unload">>, <<"id">> => <<"t12">>, <<"module">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t12">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: unload a raw BEAM module (not a Beamtalk class) returns class_not_found error.
%% BT-1239: The unload op requires the name to be a registered Beamtalk class.
tcp_unload_in_use_test(Port) ->
    DummyMod = 'bt519_tcp_in_use',
    Forms = [
        {attribute, 1, module, DummyMod},
        {attribute, 2, export, [{loop, 0}]},
        {function, 3, loop, 0, [
            {clause, 3, [], [], [
                {'receive', 3, [{clause, 3, [{atom, 3, stop}], [], [{atom, 3, ok}]}]}
            ]}
        ]}
    ],
    {ok, DummyMod, Binary} = compile:forms(Forms),
    {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
    LoopPid = spawn(DummyMod, loop, []),
    %% Load new version so running process uses "old" code
    {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
    try
        Msg = jsx:encode(#{
            <<"op">> => <<"unload">>,
            <<"id">> => <<"t12b">>,
            <<"module">> => atom_to_binary(DummyMod, utf8)
        }),
        Resp = tcp_send_op(Port, Msg),
        ?assertMatch(#{<<"id">> := <<"t12b">>}, Resp),
        %% Raw BEAM modules are not Beamtalk classes → error (class not found).
        ?assert(maps:is_key(<<"error">>, Resp))
    after
        LoopPid ! stop,
        timer:sleep(50),
        _ = code:soft_purge(DummyMod),
        _ = code:delete(DummyMod),
        _ = code:soft_purge(DummyMod)
    end.

%% Test: inspect with invalid PID string
tcp_inspect_invalid_pid_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"inspect">>, <<"id">> => <<"t13">>, <<"actor">> => <<"not-a-pid">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t13">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: kill with invalid PID string
tcp_kill_invalid_pid_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"kill">>, <<"id">> => <<"t14">>, <<"actor">> => <<"not-a-pid">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t14">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: reload module that hasn't been loaded returns error
tcp_reload_module_not_loaded_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"reload">>, <<"id">> => <<"t15">>, <<"module">> => <<"Counter">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t15">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)),
    ErrorMsg = maps:get(<<"error">>, Resp),
    ?assert(binary:match(ErrorMsg, <<"Module not loaded">>) =/= nomatch).

%% Test: docs for unknown class
tcp_docs_unknown_class_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"docs">>, <<"id">> => <<"t16">>, <<"class">> => <<"XyzzyUnknown">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t16">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: modules op returns modules list
tcp_modules_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"modules">>, <<"id">> => <<"t17">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t17">>}, Resp),
    ?assert(maps:is_key(<<"modules">>, Resp)).

%% Test: clone op creates a new session
tcp_clone_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"clone">>, <<"id">> => <<"t18">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t18">>}, Resp),
    %% Clone should return a session ID as value, or error if sup not ready
    ?assert(maps:is_key(<<"value">>, Resp) orelse maps:is_key(<<"error">>, Resp)).

%% Test: inspect a dead actor by valid PID format
tcp_inspect_dead_actor_test(Port) ->
    Pid = spawn(fun() -> ok end),
    %% Wait until the process is confirmed dead
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 1000 -> error(timeout)
    end,
    PidStr = list_to_binary(pid_to_list(Pid)),
    Msg = jsx:encode(#{<<"op">> => <<"inspect">>, <<"id">> => <<"t19">>, <<"actor">> => PidStr}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t19">>}, Resp),
    %% Should return error (not alive or not known)
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: malformed JSON falls back to raw eval
tcp_malformed_json_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    ws_send(Ws, <<"not valid json">>),
    {ok, Data} = ws_recv(Ws),
    ws_close(Ws),
    Resp = jsx:decode(Data, [return_maps]),
    %% The server should try to eval "not valid json" or return error
    ?assert(
        maps:is_key(<<"value">>, Resp) orelse maps:is_key(<<"error">>, Resp) orelse
            maps:is_key(<<"type">>, Resp)
    ).

%% Test: raw expression (non-JSON) backwards compatibility
tcp_raw_expression_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    ws_send(Ws, <<"42">>),
    {ok, Data} = ws_recv(Ws),
    ws_close(Ws),
    Resp = jsx:decode(Data, [return_maps]),
    %% Raw expressions go through protocol decode
    ?assert(is_map(Resp)).

%%% BT-523: TCP connection lifecycle tests

%% Test: multiple sequential connections to the same port
tcp_multiple_connects_test(Port) ->
    %% First connection
    Msg1 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mc1">>}),
    Resp1 = tcp_send_op(Port, Msg1),
    ?assertMatch(#{<<"id">> := <<"mc1">>}, Resp1),
    %% Second connection after first is closed
    Msg2 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mc2">>}),
    Resp2 = tcp_send_op(Port, Msg2),
    ?assertMatch(#{<<"id">> := <<"mc2">>}, Resp2),
    %% Third connection
    Msg3 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mc3">>}),
    Resp3 = tcp_send_op(Port, Msg3),
    ?assertMatch(#{<<"id">> := <<"mc3">>}, Resp3).

%% Test: multiple concurrent WebSocket connections
tcp_concurrent_clients_test(Port) ->
    Msg1 = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"cc1">>}),
    Msg2 = jsx:encode(#{<<"op">> => <<"actors">>, <<"id">> => <<"cc2">>}),
    {Ws1, _W1} = ws_connect(Port),
    {Ws2, _W2} = ws_connect(Port),
    ws_send(Ws1, Msg1),
    ws_send(Ws2, Msg2),
    {ok, Data1} = ws_recv(Ws1),
    {ok, Data2} = ws_recv(Ws2),
    ws_close(Ws1),
    ws_close(Ws2),
    Resp1 = jsx:decode(Data1, [return_maps]),
    Resp2 = jsx:decode(Data2, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"cc1">>}, Resp1),
    ?assertMatch(#{<<"id">> := <<"cc2">>}, Resp2).

%% Test: client disconnect is handled gracefully (no crash)
tcp_client_disconnect_test(Port) ->
    %% Connect and immediately close the TCP socket
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
    gen_tcp:close(Sock),
    timer:sleep(100),
    %% Server should still be responsive
    Msg = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"dc1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"dc1">>}, Resp).

%% Test: multiple requests on the same WebSocket connection
tcp_multi_request_same_conn_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    %% Send first request
    Msg1 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mr1">>}),
    ws_send(Ws, Msg1),
    {ok, Data1} = ws_recv_response(Ws),
    Resp1 = jsx:decode(Data1, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"mr1">>}, Resp1),
    %% Send second request on same connection
    Msg2 = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"mr2">>}),
    ws_send(Ws, Msg2),
    {ok, Data2} = ws_recv_response(Ws),
    Resp2 = jsx:decode(Data2, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"mr2">>}, Resp2),
    ws_close(Ws).

%%% bind:as: / unbind: session refresh tests

%% Test: Workspace bind:as: during eval makes binding appear in bindings query immediately.
%% Regression test: before the fix, the binding only appeared after reconnect.
tcp_bind_as_updates_bindings_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    EvalMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"bau1">>,
        <<"code">> => <<"Workspace bind: 42 as: #btBindUpdate">>
    }),
    ws_send(Ws, EvalMsg),
    {ok, _} = ws_recv_response(Ws),
    %% bindings op on the same connection — btBindUpdate must appear without reconnect
    BindMsg = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"bau2">>}),
    ws_send(Ws, BindMsg),
    {ok, BindData} = ws_recv_response(Ws),
    BindResp = jsx:decode(BindData, [return_maps]),
    Bindings = maps:get(<<"bindings">>, BindResp, #{}),
    ?assert(maps:is_key(<<"btBindUpdate">>, Bindings)),
    %% Cleanup
    CleanMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"bau3">>,
        <<"code">> => <<"Workspace unbind: #btBindUpdate">>
    }),
    ws_send(Ws, CleanMsg),
    {ok, _} = ws_recv_response(Ws),
    ws_close(Ws).

%% Test: Workspace unbind: during eval removes binding from bindings query immediately.
%% Regression test: before the fix, the binding persisted in :bindings until reconnect.
tcp_unbind_removes_from_bindings_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    BindMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"urb1">>,
        <<"code">> => <<"Workspace bind: 99 as: #btUnbindRemove">>
    }),
    ws_send(Ws, BindMsg),
    {ok, _} = ws_recv_response(Ws),
    UnbindMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"urb2">>,
        <<"code">> => <<"Workspace unbind: #btUnbindRemove">>
    }),
    ws_send(Ws, UnbindMsg),
    {ok, _} = ws_recv_response(Ws),
    %% bindings op — btUnbindRemove must be gone without reconnect
    QueryMsg = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"urb3">>}),
    ws_send(Ws, QueryMsg),
    {ok, QueryData} = ws_recv_response(Ws),
    QueryResp = jsx:decode(QueryData, [return_maps]),
    Bindings = maps:get(<<"bindings">>, QueryResp, #{}),
    ?assertNot(maps:is_key(<<"btUnbindRemove">>, Bindings)),
    ws_close(Ws).

%% Test: value registered via bind:as: is usable in the next eval on the same session.
tcp_bind_as_available_in_next_eval_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    BindMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"bae1">>,
        <<"code">> => <<"Workspace bind: 42 as: #btBindNextEval">>
    }),
    ws_send(Ws, BindMsg),
    {ok, _} = ws_recv_response(Ws),
    %% Next eval references the bound name — must succeed (not DNU)
    EvalMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"bae2">>,
        <<"code">> => <<"btBindNextEval">>
    }),
    ws_send(Ws, EvalMsg),
    {ok, EvalData} = ws_recv_response(Ws),
    EvalResp = jsx:decode(EvalData, [return_maps]),
    ?assert(maps:is_key(<<"value">>, EvalResp)),
    ?assertNot(maps:is_key(<<"error">>, EvalResp)),
    %% Cleanup
    CleanMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"bae3">>,
        <<"code">> => <<"Workspace unbind: #btBindNextEval">>
    }),
    ws_send(Ws, CleanMsg),
    {ok, _} = ws_recv_response(Ws),
    ws_close(Ws).

%% Test: after unbind:, the binding is no longer available in subsequent evals.
%% Regression test: before the fix, unbind: only removed from ETS but the session
%% in-memory state still had the old value, so the binding kept working.
tcp_unbind_unavailable_in_next_eval_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    BindMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"une1">>,
        <<"code">> => <<"Workspace bind: 99 as: #btUnbindNextEval">>
    }),
    ws_send(Ws, BindMsg),
    {ok, _} = ws_recv_response(Ws),
    UnbindMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"une2">>,
        <<"code">> => <<"Workspace unbind: #btUnbindNextEval">>
    }),
    ws_send(Ws, UnbindMsg),
    {ok, _} = ws_recv_response(Ws),
    %% Next eval referencing the now-unbound name must fail
    EvalMsg = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"une3">>,
        <<"code">> => <<"btUnbindNextEval">>
    }),
    ws_send(Ws, EvalMsg),
    {ok, EvalData} = ws_recv_response(Ws),
    EvalResp = jsx:decode(EvalData, [return_maps]),
    ?assert(maps:is_key(<<"error">>, EvalResp)),
    ws_close(Ws).

%%% BT-523: Connection error handling tests

%% Test: send empty string to server via WebSocket
tcp_empty_line_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    ws_send(Ws, <<>>),
    {ok, Data} = ws_recv(Ws),
    ws_close(Ws),
    Resp = jsx:decode(Data, [return_maps]),
    %% Empty message results in error
    ?assert(
        maps:is_key(<<"error">>, Resp) orelse
            maps:get(<<"type">>, Resp, undefined) =:= <<"error">>
    ).

%% Test: non-JSON text data via WebSocket
tcp_binary_garbage_test(Port) ->
    {Ws, _Welcome} = ws_connect(Port),
    %% Send garbled text (valid UTF-8 but not JSON)
    ws_send(Ws, <<"@#$%^&*not-json">>),
    {ok, Data} = ws_recv(Ws),
    ws_close(Ws),
    %% Should get some response without crashing the server
    ?assert(is_binary(Data)),
    %% Server still works after garbage
    Msg = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"bg1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"bg1">>}, Resp).

%% Test: reload with empty module name
tcp_reload_empty_module_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"reload">>, <<"id">> => <<"re1">>, <<"module">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"re1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: reload with path but no module
tcp_reload_with_path_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"reload">>,
        <<"id">> => <<"re2">>,
        <<"path">> => <<"/nonexistent/path.bt">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"re2">>}, Resp),
    %% Should error because file doesn't exist
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: docs with selector for unknown class
tcp_docs_with_selector_unknown_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"docs">>,
        <<"id">> => <<"d1">>,
        <<"class">> => <<"XyzzyUnknown523">>,
        <<"selector">> => <<"+">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"d1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: inspect unknown actor (valid PID format, not in registry)
tcp_inspect_unknown_actor_test(Port) ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    PidStr = list_to_binary(pid_to_list(Pid)),
    exit(Pid, kill),
    timer:sleep(50),
    Msg = jsx:encode(#{
        <<"op">> => <<"inspect">>,
        <<"id">> => <<"iu1">>,
        <<"actor">> => PidStr
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"iu1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: kill unknown actor (valid PID format, not in registry)
tcp_kill_unknown_actor_test(Port) ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    PidStr = list_to_binary(pid_to_list(Pid)),
    exit(Pid, kill),
    timer:sleep(50),
    Msg = jsx:encode(#{
        <<"op">> => <<"kill">>,
        <<"id">> => <<"ku1">>,
        <<"actor">> => PidStr
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"ku1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%%% BT-523: gen_server callback and lifecycle tests

%% Test: get_port returns port after start
tcp_get_port_test(Port) ->
    {ok, ActualPort} = beamtalk_repl_server:get_port(),
    ?assertEqual(Port, ActualPort).

tcp_get_nonce_test(_Port) ->
    {ok, Nonce} = beamtalk_repl_server:get_nonce(),
    ?assert(is_binary(Nonce)),
    ?assertEqual(16, byte_size(Nonce)).

tcp_unknown_call_check() ->
    %% Covers handle_call(_Request, _From, State) clause (line 106-108)
    Pid = whereis(beamtalk_repl_server),
    ?assert(is_pid(Pid)),
    Result = gen_server:call(beamtalk_repl_server, some_unknown_request),
    ?assertEqual({error, unknown_request}, Result).

tcp_cast_check() ->
    %% Covers handle_cast(_Msg, State) clause (line 112)
    Pid = whereis(beamtalk_repl_server),
    ?assert(is_pid(Pid)),
    ok = gen_server:cast(beamtalk_repl_server, some_unknown_message).

tcp_info_unknown_check2() ->
    %% Covers handle_info(_Info, State) catch-all clause (line 129)
    Pid = whereis(beamtalk_repl_server),
    ?assert(is_pid(Pid)),
    beamtalk_repl_server ! some_random_message,
    timer:sleep(50).

tcp_health_op_test(Port) ->
    %% Covers handle_op(<<"health">>, ...) which exercises get_nonce/0 (lines 714-725)
    Msg = jsx:encode(#{<<"op">> => <<"health">>, <<"id">> => <<"h1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"h1">>}, Resp),
    ?assert(maps:is_key(<<"nonce">>, Resp)).

%% Test: start_link/1 integer clause executes (backward compatibility)
tcp_start_link_integer_test() ->
    %% Server is already running inside tcp_integration_test_ fixture.
    %% Calling start_link with integer arg exercises the integer clause
    %% and returns {already_started, _} from gen_server because
    %% the server is registered as {local, beamtalk_repl_server}.
    Result = beamtalk_repl_server:start_link(0),
    case Result of
        {error, {already_started, _}} ->
            %% Expected: gen_server already registered
            ok;
        {error, {listen_failed, {already_started, _}}} ->
            %% Expected: cowboy listener already running (ADR 0020)
            ok;
        {ok, Pid} ->
            %% Server wasn't registered (race with test ordering).
            %% The integer clause worked — clean up the started server.
            gen_server:stop(Pid),
            ok
    end.

%% Test: start_link with integer port (backward compatibility)
%% The integer-port clause is tested within tcp_integration_test_ via
%% tcp_start_link_integer_test which calls start_link/1 while the server
%% is running and asserts {already_started, _}.
start_link_integer_port_test() ->
    %% Basic verification that the export exists
    Exports = beamtalk_repl_server:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)).

%%% BT-523: generate_session_id format and uniqueness tests (via clone op)

%% Clone creates new sessions, each with a unique generated ID.
%% This tests generate_session_id indirectly via the clone TCP op.
%% The test is in tcp_integration_test_ as tcp_clone_uniqueness_test.

%% Test: two clone ops produce different session IDs
tcp_clone_uniqueness_test(Port) ->
    Msg1 = jsx:encode(#{<<"op">> => <<"clone">>, <<"id">> => <<"cu1">>}),
    Msg2 = jsx:encode(#{<<"op">> => <<"clone">>, <<"id">> => <<"cu2">>}),
    Resp1 = tcp_send_op(Port, Msg1),
    Resp2 = tcp_send_op(Port, Msg2),
    %% Both clones must succeed — if the session sup isn't ready, fail loudly
    ?assertMatch(#{<<"value">> := _}, Resp1),
    ?assertMatch(#{<<"value">> := _}, Resp2),
    V1 = maps:get(<<"value">>, Resp1),
    V2 = maps:get(<<"value">>, Resp2),
    ?assertNotEqual(V1, V2),
    %% Verify session ID format: "session_<timestamp>_<random>"
    ?assert(binary:match(V1, <<"session_">>) =:= {0, 8}),
    ?assert(binary:match(V2, <<"session_">>) =:= {0, 8}).

%%% ===========================================================================
%%% BT-627: Coverage tests for internal helper functions
%%% ===========================================================================

%%% validate_actor_pid/1 tests

validate_actor_pid_invalid_format_test() ->
    ?assertEqual({error, invalid_pid}, beamtalk_repl_server:validate_actor_pid("not_a_pid")).

validate_actor_pid_unknown_actor_test() ->
    %% Valid PID format but not registered as an actor
    PidStr = pid_to_list(self()),
    ?assertEqual({error, unknown_actor}, beamtalk_repl_server:validate_actor_pid(PidStr)).

%%% is_known_actor/1 tests

is_known_actor_no_registry_test() ->
    %% When no registry is running, should return false
    ?assertEqual(false, beamtalk_repl_server:is_known_actor(self())).

%%% get_completions/1 tests

get_completions_empty_prefix_test() ->
    ?assertEqual([], beamtalk_repl_server:get_completions(<<>>)).

get_completions_no_match_test() ->
    %% No class registry running, only keywords should match
    Result = beamtalk_repl_server:get_completions(<<"zzzzz">>),
    ?assertEqual([], Result).

get_completions_keyword_match_test() ->
    Result = beamtalk_repl_server:get_completions(<<"self">>),
    ?assert(lists:member(<<"self">>, Result)).

get_completions_keyword_prefix_test() ->
    Result = beamtalk_repl_server:get_completions(<<"su">>),
    ?assert(lists:member(<<"super">>, Result)),
    ?assert(lists:member(<<"subclass:">>, Result)).

get_completions_workspace_binding_test() ->
    %% Workspace bindings (Transcript, Beamtalk, Workspace) should appear
    Result = beamtalk_repl_server:get_completions(<<"Transcri">>),
    ?assert(lists:member(<<"Transcript">>, Result)).

get_completions_workspace_binding_prefix_test() ->
    %% "Work" should match the Workspace binding
    Result = beamtalk_repl_server:get_completions(<<"Work">>),
    ?assert(lists:member(<<"Workspace">>, Result)).

%%% parse_receiver_and_prefix/1 tests (BT-783)

parse_receiver_class_and_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Integer s">>)
    ).

parse_receiver_class_empty_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Integer ">>)
    ).

parse_receiver_integer_literal_test() ->
    ?assertEqual(
        {<<"42">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"42 s">>)
    ).

parse_receiver_float_literal_no_completions_test() ->
    %% Float literal like "3.14" is not an Integer — returns no completions
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"3.14 s">>),
    ?assertEqual([], Result).

parse_no_receiver_single_word_test() ->
    ?assertEqual(
        {undefined, <<"Counter">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Counter">>)
    ).

parse_no_receiver_empty_test() ->
    ?assertEqual(
        {undefined, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<>>)
    ).

parse_receiver_string_literal_test() ->
    ?assertEqual(
        {<<"\"hello\"">>, <<"up">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"\"hello\" up">>)
    ).

parse_receiver_keyword_selector_test() ->
    %% Colons are identifier chars — "ifT:" is the prefix, not just "ifT"
    ?assertEqual(
        {<<"Integer">>, <<"ifT:">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Integer ifT:">>)
    ).

parse_receiver_multi_keyword_selector_test() ->
    %% Multi-keyword selectors like "ifTrue:ifFalse:" complete as a unit
    ?assertEqual(
        {<<"Boolean">>, <<"ifTrue:ifFalse:">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Boolean ifTrue:ifFalse:">>)
    ).

%%% get_context_completions/1 tests (BT-783)

context_completions_empty_test() ->
    ?assertEqual([], beamtalk_repl_ops_dev:get_context_completions(<<>>)).

context_completions_no_receiver_falls_back_test() ->
    %% With no class registry running, bare prefix should still match keywords
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"su">>),
    ?assert(lists:member(<<"super">>, Result)),
    ?assert(lists:member(<<"subclass:">>, Result)).

context_completions_unknown_receiver_returns_empty_test() ->
    %% Unknown class receiver returns no methods
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"NoSuchClass123 s">>),
    ?assertEqual([], Result).

context_completions_binding_actor_no_registry_test() ->
    %% Binding with #beamtalk_object{} but no class registry → empty (no crash)
    Binding = #beamtalk_object{class = 'NoSuchClass123', class_mod = undefined, pid = self()},
    Bindings = #{c => Binding},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"c in">>, Bindings),
    ?assertEqual([], Result).

context_completions_binding_reference_returns_object_methods_test() ->
    %% All values are now classifiable via class_of/1.
    %% Even Erlang references inherit Object methods like `class`, `inspect`.
    Bindings = #{x => make_ref()},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"x cl">>, Bindings),
    ?assert(lists:member(<<"class">>, Result)).

%% BT: Symbol binding (atom value) now correctly returns Symbol instance methods
context_completions_symbol_binding_returns_instance_methods_test() ->
    Bindings = #{x => some_atom},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"x cl">>, Bindings),
    %% `class` is inherited from Object; available on Symbol instances
    ?assert(lists:member(<<"class">>, Result)).

context_completions_with_bindings_empty_line_test() ->
    %% Empty line with bindings returns empty
    ?assertEqual([], beamtalk_repl_ops_dev:get_context_completions(<<>>, #{a => 1})).

%% BT-1044: Class receiver returns both class-side and instance methods
context_completions_class_receiver_returns_class_methods_test() ->
    Pid = spawn_mock_class('TestCompletionClassA', #{spawn => ok, 'spawnWith:' => ok}, [
        increment, decrement
    ]),
    try
        Result = beamtalk_repl_ops_dev:get_context_completions(
            <<"TestCompletionClassA sp">>, #{}
        ),
        %% Class-side methods matching "sp" should appear
        ?assert(lists:member(<<"spawn">>, Result)),
        ?assert(lists:member(<<"spawnWith:">>, Result)),
        %% Instance methods not matching prefix "sp" must not appear
        ?assertNot(lists:member(<<"increment">>, Result)),
        ?assertNot(lists:member(<<"decrement">>, Result))
    after
        cleanup_mock_class('TestCompletionClassA', Pid)
    end.

%% Class receiver must NOT show instance methods — they can't be called on the class object.
context_completions_class_receiver_excludes_instance_methods_test() ->
    Pid = spawn_mock_class('TestCompletionClassA2', #{spawn => ok}, [
        increment, inspect, decrement
    ]),
    try
        Result = beamtalk_repl_ops_dev:get_context_completions(
            <<"TestCompletionClassA2 in">>, #{}
        ),
        %% Instance methods must NOT appear for class-name receivers
        ?assertNot(lists:member(<<"increment">>, Result)),
        ?assertNot(lists:member(<<"inspect">>, Result)),
        ?assertNot(lists:member(<<"decrement">>, Result))
    after
        cleanup_mock_class('TestCompletionClassA2', Pid)
    end.

%% BT: Core behavior — instance binding returns instance methods, not class-side methods
context_completions_instance_binding_returns_instance_methods_test() ->
    Pid = spawn_mock_class('TestCompletionClassB', #{spawn => ok, 'spawnWith:' => ok}, [
        increment, inspect, decrement
    ]),
    try
        Binding = #beamtalk_object{
            class = 'TestCompletionClassB', class_mod = undefined, pid = self()
        },
        Bindings = #{c => Binding},
        Result = beamtalk_repl_ops_dev:get_context_completions(<<"c in">>, Bindings),
        %% Instance methods matching "in" should appear
        ?assert(lists:member(<<"increment">>, Result)),
        ?assert(lists:member(<<"inspect">>, Result)),
        %% Class-side methods must NOT appear (none match "in")
        ?assertNot(lists:member(<<"spawn">>, Result)),
        ?assertNot(lists:member(<<"spawnWith:">>, Result))
    after
        cleanup_mock_class('TestCompletionClassB', Pid)
    end.

%% Hidden meta-protocol methods (subclassResponsibility, notImplemented, fieldNames, etc.)
%% must not appear in instance completions regardless of prefix.
context_completions_instance_hides_meta_protocol_methods_test() ->
    Bindings = #{i => 42},
    ResultAll = beamtalk_repl_ops_dev:get_context_completions(<<"i ">>, Bindings),
    ?assertNot(lists:member(<<"subclassResponsibility">>, ResultAll)),
    ?assertNot(lists:member(<<"notImplemented">>, ResultAll)),
    ?assertNot(lists:member(<<"doesNotUnderstand:args:">>, ResultAll)),
    ?assertNot(lists:member(<<"fieldNames">>, ResultAll)),
    ?assertNot(lists:member(<<"fieldAt:">>, ResultAll)),
    ?assertNot(lists:member(<<"fieldAt:put:">>, ResultAll)),
    %% new/new: are also hidden on instances (they error via basicNew for most classes)
    ?assertNot(lists:member(<<"new">>, ResultAll)),
    ?assertNot(lists:member(<<"new:">>, ResultAll)),
    %% perform:/perform:withArguments: — dynamic dispatch meta-protocol
    ?assertNot(lists:member(<<"perform:">>, ResultAll)),
    ?assertNot(lists:member(<<"perform:withArguments:">>, ResultAll)).

%% new/new: must still appear for class-name receivers (they ARE the constructors there).
context_completions_class_receiver_new_methods_visible_test() ->
    Pid = spawn_mock_class('TestCompletionNewClass', #{'new' => ok, 'new:' => ok}, []),
    try
        Result = beamtalk_repl_ops_dev:get_context_completions(
            <<"TestCompletionNewClass n">>, #{}
        ),
        ?assert(lists:member(<<"new">>, Result)),
        ?assert(lists:member(<<"new:">>, Result))
    after
        cleanup_mock_class('TestCompletionNewClass', Pid)
    end.

%% BT: Uppercase global binding (e.g. Transcript) falls back to binding lookup
context_completions_uppercase_global_binding_returns_instance_methods_test() ->
    Pid = spawn_mock_class('TestCompletionClassD', #{spawn => ok, 'spawnWith:' => ok}, [
        show, subscribe, recent
    ]),
    try
        %% Transcript-style: uppercase name that is a binding, not a class
        Binding = #beamtalk_object{
            class = 'TestCompletionClassD', class_mod = undefined, pid = self()
        },
        Bindings = #{'TestCompletionClassD_inst' => Binding, 'TranscriptMock' => Binding},
        Result = beamtalk_repl_ops_dev:get_context_completions(<<"TranscriptMock s">>, Bindings),
        %% Instance methods matching "s" should appear
        ?assert(lists:member(<<"show">>, Result)),
        ?assert(lists:member(<<"subscribe">>, Result)),
        %% Class-side builtins starting with "s" must NOT appear
        ?assertNot(lists:member(<<"spawn">>, Result)),
        ?assertNot(lists:member(<<"spawnWith:">>, Result))
    after
        cleanup_mock_class('TestCompletionClassD', Pid)
    end.

%% BT: Value-type binding (tagged map from `new`) returns instance methods
context_completions_value_type_binding_returns_instance_methods_test() ->
    Pid = spawn_mock_class('TestCompletionClassC', #{spawn => ok, 'spawnWith:' => ok}, [
        parse, pairOf, printString
    ]),
    try
        %% Value-type instances are tagged maps with '$beamtalk_class' key
        Binding = #{'$beamtalk_class' => 'TestCompletionClassC'},
        Bindings = #{s => Binding},
        Result = beamtalk_repl_ops_dev:get_context_completions(<<"s p">>, Bindings),
        %% Instance methods matching "p" should appear
        ?assert(lists:member(<<"parse">>, Result)),
        ?assert(lists:member(<<"pairOf">>, Result)),
        ?assert(lists:member(<<"printString">>, Result)),
        %% Class-side methods must NOT appear
        ?assertNot(lists:member(<<"spawn">>, Result)),
        ?assertNot(lists:member(<<"spawnWith:">>, Result))
    after
        cleanup_mock_class('TestCompletionClassC', Pid)
    end.

%% BT: Integer binding (e.g. i := 42) returns Integer instance methods
%% Uses the real Integer class which is always registered in the test environment.
context_completions_integer_binding_returns_instance_methods_test() ->
    Bindings = #{i => 42},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"i cl">>, Bindings),
    %% `class` is inherited from Object; available on every Integer instance
    ?assert(lists:member(<<"class">>, Result)).

%% BT: String binding (e.g. s := 'hello') returns String instance methods
%% Uses the real String class which is always registered in the test environment.
context_completions_string_binding_returns_instance_methods_test() ->
    Bindings = #{s => <<"hello">>},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"s cl">>, Bindings),
    %% `class` is inherited from Object; available on every String instance
    ?assert(lists:member(<<"class">>, Result)).

%% BT: nil binding returns UndefinedObject instance methods
context_completions_nil_binding_returns_instance_methods_test() ->
    Bindings = #{n => nil},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"n cl">>, Bindings),
    %% `class` is inherited from Object; available on UndefinedObject instances
    ?assert(lists:member(<<"class">>, Result)).

%% BT: true binding returns True instance methods (not Boolean — Boolean is abstract)
context_completions_true_binding_returns_instance_methods_test() ->
    Bindings = #{t => true},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"t cl">>, Bindings),
    %% `class` is inherited from Object; available on True instances
    ?assert(lists:member(<<"class">>, Result)).

%% BT: false binding returns False instance methods (not Boolean — Boolean is abstract)
context_completions_false_binding_returns_instance_methods_test() ->
    Bindings = #{f => false},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"f cl">>, Bindings),
    %% `class` is inherited from Object; available on False instances
    ?assert(lists:member(<<"class">>, Result)).

%% BT: Float binding (e.g. f := 3.14) returns Float instance methods
context_completions_float_binding_returns_instance_methods_test() ->
    Bindings = #{f => 3.14},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"f cl">>, Bindings),
    ?assert(lists:member(<<"class">>, Result)).

%% BT: List binding (e.g. l := #(1 2 3)) returns List instance methods
context_completions_list_binding_returns_instance_methods_test() ->
    Bindings = #{l => [1, 2, 3]},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"l cl">>, Bindings),
    ?assert(lists:member(<<"class">>, Result)).

%% BT: Block/closure binding returns Block instance methods
context_completions_block_binding_returns_instance_methods_test() ->
    Bindings = #{b => fun() -> ok end},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"b cl">>, Bindings),
    ?assert(lists:member(<<"class">>, Result)).

%% BT: Unbound variable (not in bindings map) returns empty
context_completions_unbound_variable_returns_empty_test() ->
    Bindings = #{other => 42},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"x ">>, Bindings),
    ?assertEqual([], Result).

%% BT: String literal receiver (e.g. "hello" up) returns String instance methods
context_completions_string_literal_receiver_test() ->
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"\"hello\" cl">>),
    ?assert(lists:member(<<"class">>, Result)).

%% BT: Non-existent atom receiver returns empty (safe_to_existing_atom fails)
context_completions_nonexistent_atom_receiver_returns_empty_test() ->
    Result = beamtalk_repl_ops_dev:get_context_completions(
        <<"thisAtomDoesNotExistInTheVM ">>, #{}
    ),
    ?assertEqual([], Result).

%%% beamtalk_session_table:resolve_pid/2 tests (BT-1045)
%%% These test the ETS-based session lookup that allows the completion client
%%% and the VS Code extension to read bindings from the user's main REPL session.

%% undefined session ID → always return the default PID
session_table_undefined_returns_default_test() ->
    Default = self(),
    ?assertEqual(Default, beamtalk_session_table:resolve_pid(undefined, Default)).

%% ETS table does not exist → catch returns default without crashing
session_table_no_ets_table_returns_default_test() ->
    %% Ensure the named table does not exist for this test
    catch ets:delete(beamtalk_sessions),
    Default = self(),
    %% resolve_pid/2 catches all errors and falls back to Default
    Result = beamtalk_session_table:resolve_pid(<<"no-such-session">>, Default),
    ?assertEqual(Default, Result).

%% Session ID present in ETS with live PID → return that PID
session_table_finds_live_pid_test() ->
    beamtalk_session_table:new(),
    SessionId = <<"test-session-bt1045-live">>,
    %% Use self() as the "session pid" — it is alive
    SessionPid = self(),
    beamtalk_session_table:insert(SessionId, SessionPid),
    Default = spawn(fun() -> ok end),
    Result = beamtalk_session_table:resolve_pid(SessionId, Default),
    beamtalk_session_table:delete(SessionId),
    %% Should return the registered PID, not the default
    ?assertEqual(SessionPid, Result).

%% Session ID not in ETS → return default
session_table_unknown_session_returns_default_test() ->
    beamtalk_session_table:new(),
    SessionId = <<"test-session-bt1045-unknown">>,
    %% Make sure this key is not in the table
    beamtalk_session_table:delete(SessionId),
    Default = self(),
    Result = beamtalk_session_table:resolve_pid(SessionId, Default),
    ?assertEqual(Default, Result).

%% Dead PID in ETS → fall back to default rather than returning a dead process
session_table_dead_pid_returns_default_test() ->
    beamtalk_session_table:new(),
    SessionId = <<"test-session-bt1045-dead">>,
    MonRef = erlang:monitor(process, spawn(fun() -> ok end)),
    DeadPid =
        receive
            {'DOWN', MonRef, process, Pid, _} -> Pid
        end,
    beamtalk_session_table:insert(SessionId, DeadPid),
    Default = self(),
    Result = beamtalk_session_table:resolve_pid(SessionId, Default),
    beamtalk_session_table:delete(SessionId),
    ?assertEqual(Default, Result).

%%% get_session_bindings/1 tests (BT-1045)

%% Live shell with no user bindings → returns a map (may include workspace singletons)
get_session_bindings_from_live_shell_returns_map_test() ->
    application:ensure_all_started(beamtalk_runtime),
    {ok, ShellPid} = beamtalk_repl_shell:start_link(<<"test-gsb-1">>),
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(ShellPid),
    SessionBindings = beamtalk_repl_ops_dev:get_session_bindings(ShellPid),
    beamtalk_repl_shell:stop(ShellPid),
    %% get_session_bindings/1 must return the same map as get_bindings/1
    ?assertEqual(Bindings, SessionBindings).

%% Dead shell PID → returns empty map without crashing
get_session_bindings_dead_pid_returns_empty_test() ->
    MonRef = erlang:monitor(process, spawn(fun() -> ok end)),
    DeadPid =
        receive
            {'DOWN', MonRef, process, Pid, _} -> Pid
        end,
    Result = beamtalk_repl_ops_dev:get_session_bindings(DeadPid),
    ?assertEqual(#{}, Result).

%%% Integration: ETS session lookup feeds into completion (BT-1045)
%%% This test simulates the full handle/4 code path for a "complete" op:
%%%   create session → insert into ETS → verify resolve_pid → get_session_bindings

%% BT-1045: The protocol decoder strips "session" from params (into Msg.session).
%% Verify that a bindings op with session field has session in Msg, not in Params.
bindings_op_session_field_is_in_msg_not_params_test() ->
    Json = <<"{\"op\":\"bindings\",\"id\":\"t1\",\"session\":\"sid-abc\"}">>,
    {ok, Msg} = beamtalk_repl_protocol:decode(Json),
    Params = beamtalk_repl_protocol:get_params(Msg),
    %% session must be in Msg, not in Params
    ?assertEqual(<<"sid-abc">>, beamtalk_repl_protocol:get_session(Msg)),
    ?assertNot(maps:is_key(<<"session">>, Params)).

%% Verify that a complete op with session field has session in Msg, not in Params.
complete_op_session_field_is_in_msg_not_params_test() ->
    Json =
        <<"{\"op\":\"complete\",\"id\":\"t1\",\"session\":\"sid-abc\",\"code\":\"s \",\"cursor\":2}">>,
    {ok, Msg} = beamtalk_repl_protocol:decode(Json),
    Params = beamtalk_repl_protocol:get_params(Msg),
    %% session must be in Msg, not in Params — the old bug was maps:get(<<"session">>, Params)
    ?assertEqual(<<"sid-abc">>, beamtalk_repl_protocol:get_session(Msg)),
    ?assertNot(maps:is_key(<<"session">>, Params)).

%% beamtalk_session_table:resolve_pid/2 + get_session_bindings together reproduce what handle/4 does.
%% Key invariant: the PID returned by resolve_pid/2 is the shell PID, so
%% get_session_bindings called on it returns the same map as get_bindings/1.
session_binding_lookup_pipeline_test() ->
    application:ensure_all_started(beamtalk_runtime),
    SessionId = <<"test-pipeline-bt1045">>,
    {ok, ShellPid} = beamtalk_repl_shell:start_link(SessionId),
    DefaultPid = spawn(fun() -> timer:sleep(10000) end),
    beamtalk_session_table:new(),
    beamtalk_session_table:insert(SessionId, ShellPid),
    try
        %% Simulate what handle/4 does for a complete op with a session field
        BindingPid = beamtalk_session_table:resolve_pid(SessionId, DefaultPid),
        SessionBindings = beamtalk_repl_ops_dev:get_session_bindings(BindingPid),
        {ok, DirectBindings} = beamtalk_repl_shell:get_bindings(ShellPid),
        %% Pipeline must resolve to the shell (not the default fallback)
        %% and bindings must match what get_bindings/1 returns directly.
        ?assertEqual(ShellPid, BindingPid),
        ?assertEqual(DirectBindings, SessionBindings)
    after
        beamtalk_session_table:delete(SessionId),
        exit(DefaultPid, kill),
        beamtalk_repl_shell:stop(ShellPid)
    end.

%% handle_op bindings with session field returns bindings from target session, not WS session.
%% Regression test for BT-1063: the bindings op must read session from Msg (via get_session/1),
%% not from Params — the protocol decoder strips the top-level "session" key into Msg.
handle_op_bindings_with_session_returns_target_bindings_test() ->
    application:ensure_all_started(beamtalk_runtime),
    SessionId = <<"test-bindings-op-bt1063">>,
    {ok, ShellPid} = beamtalk_repl_shell:start_link(SessionId),
    beamtalk_session_table:new(),
    beamtalk_session_table:insert(SessionId, ShellPid),
    %% eval a binding into the target session
    {ok, _, _, _} = beamtalk_repl_shell:eval(ShellPid, "x := 42"),
    try
        %% Construct a bindings request with session field at the top level.
        %% The protocol decoder puts it in Msg, not Params.
        Json = jsx:encode(#{
            <<"op">> => <<"bindings">>,
            <<"id">> => <<"b1">>,
            <<"session">> => SessionId
        }),
        {ok, Msg} = beamtalk_repl_protocol:decode(Json),
        Params = beamtalk_repl_protocol:get_params(Msg),
        %% Use a dummy WS session pid (no bindings) as the default
        WsPid = spawn(fun() -> timer:sleep(10000) end),
        Result = beamtalk_repl_server:handle_op(<<"bindings">>, Params, Msg, WsPid),
        exit(WsPid, kill),
        Decoded = jsx:decode(Result, [return_maps]),
        ?assertMatch(#{<<"id">> := <<"b1">>}, Decoded),
        %% Must return bindings from ShellPid (the target session), not the empty WS session
        Bindings = maps:get(<<"bindings">>, Decoded),
        ?assert(maps:is_key(<<"x">>, Bindings))
    after
        beamtalk_session_table:delete(SessionId),
        beamtalk_repl_shell:stop(ShellPid)
    end.

%%% tokenise_send_chain/1 tests (BT-1006)

tokenise_send_chain_simple_chain_test() ->
    %% Single hop unary chain
    ?assertEqual(
        {ok, <<"\"hello\"">>, [size]},
        beamtalk_repl_ops_dev:tokenise_send_chain(<<"\"hello\" size">>)
    ).

tokenise_send_chain_multi_hop_test() ->
    %% Two-hop unary chain
    ?assertEqual(
        {ok, <<"counter">>, [getValue, reversed]},
        beamtalk_repl_ops_dev:tokenise_send_chain(<<"counter getValue reversed">>)
    ).

tokenise_send_chain_single_token_returns_error_test() ->
    %% Single token — no sends to chain
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"hello">>)).

tokenise_send_chain_empty_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<>>)).

tokenise_send_chain_keyword_mid_chain_returns_error_test() ->
    %% Keyword send mid-chain is not a unary chain
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"myList inject: 0">>)).

tokenise_send_chain_paren_returns_error_test() ->
    %% Parenthesised subexpression — not supported
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"(myList size)">>)).

%%% parse_receiver_and_prefix/1 multi-token tests (BT-1006)

parse_receiver_multi_token_expression_test() ->
    %% Multi-token receiver returns {expression, ReceiverExpr, Prefix}
    ?assertEqual(
        {expression, <<"\"hello\" size">>, <<"cl">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"\"hello\" size cl">>)
    ).

parse_receiver_multi_token_empty_prefix_test() ->
    %% Multi-token expression with trailing space (empty prefix)
    ?assertEqual(
        {expression, <<"\"hello\" size">>, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"\"hello\" size ">>)
    ).

parse_receiver_multi_token_three_tokens_test() ->
    %% Three-token expression with trailing space
    ?assertEqual(
        {expression, <<"counter getValue reversed">>, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"counter getValue reversed ">>)
    ).

parse_receiver_leading_whitespace_single_token_test() ->
    %% Leading whitespace must NOT trigger multi-token expression path
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"  Integer s">>)
    ).

%%% walk_chain/2 and walk_chain_class/2 tests (BT-1006)

walk_chain_empty_selectors_test() ->
    %% walk_chain with no selectors returns the class unchanged — no registry call
    ?assertEqual({ok, 'String'}, beamtalk_repl_ops_dev:walk_chain('String', [])).

walk_chain_single_hop_found_test() ->
    Pid = spawn_mock_class_with_return_types(
        'WalkChainTestA', #{size => 'Integer'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'Integer'},
            beamtalk_repl_ops_dev:walk_chain('WalkChainTestA', [size])
        )
    after
        cleanup_mock_class('WalkChainTestA', Pid)
    end.

walk_chain_broken_chain_test() ->
    %% Selector not in return types → mock returns {error, not_found} → undefined.
    %% Uses 'size' (a known atom) so tokenise_send_chain can convert it; the mock
    %% has no return type for it, so the registry returns {error, not_found}.
    Pid = spawn_mock_class_with_return_types('WalkChainTestB', #{}, #{}),
    try
        ?assertEqual(
            undefined,
            beamtalk_repl_ops_dev:walk_chain('WalkChainTestB', [size])
        )
    after
        cleanup_mock_class('WalkChainTestB', Pid)
    end.

walk_chain_class_empty_selectors_test() ->
    %% walk_chain_class with no selectors returns the class unchanged — no registry call
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_chain_class('Integer', [])).

walk_chain_class_single_hop_found_test() ->
    Pid = spawn_mock_class_with_return_types(
        'WalkChainClassTestA', #{}, #{new => 'WalkChainClassTestA'}
    ),
    try
        ?assertEqual(
            {ok, 'WalkChainClassTestA'},
            beamtalk_repl_ops_dev:walk_chain_class('WalkChainClassTestA', [new])
        )
    after
        cleanup_mock_class('WalkChainClassTestA', Pid)
    end.

%% BT-1048: `ClassName class` chain — `class` is an instance method on ProtoObject
%% (no class-side return-type annotation), but sending it to a class object returns
%% the metaclass, which has the same class-side methods. The chain should stay on the
%% class side rather than returning undefined.
walk_chain_class_class_selector_stays_on_class_side_test() ->
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_chain_class('Integer', [class])).

walk_chain_class_class_then_method_test() ->
    Pid = spawn_mock_class_with_return_types(
        'WalkChainClassB', #{}, #{new => 'WalkChainClassB'}
    ),
    try
        %% `WalkChainClassB class new` — `class` stays on class side, then `new` resolves
        ?assertEqual(
            {ok, 'WalkChainClassB'},
            beamtalk_repl_ops_dev:walk_chain_class('WalkChainClassB', [class, new])
        )
    after
        cleanup_mock_class('WalkChainClassB', Pid)
    end.

walk_chain_multi_hop_test() ->
    %% Two-hop chain: WalkChainMultiA →size→ WalkChainMultiB →reversed→ String
    PidA = spawn_mock_class_with_return_types(
        'WalkChainMultiA', #{size => 'WalkChainMultiB'}, #{}
    ),
    PidB = spawn_mock_class_with_return_types(
        'WalkChainMultiB', #{reversed => 'String'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'String'},
            beamtalk_repl_ops_dev:walk_chain('WalkChainMultiA', [size, reversed])
        )
    after
        cleanup_mock_class('WalkChainMultiA', PidA),
        cleanup_mock_class('WalkChainMultiB', PidB)
    end.

walk_chain_superclass_inheritance_test() ->
    %% 'WalkChainSub' has no return type for 'size'; its superclass 'WalkChainSuper'
    %% does.  get_method_return_type/2 walks the ETS hierarchy table, so walk_chain/2
    %% should resolve through the superclass.
    beamtalk_class_registry:ensure_hierarchy_table(),
    Saved = ets:tab2list(beamtalk_class_hierarchy),
    ets:delete_all_objects(beamtalk_class_hierarchy),
    ets:insert(beamtalk_class_hierarchy, {'WalkChainSub', 'WalkChainSuper'}),
    PidSub = spawn_mock_class_with_return_types('WalkChainSub', #{}, #{}),
    PidSuper = spawn_mock_class_with_return_types(
        'WalkChainSuper', #{size => 'Integer'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'Integer'},
            beamtalk_repl_ops_dev:walk_chain('WalkChainSub', [size])
        )
    after
        cleanup_mock_class('WalkChainSub', PidSub),
        cleanup_mock_class('WalkChainSuper', PidSuper),
        ets:delete_all_objects(beamtalk_class_hierarchy),
        ets:insert(beamtalk_class_hierarchy, Saved)
    end.

%%% resolve_chain_type/2 tests (BT-1006)

resolve_chain_type_single_hop_test() ->
    Pid = spawn_mock_class_with_return_types(
        'ResolveChainTestA', #{getValue => 'Integer'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'Integer'},
            beamtalk_repl_ops_dev:resolve_chain_type(<<"counter getValue">>, #{
                counter => #beamtalk_object{
                    class = 'ResolveChainTestA', class_mod = undefined, pid = self()
                }
            })
        )
    after
        cleanup_mock_class('ResolveChainTestA', Pid)
    end.

resolve_chain_type_multi_hop_test() ->
    %% Two-hop chain: counter getValue reversed → String
    PidA = spawn_mock_class_with_return_types(
        'ResolveChainMultiA', #{getValue => 'ResolveChainMultiB'}, #{}
    ),
    PidB = spawn_mock_class_with_return_types(
        'ResolveChainMultiB', #{reversed => 'String'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'String'},
            beamtalk_repl_ops_dev:resolve_chain_type(
                <<"counter getValue reversed">>,
                #{
                    counter => #beamtalk_object{
                        class = 'ResolveChainMultiA', class_mod = undefined, pid = self()
                    }
                }
            )
        )
    after
        cleanup_mock_class('ResolveChainMultiA', PidA),
        cleanup_mock_class('ResolveChainMultiB', PidB)
    end.

resolve_chain_type_broken_chain_returns_undefined_test() ->
    %% Unknown selector (not an existing atom) → tokeniser returns error → undefined.
    %% No mock needed: the receiver is a string literal classified by the real String class,
    %% and binary_to_existing_atom fails on the unknown selector before reaching the registry.
    ?assertEqual(
        undefined,
        beamtalk_repl_ops_dev:resolve_chain_type(
            <<"\"hello\" noSuchAnnotation">>, #{}
        )
    ).

resolve_chain_type_keyword_mid_chain_returns_undefined_test() ->
    %% Keyword sends mid-chain cannot be tokenised — returns undefined
    ?assertEqual(
        undefined,
        beamtalk_repl_ops_dev:resolve_chain_type(<<"myList inject: 0">>, #{})
    ).

resolve_chain_type_single_token_returns_undefined_test() ->
    %% Single token cannot be chain-resolved (must be at least receiver + selector)
    ?assertEqual(
        undefined,
        beamtalk_repl_ops_dev:resolve_chain_type(<<"hello">>, #{})
    ).

%%% get_context_completions chain resolution tests (BT-1006)

context_completions_chained_no_annotation_returns_empty_test() ->
    %% Chained expression where the selector exists as an atom but has no return-type
    %% annotation → mock returns {error, not_found} → walk_chain returns undefined →
    %% empty completions. Uses 'size' (a known atom) so tokenise_send_chain succeeds
    %% and the test exercises the "unannotated selector" path through the mock.
    Pid = spawn_mock_class_with_return_types('ChainCompletionTestA', #{}, #{}),
    try
        Bindings = #{
            myObj => #beamtalk_object{
                class = 'ChainCompletionTestA', class_mod = undefined, pid = self()
            }
        },
        Result = beamtalk_repl_ops_dev:get_context_completions(
            <<"myObj size cl">>, Bindings
        ),
        ?assertEqual([], Result)
    after
        cleanup_mock_class('ChainCompletionTestA', Pid)
    end.

context_completions_chained_with_return_type_test() ->
    %% Chained expression where the method has a return type → Integer completions
    Pid = spawn_mock_class_with_return_types(
        'ChainCompletionTestB', #{getValue => 'Integer'}, #{}
    ),
    try
        Bindings = #{
            myObj => #beamtalk_object{
                class = 'ChainCompletionTestB', class_mod = undefined, pid = self()
            }
        },
        Result = beamtalk_repl_ops_dev:get_context_completions(
            <<"myObj getValue cl">>, Bindings
        ),
        %% `class` is an Integer (Object) method — should appear in completions
        ?assert(lists:member(<<"class">>, Result))
    after
        cleanup_mock_class('ChainCompletionTestB', Pid)
    end.

%% Helper: spawn a mock class gen_server process and register it in the class registry.
spawn_mock_class(Name, ClassMethods, InstanceMethods) ->
    RegName = beamtalk_class_registry:registry_name(Name),
    Self = self(),
    Pid = spawn(fun() ->
        erlang:register(RegName, self()),
        Self ! registered,
        mock_class_loop(InstanceMethods, ClassMethods)
    end),
    receive
        registered -> ok
    after 1000 ->
        error({mock_class_register_timeout, Name})
    end,
    Pid.

mock_class_loop(InstanceMethods, ClassMethods) ->
    receive
        {'$gen_call', From, methods} ->
            gen_server:reply(From, InstanceMethods),
            mock_class_loop(InstanceMethods, ClassMethods);
        {'$gen_call', From, superclass} ->
            gen_server:reply(From, none),
            mock_class_loop(InstanceMethods, ClassMethods);
        {'$gen_call', From, get_local_class_methods} ->
            gen_server:reply(From, ClassMethods),
            mock_class_loop(InstanceMethods, ClassMethods);
        stop ->
            ok;
        _ ->
            mock_class_loop(InstanceMethods, ClassMethods)
    end.

cleanup_mock_class(Name, Pid) ->
    RegName = beamtalk_class_registry:registry_name(Name),
    catch erlang:unregister(RegName),
    Pid ! stop.

%% Helper: spawn a mock class that supports return-type lookups for chain resolution (BT-1006).
%% InstanceReturnTypes: #{selector() => atom()} — instance-side return types
%% ClassReturnTypes:    #{selector() => atom()} — class-side return types
spawn_mock_class_with_return_types(Name, InstanceReturnTypes, ClassReturnTypes) ->
    RegName = beamtalk_class_registry:registry_name(Name),
    Self = self(),
    Pid = spawn(fun() ->
        erlang:register(RegName, self()),
        Self ! registered,
        mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes)
    end),
    receive
        registered -> ok
    after 1000 ->
        error({mock_class_register_timeout, Name})
    end,
    Pid.

mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes) ->
    receive
        {'$gen_call', From, {get_method_return_type, Selector}} ->
            Reply =
                case maps:find(Selector, InstanceReturnTypes) of
                    {ok, ReturnType} -> {ok, ReturnType};
                    error -> {error, not_found}
                end,
            gen_server:reply(From, Reply),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        {'$gen_call', From, {get_class_method_return_type, Selector}} ->
            Reply =
                case maps:find(Selector, ClassReturnTypes) of
                    {ok, ReturnType} -> {ok, ReturnType};
                    error -> {error, not_found}
                end,
            gen_server:reply(From, Reply),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        {'$gen_call', From, superclass} ->
            gen_server:reply(From, none),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        {'$gen_call', From, methods} ->
            gen_server:reply(From, maps:keys(InstanceReturnTypes)),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        stop ->
            ok;
        _ ->
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes)
    end.

%%% resolve_class_to_module/1 tests

resolve_class_to_module_no_registry_test() ->
    %% With no class registry running, should return the class name itself
    ?assertEqual(
        some_unknown_class, beamtalk_repl_server:resolve_class_to_module(some_unknown_class)
    ).

%%% resolve_module_atoms/2 tests

resolve_module_atoms_explicit_test() ->
    ?assertEqual([my_module], beamtalk_repl_server:resolve_module_atoms(my_module, [])).

resolve_module_atoms_from_classes_test() ->
    %% Module name must exist as an atom
    _ = list_to_atom("erlang"),
    Classes = [#{name => "erlang"}],
    Result = beamtalk_repl_server:resolve_module_atoms(undefined, Classes),
    ?assert(lists:member(erlang, Result)).

resolve_module_atoms_empty_classes_test() ->
    ?assertEqual([], beamtalk_repl_server:resolve_module_atoms(undefined, [])).

resolve_module_atoms_unknown_class_test() ->
    Classes = [#{name => "xyzzy_nonexistent_class_99"}],
    ?assertEqual([], beamtalk_repl_server:resolve_module_atoms(undefined, Classes)).

resolve_module_atoms_binary_name_test() ->
    %% Test with binary class name (covers lines 917-920)
    _ = list_to_atom("erlang"),
    Classes = [#{name => <<"erlang">>}],
    Result = beamtalk_repl_server:resolve_module_atoms(undefined, Classes),
    ?assert(lists:member(erlang, Result)).

resolve_module_atoms_binary_unknown_test() ->
    Classes = [#{name => <<"xyzzy_nonexistent_binary_99">>}],
    ?assertEqual([], beamtalk_repl_server:resolve_module_atoms(undefined, Classes)).

resolve_module_atoms_empty_name_test() ->
    Classes = [#{name => ""}],
    ?assertEqual([], beamtalk_repl_server:resolve_module_atoms(undefined, Classes)).

resolve_module_atoms_other_type_test() ->
    Classes = [#{name => 42}],
    ?assertEqual([], beamtalk_repl_server:resolve_module_atoms(undefined, Classes)).

%%% generate_session_id/0 tests

generate_session_id_format_test() ->
    Id = beamtalk_repl_server:generate_session_id(),
    ?assert(is_binary(Id)),
    ?assertMatch({0, 8}, binary:match(Id, <<"session_">>)).

generate_session_id_unique_test() ->
    Id1 = beamtalk_repl_server:generate_session_id(),
    Id2 = beamtalk_repl_server:generate_session_id(),
    ?assertNotEqual(Id1, Id2).

%%% base_protocol_response/1 tests

base_protocol_response_with_id_session_test() ->
    Msg = {protocol_msg, <<"eval">>, <<"id1">>, <<"s1">>, #{}, false},
    Base = beamtalk_repl_server:base_protocol_response(Msg),
    ?assertEqual(<<"id1">>, maps:get(<<"id">>, Base)),
    ?assertEqual(<<"s1">>, maps:get(<<"session">>, Base)).

base_protocol_response_no_id_no_session_test() ->
    Msg = {protocol_msg, <<"eval">>, undefined, undefined, #{}, false},
    Base = beamtalk_repl_server:base_protocol_response(Msg),
    ?assertEqual(error, maps:find(<<"id">>, Base)),
    ?assertEqual(error, maps:find(<<"session">>, Base)).

%%% ensure_structured_error/1 tests

ensure_structured_error_already_structured_test() ->
    Err = beamtalk_error:new(some_kind, 'SomeClass'),
    ?assertEqual(Err, beamtalk_repl_server:ensure_structured_error(Err)).

ensure_structured_error_compile_error_binary_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({compile_error, <<"some msg">>}),
    ?assertEqual(compile_error, Err#beamtalk_error.kind),
    ?assertEqual('Compiler', Err#beamtalk_error.class).

ensure_structured_error_compile_error_list_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({compile_error, "some msg"}),
    ?assertEqual(compile_error, Err#beamtalk_error.kind).

ensure_structured_error_compile_error_other_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({compile_error, {some, reason}}),
    ?assertEqual(compile_error, Err#beamtalk_error.kind).

ensure_structured_error_undefined_variable_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({undefined_variable, <<"x">>}),
    ?assertEqual(undefined_variable, Err#beamtalk_error.kind).

ensure_structured_error_file_not_found_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({file_not_found, "/tmp/foo.bt"}),
    ?assertEqual(file_not_found, Err#beamtalk_error.kind).

ensure_structured_error_read_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({read_error, enoent}),
    ?assertEqual(io_error, Err#beamtalk_error.kind).

ensure_structured_error_load_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({load_error, bad_module}),
    ?assertEqual(io_error, Err#beamtalk_error.kind).

ensure_structured_error_parse_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({parse_error, {line, 1}}),
    ?assertEqual(compile_error, Err#beamtalk_error.kind).

ensure_structured_error_invalid_request_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({invalid_request, unknown_type}),
    ?assertEqual(internal_error, Err#beamtalk_error.kind).

ensure_structured_error_empty_expression_test() ->
    Err = beamtalk_repl_server:ensure_structured_error(empty_expression),
    ?assertEqual(empty_expression, Err#beamtalk_error.kind).

ensure_structured_error_timeout_test() ->
    Err = beamtalk_repl_server:ensure_structured_error(timeout),
    ?assertEqual(timeout, Err#beamtalk_error.kind).

ensure_structured_error_unknown_term_test() ->
    Err = beamtalk_repl_server:ensure_structured_error(some_random_term),
    ?assertEqual(internal_error, Err#beamtalk_error.kind).

ensure_structured_error_eval_error_nested_test() ->
    Inner = beamtalk_error:new(type_error, 'Integer'),
    Err = beamtalk_repl_server:ensure_structured_error({eval_error, error, Inner}),
    ?assertEqual(type_error, Err#beamtalk_error.kind).

ensure_structured_error_eval_error_raw_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({eval_error, error, badarg}),
    ?assertEqual(internal_error, Err#beamtalk_error.kind).

%%% ensure_structured_error/2 tests

ensure_structured_error_2_already_structured_test() ->
    Err = beamtalk_error:new(some_kind, 'SomeClass'),
    ?assertEqual(Err, beamtalk_repl_server:ensure_structured_error(Err, error)).

ensure_structured_error_2_compile_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({compile_error, <<"msg">>}, error),
    ?assertEqual(compile_error, Err#beamtalk_error.kind).

ensure_structured_error_2_unknown_test() ->
    Err = beamtalk_repl_server:ensure_structured_error(some_reason, error),
    ?assertEqual(internal_error, Err#beamtalk_error.kind),
    ?assert(binary:match(Err#beamtalk_error.message, <<"error:">>) =/= nomatch).

ensure_structured_error_2_eval_error_test() ->
    Inner = beamtalk_error:new(does_not_understand, 'Counter'),
    Err = beamtalk_repl_server:ensure_structured_error({eval_error, error, Inner}, error),
    ?assertEqual(does_not_understand, Err#beamtalk_error.kind).

ensure_structured_error_2_undefined_variable_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({undefined_variable, <<"x">>}, error),
    ?assertEqual(undefined_variable, Err#beamtalk_error.kind).

ensure_structured_error_2_file_not_found_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({file_not_found, "/path"}, error),
    ?assertEqual(file_not_found, Err#beamtalk_error.kind).

ensure_structured_error_2_read_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({read_error, enoent}, error),
    ?assertEqual(io_error, Err#beamtalk_error.kind).

ensure_structured_error_2_load_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({load_error, bad}, error),
    ?assertEqual(io_error, Err#beamtalk_error.kind).

ensure_structured_error_2_parse_error_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({parse_error, x}, error),
    ?assertEqual(compile_error, Err#beamtalk_error.kind).

ensure_structured_error_2_invalid_request_test() ->
    Err = beamtalk_repl_server:ensure_structured_error({invalid_request, x}, error),
    ?assertEqual(internal_error, Err#beamtalk_error.kind).

ensure_structured_error_2_beamtalk_object_test() ->
    Inner = beamtalk_error:new(type_error, 'String'),
    Obj = #{'$beamtalk_class' => 'Error', error => Inner},
    Err = beamtalk_repl_server:ensure_structured_error(Obj, error),
    ?assertEqual(type_error, Err#beamtalk_error.kind).

ensure_structured_error_beamtalk_object_test() ->
    Inner = beamtalk_error:new(does_not_understand, 'Integer'),
    Obj = #{'$beamtalk_class' => 'Error', error => Inner},
    Err = beamtalk_repl_server:ensure_structured_error(Obj),
    ?assertEqual(does_not_understand, Err#beamtalk_error.kind).

%%% make_class_not_found_error/1 tests

make_class_not_found_error_atom_test() ->
    Err = beamtalk_repl_server:make_class_not_found_error('Counter'),
    ?assertEqual(class_not_found, Err#beamtalk_error.kind),
    ?assert(binary:match(Err#beamtalk_error.message, <<"Counter">>) =/= nomatch).

make_class_not_found_error_binary_test() ->
    Err = beamtalk_repl_server:make_class_not_found_error(<<"MyClass">>),
    ?assertEqual(class_not_found, Err#beamtalk_error.kind),
    ?assert(binary:match(Err#beamtalk_error.message, <<"MyClass">>) =/= nomatch).

%%% format_name/1 tests

format_name_atom_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_server:format_name(hello)).

format_name_binary_test() ->
    ?assertEqual(<<"world">>, beamtalk_repl_server:format_name(<<"world">>)).

format_name_list_test() ->
    ?assertEqual(<<"abc">>, beamtalk_repl_server:format_name("abc")).

format_name_other_test() ->
    Result = beamtalk_repl_server:format_name({some, tuple}),
    ?assert(is_binary(Result)).

%% ===================================================================
%% handle_op direct tests (BT-627)
%% These call handle_op/4 directly to ensure coverage in the test process
%% (TCP tests run handle_op in spawned processes which may not report cover).
%% ===================================================================

%% Helper: create a proper protocol message via decode
make_proto_msg(Op, Id) ->
    make_proto_msg(Op, Id, #{}).

make_proto_msg(Op, Id, Params) ->
    Json = jsx:encode(#{<<"op">> => Op, <<"id">> => Id, <<"params">> => Params}),
    {ok, Msg} = beamtalk_repl_protocol:decode(Json),
    Msg.

handle_op_actors_no_registry_test() ->
    Msg = make_proto_msg(<<"actors">>, <<"a1">>),
    Result = beamtalk_repl_server:handle_op(<<"actors">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"a1">>}, Decoded).

handle_op_close_test() ->
    Msg = make_proto_msg(<<"close">>, <<"c1">>),
    Result = beamtalk_repl_server:handle_op(<<"close">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"c1">>}, Decoded).

handle_op_unknown_test() ->
    Msg = make_proto_msg(<<"foobar">>, <<"u1">>),
    Result = beamtalk_repl_server:handle_op(<<"foobar">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"u1">>}, Decoded).

handle_op_inspect_invalid_pid_test() ->
    Msg = make_proto_msg(<<"inspect">>, <<"i1">>, #{<<"actor">> => <<"notapid">>}),
    Params = #{<<"actor">> => <<"notapid">>},
    Result = beamtalk_repl_server:handle_op(<<"inspect">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"i1">>}, Decoded).

handle_op_inspect_dead_actor_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    PidStr = list_to_binary(pid_to_list(Pid)),
    Msg = make_proto_msg(<<"inspect">>, <<"i2">>, #{<<"actor">> => PidStr}),
    Params = #{<<"actor">> => PidStr},
    Result = beamtalk_repl_server:handle_op(<<"inspect">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"i2">>}, Decoded).

handle_op_inspect_live_non_actor_test() ->
    Pid = spawn(fun() ->
        receive
            stop -> ok
        after 5000 -> ok
        end
    end),
    PidStr = list_to_binary(pid_to_list(Pid)),
    Msg = make_proto_msg(<<"inspect">>, <<"i3">>, #{<<"actor">> => PidStr}),
    Params = #{<<"actor">> => PidStr},
    Result = beamtalk_repl_server:handle_op(<<"inspect">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"i3">>}, Decoded),
    Pid ! stop.

%% Success path: inspect a live tagged-map actor returns a field map, not a string.
%% Regression test for the bug where state was returned as a binary string, causing
%% the TypeScript client's Object.entries() to iterate characters instead of fields.
handle_op_inspect_live_tagged_actor_test() ->
    %% Ensure no stale registry is running
    case whereis(beamtalk_actor_registry) of
        undefined ->
            ok;
        Old ->
            Ref = erlang:monitor(process, Old),
            catch gen_server:stop(Old),
            receive
                {'DOWN', Ref, process, Old, _} -> ok
            after 1000 ->
                ok
            end
    end,
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    {ok, ActorPid} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    PidStr = list_to_binary(pid_to_list(ActorPid)),
    Msg = make_proto_msg(<<"inspect">>, <<"i4">>, #{<<"actor">> => PidStr}),
    Params = #{<<"actor">> => PidStr},
    try
        Result = beamtalk_repl_server:handle_op(<<"inspect">>, Params, Msg, self()),
        Decoded = jsx:decode(Result, [return_maps]),
        ?assertMatch(#{<<"id">> := <<"i4">>}, Decoded),
        %% state must be a JSON object (map), not a string
        State = maps:get(<<"state">>, Decoded),
        ?assert(is_map(State)),
        %% user field 'value' must be present; internal fields must be absent
        ?assert(maps:is_key(<<"value">>, State)),
        ?assertNot(maps:is_key(<<"$beamtalk_class">>, State)),
        ?assertNot(maps:is_key(<<"__methods__">>, State))
    after
        catch gen_server:stop(ActorPid),
        catch gen_server:stop(RegistryPid)
    end.

handle_op_kill_invalid_pid_test() ->
    Msg = make_proto_msg(<<"kill">>, <<"k1">>, #{<<"actor">> => <<"notapid">>}),
    Params = #{<<"actor">> => <<"notapid">>},
    Result = beamtalk_repl_server:handle_op(<<"kill">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"k1">>}, Decoded).

handle_op_kill_valid_pid_test() ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        after 5000 -> ok
        end
    end),
    PidStr = list_to_binary(pid_to_list(Pid)),
    Msg = make_proto_msg(<<"kill">>, <<"k2">>, #{<<"actor">> => PidStr}),
    Params = #{<<"actor">> => PidStr},
    Result = beamtalk_repl_server:handle_op(<<"kill">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"k2">>}, Decoded).

handle_op_complete_empty_test() ->
    Msg = make_proto_msg(<<"complete">>, <<"cp1">>, #{<<"code">> => <<>>}),
    Params = #{<<"code">> => <<>>},
    Result = beamtalk_repl_server:handle_op(<<"complete">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"cp1">>}, Decoded).

handle_op_complete_with_prefix_test() ->
    Msg = make_proto_msg(<<"complete">>, <<"cp2">>, #{<<"code">> => <<"sel">>}),
    Params = #{<<"code">> => <<"sel">>},
    Result = beamtalk_repl_server:handle_op(<<"complete">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"completions">> := _}, Decoded).

handle_op_docs_unknown_class_test() ->
    Msg = make_proto_msg(<<"docs">>, <<"d1">>, #{<<"class">> => <<"NonexistentClassXyz">>}),
    Params = #{<<"class">> => <<"NonexistentClassXyz">>},
    Result = beamtalk_repl_server:handle_op(<<"docs">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"d1">>}, Decoded).

handle_op_unload_empty_test() ->
    Msg = make_proto_msg(<<"unload">>, <<"ul1">>, #{<<"module">> => <<>>}),
    Params = #{<<"module">> => <<>>},
    Result = beamtalk_repl_server:handle_op(<<"unload">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"ul1">>}, Decoded).

handle_op_unload_nonexistent_test() ->
    Msg = make_proto_msg(<<"unload">>, <<"ul2">>, #{
        <<"module">> => <<"definitely_never_loaded_xyz">>
    }),
    Params = #{<<"module">> => <<"definitely_never_loaded_xyz">>},
    Result = beamtalk_repl_server:handle_op(<<"unload">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"ul2">>}, Decoded).

handle_op_reload_empty_test() ->
    Msg = make_proto_msg(<<"reload">>, <<"r1">>, #{<<"module">> => <<>>}),
    Params = #{<<"module">> => <<>>},
    Result = beamtalk_repl_server:handle_op(<<"reload">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"r1">>}, Decoded).

handle_op_reload_nonexistent_module_test() ->
    Msg = make_proto_msg(<<"reload">>, <<"r2">>, #{<<"module">> => <<"never_existed_xyz_99">>}),
    Params = #{<<"module">> => <<"never_existed_xyz_99">>},
    Result = beamtalk_repl_server:handle_op(<<"reload">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"r2">>}, Decoded).

handle_op_eval_empty_test() ->
    Msg = make_proto_msg(<<"eval">>, <<"e1">>, #{<<"code">> => <<>>}),
    Params = #{<<"code">> => <<>>},
    Result = beamtalk_repl_server:handle_op(<<"eval">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"e1">>}, Decoded).

handle_op_sessions_no_sup_test() ->
    Msg = make_proto_msg(<<"sessions">>, <<"s1">>),
    Result = beamtalk_repl_server:handle_op(<<"sessions">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"s1">>}, Decoded).

%% ===================================================================
%% show-codegen handle_op tests (BT-700)
%% ===================================================================

handle_op_show_codegen_empty_test() ->
    %% Empty code should return error
    Msg = make_proto_msg(<<"show-codegen">>, <<"sc1">>, #{<<"code">> => <<>>}),
    Params = #{<<"code">> => <<>>},
    Result = beamtalk_repl_server:handle_op(<<"show-codegen">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"sc1">>}, Decoded),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    Status = maps:get(<<"status">>, Decoded),
    ?assert(lists:member(<<"done">>, Status)),
    ?assert(lists:member(<<"error">>, Status)).

handle_op_show_codegen_no_code_param_test() ->
    %% Missing code param defaults to empty binary -> error
    Msg = make_proto_msg(<<"show-codegen">>, <<"sc2">>),
    Params = #{},
    Result = beamtalk_repl_server:handle_op(<<"show-codegen">>, Params, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"sc2">>}, Decoded),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_op_show_codegen_dead_session_test() ->
    %% Using a dead session PID should be caught by handle_protocol_request
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(50),
    Json = jsx:encode(#{
        <<"op">> => <<"show-codegen">>,
        <<"id">> => <<"sc3">>,
        <<"params">> => #{<<"code">> => <<"1 + 2">>}
    }),
    {ok, Msg} = beamtalk_repl_protocol:decode(Json),
    Result = beamtalk_repl_server:handle_protocol_request(Msg, DeadPid),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"sc3">>}, Decoded),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_op_show_codegen_in_describe_test() ->
    %% Verify show-codegen appears in describe ops
    Msg = make_proto_msg(<<"describe">>, <<"d1">>),
    Result = beamtalk_repl_server:handle_op(<<"describe">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"show-codegen">>, Ops)),
    ?assertMatch(#{<<"params">> := [<<"code">>]}, maps:get(<<"show-codegen">>, Ops)).

%% ===================================================================
%% handle_protocol_request direct test (BT-627)
%% ===================================================================

handle_protocol_request_crash_test() ->
    %% Test the catch clause in handle_protocol_request (lines 373-382)
    %% Create a message that will cause handle_op to crash
    %% Use eval op with a SessionPid that's dead
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(50),
    Json = jsx:encode(#{
        <<"op">> => <<"eval">>,
        <<"id">> => <<"crash1">>,
        <<"params">> => #{<<"code">> => <<"test">>}
    }),
    {ok, Msg} = beamtalk_repl_protocol:decode(Json),
    Result = beamtalk_repl_server:handle_protocol_request(Msg, DeadPid),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"crash1">>}, Decoded).

%% ===================================================================
%% recv_line tests removed — function replaced by WebSocket handler (ADR 0020)
%% ===================================================================
%% write_port_file tests (BT-627)
%% ===================================================================

write_port_file_undefined_test() ->
    %% Test with undefined workspace ID (line 146-147)
    ?assertEqual(ok, beamtalk_repl_server:write_port_file(undefined, 8080, <<"nonce">>)).

write_port_file_with_workspace_test() ->
    %% Test with a real workspace ID - will attempt to create directories
    %% This exercises lines 148-175
    Result = beamtalk_repl_server:write_port_file(<<"test_ws_627">>, 9999, <<"abc123">>),
    ?assertEqual(ok, Result),
    %% Clean up if file was created
    case beamtalk_platform:home_dir() of
        false ->
            ok;
        Home ->
            PortFile = filename:join([Home, ".beamtalk", "workspaces", "test_ws_627", "port"]),
            file:delete(PortFile)
    end.

%% ===================================================================
%% gen_server callback direct tests (BT-627)
%% ===================================================================

handle_cast_unknown_test() ->
    %% Test handle_cast with unknown message (line 110)
    %% We can't call handle_cast directly without a state, so use gen_server
    %% Instead, verify through module_info that the callback exists
    Exports = beamtalk_repl_server:module_info(exports),
    ?assert(lists:member({handle_cast, 2}, Exports)).

code_change_test() ->
    %% Test code_change callback (line 137)
    Exports = beamtalk_repl_server:module_info(exports),
    ?assert(lists:member({code_change, 3}, Exports)).

%%% BT-666: Interrupt operation tests

%% Test: interrupt op when no evaluation is running returns ok
tcp_interrupt_no_eval_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"interrupt">>, <<"id">> => <<"int1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"int1">>}, Resp),
    ?assert(lists:member(<<"done">>, maps:get(<<"status">>, Resp))).

%% Test: interrupt op with an unknown session ID falls back to current session
tcp_interrupt_unknown_session_test(Port) ->
    Msg = jsx:encode(#{
        <<"op">> => <<"interrupt">>,
        <<"id">> => <<"int2">>,
        <<"session">> => <<"nonexistent_session">>
    }),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"int2">>}, Resp),
    ?assert(lists:member(<<"done">>, maps:get(<<"status">>, Resp))).

%%% BT-686: Browser page (index.html) tests
%%% These verify the cowboy_static route serves the workspace browser UI
%%% and that the HTML contains expected interactive elements.

%% Helper: HTTP GET request via raw TCP (avoids inets dependency)
http_get(Port, Path) ->
    {ok, Sock} = gen_tcp:connect(
        {127, 0, 0, 1},
        Port,
        [binary, {active, false}, {packet, raw}],
        5000
    ),
    Req = [
        <<"GET ">>,
        Path,
        <<" HTTP/1.1\r\n">>,
        <<"Host: 127.0.0.1:">>,
        integer_to_binary(Port),
        <<"\r\n">>,
        <<"Connection: close\r\n">>,
        <<"\r\n">>
    ],
    ok = gen_tcp:send(Sock, Req),
    {ok, Response} = http_read_all(Sock, <<>>),
    gen_tcp:close(Sock),
    Response.

http_read_all(Sock, Acc) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} -> http_read_all(Sock, <<Acc/binary, Data/binary>>);
        {error, closed} -> {ok, Acc}
    end.

%% Extract HTTP body (everything after \r\n\r\n)
http_body(Response) ->
    case binary:match(Response, <<"\r\n\r\n">>) of
        {Pos, Len} ->
            Start = Pos + Len,
            binary:part(Response, Start, byte_size(Response) - Start);
        nomatch ->
            <<>>
    end.

%% Test: GET / returns 200 with HTML content
http_index_page_test(Port) ->
    Response = http_get(Port, <<"/">>),
    ?assert(binary:match(Response, <<"200">>) =/= nomatch),
    ?assert(binary:match(Response, <<"text/html">>) =/= nomatch),
    Body = http_body(Response),
    ?assert(binary:match(Body, <<"<!DOCTYPE html">>) =/= nomatch).

%% Test: HTML contains cookie input and connect button (auth panel)
http_index_has_auth_panel_test(Port) ->
    Body = http_body(http_get(Port, <<"/">>)),
    ?assert(binary:match(Body, <<"cookie-input">>) =/= nomatch),
    ?assert(binary:match(Body, <<"connect-btn">>) =/= nomatch),
    ?assert(binary:match(Body, <<"auth-panel">>) =/= nomatch).

%% Test: HTML contains transcript pane
http_index_has_transcript_test(Port) ->
    Body = http_body(http_get(Port, <<"/">>)),
    ?assert(binary:match(Body, <<"transcript">>) =/= nomatch),
    ?assert(binary:match(Body, <<"Transcript">>) =/= nomatch).

%% Test: HTML contains eval input and send button
http_index_has_eval_panel_test(Port) ->
    Body = http_body(http_get(Port, <<"/">>)),
    ?assert(binary:match(Body, <<"eval-input">>) =/= nomatch),
    ?assert(binary:match(Body, <<"send-btn">>) =/= nomatch),
    ?assert(binary:match(Body, <<"eval-panel">>) =/= nomatch).

%% Test: HTML serves workspace page with static JS reference
http_index_has_websocket_js_test(Port) ->
    Body = http_body(http_get(Port, <<"/">>)),
    ?assert(binary:match(Body, <<"workspace.js">>) =/= nomatch),
    ?assert(binary:match(Body, <<"workspace.css">>) =/= nomatch),
    ?assert(binary:match(Body, <<"Beamtalk Workspace">>) =/= nomatch),
    ?assert(binary:match(Body, <<"auth">>) =/= nomatch).

%% Test: GET /ws without WebSocket upgrade headers returns error (not 101)
http_ws_no_upgrade_test(Port) ->
    Response = http_get(Port, <<"/ws">>),
    %% Without Upgrade: websocket header, cowboy should not return 101
    ?assert(binary:match(Response, <<"101">>) =:= nomatch).
