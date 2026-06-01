%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_session_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_session module.

Covers the shutdown handler (lines 75-97), which was 0% covered prior to
this test file. The sessions, close, health, and clone (success) handlers
are already exercised by integration tests and are not duplicated here.

The shutdown handler validates the Erlang node cookie via constant-time
comparison (crypto:hash_equals/2) before scheduling a graceful shutdown.
All tests run without any live services:
- erlang:get_cookie/0 is always available in any Erlang node.
- erlang:send_after/3 called with an unregistered atom destination
  silently discards the message when the timer fires — no crash.
- beamtalk_workspace_meta and beamtalk_repl_server are not involved
  in this handler's cookie-validation branches.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op, Id, Session, Legacy) ->
    {protocol_msg, Op, Id, Session, #{}, Legacy}.

%%====================================================================
%% shutdown op — invalid cookie (false branch)
%%====================================================================

shutdown_missing_cookie_returns_auth_error_test() ->
    %% No <<"cookie">> key → maps:get defaults to <<>> → size mismatch → false.
    Msg = make_msg(<<"shutdown">>, <<"sd-1">>, undefined, false),
    Result = beamtalk_repl_ops_session:handle(<<"shutdown">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(binary:match(maps:get(<<"error">>, Decoded), <<"Invalid cookie">>) =/= nomatch).

shutdown_empty_cookie_returns_auth_error_test() ->
    %% Explicit empty binary → byte_size(<<>>) =/= byte_size(NodeCookie) → false.
    Msg = make_msg(<<"shutdown">>, <<"sd-2">>, undefined, false),
    Result = beamtalk_repl_ops_session:handle(
        <<"shutdown">>, #{<<"cookie">> => <<>>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(binary:match(maps:get(<<"error">>, Decoded), <<"Invalid cookie">>) =/= nomatch).

shutdown_wrong_length_cookie_returns_auth_error_test() ->
    %% Cookie one byte longer than the real cookie → byte_size check short-circuits
    %% before crypto:hash_equals/2 is ever called.
    NodeCookieBin = atom_to_binary(erlang:get_cookie(), utf8),
    WrongLen = byte_size(NodeCookieBin) + 1,
    WrongCookie = binary:copy(<<$X>>, WrongLen),
    Msg = make_msg(<<"shutdown">>, <<"sd-3">>, undefined, false),
    Result = beamtalk_repl_ops_session:handle(
        <<"shutdown">>, #{<<"cookie">> => WrongCookie}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(binary:match(maps:get(<<"error">>, Decoded), <<"Invalid cookie">>) =/= nomatch).

shutdown_same_length_wrong_cookie_exercises_hash_equals_test() ->
    %% Same byte-length as the real cookie but different content → exercises the
    %% crypto:hash_equals/2 false branch (constant-time comparison returns false).
    NodeCookieBin = atom_to_binary(erlang:get_cookie(), utf8),
    Len = byte_size(NodeCookieBin),
    %% XOR the first byte to guarantee the binary differs from the real cookie.
    <<First, Rest/binary>> = NodeCookieBin,
    WrongCookie =
        case <<(First bxor 16#FF), Rest/binary>> of
            NodeCookieBin -> binary:copy(<<$X>>, Len);
            Flipped -> Flipped
        end,
    Msg = make_msg(<<"shutdown">>, <<"sd-4">>, undefined, false),
    Result = beamtalk_repl_ops_session:handle(
        <<"shutdown">>, #{<<"cookie">> => WrongCookie}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assert(binary:match(maps:get(<<"error">>, Decoded), <<"Invalid cookie">>) =/= nomatch).

shutdown_error_response_includes_status_done_error_test() ->
    %% The non-legacy error response must include status: [done, error].
    Msg = make_msg(<<"shutdown">>, <<"sd-5">>, undefined, false),
    Result = beamtalk_repl_ops_session:handle(<<"shutdown">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% shutdown op — valid cookie (true branch)
%%====================================================================

shutdown_valid_cookie_returns_ok_status_test() ->
    %% Correct node cookie → ValidCookie = true → encode_status(ok) → done JSON.
    %% erlang:send_after/3 targets the atom beamtalk_repl_server which is not
    %% registered in unit tests; the timer message is silently discarded on fire.
    NodeCookieBin = atom_to_binary(erlang:get_cookie(), utf8),
    Msg = make_msg(<<"shutdown">>, <<"sd-6">>, undefined, false),
    Result = beamtalk_repl_ops_session:handle(
        <<"shutdown">>, #{<<"cookie">> => NodeCookieBin}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual(false, maps:is_key(<<"error">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% shutdown op — legacy protocol format
%%====================================================================

shutdown_valid_cookie_legacy_returns_result_type_test() ->
    %% Legacy protocol: response uses <<"type">> = <<"result">> + <<"value">> = <<"ok">>.
    NodeCookieBin = atom_to_binary(erlang:get_cookie(), utf8),
    Msg = make_msg(<<"shutdown">>, undefined, undefined, true),
    Result = beamtalk_repl_ops_session:handle(
        <<"shutdown">>, #{<<"cookie">> => NodeCookieBin}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"ok">>, maps:get(<<"value">>, Decoded)).

shutdown_wrong_cookie_legacy_returns_error_type_test() ->
    %% Legacy protocol error: <<"type">> = <<"error">>, <<"message">> key (not <<"error">>).
    Msg = make_msg(<<"shutdown">>, undefined, undefined, true),
    Result = beamtalk_repl_ops_session:handle(
        <<"shutdown">>, #{<<"cookie">> => <<"definitely-wrong-cookie">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(maps:is_key(<<"message">>, Decoded)),
    ?assert(
        binary:match(maps:get(<<"message">>, Decoded), <<"Invalid cookie">>) =/= nomatch
    ).
