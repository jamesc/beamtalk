%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_http_response module (BT-1115).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests cover all accessor functions:
%%% - status/1 accessor
%%% - headers/1 accessor
%%% - body/1 accessor
%%% - ok/1 for 2xx/non-2xx ranges
%%% - bodyAsJson/1 delegating to beamtalk_json
%%% - printString/1 human-readable representation

-module(beamtalk_http_response_tests).

-include("beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test helpers
%%% ============================================================================

make_resp(Status, Headers, Body) ->
    #{
        '$beamtalk_class' => 'HTTPResponse',
        status => Status,
        headers => Headers,
        body => Body
    }.

%%% ============================================================================
%%% status/1
%%% ============================================================================

status_200_test() ->
    Resp = make_resp(200, [], <<>>),
    ?assertEqual(200, beamtalk_http_response:status(Resp)).

status_404_test() ->
    Resp = make_resp(404, [], <<>>),
    ?assertEqual(404, beamtalk_http_response:status(Resp)).

status_500_test() ->
    Resp = make_resp(500, [], <<>>),
    ?assertEqual(500, beamtalk_http_response:status(Resp)).

%%% ============================================================================
%%% headers/1
%%% ============================================================================

headers_empty_test() ->
    Resp = make_resp(200, [], <<>>),
    ?assertEqual([], beamtalk_http_response:headers(Resp)).

headers_single_test() ->
    Headers = [[<<"content-type">>, <<"application/json">>]],
    Resp = make_resp(200, Headers, <<>>),
    ?assertEqual(Headers, beamtalk_http_response:headers(Resp)).

headers_multiple_test() ->
    Headers = [
        [<<"content-type">>, <<"text/plain">>],
        [<<"x-custom">>, <<"value">>]
    ],
    Resp = make_resp(200, Headers, <<>>),
    ?assertEqual(Headers, beamtalk_http_response:headers(Resp)).

%%% ============================================================================
%%% body/1
%%% ============================================================================

body_empty_test() ->
    Resp = make_resp(204, [], <<>>),
    ?assertEqual(<<>>, beamtalk_http_response:body(Resp)).

body_text_test() ->
    Resp = make_resp(200, [], <<"Hello, World!">>),
    ?assertEqual(<<"Hello, World!">>, beamtalk_http_response:body(Resp)).

body_json_test() ->
    Json = <<"{\"ok\":true}">>,
    Resp = make_resp(200, [], Json),
    ?assertEqual(Json, beamtalk_http_response:body(Resp)).

%%% ============================================================================
%%% ok/1 — 2xx detection
%%% ============================================================================

ok_200_test() ->
    Resp = make_resp(200, [], <<>>),
    ?assert(beamtalk_http_response:'ok'(Resp)).

ok_201_test() ->
    Resp = make_resp(201, [], <<>>),
    ?assert(beamtalk_http_response:'ok'(Resp)).

ok_299_test() ->
    Resp = make_resp(299, [], <<>>),
    ?assert(beamtalk_http_response:'ok'(Resp)).

ok_100_test() ->
    Resp = make_resp(100, [], <<>>),
    ?assertNot(beamtalk_http_response:'ok'(Resp)).

ok_300_test() ->
    Resp = make_resp(300, [], <<>>),
    ?assertNot(beamtalk_http_response:'ok'(Resp)).

ok_404_test() ->
    Resp = make_resp(404, [], <<>>),
    ?assertNot(beamtalk_http_response:'ok'(Resp)).

ok_500_test() ->
    Resp = make_resp(500, [], <<>>),
    ?assertNot(beamtalk_http_response:'ok'(Resp)).

%%% ============================================================================
%%% bodyAsJson/1
%%% ============================================================================

body_as_json_integer_test() ->
    Resp = make_resp(200, [], <<"42">>),
    ?assertEqual(42, beamtalk_http_response:'bodyAsJson'(Resp)).

body_as_json_array_test() ->
    Resp = make_resp(200, [], <<"[1,2,3]">>),
    ?assertEqual([1, 2, 3], beamtalk_http_response:'bodyAsJson'(Resp)).

body_as_json_null_test() ->
    Resp = make_resp(200, [], <<"null">>),
    ?assertEqual(nil, beamtalk_http_response:'bodyAsJson'(Resp)).

body_as_json_object_test() ->
    Resp = make_resp(200, [], <<"{\"key\":\"val\"}">>),
    Result = beamtalk_http_response:'bodyAsJson'(Resp),
    ?assertMatch(#{<<"key">> := <<"val">>}, Result).

%%% ============================================================================
%%% printString/1
%%% ============================================================================

print_string_test() ->
    Resp = make_resp(200, [], <<>>),
    ?assertEqual(<<"an HTTPResponse(200)">>, beamtalk_http_response:'printString'(Resp)).

print_string_404_test() ->
    Resp = make_resp(404, [], <<>>),
    ?assertEqual(<<"an HTTPResponse(404)">>, beamtalk_http_response:'printString'(Resp)).
