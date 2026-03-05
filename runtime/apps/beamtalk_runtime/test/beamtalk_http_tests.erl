%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_http module (BT-1114).
%%%
%%% Tests cover pure, non-network functions:
%%% - URL parsing (parse_url/1)
%%% - Method normalisation (normalise_method/1)
%%% - Request option validation (validate_request_options/3)
%%% - Header conversion (to_gun_headers/2, from_gun_headers/1)
%%% - has_method/1 and dispatch/3 routing
%%% - Type error guards for each public selector
%%% - Deadline / remaining helpers

-module(beamtalk_http_tests).

-include("beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_get_test() ->
    ?assert(beamtalk_http:has_method('get:')).

has_method_get_headers_test() ->
    ?assert(beamtalk_http:has_method('get:headers:')).

has_method_post_body_test() ->
    ?assert(beamtalk_http:has_method('post:body:')).

has_method_post_headers_body_test() ->
    ?assert(beamtalk_http:has_method('post:headers:body:')).

has_method_put_body_test() ->
    ?assert(beamtalk_http:has_method('put:body:')).

has_method_put_headers_body_test() ->
    ?assert(beamtalk_http:has_method('put:headers:body:')).

has_method_delete_test() ->
    ?assert(beamtalk_http:has_method('delete:')).

has_method_delete_headers_test() ->
    ?assert(beamtalk_http:has_method('delete:headers:')).

has_method_request_url_options_test() ->
    ?assert(beamtalk_http:has_method('request:url:options:')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_http:has_method('frobulate:')).

%%% ============================================================================
%%% dispatch/3 — class identity
%%% ============================================================================

dispatch_class_test() ->
    Self = #{'$beamtalk_class' => 'Http'},
    ?assertEqual('Http', beamtalk_http:dispatch('class', [], Self)).

dispatch_printString_test() ->
    Self = #{'$beamtalk_class' => 'Http'},
    ?assertEqual(<<"Http">>, beamtalk_http:dispatch('printString', [], Self)).

dispatch_does_not_understand_test() ->
    Self = #{'$beamtalk_class' => 'Http'},
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, selector = 'frobulate:'}
        },
        beamtalk_http:dispatch('frobulate:', [<<"x">>], Self)
    ).

%%% ============================================================================
%%% Type error guards — invalid URL
%%% ============================================================================

get_type_error_non_binary_url_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'get:headers:'}
        },
        beamtalk_http:'get:headers:'(not_a_url, [])
    ).

get_type_error_non_list_headers_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'get:headers:'}
        },
        beamtalk_http:'get:headers:'(<<"http://example.com">>, not_a_list)
    ).

post_type_error_non_binary_url_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'post:headers:body:'}
        },
        beamtalk_http:'post:headers:body:'(42, [], <<"body">>)
    ).

post_type_error_non_list_headers_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'post:headers:body:'}
        },
        beamtalk_http:'post:headers:body:'(<<"http://example.com">>, bad, <<"body">>)
    ).

post_type_error_non_binary_body_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'post:headers:body:'}
        },
        beamtalk_http:'post:headers:body:'(<<"http://example.com">>, [], 42)
    ).

delete_type_error_non_binary_url_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'delete:headers:'}
        },
        beamtalk_http:'delete:headers:'(42, [])
    ).

delete_type_error_non_list_headers_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'delete:headers:'}
        },
        beamtalk_http:'delete:headers:'(<<"http://example.com">>, oops)
    ).

request_type_error_non_binary_url_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:'request:url:options:'(get, 42, #{})
    ).

request_type_error_non_map_options_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:'request:url:options:'(get, <<"http://example.com">>, not_a_map)
    ).

%%% ============================================================================
%%% Invalid URL raises http_error
%%% ============================================================================

get_invalid_url_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = http_error, selector = 'get:headers:'}
        },
        beamtalk_http:'get:'(<<"not-a-url">>)
    ).

get_ftp_url_rejected_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = http_error, selector = 'get:headers:'}
        },
        beamtalk_http:'get:'(<<"ftp://example.com/file">>)
    ).

%%% ============================================================================
%%% normalise_method/1
%%% ============================================================================

normalise_method_binary_lowercase_test() ->
    Result = beamtalk_http:normalise_method(<<"get">>),
    ?assertEqual(<<"GET">>, Result),
    ?assert(is_binary(Result)).

normalise_method_binary_uppercase_test() ->
    Result = beamtalk_http:normalise_method(<<"POST">>),
    ?assertEqual(<<"POST">>, Result),
    ?assert(is_binary(Result)).

normalise_method_atom_test() ->
    Result = beamtalk_http:normalise_method(delete),
    ?assertEqual(<<"DELETE">>, Result),
    ?assert(is_binary(Result)).

normalise_method_atom_uppercase_test() ->
    Result = beamtalk_http:normalise_method('PUT'),
    ?assertEqual(<<"PUT">>, Result),
    ?assert(is_binary(Result)).

normalise_method_invalid_raises_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:normalise_method(42)
    ).

normalise_method_returns_binary_not_list_test() ->
    %% string:uppercase/1 can return chardata; ensure we always return binary
    Result = beamtalk_http:normalise_method(<<"patch">>),
    ?assert(is_binary(Result)).

%%% ============================================================================
%%% validate_request_options/3
%%% ============================================================================

validate_options_ok_test() ->
    ?assertEqual(ok, beamtalk_http:validate_request_options([], <<>>, 5000)).

validate_options_non_list_headers_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:validate_request_options(not_a_list, <<>>, 5000)
    ).

validate_options_non_binary_body_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:validate_request_options([], 42, 5000)
    ).

validate_options_negative_timeout_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:validate_request_options([], <<>>, -1)
    ).

validate_options_non_integer_timeout_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'request:url:options:'}
        },
        beamtalk_http:validate_request_options([], <<>>, fast)
    ).

validate_options_zero_timeout_ok_test() ->
    ?assertEqual(ok, beamtalk_http:validate_request_options([], <<>>, 0)).

%%% ============================================================================
%%% to_gun_headers/2
%%% ============================================================================

to_gun_headers_empty_test() ->
    ?assertEqual([], beamtalk_http:to_gun_headers([], 'get:headers:')).

to_gun_headers_list_pairs_test() ->
    Input = [[<<"content-type">>, <<"application/json">>], [<<"accept">>, <<"*/*">>]],
    Expected = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"*/*">>}],
    ?assertEqual(Expected, beamtalk_http:to_gun_headers(Input, 'get:headers:')).

to_gun_headers_tuple_pairs_test() ->
    Input = [{<<"x-key">>, <<"val">>}],
    ?assertEqual(Input, beamtalk_http:to_gun_headers(Input, 'get:headers:')).

to_gun_headers_malformed_raises_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, selector = 'get:headers:'}
        },
        beamtalk_http:to_gun_headers([bad_entry], 'get:headers:')
    ).

to_gun_headers_malformed_non_binary_name_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, selector = 'post:'}},
        beamtalk_http:to_gun_headers([[42, <<"val">>]], 'post:')
    ).

%%% ============================================================================
%%% from_gun_headers/1
%%% ============================================================================

from_gun_headers_empty_test() ->
    ?assertEqual([], beamtalk_http:from_gun_headers([])).

from_gun_headers_converts_tuples_test() ->
    Input = [{<<"content-type">>, <<"text/html">>}, {<<"x-custom">>, <<"val">>}],
    Expected = [[<<"content-type">>, <<"text/html">>], [<<"x-custom">>, <<"val">>]],
    ?assertEqual(Expected, beamtalk_http:from_gun_headers(Input)).

%%% ============================================================================
%%% parse_url/1 — valid URLs
%%% ============================================================================

parse_url_http_test() ->
    {ok, #{host := Host, port := Port, path := Path, transport := Transport}} =
        beamtalk_http:parse_url(<<"http://example.com/path">>),
    ?assertEqual("example.com", Host),
    ?assertEqual(80, Port),
    ?assertEqual(<<"/path">>, Path),
    ?assertEqual(tcp, Transport).

parse_url_https_test() ->
    {ok, #{transport := Transport, port := Port}} =
        beamtalk_http:parse_url(<<"https://example.com">>),
    ?assertEqual(tls, Transport),
    ?assertEqual(443, Port).

parse_url_custom_port_test() ->
    {ok, #{port := Port}} = beamtalk_http:parse_url(<<"http://localhost:8080/api">>),
    ?assertEqual(8080, Port).

parse_url_with_query_test() ->
    {ok, #{path := Path}} = beamtalk_http:parse_url(<<"http://example.com/search?q=foo">>),
    ?assertEqual(<<"/search?q=foo">>, Path).

parse_url_empty_path_defaults_test() ->
    {ok, #{path := Path}} = beamtalk_http:parse_url(<<"http://example.com">>),
    ?assertEqual(<<"/">>, Path).

parse_url_invalid_scheme_test() ->
    ?assertEqual({error, invalid_url}, beamtalk_http:parse_url(<<"ftp://example.com">>)).

parse_url_no_scheme_test() ->
    ?assertEqual({error, invalid_url}, beamtalk_http:parse_url(<<"example.com">>)).

parse_url_garbage_test() ->
    ?assertEqual({error, invalid_url}, beamtalk_http:parse_url(<<"not a url">>)).
