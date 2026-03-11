%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_http_router (BT-1344).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Verifies route compilation and matching: exact paths, parameterized
%%% routes, wildcards, method dispatch, 404/405 responses.

-module(beamtalk_http_router_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Compilation tests
%%% ============================================================================

compile_empty_routes_test() ->
    ?assertEqual([], beamtalk_http_router:compile([])).

compile_single_route_test() ->
    Handler = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/hello">>, Handler}]),
    ?assertEqual(1, length(Routes)).

compile_multiple_routes_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([
        {<<"GET">>, <<"/">>, H},
        {<<"POST">>, <<"/users">>, H},
        {<<"GET">>, <<"/users/:id">>, H}
    ]),
    ?assertEqual(3, length(Routes)).

%%% ============================================================================
%%% Exact match tests
%%% ============================================================================

match_root_path_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/">>, H}]),
    {ok, Handler, Params} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/">>),
    ?assertEqual(H, Handler),
    ?assertEqual(#{}, Params).

match_simple_path_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/hello">>, H}]),
    {ok, Handler, Params} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/hello">>),
    ?assertEqual(H, Handler),
    ?assertEqual(#{}, Params).

match_nested_path_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/api/v1/users">>, H}]),
    {ok, _, #{}} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/api/v1/users">>).

%%% ============================================================================
%%% Parameterized route tests
%%% ============================================================================

match_single_param_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/users/:id">>, H}]),
    {ok, _, Params} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/users/42">>),
    ?assertEqual(#{<<"id">> => <<"42">>}, Params).

match_multiple_params_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([
        {<<"GET">>, <<"/users/:userId/posts/:postId">>, H}
    ]),
    {ok, _, Params} = beamtalk_http_router:match(
        Routes, <<"GET">>, <<"/users/7/posts/99">>
    ),
    ?assertEqual(#{<<"userId">> => <<"7">>, <<"postId">> => <<"99">>}, Params).

param_captures_any_segment_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/files/:name">>, H}]),
    {ok, _, Params} = beamtalk_http_router:match(
        Routes, <<"GET">>, <<"/files/my-document.txt">>
    ),
    ?assertEqual(#{<<"name">> => <<"my-document.txt">>}, Params).

%%% ============================================================================
%%% Wildcard tests
%%% ============================================================================

match_wildcard_single_segment_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/static/*path">>, H}]),
    {ok, _, Params} = beamtalk_http_router:match(
        Routes, <<"GET">>, <<"/static/style.css">>
    ),
    ?assertEqual(#{<<"path">> => <<"style.css">>}, Params).

match_wildcard_multiple_segments_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/static/*path">>, H}]),
    {ok, _, Params} = beamtalk_http_router:match(
        Routes, <<"GET">>, <<"/static/css/main.css">>
    ),
    ?assertEqual(#{<<"path">> => <<"css/main.css">>}, Params).

%%% ============================================================================
%%% Method dispatch tests
%%% ============================================================================

match_correct_method_test() ->
    HGet = fun(_) -> get end,
    HPost = fun(_) -> post end,
    Routes = beamtalk_http_router:compile([
        {<<"GET">>, <<"/items">>, HGet},
        {<<"POST">>, <<"/items">>, HPost}
    ]),
    {ok, GetHandler, _} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/items">>),
    ?assertEqual(HGet, GetHandler),
    {ok, PostHandler, _} = beamtalk_http_router:match(Routes, <<"POST">>, <<"/items">>),
    ?assertEqual(HPost, PostHandler).

%%% ============================================================================
%%% Not found / method not allowed tests
%%% ============================================================================

not_found_when_no_match_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/hello">>, H}]),
    ?assertEqual(not_found, beamtalk_http_router:match(Routes, <<"GET">>, <<"/world">>)).

not_found_on_empty_routes_test() ->
    ?assertEqual(not_found, beamtalk_http_router:match([], <<"GET">>, <<"/">>)).

method_not_allowed_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/items">>, H}]),
    ?assertEqual(
        method_not_allowed,
        beamtalk_http_router:match(Routes, <<"POST">>, <<"/items">>)
    ).

method_not_allowed_with_params_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/users/:id">>, H}]),
    ?assertEqual(
        method_not_allowed,
        beamtalk_http_router:match(Routes, <<"DELETE">>, <<"/users/42">>)
    ).

%%% ============================================================================
%%% Route priority tests
%%% ============================================================================

first_matching_route_wins_test() ->
    H1 = fun(_) -> first end,
    H2 = fun(_) -> second end,
    Routes = beamtalk_http_router:compile([
        {<<"GET">>, <<"/users/:id">>, H1},
        {<<"GET">>, <<"/users/:name">>, H2}
    ]),
    {ok, Handler, _} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/users/42">>),
    ?assertEqual(H1, Handler).

exact_before_param_when_ordered_test() ->
    HExact = fun(_) -> exact end,
    HParam = fun(_) -> param end,
    Routes = beamtalk_http_router:compile([
        {<<"GET">>, <<"/users/me">>, HExact},
        {<<"GET">>, <<"/users/:id">>, HParam}
    ]),
    {ok, Handler, _} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/users/me">>),
    ?assertEqual(HExact, Handler).

%%% ============================================================================
%%% Edge case tests
%%% ============================================================================

trailing_slash_is_equivalent_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/hello">>, H}]),
    %% Trailing slash is stripped — /hello/ matches /hello
    {ok, _, #{}} = beamtalk_http_router:match(Routes, <<"GET">>, <<"/hello/">>).

wildcard_requires_at_least_one_segment_test() ->
    H = fun(_) -> ok end,
    Routes = beamtalk_http_router:compile([{<<"GET">>, <<"/static/*path">>, H}]),
    %% /static/ has no segments after "static", so wildcard does not match
    ?assertEqual(not_found, beamtalk_http_router:match(Routes, <<"GET">>, <<"/static/">>)).

%%% ============================================================================
%%% Compile-time validation tests
%%% ============================================================================

wildcard_must_be_last_segment_test() ->
    H = fun(_) -> ok end,
    ?assertError(
        #{'$beamtalk_class' := _, error := _},
        beamtalk_http_router:compile([{<<"GET">>, <<"/static/*path/subdir">>, H}])
    ).

duplicate_param_names_rejected_test() ->
    H = fun(_) -> ok end,
    ?assertError(
        #{'$beamtalk_class' := _, error := _},
        beamtalk_http_router:compile([{<<"GET">>, <<"/users/:id/posts/:id">>, H}])
    ).
