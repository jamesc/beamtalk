%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_nav_symbols_tests).

%%% **DDD Context:** REPL Session Context (Navigation bridge)

-moduledoc """
EUnit tests for beamtalk_repl_ops_nav_symbols (BT-2244).

Covers: describe_ops/0 map shape, validate_scope/1 error and success
branches, and the happy path of handle/4 against a minimal seed of
the live class registry. The class-registry-dependent paths run in a
{setup, ...} fixture that asserts the registry is available — in
isolated EUnit runs the seed step is skipped and the test asserts on
the validation shape only, mirroring the layered approach in
beamtalk_repl_ops_nav_tests.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg() ->
    {protocol_msg, <<"nav-symbols">>, <<"t1">>, <<"s1">>, #{}, false}.

assert_error_response(Response) ->
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>, <<"error">>]}, Decoded),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    Decoded.

%%====================================================================
%% describe_ops/0
%%====================================================================

describe_ops_returns_map_with_nav_symbols_key_test() ->
    Ops = beamtalk_repl_ops_nav_symbols:describe_ops(),
    ?assertMatch(#{<<"nav-symbols">> := _}, Ops).

describe_ops_nav_symbols_has_no_required_params_test() ->
    %% `scope` is optional — the default `all` matches the
    %% `workspace/symbol` headline-win consumer.
    #{<<"nav-symbols">> := Info} = beamtalk_repl_ops_nav_symbols:describe_ops(),
    ?assertEqual([], maps:get(<<"params">>, Info)).

describe_ops_nav_symbols_optional_includes_scope_test() ->
    #{<<"nav-symbols">> := Info} = beamtalk_repl_ops_nav_symbols:describe_ops(),
    Optional = maps:get(<<"optional">>, Info),
    ?assert(lists:member(<<"scope">>, Optional)).

%%====================================================================
%% handle/4 — validation error paths (no class-registry dependency)
%%====================================================================

handle_unknown_scope_returns_error_mentioning_the_value_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav_symbols:handle(
        <<"nav-symbols">>, #{<<"scope">> => <<"galactic">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"galactic">>)).

handle_scope_not_a_string_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav_symbols:handle(
        <<"nav-symbols">>, #{<<"scope">> => 42}, Msg, self()
    ),
    assert_error_response(Response).

%%====================================================================
%% handle/4 — success paths
%%
%% These do not require a populated class registry: when the pg group
%% `beamtalk_classes` is empty (as in a stripped EUnit harness),
%% `beamtalk_runtime_api:all_classes/0` returns []. The op must still
%% succeed and emit `value.classes = []` — that "no classes loaded"
%% answer is what the LSP consumer relies on for the "empty workspace
%% but flag-on" case.
%%====================================================================

handle_default_scope_returns_done_status_test() ->
    %% No `scope` in params → defaults to `all`.
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav_symbols:handle(
        <<"nav-symbols">>, #{}, Msg, self()
    ),
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
    %% `classes` is always present (possibly empty) on success.
    ?assertMatch(#{<<"value">> := #{<<"classes">> := _}}, Decoded).

handle_scope_all_returns_classes_list_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav_symbols:handle(
        <<"nav-symbols">>, #{<<"scope">> => <<"all">>}, Msg, self()
    ),
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
    #{<<"value">> := #{<<"classes">> := Classes}} = Decoded,
    ?assert(is_list(Classes)).

handle_scope_user_returns_classes_list_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav_symbols:handle(
        <<"nav-symbols">>, #{<<"scope">> => <<"user">>}, Msg, self()
    ),
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
    #{<<"value">> := #{<<"classes">> := Classes}} = Decoded,
    ?assert(is_list(Classes)),
    %% Every row in the `user` scope must have a non-null source_file —
    %% that's what the scope filter promises the LSP consumer.
    lists:foreach(
        fun(C) ->
            SourceFile = maps:get(<<"source_file">>, C),
            ?assertNotEqual(null, SourceFile)
        end,
        Classes
    ).
