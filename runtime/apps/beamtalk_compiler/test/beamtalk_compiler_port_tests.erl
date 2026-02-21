%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_compiler_port (ADR 0022, Phase 0 wire check).

-module(beamtalk_compiler_port_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% Helper to find the compiler binary
compiler_binary() ->
    %% Look in target/debug from project root
    ProjectRoot = find_project_root(filename:absname("")),
    Path = filename:join([ProjectRoot, "target", "debug", "beamtalk-compiler-port"]),
    case filelib:is_regular(Path) of
        true -> Path;
        false -> error({compiler_not_found, Path})
    end.

find_project_root("/") ->
    filename:absname("");
find_project_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "Cargo.toml")) of
        true -> Dir;
        false -> find_project_root(filename:dirname(Dir))
    end.

%% Helper to open and close a port around a test
with_port(Fun) ->
    Port = beamtalk_compiler_port:open(compiler_binary()),
    try
        Fun(Port)
    after
        catch beamtalk_compiler_port:close(Port)
    end.

%%% Tests

simple_expression_test() ->
    with_port(fun(Port) ->
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression(Port, <<"1 + 2">>, <<"test">>, []),
        %% Verify the Core Erlang contains the expected addition
        ?assert(binary:match(CoreErlang, <<"'+'(1, 2)">>) =/= nomatch)
    end).

known_vars_test() ->
    with_port(fun(Port) ->
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression(Port, <<"x + 1">>, <<"test">>, [<<"x">>]),
        %% Verify it references the variable x via maps:get
        ?assert(binary:match(CoreErlang, <<"'get'('x'">>) =/= nomatch)
    end).

invalid_expression_returns_error_test() ->
    with_port(fun(Port) ->
        {error, Diagnostics} =
            beamtalk_compiler_port:compile_expression(Port, <<"+++">>, <<"test">>, []),
        ?assert(length(Diagnostics) > 0)
    end).

multiple_compiles_on_same_port_test() ->
    with_port(fun(Port) ->
        {ok, _, []} = beamtalk_compiler_port:compile_expression(Port, <<"1 + 2">>, <<"m1">>, []),
        {ok, _, []} = beamtalk_compiler_port:compile_expression(Port, <<"3 * 4">>, <<"m2">>, []),
        {ok, _, []} = beamtalk_compiler_port:compile_expression(Port, <<"5 - 1">>, <<"m3">>, [])
    end).

port_reopens_after_close_test() ->
    Port1 = beamtalk_compiler_port:open(compiler_binary()),
    {ok, _, []} = beamtalk_compiler_port:compile_expression(Port1, <<"1">>, <<"t">>, []),
    beamtalk_compiler_port:close(Port1),
    %% Reopen — BEAM is fine
    Port2 = beamtalk_compiler_port:open(compiler_binary()),
    {ok, _, []} = beamtalk_compiler_port:compile_expression(Port2, <<"2">>, <<"t">>, []),
    beamtalk_compiler_port:close(Port2).

empty_expression_returns_error_test() ->
    with_port(fun(Port) ->
        {error, _} =
            beamtalk_compiler_port:compile_expression(Port, <<"">>, <<"test">>, [])
    end).

garbage_etf_handled_gracefully_test() ->
    %% Send raw garbage — port should return an error, not crash
    Port = beamtalk_compiler_port:open(compiler_binary()),
    port_command(Port, <<131, 0, 0, 0>>),
    receive
        {Port, {data, RespBin}} ->
            Resp = binary_to_term(RespBin),
            ?assertMatch(#{status := error}, Resp);
        {Port, {exit_status, _}} ->
            %% Port crashed but BEAM is alive — acceptable for Phase 0
            ok
    after 5000 ->
        ?assert(false)
    end,
    catch beamtalk_compiler_port:close(Port).

compile_on_closed_port_returns_error_test() ->
    Port = beamtalk_compiler_port:open(compiler_binary()),
    beamtalk_compiler_port:close(Port),
    %% Calling compile on a closed port should return error, not crash BEAM
    Result = beamtalk_compiler_port:compile_expression(Port, <<"1">>, <<"t">>, []),
    ?assertMatch({error, _}, Result).

%%% ---------------------------------------------------------------
%%% handle_response/1 — ETF response parsing
%%% ---------------------------------------------------------------

handle_response_expression_ok_test() ->
    Response = #{status => ok, core_erlang => <<"core">>, warnings => []},
    ?assertEqual(
        {ok, <<"core">>, []},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_expression_with_warnings_test() ->
    Response = #{
        status => ok,
        core_erlang => <<"core">>,
        warnings => [<<"unused var">>]
    },
    ?assertEqual(
        {ok, <<"core">>, [<<"unused var">>]},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_class_definition_test() ->
    Response = #{
        status => ok,
        kind => class_definition,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        classes => [<<"Foo">>],
        warnings => []
    },
    {ok, class_definition, Info} =
        beamtalk_compiler_port:handle_response(Response),
    ?assertEqual(<<"core">>, maps:get(core_erlang, Info)),
    ?assertEqual(<<"mod">>, maps:get(module_name, Info)),
    ?assertEqual([<<"Foo">>], maps:get(classes, Info)).

handle_response_method_definition_test() ->
    Response = #{
        status => ok,
        kind => method_definition,
        class_name => <<"Counter">>,
        selector => <<"increment">>,
        is_class_method => false,
        method_source => <<"src">>,
        warnings => []
    },
    {ok, method_definition, Info} =
        beamtalk_compiler_port:handle_response(Response),
    ?assertEqual(<<"Counter">>, maps:get(class_name, Info)),
    ?assertEqual(<<"increment">>, maps:get(selector, Info)),
    ?assertEqual(false, maps:get(is_class_method, Info)).

handle_response_error_test() ->
    Response = #{status => error, diagnostics => [<<"err">>]},
    ?assertEqual(
        {error, [<<"err">>]},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_unexpected_test() ->
    Response = #{bogus => true},
    ?assertMatch(
        {error, [<<"Unexpected compiler response">>]},
        beamtalk_compiler_port:handle_response(Response)
    ).

%%% ---------------------------------------------------------------
%%% find_compiler_binary/0 — binary discovery paths
%%% ---------------------------------------------------------------

find_compiler_binary_returns_path_test() ->
    Path = beamtalk_compiler_port:find_compiler_binary(),
    ?assert(is_list(Path) orelse is_binary(Path)),
    ?assert(filelib:is_regular(Path)).

find_compiler_binary_env_override_test() ->
    Original = os:getenv("BEAMTALK_COMPILER_PORT_BIN"),
    try
        %% Point to the real binary via env var
        RealPath = beamtalk_compiler_port:find_compiler_binary(),
        os:putenv("BEAMTALK_COMPILER_PORT_BIN", RealPath),
        Found = beamtalk_compiler_port:find_compiler_binary(),
        ?assertEqual(RealPath, Found)
    after
        case Original of
            false -> os:unsetenv("BEAMTALK_COMPILER_PORT_BIN");
            Value -> os:putenv("BEAMTALK_COMPILER_PORT_BIN", Value)
        end
    end.

find_compiler_binary_invalid_env_fallback_test() ->
    Original = os:getenv("BEAMTALK_COMPILER_PORT_BIN"),
    try
        os:putenv("BEAMTALK_COMPILER_PORT_BIN", "/nonexistent/binary"),
        %% Should fall back to dev path / PATH search
        Path = beamtalk_compiler_port:find_compiler_binary(),
        ?assert(filelib:is_regular(Path))
    after
        case Original of
            false -> os:unsetenv("BEAMTALK_COMPILER_PORT_BIN");
            Value -> os:putenv("BEAMTALK_COMPILER_PORT_BIN", Value)
        end
    end.

find_compiler_binary_empty_env_fallback_test() ->
    Original = os:getenv("BEAMTALK_COMPILER_PORT_BIN"),
    try
        os:putenv("BEAMTALK_COMPILER_PORT_BIN", ""),
        Path = beamtalk_compiler_port:find_compiler_binary(),
        ?assert(filelib:is_regular(Path))
    after
        case Original of
            false -> os:unsetenv("BEAMTALK_COMPILER_PORT_BIN");
            Value -> os:putenv("BEAMTALK_COMPILER_PORT_BIN", Value)
        end
    end.

%%% ---------------------------------------------------------------
%%% find_project_root/0 — project root discovery
%%% ---------------------------------------------------------------

find_project_root_finds_cargo_toml_test() ->
    Root = beamtalk_compiler_port:find_project_root(),
    ?assert(filelib:is_regular(filename:join(Root, "Cargo.toml"))).
