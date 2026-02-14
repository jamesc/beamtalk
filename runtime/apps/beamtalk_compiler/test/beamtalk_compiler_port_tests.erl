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

find_project_root("/") -> filename:absname("");
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
