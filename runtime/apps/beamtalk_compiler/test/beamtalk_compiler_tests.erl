%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_compiler (ADR 0022, Phase 1).
%%
%% Tests the public API, backend dispatch, in-memory Core Erlang compilation,
%% and the compile (file) command.

-module(beamtalk_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

%%% Setup / Teardown

%% Start the compiler application for integration tests.
setup() ->
    %% Ensure compiler app and dependencies are started
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

teardown(_) ->
    ok.

compiler_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"compile_expression succeeds", fun compile_expression_succeeds/0},
        {"compile_expression with known vars", fun compile_expression_known_vars/0},
        {"compile_expression with invalid source returns error", fun compile_expression_error/0},
        {"compile_expression with empty source returns error", fun compile_expression_empty/0},
        {"compile file succeeds", fun compile_file_succeeds/0},
        {"compile file with stdlib mode", fun compile_file_stdlib_mode/0},
        {"compile file with invalid source returns error", fun compile_file_error/0},
        {"compile file with no class returns error", fun compile_file_no_class/0},
        {"diagnostics returns structured diagnostics", fun diagnostics_succeeds/0},
        {"diagnostics for invalid source includes errors", fun diagnostics_errors/0},
        {"version returns binary", fun version_succeeds/0},
        {"compile_core_erlang in memory", fun compile_core_erlang_in_memory/0},
        {"compile_core_erlang with invalid source", fun compile_core_erlang_invalid/0},
        {"compile_core_erlang scan error", fun compile_core_erlang_scan_error/0},
        {"compile_core_erlang parse error", fun compile_core_erlang_parse_error/0},
        {"backend defaults to port", fun backend_default_port/0},
        {"multiple compiles on same server", fun multiple_compiles/0},
        {"compile file with workspace_mode=false", fun compile_file_workspace_mode/0},
        {"compile_expression with class definition", fun compile_expression_class_def/0},
        {"compiler app module callbacks", fun compiler_app_callbacks/0}
    ]}.

%%% Tests

compile_expression_succeeds() ->
    {ok, CoreErlang, []} =
        beamtalk_compiler:compile_expression(<<"1 + 2">>, <<"test_mod">>, []),
    ?assert(is_binary(CoreErlang)),
    ?assert(byte_size(CoreErlang) > 0),
    %% Verify Core Erlang contains the addition
    ?assert(binary:match(CoreErlang, <<"'+'(1, 2)">>) =/= nomatch).

compile_expression_known_vars() ->
    {ok, CoreErlang, []} =
        beamtalk_compiler:compile_expression(<<"x + 1">>, <<"test_mod">>, [<<"x">>]),
    ?assert(is_binary(CoreErlang)),
    %% Variable x referenced via maps:get
    ?assert(binary:match(CoreErlang, <<"'get'('x'">>) =/= nomatch).

compile_expression_error() ->
    {error, Diagnostics} =
        beamtalk_compiler:compile_expression(<<"+++">>, <<"test_mod">>, []),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0).

compile_expression_empty() ->
    {error, _} =
        beamtalk_compiler:compile_expression(<<"">>, <<"test_mod">>, []).

compile_file_succeeds() ->
    Source =
        <<"Actor subclass: TestCounter\n  count => self.count\n  increment => self.count := self.count + 1">>,
    {ok, Result} = beamtalk_compiler:compile(Source, #{}),
    ?assert(is_map(Result)),
    ?assert(is_binary(maps:get(core_erlang, Result))),
    ?assert(is_binary(maps:get(module_name, Result))),
    ?assert(is_list(maps:get(classes, Result))),
    %% Module name should follow ADR 0016 naming
    ModuleName = maps:get(module_name, Result),
    ?assert(binary:match(ModuleName, <<"bt@">>) =/= nomatch),
    %% Classes list should contain TestCounter
    Classes = maps:get(classes, Result),
    ?assert(length(Classes) > 0).

compile_file_stdlib_mode() ->
    Source = <<"Object subclass: StdlibTest\n  value => 42">>,
    {ok, Result} = beamtalk_compiler:compile(Source, #{stdlib_mode => true}),
    ModuleName = maps:get(module_name, Result),
    %% Stdlib mode should use bt@stdlib@ prefix
    ?assert(binary:match(ModuleName, <<"bt@stdlib@">>) =/= nomatch).

compile_file_error() ->
    {error, Diagnostics} =
        beamtalk_compiler:compile(<<"+++">>, #{}),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0).

compile_file_no_class() ->
    %% Source with no class definition
    {error, Diagnostics} =
        beamtalk_compiler:compile(<<"1 + 2">>, #{}),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0).

diagnostics_succeeds() ->
    {ok, Diagnostics} = beamtalk_compiler:diagnostics(<<"1 + 2">>),
    ?assert(is_list(Diagnostics)).

diagnostics_errors() ->
    {ok, Diagnostics} = beamtalk_compiler:diagnostics(<<"+++">>),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0),
    %% Check structured format
    [First | _] = Diagnostics,
    ?assert(is_map(First)),
    ?assert(is_binary(maps:get(message, First))),
    ?assert(is_binary(maps:get(severity, First))).

version_succeeds() ->
    {ok, Version} = beamtalk_compiler:version(),
    ?assert(is_binary(Version)),
    ?assert(byte_size(Version) > 0).

compile_core_erlang_in_memory() ->
    %% First, get some Core Erlang from the compiler
    {ok, CoreErlang, []} =
        beamtalk_compiler:compile_expression(<<"42">>, <<"test_in_memory">>, []),
    %% Compile it in memory
    {ok, ModuleName, Binary} = beamtalk_compiler:compile_core_erlang(CoreErlang),
    ?assert(is_atom(ModuleName)),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0),
    %% Load and execute the module
    {module, ModuleName} = code:load_binary(ModuleName, "", Binary),
    Result = ModuleName:eval(#{}),
    %% Clean up
    code:purge(ModuleName),
    code:delete(ModuleName),
    ?assertMatch({42, _}, Result).

compile_core_erlang_invalid() ->
    Result = beamtalk_compiler:compile_core_erlang(<<"this is not core erlang">>),
    ?assertMatch({error, _}, Result).

compile_core_erlang_scan_error() ->
    %% Binary that produces core_scan error
    Result = beamtalk_compiler:compile_core_erlang(<<"\x00\x01">>),
    ?assertMatch({error, _}, Result).

compile_core_erlang_parse_error() ->
    %% Scannable but invalid Core Erlang structure
    Result = beamtalk_compiler:compile_core_erlang(<<"module 'x' []\n">>),
    ?assertMatch({error, {core_parse_error, _}}, Result).

backend_default_port() ->
    Original = os:getenv("BEAMTALK_COMPILER"),
    try
        os:unsetenv("BEAMTALK_COMPILER"),
        ?assertEqual(port, beamtalk_compiler_backend:backend())
    after
        case Original of
            false -> ok;
            Value -> os:putenv("BEAMTALK_COMPILER", Value)
        end
    end.

multiple_compiles() ->
    {ok, _, []} = beamtalk_compiler:compile_expression(<<"1 + 2">>, <<"m1">>, []),
    {ok, _, []} = beamtalk_compiler:compile_expression(<<"3 * 4">>, <<"m2">>, []),
    {ok, _, []} = beamtalk_compiler:compile_expression(<<"5 - 1">>, <<"m3">>, []).

compile_file_workspace_mode() ->
    Source = <<"Actor subclass: WsModeTest\n  value => 42">>,
    {ok, Result} = beamtalk_compiler:compile(Source, #{workspace_mode => false}),
    ?assert(is_map(Result)),
    ?assert(is_binary(maps:get(core_erlang, Result))).

compile_expression_class_def() ->
    %% Inline class definition should return class_definition tuple
    Source = <<"Actor subclass: InlineClassTest\n  value => 42">>,
    Result = beamtalk_compiler:compile_expression(Source, <<"inline_test">>, []),
    ?assertMatch({ok, class_definition, _}, Result).

compiler_app_callbacks() ->
    %% stop/1 returns ok
    ?assertEqual(ok, beamtalk_compiler_app:stop(undefined)).
