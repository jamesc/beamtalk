%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_eval module
%%%
%%% Tests expression evaluation, file loading, and daemon interaction.

-module(beamtalk_repl_eval_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Module name derivation tests

derive_module_name_basic_test() ->
    ?assertEqual(counter, beamtalk_repl_eval:derive_module_name("counter.bt")).

derive_module_name_with_path_test() ->
    ?assertEqual(counter, beamtalk_repl_eval:derive_module_name("examples/counter.bt")),
    ?assertEqual(point, beamtalk_repl_eval:derive_module_name("lib/core/point.bt")).

derive_module_name_complex_path_test() ->
    ?assertEqual(my_class, beamtalk_repl_eval:derive_module_name("/absolute/path/to/my_class.bt")).

derive_module_name_no_extension_test() ->
    %% basename/2 with ".bt" works on files without extension too
    ?assertEqual(filename, beamtalk_repl_eval:derive_module_name("filename")).

%%% Assignment extraction tests

extract_assignment_valid_test() ->
    ?assertEqual({ok, count}, beamtalk_repl_eval:extract_assignment("count := 0")),
    ?assertEqual({ok, myVar}, beamtalk_repl_eval:extract_assignment("myVar := 123")),
    ?assertEqual({ok, '_privateVar'}, beamtalk_repl_eval:extract_assignment("_privateVar := nil")).

extract_assignment_with_whitespace_test() ->
    ?assertEqual({ok, x}, beamtalk_repl_eval:extract_assignment("x:=1")),
    ?assertEqual({ok, y}, beamtalk_repl_eval:extract_assignment("y := 2")),
    ?assertEqual({ok, z}, beamtalk_repl_eval:extract_assignment("z  :=  3")).

extract_assignment_not_assignment_test() ->
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("1 + 2")),
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("array at: 1")),
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("getValue")).

extract_assignment_invalid_variable_name_test() ->
    %% Variables must start with letter or underscore
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("123 := 456")),
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("$var := 1")).

%%% Daemon diagnostics formatting tests

format_daemon_diagnostics_empty_test() ->
    ?assertEqual(<<"Compilation failed">>, beamtalk_repl_eval:format_daemon_diagnostics([])).

format_daemon_diagnostics_single_test() ->
    Diagnostics = [<<"Unexpected token">>],
    ?assertEqual(<<"Unexpected token">>, beamtalk_repl_eval:format_daemon_diagnostics(Diagnostics)).

format_daemon_diagnostics_multiple_test() ->
    Diagnostics = [<<"Error 1">>, <<"Error 2">>, <<"Error 3">>],
    Result = beamtalk_repl_eval:format_daemon_diagnostics(Diagnostics),
    ?assert(binary:match(Result, <<"Error 1">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Error 2">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Error 3">>) =/= nomatch).

%%% Core Erlang compilation tests
%%%
%%% Note: Valid Core Erlang compilation requires proper erlc environment setup
%%% and module name matching. These tests focus on error handling and are
%%% suitable for unit testing. Valid compilation paths are better tested in
%%% integration tests with a running compiler daemon.

compile_core_erlang_invalid_test() ->
    %% Invalid Core Erlang
    CoreErlang = <<"not valid core erlang">>,
    ModuleName = bad_module,
    Result = beamtalk_repl_eval:compile_core_erlang(CoreErlang, ModuleName),
    ?assertMatch({error, {core_compile_error, _}}, Result).

%%% State-based do_eval tests (require mock daemon)

do_eval_increments_counter_test() ->
    %% Test that do_eval increments the eval counter
    State = beamtalk_repl_state:new(undefined, 0),
    InitialCounter = beamtalk_repl_state:get_eval_counter(State),
    
    %% Since we don't have a daemon running, this will fail compilation
    %% But it should still increment the counter
    {error, _, NewState} = beamtalk_repl_eval:do_eval("1 + 1", State),
    NewCounter = beamtalk_repl_state:get_eval_counter(NewState),
    
    ?assertEqual(InitialCounter + 1, NewCounter).

do_eval_no_daemon_error_test() ->
    %% Without a running daemon, should get daemon_unavailable error
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_eval("1 + 1", State),
    ?assertMatch({error, {compile_error, _}, _}, Result),
    
    %% Error message should mention daemon (case-insensitive)
    {error, {compile_error, Msg}, _} = Result,
    LowerMsg = string:lowercase(Msg),
    ?assert(binary:match(LowerMsg, <<"daemon">>) =/= nomatch).

%%% File loading tests

handle_load_file_not_found_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load("/nonexistent/file.bt", State),
    ?assertMatch({error, {file_not_found, _}, _}, Result).

handle_load_directory_test() ->
    %% Loading a directory should fail
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load("/tmp", State),
    %% Should get read_error since it's a directory
    ?assertMatch({error, {read_error, _}, _}, Result).

%%% Class name extraction tests

extract_class_names_no_classes_test() ->
    Result = #{<<"success">> => true, <<"core_erlang">> => <<"...">>},
    ?assertEqual([], beamtalk_repl_eval:extract_class_names(Result)).

extract_class_names_with_classes_test() ->
    Result = #{
        <<"success">> => true,
        <<"classes">> => [<<"Counter">>, <<"Point">>, <<"Actor">>]
    },
    ?assertEqual(["Counter", "Point", "Actor"], beamtalk_repl_eval:extract_class_names(Result)).

extract_class_names_empty_list_test() ->
    Result = #{<<"success">> => true, <<"classes">> => []},
    ?assertEqual([], beamtalk_repl_eval:extract_class_names(Result)).
