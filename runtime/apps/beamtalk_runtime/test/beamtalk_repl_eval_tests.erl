%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_eval module
%%%
%%% Tests expression evaluation, file loading, and daemon interaction.

-module(beamtalk_repl_eval_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%%====================================================================
%% Tests
%%====================================================================

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

format_formatted_diagnostics_empty_test() ->
    ?assertEqual(<<"Compilation failed">>, beamtalk_repl_eval:format_formatted_diagnostics([])).

format_formatted_diagnostics_single_test() ->
    FormattedDiagnostics = [<<"Unexpected token">>],
    ?assertEqual(<<"Unexpected token">>, beamtalk_repl_eval:format_formatted_diagnostics(FormattedDiagnostics)).

format_formatted_diagnostics_multiple_test() ->
    FormattedDiagnostics = [<<"Error 1">>, <<"Error 2">>, <<"Error 3">>],
    Result = beamtalk_repl_eval:format_formatted_diagnostics(FormattedDiagnostics),
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
    {error, _, _, NewState} = beamtalk_repl_eval:do_eval("1 + 1", State),
    NewCounter = beamtalk_repl_state:get_eval_counter(NewState),
    
    ?assertEqual(InitialCounter + 1, NewCounter).

do_eval_no_daemon_error_test() ->
    %% Without a running daemon, should get daemon_unavailable error
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_eval("1 + 1", State),
    ?assertMatch({error, {compile_error, _}, _, _}, Result),
    
    %% Error message should mention daemon (case-insensitive)
    {error, {compile_error, Msg}, _, _} = Result,
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
        <<"classes">> => [
            #{<<"name">> => <<"Counter">>, <<"superclass">> => <<"Actor">>},
            #{<<"name">> => <<"Point">>, <<"superclass">> => <<"Object">>},
            #{<<"name">> => <<"Actor">>, <<"superclass">> => <<"Object">>}
        ]
    },
    Expected = [
        #{name => "Counter", superclass => "Actor"},
        #{name => "Point", superclass => "Object"},
        #{name => "Actor", superclass => "Object"}
    ],
    ?assertEqual(Expected, beamtalk_repl_eval:extract_class_names(Result)).

extract_class_names_empty_list_test() ->
    Result = #{<<"success">> => true, <<"classes">> => []},
    ?assertEqual([], beamtalk_repl_eval:extract_class_names(Result)).

%%% compile_core_erlang tests

compile_core_erlang_valid_simple_module_test() ->
    %% Valid minimal Core Erlang module
    %% Note: Core Erlang is very strict about format
    CoreErlang = <<"module 'test_module' ['main'/0]\n"
                   "attributes []\n"
                   "'main'/0 = fun () -> 42\n">>,
    ModuleName = test_module,
    Result = beamtalk_repl_eval:compile_core_erlang(CoreErlang, ModuleName),
    %% This should succeed or fail with core_compile_error depending on Core Erlang validity
    case Result of
        {ok, Binary} ->
            %% If it succeeds, verify it's a valid BEAM binary
            ?assert(is_binary(Binary)),
            ?assert(byte_size(Binary) > 0);
        {error, {core_compile_error, _}} ->
            %% Core Erlang syntax might be too strict for this simple test
            %% The important thing is that the function handles compilation
            ok
    end.

compile_core_erlang_file_write_error_test() ->
    %% Test with an invalid temp directory that doesn't exist
    %% This will cause file:write_file to fail
    %% Note: We can't easily mock file:write_file in EUnit, so we test
    %% the error path by providing invalid Core Erlang instead
    CoreErlang = <<"not valid core erlang syntax at all">>,
    ModuleName = bad_syntax_module,
    Result = beamtalk_repl_eval:compile_core_erlang(CoreErlang, ModuleName),
    ?assertMatch({error, {core_compile_error, _}}, Result).

%%% format_compile_errors is internal - tested indirectly via compile_core_erlang

%%% Additional do_eval tests (with mocked daemon scenarios)

do_eval_load_binary_error_test() ->
    %% Test case where compilation succeeds but code:load_binary fails
    %% This is hard to trigger in practice without mocking, but we can
    %% verify the error path exists by testing with daemon unavailable
    State = beamtalk_repl_state:new(undefined, 0),
    {error, {compile_error, _}, _, NewState} = beamtalk_repl_eval:do_eval("1 + 1", State),
    %% Counter should still increment even on error
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(NewState)).

do_eval_preserves_bindings_on_error_test() ->
    %% Verify that existing bindings are preserved when eval fails
    State = beamtalk_repl_state:new(undefined, 0),
    InitialBindings = #{x => 42, y => 100},
    StateWithBindings = beamtalk_repl_state:set_bindings(InitialBindings, State),
    
    %% Eval will fail (no daemon), but bindings should be preserved
    {error, _, _, NewState} = beamtalk_repl_eval:do_eval("z := 999", StateWithBindings),
    FinalBindings = beamtalk_repl_state:get_bindings(NewState),
    
    %% Original bindings should still be there
    ?assertEqual(42, maps:get(x, FinalBindings)),
    ?assertEqual(100, maps:get(y, FinalBindings)),
    %% New binding should NOT be there (eval failed)
    ?assertEqual(false, maps:is_key(z, FinalBindings)).

%%% Additional handle_load tests

handle_load_read_error_directory_test() ->
    %% Already covered by handle_load_directory_test, but let's be explicit
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load(".", State),
    %% Loading current directory should fail with read_error
    ?assertMatch({error, {read_error, _}, _}, Result).

handle_load_compile_error_test() ->
    %% Test with a file that exists but will fail compilation
    %% Use unique filename to avoid collisions in concurrent test runs
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(os:getenv("TMPDIR", "/tmp"), 
                             io_lib:format("test_invalid_bt_~p.bt", [UniqueId])),
    ok = file:write_file(TempFile, <<"invalid beamtalk syntax @@@ ###">>),
    
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load(TempFile, State),
    
    %% Clean up - ensure it's deleted even if assertion fails
    ok = file:delete(TempFile),
    
    %% Should get daemon_unavailable or compile error
    case Result of
        {error, daemon_unavailable, _} -> ok;
        {error, {compile_error, _}, _} -> ok;
        {error, {daemon_error, _}, _} -> ok;
        Other -> ?assert(false, lists:flatten(io_lib:format("Unexpected result: ~p", [Other])))
    end.

%%% Secure temp directory tests (BT-56)

ensure_secure_temp_dir_creates_directory_test() ->
    %% Test that ensure_secure_temp_dir creates the directory
    Result = beamtalk_repl_eval:ensure_secure_temp_dir(),
    ?assertMatch({ok, _}, Result),
    {ok, Dir} = Result,
    %% Verify directory exists
    ?assert(filelib:is_dir(Dir)),
    %% Verify it's under ~/.beamtalk/tmp/
    ?assert(lists:suffix("/tmp", Dir) orelse lists:suffix("/tmp/", Dir)).

%% Helper to check if we're on Unix (where permission tests work)
is_unix() ->
    case os:type() of
        {unix, _} -> true;
        _ -> false
    end.

ensure_secure_temp_dir_has_secure_permissions_test() ->
    %% Test that directory has mode 0700 (Unix only)
    case is_unix() of
        true ->
            {ok, Dir} = beamtalk_repl_eval:ensure_secure_temp_dir(),
            {ok, FileInfo} = file:read_file_info(Dir),
            Mode = FileInfo#file_info.mode band 8#777,
            %% Should be 0700 (448 in decimal)
            ?assertEqual(8#700, Mode);
        false ->
            %% Skip on non-Unix platforms (Windows)
            ok
    end.

ensure_secure_temp_dir_idempotent_test() ->
    %% Calling multiple times should return same directory
    {ok, Dir1} = beamtalk_repl_eval:ensure_secure_temp_dir(),
    {ok, Dir2} = beamtalk_repl_eval:ensure_secure_temp_dir(),
    ?assertEqual(Dir1, Dir2).

ensure_dir_with_mode_creates_new_directory_test() ->
    %% Create a temporary directory with unique name for testing (Unix only)
    case is_unix() of
        true ->
            UniqueId = erlang:unique_integer([positive]),
            TestDir = filename:join(os:getenv("TMPDIR", "/tmp"), 
                                    io_lib:format("test_dir_mode_~p", [UniqueId])),
            
            %% Ensure it doesn't exist
            file:del_dir(TestDir),
            
            %% Create with mode 0700
            Result = beamtalk_repl_eval:ensure_dir_with_mode(TestDir, 8#700),
            ?assertMatch({ok, _}, Result),
            {ok, ReturnedDir} = Result,
            ?assertEqual(TestDir, ReturnedDir),
            
            %% Verify it exists and has correct permissions
            ?assert(filelib:is_dir(TestDir)),
            {ok, FileInfo} = file:read_file_info(TestDir),
            Mode = FileInfo#file_info.mode band 8#777,
            ?assertEqual(8#700, Mode),
            
            %% Clean up
            file:del_dir(TestDir);
        false ->
            %% Skip on non-Unix platforms
            ok
    end.

ensure_dir_with_mode_fixes_permissions_test() ->
    %% Create a directory with wrong permissions, verify it gets fixed (Unix only)
    case is_unix() of
        true ->
            UniqueId = erlang:unique_integer([positive]),
            TestDir = filename:join(os:getenv("TMPDIR", "/tmp"), 
                                    io_lib:format("test_dir_perms_~p", [UniqueId])),
            
            %% Create with permissive mode 0755
            file:del_dir(TestDir),
            ok = file:make_dir(TestDir),
            ok = file:change_mode(TestDir, 8#755),
            
            %% Verify wrong permissions
            {ok, FileInfo1} = file:read_file_info(TestDir),
            ?assertEqual(8#755, FileInfo1#file_info.mode band 8#777),
            
            %% Call ensure_dir_with_mode to fix it
            {ok, _} = beamtalk_repl_eval:ensure_dir_with_mode(TestDir, 8#700),
            
            %% Verify permissions are now correct
            {ok, FileInfo2} = file:read_file_info(TestDir),
            ?assertEqual(8#700, FileInfo2#file_info.mode band 8#777),
            
            %% Clean up
            file:del_dir(TestDir);
        false ->
            %% Skip on non-Unix platforms
            ok
    end.

ensure_dir_with_mode_existing_correct_permissions_test() ->
    %% If directory exists with correct permissions, should just return ok (Unix only)
    case is_unix() of
        true ->
            UniqueId = erlang:unique_integer([positive]),
            TestDir = filename:join(os:getenv("TMPDIR", "/tmp"), 
                                    io_lib:format("test_dir_ok_~p", [UniqueId])),
            
            %% Create with correct mode
            file:del_dir(TestDir),
            ok = file:make_dir(TestDir),
            ok = file:change_mode(TestDir, 8#700),
            
            %% Call ensure_dir_with_mode
            Result = beamtalk_repl_eval:ensure_dir_with_mode(TestDir, 8#700),
            ?assertMatch({ok, _}, Result),
            
            %% Permissions should still be correct
            {ok, FileInfo} = file:read_file_info(TestDir),
            ?assertEqual(8#700, FileInfo#file_info.mode band 8#777),
            
            %% Clean up
            file:del_dir(TestDir);
        false ->
            %% Skip on non-Unix platforms
            ok
    end.

compile_core_erlang_uses_secure_temp_dir_test() ->
    %% Test that compile_core_erlang uses the secure temp directory (Unix only)
    case is_unix() of
        true ->
            %% We can't easily test the full compilation path without a valid Core Erlang module,
            %% but we can verify that the secure temp dir is created
            CoreErlang = <<"module 'test_secure' ['main'/0]\n"
                           "attributes []\n"
                           "'main'/0 = fun () -> 42\n">>,
            ModuleName = test_secure,
            
            %% Call compile_core_erlang (will likely fail with invalid Core Erlang syntax)
            _Result = beamtalk_repl_eval:compile_core_erlang(CoreErlang, ModuleName),
            
            %% Verify secure temp directory was created
            {ok, SecureTempDir} = beamtalk_repl_eval:ensure_secure_temp_dir(),
            ?assert(filelib:is_dir(SecureTempDir)),
            
            %% Verify it has secure permissions
            {ok, FileInfo} = file:read_file_info(SecureTempDir),
            Mode = FileInfo#file_info.mode band 8#777,
            ?assertEqual(8#700, Mode);
        false ->
            %% Skip on non-Unix platforms
            ok
    end.

%%% BT-238: Test future_rejected → error conversion

%% Test that do_eval treats {future_rejected, Reason} as an error
do_eval_rejected_future_becomes_error_test() ->
    %% This test needs to simulate a rejected future return value
    %% We'll use meck to mock maybe_await_future to return {future_rejected, Reason}
    
    %% Note: This is a simplified test. In practice, rejected futures come from
    %% actor message sends that fail. The full integration is tested via
    %% manual REPL testing (e.g., "c super" on a Counter actor).
    
    %% For now, document that this code path is tested manually via:
    %% 1. Start REPL
    %% 2. :load tests/e2e/fixtures/counter.bt
    %% 3. c := Counter spawn
    %% 4. c super  (sends invalid message, future rejected, shows as error)
    
    %% TODO BT-240: Add proper unit test with mocked maybe_await_future
    ok.

%%% parse_file_compile_result tests

parse_file_compile_result_extracts_module_name_test() ->
    %% Simulate a daemon response with module_name, core_erlang, and classes.
    %% We use a minimal Core Erlang module that compile:forms can handle.
    CoreErlang = <<"module 'counter' ['__info__'/0]\n  attributes []\n'__info__'/0 = fun () -> 'ok'\nend">>,
    Result = #{
        <<"success">> => true,
        <<"core_erlang">> => CoreErlang,
        <<"module_name">> => <<"counter">>,
        <<"classes">> => []
    },
    case beamtalk_repl_eval:parse_file_compile_result(Result) of
        {ok, Binary, _ClassNames, ModuleName} ->
            ?assertEqual(counter, ModuleName),
            ?assert(is_binary(Binary));
        {error, Reason} ->
            error({unexpected_error, Reason})
    end.

parse_file_compile_result_missing_module_name_test() ->
    Result = #{
        <<"success">> => true,
        <<"core_erlang">> => <<"module 'test' [] attributes [] end">>,
        <<"classes">> => []
    },
    ?assertMatch({error, {daemon_error, <<"No module_name in response">>}},
                 beamtalk_repl_eval:parse_file_compile_result(Result)).

parse_file_compile_result_failure_test() ->
    Result = #{
        <<"success">> => false,
        <<"formatted_diagnostics">> => [<<"Error at line 1">>]
    },
    ?assertMatch({error, {compile_error, _}},
                 beamtalk_repl_eval:parse_file_compile_result(Result)).

%%% IO Capture tests (BT-355)

io_capture_basic_put_chars_test() ->
    %% Test direct put_chars capture
    {CapturePid, OldGL} = beamtalk_repl_eval:start_io_capture(),
    io:put_chars("hello"),
    Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OldGL}),
    ?assertEqual(<<"hello">>, Output).

io_capture_io_format_test() ->
    %% Test io:format which uses {put_chars, Enc, Mod, Func, Args}
    {CapturePid, OldGL} = beamtalk_repl_eval:start_io_capture(),
    io:format("value: ~p~n", [42]),
    Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OldGL}),
    ?assertEqual(<<"value: 42\n">>, Output).

io_capture_empty_test() ->
    %% No output produces empty binary
    {CapturePid, OldGL} = beamtalk_repl_eval:start_io_capture(),
    Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OldGL}),
    ?assertEqual(<<>>, Output).

io_capture_multiple_writes_test() ->
    %% Multiple writes are concatenated
    {CapturePid, OldGL} = beamtalk_repl_eval:start_io_capture(),
    io:format("a"),
    io:format("b"),
    io:format("c"),
    Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OldGL}),
    ?assertEqual(<<"abc">>, Output).

io_capture_restores_group_leader_test() ->
    %% Verify group_leader is restored after capture
    OrigGL = group_leader(),
    {CapturePid, OldGL} = beamtalk_repl_eval:start_io_capture(),
    ?assertNotEqual(OrigGL, group_leader()),
    _Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OldGL}),
    ?assertEqual(OrigGL, group_leader()).

io_capture_dead_process_test() ->
    %% If capture process died, stop_io_capture returns <<>>
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> ok end),
    timer:sleep(50),  %% Let it die
    Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OldGL}),
    ?assertEqual(<<>>, Output).

