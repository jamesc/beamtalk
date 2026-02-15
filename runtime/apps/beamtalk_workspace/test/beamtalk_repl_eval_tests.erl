%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_eval module
%%%
%%% Tests expression evaluation, file loading, and compilation.

-module(beamtalk_repl_eval_tests).
-include_lib("eunit/include/eunit.hrl").

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

%%% Diagnostics formatting tests

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

%%% State-based do_eval tests

do_eval_increments_counter_test() ->
    %% Test that do_eval increments the eval counter
    State = beamtalk_repl_state:new(undefined, 0),
    InitialCounter = beamtalk_repl_state:get_eval_counter(State),
    
    %% Without compiler server running, compilation will fail
    %% But it should still increment the counter
    {error, _, _, _, NewState} = beamtalk_repl_eval:do_eval("1 + 1", State),
    NewCounter = beamtalk_repl_state:get_eval_counter(NewState),
    
    ?assertEqual(InitialCounter + 1, NewCounter).

do_eval_no_compiler_error_test() ->
    %% Without a running compiler server (port backend), should get compile_error
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_eval("1 + 1", State),
    ?assertMatch({error, {compile_error, _}, _, _, _}, Result),
    
    %% Error message should mention compiler
    {error, {compile_error, Msg}, _, _, _} = Result,
    LowerMsg = string:lowercase(Msg),
    ?assert(binary:match(LowerMsg, <<"compiler">>) =/= nomatch).

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

%%% Additional do_eval tests

do_eval_load_binary_error_test() ->
    %% Test case where compilation fails without compiler server
    State = beamtalk_repl_state:new(undefined, 0),
    {error, {compile_error, _}, _, _, NewState} = beamtalk_repl_eval:do_eval("1 + 1", State),
    %% Counter should still increment even on error
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(NewState)).

do_eval_preserves_bindings_on_error_test() ->
    %% Verify that existing bindings are preserved when eval fails
    State = beamtalk_repl_state:new(undefined, 0),
    InitialBindings = #{x => 42, y => 100},
    StateWithBindings = beamtalk_repl_state:set_bindings(InitialBindings, State),
    
    %% Eval will fail (no compiler server), but bindings should be preserved
    {error, _, _, _, NewState} = beamtalk_repl_eval:do_eval("z := 999", StateWithBindings),
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
    
    %% Should get a compile error
    case Result of
        {error, {compile_error, _}, _} -> ok;
        Other -> error({unexpected_result, Other})
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

%% === BT-358: Group leader reset for spawned processes ===

io_capture_resets_spawned_process_group_leader_test() ->
    %% Verify that processes spawned during IO capture get their
    %% group_leader reset to the original GL when capture stops.
    OrigGL = group_leader(),
    {CapturePid, _OldGL} = beamtalk_repl_eval:start_io_capture(),
    %% Spawn a process that inherits the capture GL and stays alive
    SpawnedPid = spawn(fun() -> receive stop -> ok end end),
    %% Verify it inherited the capture process as GL
    {group_leader, SpawnedGL} = erlang:process_info(SpawnedPid, group_leader),
    ?assertEqual(CapturePid, SpawnedGL),
    %% Stop capture — should reset spawned process's GL
    _Output = beamtalk_repl_eval:stop_io_capture({CapturePid, OrigGL}),
    %% Verify spawned process now has the original stable GL
    {group_leader, RestoredGL} = erlang:process_info(SpawnedPid, group_leader),
    ?assertEqual(OrigGL, RestoredGL),
    SpawnedPid ! stop.

io_capture_reset_does_not_affect_unrelated_processes_test() ->
    %% Verify that processes NOT spawned during capture keep their GL.
    OrigGL = group_leader(),
    %% Spawn a process BEFORE capture starts
    PreExisting = spawn(fun() -> receive stop -> ok end end),
    {group_leader, PreGL} = erlang:process_info(PreExisting, group_leader),
    %% Start and stop capture
    CaptureRef = beamtalk_repl_eval:start_io_capture(),
    _Output = beamtalk_repl_eval:stop_io_capture(CaptureRef),
    %% Pre-existing process should keep its original GL
    {group_leader, PostGL} = erlang:process_info(PreExisting, group_leader),
    ?assertEqual(PreGL, PostGL),
    %% Clean up
    PreExisting ! stop.

%% === is_stdlib_path tests ===

is_stdlib_path_relative_lib_test() ->
    ?assert(beamtalk_repl_eval:is_stdlib_path("lib/Integer.bt")).

is_stdlib_path_absolute_test() ->
    ?assert(beamtalk_repl_eval:is_stdlib_path("/workspace/project/lib/Integer.bt")).

is_stdlib_path_non_lib_test() ->
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("src/MyClass.bt")).

is_stdlib_path_non_lib_absolute_test() ->
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("/workspace/project/src/MyClass.bt")).

is_stdlib_path_lib_without_trailing_slash_test() ->
    %% "lib" alone (no trailing slash) is NOT a lib path
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("lib")).

is_stdlib_path_libs_prefix_test() ->
    %% "libs/" is NOT the same as "lib/"
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("libs/Integer.bt")).

is_stdlib_path_embedded_lib_test() ->
    %% Path with /lib/ deeper in the tree
    ?assert(beamtalk_repl_eval:is_stdlib_path("/home/user/projects/beamtalk/lib/String.bt")).

is_stdlib_path_empty_test() ->
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("")).

%%% strip_internal_bindings tests

strip_internal_bindings_removes_registry_test() ->
    Bindings = #{'__repl_actor_registry__' => self(), x => 42, y => 100},
    Result = beamtalk_repl_eval:strip_internal_bindings(Bindings),
    ?assertEqual(false, maps:is_key('__repl_actor_registry__', Result)),
    ?assertEqual(42, maps:get(x, Result)),
    ?assertEqual(100, maps:get(y, Result)).

strip_internal_bindings_empty_map_test() ->
    ?assertEqual(#{}, beamtalk_repl_eval:strip_internal_bindings(#{})).

strip_internal_bindings_no_internal_keys_test() ->
    Bindings = #{a => 1, b => 2, c => 3},
    ?assertEqual(Bindings, beamtalk_repl_eval:strip_internal_bindings(Bindings)).

strip_internal_bindings_only_registry_test() ->
    Bindings = #{'__repl_actor_registry__' => self()},
    ?assertEqual(#{}, beamtalk_repl_eval:strip_internal_bindings(Bindings)).

%%% should_purge_module tests

should_purge_module_undefined_registry_test() ->
    %% No registry means always purge
    ?assertEqual(true, beamtalk_repl_eval:should_purge_module(some_module, undefined)).

should_purge_module_no_actors_test() ->
    %% Start a registry with no actors — module should be purged
    {ok, Registry} = beamtalk_repl_actors:start_link(registered),
    Result = beamtalk_repl_eval:should_purge_module(beamtalk_repl_eval_999, Registry),
    gen_server:stop(Registry),
    ?assertEqual(true, Result).

should_purge_module_with_actor_test() ->
    %% Start a registry, register an actor — module should NOT be purged
    {ok, Registry} = beamtalk_repl_actors:start_link(registered),
    %% Create a dummy process to act as the actor
    ActorPid = spawn(fun() -> receive stop -> ok end end),
    %% register_actor(Registry, ActorPid, ClassName, ModuleName)
    beamtalk_repl_actors:register_actor(Registry, ActorPid, 'TestClass', test_module),
    Result = beamtalk_repl_eval:should_purge_module(test_module, Registry),
    ActorPid ! stop,
    gen_server:stop(Registry),
    ?assertEqual(false, Result).

should_purge_module_different_module_test() ->
    %% Actor registered for different module — our module should be purged
    {ok, Registry} = beamtalk_repl_actors:start_link(registered),
    ActorPid = spawn(fun() -> receive stop -> ok end end),
    beamtalk_repl_actors:register_actor(Registry, ActorPid, 'OtherClass', other_module),
    Result = beamtalk_repl_eval:should_purge_module(my_module, Registry),
    ActorPid ! stop,
    gen_server:stop(Registry),
    ?assertEqual(true, Result).

%%% maybe_await_future tests

maybe_await_future_non_pid_integer_test() ->
    ?assertEqual(42, beamtalk_repl_eval:maybe_await_future(42)).

maybe_await_future_non_pid_binary_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_eval:maybe_await_future(<<"hello">>)).

maybe_await_future_non_pid_atom_test() ->
    ?assertEqual(nil, beamtalk_repl_eval:maybe_await_future(nil)).

maybe_await_future_non_pid_list_test() ->
    ?assertEqual([1, 2, 3], beamtalk_repl_eval:maybe_await_future([1, 2, 3])).

maybe_await_future_non_pid_map_test() ->
    ?assertEqual(#{a => 1}, beamtalk_repl_eval:maybe_await_future(#{a => 1})).

maybe_await_future_non_pid_tuple_test() ->
    ?assertEqual({ok, value}, beamtalk_repl_eval:maybe_await_future({ok, value})).

maybe_await_future_beamtalk_object_test() ->
    %% beamtalk_object tuple should be returned as-is
    Obj = {beamtalk_object, 'Counter', self(), #{}},
    ?assertEqual(Obj, beamtalk_repl_eval:maybe_await_future(Obj)).

maybe_await_future_resolved_future_test() ->
    %% Simulate a future that resolves
    FuturePid = spawn(fun() ->
        receive
            {await, Caller, _Timeout} ->
                Caller ! {future_resolved, self(), 42}
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future(FuturePid),
    ?assertEqual(42, Result).

maybe_await_future_rejected_future_test() ->
    %% Simulate a future that rejects
    FuturePid = spawn(fun() ->
        receive
            {await, Caller, _Timeout} ->
                Caller ! {future_rejected, self(), some_error}
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future(FuturePid),
    ?assertEqual({future_rejected, some_error}, Result).

maybe_await_future_non_future_pid_test() ->
    %% A PID that doesn't respond to the future protocol
    %% should be returned as-is after timeout
    NonFuturePid = spawn(fun() ->
        receive _ -> ok after 5000 -> ok end
    end),
    Result = beamtalk_repl_eval:maybe_await_future(NonFuturePid),
    ?assertEqual(NonFuturePid, Result).

%%% extract_assignment edge cases

extract_assignment_empty_string_test() ->
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("")).

extract_assignment_just_operator_test() ->
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment(":=")).

extract_assignment_complex_rhs_test() ->
    %% Assignment with complex RHS expression
    ?assertEqual({ok, result}, beamtalk_repl_eval:extract_assignment("result := obj doSomething: 42")).

extract_assignment_underscore_prefix_test() ->
    ?assertEqual({ok, '_temp'}, beamtalk_repl_eval:extract_assignment("_temp := 0")).

%%% handle_io_request tests

handle_io_request_put_chars_utf8_test() ->
    {Reply, Buffer} = beamtalk_repl_eval:handle_io_request(
        {put_chars, unicode, <<"hello">>}, <<>>),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"hello">>, Buffer).

handle_io_request_put_chars_latin1_test() ->
    {Reply, Buffer} = beamtalk_repl_eval:handle_io_request(
        {put_chars, latin1, <<"world">>}, <<>>),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"world">>, Buffer).

handle_io_request_put_chars_legacy_test() ->
    %% Legacy form without encoding
    {Reply, Buffer} = beamtalk_repl_eval:handle_io_request(
        {put_chars, <<"legacy">>}, <<>>),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"legacy">>, Buffer).

handle_io_request_put_chars_mfa_test() ->
    %% {put_chars, Enc, Mod, Func, Args} form used by io:format
    {Reply, Buffer} = beamtalk_repl_eval:handle_io_request(
        {put_chars, unicode, io_lib, format, ["val: ~p", [42]]}, <<>>),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"val: 42">>, Buffer).

handle_io_request_unsupported_test() ->
    %% Unsupported IO request type
    {Reply, Buffer} = beamtalk_repl_eval:handle_io_request(
        {get_chars, unicode, <<"prompt">>, 1}, <<"existing">>),
    ?assertEqual({error, enotsup}, Reply),
    ?assertEqual(<<"existing">>, Buffer).

handle_io_request_accumulates_buffer_test() ->
    %% Buffer should accumulate across calls
    {ok, Buffer1} = beamtalk_repl_eval:handle_io_request(
        {put_chars, unicode, <<"one">>}, <<>>),
    {ok, Buffer2} = beamtalk_repl_eval:handle_io_request(
        {put_chars, unicode, <<"two">>}, Buffer1),
    ?assertEqual(<<"onetwo">>, Buffer2).

%%% inject_output tests

inject_output_ok_test() ->
    State = some_state,
    Result = beamtalk_repl_eval:inject_output({ok, 42, State}, <<"output">>, [<<"warn">>]),
    ?assertEqual({ok, 42, <<"output">>, [<<"warn">>], State}, Result).

inject_output_error_test() ->
    State = some_state,
    Result = beamtalk_repl_eval:inject_output({error, reason, State}, <<"err_out">>, []),
    ?assertEqual({error, reason, <<"err_out">>, [], State}, Result).

inject_output_empty_output_test() ->
    State = some_state,
    Result = beamtalk_repl_eval:inject_output({ok, nil, State}, <<>>, []),
    ?assertEqual({ok, nil, <<>>, [], State}, Result).

inject_output_multiple_warnings_test() ->
    State = some_state,
    Warnings = [<<"warn1">>, <<"warn2">>, <<"warn3">>],
    Result = beamtalk_repl_eval:inject_output({ok, 99, State}, <<>>, Warnings),
    ?assertEqual({ok, 99, <<>>, Warnings, State}, Result).

%%% format_formatted_diagnostics edge cases

format_formatted_diagnostics_single_binary_with_newlines_test() ->
    FormattedDiagnostics = [<<"Line 1\nLine 2\nLine 3">>],
    Result = beamtalk_repl_eval:format_formatted_diagnostics(FormattedDiagnostics),
    ?assertEqual(<<"Line 1\nLine 2\nLine 3">>, Result).

%%% do_eval error edge cases

do_eval_empty_expression_test() ->
    %% Empty expression should still attempt compilation (and fail without compiler)
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_eval("", State),
    ?assertMatch({error, {compile_error, _}, _, _, _}, Result).

do_eval_counter_increments_on_each_call_test() ->
    %% Verify counter increments independently on each call
    State0 = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State0)),
    {error, _, _, _, State1} = beamtalk_repl_eval:do_eval("1", State0),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(State1)),
    {error, _, _, _, State2} = beamtalk_repl_eval:do_eval("2", State1),
    ?assertEqual(2, beamtalk_repl_state:get_eval_counter(State2)).

%%% handle_load edge cases

handle_load_empty_file_test() ->
    %% Empty file should attempt compile (and fail without compiler)
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(os:getenv("TMPDIR", "/tmp"),
                             io_lib:format("test_empty_~p.bt", [UniqueId])),
    ok = file:write_file(TempFile, <<>>),
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load(TempFile, State),
    file:delete(TempFile),
    %% Should fail with compile error
    case Result of
        {error, {compile_error, _}, _} -> ok;
        Other -> error({unexpected_result, Other})
    end.

