%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_eval_tests).

-moduledoc """
Unit tests for beamtalk_repl_eval module

Tests expression evaluation, file loading, and compilation.
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

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
    ?assertEqual(<<"Compilation failed">>, beamtalk_repl_compiler:format_formatted_diagnostics([])).

format_formatted_diagnostics_single_test() ->
    FormattedDiagnostics = [<<"Unexpected token">>],
    ?assertEqual(
        <<"Unexpected token">>,
        beamtalk_repl_compiler:format_formatted_diagnostics(FormattedDiagnostics)
    ).

format_formatted_diagnostics_multiple_test() ->
    FormattedDiagnostics = [<<"Error 1">>, <<"Error 2">>, <<"Error 3">>],
    Result = beamtalk_repl_compiler:format_formatted_diagnostics(FormattedDiagnostics),
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
    ?assertMatch({error, #beamtalk_error{kind = compile_error}, _, _, _}, Result),

    %% Error message should mention compiler
    {error, #beamtalk_error{message = Msg}, _, _, _} = Result,
    ?assert(re:run(Msg, <<"compiler">>, [caseless, {capture, none}]) =:= match).

%%% File loading tests

handle_load_file_not_found_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load("/nonexistent/file.bt", State),
    ?assertMatch({error, {file_not_found, _}, _}, Result).

handle_load_directory_test() ->
    %% Loading a directory should fail
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load(temp_dir(), State),
    %% Should get read_error since it's a directory
    ?assertMatch({error, {read_error, _}, _}, Result).

%%% Additional do_eval tests

do_eval_compile_error_no_server_test() ->
    %% Compilation fails without compiler server - load_binary is never reached
    State = beamtalk_repl_state:new(undefined, 0),
    {error, #beamtalk_error{}, _, _, NewState} = beamtalk_repl_eval:do_eval("1 + 1", State),
    %% Counter should still increment even on error
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(NewState)).

wrap_load_err_returns_structured_error_test() ->
    %% wrap_load_err/3 normalises a raw load reason to a structured #beamtalk_error{}.
    %% load_error maps to kind=io_error via ensure_structured_error.
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:wrap_load_err(bad_module, [], State),
    ?assertMatch({error, #beamtalk_error{kind = io_error}, <<>>, [], _}, Result).

wrap_load_err_message_contains_reason_test() ->
    %% The error message should describe the load failure reason.
    State = beamtalk_repl_state:new(undefined, 0),
    {error, #beamtalk_error{message = Msg}, <<>>, [], _} =
        beamtalk_repl_eval:wrap_load_err(bad_module, [], State),
    ?assert(binary:match(Msg, <<"bad_module">>) =/= nomatch).

wrap_load_err_preserves_warnings_test() ->
    %% wrap_load_err/3 passes Warnings through to the result tuple.
    State = beamtalk_repl_state:new(undefined, 0),
    Warnings = [<<"unused variable x">>, <<"deprecated function">>],
    {error, #beamtalk_error{}, <<>>, ReturnedWarnings, _} =
        beamtalk_repl_eval:wrap_load_err(bad_module, Warnings, State),
    ?assertEqual(Warnings, ReturnedWarnings).

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

%%% BT-2688: connected-session Program exit: result plumbing

inject_output_script_exit_test() ->
    %% inject_output/3 threads captured output + warnings into the script_exit
    %% shape so the shell can report the exit status (ADR 0099 §3 / Phase 5).
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:inject_output({script_exit, 7, State}, <<"out">>, [<<"w">>]),
    ?assertEqual({script_exit, 7, <<"out">>, [<<"w">>], State}, Result).

%%% rebuild_bindings_from_steps tests (BT-1261)

rebuild_bindings_from_steps_simple_assignment_test() ->
    %% A single assignment step stores the awaited value under the variable name.
    Steps = [{<<"x := 42">>, 42}],
    Bindings = #{},
    Result = beamtalk_repl_eval:rebuild_bindings_from_steps(Steps, Bindings),
    ?assertEqual(42, maps:get(x, Result)).

rebuild_bindings_from_steps_overwrites_raw_future_test() ->
    %% When CleanBindings holds a raw future handle, the awaited value must replace it.
    FakeFuture = {beamtalk_future, self()},
    Steps = [{<<"x := asyncOp">>, resolved_value}],
    Bindings = #{x => FakeFuture},
    Result = beamtalk_repl_eval:rebuild_bindings_from_steps(Steps, Bindings),
    ?assertEqual(resolved_value, maps:get(x, Result)).

rebuild_bindings_from_steps_non_assignment_leaves_bindings_unchanged_test() ->
    %% Steps that are not assignments must not modify existing bindings.
    Steps = [{<<"x + 1">>, 43}],
    Bindings = #{x => 42},
    Result = beamtalk_repl_eval:rebuild_bindings_from_steps(Steps, Bindings),
    ?assertEqual(42, maps:get(x, Result)),
    ?assertEqual(1, maps:size(Result)).

rebuild_bindings_from_steps_chained_assignments_test() ->
    %% Each assignment in a multi-step trace is applied in order.
    Steps = [
        {<<"x := 10">>, 10},
        {<<"y := 20">>, 20}
    ],
    Bindings = #{},
    Result = beamtalk_repl_eval:rebuild_bindings_from_steps(Steps, Bindings),
    ?assertEqual(10, maps:get(x, Result)),
    ?assertEqual(20, maps:get(y, Result)).

rebuild_bindings_from_steps_mixed_steps_test() ->
    %% A mix of assignment and expression steps: only assignments update bindings.
    Steps = [
        {<<"x := 5">>, 5},
        {<<"x + 1">>, 6},
        {<<"y := 7">>, 7}
    ],
    Bindings = #{},
    Result = beamtalk_repl_eval:rebuild_bindings_from_steps(Steps, Bindings),
    ?assertEqual(5, maps:get(x, Result)),
    ?assertEqual(7, maps:get(y, Result)),
    ?assertEqual(2, maps:size(Result)).

rebuild_bindings_from_steps_empty_steps_test() ->
    %% Empty step list must return bindings unchanged.
    Bindings = #{x => 99},
    Result = beamtalk_repl_eval:rebuild_bindings_from_steps([], Bindings),
    ?assertEqual(Bindings, Result).
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
    TempFile = filename:join(
        temp_dir(),
        io_lib:format("test_invalid_bt_~p.bt", [UniqueId])
    ),
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

%%% IO Capture tests (BT-355)

io_capture_basic_put_chars_test() ->
    %% Test direct put_chars capture
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    io:put_chars("hello"),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<"hello">>, Output).

io_capture_io_format_test() ->
    %% Test io:format which uses {put_chars, Enc, Mod, Func, Args}
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    io:format("value: ~p~n", [42]),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<"value: 42\n">>, Output).

io_capture_empty_test() ->
    %% No output produces empty binary
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<>>, Output).

io_capture_multiple_writes_test() ->
    %% Multiple writes are concatenated
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    io:format("a"),
    io:format("b"),
    io:format("c"),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<"abc">>, Output).

io_capture_restores_group_leader_test() ->
    %% Verify group_leader is restored after capture
    OrigGL = group_leader(),
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    ?assertNotEqual(OrigGL, group_leader()),
    _Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(OrigGL, group_leader()).

io_capture_dead_process_test() ->
    %% If capture process died, stop_io_capture returns <<>>
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> ok end),
    %% Let it die
    timer:sleep(50),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<>>, Output).

%% === BT-358: Group leader reset for spawned processes ===

io_capture_resets_spawned_process_group_leader_test() ->
    %% Verify that processes spawned during IO capture get their
    %% group_leader reset to the original GL when capture stops.
    OrigGL = group_leader(),
    {CapturePid, _OldGL} = beamtalk_io_capture:start(),
    %% Spawn a process that inherits the capture GL and stays alive
    SpawnedPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    %% Verify it inherited the capture process as GL
    {group_leader, SpawnedGL} = erlang:process_info(SpawnedPid, group_leader),
    ?assertEqual(CapturePid, SpawnedGL),
    %% Stop capture — should reset spawned process's GL
    _Output = beamtalk_io_capture:stop({CapturePid, OrigGL}),
    %% Verify spawned process now has the original stable GL
    {group_leader, RestoredGL} = erlang:process_info(SpawnedPid, group_leader),
    ?assertEqual(OrigGL, RestoredGL),
    SpawnedPid ! stop.

io_capture_reset_does_not_affect_unrelated_processes_test() ->
    %% Verify that processes NOT spawned during capture keep their GL.
    _OrigGL = group_leader(),
    %% Spawn a process BEFORE capture starts
    PreExisting = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    {group_leader, PreGL} = erlang:process_info(PreExisting, group_leader),
    %% Start and stop capture
    CaptureRef = beamtalk_io_capture:start(),
    _Output = beamtalk_io_capture:stop(CaptureRef),
    %% Pre-existing process should keep its original GL
    {group_leader, PostGL} = erlang:process_info(PreExisting, group_leader),
    ?assertEqual(PreGL, PostGL),
    %% Clean up
    PreExisting ! stop.

%% === is_stdlib_path tests ===

is_stdlib_path_relative_lib_test() ->
    ?assert(beamtalk_repl_loader:is_stdlib_path("stdlib/src/integer.bt")).

is_stdlib_path_absolute_test() ->
    ?assert(beamtalk_repl_loader:is_stdlib_path("/workspace/project/stdlib/src/integer.bt")).

is_stdlib_path_non_lib_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("src/MyClass.bt")).

is_stdlib_path_non_lib_absolute_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("/workspace/project/src/MyClass.bt")).

is_stdlib_path_lib_without_trailing_slash_test() ->
    %% "stdlib/src" alone (no trailing slash) is NOT a stdlib path
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("stdlib/src")).

is_stdlib_path_libs_prefix_test() ->
    %% "stdlib/srcs/" is NOT the same as "stdlib/src/"
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("stdlib/srcs/integer.bt")).

is_stdlib_path_embedded_lib_test() ->
    %% Path with /stdlib/src/ deeper in the tree
    ?assert(
        beamtalk_repl_loader:is_stdlib_path("/home/user/projects/beamtalk/stdlib/src/string.bt")
    ).

is_stdlib_path_empty_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("")).

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
    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    %% register_actor(Registry, ActorPid, ClassName, ModuleName)
    beamtalk_repl_actors:register_actor(Registry, ActorPid, 'TestClass', test_module),
    Result = beamtalk_repl_eval:should_purge_module(test_module, Registry),
    ActorPid ! stop,
    gen_server:stop(Registry),
    ?assertEqual(false, Result).

should_purge_module_different_module_test() ->
    %% Actor registered for different module — our module should be purged
    {ok, Registry} = beamtalk_repl_actors:start_link(registered),
    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
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
    %% Simulate a future that resolves (tagged tuple)
    FuturePid = spawn(fun() ->
        receive
            {await, Caller, _Timeout} ->
                Caller ! {future_resolved, self(), 42}
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future({beamtalk_future, FuturePid}),
    ?assertEqual(42, Result).

maybe_await_future_rejected_future_test() ->
    %% Simulate a future that rejects (tagged tuple)
    FuturePid = spawn(fun() ->
        receive
            {await, Caller, _Timeout} ->
                Caller ! {future_rejected, self(), some_error}
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future({beamtalk_future, FuturePid}),
    ?assertEqual({future_rejected, some_error}, Result).

maybe_await_future_non_future_pid_test() ->
    %% A PID that doesn't respond to the future protocol
    %% should be returned as-is after timeout
    NonFuturePid = spawn(fun() ->
        receive
            _ -> ok
        after 5000 -> ok
        end
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
    ?assertEqual(
        {ok, result}, beamtalk_repl_eval:extract_assignment("result := obj doSomething: 42")
    ).

extract_assignment_underscore_prefix_test() ->
    ?assertEqual({ok, '_temp'}, beamtalk_repl_eval:extract_assignment("_temp := 0")).

%%% handle_io_request tests

handle_io_request_put_chars_utf8_test() ->
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, unicode, <<"hello">>}, <<>>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"hello">>, Buffer).

handle_io_request_put_chars_latin1_test() ->
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, latin1, <<"world">>}, <<>>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"world">>, Buffer).

handle_io_request_put_chars_legacy_test() ->
    %% Legacy form without encoding
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, <<"legacy">>}, <<>>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"legacy">>, Buffer).

handle_io_request_put_chars_mfa_test() ->
    %% {put_chars, Enc, Mod, Func, Args} form used by io:format
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, unicode, io_lib, format, ["val: ~p", [42]]}, <<>>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"val: 42">>, Buffer).

handle_io_request_unsupported_test() ->
    %% Unsupported IO request type
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {get_chars, unicode, <<"prompt">>, 1}, <<"existing">>
    ),
    ?assertEqual({error, enotsup}, Reply),
    ?assertEqual(<<"existing">>, Buffer).

handle_io_request_accumulates_buffer_test() ->
    %% Buffer should accumulate across calls
    {ok, Buffer1} = beamtalk_io_capture:handle_io_request(
        {put_chars, unicode, <<"one">>}, <<>>
    ),
    {ok, Buffer2} = beamtalk_io_capture:handle_io_request(
        {put_chars, unicode, <<"two">>}, Buffer1
    ),
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
    Result = beamtalk_repl_compiler:format_formatted_diagnostics(FormattedDiagnostics),
    ?assertEqual(<<"Line 1\nLine 2\nLine 3">>, Result).

%%% do_eval error edge cases

do_eval_empty_expression_test() ->
    %% Empty expression should still attempt compilation (and fail without compiler)
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_eval("", State),
    ?assertMatch({error, #beamtalk_error{kind = compile_error}, _, _, _}, Result).

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
    TempFile = filename:join(
        temp_dir(),
        io_lib:format("test_empty_~p.bt", [UniqueId])
    ),
    ok = file:write_file(TempFile, <<>>),
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load(TempFile, State),
    file:delete(TempFile),
    %% Should fail with compile error
    case Result of
        {error, {compile_error, _}, _} -> ok;
        Other1 -> error({unexpected_result, Other1})
    end.

%%% ===========================================================================
%%% BT-627: Coverage tests for internal functions and edge cases
%%% ===========================================================================

%%% is_internal_key/1 tests

is_internal_key_double_underscore_test() ->
    ?assert(beamtalk_repl_compiler:is_internal_key('__repl_actor_registry__')).

is_internal_key_single_underscore_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key('_error')).

is_internal_key_regular_atom_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key(x)).

is_internal_key_empty_atom_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key('')).

%%% register_classes/2 tests

register_classes_no_function_test() ->
    ?assertEqual(ok, beamtalk_repl_loader:register_classes([], lists)).

%%% trigger_hot_reload/2 tests

trigger_hot_reload_empty_classes_test() ->
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, [])).

trigger_hot_reload_unknown_class_test() ->
    Classes = [#{name => <<"xyzzy_nonexistent_class_99999">>}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_undefined_name_test() ->
    Classes = [#{name => undefined}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_no_name_key_test() ->
    Classes = [#{}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_list_name_test() ->
    Classes = [#{name => "xyzzy_nonexistent_class_88888"}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_atom_name_test() ->
    Classes = [#{name => xyzzy_nonexistent_class_77777}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

%%% activate_module/2 tests

activate_module_nonexistent_test() ->
    ?assertEqual(ok, beamtalk_repl_loader:activate_module(lists, [])).

%%% io_passthrough_loop tests

io_passthrough_forward_test() ->
    OldGL = group_leader(),
    PassPid = spawn(fun() -> beamtalk_io_capture:io_passthrough_loop(OldGL) end),
    PassPid ! {io_request, self(), make_ref(), {put_chars, unicode, <<"test">>}},
    timer:sleep(50),
    ?assert(is_process_alive(PassPid)),
    exit(PassPid, normal).

%%% IO capture with dead capture process

io_capture_dead_capture_process_test() ->
    CapturePid = spawn(fun() -> ok end),
    timer:sleep(10),
    OldGL = group_leader(),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<>>, Output).

%%% handle_io_request edge cases

handle_io_request_put_chars_invalid_encoding_test() ->
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, utf32, <<255, 254, 0, 0>>}, <<"existing">>
    ),
    ?assertEqual(ok, Reply),
    ?assert(is_binary(Buffer)).

handle_io_request_put_chars_mfa_error_test() ->
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, utf8, erlang, error, [badarg]}, <<"existing">>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"existing">>, Buffer).

%%% handle_load with real file but no compiler

handle_load_valid_file_no_compiler_test() ->
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(
        temp_dir(),
        io_lib:format("test_valid_~p.bt", [UniqueId])
    ),
    ok = file:write_file(TempFile, <<"Object subclass: MyTest [\n]\n">>),
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_load(TempFile, State),
    file:delete(TempFile),
    case Result of
        {error, {compile_error, _}, _} -> ok;
        {error, {core_compile_error, _}, _} -> ok;
        Other2 -> error({unexpected_result, Other2})
    end.

%%% do_eval with bindings and actor registry

do_eval_with_registry_no_compiler_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    State = beamtalk_repl_state:new(RegistryPid, 0),
    {error, #beamtalk_error{}, _, _, NewState} = beamtalk_repl_eval:do_eval("1 + 2", State),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(NewState)),
    gen_server:stop(RegistryPid).

%%% format_formatted_diagnostics edge cases

format_formatted_diagnostics_two_items_test() ->
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([<<"A">>, <<"B">>]),
    ?assertEqual(<<"A\n\nB">>, Result).

%%% extract_assignment edge cases

extract_assignment_multiline_test() ->
    ?assertMatch({ok, _}, beamtalk_repl_eval:extract_assignment("x := [1, 2, 3]")).

extract_assignment_no_space_v2_test() ->
    ?assertEqual({ok, abc}, beamtalk_repl_eval:extract_assignment("abc:=123")).

%% BT-3368: a second top-level statement on its own line (no `.` at all)
%% must bail to `none`, exactly like the existing period-separated case —
%% otherwise the first statement's variable gets clobbered with the whole
%% call's `Result` (the last statement's value) in process_eval_result/4.
extract_assignment_newline_separated_statements_test() ->
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("alpha := 111\nbeta := 222")),
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("a1 := 1\na2 := 2\na3 := 3")).

%% A trailing newline with nothing (or only whitespace) after it is not a
%% second statement — the single assignment is still detected.
extract_assignment_trailing_newline_test() ->
    ?assertEqual({ok, count}, beamtalk_repl_eval:extract_assignment("count := 0\n")),
    ?assertEqual({ok, count}, beamtalk_repl_eval:extract_assignment("count := 0\n  \n")).

%% BT-3368 regression guard: a SINGLE assignment whose right-hand side
%% merely continues onto later lines (a multi-line block/collection
%% literal, or the value on a continuation line) must NOT be mistaken for
%% multiple statements — none of the continuation lines themselves open
%% with `ident :=`, so this must still resolve to the one real assignment.
extract_assignment_multiline_rhs_is_still_one_statement_test() ->
    ?assertEqual({ok, result}, beamtalk_repl_eval:extract_assignment("result :=\n  42")),
    ?assertEqual(
        {ok, doubler},
        beamtalk_repl_eval:extract_assignment("doubler := [\n  :x |\n  x * 2\n]")
    ).

%% BT-3368 regression guard (review follow-up): a period *nested* inside a
%% block/collection literal — itself part of the single outer assignment's
%% right-hand side — must not be mistaken for the pre-existing
%% period-separates-statements signal. `docs/beamtalk-language-features.md`'s
%% own `ClassBuilder` cascade example is exactly this shape: multiple
%% semicolon-chained keyword sends whose block-literal bodies use `.` to
%% separate their *own* internal statements, all as one top-level
%% assignment.
extract_assignment_nested_period_in_cascade_is_still_one_statement_test() ->
    Src =
        "account := Object classBuilder\n"
        "  name: #Account;\n"
        "  superclass: Object;\n"
        "  classVars: #{ #opened => 0 };\n"
        "  fields: #{ #balance => 0 };\n"
        "  methods: #{ #balance => [:inst | inst fieldAt: #balance] };\n"
        "  classMethods: #{ #open => [:self | self.opened := self.opened + 1. self.opened] };\n"
        "  register",
    ?assertEqual({ok, account}, beamtalk_repl_eval:extract_assignment(Src)).

%% BT-3368 regression guard (review follow-up): a backslash-escaped quote
%% inside a string literal (`"a\"b"`, per `lex_string/0`'s handling of `\`
%% in `source_analysis/lexer.rs`) must not be mistaken for the string's
%% closing delimiter — otherwise a `.` or newline that's still genuinely
%% *inside* the string reads as depth-0 code and wrongly triggers a bail.
extract_assignment_escaped_quote_in_string_is_still_one_statement_test() ->
    %% s := "a\"b. c"  (one statement: a single-quoted string containing an
    %% escaped quote, then ". " — which must stay inside the string).
    Src = "s := \"a\\\"b. c\"",
    ?assertEqual({ok, s}, beamtalk_repl_eval:extract_assignment(Src)),
    %% Same shape, but the content after the escaped quote uses a newline +
    %% ident-looking continuation instead of a period.
    Src2 = "s := \"a\\\"b\nident := 1\"",
    ?assertEqual({ok, s}, beamtalk_repl_eval:extract_assignment(Src2)),
    %% An escaped backslash (`\\`) immediately before a REAL closing quote
    %% must still close the string — only an odd run of backslashes escapes
    %% the quote.
    Src3 = "s := \"a\\\\\". y := 2",
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment(Src3)).

%% BT-3368 regression guard (review follow-up): a `$`-prefixed character
%% literal (`$(`, `$"`, `$\n`, ... — see `lex_character/0`,
%% `source_analysis/lexer.rs`) must be consumed atomically, never letting
%% its payload character be read as a real bracket/quote — otherwise a
%% `$(`/`$[`/`${` inside one statement permanently unbalances `Depth` and
%% masks a real second top-level statement later in the same call.
extract_assignment_character_literal_payload_is_not_a_bracket_test() ->
    %% Genuinely two statements — the bracket-payload character literal in
    %% the first one must not swallow the real top-level `.` separator.
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("x := $( class. y := 2")),
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("x := $[ class. y := 2")),
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("x := ${ class. y := 2")),
    %% A single statement using a bracket- or quote-payload character
    %% literal must still resolve correctly (no false bail either).
    ?assertEqual({ok, x}, beamtalk_repl_eval:extract_assignment("x := $( class")),
    ?assertEqual({ok, x}, beamtalk_repl_eval:extract_assignment("x := $\" class")),
    %% Escaped-payload form (`$\c`) consumes all three characters together.
    ?assertEqual({ok, x}, beamtalk_repl_eval:extract_assignment("x := $\\( class")).

%% BT-3368 regression guard (review follow-up): a string containing
%% interpolation (`"...{expr}..."`) may itself contain a *nested* string
%% literal inside the interpolated expression (`lex_interpolation_body`/
%% `skip_nested_string`, `source_analysis/lexer.rs`) — a single BT string
%% token can legitimately have more than two `"` characters, with the
%% interpolated expression's own `.`/`{`/`}`/`"` syntax interleaved.
%% `skip_string_literal/1` doesn't attempt to mirror that (see its own doc
%% comment) — it bails conservatively (`unsupported`) the moment it sees an
%% unescaped `{`, which `has_second_top_level_statement/1` treats as "found
%% a second statement" so `extract_assignment/1` safely returns `none`
%% (skips the future-rebinding optimization) rather than mis-pairing quotes
%% across the interpolation and misreading genuine interpolated code as a
%% top-level statement boundary.
extract_assignment_interpolated_string_is_not_mis_parsed_test() ->
    %% The exact shape from review: a nested string inside the interpolated
    %% expression, containing what would look like a `.` statement
    %% separator if the scanner mis-closed the outer string early.
    ?assertEqual(
        none, beamtalk_repl_eval:extract_assignment("x := \"pre {a == \"z. bogus\"} mid\"")
    ),
    %% Plain interpolation, no nested string — still conservatively bails
    %% (never mis-parsed), since the scanner doesn't try to reason about
    %% what's inside the interpolation at all.
    ?assertEqual(none, beamtalk_repl_eval:extract_assignment("x := \"Hello {name}\"")),
    %% A plain string with no interpolation at all is unaffected.
    ?assertEqual(
        {ok, x}, beamtalk_repl_eval:extract_assignment("x := \"no interpolation here\"")
    ).

%% BT-3368 review follow-up (CLAUDE.md Essential Rules: a "mirrors" claim
%% across the Rust/Erlang boundary needs a shared conformance fixture, not
%% just a comment): `skip_string_literal/1`/`skip_character_literal/1` are
%% hand-rolled Erlang mirrors of the Rust lexer's `lex_string/0`/
%% `lex_character/0` span computation. Both sides run the exact same cases
%% from `test/fixtures/string_and_character_literal_span_corpus.json` — see
%% `source_analysis::lexer::tests::string_and_character_literal_span_matches_shared_corpus`
%% for the Rust side.
string_and_character_literal_span_matches_shared_corpus_test() ->
    Path = filename:join([
        code:lib_dir(beamtalk_workspace),
        "test",
        "fixtures",
        "string_and_character_literal_span_corpus.json"
    ]),
    {ok, Raw} = file:read_file(Path),
    Cases = json:decode(Raw),
    ?assert(length(Cases) > 0),
    lists:foreach(fun assert_literal_span_case/1, Cases).

assert_literal_span_case(#{
    <<"kind">> := Kind,
    <<"source">> := SourceBin,
    <<"expected_end">> := ExpectedEnd,
    <<"why">> := Why
}) ->
    Source = unicode:characters_to_list(SourceBin),
    Remaining =
        case Kind of
            <<"string">> ->
                {ok, R} = beamtalk_repl_eval:skip_string_literal(Source),
                R;
            <<"character">> ->
                beamtalk_repl_eval:skip_character_literal(Source)
        end,
    Consumed = length(Source) - length(Remaining),
    ?assertEqual(
        ExpectedEnd,
        Consumed,
        lists:flatten(io_lib:format("span-end mismatch for ~p (~ts)", [Source, Why]))
    ).

%% BT-3372: a `//` line comment containing an unbalanced bracket, followed
%% by a genuine second top-level statement, must still be detected as two
%% statements — the bracket inside the comment must not permanently bump
%% `Depth` and mask the real statement boundary that follows. Exact repro
%% from the issue: without comment-awareness, the `(` in `// (see below`
%% bumps `Depth` to 1 and nothing ever closes it, so `extract_assignment/1`
%% wrongly returns `{ok, alpha}` and `process_eval_result/4` clobbers
%% `alpha`'s already-correct binding with `beta`'s value.
extract_assignment_line_comment_unbalanced_bracket_test() ->
    ?assertEqual(
        none, beamtalk_repl_eval:extract_assignment("alpha := 1 // (see below\nbeta := 2")
    ).

%% BT-3372: a `"` inside a `//` comment must not be misread as the start of
%% a real string literal — otherwise it could swallow real code that
%% follows and mask a second statement through a different path.
extract_assignment_line_comment_with_quote_test() ->
    ?assertEqual(
        none,
        beamtalk_repl_eval:extract_assignment(
            "alpha := 1 // says \"hi\" here\nbeta := 2"
        )
    ),
    %% A single statement with a trailing `//` comment containing a quote is
    %% unaffected — no false bail.
    ?assertEqual(
        {ok, alpha},
        beamtalk_repl_eval:extract_assignment("alpha := 1 // says \"hi\" here")
    ).

%% BT-3372: same shape, but with a `/* ... */` block comment instead of a
%% `//` line comment — an unbalanced brace inside the block comment must
%% not be read as real code either.
extract_assignment_block_comment_unbalanced_brace_test() ->
    ?assertEqual(
        none,
        beamtalk_repl_eval:extract_assignment("alpha := 1 /* { unbalanced */\nbeta := 2")
    ),
    %% A single statement with a block comment on the same line is
    %% unaffected — no false bail.
    ?assertEqual(
        {ok, alpha},
        beamtalk_repl_eval:extract_assignment("alpha := 1 /* note */")
    ).

%% BT-3372 (CLAUDE.md Essential Rules: a "mirrors" claim across the
%% Rust/Erlang boundary needs a shared conformance fixture, not just a
%% comment): `skip_line_comment/1`/`skip_block_comment/1` are hand-rolled
%% Erlang mirrors of the Rust lexer's `lex_line_comment/0`/
%% `lex_block_comment/0` span computation. Both sides run the exact same
%% cases from `test/fixtures/comment_span_corpus.json` — see
%% `source_analysis::lexer::tests::comment_span_matches_shared_corpus` for
%% the Rust side.
comment_span_matches_shared_corpus_test() ->
    Path = filename:join([
        code:lib_dir(beamtalk_workspace),
        "test",
        "fixtures",
        "comment_span_corpus.json"
    ]),
    {ok, Raw} = file:read_file(Path),
    Cases = json:decode(Raw),
    ?assert(length(Cases) > 0),
    lists:foreach(fun assert_comment_span_case/1, Cases).

assert_comment_span_case(#{
    <<"kind">> := Kind,
    <<"source">> := SourceBin,
    <<"expected_end">> := ExpectedEnd,
    <<"why">> := Why
}) ->
    Source = unicode:characters_to_list(SourceBin),
    Remaining =
        case Kind of
            <<"line_comment">> ->
                beamtalk_repl_eval:skip_line_comment(Source);
            <<"block_comment">> ->
                beamtalk_repl_eval:skip_block_comment(Source)
        end,
    Consumed = length(Source) - length(Remaining),
    ?assertEqual(
        ExpectedEnd,
        Consumed,
        lists:flatten(io_lib:format("span-end mismatch for ~p (~ts)", [Source, Why]))
    ).

%% ===================================================================
%% compile_expression_via_port catch clauses (BT-627)
%% ===================================================================

compile_expr_noproc_test() ->
    %% Covers exit:{noproc, _} clause (line 341-342)
    Result = beamtalk_repl_compiler:compile_expression_via_port("1+2", test_mod, #{}),
    ?assertMatch({error, _}, Result).

compile_expr_noproc_with_env_test() ->
    %% compile_expression_via_port calls beamtalk_compiler which isn't started,
    %% so it hits exit:{noproc, _} rather than the timeout path.
    %% This test verifies the function handles a missing compiler gracefully.
    Result = beamtalk_repl_compiler:compile_expression_via_port("hello", test_mod2, #{x => 1}),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% compile_file_via_port catch clauses (BT-627)
%% ===================================================================

compile_file_noproc_test() ->
    %% Covers exit:{noproc, _} clause (line 380-381)
    Result = beamtalk_repl_compiler:compile_file_via_port("x := 1", "/test.bt", false, undefined),
    ?assertMatch({error, _}, Result).

compile_file_noproc_stdlib_test() ->
    %% Covers stdlib_mode path too
    Result = beamtalk_repl_compiler:compile_file_via_port(
        "Object subclass: Foo", "/stdlib/src/Foo.bt", true, undefined
    ),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% to_snake_case (BT-775)
%% ===================================================================

to_snake_case_simple_test() ->
    ?assertEqual("counter", beamtalk_repl_loader:to_snake_case("counter")).

to_snake_case_camel_test() ->
    ?assertEqual("counter", beamtalk_repl_loader:to_snake_case("Counter")).

to_snake_case_multi_word_test() ->
    ?assertEqual("scheme_symbol", beamtalk_repl_loader:to_snake_case("SchemeSymbol")).

to_snake_case_three_words_test() ->
    ?assertEqual("my_counter_actor", beamtalk_repl_loader:to_snake_case("MyCounterActor")).

to_snake_case_acronym_test() ->
    %% Acronyms: no underscores within consecutive uppercase
    ?assertEqual("httprouter", beamtalk_repl_loader:to_snake_case("HTTPRouter")).

to_snake_case_already_snake_test() ->
    ?assertEqual("already_snake", beamtalk_repl_loader:to_snake_case("already_snake")).

to_snake_case_empty_test() ->
    ?assertEqual([], beamtalk_repl_loader:to_snake_case([])).

to_snake_case_with_digits_test() ->
    ?assertEqual("app2", beamtalk_repl_loader:to_snake_case("App2")).

%% ===================================================================
%% handle_class_definition (BT-627)
%% ===================================================================

handle_class_definition_load_error_test() ->
    %% Test the {error, Reason} branch of code:load_binary (line 226-227)
    ClassInfo = #{
        binary => <<"not_a_valid_beam">>,
        module_name => '__bt_test_bad_class',
        classes => [#{name => <<"BadClass">>}]
    },
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_class_definition(
        ClassInfo, [], "test", #{}, State, undefined, undefined
    ),
    ?assertMatch({error, #beamtalk_error{}, <<>>, [], _}, Result).

handle_class_definition_empty_classes_test() ->
    %% Test fallback branch when Classes is empty (lines 219-223)
    %% Create a minimal valid BEAM module to load
    %% We can't easily create valid BEAM, so test with invalid binary
    ClassInfo = #{
        binary => <<"bad">>,
        module_name => '__bt_test_empty_cls',
        classes => []
    },
    State = beamtalk_repl_state:new(undefined, 0),
    %% Load will fail, hitting the error branch
    Result = beamtalk_repl_eval:handle_class_definition(
        ClassInfo, [<<"warn">>], "test", #{}, State, undefined, undefined
    ),
    ?assertMatch({error, #beamtalk_error{}, <<>>, [<<"warn">>], _}, Result).

%% ===================================================================
%% handle_method_definition (BT-627)
%% ===================================================================

handle_method_definition_no_source_test() ->
    %% Test the 'undefined' branch when class has no stored source (line 237-240)
    MethodInfo = #{class_name => <<"NonexistentClass">>, selector => <<"foo">>},
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_method_definition(MethodInfo, [], "foo [] := 42", State),
    ?assertMatch({error, {compile_error, _}, <<>>, [], _}, Result).

handle_method_definition_no_source_with_warnings_test() ->
    %% Same but with warnings to verify they pass through
    MethodInfo = #{class_name => <<"Missing">>, selector => <<"bar">>},
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:handle_method_definition(MethodInfo, [<<"w1">>], "bar", State),
    ?assertMatch({error, {compile_error, _}, <<>>, [<<"w1">>], _}, Result).

handle_method_definition_with_source_compile_fail_test() ->
    %% Test the path where class source exists but recompilation fails.
    %% BT-911: compile_for_method_reload wraps compiler exits — must return {error, ...},
    %% never propagate as an exit that would kill the REPL process.
    %% BT-1174: class source is now stored in workspace_meta.
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, WsPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"eval_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second)
    }),
    ok = beamtalk_workspace_meta:set_class_source(<<"TestClass">>, "Object subclass: TestClass"),
    State = beamtalk_repl_state:new(undefined, 0),
    MethodInfo = #{class_name => <<"TestClass">>, selector => <<"doStuff">>},
    Result = beamtalk_repl_eval:handle_method_definition(
        MethodInfo, [], "doStuff [] := 42", State
    ),
    gen_server:stop(WsPid),
    ?assertMatch({error, {compile_error, _}, <<>>, [], _}, Result).

%% ===================================================================
%% maybe_await_future timeout and flush paths (BT-627)
%% ===================================================================

maybe_await_future_non_future_pid_v2_test() ->
    %% Test that a non-future PID (e.g., a plain process) returns the PID as-is
    %% Covers lines 518-530 (after timeout, flush, return Value)
    Pid = spawn(fun() ->
        receive
            stop -> ok
        after 5000 -> ok
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future(Pid),
    ?assertEqual(Pid, Result),
    Pid ! stop.

maybe_await_future_dead_pid_test() ->
    %% Test with a PID that's already dead - covers the after clause
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    Result = beamtalk_repl_eval:maybe_await_future(Pid),
    ?assertEqual(Pid, Result).

maybe_await_future_resolved_test() ->
    %% Test with a fake future that responds to the await protocol (tagged tuple)
    %% Covers lines 498-500 (future_resolved path)
    Pid = spawn(fun() ->
        receive
            {await, From, _Timeout} ->
                From ! {future_resolved, self(), 42}
        end,
        %% Keep alive briefly
        receive
            stop -> ok
        after 1000 -> ok
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future({beamtalk_future, Pid}),
    ?assertEqual(42, Result),
    Pid ! stop.

maybe_await_future_rejected_test() ->
    %% Test with a fake future that sends future_rejected (tagged tuple)
    %% Covers lines 501-505 (future_rejected path)
    Pid = spawn(fun() ->
        receive
            {await, From, _Timeout} ->
                From ! {future_rejected, self(), some_error}
        end,
        receive
            stop -> ok
        after 1000 -> ok
        end
    end),
    Result = beamtalk_repl_eval:maybe_await_future({beamtalk_future, Pid}),
    ?assertEqual({future_rejected, some_error}, Result),
    Pid ! stop.

maybe_await_future_beamtalk_object_v2_test() ->
    %% Test with a beamtalk_object tuple (line 532-535)
    Obj = {beamtalk_object, self(), counter, #{}},
    ?assertEqual(Obj, beamtalk_repl_eval:maybe_await_future(Obj)).

%% ===================================================================
%% IO handling edge cases (BT-627)
%% ===================================================================

handle_io_request_put_chars_legacy_v2_test() ->
    %% Test the {put_chars, Chars} form without encoding (lines 668-675)
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request({put_chars, "hello"}, <<>>),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"hello">>, Buffer).

handle_io_request_put_chars_legacy_binary_test() ->
    %% Test with binary input
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request({put_chars, <<"world">>}, <<"hi ">>),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"hi world">>, Buffer).

handle_io_request_put_chars_mfa_v2_test() ->
    %% Test {put_chars, Encoding, Mod, Func, Args} form (lines 676-679)
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, unicode, io_lib, format, ["~p", [42]]}, <<>>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"42">>, Buffer).

handle_io_request_put_chars_mfa_error_v2_test() ->
    %% Test with an MFA that crashes - covers catch clause (lines 680-681)
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, unicode, erlang, error, [badarg]}, <<"existing">>
    ),
    ?assertEqual(ok, Reply),
    ?assertEqual(<<"existing">>, Buffer).

handle_io_request_unknown_test() ->
    %% Test unknown IO request type - covers catch-all (line 685)
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {get_until, prompt, mod, func, []}, <<>>
    ),
    ?assertEqual({error, enotsup}, Reply),
    ?assertEqual(<<>>, Buffer).

handle_io_request_put_chars_bad_encoding_test() ->
    %% Test put_chars with data that fails unicode conversion
    %% Covers the catch clause in handle_io_request (line 666)
    {Reply, Buffer} = beamtalk_io_capture:handle_io_request(
        {put_chars, utf32, <<255, 254, 0, 0>>}, <<"prev">>
    ),
    ?assertEqual(ok, Reply),
    %% Buffer should remain unchanged on encoding error
    ?assertEqual(<<"prev">>, Buffer).

%% ===================================================================
%% reset_captured_group_leaders (BT-627)
%% ===================================================================

reset_captured_group_leaders_no_matches_test() ->
    %% Test with a capture PID that no process has as group_leader
    FakePid = spawn(fun() ->
        receive
            stop -> ok
        after 5000 -> ok
        end
    end),
    OldGL = group_leader(),
    ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(FakePid, OldGL)),
    FakePid ! stop.

%% ===================================================================
%% IO capture full lifecycle (BT-627)
%% ===================================================================

io_capture_with_output_test() ->
    %% Test full IO capture lifecycle covering start/stop paths
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    ?assert(is_pid(CapturePid)),
    %% Write some output via io:format which goes through group_leader
    io:format("hello ~s", ["world"]),
    Output = beamtalk_io_capture:stop({CapturePid, OldGL}),
    ?assertEqual(<<"hello world">>, Output).

io_capture_dead_capture_pid_test() ->
    %% Test stop_io_capture when capture process already died (line 600-601)
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(50),
    OldGL = group_leader(),
    Output = beamtalk_io_capture:stop({DeadPid, OldGL}),
    ?assertEqual(<<>>, Output).

%% ===================================================================
%% trigger_hot_reload with instances (BT-627)
%% ===================================================================

trigger_hot_reload_with_list_name_test() ->
    %% Test the is_list(N) branch in trigger_hot_reload (line 455-457)
    %% Use a class name that doesn't exist as an atom to hit the badarg catch
    Classes = [#{name => "nonexistent_class_xyz_12345"}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_mod, Classes)).

trigger_hot_reload_undefined_name_v2_test() ->
    %% Test the undefined name branch (line 459)
    Classes = [#{name => undefined}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_mod, Classes)).

trigger_hot_reload_no_name_key_v2_test() ->
    %% Test when name key is missing (maps:get returns undefined)
    Classes = [#{}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_mod, Classes)).

trigger_hot_reload_atom_name_v2_test() ->
    %% Test the is_atom(N) branch (line 454)
    %% Use an atom that exists but has no instances
    Classes = [#{name => test_atom_class}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_mod, Classes)).

%% ===================================================================
%% is_stdlib_path edge cases (BT-627)
%% ===================================================================

is_stdlib_path_abs_v2_test() ->
    ?assertEqual(
        true, beamtalk_repl_loader:is_stdlib_path("/home/user/project/stdlib/src/integer.bt")
    ).

is_stdlib_path_not_stdlib_test() ->
    ?assertEqual(false, beamtalk_repl_loader:is_stdlib_path("/home/user/src/main.bt")).

is_stdlib_path_rel_lib_v2_test() ->
    ?assertEqual(true, beamtalk_repl_loader:is_stdlib_path("stdlib/src/string.bt")).

%% ===================================================================
%% should_purge_module edge cases (BT-627)
%% ===================================================================

should_purge_module_with_registry_no_actors_test() ->
    %% Test with a live registry that returns empty actors
    {ok, Pid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    ?assertEqual(true, beamtalk_repl_eval:should_purge_module(some_module, Pid)),
    gen_server:stop(Pid).

%% ===================================================================
%% Stdin request detection tests (BT-698)
%% ===================================================================

is_stdin_request_get_line_with_encoding_test() ->
    ?assertMatch(
        {true, <<"Name: ">>},
        beamtalk_io_capture:is_stdin_request({get_line, unicode, <<"Name: ">>})
    ).

is_stdin_request_get_line_without_encoding_test() ->
    ?assertMatch(
        {true, <<"Enter: ">>},
        beamtalk_io_capture:is_stdin_request({get_line, <<"Enter: ">>})
    ).

is_stdin_request_get_line_list_prompt_test() ->
    ?assertMatch(
        {true, <<"Name: ">>},
        beamtalk_io_capture:is_stdin_request({get_line, unicode, "Name: "})
    ).

is_stdin_request_get_chars_test() ->
    ?assertMatch(
        {true, <<"Prompt">>},
        beamtalk_io_capture:is_stdin_request({get_chars, unicode, <<"Prompt">>, 5})
    ).

is_stdin_request_get_until_test() ->
    ?assertMatch(
        {true, <<"? ">>},
        beamtalk_io_capture:is_stdin_request(
            {get_until, unicode, <<"? ">>, io_lib, collect_line, []}
        )
    ).

is_stdin_request_put_chars_test() ->
    ?assertEqual(false, beamtalk_io_capture:is_stdin_request({put_chars, unicode, <<"hello">>})).

is_stdin_request_other_test() ->
    ?assertEqual(false, beamtalk_io_capture:is_stdin_request({some_other_request})).

%% ===================================================================
%% Stdin request handling tests (BT-698)
%% ===================================================================

handle_stdin_request_no_subscriber_test() ->
    %% Without subscriber, stdin returns enotsup
    ?assertEqual({error, enotsup}, beamtalk_io_capture:handle_stdin_request(undefined, <<"? ">>)).

handle_stdin_request_with_subscriber_test() ->
    %% Test stdin handling with a subscriber that provides input
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {need_input, CapturePid, Ref, <<"Name: ">>} ->
                CapturePid ! {stdin_input, Ref, <<"Alice\n">>},
                Self ! subscriber_done
        end
    end),
    Result = beamtalk_io_capture:handle_stdin_request(Subscriber, <<"Name: ">>),
    ?assertEqual(<<"Alice\n">>, Result),
    receive
        subscriber_done -> ok
    after 1000 -> ?assert(false)
    end.

handle_stdin_request_eof_test() ->
    %% Test stdin EOF handling
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {need_input, CapturePid, Ref, _Prompt} ->
                CapturePid ! {stdin_input, Ref, eof},
                Self ! subscriber_done
        end
    end),
    Result = beamtalk_io_capture:handle_stdin_request(Subscriber, <<"? ">>),
    ?assertEqual(eof, Result),
    receive
        subscriber_done -> ok
    after 1000 -> ?assert(false)
    end.

%% ===================================================================
%% Prompt conversion tests (BT-698)
%% ===================================================================

prompt_to_binary_binary_test() ->
    ?assertEqual(<<"hello">>, beamtalk_io_capture:prompt_to_binary(<<"hello">>)).

prompt_to_binary_list_test() ->
    ?assertEqual(<<"hello">>, beamtalk_io_capture:prompt_to_binary("hello")).

prompt_to_binary_atom_test() ->
    ?assertEqual(<<"ok">>, beamtalk_io_capture:prompt_to_binary(ok)).

prompt_to_binary_other_test() ->
    ?assertEqual(<<"? ">>, beamtalk_io_capture:prompt_to_binary(42)).

%% ===================================================================
%% IO capture loop stdin integration tests (BT-698)
%% ===================================================================

io_capture_stdin_with_subscriber_test() ->
    %% Test that io_capture_loop handles get_line by notifying subscriber
    Self = self(),
    %% Use self() as the subscriber — we'll handle the need_input message
    {CapturePid, OldGL} = beamtalk_io_capture:start(Self),
    %% Send a get_line request to the IO capture process
    ReplyRef = make_ref(),
    CapturePid ! {io_request, self(), ReplyRef, {get_line, unicode, <<"Enter: ">>}},
    %% Receive the need_input request from the IO capture process
    receive
        {need_input, IoCapPid, Ref, <<"Enter: ">>} ->
            %% Provide stdin input with matching ref
            IoCapPid ! {stdin_input, Ref, <<"test input\n">>}
    after 5000 ->
        ?assert(false)
    end,
    %% Receive the io_reply
    receive
        {io_reply, ReplyRef, Reply} ->
            ?assertEqual(<<"test input\n">>, Reply)
    after 5000 ->
        ?assert(false)
    end,
    _Output = beamtalk_io_capture:stop({CapturePid, OldGL}).

io_capture_stdin_stale_ref_ignored_test() ->
    %% A stdin_input with a wrong ref is ignored; the correct ref is accepted
    Self = self(),
    {CapturePid, OldGL} = beamtalk_io_capture:start(Self),
    ReplyRef = make_ref(),
    CapturePid ! {io_request, self(), ReplyRef, {get_line, unicode, <<"Enter: ">>}},
    receive
        {need_input, IoCapPid, Ref, <<"Enter: ">>} ->
            %% Send a stale/wrong ref first — should be ignored
            StaleRef = make_ref(),
            IoCapPid ! {stdin_input, StaleRef, <<"stale data\n">>},
            %% Then send the correct ref
            IoCapPid ! {stdin_input, Ref, <<"correct data\n">>}
    after 5000 ->
        ?assert(false)
    end,
    receive
        {io_reply, ReplyRef, Reply} ->
            ?assertEqual(<<"correct data\n">>, Reply)
    after 5000 ->
        ?assert(false)
    end,
    _Output = beamtalk_io_capture:stop({CapturePid, OldGL}).

io_capture_stdin_no_subscriber_test() ->
    %% Without subscriber, get_line returns {error, enotsup}
    {CapturePid, OldGL} = beamtalk_io_capture:start(),
    ReplyRef = make_ref(),
    CapturePid ! {io_request, self(), ReplyRef, {get_line, unicode, <<"? ">>}},
    receive
        {io_reply, ReplyRef, Reply} ->
            ?assertEqual({error, enotsup}, Reply)
    after 5000 ->
        ?assert(false)
    end,
    _Output = beamtalk_io_capture:stop({CapturePid, OldGL}).

%% ===================================================================
%% reload_class_file (BT-897, BT-868)
%% ===================================================================

reload_class_file_not_found_test() ->
    %% Non-existent file returns file_not_found
    Result = beamtalk_repl_eval:reload_class_file("/nonexistent/file.bt", 'SomeClass'),
    ?assertEqual({error, {file_not_found, "/nonexistent/file.bt"}}, Result).

reload_class_file_no_compiler_test() ->
    %% BT-897: reload_class_file with a real file but no compiler available.
    %% This exercises the code path that now includes compute_package_module_name.
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(
        temp_dir(),
        io_lib:format("test_reload_~p.bt", [UniqueId])
    ),
    ok = file:write_file(TempFile, <<"Actor subclass: TestActor [\n]\n">>),
    Result = beamtalk_repl_eval:reload_class_file(TempFile, 'TestActor'),
    file:delete(TempFile),
    %% Compiler not started — should fail gracefully
    case Result of
        {error, _} -> ok;
        Other -> error({unexpected_result, Other})
    end.

%% ===================================================================
%% verify_class_present (BT-868)
%% ===================================================================

verify_class_present_undefined_skips_check_test() ->
    %% undefined means no verification needed (e.g., handle_load path)
    ?assertEqual(
        ok,
        beamtalk_repl_loader:verify_class_present(
            undefined, [#{name => "Foo"}], "/some/path.bt"
        )
    ).

verify_class_present_found_test() ->
    ClassNames = [#{name => "Counter"}, #{name => "Timer"}],
    ?assertEqual(
        ok,
        beamtalk_repl_loader:verify_class_present(
            'Counter', ClassNames, "/some/path.bt"
        )
    ).

verify_class_present_not_found_test() ->
    ClassNames = [#{name => "OtherClass"}],
    Result = beamtalk_repl_loader:verify_class_present(
        'Counter', ClassNames, "/some/path.bt"
    ),
    ?assertEqual(
        {error, {class_not_found, 'Counter', "/some/path.bt", ["OtherClass"]}},
        Result
    ).

verify_class_present_empty_classes_test() ->
    Result = beamtalk_repl_loader:verify_class_present(
        'Counter', [], "/some/path.bt"
    ),
    ?assertEqual(
        {error, {class_not_found, 'Counter', "/some/path.bt", []}},
        Result
    ).

%%% do_eval_trace tests (BT-1238)

do_eval_trace_increments_counter_test() ->
    %% do_eval_trace should increment the eval counter even on compile error
    State = beamtalk_repl_state:new(undefined, 0),
    InitialCounter = beamtalk_repl_state:get_eval_counter(State),
    {error, _, _, _, NewState} = beamtalk_repl_eval:do_eval_trace("1 + 2", State),
    ?assertEqual(InitialCounter + 1, beamtalk_repl_state:get_eval_counter(NewState)).

do_eval_trace_compile_error_without_server_test() ->
    %% Without compiler server, compile_expression_trace returns a structured compile_error
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_eval_trace("1 + 2", State),
    ?assertMatch({error, #beamtalk_error{kind = compile_error}, _, _, _}, Result).

do_eval_trace_compile_error_includes_empty_output_test() ->
    %% Output should be <<>> when compilation fails before IO capture starts
    State = beamtalk_repl_state:new(undefined, 0),
    {error, _, Output, _, _} = beamtalk_repl_eval:do_eval_trace("1 + 2", State),
    ?assertEqual(<<>>, Output).

do_eval_trace_compile_error_includes_empty_warnings_test() ->
    %% Warnings should be [] when compilation fails
    State = beamtalk_repl_state:new(undefined, 0),
    {error, _, _, Warnings, _} = beamtalk_repl_eval:do_eval_trace("1 + 2", State),
    ?assertEqual([], Warnings).

do_eval_trace_preserves_existing_bindings_on_error_test() ->
    %% Existing bindings must be preserved when trace eval fails
    State = beamtalk_repl_state:new(undefined, 0),
    Bindings = #{x => 10, y => 20},
    StateWithBindings = beamtalk_repl_state:set_bindings(Bindings, State),
    {error, _, _, _, NewState} = beamtalk_repl_eval:do_eval_trace(
        "z := 99", StateWithBindings
    ),
    FinalBindings = beamtalk_repl_state:get_bindings(NewState),
    ?assertEqual(10, maps:get(x, FinalBindings)),
    ?assertEqual(20, maps:get(y, FinalBindings)),
    ?assertEqual(false, maps:is_key(z, FinalBindings)).

%%====================================================================
%% Protocol definition error handling tests (BT-1616)
%%====================================================================

-doc "Test that code:load_binary failure returns a structured #beamtalk_error{}.".
handle_protocol_definition_load_failure_returns_structured_error_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ProtocolInfo = #{
        binary => <<"not valid beam binary">>,
        module_name => '__bt_test_bad_protocol',
        protocols => [<<"BadProto">>]
    },
    Result = beamtalk_repl_eval:handle_protocol_definition(ProtocolInfo, [], State),
    ?assertMatch({error, #beamtalk_error{}, <<>>, [], _}, Result),
    {error, Err, _, _, _} = Result,
    ?assertEqual(io_error, Err#beamtalk_error.kind).

-doc "Test that register_class/0 failure surfaces as a structured #beamtalk_error{}.".
handle_protocol_definition_register_class_failure_returns_structured_error_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    %% Dynamically compile a module whose register_class/0 throws an error
    ModuleName = '__bt_test_failing_register_protocol',
    Forms = [
        {attribute, 1, module, ModuleName},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [
                {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [
                    {atom, 3, registration_boom}
                ]}
            ]}
        ]}
    ],
    {ok, ModuleName, Binary} = compile:forms(Forms),
    ProtocolInfo = #{
        binary => Binary,
        module_name => ModuleName,
        protocols => [<<"FailProto">>]
    },
    Result = beamtalk_repl_eval:handle_protocol_definition(ProtocolInfo, [], State),
    ?assertMatch({error, #beamtalk_error{}, <<>>, [], _}, Result),
    {error, Err, _, _, _} = Result,
    ?assertEqual(registration_error, Err#beamtalk_error.kind),
    %% Verify the error message mentions the module and reason
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"registration failed">>)),
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"registration_boom">>)),
    %% Cleanup
    code:purge(ModuleName),
    code:delete(ModuleName).

-doc """
BT-3337: `register_class/0` RETURNING `{error, Reason}` (rather than
throwing) is a distinct branch from the exception path above — same
structured-error contract, no exception ever raised.
""".
handle_protocol_definition_register_class_returns_error_tuple_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ModuleName = '__bt_test_error_return_register_protocol',
    Forms = [
        {attribute, 1, module, ModuleName},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [
                {tuple, 3, [{atom, 3, error}, {atom, 3, registration_declined}]}
            ]}
        ]}
    ],
    {ok, ModuleName, Binary} = compile:forms(Forms),
    ProtocolInfo = #{
        binary => Binary,
        module_name => ModuleName,
        protocols => [<<"DeclineProto">>]
    },
    Result = beamtalk_repl_eval:handle_protocol_definition(ProtocolInfo, [], State),
    ?assertMatch({error, #beamtalk_error{}, <<>>, [], _}, Result),
    {error, Err, _, _, _} = Result,
    ?assertEqual(registration_error, Err#beamtalk_error.kind),
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"registration_declined">>)),
    code:purge(ModuleName),
    code:delete(ModuleName).

-doc "Test that successful protocol definition calls register_class/0.".
handle_protocol_definition_success_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    %% Dynamically compile a module whose register_class/0 sets a process flag
    %% to prove it was actually called (not just skipped).
    ModuleName = '__bt_test_good_protocol',
    %% register_class/0 sets a process dict flag to prove it ran
    Forms = [
        {attribute, 1, module, ModuleName},
        {attribute, 2, export, [{register_class, 0}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [
                {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, put}}, [
                    {atom, 3, '__bt_test_register_called'}, {atom, 3, true}
                ]}
            ]}
        ]}
    ],
    {ok, ModuleName, Binary} = compile:forms(Forms),
    %% Clear the flag before the test
    erlang:erase('__bt_test_register_called'),
    ProtocolInfo = #{
        binary => Binary,
        module_name => ModuleName,
        protocols => [<<"GoodProto">>]
    },
    Result = beamtalk_repl_eval:handle_protocol_definition(ProtocolInfo, [], State),
    ?assertMatch({ok, <<"Protocol GoodProto defined">>, <<>>, [], _}, Result),
    %% Verify register_class/0 was actually called
    ?assertEqual(true, erlang:get('__bt_test_register_called')),
    %% Cleanup
    erlang:erase('__bt_test_register_called'),
    code:purge(ModuleName),
    code:delete(ModuleName).

-doc "Test protocol definition success when register_class/0 is not exported.".
handle_protocol_definition_no_register_class_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    %% Dynamically compile a module without register_class/0
    ModuleName = '__bt_test_no_register_protocol',
    Forms = [
        {attribute, 1, module, ModuleName},
        {attribute, 2, export, []}
    ],
    {ok, ModuleName, Binary} = compile:forms(Forms),
    ProtocolInfo = #{
        binary => Binary,
        module_name => ModuleName,
        protocols => [<<"NoRegProto">>]
    },
    Result = beamtalk_repl_eval:handle_protocol_definition(ProtocolInfo, [], State),
    ?assertMatch({ok, <<"Protocol NoRegProto defined">>, <<>>, [], _}, Result),
    %% Cleanup
    code:purge(ModuleName),
    code:delete(ModuleName).

%%====================================================================
%% Type alias definition tests (ADR 0108 Phase 8, BT-2902)
%%====================================================================

-doc "handle_type_alias_definition/3 registers the alias and echoes its name.".
handle_type_alias_definition_success_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    AliasInfo = #{
        alias_name => <<"Direction">>,
        expansion => <<"#north | #south | #east | #west">>,
        doc_comment => undefined
    },
    Result = beamtalk_repl_eval:handle_type_alias_definition(AliasInfo, [], State),
    ?assertMatch({ok, <<"Direction">>, <<>>, [], _}, Result),
    {ok, _, _, _, NewState} = Result,
    ?assertEqual(
        #{
            expansion => <<"#north | #south | #east | #west">>,
            doc_comment => undefined,
            declared_in => <<"REPL">>
        },
        maps:get(<<"Direction">>, beamtalk_repl_state:get_alias_table(NewState))
    ).

-doc "handle_type_alias_definition/3 overwrites an existing entry on redefinition.".
handle_type_alias_definition_redefine_overwrites_test() ->
    State0 = beamtalk_repl_state:new(undefined, 0),
    First = #{
        alias_name => <<"Direction">>,
        expansion => <<"#north | #south">>,
        doc_comment => undefined
    },
    {ok, _, _, _, State1} = beamtalk_repl_eval:handle_type_alias_definition(First, [], State0),
    Second = #{
        alias_name => <<"Direction">>,
        expansion => <<"#north | #south | #east | #west">>,
        doc_comment => undefined
    },
    {ok, _, _, _, State2} = beamtalk_repl_eval:handle_type_alias_definition(Second, [], State1),
    ?assertEqual(
        #{
            expansion => <<"#north | #south | #east | #west">>,
            doc_comment => undefined,
            declared_in => <<"REPL">>
        },
        maps:get(<<"Direction">>, beamtalk_repl_state:get_alias_table(State2))
    ).

-doc "format_alias_help/2 omits the comment block when there is no doc comment.".
format_alias_help_without_doc_comment_test() ->
    Entry = #{
        expansion => <<"#north | #south | #east | #west">>,
        doc_comment => undefined,
        declared_in => <<"REPL">>
    },
    Result = beamtalk_repl_eval:format_alias_help(<<"Direction">>, Entry),
    ?assertEqual(
        <<"type Direction = #north | #south | #east | #west\n\nDeclared in: REPL">>,
        Result
    ).

-doc "format_alias_help/2 renders the indented doc comment block when present.".
format_alias_help_with_doc_comment_test() ->
    Entry = #{
        expansion => <<"#temporary | #transient | #permanent">>,
        doc_comment => <<"How a supervised child restarts after exit.">>,
        declared_in => <<"REPL">>
    },
    Result = beamtalk_repl_eval:format_alias_help(<<"RestartStrategy">>, Entry),
    ?assertEqual(
        <<
            "type RestartStrategy = #temporary | #transient | #permanent\n\n"
            "  How a supervised child restarts after exit.\n\n"
            "Declared in: REPL"
        >>,
        Result
    ).

-doc "format_alias_help/2 renders the stdlib provenance line (BT-2938).".
format_alias_help_stdlib_declared_in_test() ->
    Entry = #{
        expansion => <<"#oneForOne | #oneForAll | #restForOne">>,
        doc_comment => undefined,
        declared_in => <<"stdlib">>
    },
    Result = beamtalk_repl_eval:format_alias_help(<<"SupervisionStrategy">>, Entry),
    ?assertEqual(
        <<"type SupervisionStrategy = #oneForOne | #oneForAll | #restForOne\n\nDeclared in: stdlib">>,
        Result
    ).

-doc "maybe_help_for_alias/2 answers a bare `Beamtalk help: <Alias>` for a known alias.".
maybe_help_for_alias_found_test() ->
    State0 = beamtalk_repl_state:new(undefined, 0),
    Entry = #{
        expansion => <<"#north | #south">>, doc_comment => undefined, declared_in => <<"REPL">>
    },
    State = beamtalk_repl_state:put_alias(<<"Direction">>, Entry, State0),
    Result = beamtalk_repl_eval:maybe_help_for_alias("Beamtalk help: Direction", State),
    ?assertEqual(
        {ok, <<"type Direction = #north | #south\n\nDeclared in: REPL">>}, Result
    ).

-doc "maybe_help_for_alias/2 falls through for a name that is not a session alias.".
maybe_help_for_alias_not_an_alias_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(
        not_found, beamtalk_repl_eval:maybe_help_for_alias("Beamtalk help: Integer", State)
    ).

-doc """
maybe_help_for_alias/2 does not intercept the `selector:`/`class` forms
`:help` also builds, even when the receiver names a known alias (aliases
have no methods to introspect — falls through to the ordinary eval path,
which will report the usual does-not-understand/not-found error).
""".
maybe_help_for_alias_ignores_selector_and_class_forms_test() ->
    State0 = beamtalk_repl_state:new(undefined, 0),
    Entry = #{
        expansion => <<"#north | #south">>, doc_comment => undefined, declared_in => <<"REPL">>
    },
    State = beamtalk_repl_state:put_alias(<<"Direction">>, Entry, State0),
    ?assertEqual(
        not_found,
        beamtalk_repl_eval:maybe_help_for_alias("Beamtalk help: Direction selector: #foo", State)
    ),
    ?assertEqual(
        not_found,
        beamtalk_repl_eval:maybe_help_for_alias("Beamtalk help: Direction class", State)
    ).

%%====================================================================
%% Success-path tests (require the beamtalk_compiler + beamtalk_runtime apps)
%%
%% The tests above exercise pure helpers and the compile-error path that
%% occurs when no compiler is running. These tests start the compiler port
%% and the runtime so do_eval/do_eval_trace/do_show_codegen and the
%% class/protocol/method definition handlers reach their success branches:
%% compile -> load_binary -> eval_loaded_module -> execute_and_process.
%%====================================================================

eval_setup() ->
    application:ensure_all_started(compiler),
    application:ensure_all_started(beamtalk_runtime),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Allow the runtime to register its bootstrap classes before compiling.
    timer:sleep(300),
    %% BT-3337: a live beamtalk_workspace_meta is needed by reload_file/1,
    %% precheck_method/4, remove_method/3,4 (non-stdlib), and a standalone
    %% method definition reached through do_eval/2 — all recompile from the
    %% class source workspace_meta records, same fixture pattern as
    %% handle_method_definition_with_source_compile_fail_test/0 above.
    %% `repl => false` avoids the disk-persistence side effect
    %% beamtalk_repl_loader_precheck_tests.erl's fixture also steers clear of.
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid)
    end,
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"eval_success_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    %% precheck_method/4's signature-diff baseline (previous/3) needs a live
    %% store — without it every diff exits `noproc` (BT-3337).
    case whereis(beamtalk_workspace_signature_store) of
        undefined -> {ok, _} = beamtalk_workspace_signature_store:start_link();
        _ -> ok
    end,
    ok.

eval_teardown(_) ->
    %% Stop the compiler app so this module's no-compiler / noproc error-path
    %% tests (and later test modules in the shared EUnit node) see the baseline
    %% "compiler not running" state this fixture started from.
    _ = application:stop(beamtalk_compiler),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    case whereis(beamtalk_workspace_signature_store) of
        undefined -> ok;
        SigStorePid -> gen_server:stop(SigStorePid)
    end,
    ok.

%% BT-3337: seed workspace_meta with `ClassSource` under `ClassNameBin` so a
%% recompile-from-recorded-source path (reload_method_definition,
%% precheck_method, remove_method) has real source to work from — mirrors
%% what `Workspace load:` does for a file-backed class.
seed_class_source(ClassNameBin, ClassSource) ->
    ok = beamtalk_workspace_meta:set_class_source(ClassNameBin, ClassSource).

eval_success_test_() ->
    {setup, fun eval_setup/0, fun eval_teardown/1, [
        {"do_eval arithmetic returns value", fun do_eval_arithmetic_value/0},
        {"do_eval captures empty output", fun do_eval_output_is_binary/0},
        {"do_eval assignment binds variable", fun do_eval_assignment_binds/0},
        {"do_eval reads existing binding", fun do_eval_reads_binding/0},
        {"do_eval multi-statement returns last", fun do_eval_multi_statement/0},
        {"do_eval multi-statement newline-separated bindings (BT-3368)",
            fun do_eval_multi_statement_newline_separated_bindings/0},
        {"do_eval multi-statement newline-separated bindings, three vars (BT-3368)",
            fun do_eval_multi_statement_newline_separated_bindings_three/0},
        {"do_eval runtime error wraps in _error", fun do_eval_runtime_error/0},
        {"do_eval inline class definition", fun do_eval_class_definition/0},
        {"do_eval protocol definition", fun do_eval_protocol_definition/0},
        {"do_eval class over earlier-turn alias is a collision error",
            fun do_eval_class_definition_over_earlier_turn_alias_is_a_collision_error/0},
        {"do_eval/3 with undefined subscriber", fun do_eval_with_undefined_subscriber/0},
        {"do_show_codegen returns core erlang", fun do_show_codegen_success/0},
        {"do_show_codegen invalid returns error", fun do_show_codegen_error/0},
        {"do_eval_trace single statement", fun do_eval_trace_single/0},
        {"do_eval_trace multi statement", fun do_eval_trace_multi/0},
        {"do_eval_trace assignment rebuilds binding", fun do_eval_trace_assignment/0},
        {"do_eval_trace runtime error wraps", fun do_eval_trace_runtime_error/0},
        {"compile_method on unrecorded class returns error", fun compile_method_unrecorded/0},
        {"compile_method invalid body returns error", fun compile_method_invalid_body/0},
        {"compile_method non-method expression rejected", fun compile_method_not_a_method/0},
        {"do_show_codegen with binding known var", fun do_show_codegen_with_binding/0},
        {"reload_class_file/1 missing file", fun reload_class_file_arity1/0},
        {"handle_load/3 missing file delegates", fun handle_load3_missing/0},
        {"handle_load_source/3 invalid delegates", fun handle_load_source3_invalid/0},
        {"new_class/2 invalid delegates", fun new_class_invalid/0},
        %% BT-3337 — `:help <Alias>` short-circuit reached through do_eval/2.
        {"do_eval routes :help through the session alias table", fun do_eval_help_for_alias/0},
        %% BT-3337 — standalone `Class >> selector => body` method definition.
        {"do_eval standalone method definition reloads the target class",
            fun do_eval_standalone_method_definition/0},
        %% BT-3337 — a live actor registry pid threaded into eval bindings.
        {"do_eval threads a live actor registry pid into bindings",
            fun do_eval_with_actor_registry/0},
        %% BT-3337 — do_dispatch/5 (BT-2691 connected-mode entry dispatch).
        {"do_dispatch unary success sends to the class object", fun do_dispatch_unary_success/0},
        {"do_dispatch keyword success passes argv", fun do_dispatch_keyword_success/0},
        {"do_dispatch surfaces Program exit: as script_exit", fun do_dispatch_script_exit/0},
        {"do_dispatch wraps a runtime exception", fun do_dispatch_runtime_exception/0},
        {"do_dispatch: class atom exists but names no loaded class",
            fun do_dispatch_class_not_loaded/0},
        {"do_dispatch: unknown selector is a does_not_understand error",
            fun do_dispatch_unknown_selector/0},
        %% BT-3337 — eval_with_self/2 (ADR 0095 Inspector `evaluate:`).
        {"eval_with_self rejects a class definition",
            fun eval_with_self_rejects_class_definition/0},
        {"eval_with_self rejects a method definition",
            fun eval_with_self_rejects_method_definition/0},
        {"eval_with_self rejects a protocol definition",
            fun eval_with_self_rejects_protocol_definition/0},
        {"eval_with_self rejects a type alias definition",
            fun eval_with_self_rejects_type_alias_definition/0},
        {"eval_with_self evaluates an expression with self bound", fun eval_with_self_success/0},
        {"eval_with_self wraps a compile error", fun eval_with_self_compile_error/0},
        {"eval_with_self wraps a runtime exception", fun eval_with_self_runtime_exception/0},
        %% BT-3337 — precheck_method/4 (ADR 0105 Phase 3 precheck).
        {"precheck_method refuses a stdlib class", fun precheck_method_stdlib_refused/0},
        {"precheck_method delegates for a non-stdlib class", fun precheck_method_delegates/0},
        %% BT-3337 — remove_method/3,4 stdlib-policy branches.
        {"remove_method/3 refuses a stdlib class", fun remove_method3_stdlib_refused/0},
        {"remove_method/4 allow_stdlib reaches the loader", fun remove_method4_allow_stdlib/0},
        {"remove_method/4 refuse_stdlib with a non-stdlib class reaches the loader",
            fun remove_method4_refuse_stdlib_non_stdlib/0},
        %% BT-3337 — remove_class/1 both branches.
        {"remove_class/1 removes a live user class", fun remove_class_success/0},
        {"remove_class/1 unknown name is a structured error", fun remove_class_unknown/0},
        %% BT-3337 — thin forwarding wrappers over beamtalk_repl_loader.
        {"move_class/2 delegates to the loader", fun move_class_delegates/0},
        {"revert_remove_class/2 delegates to the loader", fun revert_remove_class_delegates/0},
        {"rewrite_sites/2 delegates to the loader", fun rewrite_sites_delegates/0},
        {"validate_sites/2 delegates to the loader", fun validate_sites_delegates/0},
        {"emit_remove_change_entry/5 delegates to the loader",
            fun emit_remove_change_entry_delegates/0},
        %% BT-3337 — reload_file/1 (BT-2598 disk-revert reload).
        {"reload_file/1 reloads a class and repopulates its source cache",
            fun reload_file_success/0}
    ]}.

state0() ->
    beamtalk_repl_state:new(undefined, 0).

do_eval_arithmetic_value() ->
    {ok, Value, Output, Warnings, _State} = beamtalk_repl_eval:do_eval("1 + 1", state0()),
    ?assertEqual(2, Value),
    ?assert(is_binary(Output)),
    ?assert(is_list(Warnings)).

do_eval_output_is_binary() ->
    %% A non-printing expression yields an empty captured output binary.
    {ok, _Value, Output, _Warnings, _State} = beamtalk_repl_eval:do_eval("3 * 7", state0()),
    ?assertEqual(<<>>, Output).

do_eval_assignment_binds() ->
    {ok, Value, _Output, _Warnings, State} =
        beamtalk_repl_eval:do_eval("answer := 40 + 2", state0()),
    ?assertEqual(42, Value),
    Bindings = beamtalk_repl_state:get_bindings(State),
    ?assertEqual(42, maps:get(answer, Bindings)).

do_eval_reads_binding() ->
    %% A previously-bound variable is visible to a subsequent eval.
    S0 = beamtalk_repl_state:set_bindings(#{base => 100}, state0()),
    {ok, Value, _Output, _Warnings, _State} = beamtalk_repl_eval:do_eval("base + 1", S0),
    ?assertEqual(101, Value).

do_eval_multi_statement() ->
    %% Multiple statements separated by `.` — result is the final expression.
    {ok, Value, _Output, _Warnings, _State} =
        beamtalk_repl_eval:do_eval("1 + 1. 2 + 2. 10 * 5", state0()),
    ?assertEqual(50, Value).

%% BT-3368: a multi-statement `eval` call (statements on separate lines, no
%% `.` separators) binds every variable to its OWN value — the first
%% variable must not be silently overwritten with the call's final value.
do_eval_multi_statement_newline_separated_bindings() ->
    {ok, Value, _Output, _Warnings, State} =
        beamtalk_repl_eval:do_eval("alpha := 111\nbeta := 222", state0()),
    ?assertEqual(222, Value),
    Bindings = beamtalk_repl_state:get_bindings(State),
    ?assertEqual(111, maps:get(alpha, Bindings)),
    ?assertEqual(222, maps:get(beta, Bindings)),
    %% A later, separate eval call sees the correctly-bound first variable.
    {ok, AlphaValue, _Output2, _Warnings2, _State2} =
        beamtalk_repl_eval:do_eval("alpha", State),
    ?assertEqual(111, AlphaValue).

do_eval_multi_statement_newline_separated_bindings_three() ->
    {ok, _Value, _Output, _Warnings, State} =
        beamtalk_repl_eval:do_eval("a1 := 1\na2 := 2\na3 := 3", state0()),
    Bindings = beamtalk_repl_state:get_bindings(State),
    ?assertEqual(1, maps:get(a1, Bindings)),
    ?assertEqual(2, maps:get(a2, Bindings)),
    ?assertEqual(3, maps:get(a3, Bindings)).

do_eval_runtime_error() ->
    %% Sending an unknown message raises a does_not_understand; do_eval catches
    %% it, wraps it, and stores it under '_error' in the returned bindings.
    {error, _Reason, _Output, _Warnings, State} =
        beamtalk_repl_eval:do_eval("1 frobnicate: 2", state0()),
    Bindings = beamtalk_repl_state:get_bindings(State),
    ?assert(maps:is_key('_error', Bindings)).

do_eval_class_definition() ->
    %% An inline class definition loads the class module and returns its name.
    Source = "Actor subclass: EvalSuccessCls\n  value => 5",
    {ok, ClassName, Output, _Warnings, _State} = beamtalk_repl_eval:do_eval(Source, state0()),
    ?assertEqual(<<"EvalSuccessCls">>, ClassName),
    ?assertEqual(<<>>, Output).

do_eval_protocol_definition() ->
    Source = "Protocol define: EvalSuccessProto",
    {ok, Display, _Output, _Warnings, _State} = beamtalk_repl_eval:do_eval(Source, state0()),
    ?assert(is_binary(Display)),
    ?assert(binary:match(Display, <<"EvalSuccessProto">>) =/= nomatch).

%% ADR 0108 Semantics / BT-2899 (consolidated BT-2912): the concrete repro,
%% exercised through the real `do_eval/2` REPL path end to end — turn 1
%% declares `type EvalCollisionPoint = Integer` (a session-local alias, via
%% the SAME `type_alias_definition` handling `do_eval_type_alias_definition`
%% exercises); turn 2 (threading turn 1's resulting `State`, exactly how a
%% real multi-turn session chains) sends `Object subclass:
%% EvalCollisionPoint` — before BT-2899, `compile`/`compile_method` never
%% threaded `known_type_aliases` at all (only `compile_expression` did), so
%% `AliasRegistry::add_pre_loaded`'s existing collision check never got a
%% chance to run and the class compiled clean, silently shadowing the alias.
%% This is the un-diagnosed-collision bug this issue closes — the class
%% define must now fail with a namespace-collision error.
do_eval_class_definition_over_earlier_turn_alias_is_a_collision_error() ->
    AliasSource = "type EvalCollisionPoint = Integer",
    {ok, <<"EvalCollisionPoint">>, _Output0, _Warnings0, State1} =
        beamtalk_repl_eval:do_eval(AliasSource, state0()),

    ClassSource = "Object subclass: EvalCollisionPoint\n  hello => 42",
    Result = beamtalk_repl_eval:do_eval(ClassSource, State1),
    ?assertMatch({error, #beamtalk_error{}, _, _, _}, Result),
    {error, #beamtalk_error{message = Msg}, _, _, _} = Result,
    ?assert(
        binary:match(Msg, <<"EvalCollisionPoint">>) =/= nomatch,
        io_lib:format("expected the collision message to name the alias, got: ~p", [Msg])
    ),
    ?assert(
        binary:match(Msg, <<"namespace">>) =/= nomatch orelse
            binary:match(Msg, <<"collides">>) =/= nomatch,
        io_lib:format("expected a namespace-collision message, got: ~p", [Msg])
    ).

do_eval_with_undefined_subscriber() ->
    %% do_eval/3 with an explicit undefined subscriber exercises the streaming arg.
    {ok, Value, _Output, _Warnings, _State} =
        beamtalk_repl_eval:do_eval("6 * 7", state0(), undefined),
    ?assertEqual(42, Value).

do_show_codegen_success() ->
    {ok, CoreErlang, Warnings, _State} = beamtalk_repl_eval:do_show_codegen("1 + 2", state0()),
    ?assert(is_binary(CoreErlang)),
    ?assert(byte_size(CoreErlang) > 0),
    ?assert(is_list(Warnings)).

do_show_codegen_error() ->
    {error, _Reason, Warnings, _State} = beamtalk_repl_eval:do_show_codegen("+++", state0()),
    ?assertEqual([], Warnings).

do_eval_trace_single() ->
    {ok, Steps, Output, _Warnings, _State} = beamtalk_repl_eval:do_eval_trace("21 * 2", state0()),
    ?assertMatch([{_Src, 42}], Steps),
    ?assert(is_binary(Output)).

do_eval_trace_multi() ->
    {ok, Steps, _Output, _Warnings, _State} =
        beamtalk_repl_eval:do_eval_trace("1 + 1. 2 + 3", state0()),
    %% One step per top-level statement.
    ?assertEqual(2, length(Steps)),
    [{_, FirstVal}, {_, SecondVal}] = Steps,
    ?assertEqual(2, FirstVal),
    ?assertEqual(5, SecondVal).

do_eval_trace_assignment() ->
    {ok, _Steps, _Output, _Warnings, State} =
        beamtalk_repl_eval:do_eval_trace("total := 30 + 12", state0()),
    Bindings = beamtalk_repl_state:get_bindings(State),
    ?assertEqual(42, maps:get(total, Bindings)).

do_eval_trace_runtime_error() ->
    %% A runtime error (does_not_understand) during trace execution is caught,
    %% wrapped, and stored under '_error', returning a structured error tuple.
    {error, _Reason, Output, _Warnings, State} =
        beamtalk_repl_eval:do_eval_trace("1 frobnicate: 2", state0()),
    ?assert(is_binary(Output)),
    Bindings = beamtalk_repl_state:get_bindings(State),
    ?assert(maps:is_key('_error', Bindings)).

compile_method_unrecorded() ->
    %% Compiling a method onto a class whose source is not recorded compiles the
    %% standalone definition (method_definition path) then fails to install
    %% because there is no recorded source — returns {error, _}.
    Result = beamtalk_repl_eval:compile_method(<<"Object">>, <<"doubled">>, <<"self">>, ephemeral),
    ?assertMatch({error, _}, Result).

compile_method_invalid_body() ->
    Result = beamtalk_repl_eval:compile_method(
        <<"Object">>, <<"bad">>, <<"+++ garbage">>, ephemeral
    ),
    ?assertMatch({error, _}, Result).

compile_method_not_a_method() ->
    %% A plain expression body for a class that exists but whose source is not a
    %% method definition still routes through compile_expression; a bare unary
    %% body is wrapped as `bad => <body>` and may fail to install. Assert error.
    Result = beamtalk_repl_eval:compile_method(
        <<"Object">>, <<"plainExpr">>, <<"1 + 1">>, ephemeral
    ),
    ?assertMatch({error, _}, Result).

%%% normalize_method_source/2 — `compile:source:`/MCP pass the method BODY only;
%%% the IDE passes a full definition. The helper synthesises the `selector => '
%%% header for a bare body and leaves a full (possibly comment-led) definition
%%% untouched, so the structured install path always gets a complete method.

normalize_method_source_full_definition_unchanged_test() ->
    %% A full `selector => body' definition is returned verbatim.
    Src = <<"increment => self.value + 1">>,
    ?assertEqual(Src, beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)).

normalize_method_source_bare_body_gets_header_test() ->
    %% A bare body (no header) gets the canonical `selector => ' prepended.
    ?assertEqual(
        <<"increment => self.value + 1">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, <<"self.value + 1">>)
    ).

normalize_method_source_bare_body_resembling_selector_test() ->
    %% A bare body that merely starts with the selector name (but is not a header)
    %% is still prefixed — `incremented' is not the `increment' header.
    ?assertEqual(
        <<"increment => incremented + 1">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, <<"incremented + 1">>)
    ).

normalize_method_source_commented_full_definition_unchanged_test() ->
    %% A full definition behind `//`/`///` comments is left intact (the header is
    %% found past the comments), so saved comments round-trip.
    Src = <<"// --- Section ---\n/// Doc.\nincrement => self.value + 1">>,
    ?assertEqual(Src, beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)).

%%====================================================================
%% BT-3337 — normalize_method_source/2's private helpers: header_separator/1
%% (all five last-char branches), skip_leading_comments/1's no-trailing-
%% newline clause, has_method_header/2's `class ` recursion + trim_leading_ws/1,
%% header_after_token/1's identifier-continuation guard,
%% binary_has_arrow_before_break/1's `.`/newline breaks, and first_token/1's
%% fallback clause.
%%====================================================================

normalize_method_source_header_separator_newline_test() ->
    %% A `//` comment WITH a trailing newline immediately followed by a bare
    %% body: header_separator/1 sees the prefix ending in `\n` -> no extra
    %% separator is inserted.
    Src = <<"//x\nbareBody">>,
    ?assertEqual(
        <<"//x\nincrement => bareBody">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)
    ).

normalize_method_source_header_separator_space_test() ->
    %% A `//` comment with NO trailing newline: skip_leading_comments/1's
    %% single-element split clause returns an empty Body, so the whole
    %% comment becomes the Prefix — header_separator/1 reads its own last
    %% byte, here a trailing space, and inserts no extra separator.
    Src = <<"// comment ends with space ">>,
    ?assertEqual(
        <<Src/binary, "increment => ">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)
    ).

normalize_method_source_header_separator_tab_test() ->
    Src = <<"// comment ends with tab\t">>,
    ?assertEqual(
        <<Src/binary, "increment => ">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)
    ).

normalize_method_source_header_separator_cr_test() ->
    Src = <<"// comment ends with cr\r">>,
    ?assertEqual(
        <<Src/binary, "increment => ">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)
    ).

normalize_method_source_header_separator_other_inserts_newline_test() ->
    %% A comment-only source ending in ordinary text (no trailing whitespace)
    %% would otherwise glue the injected header onto the comment line, so a
    %% `\n` separator is inserted instead.
    Src = <<"// comment ends with letter">>,
    ?assertEqual(
        <<Src/binary, "\nincrement => ">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, Src)
    ).

normalize_method_source_class_side_header_unchanged_test() ->
    %% A `class ` modifier is skipped, then leading whitespace after it is
    %% trimmed (trim_leading_ws/1's recursive clause) before matching the
    %% selector header.
    Src = <<"class   make => 5">>,
    ?assertEqual(Src, beamtalk_repl_eval:normalize_method_source(<<"make">>, Src)).

normalize_method_source_identifier_continuation_not_a_header_test() ->
    %% `incremented + 1` for selector `increment` shares no header: the char
    %% right after the token is a lowercase continuation, not a delimiter.
    ?assertEqual(
        <<"increment => incremented + 1">>,
        beamtalk_repl_eval:normalize_method_source(<<"increment">>, <<"incremented + 1">>)
    ).

normalize_method_source_dot_break_not_a_header_test() ->
    %% No `=>` arrow appears before the first `.` statement break, so this is
    %% not a header even though the body starts with the selector token.
    ?assertEqual(
        <<"foo => foo bar. baz">>,
        beamtalk_repl_eval:normalize_method_source(<<"foo">>, <<"foo bar. baz">>)
    ).

normalize_method_source_newline_break_not_a_header_test() ->
    %% No `=>` arrow appears before the first newline statement break either.
    ?assertEqual(
        <<"foo => foo bar\nbaz">>,
        beamtalk_repl_eval:normalize_method_source(<<"foo">>, <<"foo bar\nbaz">>)
    ).

normalize_method_source_first_token_fallback_test() ->
    %% first_token/1's fallback clause: a selector binary whose split on
    %% `:`/` ` yields an empty leading segment (e.g. it starts with `:`) is
    %% returned unmodified rather than truncated to that empty head.
    ?assertEqual(
        <<":weird => body">>,
        beamtalk_repl_eval:normalize_method_source(<<":weird">>, <<"body">>)
    ).

do_show_codegen_with_binding() ->
    %% A non-internal binding key is forwarded as a known var to the codegen
    %% compiler (exercises the KnownVars comprehension in do_show_codegen).
    S = beamtalk_repl_state:set_bindings(#{myvar => 5}, state0()),
    {ok, CoreErlang, _Warnings, _State} = beamtalk_repl_eval:do_show_codegen("myvar + 1", S),
    ?assert(is_binary(CoreErlang)),
    %% The known var name appears in the generated Core Erlang lookup.
    ?assert(binary:match(CoreErlang, <<"myvar">>) =/= nomatch).

reload_class_file_arity1() ->
    %% reload_class_file/1 (no expected class name) delegates to the loader.
    Result = beamtalk_repl_eval:reload_class_file("/nonexistent/reload1.bt"),
    ?assertEqual({error, {file_not_found, "/nonexistent/reload1.bt"}}, Result).

handle_load3_missing() ->
    %% handle_load/3 with prebuilt indexes delegates to beamtalk_repl_loader.
    Result = beamtalk_repl_eval:handle_load("/nonexistent/load3.bt", state0(), #{}),
    ?assertMatch({error, {file_not_found, _}, _}, Result).

handle_load_source3_invalid() ->
    %% handle_load_source/3 delegates to the loader; invalid source fails to compile.
    Result = beamtalk_repl_eval:handle_load_source(<<"+++ not valid">>, "inline-label", state0()),
    ?assertMatch({error, _, _}, Result).

new_class_invalid() ->
    %% new_class/2 delegates to the loader; an invalid target path is rejected.
    Result = beamtalk_repl_eval:new_class(
        <<"Actor subclass: NewClsInvalid\n  v => 1">>, <<"/nonexistent/dir/x.bt">>
    ),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% BT-3337 — do_eval/2 branches not reached by the tests above:
%% the `:help <Alias>` short-circuit, a standalone method definition, and a
%% live actor-registry pid threaded into bindings.
%%====================================================================

do_eval_help_for_alias() ->
    AliasSource = "type EvalHelpAlias = Integer",
    {ok, <<"EvalHelpAlias">>, _, _, State1} =
        beamtalk_repl_eval:do_eval(AliasSource, state0()),
    Result = beamtalk_repl_eval:do_eval("Beamtalk help: EvalHelpAlias", State1),
    ?assertMatch({ok, _HelpText, <<>>, [], _}, Result),
    {ok, HelpText, _, _, _} = Result,
    ?assert(binary:match(HelpText, <<"EvalHelpAlias">>) =/= nomatch).

do_eval_standalone_method_definition() ->
    ClassSource = "Actor subclass: EvalStandaloneMethodCls\n  value => 5",
    {ok, <<"EvalStandaloneMethodCls">>, _, _, State1} =
        beamtalk_repl_eval:do_eval(ClassSource, state0()),
    %% reload_method_definition recompiles from the recorded class source
    %% (mirroring `Workspace load:`) — seed it the same way an on-disk load
    %% would have.
    seed_class_source(<<"EvalStandaloneMethodCls">>, ClassSource),
    MethodSource = "EvalStandaloneMethodCls >> value2 => 6",
    Result = beamtalk_repl_eval:do_eval(MethodSource, State1),
    ?assertMatch({ok, _, <<>>, _, _}, Result).

do_eval_with_actor_registry() ->
    %% A non-undefined actor-registry pid is threaded into the eval bindings
    %% map (INTERNAL_REGISTRY_KEY) and stripped again on the way out. A
    %% minimal fake registry answers the post-eval `list_actors` cleanup
    %% call (a real pid, but not self() — gen_server:call(self(), _) would
    %% deadlock-detect against the calling test process itself).
    FakeRegistry = spawn(fun() ->
        receive
            {'$gen_call', From, list_actors} -> gen_server:reply(From, [])
        end
    end),
    State = beamtalk_repl_state:set_actor_registry(FakeRegistry, state0()),
    Result = beamtalk_repl_eval:do_eval("5 + 5", State),
    ?assertMatch({ok, 10, _, _, _}, Result).

%%====================================================================
%% BT-3337 — do_dispatch/5 (BT-2691 connected-mode `beamtalk run` entry
%% dispatch): success (unary + keyword), Program exit:, a wrapped runtime
%% exception, and resolve_entry/2's remaining error branches.
%%====================================================================

do_dispatch_unary_success() ->
    ClassSource =
        "Object subclass: EvalDispatchCls\n"
        "  class run => 42",
    {ok, <<"EvalDispatchCls">>, _, _, _} = beamtalk_repl_eval:do_eval(ClassSource, state0()),
    Result = beamtalk_repl_eval:do_dispatch(
        <<"EvalDispatchCls">>, <<"run">>, [], undefined, state0()
    ),
    ?assertMatch({ok, 42, _, _, _}, Result).

do_dispatch_keyword_success() ->
    ClassSource =
        "Object subclass: EvalDispatchMainCls\n"
        "  class main: args :: List(String) -> Integer =>\n"
        "    args size",
    {ok, <<"EvalDispatchMainCls">>, _, _, _} =
        beamtalk_repl_eval:do_eval(ClassSource, state0()),
    Result = beamtalk_repl_eval:do_dispatch(
        <<"EvalDispatchMainCls">>, <<"main:">>, [<<"a">>, <<"b">>, <<"c">>], undefined, state0()
    ),
    ?assertMatch({ok, 3, _, _, _}, Result).

do_dispatch_script_exit() ->
    ClassSource =
        "Object subclass: EvalDispatchExitCls\n"
        "  class run => Program exit: 7",
    {ok, <<"EvalDispatchExitCls">>, _, _, _} =
        beamtalk_repl_eval:do_eval(ClassSource, state0()),
    Result = beamtalk_repl_eval:do_dispatch(
        <<"EvalDispatchExitCls">>, <<"run">>, [], undefined, state0()
    ),
    ?assertMatch({script_exit, 7, _, _, _}, Result).

do_dispatch_runtime_exception() ->
    ClassSource =
        "Object subclass: EvalDispatchBoomCls\n"
        "  class run => 1 zork",
    {ok, <<"EvalDispatchBoomCls">>, _, _, _} =
        beamtalk_repl_eval:do_eval(ClassSource, state0()),
    Result = beamtalk_repl_eval:do_dispatch(
        <<"EvalDispatchBoomCls">>, <<"run">>, [], undefined, state0()
    ),
    ?assertMatch({error, {eval_error, _Class, _ExObj}, _, _, _}, Result).

do_dispatch_class_not_loaded() ->
    %% `ok` already exists as an atom (it is used everywhere in this test
    %% suite) but names no loaded class — resolve_entry/2's whereis_class
    %% `undefined` branch, distinct from an atom that does not exist at all.
    Result = beamtalk_repl_eval:do_dispatch(<<"ok">>, <<"run">>, [], undefined, state0()),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found}, <<>>, [], _}, Result).

do_dispatch_unknown_selector() ->
    ClassSource =
        "Object subclass: EvalDispatchDnuCls\n"
        "  class run => 42",
    {ok, <<"EvalDispatchDnuCls">>, _, _, _} =
        beamtalk_repl_eval:do_eval(ClassSource, state0()),
    Result = beamtalk_repl_eval:do_dispatch(
        <<"EvalDispatchDnuCls">>,
        <<"__bt3337_never_atom_selector__">>,
        [],
        undefined,
        state0()
    ),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}, <<>>, [], _}, Result),
    {error, #beamtalk_error{message = Msg}, _, _, _} = Result,
    ?assert(binary:match(Msg, <<"EvalDispatchDnuCls">>) =/= nomatch).

%%====================================================================
%% BT-3337 — eval_with_self/2 (ADR 0095 §1, BT-2503): every
%% definition-shaped rejection, a successful evaluate-in-context, a compile
%% error, and a wrapped runtime exception.
%%====================================================================

eval_with_self_rejects_class_definition() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "Actor subclass: EvalSelfRejCls\n  v => 1"),
    ?assertMatch({error, #beamtalk_error{kind = eval_failed}}, Result).

eval_with_self_rejects_method_definition() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "EvalSelfRejCls >> foo => 1"),
    ?assertMatch({error, #beamtalk_error{kind = eval_failed}}, Result).

eval_with_self_rejects_protocol_definition() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "Protocol define: EvalSelfRejProto"),
    ?assertMatch({error, #beamtalk_error{kind = eval_failed}}, Result).

eval_with_self_rejects_type_alias_definition() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "type EvalSelfRejAlias = Integer"),
    ?assertMatch({error, #beamtalk_error{kind = eval_failed}}, Result).

eval_with_self_success() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "self + 1"),
    ?assertEqual({ok, 43}, Result).

eval_with_self_compile_error() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "+++ not valid"),
    ?assertMatch({error, #beamtalk_error{}}, Result).

eval_with_self_runtime_exception() ->
    Result = beamtalk_repl_eval:eval_with_self(42, "self zork"),
    ?assertMatch({error, #beamtalk_error{}}, Result).

%%====================================================================
%% BT-3337 — precheck_method/4 (ADR 0105 Phase 3, BT-2782): the stdlib
%% refusal and the non-stdlib delegate-to-loader branch.
%%====================================================================

precheck_method_stdlib_refused() ->
    %% `Object` ships as a stdlib class, so its methods are read-only.
    Result = beamtalk_repl_eval:precheck_method(<<"Object">>, <<"foo">>, <<"1">>, instance),
    ?assertMatch({error, #beamtalk_error{kind = runtime_error}}, Result).

precheck_method_delegates() ->
    ClassSource = "Actor subclass: EvalPrecheckCls\n  v => 1",
    {ok, <<"EvalPrecheckCls">>, _, _, _} = beamtalk_repl_eval:do_eval(ClassSource, state0()),
    seed_class_source(<<"EvalPrecheckCls">>, ClassSource),
    Result = beamtalk_repl_eval:precheck_method(
        <<"EvalPrecheckCls">>, <<"v">>, <<"2">>, instance
    ),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% BT-3337 — remove_method/3,4 (ADR 0112 Phase 1, BT-3184): the stdlib-policy
%% branches.
%%====================================================================

remove_method3_stdlib_refused() ->
    Result = beamtalk_repl_eval:remove_method(<<"Object">>, <<"printString">>, instance),
    ?assertMatch({error, #beamtalk_error{kind = runtime_error}}, Result).

remove_method4_allow_stdlib() ->
    %% allow_stdlib skips the gate entirely and reaches the loader — whatever
    %% it returns for a selector this class does not locally define.
    Result = beamtalk_repl_eval:remove_method(
        <<"Object">>, <<"__bt3337_no_such_selector__">>, instance, allow_stdlib
    ),
    ?assertMatch({error, _}, Result).

remove_method4_refuse_stdlib_non_stdlib() ->
    ClassSource = "Actor subclass: EvalRemoveMethodCls\n  v => 1",
    {ok, <<"EvalRemoveMethodCls">>, _, _, _} = beamtalk_repl_eval:do_eval(ClassSource, state0()),
    seed_class_source(<<"EvalRemoveMethodCls">>, ClassSource),
    Result = beamtalk_repl_eval:remove_method(
        <<"EvalRemoveMethodCls">>, <<"v">>, instance, refuse_stdlib
    ),
    ?assertMatch({ok, <<"EvalRemoveMethodCls">>}, Result).

%%====================================================================
%% BT-3337 — remove_class/1 (BT-2664 new-class revert case).
%%====================================================================

remove_class_success() ->
    ClassSource = "Actor subclass: EvalRemoveClassCls\n  v => 1",
    {ok, <<"EvalRemoveClassCls">>, _, _, _} = beamtalk_repl_eval:do_eval(ClassSource, state0()),
    Result = beamtalk_repl_eval:remove_class(<<"EvalRemoveClassCls">>),
    ?assertMatch({ok, _Module}, Result).

remove_class_unknown() ->
    Result = beamtalk_repl_eval:remove_class(<<"__bt3337_never_a_class__">>),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result).

%%====================================================================
%% BT-3337 — thin forwarding wrappers over beamtalk_repl_loader: only the
%% delegating call site itself needs to be reached (BT-3335 owns the
%% loader's own branch coverage).
%%====================================================================

move_class_delegates() ->
    Result = beamtalk_repl_eval:move_class('EvalNoSuchMoveCls', <<"new/path.bt">>),
    ?assertMatch({error, _}, Result).

revert_remove_class_delegates() ->
    %% A target path that does not exist yet is not itself a failure —
    %% revert_remove_class/2 reuses new_class/2's install chokepoint, which
    %% happily creates the class fresh (mirroring what a genuine `changes
    %% revert:` of a `'remove-class'` entry does). Clean the class back up
    %% so it does not linger for later tests in the shared EUnit node.
    Result = beamtalk_repl_eval:revert_remove_class(
        <<"Actor subclass: EvalRevertRemoveClassCls\n  v => 1">>,
        <<"eval_revert_remove_class_cls.bt">>
    ),
    ?assertMatch({ok, _}, Result),
    beamtalk_repl_eval:remove_class(<<"EvalRevertRemoveClassCls">>).

rewrite_sites_delegates() ->
    Result = beamtalk_repl_eval:rewrite_sites(undefined, []),
    ?assertMatch({error, _}, Result).

validate_sites_delegates() ->
    Result = beamtalk_repl_eval:validate_sites(undefined, []),
    ?assertMatch({error, _}, Result).

emit_remove_change_entry_delegates() ->
    %% Best-effort append; returns ok even when there is nothing to record.
    Result = beamtalk_repl_eval:emit_remove_change_entry(
        <<"__bt3337_never_a_class__">>, foo, instance, <<"repl">>, human
    ),
    ?assertEqual(ok, Result).

%%====================================================================
%% BT-3337 — reload_file/1 (BT-2598 disk-revert reload): the success path,
%% which also exercises repopulate_class_sources/2 and class_name_binaries/1.
%%====================================================================

reload_file_success() ->
    Path = filename:join(temp_dir(), "bt3337_reload_file_test.bt"),
    ok = file:write_file(Path, <<"Actor subclass: EvalReloadFileCls\n  v => 1\n">>),
    Result = beamtalk_repl_eval:reload_file(Path),
    file:delete(Path),
    ?assertMatch({ok, [<<"EvalReloadFileCls">>]}, Result),
    %% repopulate_class_sources/2 recorded the freshly-read source text.
    ?assertMatch(
        Src when is_list(Src),
        beamtalk_workspace_meta:get_class_source(<<"EvalReloadFileCls">>)
    ).

%%====================================================================
%% BT-3337 — class_name_binary/1 and class_name_binaries/1 pure-mapping
%% branches (atom / binary / string name key, unrecognised shape).
%%====================================================================

class_name_binary_atom_test() ->
    ?assertEqual(<<"Foo">>, beamtalk_repl_eval:class_name_binary(#{name => 'Foo'})).

class_name_binary_binary_test() ->
    ?assertEqual(<<"Foo">>, beamtalk_repl_eval:class_name_binary(#{name => <<"Foo">>})).

class_name_binary_string_test() ->
    ?assertEqual(<<"Foo">>, beamtalk_repl_eval:class_name_binary(#{name => "Foo"})).

class_name_binary_unrecognised_test() ->
    ?assertEqual(undefined, beamtalk_repl_eval:class_name_binary(#{})),
    ?assertEqual(undefined, beamtalk_repl_eval:class_name_binary(#{name => 42})).

class_name_binaries_filters_unrecognised_test() ->
    ?assertEqual(
        [<<"Foo">>, <<"Bar">>],
        beamtalk_repl_eval:class_name_binaries([
            #{name => 'Foo'}, #{}, #{name => <<"Bar">>}
        ])
    ).

%%====================================================================
%% BT-3337 — repopulate_class_sources/2 (BT-2598): a read failure is
%% non-fatal, and a successful read seeds the workspace_meta source cache
%% for each named class, skipping entries with no recognisable name.
%%====================================================================

repopulate_class_sources_read_failure_is_noop_test() ->
    ?assertEqual(
        ok,
        beamtalk_repl_eval:repopulate_class_sources(
            "/nonexistent/bt3337_repopulate.bt", [#{name => 'DoesNotMatter'}]
        )
    ).

repopulate_class_sources_success_test() ->
    %% A standalone workspace_meta instance, mirroring
    %% handle_method_definition_with_source_compile_fail_test/0 above — this
    %% test does not run inside the eval_success_test_/0 fixture.
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid)
    end,
    {ok, WsPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"repopulate_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second)
    }),
    Path = filename:join(temp_dir(), "bt3337_repopulate_sources_test.bt"),
    ok = file:write_file(Path, <<"Actor subclass: RepopulatedCls\n  v => 1\n">>),
    Result = beamtalk_repl_eval:repopulate_class_sources(
        Path, [#{name => 'RepopulatedCls'}, #{}]
    ),
    file:delete(Path),
    ?assertEqual(ok, Result),
    Src = beamtalk_workspace_meta:get_class_source(<<"RepopulatedCls">>),
    gen_server:stop(WsPid),
    ?assert(is_list(Src)),
    ?assertNotEqual(nomatch, string:find(Src, "RepopulatedCls")).

%%====================================================================
%% announce_binding_changed payload (ADR 0093 §2, BT-2530)
%%====================================================================

%% The BindingChanged payload carries the evaluating session's protocol id,
%% read from the worker pdict seed (`beamtalk_repl_shell:seed_session_context/3`).
%% Subscribed with a fun handler so the veneer async dispatch invokes it and we
%% can assert on the full typed event map.
announce_binding_changed_carries_session_id_test() ->
    ok = beamtalk_announcements:ensure_started(),
    Collector = self(),
    {ok, SubRef} = beamtalk_announcements:subscribe(
        'BindingChanged', self(), fun(E) -> Collector ! {binding_evt, E} end, false
    ),
    put(beamtalk_session_id, <<"sess-bt2530">>),
    try
        ok = beamtalk_repl_eval:announce_binding_changed(x, 42),
        receive
            {binding_evt, Event} ->
                ?assertMatch(
                    #{
                        '$beamtalk_class' := 'BindingChanged',
                        name := x,
                        value := 42,
                        sessionId := <<"sess-bt2530">>
                    },
                    Event
                )
        after 1000 -> ?assert(false)
        end
    after
        erase(beamtalk_session_id),
        beamtalk_announcements:unsubscribe(SubRef)
    end.

%% Outside a shell-spawned worker (no pdict seed) sessionId degrades to nil
%% rather than being omitted, so the typed field always exists.
announce_binding_changed_session_id_nil_outside_worker_test() ->
    ok = beamtalk_announcements:ensure_started(),
    erase(beamtalk_session_id),
    Collector = self(),
    {ok, SubRef} = beamtalk_announcements:subscribe(
        'BindingChanged', self(), fun(E) -> Collector ! {binding_evt_nil, E} end, false
    ),
    try
        ok = beamtalk_repl_eval:announce_binding_changed(y, hello),
        receive
            {binding_evt_nil, Event} ->
                ?assertMatch(#{name := y, value := hello, sessionId := nil}, Event)
        after 1000 -> ?assert(false)
        end
    after
        beamtalk_announcements:unsubscribe(SubRef)
    end.

%%====================================================================
%% Stdlib method-patch gate
%%
%% compile_method must refuse to recompile a built-in (stdlib) class: its
%% `@intrinsic'/`@primitive' bodies only compile in stdlib mode, so a
%% workspace-mode recompile cannot succeed. The refusal short-circuits before any
%% compile and is a clean structured #beamtalk_error{}, not a crash or a raw
%% compiler error.
%%====================================================================

stdlib_gate_test_() ->
    {setup, fun stdlib_gate_setup/0, fun stdlib_gate_cleanup/1, [
        {"compile_method on a stdlib class is refused", fun compile_method_stdlib_refused/0},
        {"resolve_entry finds a loaded class + selector", fun resolve_entry_loaded_class/0}
    ]}.

%%====================================================================
%% BT-2691: connected-mode `beamtalk run` entry dispatch (do_dispatch/5)
%%====================================================================

%% The arity-1 keyword form (`main:`) carries argv; the unary form (`run`) does
%% not. A trailing `:` is the discriminator the CLI has already validated.
is_keyword_selector_test() ->
    ?assert(beamtalk_repl_eval:is_keyword_selector(<<"main:">>)),
    ?assert(beamtalk_repl_eval:is_keyword_selector(<<"runWith:">>)),
    ?assertNot(beamtalk_repl_eval:is_keyword_selector(<<"run">>)),
    ?assertNot(beamtalk_repl_eval:is_keyword_selector(<<>>)).

%% BT-3090: `is_keyword_selector/1` now delegates to the canonical
%% `beamtalk_class_builder:is_keyword_selector/1` (via `beamtalk_runtime_api`).
%% A malformed selector with an interior colon but no trailing colon (e.g.
%% `at:put`) is NOT a keyword selector — only the last character matters.
is_keyword_selector_malformed_interior_colon_test() ->
    ?assertNot(beamtalk_repl_eval:is_keyword_selector(<<"at:put">>)).

%% A class that is not loaded resolves to a structured class_not_found error,
%% never a raise — so the connecting client gets a clean message + exit 1.
do_dispatch_class_not_found_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:do_dispatch(
        <<"NoSuchClassBT2691">>, <<"main:">>, [<<"a">>], undefined, State
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found}, <<>>, [], _}, Result
    ),
    {error, #beamtalk_error{message = Msg}, _, _, _} = Result,
    ?assert(binary:match(Msg, <<"NoSuchClassBT2691">>) =/= nomatch).

%% resolve_entry/2 against the live image: a loaded class + an existing selector
%% yields `{ok, ClassPid, SelectorAtom}` (the inputs `class_send/3` consumes).
resolve_entry_loaded_class() ->
    ?assert(beamtalk_runtime_api:whereis_class('ErlangModule') =/= undefined),
    Result = beamtalk_repl_eval:resolve_entry(
        <<"ErlangModule">>, <<"doesNotUnderstand:args:">>
    ),
    ?assertMatch({ok, Pid, 'doesNotUnderstand:args:'} when is_pid(Pid), Result),
    %% An unknown class is still a structured class_not_found error here.
    ?assertMatch(
        {error, #beamtalk_error{kind = class_not_found}},
        beamtalk_repl_eval:resolve_entry(<<"NoSuchClassBT2691">>, <<"main:">>)
    ).

stdlib_gate_setup() ->
    application:ensure_all_started(beamtalk_runtime),
    application:ensure_all_started(beamtalk_stdlib),
    %% Let the stdlib classes (ErlangModule, ...) register before patching.
    timer:sleep(1500),
    ok.

stdlib_gate_cleanup(_) ->
    %% Intentional no-op: beamtalk_runtime / beamtalk_stdlib are left running for
    %% the rest of the shared EUnit node (same convention as eval_teardown, which
    %% only stops the compiler). Nothing here owns those apps exclusively.
    ok.

compile_method_stdlib_refused() ->
    %% ErlangModule is a sealed stdlib class loaded from a `bt@stdlib@' module and
    %% its DNU body is `@intrinsic erlangApply'. The gate rejects the patch up
    %% front rather than crashing on the project-mode recompile.
    ?assert(beamtalk_runtime_api:whereis_class('ErlangModule') =/= undefined),
    Result = beamtalk_repl_eval:compile_method(
        <<"ErlangModule">>, <<"doesNotUnderstand:args:">>, <<"@intrinsic erlangApply">>, ephemeral
    ),
    ?assertMatch({error, #beamtalk_error{kind = runtime_error}}, Result),
    {error, #beamtalk_error{message = Msg}} = Result,
    ?assert(binary:match(Msg, <<"read-only">>) =/= nomatch).
