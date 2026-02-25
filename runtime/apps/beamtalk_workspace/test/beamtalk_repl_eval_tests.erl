%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_eval module
%%%
%%% Tests expression evaluation, file loading, and compilation.

-module(beamtalk_repl_eval_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% Cross-platform temp directory (TMPDIR on Unix, TEMP on Windows)
temp_dir() ->
    case os:getenv("TMPDIR") of
        false ->
            case os:getenv("TEMP") of
                false -> "/tmp";
                Dir -> Dir
            end;
        Dir ->
            Dir
    end.

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
    ?assertEqual(
        <<"Unexpected token">>,
        beamtalk_repl_eval:format_formatted_diagnostics(FormattedDiagnostics)
    ).

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
    Result = beamtalk_repl_eval:handle_load(temp_dir(), State),
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
    ?assert(beamtalk_repl_eval:is_stdlib_path("stdlib/src/Integer.bt")).

is_stdlib_path_absolute_test() ->
    ?assert(beamtalk_repl_eval:is_stdlib_path("/workspace/project/stdlib/src/Integer.bt")).

is_stdlib_path_non_lib_test() ->
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("src/MyClass.bt")).

is_stdlib_path_non_lib_absolute_test() ->
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("/workspace/project/src/MyClass.bt")).

is_stdlib_path_lib_without_trailing_slash_test() ->
    %% "stdlib/src" alone (no trailing slash) is NOT a stdlib path
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("stdlib/src")).

is_stdlib_path_libs_prefix_test() ->
    %% "stdlib/srcs/" is NOT the same as "stdlib/src/"
    ?assertNot(beamtalk_repl_eval:is_stdlib_path("stdlib/srcs/Integer.bt")).

is_stdlib_path_embedded_lib_test() ->
    %% Path with /stdlib/src/ deeper in the tree
    ?assert(beamtalk_repl_eval:is_stdlib_path("/home/user/projects/beamtalk/stdlib/src/String.bt")).

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
    ?assert(beamtalk_repl_eval:is_internal_key('__repl_actor_registry__')).

is_internal_key_single_underscore_test() ->
    ?assertNot(beamtalk_repl_eval:is_internal_key('_error')).

is_internal_key_regular_atom_test() ->
    ?assertNot(beamtalk_repl_eval:is_internal_key(x)).

is_internal_key_empty_atom_test() ->
    ?assertNot(beamtalk_repl_eval:is_internal_key('')).

%%% register_classes/2 tests

register_classes_no_function_test() ->
    ?assertEqual(ok, beamtalk_repl_eval:register_classes([], lists)).

%%% trigger_hot_reload/2 tests

trigger_hot_reload_empty_classes_test() ->
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_module, [])).

trigger_hot_reload_unknown_class_test() ->
    Classes = [#{name => <<"xyzzy_nonexistent_class_99999">>}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_undefined_name_test() ->
    Classes = [#{name => undefined}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_no_name_key_test() ->
    Classes = [#{}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_list_name_test() ->
    Classes = [#{name => "xyzzy_nonexistent_class_88888"}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_atom_name_test() ->
    Classes = [#{name => xyzzy_nonexistent_class_77777}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_module, Classes)).

%%% activate_module/2 tests

activate_module_nonexistent_test() ->
    ?assertEqual(ok, beamtalk_repl_eval:activate_module(lists, [])).

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
    {error, {compile_error, _}, _, _, NewState} = beamtalk_repl_eval:do_eval("1 + 2", State),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(NewState)),
    gen_server:stop(RegistryPid).

%%% format_formatted_diagnostics edge cases

format_formatted_diagnostics_two_items_test() ->
    Result = beamtalk_repl_eval:format_formatted_diagnostics([<<"A">>, <<"B">>]),
    ?assertEqual(<<"A\n\nB">>, Result).

%%% extract_assignment edge cases

extract_assignment_multiline_test() ->
    ?assertMatch({ok, _}, beamtalk_repl_eval:extract_assignment("x := [1, 2, 3]")).

extract_assignment_no_space_v2_test() ->
    ?assertEqual({ok, abc}, beamtalk_repl_eval:extract_assignment("abc:=123")).

%% ===================================================================
%% compile_expression_via_port catch clauses (BT-627)
%% ===================================================================

compile_expr_noproc_test() ->
    %% Covers exit:{noproc, _} clause (line 341-342)
    Result = beamtalk_repl_eval:compile_expression_via_port("1+2", test_mod, #{}),
    ?assertMatch({error, _}, Result).

compile_expr_noproc_with_env_test() ->
    %% compile_expression_via_port calls beamtalk_compiler which isn't started,
    %% so it hits exit:{noproc, _} rather than the timeout path.
    %% This test verifies the function handles a missing compiler gracefully.
    Result = beamtalk_repl_eval:compile_expression_via_port("hello", test_mod2, #{x => 1}),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% compile_file_via_port catch clauses (BT-627)
%% ===================================================================

compile_file_noproc_test() ->
    %% Covers exit:{noproc, _} clause (line 380-381)
    Result = beamtalk_repl_eval:compile_file_via_port("x := 1", "/test.bt", false, undefined),
    ?assertMatch({error, _}, Result).

compile_file_noproc_stdlib_test() ->
    %% Covers stdlib_mode path too
    Result = beamtalk_repl_eval:compile_file_via_port(
        "Object subclass: Foo", "/stdlib/src/Foo.bt", true, undefined
    ),
    ?assertMatch({error, _}, Result).

%% ===================================================================
%% to_snake_case (BT-775)
%% ===================================================================

to_snake_case_simple_test() ->
    ?assertEqual("counter", beamtalk_repl_eval:to_snake_case("counter")).

to_snake_case_camel_test() ->
    ?assertEqual("counter", beamtalk_repl_eval:to_snake_case("Counter")).

to_snake_case_multi_word_test() ->
    ?assertEqual("scheme_symbol", beamtalk_repl_eval:to_snake_case("SchemeSymbol")).

to_snake_case_three_words_test() ->
    ?assertEqual("my_counter_actor", beamtalk_repl_eval:to_snake_case("MyCounterActor")).

to_snake_case_acronym_test() ->
    %% Acronyms: no underscores within consecutive uppercase
    ?assertEqual("httprouter", beamtalk_repl_eval:to_snake_case("HTTPRouter")).

to_snake_case_already_snake_test() ->
    ?assertEqual("already_snake", beamtalk_repl_eval:to_snake_case("already_snake")).

to_snake_case_empty_test() ->
    ?assertEqual([], beamtalk_repl_eval:to_snake_case([])).

to_snake_case_with_digits_test() ->
    ?assertEqual("app2", beamtalk_repl_eval:to_snake_case("App2")).

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
        ClassInfo, [], "test", State, undefined, undefined
    ),
    ?assertMatch({error, {load_error, _}, <<>>, [], _}, Result).

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
        ClassInfo, [<<"warn">>], "test", State, undefined, undefined
    ),
    ?assertMatch({error, {load_error, _}, <<>>, [<<"warn">>], _}, Result).

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
    %% Test the path where class source exists but recompilation fails (line 273-276)
    %% Store some class source, then try adding a method — compiler not available
    State0 = beamtalk_repl_state:new(undefined, 0),
    State1 = beamtalk_repl_state:set_class_source(
        <<"TestClass">>, "Object subclass: TestClass", State0
    ),
    MethodInfo = #{class_name => <<"TestClass">>, selector => <<"doStuff">>},
    %% Compiler not available — compile will exit with noproc, caught by the try/catch
    %% in handle_method_definition which calls beamtalk_compiler:compile directly
    Result =
        (catch beamtalk_repl_eval:handle_method_definition(
            MethodInfo, [], "doStuff [] := 42", State1
        )),
    %% May either return compile error or exit — both are acceptable
    case Result of
        {error, {compile_error, _}, <<>>, [], _} -> ok;
        {'EXIT', _} -> ok
    end.

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
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_mod, Classes)).

trigger_hot_reload_undefined_name_v2_test() ->
    %% Test the undefined name branch (line 459)
    Classes = [#{name => undefined}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_mod, Classes)).

trigger_hot_reload_no_name_key_v2_test() ->
    %% Test when name key is missing (maps:get returns undefined)
    Classes = [#{}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_mod, Classes)).

trigger_hot_reload_atom_name_v2_test() ->
    %% Test the is_atom(N) branch (line 454)
    %% Use an atom that exists but has no instances
    Classes = [#{name => test_atom_class}],
    ?assertEqual(ok, beamtalk_repl_eval:trigger_hot_reload(some_mod, Classes)).

%% ===================================================================
%% is_stdlib_path edge cases (BT-627)
%% ===================================================================

is_stdlib_path_abs_v2_test() ->
    ?assertEqual(
        true, beamtalk_repl_eval:is_stdlib_path("/home/user/project/stdlib/src/Integer.bt")
    ).

is_stdlib_path_not_stdlib_test() ->
    ?assertEqual(false, beamtalk_repl_eval:is_stdlib_path("/home/user/src/main.bt")).

is_stdlib_path_rel_lib_v2_test() ->
    ?assertEqual(true, beamtalk_repl_eval:is_stdlib_path("stdlib/src/String.bt")).

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
%% reload_class_file (BT-897)
%% ===================================================================

reload_class_file_not_found_test() ->
    %% Non-existent file returns file_not_found
    Result = beamtalk_repl_eval:reload_class_file("/nonexistent/file.bt"),
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
    Result = beamtalk_repl_eval:reload_class_file(TempFile),
    file:delete(TempFile),
    %% Compiler not started — should fail gracefully
    case Result of
        {error, _} -> ok;
        Other -> error({unexpected_result, Other})
    end.
