%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_subprocess gen_server (ADR 0051, Phase 4a+4b).
%%%
%%% **DDD Context:** runtime
%%%
%%% Tests cover:
%%% - writeLine: writes data to subprocess stdin and returns nil
%%% - readLine: blocks until data arrives and returns the line
%%% - readLine: returns nil at EOF (subprocess exited, buffer drained)
%%% - readLine: timeout returns nil when no data arrives within the deadline
%%% - exitCode: returns nil while the subprocess is running
%%% - exitCode: returns the exit code integer after the subprocess exits
%%% - close: terminates the subprocess and marks the port closed
%%% - readStderrLine: blocks until stderr data arrives, returns nil at EOF
%%% - readStderrLine: stdout and stderr buffers are independent
%%% - lines: Stream yields correct sequence of stdout lines

-module(beamtalk_subprocess_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Cross-platform command helpers
%%% ============================================================================

%% @private Return {Executable, Args} for echo that outputs Text and exits.
echo_cmd(Text) ->
    case os:type() of
        {unix, _} -> {<<"/bin/echo">>, [Text]};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, <<"echo ", Text/binary>>]}
    end.

%% @private Return {Executable, Args} for a shell command.
shell_cmd(Script) ->
    case os:type() of
        {unix, _} -> {<<"/bin/sh">>, [<<"-c">>, Script]};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, Script]}
    end.

%% @private Return {Executable, Args} for a long-running process with no stdout.
sleep_cmd() ->
    case os:type() of
        {unix, _} -> {<<"/bin/sleep">>, [<<"60">>]};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, <<"ping -n 60 127.0.0.1 >nul">>]}
    end.

%% @private Return {Executable, Args} for a process that exits immediately with 0.
true_cmd() ->
    case os:type() of
        {unix, _} -> {<<"/bin/true">>, []};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, <<"exit 0">>]}
    end.

%% @private Return platform-appropriate shell script for stderr output.
stderr_echo_script() ->
    case os:type() of
        {unix, _} -> <<"echo err >&2">>;
        {win32, _} -> <<"echo err>&2">>
    end.

%% @private Return platform-appropriate shell script for combined stdout+stderr.
stdout_stderr_script() ->
    case os:type() of
        {unix, _} -> <<"echo out; echo err >&2">>;
        {win32, _} -> <<"echo out& echo err>&2">>
    end.

%% @private Return platform-appropriate shell script for multi-line stdout.
multiline_script() ->
    case os:type() of
        {unix, _} -> <<"printf 'alpha\\nbeta\\ngamma\\n'">>;
        {win32, _} -> <<"(echo alpha& echo beta& echo gamma)">>
    end.

%% @private Return platform-appropriate shell script for partial line (no newline).
partial_line_script() ->
    case os:type() of
        {unix, _} -> <<"printf hello">>;
        {win32, _} -> <<"<nul set /p =hello">>
    end.

%% @private Return platform-appropriate shell script for multi-line stderr.
multiline_stderr_script() ->
    case os:type() of
        {unix, _} -> <<"printf 'a\\nb\\nc\\n' >&2">>;
        {win32, _} -> <<"(echo a& echo b& echo c)>&2">>
    end.

%%% ============================================================================
%%% writeLine — success
%%% ============================================================================

writeLine_writes_line_and_returns_nil_test() ->
    case os:type() of
        {unix, _} ->
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/cat">>,
                args => []
            }),
            %% writeLine should succeed and return nil
            Result = gen_server:call(Pid, {'writeLine:', [<<"hello">>]}),
            ?assertEqual(nil, Result),
            %% Verify data reached the subprocess by reading it back
            Line = gen_server:call(Pid, {readLine, []}, 5000),
            ?assertEqual(<<"hello">>, Line),
            gen_server:call(Pid, {close, []}),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only: no unbuffered stdin echo on Windows"}
    end.

%%% ============================================================================
%%% readLine — blocks until data arrives
%%% ============================================================================

readLine_blocks_until_data_arrives_test() ->
    case os:type() of
        {unix, _} ->
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/cat">>,
                args => []
            }),
            Self = self(),
            %% Spawn a process that will block on readLine
            _Caller = spawn(fun() ->
                Result = gen_server:call(Pid, {readLine, []}, 5000),
                Self ! {readLine_result, Result}
            end),
            %% Give the caller time to issue the gen_server:call
            timer:sleep(50),
            %% Write a line — the blocked readLine should now resolve
            gen_server:call(Pid, {'writeLine:', [<<"world">>]}),
            Line =
                receive
                    {readLine_result, V} -> V
                after 5000 ->
                    error(timeout_waiting_for_readline)
                end,
            ?assertEqual(<<"world">>, Line),
            gen_server:call(Pid, {close, []}),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only: no unbuffered stdin echo on Windows"}
    end.

%%% ============================================================================
%%% readLine — returns nil at EOF
%%% ============================================================================

readLine_returns_nil_at_eof_test() ->
    {EchoExe, EchoArgs} = echo_cmd(<<"goodbye">>),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => EchoExe,
        args => EchoArgs
    }),
    %% First readLine should deliver the line from echo
    Line = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(<<"goodbye">>, Line),
    %% Second readLine: subprocess exited, buffer drained — must return nil
    Eof = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(nil, Eof),
    gen_server:stop(Pid).

%%% ============================================================================
%%% readLine — flushes partial line (no trailing newline) on exit
%%% ============================================================================

readLine_flushes_partial_line_on_exit_test() ->
    {ShExe, ShArgs} = shell_cmd(partial_line_script()),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => ShExe,
        args => ShArgs
    }),
    %% readLine should return the partial line, not nil
    Line = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(<<"hello">>, Line),
    %% Second readLine: EOF
    Eof = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(nil, Eof),
    gen_server:stop(Pid).

%%% ============================================================================
%%% readLine: — timeout returns nil
%%% ============================================================================

readLine_timeout_returns_nil_test() ->
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs
    }),
    Result = gen_server:call(Pid, {'readLine:', [100]}, 5000),
    ?assertEqual(nil, Result),
    gen_server:call(Pid, {close, []}),
    gen_server:stop(Pid).

%%% ============================================================================
%%% exitCode — nil while running
%%% ============================================================================

exitCode_nil_while_running_test() ->
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs
    }),
    ?assertEqual(nil, gen_server:call(Pid, {exitCode, []})),
    gen_server:call(Pid, {close, []}),
    gen_server:stop(Pid).

%%% ============================================================================
%%% exitCode — integer after exit
%%% ============================================================================

exitCode_integer_after_exit_test() ->
    {TrueExe, TrueArgs} = true_cmd(),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => TrueExe,
        args => TrueArgs
    }),
    %% readLine blocks until EOF (process exit), then returns nil
    Eof = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(nil, Eof),
    %% Exit event has been processed by now
    ?assertEqual(0, gen_server:call(Pid, {exitCode, []})),
    gen_server:stop(Pid).

%%% ============================================================================
%%% close — terminates subprocess interaction
%%% ============================================================================

close_terminates_subprocess_interaction_test() ->
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs
    }),
    %% exitCode should be nil while sleep is running
    ?assertEqual(nil, gen_server:call(Pid, {exitCode, []})),
    %% close should succeed and return nil
    ?assertEqual(nil, gen_server:call(Pid, {close, []})),
    %% After close, readLine must return nil immediately (port closed)
    ?assertEqual(nil, gen_server:call(Pid, {readLine, []}, 5000)),
    gen_server:stop(Pid).

%%% ============================================================================
%%% close — idempotent
%%% ============================================================================

close_is_idempotent_test() ->
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs
    }),
    ?assertEqual(nil, gen_server:call(Pid, {close, []})),
    %% Second close should not crash
    ?assertEqual(nil, gen_server:call(Pid, {close, []})),
    gen_server:stop(Pid).

%%% ============================================================================
%%% readStderrLine — blocks until data, returns nil at EOF
%%% ============================================================================

readStderrLine_returns_nil_at_eof_test() ->
    {ShExe, ShArgs} = shell_cmd(stderr_echo_script()),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => ShExe,
        args => ShArgs
    }),
    Line = gen_server:call(Pid, {readStderrLine, []}, 5000),
    ?assertEqual(<<"err">>, Line),
    %% Second call: process exited, buffer drained — must return nil
    Eof = gen_server:call(Pid, {readStderrLine, []}, 5000),
    ?assertEqual(nil, Eof),
    gen_server:stop(Pid).

%%% ============================================================================
%%% readStderrLine — timeout returns nil
%%% ============================================================================

readStderrLine_timeout_returns_nil_test() ->
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs
    }),
    Result = gen_server:call(Pid, {'readStderrLine:', [100]}, 5000),
    ?assertEqual(nil, Result),
    gen_server:call(Pid, {close, []}),
    gen_server:stop(Pid).

%%% ============================================================================
%%% readStderrLine — stdout and stderr are independent
%%% ============================================================================

stdout_and_stderr_are_independent_test() ->
    {ShExe, ShArgs} = shell_cmd(stdout_stderr_script()),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => ShExe,
        args => ShArgs
    }),
    %% Read from stdout — should get "out", not "err"
    StdoutLine = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(<<"out">>, StdoutLine),
    %% Read from stderr — should get "err", not "out"
    StderrLine = gen_server:call(Pid, {readStderrLine, []}, 5000),
    ?assertEqual(<<"err">>, StderrLine),
    %% Both channels now at EOF
    ?assertEqual(nil, gen_server:call(Pid, {readLine, []}, 5000)),
    ?assertEqual(nil, gen_server:call(Pid, {readStderrLine, []}, 5000)),
    gen_server:stop(Pid).

%%% ============================================================================
%%% lines — Stream yields correct sequence
%%% ============================================================================

lines_stream_yields_correct_sequence_test() ->
    {ShExe, ShArgs} = shell_cmd(multiline_script()),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => ShExe,
        args => ShArgs
    }),
    Stream = gen_server:call(Pid, {lines, []}),
    Lines = beamtalk_stream:as_list(Stream),
    ?assertEqual([<<"alpha">>, <<"beta">>, <<"gamma">>], Lines),
    gen_server:stop(Pid).

%%% ============================================================================
%%% fast-exit stderr race regression (BT-1148)
%%% ============================================================================

%% Regression test for the port closure race: a fast-exiting process that writes
%% only to stderr must not lose data.  Before BT-1148 this failed ~20% of the
%% time because the reaper sent the exit event before the reader thread had
%% finished flushing stderr data.
fast_exit_stderr_data_not_lost_test_() ->
    {ShExe, ShArgs} = shell_cmd(stderr_echo_script()),
    %% Guard against missing beamtalk-exec binary (not yet built in some envs).
    case beamtalk_subprocess:start(#{executable => ShExe, args => ShArgs}) of
        {error, {{exec_not_found, _}, _}} ->
            [];
        {ok, CheckPid} ->
            gen_server:stop(CheckPid),
            %% Run 50 times: pre-fix failure rate was ~20%, giving (0.8)^50 < 0.001% false-pass chance.
            [
                begin
                    {ShExe2, ShArgs2} = shell_cmd(stderr_echo_script()),
                    {ok, Pid} = beamtalk_subprocess:start(#{
                        executable => ShExe2,
                        args => ShArgs2
                    }),
                    Line = gen_server:call(Pid, {readStderrLine, []}, 5000),
                    gen_server:stop(Pid),
                    ?_assertEqual(<<"err">>, Line)
                end
             || _ <- lists:seq(1, 50)
            ]
    end.

%%% ============================================================================
%%% stderrLines — Stream yields correct sequence
%%% ============================================================================

stderrLines_stream_yields_correct_sequence_test() ->
    {ShExe, ShArgs} = shell_cmd(multiline_stderr_script()),
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => ShExe,
        args => ShArgs
    }),
    Stream = gen_server:call(Pid, {stderrLines, []}),
    Lines = beamtalk_stream:as_list(Stream),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Lines),
    gen_server:stop(Pid).

%%% ============================================================================
%%% dispatch — open returns Result ok (ADR 0060)
%%% BT-1211: Removed — dispatch/3 FFI shim no longer exists after native facade
%%% migration. Result wrapping is now handled by the Beamtalk class method.
%%% Equivalent coverage: SubprocessTest>>testSpawnAndReadLine in BUnit.
%%% ============================================================================

%%% ============================================================================
%%% validate_config — error paths (BT-1211)
%%% ============================================================================

validate_config_missing_executable_test() ->
    {error, {missing_key, executable}} =
        beamtalk_subprocess:start(#{}).

validate_config_non_binary_executable_test() ->
    {error, {type_error, executable, expected_binary}} =
        beamtalk_subprocess:start(#{executable => 42}).

validate_config_non_binary_args_test() ->
    {EchoExe, _} = echo_cmd(<<"test">>),
    {error, {type_error, args, expected_binary_elements}} =
        beamtalk_subprocess:start(#{executable => EchoExe, args => [123]}).

validate_config_non_map_env_test() ->
    {EchoExe, _} = echo_cmd(<<"test">>),
    {error, {type_error, env, expected_map}} =
        beamtalk_subprocess:start(#{executable => EchoExe, args => [], env => 42}).

validate_config_non_binary_dir_test() ->
    {EchoExe, _} = echo_cmd(<<"test">>),
    {error, {type_error, dir, expected_binary}} =
        beamtalk_subprocess:start(#{executable => EchoExe, args => [], dir => 42}).

validate_config_beamtalk_array_coercion_test() ->
    {EchoExe, _} = echo_cmd(<<"hello">>),
    %% Beamtalk Array tagged map should be coerced to a plain list
    BtArray = #{'$beamtalk_class' => 'Array', 'data' => array:from_list([<<"hello">>])},
    {ok, Pid} = beamtalk_subprocess:start(#{
        executable => EchoExe,
        args => BtArray
    }),
    Line = gen_server:call(Pid, {readLine, []}, 5000),
    ?assertEqual(<<"hello">>, Line),
    gen_server:stop(Pid).
