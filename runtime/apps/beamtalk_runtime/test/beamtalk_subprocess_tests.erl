%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_subprocess gen_server (ADR 0051, Phase 4a).
%%%
%%% **DDD Context:** runtime
%%%
%%% Tests cover (all Unix-conditional):
%%% - writeLine: writes data to subprocess stdin and returns nil
%%% - readLine: blocks until data arrives and returns the line
%%% - readLine: returns nil at EOF (subprocess exited, buffer drained)
%%% - readLine: timeout returns nil when no data arrives within the deadline
%%% - exitCode: returns nil while the subprocess is running
%%% - exitCode: returns the exit code integer after the subprocess exits
%%% - close: terminates the subprocess and marks the port closed

-module(beamtalk_subprocess_tests).

-include_lib("eunit/include/eunit.hrl").

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
            {skip, "Unix-only test"}
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
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% readLine — returns nil at EOF
%%% ============================================================================

readLine_returns_nil_at_eof_test() ->
    case os:type() of
        {unix, _} ->
            %% /bin/echo prints one line then exits — produces exactly one stdout line
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/echo">>,
                args => [<<"goodbye">>]
            }),
            %% First readLine should deliver the line from echo
            Line = gen_server:call(Pid, {readLine, []}, 5000),
            ?assertEqual(<<"goodbye">>, Line),
            %% Second readLine: subprocess exited, buffer drained — must return nil
            Eof = gen_server:call(Pid, {readLine, []}, 5000),
            ?assertEqual(nil, Eof),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% readLine — flushes partial line (no trailing newline) on exit
%%% ============================================================================

readLine_flushes_partial_line_on_exit_test() ->
    case os:type() of
        {unix, _} ->
            %% printf writes "hello" without trailing newline, then exits
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/sh">>,
                args => [<<"-c">>, <<"printf hello">>]
            }),
            %% readLine should return the partial line, not nil
            Line = gen_server:call(Pid, {readLine, []}, 5000),
            ?assertEqual(<<"hello">>, Line),
            %% Second readLine: EOF
            Eof = gen_server:call(Pid, {readLine, []}, 5000),
            ?assertEqual(nil, Eof),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% readLine: — timeout returns nil
%%% ============================================================================

readLine_timeout_returns_nil_test() ->
    case os:type() of
        {unix, _} ->
            %% /bin/sleep produces no stdout — readLine: with short timeout returns nil
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/sleep">>,
                args => [<<"60">>]
            }),
            Result = gen_server:call(Pid, {'readLine:', [100]}, 5000),
            ?assertEqual(nil, Result),
            gen_server:call(Pid, {close, []}),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% exitCode — nil while running
%%% ============================================================================

exitCode_nil_while_running_test() ->
    case os:type() of
        {unix, _} ->
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/sleep">>,
                args => [<<"60">>]
            }),
            ?assertEqual(nil, gen_server:call(Pid, {exitCode, []})),
            gen_server:call(Pid, {close, []}),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% exitCode — integer after exit
%%% ============================================================================

exitCode_integer_after_exit_test() ->
    case os:type() of
        {unix, _} ->
            %% /bin/true exits immediately with code 0, no stdout
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/true">>,
                args => []
            }),
            %% readLine blocks until EOF (process exit), then returns nil
            Eof = gen_server:call(Pid, {readLine, []}, 5000),
            ?assertEqual(nil, Eof),
            %% Exit event has been processed by now
            ?assertEqual(0, gen_server:call(Pid, {exitCode, []})),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% close — terminates subprocess interaction
%%% ============================================================================

close_terminates_subprocess_interaction_test() ->
    case os:type() of
        {unix, _} ->
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/sleep">>,
                args => [<<"60">>]
            }),
            %% exitCode should be nil while sleep is running
            ?assertEqual(nil, gen_server:call(Pid, {exitCode, []})),
            %% close should succeed and return nil
            ?assertEqual(nil, gen_server:call(Pid, {close, []})),
            %% After close, readLine must return nil immediately (port closed)
            ?assertEqual(nil, gen_server:call(Pid, {readLine, []}, 5000)),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% close — idempotent
%%% ============================================================================

close_is_idempotent_test() ->
    case os:type() of
        {unix, _} ->
            {ok, Pid} = beamtalk_subprocess:start(#{
                executable => <<"/bin/sleep">>,
                args => [<<"60">>]
            }),
            ?assertEqual(nil, gen_server:call(Pid, {close, []})),
            %% Second close should not crash
            ?assertEqual(nil, gen_server:call(Pid, {close, []})),
            gen_server:stop(Pid);
        _ ->
            {skip, "Unix-only test"}
    end.
