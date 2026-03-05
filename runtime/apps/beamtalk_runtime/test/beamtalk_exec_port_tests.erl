%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_exec_port module.
%%%
%%% **DDD Context:** runtime
%%%
%%% Tests cover:
%%% - find_exec_binary/0 (finds the dev build)
%%% - find_project_root/0 (finds Cargo.toml)
%%% - open/close lifecycle (opens the port and closes cleanly)
%%% - spawn_child: sends spawn command, receives exit event
%%% - write_stdin: sends data to a subprocess
%%% - kill_child: terminates a running subprocess

-module(beamtalk_exec_port_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% find_project_root/0
%%% ============================================================================

find_project_root_finds_cargo_toml_test() ->
    Root = beamtalk_exec_port:find_project_root(),
    ?assert(filelib:is_regular(filename:join(Root, "Cargo.toml"))).

%%% ============================================================================
%%% find_exec_binary/0
%%% ============================================================================

find_exec_binary_returns_existing_file_test() ->
    Binary = beamtalk_exec_port:find_exec_binary(),
    ?assert(filelib:is_regular(Binary)).

%%% ============================================================================
%%% open/close
%%% ============================================================================

open_close_test() ->
    Port = beamtalk_exec_port:open(),
    ?assert(is_port(Port)),
    beamtalk_exec_port:close(Port).

%%% ============================================================================
%%% spawn_child — exit event received
%%% ============================================================================

spawn_child_true_exits_with_zero_test() ->
    case os:type() of
        {unix, _} ->
            Port = beamtalk_exec_port:open(),
            beamtalk_exec_port:spawn_child(Port, 1, <<"/bin/true">>, []),
            ExitCode = receive_exit(Port, 1, 5000),
            beamtalk_exec_port:close(Port),
            ?assertEqual(0, ExitCode);
        _ ->
            {skip, "Unix-only test"}
    end.

spawn_child_false_exits_nonzero_test() ->
    case os:type() of
        {unix, _} ->
            Port = beamtalk_exec_port:open(),
            beamtalk_exec_port:spawn_child(Port, 2, <<"/bin/false">>, []),
            ExitCode = receive_exit(Port, 2, 5000),
            beamtalk_exec_port:close(Port),
            ?assertNotEqual(0, ExitCode);
        _ ->
            {skip, "Unix-only test"}
    end.

%%% ============================================================================
%%% write_stdin — data flows through stdin
%%% ============================================================================

write_stdin_cat_test() ->
    Port = beamtalk_exec_port:open(),
    beamtalk_exec_port:spawn_child(Port, 3, <<"/bin/cat">>, []),
    beamtalk_exec_port:write_stdin(Port, 3, <<"hello\n">>),
    beamtalk_exec_port:close_stdin(Port, 3),
    Stdout = receive_stdout(Port, 3, 5000),
    beamtalk_exec_port:close(Port),
    ?assertEqual(<<"hello\n">>, Stdout).

%%% ============================================================================
%%% kill_child — process is terminated
%%% ============================================================================

kill_child_sleep_test() ->
    Port = beamtalk_exec_port:open(),
    beamtalk_exec_port:spawn_child(Port, 4, <<"/bin/sleep">>, [<<"60">>]),
    beamtalk_exec_port:kill_child(Port, 4),
    % Should receive exit event (SIGTERM -> exit)
    ExitCode = receive_exit(Port, 4, 5000),
    beamtalk_exec_port:close(Port),
    % SIGTERM exit codes are system-dependent; just assert we got an exit event.
    ?assert(is_integer(ExitCode)).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Collect all stdout data for ChildId until the process exits.
receive_stdout(Port, ChildId, Timeout) ->
    receive_stdout(Port, ChildId, Timeout, <<>>).

receive_stdout(Port, ChildId, Timeout, Acc) ->
    receive
        {Port, {data, Bin}} ->
            case binary_to_term(Bin, [safe]) of
                {stdout, ChildId, Data} ->
                    receive_stdout(Port, ChildId, Timeout, <<Acc/binary, Data/binary>>);
                {exit, ChildId, _Code} ->
                    Acc;
                _ ->
                    receive_stdout(Port, ChildId, Timeout, Acc)
            end;
        {Port, {exit_status, _}} ->
            Acc
    after Timeout ->
        error({timeout_waiting_for_stdout, ChildId})
    end.

%% Wait for the exit event for ChildId and return the exit code.
receive_exit(Port, ChildId, Timeout) ->
    receive
        {Port, {data, Bin}} ->
            case binary_to_term(Bin, [safe]) of
                {exit, ChildId, Code} ->
                    Code;
                _ ->
                    receive_exit(Port, ChildId, Timeout)
            end;
        {Port, {exit_status, _}} ->
            error({port_exited_before_child, ChildId})
    after Timeout ->
        error({timeout_waiting_for_exit, ChildId})
    end.
