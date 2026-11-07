#!/usr/bin/env escript
%%! -noinput
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Fake compiler port for EUnit transport-error tests.
%%
%% Reads one framed request from stdin (via an `{fd,0,1}` OTP port with
%% `{packet,4}`), then exits with status 1.
%%
%% This triggers the `{Port, {exit_status, Status}}' arm in every
%% `beamtalk_compiler_port' receive loop, exercising the "compiler port
%% exited unexpectedly" error path without a 30-second timeout wait.
%%
%% Usage: invoke via the `escript' binary with this file as the argument,
%% opening the port with the same options the module uses:
%%   EscriptBin = filename:join(filename:dirname(os:find_executable("erl")), "escript"),
%%   Port = open_port({spawn_executable, EscriptBin},
%%                    [{args, [PathToThisFile]},
%%                     {packet,4}, binary, exit_status, use_stdio]),
%%   {error, [#{message := <<"Compiler port exited unexpectedly">>}]} =
%%       beamtalk_compiler_port:compile_expression(Port, <<"1">>, <<"m">>, []).

main(_) ->
    Port = erlang:open_port({fd, 0, 1}, [{packet, 4}, binary]),
    receive
        {Port, {data, _Request}} ->
            erlang:halt(1)
    after 5000 ->
        erlang:halt(2)
    end.
