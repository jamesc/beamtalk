#!/usr/bin/env escript
%%! -noinput
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Fake compiler port for EUnit decode-error tests.
%%
%% Reads one framed request from stdin (via an `{fd,0,1}` OTP port with
%% `{packet,4}`), then responds with four bytes that are not a valid ETF term
%% (`<<255,255,255,255>>'). The outer BEAM's `compile_expression' receive loop
%% calls `binary_to_term(Data, [safe])' on the response; that call raises an
%% EXIT exception, which is caught and mapped to the "Compiler port response
%% is malformed" error path.
%%
%% Usage: invoke via the `escript' binary with this file as the argument,
%% opening the port with the same options the module uses:
%%   EscriptBin = filename:join(filename:dirname(os:find_executable("erl")), "escript"),
%%   Port = open_port({spawn_executable, EscriptBin},
%%                    [{args, [PathToThisFile]},
%%                     {packet,4}, binary, exit_status, use_stdio]),
%%   {error, [#{message := <<"Compiler port response is malformed">>}]} =
%%       beamtalk_compiler_port:compile_expression(Port, <<"1">>, <<"m">>, []).

main(_) ->
    Port = erlang:open_port({fd, 0, 1}, [{packet, 4}, binary]),
    receive
        {Port, {data, _Request}} ->
            %% Send back four bytes that are not a valid ETF term.
            %% 0xFF is not a valid ETF version tag (131 = 0x83), so
            %% `binary_to_term(<<255,255,255,255>>, [safe])' raises.
            erlang:port_command(Port, <<255, 255, 255, 255>>),
            erlang:halt(0)
    after 5000 ->
        erlang:halt(2)
    end.
