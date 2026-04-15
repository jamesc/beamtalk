%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_opaque_ops_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_opaque_ops direct API (BT-1974).

Tests pid_to_string/1, port_to_string/1, and ref_to_string/1 formatting.
Validates output matches #Pid<>/#Port<>/#Ref<> patterns.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% pid_to_string/1
%%====================================================================

pid_to_string_returns_binary_test() ->
    Result = beamtalk_opaque_ops:pid_to_string(self()),
    ?assert(is_binary(Result)).

pid_to_string_has_pid_prefix_test() ->
    Result = beamtalk_opaque_ops:pid_to_string(self()),
    ?assertMatch(<<"#Pid<", _/binary>>, Result).

pid_to_string_ends_with_closing_bracket_test() ->
    Result = beamtalk_opaque_ops:pid_to_string(self()),
    Size = byte_size(Result),
    ?assertEqual($>, binary:at(Result, Size - 1)).

pid_to_string_contains_dots_test() ->
    Result = beamtalk_opaque_ops:pid_to_string(self()),
    %% Strip prefix "#Pid<" and suffix ">" to get "X.Y.Z"
    Inner = binary:part(Result, 5, byte_size(Result) - 6),
    Parts = binary:split(Inner, <<".">>, [global]),
    ?assertEqual(3, length(Parts)).

%%====================================================================
%% port_to_string/1
%%====================================================================

port_to_string_returns_binary_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Result = beamtalk_opaque_ops:port_to_string(Port),
    ?assert(is_binary(Result)),
    gen_tcp:close(Port).

port_to_string_has_port_prefix_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Result = beamtalk_opaque_ops:port_to_string(Port),
    ?assertMatch(<<"#Port<", _/binary>>, Result),
    gen_tcp:close(Port).

port_to_string_ends_with_closing_bracket_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Result = beamtalk_opaque_ops:port_to_string(Port),
    Size = byte_size(Result),
    ?assertEqual($>, binary:at(Result, Size - 1)),
    gen_tcp:close(Port).

%%====================================================================
%% ref_to_string/1
%%====================================================================

ref_to_string_returns_binary_test() ->
    Ref = make_ref(),
    Result = beamtalk_opaque_ops:ref_to_string(Ref),
    ?assert(is_binary(Result)).

ref_to_string_has_ref_prefix_test() ->
    Ref = make_ref(),
    Result = beamtalk_opaque_ops:ref_to_string(Ref),
    ?assertMatch(<<"#Ref<", _/binary>>, Result).

ref_to_string_ends_with_closing_bracket_test() ->
    Ref = make_ref(),
    Result = beamtalk_opaque_ops:ref_to_string(Ref),
    Size = byte_size(Result),
    ?assertEqual($>, binary:at(Result, Size - 1)).

%%====================================================================
%% Distinct values produce distinct strings
%%====================================================================

different_pids_different_strings_test() ->
    Pid1 = self(),
    Pid2 = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    S1 = beamtalk_opaque_ops:pid_to_string(Pid1),
    S2 = beamtalk_opaque_ops:pid_to_string(Pid2),
    ?assertNotEqual(S1, S2),
    Pid2 ! stop.

different_refs_different_strings_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    S1 = beamtalk_opaque_ops:ref_to_string(Ref1),
    S2 = beamtalk_opaque_ops:ref_to_string(Ref2),
    ?assertNotEqual(S1, S2).
