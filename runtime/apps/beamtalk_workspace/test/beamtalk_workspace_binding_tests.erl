%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integration tests for workspace singleton registration (BT-491).
%%%
%%% Tests that TranscriptStream and SystemDictionary singletons
%%% register themselves via gen_server name registration when
%%% started with a named server reference.

-module(beamtalk_workspace_binding_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests — TranscriptStream registration
%%====================================================================

transcript_registered_name_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
    try
        ?assertEqual(Pid, whereis('Transcript'))
    after
        cleanup(Pid)
    end.

transcript_non_named_no_registration_test() ->
    %% Non-named start_link should NOT register a name
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    try
        ?assertEqual(undefined, whereis('Transcript'))
    after
        gen_server:stop(Pid)
    end.

transcript_cleanup_on_stop_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
    ?assertEqual(Pid, whereis('Transcript')),
    gen_server:stop(Pid),
    %% Registered name is cleaned up automatically by BEAM when process dies
    ?assertEqual(undefined, whereis('Transcript')).

%%====================================================================
%% Tests — SystemDictionary registration
%%====================================================================

sysdict_registered_name_test() ->
    {ok, Pid} = beamtalk_system_dictionary:start_link({local, 'Beamtalk'}, []),
    try
        ?assertEqual(Pid, whereis('Beamtalk'))
    after
        cleanup(Pid)
    end.

sysdict_non_named_no_registration_test() ->
    %% Non-named start_link should NOT register a name
    {ok, Pid} = beamtalk_system_dictionary:start_link(),
    try
        ?assertEqual(undefined, whereis('Beamtalk'))
    after
        gen_server:stop(Pid)
    end.

sysdict_cleanup_on_stop_test() ->
    {ok, Pid} = beamtalk_system_dictionary:start_link({local, 'Beamtalk'}, []),
    ?assertEqual(Pid, whereis('Beamtalk')),
    gen_server:stop(Pid),
    ?assertEqual(undefined, whereis('Beamtalk')).

%%====================================================================
%% Helpers
%%====================================================================

cleanup(Pid) ->
    gen_server:stop(Pid).
