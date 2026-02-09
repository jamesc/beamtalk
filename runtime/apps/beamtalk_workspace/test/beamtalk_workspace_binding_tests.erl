%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integration tests for workspace binding injection (BT-373).
%%%
%%% Tests that TranscriptStream and SystemDictionary singletons
%%% register themselves in persistent_term and via register/2 when
%%% started via their singleton start functions.

-module(beamtalk_workspace_binding_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests — TranscriptStream binding
%%====================================================================

transcript_persistent_term_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link_singleton(1000),
    try
        Expected = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid},
        ?assertEqual(Expected, persistent_term:get({beamtalk_binding, 'Transcript'}))
    after
        cleanup_transcript(Pid)
    end.

transcript_registered_name_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link_singleton(1000),
    try
        ?assertEqual(Pid, whereis('Transcript'))
    after
        cleanup_transcript(Pid)
    end.

transcript_binding_matches_whereis_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link_singleton(1000),
    try
        Binding = persistent_term:get({beamtalk_binding, 'Transcript'}),
        RegisteredPid = whereis('Transcript'),
        ?assertEqual(Pid, RegisteredPid),
        ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid}, Binding)
    after
        cleanup_transcript(Pid)
    end.

transcript_cleanup_on_stop_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link_singleton(1000),
    Expected = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid},
    ?assertEqual(Expected, persistent_term:get({beamtalk_binding, 'Transcript'})),
    gen_server:stop(Pid),
    %% persistent_term should be erased after stop
    ?assertEqual(undefined, persistent_term:get({beamtalk_binding, 'Transcript'}, undefined)),
    %% register is cleaned up automatically by BEAM when process dies
    ?assertEqual(undefined, whereis('Transcript')).

transcript_non_singleton_no_binding_test() ->
    %% Non-singleton start_link should NOT register binding
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    try
        ?assertEqual(undefined, persistent_term:get({beamtalk_binding, 'Transcript'}, undefined)),
        ?assertEqual(undefined, whereis('Transcript'))
    after
        gen_server:stop(Pid)
    end.

%%====================================================================
%% Tests — SystemDictionary binding
%%====================================================================

sysdict_persistent_term_test() ->
    {ok, Pid} = beamtalk_system_dictionary:start_link_singleton(),
    try
        Expected = {beamtalk_object, 'SystemDictionary', beamtalk_system_dictionary, Pid},
        ?assertEqual(Expected, persistent_term:get({beamtalk_binding, 'Beamtalk'}))
    after
        cleanup_sysdict(Pid)
    end.

sysdict_registered_name_test() ->
    {ok, Pid} = beamtalk_system_dictionary:start_link_singleton(),
    try
        ?assertEqual(Pid, whereis('Beamtalk'))
    after
        cleanup_sysdict(Pid)
    end.

sysdict_binding_matches_whereis_test() ->
    {ok, Pid} = beamtalk_system_dictionary:start_link_singleton(),
    try
        Binding = persistent_term:get({beamtalk_binding, 'Beamtalk'}),
        RegisteredPid = whereis('Beamtalk'),
        ?assertEqual(Pid, RegisteredPid),
        ?assertMatch({beamtalk_object, 'SystemDictionary', beamtalk_system_dictionary, Pid}, Binding)
    after
        cleanup_sysdict(Pid)
    end.

sysdict_cleanup_on_stop_test() ->
    {ok, Pid} = beamtalk_system_dictionary:start_link_singleton(),
    Expected = {beamtalk_object, 'SystemDictionary', beamtalk_system_dictionary, Pid},
    ?assertEqual(Expected, persistent_term:get({beamtalk_binding, 'Beamtalk'})),
    gen_server:stop(Pid),
    ?assertEqual(undefined, persistent_term:get({beamtalk_binding, 'Beamtalk'}, undefined)),
    ?assertEqual(undefined, whereis('Beamtalk')).

sysdict_non_singleton_no_binding_test() ->
    %% Non-singleton start_link should NOT register binding
    {ok, Pid} = beamtalk_system_dictionary:start_link(),
    try
        ?assertEqual(undefined, persistent_term:get({beamtalk_binding, 'Beamtalk'}, undefined)),
        ?assertEqual(undefined, whereis('Beamtalk'))
    after
        gen_server:stop(Pid)
    end.

%%====================================================================
%% Helpers
%%====================================================================

cleanup_transcript(Pid) ->
    gen_server:stop(Pid).

cleanup_sysdict(Pid) ->
    gen_server:stop(Pid).
