%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_binding_tests).

-moduledoc """
Integration tests for workspace singleton registration.

Tests that the TranscriptStream singleton
registers itself via gen_server name registration when
started with a named server reference.
""".
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
%% Helpers
%%====================================================================

cleanup(Pid) ->
    gen_server:stop(Pid).
