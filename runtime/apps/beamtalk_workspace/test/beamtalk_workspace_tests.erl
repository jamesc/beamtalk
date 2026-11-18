%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_tests).

-moduledoc """
Unit tests for beamtalk_workspace module

Tests the workspace API (status/0).
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test helpers
%%====================================================================

test_metadata() ->
    {beamtalk_string, TmpDir} = beamtalk_file:'tempDirectory'(),
    #{
        workspace_id => <<"test-ws-123">>,
        project_path => <<TmpDir/binary, "/test-project">>,
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }.

ensure_meta_stopped() ->
    beamtalk_test_proc:stop_registered(beamtalk_workspace_meta).

%%====================================================================
%% Tests
%%====================================================================

status_returns_error_when_meta_not_started_test() ->
    ensure_meta_stopped(),
    ?assertEqual({error, not_started}, beamtalk_workspace:status()).

status_returns_ok_map_when_meta_started_test() ->
    ensure_meta_stopped(),
    Meta = test_metadata(),
    {ok, Pid} = beamtalk_workspace_meta:start_link(Meta),
    try
        {ok, Status} = beamtalk_workspace:status(),
        ?assertEqual(<<"test-ws-123">>, maps:get(workspace_id, Status)),
        ?assertEqual(maps:get(project_path, Meta), maps:get(project_path, Status)),
        ?assert(is_integer(maps:get(created_at, Status))),
        ?assert(is_integer(maps:get(last_activity, Status))),
        %% The count comes from the actor scan, not a tracked registry.
        ?assertEqual(
            length([A || #{system := false} = A <- beamtalk_workspace_actors:list()]),
            maps:get(actor_count, Status)
        ),
        ?assertEqual([], maps:get(loaded_modules, Status))
    after
        gen_server:stop(Pid)
    end.
