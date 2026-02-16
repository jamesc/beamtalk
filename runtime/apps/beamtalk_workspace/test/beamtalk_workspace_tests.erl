%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_workspace module
%%%
%%% Tests the workspace API (status/0).

-module(beamtalk_workspace_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test helpers
%%====================================================================

test_metadata() ->
    #{
        workspace_id => <<"test-ws-123">>,
        project_path => <<"/tmp/test-project">>,
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }.

ensure_meta_stopped() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end.

%%====================================================================
%% Tests
%%====================================================================

status_returns_error_when_meta_not_started_test() ->
    ensure_meta_stopped(),
    ?assertEqual({error, not_started}, beamtalk_workspace:status()).

status_returns_ok_map_when_meta_started_test() ->
    ensure_meta_stopped(),
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    {ok, Status} = beamtalk_workspace:status(),
    ?assertEqual(<<"test-ws-123">>, maps:get(workspace_id, Status)),
    ?assertEqual(<<"/tmp/test-project">>, maps:get(project_path, Status)),
    ?assert(is_integer(maps:get(created_at, Status))),
    ?assert(is_integer(maps:get(last_activity, Status))),
    ?assertEqual(0, maps:get(actor_count, Status)),
    ?assertEqual([], maps:get(loaded_modules, Status)),
    gen_server:stop(Pid).
