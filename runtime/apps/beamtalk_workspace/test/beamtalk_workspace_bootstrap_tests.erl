%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_workspace_bootstrap module (ADR 0019 Phase 2).
%%%
%%% Tests the bootstrap worker that wires singleton class variables
%%% during workspace startup.

-module(beamtalk_workspace_bootstrap_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test helpers
%%====================================================================

ensure_runtime() ->
    application:ensure_all_started(beamtalk_runtime).

cleanup_singleton(Name) ->
    case erlang:whereis(Name) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            gen_server:stop(Pid, normal, 1000),
            timer:sleep(50)
    end.

cleanup_bootstrap() ->
    case erlang:whereis(beamtalk_workspace_bootstrap) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            gen_server:stop(Pid, normal, 1000),
            timer:sleep(50)
    end.

cleanup_all() ->
    cleanup_bootstrap(),
    cleanup_singleton('Transcript'),
    cleanup_singleton('Beamtalk'),
    cleanup_singleton('Workspace').

%%====================================================================
%% Tests
%%====================================================================

%% Test that bootstrap wires transcript singleton
bootstrap_sets_transcript_class_var_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, TPid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
              {ok, _BPid} = beamtalk_workspace_bootstrap:start_link(),
              ?assertEqual(TPid, whereis('Transcript'))
          end)]
     end}.

%% Test that bootstrap wires system dictionary singleton
bootstrap_sets_beamtalk_class_var_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_system_dictionary:start_link({local, 'Beamtalk'}, []),
              {ok, _} = beamtalk_workspace_bootstrap:start_link(),
              ?assertEqual(BPid, whereis('Beamtalk'))
          end)]
     end}.

%% Test that bootstrap wires workspace actor singleton
bootstrap_sets_workspace_class_var_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, WPid} = beamtalk_workspace_environment:start_link({local, 'Workspace'}),
              {ok, _} = beamtalk_workspace_bootstrap:start_link(),
              ?assertEqual(WPid, whereis('Workspace'))
          end)]
     end}.

%% Test that bootstrap sets class variables when classes are loaded
bootstrap_sets_class_variables_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, TPid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
              {ok, _} = beamtalk_workspace_bootstrap:start_link(),
              case beamtalk_class_registry:whereis_class('TranscriptStream') of
                  undefined -> ok;
                  _ClassPid ->
                      ExpectedObj = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, TPid},
                      ?assertEqual(ExpectedObj,
                          gen_server:call(beamtalk_class_registry:whereis_class('TranscriptStream'),
                              {get_class_var, current}))
              end
          end)]
     end}.

%% Test that bootstrap handles missing singletons gracefully
bootstrap_missing_singleton_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, _} = beamtalk_workspace_bootstrap:start_link(),
              ok
          end)]
     end}.

%% Test that bootstrap re-wires after singleton restart
bootstrap_restart_rewires_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     {timeout, 10,
      fun() ->
          OldTrap = process_flag(trap_exit, true),
          try
              {ok, TPid1} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
              {ok, _BPid} = beamtalk_workspace_bootstrap:start_link(),
              ?assertEqual(TPid1, whereis('Transcript')),
              unlink(TPid1),
              exit(TPid1, kill),
              timer:sleep(50),
              {ok, TPid2} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
              ?assertNotEqual(TPid1, TPid2),
              timer:sleep(300),
              ?assertEqual(TPid2, whereis('Transcript'))
          after
              process_flag(trap_exit, OldTrap),
              receive {'EXIT', _, _} -> ok after 0 -> ok end
          end
      end}}.
