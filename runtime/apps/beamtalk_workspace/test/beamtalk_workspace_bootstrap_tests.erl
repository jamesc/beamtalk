%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_workspace_bootstrap module (ADR 0019 Phase 2).
%%%
%%% Tests the bootstrap worker that wires singleton class variables
%%% and workspace convenience bindings during workspace startup.

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
    cleanup_singleton('Workspace'),
    %% Clean up persistent_term bindings
    lists:foreach(fun(Name) ->
        persistent_term:erase({beamtalk_binding, Name})
    end, ['Transcript', 'Beamtalk', 'Workspace']).

%%====================================================================
%% Tests
%%====================================================================

%% Test that beamtalk_workspace:set_binding/2 sets persistent_term bindings
set_binding_test() ->
    Value = {beamtalk_object, 'TestClass', test_module, self()},
    try
        beamtalk_workspace:set_binding('TestBinding', Value),
        ?assertEqual(Value, persistent_term:get({beamtalk_binding, 'TestBinding'}))
    after
        persistent_term:erase({beamtalk_binding, 'TestBinding'})
    end.

%% Test that bootstrap sets workspace bindings for transcript
bootstrap_sets_transcript_binding_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
             %% Start the transcript singleton
             {ok, TPid} = beamtalk_transcript_stream:start_link_singleton(1000),
             
             %% Start the bootstrap worker
             {ok, _BPid} = beamtalk_workspace_bootstrap:start_link(),
             
             %% Verify persistent_term binding was updated
             Binding = persistent_term:get({beamtalk_binding, 'Transcript'}),
             ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, TPid}, Binding)
         end)]
     end}.

%% Test that bootstrap sets workspace bindings for system dictionary
bootstrap_sets_beamtalk_binding_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
             %% Start the system dictionary singleton
             {ok, BPid} = beamtalk_system_dictionary:start_link_singleton(),
             
             %% Start the bootstrap worker
             {ok, _} = beamtalk_workspace_bootstrap:start_link(),
             
             %% Verify persistent_term binding was updated
             Binding = persistent_term:get({beamtalk_binding, 'Beamtalk'}),
             ?assertMatch({beamtalk_object, 'SystemDictionary', beamtalk_system_dictionary, BPid}, Binding)
         end)]
     end}.

%% Test that bootstrap sets workspace bindings for workspace actor
bootstrap_sets_workspace_binding_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
             %% Start the workspace actor singleton
             {ok, WPid} = beamtalk_workspace_actor:start_link_singleton(),
             
             %% Start the bootstrap worker
             {ok, _} = beamtalk_workspace_bootstrap:start_link(),
             
             %% Verify persistent_term binding was updated with WorkspaceEnvironment class name
             Binding = persistent_term:get({beamtalk_binding, 'Workspace'}),
             ?assertMatch({beamtalk_object, 'WorkspaceEnvironment', beamtalk_workspace_actor, WPid}, Binding)
         end)]
     end}.

%% Test that bootstrap sets class variables when classes are loaded
bootstrap_sets_class_variables_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
             %% Start a transcript singleton
             {ok, TPid} = beamtalk_transcript_stream:start_link_singleton(1000),
             
             %% Start bootstrap
             {ok, _} = beamtalk_workspace_bootstrap:start_link(),
             
             %% Check if TranscriptStream class exists
             case beamtalk_object_class:whereis_class('TranscriptStream') of
                 undefined ->
                     %% Class not loaded in unit tests — that's expected
                     ok;
                 _ClassPid ->
                     %% Class is loaded — verify class var was set
                     ExpectedObj = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, TPid},
                     ?assertEqual(ExpectedObj,
                         gen_server:call(beamtalk_object_class:whereis_class('TranscriptStream'),
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
             %% No singletons started — bootstrap should not crash
             {ok, _} = beamtalk_workspace_bootstrap:start_link(),
             ok
         end)]
     end}.

%% Test that bootstrap re-sets bindings after singleton restart
bootstrap_restart_rewires_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     {timeout, 10,
      fun() ->
          OldTrap = process_flag(trap_exit, true),
          try
              %% Start transcript singleton
              {ok, TPid1} = beamtalk_transcript_stream:start_link_singleton(1000),
              
              %% Start bootstrap
              {ok, _BPid} = beamtalk_workspace_bootstrap:start_link(),
              
              %% Verify initial binding
              Binding1 = persistent_term:get({beamtalk_binding, 'Transcript'}),
              ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, TPid1}, Binding1),
              
              %% Kill the transcript singleton (unlink first to avoid test crash)
              unlink(TPid1),
              exit(TPid1, kill),
              timer:sleep(50),
              
              %% Restart the transcript singleton with a new pid
              {ok, TPid2} = beamtalk_transcript_stream:start_link_singleton(1000),
              ?assertNotEqual(TPid1, TPid2),
              
              %% Wait for the rebootstrap (100ms delay + processing)
              timer:sleep(300),
              
              %% Verify binding was re-set with new PID
              Binding2 = persistent_term:get({beamtalk_binding, 'Transcript'}),
              ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, TPid2}, Binding2)
          after
              process_flag(trap_exit, OldTrap),
              %% Drain any EXIT messages
              receive {'EXIT', _, _} -> ok after 0 -> ok end
          end
      end}}.
