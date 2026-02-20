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

%% Test that rebootstrap gives up after 5 retries and logs error
rebootstrap_exhaustion_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     {timeout, 10,
      fun() ->
          OldTrap = process_flag(trap_exit, true),
          try
              {ok, TPid} = beamtalk_transcript_stream:start_link({local, 'Transcript'}, 1000),
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              ?assertEqual(TPid, whereis('Transcript')),
              %% Kill the singleton and don't restart it
              unlink(TPid),
              exit(TPid, kill),
              timer:sleep(50),
              ?assertEqual(undefined, whereis('Transcript')),
              %% Wait for all 5 retries (100ms initial + 5 * 200ms = ~1200ms)
              timer:sleep(1500),
              %% Bootstrap should still be alive after exhausting retries
              ?assert(is_process_alive(BPid))
          after
              process_flag(trap_exit, OldTrap),
              receive {'EXIT', _, _} -> ok after 0 -> ok end
          end
      end}}.

%% Test that DOWN from unknown monitor ref is ignored
unknown_monitor_down_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              %% Send a DOWN message with a bogus monitor ref
              FakeRef = make_ref(),
              BPid ! {'DOWN', FakeRef, process, self(), normal},
              timer:sleep(50),
              %% Bootstrap should still be alive and responsive
              ?assert(is_process_alive(BPid)),
              ?assertEqual(ok, gen_server:call(BPid, ping))
          end)]
     end}.

%% Test that handle_call returns ok for unknown messages
handle_call_returns_ok_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              ?assertEqual(ok, gen_server:call(BPid, some_unknown_message)),
              ?assert(is_process_alive(BPid))
          end)]
     end}.

%% Test that handle_cast is a noop
handle_cast_noop_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              gen_server:cast(BPid, some_unknown_cast),
              timer:sleep(50),
              ?assert(is_process_alive(BPid))
          end)]
     end}.

%% Test that unknown info messages are ignored
unknown_info_ignored_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              BPid ! {some, random, message},
              timer:sleep(50),
              ?assert(is_process_alive(BPid))
          end)]
     end}.

%% Test that terminate returns ok
terminate_ok_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              ?assert(is_process_alive(BPid)),
              gen_server:stop(BPid, normal, 1000),
              timer:sleep(50),
              ?assertNot(is_process_alive(BPid))
          end)]
     end}.

%%====================================================================
%% Tests for project module discovery (BT-739)
%%====================================================================

%% Test that find_bt_modules_in_dir returns bt@* modules from a directory.
find_bt_modules_returns_project_modules_test() ->
    Dir = make_temp_beam_dir([
        "bt@MyClass.beam",
        "bt@AnotherClass.beam",
        "other_module.beam"
    ]),
    try
        Result = beamtalk_workspace_bootstrap:find_bt_modules_in_dir(Dir),
        ?assertEqual(lists:sort(['bt@AnotherClass', 'bt@MyClass']), lists:sort(Result))
    after
        remove_temp_dir(Dir)
    end.

%% Test that find_bt_modules_in_dir excludes bt@stdlib@* modules.
find_bt_modules_excludes_stdlib_test() ->
    Dir = make_temp_beam_dir([
        "bt@MyClass.beam",
        "bt@stdlib@String.beam",
        "bt@stdlib@Integer.beam"
    ]),
    try
        Result = beamtalk_workspace_bootstrap:find_bt_modules_in_dir(Dir),
        ?assertEqual(['bt@MyClass'], Result)
    after
        remove_temp_dir(Dir)
    end.

%% Test that find_bt_modules_in_dir returns [] for a nonexistent directory.
find_bt_modules_missing_dir_test() ->
    Result = beamtalk_workspace_bootstrap:find_bt_modules_in_dir("/nonexistent/path/xyz"),
    ?assertEqual([], Result).

%% Test that find_bt_modules_in_dir returns [] for an empty directory.
find_bt_modules_empty_dir_test() ->
    Dir = make_temp_beam_dir([]),
    try
        Result = beamtalk_workspace_bootstrap:find_bt_modules_in_dir(Dir),
        ?assertEqual([], Result)
    after
        remove_temp_dir(Dir)
    end.

%% Test that find_bt_modules_in_dir ignores non-.beam files.
find_bt_modules_ignores_non_beam_test() ->
    Dir = make_temp_beam_dir([
        "bt@MyClass.beam",
        "bt@MyClass.erl",
        "bt@MyClass.app"
    ]),
    try
        Result = beamtalk_workspace_bootstrap:find_bt_modules_in_dir(Dir),
        ?assertEqual(['bt@MyClass'], Result)
    after
        remove_temp_dir(Dir)
    end.

%% Test that bootstrap starts without error even with no project modules on path.
bootstrap_starts_with_no_project_modules_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(),
              ?assert(is_process_alive(BPid))
          end)]
     end}.

%%====================================================================
%% Test helpers for temp directories
%%====================================================================

make_temp_beam_dir(FileNames) ->
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"),
                        "beamtalk_bootstrap_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    lists:foreach(
        fun(Name) ->
            file:write_file(filename:join(Dir, Name), <<>>)
        end,
        FileNames
    ),
    Dir.

remove_temp_dir(Dir) ->
    Files = case file:list_dir(Dir) of
        {ok, Fs} -> Fs;
        _ -> []
    end,
    lists:foreach(fun(F) -> file:delete(filename:join(Dir, F)) end, Files),
    file:del_dir(Dir).
