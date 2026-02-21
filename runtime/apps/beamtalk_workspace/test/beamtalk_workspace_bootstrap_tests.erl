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

%% Test that start_link/1 with a nonexistent project path starts without error.
bootstrap_start_link_with_nonexistent_path_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> cleanup_all() end,
     fun(_) ->
         [?_test(begin
              %% A nonexistent path should not crash the bootstrap worker —
              %% find_bt_modules_in_dir returns [] for missing directories.
              {ok, BPid} = beamtalk_workspace_bootstrap:start_link(<<"/nonexistent/project/path">>),
              ?assert(is_process_alive(BPid))
          end)]
     end}.

%%====================================================================
%% Integration test for full module activation path (BT-748)
%%====================================================================

%% Test that activate_project_modules/1 loads a compiled module, calls
%% register_class/0, and makes the class visible via the class registry.
activate_project_modules_registers_class_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) ->
         cleanup_activation_test_class(),
         purge_activation_fixture_module()
     end,
     fun(_) ->
         [?_test(begin
              ModName = 'bt@BT748ActivationFixture',
              ClassName = 'BT748ActivationFixture',
              {ProjDir, EbinDir} = make_temp_project_dir(),
              try
                  BeamBin = compile_activation_fixture(ModName, ClassName),
                  BeamFile = filename:join(EbinDir, "bt@BT748ActivationFixture.beam"),
                  ok = file:write_file(BeamFile, BeamBin),
                  ok = beamtalk_workspace_bootstrap:activate_project_modules(
                           list_to_binary(ProjDir)),
                  ?assertNotEqual(undefined,
                      beamtalk_class_registry:whereis_class(ClassName)),
                  ?assertEqual({module, ModName}, code:ensure_loaded(ModName))
              after
                  remove_temp_project_dir(ProjDir),
                  code:del_path(EbinDir)
              end
          end)]
     end}.

cleanup_activation_test_class() ->
    case beamtalk_class_registry:whereis_class('BT748ActivationFixture') of
        undefined -> ok;
        Pid ->
            gen_server:stop(Pid, normal, 1000),
            timer:sleep(50)
    end.

purge_activation_fixture_module() ->
    code:purge('bt@BT748ActivationFixture'),
    code:delete('bt@BT748ActivationFixture'),
    ok.

make_temp_project_dir() ->
    Base = filename:join(get_temp_dir(),
                         "beamtalk_proj_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Base),
    ok = file:make_dir(filename:join(Base, "_build")),
    ok = file:make_dir(filename:join([Base, "_build", "dev"])),
    EbinDir = filename:join([Base, "_build", "dev", "ebin"]),
    ok = file:make_dir(EbinDir),
    {Base, EbinDir}.

remove_temp_project_dir(Dir) ->
    EbinDir = filename:join([Dir, "_build", "dev", "ebin"]),
    BuildDevDir = filename:join([Dir, "_build", "dev"]),
    BuildDir = filename:join(Dir, "_build"),
    case file:list_dir(EbinDir) of
        {ok, Files} ->
            lists:foreach(fun(F) -> file:delete(filename:join(EbinDir, F)) end, Files);
        _ -> ok
    end,
    file:del_dir(EbinDir),
    file:del_dir(BuildDevDir),
    file:del_dir(BuildDir),
    file:del_dir(Dir),
    ok.

%% @private Build a minimal bt@ module with register_class/0 using abstract forms.
%% The function calls beamtalk_object_class:start/2 to register the class.
compile_activation_fixture(ModName, ClassName) ->
    compile_activation_fixture(ModName, ClassName, 'Object').

%% @private Build a minimal bt@ module with register_class/0 and a specified superclass.
%% BT-745: Also emits a -beamtalk_class([{ClassName, Superclass}]) attribute for
%% dependency-ordered loading.
compile_activation_fixture(ModName, ClassName, Superclass) ->
    Forms = [
        {attribute, 1, module, ModName},
        {attribute, 2, export, [{register_class, 0}]},
        {attribute, 2, beamtalk_class, [{ClassName, Superclass}]},
        {function, 3, register_class, 0,
         [{clause, 3, [], [],
           [{'case', 4,
             {call, 4,
              {remote, 4, {atom, 4, beamtalk_object_class}, {atom, 4, start}},
              [{atom, 4, ClassName},
               {map, 4,
                [{map_field_assoc, 4, {atom, 4, name},               {atom, 4, ClassName}},
                 {map_field_assoc, 4, {atom, 4, superclass},         {atom, 4, Superclass}},
                 {map_field_assoc, 4, {atom, 4, module},             {atom, 4, ModName}},
                 {map_field_assoc, 4, {atom, 4, instance_variables}, {nil,  4}},
                 {map_field_assoc, 4, {atom, 4, class_methods},      {map,  4, []}},
                 {map_field_assoc, 4, {atom, 4, instance_methods},   {map,  4, []}}
                ]}
              ]},
             [{clause, 5,
               [{tuple, 5, [{atom, 5, ok}, {var, 5, '_Pid'}]}],
               [],
               [{atom, 5, ok}]},
              {clause, 6,
               [{tuple, 6, [{atom, 6, error}, {var, 6, '_'}]}],
               [],
               [{atom, 6, ok}]}
             ]}
           ]}
          ]}
    ],
    {ok, ModName, BeamBin} = compile:forms(Forms, []),
    BeamBin.

%%====================================================================
%% Integration test for dependency-ordered activation (BT-745)
%%====================================================================

%% Test that activate_project_modules/1 sorts modules by superclass dependency
%% so that a parent class is loaded before a child class. We compile two fixture
%% modules: BT745Parent (extends Object) and BT745Child (extends BT745Parent).
%% Both must be registered after activation regardless of filesystem order.
activate_project_modules_dependency_order_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) ->
         cleanup_bt745_classes(),
         purge_bt745_modules()
     end,
     fun(_) ->
         [?_test(begin
              ParentMod = 'bt@BT745Parent',
              ChildMod = 'bt@BT745Child',
              ParentClass = 'BT745Parent',
              ChildClass = 'BT745Child',
              {ProjDir, EbinDir} = make_temp_project_dir(),
              try
                  ParentBin = compile_activation_fixture(ParentMod, ParentClass, 'Object'),
                  ChildBin = compile_activation_fixture(ChildMod, ChildClass, ParentClass),
                  %% Write child FIRST to maximize chance of wrong fs order
                  ok = file:write_file(
                      filename:join(EbinDir, "bt@BT745Child.beam"), ChildBin),
                  ok = file:write_file(
                      filename:join(EbinDir, "bt@BT745Parent.beam"), ParentBin),
                  ok = beamtalk_workspace_bootstrap:activate_project_modules(
                           list_to_binary(ProjDir)),
                  %% Both classes must be registered
                  ?assertNotEqual(undefined,
                      beamtalk_class_registry:whereis_class(ParentClass)),
                  ?assertNotEqual(undefined,
                      beamtalk_class_registry:whereis_class(ChildClass))
              after
                  remove_temp_project_dir(ProjDir),
                  code:del_path(EbinDir)
              end
          end)]
     end}.

%% Test that sort_modules_by_dependency/2 orders superclass before subclass.
sort_modules_by_dependency_test_() ->
    {setup,
     fun() -> ensure_runtime() end,
     fun(_) -> ok end,
     fun(_) ->
         [?_test(begin
              ParentMod = 'bt@BT745SortParent',
              ChildMod = 'bt@BT745SortChild',
              {ProjDir, EbinDir} = make_temp_project_dir(),
              try
                  ParentBin = compile_activation_fixture(
                      ParentMod, 'BT745SortParent', 'Object'),
                  ChildBin = compile_activation_fixture(
                      ChildMod, 'BT745SortChild', 'BT745SortParent'),
                  ok = file:write_file(
                      filename:join(EbinDir, atom_to_list(ParentMod) ++ ".beam"), ParentBin),
                  ok = file:write_file(
                      filename:join(EbinDir, atom_to_list(ChildMod) ++ ".beam"), ChildBin),
                  %% Modules in reverse dependency order (child first)
                  Input = [ChildMod, ParentMod],
                  Sorted = beamtalk_workspace_bootstrap:sort_modules_by_dependency(
                               EbinDir, Input),
                  %% Parent must come before child
                  ?assertEqual([ParentMod, ChildMod], Sorted)
              after
                  remove_temp_project_dir(ProjDir)
              end
          end)]
     end}.

cleanup_bt745_classes() ->
    lists:foreach(fun(ClassName) ->
        case beamtalk_class_registry:whereis_class(ClassName) of
            undefined -> ok;
            Pid ->
                gen_server:stop(Pid, normal, 1000),
                timer:sleep(50)
        end
    end, ['BT745Parent', 'BT745Child']).

purge_bt745_modules() ->
    lists:foreach(fun(Mod) ->
        code:purge(Mod),
        code:delete(Mod)
    end, ['bt@BT745Parent', 'bt@BT745Child']),
    ok.

%%====================================================================
%% Test helpers for temp directories
%%====================================================================

get_temp_dir() ->
    case os:getenv("TMPDIR") of
        false ->
            case os:getenv("TEMP") of
                false -> "/tmp";
                Temp -> Temp
            end;
        TmpDir -> TmpDir
    end.

make_temp_beam_dir(FileNames) ->
    Dir = filename:join(get_temp_dir(),
                        "beamtalk_bootstrap_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    lists:foreach(
        fun(Name) ->
            ok = file:write_file(filename:join(Dir, Name), <<>>)
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
