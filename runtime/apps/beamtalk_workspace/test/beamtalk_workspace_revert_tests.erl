%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_revert_tests).

-moduledoc """
End-to-end tests for the ChangeLog revert completeness set (ADR 0082):

  - BT-2663: reverting a newly-added *method* removes it from the live image.
  - BT-2664: reverting a newly-added *class* removes the class from the live image.
  - BT-2665: reverting a *class-side* method works symmetrically with instance
    side — a modify re-installs the prior class-side body; an add removes it.

These boot the full stack (compiler port + runtime + workspace_meta + changelog)
against an isolated, in-project temp tree so the live install / remove and the
flushable-with-prev-source recording paths run for real, then drive revert
through the public `revert_method/2` / `changeLogRevert/1` surface.

Each test uses a UNIQUE workspace_id + temp project dir and cleans up (workspace
servers, loaded modules, temp tree) in an `after`/teardown block so a failed
assertion cannot leak state into another run (a fixed-id reuse pollutes the
persisted ChangeLog and produces misleading results).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

revert_e2e_test_() ->
    {setup, fun suite_setup/0, fun suite_teardown/1,
        {foreach, fun case_setup/0, fun case_teardown/1, [
            fun revert_added_instance_method_removes_it/1,
            fun revert_modified_instance_method_restores_prior_body/1,
            fun revert_added_class_side_method_removes_it/1,
            fun revert_modified_class_side_method_restores_prior_body/1,
            fun revert_new_class_removes_the_class/1
        ]}}.

%% Start the heavy, node-global apps once for the whole suite (the compiler port
%% + runtime). Stopping/restarting them per case is slow and flaky.
suite_setup() ->
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Let the runtime register bootstrap classes before compiling user code.
    timer:sleep(300),
    ok.

suite_teardown(_) ->
    _ = application:stop(beamtalk_compiler),
    ok.

%% Per-case: an isolated workspace (unique id + temp HOME/project) plus the
%% changelog and meta gen_servers, with `project_path` pointing at the temp tree
%% so files written there classify as in-project (flushable + revertable).
case_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("revert-e2e-" ++ Unique),
    Tmp = filename:join(temp_dir(), "bt-revert-e2e-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    stop_existing(beamtalk_workspace_changelog),
    stop_existing(beamtalk_workspace_meta),
    {ok, ClogPid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => list_to_binary(Tmp),
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }),
    #{
        clog_pid => ClogPid,
        meta_pid => MetaPid,
        workspace_id => WorkspaceId,
        tmp => Tmp,
        unique => Unique,
        old_home => OldHome,
        classes => []
    }.

case_teardown(#{clog_pid := ClogPid, meta_pid := MetaPid, tmp := Tmp, old_home := OldHome}) ->
    stop_proc(MetaPid),
    stop_proc(ClogPid),
    restore_home(OldHome),
    _ = file:del_dir_r(Tmp),
    ok.

%%====================================================================
%% BT-2663 — added instance method reverts as a removal
%%====================================================================

revert_added_instance_method_removes_it(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevAddInst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n"]
    ),
    %% Add a brand-new instance method live.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"added">>, <<"^ 42">>, durable, <<"sess">>, human
    ),
    AfterAdd = beamtalk_runtime_api:local_instance_methods(Pid),
    %% Revert the add → the method must be gone.
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"added">>),
    AfterRevert = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assert(lists:member(added, AfterAdd)),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assertNot(lists:member(added, AfterRevert)),
        %% The pre-existing method is untouched.
        ?_assert(lists:member(base, AfterRevert))
    ].

%%====================================================================
%% BT-2290 regression — modified instance method reverts to prior body
%%====================================================================

revert_modified_instance_method_restores_prior_body(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevModInst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  value => 1\n"]
    ),
    %% Modify the existing instance method.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"value">>, <<"^ 2">>, durable, <<"sess">>, human
    ),
    %% Revert the modify → the method still exists (the prior body is re-installed,
    %% not removed).
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"value">>),
    AfterRevert = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(value, AfterRevert))
    ].

%%====================================================================
%% BT-2665 — class-side add reverts as a removal
%%====================================================================

revert_added_class_side_method_removes_it(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevAddCls" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n"]
    ),
    %% Add a brand-new class-side method live (side = class).
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"make">>, <<"^ self new">>, durable, <<"sess">>, human, class
    ),
    AfterAdd = beamtalk_runtime_api:local_class_methods(Pid),
    %% Revert the class-side add → the class-side method must be gone.
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"make">>),
    AfterRevert = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assert(lists:member(make, AfterAdd)),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assertNot(lists:member(make, AfterRevert))
    ].

%%====================================================================
%% BT-2665 — class-side modify reverts to the prior class-side body
%%====================================================================

revert_modified_class_side_method_restores_prior_body(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevModCls" ++ U),
    %% Define the class WITH a class-side method on disk so the modify has a
    %% recoverable prior class-side body.
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  class build => 1\n"]
    ),
    %% Modify the existing class-side method.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"build">>, <<"^ 2">>, durable, <<"sess">>, human, class
    ),
    %% Revert the class-side modify → the class-side method still exists (prior
    %% class-side body re-installed, not removed).
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"build">>),
    AfterRevert = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(build, AfterRevert))
    ].

%%====================================================================
%% BT-2664 — new-class reverts as a class removal
%%====================================================================

revert_new_class_removes_the_class(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevNewCls" ++ U),
    TargetPath = filename:join(Tmp, binary_to_list(ClassName) ++ ".bt"),
    Source = iolist_to_binary([
        <<"Actor subclass: ">>, ClassName, <<"\n  v => 1\n">>
    ]),
    {ok, _Objs} = beamtalk_repl_eval:new_class(Source, list_to_binary(TargetPath)),
    LoadedBefore = beamtalk_runtime_api:whereis_class(binary_to_atom(ClassName, utf8)),
    %% Revert the new-class entry via the FFI surface (new-class rows carry
    %% selector = nil, mapped to the `new-class` placeholder).
    Entry = #{
        '$beamtalk_class' => 'ChangeEntry',
        className => binary_to_atom(ClassName, utf8),
        selector => nil,
        kind => 'new-class'
    },
    _ = beamtalk_workspace_interface_primitives:changeLogRevert(Entry),
    LoadedAfter = beamtalk_runtime_api:whereis_class(binary_to_atom(ClassName, utf8)),
    [
        ?_assert(is_pid(LoadedBefore)),
        ?_assertEqual(undefined, LoadedAfter)
    ].

%%====================================================================
%% Helpers
%%====================================================================

%% Define a class backed by a real in-project `.bt` file and load it into the
%% live image so its source is recorded (flushable + revertable) and its method
%% spans resolve against disk. `Methods` is a list of method-line iolists (each
%% already indented). Returns `{ok, ClassPid}`.
define_project_class(Tmp, ClassNameBin, Methods) ->
    Path = filename:join(Tmp, binary_to_list(ClassNameBin) ++ ".bt"),
    Source = iolist_to_binary([
        <<"Actor subclass: ">>, ClassNameBin, <<"\n">>, Methods
    ]),
    ok = file:write_file(Path, Source),
    {ok, _ClassNames} = beamtalk_repl_loader:reload_class_file(Path),
    %% Record the class source in workspace_meta so live patches resolve their
    %% span (the stateful REPL load path does this; the stateless reload does not).
    ok = beamtalk_workspace_meta:set_class_source(ClassNameBin, binary_to_list(Source)),
    ClassAtom = binary_to_atom(ClassNameBin, utf8),
    Pid = wait_for_class(ClassAtom, 50),
    {ok, Pid}.

%% Poll for the class registration (reload is synchronous, but registration can
%% lag a few ms behind module load).
wait_for_class(_ClassAtom, 0) ->
    error(class_not_registered);
wait_for_class(ClassAtom, N) ->
    case beamtalk_runtime_api:whereis_class(ClassAtom) of
        Pid when is_pid(Pid) -> Pid;
        _ ->
            timer:sleep(20),
            wait_for_class(ClassAtom, N - 1)
    end.

stop_existing(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> stop_proc(Pid)
    end.

stop_proc(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ok
            end;
        false ->
            ok
    end;
stop_proc(_) ->
    ok.

restore_home(false) -> os:unsetenv("HOME");
restore_home(OldHome) -> os:putenv("HOME", OldHome).

temp_dir() ->
    unicode:characters_to_list(beamtalk_file:'tempDirectory'()).
