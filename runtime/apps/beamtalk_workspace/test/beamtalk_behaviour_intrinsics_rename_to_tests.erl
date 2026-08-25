%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_intrinsics_rename_to_tests).

-moduledoc """
Integration tests for `beamtalk_behaviour_intrinsics:classRenameTo/2` (ADR
0114 Phase 2, BT-3278) against a real, in-project fixture graph.

Mirrors `beamtalk_repl_loader_rewrite_sites_tests.erl`'s fixture pattern
exactly (BT-3270's own test module for the shared rewrite mechanism this
primitive calls): real `.bt` files on disk, loaded through the real
file-compile path (`beamtalk_repl_loader:handle_load/2`), with
`beamtalk_workspace_meta`'s `project_path` set to the fixture directory so
`classify_source_file/1` classifies these files as in-project/flushable
rather than "dependency" — the one classification `stdlib/test/
rename_to_test.bt`'s BUnit tests cannot exercise (BUnit's `beamtalk test`
runs with no project configured at all, so every ordinary compiled `.bt`
class there classifies as "dependency" and is correctly refused; see that
file's own doc for the finding). This module is where `renameTo:`'s
ordinary-project-class success path — declaration-header rewrite,
`referencesTo:`/`direct_subclasses:` site discovery, and the resulting
`'rename-class'` ChangeLog entry's `sites` list — actually gets exercised
end-to-end.

## Fixture graph

`bt3278_widget.bt`:
```
Value subclass: Bt3278Widget
  greet -> String => "hi"
```

`bt3278_widget_sub.bt` (direct subclass — exercises the superclass-
declaration reference `referencesTo:` does NOT cover, per ADR 0114 §
Decision):
```
Bt3278Widget subclass: Bt3278WidgetSub
  extra -> Integer => 1
```

`bt3278_widget_user.bt` (exercises `referencesTo:` itself — two mentions of
`Bt3278Widget` on one line, the site-discovery spike's documented
same-line-collapse case):
```
Value subclass: Bt3278WidgetUser
  makeWidget -> Bt3278Widget => Bt3278Widget new
```

Renaming `Bt3278Widget` to `Bt3278WidgetRenamed` therefore has 4 sites: the
definition (`bt3278_widget.bt`'s own header), the subclass's superclass
reference (`bt3278_widget_sub.bt`'s header), and two reference-site
occurrences within `bt3278_widget_user.bt`'s single `makeWidget` method (the
return-type annotation and the constructor send) — `classRenameTo/2` scans
each referencing method's own resolved span for every whole-word occurrence
of the old name, not just once per `referencesTo:` row, so a same-line
double mention like this one still produces two independently-spliced
sites, not one.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixture sources
%%====================================================================

widget_source() ->
    <<
        "Value subclass: Bt3278Widget\n"
        "  greet -> String => \"hi\""
    >>.

widget_sub_source() ->
    <<
        "Bt3278Widget subclass: Bt3278WidgetSub\n"
        "  extra -> Integer => 1"
    >>.

widget_user_source() ->
    <<
        "Value subclass: Bt3278WidgetUser\n"
        "  makeWidget -> Bt3278Widget => Bt3278Widget new"
    >>.

%%====================================================================
%% Integration fixture: real compiler port + workspace_meta, real files
%%====================================================================

setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    Unique = integer_to_list(erlang:unique_integer([positive])),
    ProjDir = filename:join(temp_dir(), "bt-rename-to-" ++ Unique),
    ok = filelib:ensure_path(ProjDir),
    WidgetPath = filename:join(ProjDir, "bt3278_widget.bt"),
    WidgetSubPath = filename:join(ProjDir, "bt3278_widget_sub.bt"),
    WidgetUserPath = filename:join(ProjDir, "bt3278_widget_user.bt"),
    ok = file:write_file(WidgetPath, widget_source()),
    ok = file:write_file(WidgetSubPath, widget_sub_source()),
    ok = file:write_file(WidgetUserPath, widget_user_source()),
    %% `project_path` set to the fixture directory so `classify_source_file/1`
    %% classifies these files as in-project/flushable rather than
    %% "dependency" — see this module's own doc for why that matters here.
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"rename_to_test_ws">>,
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        repl => false
    }),
    beamtalk_compiler_server:clear_classes(),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _WidgetClasses, _State1} = beamtalk_repl_loader:handle_load(WidgetPath, State0),
    {ok, _WidgetSubClasses, _State2} = beamtalk_repl_loader:handle_load(WidgetSubPath, State0),
    {ok, _WidgetUserClasses, _State3} = beamtalk_repl_loader:handle_load(WidgetUserPath, State0),
    #{
        proj_dir => ProjDir,
        widget_path => WidgetPath,
        widget_sub_path => WidgetSubPath,
        widget_user_path => WidgetUserPath
    }.

teardown(_) ->
    lists:foreach(
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    catch gen_server:stop(Pid, normal, 5000)
            end
        end,
        ['Bt3278Widget', 'Bt3278WidgetRenamed', 'Bt3278WidgetSub', 'Bt3278WidgetUser']
    ),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    ok.

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

widget_class_object() ->
    beamtalk_class_registry:class_object_from_pid(
        beamtalk_class_registry:whereis_class('Bt3278Widget')
    ).

%%====================================================================
%% Full success path: rename + re-registration + reference-site rewrite
%%====================================================================

rename_to_success_test_() ->
    {setup, fun setup/0, fun teardown/1, fun rename_to_success/1}.

rename_to_success(_Fixture) ->
    OldPid = beamtalk_class_registry:whereis_class('Bt3278Widget'),
    Result = beamtalk_behaviour_intrinsics:classRenameTo(
        widget_class_object(), 'Bt3278WidgetRenamed'
    ),

    NewPid = beamtalk_class_registry:whereis_class('Bt3278WidgetRenamed'),

    [
        %% Re-registered under the new name; old name is gone.
        ?_assertMatch(#beamtalk_object{class = 'Bt3278WidgetRenamed class'}, Result),
        ?_assertEqual(undefined, beamtalk_class_registry:whereis_class('Bt3278Widget')),
        ?_assertMatch(P when is_pid(P), NewPid),
        %% Ordinary (project) class rename installs a FRESH class-object
        %% process via the normal compile/activate pipeline — not the same
        %% pid the old registration used (see install_class_rename/3's doc).
        ?_assert(OldPid =/= NewPid),
        %% The definition site was rewritten: the class's own tracked source
        %% now declares the new name.
        ?_assertEqual(
            <<"Value subclass: Bt3278WidgetRenamed\n  greet -> String => \"hi\"">>,
            unicode:characters_to_binary(
                beamtalk_workspace_meta:get_class_source(<<"Bt3278WidgetRenamed">>)
            )
        ),
        %% The subclass's superclass-declaration header was rewritten too —
        %% the one reference kind `referencesTo:` doesn't cover.
        ?_assertEqual(
            <<"Bt3278WidgetRenamed subclass: Bt3278WidgetSub\n  extra -> Integer => 1">>,
            unicode:characters_to_binary(
                beamtalk_workspace_meta:get_class_source(<<"Bt3278WidgetSub">>)
            )
        ),
        ?_assertEqual(
            'Bt3278WidgetRenamed',
            gen_server:call(beamtalk_class_registry:whereis_class('Bt3278WidgetSub'), superclass)
        ),
        %% The reference site (both same-line occurrences) was rewritten —
        %% and the rewrite is genuinely live: the recompiled+hot-reloaded
        %% referrer now constructs an instance of the RENAMED class.
        ?_assertEqual(
            <<
                "Value subclass: Bt3278WidgetUser\n"
                "  makeWidget -> Bt3278WidgetRenamed => Bt3278WidgetRenamed new"
            >>,
            unicode:characters_to_binary(
                beamtalk_workspace_meta:get_class_source(<<"Bt3278WidgetUser">>)
            )
        ),
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Bt3278WidgetUser new makeWidget class name",
                beamtalk_repl_state:new(undefined, 0)
            )
        )
    ].

%%====================================================================
%% ChangeLog entry: 'rename-class' kind, 4 sites (definition + subclass
%% header + 2 same-line reference occurrences), flushable (real project
%% files).
%%====================================================================

rename_to_changelog_test_() ->
    {setup, fun setup_with_changelog/0, fun teardown_with_changelog/1, fun rename_to_changelog/1}.

setup_with_changelog() ->
    Fixture = setup(),
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    %% A real (non-run-mode) ChangeLog needs a `workspace_id` AND a HOME it
    %% can resolve `<home>/.beamtalk/workspaces/<id>/changes/` under — mirrors
    %% beamtalk_repl_loader_rewrite_sites_tests.erl's identical setup.
    %%
    %% `erlang:unique_integer/1` alone is NOT enough entropy here (unlike its
    %% usual per-VM-run uses elsewhere in this codebase): its counter resets
    %% on every fresh `rebar3 eunit` invocation, and this call site is reached
    %% after a deterministic, fixed number of prior `unique_integer` calls —
    %% so two SEPARATE test runs can compute the IDENTICAL `WorkspaceId`/
    %% `ChangelogHome`. Observed directly (review follow-up, PR #3523): the
    %% real `~/.beamtalk/workspaces/<id>/changes/changes.jsonl` these resolve
    %% to persists across runs, so `beamtalk_workspace_changelog`'s own
    %% intentional `load_from_disk` restores a PRIOR run's leftover entries
    %% into this run's ETS table, and `[Entry] = entries()` intermittently
    %% sees more than one. `os:getpid()` (the OS process id — genuinely
    %% distinct per separate VM invocation, unlike the in-VM counter) closes
    %% the gap.
    Unique = os:getpid() ++ "-" ++ integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("bt-rename-to-changelog-" ++ Unique),
    ChangelogHome = filename:join(temp_dir(), "bt-rename-to-changelog-home-" ++ Unique),
    ok = filelib:ensure_path(ChangelogHome),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", ChangelogHome),
    {ok, _} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    Fixture#{old_home => OldHome}.

teardown_with_changelog(#{old_home := OldHome} = Fixture) ->
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    case OldHome of
        false -> os:unsetenv("HOME");
        _ -> os:putenv("HOME", OldHome)
    end,
    teardown(Fixture).

rename_to_changelog(_Fixture) ->
    _ = beamtalk_behaviour_intrinsics:classRenameTo(widget_class_object(), 'Bt3278WidgetRenamed'),
    [Entry] = beamtalk_workspace_changelog:entries(),
    Sites = beamtalk_workspace_changelog:entry_sites(Entry),
    [
        ?_assertEqual('rename-class', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(<<"Bt3278WidgetRenamed">>, beamtalk_workspace_changelog:entry_class(Entry)),
        ?_assertEqual(<<"Bt3278Widget">>, beamtalk_workspace_changelog:entry_old_class(Entry)),
        ?_assertEqual(true, beamtalk_workspace_changelog:entry_flushable(Entry)),
        %% sites[0] is the definition, sites[1..] are the subclass header +
        %% the two reference-site occurrences.
        ?_assertEqual(4, length(Sites))
    ].
