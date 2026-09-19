%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_shape_recheck_tests).

-moduledoc """
End-to-end wiring tests for ADR 0105 Phase 2: a real class-body
reload (`beamtalk_repl_loader:handle_load/2`, twice against the same file
path) through `beamtalk_workspace_shape_store`'s two-phase capture,
`beamtalk_recheck:trigger_shape/2`'s dependent lookup, and the same
publish/clearing path other reload-triggered re-checks built
(`beamtalk_workspace_findings_store` + the `'ReloadCheckCompleted'`
announcement).

Unlike `beamtalk_recheck_tests.erl`'s `trigger_shape/2` integration tests
(which supply a hand-built ambient `class_hierarchy` and call `trigger_shape/2`
directly), this module compiles and installs *real* `.bt` source twice,
so the ambient class-hierarchy cache the dependent's re-check reads is
populated by an actual `register_class/0` call — the same path production
takes. This is the layer `beamtalk_repl_loader_recheck_tests.erl` covers for
the method-signature path; this module is its shape
counterpart.
""".

-include_lib("eunit/include/eunit.hrl").

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

%%====================================================================
%% Integration fixture: real compiler port + xref + workspace_meta + stores
%%====================================================================

shape_loader_setup() ->
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
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"shape_loader_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    clear_xref(),
    beamtalk_compiler_server:clear_classes(),
    case whereis(beamtalk_workspace_shape_store) of
        undefined -> ok;
        ShapePid -> gen_server:stop(ShapePid)
    end,
    {ok, _} = beamtalk_workspace_shape_store:start_link(),
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        WorkerPid -> gen_server:stop(WorkerPid)
    end,
    {ok, _} = beamtalk_workspace_shape_recheck_worker:start_link(),
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
    end,
    {ok, _} = beamtalk_workspace_findings_store:start_link(),
    %% See beamtalk_repl_loader_recheck_tests:loader_recheck_setup/0's
    %% identical comment for why this must be up before any test subscribes.
    ok = beamtalk_announcements:ensure_started(),
    ok.

shape_loader_teardown(_) ->
    beamtalk_repl_subscriptions:unsubscribe(reload_check, self()),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    case whereis(beamtalk_workspace_shape_store) of
        undefined -> ok;
        ShapePid -> gen_server:stop(ShapePid)
    end,
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        WorkerPid -> gen_server:stop(WorkerPid)
    end,
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
    end,
    clear_xref(),
    _ = application:stop(beamtalk_compiler),
    ok.

clear_xref() ->
    case whereis(beamtalk_xref) of
        undefined ->
            ok;
        _Pid ->
            try
                sys:replace_state(beamtalk_xref, fun(S) ->
                    ets:delete_all_objects(beamtalk_xref_methods),
                    ets:delete_all_objects(beamtalk_xref_senders),
                    ets:delete_all_objects(beamtalk_xref_references),
                    ets:delete_all_objects(xref_class_gen),
                    S
                end)
            catch
                _:_ -> ok
            end,
            ok
    end.

subscribe_self_to_reload_check() ->
    ok = beamtalk_repl_subscriptions:subscribe(reload_check, self()).

receive_reload_check_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event} ->
            Event
    after 2000 ->
        error(timeout_waiting_for_reload_check_announcement)
    end.

%% ADR 0123 §4 (BT-3538): the shape-reload findings push — a distinct
%% announcement class from 'ReloadCheckCompleted', riding the same
%% `reload_check` subscription (`subscribe_self_to_reload_check/0` already
%% covers both, see `beamtalk_repl_subscriptions:announcement_classes/1`).
receive_shape_reload_findings_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ShapeReloadFindingsCompleted', _Handler, Event} ->
            Event
    after 2000 ->
        error(timeout_waiting_for_shape_reload_findings_announcement)
    end.

state0() -> beamtalk_repl_state:new(undefined, 0).

%%====================================================================
%% Fixtures
%%====================================================================

counter_source_gen1() ->
    <<
        "Actor subclass: LoaderShapeCounter\n"
        "  state: count :: Integer = 0\n"
        "  state: name :: String = \"\"\n"
    >>.

%% Generation 2: `name` removed.
counter_source_gen2() ->
    <<
        "Actor subclass: LoaderShapeCounter\n"
        "  state: count :: Integer = 0\n"
    >>.

dashboard_name_accessor_xref() ->
    [
        #{
            class_side => false,
            selector => 'cleanup:',
            line => 2,
            sends => [#{selector => name, line => 2, recv_kind => other}],
            references => [#{class => 'LoaderShapeCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

dashboard_name_accessor_source() ->
    <<
        "Object subclass: LoaderShapeDashboard\n"
        "  cleanup: c :: LoaderShapeCounter => c name\n"
    >>.

%%====================================================================
%% End-to-end: reload removes a state: slot, dependent gets flagged
%%====================================================================

removing_a_state_slot_flags_the_real_dependent_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_counter_~p.bt", [UniqueId])
                    ),

                    %% Generation 1: count + name. First-ever install of this
                    %% class this session — the shape store has nothing to
                    %% diff against yet, so this must NOT publish anything.
                    ok = file:write_file(Path, counter_source_gen1()),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),

                    %% Register the dependent AFTER generation 1 installs (so
                    %% its own source/xref registration is not itself part of
                    %% what generation 1's install triggers) but BEFORE
                    %% generation 2, since that is the reload trigger_shape/2
                    %% must find it for.
                    ok = beamtalk_xref:register_class(
                        'LoaderShapeDashboard', dashboard_name_accessor_xref()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"LoaderShapeDashboard">>, dashboard_name_accessor_source()
                    ),

                    %% Generation 2: `name` removed. The shape store already
                    %% holds generation 1 (captured post-install above), so
                    %% this reload diffs a real shape_change and triggers the
                    %% dependent re-check against the newly-installed ambient
                    %% hierarchy (no name slot).
                    ok = file:write_file(Path, counter_source_gen2()),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_reload_check_announcement(),
                    ?assertEqual(<<"LoaderShapeCounter">>, maps:get(changedClass, Event)),
                    ?assertEqual(shape_change, maps:get(classification, Event)),
                    ?assert(
                        lists:member(<<"LoaderShapeDashboard">>, maps:get(checkedOwners, Event))
                    ),

                    StoredFindings = beamtalk_workspace_findings_store:for_owner(
                        <<"LoaderShapeDashboard">>
                    ),
                    ?assertEqual(1, length(StoredFindings)),
                    [Finding] = StoredFindings,
                    ?assertEqual(shape_change, maps:get(classification, Finding)),
                    ?assertEqual(<<"name">>, maps:get(selector, Finding)),
                    ?assertEqual(<<"Dnu">>, maps:get(category, Finding))
                end)
            ]
        end}}.

%%====================================================================
%% End-to-end: a clean shape re-check still announces — an empty Findings
%% with non-empty CheckedOwners is the
%% "reload-fixes-reload" clearing signal, not silence.
%%====================================================================

%% `spawnWith:` (unlike a field's own accessor selectors) is *always* in
%% `trigger_shape/2`'s dependent-selector set regardless of classification
%% (`beamtalk_recheck:shape_dependent_selectors/1` — every field change,
%% `added` included, can flip a call site's key/value validity). Using it
%% here sidesteps a real gotcha: `Actor` classes never auto-generate field
%% accessors at all (only `ClassKind::Value` does — see
%% `beamtalk_recheck:field_accessor_atoms/1`'s doc), so a `c name`-style
%% dependent on an `Actor` receiver is *always* a DNU independent of any
%% shape change, which would not actually exercise a clean re-check.
%% Generation 2 only *adds* an unrelated `count` slot — `name` (the only key
%% the dependent's `spawnWith:` map uses) stays valid across both
%% generations, so the dependent recompiles clean.
clean_counter_source_gen1() ->
    <<
        "Actor subclass: LoaderShapeCleanCounter\n"
        "  state: name :: String = \"\"\n"
    >>.

clean_counter_source_gen2() ->
    <<
        "Actor subclass: LoaderShapeCleanCounter\n"
        "  state: name :: String = \"\"\n"
        "  state: count :: Integer = 0\n"
    >>.

clean_dashboard_spawn_with_xref() ->
    [
        #{
            class_side => false,
            selector => build,
            line => 2,
            sends => [#{selector => 'spawnWith:', line => 2, recv_kind => other}],
            references => [#{class => 'LoaderShapeCleanCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

%% `#name` is the only key used, and it is valid (declared, `String`-typed,
%% "x" is a `String` literal) in both generations — clean regardless of the
%% newly-added, unreferenced `count` slot.
clean_dashboard_spawn_with_source() ->
    <<
        "Object subclass: LoaderShapeCleanDashboard\n"
        "  build => LoaderShapeCleanCounter spawnWith: #{#name => \"x\"}\n"
    >>.

adding_an_unrelated_field_with_no_stale_dependents_still_announces_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_clean_counter_~p.bt", [UniqueId])
                    ),

                    %% Generation 1: first-ever install this session — no_op,
                    %% nothing published (same reasoning as the sibling test).
                    ok = file:write_file(Path, clean_counter_source_gen1()),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),

                    ok = beamtalk_xref:register_class(
                        'LoaderShapeCleanDashboard', clean_dashboard_spawn_with_xref()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"LoaderShapeCleanDashboard">>, clean_dashboard_spawn_with_source()
                    ),

                    %% Generation 2: an unrelated `count` slot is added. The
                    %% dependent is swept into trigger_shape/2's candidate
                    %% set (spawnWith: is always included, added changes
                    %% included) and re-checks clean — checked_owners is
                    %% non-empty, findings is empty.
                    ok = file:write_file(Path, clean_counter_source_gen2()),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_reload_check_announcement(),
                    ?assertEqual(<<"LoaderShapeCleanCounter">>, maps:get(changedClass, Event)),
                    ?assertEqual(shape_change, maps:get(classification, Event)),
                    ?assert(
                        lists:member(
                            <<"LoaderShapeCleanDashboard">>, maps:get(checkedOwners, Event)
                        )
                    ),
                    ?assertEqual([], maps:get(findings, Event)),

                    ?assertEqual(
                        [],
                        beamtalk_workspace_findings_store:for_owner(
                            <<"LoaderShapeCleanDashboard">>
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% End-to-end: a skipped (no live source) shape-change candidate gets its
%% pre-existing finding marked stale — the shape-path counterpart
%% of `beamtalk_repl_loader_recheck_tests:skipped_candidate_with_existing_finding_gets_marked_stale_test_`.
%% `do_trigger_shape/2` and `publish_shape_recheck_outcome/3` share
%% `not_verified_owners/2` and `mark_unverified_findings_stale/2` with the
%% instance-selector path, but nothing in this module exercised the
%% skipped/no-live-source outcome specifically until now.
%%====================================================================

%% Same shape as `clean_counter_source_gen1/2` above (an unrelated `count`
%% slot added in generation 2, `name` untouched) but under its own class
%% name — kept distinct from `LoaderShapeCleanCounter` so this test's
%% dependent-owner xref/seed fixtures below (which reference
%% `LoaderShapeSkippedCounter`) can't accidentally pass by matching the
%% wrong changed class.
skipped_counter_source_gen1() ->
    <<
        "Actor subclass: LoaderShapeSkippedCounter\n"
        "  state: name :: String = \"\"\n"
    >>.

skipped_counter_source_gen2() ->
    <<
        "Actor subclass: LoaderShapeSkippedCounter\n"
        "  state: name :: String = \"\"\n"
        "  state: count :: Integer = 0\n"
    >>.

%% `spawnWith:` (not a field accessor) so this is a valid dependent
%% regardless of `Actor` never auto-generating accessors — same reasoning as
%% `clean_dashboard_spawn_with_xref/0` above. Deliberately no
%% `beamtalk_workspace_meta:set_class_source/2` call for the dependent: its
%% source is never recorded, so `recheck_owner_for_shape/4` reaches its
%% `undefined` source branch and returns `{skipped, []}`.
skipped_dashboard_spawn_with_xref() ->
    [
        #{
            class_side => false,
            selector => build,
            line => 2,
            sends => [#{selector => 'spawnWith:', line => 2, recv_kind => other}],
            references => [#{class => 'LoaderShapeSkippedCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

skipped_candidate_shape_change_marks_existing_finding_stale_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_skipped_counter_~p.bt", [UniqueId])
                    ),

                    %% Generation 1: first-ever install this session — no_op,
                    %% nothing to diff against yet.
                    ok = file:write_file(Path, skipped_counter_source_gen1()),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),

                    %% Register the dependent via xref only — no
                    %% `set_class_source/2`, so it stays a `skipped` outcome
                    %% once generation 2 triggers its re-check.
                    ok = beamtalk_xref:register_class(
                        'LoaderShapeSkippedDashboard', skipped_dashboard_spawn_with_xref()
                    ),

                    %% Seed a stale finding as if an earlier reload (when the
                    %% dependent's source WAS still recorded) had already
                    %% flagged it.
                    SeedFinding = #{
                        owner => <<"LoaderShapeSkippedDashboard">>,
                        changed_class => <<"LoaderShapeSkippedCounter">>,
                        selector => <<"name">>,
                        classification => shape_change,
                        severity => <<"warning">>,
                        category => <<"Dnu">>,
                        message => <<"stale generation-A shape finding">>,
                        note => undefined,
                        sites => [#{method => build, line => 2}],
                        start => 0,
                        'end' => 5
                    },
                    _ = beamtalk_workspace_findings_store:put_owner_origin(
                        <<"LoaderShapeSkippedDashboard">>,
                        <<"LoaderShapeSkippedCounter">>,
                        [SeedFinding]
                    ),

                    %% Generation 2: an unrelated `count` slot is added,
                    %% triggering a shape_change re-check. The dependent's
                    %% source is still unrecorded, so its outcome is
                    %% `skipped`, not `ok` — it must never reach
                    %% `checked_owners`.
                    ok = file:write_file(Path, skipped_counter_source_gen2()),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    %% The shape re-check runs asynchronously on
                    %% `beamtalk_workspace_shape_recheck_worker`'s single-cast
                    %% queue (`maybe_trigger_shape_recheck_for_class/1`'s
                    %% doc). Unlike the sibling tests above, this scenario's
                    %% only candidate is `skipped`, so `CheckedOwners` stays
                    %% empty and `publish_shape_recheck_outcome/3` never
                    %% broadcasts a `'ReloadCheckCompleted'` announcement
                    %% (gated on `CheckedOwners` being non-empty — see that
                    %% function's doc) even though the unconditional
                    %% `mark_unverified_findings_stale/2` call still ran. A
                    %% `sys:get_state/1` round trip to the worker forces it to
                    %% finish draining its mailbox in FIFO order — our
                    %% `enqueue/1` cast above is already queued ahead of this
                    %% synchronous system message — without depending on an
                    %% announcement this scenario deliberately never sends.
                    _ = sys:get_state(beamtalk_workspace_shape_recheck_worker),

                    %% The seeded finding's message/severity/classification
                    %% stay untouched (no diagnostics round-trip actually
                    %% ran for this owner) but its `note` is overwritten to
                    %% flag it as not re-checked this reload — exactly like
                    %% the instance-selector path's skipped case.
                    [DashboardFinding] = beamtalk_workspace_findings_store:get_origin(
                        <<"LoaderShapeSkippedDashboard">>, <<"LoaderShapeSkippedCounter">>
                    ),
                    ?assertEqual(
                        <<"stale generation-A shape finding">>,
                        maps:get(message, DashboardFinding)
                    ),
                    Note = maps:get(note, DashboardFinding),
                    ?assert(is_binary(Note)),
                    ?assert(
                        binary:match(Note, <<"not re-checked against the latest reload">>) =/=
                            nomatch
                    )
                end)
            ]
        end}}.

%%====================================================================
%% End-to-end: ADR 0123 §4 shape-reload findings (BT-3538)
%%
%% One test per row of the findings table, each driving a real class-body
%% reload through `beamtalk_repl_loader:handle_load/2` twice and asserting
%% on the `'ShapeReloadFindingsCompleted'` push (`beamtalk_repl_loader:
%% publish_reload_findings/2`) — the workspace-level counterpart to
%% `beamtalk_shape_diff_tests`' pure per-row unit tests.
%%====================================================================

find_finding(Kind, Findings) ->
    case [F || F <- Findings, maps:get(kind, F) =:= Kind] of
        [F] -> F;
        [] -> error({no_finding_of_kind, Kind, Findings})
    end.

%% Row 1: field removed, shapeVersion unchanged -> Warning.
row1_dropped_without_bump_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_row1_~p.bt", [UniqueId])
                    ),
                    Gen1 =
                        <<
                            "Actor subclass: LoaderShapeRow1Counter\n"
                            "  state: count :: Integer = 0\n"
                            "  state: name :: String = \"\"\n"
                        >>,
                    Gen2 =
                        <<
                            "Actor subclass: LoaderShapeRow1Counter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    ok = file:write_file(Path, Gen1),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, state0()),
                    ok = file:write_file(Path, Gen2),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_shape_reload_findings_announcement(),
                    ?assertEqual(<<"LoaderShapeRow1Counter">>, maps:get(class, Event)),
                    Finding = find_finding(dropped_without_bump, maps:get(findings, Event)),
                    ?assertEqual(warning, maps:get(severity, Finding)),
                    ?assertEqual([<<"name">>], maps:get(fields, Finding)),
                    ?assertEqual(1, maps:get(from_version, Finding)),
                    ?assertEqual(1, maps:get(to_version, Finding))
                end)
            ]
        end}}.

%% Row 2: fields only added, version unchanged -> Hint.
row2_added_only_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_row2_~p.bt", [UniqueId])
                    ),
                    Gen1 =
                        <<
                            "Actor subclass: LoaderShapeRow2Counter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    Gen2 =
                        <<
                            "Actor subclass: LoaderShapeRow2Counter\n"
                            "  state: count :: Integer = 0\n"
                            "  state: name :: String = \"\"\n"
                        >>,
                    ok = file:write_file(Path, Gen1),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, state0()),
                    ok = file:write_file(Path, Gen2),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_shape_reload_findings_announcement(),
                    Finding = find_finding(added_only, maps:get(findings, Event)),
                    ?assertEqual(hint, maps:get(severity, Finding)),
                    ?assertEqual([<<"name">>], maps:get(fields, Finding)),
                    ?assertEqual(1, maps:get(from_version, Finding)),
                    ?assertEqual(1, maps:get(to_version, Finding))
                end)
            ]
        end}}.

%% Row 3: version bumped N -> N+1, no migrateFromVN: -> Hint.
row3_bumped_without_migration_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_row3_~p.bt", [UniqueId])
                    ),
                    Gen1 =
                        <<
                            "Actor subclass: LoaderShapeRow3Counter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    Gen2 =
                        <<
                            "Actor subclass: LoaderShapeRow3Counter\n"
                            "  shapeVersion: 2\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    ok = file:write_file(Path, Gen1),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, state0()),
                    ok = file:write_file(Path, Gen2),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_shape_reload_findings_announcement(),
                    Finding = find_finding(bumped_without_migration, maps:get(findings, Event)),
                    ?assertEqual(hint, maps:get(severity, Finding)),
                    ?assertEqual(1, maps:get(from_version, Finding)),
                    ?assertEqual(2, maps:get(to_version, Finding))
                end)
            ]
        end}}.

%% Row 4: version decreased -> Warning.
row4_version_decreased_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_row4_~p.bt", [UniqueId])
                    ),
                    Gen1 =
                        <<
                            "Actor subclass: LoaderShapeRow4Counter\n"
                            "  shapeVersion: 3\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    Gen2 =
                        <<
                            "Actor subclass: LoaderShapeRow4Counter\n"
                            "  shapeVersion: 2\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    ok = file:write_file(Path, Gen1),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, state0()),
                    ok = file:write_file(Path, Gen2),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_shape_reload_findings_announcement(),
                    Finding = find_finding(version_decreased, maps:get(findings, Event)),
                    ?assertEqual(warning, maps:get(severity, Finding)),
                    ?assertEqual(3, maps:get(from_version, Finding)),
                    ?assertEqual(2, maps:get(to_version, Finding))
                end)
            ]
        end}}.

%% Row 5: any instance left suspended by a failed migration -> Error.
%% Spawns a real live instance under generation 1, then reloads to a
%% generation whose migrateFromV1: hook unconditionally raises
%% `does_not_understand` (`old bogusPrecheckSelector`) — `beamtalk_shape_chain`
%% catches it as a step failure, `try_change_code/3` leaves the instance
%% suspended (BT-3534), and `hot_reload_class/2` publishes this
%% synchronously (not via the async shape-recheck worker the other four
%% rows go through — see its doc).
row5_instances_suspended_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_row5_~p.bt", [UniqueId])
                    ),
                    Gen1 =
                        <<
                            "Actor subclass: LoaderShapeRow5Counter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    Gen2 =
                        <<
                            "Actor subclass: LoaderShapeRow5Counter\n"
                            "  shapeVersion: 2\n"
                            "  state: count :: Integer = 0\n"
                            "\n"
                            "  class migrateFromV1: old -> Dictionary =>\n"
                            "    old bogusPrecheckSelector\n"
                        >>,
                    ok = file:write_file(Path, Gen1),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, state0()),

                    {ok, _, _, _, _} = beamtalk_repl_eval:do_eval(
                        "LoaderShapeRow5Counter spawn", state0()
                    ),
                    [Pid] = beamtalk_runtime_api:all_instances('LoaderShapeRow5Counter'),

                    ok = file:write_file(Path, Gen2),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(Path, State1),

                    Event = receive_shape_reload_findings_announcement(),
                    ?assertEqual(<<"LoaderShapeRow5Counter">>, maps:get(class, Event)),
                    Finding = find_finding(instances_suspended, maps:get(findings, Event)),
                    ?assertEqual(error, maps:get(severity, Finding)),
                    ?assertEqual(1, maps:get(suspended, Finding)),
                    ?assertEqual([Pid], maps:get(pids, Finding)),
                    ?assertEqual(
                        suspended, maps:get(sysState, beamtalk_process_navigation:status(Pid))
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Pre-save advisory: precheck_class_shape/2 (ADR 0105 Phase 3, ADR 0123 §4,
%% BT-3538) — fires the same "dropped without bump" finding before install.
%%====================================================================

precheck_class_shape_flags_dropped_field_before_install_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_precheck_~p.bt", [UniqueId])
                    ),
                    Gen1 =
                        <<
                            "Actor subclass: LoaderShapePrecheckCounter\n"
                            "  state: count :: Integer = 0\n"
                            "  state: name :: String = \"\"\n"
                        >>,
                    ok = file:write_file(Path, Gen1),
                    {ok, _, _State1} = beamtalk_repl_loader:handle_load(Path, state0()),

                    %% A pending edit that would drop `name` without bumping
                    %% shapeVersion — never installed via handle_load/2 at
                    %% all, unlike every other test in this module.
                    PendingSource =
                        <<
                            "Actor subclass: LoaderShapePrecheckCounter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    {ok, Findings} = beamtalk_repl_loader:precheck_class_shape(
                        <<"LoaderShapePrecheckCounter">>, PendingSource
                    ),
                    Finding = find_finding(dropped_without_bump, Findings),
                    ?assertEqual(warning, maps:get(severity, Finding)),
                    ?assertEqual([<<"name">>], maps:get(fields, Finding)),

                    %% Never installed: the store's own captured generation
                    %% is untouched — a *real* reload of this same pending
                    %% edit still finds the same "previous" baseline.
                    Prev = beamtalk_workspace_shape_store:previous(
                        <<"LoaderShapePrecheckCounter">>
                    ),
                    ?assertEqual(
                        #{<<"count">> => <<"Integer">>, <<"name">> => <<"String">>},
                        maps:get(shape, Prev)
                    )
                end)
            ]
        end}}.

%% A pending edit with nothing shape-relevant changed (e.g. just a method
%% body tweak — here, literally unchanged) reports no findings.
precheck_class_shape_reports_nothing_for_a_clean_edit_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_precheck_clean_~p.bt", [UniqueId])
                    ),
                    Source =
                        <<
                            "Actor subclass: LoaderShapePrecheckCleanCounter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    ok = file:write_file(Path, Source),
                    {ok, _, _State1} = beamtalk_repl_loader:handle_load(Path, state0()),

                    {ok, Findings} = beamtalk_repl_loader:precheck_class_shape(
                        <<"LoaderShapePrecheckCleanCounter">>, Source
                    ),
                    ?assertEqual([], Findings)
                end)
            ]
        end}}.

%% precheck_class_shape/2 must never leak an atom per call — a per-call
%% `unique_integer`-suffixed throwaway module name would (see
%% `beamtalk_repl_loader:precheck_temp_module_name/0`'s doc); the fixed name
%% + `global:trans/3` critical section fix means the node's atom count is
%% unchanged after several calls, including a couple of failing (bad source)
%% ones — a compile failure must not leak either.
precheck_class_shape_does_not_leak_atoms_test_() ->
    {timeout, 30,
        {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    UniqueId = erlang:unique_integer([positive]),
                    Path = filename:join(
                        temp_dir(), io_lib:format("loader_shape_precheck_leak_~p.bt", [UniqueId])
                    ),
                    Source =
                        <<
                            "Actor subclass: LoaderShapePrecheckLeakCounter\n"
                            "  state: count :: Integer = 0\n"
                            "  state: name :: String = \"\"\n"
                        >>,
                    ok = file:write_file(Path, Source),
                    {ok, _, _State1} = beamtalk_repl_loader:handle_load(Path, state0()),

                    PendingSource =
                        <<
                            "Actor subclass: LoaderShapePrecheckLeakCounter\n"
                            "  state: count :: Integer = 0\n"
                        >>,
                    %% Warm the fixed scratch atom once before measuring, so
                    %% the very first load (which does mint that one atom)
                    %% isn't counted against the repeated calls below.
                    {ok, _} = beamtalk_repl_loader:precheck_class_shape(
                        <<"LoaderShapePrecheckLeakCounter">>, PendingSource
                    ),
                    AtomCountBefore = erlang:system_info(atom_count),

                    lists:foreach(
                        fun(_) ->
                            {ok, _} = beamtalk_repl_loader:precheck_class_shape(
                                <<"LoaderShapePrecheckLeakCounter">>, PendingSource
                            ),
                            %% A syntactically-broken pending edit must not
                            %% leak an atom either.
                            {ok, _} = beamtalk_repl_loader:precheck_class_shape(
                                <<"LoaderShapePrecheckLeakCounter">>, <<"not valid beamtalk (((">>
                            )
                        end,
                        lists:seq(1, 5)
                    ),

                    AtomCountAfter = erlang:system_info(atom_count),
                    ?assertEqual(AtomCountBefore, AtomCountAfter)
                end)
            ]
        end}}.

%% A class never captured this session (no baseline) has nothing to compare
%% against — [], not an error.
precheck_class_shape_with_no_baseline_reports_nothing_test_() ->
    {setup, fun shape_loader_setup/0, fun shape_loader_teardown/1, fun(_) ->
        ?_test(begin
            {ok, Findings} = beamtalk_repl_loader:precheck_class_shape(
                <<"NeverCapturedClassXyz">>,
                <<"Actor subclass: NeverCapturedClassXyz\n  state: count :: Integer = 0\n">>
            ),
            ?assertEqual([], Findings)
        end)
    end}.
