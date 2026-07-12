%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_recheck_tests).

-moduledoc """
Tests for the publish/clearing half of ADR 0105 Phase 1 (BT-2779):
`beamtalk_repl_loader:maybe_trigger_recheck/4`'s wiring of
`beamtalk_workspace_findings_store` + the `'ReloadCheckCompleted'` system
announcement.

Integration tests against the real compiler port + a real `beamtalk_xref` +
`beamtalk_workspace_meta` + `beamtalk_workspace_findings_store`, mirroring
`beamtalk_recheck_tests.erl`'s fixture pattern (BT-2778) — this module tests
one layer up: not "does `beamtalk_recheck:trigger/4` produce the right
findings" (that module's job), but "does `maybe_trigger_recheck/4` correctly
store, replace, and announce them". `CaptureOutcome` (the signature-store's
classification) is supplied directly rather than routed through a full
`beamtalk_workspace_signature_store:capture/4` round-trip — it is just this
function's fourth argument, so constructing it directly keeps these tests
focused on the publish/clearing behaviour BT-2779 adds.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Integration fixture: real compiler port + xref + workspace_meta + store
%%====================================================================

loader_recheck_setup() ->
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
    %% `repl => false` for the same test-isolation reason
    %% `beamtalk_recheck_tests:recheck_setup/0` uses it — see that module's
    %% comment for the disk-persistence gotcha this avoids.
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"loader_recheck_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    clear_xref(),
    beamtalk_compiler_server:clear_classes(),
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
    end,
    {ok, _} = beamtalk_workspace_findings_store:start_link(),
    %% `beamtalk_repl_subscriptions:subscribe_bus/2` is a no-op (silently
    %% skipped, by design — see its doc) when the SystemAnnouncer bus isn't
    %% running yet; `system_announce/2` only starts it on demand at the
    %% *first announce*, which would be too late for a subscription made
    %% beforehand. Ensure it's up here so every test's own subscribe (see
    %% `subscribe_self_to_reload_check/0` — deliberately called from EACH
    %% test body, not here: an eunit `{setup, ...}` fixture's `Start`
    %% function is not guaranteed to run in the same process as the test
    %% body it brackets, and a `self()`-keyed subscription is only useful
    %% if subscriber and receiver are the same process) actually registers.
    ok = beamtalk_announcements:ensure_started(),
    ok.

%% Subscribe *this* process (the test body's own pid) to the `reload_check`
%% stream. Must be called from inside each `?_test(...)` body — see
%% `loader_recheck_setup/0`'s comment for why it can't live in `Setup`.
subscribe_self_to_reload_check() ->
    ok = beamtalk_repl_subscriptions:subscribe(reload_check, self()).

loader_recheck_teardown(_) ->
    beamtalk_repl_subscriptions:unsubscribe(reload_check, self()),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
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

%% Wait (up to ~2s) for a 'ReloadCheckCompleted' announcement to reach this
%% process's mailbox, decoding it into a plain map for easy assertions.
%% Announcements are async (`beamtalk_announcements:system_announce/2`
%% dispatches via the async veneer path), so a test that triggers a recheck
%% must not assert on the mailbox immediately.
receive_reload_check_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event} ->
            Event
    after 2000 ->
        error(timeout_waiting_for_reload_check_announcement)
    end.

%% Fail loudly if an announcement arrives when the test expects none —
%% distinguishes "nothing happened" from "something happened but we didn't
%% check", the same rigor `?assertNot`/refute patterns give synchronous
%% assertions.
assert_no_reload_check_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event} ->
            error({unexpected_announcement, Event})
    after 200 ->
        ok
    end.

%%====================================================================
%% Fixtures: LoaderReCheckCounter (changed class) / LoaderReCheckDashboard (caller)
%%====================================================================

dashboard_xref() ->
    [
        #{
            class_side => false,
            selector => 'refresh:',
            line => 2,
            sends => [#{selector => size, line => 2, recv_kind => other}],
            references => [#{class => 'LoaderReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

dashboard_source() ->
    <<
        "Object subclass: LoaderReCheckDashboard\n"
        "  refresh: c :: LoaderReCheckCounter -> Integer => (c size) + 1\n"
    >>.

counter_hierarchy(ReturnType) ->
    #{
        superclass => 'Object',
        method_info => #{size => #{arity => 0, param_types => [], return_type => ReturnType}}
    }.

%% Install the fixture's dependent (Dashboard) and set the changed class
%% (Counter)'s CURRENT signature to `ReturnType` — mirrors the ambient
%% class-hierarchy cache being current by the time a reload's re-check runs
%% (see `beamtalk_recheck.erl`'s moduledoc).
install_fixture(ReturnType) ->
    beamtalk_compiler_server:register_class(
        'LoaderReCheckCounter', counter_hierarchy(ReturnType)
    ),
    ok = beamtalk_xref:register_class('LoaderReCheckDashboard', dashboard_xref()),
    ok = beamtalk_workspace_meta:set_class_source(
        <<"LoaderReCheckDashboard">>, dashboard_source()
    ).

%%====================================================================
%% Clearing-by-replacement: reload-fixes-reload
%%====================================================================

reload_fixes_reload_clears_a_fixed_finding_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    %% Reload A: Counter>>size now returns String — Dashboard's
                    %% `(c size) + 1` goes stale.
                    install_fixture('String'),
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, undefined, signature_change}
                    ),

                    FindingsAfterA = beamtalk_workspace_findings_store:for_owner(
                        <<"LoaderReCheckDashboard">>
                    ),
                    ?assertEqual(1, length(FindingsAfterA)),

                    EventA = receive_reload_check_announcement(),
                    ?assertEqual(<<"LoaderReCheckCounter">>, maps:get(changedClass, EventA)),
                    ?assert(
                        lists:member(<<"LoaderReCheckDashboard">>, maps:get(checkedOwners, EventA))
                    ),
                    ?assertEqual(1, length(maps:get(findings, EventA))),

                    %% Reload B: Counter>>size is restored to Integer — the SAME
                    %% dependent re-checks clean, without anyone touching
                    %% Dashboard. `maybe_trigger_recheck/4` fires again on
                    %% Counter's own reload (the trigger the ADR's mechanism
                    %% relies on — every reload re-runs steps 1-3 automatically).
                    install_fixture('Integer'),
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, 'String', signature_change}
                    ),

                    FindingsAfterB = beamtalk_workspace_findings_store:for_owner(
                        <<"LoaderReCheckDashboard">>
                    ),
                    ?assertEqual([], FindingsAfterB),

                    EventB = receive_reload_check_announcement(),
                    ?assertEqual(
                        [<<"LoaderReCheckDashboard">>], maps:get(checkedOwners, EventB)
                    ),
                    ?assertEqual([], maps:get(findings, EventB))
                end)
            ]
        end}}.

%%====================================================================
%% Clearing-by-replacement is scoped per (owner, changed class) — the
%% CRITICAL data-loss case an adversarial review caught: a caller broken by
%% TWO independently-reloading classes must keep both findings until each
%% is fixed on its own schedule. Seeds the "other origin" finding directly
%% (as if an earlier reload of a different class had flagged it) rather
%% than building a second full compiler/xref fixture — the property under
%% test is whether `maybe_trigger_recheck/4`'s real re-check of Counter
%% threads `ClassNameBin` through as the origin key, not whether a second
%% class's re-check independently works (already covered elsewhere).
%%====================================================================

different_changed_class_origin_survives_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    %% Simulate an earlier, unrelated reload of
                    %% LoaderReCheckWidget having already flagged Dashboard.
                    WidgetFinding = #{
                        owner => <<"LoaderReCheckDashboard">>,
                        changed_class => <<"LoaderReCheckWidget">>,
                        selector => <<"area">>,
                        classification => signature_change,
                        severity => <<"warning">>,
                        category => <<"Dnu">>,
                        message => <<"widget finding">>,
                        note => undefined,
                        sites => [#{method => <<"renderWidget:">>, line => 3}],
                        start => 0,
                        'end' => 1
                    },
                    _ = beamtalk_workspace_findings_store:put_owner_origin(
                        <<"LoaderReCheckDashboard">>, <<"LoaderReCheckWidget">>, [WidgetFinding]
                    ),

                    %% Now Counter reloads and breaks Dashboard independently.
                    install_fixture('String'),
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, undefined, signature_change}
                    ),
                    _ = receive_reload_check_announcement(),

                    %% Dashboard must now show BOTH findings — Counter's
                    %% reload must not have discarded Widget's.
                    Findings = beamtalk_workspace_findings_store:for_owner(
                        <<"LoaderReCheckDashboard">>
                    ),
                    ?assertEqual(2, length(Findings)),
                    ?assert(lists:member(WidgetFinding, Findings)),

                    %% Counter is fixed (a clean re-check) — only Counter's
                    %% origin clears; Widget's survives.
                    install_fixture('Integer'),
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, 'String', signature_change}
                    ),
                    _ = receive_reload_check_announcement(),

                    ?assertEqual(
                        [WidgetFinding],
                        beamtalk_workspace_findings_store:for_owner(<<"LoaderReCheckDashboard">>)
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Clearing-by-replacement: supersession (no accumulation)
%%====================================================================

supersession_replaces_not_accumulates_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    install_fixture('String'),

                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, undefined, signature_change}
                    ),
                    _ = receive_reload_check_announcement(),
                    ?assertEqual(
                        1,
                        length(
                            beamtalk_workspace_findings_store:for_owner(
                                <<"LoaderReCheckDashboard">>
                            )
                        )
                    ),

                    %% Back-to-back reload of the SAME method, same-shaped
                    %% finding — generation B must replace generation A, not
                    %% pile up alongside it.
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, 'String', signature_change}
                    ),
                    _ = receive_reload_check_announcement(),
                    ?assertEqual(
                        1,
                        length(
                            beamtalk_workspace_findings_store:for_owner(
                                <<"LoaderReCheckDashboard">>
                            )
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% A caller's own install clears its stale findings-as-caller
%% (subsumes the ADR's explicit "Workspace changes revert:" bullet — a
%% revert re-installs through this exact same hook, see
%% beamtalk_workspace_findings_store's moduledoc).
%%====================================================================

own_install_clears_stale_findings_as_caller_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),
                    %% Seed TWO stale findings for Dashboard from DIFFERENT
                    %% origins, as if two earlier reloads (of two different
                    %% classes) had each flagged it. `clear_owner/1` is
                    %% deliberately un-scoped (unlike the scoped
                    %% `put_owner_origin/3` the dependent-recheck path
                    %% uses) — Dashboard's own source changing must wipe
                    %% BOTH, not just one origin.
                    _ = beamtalk_workspace_findings_store:put_owner_origin(
                        <<"LoaderReCheckDashboard">>, <<"SomeOtherClass">>, [
                            #{
                                owner => <<"LoaderReCheckDashboard">>,
                                changed_class => <<"SomeOtherClass">>,
                                selector => <<"foo">>,
                                classification => signature_change,
                                severity => <<"warning">>,
                                category => <<"Dnu">>,
                                message => <<"stale finding">>,
                                note => undefined,
                                sites => [#{method => <<"refresh:">>, line => 2}],
                                start => 0,
                                'end' => 1
                            }
                        ]
                    ),
                    _ = beamtalk_workspace_findings_store:put_owner_origin(
                        <<"LoaderReCheckDashboard">>, <<"YetAnotherClass">>, [
                            #{
                                owner => <<"LoaderReCheckDashboard">>,
                                changed_class => <<"YetAnotherClass">>,
                                selector => <<"bar">>,
                                classification => signature_change,
                                severity => <<"warning">>,
                                category => <<"Dnu">>,
                                message => <<"another stale finding">>,
                                note => undefined,
                                sites => [#{method => <<"refresh:">>, line => 3}],
                                start => 0,
                                'end' => 1
                            }
                        ]
                    ),
                    ?assertEqual(
                        2,
                        length(
                            beamtalk_workspace_findings_store:for_owner(
                                <<"LoaderReCheckDashboard">>
                            )
                        )
                    ),

                    %% Dashboard's own source is (re)installed — a `no_op`
                    %% capture (e.g. an unrelated header re-save) or a revert
                    %% re-install both reach `maybe_trigger_recheck/4` this way.
                    %% No dependents of Dashboard>>refresh: exist in this
                    %% fixture, so the only newsworthy thing is clearing its
                    %% own stale entry.
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckDashboard">>,
                        <<"refresh:">>,
                        instance,
                        {captured, undefined, no_op}
                    ),

                    ?assertEqual(
                        [],
                        beamtalk_workspace_findings_store:for_owner(<<"LoaderReCheckDashboard">>)
                    ),

                    Event = receive_reload_check_announcement(),
                    ?assertEqual(
                        <<"self_edit">>, atom_to_binary(maps:get(classification, Event), utf8)
                    ),
                    ?assertEqual(
                        [<<"LoaderReCheckDashboard">>], maps:get(checkedOwners, Event)
                    )
                end)
            ]
        end}}.

%% A `not_captured` install (the signature store itself failed) with nothing
%% previously recorded for the owner announces nothing — the common case,
%% not worth a push frame on every ordinary save.
no_op_install_with_nothing_to_clear_announces_nothing_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckDashboard">>, <<"refresh:">>, instance, not_captured
                    ),
                    assert_no_reload_check_announcement(),
                    ?assertEqual(
                        [],
                        beamtalk_workspace_findings_store:for_owner(<<"LoaderReCheckDashboard">>)
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Caller-cap staleness marking (ADR 0105, BT-2802)
%%====================================================================

%% Second dependent, alphabetically AFTER Dashboard — with `recheck_caller_cap`
%% set to 1, `apply_cap/2`'s alphabetically-first-N rule always keeps
%% Dashboard and drops ListView.
listview_xref() ->
    [
        #{
            class_side => false,
            selector => 'render:',
            line => 2,
            sends => [#{selector => size, line => 2, recv_kind => other}],
            references => [#{class => 'LoaderReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

listview_source() ->
    <<
        "Object subclass: LoaderReCheckListView\n"
        "  render: c :: LoaderReCheckCounter -> Integer => (c size) + 1\n"
    >>.

%% Installs both dependents of `size`, with the caller cap set to 1 so
%% Dashboard (alphabetically first) is always kept and ListView is always
%% dropped. Restores the previous cap in a `try/after`.
with_capped_fixture(ReturnType, Fun) ->
    PrevCap = application:get_env(beamtalk_workspace, recheck_caller_cap, 20),
    application:set_env(beamtalk_workspace, recheck_caller_cap, 1),
    try
        install_fixture(ReturnType),
        ok = beamtalk_xref:register_class('LoaderReCheckListView', listview_xref()),
        ok = beamtalk_workspace_meta:set_class_source(
            <<"LoaderReCheckListView">>, listview_source()
        ),
        Fun()
    after
        application:set_env(beamtalk_workspace, recheck_caller_cap, PrevCap)
    end.

%% BT-2802 core bug scenario: ListView was flagged by an earlier reload of
%% Counter (when it was still inside the cap, or the cap was higher then).
%% This reload's candidate set is capped to 1, so ListView is dropped —
%% never re-checked. Without the fix, ListView's stale, generation-A
%% finding would sit in the store unchanged forever, indistinguishable from
%% a freshly-verified one. With the fix, `maybe_trigger_recheck/4` marks it
%% in place (same message/severity/classification, `note` now says it was
%% not re-verified this reload).
capped_out_candidate_with_existing_finding_gets_marked_stale_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    with_capped_fixture('String', fun() ->
                        %% Seed ListView's origin bucket as if an earlier
                        %% reload of Counter had already checked it and
                        %% found it stale.
                        SeedFinding = #{
                            owner => <<"LoaderReCheckListView">>,
                            changed_class => <<"LoaderReCheckCounter">>,
                            selector => <<"size">>,
                            classification => signature_change,
                            severity => <<"warning">>,
                            category => <<"Dnu">>,
                            message => <<"stale generation-A finding">>,
                            note => undefined,
                            sites => [#{method => <<"render:">>, line => 2}],
                            start => 0,
                            'end' => 5
                        },
                        _ = beamtalk_workspace_findings_store:put_owner_origin(
                            <<"LoaderReCheckListView">>, <<"LoaderReCheckCounter">>, [SeedFinding]
                        ),

                        ok = beamtalk_repl_loader:maybe_trigger_recheck(
                            <<"LoaderReCheckCounter">>,
                            <<"size">>,
                            instance,
                            {captured, undefined, signature_change}
                        ),

                        %% Dashboard (kept by the cap) got a real, fresh
                        %% re-check.
                        DashboardFindings = beamtalk_workspace_findings_store:for_owner(
                            <<"LoaderReCheckDashboard">>
                        ),
                        ?assertEqual(1, length(DashboardFindings)),

                        %% ListView (dropped by the cap) keeps its original
                        %% finding's message/severity/classification —
                        %% untouched, since nothing re-verified it — but its
                        %% `note` is overwritten to flag it as not
                        %% re-checked this reload.
                        [ListViewFinding] = beamtalk_workspace_findings_store:get_origin(
                            <<"LoaderReCheckListView">>, <<"LoaderReCheckCounter">>
                        ),
                        ?assertEqual(
                            <<"stale generation-A finding">>, maps:get(message, ListViewFinding)
                        ),
                        ?assertEqual(<<"warning">>, maps:get(severity, ListViewFinding)),
                        Note = maps:get(note, ListViewFinding),
                        ?assert(is_binary(Note)),
                        ?assert(
                            binary:match(Note, <<"not re-checked against the latest reload">>) =/=
                                nomatch
                        )
                    end)
                end)
            ]
        end}}.

%% A cap-dropped candidate with NO existing finding for this origin (the
%% common case — most candidates never had a problem) is left alone: no
%% spurious entry is created just because the cap excluded it.
capped_out_candidate_with_no_existing_finding_stays_absent_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    with_capped_fixture('String', fun() ->
                        ok = beamtalk_repl_loader:maybe_trigger_recheck(
                            <<"LoaderReCheckCounter">>,
                            <<"size">>,
                            instance,
                            {captured, undefined, signature_change}
                        ),
                        ?assertEqual(
                            [],
                            beamtalk_workspace_findings_store:get_origin(
                                <<"LoaderReCheckListView">>, <<"LoaderReCheckCounter">>
                            )
                        )
                    end)
                end)
            ]
        end}}.

%% A stale note is not re-wrapped on every consecutive capped-out reload —
%% otherwise a candidate parked outside the cap for many reloads in a row
%% would accumulate one "not re-checked" suffix per reload.
capped_out_stale_note_is_idempotent_across_reloads_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    with_capped_fixture('String', fun() ->
                        SeedFinding = #{
                            owner => <<"LoaderReCheckListView">>,
                            changed_class => <<"LoaderReCheckCounter">>,
                            selector => <<"size">>,
                            classification => signature_change,
                            severity => <<"warning">>,
                            category => <<"Dnu">>,
                            message => <<"stale generation-A finding">>,
                            note => undefined,
                            sites => [#{method => <<"render:">>, line => 2}],
                            start => 0,
                            'end' => 5
                        },
                        _ = beamtalk_workspace_findings_store:put_owner_origin(
                            <<"LoaderReCheckListView">>, <<"LoaderReCheckCounter">>, [SeedFinding]
                        ),

                        %% Two consecutive reloads of Counter, both capping
                        %% ListView out.
                        ok = beamtalk_repl_loader:maybe_trigger_recheck(
                            <<"LoaderReCheckCounter">>,
                            <<"size">>,
                            instance,
                            {captured, undefined, signature_change}
                        ),
                        [FirstMarked] = beamtalk_workspace_findings_store:get_origin(
                            <<"LoaderReCheckListView">>, <<"LoaderReCheckCounter">>
                        ),
                        ok = beamtalk_repl_loader:maybe_trigger_recheck(
                            <<"LoaderReCheckCounter">>,
                            <<"size">>,
                            instance,
                            {captured, 'String', signature_change}
                        ),
                        [SecondMarked] = beamtalk_workspace_findings_store:get_origin(
                            <<"LoaderReCheckListView">>, <<"LoaderReCheckCounter">>
                        ),
                        ?assertEqual(
                            maps:get(note, FirstMarked), maps:get(note, SecondMarked)
                        )
                    end)
                end)
            ]
        end}}.

%%====================================================================
%% Skipped-candidate staleness marking (ADR 0105, BT-2828)
%%
%% A candidate that stays INSIDE the (default, un-capped) `Kept` set but has
%% no live source recorded (`recheck_owner/5` returns `{skipped, []}`) never
%% goes through `checked_owners` (not `{ok, _}`) *and*, before this fix, was
%% absent from `not_checked_owners` too (that field is strictly the
%% cap-excluded set, computed before any individual re-check runs) — so its
%% pre-existing finding was neither replaced nor marked stale. These mirror
%% `capped_out_candidate_with_existing_finding_gets_marked_stale_test_` and
%% its siblings above, substituting "no live source" for "cap-dropped" as
%% the reason the candidate's re-check never actually completed.
%%====================================================================

%% Registers Dashboard as an xref candidate of `size` WITHOUT ever recording
%% its live source (`beamtalk_workspace_meta:set_class_source/2` is
%% deliberately skipped) — the default (un-capped) caller cap keeps it in
%% `Kept`, so `recheck_owner/5` reaches its `undefined` source branch and
%% returns `{skipped, []}`.
install_fixture_no_live_source(ReturnType) ->
    beamtalk_compiler_server:register_class(
        'LoaderReCheckCounter', counter_hierarchy(ReturnType)
    ),
    ok = beamtalk_xref:register_class('LoaderReCheckDashboard', dashboard_xref()).

skipped_candidate_with_existing_finding_gets_marked_stale_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    install_fixture_no_live_source('String'),
                    %% Seed Dashboard's origin bucket as if an earlier reload
                    %% of Counter had already checked it (when its source WAS
                    %% recorded) and found it stale.
                    SeedFinding = #{
                        owner => <<"LoaderReCheckDashboard">>,
                        changed_class => <<"LoaderReCheckCounter">>,
                        selector => <<"size">>,
                        classification => signature_change,
                        severity => <<"warning">>,
                        category => <<"Dnu">>,
                        message => <<"stale generation-A finding">>,
                        note => undefined,
                        sites => [#{method => <<"refresh:">>, line => 2}],
                        start => 0,
                        'end' => 5
                    },
                    _ = beamtalk_workspace_findings_store:put_owner_origin(
                        <<"LoaderReCheckDashboard">>, <<"LoaderReCheckCounter">>, [SeedFinding]
                    ),

                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, undefined, signature_change}
                    ),

                    %% Dashboard's diagnostics round-trip never ran (no live
                    %% source), so its original finding's
                    %% message/severity/classification stay untouched — but
                    %% the `note` is overwritten to flag it as not
                    %% re-checked this reload, exactly as a cap-dropped
                    %% candidate's is.
                    [DashboardFinding] = beamtalk_workspace_findings_store:get_origin(
                        <<"LoaderReCheckDashboard">>, <<"LoaderReCheckCounter">>
                    ),
                    ?assertEqual(
                        <<"stale generation-A finding">>, maps:get(message, DashboardFinding)
                    ),
                    ?assertEqual(<<"warning">>, maps:get(severity, DashboardFinding)),
                    Note = maps:get(note, DashboardFinding),
                    ?assert(is_binary(Note)),
                    ?assert(
                        binary:match(Note, <<"not re-checked against the latest reload">>) =/=
                            nomatch
                    )
                end)
            ]
        end}}.

%% A skipped candidate with NO existing finding for this origin (the common
%% case) is left alone — no spurious entry is created just because its
%% source was unavailable this reload.
skipped_candidate_with_no_existing_finding_stays_absent_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    install_fixture_no_live_source('String'),
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, undefined, signature_change}
                    ),
                    ?assertEqual(
                        [],
                        beamtalk_workspace_findings_store:get_origin(
                            <<"LoaderReCheckDashboard">>, <<"LoaderReCheckCounter">>
                        )
                    )
                end)
            ]
        end}}.

%% A stale note is not re-wrapped on every consecutive reload where the
%% candidate's source stays unavailable — mirrors
%% `capped_out_stale_note_is_idempotent_across_reloads_test_`.
skipped_stale_note_is_idempotent_across_reloads_test_() ->
    {timeout, 30,
        {setup, fun loader_recheck_setup/0, fun loader_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    install_fixture_no_live_source('String'),
                    SeedFinding = #{
                        owner => <<"LoaderReCheckDashboard">>,
                        changed_class => <<"LoaderReCheckCounter">>,
                        selector => <<"size">>,
                        classification => signature_change,
                        severity => <<"warning">>,
                        category => <<"Dnu">>,
                        message => <<"stale generation-A finding">>,
                        note => undefined,
                        sites => [#{method => <<"refresh:">>, line => 2}],
                        start => 0,
                        'end' => 5
                    },
                    _ = beamtalk_workspace_findings_store:put_owner_origin(
                        <<"LoaderReCheckDashboard">>, <<"LoaderReCheckCounter">>, [SeedFinding]
                    ),

                    %% Two consecutive reloads of Counter, Dashboard's source
                    %% unavailable both times.
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, undefined, signature_change}
                    ),
                    [FirstMarked] = beamtalk_workspace_findings_store:get_origin(
                        <<"LoaderReCheckDashboard">>, <<"LoaderReCheckCounter">>
                    ),
                    ok = beamtalk_repl_loader:maybe_trigger_recheck(
                        <<"LoaderReCheckCounter">>,
                        <<"size">>,
                        instance,
                        {captured, 'String', signature_change}
                    ),
                    [SecondMarked] = beamtalk_workspace_findings_store:get_origin(
                        <<"LoaderReCheckDashboard">>, <<"LoaderReCheckCounter">>
                    ),
                    ?assertEqual(
                        maps:get(note, FirstMarked), maps:get(note, SecondMarked)
                    )
                end)
            ]
        end}}.
