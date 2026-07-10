%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_recheck_tests).

-moduledoc """
Tests for beamtalk_recheck (ADR 0105 Phase 1, BT-2778).

Two tiers:

  * Pure unit tests for the deterministic helpers (`group_by_owner/1`,
    `apply_cap/2`, `relevant_diagnostic/4`, `cap_note/1`) — no port, no xref,
    no workspace processes.
  * Integration tests exercising `trigger/4` end-to-end against the real
    compiler port + a real `beamtalk_xref` + `beamtalk_workspace_meta`,
    mirroring the ADR 0105 Phase 0 spike
    (`docs/internal/adr-0105-phase0-spike-findings.md`) and
    `beamtalk_repl_loader_tests.erl`'s integration-fixture pattern. The
    walking-skeleton case: `ReCheckCounter>>size` changes `Integer -> String`;
    `ReCheckDashboard>>refresh:` (a real dependent, `(c size) + 1`) goes
    stale; `ReCheckListView>>render:` sends the same selector `size` but on a
    `List` receiver — selector-keyed xref returns both candidates, and only
    the receiver-type filter (which happens naturally at re-check, not as a
    pre-filter — see the moduledoc of beamtalk_recheck) tells them apart.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Pure helpers — group_by_owner/1, apply_cap/2, cap_note/1
%%====================================================================

group_by_owner_dedupes_and_sorts_test() ->
    Sites = [
        #{owner => 'Zeta', method => m1, line => 1},
        #{owner => 'Alpha', method => m2, line => 2},
        #{owner => 'Zeta', method => m3, line => 3}
    ],
    Grouped = beamtalk_recheck:group_by_owner(Sites),
    ?assertEqual(['Alpha', 'Zeta'], [Owner || {Owner, _} <- Grouped]),
    {'Zeta', ZetaSites} = lists:keyfind('Zeta', 1, Grouped),
    ?assertEqual(2, length(ZetaSites)).

group_by_owner_empty_test() ->
    ?assertEqual([], beamtalk_recheck:group_by_owner([])).

apply_cap_under_limit_test() ->
    ?assertEqual({[a, b], 0}, beamtalk_recheck:apply_cap([a, b], 5)).

apply_cap_at_limit_test() ->
    ?assertEqual({[a, b], 0}, beamtalk_recheck:apply_cap([a, b], 2)).

apply_cap_over_limit_test() ->
    ?assertEqual({[a, b], 3}, beamtalk_recheck:apply_cap([a, b, c, d, e], 2)).

cap_note_zero_is_undefined_test() ->
    ?assertEqual(undefined, beamtalk_recheck:cap_note(0)).

cap_note_nonzero_test() ->
    ?assertEqual(<<"3 more not checked">>, beamtalk_recheck:cap_note(3)).

%%====================================================================
%% Pure helper — relevant_diagnostic/4 (attribution filter)
%%====================================================================

relevant_diagnostic_removal_matches_quoted_selector_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"'ReCheckCounter' does not understand 'reset'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assert(beamtalk_recheck:relevant_diagnostic(Diag, <<"ReCheckCounter">>, <<"reset">>, removal)).

relevant_diagnostic_removal_ignores_other_selector_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"'ReCheckCounter' does not understand 'unrelatedSelector'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assertNot(
        beamtalk_recheck:relevant_diagnostic(Diag, <<"ReCheckCounter">>, <<"reset">>, removal)
    ).

relevant_diagnostic_signature_change_keeps_type_category_test() ->
    Diag = #{
        category => <<"Type">>,
        message => <<"no member of 'String' understands '+'">>,
        severity => <<"warning">>,
        start => 0,
        'end' => 1
    },
    ?assert(
        beamtalk_recheck:relevant_diagnostic(
            Diag, <<"ReCheckCounter">>, <<"size">>, signature_change
        )
    ).

%% A signature_change also surfaces as a Dnu-category diagnostic in practice
%% — e.g. `getCount -> String` breaks a caller's `+`, and the checker reports
%% that as "`String` does not understand `'+'`" (Dnu), not a `Type` mismatch.
%% Unlike `removal`, this deliberately does NOT require the message to name
%% the changed selector (`size`) — see the moduledoc's documented precision
%% limit.
relevant_diagnostic_signature_change_keeps_dnu_category_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"String does not understand '+'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assert(
        beamtalk_recheck:relevant_diagnostic(
            Diag, <<"ReCheckCounter">>, <<"size">>, signature_change
        )
    ).

relevant_diagnostic_signature_change_ignores_unused_category_test() ->
    Diag = #{
        category => <<"Unused">>,
        message => <<"variable 'x' is never used">>,
        severity => <<"lint">>,
        start => 0,
        'end' => 1
    },
    ?assertNot(
        beamtalk_recheck:relevant_diagnostic(
            Diag, <<"ReCheckCounter">>, <<"size">>, signature_change
        )
    ).

relevant_diagnostic_never_keeps_error_severity_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"'ReCheckCounter' does not understand 'reset'">>,
        severity => <<"error">>,
        start => 0,
        'end' => 1
    },
    ?assertNot(
        beamtalk_recheck:relevant_diagnostic(Diag, <<"ReCheckCounter">>, <<"reset">>, removal)
    ).

relevant_diagnostic_ignores_undefined_category_test() ->
    Diag = #{
        category => undefined,
        message => <<"some parse-level note">>,
        severity => <<"warning">>,
        start => 0,
        'end' => 1
    },
    ?assertNot(
        beamtalk_recheck:relevant_diagnostic(
            Diag, <<"ReCheckCounter">>, <<"size">>, signature_change
        )
    ).

%%====================================================================
%% Integration fixture: real compiler port + xref + workspace_meta
%%====================================================================

recheck_setup() ->
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
    %% `repl => false` (run mode) is load-bearing for test isolation, not
    %% just tidiness: in REPL mode (the default) `beamtalk_workspace_meta`
    %% persists `class_sources` to
    %% `~/.beamtalk/workspaces/<workspace_id>/metadata.json` and *reloads* it
    %% on the next `init/1` — so a "fresh" restart under the same
    %% `workspace_id` would resurrect a previous test's `set_class_source/2`
    %% calls instead of starting empty (discovered via a genuinely-flaky
    %% no-live-source test here). Run mode skips disk entirely.
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"recheck_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    clear_xref(),
    beamtalk_compiler_server:clear_classes(),
    ok.

recheck_teardown(_) ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
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

%% xref payload: `ReCheckDashboard>>refresh:` sends `size` to a
%% `ReCheckCounter`-typed parameter — the real dependent.
dashboard_xref() ->
    [
        #{
            class_side => false,
            selector => 'refresh:',
            line => 2,
            sends => [#{selector => size, line => 2, recv_kind => other}],
            references => [#{class => 'ReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

%% xref payload: `ReCheckListView>>render:` also sends `size`, but to a
%% `List`-typed parameter — an unrelated same-selector site.
listview_xref() ->
    [
        #{
            class_side => false,
            selector => 'render:',
            line => 2,
            sends => [#{selector => size, line => 2, recv_kind => other}],
            references => [],
            source_status => indexed,
            provenance => class_body
        }
    ].

%% xref payload: `ReCheckDashboard>>cleanup:` sends the removed selector
%% `reset` to a `ReCheckCounter`-typed parameter.
dashboard_reset_xref() ->
    [
        #{
            class_side => false,
            selector => 'cleanup:',
            line => 2,
            sends => [#{selector => reset, line => 2, recv_kind => other}],
            references => [#{class => 'ReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

dashboard_source() ->
    <<
        "Object subclass: ReCheckDashboard\n"
        "  refresh: c :: ReCheckCounter -> Integer => (c size) + 1\n"
    >>.

listview_source() ->
    <<
        "Object subclass: ReCheckListView\n"
        "  render: xs :: List -> Integer => (xs size) + 1\n"
    >>.

dashboard_reset_source() ->
    <<
        "Object subclass: ReCheckDashboard\n"
        "  cleanup: c :: ReCheckCounter => c reset\n"
    >>.

%% `class_hierarchy` entry for `ReCheckCounter`, the channel that carries the
%% changed class's *current* signature to the checker (ADR 0105 §Mechanism
%% step 1 — populated in production by beamtalk_compiler_server's
%% register_class cache, ADR 0050 Phase 4).
counter_hierarchy(MethodInfo) ->
    #{superclass => 'Object', method_info => MethodInfo}.

signature_change_finding_and_receiver_filter_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% ReCheckCounter>>size now returns String (was Integer) —
                    %% the ambient class cache already reflects the just-installed
                    %% new generation, exactly as it would post-reload in production.
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter',
                        counter_hierarchy(#{
                            size => #{arity => 0, param_types => [], return_type => 'String'}
                        })
                    ),
                    ok = beamtalk_xref:register_class('ReCheckDashboard', dashboard_xref()),
                    ok = beamtalk_xref:register_class('ReCheckListView', listview_xref()),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_source()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckListView">>, listview_source()
                    ),

                    Result = beamtalk_recheck:trigger(
                        <<"ReCheckCounter">>, <<"size">>, instance, signature_change
                    ),
                    #{findings := Findings, checked := Checked, not_checked := NotChecked} = Result,

                    %% Both candidates were re-checked (xref is selector-keyed and
                    %% cannot tell them apart on its own) ...
                    ?assertEqual(2, Checked),
                    ?assertEqual(0, NotChecked),

                    %% ... but only the real dependent produced a finding — the
                    %% unrelated ListView site re-checks clean because `xs` is
                    %% statically `List`, not `ReCheckCounter`.
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"ReCheckDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(signature_change, maps:get(classification, Finding)),
                    %% The checker reports "String does not understand '+'"
                    %% as a Dnu-category diagnostic (an unresolved selector on
                    %% the new return type), not a Type mismatch — see
                    %% beamtalk_recheck's moduledoc.
                    ?assertEqual(<<"Dnu">>, maps:get(category, Finding)),
                    ?assert(lists:member(maps:get(severity, Finding), [<<"warning">>, <<"hint">>]))
                end)
            ]
        end}}.

removed_selector_hint_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% ReCheckCounter no longer has `reset` at all — the ambient
                    %% class cache reflects the post-removal generation.
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter', counter_hierarchy(#{})
                    ),
                    ok = beamtalk_xref:register_class('ReCheckDashboard', dashboard_reset_xref()),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_reset_source()
                    ),

                    Result = beamtalk_recheck:trigger(
                        <<"ReCheckCounter">>, <<"reset">>, instance, removal
                    ),
                    #{findings := Findings, checked := Checked} = Result,

                    ?assertEqual(1, Checked),
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"ReCheckDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(removal, maps:get(classification, Finding)),
                    ?assertEqual(<<"Dnu">>, maps:get(category, Finding)),
                    %% ADR 0100 Rule 1: an unresolved selector on a single
                    %% closed-complete receiver is a Hint, never escalated.
                    ?assertEqual(<<"hint">>, maps:get(severity, Finding)),
                    ?assertEqual(
                        <<"removed by the reload of ReCheckCounter">>, maps:get(note, Finding)
                    )
                end)
            ]
        end}}.

caller_cap_note_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    PrevCap = application:get_env(beamtalk_workspace, recheck_caller_cap, 20),
                    application:set_env(beamtalk_workspace, recheck_caller_cap, 1),
                    try
                        beamtalk_compiler_server:register_class(
                            'ReCheckCounter',
                            counter_hierarchy(#{
                                size => #{arity => 0, param_types => [], return_type => 'String'}
                            })
                        ),
                        ok = beamtalk_xref:register_class('ReCheckDashboard', dashboard_xref()),
                        ok = beamtalk_xref:register_class('ReCheckListView', listview_xref()),
                        ok = beamtalk_workspace_meta:set_class_source(
                            <<"ReCheckDashboard">>, dashboard_source()
                        ),
                        ok = beamtalk_workspace_meta:set_class_source(
                            <<"ReCheckListView">>, listview_source()
                        ),

                        Result = beamtalk_recheck:trigger(
                            <<"ReCheckCounter">>, <<"size">>, instance, signature_change
                        ),
                        #{
                            checked := Checked,
                            total_candidates := Total,
                            not_checked := NotChecked,
                            cap_note := CapNote
                        } = Result,

                        ?assertEqual(1, Checked),
                        ?assertEqual(2, Total),
                        ?assertEqual(1, NotChecked),
                        ?assertEqual(<<"1 more not checked">>, CapNote)
                    after
                        application:set_env(beamtalk_workspace, recheck_caller_cap, PrevCap)
                    end
                end)
            ]
        end}}.

%% xref payload: `ReCheckDashboard` sends `size` from TWO different methods —
%% multiple sites in one caller class must collapse into one re-check
%% candidate, not two (Mechanism step 3: "batched", one port round-trip per
%% distinct caller class, not per call site).
dashboard_two_sites_xref() ->
    [
        #{
            class_side => false,
            selector => 'refresh:',
            line => 2,
            sends => [#{selector => size, line => 2, recv_kind => other}],
            references => [#{class => 'ReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        },
        #{
            class_side => false,
            selector => 'refreshTwice:',
            line => 3,
            sends => [#{selector => size, line => 3, recv_kind => other}],
            references => [#{class => 'ReCheckCounter', line => 3}],
            source_status => indexed,
            provenance => class_body
        }
    ].

dashboard_two_sites_source() ->
    <<
        "Object subclass: ReCheckDashboard\n"
        "  refresh: c :: ReCheckCounter -> Integer => (c size) + 1\n"
        "  refreshTwice: c :: ReCheckCounter -> Integer => (c size) + (c size)\n"
    >>.

multi_site_same_owner_collapses_to_one_check_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter',
                        counter_hierarchy(#{
                            size => #{arity => 0, param_types => [], return_type => 'String'}
                        })
                    ),
                    ok = beamtalk_xref:register_class(
                        'ReCheckDashboard', dashboard_two_sites_xref()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_two_sites_source()
                    ),

                    Result = beamtalk_recheck:trigger(
                        <<"ReCheckCounter">>, <<"size">>, instance, signature_change
                    ),
                    #{
                        findings := Findings,
                        checked := Checked,
                        total_candidates := Total
                    } = Result,

                    %% ONE candidate (ReCheckDashboard), not two, even though
                    %% it has two distinct call sites of the changed selector.
                    ?assertEqual(1, Total),
                    ?assertEqual(1, Checked),
                    %% Both stale sends surface as separate diagnostics from
                    %% the single re-check, and both are attributed to the
                    %% one owner with all of its known trigger sites listed.
                    ?assert(length(Findings) >= 1),
                    [FirstFinding | _] = Findings,
                    ?assertEqual(<<"ReCheckDashboard">>, maps:get(owner, FirstFinding)),
                    SiteMethods = [
                        maps:get(method, S)
                     || F <- Findings, S <- maps:get(sites, F)
                    ],
                    ?assertEqual(
                        [<<"refresh:">>, <<"refreshTwice:">>],
                        lists:usort(SiteMethods)
                    )
                end)
            ]
        end}}.

no_live_source_is_skipped_not_checked_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter',
                        counter_hierarchy(#{
                            size => #{arity => 0, param_types => [], return_type => 'String'}
                        })
                    ),
                    %% xref knows about a sender, but no `set_class_source/2`
                    %% was ever recorded for it (e.g. a stdlib/dependency
                    %% class, never installed through beamtalk_repl_loader).
                    ok = beamtalk_xref:register_class('ReCheckDashboard', dashboard_xref()),

                    Result = beamtalk_recheck:trigger(
                        <<"ReCheckCounter">>, <<"size">>, instance, signature_change
                    ),
                    #{
                        findings := Findings,
                        checked := Checked,
                        total_candidates := Total,
                        not_checked := NotChecked
                    } = Result,

                    %% The candidate was a total candidate and NOT cap-dropped
                    %% (not_checked counts only cap overflow) — it was simply
                    %% never successfully checked, so `checked` must not count
                    %% it either.
                    ?assertEqual(1, Total),
                    ?assertEqual(0, Checked),
                    ?assertEqual(0, NotChecked),
                    ?assertEqual([], Findings)
                end)
            ]
        end}}.

%% `trigger/4`'s headline safety guarantee (see its moduledoc: "Never
%% raises") — a selector atom that was never compiled into any live class
%% (so `binary_to_existing_atom/2` inside `do_trigger/3` raises `badarg`)
%% must degrade to an empty result, not crash the caller (the install path
%% in beamtalk_repl_loader that calls this must never fail the reload
%% because the advisory re-check blew up).
trigger_never_raises_on_unknown_selector_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    NeverCompiledSelector = <<"reCheckNeverCompiledSelectorXyz">>,
                    Result = beamtalk_recheck:trigger(
                        <<"ReCheckCounter">>, NeverCompiledSelector, instance, signature_change
                    ),
                    ?assertEqual(
                        #{
                            findings => [],
                            checked => 0,
                            total_candidates => 0,
                            not_checked => 0,
                            cap_note => undefined
                        },
                        Result
                    )
                end)
            ]
        end}}.
