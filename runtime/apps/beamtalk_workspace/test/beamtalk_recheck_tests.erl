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
%% Pure helper — override_method_signature/4 (ADR 0105 Phase 3, BT-2782)
%%====================================================================

override_method_signature_splices_pending_type_test() ->
    AmbientMeta = #{
        superclass => 'Object',
        method_info => #{
            size => #{arity => 0, param_types => [], return_type => 'Integer'}
        }
    },
    NewMeta = beamtalk_recheck:override_method_signature(
        AmbientMeta, instance, size, #{return_type => <<"String">>, param_types => []}
    ),
    #{method_info := #{size := Entry}} = NewMeta,
    ?assertEqual('String', maps:get(return_type, Entry)),
    %% arity is preserved from the existing entry, not clobbered.
    ?assertEqual(0, maps:get(arity, Entry)),
    %% Every other field of the ambient meta is untouched.
    ?assertEqual('Object', maps:get(superclass, NewMeta)).

override_method_signature_dynamic_becomes_none_test() ->
    AmbientMeta = #{
        method_info => #{foo => #{arity => 0, param_types => [], return_type => 'Integer'}}
    },
    NewMeta = beamtalk_recheck:override_method_signature(
        AmbientMeta, instance, foo, #{return_type => <<"Dynamic">>, param_types => [<<"Dynamic">>]}
    ),
    #{method_info := #{foo := Entry}} = NewMeta,
    ?assertEqual(none, maps:get(return_type, Entry)),
    ?assertEqual([none], maps:get(param_types, Entry)).

override_method_signature_unknown_type_name_falls_back_to_none_test() ->
    %% A type name with no existing atom (never loaded this session) must not
    %% mint a fresh atom — falls back to `none` (Dynamic) instead.
    AmbientMeta = #{
        method_info => #{foo => #{arity => 0, param_types => [], return_type => 'Integer'}}
    },
    NewMeta = beamtalk_recheck:override_method_signature(
        AmbientMeta,
        instance,
        foo,
        #{return_type => <<"ThisAtomSurelyDoesNotExistYet12345">>, param_types => []}
    ),
    #{method_info := #{foo := Entry}} = NewMeta,
    ?assertEqual(none, maps:get(return_type, Entry)).

override_method_signature_class_side_uses_class_method_info_test() ->
    AmbientMeta = #{
        class_method_info => #{new => #{arity => 0, param_types => [], return_type => 'Counter'}}
    },
    NewMeta = beamtalk_recheck:override_method_signature(
        AmbientMeta, class, new, #{return_type => <<"String">>, param_types => []}
    ),
    #{class_method_info := #{new := Entry}} = NewMeta,
    ?assertEqual('String', maps:get(return_type, Entry)),
    %% instance-side method_info is untouched (absent, in this fixture).
    ?assertEqual(error, maps:find(method_info, NewMeta)).

override_method_signature_brand_new_selector_gets_bare_entry_test() ->
    %% A selector with no existing ambient entry (never installed) gets a
    %% bare entry with just the overridden type fields — no arity. Harmless:
    %% a brand-new method has no dependents for xref to find anyway.
    AmbientMeta = #{method_info => #{}},
    NewMeta = beamtalk_recheck:override_method_signature(
        AmbientMeta, instance, brandNew, #{return_type => <<"Integer">>, param_types => []}
    ),
    #{method_info := #{brandNew := Entry}} = NewMeta,
    ?assertEqual('Integer', maps:get(return_type, Entry)),
    ?assertEqual(error, maps:find(arity, Entry)).

%%====================================================================
%% Pure helper — relevant_image_diagnostic/1 (ADR 0105 Phase 3, BT-2782)
%%====================================================================

relevant_image_diagnostic_keeps_dnu_test() ->
    ?assert(
        beamtalk_recheck:relevant_image_diagnostic(#{
            category => <<"Dnu">>, severity => <<"hint">>
        })
    ).

relevant_image_diagnostic_keeps_type_test() ->
    ?assert(
        beamtalk_recheck:relevant_image_diagnostic(#{
            category => <<"Type">>, severity => <<"warning">>
        })
    ).

relevant_image_diagnostic_drops_error_severity_test() ->
    ?assertNot(
        beamtalk_recheck:relevant_image_diagnostic(#{
            category => <<"Dnu">>, severity => <<"error">>
        })
    ).

relevant_image_diagnostic_drops_unrelated_category_test() ->
    ?assertNot(
        beamtalk_recheck:relevant_image_diagnostic(#{
            category => <<"Unused">>, severity => <<"lint">>
        })
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
                            cap_note => undefined,
                            checked_owners => []
                        },
                        Result
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Shape-change re-check (ADR 0105 Phase 2, BT-2780)
%%====================================================================

%% Pure helper — shape_dependent_selectors/1.

shape_dependent_selectors_always_includes_spawn_with_test() ->
    Selectors = beamtalk_recheck:shape_dependent_selectors([{added, <<"count">>}]),
    ?assertEqual(['spawnWith:'], Selectors).

%% Referencing 'count' and 'withCount:' literally interns them as atoms for
%% this test run, satisfying the `binary_to_existing_atom/2` inside
%% `field_accessor_atoms/1` — see that function's doc for why an
%% un-interned selector is skipped rather than minted fresh.
shape_dependent_selectors_removed_field_includes_accessor_test() ->
    _ = 'withCount:',
    Selectors = beamtalk_recheck:shape_dependent_selectors([{removed, <<"count">>}]),
    ?assert(lists:member('spawnWith:', Selectors)),
    ?assert(lists:member(count, Selectors)),
    ?assert(lists:member('withCount:', Selectors)).

shape_dependent_selectors_retyped_field_includes_accessor_test() ->
    _ = 'withTimeout:',
    Selectors = beamtalk_recheck:shape_dependent_selectors(
        [{retyped, <<"timeout">>, <<"Integer">>, <<"String">>}]
    ),
    ?assert(lists:member(timeout, Selectors)),
    ?assert(lists:member('withTimeout:', Selectors)).

shape_dependent_selectors_dedupes_test() ->
    _ = 'withCount:',
    Selectors = beamtalk_recheck:shape_dependent_selectors([
        {removed, <<"count">>}, {retyped, <<"count">>, <<"Integer">>, <<"String">>}
    ]),
    ?assertEqual(lists:usort(Selectors), Selectors).

%% Pure helper — with_star_selector/1 (mirrors the Rust naming authority).

with_star_selector_test_() ->
    [
        ?_assertEqual(<<"withCount:">>, beamtalk_recheck:with_star_selector(<<"count">>)),
        ?_assertEqual(
            <<"withFirstName:">>, beamtalk_recheck:with_star_selector(<<"firstName">>)
        ),
        ?_assertEqual(<<"with:">>, beamtalk_recheck:with_star_selector(<<>>))
    ].

%% Pure helper — relevant_diagnostic_shape/3.

relevant_diagnostic_shape_matches_spawn_with_unknown_key_test() ->
    Diag = #{
        category => <<"Type">>,
        message => <<"unknown state key `name` for `ReCheckCounter`">>,
        severity => <<"warning">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        {true, {removed, <<"name">>}},
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [{removed, <<"name">>}]
        )
    ).

relevant_diagnostic_shape_ignores_unrelated_state_key_test() ->
    %% Names a real "state key" diagnostic, but for a field that is not part
    %% of this shape change — must not be misattributed.
    Diag = #{
        category => <<"Type">>,
        message => <<"unknown state key `other` for `ReCheckCounter`">>,
        severity => <<"warning">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        false,
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [{removed, <<"name">>}]
        )
    ).

relevant_diagnostic_shape_matches_removed_accessor_dnu_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"'ReCheckCounter' does not understand 'name'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        {true, {removed, <<"name">>}},
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [{removed, <<"name">>}]
        )
    ).

%% A retyped slot's accessor propagates its new return type downstream as an
%% ordinary Dnu the checker cannot pin to a quoted selector (see
%% relevant_diagnostic_shape/3's doc) — falls back to the first retyped
%% change when no removed-accessor selector matched.
relevant_diagnostic_shape_retyped_fallback_matches_unrelated_dnu_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"String does not understand '+'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        {true, {retyped, <<"count">>, <<"Integer">>, <<"String">>}},
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [{retyped, <<"count">>, <<"Integer">>, <<"String">>}]
        )
    ).

%% Two retyped slots in the same reload — the Dnu carries no field-name
%% signal (see relevant_diagnostic_shape/3's doc), so retyped_fallback/1
%% cannot tell which slot actually caused it and picks the first by list
%% order, not the true culprit. Pinned deliberately (BT-2780 adversarial
%% review): this is a known, accepted precision limit — the fallback would
%% attribute this exact diagnostic to `count` even if `label`'s retype were
%% the real cause, or vice versa. See beamtalk_recheck.erl's
%% relevant_diagnostic_shape/3 moduledoc.
relevant_diagnostic_shape_retyped_fallback_picks_first_when_multiple_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"String does not understand '+'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        {true, {retyped, <<"count">>, <<"Integer">>, <<"String">>}},
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [
                {retyped, <<"count">>, <<"Integer">>, <<"String">>},
                {retyped, <<"label">>, <<"String">>, <<"Symbol">>}
            ]
        )
    ),
    %% Order in FieldChanges alone decides the (mis)attribution — swapping
    %% the list order flips which slot gets blamed for the identical Dnu.
    ?assertEqual(
        {true, {retyped, <<"label">>, <<"String">>, <<"Symbol">>}},
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [
                {retyped, <<"label">>, <<"String">>, <<"Symbol">>},
                {retyped, <<"count">>, <<"Integer">>, <<"String">>}
            ]
        )
    ).

%% No retyped change in flight — an unrelated Dnu is not attributed at all.
relevant_diagnostic_shape_no_retyped_fallback_when_only_added_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"String does not understand '+'">>,
        severity => <<"hint">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        false,
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [{added, <<"count">>}]
        )
    ).

relevant_diagnostic_shape_never_keeps_error_severity_test() ->
    Diag = #{
        category => <<"Dnu">>,
        message => <<"'ReCheckCounter' does not understand 'name'">>,
        severity => <<"error">>,
        start => 0,
        'end' => 1
    },
    ?assertEqual(
        false,
        beamtalk_recheck:relevant_diagnostic_shape(
            Diag, <<"ReCheckCounter">>, [{removed, <<"name">>}]
        )
    ).

%%====================================================================
%% Integration: trigger_shape/2
%%====================================================================

%% `class_hierarchy` entry for an Actor subclass with the given field types
%% (ADR 0105 §Mechanism step 1 equivalent for shape — the ambient cache
%% carries the class's *current* fields/field_types, exactly as
%% `counter_hierarchy/1` carries `method_info` for the signature path).
counter_actor_hierarchy(FieldTypes) ->
    #{
        superclass => 'Actor',
        fields => maps:keys(FieldTypes),
        field_types => FieldTypes
    }.

%% xref payload: `ReCheckDashboard>>cleanup:` sends the removed accessor
%% selector `name` to a `ReCheckCounter`-typed parameter.
dashboard_name_accessor_xref() ->
    [
        #{
            class_side => false,
            selector => 'cleanup:',
            line => 2,
            sends => [#{selector => name, line => 2, recv_kind => other}],
            references => [#{class => 'ReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

dashboard_name_accessor_source() ->
    <<
        "Object subclass: ReCheckDashboard\n"
        "  cleanup: c :: ReCheckCounter => c name\n"
    >>.

removed_field_accessor_is_flagged_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% ReCheckCounter's `name` field was removed — only `count`
                    %% remains in the ambient (post-reload) hierarchy.
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter', counter_actor_hierarchy(#{count => 'Integer'})
                    ),
                    ok = beamtalk_xref:register_class(
                        'ReCheckDashboard', dashboard_name_accessor_xref()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_name_accessor_source()
                    ),

                    Result = beamtalk_recheck:trigger_shape(
                        <<"ReCheckCounter">>, [{removed, <<"name">>}]
                    ),
                    #{findings := Findings, checked := Checked} = Result,

                    ?assertEqual(1, Checked),
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"ReCheckDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(shape_change, maps:get(classification, Finding)),
                    ?assertEqual(<<"name">>, maps:get(selector, Finding)),
                    ?assertEqual(<<"Dnu">>, maps:get(category, Finding)),
                    ?assertEqual(
                        <<"state field `name` removed by the reload of ReCheckCounter">>,
                        maps:get(note, Finding)
                    )
                end)
            ]
        end}}.

%% xref payload: `ReCheckDashboard>>build` sends `spawnWith:` to the class
%% literal `ReCheckCounter`.
dashboard_spawn_with_xref() ->
    [
        #{
            class_side => false,
            selector => build,
            line => 2,
            sends => [#{selector => 'spawnWith:', line => 2, recv_kind => other}],
            references => [#{class => 'ReCheckCounter', line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ].

dashboard_spawn_with_source() ->
    <<
        "Object subclass: ReCheckDashboard\n"
        "  build => ReCheckCounter spawnWith: #{#count => 1, #name => 'x'}\n"
    >>.

removed_field_flags_spawn_with_site_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter', counter_actor_hierarchy(#{count => 'Integer'})
                    ),
                    ok = beamtalk_xref:register_class(
                        'ReCheckDashboard', dashboard_spawn_with_xref()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_spawn_with_source()
                    ),

                    Result = beamtalk_recheck:trigger_shape(
                        <<"ReCheckCounter">>, [{removed, <<"name">>}]
                    ),
                    #{findings := Findings, checked := Checked} = Result,

                    ?assertEqual(1, Checked),
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"ReCheckDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(shape_change, maps:get(classification, Finding)),
                    ?assertEqual(<<"name">>, maps:get(selector, Finding)),
                    ?assertEqual(<<"Type">>, maps:get(category, Finding)),
                    ?assert(
                        binary:match(maps:get(message, Finding), <<"unknown state key">>) =/=
                            nomatch
                    )
                end)
            ]
        end}}.

%% Added field clears a previously-invalid spawnWith: key — the same class
%% hierarchy as above but with `name` back in the ambient shape, exercising
%% the "reload-fixes-reload" clearing story: a clean re-check produces no
%% findings for this origin, which is what BT-2779's findings-store consumer
%% needs to replace a stale entry with an empty one.
added_field_clears_previously_invalid_spawn_with_key_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter',
                        counter_actor_hierarchy(#{count => 'Integer', name => 'Dynamic'})
                    ),
                    ok = beamtalk_xref:register_class(
                        'ReCheckDashboard', dashboard_spawn_with_xref()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_spawn_with_source()
                    ),

                    Result = beamtalk_recheck:trigger_shape(
                        <<"ReCheckCounter">>, [{added, <<"name">>}]
                    ),
                    #{findings := Findings, checked := Checked} = Result,

                    ?assertEqual(1, Checked),
                    ?assertEqual([], Findings)
                end)
            ]
        end}}.

%%====================================================================
%% Integration — trigger_pending/5 (ADR 0105 Phase 3, BT-2782, pre-save advisory)
%%====================================================================

trigger_pending_finds_stale_dependent_without_installing_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% Currently-live (installed) signature: size -> Integer.
                    beamtalk_compiler_server:register_class(
                        'ReCheckCounter',
                        counter_hierarchy(#{
                            size => #{arity => 0, param_types => [], return_type => 'Integer'}
                        })
                    ),
                    ok = beamtalk_xref:register_class('ReCheckDashboard', dashboard_xref()),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckDashboard">>, dashboard_source()
                    ),

                    %% Pending, NOT YET installed edit: size -> String.
                    PendingSignature = #{return_type => <<"String">>, param_types => []},
                    Result = beamtalk_recheck:trigger_pending(
                        <<"ReCheckCounter">>,
                        <<"size">>,
                        instance,
                        signature_change,
                        PendingSignature
                    ),
                    #{findings := Findings, checked := Checked} = Result,
                    ?assertEqual(1, Checked),
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"ReCheckDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(signature_change, maps:get(classification, Finding)),

                    %% Never installed: the ambient class cache is restored to
                    %% the real, still-live signature afterward — a pending
                    %% edit that never saved must not leave the compiler's
                    %% shared class cache holding a hypothetical signature.
                    #{'ReCheckCounter' := RestoredMeta} = beamtalk_compiler_server:get_classes(),
                    #{method_info := #{size := RestoredEntry}} = RestoredMeta,
                    ?assertEqual('Integer', maps:get(return_type, RestoredEntry))
                end)
            ]
        end}}.

trigger_pending_no_ambient_meta_is_empty_result_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% ReCheckCounter was never registered this session at
                    %% all — no baseline to splice a pending signature into.
                    Result = beamtalk_recheck:trigger_pending(
                        <<"ReCheckCounter">>,
                        <<"size">>,
                        instance,
                        signature_change,
                        #{return_type => <<"String">>, param_types => []}
                    ),
                    ?assertEqual(
                        #{
                            findings => [],
                            checked => 0,
                            total_candidates => 0,
                            not_checked => 0,
                            cap_note => undefined,
                            checked_owners => []
                        },
                        Result
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Integration — trigger_image/0 (ADR 0105 Phase 3, BT-2782, :recheck image)
%%====================================================================

recheck_image_clean_source() ->
    <<"Object subclass: ReCheckImageClean\n  ok -> Integer => 42\n">>.

recheck_image_broken_source() ->
    <<"Object subclass: ReCheckImageBroken\n  boom -> Integer => 42 totallyNotARealSelector\n">>.

trigger_image_reports_checked_and_stale_across_the_image_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckImageClean">>, recheck_image_clean_source()
                    ),
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"ReCheckImageBroken">>, recheck_image_broken_source()
                    ),

                    Result = beamtalk_recheck:trigger_image(),
                    #{findings := Findings, checked := Checked, stale := Stale} = Result,

                    %% Both live classes' diagnostics round-trips completed...
                    ?assertEqual(2, Checked),
                    %% ...but only the genuinely-broken one is stale.
                    ?assertEqual(1, Stale),
                    ?assert(length(Findings) >= 1),
                    ?assert(
                        lists:all(
                            fun(F) -> maps:get(owner, F) =:= <<"ReCheckImageBroken">> end, Findings
                        )
                    )
                end)
            ]
        end}}.

trigger_image_empty_when_no_classes_test_() ->
    {timeout, 30,
        {setup, fun recheck_setup/0, fun recheck_teardown/1, fun(_) ->
            [
                ?_test(
                    ?assertEqual(
                        #{findings => [], checked => 0, stale => 0},
                        beamtalk_recheck:trigger_image()
                    )
                )
            ]
        end}}.
