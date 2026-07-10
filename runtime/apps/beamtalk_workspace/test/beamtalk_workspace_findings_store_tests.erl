%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_findings_store_tests).

-moduledoc """
Unit tests for beamtalk_workspace_findings_store (ADR 0105 Phase 1, BT-2779).

Covers:
- put_owner_origin/3 replaces (never appends) a caller's finding set FOR ONE
  CHANGED CLASS, and returns the previous value for that origin
- put_owner_origin/3 for one changed class does NOT touch a different
  changed class's findings for the same owner (the CRITICAL scoping
  property — see the store's moduledoc: a caller broken by two
  independently-reloading classes must keep both findings until each is
  fixed on its own schedule)
- clear_owner/1 is deliberately UN-scoped: it wipes every origin for one
  owner in one call (mirrors "this caller's own source just changed, so
  every finding about it is stale regardless of origin")
- for_owner/1 flattens every origin for that owner, unaffected by other
  owners
- all/0 flattens every owner/origin, sorted deterministically
- clear/0 resets every owner (the explicit-reset half of the ADR's
  session-only clearing rule; the implicit half — a workspace restart starts
  a fresh, empty gen_server — is a supervision property, not testable here)

The end-to-end thread from `beamtalk_repl_loader:maybe_trigger_recheck/4`
through to a correctly-populated/cleared store entry, and the
`'ReloadCheckCompleted'` announcement, is covered in
`beamtalk_repl_loader_recheck_tests.erl`, not here — this module tests the
store's own API contract in isolation.
""".

-include_lib("eunit/include/eunit.hrl").

finding(Owner, ChangedClass, Selector, Message) ->
    #{
        owner => Owner,
        changed_class => ChangedClass,
        selector => Selector,
        classification => signature_change,
        severity => <<"warning">>,
        category => <<"Dnu">>,
        message => Message,
        note => undefined,
        sites => [#{method => <<"refresh">>, line => 14}],
        start => 0,
        'end' => 5
    }.

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = beamtalk_workspace_findings_store:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end.

store_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun fresh_store_has_no_findings/1,
        fun put_owner_origin_sets_findings/1,
        fun put_owner_origin_returns_previous_value/1,
        fun put_owner_origin_replaces_not_appends_same_origin/1,
        fun put_owner_origin_empty_clears_that_origin_only/1,
        fun different_changed_classes_are_independent_origins/1,
        fun clear_owner_wipes_every_origin_for_that_owner/1,
        fun clear_owner_returns_flattened_previous_value/1,
        fun for_owner_is_scoped_to_that_owner/1,
        fun for_owner_flattens_every_origin/1,
        fun all_flattens_every_owner_and_origin/1,
        fun all_is_sorted_deterministically/1,
        fun clear_resets_every_owner/1
    ]}.

fresh_store_has_no_findings(_Pid) ->
    [
        ?_assertEqual([], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>)),
        ?_assertEqual([], beamtalk_workspace_findings_store:all())
    ].

put_owner_origin_sets_findings(_Pid) ->
    F = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"msg1">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [F]),
    [
        ?_assertEqual([F], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>))
    ].

put_owner_origin_returns_previous_value(_Pid) ->
    F1 = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"msg1">>),
    F2 = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"msg2">>),
    Prev1 = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [F1]),
    Prev2 = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [F2]),
    [
        ?_assertEqual([], Prev1),
        ?_assertEqual([F1], Prev2)
    ].

put_owner_origin_replaces_not_appends_same_origin(_Pid) ->
    F1 = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"generation A">>),
    F2 = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"generation B">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [F1]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [F2]),
    [
        ?_assertEqual([F2], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>))
    ].

put_owner_origin_empty_clears_that_origin_only(_Pid) ->
    F = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"msg1">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [F]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, []),
    [
        ?_assertEqual([], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>)),
        ?_assertEqual([], beamtalk_workspace_findings_store:all())
    ].

%% The CRITICAL scoping property (see the store's moduledoc): Dashboard is
%% broken by both Counter and Widget, in separate reloads. Replacing
%% Counter's origin must not discard Widget's still-valid finding, and
%% clearing Counter's origin (a clean re-check) must leave Widget's intact.
different_changed_classes_are_independent_origins(_Pid) ->
    CounterFinding = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"counter msg">>),
    WidgetFinding = finding(<<"Dashboard">>, <<"Widget">>, <<"size">>, <<"widget msg">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [
        CounterFinding
    ]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Widget">>, [
        WidgetFinding
    ]),
    BothPresent = beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>),

    %% Counter is fixed (a clean re-check) — only Counter's origin clears.
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, []),
    AfterCounterFixed = beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>),
    [
        ?_assertEqual(
            lists:sort([CounterFinding, WidgetFinding]), lists:sort(BothPresent)
        ),
        ?_assertEqual([WidgetFinding], AfterCounterFixed)
    ].

%% clear_owner/1 is deliberately un-scoped: the owner's OWN source changed,
%% so every origin (regardless of which changed class produced it) is
%% invalidated in one call.
clear_owner_wipes_every_origin_for_that_owner(_Pid) ->
    CounterFinding = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"counter msg">>),
    WidgetFinding = finding(<<"Dashboard">>, <<"Widget">>, <<"size">>, <<"widget msg">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [
        CounterFinding
    ]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Widget">>, [
        WidgetFinding
    ]),
    _ = beamtalk_workspace_findings_store:clear_owner(<<"Dashboard">>),
    [
        ?_assertEqual([], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>))
    ].

clear_owner_returns_flattened_previous_value(_Pid) ->
    CounterFinding = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"counter msg">>),
    WidgetFinding = finding(<<"Dashboard">>, <<"Widget">>, <<"size">>, <<"widget msg">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [
        CounterFinding
    ]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Widget">>, [
        WidgetFinding
    ]),
    Prev = beamtalk_workspace_findings_store:clear_owner(<<"Dashboard">>),
    [
        ?_assertEqual(
            lists:sort([CounterFinding, WidgetFinding]), lists:sort(Prev)
        )
    ].

for_owner_is_scoped_to_that_owner(_Pid) ->
    FDash = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"dash msg">>),
    FStats = finding(<<"StatsView">>, <<"Counter">>, <<"getCount">>, <<"stats msg">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [FDash]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"StatsView">>, <<"Counter">>, [FStats]),
    [
        ?_assertEqual([FDash], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>)),
        ?_assertEqual([FStats], beamtalk_workspace_findings_store:for_owner(<<"StatsView">>))
    ].

for_owner_flattens_every_origin(_Pid) ->
    CounterFinding = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"counter msg">>),
    WidgetFinding = finding(<<"Dashboard">>, <<"Widget">>, <<"size">>, <<"widget msg">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [
        CounterFinding
    ]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Widget">>, [
        WidgetFinding
    ]),
    Findings = beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>),
    [
        ?_assertEqual(2, length(Findings)),
        ?_assert(lists:member(CounterFinding, Findings)),
        ?_assert(lists:member(WidgetFinding, Findings))
    ].

all_flattens_every_owner_and_origin(_Pid) ->
    FDash = finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"dash msg">>),
    FStats = finding(<<"StatsView">>, <<"Counter">>, <<"getCount">>, <<"stats msg">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [FDash]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"StatsView">>, <<"Counter">>, [FStats]),
    All = beamtalk_workspace_findings_store:all(),
    [
        ?_assertEqual(2, length(All)),
        ?_assert(lists:member(FDash, All)),
        ?_assert(lists:member(FStats, All))
    ].

all_is_sorted_deterministically(_Pid) ->
    FZeta = finding(<<"Zeta">>, <<"Counter">>, <<"a">>, <<"m">>),
    FAlpha = finding(<<"Alpha">>, <<"Counter">>, <<"a">>, <<"m">>),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Zeta">>, <<"Counter">>, [FZeta]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Alpha">>, <<"Counter">>, [FAlpha]),
    All = beamtalk_workspace_findings_store:all(),
    Owners = [maps:get(owner, F) || F <- All],
    [
        ?_assertEqual([<<"Alpha">>, <<"Zeta">>], Owners)
    ].

clear_resets_every_owner(_Pid) ->
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"Dashboard">>, <<"Counter">>, [
        finding(<<"Dashboard">>, <<"Counter">>, <<"getCount">>, <<"m">>)
    ]),
    _ = beamtalk_workspace_findings_store:put_owner_origin(<<"StatsView">>, <<"Counter">>, [
        finding(<<"StatsView">>, <<"Counter">>, <<"getCount">>, <<"m">>)
    ]),
    ok = beamtalk_workspace_findings_store:clear(),
    [
        ?_assertEqual([], beamtalk_workspace_findings_store:all()),
        ?_assertEqual([], beamtalk_workspace_findings_store:for_owner(<<"Dashboard">>)),
        ?_assertEqual([], beamtalk_workspace_findings_store:for_owner(<<"StatsView">>))
    ].
