#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Spike instrumentation: ADR 0115 Phase 1 validation (BT-3216).
%%
%% Measures the two runtime costs ADR 0115's read path assumes are cheap, and
%% exercises the proposed `is_relevant/3` default-fallback against a hand-built
%% site fixture. Nothing here is production code — `senders_of/2`,
%% `hierarchy_related_classes/1` and `is_relevant/3` are re-implemented locally,
%% exactly as ADR 0115 §"Read path" writes them, so the spike measures the
%% design as specified without landing any schema or runtime change (BT-3216 is
%% investigation-only).
%%
%% Run from the `runtime/` directory after `just build`:
%%
%%   escript perf/bench_recv_type_spike.escript

-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    timer:sleep(500),
    lists:foreach(
        fun(Beam) ->
            Mod = list_to_atom(filename:basename(Beam, ".beam")),
            code:ensure_loaded(Mod)
        end,
        filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")
    ),
    timer:sleep(2000),

    Classes = [N || {N, _M, _P} <- beamtalk_class_registry:live_class_entries()],
    io:format("~n=== Workspace ===~n"),
    io:format("loaded classes: ~p~n", [length(Classes)]),
    report_shape(Classes),

    io:format("~n=== A. hierarchy_related_classes/1 cost ===~n"),
    bench_related(Classes),

    io:format("~n=== B. beamtalk_protocol_registry:conforms_to/2 cost ===~n"),
    bench_conforms(Classes),

    io:format("~n=== C. is_relevant/3 default-fallback fixture ===~n"),
    fixture_is_relevant(),

    io:format("~n=== D. end-to-end senders_of/2 per-trigger cost ===~n"),
    bench_senders_of_2(),
    ok.

%%--------------------------------------------------------------------
%% Hierarchy shape
%%--------------------------------------------------------------------

report_shape(Classes) ->
    Depths = [{C, depth(C, 0)} || C <- Classes],
    MaxDepth = lists:max([D || {_, D} <- Depths]),
    Breadths = [{C, length(beamtalk_class_registry:all_subclasses(C))} || C <- Classes],
    Sorted = lists:reverse(lists:keysort(2, Breadths)),
    io:format("max ancestor depth: ~p~n", [MaxDepth]),
    io:format("deepest: ~p~n", [lists:sublist(lists:reverse(lists:keysort(2, Depths)), 5)]),
    io:format("widest all_subclasses/1: ~p~n", [lists:sublist(Sorted, 5)]),
    %% direct_subclasses/1 is a full-table ets:match on a set table keyed by
    %% class name (superclass is not a key), so it is O(classes) per call and
    %% all_subclasses/1 makes one such call per node in the subtree.
    Root = element(1, hd(Sorted)),
    {MatchUs, _} = time_avg(
        fun() -> beamtalk_class_metadata:match_subclasses(Root) end, 500
    ),
    {AllUs, _} = time_avg(fun() -> beamtalk_class_registry:all_subclasses(Root) end, 200),
    io:format(
        "  match_subclasses/1 ~.2f us | all_subclasses(~p) ~.1f us => ~.1f match scans~n",
        [MatchUs, Root, AllUs, AllUs / MatchUs]
    ).

depth(_C, N) when N > 64 ->
    N;
depth(C, N) ->
    case beamtalk_class_metadata:lookup_superclass(C) of
        {ok, Super} when Super =/= none, Super =/= undefined -> depth(Super, N + 1);
        _ -> N
    end.

%%--------------------------------------------------------------------
%% A. hierarchy_related_classes/1
%%--------------------------------------------------------------------

%% ADR 0115 §Read path: {C} ∪ ancestors(C) ∪ subclasses(C), built from the
%% registry's existing superclass-chain walk and direct_subclasses/1 closure.
hierarchy_related_classes(C) ->
    sets:from_list([C | ancestors(C, [])] ++ beamtalk_class_registry:all_subclasses(C)).

ancestors(_C, Acc) when length(Acc) > 64 ->
    Acc;
ancestors(C, Acc) ->
    case beamtalk_class_metadata:lookup_superclass(C) of
        {ok, Super} when Super =/= none, Super =/= undefined -> ancestors(Super, [Super | Acc]);
        _ -> Acc
    end.

bench_related(Classes) ->
    %% Root (worst case: whole hierarchy is a descendant), a mid-tree class, a leaf.
    Probes = probes(Classes),
    lists:foreach(
        fun({Label, C}) ->
            _ = hierarchy_related_classes(C),
            {Us, Size} = time_avg(
                fun() -> sets:size(hierarchy_related_classes(C)) end, 200
            ),
            io:format("  ~s ~p related=~p ~.1f us/call~n", [Label, C, Size, Us])
        end,
        Probes
    ).

probes(Classes) ->
    Breadths = [{C, length(beamtalk_class_registry:all_subclasses(C))} || C <- Classes],
    Sorted = lists:reverse(lists:keysort(2, Breadths)),
    {Widest, _} = hd(Sorted),
    {Narrowest, _} = lists:last(Sorted),
    Mid = element(1, lists:nth(max(1, length(Sorted) div 2), Sorted)),
    [{"root", Widest}, {"mid", Mid}, {"leaf", Narrowest}].

%%--------------------------------------------------------------------
%% B. conforms_to/2
%%--------------------------------------------------------------------

bench_conforms(Classes) ->
    Protocols = beamtalk_protocol_registry:all_protocol_names(),
    io:format("  registered protocols: ~p~n", [length(Protocols)]),
    case Protocols of
        [] ->
            io:format("  (no protocols registered in a bare stdlib workspace)~n"),
            io:format("  registering a synthetic 3-method protocol to measure the shape~n"),
            ok = register_synthetic_protocol(),
            bench_conforms_for(['SpikeProto'], Classes);
        _ ->
            bench_conforms_for(lists:sublist(Protocols, 3), Classes)
    end.

register_synthetic_protocol() ->
    beamtalk_protocol_registry:register_protocol(#{
        name => 'SpikeProto',
        module => spike_proto_mod,
        methods => [
            #{selector => 'printOn:', arity => 1},
            #{selector => 'size', arity => 0},
            #{selector => '=', arity => 1}
        ],
        class_methods => [],
        extending => []
    }).

bench_conforms_for(Protocols, Classes) ->
    Sample = lists:sublist(Classes, 1, 12),
    lists:foreach(
        fun(P) ->
            _ = [beamtalk_protocol_registry:conforms_to(C, P) || C <- Sample],
            {Us, _} = time_avg(
                fun() ->
                    [beamtalk_protocol_registry:conforms_to(C, P) || C <- Sample]
                end,
                20
            ),
            io:format(
                "  protocol ~p ~.1f us for ~p classes (~.1f us/conforms_to call)~n",
                [P, Us, length(Sample), Us / length(Sample)]
            )
        end,
        Protocols
    ).

%%--------------------------------------------------------------------
%% C. is_relevant/3 fixture — ADR 0115 §Read path, verbatim
%%--------------------------------------------------------------------

is_relevant(#{recv_type := dynamic}, _ChangedClass, _Related) ->
    true;
is_relevant(#{recv_type := T}, ChangedClass, Related) ->
    case beamtalk_protocol_registry:is_protocol(T) of
        true -> beamtalk_protocol_registry:conforms_to(ChangedClass, T);
        false -> sets:is_element(T, Related)
    end;
is_relevant(#{}, _ChangedClass, _Related) ->
    true.

fixture_is_relevant() ->
    %% Pick a real class with both an ancestor and a descendant so the
    %% relatedness arms are exercised against the live registry.
    Changed = pick_mid_class(),
    Related = hierarchy_related_classes(Changed),
    Ancestor = case ancestors(Changed, []) of
        [] -> 'Object';
        As -> lists:last(As)
    end,
    Descendant = case beamtalk_class_registry:all_subclasses(Changed) of
        [] -> Changed;
        [D | _] -> D
    end,
    Unrelated = pick_unrelated(Changed, Related),
    io:format("  changed=~p ancestor=~p descendant=~p unrelated=~p~n",
              [Changed, Ancestor, Descendant, Unrelated]),
    Cases = [
        {"legacy row (no recv_type key)", #{owner => 'X', line => 1}, true},
        {"explicit dynamic", #{owner => 'X', recv_type => dynamic}, true},
        {"self (== changed class)", #{owner => 'X', recv_type => Changed}, true},
        {"ancestor-typed receiver", #{owner => 'X', recv_type => Ancestor}, true},
        {"descendant-typed receiver", #{owner => 'X', recv_type => Descendant}, true},
        {"unrelated branch", #{owner => 'X', recv_type => Unrelated}, false},
        {"unknown/never-registered name", #{owner => 'X', recv_type => 'NoSuchClassXYZ'}, unknown},
        {"unregistered protocol name", #{owner => 'X', recv_type => 'UnloadedProto'}, unknown}
    ],
    lists:foreach(
        fun({Label, Site, Expect}) ->
            Got = is_relevant(Site, Changed, Related),
            Mark = case Expect of
                unknown -> "?";
                Got -> "ok";
                _ -> "MISMATCH"
            end,
            io:format("  ~s -> ~p expected ~p ~s~n", [Label, Got, Expect, Mark])
        end,
        Cases
    ).

pick_mid_class() ->
    Classes = [N || {N, _M, _P} <- beamtalk_class_registry:live_class_entries()],
    Candidates = [
        C
     || C <- Classes,
        beamtalk_class_registry:all_subclasses(C) =/= [],
        ancestors(C, []) =/= []
    ],
    case Candidates of
        [] -> 'Object';
        [C | _] -> C
    end.

pick_unrelated(Changed, Related) ->
    Classes = [N || {N, _M, _P} <- beamtalk_class_registry:live_class_entries()],
    %% Exclude registered protocol names — a protocol-typed site takes the
    %% conforms_to/2 branch, not the hierarchy branch, so it is not a valid
    %% probe for "unrelated nominal class".
    case
        [
            C
         || C <- Classes,
            C =/= Changed,
            not sets:is_element(C, Related),
            not beamtalk_protocol_registry:is_protocol(C)
        ]
    of
        [] -> 'NoUnrelatedClass';
        [C | _] -> C
    end.

%%--------------------------------------------------------------------
%% D. End-to-end senders_of/2, as ADR 0115 writes it, vs a memoised variant
%%--------------------------------------------------------------------

%% ADR 0115 §Read path, verbatim.
senders_of_2(Selector, ChangedClass) ->
    AllSites = beamtalk_xref:senders_of(Selector),
    Related = hierarchy_related_classes(ChangedClass),
    [S || S <- AllSites, is_relevant(S, ChangedClass, Related)].

bench_senders_of_2() ->
    Selector = hottest_selector(),
    Sites = beamtalk_xref:senders_of(Selector),
    N = length(Sites),
    io:format("  hottest selector: ~p (~p sites today)~n", [Selector, N]),
    Changed = 'Collection',
    io:format("  changed class: ~p~n", [Changed]),

    {BaseUs, _} = time_avg(fun() -> beamtalk_xref:senders_of(Selector) end, 200),
    io:format("  senders_of/1 (today, no filter)          ~.1f us~n", [BaseUs]),

    %% (a) Fully unmigrated index: every row hits is_relevant/3's legacy clause.
    {LegacyUs, _} = time_avg(fun() -> length(senders_of_2(Selector, Changed)) end, 200),
    io:format("  senders_of/2, all-legacy rows            ~.1f us~n", [LegacyUs]),

    %% (b) Realistic migrated index: receiver types concentrated on a handful
    %% of nominal classes (what ordinary code produces), plus dynamic.
    Skewed = stamp(Sites, ['List', 'Dictionary', 'String', 'Array', dynamic, dynamic]),
    report_predicate("migrated, nominal-skewed", Skewed, Changed),

    %% (c) Worst case for the protocol branch: every row protocol-typed, so
    %% senders_of/2 pays one conforms_to/2 per site. This is the "new call
    %% volume the registry was not sized for" case.
    case beamtalk_protocol_registry:all_protocol_names() of
        [] ->
            io:format("  (no protocols registered — skipping worst case)~n");
        Protos ->
            AllProto = stamp(Sites, Protos),
            report_predicate("migrated, all-protocol", AllProto, Changed)
    end,

    %% (d) Worst case for the hierarchy branch: changing a root class, so
    %% hierarchy_related_classes/1 closes over the whole loaded hierarchy.
    {RootUs, _} = time_avg(fun() -> length(senders_of_2(Selector, 'Object')) end, 50),
    io:format("  senders_of/2 with changed=Object         ~.1f us~n", [RootUs]).

report_predicate(Label, Rows, Changed) ->
    {Us, Kept} = time_avg(
        fun() ->
            Related = hierarchy_related_classes(Changed),
            length([S || S <- Rows, is_relevant(S, Changed, Related)])
        end,
        50
    ),
    {MemoUs, MemoKept} = time_avg(
        fun() -> length(senders_of_2_memo_list(Rows, Changed)) end, 50
    ),
    io:format(
        "  ~s: verbatim ~.1f us (kept ~p) | memoised ~.1f us (kept ~p)~n",
        [Label, Us, Kept, MemoUs, MemoKept]
    ).

senders_of_2_memo_list(Sites, ChangedClass) ->
    Related = hierarchy_related_classes(ChangedClass),
    {Kept, _} = lists:foldl(
        fun(S, {Acc, Cache}) ->
            case maps:find(recv_type, S) of
                error ->
                    {[S | Acc], Cache};
                {ok, dynamic} ->
                    {[S | Acc], Cache};
                {ok, T} ->
                    case maps:find(T, Cache) of
                        {ok, true} -> {[S | Acc], Cache};
                        {ok, false} -> {Acc, Cache};
                        error ->
                            R = is_relevant(S, ChangedClass, Related),
                            C2 = Cache#{T => R},
                            case R of
                                true -> {[S | Acc], C2};
                                false -> {Acc, C2}
                            end
                    end
            end
        end,
        {[], #{}},
        Sites
    ),
    lists:reverse(Kept).

stamp(Sites, Names) ->
    N = length(Names),
    {Out, _} = lists:foldl(
        fun(S, {Acc, I}) ->
            T = lists:nth((I rem N) + 1, Names),
            {[S#{recv_type => T} | Acc], I + 1}
        end,
        {[], 0},
        Sites
    ),
    Out.

hottest_selector() ->
    Candidates = ['printOn:', 'size', 'asString', 'value', 'at:', '='],
    Counts = [{length(beamtalk_xref:senders_of(S)), S} || S <- Candidates],
    {_, Best} = lists:max(Counts),
    Best.

%%--------------------------------------------------------------------

time_avg(Fun, Iters) ->
    T0 = erlang:monotonic_time(microsecond),
    Last = lists:foldl(fun(_, _) -> Fun() end, undefined, lists:seq(1, Iters)),
    T1 = erlang:monotonic_time(microsecond),
    {(T1 - T0) / Iters, Last}.
