#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Benchmark: reload-triggered re-check fan-out (ADR 0105 Phase 2, BT-2781).
%%
%% Measures the candidate-set size and wall-clock cost of
%% `beamtalk_recheck:trigger/4` (BT-2778) for common vs rare selectors, to
%% decide whether ADR 0087's xref schema needs a receiver-type key (ADR 0105
%% Alternatives) or whether the existing selector-keyed lookup + at-recheck
%% receiver filter + numeric caller cap is sufficient. Follows the
%% before/after methodology of `bench_senders_xref.escript` (ADR 0087 Phase
%% 3, BT-2299).
%%
%% Two parts:
%%
%%   Part A ("large image") — survey the REAL fan-out of every sent selector
%%   in the full loaded stdlib workspace (100+ classes, 1000+ methods, same
%%   workspace `bench_senders_xref.escript` loads): which selectors are
%%   common (many senders) vs rare (one sender), and how many *distinct
%%   caller classes* (the unit `beamtalk_recheck` caps and re-checks) each
%%   implies today.
%%
%%   Part B ("controlled fan-out") — drive the REAL `beamtalk_recheck:trigger/4`
%%   orchestration (real compiler port, real xref, real workspace_meta) over
%%   synthetic candidate sets of increasing size (5/20/50/200), with a fixed
%%   10% real-dependent / 90% false-positive split (modelling a heavily
%%   overloaded common selector, e.g. `size`/`at:`). Measures wall-clock at
%%   the default caller cap (20) and uncapped, so the cost of the current
%%   "pay for a compile to find out a candidate isn't a real dependent"
%%   design (moduledoc, beamtalk_recheck) is directly visible. Every
%%   candidate site here is a **legacy row** — `sends` carries no `recv_type`
%%   (mirrors pre-ADR-0115 code, or a class that has not recompiled since):
%%   `senders_of/2`'s Amendment 2 default treats an absent `recv_type` as
%%   always-relevant, so Part B's numbers are deliberately unchanged by
%%   ADR 0115 — the receiver-type filter has nothing to narrow here. This is
%%   the "coverage ceiling" ADR 0115 §Consequences names directly: untyped/
%%   legacy code keeps paying the pre-Phase-4 cost, and the cap remains the
%%   only guard for it.
%%
%%   Part C ("typed fan-out", BT-3220 / ADR 0115 Phase 5) — the identical
%%   10%/90% synthetic shape and driver as Part B, but every candidate site
%%   now carries a `recv_type` (mirroring what the real compile-time write
%%   path, BT-3217, populates for actual compiled code): real dependents are
%%   typed at the changed class, false positives at a hierarchy-unrelated
%%   class that happens to implement the same selector name. This is BT-2781's
%%   synthetic scenario, now run against `senders_of/2`'s receiver-type
%%   filter (wired into `beamtalk_recheck` by BT-3219/Phase 4) instead of
%%   `senders_of/1` — the false-positive share is expected to be excluded
%%   from `total_candidates` *before* `apply_cap/2` ever runs, not merely
%%   re-checked and discarded afterward, fixing the 90%-loss failure mode
%%   Part B still exhibits for legacy rows.
%%
%% Run from the `runtime/` directory after `just build`:
%%
%%   escript perf/bench_recheck_fanout.escript

-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = application:ensure_all_started(beamtalk_compiler),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    timer:sleep(500),
    %% Force-load the compiled stdlib modules so their on_load hooks register
    %% the full loaded stdlib workspace (mirrors bench_senders_xref.escript).
    lists:foreach(
        fun(Beam) ->
            Mod = list_to_atom(filename:basename(Beam, ".beam")),
            code:ensure_loaded(Mod)
        end,
        filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")
    ),
    timer:sleep(1500),

    part_a_large_image_survey(),
    part_b_controlled_fanout(),
    part_c_typed_fanout(),
    ok.

%%====================================================================
%% Part A: real stdlib fan-out survey (large-image scenario)
%%====================================================================

part_a_large_image_survey() ->
    io:format("~n=== Part A: large-image fan-out survey (loaded stdlib) ===~n", []),
    Classes = beamtalk_class_registry:live_class_entries(),
    io:format("Workspace: ~p loaded classes~n", [length(Classes)]),

    #{indexed := SendRows} = beamtalk_xref:all_sends_bt(),
    io:format("Indexed send sites: ~p~n", [length(SendRows)]),

    %% Group every indexed send by its sent selector into its DISTINCT owner
    %% set (`usort` here, not just accumulated with duplicates) — this is the
    %% figure that matters: `beamtalk_recheck` caps and re-checks one
    %% candidate per caller *class*, regardless of how many sites in that
    %% class send the selector (`group_by_owner/1`'s dedup). Ranking by raw
    %% site count instead would put a selector with a few classes sending it
    %% many times each ahead of one sent once each by many classes — exactly
    %% backwards for "how many re-check candidates does this selector cost."
    BySelector = lists:foldl(
        fun(#{sent := Sel, owner := Owner}, Acc) ->
            maps:update_with(Sel, fun(Owners) -> [Owner | Owners] end, [Owner], Acc)
        end,
        #{},
        SendRows
    ),
    DistinctOwnerCounts = maps:map(fun(_Sel, Owners) -> length(lists:usort(Owners)) end, BySelector),
    Ranked = lists:sort(
        fun({_, CA}, {_, CB}) -> CA >= CB end,
        maps:to_list(DistinctOwnerCounts)
    ),

    io:format("~nTop 10 most-common selectors (by distinct caller-class count):~n", []),
    io:format("~-24s ~-12s ~-18s~n", ["selector", "sites", "distinct owners"]),
    lists:foreach(
        fun({Sel, OwnerCount}) ->
            Sites = length(beamtalk_xref:senders_of(Sel)),
            io:format("~-24s ~-12w ~-18w~n", [atom_to_list(Sel), Sites, OwnerCount])
        end,
        lists:sublist(Ranked, 10)
    ),

    DefaultCap = 20,
    OverCap = [X || {_Sel, OwnerCount} = X <- Ranked, OwnerCount > DefaultCap],
    io:format(
        "~nSelectors whose distinct-owner count exceeds the default cap (~w): ~w of ~w total~n",
        [DefaultCap, length(OverCap), length(Ranked)]
    ),
    lists:foreach(
        fun({Sel, OwnerCount}) ->
            io:format("  ~w: ~w distinct owners (~w over cap)~n", [
                Sel, OwnerCount, OwnerCount - DefaultCap
            ])
        end,
        OverCap
    ),

    RareOnes = [X || {_Sel, OwnerCount} = X <- Ranked, OwnerCount =:= 1],
    io:format("~nSelectors with exactly 1 distinct caller class (rare): ~w of ~w total~n", [
        length(RareOnes), length(Ranked)
    ]),
    case RareOnes of
        [{RareSel, _} | _] ->
            io:format("  example rare selector: ~w (1 site)~n", [RareSel]);
        [] ->
            ok
    end,
    ok.

%%====================================================================
%% Part B: controlled fan-out via the real beamtalk_recheck orchestration
%%====================================================================

part_b_controlled_fanout() ->
    io:format("~n=== Part B: controlled fan-out (real beamtalk_recheck:trigger/4) ===~n", []),
    restart_workspace_meta(),

    DefaultCap = 20,
    Fanouts = [5, 20, 50, 200],

    io:format(
        "~nDefault cap (~w) -- this is what a real reload pays today:~n", [DefaultCap]
    ),
    io:format("~-8s ~-10s ~-10s ~-12s ~-12s ~-14s ~-12s~n", [
        "fanout", "checked", "dropped", "findings", "wall_ms", "ms/checked", "cap_note"
    ]),
    lists:foreach(
        fun(N) -> run_round(N, DefaultCap) end,
        Fanouts
    ),

    io:format(
        "~nUncapped -- full cost of checking every candidate (what a receiver-type-keyed~n", []
    ),
    io:format(
        "xref lookup would let the cap skip entirely, since only ~~10% are real dependents):~n", []
    ),
    io:format("~-8s ~-10s ~-10s ~-12s ~-12s ~-14s ~-12s~n", [
        "fanout", "checked", "dropped", "findings", "wall_ms", "ms/checked", "cap_note"
    ]),
    lists:foreach(
        fun(N) -> run_round(N, N) end,
        Fanouts
    ),
    ok.

%%====================================================================
%% Part C: typed fan-out via senders_of/2 (ADR 0115 Phase 5, BT-3220)
%%====================================================================

part_c_typed_fanout() ->
    io:format(
        "~n=== Part C: typed fan-out (senders_of/2 pre-cap filtering, ADR 0115, BT-3220) ===~n",
        []
    ),
    restart_workspace_meta(),

    DefaultCap = 20,
    %% 400 (not just 5/20/50/200, matching Part B) is included specifically
    %% to push the *filtered* real-dependent count (40, at the fixed 10%
    %% split) past the cap — proving the cap remains a real backstop for
    %% typed candidates too, just over the now-narrowed pool `senders_of/2`
    %% hands it (40 candidates, not 400).
    Fanouts = [5, 20, 50, 200, 400],

    io:format(
        "~nDefault cap (~w) -- same 10%%-real/90%%-false-positive shape as Part B, but every~n",
        [DefaultCap]
    ),
    io:format(
        "candidate site now carries recv_type (mirrors real compiled code, BT-3217):~n", []
    ),
    io:format("~-8s ~-10s ~-10s ~-12s ~-12s ~-14s ~-12s~n", [
        "fanout", "checked", "dropped", "findings", "wall_ms", "ms/checked", "cap_note"
    ]),
    lists:foreach(
        fun(N) -> run_round_typed(N, DefaultCap) end,
        Fanouts
    ),
    ok.

%% Typed counterpart of `run_round/2`: identical 10%-real/90%-false-positive
%% shape and driver (`beamtalk_recheck:trigger/4`), but every registered
%% candidate site's `sends` entry carries `recv_type` — real dependents typed
%% at `ChangedClass`, false positives typed at a hierarchy-unrelated
%% `UnrelatedClass`, both registered in `beamtalk_class_metadata` (not just
%% `beamtalk_xref`) so `senders_of/2`'s relatedness test actually runs the
%% hierarchy-membership check rather than falling through to Amendment 2's
%% "unresolvable name, default relevant" branch — which would silently
%% defeat this benchmark's whole point, exactly as
%% `beamtalk_recheck_tests:receiver_type_filter_narrows_pool_before_cap_test_`
%% (BT-3219 AC4) already guards against at the EUnit level.
-spec run_round_typed(pos_integer(), pos_integer()) -> ok.
run_round_typed(N, Cap) ->
    PrevCap = application:get_env(beamtalk_workspace, recheck_caller_cap, 20),
    application:set_env(beamtalk_workspace, recheck_caller_cap, Cap),
    Tag = integer_to_list(N) ++ "_typed_cap" ++ integer_to_list(Cap),
    Selector = list_to_atom("benchTypedFanoutSel" ++ Tag),
    SelectorBin = atom_to_binary(Selector, utf8),
    ChangedClass = list_to_atom("BenchTypedChanged" ++ Tag),
    ChangedClassBin = atom_to_binary(ChangedClass, utf8),
    UnrelatedClass = list_to_atom("BenchTypedUnrelated" ++ Tag),
    try
        %% Both classes must be *known* to beamtalk_class_metadata for
        %% senders_of/2's hierarchy-relatedness test to run at all (see the
        %% moduledoc note above) — a small standalone hierarchy, not related
        %% to each other beyond both subclassing (the implicit) Object.
        ok = beamtalk_class_metadata:insert(ChangedClass, undefined, undefined, none, undefined),
        ok = beamtalk_class_metadata:insert(UnrelatedClass, undefined, undefined, none, undefined),

        beamtalk_compiler_server:register_class(ChangedClass, #{
            superclass => 'Object',
            method_info => #{Selector => #{arity => 0, param_types => [], return_type => 'String'}}
        }),
        beamtalk_compiler_server:register_class(UnrelatedClass, #{
            superclass => 'Object',
            method_info => #{Selector => #{arity => 0, param_types => [], return_type => 'Integer'}}
        }),

        PadWidth = length(integer_to_list(N)),
        lists:foreach(
            fun(I) ->
                Receiver =
                    case I rem 10 =:= 1 of
                        true -> ChangedClass;
                        false -> UnrelatedClass
                    end,
                register_typed_candidate(
                    "BenchTypedCand" ++ Tag ++ "_" ++ pad_index(I, PadWidth),
                    Selector,
                    Receiver
                )
            end,
            lists:seq(1, N)
        ),

        {WallUs, Result} = timer:tc(fun() ->
            beamtalk_recheck:trigger(ChangedClassBin, SelectorBin, instance, signature_change)
        end),
        #{
            findings := Findings,
            checked := Checked,
            not_checked := NotChecked,
            cap_note := CapNote
        } = Result,
        WallMs = WallUs / 1000,
        MsPerChecked =
            case Checked of
                0 -> 0.0;
                _ -> WallMs / Checked
            end,
        io:format("~8p ~10p ~10p ~12p ~12.2f ~14.2f ~12s~n", [
            N,
            Checked,
            NotChecked,
            length(Findings),
            WallMs,
            MsPerChecked,
            format_cap_note(CapNote)
        ])
    after
        application:set_env(beamtalk_workspace, recheck_caller_cap, PrevCap),
        beamtalk_class_metadata:delete(ChangedClass),
        beamtalk_class_metadata:delete(UnrelatedClass)
    end.

%% Typed counterpart of `register_candidate/3`: the site's `sends` entry
%% carries `recv_type => ReceiverClass` (BT-3217's write-path shape for a
%% typed local receiver), instead of omitting the field entirely.
-spec register_typed_candidate(string(), atom(), atom()) -> ok.
register_typed_candidate(NameStr, Selector, ReceiverClass) ->
    Name = list_to_atom(NameStr),
    NameBin = list_to_binary(NameStr),
    ReceiverBin = atom_to_binary(ReceiverClass, utf8),
    SelectorBin = atom_to_binary(Selector, utf8),
    XrefEntry = [
        #{
            class_side => false,
            selector => 'go:',
            line => 2,
            sends => [
                #{
                    selector => Selector,
                    line => 2,
                    recv_kind => other,
                    recv_type => ReceiverClass
                }
            ],
            references => [#{class => ReceiverClass, line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ],
    Source = <<
        "Object subclass: ",
        NameBin/binary,
        "\n  go: c :: ",
        ReceiverBin/binary,
        " -> Integer => (c ",
        SelectorBin/binary,
        ") + 1\n"
    >>,
    ok = beamtalk_xref:register_class(Name, XrefEntry),
    ok = beamtalk_workspace_meta:set_class_source(NameBin, Source),
    ok.

restart_workspace_meta() ->
    %% beamtalk_workspace_meta is supervised (permanent, beamtalk_workspace_sup).
    %% Stopping it races the supervisor's automatic restart, so if it's already
    %% running just reuse it -- per-round selector names are already unique,
    %% so there's no cross-round contamination risk from skipping a fresh start.
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            {ok, _} = beamtalk_workspace_meta:start_link(#{
                workspace_id => <<"bench_recheck_fanout_ws">>,
                project_path => undefined,
                created_at => erlang:system_time(second),
                repl => false
            }),
            ok;
        _Pid ->
            ok
    end.

%% One round: register N synthetic candidate classes for a fresh,
%% round-scoped selector (so rounds never interfere with each other or with
%% the real stdlib senders from Part A), run the real recheck trigger under
%% `Cap`, and print the result row.
-spec run_round(pos_integer(), pos_integer()) -> ok.
run_round(N, Cap) ->
    PrevCap = application:get_env(beamtalk_workspace, recheck_caller_cap, 20),
    application:set_env(beamtalk_workspace, recheck_caller_cap, Cap),
    try
        Tag = integer_to_list(N) ++ "_cap" ++ integer_to_list(Cap),
        Selector = list_to_atom("benchFanoutSel" ++ Tag),
        SelectorBin = atom_to_binary(Selector, utf8),
        ChangedClass = list_to_atom("BenchChanged" ++ Tag),
        ChangedClassBin = atom_to_binary(ChangedClass, utf8),
        UnrelatedClass = list_to_atom("BenchUnrelated" ++ Tag),

        %% ~10% real dependents (typed at the changed class), the rest false
        %% positives (typed at an unrelated class that happens to implement
        %% the same selector name unchanged) — models a heavily-overloaded
        %% common selector where only a small fraction of same-selector sites
        %% are real dependents of the reloaded class.

        %% ChangedClass>>Selector: was Integer, now String (the reload).
        beamtalk_compiler_server:register_class(ChangedClass, #{
            superclass => 'Object',
            method_info => #{Selector => #{arity => 0, param_types => [], return_type => 'String'}}
        }),
        %% UnrelatedClass>>Selector: unchanged Integer — re-checks clean.
        beamtalk_compiler_server:register_class(UnrelatedClass, #{
            superclass => 'Object',
            method_info => #{Selector => #{arity => 0, param_types => [], return_type => 'Integer'}}
        }),

        %% Candidates are named `BenchCand<Tag>_<PaddedIndex>` uniformly (not
        %% `BenchReal.../BenchFalse...`) and every 10th one is wired to the
        %% real dependent (ChangedClass), the rest to the false-positive
        %% (UnrelatedClass) — interleaved so alphabetic sort order (what
        %% `apply_cap/2` keeps under a cap, per its documented "always drops
        %% the same alphabetically-last owners" limitation) does not
        %% systematically favour or penalise real dependents. A naming
        %% scheme where all real names sorted after all false names would
        %% silently make every capped round's `findings` artificially low —
        %% not a property of the cap, just of the fixture.
        PadWidth = length(integer_to_list(N)),
        lists:foreach(
            fun(I) ->
                Receiver =
                    case I rem 10 =:= 1 of
                        true -> ChangedClass;
                        false -> UnrelatedClass
                    end,
                register_candidate(
                    "BenchCand" ++ Tag ++ "_" ++ pad_index(I, PadWidth),
                    Selector,
                    Receiver
                )
            end,
            lists:seq(1, N)
        ),

        {WallUs, Result} = timer:tc(fun() ->
            beamtalk_recheck:trigger(ChangedClassBin, SelectorBin, instance, signature_change)
        end),
        #{
            findings := Findings,
            checked := Checked,
            not_checked := NotChecked,
            cap_note := CapNote
        } = Result,
        WallMs = WallUs / 1000,
        MsPerChecked =
            case Checked of
                0 -> 0.0;
                _ -> WallMs / Checked
            end,
        io:format("~8p ~10p ~10p ~12p ~12.2f ~14.2f ~12s~n", [
            N,
            Checked,
            NotChecked,
            length(Findings),
            WallMs,
            MsPerChecked,
            format_cap_note(CapNote)
        ])
    after
        application:set_env(beamtalk_workspace, recheck_caller_cap, PrevCap)
    end.

-spec format_cap_note(binary() | undefined) -> string().
format_cap_note(undefined) -> "-";
format_cap_note(Bin) when is_binary(Bin) -> binary_to_list(Bin).

%% Zero-pad `I` to `Width` digits so lexicographic (atom-name) sort order
%% matches numeric order — see run_round/2's doc for why this matters.
-spec pad_index(pos_integer(), pos_integer()) -> string().
pad_index(I, Width) ->
    Digits = integer_to_list(I),
    Padding = lists:duplicate(max(0, Width - length(Digits)), $0),
    Padding ++ Digits.

-spec register_candidate(string(), atom(), atom()) -> ok.
register_candidate(NameStr, Selector, ReceiverClass) ->
    Name = list_to_atom(NameStr),
    NameBin = list_to_binary(NameStr),
    ReceiverBin = atom_to_binary(ReceiverClass, utf8),
    SelectorBin = atom_to_binary(Selector, utf8),
    XrefEntry = [
        #{
            class_side => false,
            selector => 'go:',
            line => 2,
            sends => [#{selector => Selector, line => 2, recv_kind => other}],
            references => [#{class => ReceiverClass, line => 2}],
            source_status => indexed,
            provenance => class_body
        }
    ],
    Source = <<
        "Object subclass: ",
        NameBin/binary,
        "\n  go: c :: ",
        ReceiverBin/binary,
        " -> Integer => (c ",
        SelectorBin/binary,
        ") + 1\n"
    >>,
    ok = beamtalk_xref:register_class(Name, XrefEntry),
    ok = beamtalk_workspace_meta:set_class_source(NameBin, Source),
    ok.
