#!/usr/bin/env escript
%%% Copyright 2026 James Casey
%%% SPDX-License-Identifier: Apache-2.0
%%%
%%% ADR 0088 Phase 0b — Erlang-side timing harness (BT-2315).
%%%
%%% Pair to `crates/beamtalk-core/examples/cerl_napkin_timing.rs`.
%%% The Rust example generates fixtures (empty / many-function modules) and
%%% writes their ETF + text encodings to a directory; this script reads
%%% those fixtures and times the BEAM-side decode + compile work for both
%%% wire formats.
%%%
%%% Output: a CSV-shaped timing table (best/mean/p95 nanoseconds per phase)
%%% suitable for pasting into `docs/ADR/0088-phase-0b-napkin.md`.
%%%
%%% Usage:
%%%   escript scripts/cerl-napkin-timing.escript [fixture_dir]
%%%
%%% Default fixture dir is `target/cerl-napkin-timing`.

-mode(compile).

-define(ITERATIONS, 1000).
-define(WARMUP, 50).

main(Args) ->
    FixtureDir =
        case Args of
            [Dir] -> Dir;
            _ -> "target/cerl-napkin-timing"
        end,
    io:format("ADR 0088 Phase 0b — Erlang-side timing harness~n"),
    io:format("===============================================~n"),
    io:format("Iterations per measurement: ~p~n", [?ITERATIONS]),
    io:format("Fixture dir: ~s~n~n", [FixtureDir]),
    measure(FixtureDir, "empty"),
    measure(FixtureDir, "many"),
    io:format("~n"),
    io:format("Done. See docs/ADR/0088-phase-0b-napkin.md for analysis.~n").

measure(FixtureDir, Tag) ->
    EtfPath = filename:join(FixtureDir, Tag ++ ".etf"),
    TextPath = filename:join(FixtureDir, Tag ++ ".core"),
    case {file:read_file(EtfPath), file:read_file(TextPath)} of
        {{ok, EtfBin}, {ok, TextBin}} ->
            %% Pre-intern atoms the fixture references so `binary_to_term/2`
            %% with `[safe]' accepts them. In production these atoms are
            %% pre-allocated by the existing text path; for an isolated
            %% escript we have to create them up-front via `core_scan'.
            {ok, _, _} = core_scan:string(binary_to_list(TextBin)),
            io:format("Fixture: ~s~n", [Tag]),
            io:format("  ETF size : ~p bytes~n", [byte_size(EtfBin)]),
            io:format("  Text size: ~p bytes~n", [byte_size(TextBin)]),

            %% ── cerl wire path ─────────────────────────────────────
            CerlSamples = run_samples(?ITERATIONS, fun() -> sample_cerl(EtfBin) end),
            summarise("cerl decode (binary_to_term)",
                      [DT || {DT, _, _, _} <- CerlSamples]),
            summarise("cerl compile (compile:forms)",
                      [CT || {_, CT, _, _} <- CerlSamples]),
            summarise("cerl total (decode + compile)",
                      [TT || {_, _, TT, _} <- CerlSamples]),
            BinSize = element(4, hd(CerlSamples)),
            io:format("  cerl beam size: ~p bytes~n", [BinSize]),

            %% ── text wire path ────────────────────────────────────
            TextStr = binary_to_list(TextBin),
            TextSamples = run_samples(?ITERATIONS,
                                      fun() -> sample_text(TextStr) end),
            summarise("text scan  (core_scan:string)",
                      [ST || {ST, _, _, _, _} <- TextSamples]),
            summarise("text parse (core_parse:parse)",
                      [PT || {_, PT, _, _, _} <- TextSamples]),
            summarise("text compile (compile:forms)",
                      [CT || {_, _, CT, _, _} <- TextSamples]),
            summarise("text total (scan + parse + compile)",
                      [TT || {_, _, _, TT, _} <- TextSamples]),
            TextBinSize = element(5, hd(TextSamples)),
            io:format("  text beam size: ~p bytes~n", [TextBinSize]),
            io:format("~n");
        {{error, _}, _} ->
            io:format("ERROR: missing ~s (run `cargo run --release --example cerl_napkin_timing`)~n",
                      [EtfPath]);
        {_, {error, _}} ->
            io:format("ERROR: missing ~s (run `cargo run --release --example cerl_napkin_timing`)~n",
                      [TextPath])
    end.

run_samples(N, Fun) ->
    %% Warm-up.
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, ?WARMUP)),
    [Fun() || _ <- lists:seq(1, N)].

sample_cerl(EtfBin) ->
    T0 = erlang:monotonic_time(nanosecond),
    Term = binary_to_term(EtfBin, [safe]),
    T1 = erlang:monotonic_time(nanosecond),
    {ok, _Mod, Beam} = compile:forms(Term, [from_core, binary, return_errors]),
    T2 = erlang:monotonic_time(nanosecond),
    DecodeNs = T1 - T0,
    CompileNs = T2 - T1,
    TotalNs = T2 - T0,
    {DecodeNs, CompileNs, TotalNs, byte_size(Beam)}.

sample_text(Text) ->
    T0 = erlang:monotonic_time(nanosecond),
    {ok, Tokens, _} = core_scan:string(Text),
    T1 = erlang:monotonic_time(nanosecond),
    {ok, CoreMod} = core_parse:parse(Tokens),
    T2 = erlang:monotonic_time(nanosecond),
    {ok, _Mod, Beam} = compile:forms(CoreMod, [from_core, binary, return_errors]),
    T3 = erlang:monotonic_time(nanosecond),
    ScanNs = T1 - T0,
    ParseNs = T2 - T1,
    CompileNs = T3 - T2,
    TotalNs = T3 - T0,
    {ScanNs, ParseNs, CompileNs, TotalNs, byte_size(Beam)}.

summarise(Label, Samples0) ->
    Samples = lists:sort(Samples0),
    Len = length(Samples),
    Best = hd(Samples),
    Mean = lists:sum(Samples) div Len,
    P95Idx = max(1, min(Len, trunc(Len * 0.95))),
    P95 = lists:nth(P95Idx, Samples),
    io:format("  ~-38s best ~10w ns | mean ~10w ns | p95 ~10w ns~n",
              [Label, Best, Mean, P95]).
