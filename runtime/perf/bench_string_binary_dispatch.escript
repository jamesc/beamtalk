#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Selector-aware UTF-8 scan skip (BT-3033): module_for_value/1 (BT-2999) pays
%% an O(byte_size) is_utf8/1 validating scan on every dynamic send to a bare
%% binary, to decide whether it dispatches as String or Binary. That scan is
%% wasted for selectors where String and Binary behave identically regardless
%% of which one the receiver "really" is (byteAt:, byteSize, part:size:,
%% concat:, toBytes, asStringUnchecked, asBase64, asBase64Url, asHex) — see
%% is_string_binary_shared_selector/1. This benchmark measures the O(1)
%% byte-level selector `byteAt:` sent dynamically, in a loop, to the *same*
%% >=1 MB untyped binary receiver (the acceptance-criteria scenario: an
%% N-iteration loop over one large binary is O(N x size) before the fix).
%%
%% BT-3049 follow-up: the fast path now routes straight to
%% 'bt@stdlib@binary' instead of 'bt@stdlib@string', skipping the redundant
%% hop where bt@stdlib@string's dispatch/3 re-checks the extension registry
%% before delegating to bt@stdlib@binary anyway. bench_hop/0 isolates that
%% one hop's cost directly (outside the noise of the full send/3 pipeline,
%% where it's a small fraction of total overhead).
-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    timer:sleep(300),
    lists:foreach(fun(B) -> code:ensure_loaded(list_to_atom(filename:basename(B, ".beam"))) end,
                  filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")),
    timer:sleep(800),
    beamtalk_extensions:init(),
    Sizes = [1024, 65536, 1048576],
    lists:foreach(fun bench/1, Sizes),
    bench_hop(),
    ok.

bench(Size) ->
    K = 2000,
    %% Valid UTF-8 (ASCII) so is_utf8/1 pays its full scan cost (it does not
    %% bail early the way invalid-UTF-8 input does) and the receiver
    %% classifies as String — the case the BT-2999 doc calls out as the
    %% expensive one for hot dynamic-dispatch loops.
    Bin = binary:copy(<<"A">>, Size),
    Idx = [rand:uniform(Size) - 1 || _ <- lists:seq(1, K)],
    io:format("~n=== byteAt: on ~p-byte binary, ~p dynamic sends ===~n", [Size, K]),

    %% "before" (simulated BT-2999 path): every dynamic send re-validates
    %% UTF-8 before picking the dispatch module — module_for_value/1's own
    %% logic, replicated here since that unconditional scan is exactly what
    %% BT-3033 removes for this selector; module_for_value/1 itself is
    %% unchanged and still used by callers that don't have a selector.
    BeforeF = fun(I) ->
        Mod =
            case beamtalk_primitive:is_utf8(Bin) of
                true -> 'bt@stdlib@string';
                false -> 'bt@stdlib@binary'
            end,
        Mod:dispatch('byteAt:', [I], Bin)
    end,

    %% "after" (real, current code path): beamtalk_primitive:send/3, which
    %% now routes byteAt: through module_for_value/2 and skips the scan.
    AfterF = fun(I) -> beamtalk_primitive:send(Bin, 'byteAt:', [I]) end,

    %% Sanity: both paths must agree with the actual byte, or the "after"
    %% path would be a correctness regression, not just a speedup.
    Sample = hd(Idx),
    Expected = binary:at(Bin, Sample),
    Expected = BeforeF(Sample),
    Expected = AfterF(Sample),

    run("before (BT-2999: always scans)   ", BeforeF, Idx, K),
    run("after  (BT-3033: skips for byteAt:)", AfterF, Idx, K),

    %% Contrast: `size` still differs between String/Binary, so it must keep
    %% paying the scan on both sides — no regression for selectors that
    %% genuinely need the real answer.
    SizeF = fun(_I) -> beamtalk_primitive:send(Bin, size, []) end,
    run("after  (size: still scans, unchanged)", SizeF, Idx, K).

run(Label, F, Idx, K) ->
    F(hd(Idx)),
    {Us, _} = timer:tc(fun() -> lists:foreach(F, Idx) end),
    io:format("  ~s ~10.3f us/op~n", [Label, Us / K]).

%% BT-3049: isolates the cost of the redundant module hop the fast path used
%% to pay before routing directly to bt@stdlib@binary — calls each compiled
%% module's dispatch/3 directly (no beamtalk_primitive:send/3 wrapper), so
%% the ~40-200 ns of module_for_value/2's own overhead (selector match,
%% extensions:has check) doesn't dilute the signal.
bench_hop() ->
    K = 200000,
    Bin = binary:copy(<<"A">>, 1048576),
    ViaString = fun() -> 'bt@stdlib@string':dispatch('byteAt:', [0], Bin) end,
    ViaBinary = fun() -> 'bt@stdlib@binary':dispatch('byteAt:', [0], Bin) end,
    Expected = binary:at(Bin, 0),
    Expected = ViaString(),
    Expected = ViaBinary(),
    io:format("~n=== BT-3049: isolated hop cost (byteAt: on 1 MB binary, ~p calls) ===~n", [K]),
    run0("via bt@stdlib@string (old: extra hop + re-check)", ViaString, K),
    run0("via bt@stdlib@binary (new: direct)              ", ViaBinary, K).

run0(Label, F, K) ->
    F(),
    {Us, _} = timer:tc(fun() -> lists:foreach(fun(_) -> F() end, lists:seq(1, K)) end),
    io:format("  ~s ~10.4f us/op~n", [Label, Us / K]).
