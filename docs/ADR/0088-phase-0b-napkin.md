# ADR 0088 Phase 0b Napkin: cerl-ETF Wire Mechanism + Timing

**Date:** 2026-05-28
**Issue:** BT-2315 (part of epic BT-2313, Wave 2 — Phase 0a is BT-2314)
**Evaluator:** end-to-end cerl-ETF round-trip on the existing OTP Port, plus
encode/decode/compile timings on two fixtures
**Status:** Recommendation — **Proceed with ETF as the Phase 1 wire**. ETF
encode+decode is a small share (<5%) of per-compile cost on both fixtures; the
ADR's pivot-to-Alternative-7 gate does not fire.

## Executive Summary

Phase 0b is the wire-mechanism napkin. It answers two questions:

1. **Does the cerl-ETF Port contract actually work end-to-end?** —
   Rust constructs a cerl term, encodes to ETF, ships it over the existing
   Port wire as `{cerl, Etf}`, BEAM decodes via `binary_to_term/2` with `[safe]`,
   `compile:forms/2` produces a `.beam`, `code:load_binary/3` installs it,
   and `module_info(module)` returns the expected name.
2. **Is ETF encode/decode a meaningful fraction of per-compile cost?** —
   The ADR's commitment to ETF over Alternative 7 (Port + NIF-for-conversion)
   is contingent on this number being small. If ETF dominates, the right move
   is to pivot to a NIF before Phase 1.

**Answers:**

1. **Yes** — both compile paths (`beamtalk_compiler_server`, no
   `debug_info`; `beamtalk_build_worker`, with `debug_info`) accept the
   `{cerl, Etf}` variant, decode via `binary_to_term/2` with `[safe]`,
   compile through `compile:forms/2`, and produce loadable BEAM modules.
   Byte-equivalence between the Rust ETF encoder and
   `term_to_binary(cerl:c_module(...))` is asserted in both Rust and Erlang
   unit tests for the empty-module and minimum-module shapes.
2. **ETF is a small share.** On a representative `~1500`-node module the
   ETF encode+decode pair is **~3.4% of total per-compile time**. On the
   smallest possible (module_info-only) module the share is **~2.4%**.
   `compile:forms/2` dominates both cases. The
   pivot-to-Alternative-7 gate **does not fire**.

**Recommendation:** **Proceed with Phase 1 (ETF wire) as committed in
ADR 0088.** Phase 0a's qualified shrinkage signal (~10–12%) plus this
phase's "ETF cost is negligible" signal together support Phase 1 work,
*if* the wider epic gates pass. The independent typed-Document-leaves
follow-up (Phase 0c, suggested by the Phase 0a memo) still merits the
same three-function comparison before final commitment — Phase 0b's
timing data does not refute it, only the alternative-NIF pivot.

## What was built

### Wire-mechanism napkin

| Component | Location |
|---|---|
| Minimum cerl Rust mirror | `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs` (~430 LOC + 280 LOC tests) |
| `{cerl, Etf}` Port arm (server, no `debug_info`) | `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl` (`compile_core_erlang/1`) |
| `{cerl, Etf}` Port arm (build worker, with `debug_info`) | `runtime/apps/beamtalk_compiler/src/beamtalk_build_worker.erl` (`compile_core_erlang/1`) |
| End-to-end EUnit suite (both paths) | `runtime/apps/beamtalk_compiler/test/beamtalk_cerl_wire_tests.erl` (11 tests) |
| Byte-equivalence Rust↔Erlang | `cerl.rs::tests::{truly_empty,minimum}_module_matches_otp_term_to_binary` + Erlang `empty_module_etf_layout_test` |
| Reference-bytes regenerator | `scripts/cerl-napkin-dump.escript` |
| Rust-side timing harness | `crates/beamtalk-core/examples/cerl_napkin_timing.rs` |
| Erlang-side timing harness | `scripts/cerl-napkin-timing.escript` |

The cerl Rust mirror covers `c_module`, `c_literal` (atom-only payload),
`c_var` (atom name + `{Name, Arity}` fun-reference name), `c_fun`, and
`c_call` — the minimum needed for an empty *self-identifying* module
(one that exposes `module_info/0,1`, the acceptance criterion's probe).

The acceptance criterion's phrasing — "minimum cerl node set needed for an
empty module: `c_module`, `c_var`, `c_literal`, annotation wrapper" —
nominally excludes `c_fun` and `c_call`. We extend slightly here because
`compile:forms/2` with `from_core` does **not** auto-inject `module_info`
(unlike `from_erl`), so a truly empty cerl module compiles and loads but
fails the AC's `module_info(module) =:= bt_napkin` check. The two extra
node kinds (and a broadened `c_var`) cost ~120 LOC; the wire-format
verification still operates on a tree that is dramatically smaller than
the Phase 1 mirror's expected ~800–1500 LOC. The byte-equivalence test
for the *truly* empty (no-defs) shape is retained so the smallest
possible record layout is still pinned.

### `[safe]` decode invariant

Both `compile_core_erlang/1` clauses call `binary_to_term(Etf, [safe])` —
the atom-table-safety guarantee from ADR 0022. The cerl record tags
(`c_module`, `c_literal`, `c_var`, `c_fun`, `c_call`) are a fixed finite
set already known to the VM (the OTP `cerl` module exports them), and
Beamtalk-generated names (module names, function-ref `{Name, Arity}`
atoms, `'X'` and the like) are pre-allocated by the existing text path
before any Phase 0b traffic could exist in production. The
`{error, {cerl_decode_error, ...}}` failure mode on malformed ETF is
verified by `cerl_variant_rejects_malformed_etf_test` and
`build_worker_cerl_variant_rejects_malformed_etf_test`.

The timing escript (which runs in isolation, without prior Beamtalk
compile traffic) has to pre-touch the atoms via `core_scan:string/1` on
the equivalent text fixture before it can decode the cerl ETF — this is
called out in the script and is not a constraint on production traffic.

## Timing data

Methodology:

* Two fixtures per run: a *minimum* module (only `module_info/0,1`, ~30
  cerl nodes, 425 bytes of ETF) and a *many-function* module
  (250 nullary functions returning fresh atoms, ~1500 cerl nodes,
  19,092 bytes of ETF). The many-function fixture is the
  "hand-constructed cerl tree with ~thousands of nodes" the AC requires.
* `ITERATIONS = 1000` measurements per phase per fixture, with a 50-sample
  warm-up to prime caches and JIT effects.
* `erlang:monotonic_time(nanosecond)` brackets each phase.
* Rust encode times come from
  `target/release/examples/cerl_napkin_timing`; Erlang decode/compile
  times from `scripts/cerl-napkin-timing.escript`.
* All times reported are the **best** observed sample per phase — the
  least-noisy proxy for steady-state throughput. Mean and p95 are
  available in the raw harness output and reproduce the same conclusion.
* Machine: same workstation across runs; release-mode Rust; OTP 27.

### Empty module (425 bytes ETF / 242 bytes text)

| Phase | Best (ns) |
|---|---|
| **Rust ETF encode** | 1,990 |
| **cerl: BEAM decode** (`binary_to_term`) | 4,000 |
| **cerl: `compile:forms/2`** | 243,257 |
| **cerl: total (decode + compile)** | 247,542 |
| **text: `core_scan:string/1`** | 12,951 |
| **text: `core_parse:parse/1`** | 3,283 |
| **text: `compile:forms/2`** | 250,618 |
| **text: total (scan + parse + compile)** | 268,089 |

`.beam` output size is identical between the two paths (448 bytes).

### Many-function module (19,092 bytes ETF / 10,159 bytes text)

| Phase | Best (ns) |
|---|---|
| **Rust ETF encode** | 121,042 |
| **cerl: BEAM decode** (`binary_to_term`) | 165,754 |
| **cerl: `compile:forms/2`** | 8,118,780 |
| **cerl: total (decode + compile)** | 8,294,428 |
| **text: `core_scan:string/1`** | 703,129 |
| **text: `core_parse:parse/1`** | 46,054 |
| **text: `compile:forms/2`** | 8,437,450 |
| **text: total (scan + parse + compile)** | 9,223,468 |

`.beam` output size is identical between the two paths (10,956 bytes).

### Cost-share analysis

For the cerl wire, the question is: what fraction of total per-compile
time is the ETF encode + decode pair? (The `compile:forms/2` cost is
unavoidable on both wires.)

| Fixture | Rust encode | BEAM decode | ETF total | cerl total | ETF share of cerl total |
|---|---|---|---|---|---|
| Empty | 1,990 ns | 4,000 ns | 5,990 ns | 249,532 ns | **2.4%** |
| Many-fn (~1500 nodes) | 121,042 ns | 165,754 ns | 286,796 ns | 8,415,470 ns | **3.4%** |

(Rust encode is included as part of "total" by summing it into the
cerl-side budget. The "cerl total" column above is `Rust encode + BEAM
decode + compile`; the percentages reflect ETF cost vs total pipeline
including the Rust encode.)

For the text wire, the analogous overhead is `core_scan` +
`core_parse`. Same fixtures:

| Fixture | scan + parse | text total | scan+parse share of text total |
|---|---|---|---|
| Empty | 16,234 ns | 266,851 ns | **6.1%** |
| Many-fn | 749,183 ns | 9,186,633 ns | **8.2%** |

So:

* **ETF cost (cerl wire) is consistently smaller than scan+parse cost
  (text wire)**, both in absolute terms and as a share of total.
* The compile-side cost (`compile:forms/2`) dominates **both** wires —
  it accounts for 96–98% of per-compile time on the cerl wire and
  91–94% on the text wire.
* The cerl wire's net win over the text wire is ~7% (empty) and ~8%
  (many-fn), driven primarily by removing `core_scan` (which is
  ~50× more expensive than `binary_to_term` on the many-fn fixture).

## Recommendation against the ADR's pivot gate

ADR 0088 commits to ETF over Alternative 7 (Port + NIF-for-conversion)
**explicitly contingent on this napkin's timing data**:

> If ETF cost dominates for representative-sized modules, the right move
> is to pivot to a NIF-based conversion before Phase 1, not to ship ETF
> anyway.

ETF cost does **not** dominate. On the largest fixture measured here
(~1500 nodes, ~19 KB ETF — already larger than the ADR's "representative
real Beamtalk module"), the ETF encode+decode pair is **3.4%** of total
per-compile time. The compile step (`compile:forms/2`) is the
overwhelming cost driver at 96.4%.

The NIF alternative's claimed win is "heap-resident term construction
with literal sharing" — a measurable win when ETF encode/decode is on
the critical path. With ETF at ~3.4% of total, even a hypothetical NIF
that eliminated the encode/decode entirely would buy back at most ~3.4%
of per-compile latency, at the cost of:

* A `.so` per platform in the distribution surface
* A new failure mode in the build pipeline
* NIF-version vs OTP-version compatibility
* Blurring the Port boundary ADR 0022 established

The trade is not justified by the data. **Alternative 7 should remain
deferred.** Reopening it as a separate ADR is appropriate only if a
later, larger-fixture measurement shows ETF cost dominating
qualitatively — not on the data we have here.

## What this does and does not say

**Says:**

* The cerl-ETF Port contract works end-to-end on both compile paths
  (server without `debug_info`, build worker with `debug_info`).
* Rust ETF encoding matches `term_to_binary(cerl:c_module(...))`
  byte-for-byte at the smallest (no-defs) and minimum (module_info-only)
  module shapes.
* ETF encode+decode is a small share (≤3.4% on the larger fixture) of
  per-compile time at sizes representative of a real Beamtalk module.
* The ADR's "ETF dominance" pivot gate does not fire.

**Does not say:**

* That Phase 1 is unambiguously justified. The Phase 0a audit landed in
  the qualified-middle zone (~10–12% projected shrinkage, between the
  ADR's 5% and 15% gates). The annotation-fidelity benefit (stack
  traces, etc.) remains the strongest non-shrinkage justification. The
  full Phase 1 decision should weigh both Phase 0a and this Phase 0b
  result, plus the typed-Document-leaves comparison Phase 0a flagged as
  a separate follow-up.
* That ETF cost is negligible for **all** workloads. The fixtures used
  here cover empty (~30 nodes) and many-fn (~1500 nodes). A 10K-node
  module — plausible for a large `.bt` file with rich method bodies —
  was not measured. ETF cost grows linearly with node count; compile
  time grows super-linearly with some passes. The cost-share at very
  large sizes could shift, but qualitatively `compile:forms/2` remains
  the dominant cost: a 10× increase in node count would put ETF at
  roughly 10–15% of total, still well below the ADR's "dominates" bar.
* That the Rust encode time is representative of production. The harness
  uses `eetf` 0.12 to do the encoding via a small intermediate `Term`
  tree. A Phase 1 production encoder could plausibly emit ETF bytes
  directly from the cerl Rust AST, skipping the intermediate
  conversion — but the win would be measured in tens of microseconds at
  most on the many-fn fixture and is not in scope for the gate decision.

## Files added/modified

* **New:** `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs`
  (minimum cerl Rust mirror + ETF encoder + 13 unit tests)
* **New:** `crates/beamtalk-core/examples/cerl_napkin_timing.rs`
  (Rust-side timing harness + fixture writer)
* **New:** `scripts/cerl-napkin-dump.escript`
  (reference-bytes regenerator)
* **New:** `scripts/cerl-napkin-timing.escript`
  (Erlang-side timing harness)
* **New:** `runtime/apps/beamtalk_compiler/test/beamtalk_cerl_wire_tests.erl`
  (11 end-to-end + byte-equivalence tests across both compile paths)
* **New:** `docs/ADR/0088-phase-0b-napkin.md` (this memo)
* **Modified:** `crates/beamtalk-core/Cargo.toml` (adds `eetf = "0.12"`
  to `[dependencies]` for the cerl ETF encoder)
* **Modified:** `crates/beamtalk-core/src/codegen/core_erlang/mod.rs`
  (declares `pub mod cerl;`)
* **Modified:** `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl`
  (`compile_core_erlang/1` gains the `{cerl, Etf}` clause)
* **Modified:** `runtime/apps/beamtalk_compiler/src/beamtalk_build_worker.erl`
  (`compile_core_erlang/1` gains the `{cerl, Etf}` clause)

## No regressions

* The new `{cerl, Etf}` clause is additive: the existing text-wire
  binary clause is unchanged. The `cerl_variant_text_path_still_works_test`
  asserts the text path continues to function.
* The cerl wire variant is a side-channel — it is **not** reachable from
  production compile paths today. No production caller constructs
  `{cerl, Etf}` messages; only the EUnit tests do. The acceptance
  criterion ("wire variant is a side-channel for testing only, not
  reachable from production paths") is satisfied.
* The Rust cerl mirror is gated behind a public `pub mod cerl;` so
  Phase 1 codegen can build on it, but no codegen function returns a
  `cerl::*` value yet.
* `just ci` passes locally with the new tests included.

## References

* ADR (proposed, Phase 0a + 0b authorised): `docs/ADR/0088-direct-cerl-emission.md`
  (see §"Phase 0b: Napkin")
* Phase 0a audit (Wave 1, merged): `docs/ADR/0088-phase-0a-audit.md`
* Epic: BT-2313 (ADR 0088 Phase 0 decision gates)
* Wire architecture: ADR 0022 (`docs/ADR/0022-embedded-compiler-via-otp-port.md`)
* Alternative 7 (the pivot target — *not triggered* by this data):
  ADR 0088 §"Alternative 7. Hybrid Port + NIF-for-cerl-Conversion"
* OTP `cerl` module: <https://www.erlang.org/doc/man/cerl.html>
* OTP `compile:forms/2`: <https://www.erlang.org/doc/man/compile.html#forms-2>
