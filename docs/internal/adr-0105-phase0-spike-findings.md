# ADR 0105 — Phase 0 Spike Findings (BT-2776)

**Status:** complete · **Branch:** `claude/bt-2776-live-recheck-spike`
**Deliverable:** knowledge. Prove (or refute) the three Phase-0 assumptions of
ADR 0105 (Live Image Re-Checking on Hot Reload) end-to-end for one case, plus
probe the two flagged edges.

**Walking skeleton:** `Counter>>getCount` changes return type `Integer → String`;
caller `Dashboard>>refresh: c :: Counter -> Integer => (c getCount) + 1` becomes
stale (`+ 1` on a `String`).

**Evidence artefacts committed:**
- `runtime/apps/beamtalk_runtime/test/beamtalk_recheck_spike_tests.erl` — runnable
  EUnit proof (assumptions 2 & 3 + supersession). `rebar3 eunit
  --module=beamtalk_recheck_spike_tests` → **2 tests, 0 failures**.

## Headline

| # | Assumption / edge | Verdict | One-line evidence |
|---|---|---|---|
| 1 | Signature capture at patch time | **Partial** | put_method *does* clear (code-confirmed), but the compile **response carries no signature** — the ADR's named capture source doesn't exist yet; it's genuinely new plumbing. |
| 2 | Dependent lookup (selector query + receiver filter) | **Held** | `senders_of/getCount` returns the caller site; sites are selector-keyed with `recv_kind` but **no receiver class**, so the filter must run at re-check. |
| 3 | Batched re-check via compiler port | **Held** | Real port re-check: `String`→diagnostic, `Integer`→clean; warm median **18.5 ms**. |
| E1 | Supersession | **Held (reachable)** | Two back-to-back re-checks return different results from one warm port; replacement is a re-run, not new machinery. |
| E2 | Proxy-routed-call miss | **Broke (miss confirmed, larger than expected)** | A `getCount` sent to a `withTimeout:` result is recorded as **zero sends** by the live-reindex walker — missed *before* the receiver filter even runs. |

**Single most important design implication:** the re-check's diagnostic and the
LSP-publish path each need a port command that does *not* exist in usable form
today. Type mismatches come back as **warnings inside a `{status, ok}` response**
(not errors), `compile_expression` accepts the changed signature
(`class_hierarchy`) but returns **warnings as bare strings with no location**, and
the `diagnostics` command returns **located, severity-tagged** diagnostics but
**ignores `class_hierarchy`** so it can't see the changed callee. Generalisation
must add class-hierarchy awareness to the located-diagnostics path (a small,
identified change — see §3 / §Publish).

---

## Assumption 1 — Signature captured at patch time, retained across generations

**Verdict: PARTIAL.** The *premise* holds exactly as the ADR states; the *named
capture mechanism* (read it from the compile response) does not exist yet, and
the live-edit paths are not uniform.

### 1a. put_method clears type metadata — confirmed, pre-patch signature unrecoverable
`beamtalk_object_class.erl` `{put_method, …}` handler removes the patched
selector's signature and return type on install:
- `beamtalk_object_class.erl:858-859` — `method_signatures = maps:remove(Selector,…)`,
  `method_return_types = maps:remove(Selector,…)`.
- `notify_hot_patch/1` then synthesises the compiler-facing metadata with
  `return_type => none, param_types => none`
  (`beamtalk_object_class.erl:1419-1424`).

So after a `>>` patch the class's own state reports the selector as dynamic.
**Reading the pre-patch signature back from class state after install is
impossible** — it must be captured before. Premise **held**.

### 1b. The compile response does NOT carry the new method's signature — the gap
The ADR says "the compile response must carry [the signature]". It does not
today:
- `method_definition` response fields (Rust `method_definition_ok_response`,
  `crates/beamtalk-compiler-port/src/main.rs:444-470`): `class_name`, `selector`,
  `is_class_method`, `method_source`, `warnings`. **No return/param types.**
- `compile_method` response (`main.rs:519-554`) adds `core_erlang`,
  `module_name`, `classes`, `merged_class_source` — still no signature.
- The type checker *does* compute them (`type_checker/inference.rs`, public
  wrappers `infer_method_return_types` / `infer_types_and_returns` at
  `type_checker/mod.rs:298/315`), but the port path
  (`parse_and_check_expression` → `compute_diagnostics…`) **drops the `TypeMap`
  and returns only `Vec<Diagnostic>`**. Only user-written AST annotations survive
  (`ast/method.rs` `return_type` / parameter `type_annotation`).

**Implication:** capturing the signature "from the compile response" requires new
plumbing — serialize the inferred `return_type` + `param_types` into the
`method_definition` / `compile_method` response (the checker already computes
them; expose them). Reading only the *declared* annotations is a cheaper interim
but misses inferred signatures.

### 1c. The two live-edit paths are not the same — the ADR conflates them
- **REPL `>>` path** → `beamtalk_object_class:put_method/4` gen_server call →
  clears (1a). This is the path the ADR's caveat describes.
- **IDE-save / `compile:source:` / MCP `save_method` path** →
  `beamtalk_repl_loader:install_method` → `compile_method_reload` → **full class
  recompile** → `code:load_binary` + `register_class/0`
  (`beamtalk_repl_loader.erl:751-824, 933-986`). This path does **not** go through
  `object_class:put_method`; it reloads a freshly compiled module whose
  `__beamtalk_meta/0` carries the *new* return types, and the register path
  re-populates `method_return_types`. So on this path the **new** signature *is*
  present in class state post-install — but the **previous generation's is
  overwritten**, so a diff still needs the pre-patch value stored first.

**Design adjustment for BT-2777 (per-selector signature store):**
1. The store must be fed from **both** paths — hook `notify_hot_patch` (the `>>`
   path) *and* the recompile/`register_class` path (IDE save), not just guard
   `put_method`.
2. On the IDE-save path the new signature is already in the freshly-compiled
   `__beamtalk_meta`; on the `>>` path it is only available *before*
   `put_method` clears it. A uniform "record new signature at patch time" hook
   should read the compile output (once 1b exposes it) rather than class state,
   so both paths share one capture point.
3. "Previous generation" = the value in the store from the last patch; seed it
   from `__beamtalk_meta` for never-patched methods (as the ADR says).

---

## Assumption 2 — Dependent lookup: selector query, then receiver filter

**Verdict: HELD**, with the caveat the ADR predicted. Proven by
`senders_of_finds_caller_test_`.

- `beamtalk_xref:senders_of(getCount)` returns the caller site
  (`owner => Dashboard, method => refresh:, recv_kind => other`).
- The site type (`beamtalk_xref.erl:137-147`) carries `owner`, `class_side`,
  `method`, `line`, `recv_kind`, `target_module`, `gen` — **no receiver class**.
- The test registers a *second, unrelated* `getCount` sender (`ListView>>render`)
  and confirms `senders_of` returns **both** — selector-keyed fan-out. xref alone
  cannot tell the real `Counter` dependent from the false one.

**So the "filter candidates by inferred receiver type" is not an xref
operation** — it happens naturally at re-check: the checker types the receiver
(`c :: Counter`) and only then does the `Counter>>getCount` signature apply. A
site whose receiver is a `List` re-checks clean against `List`'s own `getCount`
and drops out. Assumption 2's design (query wide, filter at re-check) is sound.

**Design adjustment for BT-2778:** the receiver filter is a *side effect* of
re-checking, not a pre-filter — you cannot cheaply prune the candidate list
before compiling. This makes the ADR's **per-reload caller cap** load-bearing
(you pay a compile per candidate to discover it's not a real dependent). The
xref-receiver-key follow-up (ADR Alternatives) is the real fix if common-selector
fan-out bites.

---

## Assumption 3 — Batched re-check via the compiler port (+ latency)

**Verdict: HELD**, proven end-to-end through the real `beamtalk-compiler-port` OS
process (`recheck_via_port_test_`).

- Caller unit re-checked with `Counter>>getCount -> String` supplied via the
  `class_hierarchy` option → warning `String does not understand '+'`.
- Same caller with `getCount -> Integer` → **clean** (`warnings = []`).
- **Latency (warm port):** median **18.5 ms** over 5 samples
  (`18.9/17.5/17.5/19.5/18.5 ms`). Bare round-trip (no compile) ≈ **5.9 ms**; a
  cold single-class compile ≈ **58 ms**. Comfortably interactive.

### Three constraints discovered that reshape the re-check unit
1. **The re-check unit is ONE class.** A two-class source is rejected: *"Multiple
   classes in one file is not supported."* The caller's enclosing unit is the
   caller's own class; the **changed callee's signature must be injected via
   `class_hierarchy`**, not compiled inline. `class_hierarchy` shape (Rust
   `parse_class_info_from_meta_term`, `main.rs:183-271`):
   `#{ClassAtom => #{superclass => atom, method_info => #{Sel => #{arity, param_types, return_type}}}}`.
   In production this map is what `beamtalk_compiler_server` accumulates from
   `register_class` casts (`beamtalk_compiler_server.erl:590-593`) and forwards on
   every compile.
2. **`Object subclass:` cannot hold `state:`** (needs `Actor`/`Value`). The spike
   used a **typed parameter** (`refresh: c :: Counter`) rather than a field to
   keep the receiver typed without an actor. A field-typed receiver
   (`self.counter getCount`, the ADR's exact demo) requires the caller be an
   `Actor`/`Value` and the field type be readable — a shape detail Phase 1 must
   respect.
3. **The mismatch is a WARNING, not an error.** The checker emits type mismatches
   as `Diagnostic::warning`/`hint` (`type_checker/inference.rs`), so they ride the
   `warnings` list of a `{status, ok}` response — `compile_expression` returns
   `{ok, Core, Warnings}`, not `{error, Diagnostics}`. **The re-check must inspect
   warnings, not just the error branch.** (Consistent with ADR 0100/severity: a
   `+ 1` mismatch takes the ordinary checker severity.)

**Design adjustment for BT-2778:** batch is one class per port request as the ADR
says, but the *callee signature delivery* is the crux — the changed method's new
signature must be in `class_hierarchy` for the request. If BT-2777's store holds
it, BT-2778 just injects it. Reuse `check_module`/`infer_types` unchanged (whole-
Module entry, no single-method API — `type_checker/mod.rs`), exactly as planned.

---

## Publish — one LSP diagnostic attributed to the caller (location gap)

**Verdict: PARTIAL — diagnostic is produced, but no single existing command gives
located + hierarchy-aware output.**

- `compile_expression` accepts `class_hierarchy` (re-checks correctly) but returns
  warnings as **bare binaries with no line/span** — unusable for LSP attribution
  as-is.
- The `diagnostics` command returns **located, severity-tagged** diagnostics
  (`#{message, severity, start, end}`, `beamtalk_compiler.erl:129-154`) — but
  `handle_diagnostics` (`main.rs:1566`) **never extracts `class_hierarchy`**
  (calls `compute_diagnostics_with_known_vars` without classes), so it cannot see
  the changed callee. Probed directly: it returns `diagnostics => []` for the
  stale case.

**Design adjustment for BT-2779 (publish):** extend the `diagnostics` command to
accept `class_hierarchy` and route through the already-existing
`compute_diagnostics_with_known_vars_and_classes`
(`queries/diagnostic_provider.rs:237`). That one change yields class-hierarchy-
aware, **located**, severity-tagged diagnostics — the exact input the LSP publish
and caller-attribution need. Then map each diagnostic's `start`/`end` (relative to
the re-checked unit) onto the caller's `method_source` offset and tag it
reload-induced. LSP `textDocument/publishDiagnostics` replaces all diagnostics for
a URI per publish, which also gives clearing/supersession for free (see E1).

---

## Edge E1 — Supersession

**Verdict: HELD (reachable).** `run_port_checks` re-checks the same caller against
generation A (`String` → stale) then B (`Integer` → clean) on one warm port and
asserts the results differ (`?assertNotEqual`) with B clean. Because Mechanism
steps 1–3 fire on *every* reload, the later re-check is authoritative and its
result replaces the earlier one — no accumulation, no separate clearing engine
needed for the port half. On the publish half, LSP per-URI replacement finishes
the job (a fresh `publishDiagnostics` for the caller's file supersedes the prior
set). The ADR's "clearing is replacement" is reachable with the existing publish
semantics; BT-2779 only needs to re-publish on every reload, including the empty
set when the caller re-checks clean.

## Edge E2 — Proxy-routed-call miss

**Verdict: BROKE — miss confirmed, and it starts *earlier* than the ADR assumed.**

The ADR expects the site to be *recorded* in xref (under the proxy path) but
*mis-attributed* by the receiver filter. Probing the live-reindex walker
(`find_all_sends_in_source`, which feeds `beamtalk_xref:put_method`):
- `c getCount` → recorded: `#{selector => getCount, recv => other, line => 1}`.
- `(c withTimeout: 5000) getCount` → **`sends => []`** — *neither* `withTimeout:`
  *nor* `getCount` recorded.
- `c getCount; withTimeout: 5000` (cascade, no nesting) → both recorded.

So a selector sent to a **parenthesised message-send receiver** is dropped by this
walker entirely (observed in bare-expression form; the compile-time codegen xref
walker may differ and should be confirmed in method-body context). The miss scope
is therefore **worse than "wrong receiver class"**: for proxy-wrapped usage the
`getCount` site can be **absent from `senders_of` altogether**, so the re-check
never even considers it — a silent false "no stale callers", exactly the
"files nobody has open" hazard the ADR names.

**Design adjustment:** treat E2 as a **two-layer** gap for BT-2778/beyond:
1. **Indexing layer** — confirm whether the compile-time xref walker records
   `(recv-expr) selector` sends (the live-reindex walker demonstrably does not for
   the nested-send receiver). If it doesn't, proxy sites are invisible before any
   type reasoning.
2. **Attribution layer** — even when recorded, a `withTimeout:`-forwarded
   `getCount` has `recv_kind => other` and the receiver's static type is the proxy
   (ADR 0104 territory, itself unbuilt), so attributing it to `Counter` needs
   proxy-as-meta-dependent tracking. Interim: document the gap (as the ADR's
   Consequences already flag). Recommend Phase 2 record it as a known false-
   negative with a test, not silently.

---

## What passed / what I did not build (honesty)

- **Passes:** `just build`; `rebar3 eunit --module=beamtalk_recheck_spike_tests`
  (2 tests, 0 failures); `just fmt` clean on the new file.
- **Not built (out of Phase-0 scope):** the per-selector signature store; the
  actual runtime→compiler re-check trigger on save; real LSP publish wiring; the
  `diagnostics`-command `class_hierarchy` extension (identified, not implemented).
  The spike proves the mechanism and locates every gap; the generalisation phases
  (BT-2777 store, BT-2778 lookup+re-check, BT-2779 publish) build them.
