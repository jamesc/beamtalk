# Downstream Adoption of the Pattern and Type Features (2026-11)

**Issue:** BT-5227 (final issue of epic BT-5221)
**Date:** 2026-11-27
**Status:** Complete. The report records verdicts; nothing is removed here. Each
removal candidate has a `needs-spec` issue for the owner.

## TL;DR

Epic BT-5221 asked the downstream packages to adopt the pattern and type
features that had no real user, only where the new form reads better. This
report turns the per-repo notes into one verdict per feature.

- **Established:** destructuring assignment, tuple patterns, `Result`
  constructor patterns, binary construction and patterns, map-shape types.
- **Niche but justified:** array patterns, map patterns, `matchExhaustive:`,
  `shapeVersion:` / `migrateFromVN:`, trait `excluding:`.
- **Candidates for removal (no real use after a deliberate search):** bounded
  type parameters, protocol `extending:`, difference and intersection types,
  trait `overriding:`. Each has a `needs-spec` issue. These are questions for
  the owner, not recommendations to delete. Bounded type parameters (enforcement
  is a day old) and `overriding:` (half of ADR 0127's conflict rule) carry the
  strongest caveats; see [Removal candidates](#removal-candidates).
- **Not assessable:** list cons patterns. The pattern form is still rejected
  (BT-5301, open).
- The type patterns (`x :: T`), literal patterns and `nil` arms that the epic
  did not list turned out to be the most adopted form: they replace
  `isKindOf:` and string-compare ladders in six packages (buccaneer, catalina,
  liquid, linear, agents, watcher).
- The adoption surfaced more than 20 compiler, type-checker and formatter
  issues. All but five are fixed; the open ones are listed under
  [Open items](#open-items).

## Method and how to read the numbers

Sources, in order of weight:

1. The per-repo checklists on the epic's children (BT-5219, 5222, 5223, 5224,
   5225, 5226, 5330, 5340, 5228), each written by the session that did the work.
2. The compiler issues the adoption filed (table in
   [Gaps the adoption filed](#gaps-the-adoption-filed)) and their PRs.
3. A grep sweep run for this report on 2026-11-27 over `src/` and `test/` of the
   15 public satellite repos (depth-1 clones of `main`) and over `stdlib/` and
   `examples/` here. It is the "deliberate search" the removal rule asks for.

Two cautions:

- **Adoption counts come from the checklists. Grep counts are a present-state
  snapshot.** The snapshot includes code that predates the epic (buccaneer has
  106 tuple-destructuring lines, almost all older than BT-5222, which added 9).
  Do not read a grep count as "sites adopted".
- **Zero in the grep is not proof of a missed site.** It is proof that the
  feature appears nowhere in the 15 repos today. Where a feature is zero, the
  checklists give the reason it was rejected where one exists.

Unless a line says otherwise, "no site" means a session looked and recorded it.

The sweep is reproducible. Clones were `git clone --depth 1` of each
`jamesc/beamtalk-<name>` on 2026-11-27 at: agents `eb2dcbc`, announcements
`077df9d`, buccaneer `dd1825c`, catalina `c994d00`, collections `22702af`,
exdura `a7fade2`, hpack `6e2e0d9`, http `1520c25`, linear `d9311d4`, liquid
`82ac634`, mint `80e811f`, oauth `b4c92a3`, symphony `4274c66`, watcher
`b8eff1e`, yaml `48aac7d`. The searches were `grep -rE --include=*.bt` over
`src/` and `test/` for: `matchExhaustive:`; `shapeVersion:|migrateFromV`;
`extending:`; `uses: .*(excluding:|overriding:)`; `subclass: \w+\([A-Z]\w* ::`
(bounded parameters, also checked by listing every `subclass: Name(T` header);
`<<` (binary); `type \w+ = #\{` (shapes); `:: [^=]*[A-Za-z)] (&|\\) [#A-Z(]`
(intersection and difference); `#\([a-z_]+ \|` (cons); and the destructuring
forms `^\s*(\{...\}|#\[...\]|#\{...\}) :=`.

## Verdicts

| Feature | Verdict | Where it is used | Considered and rejected |
|---|---|---|---|
| Destructuring assignment | **Established** | stdlib `system_navigation.bt` (four `at:` chains), `examples/getting-started/src/shipping.bt`; buccaneer (9 sites); http, mint (`MintHTTP2` frame handling), hpack (Huffman trie, static index); catalina (`Listener`, `Handler onReady:`); oauth (`{oldest} :=`, two map destructures); collections (2) | yaml (tests only), announcements (no `at: 1`), exdura/symphony/linear/agents/watcher/liquid (no site recorded) |
| Tuple patterns in `match:` | **Established** | stdlib `tuple.bt` (`unwrap`, `unwrapOr:`, `unwrapOrElse:`); oauth `LoopbackReceiver await:from:timeout:`, `ProviderWorker errorFor:`; mint `MintHTTP2` | A type pattern could not nest in a tuple pattern until BT-5294 (fixed) |
| Constructor patterns (`Result ok:` / `error:`) | **Established** | catalina (`Socket` recv/send/sendfile, `Acceptor acceptOnce`, `Handler`, `ServerSupervisor info`); buccaneer (3); exdura `ExduraClient` teardown; symphony (`AgentRunner begin`, `WorkflowWatcher getModTime:`); `shipping.bt` | `isError ifTrue: [^x]` guards in oauth, catalina and symphony: matching would nest the happy path. Only `Result` has constructor patterns (Phase 1); see [recommendation 2](#2-open-design-question-user-level-sealed-families) |
| Array patterns | **Niche but justified** | collections `SortedMap` (`#[k, v] := pair`, 2 sites) | No `match:` arm site anywhere: buccaneer's inventory of 182 matches has 0 array arms; stdlib/examples found none where it reads better |
| Map patterns | **Niche but justified** | oauth `OAuthServer fromShim:preset:` and `OAuthFlows tokenSetFrom:obtainedAt:` (map destructuring of the shim's always-complete maps) | linear `fromNode:`, agents lookups, oauth claims and metadata: every listed key must be present and these APIs have optional keys. Filed as BT-5344 (`needs-spec`, open) |
| `matchExhaustive:` | **Niche but justified; underused** | exdura `WorkflowStatus` closed alias (4 wait loops); buccaneer (`Exchange>>commit`, `sendHeaders:`); `shipping.bt` over `type ShippingSpeed` | See below. Most scrutinees are open sets, so a wildcard is right |
| Bounded type parameters | **Candidate for removal** | `examples/getting-started/src/report.bt` only, an example written for the epic | collections (ordering is injected through `sortedBy:`, so a `Comparable` bound rejects valid uses); buccaneer (no generic class); stdlib (`DynamicSupervisor(C)` needs a class bound). 0 of 15 satellites declare a bound |
| Protocol `extending:` | **Candidate for removal** | None. Only `tests/repl-protocol/cases/protocol_define.btscript` | stdlib: `JsonMappable extending: JsonRepresentable` was tried and dropped because it makes conformance stricter (BT-5219); collections: no protocol-typed parameter |
| Binary construction and patterns | **Established** | mint (frame headers, integer encoders, sized payload split, padding with computed sizes); hpack (computed-size patterns, bit-level dispatch, nibble splitting); http (`%XX` decoding); buccaneer (24 binary arms in its pre-adoption inventory) | http/1 parsing (CRLF search and byte loops); hpack encoders; mint native port (none exists) |
| Map-shape types | **Established** (3 packages) | buccaneer and http (both predate the epic and motivated ADR 0147); oauth `TokenResponse` on `tokenSetFrom:` (kept) | catalina decodes no dictionaries |
| Difference and intersection types | **Candidate for removal** | None outside `stdlib/test/set_theoretic_operators_e2e_test.bt` | stdlib: ADR 0102 review note (BT-5219); 15 satellites: 0 occurrences |
| `shapeVersion:` / `migrateFromVN:` | **Niche but justified** | exdura `ExduraHttpServer` (`shapeVersion: 2`, `migrateFromV1:`, migration tests) | symphony: state is ephemeral by design (BT-5287). exdura's persisted Values are not reached (BT-5282) |
| List cons patterns | **Not assessable** | None. `#(h \| t)` is rejected in `match:` and in destructuring (BT-5301, open) | collections wants it for `PriorityQueue>>pairUp:acc:`, `Deque` and `ListZipper`; mint, catalina, oauth, liquid, yaml and announcements list it as "not implemented" |
| Trait `excluding:` | **Niche but justified** | stdlib `supervision_tree.bt`, `ets.bt` (predate the epic) | 0 downstream uses |
| Trait `overriding:` | **Candidate for removal** | None | 0 uses in stdlib, examples and 15 satellites. It is the acknowledgement half of ADR 0127's conflict rule, so removal means redesigning that rule |

The rest of this section gives the evidence behind each verdict that is not
obvious from the table.

### `matchExhaustive:`

It has two packages and one example, which is why it is "niche" and not
"established". The reason is not that it is useless. The scrutinee has to be a
closed set (a union of symbol singletons, `nil`, or concrete leaf classes), and
the adopters recorded these refusals:

- Open sets by design: exdura event types, symphony tracker states and exit
  reasons, catalina option and error sets, agents message and method kinds
  (unknown kinds must pass through), announcements event classes.
- Virtual dispatch: liquid dispatches on AST nodes with `render:` / `eval:`,
  not a ladder.
- Rejected buccaneer candidates: `WriteState`, `CompressionMethod`, `Dynamic`
  tuple scrutinees, `HttpVersion`, `Object`-typed frames.

Closed sets already exist in the language as `type` aliases over symbols or
leaf classes (language-features, "Type aliases" and "Asserted exhaustiveness").
The one real adoption that used one, exdura's `WorkflowStatus`, got a
compile-time guarantee that a new state breaks every wait loop. Adopters rarely
reached for the idiom, so BT-5228 now teaches it in the `beamtalk new`
`AGENTS.md` template, including a closed-set alias example.

One compiler gap showed up and is fixed: `Result` scrutinees were rejected
(BT-5278). A bare `Symbol` scrutinee is still rejected, by design: dispatch like
hpack's needs a closed union (see the language-features text on non-closed
scrutinees).

### Bounded type parameters

The only use is the example `Report(T :: Printable)`. The sessions did not find
a package that orders by `<` directly, which is the shape of class a bound
fits. This is the weakest verdict in the report. The feature had no enforcement
at annotation sites until this burn (BT-5333, BT-5338, BT-5343, BT-5345), so
the absence of use is partly an absence of feedback. The `needs-spec` issue
asks the owner to decide after one release, not now.

### Binary patterns

Computed-size patterns, bit fields and nibble splitting all worked (hpack,
mint). Two frictions were recorded:

- A failed match gives no position or reason, so every parser needs an explicit
  error arm (hpack; also BT-5279). The adoption added malformed and truncated
  input tests in mint (41), hpack and http for this reason.
- A segment size cannot contain a message send (BT-5285, Done; the language
  reference now says to bind `p size` to a local first).

Performance: mint measured 48.3 ms before and 45.3 ms after; hpack 55.5 ms
before and 48.7 ms after, reported as noise. **A "3x faster" figure for http
that circulated during the burn does not appear in the BT-5223 checklist or the
squash commit; it is unverified and this report does not rely on it.**

### Map-shape types

oauth tried `TokenResponse` on the shim's 7-key token map and kept it. A literal
with a missing or mistyped key at a call site is rejected. The limit is that
the real response comes from FFI as `Dynamic`, so the check covers literals, not
the shim's output.

### `shapeVersion:` / `migrateFromVN:`

One real site (exdura's derived `listening` field, which a structural reconcile
cannot express). Two ADR 0123 gaps are open and unlabelled in Linear: BT-5282
(persisted `Value` state written with `Binary serialize:` is never migrated)
and BT-5287 (the intended audience should be stated more narrowly: actors whose
new slots need values derived from old slots). Together they mean ADR 0123 has
one downstream user.

## Per-package results

| Package | PR | Adopted | No site (reason given in the checklist) |
|---|---|---|---|
| stdlib and examples | [beamtalk#5860](https://github.com/jamesc/beamtalk/pull/5860) (BT-5219) | Destructuring, tuple and constructor patterns, `matchExhaustive:`, one bounded parameter | Array, map, `nil` patterns; protocol `extending:`; stdlib bounded parameter; `A \ B`, `A & B` |
| buccaneer | [#70](https://github.com/jamesc/beamtalk-buccaneer/pull/70) | 3 `Result` constructor sites, 2 typed-pattern matches, 9 destructures, 2 `matchExhaustive:` | Array and map patterns, bounded parameters, list cons, `matchExhaustive:` over `Result` (then blocked by BT-5278) |
| http | [#50](https://github.com/jamesc/beamtalk-http/pull/50) | Binary pattern for `%XX` decoding, tuple destructuring | Not recorded in the BT-5223 http comment |
| mint | [#19](https://github.com/jamesc/beamtalk-mint/pull/19) | Binary construction and sized patterns for HTTP/2 frames, tuple patterns in `MintHTTP2` | `matchExhaustive:` (open sets), `Result` patterns, HTTP/1 parsing, list cons |
| hpack | [#14](https://github.com/jamesc/beamtalk-hpack/pull/14) | Huffman trie and static index as tuple destructuring | `matchExhaustive:`, `Result` patterns, binary construction in the encoders. The package already used binary patterns |
| exdura | [#59](https://github.com/jamesc/beamtalk-exdura/pull/59) | `WorkflowStatus` closed union with `matchExhaustive:` in 4 loops, `Result` pattern in `ExduraClient`, `shapeVersion: 2` | Destructuring, bounds, `extending:`, binary; `matchExhaustive:` on event types (open set) |
| symphony | [#31](https://github.com/jamesc/beamtalk-symphony/pull/31) | `Result` patterns in `AgentRunner begin` and `WorkflowWatcher getModTime:` | `matchExhaustive:` (open sets), early-return guards, `Config resolve` and `doReload` (tried as `match:`, reverted: arms are single expressions, so multi-statement arms need helper methods), `shapeVersion:` |
| catalina | [#52](https://github.com/jamesc/beamtalk-catalina/pull/52) | `Result` patterns replacing `isOk` ladders, `SocketAddress` pair destructuring, 2 type-pattern matches | `matchExhaustive:`, map patterns, multi-statement arms, a nested type test (BT-5294, since fixed) |
| oauth | [#42](https://github.com/jamesc/beamtalk-oauth/pull/42) | 2 tuple-pattern matches, 1-tuple destructuring, 2 map destructures, `TokenResponse` shape | `isKindOf:` ladders in early-return guards, `matchExhaustive:`, list cons |
| collections | [#19](https://github.com/jamesc/beamtalk-collections/pull/19) | 2 array destructures in `SortedMap` | Bounds and `extending:` (written reason: ordering is injected), list cons (BT-5301) |
| liquid | [#3](https://github.com/jamesc/beamtalk-liquid/pull/3) | Type and literal patterns replacing `isKindOf:` ladders in 8 methods | `matchExhaustive:` (AST dispatch is virtual), single-guard `isKindOf:` sites |
| linear | [#9](https://github.com/jamesc/beamtalk-linear/pull/9) | `body :: Dictionary` type pattern and a `nil` arm in `parseResponse:` | Map patterns on `fromNode:` (optional keys) |
| agents | [#7](https://github.com/jamesc/beamtalk-agents/pull/7) | Type patterns and literal string patterns in 3 methods | `matchExhaustive:` (open-ended kinds), map patterns (`at:ifAbsent:` defaults) |
| watcher | [#30](https://github.com/jamesc/beamtalk-watcher/pull/30) | Type patterns and `nil` arms in 3 files | The actor-alive map (`fmt` made the match form worse), `matchExhaustive:` |
| announcements | none | n/a | Its two `isKindOf:` checks test a runtime `aClass :: Class` argument, which a type pattern cannot express |
| yaml | none | n/a | Every method is a native delegate; there is no Beamtalk-side logic to match |

## Gaps the adoption filed

Every one of these was found by adopting the feature in real code. All are
Done unless the status column says otherwise.

| Issue | Gap | PR / status |
|---|---|---|
| BT-5277 | `Result ok: {a, b}`: nested tuple, map pattern after a constructor keyword was a parse error | [#5863](https://github.com/jamesc/beamtalk/pull/5863) |
| BT-5278 | `matchExhaustive:` rejected a `Result(T, E)` scrutinee | [#5863](https://github.com/jamesc/beamtalk/pull/5863) |
| BT-5281 | Codegen error for a `match:` arm that assigns a local in `do:` in an Actor | Done |
| BT-5283 | `^#sym` in a `-> Tuple` method accepted, but the same value as a match arm rejected: every `^` is now checked against the declared return type | [#5862](https://github.com/jamesc/beamtalk/pull/5862) |
| BT-5284 | Tuple destructuring could not carry `::` annotations | Done |
| BT-5285 | Binary segment size with a send passed analysis and failed codegen with an unlocated error | Done |
| BT-5289, BT-5291 | ThreadedIr verifier failures for a `match:` whose scrutinee advances State (`NonLinearVersion`, `UnbracketedNlrClosure`) | Done |
| BT-5290 | The `^` return check rejected `x isError ifTrue: [^x]` (13 methods in mint's `mint_http1.bt`) | [#5869](https://github.com/jamesc/beamtalk/pull/5869) |
| BT-5292 | Early-`^` diagnostics anchored on the method, and a union-tail member was reported twice | [#5872](https://github.com/jamesc/beamtalk/pull/5872) |
| BT-5294 | Type patterns could not nest in tuple or constructor patterns, and `_ :: T` did not parse | [#5880](https://github.com/jamesc/beamtalk/pull/5880) |
| BT-5298 | A `nil`-initialised local reassigned in a loop was typed `Nil` at `^Result error: x` | [#5881](https://github.com/jamesc/beamtalk/pull/5881) |
| BT-5299 | `beamtalk fmt` joined a long map destructuring pattern onto one line | [#5878](https://github.com/jamesc/beamtalk/pull/5878) |
| BT-5328 | A nested type pattern failed at codegen | Done |
| BT-5332, BT-5336 | Nested `x :: T` bindings bound `Dynamic`, then lost the scrutinee's position type | [#5885](https://github.com/jamesc/beamtalk/pull/5885), [#5896](https://github.com/jamesc/beamtalk/pull/5896) |
| BT-5333, BT-5338, BT-5343, BT-5345 | Type-parameter bounds and protocol annotations were not enforced (see [recommendation 5](#5-the-type-checker-strictness-work-and-its-lesson)) | [#5887](https://github.com/jamesc/beamtalk/pull/5887), [#5890](https://github.com/jamesc/beamtalk/pull/5890), [#5897](https://github.com/jamesc/beamtalk/pull/5897), [#5899](https://github.com/jamesc/beamtalk/pull/5899) |
| BT-5342 | `beamtalk check` ignored a package's own native `-spec`s on a clean tree | [#5889](https://github.com/jamesc/beamtalk/pull/5889) |
| BT-5296 | `Result tryDo:` error type is `Exception`, not `Error` | Canceled: not a bug. catalina maps it with `asError:` ([catalina#53](https://github.com/jamesc/beamtalk-catalina/pull/53)) |
| BT-5301 | List cons pattern `#(h \| t)` rejected in `match:` and destructuring | **Open** (Backlog) |
| BT-5344 | Map patterns need optional keys | **Open**, `needs-spec` |
| BT-5279 | A `rest/binary` tail of a `String` scrutinee is a `Binary`, so recursing needs `asStringUnchecked` | **Open** (Backlog) |
| BT-5282, BT-5287 | ADR 0123 feedback: persisted Values are not migrated; ephemeral services have no site | **Open**, no labels |

Pattern: once a pattern compiled, the adoption found no bug in what it
matched. The bugs were in what the pattern forms accepted and where they met
other machinery: pattern syntax and codegen limits (BT-5277, 5284, 5285, 5294,
5328), state-threading codegen (BT-5281, 5289, 5291), the type checker (the
BT-5283 and BT-5332 families) and the formatter (BT-5299).

## Recommendations

These are the owner-discussion conclusions, checked against the sources.

### 1. `matchExhaustive:` is underused, not useless

Keep it. The data above supports the owner's reading: the two real adopters
both reached it through a closed `type` alias or a small symbol union, and
every refusal was an open set. The gap is discoverability, which BT-5228
addresses (template text plus a closed-set alias example; the refresh is on `main` in
14 of the 15 satellites).

### 2. Open design question: user-level sealed families

Phase 1 of constructor patterns is `Result` only. The adoption records several
sites that want a user-declared closed family, not a `Result`:

- exdura and symphony treat event types, tracker states and exit reasons as
  open sets, so they cannot use `matchExhaustive:` today.
- liquid dispatches on AST node classes virtually. A sealed family with
  constructor patterns could replace that.
- catalina has "a nested type test the language cannot express".
- buccaneer's inventory has 0 constructor arms other than `Result`.

Recommend an ADR for an abstract sealed base with package-local subclasses
(the Kotlin and Scala shape), plus constructor patterns beyond `Result`. This
report does not decide it. The ADR should say how it interacts with the
existing "small closed union of concrete leaf classes" support in
`matchExhaustive:`.

### 3. Map patterns need optional keys

BT-5344 (`needs-spec`, open) holds the evidence: linear `fromNode:`, agents
`at:ifAbsent:` defaults and oauth token responses, claims and provider
metadata. Only oauth adopted map destructuring (2 sites), because only
always-complete maps fit. Shape types already have `?#k => T`; patterns should
agree with them.

### 4. List cons patterns: not landed

BT-5301 is still in Backlog as of 2026-11-27 and `main` still rejects
`#(h | t)` (`Cons syntax ... is not supported in destructuring patterns`,
`parser/expressions.rs`). The language reference already says there is no cons
pattern yet. collections is the only package with a concrete site
(`PriorityQueue>>pairUp:acc:`, `Deque`, `ListZipper`). Until it lands, the
feature cannot be given a verdict.

### 5. The type-checker strictness work and its lesson

The adoption triggered three pieces of checker tightening. Each caught real
bugs and each produced false positives that had to be fixed.

**The `^` return check (BT-5283).** Every `^` is checked against the declared
return type. It caught genuine bugs, fixed in BT-5330:

| Package | Bug |
|---|---|
| linear | `^#()` inside a `map:` block returned a bare `List` from a method declared to return a `Result` whenever the response had no `issues` key. A real logic bug |
| exdura | `version:defaultVersion:maxVersion:` returned a `JsonValue` where `Integer` was declared. A corrupt marker now raises instead of leaking |
| symphony | `TemplateError`, `AgentError`, `WorkspaceError` and `LinearError` are `Value` subclasses, not `Error`, so four `Result(_, Error)` declarations were wrong |
| http | `readChunk` declared `Error` for what are Symbols (`#not_supported`, `#timeout`, `#closed`) |

False positives, all fixed in the checker, not downstream: `x isError ifTrue:
[^x]` (BT-5290; 13 methods in mint's `mint_http1.bt`, about 30 `^` sites that would have needed rewriting), a `nil` local reassigned
in a loop (BT-5298), and double-reported or mis-anchored diagnostics (BT-5292).
BT-5296 was a false alarm: `tryDo:` catches every `Exception`.

**Bound enforcement (BT-5333, 5338, 5343, 5345).** Bounds were silently not
checked at local annotations, and stdlib protocols were unknown to the checker
in user packages, so an unknown protocol counted as conforming. After the fix,
14 of 15 satellites had zero new diagnostics in the first sweep (none declares a
bound). symphony got three: two genuine (`Error` and `WorkspaceError` passed to
`detail :: Printable`; neither has `asString`) and one `Dynamic` false positive.
The owner decided: widen `detail` to `Object` in symphony
([#34](https://github.com/jamesc/beamtalk-symphony/pull/34)), do not give
`Object` an `asString`, and stop warning when a `Dynamic` argument meets a
protocol parameter (gradual typing; the "Dynamic in typed class" lint covers it,
confirmed by BT-5345). `Printable` requires `asString` and `printString`. It
does not mean "displayable".

**Lesson: sweep the satellites with `build`, `check` and `lint`.** What the
sources show:

- Every sweep recorded in BT-5333, 5338, 5343 and 5345 ran
  `beamtalk check --warnings-as-errors` with the old edge and the branch
  compiler (the BT-5338 and BT-5345 sweeps built first). None records a `lint`
  run.
- `check` alone is not a stable gate: BT-5340 and BT-5342 showed its result
  depends on whether `_build` exists, and announcements' CI had never run
  `check`, so main was green while `check` failed.
- The owner conclusion is that a sweep must also run `just lint`. **I found no
  Linear comment or PR that records a lint failure missed by a sweep**,
  including in symphony, so this report states the rule without a cited
  incident. If there was one, add the link here.

## Removal candidates

Per the rubric, a feature with no real use after a deliberate search is a
candidate. Nothing is removed. Each has a `needs-spec` issue.

| Feature | Issue | Caveat the owner should weigh |
|---|---|---|
| Bounded type parameters | BT-5347 | Enforcement landed in the last 24 hours, so there has been no time for use. The only fit found is a class that calls `<` on its type argument |
| Protocol `extending:` | BT-5348 | Zero uses anywhere, including stdlib. ADR 0068 specified it |
| Difference and intersection types | BT-5349 | The ADR 0102 review note of 2026-11-26 already asked to reconsider. Used only by their own tests. Not a syntax-only removal: the checker's `Negation` type also drives `Symbol` narrowing after a failed comparison |
| Trait `overriding:` | BT-5350 | The acknowledgement half of ADR 0127's conflict rule. Zero uses because no conflict has arisen, not because it is wrong |

## Open items

- **BT-5301** list cons patterns and **BT-5344** optional map-pattern keys:
  open children of the epic.
- **BT-5279**, **BT-5282**, **BT-5287**: open, not children of the epic, no
  `needs-spec` or area labels.
- **hpack `AGENTS.md` conflict.** The template region (BT-5228) says to build a
  `Value` with the keyword constructor by default and use `new:` for many
  slots. hpack's `## Local Notes` still says to prefer the `new:` map form for
  multi-field construction. Both are in the same file. The owner should decide
  which wins and delete the other.
- **yaml has no template region.** BT-5228's downstream refresh did not reach
  `beamtalk-yaml` (its `AGENTS.md` has no `beamtalk:template` markers on
  `main`). Fourteen of the 15 satellites have them.
- **Formatter and toolchain skew.** BT-5228's oauth note says `fmt-check` with
  the local edge build and CI's nightly formatter disagree on two long
  destructuring patterns, so those files were left unchanged. BT-5299 fixed the
  wrapping; the disagreement itself is not filed.
- **Unverified notes from the burn**, not relied on above: the "3x faster"
  http parse, and the claim that a multi-statement arm in an actor becomes a
  public selector with no private option (agents' checklist records helper
  methods, not visibility).

## Related

- ADR notes added by this issue: 0102, 0106, 0123, 0143, 0147.
- `docs/beamtalk-language-features.md`, "Patterns in Real Code".
