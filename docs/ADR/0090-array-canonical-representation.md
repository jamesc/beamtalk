# ADR 0090: Canonical Array Representation for O(log n) `at:put:`

## Status
Proposed (2026-06-02)

## Context

### Problem statement

Beamtalk's `Array` is a value type backed by BEAM's `array` module, wrapped in a
tagged map:

```erlang
#{'$beamtalk_class' => 'Array', 'data' => ErlangArray}
```

`array:get/2` and `array:set/3` are O(log n), giving Array its reason to exist
over a plain list. But `array:set/3` is **not canonical**: it leaves a stale
copy-on-write cache node inside the `array` tuple, so the result is not `=:=` to
an `array:from_list/1`-built array with identical elements:

```
from_list([1,99,3])         -> {array,3,0,false,undefined,{1,99,3,...},0,{1,99,3,...},0}
set(1,99,from_list([1,2,3])) -> {array,3,0,false,undefined,{1,99,3,...},0,{1,2,3,...},0}
                                                                          ^^ stale cache
```

Both render as `#[1, 99, 3]`, but they compare unequal and hash differently.

### Current state

BT-2362 (PR #2407) fixed the resulting bug — `#[1,2,3] at: 2 put: 99` comparing
unequal to the literal `#[1, 99, 3]` — by re-canonicalising every `at:put:`
result through `from_list(array:to_list(...))`:

```erlang
%% beamtalk_array:at_put/3 (current)
Updated = array:set(Index - 1, Value, Arr),
from_list(array:to_list(Updated)).
```

This is correct but **O(n)** per update (vs `array:set/3`'s O(log n)), making
repeated `at:put:` in a loop **O(n²)**. Measured cost: 5000 `at:put:` calls on a
200k-element array take **96.8 s**, versus **583 µs** for the raw `array:set`
path — roughly a **166,000× slowdown**. The current `at_put/3` doc comment
already flags this as a deliberate, temporary trade-off pending this ADR.

Every other Array operation (`collect`, `select`, `slice_from`, `withAll:`,
literals) already builds via `array:from_list`/`array:map`, which produce the
canonical representation. `at:put:` was the *only* `array:set` caller and thus
the only outlier — which is why this is purely an internal-representation
question, not a question of Array's public protocol.

### Constraints on the solution space

These constraints were discovered while scoping BT-2377 and shape the entire
decision:

1. **Equality is inlined, not dispatched.** `=` and `=:=` both compile to inline
   `erlang:'=:='(L, R)` at every call site
   (`crates/beamtalk-core/src/codegen/core_erlang/operators.rs:61`), per **ADR
   0002** (strict structural equality, no coercion). There is *no* runtime
   equality dispatch for value types — Beamtalk never calls an `Array>>=`
   method. Any approach that wants "two structurally-equal arrays compare equal"
   must either (a) make the underlying terms genuinely `=:=`, or (b) amend ADR
   0002 to introduce runtime equality dispatch for value types.

2. **Dictionary keys use BEAM `=:=`.** `Dictionary` is backed by native Erlang
   maps (`beamtalk_map.erl`, `maps:find/2`). Map key identity is BEAM `=:=`,
   hard-wired in the VM. There is no interception hook for custom key identity.

3. **Set membership uses term order.** `Set` is backed by `ordsets`
   (`beamtalk_set.erl`), whose membership and dedup use Erlang term comparison
   (`==`/`<`). Again, no custom-identity hook.

4. **Array operations are fully encapsulated.** All Array message sends route
   through `beamtalk_array:*` helper functions
   (`crates/beamtalk-core/src/codegen/core_erlang/primitives/array.rs`), and
   array literals route through `beamtalk_array:from_list/1`
   (`expressions.rs:411`). No codegen site inspects the internal `'data'` shape;
   class dispatch keys only on the `'$beamtalk_class' => 'Array'` tag. **The
   internal representation can therefore change without touching codegen or the
   public protocol.**

Constraints 1–3 together mean a "structural `=`/`hash` for Array only" approach
**cannot** make two cache-different-but-element-equal arrays usable as equivalent
dict/set keys without *also* amending ADR 0002 and adding key-identity hooks to
maps and ordsets — which the BEAM does not offer. This is the crux: correctness
for arrays-as-keys is essentially free if the underlying term is canonical, and
expensive-to-impossible otherwise.

## Decision

**Replace Array's internal representation with a structure whose updates are
naturally `=:=`-canonical** — i.e. any array holding a given element sequence is
`=:=` to any other array holding the same sequence, regardless of edit history —
while preserving O(log n) random access and update.

The leading candidate is a **canonical persistent vector** (a wide,
fixed-branching nested-tuple trie, in the Clojure/RRB family), built so that the
in-memory term is a pure deterministic function of `(length, elements)`. Because
Erlang `=:=` is purely structural over the term and the trie carries **no
mutable cache slot** (the exact thing that makes BEAM `array` non-canonical),
two such vectors with identical contents are `=:=` and `erlang:phash2`-equal by
construction.

The public protocol, the tagged-map wrapper, and the `'$beamtalk_class' =>
'Array'` tag are unchanged. Only `beamtalk_array.erl`'s internals and its
`'data'` payload change.

### What the user sees

Nothing changes in behaviour — only performance and correctness-at-scale:

```beamtalk
a := #[1, 2, 3]
b := a at: 2 put: 99      // O(log n), not O(n)
b                          // => #[1, 99, 3]
b =:= #[1, 99, 3]          // => true   (canonical: same contents ⇒ =:=)

// Arrays as dictionary keys — works because terms are =:=-canonical
d := Dictionary new
d at: (#[1, 2] at: 2 put: 9) put: "hit"
d at: #[1, 9]              // => "hit"  (cache history irrelevant)

// Repeated updates in a loop — O(n log n) total, not O(n²)
big := Array withAll: (1 to: 200000)
result := (1 to: 5000) inject: big into: [:acc :i | acc at: i put: 0]
result size                // => 200000   (sub-second, not ~97s)
```

### Error behaviour is unchanged

Out-of-bounds and type errors keep raising the same `#beamtalk_error{}` values
with the same selectors and hints — those paths live in `at_put/3` above the
representation layer:

```beamtalk
#[1, 2, 3] at: 9 put: 0    // index_out_of_bounds, selector at:put:
#[1, 2, 3] at: "x" put: 0  // type_error, selector at:put:
```

### Acceptance criteria for the chosen representation

Whatever concrete structure is selected during implementation must satisfy:

1. **Canonical:** for all element sequences `Es`, every construction path
   (`from_list`, repeated `at_put`, `collect`, `select`, `slice_from`) yielding
   `Es` produces terms that are mutually `=:=` and `erlang:phash2`-equal.
2. **O(log n)** `at`/`at_put` (the whole point — must beat the list fallback and
   the current O(n) canonicalisation).
3. **Equality/hash cost** acceptable for dict/set keys: `=:=` short-circuits on
   first difference; `phash2` is O(n) but no worse than today.
4. **Memory** within a small constant factor of BEAM `array` for typical sizes.
5. **No mutable/COW cache** in the term (the BT-2362 footgun must not recur).

## Prior Art

| Platform | Array/vector representation | Canonical under update? | Notes |
|---|---|---|---|
| **BEAM `array`** | Tuple tree with a COW cache slot | **No** | The cache slot is exactly the source of BT-2362. O(log n) ops, but non-canonical. |
| **Clojure** | Persistent vector (32-way trie) | Yes (via `.equals`) | The canonical persistent-vector *shape*; O(log₃₂ n) ≈ effectively-constant updates. **Equality is by `.equals`/`hashCode` override, not JVM reference identity** — see caveat below. |
| **Scala `Vector` / RRB** | Relaxed Radix Balanced trees | Yes (via `equals`) | Adds O(log n) concat/slice. Same as Clojure: value-equality via an `equals` method, not structural object identity. |
| **Erlang `gb_trees`/maps** | Balanced trees / HAMT | Yes (maps are canonical by key) | A map keyed by index is canonical and O(log n), at higher memory and slower iteration. |
| **Erlang lists** | Cons cells | Yes | Canonical and simple but O(n) random access — the status quo we improved away from in BT-822. |
| **Pharo/Squeak `Array`** | Mutable fixed-size object | N/A | Smalltalk arrays are mutable in place; identity ≠ equality. Beamtalk arrays are immutable value objects, so this model doesn't transfer. |

What we adopt: the **persistent-vector / nested-tuple-trie** *shape* (O(log n),
no cache slot). What we reject: BEAM `array`'s cache optimisation (non-canonical)
and Smalltalk's mutable arrays (wrong semantics for a value type).

**Caveat — we adopt the shape, not the equality mechanism.** Clojure and Scala
get vector equality from an `equals`/`hashCode` *override*, because the JVM
compares objects by reference identity otherwise — that is the JVM analog of this
ADR's *rejected* Option 2 (dispatched equality). Beamtalk instead relies on
Erlang's `=:=` being purely structural over the term, so it gets equality "for
free" **only if the term itself is canonical** — i.e. there is exactly one term
shape per `(length, elements)`, independent of construction history. That
canonicality is an *additional discipline* the implementation must enforce (no
Clojure-style transient/tail variation that yields two different node layouts for
the same logical vector); it is not automatic just because the structure is a
trie. Acceptance criterion 1 below makes this requirement explicit, and it is the
load-bearing invariant of the whole decision.

## User Impact

- **Newcomer:** No visible change except that arrays "just work" as dictionary
  keys and large-array loops don't mysteriously hang. The O(n²) cliff they could
  previously fall off (5000 updates → 97 s) disappears.
- **Smalltalk developer:** Arrays remain immutable value objects with structural
  `=`. This is *more* consistent than Pharo (where array `=` is element-wise but
  identity differs); the change strengthens "same contents ⇒ equal."
- **Erlang/BEAM developer:** The `'data'` payload is no longer an Erlang `array`
  record. Anyone reaching into the raw term via FFI would need to use the
  `beamtalk_array` API instead — but that was always the supported boundary.
  `phash2` and `=:=` behave predictably (canonical terms), which is what BEAM
  developers expect.
- **Operator:** No hot-code-reload impact (internal helper module). Removes a
  latent production hazard: a tight `at:put:` loop over a large array currently
  degrades to O(n²) and can stall a process for tens of seconds.
- **Tooling developer:** No AST/LSP impact; the representation is below the
  language surface.

## Steelman Analysis

### Option 1 — Change the representation (Recommended)
- 🧑‍💻 **Newcomer:** "Arrays behave the way I'd assume — equal contents are equal,
  and a loop of updates doesn't freeze. I never have to know why."
- 🎩 **Smalltalk purist:** "Immutable value object with structural equality —
  this is the *clean* version of Smalltalk arrays, with history-independent
  identity."
- ⚙️ **BEAM veteran:** "A cache-free nested-tuple trie is genuinely `=:=` and
  `phash2`-stable. No VM-level trickery, no key-identity hooks — it just works
  with maps and ordsets."
- 🏭 **Operator:** "Eliminates an O(n²) stall in production. One contained module
  change, no protocol churn, no migration."
- 🎨 **Language designer:** "Keeps ADR 0002's strict-`=:=` contract intact and
  makes dict/set keys correct for free. The invariant 'contents determine the
  term' is the elegant one."

### Option 2 — Structural `=`/`hash` for Array + amend ADR 0002
- 🧑‍💻 **Newcomer:** "`a =:= b` would still work for arrays" — but only if they
  never put arrays in a dict/set, where it silently breaks.
- 🎩 **Smalltalk purist:** "Runtime equality dispatch is *more* Smalltalk-like —
  `=` is a real message you can override." (Weaker than it sounds: `=` is not a
  capability Beamtalk is declining to wire up — it is one of an entire family of
  binary operators (`+ - * / % ** == =:= /= =/= < > <= >=`) that *all* codegen to
  inline `erlang:'<op>'(L, R)` with no dispatch and no override hook
  (`operators.rs:54–101`). Making `=` dispatched would make it the *lone*
  overridable binary operator in a language that inlines every other one for BEAM
  efficiency — an inconsistency, not a return to Smalltalk faithfulness.)
- ⚙️ **BEAM veteran:** "I'd rather not pay a dispatch call on every `=`; and this
  still can't fix map keys, since BEAM `=:=` is hard-wired." (Weak for this
  cohort.)
- 🏭 **Operator:** "Smaller diff to the array module" — but `at:put:` stays
  O(log n) only for equality, and the dict/set-key gap is a lurking correctness
  bug.
- 🎨 **Language designer:** "Opening value-type equality to dispatch is a big
  semantic lever for one collection's perf bug — and it descopes the dict/set
  case rather than solving it."

### Option 3 — Status quo (O(n) canonicalisation)
- 🧑‍💻 **Newcomer:** "Correct in every case I can observe." (True — until the
  perf cliff bites.)
- 🎩 **Smalltalk purist:** "Behaviourally indistinguishable; arrays are equal and
  usable as keys."
- ⚙️ **BEAM veteran:** "Dead simple, no new data structure to audit."
- 🏭 **Operator:** "This is the one that stalls for 97 s on a big update loop.
  Hard no at scale."
- 🎨 **Language designer:** "Correct but quadratic — fine as a stopgap, not as the
  intended design."

### Tension points
- Only the **Smalltalk purist** has any pull toward Option 2 (equality as an
  overridable message) — and even that pull is weak, because overridable `=`
  would be a lone exception in a language that inlines its *entire* binary-operator
  family to Erlang BIFs with no dispatch (see the Option 2 steelman). Every other
  cohort favours Option 1, and Option 2 cannot fix dict/set keys regardless — so
  the purist's argument is not just "about language philosophy rather than solving
  the problem," it argues for a one-off inconsistency the language has
  deliberately avoided everywhere else.
- **Newcomer** and **operator** disagree on Option 3: it looks correct in casual
  use but is the production hazard. That gap (correct-looking but quadratic) is
  the strongest reason not to leave it as the long-term answer.

## Alternatives Considered

### Option 2 — Structural `=`/`hash` for Array, amend ADR 0002
Keep BEAM `array` and stop canonicalising in `at:put:` (restore O(log n)).
Instead, introduce runtime equality dispatch for value types so that `Array>>=`
compares element-wise, and define `Array>>hash` element-wise.

Rejected as the primary choice because:
- It **requires amending ADR 0002**, replacing inlined `erlang:'=:='` with
  dispatched equality for value types — a large semantic and performance change
  to the whole language to fix one collection's update cost.
- It **does not fix dict/set keys.** Map key identity is BEAM `=:=` and ordset
  membership is term order; neither offers a custom-identity hook. Two
  cache-different arrays would still be distinct map keys. So this option
  *descopes* arrays-as-keys rather than solving them — leaving a latent
  correctness bug.
- Net: more invasive *and* less complete than Option 1.

### Option 3 — Status quo (O(n) canonicalisation in `at:put:`)
Keep PR #2407's `from_list(array:to_list(...))`. Correct in all cases, zero new
code. Rejected as the long-term answer because repeated `at:put:` is O(n²)
(measured ~166,000× slowdown at 200k×5000), which is a real production hazard for
any array-mutation loop. Acceptable only as the interim state until Option 1
lands.

### Option 4 — Patch the BEAM `array` cache slot in place (rejected)
The cheapest-looking fix: keep BEAM `array`, and after `array:set/3` normalise the
stale copy-on-write cache node back to canonical form with `setelement/3`,
avoiding the O(n) `from_list` round-trip. Investigated and rejected: for a
single-leaf array the divergence is one tuple slot and the patch works, but for
multi-leaf arrays the internal subtree caches diverge at **multiple** nesting
levels, and the `setelement` patch breaks (verified during BT-2377 scoping).
There is no robust, cheap in-place canonicalisation of a BEAM `array` — which is
precisely why the representation has to change.

### Sub-alternatives within Option 1 (to settle at implementation time)
- **Map-backed (index → value):** canonical and O(log n), simplest to implement,
  but higher memory and slower full-iteration (`do:`/`collect:`). Good fallback
  if the trie proves too costly to build well. **Canonicality here is a BEAM VM
  guarantee, not a discipline we maintain:** two Erlang maps are `=:=` iff they
  hold the same key→value associations, *independent of insertion order or
  internal HAMT layout*, and `erlang:phash2` hashes maps by content the same way.
  So `from_list([1,99,3])` and `(from_list([1,2,3]))#{1 => 99}` are genuinely
  `=:=` with zero extra work — which is what makes this the low-risk fallback.
- **`gb_trees` (rejected):** technically canonical *only* under a fragile
  discipline — always build via `from_orddict` (deterministic balanced shape over
  keys `0..n-1`) and only ever `update/3` (which replaces a value in place without
  rebalancing; safe because bounds-checked `at:put:` never inserts a new key). But
  general balanced trees are otherwise insertion-order-dependent, it stores the
  integer keys *and* two child pointers per element (heaviest memory of the
  candidates), and it offers no advantage over a map (which gets the same
  canonicality from the VM with no invariant to maintain). Dominated by the
  map-backed option for simplicity and by the persistent vector for performance.
- **Chunked tuples:** flat tuple split into fixed-size chunks; O(√n) or O(log n)
  depending on nesting. Simpler than a full trie but with worse asymptotics.
- **Persistent vector / RRB trie (preferred):** best asymptotics and canonical,
  stores *only* values (no keys → lowest memory), highest implementation effort.
  The implementation issue should benchmark at least the map-backed and trie
  options against the acceptance criteria before committing.

## Consequences

### Positive
- O(log n) `at:put:`; eliminates the O(n²) loop hazard (≈166,000× faster on the
  measured workload).
- Arrays-as-dict/set-keys are correct **for free** — no map/ordset hooks needed.
- ADR 0002's strict-`=:=`, no-coercion, inlined-equality contract stays intact.
- Change is confined to `beamtalk_array.erl` and the array-literal construction
  path; no codegen, protocol, or migration impact (constraint 4).
- Removes the BT-2362 footgun class entirely (no mutable cache slot to leak).

### Negative
- A new data structure to implement, test, and maintain (vs leaning on stdlib
  `array`). Highest-effort option.
- `phash2` of a large array key remains O(n) (unchanged from today, but now a
  documented, intended cost rather than incidental).
- Possible memory-overhead change vs BEAM `array`; must be bounded by the
  acceptance criteria.

### Neutral
- Public Array protocol, the tagged-map wrapper, and the `'$beamtalk_class'` tag
  are unchanged.
- FFI consumers reaching into the raw `'data'` term (unsupported) would break;
  the supported boundary (`beamtalk_array` API) is unaffected.

## Implementation

Confined to the Runtime context:

1. **Choose the representation.** Prototype and benchmark the map-backed and
   persistent-vector candidates against the five acceptance criteria
   (canonicality, O(log n) ops, equality/hash cost, memory, no cache slot).
   Document the choice and benchmark numbers.
2. **Reimplement `beamtalk_array.erl`** internals over the chosen structure:
   `from_list`, `at`, `at_put` (now O(log n), no `from_list` round-trip),
   `size`, `is_empty`, `do`, `collect`, `select`, `inject_into`, `includes`,
   `slice_from`, `print_string`. Keep all `#beamtalk_error{}` paths and selectors
   identical.
3. **Tests** (per MEMORY runtime rule — every runtime function needs EUnit
   coverage): port the BT-2362 regressions (structural `=:=` vs literal,
   `phash2` consistency, chained `at_put`) and add canonicality property tests
   (random construction paths to the same sequence must be `=:=`), plus a
   large-array `at:put:`-loop perf guard.
4. **e2e btscript** (per MEMORY epic-validation rule): arrays as Dictionary/Set
   keys across the canonicalisation boundary; large-array update loop completing
   sub-second.
5. **Update docs:** revise the `at_put/3` doc comment (drop the O(n)/O(n²)
   caveat), and note Array's representation in `docs/beamtalk-language-features.md`
   if it documents internals.

Affected components: `runtime/apps/beamtalk_stdlib/src/beamtalk_array.erl`
(primary), plus tests. Codegen (`primitives/array.rs`, `expressions.rs` literal
path) is unaffected unless the literal builder is optimised to call a new bulk
constructor.

## Migration Path

No user-facing migration. Array's public protocol and observable behaviour are
unchanged; only performance and internal term shape change. The interim O(n)
`at:put:` from PR #2407 is replaced wholesale. The only break is for unsupported
code that inspects the raw Erlang `array` term via FFI, which was never a
contract.

## References
- Related issues: BT-2377 (this decision), BT-2362 (interim fix), BT-2364
- Pull request: #2407 (interim O(n) canonicalisation)
- Related ADRs: ADR 0002 (strict `=:=` equality, inlined), ADR 0037 (Collection
  class hierarchy)
- Code: `runtime/apps/beamtalk_stdlib/src/beamtalk_array.erl` (`at_put/3`);
  `crates/beamtalk-core/src/codegen/core_erlang/operators.rs:61`;
  `crates/beamtalk-core/src/codegen/core_erlang/primitives/array.rs`;
  `runtime/apps/beamtalk_stdlib/src/beamtalk_map.erl`;
  `runtime/apps/beamtalk_stdlib/src/beamtalk_set.erl`
- Prior art: Clojure persistent vectors; Scala RRB-Vector; Erlang `array` module
  internals (copy-on-write cache)
