# ADR 0087: Maintained Selector→Sites Cross-Reference Index for SystemNavigation

## Status
Proposed (2026-05-26)

## Context

### Problem

Every `SystemNavigation` query that needs AST information — `sendersOf:`,
`referencesTo:`, `unimplementedSelectors`, `unusedSelectors`,
`fieldReadersOf:in:`, `fieldWritersOf:in:`, `ffiSitesFor:` — currently
**re-parses every method's source on every call**, and does so by crossing
the runtime→compiler process boundary on each method:

```text
SystemNavigation.bt
  └── (Erlang beamtalk_interface) findSendersIn:selector:        [BT]
        └── beamtalk_interface                                    [Erlang]
              └── beamtalk_compiler:find_senders_in_source/2      [Erlang]
                    └── beamtalk_compiler_server (gen_server)     [Erlang]
                          └── beamtalk_compiler_port (ETF wire)   [Erlang]
                                └── Rust query (re-parse + walk)  [Rust]
```

Each query's own docstring already flags this:
"a maintained `selector -> [sites]` index is a follow-up optimization"
(`stdlib/src/SystemNavigation.bt:303-306`). At a workspace scale of a few
hundred classes, `sendersOf:` takes seconds per call — fine for an
on-demand IDE pane, untenable for live-highlight, hover-driven incoming
calls, or a typo lint that wants to run on every save.

### Why this cost is structural, not incidental

In a classic Smalltalk image the compiler and the runtime are the same
process: a `CompiledMethod` retains its literal frame, so `aMethod messages`
(the selectors it sends) is a cheap in-process read. Implementors are a
flat in-image method-dictionary index. `allUnimplementedCalls` is then
`(all sent) − (all defined)` over already-materialised structures.

Beamtalk has a **split brain**:

- The running artifact is BEAM bytecode, which does not retain queryable
  Beamtalk-level send metadata after codegen.
- The parser/AST lives in the out-of-process Rust compiler.

So today we keep method *source text* (ADR 0033) and re-parse on demand to
recover what a Smalltalk image would have kept natively. That re-parse +
port round-trip is the entire multi-layer tax visible across every
navigation query.

### Current State

| Aspect | Today |
|---|---|
| Per-query parse cost | One full AST walk per method per query |
| Process boundary | One ETF round-trip per method per query |
| Index storage | None — pure recomputation |
| Cache invalidation surface | Zero (recomputed from source every call) |
| Synthetic methods (compiler-generated accessors, context accessors) | Invisible to source-text queries |
| Source-bearing extensions (`register/5`, BT-2196) | Visible, re-parsed each call |
| Sourceless runtime funs (`register/4`, `put_method/3`, computed-fun builders) | Invisible |

### Constraints

1. **Public `SystemNavigation` BT API must not change.** This is an
   internal swap. Existing query tests (BUnit + repl-protocol E2E) are
   the regression net — results must be identical before/after, modulo
   the documented "more complete" exceptions below.
2. **Coherence with the running image.** Queries must reflect what the
   live image actually dispatches, not a snapshot of `.bt` files. The
   image is authoritative; the index mirrors it.
3. **OTP logger discipline** (per `CLAUDE.md`). All log calls use
   `?LOG_*` with `domain => [beamtalk, runtime]`.
4. **Codegen rules**: any Core Erlang the compiler emits for index
   metadata still goes through the `Document`/`docvec!` API. Wire-format
   payloads from the compiler port use `eetf::Term` builders, not string
   concatenation.

## Decision

**Build the BEAM analog of Smalltalk's literal frame: a runtime-resident,
ETS-backed cross-reference index, populated at compile/load time and
maintained across reload / unload / method-level edit / extension
register-unregister.** Navigation queries read it directly instead of
re-parsing source.

The index is the **single source of truth** for navigation queries and
MUST stay consistent with the running image at all times. Image events
drive index updates; file events do not. The cost we are deliberately
buying is the cache-invalidation surface; the savings are query latency
and process-boundary hops eliminated.

### Index data model

Three ETS bag tables (methods, senders, references) plus a small
fourth `xref_class_gen` metadata table for the per-class generation
counter (see "Atomicity"), all owned by a supervised gen_server
`beamtalk_xref` and created `protected, named_table, {read_concurrency,
true}` so writes serialise through the gen_server but reads bypass it.

```erlang
%% Per-method registration. Keyed by {class, class_side, selector} so
%% reload can match_delete a whole class's rows cheaply.
beamtalk_xref_methods :: bag of
    { Key :: {Class :: atom(), ClassSide :: boolean(), Selector :: atom()},
      MethodInfo :: #{
        owner          := atom(),
            %% normalised class atom (instance tag, or 'Foo class' for metaclass)
        line           := pos_integer(),
            %% method definition line in source
        source_status  := indexed | unindexed_runtime_fun | synthetic,
            %% indexed                = normal compiled-from-source method
            %% unindexed_runtime_fun  = register/4, put_method/3, computed fun
            %% synthetic              = compiler-generated accessor
        provenance     := class_body | extension | class_builder | put_method,
        gen            := pos_integer()
            %% per-class generation, see "Atomicity"
      }
    }.

%% Reverse index: selector -> sites that send it. Drives sendersOf:.
beamtalk_xref_senders :: bag of
    { Selector :: atom(),
      Site :: #{
        owner          := atom(),
        class_side     := boolean(),
        method         := atom(),        %% the calling method's selector
        line           := pos_integer(), %% line of the send within the method
        recv_kind      := self_recv | super_recv | erlang_ffi | other,
        gen            := pos_integer()
      }
    }.

%% Reverse index: class -> sites that reference it. Drives referencesTo:.
beamtalk_xref_references :: bag of
    { ReferencedClass :: atom(),
      Site :: #{
        owner          := atom(),
        class_side     := boolean(),
        method         := atom(),
        line           := pos_integer(),
        gen            := pos_integer()
      }
    }.
```

The defined-selector set is the implicit union of
`beamtalk_xref_methods`; no separate table.

`fieldReadersOf:in:` / `fieldWritersOf:in:` and `ffiSitesFor:` follow the
same pattern with their own bag tables in later phases; the initial slice
covers `sendersOf:` / `referencesTo:` / `implementorsOf:` /
`unimplementedSelectors` / `unusedSelectors` / `selectorsMatching:`.

### Coherence contract

#### Governing invariant

The index reflects the **live image** at all times. A reader must never
observe the index and the running class dictionary disagreeing. The image
(loaded classes, REPL-defined classes with no source file, live-patched
methods) is the authority; the index mirrors it.

#### One source→xref write path

Every mutation that changes what a method sends or defines is driven by
**image events, not file events**, and funnels through a single uniform
operation: **(re)parse the affected method's source → recompute its xref
rows → atomically update the index.**

The xref payload **co-locates with the class registration**: codegen
emits the per-method xref list as a field of the `ClassInfo` map that
`register_class/0` already passes to `beamtalk_object_class:start/2`
(the same map carrying `method_source` / `class_method_source` per
BT-2195/BT-2246). The class-registration path forwards that payload
synchronously to `beamtalk_xref` during class creation. No separate
wire-format payload; no separate consumer; the data lives with the
module that owns it and survives node restarts as part of the BEAM
module's `register_class/0` body.

Entries:

| Trigger | Source provider | Re-parse needed? |
|---|---|---|
| Class load | `ClassInfo.method_xref` baked into `register_class/0` | No — compiler already walked the AST |
| Class reload (`Behaviour reload` → `beamtalk_hot_reload`) | new module's `register_class/0` payload | No — same |
| Class unload / destroy | n/a (registry-coordinated purge of the class's rows) | n/a |
| Method-level edit (`put_method/4` with source, ADR 0082) | `method_source` field of the patch | Yes — re-parse the one method |
| Extension register (`register/5`, BT-2196) | `beamtalk_extension_sources` | Yes — re-parse the one method |
| Extension unregister | n/a (purge only) | n/a |
| ClassBuilder install (with `methodSource:`, BT-2195/BT-2246) | builder state map | Yes — re-parse the one method |

For load/reload the compiler already produced xref metadata, so those
entries consume the payload; the edit/extension/builder entries re-parse
the one method. **Same destination, same atomic update.** `put_method`
is not a special case — it is one entry into the uniform path.

#### Atomicity (per-class generation counter)

Index updates MUST be atomic from a reader's perspective. A reload
(purge + reinsert) must never expose a window where a loaded class has
zero rows.

**Mechanism: per-class generation.** A small `xref_class_gen` metadata
table tracks the current `gen` for each loaded class. Each row in the
three index tables carries the `gen` it was inserted under. On reload:

1. The gen_server reads `current_gen` for the class, increments to
   `new_gen`.
2. New rows are inserted with `gen = new_gen` (the old rows still exist,
   tagged with `old_gen`).
3. The class's generation is bumped to `new_gen` in `xref_class_gen` in
   one ETS write.
4. Old-generation rows are swept asynchronously (a background `select_delete`).

Readers fetch `current_gen` for the class first, then filter rows by that
gen. Between steps (2) and (3) readers still see the old gen — consistent
old view. After step (3) readers see the new gen — consistent new view.
**There is no instant at which a reader sees a partially-built new
state.** The async sweep in step (4) is a memory hygiene operation, not a
correctness one.

For a fresh class load (no prior gen), step (1) reads "no current gen",
sets `new_gen = 1`, inserts, then atomically publishes `current_gen = 1`.
Same shape; the old-rows set is empty.

**Scope of atomicity is per-class, not global.** A multi-class walk
(e.g. `unimplementedSelectors` over `allClasses`) reads each class's
`current_gen` independently, so a reload completing mid-walk can show
the walk a pre-reload view of class A and a post-reload view of class B.
This matches today's source-scan behaviour (each class is scanned
independently) and matches what an IDE user would expect — there is no
cross-class transaction. Callers that want a global snapshot must
take it externally (none do today).

#### Authoritativeness + miss policy

Because the per-query re-parse is removed from migrated queries, an index
**miss** must not silently become a wrong answer. A miss on a class that
`beamtalk_class_registry` reports as loaded is a defect. The read path
MUST:

1. Fall back to on-demand re-parse for that class — so the user gets the
   right answer.
2. Log a warning with `domain => [beamtalk, runtime]` recording the class
   and the calling query — so the gap surfaces and self-heals at read
   time rather than hiding.

Queries never return a silently-incomplete result.

The fallback path is the existing `findSendersIn:selector:` /
`findReferencesToIn:class:` / etc. FFI calls, which remain available
exactly because they are also the only way to handle the documented
sourceless runtime-fun case (see below).

#### Interaction with ADR 0082 (method-level edit and save)

ADR 0082 (live workspace `>>` patches, ChangeLog, `Workspace flush`,
external-edit detection, LSP write coordination) introduces no
image-mutation paths beyond the ones already in the write-path table.
It adds orchestration around them. The sync analysis:

| ADR 0082 surface | Mutates image? | xref sync |
|---|---|---|
| Live `>>` patch | Yes — lowers to `put_method/4` | covered by `put_method/4` entry; the affected method is re-xrefed from new source |
| `Workspace flush` (memory → disk) | No — image already mutated by the prior `>>`; flush writes disk only | no xref action; the image is already in the new state |
| External-edit detection prompt (user defers reload) | No — image unchanged until reload | xref correctly reflects pre-edit image; matches governing invariant ("image is authoritative") |
| External-edit detection prompt (user accepts reload) | Yes — `Behaviour reload` | covered by reload entry; per-class purge + new-gen install |
| ChangeLog replay / undo | Yes — replays through `put_method/4` forward | xref reacts to the forward write; no ChangeLog awareness required |
| LSP textDocument/edit applied | Yes — lowers to `put_method/4` | same as `>>` |

The only failure mode is contract break — a new image-mutating path
that bypasses every documented entry. The miss-policy fallback
surfaces that defect as a logged warning rather than letting it
silently lie.

**Line-number stability across flush.** `Workspace flush` splices a
patched method body back into its `.bt` file via the trivia-preserving
printer (ADR 0082). That changes *file-relative* line numbers but not
*method-relative* ones. The xref stores method-relative line numbers
(same semantics as today's source-scan FFI returns). Flush therefore
does not invalidate xref rows; consumers (LSP, browser, REPL) translate
method-relative → file-relative using the method's current file
position, as they already do.

#### Bootstrap ordering (no backfill required)

`beamtalk_xref` is the **first non-pg child** of `beamtalk_runtime_sup`,
ordered ahead of `beamtalk_bootstrap` and `beamtalk_stdlib`. By the
time any class's `on_load` → `register_class/0` runs, the xref tables
exist and are accepting writes. Each class registers its xref payload
synchronously alongside its class-state, so the index grows atomically
with the class registry itself. No separate backfill protocol is
needed.

The supervisor's child order becomes the load-bearing invariant:

```
1. beamtalk_xref            (this ADR)
2. beamtalk_bootstrap       (process group + Erlang stub classes)
3. beamtalk_stdlib          (compiled .bt class modules)
4. beamtalk_object_instances
5. ...
```

Erlang stub classes registered from `beamtalk_bootstrap` (`Behaviour`,
`Class`, `Metaclass`, `ClassBuilder` — the metaclass-tower scaffolding
that pre-dates the .bt-compiled stdlib) emit `method_xref => []` and
their methods land as `source_status = unindexed_runtime_fun` rows —
the genuine narrow sourceless category.

Stdlib classes — including primitives like `Integer`, `Float`, `Symbol`,
`String` — have full `.bt` source. The method bodies are
`@primitive "..."` declarations, but the surrounding class headers,
selectors, type annotations and AST are real Beamtalk that the compiler
walks like any other. They ride the `method_xref` payload baked into
`register_class/0` like any other compiled-from-source class.
`@primitive` method bodies parse to empty send lists (they delegate to
Erlang) but the type-annotation references on the signature
(`+ other :: Number -> Integer`) are preserved normally and visible to
`referencesTo:`.

#### Synthetic / compiler-inserted methods

Compiler-generated methods — auto-accessors (`field/1` getters,
`withField:/2` setters; `value_type_codegen.rs`), and any future
compiler-emitted method shape that does not originate in user
`.bt` source — have **no user source text** but are **fully known to
the compiler**. They ride the same xref write path: the compiler emits
their sent/defined selectors alongside hand-written methods. No source
channel required.

Two consequences:

1. **They become newly visible to navigation queries.** Today's
   source-scan queries cannot see them (no source to scan). Surfacing
   them is *more complete* than the pre-index behaviour — e.g.
   `implementorsOf: #value` on an auto-accessor returns empty today but
   returns the synthetic getter under the index. This is a **documented
   parity exception**, not a regression.
2. **They have no source line.** Their rows carry `source_status =
   synthetic` and a **derived location** — the generating declaration
   (the `field:` / `state:` slot line, or the class header) — so the LSP
   and System Browser can navigate to the origin rather than a missing
   line.

Default: synthetic methods are **included** in results. The `synthetic`
flag lets a consumer filter them if needed.

#### Sourceless methods (the irreducible remainder)

Both metaprogramming entry points already have a source channel:

- Extensions via `register/5` → `beamtalk_extension_sources` (BT-2196).
- ClassBuilder via `methodSource:` → `method_source` (BT-2195/BT-2246).

So a *source-bearing* extension or builder-defined method **is**
indexable like any other. ClassBuilder and `register/4` are not blind
spots as categories.

A method is unindexable only when its source channel is **empty** —
methods whose behaviour is a closure with no originating source text:

- `register/4` (≡ `register/5` with `Source = undefined`).
- ClassBuilder invoked without `methodSource:`.
- `put_method/3` (the sourceless arity).
- Any runtime-synthesised method assembled from a computed fun.

For these there is nothing to parse, by construction. They are recorded
in `beamtalk_xref_methods` with `source_status = unindexed_runtime_fun`,
**distinct from absent** — so a query reports "defined here, sends not
analysable" rather than lying (absent) or returning a silent split-brain.
This residue is shrinkable (see BT-2228 follow-ups) but not zero.

This is the *runtime-fun* sourceless case — distinct from
compiler-synthesised methods above, which the write path indexes fully.

### Codegen extension (no separate wire-format payload)

The xref data is emitted **directly into the Core Erlang
`register_class/0` body** alongside the `method_source` /
`class_method_source` maps already there. Same `Document` / `docvec!`
codegen pattern as `method_source` (see
`crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`).
The `ClassInfo` map passed to `beamtalk_object_class:start/2` gains one
field:

```erlang
ClassInfo = #{
    name => 'Counter',
    superclass => 'Object',
    module => bt@example@counter,
    fields => [...],
    is_abstract => false,
    class_methods => #{...},
    instance_methods => #{...},
    method_source => #{...},           %% BT-2195
    class_method_source => #{...},     %% BT-2195
    method_xref => [                   %% this ADR
      #{
        class_side       => false,
        selector         => 'increment',
        line             => 14,
        sends            => [#{
                              selector  => '+',
                              line      => 17,
                              recv_kind => self_recv
                           }],
        references       => [#{
                              class => 'Integer',
                              line  => 16
                           }],
        source_status    => indexed,
        synthetic_origin => undefined  %% pos_integer() when source_status = synthetic
      },
      ...
    ]
}
```

`beamtalk_object_class:start/2` forwards the `method_xref` value
synchronously to `beamtalk_xref` during class creation, before
returning. The class is not considered "loaded" until xref has the
rows.

The Rust side reuses the existing `senders_query`/`references_to_query`/
`all_sends_query` AST walkers — they already produce `SendHit` /
reference / send-with-receiver records. They run once per method during
compilation, on the AST the compiler already has. The results are
emitted as Core Erlang literal terms inside the `register_class/0`
function body via `docvec!` (CLAUDE.md mandates Document/docvec! for
all Core Erlang fragments). No new walker, no new wire-format field,
no new REPL load path — REPL `:load` and stdlib `on_load` both reach
the same `register_class/0` and inherit the index update for free.

### Migration scope (this ADR)

The initial migration covers the six queries the issue calls out:
`sendersOf:`, `referencesTo:`, `implementorsOf:`, `selectorsMatching:`,
`unimplementedSelectors`, `unusedSelectors`. `methodsMatching:` still
needs source text for the regex match and stays on the source channel
(no AST port hop today either). `fieldReadersOf:in:`,
`fieldWritersOf:in:`, `ffiSitesFor:` migrate in a later phase using the
same machinery and a parallel set of bag tables.

### Implementation phasing (high-level)

This ADR is the predicate; implementation lands in phases below.

| Phase | Slice | Closes |
|---|---|---|
| 1 | `beamtalk_xref` gen_server skeleton + tables + supervisor wired as the first non-pg child | infrastructure |
| 2 | Codegen emits `method_xref` into `register_class/0`; `beamtalk_object_class:start/2` forwards to xref synchronously | write path |
| 3 | Migrate `sendersOf:` end-to-end with miss-policy fallback; benchmark before/after | first query (wire-check) |
| 4 | Reload purge + new-gen install via `beamtalk_hot_reload`; `put_method/4` re-xref; extension and ClassBuilder hooks | full lifecycle |
| 5 | Migrate remaining five queries in scope | read-path migration |
| 6 | Synthetic accessor emission + tagging; parity exception encoded in tests | completeness |

Phases 1–3 are the minimum-viable index — Phase 3 acts as the napkin
wire-check that the whole compiler→`register_class/0`→ETS→stdlib path
round-trips correctly before broader migration. Phases 4–6 close the
acceptance criteria. There is no separate backfill phase: the
co-located write path covers stdlib boot via supervisor ordering, REPL
load via the same `register_class/0`, and dynamic class creation via
the same hook.

## Prior Art

### Pharo / Squeak / Cuis

`CompiledMethod >> messages` is a cheap in-image read of the method's
literal frame — the selectors the method sends are stored alongside the
bytecode at compile time, not recomputed.
`SystemNavigation >> #allImplementorsOf:` walks `SystemDictionary`
(a flat in-image hash) once. `#allSendersOf:` is backed by the
literal-frame walk.

We adopt the *idea* (precompute and store the per-method send set) but
not the storage location. Smalltalk stores it on the CompiledMethod;
Beamtalk stores it in ETS owned by a runtime gen_server because BEAM
modules don't have the equivalent of a Smalltalk CompiledMethod that we
control the layout of.

### Pharo `SystemNavigation` selector index

Pharo maintains a global `Symbol >> #implementors` view: each Symbol
knows the methods that hold it. Beamtalk's `beamtalk_xref_senders` is the
direct analogue, scoped to message sends rather than to the more general
"holds this literal".

### Erlang xref (kernel app)

OTP ships an `xref` analyser that runs over loaded modules. It targets
the Erlang module/function level, not the Beamtalk selector level, and
operates as a one-shot analysis tool rather than a live maintained index.
It cannot answer "which Beamtalk class sends `#asString`" because
Beamtalk sends compile through `beamtalk_dispatch` and lose their
selector identity at the BEAM-function level. So we cannot piggyback on
Erlang xref; the index has to live at Beamtalk-semantic granularity.

### Newspeak / Self mirrors

Both languages put navigation behind a `Mirror` protocol implemented
against the live image. `SystemNavigation` already follows that shape on
the read side; this ADR is the write-side counterpart.

## User Impact

- **Newcomers** see no API change. Same selectors, same return shapes.
  The `synthetic` parity exception (auto-accessors appearing in
  `implementorsOf:` results) is the only externally-visible difference,
  and is more complete rather than weirder.
- **Smalltalk developers** get the latencies they expect from a System
  Browser: sub-millisecond `sendersOf:`, `implementorsOf:`,
  `referencesTo:`. Equivalent to opening a Pharo Senders pane. The
  "Find Senders" / "Find Implementors" interaction loop becomes viable
  for live editing rather than just batch inspection.
- **Beamtalk-on-BEAM developers** writing tooling against navigation
  queries (LSP integrations, ADR 0017 LiveView IDE, the
  `beamtalk-lsp` epic) can back hover-driven incoming/outgoing call
  panels and on-save lint surfaces without re-parsing thousands of
  methods. Adjustment for tool authors: they must tolerate the
  `synthetic` and `unindexed_runtime_fun` markers on returned rows and
  decide whether to filter them. The miss-policy warning is the
  affordance that surfaces accidental gaps during tool development.
- **Erlang/BEAM operators** see one new supervised worker
  (`beamtalk_xref`) as the first non-pg child of `beamtalk_runtime_sup`,
  with `domain => [beamtalk, runtime]` log lines for reload purge and
  miss-policy warnings. The new tables (`beamtalk_xref_methods`,
  `_senders`, `_references`, `xref_class_gen`) are observable via
  standard `ets:info/1` / `ets:tab2list/1` for debugging. A
  `beamtalk_xref` crash drops the tables; the supervisor restart
  brings up empty tables, and in-flight navigation queries hit the
  miss-policy fallback (correct results, warning logged) until each
  class's `register_class/0` is re-driven — which happens naturally
  on the next class load, hot-reload, or operator-triggered
  `Behaviour reload` of affected classes. (A future enhancement could
  trigger a full re-register via the class registry; not required for
  correctness.)

### Discoverability

The index is invisible to surface code; discoverability is unchanged.
Operators get one named process to inspect (`sys:get_state(beamtalk_xref)`)
and three named ETS tables. The miss-policy warning is the affordance
that surfaces accidental gaps in coverage.

## Steelman Analysis

### "Don't build it — the naive scan is fast enough"
The strongest argument against this ADR: the queries already work, the
docstrings already note they're acceptable for an on-demand IDE pane,
and every line of this index is new state to keep coherent. The current
zero-cache-invalidation property is worth a lot.

Counter: it stays true as long as queries remain on-demand and triggered
by user clicks. The moment the IDE wants to drive a feature off them —
hover-incoming-calls, on-save typo lint, live-highlight, semantic-token
rainbow — the seconds-per-call envelope is the wrong shape regardless of
how clean the implementation is. ADR 0017 and the `beamtalk-lsp` epic
are explicit about wanting those features.

### "Build something narrower"
Make `beamtalk_extensions` keep its own narrow reverse index (BT-2202
already did this for `extendersOf:`) and let the rest stay naive.

Counter: that doesn't help the queries that are actually slow
(`sendersOf:`, `referencesTo:`, the composite typo finder, the dead-code
finder). Those all need send/reference information at method-body
granularity, which is exactly what this index provides and nothing
narrower does.

### "Cache the AST, not the analysis"
Store the parsed AST per method on the runtime side; queries run their
walks against the cached AST instead of re-parsing.

Counter: avoids the parse cost but not the AST walk cost, and the AST is
larger than the analysis output (rows are a few words; an AST is dozens
of nodes per method). Worse, the AST schema is internal to the Rust
crate and would have to be shipped over the wire and decoded into ETS —
a much heavier coupling than shipping pre-computed `{selector, line,
recv}` triples. We rejected this in favour of shipping the analysis.

## Alternatives Considered

### Reverse-index in each subsystem (per-feature mini-indices)
Keep `beamtalk_extensions:extenders_of/1` (BT-2202) as a model; add
parallel narrow indices in `beamtalk_object_class` for instance methods,
in `beamtalk_class_registry` for class names, etc.

Rejected because it leaks the same data shape into multiple subsystems
and forces each subsystem to grow a lifecycle protocol. One index, one
lifecycle, one set of tables is simpler to reason about and harder to
desync.

### Index in the compiler, not the runtime
Have `beamtalk-core` keep an in-memory index across compilations and
expose queries against it via the existing port.

Rejected because the compiler is not the source of truth for the running
image — REPL-defined classes, hot-reloaded methods, and `put_method`
patches do not flow through the compiler's persistent state. The index
has to live on the side that sees every image event, which is the
runtime.

### Recompute on first query, cache thereafter
Lazy population on first navigation call; index TTL governs staleness.

Rejected because cache TTL never reconciles correctly with hot reload.
The whole point of the coherence contract is that the index tracks
image mutation events, not wall-clock time. A TTL'd cache would either
be too eager (rebuild constantly) or too lax (return stale results across
a reload).

### Use Mnesia / dets
Persistence to disk for survival across node restarts.

Rejected because the index is **derivable** — the runtime can always
rebuild it from the class registry's stored sources via backfill. Adding
persistence adds a sync surface (disk vs ETS) without a correctness gain.

## Consequences

### Positive
- `sendersOf:`, `referencesTo:`, etc. drop from seconds to
  sub-milliseconds. Unblocks LiveView IDE incoming/outgoing-call panes
  and the on-save typo-lint use case.
- Five-to-seven-layer port plumbing collapses to a single ETS
  `match_object` for the migrated queries.
- Synthetic compiler-generated methods become visible to navigation
  queries (closes a longstanding completeness gap).
- Source-bearing extensions and ClassBuilder-built methods join the
  query surface without extra plumbing on the read side.

### Negative
- New stateful subsystem to maintain. Cache-invalidation surface where
  there was zero before.
- Index updates ride every class load and reload — adds a small fixed
  cost to those paths (one extra walk during compile + one ETS write
  batch per class).
- Memory growth: one row per `{class, method, send}` and per
  `{class, method, reference}` triple. A 200-class workspace with ~10
  sends per method comes out to ~tens of thousands of rows. Negligible
  but non-zero.
- Two new sources of subtle bugs: forgetting to wire an image event
  into the write path, and the per-class-generation atomicity scheme
  being implemented incorrectly. Heavy lifecycle test coverage
  required.

### Neutral
- The public `SystemNavigation` BT API is unchanged. All migration is
  internal.
- A documented parity exception (synthetic methods, source-bearing
  extensions appearing in results) replaces existing query test
  assumptions — tests update to assert the additions rather than treat
  them as regressions.
- The `Document` / `docvec!` codegen discipline (CLAUDE.md) and OTP
  logger discipline carry across unchanged.

## Implementation

Phases listed under **Migration scope** above; details tracked in
BT-2228 acceptance criteria. Key entry points:

- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` —
  compute per-method xref (reusing `queries/senders_query.rs`,
  `queries/references_to_query.rs`, `queries/all_sends_query.rs` as the
  source of truth for the walk) and emit it as a `method_xref` map
  field in the `ClassInfo` literal built by `register_class/0`, using
  the same `build_selector_map`-style `Document`/`docvec!` pattern as
  `method_source`. Synthetic-method emission piggybacks on
  `codegen/core_erlang/value_type_codegen.rs`.
- `runtime/apps/beamtalk_runtime/src/beamtalk_xref.erl` (new) —
  gen_server with three protected/named ETS tables, a fourth small
  `xref_class_gen` metadata table, and the per-class-generation
  atomicity protocol.
- `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_sup.erl` — add
  `beamtalk_xref` as the **first non-pg child**, ahead of
  `beamtalk_bootstrap` and `beamtalk_stdlib`, so the index is up before
  any `register_class/0` runs.
- `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` —
  `start/2` forwards the `method_xref` field of `ClassInfo` to
  `beamtalk_xref` synchronously during class creation; `put_method/4`
  re-xrefs the affected method from `Source`.
- `runtime/apps/beamtalk_runtime/src/beamtalk_hot_reload.erl` —
  coordinates the per-class purge with the new module's
  `register_class/0` call (generation bump in xref).
- `runtime/apps/beamtalk_runtime/src/beamtalk_class_registry.erl` —
  unload hook purges xref rows for the destroyed class.
- `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` —
  `register/5` re-xrefs the new method; `unregister` purges;
  `register/4` inserts a `source_status = unindexed_runtime_fun` row.
- `runtime/apps/beamtalk_runtime/src/beamtalk_class_builder.erl` —
  feeds `methodSource:` per-method source into the xref during install
  (riding through `beamtalk_object_class:start/2`'s `ClassInfo`).
- `stdlib/src/SystemNavigation.bt` — repoint migrated queries at
  `(Erlang beamtalk_xref) sendersOf:` etc., keeping the on-miss
  fallback to the existing source-scan helpers.

The fallback path (`findSendersIn:` etc.) is retained — it is the
miss-policy backstop and the read path for sourceless runtime-fun cases.

No backfill module is needed: the xref is the first non-pg supervisor
child, so every class's `register_class/0` runs after xref is alive
and registers its data inline. The compile-port wire format
(`beamtalk_compiler:compile/2` response) is unchanged.

## Migration Path

Not user-visible. Internal swap with a documented parity exception
(synthetic methods, previously-invisible source-bearing extensions now
appear in results). Tests assert the additions explicitly.

## References

- Related issues: BT-2228 (this work), BT-2201 (parent epic, done),
  BT-2190 (`sendersOf:` naive scan, done), BT-2195 (class-side
  `method_source`, done), BT-2196 (`beamtalk_extension_sources`, done),
  BT-2202 (reverse extension index, done), BT-2206 / BT-2207
  (composite queries, done), BT-2246 (ClassBuilder `methodSource:`
  auto-populate, done), BT-2250 (`register/5` foreign-extension codegen,
  done)
- Related ADRs: 0017 (LiveView IDE — consumer), 0022 (compiler-over-port —
  the boundary the index lets us collapse), 0033 (runtime-embedded source —
  the read substrate the source-scan path uses for fallback), 0036
  (full metaclass tower — class-side vs metaclass row keying), 0066
  (open class extension methods — extension write path), 0070 (packages —
  owner provenance), 0082 (method-level edit and save — `put_method/4`
  invocation surface), 0084 (class-side runtime method-fun dispatch —
  sourceless-fun edge case)
- Documentation: `stdlib/src/SystemNavigation.bt`,
  `docs/development/surface-parity.md`
