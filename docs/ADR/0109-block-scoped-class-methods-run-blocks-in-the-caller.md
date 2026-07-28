# ADR 0109: Block-Scoped Class Methods Run Their Block in the Caller's Process

## Status
Accepted (2026-07-28)

## Implementation Tracking

**Issues:** BT-3018 (class-process block execution) · BT-3020 (handle ownership/leak, partially)
**Related:** BT-2975 (FileHandle incremental I/O — surfaced the problem), BT-3019 (unbounded `readAll` — same blast radius)

## Context

### Problem statement

A class-side message send does not run in the caller's process. `beamtalk_class_dispatch:class_send_dispatch/3` performs a `gen_server:call(ClassPid, {class_method_call, Selector, Args, Ctx}, 60000)`, so the method body executes inside the singleton gen_server for that class.

That is fine for `File readAll:` — a short, self-contained operation. It is not fine when the class method takes a **Block**, because the *user's* block then runs inside the class process too. `File open:do:` has always had this shape; BT-2975 added `File open:mode:do:` and made it much easier to reach, because the whole point of that API is a loop of writes with an fsync per record.

Three distinct problems fall out, all currently mitigated only by documentation:

**1. Deadlock on any re-entrant `File` send.** The block cannot message `File` again — it is already inside that process:

```beamtalk
File open: "a.txt" mode: #read do: [:h | File exists: "b.txt"]
```

fails with `{calling_self, {gen_server, call, [<0.138.0>, {class_method_call, 'exists:', ...}, 60000]}}` — a raw gen_server tuple, not a `#beamtalk_error{}`. There is precedent for doing better: BT-2005 added `handle_metaclass_self_call/2`, which intercepts `Pid =:= self()` on the *metaclass* path and raises a structured `dispatch_error` "rather than an opaque `calling_self` timeout." `class_send/3` has the same `ClassPid =:= self()` guard, but only for `new`/`new:`/`spawn`/`spawnWith:` — every other selector falls through to the deadlocking call.

**2. Global serialization.** Every `File` operation in the node queues behind the block. A block holding the class process for the duration of an append-only write loop turns `File` into a global I/O lock. `open:do:` had this too, but it only streamed lines; `open:mode:do:` invites long write loops.

**3. A 60-second ceiling.** `class_send_dispatch/3` uses `Timeout = 60000` for non-test selectors. A block that runs longer raises a timeout **in the caller** while the block keeps running in the class process — the caller cannot tell whether its writes landed. There is no way to opt out.

### Why this is worth an ADR

The obvious framing — "make class methods run blocks in the caller" — reads like a change to `beamtalk_class_dispatch`, i.e. to how *every* class method in the system dispatches. That framing is wrong in a way worth recording, because it is the expensive and risky reading of the problem.

A general "run the Block argument in the caller" mechanism would require the class process to hand a continuation back to the caller and resume afterwards: a two-phase protocol on every class-method call, replacing one `gen_server:call` with a call/callback/resume handshake. That is a large change to the hottest dispatch path in the language, it would need new failure semantics (what if the caller dies mid-block?), and it buys nothing for the overwhelming majority of class methods, which take no Block at all.

The narrower reading gets the same user-visible outcome: **the block never needs to reach the class process in the first place.**

## Decision

Lower block-scoped resource methods **at the call site**, so the class process only performs the resource acquisition and the block runs in the caller.

`File open: p mode: m do: blk` becomes, in the caller's own process:

```
case File open: p mode: m of         % short class-process call, returns a handle
  ok Handle  -> [blk value: Handle] ensure: [Handle close]
  error E    -> Result error: E
end
```

The compiler already has the machinery: `WellKnownSelector` intercepts selectors at the call site today (`ifTrue:`, `on:do:`, `ensure:`, `value*`), and `ensure:` in particular is already lowered to Core Erlang `try`/`after` in the caller. This decision adds `open:do:` and `open:mode:do:` to that set, keyed on a `File` receiver.

Three consequences follow immediately, without touching class dispatch:

- **The deadlock disappears** rather than being reported — the block runs in the caller, so a nested `File exists:` is an ordinary send.
- **Serialization disappears** — the class process is held only for the `open`, not the block.
- **The 60-second ceiling disappears** — the only class call is the open.

Independently, and because a raw `calling_self` tuple is a bad diagnostic regardless of this API, **`class_send/3` gains a general `ClassPid =:= self()` clause** that raises a structured `#beamtalk_error{}` naming the deadlock, mirroring BT-2005's `handle_metaclass_self_call/2` catch-all. This is a safety net for every other class method, not just `File`.

### Scope

In scope: `File open:do:`, `File open:mode:do:`, and the general `class_send/3` self-call diagnostic.

Not in scope: a general continuation protocol for class methods; changing the 60-second class-call timeout; making arbitrary user-defined block-taking class methods run their block in the caller. If a third such method appears, this ADR should be revisited to decide whether the interception becomes a declarable property (e.g. a `blockScoped` marker on the method) rather than a hard-coded selector list.

## Consequences

### Positive

- The advertised BT-2975 use case — an append-only log with `sync` per record — stops being a global lock and stops being capped at 60s.
- Re-entrant `File` calls inside an open block simply work, removing a documented footgun that only reads as a footgun once you already know about it.
- Every other class method gets a structured self-call error instead of a gen_server tuple.
- No change to the dispatch hot path, so no regression surface for the ~100 stdlib classes that take no Block.

### Negative

- The selector list is hard-coded in the compiler, so a fourth block-scoped class method needs a compiler change rather than just a stdlib change. This is the explicit trade for not building the general mechanism; the "Not in scope" note records the trigger for revisiting.
- Two lowering paths now exist for `open:...do:` — the intercepted call site and the Erlang `'open:mode:do:'/3` that remains for dynamic dispatch (`perform:`). They must stay semantically identical, which needs a test asserting both paths close the handle on a raised error.
- The block no longer runs in the class process, so a block that relied on that serialization for mutual exclusion between concurrent writers would lose it. Nothing in-tree relies on this, and relying on it was never documented or intended.

### Neutral

- `File open:mode:` (no block) is unaffected — it already returns to the caller immediately.
- The handle remains non-`raw` (BT-2975): the descriptor is still opened in the class process, so it must stay usable from another process.

## Alternatives Considered

**Restructure `beamtalk_class_dispatch` for a general call/callback/resume protocol.** Rejected: large change to the hottest path, new failure semantics to define, and no benefit to the ~99% of class methods that take no Block. Revisit only if block-taking class methods become common.

**Document the constraints and stop there.** This is the status quo after BT-2975, which documented all three consequences in `File.bt` and `beamtalk-language-features.md`. Rejected because the append-only-log pattern the API exists to serve is precisely the one that trips the 60s ceiling, and "don't message `File` from inside `File`" is a rule users only learn by hitting an opaque gen_server tuple.

**Make the class-call timeout configurable.** Addresses only the third problem, and by making the global-lock window longer. Rejected as strictly worse than not holding the lock.

**Run the block in a spawned helper process owned by the class.** Fixes serialization but not the deadlock (the helper still isn't the caller, so `self`-relative state and the process dictionary diverge), and adds a process per open. Rejected.

## References

- BT-3018 — the issue this ADR resolves
- BT-2975 — added `open:mode:do:`; documented all three consequences in lieu of fixing them
- ADR 0056 — actor dispatch and `self delegate`
- `beamtalk_class_dispatch.erl` — `class_send/3`, `class_send_dispatch/3`, `handle_metaclass_self_call/2` (BT-2005 precedent)
- `crates/beamtalk-core/src/ast/well_known.rs` — the call-site interception mechanism this decision extends
