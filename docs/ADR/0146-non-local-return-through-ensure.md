# ADR 0146: A `^` Commits the Actor's Cell, Not a Copy

## Status
Accepted (2026-11-07); proposed 2026-11-06. Tracking: BT-4505.

Builds on ADR 0137 (the actor-state cell) and ADR 0142 (where class methods
run). Amends ADR 0041: for an actor, the state a `^` throw carries is no
longer the state the method commits. Decides BT-4459. BT-4430, which came up
in the same review, was decided separately as an amendment to ADR 0133.

## Context

A non-local return (`^` inside a block) unwinds by throwing. When it starts in
an actor, the throw carries a copy of the actor's state, and the method it
returns from commits that copy as the actor's new state.

Since ADR 0137, an actor's fields also live in a cell that every writer updates
in place. So while a `^` is in flight the fields exist twice: once in the cell
and once in the copy. Any code that writes a field on the way out (most often
an `ensure:` cleanup) updates the cell, and the method then commits the copy,
which does not have the write. Each way a `^` can travel has needed its own
patch to copy the cell back into the throw.

That design produced six fixes in one week (BT-4312, BT-4424, BT-4429, BT-4430,
BT-4433, BT-4448) and one open bug (BT-4459). Two of them came from the same
question every patch must answer: whose state is this copy? BT-4448 answered it
by comparing state maps and took a neighbouring actor's state when the maps
happened to be equal. BT-4459 is the case where the copy was stale from the
start: a block created by actor A, run by actor B, returns to A, and A commits
the state it had when the block was created, losing every write since.

```beamtalk
Actor subclass: Owner
  state: n = 0

  run: peer =>
    blk := [^1]
    self.n := 5
    peer call: blk
    0
```

Here `peer` is another actor whose `call: aBlock` evaluates the block. Today
`Owner` answers 1 from `run:` and is left with `n = 0`. It should keep
`n = 5`.

Reviewing this ADR also found three bugs where the cell itself was out of date
when a `^` started (BT-4523, BT-4524, BT-4525). Those are fixed. They matter
here because the decision below relies on the cell being current.

## Decision

**The method a `^` returns from commits the actor's cell, not the copy in the
throw.** The cell is the one place an actor's fields live. A `^` no longer
carries the fields as state of record, and nothing on the way back has to patch
them.

This adds no new storage. The cell is the one ADR 0137 already introduced: a
single process-dictionary entry that exists only while an actor callback runs.
The commit reads it once. The only stored data this ADR changes is the BT-4448
owner stamp written into state maps, which goes away.

What still travels in the throw is state that has nowhere else to live: the
REPL's bindings, a value type's `self`, and the compiler's temporary locals. An
`ensure:` cleanup that writes those still has to pass its result on, as it does
today.

Whose cell it is gets decided once, by process identity: the commit reads the
cell only when the method is running in the actor that owns it. Nothing
compares two state maps to decide ownership. At home the commit is the cell
as it stands. The throw's own `__local__` keys are dead once the `^` reaches
its method, so none of them is merged back. Away (a block's `super` send runs
the method in another process) there is no cell of the actor's, and the commit
is the carried state unchanged, as today.

This amends ADR 0041 for actors. Their `^` still carries a state slot, because
the same throw shape serves the REPL and value types, and away from home the
slot is the only copy there is. At an actor method's boundary, though, the
slot is no longer the state of record.

Two behaviours follow, and both are deliberate:

- A `^` passing through an `ensure:` keeps the cleanup's field writes, whether
  the `^` came from this method or from a block another method of the same
  actor passed in.
- A `^` that passes through a *different* actor on its way home does not commit
  anything in that actor. That actor keeps the state it had before the call,
  which is how a gen_server callback behaves when it exits by a throw (ADR
  0137's callback contract). Class variables are different: a class written
  during the same unwinding keeps the write, because class variables are a
  store written in place with no callback commit (ADR 0130).

The relay machinery added by BT-4429 and BT-4448, including the owner stamp,
becomes unnecessary and is removed. It is removed one release after the
commit change ships, not with it. Code compiled before the commit change still
commits the copy, and for that code the relay in the stdlib's `Block>>ensure:`
is what keeps a cleanup's writes when `ensure:` is reached through `perform:`.
The stdlib is always rebuilt with the runtime, so removing the relay from it
along with the commit change would bring BT-4429 back for every library not
yet recompiled.

## Prior Art

In Pharo and Ruby, a `^` (Ruby: `return` in a proc) unwinds through `ensure:`
blocks, and because objects live on one shared heap a cleanup's writes are
simply there afterwards. There is no copy to reconcile. This decision gives an
actor the same property: its cell plays the role of the heap.

Neither language lets a non-local return cross into another process. Pharo
raises `BlockCannotReturn`, and Ruby raises `LocalJumpError`, when the home
frame is gone. Beamtalk does relay a `^` across actors (BT-3582). That has no
precedent, so the rule for an intermediate actor is taken from Erlang instead:
a process whose callback exits by a throw keeps its previous state.

## User Impact

For most code nothing changes. Two cases change, both from losing a write to
keeping it. An actor whose block is run by another actor now keeps the writes
it made after creating the block (BT-4459). A `^` from a block passed in by
another method of the same actor now keeps an `ensure:` cleanup's writes.

The cost is one merge of the cell into the reply each time a `^` reaches an
actor method, which is small next to the throw itself.

## Steelman Analysis

**Keep patching the copy.** Each fix is small and local, and the throw stays a
plain value. But every new route for a `^` needs another patch, every patch
needs an ownership test, and two of the last six fixes got ownership wrong.

**Make a `^` that leaves its own actor an error, as Pharo does.** This is the
simplest model: it removes the cross-actor case, the intermediate-actor rule
and the BT-4459 class of bug in one step. A measurement over the stdlib,
Buccaneer, Catalina, beamtalk-http and hpack found no application code that
relies on a cross-actor `^`. What does rely on it is the standard `Collection`
protocol: `includes:`, `detect:` and similar methods are written with `^`, and
work for a user collection whose `do:` runs in an actor only because the `^`
crosses back. Making it an error would need a different early-exit protocol for
user collections. It stays the fallback if cross-actor `^` keeps causing bugs.

**Let the intermediate actor keep its writes.** It would match what a
Smalltalker expects, but one `^` would then commit two actors' state, which
breaks the rule that a message changes one actor at a time.

## Alternatives Considered

**Resolve ownership where the throw crosses a process.** This fixes BT-4459 by
re-homing the copy at each hop. It is another per-route patch, and it keeps the
map comparison BT-4448 introduced.

**Put the owner in the throw.** Adding the owner's identity to the thrown term
changes a shape matched throughout the runtime and generated code, and the
method that commits already knows its owner.

**Give value types and the REPL a cell too.** They thread their state correctly
today. Adding a store for them would be a new runtime mechanism with nothing
to fix.

**Stop carrying actor fields in the throw.** A class method's `^` already
throws no state. If an actor's `^` did the same at home, "commit the cell"
would hold by construction, and the verifier could reject any read of the
slot. It is not adopted now for three reasons. A closure only knows at run
time whether it is at home. Away, the capture is the only copy of the fields,
and an away `super` send's illegal-write check reads it. And the slot's shape
is shared with the REPL and value types. Committing the cell gives the same
guarantee without changing the thrown term. Once the relay is gone, this
remains open as a later simplification.

## Consequences

Ownership is decided in one place, by identity. BT-4459 is fixed by the general
rule rather than by another patch, and the BT-4429/BT-4448 relay code goes. The
compiler's obligation narrows to state that has no cell, which its verifier can
check.

The decision depends on the cell being current whenever a `^` starts. Stated
for every scope: any call that may run a closure holding a `^` is entered with
the method's current state already stored to the cell. That covers a block
passed as a literal, a block held in a variable or parameter, and a block
reached some other way. The three bugs found during review show that this has
to be enforced rather than assumed. Today the check (`UnbracketedNlrClosure`)
only sees block-literal operands, so it is widened to cover any operand that
may be a block. A test build also asserts the invariant directly at the commit.
When a method's own `^` reaches its boundary at home without crossing an
`ensure:`, the fields it carries must equal the cell. Any new way of compiling
a `^` that breaks the invariant then fails across the whole test corpus,
without its own verifier check having to exist first.

Libraries compiled before the commit change depend on the relay for one more
release, so the relay and the new commit live side by side for that release.

## Implementation

1. **Foundation.**
   - The foreign-`^` change: a cleanup's writes are stored to the cell when a
     `^` from another method of the same actor passes through an inlined
     `ensure:`. Only actor instance methods are affected. A class method that
     runs in its caller (ADR 0142) has no actor state, so nothing is emitted
     for it.
   - The pre-store from the BT-4524 fix moves into ThreadedIr.
   - The invariant check is widened to any operand that may be a block.
2. **The commit reads the cell.** A new `beamtalk_actor_state:commit/2`
   answers the cell at home and the carried state away. Also in this phase:
   the test-build assertion described under Consequences, and inspection and
   printing skip `__local__` keys.
3. **Removal, one release later.** In the release after phase 2 ships, the
   relay, the owner stamp and `refresh_carried/2` are deleted. Only the
   stdlib's `Block>>ensure:` emits `refresh_carried/2`, and the stdlib is
   rebuilt with the runtime. Closures compiled between BT-4448 and BT-4536 called
   `nlr_state/2`; that alias of `view/2` never shipped in a release and was
   removed before one (BT-4660), so such nightly builds must be recompiled.

The foreign-`^` change lands before the commit change, or with it, but never
after it. On its own it changes no visible behaviour, because the commit still
uses the copy. In the other order, a foreign `^`'s cleanup writes would survive
only up to the cleanup's last bracketed call. The `ensure:` test corpus is
extended to cover the REPL, a `^` from another method, and a `^` from another
actor. Work items are tracked under epic BT-4533 (design review: BT-4505).

## Migration Path

No source changes. The two behaviour changes described under User Impact go in
the release notes. Code compiled before the commit change keeps its old
behaviour, including BT-4429's relay, until the release that removes the
relay. It must be recompiled by then. The `nlr_state/2` alias that closures
compiled between BT-4448 and BT-4536 call never shipped in a release, so it was
removed before one (BT-4660); such pre-release builds must be recompiled.

## Implementation Tracking

**Epic:** BT-4533
**Issues:** BT-4534 (R1, foreign `^` keeps cleanup writes), BT-4532 (pre-store through ThreadedIr; check covers any block operand), BT-4535 (commit reads the cell; fixes BT-4459), BT-4536 (delete relay and stamp, one release after BT-4535), BT-4660 (remove the `nlr_state/2` alias, one release after BT-4536)
**Status:** Implemented (BT-4660 removed the `nlr_state/2` alias)

## References
- Issues: BT-4505, BT-4459, BT-4448, BT-4429, BT-4312, BT-4523, BT-4524, BT-4525, BT-4532, BT-3582
- ADRs: 0041, 0130, 0133 (BT-4430 amendment), 0137, 0142
- `docs/development/actor-state-cell.md`
