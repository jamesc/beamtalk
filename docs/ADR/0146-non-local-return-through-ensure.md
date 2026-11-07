# ADR 0146: A `^` Commits the Actor's Cell, Not a Copy

## Status
Proposed (2026-11-06). Tracking: BT-4505.

Builds on ADR 0041 (state-carrying `^`), ADR 0137 (the actor-state cell) and
ADR 0142 (where class methods run). Decides BT-4459. BT-4430, which came up
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
compares two state maps to decide ownership.

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
becomes unnecessary and is removed. The runtime keeps its two entry points for
one release so that code compiled by an older compiler still runs.

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

## Consequences

Ownership is decided in one place, by identity. BT-4459 is fixed by the general
rule rather than by another patch, and the BT-4429/BT-4448 relay code goes. The
compiler's obligation narrows to state that has no cell, which its verifier can
check.

The decision depends on the cell being current whenever a `^` starts. The
three bugs found during review show that this has to be enforced rather than
assumed. Their fixes added verifier checks, and new ways of compiling a `^`
need the same.

## Implementation

Change the actor-method commit to read the cell; keep a cleanup's writes when a
foreign `^` passes through it; remove the relay and stamp code with
compatibility shims for one release; extend the `ensure:` test corpus to cover
the REPL, a `^` from another method, and a `^` from another actor. The commit
change and the foreign-`^` change land together. If they land separately, the
cleanup's writes would survive only partly, depending on where the cleanup
makes a call. Work items are tracked under BT-4505; BT-4532 holds the remaining
cleanup from the BT-4524 fix.

## Migration Path

No source changes. The two behaviour changes described under User Impact go in
the release notes. Libraries compiled by an older compiler keep working for one
release and must be recompiled after that.

## References
- Issues: BT-4505, BT-4459, BT-4448, BT-4429, BT-4312, BT-4523, BT-4524, BT-4525, BT-4532, BT-3582
- ADRs: 0041, 0130, 0133 (BT-4430 amendment), 0137, 0142
- `docs/development/actor-state-cell.md`
