# ADR 0081: First-Class Session Object — Walkable Binding Layers

## Status
Proposed (2026-05-09)

## Context

### Problem

The Beamtalk REPL has two binding layers, merged into a single map at eval time:

| Layer | Owner | Source |
|-------|-------|--------|
| **Workspace globals** | `beamtalk_workspace_interface_primitives` (ETS, shared) | Singletons (`Transcript`, `Beamtalk`, `Workspace`) + `bind:as:` entries |
| **Session locals** | `beamtalk_repl_shell` gen_server (per session, owned by session PID) | `x := 42` typed in the shell |

The session-locals layer has **no object-level accessor**. The only way to view or clear it is through two REPL meta-commands:

| Meta-command | Purpose |
|--------------|---------|
| `:bindings` / `:b` | List all bindings (session locals + workspace globals merged) |
| `:clear` | Clear session locals |

The surface-only audit ([BT-2083](https://linear.app/beamtalk/issue/BT-2083)) un-deprecated `:bindings` and `:clear` because no object-side equivalent exists. ADR 0040 successfully moved most meta-commands onto `Workspace` and `Beamtalk` — but the session layer was left behind because *the session itself is not a first-class object*.

This violates the architectural pattern established by ADR 0040 (workspace-native REPL commands) and Principle 6 (everything is a message send). Users can introspect classes, actors, the workspace, even singletons — but cannot programmatically inspect or manipulate the binding layers that resolve their own variable names.

### Current State

Each REPL connection gets a `beamtalk_repl_shell` gen_server, supervised under `beamtalk_session_sup` and tracked in the `beamtalk_sessions` ETS table (`beamtalk_session_table`). The shell holds session-local bindings in `beamtalk_repl_state`, reconciled with workspace globals after each eval (`refresh_ws_bindings/1` in `beamtalk_repl_shell.erl:441`, called during eval-result processing to pick up `bind:as:`/`unbind:` changes). Eval workers spawn from the shell with a snapshot of state, returning updated state when they complete.

The CLI REPL has one session. WebSocket and MCP clients can have many — `beamtalk_session_sup` already supports multi-session, and `beamtalk_repl_ops_session.erl` exposes a `sessions` protocol op listing live shells. But no Beamtalk class represents an individual session.

### Constraints

- **Per-process state.** Session locals are owned by the shell PID; any object representation must respect that ownership boundary.
- **Eval ordering.** Eval workers run in spawned worker processes with a state snapshot. Mutations to session bindings during an eval interact with the shell's state-update flow.
- **Hot reload safety.** Session locals must persist across `Workspace sync` and class reloads (current behaviour).
- **Existing infrastructure.** `beamtalk_session_table`, `beamtalk_session_sup`, and the `sessions` protocol op already exist — the design should build on them rather than replace.
- **Surface parity.** `:bindings` / `:clear` work in the REPL surface only; the object-level replacement must be reachable from MCP `evaluate`, LSP eval, and any future client.
- **Call safety.** Session primitives call back to the shell gen_server via `gen_server:call`. This is safe from eval worker processes (the shell is in `noreply` state, processing its mailbox), but unsafe from `handle_call`/`handle_cast` handlers running directly on the shell process (would deadlock). Session methods are intended for eval context, not shell-internal dispatch.

## Decision

Introduce a new `Session` class used as a **factory**: two class methods return session *values*, and **all binding operations are instance methods on the returned value**.

- `Session current` returns the calling process's session (or `nil` outside a REPL eval — see below), mirroring `Date today` / `Smalltalk current`: the class method returns an instance you then message (`Date today year`, not `Date year`).
- `Session withId: aSessionId` returns another session by its protocol id, for cross-session tooling access.

There is **no class-side operation mirror** — no `Session bindings`, no `Session clear`. You write `Session current bindings`, `(Session withId: x) bindings`. This keeps `Session` a *normal class* (factory class-methods plus instance methods, like `Array new` then instance sends), rather than a bespoke singleton-with-sugar. The system's true singletons (`Transcript`, `Beamtalk`, `Workspace`) are reached through an injected binding that *is* the instance; `Session` cannot be one of those because it is **per-process, not per-node** (each connection has its own), so it earns factory access instead — but it does not earn a duplicate class-side API. (See "Why no class-side convenience mirror" under Tension Points.)

There is **no injected `Session` binding** at the shell level; the class is reachable because it is loaded, not because it was singled out.

`Session` is **not** an Actor (gen_server). It is a Beamtalk class whose methods delegate to `beamtalk_session_primitives`. `Session current` uses process-context resolution (process dictionary seeded during eval worker spawn) to find the calling session; instance methods carry their session's id for cross-session dispatch.

`aSession bindings` returns a **live binding view** of the session-locals layer — mutating it writes through to session state. `view at: #x put: 42` sets a session-local; `view removeKey: #x` removes it. The *globals* layer is **not** a Session method (globals are workspace-owned, not session state); it lives on `Workspace globals`, which this ADR upgrades to the same live `BindingsView` type — see "Two layers, two owners" below.

### API

```beamtalk
sealed typed Object subclass: Session

  // ─── Class-side: factory methods only ───

  /// The calling process's session as a value. Returns `nil` outside a
  /// REPL eval context (compiled program code has no session), matching
  /// `Workspace currentSession`.
  class current -> Session | Nil =>
    (Erlang beamtalk_session_primitives) current

  /// Look up a session by its protocol session ID. Returns `nil` if no
  /// such session exists or it is no longer alive. Used for cross-session
  /// access — see "Cross-session access (LSP, VS Code)" below.
  class withId: aSessionId :: String -> Session | Nil =>
    (Erlang beamtalk_session_primitives) withId: aSessionId

  // ─── Instance-side: all operations live here ───

  /// Live view of this session's local bindings (the `x := 42` layer).
  /// Reads return current values; `at:put:` and `removeKey:` mutate
  /// session state (write-through, for the calling session only).
  /// (There is no `globals` here — globals are workspace-owned;
  /// use `Workspace globals`.)
  bindings -> BindingsView =>
    (Erlang beamtalk_session_primitives) bindingsViewFor: self

  /// Resolve a name exactly the way bare-name lookup does, returning the
  /// first match or nil. Order: session locals → workspace globals
  /// (bind:as: + singletons) → class registry. Primarily a REPL debugging
  /// tool: answers "where does this name resolve from?" interactively.
  /// Uses the one shared resolver (see "Binding storage model"), so it can
  /// never disagree with how the name actually resolves.
  resolve: aName :: Symbol -> Object =>
    (Erlang beamtalk_session_primitives) resolveFor: self name: aName

  /// Clear this session's local bindings. Workspace globals remain.
  clear -> Nil =>
    (Erlang beamtalk_session_primitives) clearFor: self

  /// Stable session identifier (matches the protocol session ID).
  id -> String =>
    (Erlang beamtalk_session_primitives) idOf: self
```

The common idiom is `Session current bindings keys`; for another session, `(Session withId: "…") bindings keys`. Binding a session to a temp (`s := Session current`) works too, but note `s` then becomes a session-local and will appear in `s bindings keys` — so the inline form is cleaner for one-off inspection.

**Session instance representation.** `Session` is declared `Object subclass` (no `field:`/`state:` declarations, which an `Object` subclass cannot have), yet a `Session` *value* returned by `current` / `withId:` must carry its session's PID and protocol ID. This follows the existing `FileHandle` / `Port` pattern: a `sealed typed Object subclass` whose per-instance identity lives in the runtime representation (a tagged map produced by the primitive), not in a declared field. Instance methods pass `self` to a `*For:` primitive, which extracts the PID/ID from that representation — exactly as `FileHandle lines` passes `self` to `(Erlang beamtalk_file) handleLines: self`.

`BindingsView` is a small Dictionary-protocol-compatible class backed by primitives that read/write session or workspace state. It implements `at:`, `at:put:`, `removeKey:`, `includesKey:`, `keys`, `values`, `size`, and `do:`. Mutations via `at:put:` / `removeKey:` are write-through.

**Return-value contract.** `at:put:` returns the value put (standard Dictionary protocol), for both session and workspace views. `removeKey:` uniformly returns `nil` rather than the removed value: session-local removals are *enqueued* (see the eval-ordering note in Consequences) so the removed value is not read back synchronously, and returning it for one view but not the other would be a worse inconsistency — `nil` for both is the honest contract. Reads (`at:`, `keys`, `values`, `size`, `do:`) reflect committed state — within a single eval they do **not** observe mutations enqueued earlier in that same eval (see Consequences → Negative → eval ordering).

A `BindingsView` is a handle over live session/workspace state. Its **runtime representation** follows `FileHandle` / `Port` — a `sealed typed Object subclass` with no `field:` declarations, minted by a primitive as a tagged map carrying per-instance identity, with instance dispatch routed to a primitive module. Note the precedent is for the *representation only*, not the lifetime contract: `FileHandle` must **not** escape its `open:do:` block, whereas a `Session` value is explicitly meant to be stored and passed (pass-by-reference is the whole justification over Option B). Escape-safety for `Session` is handled not by the FileHandle "don't escape" rule but by the liveness check on every send (`session_not_found` if the backing shell has died — see "Stored Session / view lifetime" in Consequences). A view read against a dead session raises rather than returning stale data.

**No injected binding.** `Session` is a class name resolved through the class registry, like `Counter` or `Integer`. It cannot be shadowed by a user `:=` assignment because class names are not in the binding map; assigning `Session := foo` creates a local binding that shadows the class reference *only in that local scope*, the same way assigning `Counter := foo` works. This is normal Smalltalk behaviour and not a footgun specific to Session.

`Workspace currentSession` is **also** added to `WorkspaceInterface` (it does not exist today) as a navigation method — Workspace is a discovery entry point, and "find the current session" fits the pattern of the existing `Workspace actors`, `Workspace supervisor`, `Workspace globals`, etc. It returns the same value as `Session current` (the session value, or `nil` outside a REPL eval). Code that conditionally uses session state guards with `ifNotNil:` — `Session current ifNotNil: [:s | …]` — rather than a separate `hasSession` predicate; the nil-returning factory already carries that information.

### Two layers, two owners

Each layer is owned by exactly one object, and there is **one** accessor per layer — no duplicate surface:

| Layer | Owner | Read | Write |
|-------|-------|------|-------|
| Session locals (`x := 42`) | the session | `Session current bindings` (`BindingsView`) | `… bindings at:put:` / `removeKey:` (write-through, deferred — see eval ordering) |
| Workspace globals (singletons + `bind:as:`) | the workspace | `Workspace globals` (`BindingsView`) | `… globals at:put:` / `removeKey:`, **or** the intention-revealing `Workspace bind:as:` / `unbind:` (both route through the same conflict-checked path) |

`Session` deliberately has **no `globals` method** — globals are workspace state shared across every session, not session state. Putting a `globals` accessor on the session would duplicate `Workspace globals` and falsely imply globals are session-scoped (the same category error as the rejected class-side mirror). The one cross-layer operation, `Session current resolve:`, legitimately lives on the session because *shadowing is session-relative* (locals shadow globals); it reads `Workspace globals` internally.

**`Workspace globals` is upgraded from a `Dictionary` snapshot to a live `BindingsView`** (same type as `Session current bindings`), so both layers share one Dictionary protocol. This is a pre-1.0 breaking change to an interactive-only method; the audit found callers use only `at:` / `includesKey:` / `keys`, which `BindingsView` implements. `BindingsView printOn:` renders like a `Dictionary` so REPL output stays familiar. Writes route through `bind/2` / `unbind/1`, inheriting the protected-name conflict checks. (`Beamtalk globals` — the *class registry* snapshot — stays a `Dictionary`; it is not a mutable binding layer.)

**One documented asymmetry under the shared type:** session-local writes are *deferred to end of eval* (the eval worker holds a state snapshot — see eval ordering), while workspace-global writes hit shared ETS *synchronously* (other sessions must see them at once; they cannot be deferred into one session's eval). So within a single expression, `Session current bindings at: #x put: 9. x` sees the **old** `x`, whereas `Workspace globals at: #G put: 9. G` sees the **new** `G`. This reflects genuinely different layer semantics — *your* locals settle at expression end; the *shared* namespace updates immediately — not a quirk of the API spelling.

### Binding storage model — locals-only with lazy global resolution

Today (BT-883) each shell **eagerly copies** workspace globals (the `Transcript`/`Beamtalk`/`Workspace` singletons plus every `bind:as:` entry) into its per-session binding map at init (`inject_workspace_bindings/1`), re-syncs them after every eval (`refresh_ws_bindings/1`), and tracks which keys came from the copy (`injected_ws_keys`). This ADR replaces eager injection with **lazy resolution**, because the two-layer model the `Session` object exposes is far cleaner if the layers are actually stored separately rather than merged-then-unmerged:

- A session's binding map holds **only session locals** (`x := 42`).
- A free identifier `N` not bound locally is resolved against the **live** workspace sources, in order: (1) the `bind:as:` registry (workspace bindings ETS), (2) the singleton registry (`beamtalk_workspace_config:singletons/0` + `value_singletons/0` → `Transcript`/`Beamtalk`/`Workspace`), (3) the class registry (`Counter`, `Integer`, `Session` — already the case today), else `undefined_variable`.
- Locals are checked first, so **shadowing is preserved** (`Transcript := 5` shadows the singleton within that session).
- **One shared resolution function** backs both bare-name resolution and `Session current resolve:`, so they cannot drift.

This is behaviour-preserving: the REPL compiler runs in-workspace and reads the same registries the injected snapshot was copied from, at the same point, so a `bind:as:` from a prior eval is visible on the next expression exactly as before (next-eval visibility either way). What changes is that the copy — and its reconciliation — disappear.

Why it is worth doing here (not just later):
- **Smaller, honest session state.** Each session map is true locals; the global set is not duplicated N times across connections.
- **Removes machinery.** `inject_workspace_bindings/1`, `refresh_ws_bindings/1`, and `injected_ws_keys` all go away, along with the post-eval reconciliation they force.
- **Simplifies this ADR.** `Session current bindings` becomes *the whole session map* — no `map − injected_ws_keys` subtraction, no extra `get_session_locals` gen_server call. Cross-session read is just the target shell's existing `get_bindings`. `Workspace globals` reads the same registries the resolver uses. The bindings/globals split *falls out of* the storage model (locals on the session, globals in the registries) instead of being reconstructed by filtering.
- **More correct cross-session.** A `bind:as:` in one session is visible to others on their next resolution (read live from ETS) instead of waiting for each session's own `refresh`.

Sequencing: this is foundational and lands **before** the binding-view phases, so the throwaway subtraction is never built. Design parity to verify (see Phase 1): resolution order matches `resolve:`; `undefined_variable` fires at the same phase as today; ETS/registry lookups stay O(1) (interactive path, not hot).

### Cross-session access (LSP, VS Code)

The VS Code extension and LSP open their own protocol sessions for completion queries, but need to read bindings from the *user's* REPL session. This is currently handled at the protocol layer (BT-1045): the `bindings` op carries a `session` parameter and `beamtalk_session_table:resolve_pid/2` looks up the target shell.

The new Session API supports this cleanly:

```beamtalk
// On the LSP completion session:
userSession := Session withId: "user-repl-abc-123"  // returns Session | Nil
userSession ifNotNil: [
  candidates := userSession bindings keys
]

// Discover session ids without knowing them out-of-band (Phase 7):
Workspace sessions collect: [:s | s id]
```

`Session current bindings` operates on the calling process's session — semantically "my session". `(Session withId: x) bindings` operates on whichever session the id names, including ones owned by other connections. Both are the *same* instance methods on a `Session` value; only the factory call differs (`current` vs `withId:`). This is the load-bearing reason `Session` is value-shaped at all: cross-session **introspection** from tooling.

Cross-session access concerns the **session-locals** layer only (globals are shared — any session reads them via `Workspace globals`). It is **read-only**: the reads (`bindings keys`, `bindings at:`, `resolve:`, `id`) work against any live session; the mutators on another session's `bindings` view (`at:put:`, `removeKey:`) and `clear` raise `cross_session_mutation_unsupported` (see Consequences → Negative → "Cross-session writes"). The LSP/completion use case needs only reads, and cross-session writes would race the target session's in-flight eval. `withId:` itself performs a liveness check and returns `nil` for an unknown *or* dead session id; a previously-captured `Session` whose shell later dies raises `session_not_found` on its next send.

The LSP and any other client that previously called the `bindings` / `clear` protocol ops moves to `Session withId:` + instance methods in the same release.

### REPL Examples

```text
beamtalk> x := 42
42
beamtalk> Session current bindings keys
#(#x)
beamtalk> Session current bindings at: #x
42
beamtalk> Workspace globals keys
#(#Transcript, #Beamtalk, #Workspace)
beamtalk> Session current resolve: #x
42
beamtalk> Session current resolve: #Transcript
#<TranscriptStream>
beamtalk> Session current resolve: #notDefined
nil

// Live mutations write through:
beamtalk> Session current bindings at: #y put: 99
99
beamtalk> y
99
beamtalk> Session current bindings removeKey: #x
nil
beamtalk> x
// => #beamtalk_error{kind: undefined_variable, name: #x}

// Workspace globals is the same live BindingsView; writes route through bind:as:.
// at:put: returns the value put (Dictionary protocol), like the session view above.
beamtalk> Workspace globals at: #MyTool put: someActor
#<Counter @ <0.123.0>>
beamtalk> MyTool
#<Counter @ <0.123.0>>
beamtalk> Workspace globals removeKey: #MyTool
nil

// Pass-by-reference (cross-session access from LSP / tooling):
beamtalk> userSession := Session withId: "user-repl-abc-123"
#<Session: "user-repl-abc-123">
beamtalk> userSession bindings keys
#(#x, #y, #counter)

beamtalk> Workspace currentSession id
"abc123-..."
beamtalk> Session current clear
nil
beamtalk> Session current bindings keys
#()
```

### Error Examples

Outside a REPL eval, `Session current` and `Workspace currentSession` return `nil`:

```beamtalk
// In a .bt file run via `beamtalk run`:
Session current        // => nil
Workspace currentSession  // => nil
Session current isNil  // => true
```

Outside a REPL, `Session current` is `nil`, so messaging it DNUs on `nil`:

```beamtalk
Session current bindings
// `Session current` => nil, then:
// => #beamtalk_error{kind: does_not_understand, selector: #bindings, receiver: nil}
```

This is the *intended* trade for keeping `current` consistent with `Workspace currentSession` (both return `nil` with no session). It is acceptable because `Session` is an interactive/tooling surface, not something production library code depends on — and code that wants to guard uses `ifNotNil:` (no separate predicate):

```beamtalk
Session current ifNotNil: [:s | s clear]
```

Cross-session `withId:` for an unknown (or dead) session ID returns nil:

```beamtalk
Session withId: "nonexistent"  // => nil
```

Globals mutations that violate protected-name rules raise the existing `bind:as:` error:

```beamtalk
Workspace globals at: #Workspace put: nil
// => #beamtalk_error{kind: name_conflict, selector: #'bind:as:',
//    message: "Workspace is a system name and cannot be shadowed"}
```

Cross-session **writes** are rejected (reads are allowed — see "Cross-session access"):

```beamtalk
otherSession := Session withId: "user-repl-abc-123".
otherSession bindings at: #x put: 99
// => #beamtalk_error{kind: cross_session_mutation_unsupported,
//    message: "cannot mutate bindings of another session; reads only"}
```

A `Session` value whose shell has since died raises on the next send (rather than
blocking on a call to a dead PID):

```beamtalk
// captured earlier; the owning REPL tab has since closed
staleSession bindings keys
// => #beamtalk_error{kind: session_not_found, message: "session is no longer alive"}
```

### Open Design Questions — Resolved

| Question | Decision | Rationale |
|----------|----------|-----------|
| Singleton or per-PID? | **Per-PID** | CLI has one session, WebSocket/MCP clients can have many. `Session current` uses process-context resolution to find the calling session; instance values carry their own session ID for cross-session access. |
| Class-side operation mirror (`Session bindings`)? | **No.** Factory class-methods only (`current`, `withId:`); all operations are instance methods. | A class-side mirror would make `Session` a bespoke pattern unlike every other class (the true singletons use an injected binding; Session can't be one). It buys only terseness and a marginally nicer no-session error — not worth a duplicated surface. See Tension Points. |
| What outside REPL eval? | **`Session current` returns `nil`** (matching `Workspace currentSession`); `Session current bindings` then DNUs on `nil`. | Compiled program code has no session. Returning `nil` keeps `current` consistent with `Workspace currentSession`; the DNU-on-nil is acceptable because `Session` is an interactive/tooling surface, not a production dependency. Guard with `Session current ifNotNil: [:s | …]` — no `hasSession` predicate. |
| Persist across `:sync` / reload? | **Yes** — unchanged from current behaviour. | Session locals are shell-process state, owned independently of class definitions. Sync rebuilds modules; the session keeps its bindings. |
| Are the binding views live or snapshots? | **Live views.** Mutations write through. | Matches Pharo semantics (`Smalltalk globals at:put:`). A snapshot would force a parallel setter API; the live view collapses read and write into one Dictionary protocol. |
| Where do globals live — on Session or Workspace? | **Workspace.** `Session current bindings` is locals only; globals are `Workspace globals`. `Session` has no `globals` method. | Globals are shared workspace state, not session state. A `Session globals` accessor would duplicate `Workspace globals` and imply globals are session-scoped (same category error as the class-side mirror). `resolve:` still walks both, reading `Workspace globals` internally. |
| `Workspace globals` return type? | **Upgraded `Dictionary` → live `BindingsView`** (pre-1.0 break). | Unifies both layers under one type; makes `Workspace globals at:put:` a discoverable write path. Interactive-only; audited callers use only `at:`/`includesKey:`/`keys`. |
| Layer-walking abstraction? | **Dropped.** No `layers` method, no recursive `parent`. | Beamtalk has settled on exactly two layers; package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer. A `layers` constant would be pure decoration. |
| `bind:as:` interaction? | **Unchanged.** `Workspace globals at:put:` is equivalent to `Workspace bind:as:`, including conflict checks. | One write path through the existing primitives; the view is sugar over the established API. |
| Cross-session access? | **Instance methods on a value from `Session withId:`, read-only** | LSP/VS Code completion sessions need to query the user's session. `Session current` is "my session"; `Session withId:` is "that session"; both return values you message identically. Cross-session *writes* race the target's in-flight eval and are unneeded, so they raise `cross_session_mutation_unsupported`. |
| Stale / dead session reference? | **`withId:` does a liveness check (returns `nil`); a captured value whose shell later dies raises `session_not_found`.** | Avoids a `gen_server:call` to a dead PID blocking for the full timeout. |

## Prior Art

| System | Binding model | What we adopt | What we reject |
|--------|---------------|---------------|----------------|
| **Pharo `Smalltalk current` / `Date today` / `Time now`** | Class-side *factory* methods that return an instance — you then message the instance (`Date today year`, not `Date year`). | The factory-then-instance pattern exactly: `Session current` / `Session withId:` return a session value you message. We deliberately do **not** add a class-side operation mirror — `Date` itself doesn't (`Date today daysInMonth`, never `Date daysInMonth`). | A class-side operation mirror — it would be unlike `Date`/`Time` and every other class. |
| **Pharo / Squeak Workspace** | `Dictionary` of bindings on the workspace, mutable via `at:put:`, with fallback to `Smalltalk globals`. Methods: `bindings`, `bindingOf:`, `removeBinding:`, `resetBindings`. | The mutable-Dictionary shape (`aSession bindings at:put:` writes through, matching `Smalltalk globals at:put:`). | Pharo merges workspace and session — Beamtalk has separate workspace (shared) and session (per-PID) layers because of multi-client BEAM hosting. |
| **Pharo Environment** | Wraps a `Dictionary` of globals; multiple Environments allowed. | Layer reification via named accessors. | Beamtalk does not need user-creatable environments — workspace is bootstrapped once per BEAM node. |
| **Newspeak** | Lexical nesting only; no global namespace; capability passing via constructor parameter. | The principle that scope is explicit, not magic. | Beamtalk has Smalltalk-style globals (`Transcript`, classes); a fully-lexical model would invalidate the existing namespace design. |
| **Self** | Parent slot chain; scope walks via `parent*` slots. | Explicit layer separation by owner (`Session current bindings` vs `Workspace globals`), inspired by Self's transparent parent hierarchy. | Recursive `parent` chains overstate the complexity for two layers. |
| **IPython `get_ipython()`** | Shell singleton with `user_ns` dict, `who_ls()`, `reset()`. | The "current session is reachable through a global accessor" pattern (`Workspace currentSession`). | A single mutable dict obscures layer ownership; we keep layers separate. |
| **Ruby `Binding`** | First-class scope object. `binding` returns the current scope; `Binding#local_variables` lists names. Can be passed for scoped expression execution. Closest prior art to the `Session` design. | A scope object that can be inspected and passed around. | Ruby's Binding captures lexical scope (closures); Beamtalk's Session wraps a session process, not a lexical frame. |
| **Erlang shell `b()` / `f()`** | `b()` lists shell bindings; `f()` clears all; `f(X)` clears one. Shell-only built-in commands, not callable from program code. | Demonstrates the universal need to inspect REPL state. | Shell commands, not values — cannot be composed, stored, or called from non-shell code. Exactly the problem this ADR solves. |
| **Elixir `binding()`** | Macro returning current bindings as a keyword list. | Treating bindings as inspectable data. | A macro/keyword-list shape is non-Smalltalk; we use Dictionary returns and explicit objects. |

## User Impact

**Newcomer (Python/JS background).**
`Session` is discoverable via tab-completion on the class name — type `Ses<TAB>` and `current` / `withId:` appear; `Session current <TAB>` then surfaces the operations. Discovering "how do I see my variables" via `Session current bindings` is more direct than memorising a `:b` meta-command. The live-mutation pattern (`Session current bindings at: #x put: 42`) matches how Dictionary works everywhere else in the language.

**Smalltalk developer.**
The session-as-object model is recognisably Smalltalk: `Session` is to a Beamtalk shell what `Smalltalk` is to a Pharo image. The two-layer split (session vs. workspace) is more honest about multi-client BEAM hosting than Pharo's single-image model.

**Erlang/BEAM developer.**
Class-side methods that use process-context resolution match how BEAM convention works — the "calling process determines context" pattern is idiomatic (e.g. `self()`, process dictionary, `$ancestors`). `Session withId:` for cross-session access maps directly to the existing `beamtalk_session_table:lookup/1`. Multi-session debugging (MCP, WebSocket) gets a first-class object to work with.

**Production operator.**
`Workspace sessions` (Phase 7) returns a list of `Session` objects, each individually inspectable. Compared to opaque PIDs in `supervisor:which_children`, this gives operators a structured view of who is connected and what they have bound.

**Tooling developer (LSP / VS Code).**
The MCP server can call `Session current bindings` and `Workspace globals` via `evaluate` to display per-session state in the inspector panel. Tooling that needs the merged binding map (e.g. completions) merges the two layers in resolution order — no separate primitive needed. Session ids for cross-session reads come from `Workspace sessions collect: [:s | s id]`.

## Steelman Analysis

### Option B: Workspace Extension (rejected)

**Newcomer:** "I already know `Workspace` from `Workspace classes` and `Workspace actors`; adding `Workspace sessionBindings` is one more method on a familiar object. No new concept to learn, no new top-level binding to discover."

**Smalltalk purist:** "Adding methods to `Workspace` matches how `Smalltalk` accumulates introspection in Pharo — `Smalltalk globals`, `Smalltalk allClasses`, `Smalltalk current`. Why is sessions special enough to warrant its own class?"

**BEAM veteran:** "`Workspace sessionBindings` could use the same process-context resolution that `Session current bindings` uses anyway. The dispatch cost is identical; you're just spelling it differently. For the 99% case — introspecting your own session — adding methods to an existing object is simpler than introducing a whole new class. No new Erlang module, no new class registry entry."

**Operator:** "Fewer object types in the system means fewer surfaces to learn for postmortem debugging. `Workspace` is already the operator's entry point — keep it that way."

**Language designer:** "Adding methods is cheaper than adding classes. The composability argument for first-class Session is partly speculative — today nobody writes `Workspace sessions collect: [:s | s bindings]`. YAGNI."

**Why not adopted:** Option B *works* for the common case and is genuinely simpler. Two factors push us to a dedicated `Session` class despite this:

1. **Pass-by-reference.** `Session` as a value can be stored, passed to library code, or used for cross-session access — `Session withId:` for LSP completion against the user's session, `Workspace sessions collect: [:s | s id]` for multi-session enumeration. Option B cannot express any of these without inventing per-call session-id parameters.
2. **Workspace's role.** `Workspace` is a navigation entry point — `Workspace actors`, `Workspace classes`, `Workspace supervisor`, `Workspace dependencies` — each method *finds you something*. Session bindings are not something you navigate to from the workspace; they belong to a session, which has its own identity. Loading four session-state methods (`sessionBindings`, `sessionGlobals`, `clearSession`, `resolveBinding:`) directly onto `Workspace` would be the same category error as putting `Counter increment` on `Workspace`. A dedicated `Session` class keeps the surface focused.

The chosen design gives us almost all of B's simplicity (no injection, no shadowing, no `injected_ws_keys` plumbing) plus pass-by-reference for the cross-session and multi-session cases that B cannot express — while keeping the surface minimal by exposing only factory class-methods (`current`, `withId:`), not a class-side operation mirror.

### Option C: Recursive Scope Chain (rejected)

**Newcomer:** "Once I learn the parent pattern, I can walk any depth. It's just like prototype chains in JavaScript or `__getattr__` chains in Python — a familiar mental model from outside the Smalltalk world."

**Smalltalk purist:** "Self's slot-chain model is the most principled scoping mechanism in any object language. Walking `parent` is more uniform than special-casing each layer with a named accessor (`bindings`, `globals`, ...). Adopt proven prior art."

**BEAM veteran:** "Process linking already gives BEAM a parent chain (`$ancestors`). A uniform `parent` accessor on Scope objects mirrors what's already happening at the OTP level — recursive walks are idiomatic on BEAM."

**Operator:** "If we ever introduce per-package, per-module, or per-actor scopes, depth-flexible scopes absorb them without redesign. Today's two-layer model becomes tomorrow's five-layer model without breaking callers."

**Language designer:** "Most elegant. Two layers IS the special case; the general case is a chain. Designing for the general case prevents future entrenchment of the wrong abstraction."

**Why not adopted:** Beamtalk has explicitly settled on a two-layer binding model — session locals over workspace globals — with no plans for additional layers. Package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer; module scope is an Erlang concept invisible to Beamtalk users (per project memory). The general-case argument requires a third layer to ever materialise; given the design direction, that's a bet against settled decisions. The cost — a `Scope` class hierarchy, recursive resolution semantics, tooling for chain inspection, plus the foreign vocabulary of "scope" in a Smalltalk context — is paid for an extension we are choosing not to take.

### Tension Points

- **Why no class-side convenience mirror.** It is tempting to add class-side sugar (`Session bindings` ≡ `Session current bindings`) so the common case is one word shorter. We reject it deliberately:
  - **It makes `Session` a bespoke pattern.** No other class has it. The true singletons (`Transcript`, `Beamtalk`, `Workspace`) are an *injected binding that is the instance* — `Transcript show:` is a plain instance send, with no class-side `TranscriptStream show:` mirror. Factory classes like `Date` return an instance you message (`Date today year`, never `Date year`). A class-side operation mirror on `Session` would match neither pattern.
  - **It buys little.** Only terseness (one word) and a slightly nicer no-session error (`Session bindings` could raise a structured `no_session`, vs `Session current bindings` DNUing on `nil`). Neither justifies a duplicated surface or the internal inconsistency it creates (`current` returns `nil` but `bindings` would raise, for the *same* condition).
  - **`Session` genuinely can't be an injected singleton** (it's per-process, not per-node), so it earns *factory* access (`current`/`withId:`) — but that is the whole of its specialness. The operations belong on the value.
- **Returning `nil` outside a REPL, not raising.** `Session current` returns `nil` to stay consistent with `Workspace currentSession`. The cost is that `Session current bindings` DNUs on `nil` outside a REPL rather than giving a domain-specific error — acceptable because `Session` is an interactive/tooling surface, and library code guards with `Session current ifNotNil: [:s | …]` (no `hasSession` predicate).
- **The LSP cross-session case is real, not speculative.** BT-1045 already established the protocol-level cross-session pattern; the LSP runs its own completion session and queries the user's session for bindings. Option B has no Beamtalk-native expression of this; `Session withId:` does.
- **Live mutation cost.** Pharo-style `Smalltalk globals at:put:` write-through is genuinely useful, but adds a `pending_mutations` queue to `beamtalk_repl_state` to handle eval-ordering. This complexity is a property of the live-view decision, independent of the entry-point shape.
- **Two layers is settled.** Option C's strongest argument (future layer flexibility) requires bet-against-settled-design. Beamtalk has committed to exactly two binding layers; package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer.

## Alternatives Considered

### Alternative A: Per-session injected `Session` binding

The original sketch in BT-2092 — inject `Session` as a per-shell binding (alongside `Transcript`, `Beamtalk`, `Workspace`) so `Session bindings` resolves to an instance method on the injected value.

Rejected because:
- **Less Smalltalk than the factory approach.** Pharo's pattern for "the current X" is a class-side factory (`Date today`, `Time now`, `Smalltalk current`) returning an instance, not a top-level singleton binding. Sessions are per-PID, not singular — injecting them as if they were singletons is dishonest.
- **Shadowing footgun.** `Session := MyThing new` in the shell silently overwrites the injected binding. The existing `bind:as:` conflict checks don't catch shell-level `:=`. Mitigating this requires a new compiler warning, which is cost we don't pay if there is no injected binding.
- **Implementation complexity.** Requires extending `injected_ws_keys`, modifying `inject_workspace_bindings`, adding `Session` to `is_protected_name/1`, and a clear-and-reinject loop. The factory approach skips all of this.
- **Tooling parity is messier.** Cross-session access (LSP completion against the user's session) doesn't fit naturally — the LSP's session sees its own injected `Session`, not the user's. Factory methods `Session current` / `Session withId:` separate "my session" from "that session" cleanly.

### Alternative B: Workspace extension methods (no new class)

Add `Workspace sessionBindings`, `Workspace clearSession`, `Workspace resolveBinding:` directly to `WorkspaceInterface`. No new class, no new top-level binding.

Rejected because:
- Cannot represent "the session" as a value to pass, store, or compose.
- Cannot answer "which session?" cleanly when multiple sessions exist (`Workspace sessions` lists them, but they cannot expose their own state).
- Method names grow long and clunky as session-related operations accumulate.

### Alternative C: Recursive `Scope` chain with `parent` links

Introduce a `Scope` abstraction with a `parent` accessor, walked recursively for resolution. `Workspace currentScope` returns the session scope; `currentScope parent` returns the workspace scope.

Rejected because:
- Two layers do not justify a recursive abstraction.
- "Scope" is a compiler concept, not a Smalltalk concept; introduces foreign vocabulary.
- Premature: `Session` can grow a `parent` accessor later non-breakingly if a third layer appears.

### Alternative D: Make `:bindings` and `:clear` permanent meta-commands

Accept the surface asymmetry permanently and document `bindings` / `clear` as REPL-specific.

Rejected because it locks in the violation of Principle 6 (messages all the way down) and ADR 0040 (workspace-native commands), and leaves the session layer permanently invisible to MCP and other clients.

## Consequences

### Positive

- **Layers become inspectable, with clear ownership.** `Session current bindings` (locals) and `Workspace globals` (globals) make the two-layer model visible — and each layer has exactly one accessor on its owner; users no longer wonder why `Transcript` shows up in `:bindings` but is not something they assigned.
- **Smaller, honest session state.** Lazy global resolution stops eagerly copying the global set into every session's binding map. Each session map is true locals; `inject_workspace_bindings/1`, `refresh_ws_bindings/1`, and `injected_ws_keys` (plus their post-eval reconciliation) are removed. Cross-session reads need no special subtraction — the target shell's binding map *is* its locals.
- **Resolution becomes debuggable.** `Session current resolve: #x` answers "where does this name come from?" interactively, and shares one resolution function with bare-name lookup so they cannot drift.
- **`:bindings` and `:clear` get a Beamtalk-native replacement.** Surface parity restored: MCP `evaluate` and any other client can drive session inspection through the standard message-send surface.
- **Multi-session aware.** The design works unchanged for CLI (one session), WebSocket, MCP, and any future multi-client surface, and `Workspace sessions` (Phase 7) makes the live session set first-class.
- **Foundation for future ops.** Per-session metadata (connect time, client kind, idle status) and session-to-session messaging become straightforward extensions on the `Session` value.

### Negative

- **New `BindingsView` class.** A small Dictionary-protocol class is required to support live read/write views. Implementation cost is moderate (one class plus primitives for read/write/iterate). The class is simple — every method delegates to a primitive — but it is a new public surface that has to be documented and tested.
- **Session-local mutation during eval has ordering constraints.** Both `Session current clear` and `Session current bindings at:put:` mutate shell state. The eval worker holds a *state snapshot*; on completion its returned state is merged back, so a naive direct write would be overwritten. The implementation adds a `pending_mutations` queue to `beamtalk_repl_state`, modelled on the existing `pending_module_removals` mechanism (`beamtalk_repl_shell.erl` `apply_pending_removals/2` + `drain_pending_removals/1`). Primitives enqueue `{op, key, value}` tuples rather than writing directly; the queue is drained in the eval-result handler. The exact merge order and the behaviour on every eval exit path are specified in Phase 2 — they are correctness-critical and must not be left implicit. (This applies to **session-local** writes only; workspace-global writes go straight to shared ETS — see next bullet.)
- **Reads do not observe same-eval session-local writes (read-your-own-writes lag).** Because session-local mutations are *enqueued* and drained only at eval-result time, such a mutation does not take effect until the *next* eval:
  - `Session current bindings at: #x put: 99. x + 1` evaluates `x + 1` against the **original** `x`.
  - `Session current bindings at: #x put: 99. Session current bindings at: #x` returns the **original** value of `x`, not `99`.
  This matches the expectation that an expression sees its own pre-state. **Workspace-global writes differ**: `Workspace globals at: #G put: 9. G` *does* see `9` in the same eval, because globals hit shared ETS synchronously (other sessions must see them at once). Same `BindingsView` type, different timing — documented under "Two layers, two owners". To thread a freshly-computed session-local value into the same expression, use a local (`y := 99. Session current bindings at: #x put: y. y`) instead of reading back through the view.
- **Cross-session writes are not supported.** A cross-session `bindings at:put:` / `removeKey:` / `clear` (against a session from `Session withId:`) would mutate that shell's state while its own in-flight eval worker holds an older snapshot — the worker's writeback would silently clobber it, and the `pending_mutations` queue (which lives in the *calling* eval's flow) does not help the target. Because the only concrete cross-session use case — LSP/VS Code completion — needs **reads only**, those mutators raise `#beamtalk_error{kind: cross_session_mutation_unsupported}` rather than racing. Cross-session **reads** (`bindings keys`, `bindings at:`, `resolve:`, `id`) are fully supported. (Globals are shared, so there is no "cross-session globals" — every session sees the same `Workspace globals`.) This keeps the win over Option B honest: the load-bearing gain is cross-session *introspection*, not mutation.
- **Stored `Session` / view lifetime.** A `Session` value (or a `BindingsView`) can outlive the shell it refers to — e.g. the user closes their REPL tab after an LSP session captured `Session withId: …`. `withId:` performs a liveness check (`resolve_pid`-style `is_process_alive`, not a raw `lookup`) and returns `nil` for a session that is gone. A `Session` value captured earlier whose shell *subsequently* dies raises `#beamtalk_error{kind: session_not_found}` on the next message send rather than blocking on a `gen_server:call` to a dead PID until timeout. Views are transient handles (like `FileHandle`); they are not durable references.
- **Outside-REPL behaviour.** `Session current` returns `nil`, so `Session current bindings` DNUs on `nil` outside a REPL eval. There is no class-side `no_session` error because there are no class-side operation methods — `current` returning `nil` (consistent with `Workspace currentSession`) is the single source of "no session here". Library code that wants graceful degradation uses `Session current ifNotNil: [:s | …]`.
- **`clear` is asymmetric with the view.** `aSession bindings removeKey: #x` removes one binding; `aSession clear` removes all. There is no `bindings clear` on the view because its protocol is generic Dictionary semantics — `bindings` returns a *view*, not a Dictionary the user owns, so a `clear` on the view would be ambiguous (clear-the-view or clear-the-state). Keeping `clear` as a method on the session avoids this ambiguity.

### Neutral

- **`bind:as:` write semantics unchanged.** `Workspace bind:as:` / `unbind:` continue to write the shared workspace ETS layer; `Workspace globals at:put:` / `removeKey:` are sugar over the same path. `Session current bindings` (locals) never includes `bind:as:` entries — those are globals, read via `Workspace globals`. (Under lazy resolution they are no longer copied into the session map at all.)
- **Session persistence across sync unchanged.** Existing behaviour preserved: session locals survive `Workspace sync`.
- **Protocol `bindings` / `clear` ops are removed** in Phase 6, alongside the rest of the work. The Session API ships first (Phases 1–5), so when the meta-commands disappear the replacement is already in place. There is no deprecation window — single coordinated release.

## Implementation

The implementation breaks down into a Phase 0 wire-check plus seven shippable phases.

### Phase 0: Wire-check / napkin (XS)

Before building any of the surface, prove the single load-bearing assumption end-to-end: *a primitive running inside an eval worker can recover the calling session*. Seed the worker's process dictionary with `beamtalk_session_pid` / `beamtalk_session_id` at spawn, add a throwaway `beamtalk_session_primitives:current/0` that reads it back, and confirm `(Erlang beamtalk_session_primitives) current` returns the right session from a live eval — and `nil` from a non-eval context. One EUnit test plus one manual REPL round-trip. If this does not work cleanly (e.g. process-dictionary seeding interacts badly with the worker spawn or the streaming path), the rest of the design is re-examined before investing in `BindingsView` and `pending_mutations`.

### Phase 1: Locals-only binding storage + lazy global resolution (M–L)

Replace eager global injection with lazy resolution (see "Binding storage model" in the Decision). This is the foundational, **highest-risk** phase — it touches name resolution — which is why it lands first: every later phase assumes a locals-only session map, so building it first avoids a throwaway `bindings − injected` subtraction.

- Stop calling `inject_workspace_bindings/1` at shell init; a new session's binding map starts **empty** and accumulates only locals as the user assigns.
- Remove `refresh_ws_bindings/1` from the eval-result handler and delete the `injected_ws_keys` field (and `get_injected_ws_keys/1` / `set_injected_ws_keys/2`) from `beamtalk_repl_state` — there is no injected copy to reconcile.
- Add one shared resolver, `beamtalk_workspace:resolve_name/2` (locals-map, Name), used by **both** REPL name resolution and `Session resolve:`. Order: (1) locals map, (2) `bind:as:` ETS, (3) singleton registry (`beamtalk_workspace_config:singletons/0` + `value_singletons/0`), (4) class registry, else `undefined_variable`.
- Point the REPL compiler's free-identifier resolution at `resolve_name/2` (it already runs in-workspace and can read the registries directly) instead of consulting a pre-injected binding map.

Parity tests (this phase must not regress behaviour): shadowing (`Transcript := 5` then `Transcript` returns `5`); `bind:as:` visibility on the next eval; `undefined_variable` fires at the same phase as today; and `resolve_name/2`'s order matches `Session resolve:` exactly (one function, two callers, so they cannot drift).

### Phase 2: Process-context plumbing + pending-mutation merge (M)

**Seed both eval spawn paths.** Modify the worker spawn in `beamtalk_repl_shell` to seed the worker's process dictionary with `beamtalk_session_pid` (the shell's `self()`) and `beamtalk_session_id` (the protocol session ID) before calling `do_eval`. There are **two** spawn paths and both must be seeded:
- `handle_call({eval, _}, ...)` and `handle_call({eval_trace, _}, ...)` — the synchronous REPL path.
- `handle_cast({eval_async, _, Subscriber}, ...)` — the streaming path (`do_eval/3`).

A primitive that finds `get(beamtalk_session_pid) =:= undefined` returns `nil` (`current` / `Workspace currentSession`); seeding only one path would make `Session` behave inconsistently between normal and streaming eval.

**Add the `pending_mutations` queue.** Add `pending_mutations` to `beamtalk_repl_state` — a list of `{op, key, value}` tuples (`op ∈ {put, remove, clear}`) — with accessors mirroring the existing `pending_module_removals` field. Primitives **enqueue** rather than write directly. The queue is owned by the shell's `ShellState` (not the worker snapshot): primitives enqueue via a `gen_server:call` to the shell, which is safe from the worker because the shell is in `noreply` while the worker runs and is free to service its mailbox. Visibility is guaranteed by ordering: the worker's enqueue `gen_server:call` is synchronous and completes *before* the worker sends `{eval_result, …}`, so the shell processes every enqueue (updating its loop state) strictly before it processes the result message — the `ShellState` bound in the `eval_result` handler always reflects the enqueued mutations.

(No `get_session_locals` call is needed: after Phase 1 a session's binding map *is* its locals, so a cross-session read is just the target shell's existing `get_bindings` — no subtraction, no companion call.)

**Specify the merge order on every eval exit path.** After Phase 1 the eval-result handler no longer calls `refresh_ws_bindings/1` (globals are resolved live, not injected). The pending-mutation drain slots in as follows:

| Eval exit path | Handler | Action |
|----------------|---------|--------|
| Success | `handle_info({eval_result, _, {ok, ...}}, ...)` | `M1 = apply_pending_removals(ShellState, WorkerState)`; `apply_pending_mutations(ShellState, M1)`. Mutations apply **on top of** the worker's returned bindings (so the worker's own `x := …` is visible) and **after** removals. (No `refresh_ws_bindings` — removed in Phase 1.) |
| Error | `handle_info({eval_result, _, {error, ...}}, ...)` | `apply_pending_removals` + `apply_pending_mutations` for `put`/`remove` ops only. `put`/`remove` are explicit per-key edits the user issued, independent of the failed expression, so they take effect. A queued **`clear`** (whole-session destruction) is **dropped** on the error path: `Session current clear. someTypo` should not wipe every local because the line after `clear` failed. `clear` applies only on the success path. |
| Interrupt | `handle_call(interrupt, ...)` | Drain `pending_mutations` alongside the existing `drain_pending_removals/1` so an interrupted eval's issued mutations are not lost. |
| Worker crash | `handle_info({'DOWN', ...}, ...)` | **Discard** `pending_mutations` — a crashed worker's partial mutations are not trustworthy; drain removals as today but drop the mutation queue. |

`apply_pending_mutations/2` reads the queue from `ShellState` and folds it over the locals map in enqueue order (`put`/`remove` per key, `clear` empties the locals map — no injected-key caveat now that the map is locals only), then resets the queue to `[]`.

EUnit tests: worker-spawn seeding on **both** paths; `apply_pending_mutations` fold order; the four exit-path behaviours above (success/error apply, interrupt applies, crash discards); and a `clear`-then-`put` ordering case.

### Phase 3: Runtime primitives module (M)

Create `beamtalk_session_primitives.erl` in `runtime/apps/beamtalk_workspace/src/`. Two **factory** primitives mint Session values; the rest take a Session value (`self`) and act on the session it names:

Factory (class-side):
- `current/0` — returns a Session value carrying the calling session's PID and ID (read from the process dictionary), or `nil` outside an eval context.
- `withId/1` — looks up a session by protocol ID and **checks liveness** (`is_process_alive`, the `resolve_pid` discipline — not a raw `beamtalk_session_table:lookup/1`, which can return a dead PID). Returns a Session value or `nil`.

Operations (instance-side, all take a Session value):
- `bindingsViewFor/1` — returns a `BindingsView` over the target session's locals map. After Phase 1 that map *is* the locals (no subtraction); a cross-session read is the target shell's existing `get_bindings`. The view records the target session id so cross-session writes can be rejected.
- `resolveFor/2` — delegates to the shared `resolve_name/2` (Phase 1): locals → `bind:as:` → singletons → classes.
- `clearFor/1` — enqueue a clear mutation via the shell's pending-mutations API (Phase 2). Against a non-self session, raises `cross_session_mutation_unsupported`.
- `idOf/1` — extract the protocol id from a Session value.

Plus the **workspace-globals** view primitive backing `Workspace globals` (not a Session method): `globalsView/0` returns a `BindingsView` tagged for workspace scope, reading the singleton registry + `bind:as:` ETS live.

`BindingsView` read/write primitives: `view_at/2`, `view_at_put/3`, `view_remove/2`, `view_keys/1`, `view_size/1`. Writes dispatch by the view's tagged scope:
- **Session scope, calling session** → enqueue against `pending_mutations` (deferred; read-your-own-writes lag).
- **Session scope, another session** → raise `cross_session_mutation_unsupported`.
- **Workspace scope** → route through `beamtalk_workspace_interface_primitives:bind/2` / `unbind/1` (synchronous ETS, protected-name conflict checks).

**Liveness on every cross-session send.** A `Session` value captured earlier may outlive its shell. The `*For:` / `idOf:` primitives check `is_process_alive` on the carried PID and raise `#beamtalk_error{kind: session_not_found}` rather than issuing a `gen_server:call` that blocks to timeout against a dead PID.

EUnit tests for each primitive, including: cross-session **read** via `withId/1`, cross-session **write** rejection, the workspace-globals view (read + write-through to `bind:as:`), and a dead-session `session_not_found` case.

### Phase 4: Stdlib `Session` and `BindingsView` classes; upgrade `Workspace globals` (M)

Add `stdlib/src/Session.bt` with the two factory class-methods (`current`, `withId:`) and the instance-side operation methods defined in the API (`bindings`, `resolve:`, `clear`, `id`) — no class-side operation mirror, **no `globals` method** (globals are workspace-owned). Add `stdlib/src/BindingsView.bt` implementing the Dictionary protocol subset (`at:`, `at:put:`, `removeKey:`, `includesKey:`, `keys`, `values`, `size`, `do:`, `printOn:`); `printOn:` renders like a `Dictionary` so REPL output stays familiar.

**Upgrade `Workspace globals`** in `WorkspaceInterface.bt` from `-> Dictionary` to `-> BindingsView` (the same view type), backed by `globalsView/0` (Phase 3). This is a pre-1.0 breaking change to an interactive-only method; update its doc-comment, the REPL display expectation, and the `tests/repl-protocol/cases/*.btscript` cases that read `Workspace globals` (they use `at:` / `includesKey:` / `keys`, which `BindingsView` supports). `Beamtalk globals` (class registry) stays `-> Dictionary`.

No binding injection: `Session` is reachable as a class name through the class registry, no special-case handling, no protected-name additions.

### Phase 5: `Workspace currentSession` (S)

Add `currentSession` to `WorkspaceInterface` (Beamtalk class), delegating to a one-line primitive that reads the same `beamtalk_session_pid` process dictionary key seeded in Phase 2. Returns the same value as `Session current` (the session value, or `nil`). No `hasSession` predicate — callers guard with `Session current ifNotNil: [:s | …]` / `Workspace currentSession isNil`.

### Phase 6: Remove `:bindings` / `:clear` meta-commands and the `get_bindings` MCP tool (M)

Delete the `<<"bindings">>` and `<<"clear">>` op handlers in `beamtalk_repl_ops_eval.erl` and remove their entries from `beamtalk_repl_ops_dev.erl`'s op map. Update `surface-parity.md` rows for `bindings` and `clear` to cite `via Session current bindings` / `via Session current clear`.

**Resolve the `get_bindings` MCP tool.** A dedicated `get_bindings` MCP tool exists today (`crates/beamtalk-mcp/src/server.rs`), backed by the now-deleted `bindings` op. **Remove it.** MCP clients discover and read session state through the existing general surface: `search_classes` finds `Session`; `evaluate "Session current bindings keys"` (and `Workspace globals keys`) reads the layers; `evaluate "(Session withId: id) bindings keys"` reads another session. This matches the convention that per-feature MCP tools are thin front-ends over `evaluate` — and once `Workspace sessions` (Phase 7) lands, an MCP client enumerates session ids with `evaluate "Workspace sessions collect: [:s | s id]"` rather than needing them out-of-band. Update the `get_bindings` row in `surface-parity.md` accordingly.

**Bindings-changed push is preserved; only the fetch moves.** The `{bindings_changed, SessionId}` pub/sub (`beamtalk_bindings_events`, fired by the shell after each successful eval) is *not* removed — the VS Code sidebar still relies on it to know *when* to refresh. What changes is *how* the client fetches after the notification: it moves from the removed `bindings` op / `get_bindings` tool to `evaluate "Session current bindings keys"` (and `Workspace globals keys`) carrying the user's `session` id, or to `Session withId:` + instance reads. This is a client-side change in the VS Code extension and must land with this phase, not after it — otherwise the sidebar refreshes against a dead op.

**Test migration is non-trivial.** Migrating `:bindings` / `:clear` from meta-commands to evaluated expressions changes the protocol op (`bindings`/`clear` → `eval`) and therefore the response shape (a bindings map → an eval result value). Existing `tests/repl-protocol/cases/*.btscript` cases that assert on the old op responses need real rewrites, not find-replace — audit and enumerate them before locking the estimate. Add a new e2e btscript case covering the full Session API including cross-session read access (`Session withId:`) and cross-session write rejection.

### Phase 7: `Workspace sessions -> List(Session)` (S)

Add `sessions` to `WorkspaceInterface`, returning a `List` of `Session` values — one per live shell — built on the existing `sessions` protocol op (`beamtalk_repl_ops_session.erl`) + `beamtalk_session_table`, minting each `Session` value the same way `withId:` does. This serves two audiences at once:
- **Operators:** `Workspace sessions` gives a structured, individually-inspectable view of who is connected — `Workspace sessions collect: [:s | s id]`, `Workspace sessions size` — versus opaque PIDs in `supervisor:which_children`. Per-session metadata (connect time, client kind, idle status) is a later extension on the `Session` value, explicitly out of scope here.
- **Tooling cross-session discovery:** it closes the gap left by `withId:` (which assumes you already know the id). An LSP/MCP client finds the user's session with `Workspace sessions` instead of an out-of-band id.

EUnit + e2e btscript: list shape, ids match live shells, values are usable with the instance reads (and reject cross-session writes).

## Migration Path

The `:bindings` and `:clear` meta-commands (and the `get_bindings` MCP tool) are removed in Phase 6; the replacement API is shipped in Phases 1–5 first so they land in the same release.

| Before | After |
|--------|-------|
| `:bindings` | `Session current bindings keys` (session locals only) <br> `Workspace globals keys` (workspace globals) <br> ⚠️ **behaviour change:** `:bindings` today (via `binding_names/0`) *shows* `bind:as:` names mixed in; under this ADR they are globals, read via `Workspace globals` — they no longer appear in `Session current bindings`. |
| `:clear` | `Session current clear` |
| MCP `get_bindings` tool | `evaluate "Session current bindings keys"` (and `Workspace globals keys`) |
| `Workspace globals` returns `Dictionary` | `Workspace globals` returns a live `BindingsView` (write-through; ⚠️ pre-1.0 return-type change, interactive-only) |
| (no equivalent) | `Session current bindings at: #x put: 99` (live mutation) |
| (no equivalent) | `Session current bindings removeKey: #x` |
| (no equivalent) | `Session current resolve: #name` (REPL debugging tool) |
| (no equivalent) | `Workspace currentSession` / `Session current` |
| (no equivalent) | `Session withId: aSessionId` (LSP cross-session) |
| (no equivalent) | `Workspace sessions` → `List(Session)` (operators; cross-session discovery) |

The meta-commands existed only on the REPL surface. Removing them is a small loss for muscle-memory CLI users (one `migrate_to` shell-side error message in the release notes is sufficient) and a strict gain for MCP and WebSocket clients, which now have a Beamtalk-native path to per-session state with both reads and write-through mutations.

## References

- [BT-2092](https://linear.app/beamtalk/issue/BT-2092/first-class-session-object-walkable-binding-layers-smalltalk-style) — driving issue
- [BT-2083](https://linear.app/beamtalk/issue/BT-2083/surface-only-audit-decide-promote-or-lock-for-asymmetric-ops) — surface audit that un-deprecated `:bindings` / `:clear` and filed BT-2092
- ADR 0040 — Workspace-Native REPL Commands (the precedent for moving meta-commands onto Beamtalk objects)
- ADR 0010 — Global Objects and Singleton Dispatch (established the `Beamtalk` / `Workspace` / `Transcript` bindings that form the workspace globals layer)
- ADR 0004 — Persistent Workspace Management (workspace lifecycle context)
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_eval.erl:62-77` — current `:bindings` / `:clear` handlers
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_shell.erl:155-244` — shell init and binding lifecycle
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_shell.erl:432` — `inject_workspace_bindings/1` (today's eager injection of workspace globals into session bindings at init; **removed in Phase 1** in favour of lazy resolution)
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl:765-786` — `handle_session_bindings/1` (resolves singletons + `bind:as:` entries that injection copies in today)
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl` — `globals/0` (the `Workspace globals` primitive, upgraded to a live `BindingsView` in Phase 4)
- `crates/beamtalk-mcp/src/server.rs` — the `get_bindings` MCP tool (removed in Phase 6)
- `runtime/apps/beamtalk_workspace/src/beamtalk_session_table.erl` — session registry
- Pharo Workspace `bindings` accessor; Squeak `Environment`; IPython `get_ipython()` — prior art for first-class binding scope objects
