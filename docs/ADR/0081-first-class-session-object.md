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

Introduce a new `Session` class with a **class-side API** as the primary interface. The class itself is the entry point — `Session bindings` is a class-side message send, mirroring Smalltalk patterns like `Date today`, `Time now`, and `Smalltalk current`. There is **no injected `Session` binding** at the shell level; the class is reachable because it is loaded, not because it was singled out.

`Session` is **not** an Actor (gen_server). It is a Beamtalk class whose class-side methods delegate to `beamtalk_session_primitives`, which uses process-context resolution (process dictionary seeded during eval worker spawn) to find the calling session.

`Session bindings` and `Session globals` return **live binding views** — mutating them writes through to the underlying state. `view at: #x put: 42` sets a session-local; `view removeKey: #x` removes it. Globals mutations route through the existing `bind:as:` / `unbind:` machinery and inherit those conflict checks.

### API

```beamtalk
sealed typed Object subclass: Session

  /// Live view of session-local bindings. Reads return current values;
  /// `at:put:` and `removeKey:` mutate session state directly.
  class bindings -> BindingsView =>
    (Erlang beamtalk_session_primitives) bindingsView

  /// Live view of workspace globals (singletons + bind:as: entries).
  /// Mutations route through `Workspace bind:as:` / `unbind:`, including
  /// the existing protected-name conflict checks for system singletons.
  class globals -> BindingsView =>
    (Erlang beamtalk_session_primitives) globalsView

  /// Walk binding layers, return first match or nil.
  /// Lookup order: session locals → workspace globals.
  /// Primarily a REPL debugging tool: answers "where does this name
  /// resolve from?" interactively.
  class resolve: aName :: Symbol -> Object =>
    (Erlang beamtalk_session_primitives) resolve: aName

  /// Clear all session-local bindings. Workspace globals remain.
  class clear -> Nil =>
    (Erlang beamtalk_session_primitives) clear

  /// Stable session identifier (matches the protocol session ID).
  class id -> String =>
    (Erlang beamtalk_session_primitives) id

  /// Return the calling process's session as a value, for pass-by-reference.
  /// Returns `nil` outside a REPL eval context.
  class current -> Session | Nil =>
    (Erlang beamtalk_session_primitives) current

  /// Look up a session by its protocol session ID (returns nil if no
  /// such session). Used for cross-session access — see "Cross-session
  /// access (LSP, VS Code)" below.
  class withId: aSessionId :: String -> Session | Nil =>
    (Erlang beamtalk_session_primitives) withId: aSessionId

  // Instance-side methods (used when a Session value is passed around):
  bindings -> BindingsView =>
    (Erlang beamtalk_session_primitives) bindingsViewFor: self
  globals -> BindingsView =>
    (Erlang beamtalk_session_primitives) globalsView
  resolve: aName :: Symbol -> Object =>
    (Erlang beamtalk_session_primitives) resolve: aName for: self
  clear -> Nil =>
    (Erlang beamtalk_session_primitives) clearFor: self
  id -> String => (Erlang beamtalk_session_primitives) idOf: self
```

**Session instance representation.** `Session` is declared `Object subclass` (no `field:`/`state:` declarations, which an `Object` subclass cannot have), yet a `Session` *value* returned by `current` / `withId:` must carry its session's PID and protocol ID. This follows the existing `FileHandle` / `Port` pattern: a `sealed typed Object subclass` whose per-instance identity lives in the runtime representation (a tagged map produced by the primitive), not in a declared field. Instance methods pass `self` to a `*For:` primitive, which extracts the PID/ID from that representation — exactly as `FileHandle lines` passes `self` to `(Erlang beamtalk_file) handleLines: self`.

`BindingsView` is a small Dictionary-protocol-compatible class backed by primitives that read/write session or workspace state. It implements `at:`, `at:put:`, `removeKey:`, `includesKey:`, `keys`, `values`, `size`, and `do:`. Mutations via `at:put:` / `removeKey:` are write-through.

**Return-value contract.** `at:put:` returns the value put (standard Dictionary protocol), for both session and workspace views. `removeKey:` uniformly returns `nil` rather than the removed value: session-local removals are *enqueued* (see the eval-ordering note in Consequences) so the removed value is not read back synchronously, and returning it for one view but not the other would be a worse inconsistency — `nil` for both is the honest contract. Reads (`at:`, `keys`, `values`, `size`, `do:`) reflect committed state — within a single eval they do **not** observe mutations enqueued earlier in that same eval (see Consequences → Negative → eval ordering).

A `BindingsView` is a handle over live session/workspace state. Its **runtime representation** follows `FileHandle` / `Port` — a `sealed typed Object subclass` with no `field:` declarations, minted by a primitive as a tagged map carrying per-instance identity, with instance dispatch routed to a primitive module. Note the precedent is for the *representation only*, not the lifetime contract: `FileHandle` must **not** escape its `open:do:` block, whereas a `Session` value is explicitly meant to be stored and passed (pass-by-reference is the whole justification over Option B). Escape-safety for `Session` is handled not by the FileHandle "don't escape" rule but by the liveness check on every send (`session_not_found` if the backing shell has died — see "Stored Session / view lifetime" in Consequences). A view read against a dead session raises rather than returning stale data.

**No injected binding.** `Session` is a class name resolved through the class registry, like `Counter` or `Integer`. It cannot be shadowed by a user `:=` assignment because class names are not in the binding map; assigning `Session := foo` creates a local binding that shadows the class reference *only in that local scope*, the same way assigning `Counter := foo` works. This is normal Smalltalk behaviour and not a footgun specific to Session.

`Workspace currentSession` is **also** retained on `WorkspaceInterface` as a navigation method — Workspace is a discovery entry point, and "find the current session" fits the pattern of `Workspace actors`, `Workspace supervisor`, etc. It returns the same value as `Session current`. A companion `Workspace hasSession` predicate returns true/false, useful for library code that conditionally uses session state.

### Cross-session access (LSP, VS Code)

The VS Code extension and LSP open their own protocol sessions for completion queries, but need to read bindings from the *user's* REPL session. This is currently handled at the protocol layer (BT-1045): the `bindings` op carries a `session` parameter and `beamtalk_session_table:resolve_pid/2` looks up the target shell.

The new Session API supports this cleanly:

```beamtalk
// On the LSP completion session:
userSession := Session withId: "user-repl-abc-123"  // returns Session | Nil
userSession ifNotNil: [
  candidates := userSession bindings keys
]

// Or for full search across all sessions (future):
Workspace sessions collect: [:s | s id]
```

The class-side methods (`Session bindings`) operate on the calling process's session — semantically "my session". The instance-side methods (`aSession bindings`) operate on whichever session the value represents, including ones owned by other connections. This is the load-bearing reason for instance methods on `Session`: cross-session **introspection** from tooling.

Cross-session access is **read-only**. The instance-side reads (`bindings keys`, `bindings at:`, `resolve:`, `id`) work against any live session; the instance-side mutators (`at:put:`, `removeKey:`, `clear`) raise `cross_session_mutation_unsupported` (see Consequences → Negative → "Cross-session writes"). The LSP/completion use case needs only reads, and cross-session writes would race the target session's in-flight eval. `withId:` itself performs a liveness check and returns `nil` for an unknown *or* dead session id; a previously-captured `Session` whose shell later dies raises `session_not_found` on its next send.

The LSP and any other client that previously called the `bindings` / `clear` protocol ops moves to `Session withId:` + instance methods in the same release.

### REPL Examples

```text
beamtalk> x := 42
42
beamtalk> Session bindings keys
#(#x)
beamtalk> Session bindings at: #x
42
beamtalk> Session globals keys
#(#Transcript, #Beamtalk, #Workspace)
beamtalk> Session resolve: #x
42
beamtalk> Session resolve: #Transcript
#<TranscriptStream>
beamtalk> Session resolve: #notDefined
nil

// Live mutations write through:
beamtalk> Session bindings at: #y put: 99
99
beamtalk> y
99
beamtalk> Session bindings removeKey: #x
nil
beamtalk> x
// => #beamtalk_error{kind: undefined_variable, name: #x}

// Globals mutations route through bind:as: / unbind:
// at:put: returns the value put (Dictionary protocol), like the session view above.
beamtalk> Session globals at: #MyTool put: someActor
#<Counter @ <0.123.0>>
beamtalk> MyTool
#<Counter @ <0.123.0>>
beamtalk> Session globals removeKey: #MyTool
nil

// Pass-by-reference (cross-session access from LSP / tooling):
beamtalk> userSession := Session withId: "user-repl-abc-123"
#<Session: "user-repl-abc-123">
beamtalk> userSession bindings keys
#(#x, #y, #counter)

beamtalk> Workspace currentSession id
"abc123-..."
beamtalk> Session clear
nil
beamtalk> Session bindings keys
#()
```

### Error Examples

Outside a REPL eval, `Session current` and `Workspace currentSession` return `nil`:

```beamtalk
// In a .bt file run via `beamtalk run`:
Session current        // => nil
Workspace currentSession  // => nil
Workspace hasSession   // => false
```

Class-side `Session bindings` outside a REPL session raises a structured error:

```beamtalk
Session bindings
// => #beamtalk_error{kind: no_session, message: "no session in scope"}
```

Cross-session `withId:` for an unknown session ID returns nil:

```beamtalk
Session withId: "nonexistent"  // => nil
```

Globals mutations that violate protected-name rules raise the existing `bind:as:` error:

```beamtalk
Session globals at: #Workspace put: nil
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
| Singleton or per-PID? | **Per-PID** | CLI has one session, WebSocket/MCP clients can have many. Class-side methods use process-context resolution to find the calling session; instance methods carry their own session ID for cross-session access. |
| What outside REPL eval? | **`Session current` returns `nil`**; class-side methods like `Session bindings` raise a structured `no_session` error. | Compiled program code has no session. Returning a stub or nil from `bindings` would mask the meaningful absence; an error is louder. |
| Persist across `:sync` / reload? | **Yes** — unchanged from current behaviour. | Session locals are shell-process state, owned independently of class definitions. Sync rebuilds modules; the session keeps its bindings. |
| Are bindings/globals views live or snapshots? | **Live views.** Mutations write through. | Matches Pharo semantics (`Smalltalk globals at:put:`). A snapshot would force a parallel setter API (`Session at:put:`); the live view collapses both into one Dictionary protocol. |
| Layer-walking abstraction? | **Dropped.** No `layers` method, no recursive `parent`. | Beamtalk has settled on exactly two layers; package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer. A `layers` constant would be pure decoration. |
| `bind:as:` interaction? | **Unchanged.** `Session globals at:put:` is equivalent to `Workspace bind:as:`, including conflict checks. | One write path through the existing primitives; the new view is sugar over the established API. |
| Cross-session access? | **Instance methods + `Session withId:`, read-only** | LSP/VS Code completion sessions need to query the user's session. Class-side methods are "my session"; instance methods are "that session". Cross-session *writes* race the target's in-flight eval and are unneeded, so they raise `cross_session_mutation_unsupported`. |
| Stale / dead session reference? | **`withId:` does a liveness check (returns `nil`); a captured value whose shell later dies raises `session_not_found`.** | Avoids a `gen_server:call` to a dead PID blocking for the full timeout. |

## Prior Art

| System | Binding model | What we adopt | What we reject |
|--------|---------------|---------------|----------------|
| **Pharo `Smalltalk current` / `Date today` / `Time now`** | Class-side methods that return a "current" instance — the class itself is the entry point. | The class-as-entry-point pattern (`Session bindings` is a class-side message, like `Date today`). | None — this is the load-bearing prior art for Option F. |
| **Pharo / Squeak Workspace** | `Dictionary` of bindings on the workspace, mutable via `at:put:`, with fallback to `Smalltalk globals`. Methods: `bindings`, `bindingOf:`, `removeBinding:`, `resetBindings`. | The mutable-Dictionary shape (`Session bindings at:put:` writes through, matching `Smalltalk globals at:put:`). | Pharo merges workspace and session — Beamtalk has separate workspace (shared) and session (per-PID) layers because of multi-client BEAM hosting. |
| **Pharo Environment** | Wraps a `Dictionary` of globals; multiple Environments allowed. | Layer reification via named accessors. | Beamtalk does not need user-creatable environments — workspace is bootstrapped once per BEAM node. |
| **Newspeak** | Lexical nesting only; no global namespace; capability passing via constructor parameter. | The principle that scope is explicit, not magic. | Beamtalk has Smalltalk-style globals (`Transcript`, classes); a fully-lexical model would invalidate the existing namespace design. |
| **Self** | Parent slot chain; scope walks via `parent*` slots. | Explicit layer separation (`bindings` vs `globals`), inspired by Self's transparent parent hierarchy. | Recursive `parent` chains overstate the complexity for two layers. |
| **IPython `get_ipython()`** | Shell singleton with `user_ns` dict, `who_ls()`, `reset()`. | The "current session is reachable through a global accessor" pattern (`Workspace currentSession`). | A single mutable dict obscures layer ownership; we keep layers separate. |
| **Ruby `Binding`** | First-class scope object. `binding` returns the current scope; `Binding#local_variables` lists names. Can be passed for scoped expression execution. Closest prior art to the `Session` design. | A scope object that can be inspected and passed around. | Ruby's Binding captures lexical scope (closures); Beamtalk's Session wraps a session process, not a lexical frame. |
| **Erlang shell `b()` / `f()`** | `b()` lists shell bindings; `f()` clears all; `f(X)` clears one. Shell-only built-in commands, not callable from program code. | Demonstrates the universal need to inspect REPL state. | Shell commands, not values — cannot be composed, stored, or called from non-shell code. Exactly the problem this ADR solves. |
| **Elixir `binding()`** | Macro returning current bindings as a keyword list. | Treating bindings as inspectable data. | A macro/keyword-list shape is non-Smalltalk; we use Dictionary returns and explicit objects. |

## User Impact

**Newcomer (Python/JS background).**
`Session` is discoverable via tab-completion on the class name — type `Ses<TAB>` and the class-side methods appear. Discovering "how do I see my variables" via `Session bindings` is more direct than memorising a `:b` meta-command. The live-mutation pattern (`Session bindings at: #x put: 42`) matches how Dictionary works everywhere else in the language.

**Smalltalk developer.**
The session-as-object model is recognisably Smalltalk: `Session` is to a Beamtalk shell what `Smalltalk` is to a Pharo image. The two-layer split (session vs. workspace) is more honest about multi-client BEAM hosting than Pharo's single-image model.

**Erlang/BEAM developer.**
Class-side methods that use process-context resolution match how BEAM convention works — the "calling process determines context" pattern is idiomatic (e.g. `self()`, process dictionary, `$ancestors`). `Session withId:` for cross-session access maps directly to the existing `beamtalk_session_table:lookup/1`. Multi-session debugging (MCP, WebSocket) gets a first-class object to work with.

**Production operator.**
`Workspace sessions` (a follow-up extension) can return a list of `Session` objects, each individually inspectable. Compared to opaque PIDs in `supervisor:which_children`, this gives operators a structured view of who is connected and what they have bound.

**Tooling developer (LSP / VS Code).**
The MCP server can call `Session bindings` and `Session globals` via `evaluate` to display per-session state in the inspector panel. Tooling that needs the merged binding map (e.g. completions) merges those two Dictionaries in resolution order — no separate primitive needed.

## Steelman Analysis

### Option B: Workspace Extension (rejected)

**Newcomer:** "I already know `Workspace` from `Workspace classes` and `Workspace actors`; adding `Workspace sessionBindings` is one more method on a familiar object. No new concept to learn, no new top-level binding to discover."

**Smalltalk purist:** "Adding methods to `Workspace` matches how `Smalltalk` accumulates introspection in Pharo — `Smalltalk globals`, `Smalltalk allClasses`, `Smalltalk current`. Why is sessions special enough to warrant its own class?"

**BEAM veteran:** "`Workspace sessionBindings` could use the same process-context resolution that `Session bindings` uses anyway. The dispatch cost is identical; you're just spelling it differently. For the 99% case — introspecting your own session — adding methods to an existing object is simpler than introducing a whole new class. No new Erlang module, no new class registry entry."

**Operator:** "Fewer object types in the system means fewer surfaces to learn for postmortem debugging. `Workspace` is already the operator's entry point — keep it that way."

**Language designer:** "Adding methods is cheaper than adding classes. The composability argument for first-class Session is partly speculative — today nobody writes `Workspace sessions collect: [:s | s bindings]`. YAGNI."

**Why not adopted:** Option B *works* for the common case and is genuinely simpler. Two factors push us to F (class-side Session) despite this:

1. **Pass-by-reference.** `Session` as a value can be stored, passed to library code, or used for cross-session access — `Session withId:` for LSP completion against the user's session, `Workspace sessions collect: [:s | s id]` for multi-session enumeration. Option B cannot express any of these without inventing per-call session-id parameters.
2. **Workspace's role.** `Workspace` is a navigation entry point — `Workspace actors`, `Workspace classes`, `Workspace supervisor`, `Workspace dependencies` — each method *finds you something*. Session bindings are not something you navigate to from the workspace; they belong to a session, which has its own identity. Loading four session-state methods (`sessionBindings`, `sessionGlobals`, `clearSession`, `resolveBinding:`) directly onto `Workspace` would be the same category error as putting `Counter increment` on `Workspace`. The class-side `Session` API keeps the surface focused.

Option F gives us almost all of B's simplicity (no injection, no shadowing, no `injected_ws_keys` plumbing) plus pass-by-reference for the cross-session and multi-session cases that B cannot express.

### Option C: Recursive Scope Chain (rejected)

**Newcomer:** "Once I learn the parent pattern, I can walk any depth. It's just like prototype chains in JavaScript or `__getattr__` chains in Python — a familiar mental model from outside the Smalltalk world."

**Smalltalk purist:** "Self's slot-chain model is the most principled scoping mechanism in any object language. Walking `parent` is more uniform than special-casing each layer with a named accessor (`bindings`, `globals`, ...). Adopt proven prior art."

**BEAM veteran:** "Process linking already gives BEAM a parent chain (`$ancestors`). A uniform `parent` accessor on Scope objects mirrors what's already happening at the OTP level — recursive walks are idiomatic on BEAM."

**Operator:** "If we ever introduce per-package, per-module, or per-actor scopes, depth-flexible scopes absorb them without redesign. Today's two-layer model becomes tomorrow's five-layer model without breaking callers."

**Language designer:** "Most elegant. Two layers IS the special case; the general case is a chain. Designing for the general case prevents future entrenchment of the wrong abstraction."

**Why not adopted:** Beamtalk has explicitly settled on a two-layer binding model — session locals over workspace globals — with no plans for additional layers. Package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer; module scope is an Erlang concept invisible to Beamtalk users (per project memory). The general-case argument requires a third layer to ever materialise; given the design direction, that's a bet against settled decisions. The cost — a `Scope` class hierarchy, recursive resolution semantics, tooling for chain inspection, plus the foreign vocabulary of "scope" in a Smalltalk context — is paid for an extension we are choosing not to take.

### Tension Points

- **Class-side vs. injected-binding ergonomics.** Both are two words to type (`Session bindings` either way). The class-side approach wins on principled grounds — Pharo's `Date today` / `Smalltalk current` pattern is the right precedent for a per-PID "current" value. The injected-binding approach (Alternative A) was the original sketch but pretends Session is a singleton when it isn't.
- **The LSP cross-session case is real, not speculative.** BT-1045 already established the protocol-level cross-session pattern; the LSP runs its own completion session and queries the user's session for bindings. Option B has no Beamtalk-native expression of this; Option F's `Session withId:` does.
- **Live mutation cost.** Pharo-style `Smalltalk globals at:put:` write-through is genuinely useful, but adds a `pending_mutations` queue to `beamtalk_repl_state` to handle eval-ordering. This complexity is paid regardless of class-side vs. injected-binding — it's a property of the live-view decision, not the entry-point decision.
- **Two layers is settled.** Option C's strongest argument (future layer flexibility) requires bet-against-settled-design. Beamtalk has committed to exactly two binding layers; package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer.

## Alternatives Considered

### Alternative A: Per-session injected `Session` binding

The original sketch in BT-2092 — inject `Session` as a per-shell binding (alongside `Transcript`, `Beamtalk`, `Workspace`) so `Session bindings` resolves to an instance method on the injected value.

Rejected because:
- **Less Smalltalk than the class-side approach.** Pharo's pattern for "the current X" is class-side (`Date today`, `Time now`, `Smalltalk current`), not a top-level singleton binding. Sessions are per-PID, not singular — injecting them as if they were singletons is dishonest.
- **Shadowing footgun.** `Session := MyThing new` in the shell silently overwrites the injected binding. The existing `bind:as:` conflict checks don't catch shell-level `:=`. Mitigating this requires a new compiler warning, which is cost we don't pay if there is no injected binding.
- **Implementation complexity.** Requires extending `injected_ws_keys`, modifying `inject_workspace_bindings`, adding `Session` to `is_protected_name/1`, and a clear-and-reinject loop. The class-side approach skips all of this.
- **Tooling parity is messier.** Cross-session access (LSP completion against the user's session) doesn't fit naturally — the LSP's session sees its own injected `Session`, not the user's. Class-side methods plus `Session withId:` separate "my session" from "that session" cleanly.

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

- **Layers become inspectable.** `Session bindings` and `Session globals` make the two-layer model visible; users no longer wonder why `Transcript` shows up in `:bindings` but is not something they assigned.
- **Resolution becomes debuggable.** `Session resolve: #x` answers "where does this name come from?" interactively.
- **`:bindings` and `:clear` get a Beamtalk-native replacement.** Surface parity restored: MCP `evaluate` and any other client can drive session inspection through the standard message-send surface.
- **Multi-session aware.** The design works unchanged for CLI (one session), WebSocket, MCP, and any future multi-client surface.
- **Foundation for future ops.** Per-session metadata (connect time, client kind, idle status), session-to-session messaging, and live-session listings (`Workspace sessions`) all become straightforward extensions.

### Negative

- **New `BindingsView` class.** A small Dictionary-protocol class is required to support live read/write views. Implementation cost is moderate (one class plus primitives for read/write/iterate). The class is simple — every method delegates to a primitive — but it is a new public surface that has to be documented and tested.
- **Mutation during eval has ordering constraints.** Both `Session clear` and `Session bindings at:put:` mutate shell state. The eval worker holds a *state snapshot*; on completion its returned state is merged back, so a naive direct write would be overwritten. The implementation adds a `pending_mutations` queue to `beamtalk_repl_state`, modelled on the existing `pending_module_removals` mechanism (`beamtalk_repl_shell.erl` `apply_pending_removals/2` + `drain_pending_removals/1`). Primitives enqueue `{op, key, value}` tuples rather than writing directly; the queue is drained in the eval-result handler. The exact merge order and the behaviour on every eval exit path are specified in Phase 1 — they are correctness-critical and must not be left implicit.
- **Reads do not observe same-eval writes (read-your-own-writes lag).** Because mutations are *enqueued* and drained only at eval-result time, a mutation does not take effect until the *next* eval. This holds for both bare-variable reads and view reads:
  - `Session bindings at: #x put: 99. x + 1` evaluates `x + 1` against the **original** `x`.
  - `Session bindings at: #x put: 99. Session bindings at: #x` returns the **original** value of `x`, not `99`.
  This matches the user expectation that an expression sees its own pre-state, and matches how `bind:as:` already behaves mid-eval (its effect is re-injected by `refresh_ws_bindings` only after the eval completes). It is nonetheless a surprise worth documenting: to thread a freshly-computed value into the same expression, use a local (`y := 99. Session bindings at: #x put: y. y`) instead of reading back through the view.
- **Cross-session writes are not supported.** Instance-side `aSession bindings at:put:` / `removeKey:` against a session owned by *another* connection (obtained via `Session withId:`) would mutate that shell's state while its own in-flight eval worker holds an older snapshot — the worker's writeback would silently clobber the cross-session mutation, and the `pending_mutations` queue (which lives in the *calling* eval's flow) does not help the target. Because the only concrete cross-session use case — LSP/VS Code completion — needs **reads only**, cross-session `at:put:` / `removeKey:` / `clear` raise `#beamtalk_error{kind: cross_session_mutation_unsupported}` rather than racing. Cross-session **reads** (`keys`, `at:`, `resolve:`, `id`) are fully supported. This also keeps the F-over-B justification honest: the load-bearing win over Option B is cross-session *introspection*, not mutation.
- **Stored `Session` / view lifetime.** A `Session` value (or a `BindingsView`) can outlive the shell it refers to — e.g. the user closes their REPL tab after an LSP session captured `Session withId: …`. `withId:` performs a liveness check (`resolve_pid`-style `is_process_alive`, not a raw `lookup`) and returns `nil` for a session that is gone. A `Session` value captured earlier whose shell *subsequently* dies raises `#beamtalk_error{kind: session_not_found}` on the next message send rather than blocking on a `gen_server:call` to a dead PID until timeout. Views are transient handles (like `FileHandle`); they are not durable references.
- **Outside-REPL behaviour.** `Session current` returns `nil`; class-side methods like `Session bindings` raise a structured `no_session` error. Library code that wants graceful degradation must check `Workspace hasSession` first.
- **`Session clear` is asymmetric with the view.** `Session bindings removeKey: #x` removes one binding; `Session clear` removes all. There is no `Session bindings clear` because the view's protocol is generic Dictionary semantics — `Session bindings` returns a *view*, not a Dictionary the user owns, so a `clear` on the view would be ambiguous (clear-the-view or clear-the-state). Keeping `Session clear` as a top-level method avoids this ambiguity.

### Neutral

- **`bind:as:` semantics unchanged.** Continues to write to the workspace ETS layer. `Session bindings` will not include `bind:as:` entries (those appear in `Session globals`). This is a clarification, not a behaviour change.
- **Session persistence across sync unchanged.** Existing behaviour preserved: session locals survive `Workspace sync`.
- **Protocol `bindings` / `clear` ops are removed** in Phase 5, alongside the rest of the work. The Session API ships first (Phases 1–4), so when the meta-commands disappear the replacement is already in place. There is no deprecation window — single coordinated release.

## Implementation

The implementation breaks down into a Phase 0 wire-check plus five shippable phases.

### Phase 0: Wire-check / napkin (XS)

Before building any of the surface, prove the single load-bearing assumption end-to-end: *a primitive running inside an eval worker can recover the calling session*. Seed the worker's process dictionary with `beamtalk_session_pid` / `beamtalk_session_id` at spawn, add a throwaway `beamtalk_session_primitives:id/0` that reads it back, and confirm `(Erlang beamtalk_session_primitives) id` returns the right id from a live eval — and `nil`/`no_session` from a non-eval context. One EUnit test plus one manual REPL round-trip. If this does not work cleanly (e.g. process-dictionary seeding interacts badly with the worker spawn or the streaming path), the rest of the design is re-examined before investing in `BindingsView` and `pending_mutations`.

### Phase 1: Process-context plumbing + pending-mutation merge (M)

**Seed both eval spawn paths.** Modify the worker spawn in `beamtalk_repl_shell` to seed the worker's process dictionary with `beamtalk_session_pid` (the shell's `self()`) and `beamtalk_session_id` (the protocol session ID) before calling `do_eval`. There are **two** spawn paths and both must be seeded:
- `handle_call({eval, _}, ...)` and `handle_call({eval_trace, _}, ...)` — the synchronous REPL path.
- `handle_cast({eval_async, _, Subscriber}, ...)` — the streaming path (`do_eval/3`).

A primitive that finds `get(beamtalk_session_pid) =:= undefined` reports `no_session` (class-side) or `nil` (`current`/`hasSession`); seeding only one path would make `Session` behave inconsistently between normal and streaming eval.

**Add the `pending_mutations` queue.** Add `pending_mutations` to `beamtalk_repl_state` — a list of `{op, key, value}` tuples (`op ∈ {put, remove, clear}`) — with accessors mirroring the existing `pending_module_removals` field. Primitives **enqueue** rather than write directly. The queue is owned by the shell's `ShellState` (not the worker snapshot): primitives enqueue via a `gen_server:call` to the shell, which is safe from the worker because the shell is in `noreply` while the worker runs and is free to service its mailbox. Visibility is guaranteed by ordering: the worker's enqueue `gen_server:call` is synchronous and completes *before* the worker sends `{eval_result, …}`, so the shell processes every enqueue (updating its loop state) strictly before it processes the result message — the `ShellState` bound in the `eval_result` handler always reflects the enqueued mutations.

**Add a `get_session_locals` gen_server call.** Add `handle_call(get_session_locals, ...)` to `beamtalk_repl_shell` returning `maps:without(get_injected_ws_keys(State), get_bindings(State))`. This is required for cross-session reads (Phase 2): the subtraction needs both the bindings map and the injected-key set, which only co-exist inside the owning shell's state. `get_bindings` alone (which returns the merged map) is insufficient.

**Specify the merge order on every eval exit path.** The current eval-result handler does `apply_pending_removals(ShellState, WorkerState)` then (success only) `refresh_ws_bindings/1`. The drain slots in as follows:

| Eval exit path | Handler | Action |
|----------------|---------|--------|
| Success | `handle_info({eval_result, _, {ok, ...}}, ...)` | `M1 = apply_pending_removals(ShellState, WorkerState)`; `M2 = apply_pending_mutations(ShellState, M1)`; `refresh_ws_bindings(M2)`. Mutations apply **on top of** the worker's returned bindings (so the worker's own `x := …` is visible) but **after** removals, and before ws-refresh re-injects globals. |
| Error | `handle_info({eval_result, _, {error, ...}}, ...)` | `apply_pending_removals` + `apply_pending_mutations` for `put`/`remove` ops only, **no** `refresh_ws_bindings` (unchanged from today). `put`/`remove` are explicit per-key edits the user issued, independent of the failed expression, so they take effect. A queued **`clear`** (whole-session destruction) is **dropped** on the error path: `Session clear. someTypo` should not wipe every local because the line after `clear` failed. `clear` applies only on the success path. |
| Interrupt | `handle_call(interrupt, ...)` | Drain `pending_mutations` alongside the existing `drain_pending_removals/1` so an interrupted eval's issued mutations are not lost. |
| Worker crash | `handle_info({'DOWN', ...}, ...)` | **Discard** `pending_mutations` — a crashed worker's partial mutations are not trustworthy; drain removals as today but drop the mutation queue. |

`apply_pending_mutations/2` reads the queue from `ShellState` and folds it over the merged bindings in enqueue order (`put`/`remove` per key, `clear` empties session-locals while preserving injected ws-keys), then resets the queue to `[]`.

EUnit tests: worker-spawn seeding on **both** paths; `apply_pending_mutations` fold order; the four exit-path behaviours above (success/error apply, interrupt applies, crash discards); and a `clear`-then-`put` ordering case.

### Phase 2: Runtime primitives module (M)

Create `beamtalk_session_primitives.erl` in `runtime/apps/beamtalk_workspace/src/`. Each primitive resolves the session via `get(beamtalk_session_pid)` (or the explicit Session instance for the `*For:` variants):

- `current/0` — returns a Session value with the calling session's PID and ID, or `nil`.
- `withId/1` — looks up a session by protocol ID and **checks liveness** (`is_process_alive`, the `resolve_pid` discipline — not a raw `beamtalk_session_table:lookup/1`, which can return a dead PID). Returns a Session value or `nil`.
- `bindingsView/0` — returns a `BindingsView` tagged for session-local scope, bound to the calling session.
- `globalsView/0` — returns a `BindingsView` tagged for workspace scope (shared across sessions). Note `globals` carries no per-session identity; the instance-side `globals` returns the same shared view for any receiver.
- `bindingsViewFor/1` — instance variant; the view records the **target** session id so cross-session reads resolve against it and cross-session writes can be rejected.
- `resolve/1`, `resolve/2` — walk session locals first, then workspace globals; return value or nil.
- `clear/0`, `clearFor/1` — enqueue a clear mutation via the shell's pending-mutations API (Phase 1). `clearFor/1` against a non-self session raises `cross_session_mutation_unsupported`.
- `id/0`, `idOf/1` — read `beamtalk_session_id` (process dictionary) / extract it from a Session value.

**Session-locals vs. merged map — and why it must be computed shell-side.** Because workspace globals are *injected* into each shell's single bindings map at init (`inject_workspace_bindings/1`, BT-883) and refreshed after each eval, "session locals" is not a separate map — it is the bindings map **minus** the injected keys. The filter is `maps:without(get_injected_ws_keys(State), get_bindings(State))`.

The two operands (`bindings` and `injected_ws_keys`) live *together* in a shell's `beamtalk_repl_state`, so the subtraction can only be done where both are in hand — **on the owning shell**. The existing `get_bindings` gen_server call returns only the merged map (`beamtalk_repl_shell.erl:235`), with no companion call for the injected-key set; computing locals on the *reader* side is therefore impossible for a cross-session target. Phase 1 must add a `get_session_locals` gen_server call to `beamtalk_repl_shell` that performs the subtraction and returns session-locals directly. Both `bindingsView/0` (calls its own shell) and `bindingsViewFor/1` (calls the target shell) route through it, so local and cross-session reads share one code path and one definition of "locals".

Use `get_injected_ws_keys/1` (the per-session dynamic set, which *includes* `bind:as:` names) — **not** `beamtalk_workspace_config:binding_names/0`, which lists only the static singletons and excludes `bind:as:` names. The two are not interchangeable. This is a deliberate behaviour change from today's `:bindings` op (which filters with `binding_names/0` and therefore *shows* `bind:as:` names): under this ADR `bind:as:` names move out of `Session bindings` and into `Session globals`, which is the correct home for them (see Migration Path). This is the load-bearing step that keeps `Session bindings` (locals) and `Session globals` (injected ws-keys + ETS) disjoint.

**Liveness on every cross-session send.** A `Session` value captured earlier may outlive its shell. The `*For:` / `idOf:` primitives check `is_process_alive` on the carried PID and raise `#beamtalk_error{kind: session_not_found}` rather than issuing a `gen_server:call` that blocks to timeout against a dead PID.

Plus the BindingsView read/write primitives: `view_at/2`, `view_at_put/3`, `view_remove/2`, `view_keys/1`, `view_size/1`. Session-scope writes for the **calling** session enqueue against its pending-mutations queue; session-scope writes targeting **another** session (a cross-session `BindingsView`) raise `cross_session_mutation_unsupported`. Workspace-scope writes route through `beamtalk_workspace_interface_primitives:bind/2` and `unbind/1`, inheriting protected-name conflict checks.

**Two write models, by design.** Session-scope writes are *deferred* (queued, applied at eval-end — the read-your-own-writes lag in Consequences). Workspace-scope writes are *synchronous*: `bind/2` / `unbind/1` hit the shared ETS table immediately, exactly as `Workspace bind:as:` does today. The asymmetry is intrinsic — session state is per-shell and snapshot-isolated during eval, workspace state is shared ETS. One consequence: a mid-eval `Session globals at:put:` is written to ETS at once but is not re-injected into the shell's bindings map until `refresh_ws_bindings` runs, which (as today for `bind:as:`) happens only on the **success** path; on the error path the injected copy stays stale until the next successful eval. This matches existing `bind:as:` behaviour and is not a regression.

EUnit tests for each primitive, including: cross-session **read** via `withId/1`, cross-session **write** rejection, the session-locals filter, and a dead-session `session_not_found` case.

### Phase 3: Stdlib `Session` and `BindingsView` classes (M)

Add `stdlib/src/Session.bt` with the class-side and instance-side methods defined in the API. Add `stdlib/src/BindingsView.bt` implementing the Dictionary protocol (`at:`, `at:put:`, `removeKey:`, `includesKey:`, `keys`, `values`, `size`, `do:`, `printOn:`).

No binding injection: `Session` is reachable as a class name through the class registry, no special-case handling. No protected-name additions, no `injected_ws_keys` changes.

### Phase 4: `Workspace currentSession` / `hasSession` (S)

Add `currentSession` and `hasSession` to `WorkspaceInterface` (Beamtalk class), each delegating to a one-line primitive that reads the same `beamtalk_session_pid` process dictionary key seeded in Phase 1. Returns the same value as `Session current` (or its boolean).

### Phase 5: Remove `:bindings` and `:clear` meta-commands (M)

Delete the `<<"bindings">>` and `<<"clear">>` op handlers in `beamtalk_repl_ops_eval.erl` and remove their entries from `beamtalk_repl_ops_dev.erl`'s op map. Update `surface-parity.md` rows for `bindings` and `clear` to cite `via Session bindings` / `via Session clear`.

**Bindings-changed push is preserved; only the fetch moves.** The `{bindings_changed, SessionId}` pub/sub (`beamtalk_bindings_events`, fired by the shell after each successful eval) is *not* removed — the VS Code sidebar still relies on it to know *when* to refresh. What changes is *how* the client fetches after the notification: it moves from the removed `bindings` op to `evaluate "Session bindings keys"` (and `Session globals keys`) carrying the user's `session` id, or to `Session withId:` + instance reads. This is a client-side change in the VS Code extension and must land with this phase, not after it — otherwise the sidebar refreshes against a dead op.

**Test migration is non-trivial.** Migrating `:bindings` / `:clear` from meta-commands to evaluated expressions changes the protocol op (`bindings`/`clear` → `eval`) and therefore the response shape (a bindings map → an eval result value). Existing `tests/repl-protocol/cases/*.btscript` cases that assert on the old op responses need real rewrites, not find-replace — audit and enumerate them before locking the estimate. Add a new e2e btscript case covering the full Session API including cross-session read access (`Session withId:`) and cross-session write rejection.

## Migration Path

The `:bindings` and `:clear` meta-commands are removed in Phase 5; the replacement API is shipped in Phases 1–4 first so they land in the same release.

| Before | After |
|--------|-------|
| `:bindings` | `Session bindings keys` (session locals only) <br> `Session globals keys` (workspace globals) <br> ⚠️ **behaviour change:** `:bindings` today (via `binding_names/0`) *shows* `bind:as:` names mixed in; under this ADR they move to `Session globals` and no longer appear in `Session bindings`. |
| `:clear` | `Session clear` |
| (no equivalent) | `Session bindings at: #x put: 99` (live mutation) |
| (no equivalent) | `Session bindings removeKey: #x` |
| (no equivalent) | `Session resolve: #name` (REPL debugging tool) |
| (no equivalent) | `Workspace currentSession` / `Session current` |
| (no equivalent) | `Session withId: aSessionId` (LSP cross-session) |

The meta-commands existed only on the REPL surface. Removing them is a small loss for muscle-memory CLI users (one `migrate_to` shell-side error message in the release notes is sufficient) and a strict gain for MCP and WebSocket clients, which now have a Beamtalk-native path to per-session state with both reads and write-through mutations.

## References

- [BT-2092](https://linear.app/beamtalk/issue/BT-2092/first-class-session-object-walkable-binding-layers-smalltalk-style) — driving issue
- [BT-2083](https://linear.app/beamtalk/issue/BT-2083/surface-only-audit-decide-promote-or-lock-for-asymmetric-ops) — surface audit that un-deprecated `:bindings` / `:clear` and filed BT-2092
- ADR 0040 — Workspace-Native REPL Commands (the precedent for moving meta-commands onto Beamtalk objects)
- ADR 0010 — Global Objects and Singleton Dispatch (established the `Beamtalk` / `Workspace` / `Transcript` bindings that form the workspace globals layer)
- ADR 0004 — Persistent Workspace Management (workspace lifecycle context)
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_eval.erl:62-77` — current `:bindings` / `:clear` handlers
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_shell.erl:155-244` — shell init and binding lifecycle
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_shell.erl:432` — `inject_workspace_bindings/1` (the actual injection of workspace globals into session bindings at init)
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl:765-786` — `handle_session_bindings/1` (resolves singletons + `bind:as:` entries that injection copies in)
- `runtime/apps/beamtalk_workspace/src/beamtalk_session_table.erl` — session registry
- Pharo Workspace `bindings` accessor; Squeak `Environment`; IPython `get_ipython()` — prior art for first-class binding scope objects
