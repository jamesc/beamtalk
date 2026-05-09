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
  id -> String => self.id
```

`BindingsView` is a small Dictionary-protocol-compatible class backed by primitives that read/write session or workspace state. It implements `at:`, `at:put:`, `removeKey:`, `includesKey:`, `keys`, `values`, `size`, and `do:`. Mutations via `at:put:` / `removeKey:` are write-through.

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

The class-side methods (`Session bindings`) operate on the calling process's session — semantically "my session". The instance-side methods (`aSession bindings`) operate on whichever session the value represents, including ones owned by other connections. This is the load-bearing reason for instance methods on `Session`: cross-session access from tooling.

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
beamtalk> Session globals at: #MyTool put: someActor
nil
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

### Open Design Questions — Resolved

| Question | Decision | Rationale |
|----------|----------|-----------|
| Singleton or per-PID? | **Per-PID** | CLI has one session, WebSocket/MCP clients can have many. Class-side methods use process-context resolution to find the calling session; instance methods carry their own session ID for cross-session access. |
| What outside REPL eval? | **`Session current` returns `nil`**; class-side methods like `Session bindings` raise a structured `no_session` error. | Compiled program code has no session. Returning a stub or nil from `bindings` would mask the meaningful absence; an error is louder. |
| Persist across `:sync` / reload? | **Yes** — unchanged from current behaviour. | Session locals are shell-process state, owned independently of class definitions. Sync rebuilds modules; the session keeps its bindings. |
| Are bindings/globals views live or snapshots? | **Live views.** Mutations write through. | Matches Pharo semantics (`Smalltalk globals at:put:`). A snapshot would force a parallel setter API (`Session at:put:`); the live view collapses both into one Dictionary protocol. |
| Layer-walking abstraction? | **Dropped.** No `layers` method, no recursive `parent`. | Beamtalk has settled on exactly two layers; package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer. A `layers` constant would be pure decoration. |
| `bind:as:` interaction? | **Unchanged.** `Session globals at:put:` is equivalent to `Workspace bind:as:`, including conflict checks. | One write path through the existing primitives; the new view is sugar over the established API. |
| Cross-session access? | **Instance methods + `Session withId:`** | LSP/VS Code completion sessions need to query the user's session. Class-side methods are "my session"; instance methods are "that session". |

## Prior Art

| System | Binding model | What we adopt | What we reject |
|--------|---------------|---------------|----------------|
| **Pharo `Smalltalk current` / `Date today` / `Time now`** | Class-side methods that return a "current" instance — the class itself is the entry point. | The class-as-entry-point pattern (`Session bindings` is a class-side message, like `Date today`). | None — this is the load-bearing prior art for Option F. |
| **Pharo / Squeak Workspace** | `Dictionary` of bindings on the workspace, mutable via `at:put:`, with fallback to `Smalltalk globals`. Methods: `bindings`, `bindingOf:`, `removeBinding:`, `resetBindings`. | The mutable-Dictionary shape (`Session bindings at:put:` writes through, matching `Smalltalk globals at:put:`). | Pharo merges workspace and session — Beamtalk has separate workspace (shared) and session (per-PID) layers because of multi-client BEAM hosting. |
| **Pharo Environment** | Wraps a `Dictionary` of globals; multiple Environments allowed. | Layer reification via named accessors. | Beamtalk does not need user-creatable environments — workspace is bootstrapped once per BEAM node. |
| **Newspeak** | Lexical nesting only; no global namespace; capability passing via constructor parameter. | The principle that scope is explicit, not magic. | Beamtalk has Smalltalk-style globals (`Transcript`, classes); a fully-lexical model would invalidate the existing namespace design. |
| **Self** | Parent slot chain; scope walks via `parent*` slots. | Layer ordering as a first-class concept (`layers` returns the resolution order). | Recursive `parent` chains overstate the complexity for two layers. |
| **IPython `get_ipython()`** | Shell singleton with `user_ns` dict, `who_ls()`, `reset()`. | The "current session is reachable through a global accessor" pattern (`Workspace currentSession`). | A single mutable dict obscures layer ownership; we keep layers separate. |
| **Ruby `Binding`** | First-class scope object. `binding` returns the current scope; `Binding#local_variables` lists names. Can be passed for scoped expression execution. Closest prior art to the `Session` design. | A scope object that can be inspected and passed around. | Ruby's Binding captures lexical scope (closures); Beamtalk's Session wraps a session process, not a lexical frame. |
| **Erlang shell `b()` / `f()`** | `b()` lists shell bindings; `f()` clears all; `f(X)` clears one. Shell-only built-in commands, not callable from program code. | Demonstrates the universal need to inspect REPL state. | Shell commands, not values — cannot be composed, stored, or called from non-shell code. Exactly the problem this ADR solves. |
| **Elixir `binding()`** | Macro returning current bindings as a keyword list. | Treating bindings as inspectable data. | A macro/keyword-list shape is non-Smalltalk; we use Dictionary returns and explicit objects. |

## User Impact

**Newcomer (Python/JS background).**
`Session` joins `Transcript`, `Beamtalk`, `Workspace` as a small, discoverable set of always-available objects. Tab-completion on `Session` lists every session operation. Discovering "how do I see my variables" via `Session bindings` is more direct than memorising a `:b` meta-command.

**Smalltalk developer.**
The session-as-object model is recognisably Smalltalk: `Session` is to a Beamtalk shell what `Smalltalk` is to a Pharo image. The two-layer split (session vs. workspace) is more honest about multi-client BEAM hosting than Pharo's single-image model.

**Erlang/BEAM developer.**
`Session` maps cleanly to the existing `beamtalk_repl_shell` gen_server. The PID-handle pattern is familiar from `gen_server:call/2` and matches the `Supervisor` wrapper. Multi-session debugging (MCP, WebSocket) gets a first-class object to work with.

**Production operator.**
`Workspace sessions` (a follow-up extension) can return a list of `Session` objects, each individually inspectable. Compared to opaque PIDs in `supervisor:which_children`, this gives operators a structured view of who is connected and what they have bound.

**Tooling developer (LSP / VS Code).**
The MCP server can call `Session bindings` and `Session globals` via `evaluate` to display per-session state in the inspector panel. Tooling that needs the merged binding map (e.g. completions) merges those two Dictionaries in resolution order — no separate primitive needed.

## Steelman Analysis

### Option B: Workspace Extension (rejected)

**Newcomer:** "I already know `Workspace` from `Workspace classes` and `Workspace actors`; adding `Workspace sessionBindings` is one more method on a familiar object. No new concept to learn, no new top-level binding to discover."

**Smalltalk purist:** "Adding methods to `Workspace` matches how `Smalltalk` accumulates introspection in Pharo — `Smalltalk globals`, `Smalltalk allClasses`, `Smalltalk current`. Why is sessions special enough to warrant its own class?"

**BEAM veteran:** "`Workspace sessionBindings` could use the same process-context resolution that `Workspace currentSession` uses anyway. The dispatch cost is identical; you're just spelling it differently. For the 99% case — introspecting your own session — Option B is shorter and avoids the shadowing-via-`:=` footgun entirely (no injected binding, nothing to shadow)."

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
- **Mutation during eval has ordering constraints.** Both `Session clear` and `Session bindings at:put:` mutate shell state via gen_server calls. The eval worker holds a state snapshot; without reconciliation the worker's writeback would overwrite the mutation. The implementation must add a `pending_mutations` queue to `beamtalk_repl_state`: mutations append to the queue (instead of being applied directly); the eval-result handler drains the queue after merging the worker's state. This ensures `Session bindings at: #x put: 99. x + 1` evaluates `x + 1` against the original `x` (the mutation takes effect for the *next* eval), which matches user expectations that an expression sees its own pre-state.
- **Outside-REPL behaviour.** `Session current` returns `nil`; class-side methods like `Session bindings` raise a structured `no_session` error. Library code that wants graceful degradation must check `Workspace hasSession` first.
- **`Session clear` is asymmetric with the view.** `Session bindings removeKey: #x` removes one binding; `Session clear` removes all. There is no `Session bindings clear` because the view's protocol is generic Dictionary semantics — `Session bindings` returns a *view*, not a Dictionary the user owns, so a `clear` on the view would be ambiguous (clear-the-view or clear-the-state). Keeping `Session clear` as a top-level method avoids this ambiguity.

### Neutral

- **`bind:as:` semantics unchanged.** Continues to write to the workspace ETS layer. `Session bindings` will not include `bind:as:` entries (those appear in `Session globals`). This is a clarification, not a behaviour change.
- **Session persistence across sync unchanged.** Existing behaviour preserved: session locals survive `Workspace sync`.
- **Protocol `bindings` / `clear` ops are removed** in Phase 5, alongside the rest of the work. The Session API ships first (Phases 1–4), so when the meta-commands disappear the replacement is already in place. There is no deprecation window — single coordinated release.

## Implementation

The implementation breaks down into five phases. Each is independently shippable.

### Phase 1: Process-context plumbing (S)

Modify `beamtalk_repl_shell:handle_call({eval, _}, ...)` to seed the eval worker's process dictionary with `beamtalk_session_pid` (the shell's `self()`) and `beamtalk_session_id` (the protocol session ID) before calling `do_eval`. This makes the calling session reachable from any primitive without explicit threading.

Add `pending_mutations` to `beamtalk_repl_state` — a queue of `{op, key, value}` tuples. Add an applicator that drains the queue at eval-result time, after merging the worker's returned state. This is the foundation for `Session clear` and `Session bindings at:put:` correctness during eval.

EUnit tests for the worker spawn seeding and the pending-mutation drain ordering.

### Phase 2: Runtime primitives module (M)

Create `beamtalk_session_primitives.erl` in `runtime/apps/beamtalk_workspace/src/`. Each primitive resolves the session via `get(beamtalk_session_pid)` (or the explicit Session instance for the `*For:` variants):

- `current/0` — returns a Session value with the calling session's PID and ID, or `nil`.
- `withId/1` — looks up a session by protocol ID via `beamtalk_session_table:lookup/1`, returns Session or nil.
- `bindingsView/0` — returns a `BindingsView` value tagged for session-local scope, bound to the calling session.
- `globalsView/0` — returns a `BindingsView` value tagged for workspace scope (shared across sessions).
- `bindingsViewFor/1`, `clearFor/1` — instance variants (target a specific Session value).
- `resolve/1`, `resolve/2` — walk session locals first, then workspace globals; return value or nil.
- `clear/0`, `clearFor/1` — enqueue a clear mutation via the shell's pending-mutations API (Phase 1).
- `id/0` — reads `beamtalk_session_id` from process dictionary.

Plus the BindingsView read/write primitives: `view_at/2`, `view_at_put/3`, `view_remove/2`, `view_keys/1`, `view_size/1`. Session-scope writes enqueue against the shell's pending-mutations queue; workspace-scope writes route through `beamtalk_workspace_interface_primitives:bind/2` and `unbind/1`, inheriting protected-name conflict checks.

EUnit tests for each primitive, including cross-session access via `withId/1`.

### Phase 3: Stdlib `Session` and `BindingsView` classes (M)

Add `stdlib/src/Session.bt` with the class-side and instance-side methods defined in the API. Add `stdlib/src/BindingsView.bt` implementing the Dictionary protocol (`at:`, `at:put:`, `removeKey:`, `includesKey:`, `keys`, `values`, `size`, `do:`, `printOn:`).

No binding injection: `Session` is reachable as a class name through the class registry, no special-case handling. No protected-name additions, no `injected_ws_keys` changes.

### Phase 4: `Workspace currentSession` / `hasSession` (S)

Add `currentSession` and `hasSession` to `WorkspaceInterface` (Beamtalk class), each delegating to a one-line primitive that reads the same `beamtalk_session_pid` process dictionary key seeded in Phase 1. Returns the same value as `Session current` (or its boolean).

### Phase 5: Remove `:bindings` and `:clear` meta-commands (S)

Delete the `<<"bindings">>` and `<<"clear">>` op handlers in `beamtalk_repl_ops_eval.erl` and remove their entries from `beamtalk_repl_ops_dev.erl`'s op map. Update `surface-parity.md` rows for `bindings` and `clear` to cite `via Session bindings` / `via Session clear`. Update any existing e2e tests that exercised the meta-commands to use the new Session API. Add an e2e btscript test under `tests/repl-protocol/cases/` covering the full Session API including cross-session access (`Session withId:`).

## Migration Path

The `:bindings` and `:clear` meta-commands are removed in Phase 5; the replacement API is shipped in Phases 1–4 first so they land in the same release.

| Before | After |
|--------|-------|
| `:bindings` | `Session bindings keys` (session locals only) <br> `Session globals keys` (workspace globals) |
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
- ADR 0010 — Global Objects and Singleton Dispatch (the singleton injection mechanism `Session` reuses)
- ADR 0004 — Persistent Workspace Management (workspace lifecycle context)
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_eval.erl:62-77` — current `:bindings` / `:clear` handlers
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_shell.erl:155-244` — shell init and binding lifecycle
- `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl:765-786` — workspace-globals injection
- `runtime/apps/beamtalk_workspace/src/beamtalk_session_table.erl` — session registry
- Pharo Workspace `bindings` accessor; Squeak `Environment`; IPython `get_ipython()` — prior art for first-class binding scope objects
