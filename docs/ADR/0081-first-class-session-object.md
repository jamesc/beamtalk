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
- **Call safety.** Session primitives call back to the shell gen_server via `gen_server:call`. This is safe from eval worker processes (the shell is in `noreply` state, processing its mailbox). It is NOT safe from `handle_call`/`handle_cast` handlers running directly on the shell process (would deadlock). Session methods must only be called from eval context, never from shell-internal dispatch.

## Decision

Introduce a new `Session` class — a lightweight `Object subclass` that wraps a session shell PID — and make it accessible via two paths:

1. **Per-session binding `Session`**, injected into the shell's binding map at startup (alongside `Transcript`, `Beamtalk`, `Workspace`). Each shell sees a `Session` binding pointing to *its own* session object.
2. **`Workspace currentSession`**, a method on the existing `WorkspaceInterface` singleton that resolves the current eval's session via process context. Returns `nil` outside a REPL eval context. A companion **`Workspace hasSession`** predicate returns `true` or `false`, allowing library code to guard session-dependent logic without nil-checking.

`Session` is **not** an Actor (gen_server). It is a handle object whose methods delegate to the shell process via `gen_server:call`. Similar to how `Ets` wraps a table reference without exposing it as a Beamtalk field, Session's internal shell PID is managed by the primitives module, not stored in a user-visible `field:`.

### API

```beamtalk
sealed typed Object subclass: Session

  /// Session-local bindings only (the `x := 42` layer).
  bindings -> Dictionary(Symbol, Object) =>
    (Erlang beamtalk_session_primitives) bindings

  /// Workspace globals visible to this session (singletons + bind:as:).
  globals -> Dictionary(Symbol, Object) =>
    (Erlang beamtalk_session_primitives) globals

  /// Walk binding layers, return first match or nil.
  /// Lookup order: session locals → workspace globals.
  /// Primarily a REPL debugging tool: answers "where does this name
  /// resolve from?" interactively. Compiled code uses normal scope
  /// resolution; this method is for introspection, not dispatch.
  resolve: aName :: Symbol -> Object =>
    (Erlang beamtalk_session_primitives) resolve: aName

  /// Clear session-local bindings. Workspace globals remain.
  clear -> Nil =>
    (Erlang beamtalk_session_primitives) clear

  /// Layer name symbols, in resolution order. Stable — Beamtalk's binding
  /// model has exactly two layers (session locals over workspace globals)
  /// and no plan for additional layers. Package-level encapsulation
  /// (ADR 0070) is class-scoped, not a binding-resolution layer.
  layers -> List(Symbol) => #(#session, #workspace)

  /// Stable session identifier (matches the protocol session ID).
  id -> String => (Erlang beamtalk_session_primitives) id
```

The shell PID is not stored in Beamtalk-level fields. Primitives resolve the calling session from process context (process dictionary seeded during eval worker spawn), matching the pattern used by `Ets` and other runtime-backed objects.

**`Session` itself is a system-injected binding.** It does not appear in `Session bindings` (that shows user-typed locals only) and does not appear in `Session globals` (that shows workspace-level globals from ETS). It is part of the `injected_ws_keys` set — visible during normal name resolution, but excluded from the per-layer introspection views. `Session resolve: #Session` returns the Session object (it walks the merged binding map). This is consistent with how `Transcript`, `Beamtalk`, and `Workspace` are treated by the existing `:bindings` meta-command, which already strips system bindings from its output.

### REPL Examples

```text
beamtalk> x := 42
42
beamtalk> Session bindings
#{#x => 42}
beamtalk> Session globals keys
#(#Transcript, #Beamtalk, #Workspace)
beamtalk> Session resolve: #x
42
beamtalk> Session resolve: #Transcript
#<TranscriptStream>
beamtalk> Session resolve: #notDefined
nil
beamtalk> Session layers
#(#session, #workspace)
beamtalk> Workspace currentSession id
"abc123-..."
beamtalk> Session clear
nil
beamtalk> Session bindings
#{}
beamtalk> Session globals keys
#(#Transcript, #Beamtalk, #Workspace)
```

### Error Examples

Outside a REPL eval (compiled program code), `Workspace currentSession` returns `nil`:

```beamtalk
// In a .bt file run via `beamtalk run`:
Workspace currentSession  // => nil
Workspace currentSession bindings
// => #beamtalk_error{kind: does_not_understand, selector: #bindings, receiver: nil}
```

Calling `Session` from a context where it is not bound:

```beamtalk
// In compiled program code, Session is not in scope:
Session bindings
// => #beamtalk_error{kind: undefined_variable, name: #Session}
```

### Open Design Questions — Resolved

| Question | Decision | Rationale |
|----------|----------|-----------|
| Singleton or per-PID? | **Per-PID** | CLI has one session, but WebSocket/MCP clients have many. Each shell injects its own `Session` binding; `Workspace currentSession` resolves from eval context. |
| What outside REPL eval? | **`nil`** for `Workspace currentSession`; `Session` binding not present. | Compiled program code has no session. Returning a stub object would mask the meaningful absence. |
| Persist across `:sync` / reload? | **Yes** — unchanged from current behaviour. | Session locals are shell-process state, owned independently of class definitions. Sync rebuilds modules; the session keeps its bindings. |
| Expose `parent` for layer walking? | **No, not initially.** | Two layers; `bindings` and `globals` cover both. Parent links can be added later if a third layer (e.g. per-module scope) appears. Keeps the initial surface small. |
| `bind:as:` interaction? | **Unchanged.** `Workspace bind:value as:#name` continues to write to the workspace ETS layer. Visible via `Session globals`. | `bind:as:` is workspace-scoped (shared across sessions). Confirms the layer separation: session = per-PID, workspace = shared. |

## Prior Art

| System | Binding model | What we adopt | What we reject |
|--------|---------------|---------------|----------------|
| **Pharo / Squeak Workspace** | `Dictionary` of bindings on the workspace, with fallback to `Smalltalk globals`. Methods: `bindings`, `bindingOf:`, `removeBinding:`, `resetBindings`. | The flat-Dictionary shape and the `bindings` accessor name. | Pharo merges workspace and session — Beamtalk has separate workspace (shared) and session (per-PID) layers because of multi-client BEAM hosting. |
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

**Why not adopted:** The BEAM-veteran argument is the strongest — Option B *works* for the common case and avoids the injected-binding shadowing problem. Two factors push us to A despite this:

1. **Pass-by-reference.** `Session` as a value can be stored, passed to library code, or returned from `Workspace sessions`. Option B cannot express `analyzer analyze: aSession` — there's no first-class session value to pass.
2. **Namespace coherence.** `Workspace` already mixes actors, classes, supervisors, packages, sync, test, bind:as:, and load. Adding `sessionBindings`, `sessionGlobals`, `clearSession`, `resolveBinding:` would push it past discoverable scale. A dedicated `Session` class keeps each surface focused.

The shadowing footgun (Option B's genuine advantage) is mitigated by the proposed shadowing warning. The ergonomic loss in Option B (no first-class session value) is permanent and not mitigable.

### Option C: Recursive Scope Chain (rejected)

**Newcomer:** "Once I learn the parent pattern, I can walk any depth. It's just like prototype chains in JavaScript or `__getattr__` chains in Python — a familiar mental model from outside the Smalltalk world."

**Smalltalk purist:** "Self's slot-chain model is the most principled scoping mechanism in any object language. Walking `parent` is more uniform than special-casing each layer with a named accessor (`bindings`, `globals`, ...). Adopt proven prior art."

**BEAM veteran:** "Process linking already gives BEAM a parent chain (`$ancestors`). A uniform `parent` accessor on Scope objects mirrors what's already happening at the OTP level — recursive walks are idiomatic on BEAM."

**Operator:** "If we ever introduce per-package, per-module, or per-actor scopes, depth-flexible scopes absorb them without redesign. Today's two-layer model becomes tomorrow's five-layer model without breaking callers."

**Language designer:** "Most elegant. Two layers IS the special case; the general case is a chain. Designing for the general case prevents future entrenchment of the wrong abstraction."

**Why not adopted:** Beamtalk has explicitly settled on a two-layer binding model — session locals over workspace globals — with no plans for additional layers. Package encapsulation (ADR 0070) is class-scoped, not a binding-resolution layer; module scope is an Erlang concept invisible to Beamtalk users (per project memory). The general-case argument requires a third layer to ever materialise; given the design direction, that's a bet against settled decisions. The cost — a `Scope` class hierarchy, recursive resolution semantics, tooling for chain inspection, plus the foreign vocabulary of "scope" in a Smalltalk context — is paid for an extension we are choosing not to take.

### Tension Points

- **Common-case ergonomics vs. composability.** Option B genuinely wins for the introspect-your-own-session case (no injected binding to shadow, no new class to learn). Option A wins when sessions need to be passed as values or enumerated. The decision turns on whether composability matters enough to pay the shadowing-warning cost.
- **`Session` as injected binding** is the load-bearing footgun. Without it, Option A loses much of its ergonomic advantage (you'd always type `Workspace currentSession bindings`). With it, we add a name that can be shadowed by `:=`. The shadowing warning is the bridge.
- **Speculative future use.** `Workspace sessions collect: [:s | s bindings]` is not exercised by any current caller. We're paying class-design complexity for a use case that today only appears in the Consequences section. Honest assessment: if this stays unused for 12 months, the value of a first-class Session over Option B is substantially weaker.
- **Two layers is settled.** Option C's strongest argument (future layer flexibility) requires bet-against-settled-design. Both A and C agree the current need is two layers; A says "two named accessors", C says "n-arity chain". Given Beamtalk's commitment to two layers, A is the right level of generality.

## Alternatives Considered

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

- **One more top-level binding name.** `Session` joins `Transcript`, `Beamtalk`, `Workspace` as an injected per-session global. Users could shadow it via `Session := MyThing new` in the shell — the existing `bind:as:` conflict checks only protect workspace-level assignments, not shell-level `:=`. The implementation should add a guard in `beamtalk_repl_eval` that emits a compiler warning (not an error) when a user assignment shadows an injected system binding, consistent with the project's footgun-warning philosophy (emit a compiler warning whenever we detect a footgun, rather than leaving it as a known limitation).
- **`Session clear` during eval has ordering constraints.** The eval worker has a state snapshot; when it completes, the shell merges the worker's state back. A naive `gen_server:call(ShellPid, clear_bindings)` during eval would be overwritten by the worker's stale snapshot. The implementation must add a `clear_requested` flag to `beamtalk_repl_state`: `clear` sets the flag via the shell; eval-result processing checks the flag and applies the clear after merging the worker's state. This ensures `Session clear` followed by more expressions in the same eval works correctly (the clear takes effect for the *next* eval, not mid-expression).
- **Outside-REPL `nil`.** `Workspace currentSession` returns `nil` in compiled program code, which means `Workspace currentSession bindings` raises `does_not_understand` (correct DNU semantics, but a footgun if users assume the API is always available). Documentation must call this out.

### Neutral

- **`bind:as:` semantics unchanged.** Continues to write to the workspace ETS layer. `Session bindings` will not include `bind:as:` entries (those appear in `Session globals`). This is a clarification, not a behaviour change.
- **Session persistence across sync unchanged.** Existing behaviour preserved: session locals survive `Workspace sync`.
- **Protocol surface unchanged initially.** The existing `bindings` and `clear` ops continue to work, redirected to `Session bindings` / `Session clear` semantics. Hard removal is a follow-up after the new API has soaked.

## Implementation

The implementation breaks down into four phases. Each is independently shippable.

### Phase 1: Runtime primitives module (S)

Create `beamtalk_session_primitives.erl` in `runtime/apps/beamtalk_workspace/src/` with the four primitive operations. Each primitive reads the calling process's session PID from process dictionary (seeded during eval worker spawn in Phase 2):

- `bindings/0` — reads the process dictionary session PID, calls `beamtalk_repl_shell:get_bindings/1`, then strips workspace-injected keys using the `injected_ws_keys` tracking already present in `beamtalk_repl_state`. Returns session-locals only.
- `globals/0` — calls `beamtalk_workspace_interface_primitives:get_session_bindings/0`, the existing workspace-globals accessor.
- `resolve/1` — combines the two; session locals first, workspace globals second. Returns `nil` if name not found.
- `clear/0` — calls `beamtalk_repl_shell:clear_bindings/1` on the session PID. Note: during eval, this clears the shell's state via gen_server:call. The eval worker's snapshot is stale after clear, but clear is typically the final expression in an eval. If the cleared state is overwritten by the worker's return, the shell must reconcile by checking a `clear_requested` flag (set by the clear handler) during eval-result processing.
- `id/0` — reads the session ID from process dictionary.

EUnit tests for each primitive. Existing infrastructure (`beamtalk_repl_shell`, `beamtalk_session_table`) is reused.

### Phase 2: Stdlib `Session` class and binding injection (M)

Add `stdlib/src/Session.bt` with the API defined above. Session is NOT a workspace singleton (it varies per shell), so it cannot be added to `beamtalk_workspace_config:singletons/0`.

Injection path: modify `beamtalk_repl_shell:init/1` to construct a Session object (via `beamtalk_object:create/2`) with the shell's PID and session ID, and inject it as a binding alongside workspace globals. Add `'Session'` to the `injected_ws_keys` set so that `clear` preserves it and `Session bindings` excludes it from user-locals.

Seed process dictionary: modify the eval worker spawn (`handle_call({eval, ...})`) to set `beamtalk_session_pid` and `beamtalk_session_id` in the worker's process dictionary before calling `do_eval`. This allows Phase 1 primitives to resolve the session context.

Add `'Session'` to `is_protected_name/1` in `beamtalk_workspace_interface_primitives.erl` so `bind:as:` cannot shadow it.

### Phase 3: `Workspace currentSession` and process-context resolution (S)

Add `currentSession` to `WorkspaceInterface` (Beamtalk class) with an Erlang primitive that:
1. Reads `beamtalk_session_pid` from the calling process's dictionary, or
2. Returns `nil` if no session marker is set (compiled program code, non-REPL contexts).

This uses the same process dictionary key seeded in Phase 2 — no additional plumbing needed.

### Phase 4: Migration and meta-command deprecation (M)

Re-deprecate `:bindings` and `:clear` in `beamtalk_repl_ops_dev.erl` with `migrate_to` hints pointing at `Session bindings` and `Session clear`. Update `surface-parity.md` rows for `bindings` and `clear` to cite `via Session bindings` / `via Session clear`. Add an e2e btscript test under `tests/repl-protocol/cases/` covering the full Session API. Hard removal of the meta-commands is a separate follow-up issue once the new API has been validated in real use.

## Migration Path

For users of `:bindings` and `:clear`:

| Before | After |
|--------|-------|
| `:bindings` | `Session bindings` (session locals only) <br> `Session globals` (workspace globals) |
| `:clear` | `Session clear` |
| (no equivalent) | `Session resolve: #name` |
| (no equivalent) | `Workspace currentSession` |

The meta-commands continue to work during the deprecation window. CLI users see a `migrate_to` hint in the deprecated-op response. MCP and WebSocket clients gain a new capability (the meta-commands were REPL-only; the object API works on every surface).

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
