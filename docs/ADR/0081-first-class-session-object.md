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

Each REPL connection gets a `beamtalk_repl_shell` gen_server, supervised under `beamtalk_session_sup` and tracked in the `beamtalk_sessions` ETS table (`beamtalk_session_table`). The shell holds session-local bindings in `beamtalk_repl_state`, refreshed with workspace globals before each eval (`refresh_ws_bindings/1` in `beamtalk_repl_shell.erl:441`). Eval workers spawn from the shell with a snapshot of state, returning updated state when they complete.

The CLI REPL has one session. WebSocket and MCP clients can have many — `beamtalk_session_sup` already supports multi-session, and `beamtalk_repl_ops_session.erl` exposes a `sessions` protocol op listing live shells. But no Beamtalk class represents an individual session.

### Constraints

- **Per-process state.** Session locals are owned by the shell PID; any object representation must respect that ownership boundary.
- **Eval ordering.** Eval workers run in spawned worker processes with a state snapshot. Mutations to session bindings during an eval interact with the shell's state-update flow.
- **Hot reload safety.** Session locals must persist across `Workspace sync` and class reloads (current behaviour).
- **Existing infrastructure.** `beamtalk_session_table`, `beamtalk_session_sup`, and the `sessions` protocol op already exist — the design should build on them rather than replace.
- **Surface parity.** `:bindings` / `:clear` work in the REPL surface only; the object-level replacement must be reachable from MCP `evaluate`, LSP eval, and any future client.

## Decision

Introduce a new `Session` class — a lightweight `Object subclass` that wraps a session shell PID — and make it accessible via two paths:

1. **Per-session binding `Session`**, injected into the shell's binding map at startup (alongside `Transcript`, `Beamtalk`, `Workspace`). Each shell sees a `Session` binding pointing to *its own* session object.
2. **`Workspace currentSession`**, a method on the existing `WorkspaceInterface` singleton that resolves the current eval's session via process context. Returns `nil` outside a REPL eval context.

`Session` is **not** an Actor (gen_server). It is a value-shaped handle holding the shell PID, with primitive methods that delegate to the shell process via `gen_server:call`. This mirrors the `Supervisor` pattern (a Beamtalk object wrapping an OTP supervisor PID).

### API

```beamtalk
sealed typed Object subclass: Session
  field: pid :: Pid
  field: id :: String

  /// Session-local bindings only (the `x := 42` layer).
  bindings -> Dictionary(Symbol, Object) =>
    (Erlang beamtalk_session_primitives) bindings: self.pid

  /// Workspace globals visible to this session (singletons + bind:as:).
  globals -> Dictionary(Symbol, Object) =>
    (Erlang beamtalk_session_primitives) globals

  /// Walk binding layers, return first match or nil.
  /// Lookup order: session locals → workspace globals.
  resolve: aName :: Symbol -> Object =>
    (Erlang beamtalk_session_primitives) resolve: aName for: self.pid

  /// Clear session-local bindings. Workspace globals remain.
  clear -> Nil =>
    (Erlang beamtalk_session_primitives) clear: self.pid

  /// Layer name symbols, in resolution order.
  layers -> List(Symbol) => #(#session #workspace)

  /// Stable session identifier (matches the protocol session ID).
  id -> String => self.id
```

### REPL Examples

```
beamtalk> x := 42
42
beamtalk> Session bindings
#{#x => 42}
beamtalk> Session globals keys
#(#Transcript #Beamtalk #Workspace #Session)
beamtalk> Session resolve: #x
42
beamtalk> Session resolve: #Transcript
#TranscriptStream<default>
beamtalk> Session resolve: #notDefined
nil
beamtalk> Session layers
#(#session #workspace)
beamtalk> Workspace currentSession id
"abc123-..."
beamtalk> Session clear
nil
beamtalk> Session bindings
#{}
beamtalk> Session globals keys
#(#Transcript #Beamtalk #Workspace #Session)
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
LSP completions can call `Session resolve: aSymbol` to mirror the runtime resolution path exactly, instead of duplicating the merge-and-shadow logic. The MCP server can call `Session bindings` directly via `evaluate` to display per-session state in the inspector panel.

## Steelman Analysis

### Option B: Workspace Extension (rejected)

**Newcomer:** "I already know `Workspace` from `Workspace classes` and `Workspace actors`; adding `Workspace sessionBindings` is one more method on a familiar object. No new concept to learn."

**Smalltalk purist:** "`Workspace` is the natural place for workspace-state operations. Splitting Session out is a needless ontological commitment."

**BEAM veteran:** "Function calls on a singleton are simpler than navigating to a per-PID object. Less indirection in the dispatch path."

**Operator:** "Fewer object types to debug. `Workspace` is already the operator's entry point."

**Language designer:** "Adding methods is cheaper than adding classes. YAGNI applies."

**Why not adopted:** Single-session ergonomics break in multi-session deployments. `Workspace sessionBindings` cannot answer "which session?" without extra parameters or context-aware lookup. The first-class object generalises naturally to `Workspace sessions collect: [:s | s bindings]`, which Option B cannot express.

### Option C: Recursive Scope Chain (rejected)

**Newcomer:** "Once I learn the parent pattern, I can walk any depth — just like prototype chains in JavaScript."

**Smalltalk purist:** "Self's slot-chain model is the most principled scoping mechanism in any object language. This adopts proven prior art."

**BEAM veteran:** "A uniform parent walk is easier to implement recursively than a fixed two-layer structure."

**Operator:** "Depth-flexible scopes future-proof the design — package-level scopes, module-level scopes, etc., all fit without redesign."

**Language designer:** "Most elegant. Layers are a special case of a more general scope-chain abstraction."

**Why not adopted:** Premature generalisation. We have two layers and no concrete plan for a third. The cost (a `Scope` class hierarchy, recursive resolution semantics, tooling for chain inspection) is paid up front for hypothetical future flexibility. If a third layer appears, `Session` can grow a `parent` accessor non-breakingly.

### Tension Points

- **Newcomers** would mildly prefer Option B (fewer concepts) but are not harmed by Option A — `Session` is itself a small, learnable concept.
- **Operators** prefer Option A as soon as multi-session is a real concern; Option B becomes brittle.
- **Language designers** are split between A and C; the deciding factor is current need versus speculative need.

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

- **One more top-level binding name.** `Session` joins `Transcript`, `Beamtalk`, `Workspace` as an injected per-session global. Users could shadow it with a local; the runtime should warn when that happens (covered by existing `bind:as:` conflict checks).
- **`Session clear` during eval has subtle semantics.** The eval worker has a state snapshot; mutations propagate via the worker's returned state. The implementation must ensure `Session clear` clears in a way that survives the worker's state writeback, either by routing through the worker's state thread or by post-eval reconciliation. This is an implementation detail, not a design flaw, but it requires care.
- **Outside-REPL `nil`.** `Workspace currentSession` returns `nil` in compiled program code, which means `Workspace currentSession bindings` raises `does_not_understand` (correct DNU semantics, but a footgun if users assume the API is always available). Documentation must call this out.

### Neutral

- **`bind:as:` semantics unchanged.** Continues to write to the workspace ETS layer. `Session bindings` will not include `bind:as:` entries (those appear in `Session globals`). This is a clarification, not a behaviour change.
- **Session persistence across sync unchanged.** Existing behaviour preserved: session locals survive `Workspace sync`.
- **Protocol surface unchanged initially.** The existing `bindings` and `clear` ops continue to work, redirected to `Session bindings` / `Session clear` semantics. Hard removal is a follow-up after the new API has soaked.

## Implementation

The implementation breaks down into four phases. Each is independently shippable.

### Phase 1: Runtime primitives module

Create `beamtalk_session_primitives.erl` with the four primitive operations:
- `bindings/1` — gen_server:call to the shell, returning session-locals only (with workspace-injected keys removed via `injected_ws_keys` tracking already present in `beamtalk_repl_state`).
- `globals/0` — calls `beamtalk_workspace_interface_primitives:get_session_bindings/0`, the existing workspace-globals accessor.
- `resolve/2` — combines the two; session locals first, workspace globals second.
- `clear/1` — gen_server:call to the shell's existing `clear_bindings` handler.

EUnit tests for each. Existing infrastructure (`beamtalk_repl_shell`, `beamtalk_session_table`) is reused.

### Phase 2: Stdlib `Session` class and binding injection

Add `stdlib/src/Session.bt` with the API defined above. Extend `beamtalk_workspace_config:singletons/0` and shell init (`beamtalk_repl_shell:init/1`) to inject a `Session` binding pointing to a Session object constructed with the shell's PID and session ID. Update `beamtalk_workspace_interface_primitives:handle_session_bindings/1` to include `Session` in the workspace-injected keys (so it propagates correctly through `clear` and refresh paths).

### Phase 3: `Workspace currentSession` and process-context resolution

Add `currentSession` to `WorkspaceInterface` (Beamtalk class) with an Erlang primitive that:
1. Reads the calling process's session PID from process dictionary, or
2. Walks up to find the shell PID via `$ancestors` if direct lookup fails, or
3. Returns `nil` if no session is in scope.

Eval worker spawn must seed the worker's process dictionary with the shell PID for the lookup to work.

### Phase 4: Migration and meta-command deprecation

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
