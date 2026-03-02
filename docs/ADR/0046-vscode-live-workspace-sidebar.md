# ADR 0046: VSCode Live Workspace Sidebar

## Status
Proposed (2026-03-02)

## Context

Beamtalk is an interactive-first language: actors persist in running workspaces, bindings accumulate across REPL sessions, and code is reloaded live without restarts. This live environment is the defining development experience.

Today the VSCode extension is a thin LSP client wrapper. It provides editing intelligence (completions, hover, diagnostics, go-to-definition) but has no awareness of running workspaces. A developer using the REPL in a terminal has no visibility into workspace state from within their editor — they cannot see what actors are running, what bindings exist, or what the Transcript is streaming.

The WebSocket infrastructure (ADR 0017) already supports everything needed:
- `bindings` — list session-local variable bindings
- `actors` — list running actors with class and PID
- `inspect` — get actor internal state
- `classes` — list loaded classes
- `transcript` push — server-initiated Transcript streaming
- `actors` push — server-initiated actor lifecycle events (spawned, stopped)

The gap is purely in the editor: nothing consumes this rich runtime data to surface it where developers work.

### What We Are Not Building Here

**ADR 0017 Phase 3** describes a Phoenix LiveView browser-based IDE with full workspace panes. That remains planned. This ADR describes a VSCode sidebar that:
1. Delivers the live development experience now, in the editor developers already use
2. Validates the information architecture (which panels, what data, what interactions) before committing to the larger browser UI project
3. Shares the same WebSocket protocol — the browser UI is additive, not a replacement

**ADR 0024 Tier 3** describes live workspace augmentation for LSP completions (type information from the running workspace). That is a separate concern; this ADR is about developer visibility and inspection, not completion quality. Tier 3 uses its own independent workspace WebSocket connection; the sidebar and Tier 3 both speak the same workspace WebSocket protocol but maintain separate, fully autonomous connections.

### Constraints

- The workspace is a **separate BEAM node** — not embedded in the editor process. Connection is always over WebSocket.
- Sessions are **ephemeral** — bindings are session-local and lost on disconnect. Actor state persists in the workspace.
- Workspaces **self-terminate on idle** (4-hour default). The sidebar must handle disconnection and reconnection gracefully.
- The extension already manages one connection (LSP). Adding a WebSocket connection must not destabilise the extension host.
- The `vscode-webview-ui-toolkit` was deprecated in January 2025. New views should use plain web components or a framework directly.

## Decision

The VSCode extension gains a **Beamtalk Workspace** sidebar container with two views (Phase 1), using a **hybrid TreeView + WebviewView** architecture, connected via a **direct WebSocket** from the extension host.

### Views

**1. Workspace Explorer (TreeView)**

A `TreeDataProvider` showing the live workspace state as a native VSCode tree:

```text
▾ WORKSPACE                    [● Connected]
  ▾ Bindings
      counter : Counter         [inspect]
      result  : 42
  ▾ Actors  (3 running)
    ▾ Counter  <0.234.0>        [inspect] [kill]
        value : 0
        spawned: 10:42:31
      Timer    <0.235.0>
      Logger   <0.240.0>
  ▾ Classes  (12 loaded)
      Counter
      Timer
      Logger
      ...
```

- Bindings section: shows bindings from the extension-owned REPL session. The extension started the REPL, knows the session ID, and queries `bindings` directly on that session. Refreshed on eval completion in the integrated terminal.
- Actors section: workspace-wide, updated via push events (`actors` channel)
- Classes section: workspace-wide, refreshed on `class-loaded` push event (see Protocol Changes below)
- Clicking an actor or binding sends `inspect` and expands a detail subtree
- Context menus: `reload` on classes, `kill` on actors, `inspect` on bindings

**2. Transcript (WebviewView)**

A streaming text view in the sidebar showing live Transcript output pushed from the workspace. Uses a `WebviewViewProvider` (sidebar-resident, not a floating panel) with a lightweight HTML view — no framework required, just a scrolling `<pre>` with auto-scroll, clear button, and a **max-lines cap** (e.g., 10,000 lines, matching the workspace's ring buffer). The cap is a safety requirement — a runaway actor writing to Transcript could otherwise freeze the webview with unbounded DOM growth.

Transcript is workspace-wide (shared across all sessions), matching the Smalltalk Transcript model.

**3. Connection lifecycle**

The extension owns the REPL session. There is no support for connecting to sessions started outside VSCode — if a user runs `beamtalk repl` in an external terminal, bindings from that session are not shown. Actors and classes are workspace-wide and always visible regardless.

> **Corner case — manual `beamtalk repl` in a VSCode integrated terminal**: If a user opens a VSCode integrated terminal and runs `beamtalk repl` themselves (rather than using the "Start REPL" command), the sidebar sees the workspace (actors, classes) but not their bindings. This is intentional: the extension doesn't know the session ID. A future expansion could monitor the active terminal's output stream for the `[beamtalk] session: <id>` line and adopt the session automatically — the Python extension does this to detect which conda/venv environment a user activates in a terminal, and the Azure CLI extension similarly detects login completions. This would close the gap for power users who prefer manual terminal control without changing the default ownership model.

The extension host manages a single `WorkspaceClient` per VSCode window:

```text
On "Start REPL" command (or auto-start on .bt file open):
  1. Read beamtalk.toml to find project root
  2. Run `beamtalk repl` in a new integrated terminal
  3. REPL prints session ID to stdout on connect (e.g. "[beamtalk] session: abc123")
  4. Extension captures session ID from terminal output
  5. Connect WebSocket to workspace, authenticate, subscribe to push channels
  6. Store session ID — used for bindings queries

On disconnect / workspace idle-timeout:
  → Show disconnected state in tree
  → Offer "Restart REPL" action

On terminal closed by user:
  → Sidebar shows disconnected state, session bindings cleared
```

Workspace persistence: a Beamtalk workspace (BEAM node) survives REPL disconnects (ADR 0004). If the extension reconnects after a terminal close, actors and classes persist. Bindings from the previous session are gone (ephemeral by design), and the new session starts empty — this is correct and expected.

### Connection Architecture: Direct WebSocket (not LSP-proxied)

The extension host opens the WebSocket connection directly, independent of the LSP server:

```text
VSCode Extension Host
  ├── LanguageClient  →  beamtalk-lsp (stdio)      [editing intelligence]
  └── WorkspaceClient →  ws://127.0.0.1:{port}/ws   [live runtime state]
```

The LSP connection and workspace connection are independent. The workspace sidebar works even if the LSP is restarting, and vice versa.

**Interaction with ADR 0024 Tier 3:** The LSP opens its own independent WebSocket connection to the workspace for Tier 3. The two connections have different purposes and are fully autonomous:

```text
VSCode Extension Host
  ├── LanguageClient  →  beamtalk-lsp (stdio)        [editing intelligence]
  └── WorkspaceClient →  ws://127.0.0.1:{port}/ws    [sidebar: session, bindings, actors, transcript]

beamtalk-lsp (Rust)
  └── WorkspaceTypeClient → ws://127.0.0.1:{port}/ws [Tier 3: class-loaded events, class hierarchy]
```

The LSP's connection is read-only and minimal — it subscribes to `class-loaded` push events, queries `classes` and class metadata on connect, and maintains an updated in-memory class hierarchy for completions. No REPL session is needed. The LSP discovers the workspace via `--print-sysroot` (BT-1010). This keeps the LSP autonomous and independent of the extension host's connection lifecycle.

### Refresh Strategy

- **Actor lifecycle**: event-driven via `actors` push channel — no polling
- **Transcript**: event-driven via `transcript` push — no polling
- **Bindings**: refreshed on eval completion in the extension-owned terminal session
- **Actor state (expanded)**: refreshed on expand and on manual refresh action, not continuously polled
- **Classes**: event-driven via `class-loaded` push event (see Protocol Changes below)

Most updates are event-driven via existing push channels. The extension owns the REPL terminal, so it can observe eval completion directly (terminal output parsing or `eval` op response). The only missing push event is `class-loaded` — see Protocol Changes below.

### Protocol Changes Required

One small addition to the existing protocol:

**`class-loaded` push event**: When any session loads, reloads, or eval-defines a class, the workspace broadcasts a push event to all WebSocket subscribers — analogous to the existing `actor_spawned`/`actor_stopped` push events. This lets the sidebar refresh the Classes section without polling.

No `bindings` session param is needed: the extension owns the REPL session and queries its own session's bindings directly. The `complete` op's session param pattern already exists and works the same way.

### Information Architecture (Informing ADR-0017 Phase 3)

This sidebar defines the panel structure that ADR 0017 Phase 3 should implement in the browser:

| Panel | Data Source | Update Trigger |
|-------|-------------|----------------|
| Workspace Explorer | `bindings`, `actors`, `classes` ops | push events + eval completion |
| Transcript | `transcript` push channel | server push |
| Inspector | `inspect` op | on demand (expand / click) |
| *(future)* Test Results | `test`/`test-all` ops | on demand |

The browser IDE should implement the same panels with the same data sources.

## Prior Art

### Jupyter VSCode Extension (Variables Panel)
Jupyter uses a React-based `WebviewPanel` for the Variables panel (legacy) and a native `TreeView` via `NotebookVariableProvider` for the newer debugger-integrated view. The key lesson: **TreeView integrates cleanly with native VSCode UI but can only render label+icon; WebviewPanel provides full rendering freedom at the cost of iframe overhead**. Jupyter ended up with both: tree for the debugger, webview for the rich data viewer. We adopt the same hybrid split — tree for structured state, webview for the streaming Transcript.

The Jupyter ownership model is the exact parallel: the extension **starts the kernel** and therefore always knows the session. We adopt this directly — the extension starts `beamtalk repl` in an integrated terminal and owns the session ID. There is no support for externally-started sessions, just as Jupyter's variables panel only reflects the kernel it started. Jupyter triggers variable refresh on cell execution completion, panel activation, and kernel restart — no polling. We adopt the same.

### Docker / Kubernetes Extensions (TreeView polling)
These extensions poll every few seconds with a configurable interval, but **only while the view is visible** (guarded by `onDidChangeVisibility`). We avoid polling entirely by using the workspace's existing push channels — the BEAM already pushes actor lifecycle events and Transcript output, so polling is unnecessary.

### DAP (Debug Adapter Protocol)
The Erlang LS and ElixirLS extensions use DAP for variable inspection during breakpoint debugging. DAP's `scopes` → `variables` → recursive `variablesReference` chain maps well to tree exploration. We do not use DAP here — Beamtalk's live workspace model is not a debugger (no breakpoints, no stepping, no paused execution). The workspace is always running; inspection is non-intrusive. DAP would be an impedance mismatch.

### Pharo VSCode Extension
The Pharo extension connects to a live Pharo image via TCP socket and uses DAP for variable inspection, but provides no image browser UI. The full Smalltalk browser experience is deferred. This confirms the prior art gap: **nobody has yet built a live Smalltalk-style workspace browser in VSCode**. We can be the first.

### Observer / LiveDashboard (Elixir)
The Elixir community uses Observer (standalone Wx GUI) or LiveDashboard (browser) for live OTP inspection. Neither is integrated into the editor. The absence is a noted gap in the community. Beamtalk's sidebar fills this gap — live actor inspection without leaving the editor.

## User Impact

### Newcomer (from Python/JS/Ruby)
- The sidebar makes the workspace tangible: "I can see my actors are running"
- Auto-connect means zero configuration — just `beamtalk repl` and the sidebar lights up
- Clicking an actor to inspect its state is more discoverable than typing `actors inspect: somePid` in the REPL
- Transcript in the sidebar means they don't need to understand `Transcript show:` output routing
- **Risk**: information overload if too much is shown by default. Mitigation: collapse Classes by default, show only top-level actor list until expanded.

### Smalltalk Developer
- The Workspace Explorer is a simplified System Browser — classes, instances, live state
- Transcript in the sidebar is familiar (Pharo has a Transcript window)
- The inspector is the Smalltalk Inspector — drill into object state by clicking
- The absence of a method browser is notable (the class browser shows loaded classes but not methods). This is Phase 2 scope.
- **Risk**: Smalltalk developers expect to be able to edit code in the inspector and send it to an object. That is REPL-native today; the sidebar is read-only inspection for now.

### Erlang/BEAM Developer
- The Actors panel is a simplified Observer process list scoped to the Beamtalk workspace
- `inspect` showing gen_server state is familiar from `:sys.get_state/1`
- Class reload from context menu is familiar from `c:l/1` in the Erlang shell
- **Risk**: they will miss the supervision tree view. That is future scope.

### Production Operator
- Zero runtime overhead — the workspace push infrastructure was already running
- The sidebar is a development tool only; it has no production-mode surface area
- Connection is localhost-only (inheriting workspace security model)

### Tooling Developer (ADR 0024 Tier 3)
- The LSP opens its own independent WebSocket connection for Tier 3 (live class hierarchy augmentation)
- The connection architecture is clean: extension host owns the sidebar session, LSP owns its own read-only type data connection
- Both discover the workspace independently — no relay complexity

## Steelman Analysis

### Option A: Hybrid TreeView + WebviewView (Recommended)
- 🧑‍💻 **Newcomer**: "The tree looks like every other VSCode sidebar I've used — files, git, extensions. Zero learning curve. I know how to expand nodes, right-click for context menus, and use keyboard navigation. A custom webview would make me learn a new UI."
- 🎩 **Smalltalk purist**: "TreeView for structured state + Transcript webview matches how Pharo separates the Inspector (structured drill-down) from the Transcript (streaming output). These are fundamentally different interaction patterns — combining them in one webview would flatten that distinction."
- ⚙️ **BEAM veteran**: "Actors as tree nodes with right-click `kill` and `inspect` is Observer-lite inside my editor. Observer is the single most useful debugging tool in the BEAM ecosystem and nobody has put it in an editor. This is exactly what I want."
- 🏭 **Operator**: "TreeView is low-memory and native — no bundled framework, no iframe overhead, no version conflicts with other extensions' webview toolkits. One less thing to maintain."
- 🎨 **Language designer**: "The extension owns the REPL session and the WebSocket is direct — clean ownership model. Event-driven refresh consumes push infrastructure that already exists. No new abstractions, no relay layers."

### Option B: Full WebviewPanel
- 🧑‍💻 **Newcomer**: "A rich HTML panel can show formatted object graphs, syntax-highlighted values, and inline visualisation. When I inspect a Dictionary, I want to see its key-value pairs formatted nicely — not `Dictionary (size: 3)` as a text label."
- 🎩 **Smalltalk purist**: "The whole POINT of Smalltalk is the live, visual, rich environment. A TreeView with text labels is a pale imitation of the Pharo Inspector, which renders each object with a custom presentation. If you build the WebviewPanel now using web components, those components transfer directly to ADR 0017 Phase 3 — you build the rich inspector once, not twice."
- ⚙️ **BEAM veteran**: "LiveDashboard is a rich web UI and it works brilliantly — tables, charts, process graphs, all in one surface. The BEAM community has proved that a web-based inspector is the right UX. A tree of strings is a step backward from what we already have in Observer."
- 🏭 **Operator**: "One consistent rendering surface is simpler to maintain than a hybrid. With a TreeView, you maintain TypeScript TreeDataProvider code AND a separate webview for Transcript. With a full webview, it's one rendering technology, one state model, one test surface."
- 🎨 **Language designer**: "A WebviewPanel forces you to design the inspector UI now, with real constraints — how to render a Dictionary, how to show actor state, how to handle deep nesting. That design work feeds directly into ADR 0017 Phase 3 even if the code doesn't transfer. You're making design decisions with production feedback, not in a vacuum. Starting with TreeView defers all those decisions and you'll face them cold when Phase 3 arrives."

### Option C: LSP-Proxied WebSocket
- 🧑‍💻 **Newcomer**: "One logical connection from the editor is simpler to understand. 'The language server handles everything' is a mental model that just works."
- 🎩 **Smalltalk purist**: "The LSP is the brain of the editor — completions, diagnostics, hover, navigation all flow through it. Adding workspace awareness means the LSP can do smart things: 'your code references an actor that isn't running', 'this class was modified at runtime and differs from source'. Splitting the workspace connection means splitting the brain."
- ⚙️ **BEAM veteran**: "The Rust LSP is a long-lived process that already handles async I/O (tower-lsp, tokio). Adding a WebSocket client via tungstenite is trivial — it's the same async runtime. The extension host is Node.js; adding WebSocket handling there is adding a second async runtime to worry about."
- 🏭 **Operator**: "One connection to the workspace means one auth flow, one reconnection strategy, one session to manage. Two independent connections means two points of failure and two connection states that can disagree."
- 🎨 **Language designer**: "The LSP already needs workspace data for Tier 3. If the LSP owns the connection, Tier 3 is a trivial addition — the data is already flowing. With the current design, the LSP opens its own separate connection for Tier 3 anyway, so you end up with two WebSocket connections in total (extension + LSP). If the LSP owned the sidebar connection too, you'd have one."

### Option D: Do Nothing (Status Quo)
- 🧑‍💻 **Newcomer**: "The REPL in the terminal already shows everything. `actors`, `bindings`, `inspect`, Transcript — all accessible from the command line. Do I really need a sidebar for this?"
- 🎩 **Smalltalk purist**: "Smalltalk's power comes from the language, not from fancy tooling chrome. The REPL is the natural interface. Pharo survived for decades without a VSCode sidebar."
- ⚙️ **BEAM veteran**: "I already have Observer for process inspection. Adding a sidebar adds maintenance burden — the team should focus on the language and compiler, not IDE features."
- 🏭 **Operator**: "Every new connection surface is a new thing to secure, a new thing that can fail, a new thing to audit. The extension is stable as an LSP client. Don't add complexity without clear user demand."
- 🎨 **Language designer**: "Engineering time is finite. The browser IDE (ADR 0017 Phase 3) is the differentiated product — it's a full Smalltalk-style workspace, not a sidebar. Build that first, not a half-measure in VSCode."

### Tension Points
- **Option B's Smalltalk purist argues** that building web components now means building the rich inspector once and reusing it in ADR 0017 Phase 3. This breaks on a concrete technical detail: ADR 0017 Phase 3 is **Phoenix LiveView** — server-rendered HTML with diffs pushed over WebSocket. Web components built for a VSCode WebviewPanel (sandboxed iframe, `postMessage` communication, strict CSP) are architecturally incompatible with LiveView. The rendering model, event model, and state model are all different. There is no code reuse path. What *does* transfer from this sidebar to Phase 3 is the information architecture — which panels, which data, which interactions — and that validates regardless of whether the VSCode sidebar is a TreeView or a WebviewPanel.
- **Option C's language designer makes a genuinely uncomfortable point**: the LSP opens its own WebSocket for Tier 3 anyway, so "direct WebSocket avoids two connections" was never true — there will be two connections regardless. Option C (LSP owns everything) would give you one. The ADR should not claim connection count as a win for Option A — it isn't. The real argument for Option A is **lifecycle independence and separation of concerns**. The extension host owns the terminal it started (`beamtalk repl` is its child process) — the sidebar session's lifecycle is naturally tied to that terminal. If the LSP owned the sidebar session, "Restart Language Server" would kill your running REPL session and clear all bindings. That's terrible UX the extension cannot protect against. Separately: `$/beamtalk/killActor` as an LSP request is a category error — the LSP is a language intelligence service, not an operations console. These responsibilities belong in different processes.
- **The "Do Nothing" option's surface argument** — "the REPL already shows everything" — is easy to dismiss. Discoverability clearly has value; nobody discovers `actors inspect: somePid` without documentation. **The strong version is opportunity cost and focus**: ADR 0017 Phase 3 (browser IDE) is the genuinely differentiated product — remote-accessible, multi-user, full Smalltalk workspace. Is 2-3 months on a VSCode sidebar the right sequence, or does shipping it cause Phase 3 to be deprioritised because "we already have something"? There is also a subtler risk: TreeView constraints could lead to an information architecture optimised for a tree model rather than what's best for the browser IDE. The counterargument is about **sequencing risk, not opportunity cost**: Phase 3 has significantly more unknowns (Phoenix LiveView, authentication, remote access). The sidebar is achievable now, ships real value, and validates the information architecture under low risk. It doesn't block or constrain Phase 3 — the IA validation is technology-independent. Building the smaller thing first is how you de-risk the larger thing.

## Alternatives Considered

### Full WebviewPanel for Everything
Show all four panels (explorer, transcript, inspector, classes) as a single React webview, like the browser workspace UI.

**Rejected because:**
- The structured state (bindings, actors, classes) is hierarchical — TreeView renders it with zero code and native VSCode feel (themes, keyboard nav, accessibility)
- We don't need React for a tree of strings; adding a framework for Phase 1 is premature
- `vscode-webview-ui-toolkit` is deprecated; choosing a replacement framework adds maintenance burden
- The browser UI (ADR 0017 Phase 3) is where the rich WebviewPanel investment should go — doing it twice (in VSCode sidebar and in browser) wastes effort

### LSP-Proxied Connection
Route all workspace communication through the LSP server: extension → LSP → WebSocket → workspace.

**Rejected because:**
- "Restart Language Server" is a common user action (fixing stale completions, picking up config changes). If the LSP owns the sidebar session, restarting it kills the REPL session and clears all bindings — UX the extension cannot prevent.
- The extension host owns the terminal running `beamtalk repl`. Routing its session through the LSP breaks the natural ownership — the child process lifecycle belongs with the parent that started it.
- Actor management via LSP requests (`$/beamtalk/killActor`, `$/beamtalk/reloadClass`) is a category error. The LSP is a language intelligence service, not an operations console.
- Note: connection count is NOT a win for Option A — the LSP opens its own separate WebSocket for Tier 3 anyway, so two connections exist regardless of this choice.

### Debug Adapter Protocol (DAP)
Implement a DAP server backed by the workspace, exposing bindings and actor state as DAP `variables`.

**Rejected because:**
- DAP models paused execution (breakpoints, stepping, call stacks). Beamtalk's workspace is always running.
- Actor state inspection is non-intrusive; DAP `variables` semantics imply a paused process with a frame stack
- Mapping running actors to DAP "threads" and state fields to DAP "variables" is a category error — it would confuse both users and tools that understand DAP
- Beamtalk may add true debugging (breakpoints via `:int`) later; that should be a separate DAP implementation, not conflated with workspace inspection

### Polling TreeView (no push)
Connect to the workspace, poll `bindings`/`actors`/`classes` on a configurable interval.

**Rejected because:**
- The workspace already pushes actor lifecycle events and Transcript — polling is unnecessary
- Polling causes unnecessary load on the workspace node
- Polling-while-hidden wastes CPU (mitigated by `onDidChangeVisibility` but better avoided entirely)
- Event-driven refresh is more responsive for actor spawn/stop events

### Do Nothing (Status Quo)
Rely on the terminal REPL for all workspace inspection. The REPL already supports `actors`, `bindings`, `inspect`, `classes`, and Transcript output.

**Rejected because:**
- Live programming is Beamtalk's differentiation — but it's invisible if users have to know to type inspection commands
- The sidebar makes workspace state persistent and always-visible, not ephemeral per command
- Newcomers won't discover `actors inspect: somePid` without documentation — a clickable tree is self-documenting
- The browser IDE (ADR 0017 Phase 3) needs the same information architecture validated — this sidebar does that with less risk
- However: the REPL IS the fallback, and all sidebar features degrade gracefully to it

## Consequences

### Positive
- Live workspace state visible in the editor without leaving VSCode
- Auto-connect means zero configuration for most users
- Event-driven updates — no polling, no background timers
- TreeView is native VSCode: accessible, themeable, keyboard-navigable, low memory
- Validates the panel information architecture for ADR 0017 Phase 3 before building the browser UI
- The workspace WebSocket protocol is the shared foundation for ADR 0024 Tier 3 — the LSP opens its own `WorkspaceTypeClient` connection independently
- Fills a genuine gap in the BEAM/Smalltalk ecosystem — no other language server does live workspace inspection in VSCode

### Negative
- TreeView cannot display richly formatted values (Dictionary, Array contents) — labels only. Phase 2 can add a detail WebviewPanel for expanded inspection.
- The extension now manages two connections (LSP + WebSocket). Extension host complexity increases. The two connections have independent lifecycle — the sidebar could show "Connected" while the LSP is dead, or vice versa. This needs clear, distinct status indicators.
- Workspace auto-connect requires reading `~/.beamtalk/workspaces/{id}/port` and `cookie` files — both must be reliable across platforms (Windows NTFS ACLs vs Unix permissions, path conventions). The 1:1 mapping between project directory and workspace (enforced by hashing the project path) means there is no ambiguity about which workspace to connect to.
- **Protocol gap**: the current WebSocket protocol lacks `class-loaded` push events. This must be added before or alongside the sidebar implementation. Small but real prerequisite work on the Erlang side.
- Each VSCode window creates a cloned session on the workspace. Multiple windows, plus crash/force-quit without cleanup, can leave orphan sessions. The workspace's idle monitor and session timeout should handle this, but session proliferation under multi-window use needs testing.

### Neutral
- The Transcript WebviewView is a simple `<pre>` with auto-scroll — no framework needed, minimal maintenance
- The browser UI (ADR 0017 Phase 3) is not replaced or delayed — it remains planned as a richer, remote-accessible alternative
- The sidebar is purely additive — existing LSP behaviour is unchanged

## Implementation

### Phase 0: Protocol Prerequisites (S effort)

One runtime-side change:

1. **`class-loaded` push event** (`beamtalk_repl_server.erl`, `beamtalk_ws_handler.erl`)
   - Broadcast `{"push":"class-loaded","class":"Counter"}` to all WebSocket subscribers when any session loads, reloads, or eval-defines a class — flat structure matching the existing `actor-spawned`/`actor-stopped` convention

2. **Session ID on REPL startup** (`beamtalk-cli/src/commands/repl.rs`)
   - REPL prints session ID to stdout on connect, e.g. `[beamtalk] session: abc123`
   - Extension parses this from terminal output to establish session ownership

### Phase 1: Connection + Workspace Explorer + Transcript (M effort)

1. **`WorkspaceClient` class** (`editors/vscode/src/workspaceClient.ts`)
   - WebSocket connection management (connect, auth, reconnect with backoff)
   - `bindings(sessionId)`, `actors()`, `classes()`, `inspect(pid)`, `sessions()` op wrappers
   - Push channel subscription (`transcript`, `actors`, `classes`)
   - Session lifecycle (`clone`, `close`)

2. **`WorkspaceTreeDataProvider`** (`editors/vscode/src/workspaceTreeView.ts`)
   - `TreeDataProvider<WorkspaceNode>` implementation
   - Nodes: ConnectedRoot / DisconnectedRoot / BindingsSection / ActorSection / ClassesSection / BindingItem / ActorItem / ClassItem
   - `onDidChangeTreeData` wired to push events
   - Context menus: `kill`, `inspect`, `reload` via `package.json` contributions

3. **`TranscriptViewProvider`** (`editors/vscode/src/transcriptView.ts`)
   - `WebviewViewProvider` for sidebar Transcript
   - Auto-scrolling `<pre>`, clear button, max-lines cap (10,000 lines)
   - Wired to `transcript` push channel from `WorkspaceClient`

4. **Auto-connect** (`editors/vscode/src/extension.ts`)
   - File watcher on `~/.beamtalk/workspaces/*/port`
   - Derive workspace ID from `beamtalk.toml` project root hash
   - Connect on port file appearance; disconnect on file removal

5. **`package.json` additions**
   - `viewsContainers`: `beamtalk-workspace` sidebar container with icon
   - `views`: `beamtalk.workspaceExplorer`, `beamtalk.transcript`
   - `menus/view/item/context`: kill, inspect, reload commands
   - `configuration`: `beamtalk.workspace.autoConnect` (default: true)

### Phase 2: Rich Inspector (M effort)
- WebviewPanel for expanded object inspection (show Dictionary, List, Array contents with formatting)
- Triggered by clicking "Inspect" on a binding or actor — opens a panel in the editor area
- Value rendering: syntax-highlighted Beamtalk-like display of nested structures

### Files to Create/Modify

| File | Change |
|------|--------|
| `editors/vscode/src/workspaceClient.ts` | New — WebSocket client |
| `editors/vscode/src/workspaceTreeView.ts` | New — TreeDataProvider |
| `editors/vscode/src/transcriptView.ts` | New — WebviewViewProvider |
| `editors/vscode/src/extension.ts` | Register views, wire auto-connect |
| `editors/vscode/package.json` | View containers, commands, config |

## References

- Related ADRs:
  - [ADR 0004: Persistent Workspace Management](0004-persistent-workspace-management.md) — workspace lifecycle, session model
  - [ADR 0017: Browser Connectivity to Running Workspaces](0017-browser-connectivity-to-running-workspaces.md) — WebSocket protocol, push channels, Phase 3 browser UI
  - [ADR 0024: Static-First, Live-Augmented IDE Tooling](0024-static-first-live-augmented-ide-tooling.md) — Tier 3 live augmentation (uses an independent LSP-owned workspace connection)
- Protocol documentation: `docs/repl-protocol.md`
- VSCode API references:
  - [TreeView API](https://code.visualstudio.com/api/extension-guides/tree-view)
  - [Webview API](https://code.visualstudio.com/api/extension-guides/webview)
  - [UX Guidelines — Views](https://code.visualstudio.com/api/ux-guidelines/views)
- Prior art:
  - [vscode-jupyter Variables panel](https://github.com/microsoft/vscode-jupyter/wiki/How-variables-are-fetched) — hybrid TreeView/WebviewPanel pattern
  - [vscode-pharo](https://github.com/badetitou/vscode-pharo) — live Smalltalk image connection via socket
