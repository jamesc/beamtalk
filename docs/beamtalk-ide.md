# Beamtalk IDE: Live Development Environment

The Beamtalk IDE provides a Smalltalk-style live programming experience where there's no gap between "coding" and "running." You're always working in a running system.

This document describes the integrated development environment vision. The IDE is built with Phoenix LiveView and connects to running BEAM nodes.

---

## The Smalltalk Philosophy

In traditional Smalltalk (Pharo/Squeak):

1. **Never touch files directly** — code lives in the image
2. **Browser** — navigate packages → classes → methods
3. **Edit in place** — change a method, press Accept, it's live immediately
4. **Workspace** — scratch pad to evaluate expressions
5. **Inspector** — click any object to see its state
6. **Debugger** — fix code *in the debugger*, resume execution

The key insight: **there's no separation between "coding" and "running"**.

Beamtalk adapts this for modern infrastructure: files remain the source of truth (for git, CI, collaboration), but the IDE makes it *feel* like an image.

---

## IDE Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│  Beamtalk IDE                                                           │
│  ┌─────────────────┬─────────────────────────────────────────────────┐ │
│  │  Actor Browser  │  Method Editor                                   │ │
│  │  ─────────────  │  ─────────────────────────────────────────────  │ │
│  │  ▼ Supervisors  │  Counter >> increment                           │ │
│  │    ▼ WebApp     │  ─────────────────────────────────────────────  │ │
│  │      Router     │                                                  │ │
│  │      DBPool     │  increment =>                                   │ │
│  │  ▼ Actors       │    self.value += 1                              │ │
│  │    ▶ Counter    │    Telemetry emit: #incremented                 │ │
│  │      Agent      │                                                  │ │
│  │      Worker     │                                                  │ │
│  ├─────────────────┤                              [Accept] [Revert]   │ │
│  │  Running        │  ─────────────────────────────────────────────  │ │
│  │  Instances      │  Messages:  increment getValue incrementBy:     │ │
│  │  ─────────────  │                                                  │ │
│  │  Counter<0.45>  │                                                  │ │
│  │  Counter<0.67>  │                                                  │ │
│  │  Counter<0.89>  │                                                  │ │
│  └─────────────────┴─────────────────────────────────────────────────┘ │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │  Workspace                                                         │ │
│  │  ───────────────────────────────────────────────────────────────  │ │
│  │  counter := Counter spawn                                          │ │
│  │  counter increment                                                 │ │
│  │  counter getValue await  // => 1                                   │ │
│  │                                                 [Do It] [Print It] │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │  Inspector: Counter<0.45>                           [Refresh] [X] │ │
│  │  ───────────────────────────────────────────────────────────────  │ │
│  │  State:                                                            │ │
│  │    value: 1                                                        │ │
│  │    __class__: Counter                                              │ │
│  │  Mailbox: (empty)                                                  │ │
│  │  Links: [<0.44> Supervisor]                                        │ │
│  └───────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Core Components

### 1. Actor Browser

Navigate the system from two perspectives:

**By Source (like Pharo Class Browser)**
```
▼ my_app
  ▼ actors
    ▶ Counter          ← click to see methods
      Agent
      Worker
  ▼ supervisors
    WebApp
▼ my_lib
  ▶ HTTPClient
```

**By Running System**
```
▼ Supervision Tree
  ▼ <0.100> my_app_sup
    ▼ <0.101> WebApp
      <0.102> Router
      <0.103> DBPool [10 workers]
    <0.150> Counter     ← click to inspect
    <0.151> Counter
    <0.152> Agent
```

Features:
- **Click actor class** → see its methods in editor
- **Click running instance** → open inspector
- **Right-click** → spawn new instance, stop, trace
- **Search** → find actors by name, method, state

### 2. Method Editor

Edit methods with immediate hot-reload:

```
┌─────────────────────────────────────────────────────────────────┐
│  Counter >> increment                                            │
│  ───────────────────────────────────────────────────────────────│
│  increment =>                                                    │
│    self.value += 1                                               │
│    Telemetry emit: #incremented value: self.value               │
│                                                                  │
│                                            [Accept] [Revert]    │
│  ───────────────────────────────────────────────────────────────│
│  Methods: increment  decrement  getValue  incrementBy:          │
└─────────────────────────────────────────────────────────────────┘
```

**Workflow:**
1. Select method from list
2. Edit code
3. Press **Accept** (Ctrl+S)
4. Code is hot-reloaded into all running instances
5. **< 200ms** from keystroke to live

**Behind the scenes:**
1. IDE writes change to `.bt` file
2. Compiler daemon detects change
3. Compiles to `.beam`
4. Hot-loads into running node

The user never sees files — but git, CI, and other tools can.

### 3. Workspace

Interactive expression evaluation (like Smalltalk's Workspace):

```
┌─────────────────────────────────────────────────────────────────┐
│  Workspace                                                       │
│  ───────────────────────────────────────────────────────────────│
│  // Create and use actors                                        │
│  counter := Counter spawn                                        │
│  counter increment                                               │
│  counter increment                                               │
│  counter getValue await  => 2                                    │
│                                                                  │
│  // Inspect the counter                                          │
│  counter state  => {value: 2, __class__: Counter}               │
│                                                                  │
│                              [Do It] [Print It] [Inspect It]    │
└─────────────────────────────────────────────────────────────────┘
```

**Commands:**
| Command | Shortcut | Behavior |
|---------|----------|----------|
| **Do It** | Ctrl+D | Execute selected code, no output |
| **Print It** | Ctrl+P | Execute, show result inline |
| **Inspect It** | Ctrl+I | Execute, open inspector on result |
| **Debug It** | Ctrl+Shift+D | Execute with debugger attached |

**Variables persist** across evaluations — the workspace maintains a binding context.

### 4. Inspector

Examine any actor in detail:

```
┌─────────────────────────────────────────────────────────────────┐
│  Inspector: Counter<0.150>                      [Refresh] [X]   │
│  ───────────────────────────────────────────────────────────────│
│  State                                                           │
│  ───────────────────────────────────────────────────────────────│
│  value: 42                                    [click to drill]  │
│  __class__: Counter                                              │
│  __methods__: {increment: #Fun, ...}          [click to expand] │
│                                                                  │
│  ───────────────────────────────────────────────────────────────│
│  Mailbox (3 pending)                                             │
│  ───────────────────────────────────────────────────────────────│
│  1. {increment, [], <0.200>}                                     │
│  2. {increment, [], <0.201>}                                     │
│  3. {getValue, [], <0.202>}                                      │
│                                                                  │
│  ───────────────────────────────────────────────────────────────│
│  Process Info                                                    │
│  ───────────────────────────────────────────────────────────────│
│  PID: <0.150.0>                                                  │
│  Status: waiting                                                 │
│  Memory: 2,847 bytes                                             │
│  Reductions: 1,234,567                                           │
│  Links: [<0.101> Supervisor]                                     │
│  Monitors: []                                                    │
│                                                                  │
│  ───────────────────────────────────────────────────────────────│
│  Self (evaluate in actor context)                                │
│  ───────────────────────────────────────────────────────────────│
│  > self.value := 100                          [Eval]            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Features:**
- **State** — all fields, click values to drill down
- **Mailbox** — pending messages (non-destructive peek via `process_info`)
- **Process Info** — memory, reductions, links, monitors
- **Self** — evaluate expressions in the actor's context
- **History** — track state changes over time
- **Auto-refresh** — optionally poll for updates

### 5. Debugger

The killer feature — **fix code mid-execution**:

```
┌─────────────────────────────────────────────────────────────────┐
│  Debugger: Counter<0.150> — error in increment                  │
│  ───────────────────────────────────────────────────────────────│
│  Stack                                                           │
│  ───────────────────────────────────────────────────────────────│
│  → Counter >> increment (line 3)                  [select]      │
│    Agent >> processTask: (line 23)                              │
│    Supervisor >> handleChild (line 12)                          │
│                                                                  │
│  ───────────────────────────────────────────────────────────────│
│  Source                                                          │
│  ───────────────────────────────────────────────────────────────│
│  increment =>                                                    │
│    self.value += 1                                               │
│    self.count += 1   // ← Error: undefined field 'count'        │
│    ^self.value                                                   │
│                                                                  │
│  ───────────────────────────────────────────────────────────────│
│  Context                                                         │
│  ───────────────────────────────────────────────────────────────│
│  self.value = 5                                                  │
│  self.__class__ = Counter                                        │
│                                                                  │
│  ───────────────────────────────────────────────────────────────│
│  [Proceed] [Restart] [Into] [Over] [Return]   [Fix & Continue]  │
└─────────────────────────────────────────────────────────────────┘
```

**Fix & Continue Workflow:**

1. Error occurs in running actor
2. Actor **pauses** (not crashes) — enters debug mode
3. Debugger window opens automatically
4. You see the stack, source, and context
5. **Edit the method** directly in the debugger
6. Press **Accept** — code hot-reloads
7. Press **Restart** — re-run the method with fixed code
8. Actor continues as if nothing happened

This is Smalltalk's superpower: **bugs are not fatal, they're edit opportunities**.

**Debugger Commands:**

| Command | Behavior |
|---------|----------|
| **Proceed** | Continue execution (may hit same error) |
| **Restart** | Re-run current method from beginning |
| **Into** | Step into next message send |
| **Over** | Step over next message send |
| **Return** | Return from current method with specified value |
| **Fix & Continue** | Accept changes + Restart |

---

## Message Timeline

Visualize actor interactions:

```
┌─────────────────────────────────────────────────────────────────┐
│  Message Timeline                                    [Record]   │
│  ───────────────────────────────────────────────────────────────│
│                                                                  │
│  Agent<0.150>  ─────●────────────────●─────────────────────────│
│                     │                 │                          │
│                     │ analyze:        │ store:                   │
│                     ▼                 ▼                          │
│  Critic<0.151> ────────●────────────────────────────●──────────│
│                        │                             │           │
│                        │ review:                     │ done      │
│                        ▼                             │           │
│  Writer<0.152> ───────────────●─────────────────────┴──────────│
│                               │                                  │
│                               │ write:                           │
│                               ▼                                  │
│                                                                  │
│  Time ──────────────────────────────────────────────────────►   │
│        10:23:45.001    .050           .120          .200         │
└─────────────────────────────────────────────────────────────────┘
```

**Features:**
- See message flow between actors
- Click message to see payload
- Filter by actor, message type, time range
- Identify bottlenecks and message storms

---

## Live Queries

Ask questions about the running system:

```
// Find all actors with mailbox > 100 messages
System actors select: [:a | a mailboxSize > 100]

// Find all Counter instances
System actors select: [:a | a class = Counter]

// Get total memory used by agents
(System actors select: [:a | a class inheritsFrom: Agent])
  inject: 0 into: [:sum :a | sum + a memory]

// Trace all messages to a specific actor
counter trace: #all

// Set a conditional breakpoint
counter breakOn: #increment when: [self.value > 1000]
```

---

## File Synchronization

The IDE hides files but keeps them as the source of truth:

### IDE → Files

| User Action | File System Effect |
|-------------|-------------------|
| Accept method | Update `.bt` file |
| Create actor | Create new `.bt` file |
| Delete method | Update `.bt` file |
| Rename actor | Rename `.bt` file |
| Delete actor | Delete `.bt` file |

### Files → IDE

| File System Change | IDE Effect |
|-------------------|-----------|
| External edit to `.bt` | Reload, show notification |
| Git pull with changes | Reload changed files |
| Conflict | Show merge UI |

### Why Keep Files?

| Benefit | Explanation |
|---------|-------------|
| **Git** | Meaningful diffs, branches, history |
| **Code Review** | PR workflow with GitHub/GitLab |
| **CI/CD** | Standard build pipelines |
| **Text editors** | VS Code, vim still work |
| **AI agents** | Tools can read/write code |
| **Team collaboration** | Mergeable text files |

---

## Implementation Architecture

### Phoenix LiveView

The IDE runs as a Phoenix LiveView app:

```elixir
defmodule BeamtalkWeb.IDELive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    # Connect to running BEAM node
    Node.connect(:"myapp@localhost")

    # Subscribe to system events
    Phoenix.PubSub.subscribe(Beamtalk.PubSub, "actors")
    Phoenix.PubSub.subscribe(Beamtalk.PubSub, "compiler")

    {:ok, assign(socket,
      actors: list_all_actors(),
      selected_actor: nil,
      selected_method: nil,
      workspace_bindings: %{},
      inspectors: []
    )}
  end

  def handle_event("accept_method", %{"code" => code}, socket) do
    actor = socket.assigns.selected_actor
    method = socket.assigns.selected_method

    # Write to file system
    :ok = Beamtalk.SourceWriter.write_method(actor, method, code)

    # Compiler daemon will pick up, compile, hot-reload
    # We'll get notified via PubSub

    {:noreply, assign(socket, compiling: true)}
  end

  def handle_event("eval_workspace", %{"code" => code}, socket) do
    bindings = socket.assigns.workspace_bindings

    # Compile expression
    {:ok, beam} = Beamtalk.Compiler.compile_expression(code)

    # Evaluate in connected node
    {result, new_bindings} = :rpc.call(
      :"myapp@localhost",
      :beamtalk_eval,
      :eval,
      [beam, bindings]
    )

    {:noreply, assign(socket,
      workspace_bindings: new_bindings,
      workspace_result: result
    )}
  end

  def handle_info({:actor_state_changed, pid, new_state}, socket) do
    # Update inspector if open for this actor
    {:noreply, update_inspector(socket, pid, new_state)}
  end

  def handle_info({:compile_complete, module, :ok}, socket) do
    {:noreply, assign(socket, compiling: false)}
  end
end
```

### Debug Protocol

Actors can be paused and controlled:

```erlang
-module(beamtalk_debugger).

%% Pause an actor and enter debug mode
pause(Pid) ->
    sys:suspend(Pid),
    Pid ! {debug_attach, self()}.

%% Get current state
get_state(Pid) ->
    sys:get_state(Pid).

%% Evaluate in actor context
eval_in_context(Pid, Code) ->
    %% Compile the expression
    {ok, Beam} = beamtalk_compiler:compile_expr(Code),

    %% Load and execute in actor's context
    Pid ! {debug_eval, Beam, self()},
    receive
        {debug_result, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

%% Resume execution
resume(Pid) ->
    Pid ! debug_continue,
    sys:resume(Pid).

%% Restart current method
restart(Pid) ->
    Pid ! debug_restart,
    sys:resume(Pid).
```

### Debug-Enabled Actor

Actors compiled with debug support:

```erlang
%% Generated gen_server with debug hooks
handle_info({debug_attach, DebuggerPid}, State) ->
    %% Enter debug loop
    debug_loop(State, DebuggerPid);

debug_loop(State, DebuggerPid) ->
    receive
        {debug_eval, Beam, ReplyTo} ->
            Result = eval_beam_in_context(Beam, State),
            ReplyTo ! {debug_result, Result},
            debug_loop(State, DebuggerPid);

        {debug_set_state, NewState} ->
            debug_loop(NewState, DebuggerPid);

        debug_continue ->
            %% Exit debug mode, continue normal operation
            {noreply, State};

        debug_restart ->
            %% Re-dispatch the failed message with new code
            {noreply, State}
    end.
```

---

## VS Code vs Web IDE

Both interfaces are supported:

| Feature | VS Code Extension | Web IDE (LiveView) |
|---------|-------------------|-------------------|
| **Method editing** | ✓ | ✓ |
| **Hot reload** | ✓ | ✓ |
| **Workspace** | Terminal REPL | Integrated |
| **Inspector** | Basic panel | Full graphical |
| **Debugger** | VS Code debugger | Custom UI |
| **Message timeline** | Limited | Full visualization |
| **Supervision tree** | Tree view | Interactive diagram |
| **Remote nodes** | SSH + attach | Direct connection |

The VS Code extension is good for text-focused work. The web IDE is better for visual exploration and debugging.

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| **Ctrl+S** | Accept (save + hot reload) |
| **Ctrl+D** | Do It (evaluate selection) |
| **Ctrl+P** | Print It (evaluate and show result) |
| **Ctrl+I** | Inspect It (evaluate and open inspector) |
| **Ctrl+B** | Browse (open actor browser) |
| **Ctrl+Shift+I** | Open inspector on selection |
| **Ctrl+Shift+D** | Debug It (evaluate with debugger) |
| **F5** | Continue (in debugger) |
| **F10** | Step Over |
| **F11** | Step Into |
| **Shift+F11** | Step Out |

---

## Future Enhancements

### Refactoring Tools

- **Rename** — actor, method, field (updates all references)
- **Extract Method** — select code, create new method
- **Inline Method** — replace calls with body
- **Move Method** — move between actors
- **Add Parameter** — update all call sites

### Version Control Integration

- **Show History** — git log for a method
- **Diff View** — compare versions
- **Blame** — who changed what
- **Branch Indicator** — current git branch

### Collaboration

- **Shared Workspace** — multiple users, same running system
- **Live Cursors** — see what others are editing
- **Chat** — discuss while coding

### AI Integration

- **Explain Code** — ask AI about selected code
- **Suggest Fix** — AI proposes debugger fix
- **Generate Tests** — AI writes test cases
- **Code Review** — AI reviews changes before accept

---

## References

- [Pharo Smalltalk](https://pharo.org/) — Modern Smalltalk environment
- [Glamorous Toolkit](https://gtoolkit.com/) — Moldable development environment
- [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view/) — Real-time web UI
- [Erlang sys Module](https://www.erlang.org/doc/man/sys.html) — Process debugging
