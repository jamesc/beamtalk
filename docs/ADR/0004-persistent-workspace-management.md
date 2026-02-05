# ADR 0004: Persistent Workspace Management

## Status

Proposed

## Context

Beamtalk aims to provide a **live coding** experience inspired by Smalltalk, where developers can work with long-running systems that preserve state across sessions. Currently, the REPL starts a fresh BEAM node that terminates when the session ends, killing all spawned actors and losing all state.

This ADR explores how to implement persistent workspaces that allow:

1. **Actor survival** across REPL disconnects
2. **Multiple isolated workspaces** for different projects/contexts
3. **Seamless reconnection** to running systems
4. **Automatic cleanup** of abandoned workspaces

We examine two primary models:

1. **Smalltalk/Pharo**: Image-based persistence (serialize entire VM state to disk)
2. **BEAM/Erlang**: Distributed node architecture with hot code reloading

### The Smalltalk/Pharo Model

In Pharo (a modern Smalltalk implementation), the development environment centers around an **image**:

#### How Pharo Images Work

```
┌─────────────────────────────────────┐
│ Pharo Image (.image file)           │
├─────────────────────────────────────┤
│ • All objects in memory              │
│ • All class definitions              │
│ • Development tools (debuggers)      │
│ • Running processes                  │
│ • Stack frames and execution state   │
│ • Open windows and UI state          │
└─────────────────────────────────────┘
         ↓ Save (snapshot)
┌─────────────────────────────────────┐
│ Disk File (~50-200 MB)               │
└─────────────────────────────────────┘
         ↓ Load
┌─────────────────────────────────────┐
│ Restored VM with exact state         │
└─────────────────────────────────────┘
```

**Workflow:**
```smalltalk
"Morning: Define a class"
Object subclass: #Counter
    instanceVariableNames: 'value'
    ...

counter := Counter new.
counter increment.
counter increment.
counter value. "=> 2"

"Save image (Ctrl+S or auto-save)"
Smalltalk snapshot: true andQuit: false.

"Close Pharo, go to lunch"

"Afternoon: Resume"
"Open same image file - counter still exists with value 2"
counter value. "=> 2"
counter increment.
counter value. "=> 3"
```

#### Pharo's Image Benefits

1. **True persistence**: Everything in memory is saved (objects, closures, UI state)
2. **Zero boilerplate**: No serialization code needed—just save/load
3. **Time travel**: Can save multiple snapshots and switch between them
4. **Live debugging**: Debugger state persists—pause execution, save image, resume later
5. **Self-contained**: Image includes entire development environment

#### Pharo's Image Drawbacks

1. **Large files**: Images grow to hundreds of MB (includes IDE, tools, history)
2. **Slow startup**: Loading a full image takes 2-10 seconds
3. **Version control friction**: Binary image files don't work with git
4. **Accumulates cruft**: Old objects, temporary variables persist indefinitely
5. **Single workspace**: Only one active image at a time (can't easily run multiple)
6. **Portability issues**: Images are platform/VM-version specific
7. **No isolation**: All code and data in one global namespace
8. **Corruption risk**: Single file corruption loses all work

### The BEAM/Erlang Model

Erlang/OTP provides a fundamentally different architecture based on **distributed nodes** and **hot code reloading**:

#### How BEAM Nodes Work

```
┌────────────────────────────────────┐
│ BEAM Node (detached process)       │
├────────────────────────────────────┤
│ • Running processes (actors)        │
│ • Loaded modules (.beam bytecode)  │
│ • Process mailboxes and state       │
│ • Supervision trees                 │
│ • Distributed protocols             │
└────────────────────────────────────┘
         ↕ Remote shell attach
┌────────────────────────────────────┐
│ REPL Client (ephemeral)             │
└────────────────────────────────────┘
```

**Workflow:**
```bash
# Morning: Start detached BEAM node
erl -detached -name workspace@localhost -setcookie secret

# Connect with remote shell
erl -remsh workspace@localhost -name repl@localhost -setcookie secret

# Spawn actors
1> counter = counter:spawn().
<0.123.0>
2> counter ! increment.
1
3> counter ! increment.
2

# Disconnect (Ctrl+C or exit shell)
# BEAM node keeps running, actors alive

# Afternoon: Reconnect
erl -remsh workspace@localhost -name repl@localhost -setcookie secret
1> counter ! increment.
3  % State preserved!

# Hot reload code (without losing state)
2> c(counter).  % Recompile
{ok, counter}
3> sys:suspend(counter).  % Pause actor
4> sys:change_code(counter, counter, "old_version", []).  % Upgrade
5> sys:resume(counter).  % Resume with new code
```

#### BEAM's Benefits

1. **Lightweight**: Only running processes persist (no IDE baggage)
2. **Fast startup**: Reconnect in milliseconds, not seconds
3. **Multiple workspaces**: Run many nodes in parallel (different ports/names)
4. **Hot code reload**: Update code without stopping actors
5. **Supervision trees**: Built-in fault tolerance and actor lifecycle management
6. **Distributed by default**: Can connect to remote nodes over network
7. **Introspection tools**: Observer, recon, sys module for live debugging
8. **Clean separation**: Code (in files) vs. state (in processes)

#### BEAM's Drawbacks

1. **Manual persistence**: Process state lost on node shutdown (unless using Mnesia/ETS with disk persistence)
2. **Code-data split**: Changes require hot reload—not as seamless as Smalltalk
3. **Setup complexity**: Need to manage node names, cookies, distributed Erlang
4. **Limited UI persistence**: No GUI state preservation (though Beamtalk is terminal-based)
5. **Learning curve**: Distributed Erlang concepts (nodes, cookies, remsh) unfamiliar to Smalltalk developers

### Hybrid Approach: Beamtalk Workspaces

Beamtalk can combine the best of both worlds:

```
┌─────────────────────────────────────────────────────┐
│ Beamtalk Workspace (per project/session)            │
├─────────────────────────────────────────────────────┤
│ • Detached BEAM node (beamtalk_workspace_<id>)      │
│ • Persistent actors with supervision                 │
│ • Hot code reloading via compiler daemon             │
│ • Workspace metadata (project path, created time)    │
│ • Automatic cleanup (detect stale workspaces)        │
│ • Socket/port for REPL reconnection                  │
└─────────────────────────────────────────────────────┘
         ↕ TCP/Unix socket
┌─────────────────────────────────────────────────────┐
│ REPL CLI (ephemeral, multiple instances allowed)     │
└─────────────────────────────────────────────────────┘
```

**Architecture:**

```erlang
%% Per-workspace supervisor (inside each workspace node)
beamtalk_workspace_sup
  ├─ beamtalk_repl_server      % TCP server for REPL connections
  ├─ beamtalk_idle_monitor     % Tracks activity, self-terminates if idle
  ├─ beamtalk_workspace_meta   % Metadata (project path, created_at)
  ├─ beamtalk_actor_sup        % Supervises ALL actors in workspace (shared)
  │    ├─ Counter#<0.123.0>    %   ← Visible to all sessions
  │    ├─ Logger#<0.124.0>     %   ← Visible to all sessions
  │    └─ HttpServer#<0.125.0> %   ← Visible to all sessions
  └─ beamtalk_session_sup      % Supervises session shell processes
       ├─ session_alice        %   ← Just the shell, bindings
       └─ session_bob          %   ← Just the shell, bindings
```

**Features:**

1. **Persistent BEAM node**: Detached process survives REPL disconnect
2. **Actor supervision**: User actors supervised, restart on crash
3. **Workspace isolation**: Each workspace = separate BEAM node
4. **Reconnection**: REPL connects via TCP/Unix socket to running node
5. **Auto-cleanup**: Detect abandoned workspaces (no activity for N hours)
6. **Hot reload**: Compiler daemon pushes code updates to workspace node
7. **Multi-project**: Different workspaces for different directories
8. **Fast switching**: Switch workspace in <100ms (just reconnect)

## Decision

**Implement persistent workspaces using detached BEAM nodes with supervision trees, not Pharo-style image snapshots.**

### Key Concepts: Nodes vs Sessions

The design distinguishes between two levels:

| Term | What it is | Scope | Lifecycle |
|------|------------|-------|-----------|
| **Workspace** | BEAM node + project context | Actors, modules, ETS (shared) | Long-lived |
| **Session** | REPL connection | Variable bindings (local) | Ephemeral |

```
┌─────────────────────────────────────────────────────────────┐
│ Workspace: my-project (BEAM Node)                            │
│                                                              │
│   Shared State (all sessions see this):                      │
│   ├── Actors: Counter <0.123>, Logger <0.124>               │
│   ├── Modules: Counter, Logger, HttpServer                  │
│   ├── ETS tables, registered names                          │
│   └── Workspace global object                               │
│                                                              │
│   Sessions (REPL connections):                               │
│   ┌─────────────────┐  ┌─────────────────┐                  │
│   │ Session: alice  │  │ Session: bob    │                  │
│   │ (Terminal 1)    │  │ (Terminal 2)    │                  │
│   │                 │  │                 │                  │
│   │ Local bindings: │  │ Local bindings: │                  │
│   │ counter = <123> │  │ c = <123>       │  ← Same actor!  │
│   │ x = 42          │  │ y = 100         │                  │
│   └─────────────────┘  └─────────────────┘                  │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

**What's workspace-scoped (shared by all sessions):**
- All actors (spawned by anyone in the workspace)
- Loaded modules (hot reload affects everyone)
- ETS tables and registered names
- The `Workspace` global object

**What's session-scoped (local to each REPL):**
- Variable bindings (`counter := ...`)
- Command history
- Current namespace context

### The Beamtalk Global Object

`Beamtalk` is a global object (like `Smalltalk` in Smalltalk) available in every session:

```beamtalk
// Query runtime state
Beamtalk actors                    // List all actors in workspace
Beamtalk actorNamed: #myCounter    // Get actor by registered name
Beamtalk sessions                  // List connected REPL sessions
Beamtalk modules                   // List loaded modules

// Runtime metadata
Beamtalk projectPath               // "/home/user/my-project"
Beamtalk nodeName                  // 'beamtalk_my_project@localhost'
Beamtalk version                   // "0.1.0"

// Session management
Beamtalk currentSession            // This REPL's session
Beamtalk broadcast: "Taking break" // Send message to all sessions

// Hot reload
Beamtalk reload: Counter           // Reload specific module
Beamtalk reloadAll                 // Reload all modified modules
```

**Note on terminology:**
- **Beamtalk** = The runtime global (actors, modules, node state)
- **Workspace** = The IDE concept (your development session spanning multiple terminals)
- **Session** = Each REPL connection (local bindings, command history)

Think of it like VS Code: one workspace with multiple editor panes sharing the same project. Here: one workspace with multiple REPLs sharing the same runtime.

**Why this distinction matters:**

- **Actors belong to the workspace, not the session.** When you spawn an actor, any REPL in the workspace can interact with it.
- **Bindings are session-local.** Your variable `counter` pointing to `<0.123>` is yours; Bob's variable `c` pointing to the same actor is his.
- **Hot reload affects everyone.** When you `:reload Counter`, all sessions see the new code.

| Scenario | Same workspace? | Why |
|----------|-----------------|-----|
| Two terminals, same project | ✓ Yes | Share actors, different bindings |
| Feature branch experiment | ✗ No (different workspace) | Different code versions |
| Attach to production | ✓ Yes (new session) | Debug with shared actors |
| Two unrelated projects | ✗ No (different workspaces) | Completely different code |

### Development vs Production

Workspaces serve different purposes in development and production:

```
┌─────────────────────────────────────────────────────────────┐
│ DEVELOPMENT                                                  │
│                                                              │
│   beamtalk repl                                              │
│   - Auto-creates node for project directory                  │
│   - Auto-creates session for this terminal                   │
│   - Auto-cleanup after 4 hours idle                          │
│   - Hot reload from source files                             │
│                                                              │
│   beamtalk repl --session experiment                         │
│   - Same node (same code)                                    │
│   - New session (isolated actors)                            │
│                                                              │
│   beamtalk repl --workspace feature-x                        │
│   - Different node (can have different code)                 │
│   - Useful for testing breaking changes                      │
│                                                              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ PRODUCTION                                                   │
│                                                              │
│   # Started by systemd/Docker/k8s                            │
│   beamtalk run server.bt --name prod@host                    │
│   - Runs as daemon, no auto-cleanup                          │
│   - Application supervisor manages services                  │
│                                                              │
│   # Live debugging (attach session to running node)          │
│   beamtalk attach prod@host                                  │
│   - Creates debug session in production node                 │
│   - Can inspect actors, send messages, hot reload            │
│   - Detach when done, node keeps running                     │
│                                                              │
│   # Or use standard OTP releases                             │
│   beamtalk build --release                                   │
│   ./rel/myapp/bin/myapp start                                │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

**Key insight:** The same attach mechanism works for both:
- **Dev:** `beamtalk repl` → auto-create node + session
- **Prod:** `beamtalk attach` → session on existing node

This is `erl -remsh` / `iex --remsh` productized with automatic node discovery and cookie management.

### Rationale

1. **BEAM-native**: Leverages Erlang/OTP's built-in distributed node capabilities
2. **Lightweight**: Only state that matters (running actors) persists, not IDE/tools
3. **Multiple workspaces**: Natural with BEAM's distributed architecture
4. **Fault tolerance**: Supervision trees provide built-in actor restart
5. **Hot reload**: BEAM's hot code reloading is mature and battle-tested
6. **Simpler implementation**: Less code than VM image serialization
7. **Fits Beamtalk philosophy**: Actor-based, BEAM-first, not trying to be Smalltalk-exact

### Architecture Components

#### 1. Workspace Lifecycle

```bash
# Create workspace (automatically named or explicit)
beamtalk workspace create my-feature
# → Starts detached BEAM node: beamtalk_workspace_my_feature@localhost

# Connect REPL
beamtalk repl
# → Detects workspace for current directory, connects via TCP

# Disconnect (close terminal, Ctrl+D)
# → BEAM node continues running, actors alive

# Reconnect (same directory)
beamtalk repl
# → Reconnects to existing workspace

# List workspaces
beamtalk workspace list
# workspace: my-feature, project: ~/code/app, active: 2m ago

# Clean up manually
beamtalk workspace stop my-feature
# → Gracefully shuts down BEAM node, kills actors
```

#### 2. Automatic Workspace Discovery

```
Current directory: /home/user/project-a
→ Look for workspace: SHA256(pwd) → beamtalk_workspace_abc123
→ If exists and active → reconnect
→ If not exists → create new workspace
```

#### 3. Automatic Cleanup

Cleanup targets **nodes** (not sessions). Sessions are ephemeral and just die when REPL disconnects.

```erlang
%% Node cleaner runs every 10 minutes (inside each node, self-terminating)
-define(MAX_IDLE_TIME, 3600 * 4).  % 4 hours

check_idle() ->
    case has_active_sessions() orelse has_recent_activity() of
        true -> ok;  % Keep running
        false ->
            case idle_time() > ?MAX_IDLE_TIME of
                true -> init:stop();  % Self-terminate
                false -> ok
            end
    end.
```

**What counts as "activity":**
- Session connected (even if idle in REPL)
- Actor message sent/received
- Code hot-reloaded
- Any user interaction

**Cleanup mechanism (no external daemon):**

| Component | Responsibility |
|-----------|----------------|
| **Node self-monitor** | Each node tracks `last_active`, self-terminates if idle too long |
| **CLI on startup** | `beamtalk repl` checks for orphan files (node died but files remain), cleans up |
| **Production nodes** | Started with `--persistent` or via systemd — never auto-cleanup |

```bash
# Development node: auto-cleanup enabled
beamtalk repl                        # Creates node, will auto-terminate if idle

# Production node: auto-cleanup disabled
beamtalk run server.bt --persistent  # Never self-terminates
systemctl start myapp                # Managed externally, no beamtalk cleanup
```

#### 4. Actor Supervision

```erlang
%% User actors are supervised (not Pharo's global object soup)
user_actor_sup:start_child(CounterModule, InitArgs)
  → Supervisor monitors actor, restarts on crash
  → Actor state lost on crash (Erlang semantics)
  → Use persistent storage (Mnesia/ETS) for critical state
```

#### 5. Hot Code Reloading

```erlang
%% When user modifies counter.bt and saves:
Compiler Daemon:
  1. Compile counter.bt → counter.beam
  2. Push to workspace node via net_kernel
  3. Workspace node: code:load_binary(counter, Beam)
  4. Optionally: sys:change_code(Actor, counter, OldVsn, Extra)

%% Actors can handle code upgrades:
code_change(OldVsn, State, _Extra) ->
    %% Migrate state to new format if needed
    {ok, migrate_state(State)}.

### Class Definition Hot Reload

**The hardest problem in live coding:** What happens to existing actor instances when you modify a class definition?

**Important limitation:** Hot reloaded code is **memory-only**. If the node restarts, it loads code from disk (release files), not the hot-reloaded version. This is a fundamental BEAM characteristic — hot reload is for immediate fixes, not persistent changes. To persist changes across restarts, you must update source files and redeploy.

```beamtalk
// Original Counter.bt
class Counter
  @value := 0
  
  increment
    @value := @value + 1
end
```

```erlang
> counter := Counter spawn
> counter increment   % @value = 1
> counter increment   % @value = 2
```

Now you modify Counter.bt:

```beamtalk
// Modified Counter.bt  
class Counter
  @value := 0
  @step := 1          // NEW: added instance variable
  
  increment
    @value := @value + @step   // CHANGED: uses @step
  
  step: newStep       // NEW: setter
    @step := newStep
end
```

**Hot reload behavior:**

| Change Type | Behavior | Notes |
|-------------|----------|-------|
| **New method** | Available immediately | Existing actors gain new method |
| **Modified method** | Takes effect on next call | No state change needed |
| **Removed method** | `doesNotUnderstand:` on next call | Calling removed method fails |
| **New instance variable** | **Defaults to nil** | `@step` is `nil` until set |
| **Removed instance variable** | **Orphaned** | Old data in state, inaccessible |
| **Renamed instance variable** | **Both orphaned and nil** | Old name orphaned, new name nil |

**Example after hot reload:**

```erlang
> counter increment   
%% ERROR: @step is nil, can't add nil to integer!

> counter step: 1     % Set the new variable
> counter increment   % Now works: @value = 3
```

**Strategies for safe upgrades:**

1. **code_change/3 callback** (recommended for critical actors):
   ```beamtalk
   class Counter
     codeChange: oldVersion state: oldState extra: extra
       // Migrate state: add default for @step
       self step: (oldState at: #step ifAbsent: [1])
   end
   ```

2. **Lazy initialization** (recommended for simple cases):
   ```beamtalk
   step
     @step ifNil: [@step := 1].
     ^ @step
   ```

3. **Versioned spawning** (for incompatible changes):
   ```erlang
   > oldCounter := Counter spawn       % Old version
   > newCounter := CounterV2 spawn     % New version
   > newCounter migrateFrom: oldCounter
   ```

**Key insight:** BEAM's hot reload is *module-level*, not *instance-level*. All instances of a class share the same code. When code changes:
- **Code pointer updates** — next method call uses new code
- **State unchanged** — instance variables retain their values
- **Shape mismatch** — new variables are nil, removed variables are orphaned

This is different from Smalltalk, where you can modify individual methods and the change applies immediately to all instances. Beamtalk's approach is BEAM-native: reload the module, let actors handle migration.
```

### REPL Binding Persistence

**Important distinction:** Workspaces preserve *running actors*, not *REPL variable bindings*.

```erlang
%% Session 1
> counter := Counter spawn    % Creates actor, binds to 'counter'
> counter increment           % Works
> counter increment           % Works
%% Disconnect

%% Session 2 (reconnect)
> counter increment           % ERROR: 'counter' is unbound!
```

**Why?** REPL bindings are local to the Erlang shell process. When you disconnect, that process dies. The *actor* (a separate process) survives, but the *variable* pointing to it does not.

**Solutions:**

1. **Named actors (recommended for important actors):**
   ```erlang
   > counter := Counter spawnAs: #myCounter
   %% Reconnect later
   > counter := Actor named: #myCounter   % Rebind to existing actor
   > counter increment                    % Works!
   ```

2. **Beamtalk actor registry:**
   ```erlang
   > Beamtalk actors   // List all supervised actors
   #(#myCounter -> <0.123.0>, #logger -> <0.124.0>)
   
   > Beamtalk actorNamed: #myCounter   // Get actor by name
   <0.123.0>
   ```

3. **Session restore (future enhancement):**
   ```erlang
   %% On reconnect, REPL could auto-restore bindings from registry
   > :bindings              % Show available bindings
   counter = <0.123.0> (Counter)
   logger = <0.124.0> (Logger)
   
   > :restore counter       % Rebind 'counter' to the actor
   ```

**Mental model:** Think of workspaces like a server room. Actors are servers that keep running. REPL bindings are sticky notes on your desk pointing to servers. When you leave, the sticky notes get thrown away, but the servers keep running. You can write new sticky notes when you return.

### Security and Node Authentication

Workspaces use Erlang's distributed node authentication via **cookies**:

```
~/.beamtalk/
├── workspaces/
│   ├── my-feature/
│   │   ├── cookie          # Random 32-char secret
│   │   ├── node.info       # Node name, port, PID
│   │   └── metadata.json   # Project path, created_at, etc.
│   └── b7a3f9.../
│       └── ...
└── config.toml             # Global settings
```

**Cookie generation:**
```erlang
%% On workspace creation
Cookie = base64:encode(crypto:strong_rand_bytes(24)),
file:write_file(CookiePath, Cookie),
file:change_mode(CookiePath, 8#600).  % Owner read/write only
```

**Security properties:**

| Property | Implementation |
|----------|----------------|
| **Isolation** | Each workspace has unique cookie—can't connect to wrong workspace |
| **Local-only** | Nodes bind to `127.0.0.1` by default (no network exposure) |
| **File permissions** | Cookie files are `chmod 600` (owner only) |
| **No shared secrets** | Workspaces don't share cookies with each other |

**Advanced: Remote workspaces (future)**
```bash
# Enable network access (explicit opt-in)
beamtalk workspace create my-feature --network

# Connect from another machine
beamtalk repl --remote user@host:~/project-a
```

### Multi-REPL Same-Workspace Support

Multiple REPL sessions can connect to the same workspace simultaneously, sharing all actors:

```
┌─────────────────────────────────────────────────────┐
│ Workspace: my-project (BEAM node)                   │
│                                                      │
│   Actors (SHARED by all sessions):                  │
│   ┌─────────┐  ┌─────────┐  ┌─────────┐            │
│   │ Counter │  │ Logger  │  │ Worker  │            │
│   │ <0.123> │  │ <0.124> │  │ <0.125> │            │
│   └─────────┘  └─────────┘  └─────────┘            │
│        ↑              ↑              ↑              │
│   ┌────┴────┐    ┌────┴────┐    ┌────┴────┐        │
│   │ Session │    │ Session │    │ Session │        │
│   │ alice   │    │ bob     │    │ vscode  │        │
│   │ (REPL1) │    │ (REPL2) │    │ (ext)   │        │
│   └─────────┘    └─────────┘    └─────────┘        │
│                                                      │
└─────────────────────────────────────────────────────┘
```

**Behavior:**

| Scenario | Behavior |
|----------|----------|
| Alice spawns actor | Bob and VSCode can see and message it |
| Alice binds `counter := Counter spawn` | Bob does NOT see `counter` binding (local) |
| Bob sends `<0.123> increment` | Same actor Alice spawned |
| Alice reloads Counter module | Bob's actors get new code too |
| Alice's REPL crashes | Actors survive, Bob unaffected |

**Use cases:**

1. **Pair programming:** Two developers, same workspace, same actors
2. **Debugging:** One REPL for normal work, one for inspection
3. **IDE integration:** VSCode extension connects alongside terminal REPL

**Workspace object for coordination:**
```beamtalk
Beamtalk sessions                  // List connected REPLs
// #(#alice -> <0.200.0>, #bob -> <0.201.0>)

Beamtalk broadcast: "Taking a break"   // Notify other REPLs
// Bob and VSCode see: [workspace] "Taking a break"

Beamtalk actorNamed: #myCounter    // Both Alice and Bob can access
```

### Workspace Metadata

Each workspace stores metadata in ETS table:

```erlang
-record(workspace, {
    id :: binary(),                     % "my-feature" or SHA256 of directory
    node_name :: atom(),                % beamtalk_workspace_my_feature@localhost
    project_path :: string(),           % "/home/user/project-a"
    created_at :: integer(),            % Unix timestamp
    last_active :: integer(),           % Unix timestamp (updated on REPL activity)
    repl_port :: inet:port_number(),    % TCP port for REPL connection (9001, 9002, ...)
    supervised_actors :: [pid()],       % PIDs of spawned actors
    loaded_modules :: [atom()]          % Modules loaded in this workspace
}).
```

### Multi-Workspace Scenario

```bash
# Terminal 1: Project A
cd ~/project-a
beamtalk repl
> counter := Counter spawn
> counter increment
1

# Terminal 2: Project B (parallel development)
cd ~/project-b
beamtalk repl
> logger := Logger spawn
> logger log: "Starting work"

# Terminal 3: Experiment (same project, different workspace)
cd ~/project-a
beamtalk workspace create experiment
beamtalk repl
> # Try risky changes without affecting Terminal 1
```

## Consequences

### Positive

1. **BEAM-native**: Uses Erlang/OTP's built-in features (distributed nodes, supervision)
2. **Lightweight**: No VM image overhead—only running actors persist
3. **Fast reconnection**: Milliseconds, not seconds (no image loading)
4. **Multiple workspaces**: Natural with BEAM's distributed architecture
5. **Fault tolerance**: Supervision trees provide actor restart
6. **Hot code reload**: Mature BEAM feature, well-tested
7. **Clean separation**: Code in files (version control), state in processes
8. **Tooling integration**: Observer, recon, sys module work out-of-box
9. **Scalable**: Can run dozens of workspaces in parallel
10. **No version control issues**: No binary image files to commit

### Negative

1. **Not Smalltalk-exact**: Different from Pharo's image model (philosophical departure)
2. **State lost on node crash**: No automatic persistence to disk (use Mnesia/ETS if needed)
3. **Learning curve**: Developers must understand distributed Erlang concepts
4. **Manual hot reload**: Not as seamless as Smalltalk (though close)
5. **Workspace discovery**: Heuristics (directory-based) may not always match user intent
6. **Node management complexity**: Need to track node names, ports, cookies
7. **No UI persistence**: Can't save debugger state, open windows (terminal-only anyway)
8. **Hot reload not persisted**: Code changes are memory-only; node restart loads from disk (see "Class Definition Hot Reload")

### Open Questions

**Source code in production releases:**

Following BEAM conventions (Erlang, Elixir, Gleam), production releases ship `.beam` files only, not source. This keeps deploys small and source private. However, this creates tension with Smalltalk-style workflows:

- **`:save Counter`** — A future feature to persist hot-reloaded code back to source files would require source to be present (or reconstructible) on the production server.
- **Decompilation** — Reconstructing `.bt` from BEAM loses comments, formatting, and possibly semantic information.
- **Embedded source** — Storing source in BEAM metadata (like Elixir's `@doc`) is possible but increases artifact size.

This is a philosophical choice: How Smalltalk-like should production be? Pharo's image model doesn't have this problem because source and bytecode live together. BEAM's separation forces a decision.

**Options (not decided):**
1. Ship `.beam` only (BEAM convention) — `:save` is dev-only
2. Opt-in `--include-source` for releases that need it
3. Embed source in BEAM metadata, extract on `:save`
4. `:save` pushes to remote git repo, not local disk

This decision is deferred to a future ADR on build/release tooling.

**Package management in workspaces:**

Workspaces need to load project dependencies (packages). When a workspace node starts, it must add all dependency `ebin/` directories to the code path. Key considerations:

- File format (Tonel-style, flat `.bt`, Mix project structure)
- Build tool integration (Mix handles Erlang + Elixir deps natively, access to Hex.pm)
- Compiled dependency caching (per-project vs shared cache)
- Version conflicts across workspaces (different nodes can have different versions)
- Hot reloading dependencies in running workspaces

This is deferred to a separate ADR on package management and build tooling.

### Neutral

1. **Philosophical shift**: Embraces BEAM-first philosophy over Smalltalk purity
2. **Different mental model**: Think "long-running server process" not "VM snapshot"
3. **Requires infrastructure**: Workspace registry, cleaner, discovery logic
4. **Trade-off**: Simplicity (image) vs. flexibility (distributed nodes)

## Implementation Phases

### Phase 1: Foundation (BT-184, BT-182)

- [ ] Design workspace architecture and API
- [ ] Implement detached BEAM node startup
- [ ] Basic workspace registry (track active nodes)
- [ ] REPL reconnection to running workspace
- [ ] Manual workspace creation/stop commands

### Phase 2: Developer Experience

- [ ] Automatic workspace discovery (directory-based)
- [ ] Workspace metadata tracking (last_active, project_path)
- [ ] CLI commands: `workspace list`, `workspace switch`
- [ ] Graceful error handling (node not responding, etc.)

### Phase 3: Advanced Features

- [ ] Automatic cleanup of abandoned workspaces
- [ ] Hot code reloading to running workspace
- [ ] Actor supervision for user-spawned actors
- [ ] Workspace isolation (prevent cross-workspace interference)
- [ ] Performance monitoring (workspace resource usage)

### Phase 4: Polish

- [ ] Workspace naming and renaming
- [ ] Export/import workspace state (manual persistence)
- [ ] Integration with Observer GUI (inspect actors)
- [ ] Distributed debugging tools integration

## Alternatives Considered

### Alternative 1: Pharo-Style Image Persistence

**Approach:** Serialize entire BEAM VM state to disk file

**Pros:**
- True Smalltalk experience
- Zero boilerplate persistence
- Can save debugger state

**Cons:**
- Requires VM modifications (BEAM doesn't support this natively)
- Large files (includes all loaded modules, ETS tables)
- Slow startup (loading gigabytes of data)
- Single workspace limitation
- Portability issues (platform-specific)
- High implementation complexity (months of work)

**Verdict:** Rejected—too much complexity for limited benefit

### Alternative 2: Mnesia-Based State Persistence

**Approach:** Automatically persist all actor state to Mnesia (Erlang's built-in database)

**Pros:**
- State survives node restarts
- ACID transactions for state updates
- Distributed (replicate across nodes)

**Cons:**
- Performance overhead (every state change writes to disk)
- Requires schema definition (not transparent like Smalltalk)
- Complex failure modes (disk full, corruption)
- Doesn't fit Erlang's "let it crash" philosophy
- Tight coupling between actors and storage

**Verdict:** Rejected—use Mnesia only for critical state, not all actor state

### Alternative 3: Session Recordings (Event Sourcing)

**Approach:** Record all REPL commands, replay on reconnect

**Pros:**
- Reproducible sessions
- Git-friendly (text files)
- Can share sessions with teammates

**Cons:**
- Non-deterministic actors (timestamps, PIDs) break replay
- Slow replay for long sessions
- Doesn't handle interactive input (user choices)
- Debugging sessions not reproducible

**Verdict:** Rejected—useful for logging, not for live persistence

### Alternative 4: Hybrid (BEAM Nodes + Optional Mnesia)

**Approach:** Default to ephemeral actors, opt-in to Mnesia for critical state

```erlang
%% Ephemeral actor (default)
counter := Counter spawn

%% Persistent actor (opt-in)
counter := Counter spawnPersistent: #{db => mnesia, table => counters}
```

**Pros:**
- Best of both worlds
- Developers choose persistence explicitly
- No performance overhead for simple actors

**Cons:**
- Two code paths (complexity)
- API surface increase
- Developers must understand when to use each

**Verdict:** Possible future enhancement, not initial design

## Migration Path

1. **Phase 1 (Current → Foundation)**:
   - Modify REPL to start detached BEAM nodes
   - Implement workspace registry (ETS table in runtime)
   - Add `beamtalk workspace` subcommands

2. **Phase 2 (Foundation → Automatic Discovery)**:
   - Implement directory-based workspace hashing
   - Auto-detect/create workspace on `beamtalk repl`
   - Add cleanup logic for stale workspaces

3. **Phase 3 (Discovery → Hot Reload)**:
   - Connect compiler daemon to workspace nodes
   - Implement `code_change/3` in actor templates
   - Add `:reload` command to REPL

4. **Phase 4 (Hot Reload → Polish)**:
   - Add workspace naming/renaming
   - Integrate with Observer and debugging tools
   - Add workspace resource monitoring

## Future Considerations: Namespaces

A future ADR will address namespaces (à la GNU Smalltalk). This section captures how namespaces would interact with workspaces.

### The Two Axes of Isolation

| Dimension | What it isolates | Mechanism |
|-----------|------------------|-----------|
| **Namespace** | Class definitions, name bindings | Compile-time name resolution |
| **Workspace** | Running processes, actor state | Runtime BEAM nodes |

These are **orthogonal**—you need both, and they compose:

```
                    Namespace A         Namespace B
                   ┌───────────────┬───────────────┐
    Workspace 1    │ Counter (v1)  │ Counter (v2)  │  ← Same node, different namespaces
    (BEAM node)    │ Actor <0.123> │ Actor <0.456> │
                   ├───────────────┼───────────────┤
    Workspace 2    │ Counter (v1)  │ Counter (v2)  │  ← Different node, same namespaces
    (BEAM node)    │ Actor <0.789> │ Actor <0.012> │
                   └───────────────┴───────────────┘
```

### Design Questions for Future ADR

**1. Where do namespaces live?**

| Option | Description | Pros | Cons |
|--------|-------------|------|------|
| **Per-workspace** | Each workspace has its own namespace tree | Full isolation, can experiment freely | Code not shared, duplication |
| **Global (code-level)** | Namespaces defined in source files, loaded into workspaces | Source-of-truth in files, hot reload works | All workspaces see same namespace structure |
| **Hybrid** | Source defines namespaces, workspaces can overlay/shadow | Flexible, experimental-friendly | Complexity, debugging confusion |

**Leaning toward:** Global namespaces from source, loaded into workspaces. Matches BEAM's module system.

**2. How do actors know their namespace?**

When you spawn an actor, which namespace context does it use?

```beamtalk
// In namespace Banking
counter := Counter spawn   // Which Counter? Banking::Counter or root Counter?
```

Options:
- **Lexical:** Actor uses namespace where `spawn` was written
- **Dynamic:** Actor uses REPL's current namespace
- **Explicit:** `counter := Banking::Counter spawn`

**Leaning toward:** Lexical default, explicit override. Like Erlang module calls.

**3. Can actors in different namespaces communicate?**

On BEAM, all pids are equal. Namespace is compile-time, not runtime. Actors are just processes—they don't carry namespace metadata at runtime (unless we add it).

**Leaning toward:** Yes, actors communicate freely. Namespaces are for code organization, not runtime isolation. Use supervision trees for runtime isolation.

**4. Hot reload scope?**

When you modify `Banking::Counter`, what happens?

| Scope | Behavior | Implication |
|-------|----------|-------------|
| **Namespace-scoped** | Only `Banking::Counter` reloads | Other namespaces' `Counter` unchanged |
| **All workspaces** | All workspaces with `Banking` namespace see change | Consistent, but can break running workspaces |
| **Per-workspace opt-in** | Workspace must `:reload Banking::Counter` | Safe, but manual |

**Leaning toward:** Namespace-scoped, pushed to all workspaces by default, with opt-out. Like Erlang's code server.

**5. Workspace-local namespace overlays?**

Can a workspace have its own experimental version of a namespace?

```bash
# Workspace: experiment
beamtalk repl --overlay Banking=./experimental/banking.bt
```

This would shadow `Banking::Counter` with the experimental version, only in this workspace.

**Leaning toward:** Yes, but explicit. Great for testing, dangerous if implicit.

### Proposed Interaction Model

```beamtalk
namespace Banking
  class Counter ... end
  class Account ... end
  
  namespace Internal    // Nested (GNU Smalltalk-style hierarchy)
    class AuditLog ... end
  end
end

// Usage
counter := Banking::Counter spawn
log := Banking::Internal::AuditLog spawn
```

**Interaction Rules (tentative):**

1. **Namespaces are loaded into workspaces** — workspace decides which namespaces to load
2. **Actors are namespace-unaware at runtime** — pids don't carry namespace metadata
3. **Hot reload is namespace-scoped** — `Banking::Counter` reloads without affecting `Parser::Counter`
4. **Workspace overlays shadow namespaces** — experimental workspace can override `Banking` without affecting others
5. **Name resolution is lexical** — code compiled in `Banking` resolves `Counter` to `Banking::Counter`

### Example Workflow

```bash
# Terminal 1: Normal development
cd ~/project
beamtalk repl
> import Banking
> counter := Counter spawn    // Banking::Counter
> counter increment

# Terminal 2: Experimental workspace with overlay
cd ~/project
beamtalk workspace create experiment --overlay Banking=./my-experimental-banking.bt
beamtalk repl --workspace experiment
> import Banking
> counter := Counter spawn    // Experimental Banking::Counter
> counter increment           // Uses experimental implementation

# Both workspaces coexist, different Counter implementations running
```

### Why This Matters for Workspaces

The key insight: **namespaces and workspaces serve different purposes**.

- **Namespaces** prevent name collisions and organize code (compile-time)
- **Workspaces** isolate running systems and preserve state (runtime)

A developer might:
- Use **one workspace** with **many namespaces** (normal development)
- Use **many workspaces** with **same namespaces** (parallel experiments)
- Use **workspace overlays** to test namespace changes safely

This ADR's workspace design supports all three patterns. The namespace ADR will formalize the compile-time side.

## Tooling Integration

Workspaces must integrate with modern development tooling (VS Code, LSP, DAP). This section defines how.

### Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│ Developer Machine                                             │
│                                                               │
│  ┌─────────────┐      ┌─────────────┐      ┌──────────────┐  │
│  │ VS Code     │      │ beamtalk-lsp│      │ Workspace    │  │
│  │             │←LSP─→│ (Rust)      │←TCP─→│ Node         │  │
│  │ Editor      │      │             │      │ (BEAM)       │  │
│  │ Terminal    │──────┼─────────────┼──────│              │  │
│  │ Debug       │←DAP─→│ DAP adapter │─────→│ Actors       │  │
│  └─────────────┘      └─────────────┘      └──────────────┘  │
│                                                               │
│  beamtalk-lsp:                                                │
│    - Owns file watching (*.bt files)                          │
│    - Parse + type-check on change (fast diagnostics)          │
│    - Compile + hot reload on save                             │
│    - Push .beam to workspace node                             │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

### LSP-Workspace Interaction

**Model: Hybrid** — LSP works standalone for static analysis, enhanced features when workspace node is available.

| LSP Feature | Static (no node) | Live (with node) |
|-------------|------------------|------------------|
| Completions | Class/method names from source | + Running actor names, live values |
| Hover | Type info, documentation | + Current actor state on hover |
| Diagnostics | Parse errors, type errors | + Runtime warnings from node |
| Go to definition | Source locations | Same |
| Rename/refactor | Source-only refactor | + Hot reload after rename |
| Code actions | Static fixes | + "Send message to actor" actions |

**Fallback behavior:** If no workspace node is running, LSP provides full static functionality. When node becomes available, enhanced features activate automatically.

```typescript
// VS Code status bar shows workspace state
[Beamtalk: my-project ● connected]   // Node running, live features
[Beamtalk: my-project ○ static]      // No node, static analysis only
```

### File Watching and Hot Reload

**Owner: LSP** (like rust-analyzer)

**Trigger: Auto-reload on save**

```
User edits counter.bt
        ↓
LSP detects change (file watcher)
        ↓
Parse + type-check (<50ms)
        ↓
Diagnostics appear in editor
        ↓
User saves (Ctrl+S)
        ↓
LSP compiles to counter.beam
        ↓
LSP pushes to workspace node (if connected)
        ↓
Node: code:load_binary(counter, Beam)
        ↓
Running actors get new code on next message
```

**File events:**

| Event | Action |
|-------|--------|
| File changed | Re-parse, update diagnostics |
| File saved | Compile, hot reload to node |
| File created | Add to project, compile |
| File deleted | Remove from code path, warn if in use |
| File renamed | Update references, reload |

### Debugging (DAP)

**Model: Erlang debugger via DAP adapter**

Leverage Erlang's built-in debugger infrastructure (`:int`, `:dbg` modules) with a DAP protocol wrapper. This is the same approach ElixirLS uses.

**Capabilities:**

| Feature | Support | Notes |
|---------|---------|-------|
| Breakpoints | ✓ | Line breakpoints via `:int` |
| Step over/into/out | ✓ | Standard Erlang debugger |
| Variable inspection | ✓ | View actor state, locals |
| Call stack | ✓ | Full stack trace |
| Conditional breakpoints | ✓ | Erlang debugger supports |
| Actor-specific | Future | Break on message receive, etc. |

**VS Code launch.json:**

```json
{
  "type": "beamtalk",
  "request": "attach",
  "name": "Attach to workspace",
  "workspace": "my-project"
}
```

**Debugging workflow:**

1. Set breakpoint in VS Code (click gutter)
2. LSP sends breakpoint to workspace node via `:int.break/2`
3. Actor hits breakpoint, pauses
4. DAP adapter notifies VS Code
5. VS Code shows call stack, variables
6. User steps/continues
7. Actor resumes

### VS Code Extension Features

The Beamtalk VS Code extension should provide:

| Feature | Description |
|---------|-------------|
| **Workspace status** | Status bar shows connected/disconnected |
| **Actor explorer** | Tree view of running actors in workspace |
| **REPL terminal** | Integrated terminal with `beamtalk repl` |
| **Hot reload indicator** | Flash notification on successful reload |
| **Commands** | `Beamtalk: Restart Workspace`, `Beamtalk: Attach`, etc. |
| **Diagnostics** | Problems panel with parse/type/runtime errors |
| **Debug** | Full DAP integration (breakpoints, stepping) |

**Actor Explorer tree view:**

```
WORKSPACE: my-project
├── Sessions
│   └── main (you)
├── Actors
│   ├── Counter #myCounter <0.123.0>
│   │   └── @value: 42
│   ├── Logger #logger <0.124.0>
│   └── HttpServer <0.125.0>
└── Modules
    ├── Counter (modified)
    ├── Logger
    └── HttpServer
```

### Diagnostics Pipeline

Diagnostics flow from multiple sources:

```
┌─────────────────┐
│ Source file     │
└────────┬────────┘
         ↓
┌─────────────────┐     ┌─────────────────┐
│ Parser          │────→│ Syntax errors   │──→ Problems panel
└────────┬────────┘     └─────────────────┘
         ↓
┌─────────────────┐     ┌─────────────────┐
│ Type checker    │────→│ Type errors     │──→ Problems panel
└────────┬────────┘     └─────────────────┘
         ↓
┌─────────────────┐     ┌─────────────────┐
│ Compiler        │────→│ Compile errors  │──→ Problems panel
└────────┬────────┘     └─────────────────┘
         ↓
┌─────────────────┐     ┌─────────────────┐
│ Workspace node  │────→│ Runtime warnings│──→ Problems panel
└─────────────────┘     └─────────────────┘
```

**Latency targets:**

| Stage | Target | Notes |
|-------|--------|-------|
| Parse | <20ms | Per-file, on keystroke |
| Type check | <50ms | Incremental |
| Compile | <200ms | Full file to .beam |
| Hot reload | <100ms | Push to node |
| Total save-to-running | <500ms | User-perceptible limit |

## Troubleshooting

Common issues and solutions:

| Problem | Symptoms | Solution |
|---------|----------|----------|
| **Workspace not responding** | `beamtalk repl` hangs or times out | Check if node is running: `ps aux \| grep beamtalk_workspace`. Kill stale process: `kill <PID>`. Then `beamtalk repl` creates fresh workspace. |
| **Port conflict** | "Address already in use" on startup | Another workspace or process on same port. Use `beamtalk workspace list` to see ports. Stop conflicting workspace or use `--port <N>` to specify alternate. |
| **Can't find my actor** | Variable unbound after reconnect | Actors survive, bindings don't. Use `Beamtalk actors` to list running actors, then rebind: `counter := Beamtalk actorNamed: #myCounter` |
| **Wrong workspace** | Connected to different project's workspace | Check `Beamtalk projectPath`. Use `beamtalk workspace list` to see all, then `beamtalk repl --workspace <name>` to connect to specific one. |
| **Cookie mismatch** | "Connection refused" or "not allowed to connect" | Cookie file corrupted or mismatched. Delete `~/.beamtalk/workspaces/<id>/cookie` and restart workspace. |
| **Orphaned workspace** | Workspace running but not in registry | Kill manually: find PID with `ps aux \| grep beamtalk_workspace_<name>`, then `kill <PID>`. Cleanup: `beamtalk workspace cleanup --force`. |
| **Hot reload not working** | Changes to `.bt` file not reflected | Check compiler daemon is running. Try `:reload` in REPL. Verify module loaded: `code:which(counter)`. |
| **Actor crashed** | `doesNotUnderstand:` or unexpected behavior | Check supervisor: `Beamtalk supervisorStatus`. Actor may have restarted with fresh state. Use `Beamtalk actorNamed:` to get new PID. |

## Prior Art: Alternative Persistence Models

Beyond Pharo and BEAM, several other systems offer instructive approaches to persistent development environments.

### Clojure nREPL

**Architecture:** Persistent JVM process with nREPL server. Editors (CIDER, Calva) connect via TCP using EDN message protocol.

```
┌─────────────────┐     ┌──────────────────────┐
│  CIDER/Calva    │     │   JVM Process        │
│  (Editor)       │────▶│   nREPL Server       │
│                 │ EDN │   Session A, B, ...  │
└─────────────────┘     └──────────────────────┘
```

**Key concepts:**
- **Sessions:** Each client gets isolated namespace state and bindings
- **Message protocol:** Structured ops (`eval`, `complete`, `load-file`) not raw text
- **Middleware:** Extensible handlers for completion, debugging, pretty-printing

**Relevance to Beamtalk:**
- ✅ Our node/session model is nearly identical
- 💡 Consider formal protocol spec like nREPL's message ops
- **Difference:** nREPL is single-JVM; we can run multiple nodes with actor supervision

### Unison (Content-Addressed Code)

**Architecture:** Code stored in SQLite database, identified by SHA3-512 hash of AST. Names are mutable pointers to immutable definitions.

```
┌─────────────────────────────────────────────────┐
│ Unison Codebase (SQLite)                         │
│                                                  │
│   Hash: abc123 → (+ 1 2)        ← Immutable     │
│   Hash: def456 → (fn [x] ...)   ← Immutable     │
│   Name: "increment" → def456    ← Mutable ptr   │
└─────────────────────────────────────────────────┘
```

**Key concepts:**
- **Content-addressed:** Code identified by hash, not file path
- **Immutable definitions:** Code never changes—new code = new hash
- **Mutable names:** Rename = change pointer, code unchanged
- **No files:** Source rendered to files for editing, but DB is truth
- **Perfect versioning:** All versions exist forever

**Relevance to Beamtalk:**
- 💡 Solves the `:save` problem—hot-reloaded code IS the persisted code
- 💡 Rename is free—change name pointer, all references update
- ⚠️ Radical departure from file-based workflow
- **Possible future:** Content-addressing internally, files as external interface

### Akka Persistence (Event Sourcing)

**Architecture:** Actors persist events (facts), not state. Recovery replays events to reconstruct state. Snapshots checkpoint periodically.

```
┌─────────────────────────────────────────────────┐
│ Persistent Actor                                 │
│                                                  │
│   Event Journal:                                 │
│   ├── Event 1: Incremented                      │
│   ├── Event 2: Incremented                      │
│   └── Snapshot @ Event 100: { count: 42 }       │
│                                                  │
│   Recovery: Load snapshot → replay events        │
└─────────────────────────────────────────────────┘
```

**Key concepts:**
- **Event sourcing:** Persist events, replay to recover state
- **Snapshots:** Checkpoint state to speed up recovery
- **Command/Event split:** Commands = requests, Events = facts (only events replayed)
- **Durable state:** Alternative CRUD-style persistence (latest state only)

**Relevance to Beamtalk:**
- 💡 Solves "node restart = state lost" with event replay
- 💡 Could be optional: `Counter spawnPersistent` vs `Counter spawn`
- ⚠️ Adds complexity: schema evolution, storage management
- **Possible future:** Event sourcing as opt-in for critical actors

### Jupyter (Persistent Kernels)

**Architecture:** Kernel process (Python/R/etc.) runs persistently. Jupyter Server manages sessions. Frontends connect via WebSocket.

```
┌─────────────────┐     ┌──────────────────────┐     ┌─────────────────┐
│  Browser/Lab    │     │   Jupyter Server     │     │   Kernel        │
│  (Frontend)     │────▶│   Session Manager    │────▶│   State in RAM  │
│                 │ WS  │   Kernel Manager     │ ZMQ │                 │
└─────────────────┘     └──────────────────────┘     └─────────────────┘
```

**Key concepts:**
- **Kernel = persistent process:** State survives cell runs and client disconnects
- **Session manager:** Maps notebooks to kernels, tracks metadata
- **Multiple clients:** Multiple frontends can connect to same kernel
- **No disk persistence:** Kernel restart = state lost (by design)

**Relevance to Beamtalk:**
- ✅ Our model is very similar (node = kernel, session = notebook)
- 💡 Session manager pattern for central registry
- 💡 WebSocket reconnection survives browser refresh
- **Difference:** Jupyter is cell-based (sequential); Beamtalk is actor-based (concurrent)

### Common Lisp (SLIME/Swank)

**Architecture:** Lisp image runs with Swank server. Emacs connects via SLIME. Image can be saved to disk.

```
┌─────────────────┐     ┌──────────────────────┐
│  Emacs + SLIME  │     │   Lisp Image         │
│  (Editor)       │────▶│   Swank Server       │
│                 │ TCP │   (full state)       │
└─────────────────┘     └──────────────────────┘
```

**Key concepts:**
- **Image-based:** Entire Lisp state serializable to disk
- **Swank protocol:** Editor talks to running Lisp
- **Hybrid workflow:** Code in files (for VCS) + live in image
- **Save-image command:** User explicitly persists state when wanted

**Relevance to Beamtalk:**
- 💡 Hybrid source-of-truth: files for VCS, image for live state
- 💡 Explicit save-state command for user control
- ⚠️ Same image problems as Pharo (large, platform-specific)
- **Difference:** BEAM can't serialize VM state like Lisp can

### Comparison Matrix

| System | Runtime Persistence | Code Persistence | State Recovery | Files? |
|--------|---------------------|------------------|----------------|--------|
| **Pharo** | Image snapshot | In image | Load image | No |
| **BEAM/Erlang** | Node running | Files → hot reload | None | Yes |
| **Clojure nREPL** | JVM running | Files → reload | None | Yes |
| **Unison** | Process running | Content-addressed DB | None | Derived |
| **Akka** | Process running | Files | Event replay | Yes |
| **Jupyter** | Kernel running | Notebook cells | None | Yes |
| **Common Lisp** | Image running | Image + files | Load image | Hybrid |
| **Beamtalk** | Node running | Files → hot reload | None* | Yes |

*Future enhancement: optional event sourcing for actor state persistence

### Potential Future Enhancements

Based on prior art analysis:

| Enhancement | Inspired By | Complexity | Value |
|-------------|-------------|------------|-------|
| **Formal message protocol** | nREPL | Medium | High (tooling) |
| **Content-addressed modules** | Unison | High | Medium (versioning) |
| **Optional event sourcing** | Akka | High | High (state survives) |
| **Session registry service** | Jupyter | Low | High (multi-client) |
| **Explicit save-state command** | SLIME | Medium | Medium (user control) |

These enhancements are not part of this ADR but inform future design decisions. See:
- **BT-253**: Research: Formal REPL message protocol (nREPL-style)
- **BT-254**: Research: Actor state persistence (Akka-style event sourcing)

## References

### Beamtalk Design
- **BT-182**: Persistent BEAM runtime across REPL restarts (child issue)
- **BT-184**: Workspace architecture design (child issue)
- **BT-185**: Epic: Persistent Workspace Management (parent issue)
- **BT-252**: ADR: Package Management and Build Tooling (related)
- Linear: <https://linear.app/beamtalk/issue/BT-185>

### BEAM/Erlang
- **Erlang Distributed Systems**: <https://www.erlang.org/doc/reference_manual/distributed.html>
- **OTP Supervisor Behavior**: <https://www.erlang.org/doc/design_principles/sup_princ.html>
- **Hot Code Reloading**: <https://www.erlang.org/doc/reference_manual/code_loading.html>
- **BEAM Wisdoms**: <http://beam-wisdoms.clau.se/en/latest/> (VM internals)

### Smalltalk/Pharo
- **Pharo by Example**: <https://books.pharo.org/> (image model documentation)
- **Tonel file format**: <https://github.com/pharo-vcs/tonel>

### Prior Art Systems
- **nREPL (Clojure)**: <https://nrepl.org/nrepl/design/overview.html>
- **Unison Language**: <https://www.unison-lang.org/docs/the-big-idea/>
- **Akka Persistence**: <https://doc.akka.io/libraries/akka-core/current/typed/persistence.html>
- **Jupyter Architecture**: <https://docs.jupyter.org/en/stable/projects/architecture/content-architecture.html>
- **SLIME/Swank**: <https://slime.common-lisp.dev/>

### Tooling
- **Language Server Protocol**: <https://microsoft.github.io/language-server-protocol/>
- **Debug Adapter Protocol**: <https://microsoft.github.io/debug-adapter-protocol/>
- **ElixirLS** (reference implementation): <https://github.com/elixir-lsp/elixir-ls>

## Notes

This decision represents a **philosophical choice**: Beamtalk is a **BEAM-first** language, not a Smalltalk clone. We prioritize:

1. **Erlang/OTP strengths** (supervision, distribution, hot reload) over Smalltalk conventions
2. **Simplicity and speed** (lightweight nodes) over completeness (full image persistence)
3. **Modern developer workflows** (git, multiple terminals) over single-image development

While Pharo's image model is elegant, it doesn't fit the BEAM's process-oriented architecture. Distributed nodes with hot reloading provide a better foundation for Beamtalk's actor-based paradigm.

**Key insight**: In Smalltalk, the image *is* the system. In Erlang/OTP, the system *is* a collection of supervised processes. Beamtalk embraces the latter.

This approach also aligns with modern trends:

- **Jupyter notebooks**: Persistent kernels that clients connect/disconnect from
- **tmux/screen**: Persistent terminal sessions with reconnection
- **Kubernetes pods**: Long-running processes with liveness probes and restarts
- **Hot reload**: Live in React, Next.js, Rust Analyzer—not just Smalltalk

By using BEAM's native distributed architecture, Beamtalk gets a robust, battle-tested foundation for persistent workspaces without reinventing the wheel.
