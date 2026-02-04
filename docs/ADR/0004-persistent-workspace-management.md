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
%% Workspace supervisor (top-level)
beamtalk_workspace_sup
  ├─ beamtalk_workspace_registry  % Track active workspaces
  ├─ beamtalk_workspace_cleaner   % Auto-cleanup abandoned workspaces
  └─ per-workspace supervision tree
       ├─ beamtalk_repl_server    % TCP server for REPL connections
       ├─ beamtalk_workspace_meta % Workspace metadata (last_active)
       └─ user_actor_sup          % Supervise spawned actors
            ├─ Counter#<0.123.0>
            ├─ Logger#<0.124.0>
            └─ ...
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

```erlang
%% Workspace cleaner runs every 10 minutes
-define(MAX_IDLE_TIME, 3600 * 4).  % 4 hours

cleanup_stale_workspaces() ->
    Workspaces = beamtalk_workspace_registry:list(),
    Now = erlang:system_time(second),
    [shutdown_workspace(Id) || {Id, #{last_active := LastActive}} <- Workspaces,
                                Now - LastActive > ?MAX_IDLE_TIME].
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

## References

- **Pharo by Example**: <https://books.pharo.org/> (image model documentation)
- **Erlang Distributed Systems**: <https://www.erlang.org/doc/reference_manual/distributed.html>
- **OTP Supervisor Behavior**: <https://www.erlang.org/doc/design_principles/sup_princ.html>
- **Hot Code Reloading**: <https://www.erlang.org/doc/reference_manual/code_loading.html>
- **BEAM Wisdoms**: <http://beam-wisdoms.clau.se/en/latest/> (VM internals)
- **BT-182**: Persistent BEAM runtime across REPL restarts (child issue)
- **BT-184**: Workspace architecture design (child issue)
- **BT-185**: Epic: Persistent Workspace Management (parent issue)
- Linear: <https://linear.app/beamtalk/issue/BT-185>

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
