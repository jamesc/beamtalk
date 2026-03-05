# ADR 0051: Subprocess Execution — System Commands and Interactive Subprocess Actor

## Status
Proposed (2026-03-05)

## Context

### Problem Statement

Beamtalk has no way to execute external OS commands. The existing `Port` class (`stdlib/src/Port.bt`) is an opaque wrapper for BEAM port identifiers — it provides `asString`, `=:=`, and `hash`, but cannot create, communicate with, or manage subprocesses.

This blocks two categories of use cases:

1. **Simple commands** — run `git status`, capture output, check the exit code. Every developer expects this from a general-purpose language.
2. **Interactive long-lived subprocesses** — launch a daemon process, write to its stdin, read its stdout line-by-line, and manage its lifecycle. This is required for building orchestration services like Symphony (BT-1123) where a coding agent subprocess runs for minutes to hours with bidirectional JSON-RPC communication.

This ADR addresses both use cases:
- **Tier 1** (System class methods) — one-shot command execution with blocking capture and streaming output.
- **Tier 2** (Subprocess actor) — interactive long-lived subprocesses with bidirectional stdin/stdout communication, modeled as an Actor that owns the port and mediates access via sync message sends.

### Current State

- `Port.bt` provides only display/comparison methods for port identifiers received from Erlang interop
- No `beamtalk_port.erl` runtime module exists — port primitives are handled inline in `beamtalk_primitive.erl`
- The workaround is raw Erlang FFI: `Erlang os cmd: "ls -la"` — which has no exit code, no streaming, shell injection risk, and blocks the caller
- ADR 0021 explicitly deferred `Process output: "ls -la"` as a future Stream integration point
- ADR 0022 established the OTP Port pattern for the compiler: `open_port({spawn_executable, ...})` with `{packet, 4}`, ETF framing, and OTP supervision — this is the internal template for subprocess management
- `System.bt` already handles OS-level concerns (`getEnv:`, `osPlatform`, `pid`) — a natural home for simple command execution
- `Actor.bt` provides `spawn` / `spawnWith:` for creating actor processes with `gen_server`-backed lifecycle — the foundation for interactive subprocess management

### Constraints

1. **BEAM port limitations** — `open_port/2` cannot deliver stdout and stderr as separate streams. `stderr_to_stdout` merges them; otherwise stderr goes to the VM's terminal unseen. Separate stderr requires a helper process. A Rust helper binary (`beamtalk_exec`) provides this — the BEAM communicates with the helper via an ETF port protocol (same pattern as `beamtalk_compiler_port`), and the helper manages the actual subprocess with separate stdio pipes.
2. **Port ownership is process-local** — the process that calls `open_port/2` becomes the port owner. Only the owner can send commands. Port-backed streams have the same cross-process constraint as file-backed streams (ADR 0021). This means a port-backed Stream cannot be returned from an actor to its caller — the Stream generator would run in the caller's process but the port lives in the actor's process.
3. **Zombie process risk** — closing a port sends EOF to the subprocess's stdin. Programs that don't monitor stdin for EOF become orphans. The BEAM has no built-in mechanism to forcefully terminate port children.
4. **Security** — shell invocation (`{spawn, Command}`) enables injection. `{spawn_executable, Path}` with `{args, List}` is safe but requires absolute paths resolved via `os:find_executable/1`.
5. **Actor message model** — ADR 0043 establishes sync-by-default messaging (`.` = `gen_server:call`). All actor methods must return complete values, not lazy generators that depend on actor-internal resources. This shapes the Tier 2 API: `readLine` returns a String (or nil), not a Stream.

## Decision

### Tier 1: System Class Methods (One-Shot Commands)

Add **three new class methods to `System`** for one-shot command execution: blocking capture, streaming output, and block-scoped streaming with deterministic cleanup. Introduce a `CommandResult` value class for structured results.

All methods use `spawn_executable` with explicit argument lists — no shell invocation, no injection risk. This is the same approach recommended by the EEF Security Working Group.

#### API

```beamtalk
// Blocking capture — run to completion, return result value
result := System run: "git" args: #("status")
result output      // => "On branch main\nnothing to commit\n"
result exitCode    // => 0
result isSuccess   // => true

// With custom environment and working directory
result := System run: "make" args: #("test") env: #{
  "CC" => "clang",
  "CFLAGS" => "-O2"
} dir: "/path/to/project"

// Streaming output — lazy Stream of stdout lines (like File lines:)
(System output: "find" args: #(".", "-name", "*.bt")) do: [:line |
  Transcript show: line
]

// Compose with Stream pipeline
(System output: "cat" args: #("server.log"))
  select: [:line | line includesSubstring: "ERROR"]
  take: 10

// Block-scoped streaming with deterministic cleanup (like File open:do:)
System output: "tail" args: #("-f", "server.log") do: [:stream |
  stream take: 100
]
// subprocess terminated and port closed when block exits
```

**Return types:**
- `System run:args:` returns a `CommandResult` value object with `output` (String, stdout), `stderr` (String), `exitCode` (Integer), and `isSuccess` (Boolean)
- `System output:args:` returns a `Stream` of lines (lazy, constant memory) with a finalizer that closes the port and terminates the subprocess
- `System output:args:do:` evaluates the block with a `Stream`, then deterministically closes the port and terminates the subprocess when the block returns — even on exception. This is the preferred form for long-running commands where cleanup timing matters.

#### `CommandResult` Value Class

```beamtalk
sealed Value subclass: CommandResult

  /// The subprocess stdout as a String.
  output -> String => @primitive "output"

  /// The subprocess stderr as a String.
  stderr -> String => @primitive "stderr"

  /// The process exit code (0 = success by convention).
  exitCode -> Integer => @primitive "exitCode"

  /// True if exitCode is 0.
  isSuccess -> Boolean => self exitCode =:= 0
```

### Tier 2: Interactive Subprocess Actor

Add a `Subprocess` Actor subclass for bidirectional communication with long-lived OS processes. The actor owns the port, buffers stdout data internally, and exposes sync methods for reading/writing.

#### Design Resolution: Why Not Streams?

The initial design considered returning a Stream from the actor (e.g., `agent lines.`). This fails for three reasons, all now resolved:

1. **Cross-process Stream constraint (ADR 0021)** — the port lives in the actor's gen_server process, but a returned Stream's generator would execute in the caller's process. Port-backed Streams cannot cross process boundaries.

   **Resolution:** Don't use Streams. The actor buffers port output internally and exposes `readLine` as a sync `gen_server:call`. Each call returns the next buffered line (String) or `nil` at EOF. No Stream crosses the process boundary.

2. **Actor constructor pattern** — `Actor.bt` only has `spawn` and `spawnWith:`. A `Subprocess open: "cmd" args: #("a")` factory method doesn't exist in the current protocol.

   **Resolution:** `spawnWith:` with a config dictionary is sufficient. The Erlang-side `init/1` callback receives the config map and opens the port. A convenience class method `open:args:` desugars to `spawnWith:`:

   ```beamtalk
   class open: command args: args =>
     self spawnWith: #{"command" => command, "args" => args}

   class open: command args: args env: env dir: dir =>
     self spawnWith: #{"command" => command, "args" => args, "env" => env, "dir" => dir}
   ```

   This requires no new Actor protocol machinery — it's a standard class method calling the existing `spawnWith:`. The config dictionary supports optional `"env"` (Dictionary of String => String) and `"dir"` (String working directory) keys — omitted keys inherit from the parent process.

3. **Sync-messaging conflict (ADR 0043)** — `.` terminator = `gen_server:call`. Returning a lazy Stream from a sync call is semantically broken when the Stream's generator needs the actor's port.

   **Resolution:** Already solved by point 1. All methods return simple values (String, Integer, Boolean, nil). `readLine.` is a sync call that returns the next line. No lazy data structures escape the actor.

#### API

```beamtalk
// Spawn a subprocess — command and args passed via spawnWith:
agent := Subprocess spawnWith: #{
  "command" => "codex",
  "args" => #("--full-auto", "fix the login bug")
}

// Convenience factory method (desugars to spawnWith:)
agent := Subprocess open: "codex" args: #("--full-auto", "fix the login bug")

// With custom environment and working directory
agent := Subprocess open: "make" args: #("test") env: #{
  "CC" => "clang",
  "CFLAGS" => "-O2"
} dir: "/path/to/project"

// Write a line to the subprocess's stdin
agent writeLine: (JSON generate: #{
  "jsonrpc" => "2.0",
  "method" => "initialize",
  "id" => 1,
  "params" => #{"model" => "gpt-4"}
}).

// Read one line from stdout (blocks forever until a line is available or EOF)
line := agent readLine.       // => "{\"jsonrpc\":\"2.0\",\"result\":...}"

// Read with timeout (milliseconds) — returns nil on timeout or EOF
line := agent readLine: 5000. // => String, or nil after 5 seconds

// Non-blocking check — returns nil immediately if no data buffered
line := agent tryReadLine.    // => String if data available, nil otherwise

// Read in a loop until EOF
line := agent readLine.
[line notNil] whileTrue: [
  event := JSON parse: line.
  Transcript show: event.
  line := agent readLine.
]

// Timeout-based loop — detect hung subprocess
line := agent readLine: 30000.
[line notNil] whileTrue: [
  Transcript show: line.
  line := agent readLine: 30000.
].
line isNil ifTrue: [
  agent isAlive ifTrue: [
    Transcript show: "Subprocess appears hung — killing".
    agent close.
  ]
]

// Check if the subprocess has exited
agent isAlive.                // => true / false (inherited from Actor)

// Get exit code (nil if still running)
agent exitCode.               // => 0, 1, ..., or nil

// Graceful shutdown — closes stdin (sends EOF), waits for exit
agent stop.                   // inherited from Actor

// Force kill — terminates OS process immediately
agent close.                  // port_close + OS kill signal
```

#### Class Hierarchy

Both pull-mode (`Subprocess`) and push-mode (`SubprocessListener`) actors share port lifecycle, stdin writing, and exit code management via a common abstract base class:

```
Actor
└── SubprocessBase (abstract — owns port, lifecycle, writeLine:)
    ├── Subprocess (pull — readLine, buffering, deferred reply)
    └── SubprocessListener (push — onLine:, onStderrLine:, onExit:)
```

#### SubprocessBase — Shared Port Lifecycle

```beamtalk
Actor subclass: SubprocessBase
  state: port = nil
  state: exitCode = nil
  state: portClosed = false

  /// Convenience factory — desugars to spawnWith:
  class open: command args: args =>
    self spawnWith: #{"command" => command, "args" => args}

  /// Convenience factory with environment and working directory.
  class open: command args: args env: env dir: dir =>
    self spawnWith: #{"command" => command, "args" => args, "env" => env, "dir" => dir}

  /// Write a line to the subprocess's stdin (appends newline).
  writeLine: data -> Nil => @primitive "writeLine:"

  /// Get the exit code. Returns nil if the subprocess is still running.
  exitCode -> Integer | Nil => @primitive "exitCode"

  /// Force-close the subprocess (sends kill to process group).
  close -> Nil => @primitive "close"
```

#### Subprocess — Pull Mode (Buffered Reads)

For bidirectional protocols (JSON-RPC, orchestration) where the caller drives the conversation.

```beamtalk
SubprocessBase subclass: Subprocess
  state: stdoutBuffer = nil
  state: stderrBuffer = nil

  /// Read one line from stdout. Blocks forever until available. Returns nil at EOF.
  readLine -> String | Nil => @primitive "readLine"

  /// Read one line from stdout with timeout (ms). Returns nil on timeout or EOF.
  readLine: timeout -> String | Nil => @primitive "readLine:"

  /// Non-blocking read from stdout. Returns nil immediately if no data buffered.
  tryReadLine -> String | Nil => @primitive "tryReadLine"

  /// Read one line from stderr. Blocks forever until available. Returns nil at EOF.
  readStderrLine -> String | Nil => @primitive "readStderrLine"

  /// Read one line from stderr with timeout (ms). Returns nil on timeout or EOF.
  readStderrLine: timeout -> String | Nil => @primitive "readStderrLine:"

  /// Non-blocking read from stderr. Returns nil immediately if no data buffered.
  tryReadStderrLine -> String | Nil => @primitive "tryReadStderrLine"
```

#### SubprocessListener — Push Mode (Callbacks)

For event-driven use cases (log tailing, build output, monitoring) where the subprocess drives the flow. Callbacks execute in the actor's process — no cross-process constraint.

```beamtalk
SubprocessBase subclass: SubprocessListener
  state: lineCallback = nil
  state: stderrLineCallback = nil
  state: exitCallback = nil

  /// Register a callback for each stdout line. Replaces any previous callback.
  onLine: aBlock -> Nil => @primitive "onLine:"

  /// Register a callback for each stderr line. Replaces any previous callback.
  onStderrLine: aBlock -> Nil => @primitive "onStderrLine:"

  /// Register a callback for subprocess exit. Receives the exit code.
  onExit: aBlock -> Nil => @primitive "onExit:"
```

**Usage:**

```beamtalk
// Event-driven log monitoring
tail := SubprocessListener open: "tail" args: #("-f", "server.log").
tail onLine: [:line |
  (line includesSubstring: "ERROR") ifTrue: [
    Transcript show: "ALERT: ", line
  ]
].
tail onExit: [:code |
  Transcript show: "tail exited with code ", code asString
].

// Build output streaming
build := SubprocessListener open: "make" args: #("all").
build onLine: [:line | Transcript show: line].
build onStderrLine: [:line | Transcript show: "[stderr] ", line].
build onExit: [:code |
  code =:= 0
    ifTrue: [Transcript show: "Build succeeded"]
    ifFalse: [Transcript show: "Build failed"]
].
```

**Callback semantics:**
- Callbacks execute in the actor's gen_server process, not the caller's — so they are serialized and safe to mutate actor state from.
- `onLine:` replaces any previous stdout callback. Pass `nil` to unregister.
- Lines that arrive before `onLine:` is registered are dropped (no buffering in push mode).
- If no callback is registered for a channel, lines on that channel are silently discarded.
- `onExit:` fires once when the subprocess exits, after all buffered lines have been delivered to `onLine:`/`onStderrLine:`.

#### Runtime Implementation

All three classes are backed by hand-written Erlang modules rather than generated codegen. This follows the same pattern as `beamtalk_compiler_port.erl` — specialized port management that needs direct `handle_info` control.

- `beamtalk_subprocess_base.erl` — shared `init/1` (opens port), `writeLine`, `exitCode`, `close`, `terminate/2` cleanup
- `beamtalk_subprocess.erl` — extends base with buffering, deferred reply, timeout management
- `beamtalk_subprocess_listener.erl` — extends base with callback dispatch, no buffering

The `handle_info` for incoming data differs between the two subclasses:

Key runtime mechanics:

```erlang
%% In beamtalk_subprocess.erl
%%
%% The BEAM port connects to the beamtalk_exec Rust helper, which
%% communicates via ETF over {packet, 4}. The helper sends tagged
%% tuples: {stdout, ChildId, Data}, {stderr, ChildId, Data},
%% {exit, ChildId, Code}.

%% Helper messages arrive as ETF via {packet,4} — decode and dispatch
handle_info({Port, {data, Packet}}, #{port := Port} = State) ->
    case erlang:binary_to_term(Packet) of
        {stdout, _ChildId, Data} ->
            buffer_and_maybe_reply(stdout, Data, State);
        {stderr, _ChildId, Data} ->
            buffer_and_maybe_reply(stderr, Data, State);
        {exit, _ChildId, Code} ->
            NewState = State#{exit_code => Code, port_closed => true},
            %% If readLine callers are waiting and buffers are empty, reply nil (EOF)
            S1 = maybe_reply_eof(stdout, NewState),
            S2 = maybe_reply_eof(stderr, S1),
            {noreply, S2};
        _Other ->
            {noreply, State}
    end;

handle_info(Msg, State) ->
    beamtalk_actor:handle_info(Msg, State).

%% Shared helper — buffers data for stdout or stderr, replies to waiting caller
buffer_and_maybe_reply(Channel, Data, State) ->
    BufKey = {Channel, buffer},
    PendKey = {Channel, pending},
    WaitKey = {Channel, waiting},
    Buffer = maps:get(BufKey, State),
    Pending = maps:get(PendKey, State, <<>>),
    Combined = <<Pending/binary, Data/binary>>,
    {Lines, Remainder} = split_lines(Combined),
    NewBuffer = queue:join(Buffer, queue:from_list(Lines)),
    case maps:get(WaitKey, State, undefined) of
        undefined ->
            {noreply, State#{BufKey => NewBuffer, PendKey => Remainder}};
        From when not queue:is_empty(NewBuffer) ->
            {{value, Line}, Rest} = queue:out(NewBuffer),
            gen_server:reply(From, Line),
            {noreply, State#{BufKey => Rest, PendKey => Remainder,
                             WaitKey => undefined}}
    end.

%% readLine — blocks forever until a line or EOF
handle_readLine([], From, State) ->
    read_line_for(stdout, From, infinity, State).

%% readLine: — blocks up to Timeout ms, returns nil on timeout
handle_readLine([Timeout], From, State) ->
    read_line_for(stdout, From, Timeout, State).

%% tryReadLine — non-blocking, returns nil if no data buffered
handle_tryReadLine([], _From, State) ->
    try_read_line_for(stdout, State).

handle_readStderrLine([], From, State) ->
    read_line_for(stderr, From, infinity, State).

handle_readStderrLine([Timeout], From, State) ->
    read_line_for(stderr, From, Timeout, State).

handle_tryReadStderrLine([], _From, State) ->
    try_read_line_for(stderr, State).

read_line_for(Channel, From, Timeout, State) ->
    BufKey = {Channel, buffer},
    WaitKey = {Channel, waiting},
    Buffer = maps:get(BufKey, State),
    case queue:is_empty(Buffer) of
        false ->
            {{value, Line}, Rest} = queue:out(Buffer),
            {reply, Line, State#{BufKey => Rest}};
        true when maps:get(port_closed, State, false) ->
            %% EOF — no more data coming
            {reply, nil, State};
        true when Timeout =:= infinity ->
            %% No data yet — stash From, reply from handle_info
            {noreply, State#{WaitKey => From}};
        true ->
            %% No data yet — stash From with timer ref
            TimerRef = erlang:send_after(Timeout, self(),
                                         {read_timeout, Channel}),
            {noreply, State#{WaitKey => {From, TimerRef}}}
    end.

try_read_line_for(Channel, State) ->
    BufKey = {Channel, buffer},
    Buffer = maps:get(BufKey, State),
    case queue:is_empty(Buffer) of
        false ->
            {{value, Line}, Rest} = queue:out(Buffer),
            {reply, Line, State#{BufKey => Rest}};
        true ->
            {reply, nil, State}
    end.

%% writeLine — sync call, writes to port stdin
handle_writeLine([Data], _From, #{port := Port} = State) ->
    Line = <<(iolist_to_binary(Data))/binary, $\n>>,
    port_command(Port, Line),
    {reply, nil, State}.
```

The deferred reply pattern means `readLine` blocks the caller's `gen_server:call` until either:
- New data arrives via `handle_info` → the actor calls `gen_server:reply(From, Line)`
- The subprocess exits with empty buffer → the actor calls `gen_server:reply(From, nil)`

This is standard OTP — `handle_call` returns `{noreply, State}` to defer the reply, stashing `From` in the state. When data arrives via `handle_info`, the actor calls `gen_server:reply(From, Value)` to unblock the caller. The same pattern is used for both `readLine` (stdout) and `readStderrLine` (stderr), each with independent buffers and waiting callers.

**Timeout handling:** Three read variants cover different needs:
- `readLine` — blocks forever (`gen_server:call` with `infinity` timeout). For loops where data is expected eventually.
- `readLine: timeout` — blocks up to `timeout` milliseconds. The runtime uses `erlang:send_after/3` to schedule a `{read_timeout, Channel}` message. If the timer fires before data arrives, `handle_info` replies `nil` and cancels the wait. Returns `nil` on timeout or EOF.
- `tryReadLine` — non-blocking. Returns the next buffered line immediately, or `nil` if the buffer is empty. Does not stash `From` or defer the reply.

All three variants exist for stderr too (`readStderrLine`, `readStderrLine:`, `tryReadStderrLine`).

ADR 0043 notes that `gen_server:call` defaults to a 5000ms timeout. For the blocking `readLine` variant, the runtime must use `gen_server:call(AgentPid, {readLine, []}, infinity)` — a subprocess may produce no output for minutes (e.g., a coding agent thinking). For `readLine:`, the gen_server:call also uses `infinity` — the timeout is managed internally via `erlang:send_after`, not via OTP's call timeout, so the actor can clean up its waiting state properly.

#### Actor Lifecycle and Cleanup

```
 Subprocess spawn         stop (graceful)        close (forced)
      │                       │                       │
      ▼                       ▼                       ▼
  open_port ──data──▶  port_close (EOF)         port_close + kill
      │                  │                         │
      ▼                  ▼                         ▼
  handle_info      wait for exit_status       OS process killed
  buffers data       (timeout → kill)           immediately
      │                  │
      ▼                  ▼
  readLine ◄─── gen_server:call ───▶ nil (EOF)
  writeLine ──▶ port_command
```

- **`stop.`** (inherited from Actor) — gracefully stops the gen_server, which triggers `terminate/2`. The `beamtalk_subprocess.erl` terminate callback (not inherited — Subprocess overrides it) closes the port (sends EOF to stdin), waits briefly for the subprocess to exit, then sends SIGTERM. If the subprocess still hasn't exited after a grace period, it sends SIGKILL.
- **`close.`** — force-closes the port and sends SIGTERM followed by SIGKILL to the OS process. This is the explicit "shut down now" method for when `stop` is too gentle.
- **`kill.`** (inherited from Actor) — sends `exit(Pid, kill)` to the actor's gen_server process. **Warning:** SIGKILL to the BEAM process bypasses `terminate/2`, which means `cleanup_port/1` does not run. The OS subprocess may become a zombie. However, the port is linked to the actor process, so port closure still sends EOF to the subprocess's stdin. Programs that monitor stdin for EOF will exit. Programs that don't will be orphaned. For this reason, prefer `close.` (which cleans up deterministically) over `kill.` for Subprocess actors. Document this prominently.
- **BEAM process crash** — if the actor's gen_server crashes (not killed), `terminate/2` runs with `Reason =/= kill`, so cleanup runs normally. The port is also linked to the owning process, providing a backup EOF signal.
- **Deferred readLine callers** — if the actor stops while a caller is blocked on `readLine`, the caller receives an exit signal from the gen_server (standard OTP behavior for `gen_server:call` when the server dies).

### Shared Security Model

Both Tier 1 and Tier 2 share the same security model:

1. **No shell invocation** — always `spawn_executable`, never `spawn`. Arguments are passed as a list directly to `execve(3)`, preventing injection.
2. **PATH resolution** — command names without `/` are resolved via `os:find_executable/1`. Absolute paths are allowed (unlike File's relative-only policy) because executables legitimately live in system directories.
3. **Argument validation** — all arguments must be strings. Non-string values raise `#type_error`.
4. **No shell metacharacters** — since there's no shell, `|`, `>`, `&&`, `;`, backticks, and `$()` are literal characters, not operators. Users who need piping must compose at the Beamtalk level:

```beamtalk
// Instead of: System run: "cat file.txt | grep ERROR" (no shell!)
// Do:
(System output: "cat" args: #("file.txt"))
  select: [:line | line includesSubstring: "ERROR"]
```

**Windows caveat:** On Windows, `os:find_executable/1` searches `PATHEXT` and may resolve to `.bat`/`.cmd` files, which implicitly invoke `cmd.exe` when passed to `spawn_executable`. This partially defeats the "no shell" guarantee. The implementation must detect `.bat`/`.cmd` resolution on Windows and either reject the command with a clear error or document the risk. This is a known limitation of `spawn_executable` on Windows, not specific to Beamtalk.

### Error Handling

```beamtalk
// Command not found (Tier 1 and Tier 2)
System run: "nonexistent" args: #()
// => Error: #command_not_found "Command not found: nonexistent"
//    Hint: "Check that the command is installed and on $PATH"

// Non-zero exit (NOT an error — exit code is in the result)
result := System run: "grep" args: #("pattern", "file.txt")
result exitCode    // => 1 (no match)
result isSuccess   // => false

// Non-string argument
System run: 42 args: #()
// => Error: #type_error "run:args: expects a String command, got Integer"

// Writing to a closed subprocess (Tier 2)
agent writeLine: "hello".
// => Error: #port_closed "Cannot write to closed subprocess"
//    Hint: "The subprocess has exited with code 1"
```

### Stderr Handling

The Rust helper binary (`beamtalk_exec`) manages separate stdout and stderr pipes to the subprocess. Both streams are forwarded to the BEAM over the ETF port protocol with tagged messages (`{stdout, Data}` / `{stderr, Data}`).

**Tier 1:** `CommandResult` exposes both `output` (stdout) and `stderr` as separate strings:

```beamtalk
result := System run: "gcc" args: #("-Wall", "main.c")
result output      // => "" (stdout — empty for compile-only)
result stderr      // => "main.c:5: warning: unused variable\n"
result exitCode    // => 0
```

**Tier 2:** `Subprocess` provides `readLine` (stdout) and `readStderrLine` (stderr) as separate methods:

```beamtalk
agent := Subprocess open: "codex" args: #("--full-auto", "fix the bug")
line := agent readLine.              // reads from stdout
errLine := agent readStderrLine.     // reads from stderr
```

Both methods use the same deferred-reply buffering pattern, with independent queues for stdout and stderr.

### Zombie Process Prevention

The Rust helper binary (`beamtalk_exec`) handles all subprocess lifecycle management, including clean shutdown and process group kill. This eliminates the PID reuse, grandchild orphaning, and shell-based kill issues that raw `open_port` would have.

**Rust helper cleanup sequence:**
1. The helper spawns each subprocess in its own process group (Unix: `setsid()`; Windows: Job Object)
2. On shutdown request from the BEAM: sends SIGTERM to the process group, waits up to 2 seconds, then sends SIGKILL to the process group
3. Process-group kill ensures grandchild processes are also terminated — no orphaning
4. On Windows: `TerminateJobObject()` kills all processes in the Job Object

**Tier 1 (System output:):**
1. **Stream finalizer** — `System output:args:` attaches a Stream finalizer that sends a shutdown command to the Rust helper. The finalizer runs when the stream is fully consumed or when a terminal operation completes.
2. **Block-scoped cleanup** — `System output:args:do:` sends a shutdown command to the helper in an `ensure:` block when the block exits (normally or via exception). This is the deterministic alternative — use it for long-running commands where relying on the finalizer is insufficient.
3. **Process linking** — the BEAM port to the Rust helper is linked to the calling process. If the caller crashes, the port closes, and the helper cleans up the subprocess.

**Tier 2 (Subprocess actor):**
4. **Actor lifecycle** — the port to the Rust helper is owned by the actor's gen_server process. When the actor stops (via `stop.` or crash), `terminate/2` sends a shutdown command to the helper, which cleans up the subprocess with the SIGTERM → SIGKILL sequence.
5. **Explicit close** — `agent close.` sends an immediate kill command to the helper.
6. **Supervision** — Subprocess actors can be supervised like any other Actor. If the supervisor restarts the actor, a new subprocess is spawned via the helper.

**Rust helper crash safety:** If the Rust helper itself crashes or is killed, the subprocess's process group becomes orphaned. However, the helper is a simple, well-tested binary with no dynamic memory management concerns — it is far less likely to crash than a complex C++ program. If the BEAM VM exits, the helper detects stdin EOF and cleans up all managed subprocesses before exiting.

### Abandoned Stream Cleanup (Tier 1)

An abandoned `System output:` Stream (assigned to a variable but never fully consumed) will not be cleaned up until the owning process exits. For long-running REPL sessions, this means a `tail -f` subprocess could leak indefinitely. Mitigation:
- Use `System output:args:do:` (block-scoped, deterministic cleanup) for commands that may run indefinitely
- The Stream finalizer runs when a terminal operation completes, preventing leaks in normal pipeline usage
- Document prominently: "Prefer `output:args:do:` for long-running or potentially infinite commands"

### Cross-Process Constraints

**Tier 1:** Port-backed Streams from `System output:` have the same cross-process limitation as file-backed Streams (ADR 0021): they must be consumed by the same process that created them. Do not pass a `System output:` Stream to an actor — materialize it first with `asList` or `take:`.

**Tier 2:** No cross-process constraint for callers. The actor owns the port internally and exposes only simple values (String, Integer, nil) via sync message sends. Any process can call `agent readLine.` — the actor mediates all port access. This is the key advantage of the actor model for interactive subprocesses.

## Prior Art

### Erlang/OTP: `open_port/2` and `os:cmd/1`
- `os:cmd/1` — blocking, shell-invoked, no exit code, no streaming. Adequate for trusted one-liners only.
- `open_port({spawn_executable, Path}, Opts)` — full control, no shell, exit status via `{Port, {exit_status, N}}`, but single-channel (no separate stderr), port-owner-only communication, zombie risk.
- **Adopted:** `spawn_executable` as the foundation. `exit_status` option for exit code capture. `stderr_to_stdout` for merged output.
- **Not adopted:** `{spawn, Command}` (shell injection risk), `os:cmd` (no exit code, no streaming).

### Elixir: `System.cmd/3` and `Port.open/2`
- `System.cmd("git", ["status"])` — returns `{output, exit_code}`. Uses `spawn_executable` internally. No stdin, no streaming, blocks until exit.
- `Port.open/2` wraps `open_port` with Elixir syntax.
- **Adopted:** `System.cmd` as the model for `System run:args:` (blocking capture with exit code).
- **Improved:** Added streaming via `System output:args:` (Elixir has no built-in lazy stream over port output). Added interactive actor via `Subprocess` (Elixir has no built-in actor wrapper for ports).

### Pharo Smalltalk: `OSSubprocess`
- Mutable object model: create, configure (`redirectStdout`, `redirectStderr`), then `run`.
- Separate stdout/stderr — trivially achievable because the Pharo VM does `fork/exec` with direct fd control.
- `waitForExitPollingEvery:retrievingStreams:` — polls and drains output buffers to prevent pipe deadlock.
- Manual cleanup required (`closeAndCleanStreams`).
- **Adopted:** Concept of result object with exit status and output. Concept of an interactive process handle (Pharo's `OSSUnixSubprocess` ↔ Beamtalk's `Subprocess` actor).
- **Rejected:** Mutable configure-then-run pattern — doesn't fit Beamtalk's value-object philosophy (ADR 0042). Configuration is passed as method arguments instead.
- **Adapted:** Pharo's `stdout` stream → Beamtalk's `readLine` sync method. The BEAM's cross-process port constraint prevents exposing a Stream, so we use per-line sync calls instead. This is actually simpler and avoids Pharo's pipe-deadlock polling problem.
- **Achieved via helper:** Separate stderr — the Rust helper binary provides separate stdout/stderr pipes, matching Pharo's fd-level control.

### Newspeak
- No subprocess concept — deliberately, as an object-capability language. OS authority must be explicitly injected. Beamtalk is more pragmatic about OS access.

### erlexec (Erlang library) and Porcelain (Elixir library)
- erlexec uses a C++ helper process for separate stdout/stderr, signal management, and process group control. **Unix-only** — does not support Windows.
- Porcelain uses a Go helper binary (goon) for the same purpose, also Unix-focused.
- Both confirm that separate stderr on BEAM requires a helper process.
- **Adopted (architecture):** The helper-process pattern is the right approach. Beamtalk uses a Rust helper binary (`beamtalk_exec`) instead of erlexec's C++ or Porcelain's Go, because: (a) Beamtalk already has a Rust toolchain and the `beamtalk_compiler_port` establishes the ETF-over-port protocol pattern, (b) Rust's `std::process::Command` provides cross-platform subprocess management (Linux, macOS, Windows), and (c) Rust eliminates the C++ memory safety concerns of erlexec's implementation.
- **Not adopted (erlexec directly):** erlexec is Unix-only, requires a C++ compiler, and is a runtime dependency we'd need to maintain. Building the equivalent functionality in Rust (~400-500 lines) is a small effort and gives us cross-platform support and full control.

### Node.js `child_process`
- `child_process.spawn()` returns a `ChildProcess` object with `.stdin` (writable stream), `.stdout` (readable stream), and `.stderr` (readable stream).
- Event-driven: `.on('data', ...)`, `.on('close', ...)`.
- **Relevant for Tier 2:** The `Subprocess` actor is conceptually similar — a handle to a long-running child process with separate read/write channels. The key difference is that Beamtalk uses sync message sends (`readLine.`, `writeLine:.`) instead of event callbacks, consistent with ADR 0043's sync-by-default messaging.

## User Impact

### Newcomer
- **Tier 1:** `System run: "git" args: #("status")` is immediately intuitive — similar to Python's `subprocess.run()` or Node's `execFileSync()`.
- **Tier 2:** `agent := Subprocess open: "cmd" args: #("a")` then `agent readLine.` / `agent writeLine: "data".` follows a familiar handle-based pattern from Node.js `child_process.spawn()`.
- `System output:` returning a Stream follows the same pattern as `File lines:` — consistent mental model.
- Error messages guide toward correct usage (command not found → check PATH, type error → use strings).

### Smalltalk Developer
- Pharo's `OSSubprocess` is the closest analog but uses mutable configure-then-run. Beamtalk's method-argument approach is simpler.
- `CommandResult` as a Value class aligns with ADR 0042 (immutable values for data).
- `Subprocess` as an Actor aligns with ADR 0043 (sync-by-default messaging) — messages to the subprocess actor use the same `.` syntax as any other actor message.
- Stream integration is Beamtalk-idiomatic, not a Smalltalk pattern (Smalltalk uses positional ReadStream).

### Erlang/BEAM Developer
- Wraps `open_port` + `os:find_executable` — familiar mechanics, safer API.
- The Subprocess actor is a gen_server that owns a port — a common OTP pattern. `readLine` as `gen_server:call` with deferred reply is textbook OTP.
- Port cross-process constraint is the same as file-backed streams (documented in ADR 0021) — but Tier 2 hides it behind the actor.
- Can drop to raw Erlang interop for edge cases (`Erlang erlang open_port: ...`).

### Production Operator
- `System run:` is stateless — no long-lived resources, no lifecycle concerns.
- `Subprocess` actors appear in the BEAM process registry and can be observed with standard tools (observer, recon). The actor's state map shows buffer size, exit code, and port status.
- Actor supervision ensures subprocess cleanup on failures — no orphan processes.
- Standard BEAM process linking ensures subprocess cleanup on caller crash.

## Steelman Analysis

### Option A: Start with `os:cmd` wrapper, upgrade to `open_port` later

- **Newcomer**: "`os:cmd` works today, right now, zero risk. Ship something that works in a day, iterate."
- **BEAM veteran**: "`os:cmd` is a known quantity — no port lifecycle complexity, no zombie concerns, no finalizer design. Add streaming later when there's a real use case that demands it."
- **Language designer**: "You are about to add permanent surface area to `System`. The argument for `os:cmd` first is not about speed or security — it's about **API validation before lock-in**. Pharo's `OSSubprocess` was initially the wrong API and had to be replaced. Prove that `run:args:` is the right granularity with a cheap shim. If it's right, swap in `open_port` later — pure implementation change. If it's wrong, you've wasted far less time than committing to the full port lifecycle design."
- **Incrementalist**: "You're already phasing Tier 1 and Tier 2 across three phases. If you're comfortable with that level of incrementalism, why not also phase the underlying implementation? Phase 0 with `os:cmd` proves the API ergonomics; Phase 1 swaps in `open_port` without changing the API."

**Why rejected:** `os:cmd/1` invokes `/bin/sh -c` — shell injection by design. It also discards the exit code, which is critical for scripting (checking `$?`). Starting with `os:cmd` would mean either (a) shipping an insecure API that we immediately deprecate, or (b) not exposing exit codes in Phase 0 and adding them as a breaking change in Phase 1.

The API-validation argument is genuine: committing to `run:args:` is a surface area decision. However, the `open_port` implementation is only marginally more complex than `os:cmd` — the real complexity is in the *streaming* and *actor* phases, not in `run:args:`. Phase 1 with `open_port` already serves as the API validation step. If `run:args:` turns out to be the wrong shape, we learn that in Phase 1 regardless of whether the implementation uses `os:cmd` or `open_port` underneath. The `os:cmd` indirection would save almost no implementation effort while adding a security liability.

### Option B: Use Streams from the Subprocess actor instead of readLine

- **API designer**: "Streams are the established pattern for sequential data in Beamtalk (ADR 0021). `agent lines.` returning a Stream would be consistent with `File lines:` and `System output:`. The readLine polling loop creates a **conceptual discontinuity** — users who graduate from `System output:` (Stream) to `Subprocess` (readLine) must abandon everything they learned about Streams and adopt a different iteration model."
- **Newcomer**: "I already know how Streams work from File and System output. Having to switch to a `whileTrue:` loop with explicit nil-checking is a step backward in ergonomics."
- **Language designer**: "ADR 0021 explicitly promises that `Process output: 'ls -la'` will return a Stream. Tier 1 delivers on this promise with `System output:`. But Tier 2 breaks the promise for interactive processes — the same conceptual operation (reading output from a running program) uses two completely different APIs depending on whether the program is short-lived or long-lived. This is a model-level inconsistency, not just a syntactic inconvenience."

**Why rejected:** The cross-process Stream constraint (ADR 0021) makes this impossible without fundamental changes to Stream semantics. A Stream's generator function runs in the caller's process, but the port is owned by the actor's gen_server process. Returning a Stream would require either: (a) violating the cross-process constraint (unsafe), (b) spawning a proxy process per Stream consumer (complex, fragile), or (c) redesigning Streams to support remote generators (large scope via BT-507).

The API coherence concern is real and should not be dismissed as "2 lines of code." The `readLine` model creates genuine conceptual discontinuity between Tier 1 and Tier 2. If BT-507 (Future combinators / cross-process Streams) is implemented, the Subprocess API should be revisited to add an `agent lines.` method that returns a cross-process-safe Stream. The `readLine` API would remain as the lower-level primitive, and `lines` would be sugar built on top of it. This future path is noted in Phase 5.

### Option C: Use `Process` as the class name (per ADR 0021 sketch)

- **Smalltalk purist**: "Pharo uses `OSProcess` / `OSSubprocess` — `Process` is the natural Smalltalk vocabulary."
- **Newcomer**: "Process is what every other language calls it — Python has `Process`, Ruby has `Process`, Go has `os.Process`."
- **Language designer**: "Beamtalk already hides BEAM processes behind the name 'Actor' — users never see the word 'process' for BEAM-level concurrency. So 'Process' is actually *available* as a class name. More importantly, naming signals intent: `Subprocess` imports Unix parent-child hierarchy semantics into the language vocabulary. `Process` or `ExternalProcess` frames this as a language-level abstraction for coordinating with external computations, which is more aligned with Beamtalk's Smalltalk heritage. Beamtalk shouldn't import OS implementation details into its vocabulary any more than it needs to."

**Why rejected:** While Beamtalk hides BEAM processes as "Actors" in user-facing code, the BEAM ecosystem documentation, tooling (observer, recon), and error messages all use "process." A Beamtalk class named `Process` would create confusion when users encounter BEAM process IDs, process monitors, and OTP process terminology in error messages and debugging output. `Subprocess` is unambiguous and widely understood (Python uses `subprocess`, Pharo uses `OSSubprocess`). The vocabulary concern is valid but the pragmatic disambiguation outweighs it — especially since Beamtalk targets BEAM developers as a key audience.

### Option D: Message-push model instead of readLine polling

- **Concurrency expert**: "Push-based is more efficient — the actor pushes `{stdout, Line}` messages to a subscriber process via `gen_server:cast`. No blocking, no wasted gen_server:call round-trips. The subscriber registers interest and gets notified."
- **Event-driven developer**: "This is how Node.js `child_process` works — events, not polling. For the Symphony use case (streaming JSON-RPC events from a long-running agent), push is the natural model."
- **Operator**: "With `readLine` as a blocking `gen_server:call`, the actor has at most one pending caller at a time. Every consumer of Subprocess output must be sequential — there is no way to have two BEAM processes independently reading lines from the same subprocess. A push model makes the subscription relationship explicit in state and visible to BEAM tooling like `sys:get_state/1`."
- **Language designer**: "The `readLine` + `onLine:` future path creates two competing APIs for consuming output. If both are active simultaneously, they race for lines. This is not a neutral future extension — it's a protocol design commitment that constrains what `onLine:` can look like."

**Why rejected for Phase 1:** Push-based messaging adds significant complexity: subscriber registration, back-pressure handling, message ordering guarantees, and what happens when the subscriber crashes. The sync `readLine` model is simpler, correct, and sufficient for the motivating JSON-RPC use case (request-response with line-delimited messages).

The single-consumer limitation is real but acceptable for Phase 1: the Symphony orchestrator pattern is one coordinator process per subprocess, not multiple concurrent readers. The operator's observability concern is valid — the implementation should include the subscriber identity in the `waiting` state field (not just `{From, _}` but `{From, WaitingSince}`) for debugging.

A push-based extension can be added later, but the ADR acknowledges this is not fully neutral — `readLine` and `onLine:` will need coordination to avoid consuming lines intended for the other. The likely design is mutual exclusion: a Subprocess uses either pull (`readLine`) or push (`onLine:`) mode, not both simultaneously. This constraint should be documented when `onLine:` is designed.

### Tension Points
- **Stream vs readLine:** Stream advocates want `agent lines.` for API consistency; the cross-process constraint makes this impossible today. If BT-507 enables cross-process Streams, the Subprocess API should be revisited.
- **Push vs pull:** Push-model advocates want event-driven efficiency and multi-consumer support; Phase 1 prioritizes simplicity. The single-consumer limitation is acceptable for the Symphony use case but should be documented.
- **Incrementalists vs security-first:** `os:cmd` first would validate API ergonomics cheaply; `open_port` first gets security right from day one at minimal extra cost.
- **Naming:** `Process` is available (BEAM processes are "Actors") but clashes with ecosystem tooling. `Subprocess` is unambiguous but imports Unix vocabulary.

## Alternatives Considered

### Alternative: Shell-string API (`System shell: "ls -la | grep foo"`)
Provide a convenience API that invokes the system shell, enabling pipes, redirects, and shell features.

**Rejected because:** Shell injection is a top security vulnerability in subprocess execution. The EEF Security Working Group explicitly recommends against `{spawn, Command}` in favor of `spawn_executable`. Users who need piping can compose Beamtalk streams. The composability of `System output:` with `select:` / `collect:` makes shell piping unnecessary for most cases. (Note: Beamtalk stream composition is sequential, not concurrent like shell pipes — each stage blocks while the previous produces output.)

### Alternative: Port class extension
Extend the existing `Port.bt` to add subprocess creation methods.

**Rejected because:** `Port` is a BEAM interop type representing port *identifiers* from Erlang code (ADR 0028). Adding subprocess creation conflates BEAM interop artifacts with OS process management.

### Alternative: Block-scoped only (no bare `System output:`)
Only provide `System output:args:do:` (block-scoped), not the bare `System output:args:` that returns a Stream directly.

**Rejected because:** The bare Stream form enables pipeline composition (`System output: ... select: ... take: 10`) which is the idiomatic Beamtalk pattern established by `File lines:` in ADR 0021. Removing it would make subprocess output less composable than file I/O. Both forms coexist: bare for pipelines, block-scoped for long-running commands where deterministic cleanup matters. This mirrors the `File lines:` / `File open:do:` dual pattern.

### Alternative: Subprocess as a Value class with mutable handle (Pharo-style)
Create `Subprocess` as a non-actor class wrapping a port handle, with methods like `readLine`, `writeLine:`, `close`. The caller's process owns the port directly.

**Rejected because:** This would work for single-threaded use but breaks Beamtalk's concurrency model. If two BEAM processes share a reference to the same `Subprocess` value, both would try to interact with the port — but only the owner can. The actor model avoids this by routing all port access through the actor's gen_server process, serializing access naturally. Additionally, actors provide supervision, lifecycle management, and standard BEAM observability tools — all free from the OTP framework.

## Consequences

### Positive
- Beamtalk gains both one-shot and interactive OS command execution — unblocks scripting, automation, tooling, and orchestration
- No shell injection by construction — `spawn_executable` with argument lists only
- **Separate stdout/stderr from day one** — the Rust helper provides separate pipes, unlike raw `open_port` which can only merge them
- **Process group kill** — the Rust helper spawns subprocesses in their own process group, so grandchild processes are cleaned up correctly. No PID reuse risk.
- **Cross-platform** — the Rust helper works on Linux, macOS, and Windows, unlike erlexec (Unix-only)
- Tier 1 Stream integration follows established `File lines:` pattern from ADR 0021
- Tier 2 actor model provides natural lifecycle management — subprocess cleanup is tied to actor lifecycle via OTP supervision
- `readLine` / `writeLine:` as sync actor messages aligns with ADR 0043 (sync-by-default)
- `CommandResult` as a Value class is consistent with ADR 0042
- Tier 2 hides the cross-process port constraint — callers interact with a simple actor, not a port
- Two consumption modes (pull via `Subprocess`, push via `SubprocessListener`) with shared base class — users pick the right tool for their use case
- Symphony orchestrator use case is fully addressable with Tier 2
- The Rust helper follows the same ETF-over-port pattern as `beamtalk_compiler_port` — proven architecture

### Negative
- `Subprocess readLine` loop is less ergonomic than a Stream pipeline for consuming output — `whileTrue:` with nil-check instead of `do:`. `SubprocessListener` with `onLine:` is more ergonomic for consumption but doesn't support request-response protocols. This creates conceptual discontinuity between Tier 1 (`System output:` returns a Stream) and Tier 2 (two different consumption modes). If BT-507 enables cross-process Streams, a `lines` method should be added to bridge this gap.
- **Single consumer:** Subprocess's `readLine` supports at most one blocked caller at a time. Multiple concurrent readers will see lines distributed non-deterministically between them (not duplicated). This is sufficient for the Symphony use case (one coordinator per subprocess) but limits fan-out patterns.
- **`kill.` bypasses cleanup:** Actor's inherited `kill.` sends SIGKILL to the BEAM process, bypassing `terminate/2`. The Rust helper will detect the port close and clean up the subprocess, but there is a brief window where the subprocess may still be running. Users should prefer `close.` for Subprocess actors.
- Bare `System output:` Streams from long-running commands can leak subprocess resources if abandoned (mitigated by block-scoped form and documentation)
- Windows: `.bat`/`.cmd` resolution via `os:find_executable` may reintroduce shell semantics (documented caveat, not fully preventable)
- `beamtalk_subprocess.erl` is a hand-written Erlang module rather than generated code — it won't benefit from codegen improvements to actor dispatch
- The Rust helper binary adds ~400-500 lines of Rust code to the build — a new binary artifact alongside the compiler binary

### Neutral
- `Port.bt` remains unchanged — still an opaque BEAM interop type
- `System.bt` gains three method families — moderate surface area expansion
- `SubprocessBase`, `Subprocess`, and `SubprocessListener` add three new Actor subclasses — moderate addition to the class hierarchy
- Port-backed Streams (Tier 1) have the same cross-process constraint as file-backed Streams (ADR 0021) — no new constraint introduced
- The stdlib dependency on OTP's `os:find_executable/1` is minimal and well-tested
- Deferred reply pattern in readLine is standard OTP — no new runtime mechanisms needed
- The Rust helper binary is built by the same `cargo build` that builds the compiler — no new build tooling needed

## Implementation

### Phase 1: Rust subprocess helper binary — `beamtalk_exec` (S-M)

Build a standalone Rust binary that the BEAM spawns as a port. The binary manages child process spawning, stdio piping, process group lifecycle, and signal handling. The BEAM communicates with it via ETF-encoded messages over the port protocol (same pattern as `beamtalk_compiler_port`).

**New Rust crate: `beamtalk-exec`** (~400-500 lines)
- `crates/beamtalk-exec/src/main.rs` — entry point, ETF port protocol loop
- Spawns child processes via `std::process::Command` with separate stdout/stderr/stdin pipes
- Creates each subprocess in its own process group (Unix: `setsid()` via `pre_exec`; Windows: Job Object via `windows-rs`)
- Handles shutdown commands: SIGTERM → wait (2s) → SIGKILL to process group (Unix); `TerminateJobObject` (Windows)
- Forwards tagged stdout/stderr data to the BEAM: `{stdout, ChildId, Data}`, `{stderr, ChildId, Data}`, `{exit, ChildId, Code}`
- On stdin EOF (BEAM port closed / VM exited): cleans up all managed subprocesses before exiting

**Erlang interface: `beamtalk_exec_port.erl`**
- `runtime/apps/beamtalk_runtime/src/beamtalk_exec_port.erl` — singleton port manager (like `beamtalk_compiler_port.erl`)
- Opens the `beamtalk_exec` binary as a port on first use, reuses for subsequent subprocess requests
- Translates Beamtalk subprocess commands to ETF messages and routes responses

**Build integration:**
- Add `beamtalk-exec` crate to the Cargo workspace
- `just build` compiles both the compiler binary and the exec helper
- Binary is distributed alongside the compiler in the same package

**Components:** new Rust crate, runtime
**Tests:** Rust unit tests for protocol encoding/decoding, integration tests via `stdlib/test/exec_helper_test.bt` — spawn echo process, verify separate stdout/stderr, process group kill

### Phase 2: `System run:args:` and `CommandResult` (M)

**New class: `CommandResult`**
- `stdlib/src/CommandResult.bt` — sealed Value subclass with `output`, `stderr`, `exitCode`, `isSuccess`
- `runtime/apps/beamtalk_runtime/src/beamtalk_command_result.erl` — value dispatch

**Extend `System`:**
- Add `run:args:` and `run:args:env:dir:` to `stdlib/src/System.bt`
- Add runtime dispatch in `runtime/apps/beamtalk_runtime/src/beamtalk_system.erl`
- Implement via `beamtalk_exec_port` — sends spawn command to Rust helper, collects tagged stdout/stderr until exit, returns `CommandResult`

**Components:** stdlib, runtime, codegen (builtins registration for `CommandResult`)
**Tests:** `stdlib/test/system_command_test.bt` — spawn/capture, exit codes, separate stdout/stderr, command not found, env/dir overrides

### Phase 3: `System output:args:` streaming (M)

**Extend `System`:**
- Add `output:args:` and `output:args:do:` to `stdlib/src/System.bt`
- Implement Stream generator backed by `beamtalk_exec_port` — the Erlang module receives tagged stdout data from the Rust helper and feeds the Stream
- `output:args:do:` wraps block in `ensure:` for deterministic cleanup — sends shutdown command to helper on block exit

**Components:** stdlib, runtime
**Tests:** `stdlib/test/system_output_test.bt` — streaming, pipeline composition, block-scoped cleanup, abandoned stream behavior

### Phase 4: `SubprocessBase` and `Subprocess` actor (L)

**New abstract base class: `SubprocessBase`**
- `stdlib/src/SubprocessBase.bt` — Actor subclass with `open:args:`, `open:args:env:dir:`, `writeLine:`, `exitCode`, `close`
- `runtime/apps/beamtalk_runtime/src/beamtalk_subprocess_base.erl` — shared gen_server logic:
  - `init/1` — sends spawn command to `beamtalk_exec_port`, initializes port state
  - `handle_call/3` — dispatches `writeLine:`, `exitCode`, `close`
  - `terminate/2` — sends shutdown command to `beamtalk_exec_port` for this child

**New class: `Subprocess` (pull mode)**
- `stdlib/src/Subprocess.bt` — SubprocessBase subclass adding `readLine`, `readLine:`, `tryReadLine`, `readStderrLine`, `readStderrLine:`, `tryReadStderrLine`
- `runtime/apps/beamtalk_runtime/src/beamtalk_subprocess.erl` — extends base with:
  - Stdout/stderr buffer queues
  - `handle_info/2` — receives `{stdout, ChildId, Data}`, `{stderr, ChildId, Data}`, and `{exit, ChildId, Code}` from the Rust helper, buffers lines, replies to deferred `readLine`/`readStderrLine` callers
  - `handle_call/3` — adds `readLine`, `readLine:`, `tryReadLine` and stderr equivalents
  - Timeout management via `erlang:send_after`

**Codegen registration:**
- Register `SubprocessBase`, `Subprocess` as built-in actor classes in codegen (similar to how `Actor` is registered)
- The class method `open:args:` is a standard class method that calls `spawnWith:` — no codegen changes needed

**Components:** stdlib, runtime, codegen (builtins registration)
**Tests:** `stdlib/test/subprocess_test.bt` — spawn/readLine/writeLine, separate stderr, exit codes, close/stop lifecycle, deferred readLine blocking, timeout variants, tryReadLine, EOF handling, error on write after close

### Phase 5: SubprocessListener — Push-Based Callbacks
- Implement `beamtalk_subprocess_listener.erl` extending `beamtalk_subprocess_base.erl`
- `onLine:`, `onStderrLine:`, `onExit:` — callback registration via `gen_server:call`
- `handle_info` dispatches decoded lines directly to registered callbacks (no buffering)
- `onExit:` fires after final line callbacks, with exit code as argument
- Create `stdlib/src/SubprocessListener.bt` class definition
- Register in codegen builtins

**Components:** stdlib, runtime
**Tests:** `stdlib/test/subprocess_listener_test.bt` — callback registration, stdout/stderr dispatch, onExit sequencing, no-callback-drops-lines, writeLine from callback

## References
- Related issues: [BT-1119](https://linear.app/beamtalk/issue/BT-1119) (Subprocess execution), [BT-1125](https://linear.app/beamtalk/issue/BT-1125) (Interactive Subprocess actor), [BT-1118](https://linear.app/beamtalk/issue/BT-1118) (Epic: Stdlib Process, Timer, and Directory Libraries), [BT-1123](https://linear.app/beamtalk/issue/BT-1123) (Epic: Symphony-Style Orchestrator)
- Related ADRs: [ADR 0021](0021-streams-and-io-design.md) (Stream — `Process output:` deferred as future integration), [ADR 0022](0022-embedded-compiler-via-otp-port.md) (OTP Port pattern for compiler), [ADR 0028](0028-beam-interop-strategy.md) (Port as BEAM interop type), [ADR 0041](0041-universal-state-threading-block-protocol.md) (Block protocol — affects `whileTrue:` loop in readLine pattern), [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) (Value objects vs actors), [ADR 0043](0043-sync-by-default-actor-messaging.md) (Sync-by-default messaging — timeout implications for readLine)
- Erlang `open_port/2`: https://www.erlang.org/doc/apps/erts/erlang#open_port/2
- EEF Security WG on external executables: https://security.erlef.org/secure_coding_and_deployment_hardening/external_executables.html
- erlexec: https://github.com/saleyn/erlexec
- Elixir `System.cmd/3`: https://hexdocs.pm/elixir/System.html#cmd/3
- Pharo OSSubprocess: https://github.com/pharo-contributions/OSSubprocess
- Node.js child_process: https://nodejs.org/api/child_process.html
- Symphony spec (motivating use case): https://github.com/openai/symphony/blob/main/SPEC.md
