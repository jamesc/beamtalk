# ADR 0051: Subprocess Execution — Two-Tier OS Process API

## Status
Proposed (2026-03-05)

## Context

### Problem Statement

Beamtalk has no way to execute external OS commands. The existing `Port` class (`stdlib/src/Port.bt`) is an opaque wrapper for BEAM port identifiers — it provides `asString`, `=:=`, and `hash`, but cannot create, communicate with, or manage subprocesses.

This blocks two categories of use cases:

1. **Simple commands** — run `git status`, capture output, check the exit code. Every developer expects this from a general-purpose language.
2. **Interactive long-lived subprocesses** — launch a daemon process, write to its stdin, stream its stdout, and manage its lifecycle. This is required for building orchestration services like Symphony (BT-1118) where a coding agent subprocess runs for minutes to hours with bidirectional JSON-RPC communication.

### Current State

- `Port.bt` provides only display/comparison methods for port identifiers received from Erlang interop
- No `beamtalk_port.erl` runtime module exists — port primitives are handled inline in `beamtalk_primitive.erl`
- The workaround is raw Erlang FFI: `Erlang os cmd: "ls -la"` — which has no exit code, no streaming, shell injection risk, and blocks the caller
- ADR 0021 explicitly deferred `Process output: 'ls -la'` as a future Stream integration point
- ADR 0022 established the OTP Port pattern for the compiler: `open_port({spawn_executable, ...})` with `{packet, 4}`, ETF framing, and OTP supervision — this is the internal template for subprocess management
- `System.bt` already handles OS-level concerns (`getEnv:`, `osPlatform`, `pid`) — a natural home for simple command execution

### Constraints

1. **BEAM port limitations** — `open_port/2` cannot deliver stdout and stderr as separate streams. `stderr_to_stdout` merges them; otherwise stderr goes to the VM's terminal unseen. Separate stderr requires a helper process (erlexec pattern).
2. **Port ownership is process-local** — the process that calls `open_port/2` becomes the port owner. Only the owner can send commands. Port-backed streams have the same cross-process constraint as file-backed streams (ADR 0021).
3. **Zombie process risk** — closing a port sends EOF to the subprocess's stdin. Programs that don't monitor stdin for EOF become orphans. The BEAM has no built-in mechanism to forcefully terminate port children.
4. **Security** — shell invocation (`{spawn, Command}`) enables injection. `{spawn_executable, Path}` with `{args, List}` is safe but requires absolute paths resolved via `os:find_executable/1`.

## Decision

Introduce a **two-tier API** for subprocess execution:

- **Tier 1: `System` class methods** for one-shot commands — blocking capture and streaming output. Simple, stateless, value-oriented.
- **Tier 2: `Subprocess` actor class** for interactive, long-lived subprocesses — bidirectional communication, lifecycle management, supervised by OTP.

Both tiers use `spawn_executable` with explicit argument lists — no shell invocation, no injection risk. This is the same approach recommended by the EEF Security Working Group.

### Tier 1: Simple Commands on `System`

Three new class methods on the existing `System` class:

```beamtalk
// Blocking capture — run to completion, return result value
result := System run: "git" args: #("status")
result output      // => "On branch main\nnothing to commit\n"
result exitCode    // => 0

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
```

**Return types:**
- `System run:args:` returns a `CommandResult` value object with `output` (String), `exitCode` (Integer), and `isSuccess` (Boolean)
- `System output:args:` returns a `Stream` of lines (lazy, constant memory) with a finalizer that closes the port and kills the subprocess

**Error handling:**
```beamtalk
// Command not found
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
```

### Tier 2: Interactive Subprocesses via `Subprocess` Actor

For long-lived, bidirectional subprocess management:

```beamtalk
// Launch an interactive subprocess
agent := Subprocess open: "codex" args: #("app-server")

// Write to stdin
agent send: (JSON generate: #{
  method: "initialize",
  params: #{prompt: "Fix the login bug", workspace: "/tmp/ws"}
})

// Read stdout lines as a Stream
agent lines do: [:line |
  event := JSON parse: line.
  Transcript show: (event at: "type")
]

// Or read one line at a time
response := agent readLine    // => "{\"type\":\"model_response\",...}"

// Check lifecycle
agent isAlive                 // => true

// Send signal / terminate
agent close                   // close stdin (graceful — subprocess sees EOF)
agent kill                    // forceful termination via OS PID

// Wait for natural exit
exitCode := agent waitForExit // => 0
```

**`Subprocess` is an Actor** (gen_server) because:
- The OTP port is process-local — the actor owns it
- Multiple callers may need to interact (send commands, read events, kill)
- OTP supervision handles crash recovery
- This follows the established pattern from `beamtalk_compiler_port.erl` (ADR 0022)

**Actor lifecycle:**
```
                 open:args:
                     │
                     ▼
              ┌──────────┐
              │  running  │──── kill / subprocess crash
              │           │──── close (sends EOF)
              └─────┬─────┘
                    │ subprocess exits
                    ▼
              ┌──────────┐
              │  exited   │  exitCode available
              └──────────┘
```

**Error handling:**
```beamtalk
// Writing to an exited subprocess
agent send: "data"
// => Error: #port_closed "Subprocess has exited (exit code: 1)"

// Timeout waiting for exit
agent waitForExit: 5000
// => Error: #timeout "Subprocess did not exit within 5000ms"
```

### Security Model

Both tiers enforce the same security rules:

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

### Stderr Handling (Phase 1)

Phase 1 merges stderr into stdout via `stderr_to_stdout`. The API does not expose this as a feature — `output` is simply "the subprocess output." This is forward-compatible with separate stderr in a future phase:

```beamtalk
// Phase 1: output contains both stdout and stderr (merged)
result := System run: "gcc" args: #("-Wall", "main.c")
result output      // => "main.c:5: warning: unused variable\n"

// Future phase: separate stderr (non-breaking addition)
result stderr      // => "main.c:5: warning: unused variable\n"
result stdout      // => "" (separate from stderr)
```

For `Subprocess` actors, Phase 1 provides `lines` (merged). A future phase could add `stdoutLines` and `stderrLines` as separate streams, or event-based callbacks with tagged messages.

### Zombie Process Prevention

Both tiers implement active cleanup:

1. **Port finalizer** — Stream-based `System output:` attaches a finalizer (per ADR 0021 pattern) that calls `port_close/1` and then terminates the OS process via its PID.
2. **Actor termination** — `Subprocess` actor's `terminate/2` callback retrieves the OS PID via `erlang:port_info(Port, os_pid)` and sends a kill signal if the process hasn't exited.
3. **Process linking** — the owning process (or actor) is linked to the port. If the owner crashes, the port closes, sending EOF to the subprocess. The subprocess is then terminated after a brief grace period.

```erlang
%% In beamtalk_subprocess.erl terminate/2:
terminate(_Reason, #{port := Port} = _State) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            catch port_close(Port),
            %% Grace period for EOF handling, then force terminate
            timer:sleep(100),
            beamtalk_subprocess_util:kill_os_process(OsPid);
        undefined ->
            ok  %% already exited
    end.
```

## Prior Art

### Erlang/OTP: `open_port/2` and `os:cmd/1`
- `os:cmd/1` — blocking, shell-invoked, no exit code, no streaming. Adequate for trusted one-liners only.
- `open_port({spawn_executable, Path}, Opts)` — full control, no shell, exit status via `{Port, {exit_status, N}}`, but single-channel (no separate stderr), port-owner-only communication, zombie risk.
- **Adopted:** `spawn_executable` as the foundation for both tiers. `exit_status` option for exit code capture. `stderr_to_stdout` for merged output.
- **Not adopted:** `{spawn, Command}` (shell injection risk), `os:cmd` (no exit code, no streaming).

### Elixir: `System.cmd/3` and `Port.open/2`
- `System.cmd("git", ["status"])` — returns `{output, exit_code}`. Uses `spawn_executable` internally. No stdin, no streaming, blocks until exit.
- `Port.open/2` wraps `open_port` with Elixir syntax.
- GenServer pattern for long-lived ports is idiomatic (handle_info for port messages).
- **Adopted:** `System.cmd` as the model for Tier 1 (`System run:args:`). GenServer pattern as the model for Tier 2 (`Subprocess` actor).
- **Improved:** Added streaming via `System output:args:` (Elixir has no built-in lazy stream over port output).

### Pharo Smalltalk: `OSSubprocess`
- Mutable object model: create, configure (`redirectStdout`, `redirectStderr`), then `run`.
- Separate stdout/stderr — trivially achievable because the Pharo VM does `fork/exec` with direct fd control.
- `waitForExitPollingEvery:retrievingStreams:` — polls and drains output buffers to prevent pipe deadlock.
- Manual cleanup required (`closeAndCleanStreams`).
- **Adopted:** Concept of subprocess as an object with lifecycle methods (`isAlive`, `kill`, `waitForExit`).
- **Rejected:** Mutable configure-then-run pattern — doesn't fit Beamtalk's value-object philosophy (ADR 0042). Configuration is passed at creation time instead.
- **Not achievable (Phase 1):** Separate stderr — BEAM's port mechanism cannot deliver it without a helper process.

### Newspeak
- No subprocess concept — deliberately, as an object-capability language. OS authority must be explicitly injected. Beamtalk is more pragmatic about OS access.

### erlexec (Erlang library)
- Uses a C++ helper process (`exec-port`) as intermediary between BEAM and subprocesses.
- Provides separate stdout/stderr, signal management, process group control, PTY support.
- **Deferred:** erlexec's architecture is the path to separate stderr in a future phase. Phase 1 avoids the C++ dependency by using raw `open_port` with merged streams.

### Porcelain (Elixir library)
- Two-driver model: Basic (raw port, merged stderr) and Goon (Go helper binary, separate stderr).
- Confirms that separate stderr on BEAM requires a helper process — not achievable with raw ports alone.
- **Validated our approach:** Phase 1 = Basic driver pattern. Future phase = helper process pattern (Rust instead of Go, since Rust is already in our build chain).

## User Impact

### Newcomer
- `System run: "git" args: #("status")` is immediately intuitive — similar to Python's `subprocess.run()` or Node's `execFileSync()`.
- `System output:` returning a Stream follows the same pattern as `File lines:` — consistent mental model.
- `Subprocess open:args:` for interactive processes is a step up in complexity, but the Actor model provides clear lifecycle semantics.
- Error messages guide toward correct usage (command not found → check PATH, type error → use strings).

### Smalltalk Developer
- Pharo's `OSSubprocess` is the closest analog but uses mutable configure-then-run. Beamtalk's immutable-config-at-creation is a departure but aligns with ADR 0042 (value objects).
- `Subprocess` as an Actor matches Beamtalk's actor model — mutable state belongs in actors, not value objects.
- The `send:` / `lines` / `kill` protocol on `Subprocess` is Smalltalk-idiomatic message passing.

### Erlang/BEAM Developer
- Tier 1 wraps `open_port` + `os:find_executable` — familiar mechanics, safer API.
- Tier 2 is a gen_server owning a port — the standard OTP pattern (exactly like `beamtalk_compiler_port.erl`).
- Port cross-process constraint is documented (same as file streams in ADR 0021).
- Can drop to raw Erlang interop for edge cases (`Erlang erlang open_port: ...`).

### Production Operator
- Subprocess crashes are isolated to the owning actor — no cascade.
- Zombie prevention via active cleanup in terminate/2.
- `Subprocess` actors can be supervised by user-defined supervisors (when user-facing supervision lands, BT-448).
- Observable with standard BEAM tools — `observer` shows the actor process and its port.

## Steelman Analysis

### Option A: Single-tier `System` only (no `Subprocess` actor)

- **Newcomer**: "Simpler API surface — one class to learn, not two. `System run:` covers 90% of use cases. Interactive subprocesses are an advanced pattern I don't need yet."
- **Smalltalk purist**: "Adding another Actor subclass muddies the class hierarchy. Subprocess management can be built by the user from raw Erlang port interop when needed — the stdlib shouldn't try to cover every OS interaction."
- **BEAM veteran**: "I can build a gen_server port wrapper myself in 50 lines of Erlang if I need it. The stdlib should provide the simple primitives (`System run:`, `System output:`), not opinionated Actor patterns. Let the community decide the right abstraction for interactive processes."
- **Operator**: "Fewer stdlib classes = smaller attack surface. `System run:` with timeout is sufficient for health checks and automation scripts."

**Why rejected:** Symphony-style orchestrators need bidirectional stdin/stdout communication, which `System run:` cannot provide (it runs to completion). Building gen_server port wrappers from Erlang interop is possible but defeats the purpose of a batteries-included stdlib. The Tier 2 actor pattern is already proven in the codebase (`beamtalk_compiler_port.erl`) — promoting it to stdlib is low-risk.

### Option B: erlexec dependency for full subprocess control

- **Newcomer**: "Separate stderr out of the box — I don't have to wonder why my error messages are mixed into stdout."
- **BEAM veteran**: "erlexec is battle-tested, handles process groups, signals, PTY. Why reinvent the wheel? The C++ dependency is well-maintained and the build is straightforward."
- **Operator**: "erlexec's process group management prevents zombie processes properly — not with shell commands but with kernel-level process group signals. More reliable in production."
- **Language designer**: "Starting with full capability means the API never has to change. Forward-compatible is good; already-complete is better."

**Why deferred, not rejected:** erlexec adds a C++ NIF dependency, complicating cross-platform builds (especially Windows). Phase 1's merged-stderr approach covers the common case. The API is designed so erlexec (or a Rust helper) can be swapped in later without breaking user code — `output` stays the same, `stderr` is added as a new method.

### Option C: Use `Process` as the class name (per ADR 0021 sketch)

- **Smalltalk purist**: "Pharo uses `OSProcess` — `Process` is the natural name. BEAM processes are actors/lightweight threads, which Beamtalk already calls 'actors.' There's no naming conflict."
- **Newcomer**: "Process is what every other language calls it — Python's `subprocess`, Node's `child_process`, Ruby's `Process`."

**Why rejected:** In the BEAM ecosystem, "process" universally means a lightweight BEAM process (gen_server, actor). Beamtalk's own codebase uses "process" for BEAM processes throughout. Using `Process` for OS subprocesses creates confusion, especially in error messages and documentation. `Subprocess` is unambiguous. Tier 1 lives on `System`, which has no naming conflict.

### Tension Points
- Newcomers and Smalltalk purists prefer `Process` naming; BEAM veterans strongly prefer `Subprocess` to avoid BEAM process confusion.
- BEAM veterans would skip Tier 2 entirely (build it yourself from ports); newcomers and operators want it in stdlib.
- Everyone agrees that shell injection prevention via `spawn_executable` is non-negotiable.

## Alternatives Considered

### Alternative: Shell-string API (`System shell: "ls -la | grep foo"`)
Provide a convenience API that invokes the system shell, enabling pipes, redirects, and shell features.

```beamtalk
System shell: "find . -name '*.bt' | xargs grep TODO | wc -l"
```

**Rejected because:** Shell injection is a top security vulnerability in subprocess execution. Even with "trusted" input, shell metacharacters create subtle bugs. The EEF Security Working Group explicitly recommends against `{spawn, Command}` in favor of `spawn_executable`. Users who need piping can compose Beamtalk streams:

```beamtalk
// Safe equivalent of: find . -name '*.bt' | xargs grep TODO | wc -l
count := (System output: "find" args: #(".", "-name", "*.bt"))
  collect: [:file | System run: "grep" args: #("TODO", file)]
  select: [:result | result isSuccess]
  inject: 0 into: [:n :_ | n + 1]
```

### Alternative: `System` class for everything (no `Subprocess` actor)
Put both tiers on `System` with `System open:args:` returning a handle object.

**Rejected because:** A subprocess handle with `send:`, `lines`, `kill`, and mutable lifecycle state is an Actor by any other name. Making it a proper Actor subclass gives it OTP supervision, crash isolation, and cross-process messaging for free. Hiding actor semantics behind a "handle" value type creates confusion about ownership and thread safety.

### Alternative: Port class extension
Extend the existing `Port.bt` to add subprocess creation methods.

**Rejected because:** `Port` is a BEAM interop type — it represents port *identifiers* from Erlang code. Adding subprocess creation to it conflates two concerns: BEAM interop artifacts and OS process management. The existing Port class should remain a thin wrapper for port values, consistent with ADR 0028 (BEAM Interop Strategy).

## Consequences

### Positive
- Beamtalk gains OS command execution — unblocks automation, tooling, and orchestration use cases
- Two-tier design serves both simple scripts (Tier 1) and complex orchestrators (Tier 2)
- No shell injection by construction — `spawn_executable` with argument lists only
- Stream integration (Tier 1) follows established `File lines:` pattern from ADR 0021
- Actor pattern (Tier 2) follows established `beamtalk_compiler_port.erl` from ADR 0022
- API is forward-compatible with separate stderr (adding methods, not changing existing ones)
- Active zombie prevention via port finalizers and actor terminate/2

### Negative
- No separate stderr in Phase 1 — merged output may confuse users who expect to distinguish error output
- `Subprocess` actor adds cross-process constraints (port ownership) that must be documented
- Zombie prevention relies on OS-level process termination — not elegant, but effective and standard in the BEAM ecosystem
- Windows subprocess management has platform-specific edge cases (no SIGKILL, different process group semantics)
- Adding a subprocess capability increases the security surface — subprocess execution is inherently powerful

### Neutral
- `Port.bt` remains unchanged — still an opaque BEAM interop type
- `System.bt` gains three methods — small surface area expansion
- `Subprocess` is a new stdlib class but follows existing Actor patterns
- The stdlib dependency on OTP's `os:find_executable/1` is minimal and well-tested

## Implementation

### Phase 1: Tier 1 — `System run:args:` and `System output:args:` (M)

**New class: `CommandResult`**
- `stdlib/src/CommandResult.bt` — sealed Value subclass with `output`, `exitCode`, `isSuccess`
- `runtime/apps/beamtalk_runtime/src/beamtalk_command_result.erl` — value dispatch

**Extend `System`:**
- Add `run:args:`, `run:args:env:dir:`, `output:args:` to `stdlib/src/System.bt`
- Add runtime dispatch in `runtime/apps/beamtalk_runtime/src/beamtalk_system.erl`
- Implement via `open_port({spawn_executable, os:find_executable(Cmd)}, [binary, exit_status, stderr_to_stdout, {args, Args}])`
- `System output:` returns a Stream with port-closing finalizer (ADR 0021 pattern)

**Components:** stdlib, runtime
**Tests:** `stdlib/test/system_command_test.bt`

### Phase 2: Tier 2 — `Subprocess` actor (L)

**New class: `Subprocess`**
- `stdlib/src/Subprocess.bt` — Actor subclass with `open:args:`, `send:`, `lines`, `readLine`, `close`, `kill`, `isAlive`, `waitForExit`
- `runtime/apps/beamtalk_runtime/src/beamtalk_subprocess.erl` — gen_server owning an OTP port
- Port opened with `[binary, exit_status, stderr_to_stdout, use_stdio, {args, Args}]`
- `send:` → `port_command(Port, Data)`
- `lines` → Stream wrapping port message receive loop
- `terminate/2` → port close + OS PID termination

**Components:** stdlib, runtime, codegen (builtins registration)
**Tests:** `stdlib/test/subprocess_test.bt`

### Phase 3 (Future): Separate stderr
- Add erlexec dependency or build a Rust helper binary (following Porcelain/goon pattern)
- Add `stderr` to `CommandResult`, `stderrLines` to `Subprocess`
- Non-breaking: new methods on existing classes

### Phase 4 (Future): Enhanced `Subprocess` features
- Custom environment variables and working directory
- Timeout on `waitForExit:`
- `onExit:` callback for async exit notification
- User-facing supervision integration (depends on BT-448)

## References
- Related issues: [BT-1119](https://linear.app/beamtalk/issue/BT-1119) (Subprocess execution — Process stdlib class), [BT-1118](https://linear.app/beamtalk/issue/BT-1118) (Epic: Stdlib Process, Timer, and Directory Libraries)
- Related ADRs: [ADR 0021](0021-streams-and-io-design.md) (Stream — `Process output:` deferred as future integration), [ADR 0022](0022-embedded-compiler-via-otp-port.md) (OTP Port pattern for compiler), [ADR 0028](0028-beam-interop-strategy.md) (Port as BEAM interop type), [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) (Value objects vs actors), [ADR 0043](0043-sync-by-default-actor-messaging.md) (Sync-by-default messaging)
- Erlang `open_port/2`: https://www.erlang.org/doc/apps/erts/erlang#open_port/2
- EEF Security WG on external executables: https://security.erlef.org/secure_coding_and_deployment_hardening/external_executables.html
- erlexec: https://github.com/saleyn/erlexec
- Elixir `System.cmd/3`: https://hexdocs.pm/elixir/System.html#cmd/3
- Pharo OSSubprocess: https://github.com/pharo-contributions/OSSubprocess
- Symphony spec (motivating use case): https://github.com/openai/symphony/blob/main/SPEC.md
