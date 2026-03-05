# ADR 0051: Subprocess Execution — System Command API

## Status
Proposed (2026-03-05)

## Context

### Problem Statement

Beamtalk has no way to execute external OS commands. The existing `Port` class (`stdlib/src/Port.bt`) is an opaque wrapper for BEAM port identifiers — it provides `asString`, `=:=`, and `hash`, but cannot create, communicate with, or manage subprocesses.

This blocks two categories of use cases:

1. **Simple commands** — run `git status`, capture output, check the exit code. Every developer expects this from a general-purpose language.
2. **Interactive long-lived subprocesses** — launch a daemon process, write to its stdin, stream its stdout, and manage its lifecycle. This is required for building orchestration services like Symphony (BT-1118) where a coding agent subprocess runs for minutes to hours with bidirectional JSON-RPC communication.

This ADR addresses use case 1 fully. Use case 2 (interactive subprocesses via an Actor) is deferred to a separate ADR pending resolution of the stream-from-actor cross-process constraint (see "Future Direction: Interactive Subprocess Actor" below and BT-507).

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

Add **three new class methods to `System`** for one-shot command execution: blocking capture, streaming output, and block-scoped streaming with deterministic cleanup. Introduce a `CommandResult` value class for structured results.

All methods use `spawn_executable` with explicit argument lists — no shell invocation, no injection risk. This is the same approach recommended by the EEF Security Working Group.

### API

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
- `System run:args:` returns a `CommandResult` value object with `output` (String), `exitCode` (Integer), and `isSuccess` (Boolean)
- `System output:args:` returns a `Stream` of lines (lazy, constant memory) with a finalizer that closes the port and terminates the subprocess
- `System output:args:do:` evaluates the block with a `Stream`, then deterministically closes the port and terminates the subprocess when the block returns — even on exception. This is the preferred form for long-running commands where cleanup timing matters.

### `CommandResult` Value Class

```beamtalk
sealed Value subclass: CommandResult

  /// The subprocess output (stdout + stderr merged) as a String.
  output -> String => @primitive "output"

  /// The process exit code (0 = success by convention).
  exitCode -> Integer => @primitive "exitCode"

  /// True if exitCode is 0.
  isSuccess -> Boolean => self exitCode =:= 0
```

### Error Handling

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

### Security Model

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

### Zombie Process Prevention

Both `System output:` forms implement active cleanup:

1. **Port finalizer** — `System output:args:` attaches a Stream finalizer (per ADR 0021 pattern) that calls `port_close/1` and then terminates the OS process via its PID. The finalizer runs when the stream is fully consumed or when a terminal operation completes.
2. **Block-scoped cleanup** — `System output:args:do:` closes the port and terminates the OS process in an `ensure:` block when the block exits (normally or via exception). This is the deterministic alternative — use it for long-running commands where relying on the finalizer is insufficient.
3. **Process linking** — the port is linked to the calling process. If the caller crashes, the port closes, sending EOF to the subprocess.
4. **Explicit kill** — `port_close/1` alone sends EOF but may not terminate the subprocess. After closing the port, the runtime retrieves the OS PID via `erlang:port_info(Port, os_pid)` and sends a platform-appropriate kill signal.

```erlang
%% In beamtalk_system.erl — cleanup helper:
cleanup_port(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            catch port_close(Port),
            %% Brief grace for EOF-aware programs, then force terminate
            timer:sleep(50),
            kill_os_process(OsPid);
        undefined ->
            catch port_close(Port)
    end.

%% Platform-appropriate process termination
-ifdef(WIN32).
kill_os_process(OsPid) ->
    os:cmd("taskkill /F /PID " ++ integer_to_list(OsPid)).
-else.
kill_os_process(OsPid) ->
    os:cmd("kill -9 " ++ integer_to_list(OsPid) ++ " 2>/dev/null").
-endif.
```

**Cross-process constraint:** Port-backed Streams from `System output:` have the same cross-process limitation as file-backed Streams (ADR 0021): they must be consumed by the same process that created them. Do not pass a `System output:` Stream to an actor — materialize it first with `asList` or `take:`.

### Abandoned Stream Cleanup

An abandoned `System output:` Stream (assigned to a variable but never fully consumed) will not be cleaned up until the owning process exits. For long-running REPL sessions, this means a `tail -f` subprocess could leak indefinitely. Mitigation:
- Use `System output:args:do:` (block-scoped, deterministic cleanup) for commands that may run indefinitely
- The Stream finalizer runs when a terminal operation completes, preventing leaks in normal pipeline usage
- Document prominently: "Prefer `output:args:do:` for long-running or potentially infinite commands"

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
- **Improved:** Added streaming via `System output:args:` and block-scoped `System output:args:do:` (Elixir has no built-in lazy stream over port output).

### Pharo Smalltalk: `OSSubprocess`
- Mutable object model: create, configure (`redirectStdout`, `redirectStderr`), then `run`.
- Separate stdout/stderr — trivially achievable because the Pharo VM does `fork/exec` with direct fd control.
- `waitForExitPollingEvery:retrievingStreams:` — polls and drains output buffers to prevent pipe deadlock.
- Manual cleanup required (`closeAndCleanStreams`).
- **Adopted:** Concept of result object with exit status and output.
- **Rejected:** Mutable configure-then-run pattern — doesn't fit Beamtalk's value-object philosophy (ADR 0042). Configuration is passed as method arguments instead.
- **Not achievable (Phase 1):** Separate stderr — BEAM's port mechanism cannot deliver it without a helper process.

### Newspeak
- No subprocess concept — deliberately, as an object-capability language. OS authority must be explicitly injected. Beamtalk is more pragmatic about OS access.

### erlexec (Erlang library) and Porcelain (Elixir library)
- erlexec uses a C++ helper process for separate stdout/stderr, signal management, and process group control.
- Porcelain uses a Go helper binary (goon) for the same purpose.
- Both confirm that separate stderr on BEAM requires a helper process.
- **Deferred:** erlexec/helper-process architecture is the path to separate stderr in a future phase. Phase 1 avoids the dependency by using raw `open_port` with merged streams. The API is designed so stderr can be added as new methods without breaking existing code.

## User Impact

### Newcomer
- `System run: "git" args: #("status")` is immediately intuitive — similar to Python's `subprocess.run()` or Node's `execFileSync()`.
- `System output:` returning a Stream follows the same pattern as `File lines:` — consistent mental model.
- `System output:args:do:` mirrors `File open:do:` — consistent cleanup pattern.
- Error messages guide toward correct usage (command not found → check PATH, type error → use strings).

### Smalltalk Developer
- Pharo's `OSSubprocess` is the closest analog but uses mutable configure-then-run. Beamtalk's method-argument approach is simpler.
- `CommandResult` as a Value class aligns with ADR 0042 (immutable values for data).
- Stream integration is Beamtalk-idiomatic, not a Smalltalk pattern (Smalltalk uses positional ReadStream).

### Erlang/BEAM Developer
- Wraps `open_port` + `os:find_executable` — familiar mechanics, safer API.
- Port cross-process constraint is the same as file-backed streams (documented in ADR 0021).
- Can drop to raw Erlang interop for edge cases (`Erlang erlang open_port: ...`).

### Production Operator
- `System run:` is stateless — no long-lived resources, no lifecycle concerns.
- `System output:args:do:` provides deterministic cleanup for scripting.
- Standard BEAM process linking ensures subprocess cleanup on caller crash.

## Steelman Analysis

### Option A: Start with `os:cmd` wrapper, upgrade to `open_port` later

- **Newcomer**: "`os:cmd` works today, right now, zero risk. Ship something that works in a day, iterate."
- **BEAM veteran**: "`os:cmd` is a known quantity — no port lifecycle complexity, no zombie concerns, no finalizer design. Add streaming later when there's a real use case that demands it."
- **Incrementalist**: "Phase 0 with `os:cmd` proves the API ergonomics. If `System run: 'git' args: #('status')` feels right, then upgrade the internals to `open_port` without changing the API."

**Why rejected:** `os:cmd/1` invokes `/bin/sh -c` — shell injection by design. It also discards the exit code, which is critical for scripting (checking `$?`). Starting with `os:cmd` would mean either (a) shipping an insecure API that we immediately deprecate, or (b) not exposing exit codes in Phase 0 and adding them as a breaking change in Phase 1. The `open_port` implementation is only marginally more complex and gets both security and exit codes right from day one.

### Option B: Include Tier 2 (interactive `Subprocess` actor) in this ADR

- **Symphony developer**: "The whole point is to build orchestrators. One-shot commands are a stepping stone, not the goal. Shipping Tier 1 without Tier 2 leaves the motivating use case unresolved."
- **API designer**: "Designing both tiers together ensures API consistency — `System run:args:` and `Subprocess open:args:` share naming conventions and security model."

**Why deferred to a separate ADR:** The `Subprocess` actor pattern has unresolved design questions that don't affect Tier 1:
- **Stream-from-actor cross-process constraint** — an actor-owned port cannot produce a Stream consumable by the caller (ADR 0021's cross-process limitation). This requires either `gen_server:call` per line (slow), a message-passing protocol, or resolution of BT-507 (Future combinators).
- **Actor constructor pattern** — `Subprocess open:args:` is a custom class-side factory method that doesn't exist in the current Actor spawning protocol (`spawn` / `spawnWith:`). This needs design work.
- **Sync-by-default messaging conflict** — ADR 0043's sync `.` terminator means `agent lines.` is a `gen_server:call` that must return a complete value. Returning a lazy Stream from a sync call is semantically problematic when the Stream's generator needs the actor's port.

Tier 1 stands alone and is independently useful. Shipping it now unblocks scripting and simple automation. Tier 2 can be designed properly once the actor-stream interaction model is resolved.

### Option C: Use `Process` as the class name (per ADR 0021 sketch)

- **Smalltalk purist**: "Pharo uses `OSProcess` — `Process` is the natural name."
- **Newcomer**: "Process is what every other language calls it."

**Why rejected:** In the BEAM ecosystem, "process" universally means a lightweight BEAM process (gen_server, actor). Using `Process` for OS subprocesses creates confusion. The methods live on `System` (no naming conflict), and a future interactive actor class will use `Subprocess` (unambiguous).

### Tension Points
- Symphony developers want Tier 2 now; architectural rigor demands it be deferred until stream-from-actor semantics are resolved.
- Incrementalists prefer starting with `os:cmd`; security-first design demands `spawn_executable` from day one.
- Naming: `Process` is intuitive but collides with BEAM terminology. `Subprocess` is unambiguous but unfamiliar to Smalltalk developers.

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

## Consequences

### Positive
- Beamtalk gains OS command execution — unblocks scripting, automation, and tooling
- No shell injection by construction — `spawn_executable` with argument lists only
- Stream integration follows established `File lines:` pattern from ADR 0021
- Block-scoped `output:args:do:` provides deterministic cleanup for long-running commands
- `CommandResult` as a Value class is consistent with ADR 0042
- API is forward-compatible with separate stderr (adding methods, not changing existing ones)
- API is forward-compatible with a future `Subprocess` actor (consistent naming and security model)

### Negative
- No separate stderr in Phase 1 — merged output may confuse users who expect to distinguish error output
- No interactive subprocess support (stdin writing, bidirectional communication) — deferred to separate ADR
- Bare `System output:` Streams from long-running commands can leak subprocess resources if abandoned (mitigated by block-scoped form and documentation)
- Windows: `.bat`/`.cmd` resolution via `os:find_executable` may reintroduce shell semantics (documented caveat, not fully preventable)
- Zombie prevention relies on OS-level process termination via `os:cmd("kill ...")` — which itself uses a shell. This is standard practice in the BEAM ecosystem (Elixir's `System.cmd` does the same) but is ironic given the "no shell" security stance for user-facing APIs.

### Neutral
- `Port.bt` remains unchanged — still an opaque BEAM interop type
- `System.bt` gains three method families — moderate surface area expansion
- Port-backed Streams have the same cross-process constraint as file-backed Streams (ADR 0021) — no new constraint introduced
- The stdlib dependency on OTP's `os:find_executable/1` is minimal and well-tested

## Implementation

### Phase 1: `System run:args:` and `CommandResult` (M)

**New class: `CommandResult`**
- `stdlib/src/CommandResult.bt` — sealed Value subclass with `output`, `exitCode`, `isSuccess`
- `runtime/apps/beamtalk_runtime/src/beamtalk_command_result.erl` — value dispatch

**Extend `System`:**
- Add `run:args:` and `run:args:env:dir:` to `stdlib/src/System.bt`
- Add runtime dispatch in `runtime/apps/beamtalk_runtime/src/beamtalk_system.erl`
- Implement via `open_port({spawn_executable, os:find_executable(Cmd)}, [binary, exit_status, stderr_to_stdout, {args, Args}])`
- Collect all port data until `{Port, {exit_status, N}}`, return `CommandResult`

**Components:** stdlib, runtime, codegen (builtins registration for `CommandResult`)
**Tests:** `stdlib/test/system_command_test.bt` — spawn/capture, exit codes, command not found, env/dir overrides

### Phase 2: `System output:args:` streaming (M)

**Extend `System`:**
- Add `output:args:` and `output:args:do:` to `stdlib/src/System.bt`
- Implement port-backed Stream generator following `beamtalk_file.erl` line-stream pattern
- Stream finalizer calls `cleanup_port/1` (port close + OS PID kill)
- `output:args:do:` wraps block in `ensure:` for deterministic cleanup

**Components:** stdlib, runtime
**Tests:** `stdlib/test/system_output_test.bt` — streaming, pipeline composition, block-scoped cleanup, abandoned stream behavior

### Phase 3 (Future): Separate stderr
- Add erlexec dependency or build a Rust helper binary (following Porcelain/goon pattern)
- Add `stderr` and `stdout` to `CommandResult`
- Non-breaking: new methods on existing class

### Phase 4 (Future): Interactive `Subprocess` actor
- Separate ADR required — resolves stream-from-actor semantics (BT-507), actor constructor pattern, and sync-messaging interaction (ADR 0043)
- Enables Symphony-style orchestrator use case

## Future Direction: Interactive Subprocess Actor

The motivating Symphony use case requires bidirectional subprocess communication (write JSON-RPC to stdin, stream events from stdout). This needs an Actor that owns the port:

```beamtalk
// Sketch — requires separate ADR
agent := Subprocess spawnWith: #{#command => "codex", #args => #("app-server")}

// Write to stdin
agent send: (JSON generate: #{"method" => "initialize", "params" => #{"prompt" => "Fix the bug"}})

// Read lines — design TBD (gen_server:call per line? message-passing protocol?)
line := agent readLine
```

Key design questions for the future ADR:
1. How does the actor expose port output to callers without violating the cross-process Stream constraint?
2. Should `readLine` be a sync `gen_server:call` (simple but one-at-a-time), or should the actor push messages to a subscriber (more complex but streaming)?
3. How does `Subprocess` interact with the `calling_self` mechanism for self-sends within actor blocks?
4. What is the actor constructor pattern — `open:args:` as a custom class method, or `spawnWith:` with a config dictionary?

These questions are tractable but independent of Tier 1 and should not block shipping basic command execution.

## References
- Related issues: [BT-1119](https://linear.app/beamtalk/issue/BT-1119) (Subprocess execution — Process stdlib class), [BT-1118](https://linear.app/beamtalk/issue/BT-1118) (Epic: Stdlib Process, Timer, and Directory Libraries), BT-507 (Future combinators / stream-from-actor)
- Related ADRs: [ADR 0021](0021-streams-and-io-design.md) (Stream — `Process output:` deferred as future integration), [ADR 0022](0022-embedded-compiler-via-otp-port.md) (OTP Port pattern for compiler), [ADR 0028](0028-beam-interop-strategy.md) (Port as BEAM interop type), [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) (Value objects vs actors), [ADR 0043](0043-sync-by-default-actor-messaging.md) (Sync-by-default messaging)
- Erlang `open_port/2`: https://www.erlang.org/doc/apps/erts/erlang#open_port/2
- EEF Security WG on external executables: https://security.erlef.org/secure_coding_and_deployment_hardening/external_executables.html
- erlexec: https://github.com/saleyn/erlexec
- Elixir `System.cmd/3`: https://hexdocs.pm/elixir/System.html#cmd/3
- Pharo OSSubprocess: https://github.com/pharo-contributions/OSSubprocess
- Symphony spec (motivating use case): https://github.com/openai/symphony/blob/main/SPEC.md
