# ADR 0099: CLI Application Story — Console, Arguments, Exit, and Packaging

## Status
Proposed (2026-06-25)

> **Numbering note:** This ADR was originally tracked (BT-1687) as "ADR 0071".
> That number was taken by ADR 0071 (Class Visibility — `internal` Modifier)
> before this draft landed; it is renumbered 0099.

## Context

Beamtalk has strong foundations for system-level I/O, but **you cannot yet
write a standalone command-line application** in it. The pieces that exist
solve adjacent problems:

- **`System`** (`stdlib/src/System.bt`) — env vars, platform detection,
  `pid`, `uniqueId`. Class-side, no instances.
- **`File`** — full file I/O with lazy streams.
- **`Subprocess`** (ADR 0051) — bidirectional stdin/stdout/stderr with
  *child* processes. It is about talking to processes you spawn, not about the
  running program's own stdio.
- **`OS`** — one-shot shell command execution.
- **`TranscriptStream` / `Transcript`** (ADR 0010, ADR 0021) — a
  *per-workspace shared log* with pub/sub semantics. Subscribers (REPL
  clients, the LiveView IDE) receive output in real time. This is an
  interactive-development surface, **not** the OS process's stdout.

Three primitives are missing, and one packaging story is absent:

1. **Console / Stdio** — direct stdin/stdout/stderr for the *current* process.
   There is no `Console printLine: "hello"`, and no way to read a line from
   stdin. `Subprocess` covers child I/O; `Transcript` covers the workspace
   log; neither is the running program's own terminal.
2. **CommandLine / Arguments** — access to the argv passed to the program.
   `beamtalk run ClassName selector` (ADR 0061) dispatches a *unary* class
   message and forwards nothing after it, so a script cannot see its arguments.
3. **Exit code** — clean process termination with a status code
   (`erlang:halt/1`). Scripts currently exit 0 when the entry method returns
   and have no way to signal failure to the shell.
4. **Packaging** — there is no way to produce a distributable artifact. You
   need a Beamtalk project checkout and `beamtalk run` to execute anything.

The Erlang primitives behind all of this are trivial one-liners
(`io:get_line/1`, `io:put_chars/2`, `io:format/2`, `init:get_plain_arguments/0`,
`erlang:halt/1`). **The design work is the Beamtalk class API and the
packaging/distribution story**, not the FFI.

### Constraints

- **Unary entry points (ADR 0061).** `beamtalk run ClassName selector` accepts
  exactly two positional CLI tokens and dispatches a unary class message. The
  CLI validator explicitly rejects keyword selectors
  (`crates/beamtalk-cli/src/commands/run.rs`). Whatever we do for arguments
  must *not* reintroduce keyword-selector entry points — args reach the program
  some other way.
- **Group leader / where stdout goes.** In `beamtalk run` script mode the BEAM
  node's group leader is the invoking terminal, so `io:format/2` reaches the
  user. Under `beamtalk repl` (a detached/persistent workspace), the node's
  stdout is **not** the connected client's terminal. Interactive console I/O is
  therefore fundamentally a `beamtalk run` story, not a REPL story; the design
  must say so rather than silently misbehave.
- **Reproducible, workspace-first startup (ADR 0061).** Any packaged artifact
  must still register all project classes (topo-ordered bootstrap) before user
  code dispatches, or it hits the same class-loading crash ADR 0061 fixed.
- **Cross-platform (ADR 0027, CLAUDE.md).** No hardcoded paths; escript
  packaging must work on Unix and Windows.

### Desired end state

```beamtalk
// greeter.bt
Object subclass: Greeter
  class run -> Nil =>
    name := CommandLine arguments at: 1 ifAbsent: ["World"]
    Console printLine: "Hello, " ++ name ++ "!"
```

```text
$ beamtalk run Greeter run -- Alice
Hello, Alice!
$ beamtalk run Greeter run
Hello, World!
```

…and, packaged:

```text
$ beamtalk build --escript --entry "Greeter run" -o greeter
$ ./greeter Alice
Hello, Alice!
```

## Decision

Add three small stdlib surfaces and one packaging command. Each is scoped to
the one concern it owns; none overload an existing class whose meaning would
blur.

### 1. `Console` — the current process's stdio

A new `sealed typed Object subclass: Console` singleton (class-side, no
instances — same shape as `System` and `OS`). It wraps the BEAM standard I/O
of the *running* program.

```beamtalk
Console printLine: "ready"          // stdout + newline
Console print: "no newline"          // stdout, no newline
Console errorLine: "something broke" // stderr + newline
Console error: aValue                // stderr, no newline
Console flush                        // flush buffered output

line := Console readLine             // one line from stdin; nil at EOF
line := Console readLine: "name? "   // prompt to stdout, then read
Console isInteractive                // true when stdin is a tty
```

- **What it prints.** `print:` / `printLine:` accept any object and render it
  via the **`displayString`** protocol (ADR 0094) — `"abc"` prints as `abc`,
  not `"abc"`. `error:` / `errorLine:` are the stderr equivalents.
- **Reading.** `readLine` returns a `String` with the trailing newline
  stripped, or `nil` at end of input. `readLine:` writes a prompt to stdout
  first (no newline) then reads.
- **Backed by** a thin `beamtalk_console` Erlang module: `io:put_chars/2` to
  `standard_io` / `standard_error`, `io:get_line/1`, `io:setopts` for
  tty detection.

**`Console` vs `Transcript` — the line that must stay sharp.** `Transcript`
is the *workspace* log: pub/sub, per-workspace, consumed by REPL/IDE
subscribers, survives across script runs. `Console` is *this OS process's*
stdio: line-oriented, point-to-point with the terminal, meaningful only while
the program runs. A CLI tool writes to `Console`; a live-coding session
observes `Transcript`. They are different surfaces and we keep both. (`Object
>> show:` / `showCr:` continue to target `Transcript`, unchanged.)

**REPL caveat (documented, not silently broken).** Under a detached/persistent
workspace, `Console` writes reach the *node's* stdout, not the connected
client. `Console readLine` in that context reads from the server's stdin (which
is typically closed) and returns `nil`. The hover/doc for `Console` states
plainly: interactive console I/O is for `beamtalk run` / packaged scripts;
inside the REPL, prefer `Transcript`.

### 2. `CommandLine` — program arguments

A new `sealed typed Object subclass: CommandLine` singleton wrapping
`init:get_plain_arguments/0`.

```beamtalk
CommandLine arguments            // => List(String) — args after `--`
CommandLine arguments size       // => count
CommandLine arguments at: 1 ifAbsent: ["default"]
CommandLine programName          // => "greeter" (escript) or class name (run)
```

Arguments reach the program through the **node tail**, not the entry selector,
which keeps ADR 0061's unary-selector rule intact:

```text
beamtalk run Greeter run -- Alice Bob
                         └─────────── forwarded as `-extra Alice Bob`
```

`beamtalk run` gains a `--` separator: everything after it is appended to the
BEAM invocation as `-extra <args...>`, which is exactly what
`init:get_plain_arguments/0` returns. The entry method stays unary
(`Greeter run`); the program asks `CommandLine arguments` for its argv. This
resolves the original ticket's `main: args` sketch — args are *pulled*, not
*pushed* through the selector.

For a packaged escript, `CommandLine arguments` returns the escript's `main/1`
argument list directly.

### 3. `System exit` / `System exit:` — process termination with status

Exit is process-level system behaviour, so it extends **`System`** rather than
introducing a one-method `ExitCode` class.

```beamtalk
System exit          // erlang:halt(0) after flushing stdio
System exit: 1       // erlang:halt(1)
```

- `exit:` requires an `Integer` 0–255 (POSIX status range); out-of-range or
  non-Integer raises `#type_error` (consistent with `System setEnv:`).
- Both flush `Console` before halting so buffered output is not lost.
- **Semantics:** `erlang:halt/1` is an *immediate* VM halt — it does **not**
  run OTP shutdown or `terminate/2` callbacks. That is the correct semantics
  for a script (run to completion, report status, exit). Graceful shutdown of a
  *service* is a different concern (`init:stop()`, supervision tree teardown)
  and stays out of scope here — it belongs to the service-lifecycle / release
  story (ADR 0061 "Future: Release Mode").

**Implicit exit code from the entry method (recommended).** When the entry
method returns normally and never calls `System exit:`, the run exits `0`. If
the entry method raises an uncaught `#beamtalk_error{}`, the script prints the
error to stderr and exits non-zero (proposed: `1`). This gives well-behaved
shell semantics without forcing every script to call `System exit:` explicitly.

### 4. `beamtalk build --escript` — single-file packaging

`beamtalk build --escript --entry "ClassName selector" -o <name>` produces a
self-contained executable escript.

```text
$ beamtalk build --escript --entry "Greeter run" -o greeter
$ ./greeter Alice
Hello, Alice!
```

- **Archive contents.** The escript embeds a zip archive (standard escript
  format) containing: all project `bt@*.beam`, the `beamtalk_runtime` and
  `beamtalk_stdlib` application beams, and a generated bootstrap module
  exposing `main/1`.
- **Entry resolution.** Escript requires a `main/1`. We generate a tiny wrapper
  whose `main(Args)`:
  1. starts `beamtalk_runtime`,
  2. starts a **run-mode workspace** (`repl = false` — ADR 0061 §5: no REPL
     server, not registered, no idle monitor) so all classes bootstrap in topo
     order,
  3. seeds `CommandLine` from `Args`,
  4. dispatches `ClassName>>selector`,
  5. maps the result / any uncaught error to an exit code (per §3).
- **Why run-mode reuse.** ADR 0061 already defined the exact
  "bootstrap-then-dispatch-then-exit" lifecycle a script needs. The escript
  wrapper is that lifecycle with argv seeded and a `main/1` shell — no new
  startup path to maintain.
- **`--entry` default.** If the project manifest declares a single obvious
  entry (future `[scripts]` table, ADR 0061), `--entry` may be omitted.

## Prior Art

**Smalltalk (Pharo / Squeak).** `Transcript` is the workspace log — exactly our
`TranscriptStream`. Pharo has no clean separate "current-process stdout" for
headless scripts historically; `Stdio stdout` and `FileStream stdout` were
bolted on later for command-line use. The split we draw (`Transcript` =
workspace log, `Console` = OS stdio) is the lesson learned from Pharo
conflating them. Pharo exits via `Smalltalk quit:` (status code).

**Erlang.** `io:format/2`, `io:get_line/1`, `init:get_plain_arguments/0`,
`erlang:halt/1`, and the escript format (`main/1`, embedded archive) are the
substrate we wrap directly. Escript is Erlang's canonical single-file CLI
packaging — we adopt it wholesale rather than invent a format.

**Elixir.** `IO.puts/2`, `IO.gets/1`, `IO.read/2`; `System.argv/0`,
`System.halt/1`; packaging via `Mix.escript` (`mix escript.build` →
single-file executable). Our `Console` / `CommandLine` / `System exit:` /
`--escript` map almost one-to-one onto `IO` / `System.argv` / `System.halt` /
`escript.build`. Notably Elixir keeps stdio (`IO`) and argv/exit (`System`)
in *separate* modules — we follow that split (`Console` vs `System`).

**Gleam.** `gleam/io.println`, and `argv.load().arguments` from the `argv`
package. Minimal, function-based; less relevant to a class-based API but
confirms argv-as-a-pulled-list is the common shape.

**Rust.** `std::io` (`stdin`/`stdout`/`stderr` handles, line reading),
`std::env::args`, `std::process::exit`, and `clap` for declarative arg parsing.
We deliberately scope *parsing* (clap-style) out — `CommandLine arguments`
returns raw strings; a richer arg-parser is a library/follow-up, not a
language primitive.

**What Beamtalk adopts:** Erlang escript packaging verbatim; the Elixir
module split (stdio separate from system/exit); argv as a pulled list (Gleam /
Rust / Elixir). **What it rejects:** conflating workspace log with process
stdio (the Pharo trap); built-in declarative arg parsing (clap) — left to a
library.

## User Impact

**Newcomer.** Can finally write the canonical first program: read args, print a
greeting, exit. `Console printLine:` is discoverable and reads naturally.
`beamtalk run Greeter run -- Alice` works; `beamtalk build --escript` produces
something they can hand to a friend.

**Smalltalk developer.** The `Console`/`Transcript` distinction needs one
sentence of explanation but maps to the Pharo `Stdio` vs `Transcript`
distinction they may already know. `System exit:` mirrors `Smalltalk quit:`.

**Erlang/BEAM developer.** Recognises every primitive immediately —
`io:format`, `init:get_plain_arguments`, `halt`, escript. The wrapper-`main/1`
escript story is idiomatic. The "halt does not run terminate/2" note is exactly
the semantics they expect.

**Operator.** Gets POSIX-correct exit codes (0 on success, non-zero on error)
so Beamtalk scripts compose in shell pipelines and CI `set -e`. Escript output
is a single deployable file.

## Steelman Analysis

**"Put stdout on `Transcript`, don't add `Console`."** *(Smalltalk cohort.)*
One less class; `Transcript show:` already exists and Smalltalkers reach for it.
Counter: `Transcript` is pub/sub and per-workspace; wiring real stdin/tty
detection/stderr/exit-flush into it would overload a class whose whole point is
the *workspace* log, and would behave bizarrely under the REPL (where Transcript
goes to subscribers, not a terminal). The surfaces are genuinely different.

**"Pass args to the entry method: `Greeter run: args`."** *(Newcomer cohort.)*
`main(argv)` is the universal mental model; pulling from a `CommandLine`
singleton is indirection. Counter: ADR 0061 deliberately fixed entry points at
*unary* selectors and removed config-driven entry expressions; reintroducing a
keyword entry selector reopens that design. `CommandLine arguments` keeps one
entry contract and still gives the program its argv.

**"Fold exit into `Console` (or a dedicated `ExitCode`)."** *(Consistency
cohort.)* `Console exit: 1` reads as "I'm done writing, leave with this code."
Counter: exit is whole-VM behaviour, not stdio; `System` already owns
process-level concerns (`pid`, env, platform). A one-method `ExitCode` class is
ceremony. `System exit:` is the cohesive home.

**"Ship OTP releases, not escripts."** *(Operator cohort.)* Releases are the
production-grade BEAM artifact (hot upgrades, ERTS bundling, config). Counter:
releases are heavyweight and ADR 0061 already earmarks `beamtalk release` as a
separate future ADR. Escript is the right *first* packaging for scripts —
single file, zero config — and does not preclude releases later.

**Tension point.** The `Console`/`Transcript` boundary is the one place
reasonable people will disagree and the one most likely to confuse users. The
mitigation is documentation-heavy: explicit hover text, an example in the
language guide, and the REPL caveat called out on `Console` itself.

## Alternatives Considered

### Single `Stdio` class holding stdin/stdout/stderr/argv/exit

```beamtalk
Stdio out printLine: "hi".  Stdio args.  Stdio exit: 1.
```

One import, everything-CLI in one place (closer to Rust's `std::io` + `std::env`
grab-bag). Rejected: bundles three unrelated concerns (I/O, argv, process exit)
and competes with `System`'s existing ownership of process-level info. The
split (`Console` / `CommandLine` / `System exit:`) matches Elixir and keeps each
class coherent.

### `Console` as a real instance / stream object (ADR 0021 Stream)

`Console stdout` returns a writable `Stream` you can pipe into. More composable
with the existing Stream protocol. Partially deferred: the **class-side
convenience API** (`Console printLine:`) covers the 90% case now; exposing
`Console stdin` / `Console stdout` as `Stream`s for piped/lazy I/O is a natural
follow-up once the basics land, not a blocker.

### Forward args via a generated entry-wrapper instead of `-extra`

Generate a per-run module that closes over the parsed args. Rejected for
`beamtalk run`: `-extra` + `init:get_plain_arguments/0` is the zero-codegen
path and is exactly what the escript `main/1` already receives, so both
surfaces share one argv source.

### Declarative arg parsing as a primitive (clap-style)

`CommandLine flag: "--verbose"` etc. baked into the language. Rejected: arg
*parsing* policy (subcommands, validation, help generation) is library
territory; the primitive is the raw `List(String)`. A `clap`-equivalent can be
a Beamtalk package built on `CommandLine arguments`.

## Consequences

### Positive
- Standalone CLI applications become possible — a capability the language
  currently lacks entirely.
- POSIX-correct exit codes make Beamtalk scripts first-class shell/CI citizens.
- Single-file escript artifacts are distributable without a project checkout.
- Clear `Console` vs `Transcript` separation prevents the Pharo conflation.
- Reuses ADR 0061's run-mode lifecycle for escript boot — no new startup path.

### Negative
- One more concept (`Console`) that overlaps superficially with `Transcript`
  and needs documentation to disambiguate.
- `beamtalk run … -- args` adds a CLI surface (the `--` separator) and a
  forwarding path through `-extra`.
- Escript packaging adds a build mode (archive assembly, `main/1` generation,
  Windows executable-wrapper handling) to maintain.
- `Console readLine` returning `nil` under the REPL is a sharp edge that will
  surface as a "why is stdin empty" question despite the documented caveat.

### Neutral
- `System` grows two methods (`exit`, `exit:`); `Object >> show:`/`showCr:` and
  `Transcript` are unchanged.
- Full OTP releases remain a separate future ADR; this ADR neither blocks nor
  presumes them.

## Implementation

Rough phases; each is independently shippable and testable.

### Phase 1 — `Console` (stdio)
- `runtime/.../beamtalk_console.erl`: `printLine:`/`print:`/`errorLine:`/
  `error:`/`flush`/`readLine`/`readLine:`/`isInteractive` over `standard_io` /
  `standard_error`, with tty detection via `io:getopts`.
- `stdlib/src/Console.bt`: class-side methods delegating via `(Erlang
  beamtalk_console) …`, doc comments incl. the REPL caveat. `print:`/`printLine:`
  render via `displayString` (ADR 0094).
- Tests: bootstrap/BUnit for the non-interactive methods; an
  `examples/`-style smoke script for interactive read under `beamtalk run`.

### Phase 2 — `CommandLine` (argv) + `beamtalk run -- args`
- `beamtalk_command_line.erl` wrapping `init:get_plain_arguments/0` →
  `List(String)`; `programName`.
- `stdlib/src/CommandLine.bt`.
- CLI: `crates/beamtalk-cli/src/commands/run.rs` — accept a `--` separator,
  append trailing tokens as `-extra` to the BEAM invocation
  (`build_script_eval_cmd` / arg assembly). Validate that `--`/`-extra` does not
  collide with existing flag parsing.

### Phase 3 — `System exit` / `exit:` + implicit exit code
- `beamtalk_system:exit/0,1` → flush `Console`, `erlang:halt/0,1`; Integer
  0–255 validation, `#type_error` otherwise.
- Run-mode dispatch (`run.rs` eval / escript wrapper): map normal return → 0,
  uncaught `#beamtalk_error{}` → stderr + non-zero.
- `stdlib/src/System.bt`: add `class sealed exit -> Nil` and `class sealed
  exit: code :: Integer -> Nil` (this method does not return).

### Phase 4 — `beamtalk build --escript`
- `crates/beamtalk-cli/src/commands/build.rs` (+ a new `escript` assembly
  module): collect `bt@*.beam` + runtime/stdlib beams, generate the `main/1`
  bootstrap module (run-mode workspace + `CommandLine` seed + dispatch + exit
  mapping), emit an escript archive; set the executable bit (Unix) / `.escript`
  + launcher (Windows, per ADR 0027).
- `--entry "ClassName selector"` parsing; default from manifest if unambiguous.

### Surface parity
Per CLAUDE.md, update `docs/development/surface-parity.md`: `Console` and
`CommandLine` are runtime stdlib (available everywhere a workspace runs);
`beamtalk build --escript` and `beamtalk run -- args` are **CLI-specific**
operations and should be labelled as such.

## Migration Path

Net-new capability — nothing to migrate. The `--` separator and the two new
`System` methods are additive; existing `beamtalk run ClassName selector`
invocations (no `--`) behave identically.

## References
- Related issues: BT-1687 (this ADR)
- Related ADRs:
  - ADR 0061 — Program Entry Points and Run Lifecycle (unary entry, run-mode
    workspace, "Future: Release Mode")
  - ADR 0051 — Subprocess Execution (child-process I/O; contrast with Console)
  - ADR 0021 — Stream and I/O Design (future `Console` Stream surface)
  - ADR 0010 — Global Objects and Singleton Dispatch (`Transcript` global)
  - ADR 0094 — Object String Representation Protocols (`displayString` for
    `Console print:`)
  - ADR 0027 — Cross-Platform Support (escript on Windows)
- Existing stdlib: `stdlib/src/System.bt`, `File.bt`, `Subprocess.bt`,
  `OS.bt`, `TranscriptStream.bt`, `Object.bt` (`show:`/`showCr:`)
- Erlang primitives: `io:put_chars/2`, `io:get_line/1`, `io:format/2`,
  `init:get_plain_arguments/0`, `erlang:halt/1`, escript `main/1`
- CLI: `crates/beamtalk-cli/src/commands/run.rs`, `build.rs`
- Follow-up work: `Console` Stream surface (ADR 0021), declarative arg-parsing
  library, `[scripts]` manifest table (ADR 0061), `beamtalk release` (future ADR)
