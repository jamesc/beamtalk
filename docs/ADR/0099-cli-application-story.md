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
3. **Exit with a status code** — a program needs to end with a status the shell
   can read. Scripts currently exit 0 when the entry method returns and have no
   way to signal failure. The subtlety (see Constraints) is that "exit" has
   *two* meanings: halt the whole VM (right for a standalone app that owns the
   node) versus end *this* invocation/job with a status (right for a script
   dispatched into a shared, already-running workspace, where halting the VM
   would kill the service and every other session).
4. **Packaging** — there is no way to produce a distributable artifact. You
   need a Beamtalk project checkout and `beamtalk run` to execute anything.

Most of the Erlang primitives behind this are one-liners (`io:get_line/1`,
`io:put_chars/2`, `io:format/2`, `init:get_plain_arguments/0`,
`erlang:halt/1`). Two are *not* trivial and the design must treat them as such:
(a) portable **tty detection** — BEAM has no simple `isatty`, only heuristics
(`io:getopts`/`io:columns`, `prim_tty`); and (b) **job-level exit** — ending
one invocation's process group without halting the node, which needs a
cooperating run harness, not a single FFI call. **The design work is the
Beamtalk class API, the two-tier exit model, and the packaging/distribution
story**, not the bulk of the FFI.

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
- **Two execution contexts, two exit meanings (ADR 0061).** `beamtalk run
  ClassName selector` runs in one of two ways: it starts a fresh **run-mode**
  workspace that owns the VM (the program *is* the node), **or** it detects a
  live workspace for the project and **connects + evals** the entry point
  *inside that shared node* (ADR 0061 §3). In the first case "exit" can mean
  "halt the VM"; in the second, halting the VM would tear down a service and
  every other connected session, so "exit" must mean "end this job and report a
  status to the connecting CLI." The exit design must serve both.
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

Add three stdlib surfaces and one packaging command. Each is scoped to the one
concern it owns; none overload an existing class whose meaning would blur. The
key shape: `Console` owns the process's *stdio*, `CommandLine` owns *this
invocation* (its args **and** its exit), and `System` owns *the machine/VM*
(including the nuclear whole-node halt).

### 1. `Console` — the current process's stdio

A new `sealed typed Object subclass: Console` singleton (class-side, no
instances — same shape as `System` and `OS`). It wraps the BEAM standard I/O
of the *running* program.

```beamtalk
Console printLine: aValue            // stdout + newline   (aValue :: Printable)
Console print: aValue                // stdout, no newline
Console errorLine: aValue            // stderr + newline
Console error: aValue                // stderr, no newline
Console flush                        // flush buffered output

line := Console readLine             // one line from stdin; nil at EOF
line := Console readLine: "name? "   // prompt to stdout, then read
```

- **What it prints.** `print:` / `printLine:` take a `Printable` (the same
  parameter type `TranscriptStream show:` and `Json generate:` use) and render
  it via the **`displayString`** protocol (ADR 0094, implemented) — `"abc"`
  prints as `abc`, not `"abc"`. `error:` / `errorLine:` are the stderr
  equivalents.
- **Reading.** `readLine` returns a `String` with the trailing newline
  stripped, or `nil` at end of input. `readLine:` writes a prompt to stdout
  first (no newline) then reads.
- **Backed by** a thin `beamtalk_console` Erlang module: `io:put_chars/2` to
  `standard_io` / `standard_error`, `io:get_line/1`.
- **Deferred — `Console isInteractive` (tty detection).** Useful for "prompt
  only when attached to a terminal," but BEAM has no portable `isatty`; it
  requires version-sensitive heuristics (`io:getopts`/`io:columns` returning
  `{error, enotsup}`, or `prim_tty`). It is **not** in the v1 surface — added
  later once a robust, cross-platform check is settled, so the rest of `Console`
  is not blocked on it.

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

### 2. `CommandLine` — this invocation: arguments and exit

A new `sealed typed Object subclass: CommandLine` singleton. It represents the
*current program invocation* — its arguments **and** its termination. This dual
ownership is deliberate (see Steelman): `CommandLine` is the "job" object, which
is exactly why argv lives here rather than on `System`.

```beamtalk
CommandLine arguments            // => List(String) — args after `--`
CommandLine arguments size       // => count
CommandLine arguments at: 1 ifAbsent: ["default"]
CommandLine programName          // => see below

CommandLine exit                 // end this program with status 0
CommandLine exit: 1              // end this program with status 1
```

**Arguments.** `arguments` wraps `init:get_plain_arguments/0`. They reach the
program through the **node tail**, not the entry selector, which keeps ADR
0061's unary-selector rule intact:

```text
beamtalk run Greeter run -- Alice Bob
                         └─────────── forwarded as `-extra Alice Bob`
```

`beamtalk run` gains a `--` separator: everything after it is appended to the
BEAM invocation as `-extra <args...>`, which `init:get_plain_arguments/0`
returns. The entry method stays unary (`Greeter run`); the program *pulls* its
argv from `CommandLine arguments` rather than having it *pushed* through the
selector — resolving the original ticket's `main: args` sketch. For a packaged
escript, `arguments` returns the escript's `main/1` list directly.

**`programName`** is defined crisply per surface, not hand-waved: under a
packaged escript it is `escript:script_name/0` (the invoked filename); under
`beamtalk run` there is no argv[0], so it is the literal `"beamtalk"`. (It is a
convenience for usage/`--help` text, not load-bearing.)

**Exit — the job-level, safe default.** `CommandLine exit` / `exit:` end *this
invocation* with a status **without halting the VM**:

- In **run-mode** (the program owns the node), it unwinds the entry dispatch,
  tears down the run-mode workspace this invocation started, and the node exits
  with the status — a graceful shutdown, not a raw `halt`.
- In **connected mode** (entry evaluated inside a live shared workspace, ADR
  0061 §3), it stops only *this job's process group* (the session's
  group-leader-scoped processes) and reports the status back over the REPL
  protocol to the connecting CLI, which sets the shell exit code. **The shared
  node and every other session keep running.**
- In an **escript**, it ends `main/1` with the status.

Mechanically, `exit:` raises a tagged `script_exit` signal that the run harness
(run-mode dispatcher, escript `main/1`, or connected-session evaluator) catches
and translates to the context-appropriate teardown. This is the cooperating-
harness "job-level exit" flagged in Context as the one non-trivial exit
primitive. `CommandLine exit:` is the **recommended** way for a program to exit
deliberately; `System exit:` (§3) is the explicit nuclear alternative.

### 3. `System exit` / `System exit:` — whole-VM halt (the nuclear option)

Where `CommandLine exit:` ends the *job*, `System exit:` halts the *whole node*.
It lives on `System` because halting the VM is machine-level behaviour, next to
`System`'s existing process/platform info (`pid`, env, architecture).

```beamtalk
System exit          // erlang:halt(0) after flushing stdio
System exit: 1       // erlang:halt(1)
```

- `exit:` requires an `Integer` 0–255 (POSIX status range); out-of-range or
  non-Integer raises `#type_error` (consistent with `System setEnv:`).
- Both flush `Console` before halting so buffered output is not lost.
- **Semantics:** `erlang:halt/1` is an *immediate* VM halt — it does **not**
  run OTP shutdown or `terminate/2` callbacks. Correct for a standalone app
  that owns the node and wants to stop *now*.
- **⚠ Shared-workspace warning.** `System exit:` halts the entire node. Called
  inside a live, shared workspace (a connected `beamtalk run`, or in the REPL),
  it kills the service and every other connected session. In a non-run-mode
  workspace the runtime therefore **refuses** `System exit:` and raises a
  `#beamtalk_error{}` directing the caller to `CommandLine exit:` instead; the
  raw halt is permitted only when the program owns the node (run-mode / escript).
  Graceful shutdown of a service is a separate concern (`init:stop()`,
  supervision teardown) owned by the service-lifecycle / release story (ADR 0061
  "Future: Release Mode").

**Implicit exit code from the entry method (recommended path).** Most programs
never call either `exit:` explicitly. When the entry method returns normally,
the run exits `0`. If it raises an uncaught `#beamtalk_error{}`, the harness
prints the error to stderr and exits non-zero (proposed: `1`). This gives
well-behaved shell semantics for free; `CommandLine exit:` is for *early*
termination, and `System exit:` for the deliberate whole-node halt.

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
  5. catches a `script_exit` from `CommandLine exit:` (→ its status) and maps a
     normal return → 0 / any uncaught error → non-zero (per §2–§3).
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
single-file executable). Our `Console` maps onto `IO`, and `--escript` onto
`escript.build`. We **diverge** on argv and exit: Elixir puts both `argv` and
`halt` on `System`, whereas we put argv and the *safe job-level* exit on
`CommandLine` and reserve `System exit:` for the whole-VM halt. The reason is
that Elixir's `System.halt` always halts the OS process — Elixir has no notion
of a long-lived *shared* node that a script is dispatched into, so it never
needs a job-level exit. Beamtalk does (ADR 0061's connect-and-eval path), so the
two-tier split is a real distinction, not gratuitous.

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
distinction they may already know. `System exit:` mirrors `Smalltalk quit:`;
`CommandLine exit:` is the gentler "end this doit" they'd want when evaluating
against a live image.

**Erlang/BEAM developer.** Recognises every primitive immediately —
`io:format`, `init:get_plain_arguments`, `halt`, escript. The wrapper-`main/1`
escript story is idiomatic, and the `CommandLine exit:` / `System exit:` split
maps to the familiar "end the job vs `halt` the node" distinction. The "halt
does not run terminate/2" note is exactly the semantics they expect.

**Operator.** Gets POSIX-correct exit codes (0 on success, non-zero on error)
so Beamtalk scripts compose in shell pipelines and CI `set -e`. Crucially, a
script run against a live service (`beamtalk run Db migrate` on a running node)
exits its job cleanly without taking the service down. Escript output is a
single deployable file.

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

**"Put argv on `System` too, next to `exit` — like Elixir."** *(Consistency
cohort.)* If exit is process-level and lives on `System`, argv is equally
process-level; splitting them across `CommandLine` and `System` looks
arbitrary, and Elixir keeps both on `System`. Counter: the split is *not*
arbitrary once exit is two-tier. `CommandLine` is the **invocation/job** object
— "this program, its inputs (argv) and its outcome (job-level `exit:`)" — while
`System` is the **machine** ("this node, its env, its whole-VM halt"). Argv and
the safe `exit:` belong together on the job; the nuclear `System exit:` belongs
on the machine. Folding argv into `System` would split the *job's* two halves
(its args and its exit) across two classes instead — the worse cut.

**"One exit, not two — `Console exit:` or a single `System exit:`."**
*(Simplicity cohort.)* Two exit methods on two classes is more to learn than
one. Counter: a single `erlang:halt`-backed exit is actively dangerous in the
connect-and-eval path (it kills a shared service); a single *context-aware*
exit hides a 100×-consequence difference behind one name. Two clearly-named
tiers — safe-job-exit (`CommandLine exit:`) vs nuke-the-node (`System exit:`) —
make the consequence legible at the call site. A one-method `ExitCode` class
would be pure ceremony.

**"Ship OTP releases, not escripts."** *(Operator cohort.)* Releases are the
production-grade BEAM artifact (hot upgrades, ERTS bundling, config). Counter:
releases are heavyweight and ADR 0061 already earmarks `beamtalk release` as a
separate future ADR. Escript is the right *first* packaging for scripts —
single file, zero config — and does not preclude releases later.

**Tension points.** Two places reasonable people will disagree: (1) the
`Console`/`Transcript` boundary — mitigated documentation-heavily via explicit
hover text, a language-guide example, and the REPL caveat on `Console` itself;
and (2) the `CommandLine exit:` / `System exit:` split — mitigated by making
`CommandLine exit:` the documented default everywhere and having the runtime
*refuse* `System exit:` in a shared workspace rather than letting it silently
nuke the node.

## Alternatives Considered

### Single `Stdio` class holding stdin/stdout/stderr/argv/exit

```beamtalk
Stdio out printLine: "hi".  Stdio args.  Stdio exit: 1.
```

One import, everything-CLI in one place (closer to Rust's `std::io` + `std::env`
grab-bag). Rejected: bundles unrelated concerns (stdio, argv, exit) into one
class and competes with `System`'s existing ownership of process-level info.
The chosen split — `Console` (stdio) / `CommandLine` (this invocation: argv +
job exit) / `System exit:` (whole-VM halt) — keeps each class coherent around a
single subject (the terminal, the job, the machine).

### Context-aware single exit (`System exit:` that degrades when connected)

One `System exit: N` that means `erlang:halt(N)` when the program owns the node
but silently becomes a job-level exit when dispatched into a shared workspace.
Fewer surfaces. Rejected: same name, two outcomes that differ by 100× in
blast radius (end my script vs. kill everyone's service), with the difference
invisible at the call site and dependent on runtime context the author may not
know. The two-tier model makes the choice explicit; the dangerous one
(`System exit:`) is named for what it does and refused where it would be
catastrophic.

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
- The two-tier exit makes scripts safe to run *against a live shared workspace*
  — `CommandLine exit:` ends the job without endangering the node.
- Single-file escript artifacts are distributable without a project checkout.
- Clear `Console` vs `Transcript` separation prevents the Pharo conflation.
- Reuses ADR 0061's run-mode lifecycle for escript boot — no new startup path.

### Negative
- One more concept (`Console`) that overlaps superficially with `Transcript`
  and needs documentation to disambiguate.
- Two exit methods to teach (`CommandLine exit:` vs `System exit:`); the
  distinction must be made legible or users will reach for the wrong one.
- The job-level `CommandLine exit:` is the one genuinely non-trivial primitive
  — it needs a cooperating `script_exit` catch in three harness paths (run-mode
  dispatcher, escript `main/1`, connected-session evaluator).
- `beamtalk run … -- args` adds a CLI surface (the `--` separator) and a
  forwarding path through `-extra`.
- Escript packaging adds a build mode (archive assembly, `main/1` generation,
  Windows executable-wrapper handling) to maintain.
- `Console readLine` returning `nil` under the REPL is a sharp edge that will
  surface as a "why is stdin empty" question despite the documented caveat.

### Neutral
- `System` grows two methods (`exit`, `exit:`); `CommandLine` is net-new;
  `Object >> show:`/`showCr:` and `Transcript` are unchanged.
- `Console isInteractive` (tty detection) is explicitly deferred out of v1.
- Full OTP releases remain a separate future ADR; this ADR neither blocks nor
  presumes them.

## Implementation

Rough phases; each is independently shippable and testable.

### Phase 1 — `Console` (stdio)
- `runtime/.../beamtalk_console.erl`: `printLine:`/`print:`/`errorLine:`/
  `error:`/`flush`/`readLine`/`readLine:` over `standard_io` / `standard_error`
  (`io:put_chars/2`, `io:get_line/1`). `isInteractive` is **deferred** (no
  portable tty check yet — see §1).
- `stdlib/src/Console.bt`: class-side methods delegating via `(Erlang
  beamtalk_console) …`, `Printable`-typed print params, doc comments incl. the
  REPL caveat. `print:`/`printLine:` render via `displayString` (ADR 0094,
  already implemented).
- Tests: bootstrap/BUnit for the non-interactive methods; an
  `examples/`-style smoke script for interactive read under `beamtalk run`.

### Phase 2 — `CommandLine` argv + `beamtalk run -- args`
- `beamtalk_command_line.erl` wrapping `init:get_plain_arguments/0` →
  `List(String)`; `programName` (`escript:script_name/0` under escript, else
  `"beamtalk"`).
- `stdlib/src/CommandLine.bt` (argv methods; `exit`/`exit:` land in Phase 3).
- CLI: `crates/beamtalk-cli/src/commands/run.rs` — accept a `--` separator and
  append trailing tokens as `-extra <args...>`. **Ordering matters:**
  everything after `-extra` becomes a plain argument, so `-extra` must be the
  *last* group on the `erl` line — after the existing `-eval` (which
  `build_script_eval_cmd` / arg assembly currently appends). Validate `--`
  does not collide with existing flag parsing.

### Phase 3 — two-tier exit + implicit exit code
- **Job-level (`CommandLine exit`/`exit:`):** raise a tagged `script_exit`
  carrying the status. Add the catch to all three harnesses: the run-mode
  dispatcher (`run.rs` eval), the escript `main/1`, and the connected-session
  evaluator (report status back over the REPL protocol; stop the session's
  process group; leave the node up). `stdlib/src/CommandLine.bt`: `class sealed
  exit -> Nil`, `class sealed exit: code :: Integer -> Nil` (do not return).
- **VM-level (`System exit`/`exit:`):** `beamtalk_system:exit/0,1` → flush
  `Console`, `erlang:halt/0,1`; Integer 0–255 validation (`#type_error`
  otherwise); **refuse with a `#beamtalk_error{}` when not in a node-owning
  (run-mode/escript) context.** `stdlib/src/System.bt`: `class sealed exit ->
  Nil`, `class sealed exit: code :: Integer -> Nil`.
- **Implicit code:** harness maps normal return → 0, uncaught
  `#beamtalk_error{}` → stderr + non-zero.

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

Net-new capability — nothing to migrate. The `--` separator, the new
`CommandLine` class, and the two new `System` methods are all additive;
existing `beamtalk run ClassName selector` invocations (no `--`) behave
identically.

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
