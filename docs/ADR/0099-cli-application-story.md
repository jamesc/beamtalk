# ADR 0099: CLI Application Story — Console, Arguments, Exit, and Packaging

## Status
Proposed (2026-06-25)

**Amends ADR 0061** (Program Entry Points and Run Lifecycle): relaxes its
unary-only entry-selector rule to also accept a single arity-1 keyword selector
(any name, e.g. `main:`) that receives the program's arguments. See "Amendment
to ADR 0061" below.

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
2. **Arguments** — a program can't see its argv. `beamtalk run ClassName
   selector` (ADR 0061) dispatches a *unary* class message and forwards nothing
   after it. ADR 0061 deliberately fixed the entry point at two positional
   tokens and *rejects* keyword selectors and trailing args (the validator in
   `crates/beamtalk-cli/src/commands/run.rs`). That was the right call against
   the old free-form `[run] entry = "App start: 'prod'"` footgun — but it also
   means there is no argv path at all. This ADR reopens that, narrowly.
3. **Exit with a status code** — a program needs to end with a status the shell
   can read. Scripts currently exit 0 when the entry method returns and have no
   way to signal failure. The subtlety (see Constraints) is that "exit" has
   *two* meanings: halt the whole VM (right for a standalone app that owns the
   node) versus end *this* program/job with a status (right for a script
   dispatched into a shared, already-running workspace, where halting the VM
   would kill the service and every other session).
4. **Packaging** — there is no way to produce a distributable artifact. You
   need a Beamtalk project checkout and `beamtalk run` to execute anything.

Most of the Erlang primitives behind this are one-liners (`io:get_line/1`,
`io:put_chars/2`, `io:format/2`, `init:get_plain_arguments/0`,
`erlang:halt/1`). Two are *not* trivial and the design must treat them as such:
(a) portable **tty detection** — BEAM has no simple `isatty`, only heuristics
(`io:getopts`/`io:columns`, `prim_tty`); and (b) **job-level exit** — ending
one program without halting the node, which needs a cooperating run harness,
not a single FFI call. **The design work is the Beamtalk class API, the entry
argument contract, the two-tier exit model, and packaging** — not the bulk of
the FFI.

### Constraints

- **Entry-point contract (ADR 0061, amended here).** Today `beamtalk run` takes
  exactly two positional tokens — `ClassName` + a *unary* selector — and the
  validator rejects keyword selectors and extra args. This ADR amends that to
  also allow a *single arity-1 keyword selector* (`main:`) carrying argv as one
  list. It deliberately does **not** reopen general keyword-message dispatch on
  the CLI (see "Amendment" and Alternatives for why that part stays closed).
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
  class main: args :: List(String) -> Nil =>
    name := args at: 1 ifAbsent: ["World"]
    Console printLine: "Hello, " ++ name ++ "!"
```

```text
$ beamtalk run Greeter main: Alice
Hello, Alice!
$ beamtalk run Greeter main:
Hello, World!
```

…and, packaged:

```text
$ beamtalk build --escript --entry "Greeter main:" -o greeter
$ ./greeter Alice
Hello, Alice!
```

## Decision

Add two stdlib classes (`Console`, `Program`), one entry-argument contract, two
new `System` methods, and one packaging command. Each surface is scoped to the
one concern it owns; none overload an existing class whose meaning would blur.
The key shape: `Console` owns the process's *stdio*, the **entry contract**
delivers argv as a normal `main:` parameter, `Program` owns *this running
program* (its name and its *job-level* exit), and `System` owns *the machine/VM*
(including the nuclear whole-node `halt`).

### 1. `Console` — the current process's stdio

A new `sealed typed Object subclass: Console` singleton (class-side, no
instances — same shape as `System` and `OS`). It wraps the BEAM standard I/O
of the *running* program.

```beamtalk
Console printLine: aValue            // stdout + newline   (aValue :: Printable)
Console print: aValue                // stdout, no newline  (aValue :: Printable)
Console errorLine: aValue            // stderr + newline    (aValue :: Printable)
Console error: aValue                // stderr, no newline  (aValue :: Printable)
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
  stripped, or `nil` at end of input (`io:get_line/1` → `eof`). On an I/O error
  (`io:get_line/1` → `{error, Reason}`) it raises a `#beamtalk_error{}` rather
  than returning `nil`, so EOF and a genuine read failure stay distinguishable.
  `readLine:` writes a prompt to stdout first (no newline) then reads, with the
  same return contract. A **closed or absent stdin** (e.g. a detached node,
  where `io:get_line/1` may yield `eof` *or* a closed-fd error such as `{error,
  terminated}`) is treated as end-of-input → `nil`; only a *genuine mid-stream*
  I/O failure raises `#beamtalk_error{}`. Phase 1 owns the precise closed-stdin
  allowlist and the per-atom rationale (see Open Questions). This keeps the
  "returns `nil` under the REPL" caveat below consistent with the error
  contract.
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

### 2. Program arguments — the arity-1 entry contract (amends ADR 0061)

**Args arrive as a normal method parameter, not a global.** The entry selector
may be either:

- **unary** (`run`) — the program takes no arguments; or
- **a single arity-1 keyword selector** (`main:`) — the program receives all
  of its CLI arguments as one `List(String)`.

```beamtalk
class main: args :: List(String) -> Nil =>
    // args is an ordinary value: thread it, pattern-match it, pass it to a parser
    args isEmpty ifTrue: [Console errorLine: "usage: greeter <name>". Program exit: 2]
    Console printLine: "Hello, " ++ (args at: 1) ++ "!"
```

On the CLI, everything after the entry selector is the program's argv:

```text
beamtalk run Greeter main: Alice Bob       // args = #("Alice" "Bob")
beamtalk run Greeter main: -- --verbose    // args = #("--verbose")
```

The `--` separator is **optional** — needed only to pass *flag-shaped* tokens
(`--verbose`) that the `beamtalk` CLI's own argument parser would otherwise try
to interpret. Bare-word args need no separator. (This is exactly the
`cargo run alice` / `cargo run -- --flag` convention.) ADR 0061 already caps the
CLI at two positionals — class + selector — so there are no further CLI slots
for trailing bare words to collide with.

**Why a parameter, not a global argv reader.** Delivering argv as the `main:`
parameter makes it *ordinary data*: threaded explicitly, passed to an arg-parser
at the top of the program, unit-testable by calling `Greeter main: #("x")`
directly. A global reader would instead require a per-session argv store on the
node (so a script dispatched into a shared workspace reads *its* argv, not the
node's) — reintroducing cross-process reads, a backing-store choice, and a
cleanup path for crashed sessions. The parameter model deletes all of that. It
also mirrors the exit design symmetrically: **args flow *down* as parameters;
status flows *up* as a return value or `Program exit:`** — no ambient argv
singleton in the middle. A program that declares a unary entry (`run`) has
simply opted out of args.

**How each context delivers argv** — in every case it is a one-shot dispatch
argument, never stored:

- **Run-mode** (new node): the CLI passes the post-selector tokens; the run
  harness dispatches `Class>>main: List`.
- **Connected mode** (ADR 0061 §3 — dispatched into a live node): the CLI
  carries the tokens *inside the REPL-protocol eval request*; the session
  evaluator dispatches `Class>>main: List` with them. No node-global state, so
  concurrent sessions never see each other's argv.
- **Escript**: the generated `main/1` dispatches `Class>>main:` with its `Args`.

**`Program name`.** A convenience for usage/`--help` text (not load-bearing):
under a packaged escript it is `escript:script_name/0` (the invoked filename);
under `beamtalk run` there is no argv[0], so it is the literal `"beamtalk"`.

### 3. Exit — two tiers (`Program exit:` vs `System halt:`)

Exit has two legitimate meanings depending on who owns the node. They get two
methods, named for what they do — a different *subject* (the program vs the
machine) **and** a different *verb* (`exit` vs `halt`), so the 100×-blast-radius
difference is legible at the call site.

**`Program exit` / `exit:` — the job-level, safe default.** `Program` is the
running program/invocation; ending it is its responsibility. It is the object
that exists in *every* execution context (run-mode, escript, connected) — unlike
`Session` (ADR 0081), which is `nil` in compiled program code. `Program exit`
ends *this program* with a status **without halting the VM**:

```beamtalk
Program exit                 // end this program with status 0
Program exit: 2              // end this program with status 2
```

- In **run-mode** (the program owns the node), it unwinds the entry dispatch,
  tears down the run-mode workspace this invocation started, and the node exits
  with the status — a graceful shutdown, not a raw `halt`.
- In **connected mode** (entry evaluated inside a live shared workspace), it
  ends *this session's* job — internally it terminates the current `Session`
  (ADR 0081), which is precisely "this program" in a live workspace — stopping
  only that session's process group and reporting the status back over the REPL
  protocol to the connecting CLI, which sets the shell exit code. **The shared
  node and every other session keep running.**
- In an **escript**, it ends `main/1` with the status.

Mechanically, `exit:` raises a tagged `script_exit` signal that the run harness
(run-mode dispatcher, escript `main/1`, or connected-session evaluator) catches
and translates to the context-appropriate teardown.

*Process-boundary caveat.* Because the signal is a `throw`, it only unwinds to
the harness when raised **in the harness-dispatched process** — the entry method
and any synchronous call chain it drives. A `Program exit:` called from a
*spawned Actor* (each Actor is its own OTP process) is not caught by the
harness: the Actor exits with `{nocatch, {script_exit, N}}`, and — if it is
under a supervisor (the normal OTP pattern) — the supervisor simply *restarts*
it rather than propagating the status. Repeated restarts can even exhaust the
supervisor's restart intensity and cascade a shutdown up the tree — the opposite
of a clean exit. The contract is therefore: **`Program exit:` is effective only
from the entry method's own process.** An Actor that wants to end the program
returns a status to the entry method (a normal message/reply) and lets *it* call
`exit:`. A cross-process "exit from anywhere" primitive is deliberately out of
scope for v1; the ADR states the boundary rather than hiding it. (A *linked*,
non-supervised Actor is a related case: the link propagates the exit to the
harness, so the harness must `trap_exit` or document linked-process callers as
unsupported — see Open Questions.)

**`System halt` / `halt:` — whole-VM halt (the nuclear option).** Where
`Program exit:` ends the *program*, `System halt:` halts the *whole node*. It
lives on `System` because halting the VM is machine-level behaviour, next to
`System`'s existing process/platform info (`pid`, env, architecture); it is
named `halt` (matching `erlang:halt`), so the nuclear option is lexically
distinct from the safe `Program exit:`.

```beamtalk
System halt          // erlang:halt(0)
System halt: 1       // erlang:halt(1)
```

- `halt:` requires an `Integer`: a non-Integer raises `#type_error`
  (consistent with `System setEnv:`), while a value outside the POSIX range
  0–255 raises a `#beamtalk_error{}` — a wrong *value*, not a wrong *type*.
- **Semantics:** `erlang:halt/1` is an *immediate* VM halt — it does **not**
  run OTP shutdown or `terminate/2` callbacks. (`halt/1` flushes standard I/O
  through the BEAM I/O server before terminating, so no explicit `Console`
  pre-flush is needed unless Beamtalk later adds a user-space output buffer
  above the `io` layer.) Correct for a standalone app that owns the node and
  wants to stop *now*.
- **⚠ Shared-workspace warning.** `System halt:` halts the entire node. Called
  inside a live, shared workspace (a connected `beamtalk run`, or in the REPL),
  it kills the service and every other connected session. The runtime therefore
  **refuses** `System halt:` outside a node-owning context and raises a
  `#beamtalk_error{}` directing the caller to `Program exit:` instead. Graceful
  shutdown of a service is a separate concern (`init:stop()`, supervision
  teardown) owned by the service-lifecycle / release story (ADR 0061 "Future:
  Release Mode").
- **How "owns the node" is detected.** The decision must not depend on a live
  metadata process that might be starting up or unreachable. Each entry harness
  sets a **`beamtalk_runtime` application env** (`node_owning => true`) *at boot*
  — run-mode startup and the escript `main/1` set it; the persistent workspace
  path (REPL / connected) does not. `beamtalk_system:halt/1` reads that env:
  `true` → `erlang:halt`, absent/`false` → the refusal error. An application env
  is process-independent, set synchronously before any user code dispatches, and
  trivially testable — preferred over inferring mode from the
  `beamtalk_workspace_meta` `repl` flag (a process that may not be reachable when
  `halt` is called) or from workspace registration on disk.

**Implicit exit code (recommended path).** Most programs never call `exit:` or
`halt:`. When the entry method returns normally, the run exits `0`. If it raises
an uncaught `#beamtalk_error{}`, the harness prints the error to stderr and
exits non-zero (proposed: `1`). `Program exit:` is for *early* termination;
`System halt:` for the deliberate whole-node halt.

### 4. `beamtalk build --escript` — single-file packaging

`beamtalk build --escript --entry "ClassName selector" -o <name>` produces a
self-contained executable escript.

```text
$ beamtalk build --escript --entry "Greeter main:" -o greeter
$ ./greeter Alice
Hello, Alice!
```

- **Archive contents.** The escript embeds a zip archive (standard escript
  format) containing: all project `bt@*.beam`, the `beamtalk_runtime` and
  `beamtalk_stdlib` application beams, and a generated bootstrap module
  exposing `main/1`.
- **Entry resolution.** Escript requires a `main/1`. We generate a tiny wrapper
  whose `main(Args)`:
  1. starts `beamtalk_runtime` (with `node_owning => true`),
  2. starts a **run-mode workspace** (`repl = false` — ADR 0061 §5: no REPL
     server, not registered, no idle monitor) so all classes bootstrap in topo
     order,
  3. dispatches `ClassName>>main: Args` (or the unary `ClassName>>selector`),
  4. catches a `script_exit` from `Program exit:` (→ its status) and maps a
     normal return → 0 / any uncaught error → non-zero (per §3).
- **Why run-mode reuse.** ADR 0061 already defined the exact
  "bootstrap-then-dispatch-then-exit" lifecycle a script needs. The escript
  wrapper is that lifecycle with a `main/1` shell — no new startup path to
  maintain.
- **`--entry` default.** If the project manifest declares a single obvious
  entry (future `[scripts]` table, ADR 0061), `--entry` may be omitted.

### Amendment to ADR 0061

ADR 0061 fixed the entry point at two positional tokens (`ClassName` + a unary
selector) and the validator rejects keyword selectors and trailing args, to kill
the old `[run] entry = "App start: 'prod'"` free-form-string footgun. This ADR
**narrows that restriction rather than lifting it**: it adds exactly one new
entry shape — a single arity-1 keyword selector (`main:`) receiving all argv as
one `List(String)`. It does **not** restore general keyword-message dispatch on
the CLI (`Robot move: 3 to: 5`), which is the genuinely-ambiguous part ADR 0061
was right to close (see Alternatives). Implementation: relax
`validate_class_and_selector` in `run.rs` to accept a lone trailing `:`
(arity-1 keyword) and treat post-selector tokens as the argument list.

The contract accepts **any** arity-1 keyword selector, not the literal string
`main:` — `beamtalk run Migration runWith: db.sql` is equally valid. `main:` is
the recommended *convention* (matching `main(argv)` elsewhere), not an enforced
name; implementors must not hard-code a `main:` check in the validator.

## Prior Art

**Smalltalk (Pharo / Squeak).** `Transcript` is the workspace log — exactly our
`TranscriptStream`. Pharo grew a separate `Stdio`/`FileStream stdout` for
headless command-line use; our `Console`-vs-`Transcript` split is that lesson
learned up front. Pharo's command-line entry is `CommandLineHandler` subclasses
with an `activate` hook reading `self commandLine arguments` — argv-as-a-list,
like ours, though they pull from a handler rather than a method parameter. Pharo
exits via `Smalltalk quit:` (image-level), our `System halt:`.

**Erlang.** `io:format/2`, `io:get_line/1`, `erlang:halt/1`, and the escript
format (`main/1`, embedded archive) are the substrate we wrap directly. Escript
is Erlang's canonical single-file CLI packaging and its `main(Args)` *passes
argv as a parameter* — precisely the model this ADR adopts for `main:`. `System
halt:` is the thin wrapper over `erlang:halt/1`, named to match.

**Elixir.** `IO.puts/2`, `IO.gets/1`; `System.argv/0`, `System.halt/1`;
`mix escript.build`. Elixir reads argv from the global `System.argv` and exits
via `System.halt`. We diverge on both: argv arrives as the `main:` parameter
(no global), and exit is two-tier (`Program exit:` job vs `System halt:` VM),
because Elixir has no notion of a script dispatched into a long-lived *shared*
node — Beamtalk does (ADR 0061's connect-and-eval path), and only the *VM* tier
maps to `System.halt`.

**Go / Rust / Java.** `func main()` reading `os.Args`; `fn main()` with
`std::env::args`; `static void main(String[] args)`. Java's
`main(String[] args)` is the closest match to our `main: List(String)` — argv
delivered as a parameter, all strings, parsing left to the program. Rust/Go pull
from a global; we prefer the parameter for testability and explicit data flow.

**What Beamtalk adopts:** Erlang escript packaging verbatim and its
parameter-passed `main(Args)` (Java-style `main:`); the Console/Transcript
split (the Pharo lesson). **What it rejects:** general keyword-message dispatch
on the CLI (the parsing-ambiguity trap); a global argv reader (the per-session
store it would require); built-in declarative arg parsing (clap) — left to a
library built on the `List(String)`.

## User Impact

**Newcomer.** Writes the canonical first program — `class main: args` — exactly
as in every other language. `beamtalk run Greeter main: Alice` works; no `--`
ceremony for ordinary args; `beamtalk build --escript` yields a file to hand to
a friend.

**Smalltalk developer.** The `Console`/`Transcript` distinction needs one
sentence but maps to Pharo's `Stdio` vs `Transcript`. `main:` echoes Pharo's
`CommandLineHandler … arguments`. `System halt:` mirrors `Smalltalk quit:`;
`Program exit:` is the gentler "end this program" wanted when evaluating against
a live image.

**Erlang/BEAM developer.** Recognises every primitive — `io:format`, `halt`,
escript `main(Args)`. `main:`-as-parameter *is* the escript model. The
`Program exit:` / `System halt:` split maps to "end the job vs `halt` the node,"
and the process-boundary/supervisor caveat is the behaviour they'd predict.

**Operator.** POSIX-correct exit codes (0 on success, non-zero on error) compose
in pipelines and CI `set -e`. A script run against a live service exits its job
cleanly without taking the service down. Escript output is a single deployable
file.

## Steelman Analysis

**"Put stdout on `Transcript`, don't add `Console`."** *(Smalltalk cohort.)*
One less class; `Transcript show:` already exists. Counter: `Transcript` is
pub/sub and per-workspace; wiring stdin/tty/stderr into it would overload a
class whose whole point is the *workspace* log and would misbehave under the
REPL (output goes to subscribers, not a terminal). Genuinely different surfaces.

**"Keep ADR 0061 unary-only; pass args via a `--` tail + a global reader."**
*(Conservative cohort.)* Don't amend an accepted ADR; keep the entry contract
frozen and expose argv through a global `arguments` reader. Counter: a global
reader forces a per-session argv store on the node (so a script dispatched into
a shared workspace reads *its* argv) — bringing back cross-process reads, a
backing-store choice, and crashed-session cleanup. The `main:` parameter deletes
all of that, and the amendment is *narrow* (one new arity-1 shape), not a
reopening of the free-form-entry footgun ADR 0061 actually killed. We judged the
ergonomic + simplicity win worth the amendment.

**"Then go all the way — general keyword-message entry (`App start: 'prod'`)."**
*(Expressiveness cohort.)* If we're amending anyway, support full keyword
sends. Counter: that re-imports every ambiguity ADR 0061 closed — selector
arity vs. token count, String-vs-Integer coercion of bare tokens, quoting for
blocks/symbols/arrays, flag/arg/selector collision. The arity-1 `main:` shape
gets the `main(argv)` ergonomics while keeping that can closed; a program that
wants structured args parses the `List(String)` itself (or with a library).

**"One exit, not two — or put exit on `Session`/`System`."** *(Simplicity
cohort.)* Two methods on two classes is more to learn. Counter: a single
`halt`-backed exit is dangerous in the connect-and-eval path (kills a shared
service); a single *context-aware* exit hides the 100× difference behind one
name. Two named tiers — `Program exit:` (safe) vs `System halt:` (nuclear) —
make the consequence legible by *both* subject and verb. As for the home of the
safe tier: `Session` is `nil` in compiled programs (run-mode/escript), so it
cannot host the standalone case; `System exit:` reads as "exit the system" =
halt, muddying the very distinction. `Program` exists in every context and names
the subject truthfully. (See Alternatives for the full comparison.)

**"Ship OTP releases, not escripts."** *(Operator cohort.)* Releases are the
production artifact. Counter: heavyweight, and ADR 0061 already earmarks
`beamtalk release` as a future ADR. Escript is the right *first* packaging —
single file, zero config — and doesn't preclude releases later.

**Tension points.** Two places reasonable people will disagree: (1) the
`Console`/`Transcript` boundary — mitigated documentation-heavily; and (2)
amending ADR 0061 at all — mitigated by keeping the amendment to a single,
unambiguous entry shape and refusing the general-keyword temptation.

## Alternatives Considered

### Args via a `--` tail + a global argv reader (the earlier draft)

Keep ADR 0061 unary-only; require `beamtalk run Greeter run -- a b` and read
argv from a global reader (a `CommandLine arguments`-style class) backed by a
per-session seed store (ETS keyed by session, seeded from `-extra` / `main/1` /
the REPL eval request). Rejected: the global forces a node-side per-session
store with cross-process reads, a gen_server-vs-ETS choice, and a `DOWN`-monitor
cleanup path to avoid leaking entries when a session crashes — all to avoid
amending ADR 0061. The `main:` parameter is strictly simpler and removes that
whole mechanism; the mandatory `--` was also awkward for the common bare-word
case.

### Home for the job-level exit: `CommandLine` vs `Session` vs `System` vs `Program`

The earlier draft put the safe exit (and a thin `programName`) on a
`CommandLine` class that *also* owned argv. Once argv moved to the `main:`
parameter, that class was down to `exit`/`exit:`/`programName` — and its name
implied the arguments it no longer held. Options weighed:

- **`Session exit:` (ADR 0081).** Tempting — in connected mode the job *is* a
  session. But `Session current` is **`nil` in compiled program code**
  (run-mode, escript), which is the headline standalone-CLI case; it would be
  unavailable exactly where it is needed. So `Session` becomes the connected-mode
  *implementation* behind `Program exit:`, not the user-facing surface.
- **Fold onto `System` (`System exit:` job + `System halt:` VM).** Fewest
  classes, but `System exit:` reads as "exit the system" = halt, collapsing the
  distinction the two tiers exist to draw.
- **`Program exit:` (chosen).** A truthful name for an exit-plus-identity object
  that exists in every context; pairs with `System halt:` so the two tiers
  differ by subject *and* verb. `CommandLine` is dropped.

### General keyword-message entry on the CLI

```text
beamtalk run Robot move: 3 to: 5      # arity-2 keyword send on argv
```

Most expressive. Rejected — it reopens exactly what ADR 0061 closed:
- **Selector arity vs. token count** — the CLI must parse keyword-message
  grammar and know the method's arity to find where the selector ends.
- **Type coercion** — shell tokens are all strings; is `3` an `Integer` or
  `"3"`? The old config form worked only because the compiler parsed it as
  Beamtalk source (quotes → String). On bare argv there are no quotes.
- **Quoting / shell-hostile syntax** — blocks, symbols, arrays, colons.
- **Flag/arg/selector collision.**
The arity-1 `main:` shape sidesteps all of these (one token, one List, all
strings).

### `Console` as a real `Stream` object (ADR 0021)

`Console stdout` returns a writable `Stream` for piping. Partially deferred: the
class-side convenience API covers the 90% case now; exposing `stdin`/`stdout`
as `Stream`s is a natural follow-up, not a blocker.

### Declarative arg parsing as a primitive (clap-style)

A `flag:`/`option:` parsing API baked into the language. Rejected: parsing
policy (subcommands, validation, help) is library territory; the primitive is
the raw `List(String)`. A clap-equivalent is a package built on `main:`'s
argument.

## Consequences

### Positive
- Standalone CLI applications become possible — a capability the language lacks
  entirely today.
- Argv as a `main:` parameter is ordinary, testable data — no node-side
  per-session store, no cross-process reads, no cleanup path.
- POSIX-correct exit codes make scripts first-class shell/CI citizens; the
  two-tier exit makes them safe to run *against a live shared workspace*.
- `Program exit:` vs `System halt:` makes the blast-radius difference legible by
  both subject and verb; `Program` exists in every execution context.
- Single-file escript artifacts are distributable without a project checkout.
- Clear `Console` vs `Transcript` separation prevents the Pharo conflation.
- Reuses ADR 0061's run-mode lifecycle for escript boot — no new startup path.

### Negative
- Amends an accepted ADR (0061). The amendment is narrow, but it is a change to
  a contract other docs/code reference, and `run.rs`'s validator must be
  relaxed carefully (accept a lone arity-1 `:`, still reject multi-keyword).
- Two new classes (`Console`, `Program`); `Console` overlaps superficially with
  `Transcript` and needs documentation to disambiguate.
- Two termination methods to teach (`Program exit:` vs `System halt:`); the
  distinction must be legible or users reach for the wrong one.
- `Program exit:`'s process boundary (no effect from spawned/supervised Actors)
  is a real footgun that documentation must surface.
- Escript packaging adds a build mode (archive assembly, `main/1` generation,
  Windows launcher) to maintain.
- `Console readLine` returning `nil` under the REPL is a sharp edge.

### Neutral
- `System` grows two methods (`halt`, `halt:`); `Console` and `Program` are
  net-new; `Object >> show:`/`showCr:` and `Transcript` are unchanged.
- `Program` is intentionally small (`name`, `exit`, `exit:`) — argv lives on the
  `main:` parameter, not on the class.
- `Console isInteractive` (tty detection) is explicitly deferred out of v1.
- Full OTP releases remain a separate future ADR.

## Implementation

Rough phases; each is independently shippable and testable.

### Phase 1 — `Console` (stdio)
- `runtime/.../beamtalk_console.erl`: `printLine:`/`print:`/`errorLine:`/
  `error:`/`flush`/`readLine`/`readLine:` over `standard_io` / `standard_error`
  (`io:put_chars/2`, `io:get_line/1`). `readLine` maps `eof` to `nil`; the
  closed-stdin allowlist (definitely `{error, terminated}`; evaluate `{error,
  ebadf}` — only the never-opened case, *not* a mid-stream close — and confirm
  whether `{error, enotsup}` ever arises for a readable stdin before including
  it, since ordinary piped/redirected stdin returns data then `eof`; each with
  documented rationale) also maps to
  `nil`, while any *other* `{error, Reason}` raises `#beamtalk_error{}`. Settle
  the write-side error contract too (see Open Questions). `isInteractive` is
  **deferred** (no portable tty check yet — see §1).
- `stdlib/src/Console.bt`: class-side methods delegating via `(Erlang
  beamtalk_console) …`, `Printable`-typed print params, doc comments incl. the
  REPL caveat. `print:`/`printLine:` render via `displayString` (ADR 0094,
  implemented).
- Tests: bootstrap/BUnit for the non-interactive methods; an `examples/`-style
  smoke script for interactive read under `beamtalk run`.

### Phase 2 — `main:` entry contract + `Program name`
- `crates/beamtalk-cli/src/commands/run.rs`: relax `validate_class_and_selector`
  to accept a single arity-1 keyword selector (one trailing `:`, still reject
  multi-keyword and arbitrary args); treat post-selector tokens as the argument
  list (clap `trailing_var_arg`/`allow_hyphen_values`, `--` optional and only
  needed to shield flag-shaped tokens). Dispatch `Class>>main: List` (or unary).
- Connected mode: carry the argument list inside the REPL-protocol eval request;
  the session evaluator dispatches `Class>>main: List`. One-shot, no node-global
  store.
- `beamtalk_program.erl` + `stdlib/src/Program.bt`: `name`
  (`escript:script_name/0` under escript, else `"beamtalk"`). `exit`/`exit:`
  land in Phase 3.

### Phase 3 — two-tier exit + implicit exit code
- **Job-level (`Program exit`/`exit:`):** raise a tagged `script_exit` carrying
  the status; catch in all three harnesses (run-mode dispatcher, escript
  `main/1`, connected-session evaluator — the last terminates the current
  `Session` (ADR 0081), reports status over the REPL protocol, stops the
  session's process group, leaves the node up). `stdlib/src/Program.bt`: `class
  sealed exit -> Nil`, `class sealed exit: code :: Integer -> Nil` (do not
  return). Document the entry-process-only boundary and the supervised-Actor
  restart caveat (§3).
  - **Prerequisite (connected-mode path only).** This path depends on a
    **session-termination hook** — ending the current session's job with a
    status. ADR 0081 (First-Class Session Object) is *Accepted* and `Session`
    exists, but it is read-mostly (binding layers) and exposes **no** termination
    operation today; that hook must land before the connected-mode `Program
    exit:` can ship. The run-mode and escript paths have no such dependency, so
    Phase 3 must not be marked complete on those alone — the connected-mode path
    is gated on the session-termination capability (tracked: file a follow-up
    issue before closing Phase 3; see Open Questions).
- **VM-level (`System halt`/`halt:`):** `beamtalk_system:halt/0,1` →
  `erlang:halt/0,1` (no explicit pre-flush; `halt` flushes the `io` layer);
  Integer-type validation (`#type_error`) + range 0–255 (`#beamtalk_error{}`).
  Detect node-ownership via
  the `beamtalk_runtime` `node_owning` application env (set at boot by run-mode
  startup and escript `main/1`; absent on the persistent/connected path);
  absent → **refuse with `#beamtalk_error{}`** pointing at `Program exit:`.
  `stdlib/src/System.bt`: `class sealed halt -> Nil`, `class sealed halt: code
  :: Integer -> Nil`.
- **Implicit code:** harness maps normal return → 0, uncaught
  `#beamtalk_error{}` → stderr + non-zero.

### Phase 4 — `beamtalk build --escript`
- `crates/beamtalk-cli/src/commands/build.rs` (+ a new `escript` assembly
  module): collect `bt@*.beam` + runtime/stdlib beams, generate the `main/1`
  bootstrap module (run-mode workspace + `node_owning` + `Class>>main: Args`
  dispatch + exit mapping), emit an escript archive; set the executable bit
  (Unix) / `.escript` + launcher (Windows, per ADR 0027).
- `--entry "ClassName selector"` parsing; default from manifest if unambiguous.

### Surface parity
Per CLAUDE.md, update `docs/development/surface-parity.md`: `Console` and
`Program` are runtime stdlib (available everywhere a workspace runs);
`beamtalk build --escript` and the `beamtalk run … <args>` entry contract are
**CLI-specific** and should be labelled as such.

## Open Questions (for implementation)

These are deferred to the implementation phases — not design blockers. A
*Proposed* ADR records them rather than guessing the answers now; each is
resolved (and tested) when its phase is built.

- **`Program exit:` from a *linked* (non-supervised) Actor.** A link propagates
  the `{nocatch, {script_exit, N}}` exit to the harness; unless the harness runs
  `process_flag(trap_exit, true)`, it is killed by the signal instead of mapping
  it to status N. Phase 3 must trap exits in the harness or document
  linked-process callers as unsupported.
- **`Console` write-side error contract.** `print:` / `printLine:` / `errorLine:`
  / `error:` / `flush` have no specified behaviour when the underlying stream
  fails (e.g. stdout closed mid-run). Phase 1 must choose drop-silently vs raise
  `#beamtalk_error{}`, consistent with the `readLine` contract.
- **Precise closed-stdin allowlist.** `{error, terminated}` is in; Phase 1 must
  decide `{error, ebadf}` (only the never-opened case, distinguished from a
  mid-stream close) and whether `{error, enotsup}` ever arises for a *readable*
  stdin at all (ordinary piped/redirected stdin returns data then `eof`), each
  with documented rationale.
- **`Program name` per-context value.** Run-mode and escript both set
  `node_owning => true`, so a second signal is needed to return `"beamtalk"` vs
  `escript:script_name/0` — a dedicated `program_name` `beamtalk_runtime` app env
  seeded at boot is the likely mechanism (Phase 2).
- **Connected-mode session-termination hook (Phase 3).** The connected-mode
  `Program exit:` needs a `Session` termination operation that ADR 0081 does not
  expose today; file a tracking follow-up before that path is implemented.

## Migration Path

Net-new capability — nothing to migrate. The `main:` entry shape, the new
`Console`/`Program` classes, and the two `System` methods (`halt`/`halt:`) are
additive; existing `beamtalk run ClassName selector` (unary, no args)
invocations behave identically. ADR 0061's Status now carries a reciprocal
"Amended by ADR 0099" note pointing at this ADR's entry-point change.

## References
- Related issues: BT-1687 (this ADR)
- Related ADRs:
  - ADR 0061 — Program Entry Points and Run Lifecycle (**amended here**: entry
    selector may be unary or a single arity-1 keyword `main:`)
  - ADR 0081 — First-Class Session Object (`Session` is the connected-mode
    implementation behind `Program exit:`; `nil` in compiled programs)
  - ADR 0051 — Subprocess Execution (child-process I/O; contrast with Console)
  - ADR 0021 — Stream and I/O Design (future `Console` Stream surface)
  - ADR 0010 — Global Objects and Singleton Dispatch (`Transcript` global)
  - ADR 0094 — Object String Representation Protocols (`displayString`)
  - ADR 0027 — Cross-Platform Support (escript on Windows)
- Existing stdlib: `stdlib/src/System.bt`, `File.bt`, `Subprocess.bt`,
  `OS.bt`, `TranscriptStream.bt`, `Session.bt`, `Object.bt` (`show:`/`showCr:`)
- Erlang primitives: `io:put_chars/2`, `io:get_line/1`, `io:format/2`,
  `erlang:halt/1`, escript `main/1`
- CLI: `crates/beamtalk-cli/src/commands/run.rs` (`validate_class_and_selector`),
  `build.rs`
- Follow-up work: `Console` Stream surface (ADR 0021), declarative arg-parsing
  library, `[scripts]` manifest table (ADR 0061), `beamtalk release` (future ADR)
