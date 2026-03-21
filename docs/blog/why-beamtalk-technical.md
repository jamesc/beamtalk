# Beamtalk: A Language Designed for Agentic Development

*The companion technical post to [Do I Sound Crazy?](do-i-sound-crazy.md) — the design choices behind a language built to kill dead text.*

---

In the [first post](do-i-sound-crazy.md), I argued that the file-based development loop is dead text — and that AI agents revealed a problem humans had been compensating for since the 1980s. This post is the technical deep dive: what Beamtalk actually is, why it makes the choices it does, and how the pieces fit together.

## Why the BEAM

The BEAM virtual machine — Erlang's runtime — is the best platform ever built for concurrent, fault-tolerant systems. WhatsApp, Discord, telecom infrastructure that can't go down. It handles always-on, multi-process, failure-resilient workloads better than anything else.

The BEAM was designed around lightweight processes that communicate by sending messages. Each process has its own state. No shared memory. If a process crashes, its supervisor restarts it.

Alan Kay described a remarkably similar architecture in the 1970s:

> "I thought of objects being like biological cells and/or individual computers on a network, only able to communicate with messages."

Joe Armstrong was clear that Erlang wasn't inspired by Kay's OOP — its lineage runs through process algebras and CSP. The parallel is convergent evolution, not derivation. But the convergence is striking: isolated processes with encapsulated state, communicating exclusively through messages, failing independently. Whether you call them objects or processes, the architecture is the same.

What's strange is that every language on the BEAM is functional. Erlang, Elixir, Gleam — immutable data, no encapsulated mutable state. The BEAM has processes with state that communicate via messages, but all its languages wrap that in functional abstractions — pattern-matched callbacks, tuple return values, explicit state threading — rather than making the process-with-state the primary programming model.

## The Obvious Objection: Why Not Just Add MCP to Elixir?

You could absolutely add an MCP server to Elixir's IEx. Elixir already has LiveBook for interactive development. It already has hot code reload. It already compiles fast. Wrapping those capabilities in MCP tools would give agents a live development experience today, with a mature ecosystem behind it.

Similarly, you could add MCP to GemStone/S — a production-grade Smalltalk with an object database and decades of battle-testing. The live-object model already exists there.

So why build a new language?

Because the development model — liveness, introspection, structured feedback — isn't a feature you bolt onto a language. It's a property of the language's design. When you design from scratch for this model, you make different choices than when you retrofit it.

**The class-kind system.** Beamtalk has three kinds of classes — Value (immutable data), Actor (mutable process), Object (pure behavior) — enforced by the compiler. `field:` means immutable. `state:` means mutable-in-a-process. Wrong combinations are compile errors. When an agent reads a class definition, the keyword itself tells it the mutability contract. Elixir doesn't express this — GenServer vs. plain module is a convention, not a type-level distinction. That's a deliberate design choice on Elixir's part, but it means agents have to infer what humans know from context.

**Syntax surface area.** An AI agent writing an Elixir GenServer has to get the tuple structure right (`{:reply, result, new_state}`), remember which callback to use (`handle_call` vs `handle_cast` vs `handle_info`), and thread state correctly through the return value. These are exactly the kind of structural details that language models hallucinate. In Beamtalk, `self.value := self.value + 1` inside an actor is nearly impossible to get wrong.

**Structured errors from the ground up.** Beamtalk's errors aren't strings — they're records with `kind`, `class`, `selector`, and `hint` fields. An agent reads structured data, not regex-parseable text. You could add structured errors to Elixir, but you'd be layering them on top of an ecosystem that already has its own well-established error conventions.

These aren't additive features. They're consequences of designing the language and runtime together, knowing that both humans and agents will be the developers.

That said — if someone adds a good MCP server to Elixir, that's a win for everyone. Agents interacting with live systems is a better model regardless of the language. Beamtalk's bet is that designing for it from scratch produces a qualitatively better result.

## What Beamtalk Actually Is

Beamtalk is a Smalltalk-inspired language that compiles to BEAM bytecode through Core Erlang. The compiler is written in Rust. Here's a value type — immutable data, no process overhead:

```beamtalk
Value subclass: Point
  field: x = 0
  field: y = 0

  distanceSquared => (self.x * self.x) + (self.y * self.y)

  translateBy: dx and: dy =>
    Point x: (self.x + dx) y: (self.y + dy)

  printString => "Point({self.x}, {self.y})"
```

Here's an actor — a BEAM process with encapsulated mutable state:

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  getValue => self.value
  reset => self.value := 0
```

`Counter spawn` creates a `gen_server` process. `counter increment` is a `gen_server:call` — synchronous by default, because send-message-get-answer is the natural model for both humans and agents. Async is opt-in with `!`:

```beamtalk
counter := Counter spawn
counter increment       // => 1 (synchronous, blocks until result)
counter increment       // => 2
counter increment!      // fire-and-forget (gen_server:cast)
```

## The Three Kinds of Things

Most language design arguments about mutability are binary: everything is immutable (Elixir, Haskell) or everything is mutable (Ruby, Python). Beamtalk makes a different choice. There are three kinds of classes, and the kind determines what you're allowed to do:

**Value** — immutable data. Uses `field:` for its data. Methods return new instances. No process, no overhead. Compiles to plain Erlang maps.

**Actor** — a BEAM process with mutable state. Uses `state:` for its data. Lives in a process. Communicates via messages. Supervised, restartable, hot-reloadable.

**Object** — pure behavior. No data at all. Class methods only. Your `Math`, your `System`, your utility namespaces.

The keywords `field:` and `state:` are not interchangeable. Use the wrong one, the compiler rejects it. The keyword carries the semantic contract — you know the mutability story by reading a single line, without consulting the class hierarchy. For an agent reading unfamiliar code, this is the difference between understanding the architecture instantly and having to trace through conventions.

The BEAM's concurrency safety comes from **process isolation**, not immutable data. Each process has its own heap. No shared memory. Data is copied between processes. Elixir argues for immutability everywhere — a legitimate design choice that prioritizes local reasoning. Beamtalk's position is different: immutability for data flowing between processes (Value types), natural mutation inside the process boundary where the VM guarantees single-threaded access (Actor state). Both the safety and the ergonomics.

## Fearless Experimentation

Here's the agent-specific argument for the BEAM that I think is underappreciated: **an agent can try things fearlessly because the supervision tree catches its mistakes.**

When an agent sends a message that crashes an actor, the supervisor restarts it. The agent gets a clean error, not a segfault. It can inspect what happened, adjust its approach, and try again — without restarting the entire system.

This is Erlang's "let it crash" philosophy, designed for telecom switches that couldn't go down. It turns out it's also perfect for AI agents that are going to make mistakes:

```beamtalk
// Agent spawns a supervised worker pool
DynamicSupervisor subclass: RoomPool
  class childClass => ChatRoom

RoomPool supervise

// Agent creates a room through the supervisor
room := RoomPool startChild
room addUser: "alice"    // Works fine
room addUser: nil        // Crashes — nil isn't a valid user

// Supervisor notices the crash. Agent can spawn a fresh room.
newRoom := RoomPool startChild
newRoom addUser: "alice"  // Back in business. Rest of the system never noticed.
```

You can approximate this with external process managers — supervisord, pm2 — but those are bolted on, not built in. On the BEAM, supervision is a language-level primitive. The agent's mistakes are contained, recovered from, and observable — and the rest of the system keeps running. No restart script required.

## The Agent Development Loop

Beamtalk exposes its runtime through the Model Context Protocol — MCP. An agent doesn't read files and guess. It talks to a living system.

The workspace runs as a persistent BEAM node. The agent connects via MCP and gets tools like:

| Tool | What it does |
|------|-------------|
| `evaluate` | Run an expression, get back the result |
| `load_file` | Load a `.bt` file into the running workspace |
| `test` | Run tests by class name, get structured results |
| `inspect` | Look at a live actor's current state |
| `list_actors` | See every actor running in the workspace |
| `list_classes` | See every class that's loaded |
| `docs` | Get documentation for any class or method |
| `search_examples` | Search a corpus of machine-verified code examples |
| `reload_module` | Hot-reload a class — running actors get new methods |

The development loop becomes:

1. Agent edits a `.bt` file
2. Agent calls `load_file` — the class is compiled and loaded in milliseconds
3. Agent calls `evaluate` to test an expression — immediate result
4. Something's wrong? Agent calls `inspect` to look at live actor state
5. Agent fixes the method, calls `reload_module` — running actors get the new code *without losing state*
6. Agent calls `test` — structured pass/fail, not text to parse

Compare this to the dead-text loop: edit, save, compile (wait), run tests (wait), read stdout, parse error messages, figure out what went wrong, repeat. Every step is a potential hallucination point. Every wait is wasted tokens. Every error message is unstructured text the model has to interpret.

## State That Survives

REPL bindings persist within a session, and actors persist across sessions.

```beamtalk
// Monday: agent spawns an actor and names it
server := ChatServer spawn
Workspace bind: server as: #chatServer

// Agent disconnects. Days pass.

// Wednesday: agent reconnects
server := Workspace actorsOf: ChatServer first
server rooms    // State is still there. Running the whole time.
```

For a human, this is a nice convenience. For an agent, it's transformative. The workspace is a persistent, living environment that the agent *inhabits* rather than reconstructs.

This is the Smalltalk image model — but without Smalltalk's historical baggage. The classic critique of Smalltalk images is real: image corruption, version control nightmares, "the source files aren't the ground truth" as a liability. Beamtalk sidesteps this. Source files *are* the ground truth — they're in git, they're diffable, they're reviewable. But the running workspace is the development environment. Liveness of an image, reproducibility of files.

## Structured Errors, Not Dead Text

When something goes wrong in Beamtalk, the error is structured data, not a string to parse:

```erlang
#beamtalk_error{
  kind    = does_not_understand,
  class   = 'Counter',
  selector = decrement,
  hint    = <<"Counter does not understand 'decrement'. Available: increment, getValue, reset">>
}
```

The `kind` field tells the agent what category of error. The `class` and `selector` tell it exactly where. The `hint` gives actionable guidance. Every error in the system follows this structure — type errors, method-not-found, argument mismatches, supervision failures. All structured, all machine-readable, all with hints.

This extends to error handling at the language level: **exceptions are for bugs, Results are for expected failures.** Reading a file might fail — use a `Result`. Calling a method that doesn't exist? That's a bug — let it crash, let the supervisor recover. The split is immediately legible: if a method returns `Result`, handle both cases. If it doesn't, trust it or let the supervisor deal with it.

## Machine-Verified Documentation

Every language ships documentation. Beamtalk ships a curated corpus of several hundred working code examples, embedded in the compiler binary, searchable via MCP, and **parsed by the compiler itself to guarantee syntactic validity**.

Before writing any code, the agent searches the corpus and finds verified, working examples of the pattern it needs. The corpus is generated from actual test files and learning materials. If an example doesn't parse, it doesn't ship.

Rust and Go have doc-tests that compile. But a searchable, concept-tagged example corpus exposed as an MCP tool — so an agent can find "how do I write a supervisor?" before writing a line of code — that's something new. Documentation that's guaranteed to compile *and* designed to be queried by machines.

## Supervision Without the Ceremony

OTP supervision trees are the BEAM's killer feature. No other mainstream platform has anything comparable.

```beamtalk
Supervisor subclass: WebApp
  class strategy    => #oneForAll
  class maxRestarts => 3
  class children    => #(DatabasePool HTTPRouter MetricsCollector)
```

That's a complete OTP supervisor. Start it with `WebApp supervise`. Done.

The equivalent Erlang or Elixir requires a supervisor module with `init/1` returning a supervision specification tuple, child spec maps for each child, and careful ordering of start phases. Same machinery underneath, dramatically different surface area — and for an agent, fewer structural details to get right on the first try.

## Hot Reload as Development Model

```beamtalk
>> counter := Counter spawn
=> Counter<0.156.0>

>> counter increment
=> 1

>> counter increment
=> 2

// The >> syntax redefines a method on an existing class.
// Running actors pick up the change on their next message dispatch.
>> Counter >> increment => self.value := self.value + 10

>> counter increment
=> 12
```

The actor kept its state (`value = 2`). The method changed. No restart, no recompile, no redeploy.

For agents, this is fundamental: the agent never loses context. It doesn't need to reconstruct state after a restart. The system is always live, always responsive, always remembering. The dead text never enters the loop.

## What's Next

The BEAM has transparent distribution — processes can communicate across nodes in a cluster as easily as on a single machine. Process identifiers work across the network. Supervisors can manage processes on remote nodes. This is infrastructure that Beamtalk inherits for free, and exposing it through MCP — so an agent can inspect and manage a distributed system through the same tools it uses locally — is on the roadmap.

The compiler, REPL, LSP, VS Code extension, and MCP server are working and used daily. The standard library has 82 classes. Types are gradual and opt-in, following the BEAM's Dialyzer tradition — add annotations when you're ready, enforce with `--warnings-as-errors` in CI. Parametric types and structural protocols are coming.

What doesn't exist yet: a package manager (next milestone), a production deployment story beyond "compile to BEAM and run it like Erlang," and the large ecosystem that comes with time.

Elixir has Hex with 15,000+ packages. Phoenix. Ecto. LiveView. A decade of production battle-testing. Beamtalk is at version 0.2.0.

Maybe the answer is adding MCP to Elixir's IEx. Maybe it's exposing GemStone/S through a protocol agents speak. Maybe it's Beamtalk. Maybe it's all three. The insight isn't "use my language" — it's that **live-object development environments, the kind Smalltalk pioneered and the industry mostly ignored, are about to matter again**. Agents need them. And once you build for agents, you realize humans needed them too.

Beamtalk is what happens when you take that seriously — and then hand the keyboard to an AI.

---

*Beamtalk is open source at [beamtalk.dev](https://www.beamtalk.dev). The compiler is written in Rust, the runtime in Erlang, and the standard library in Beamtalk itself. Contributions, skepticism, and genuinely hard questions are all welcome.*
