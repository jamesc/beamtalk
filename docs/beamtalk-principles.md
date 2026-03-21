# Beamtalk Design Principles

**Status:** Complete — Core design philosophy. Updated 2026-03-20 to reflect ADR 0004 (persistent workspaces), ADR 0036 (metaclass tower), ADR 0059 (supervision trees), MCP server, and multi-interface architecture.

Core philosophy guiding beamtalk design. These inform all implementation decisions.

For detailed syntax and features, see [beamtalk-language-features.md](beamtalk-language-features.md). For syntax design rationale, see [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md).

---

## 1. Interactive-First

Beamtalk is a **live environment**, not a batch compiler.

- The primary interface is an interactive workspace, not a command-line build tool
- Code is written, modified, and tested in the context of a running system
- Feedback is immediate - no compile-deploy-restart cycle
- The compiler exists to support the live environment, not the other way around

**Implication:** Workspace tooling is first-class, not an afterthought. The same live workspace serves REPL, VS Code, and AI agents (via MCP) over one WebSocket protocol.

---

## 2. Hot Reload is Core

Code changes apply to the **running system** without restart.

- Leverages BEAM's built-in hot code loading
- Modifying a class updates all future message sends immediately
- Stateful objects can migrate their state across code versions
- The system evolves continuously rather than being replaced

**Implication:** Generated code must support BEAM's code upgrade patterns.

---

## 3. Actors for Concurrency, Values for Data

Object is the root — two specializations map to natural BEAM concepts ([ADR 0067](ADR/0067-separate-state-field-keywords-by-class-kind.md)):

- **Value** (`Value subclass:` with `field:`) — immutable data, compiled to plain BEAM maps. No process, no mailbox.
- **Actor** (`Actor subclass:` with `state:`) — mutable state, compiled to BEAM processes (gen_server). Fault-isolated, message-passing concurrency.
- **Object** (`Object subclass:`) — the base. No `field:` or `state:` declarations. Often class-methods-only (FFI namespaces like `File`, `Json`), but can have instances with runtime-backed state — e.g. `Ets`, `AtomicCounter`, `FileHandle` have constructors and instance methods where state lives in ETS or Erlang handles rather than Beamtalk-managed maps.
- All respond to messages with the same syntax — the distinction is transparent to the sender
- Unknown messages trigger `doesNotUnderstand:` for metaprogramming

**Implication:** The data keyword (`field:` vs `state:`) signals which BEAM concept the class maps to. Actor instantiation creates processes; value types compile to direct operations; plain Object subclasses are method containers.

---

## 4. No Image, But Live

State persists in **running processes**, not a monolithic snapshot file.

- Unlike traditional Smalltalk, no single image file to save/load
- Workspaces are detached BEAM nodes that survive REPL disconnections ([ADR 0004](ADR/0004-persistent-workspace-management.md))
- Actors keep running between sessions; variable bindings are session-local
- Persistence via OTP patterns: ETS, DETS, Mnesia, or external stores
- System can be reconstructed from code + persisted state
- Multiple workspaces can run simultaneously for different projects

**Implication:** The workspace is the live system. Reconnecting picks up where you left off — actors, state, loaded modules all persist.

---

## 5. Code Lives in Files

Source code lives in the **filesystem**, not in a binary image.

- `.bt` files are plain text, one file per actor/class (like Pharo's Tonel format)
- Standard directory structure: `src/`, `test/`, `lib/`
- Git-friendly: meaningful diffs, branches, pull requests work naturally
- Tool-friendly: editors, grep, sed, AI agents can read/write code
- No opaque binary blobs — the filesystem IS the source of truth
- Live system and files stay in sync: `Workspace load:` bootstraps from files, `>>` live-patches individual methods, `Counter reload` recompiles from source

**Why this matters:**
- Traditional Smalltalk images are opaque to external tools
- Modern development requires version control, code review, CI/CD
- AI coding agents need to read and write files
- Team collaboration requires mergeable text files

**Implication:** Compiler reads from filesystem; tooling writes changes back to files.

---

## 6. Messages All The Way Down

**Everything** is a message send, following Smalltalk/Newspeak philosophy (with pragmatic BEAM-specific adaptations—see [Syntax Rationale](beamtalk-syntax-rationale.md)).

- No special syntax for "primitives" - even `+` is a message
- Control flow via messages to booleans and blocks
- Encapsulation enforced: only way to interact with an object is via messages

**Implication:** Even basic operations compile to message sends (optimized where possible).

---

## 7. Sync-by-Default Actors with Explicit Cast

Inter-actor `.` message sends are **synchronous** (gen_server:call) — they block the caller until the actor replies. Fire-and-forget sends use the explicit `!` operator (gen_server:cast).

- `.` sends block until the actor processes the message and returns a value
- `!` sends are non-blocking (cast) — use for intentional fire-and-forget
- Self-sends, primitive operations, and class methods are direct synchronous calls
- The default gen_server:call timeout is 5000ms; circular sync calls deadlock

**Implication:** Actor communication is synchronous by default, giving a natural call-and-return model while preserving full BEAM process isolation. Use `!` when you explicitly want non-blocking dispatch.

---

## 8. Reflection as Primitive

**Full runtime introspection** is a language feature, not a library.

- `Beamtalk` singleton: class registry, documentation, globals
- `Workspace` singleton: live actors, file loading, testing, namespace binding
- `CompiledMethod`: inspect source, selector, argument count at runtime
- Full metaclass tower (ADR 0036): `Counter class`, `Counter methods`, `Counter superclass`
- `///` doc comments flow to runtime — `Beamtalk help: Counter` returns formatted docs
- `respondsTo:` — ask any object if it understands a message before sending it

**Implication:** Compiler preserves metadata for full introspection. The system is self-describing — code is queryable, not just readable.

---

## 9. Seamless BEAM Ecosystem Integration

Beamtalk is a **first-class BEAM citizen**, not an isolated language.

Interop is not an afterthought — it's essential for adoption. The BEAM ecosystem has decades of battle-tested libraries (OTP, Phoenix, Ecto, Nx). Beamtalk must use them seamlessly.

**Design principles:**
- **Call any BEAM module** — Erlang, Elixir, Gleam, LFE libraries work directly
- **Be callable by any BEAM language** — Beamtalk actors are standard `gen_server` modules
- **Share supervision trees** — Mix Beamtalk actors with Erlang/Elixir processes under one supervisor
- **Use Hex.pm** — Standard package manager, no separate ecosystem
- **OTP-native** — Actors implement OTP behaviors; appear in Observer

**Why this matters:**
- Zero libraries means zero adoption
- Teams won't rewrite working Elixir/Erlang code
- Phoenix, Ecto, Nx are too valuable to abandon
- Gradual migration must be possible

**Implication:** Generated code follows BEAM conventions exactly. Foreign function interface is simple and low-friction. Build tools integrate with Mix/Rebar3.

---

## 10. Fault Tolerance via OTP

Embrace BEAM's "let it crash" philosophy — actors crash independently, the supervisor restarts them.

- Actors are automatically supervised by OTP on spawn
- No defensive programming — let the runtime handle failures
- Declarative supervision trees: `Supervisor subclass:` and `DynamicSupervisor subclass:` ([ADR 0059](ADR/0059-supervision-tree-syntax.md))
- `class children`, `class strategy`, `class supervisionPolicy` configure restart behaviour
- Nested supervisors compose into full OTP supervision hierarchies

**Implication:** Fault isolation is guaranteed. Supervision is native Beamtalk syntax — no Erlang FFI needed.

---

## 11. Live Patching is a Message Send

Hot code reload is just **message sends to class objects** — no special syntax.

- Redefine a class to update all future instances
- Replace individual methods: `Counter >> increment => self.value := self.value + 1`
- BEAM's built-in hot code loading provides the foundation
- State preservation across code versions is automatic
- Sub-200ms from edit to running in production

**Implication:** No dedicated syntax needed — live patching falls out naturally from "everything is a message send."

---

## 12. Compiler is the Language Service (TypeScript Approach)

The compiler is **architecturally designed for tooling**, not adapted for it later.

Following Anders Hejlsberg's TypeScript principle: the compiler IS the language service. They are the same code path, not "compiler + separate LSP wrapper."

**Architecture requirements:**
- **Error-recovering parser** - Never fail completely; always produce an AST, mark error nodes, continue
- **Incremental/partial parsing** - Handle incomplete code gracefully (mid-keystroke)
- **Query-based computation** - Don't recompute everything; cache and invalidate incrementally
- **Rich AST with trivia** - Preserve comments, whitespace for formatting
- **Source spans everywhere** - Every node knows its exact source location
- **Lazy evaluation** - Don't type-check or codegen until explicitly requested
- **<100ms response time** - IDE operations must be instant

**The compiler answers questions:**
- What completions are valid here?
- What's the type/signature of this expression?
- Where is this symbol defined? Where is it used?
- What refactorings are available?
- What are the errors in this file?

**Implication:** Compiler architecture is designed for responsiveness first, batch compilation second. Code generation is just one of many services the compiler provides.

---

## Non-Goals (For Now)

- **Full Smalltalk compatibility** - We're Smalltalk-**like**, not Smalltalk-**compatible**. We take inspiration but make pragmatic changes for BEAM and modern development.
- **Image snapshots** - BEAM's approach to persistence is different and better for distributed systems
- **Single-threaded semantics** - BEAM is concurrent by nature; embrace it
- **Mandatory static typing** — Dynamic by default with optional gradual typing ([ADR 0025](ADR/0025-gradual-typing-and-protocols.md)). `typed` classes require annotations; untyped classes work freely. Type mismatches are warnings, not errors — interactive workflows are never blocked

---

## References

- [Architecture](beamtalk-architecture.md) - Compiler, runtime, and live development flow
- [TypeScript Compiler Architecture](https://github.com/microsoft/TypeScript/wiki/Architectural-Overview) - Tooling-first design
- [Newspeak Language](https://newspeaklanguage.org/) - Module system, encapsulation, async actors
- [Dylan](https://opendylan.org/) - Sealing, conditions, method combinations
- [Pharo Tonel Format](https://github.com/pharo-vcs/tonel) - File-based Smalltalk source format
- [BEAM Book](https://blog.stenmans.org/theBeamBook/) - VM internals
- [Smalltalk-80](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) - Original semantics
- [Armstrong Thesis](https://erlang.org/download/armstrong_thesis_2003.pdf) - "Making reliable distributed systems"
