# Beamtalk Design Principles

**Status:** Complete - Core design philosophy

Core philosophy guiding beamtalk design. These inform all implementation decisions.

For detailed syntax and features, see [beamtalk-language-features.md](beamtalk-language-features.md). For syntax design rationale, see [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md).

---

## 1. Interactive-First

Beamtalk is a **live environment**, not a batch compiler.

- The primary interface is an interactive workspace, not a command-line build tool
- Code is written, modified, and tested in the context of a running system
- Feedback is immediate - no compile-deploy-restart cycle
- The compiler exists to support the live environment, not the other way around

**Implication:** REPL and workspace tooling are first-class, not afterthoughts.

---

## 2. Hot Reload is Core

Code changes apply to the **running system** without restart.

- Leverages BEAM's built-in hot code loading
- Modifying a class updates all future message sends immediately
- Stateful objects can migrate their state across code versions
- The system evolves continuously rather than being replaced

**Implication:** Generated code must support BEAM's code upgrade patterns.

---

## 3. Actors Are Everything

Every entity is an **actor** - a BEAM process with state and a mailbox.

- There are no functions, methods, or procedures - only message sends between actors
- Message sends become actual inter-process messages
- Built-in fault isolation - one actor crashing doesn't take down others
- Unknown messages trigger `doesNotUnderstand:` for metaprogramming

**Implication:** Object instantiation creates processes; all behavior is message dispatch.

---

## 4. No Image, But Live

State persists in **running processes**, not a monolithic snapshot file.

- Unlike traditional Smalltalk, no single image file to save/load
- State is distributed across processes (and potentially nodes)
- Persistence via OTP patterns: ETS, DETS, Mnesia, or external stores
- System can be reconstructed from code + persisted state

**Implication:** Need clear patterns for state persistence and system bootstrap.

---

## 5. Code Lives in Files

Source code lives in the **filesystem**, not in a binary image.

- `.bt` files are plain text, one file per actor/class (like Pharo's Tonel format)
- Standard directory structure: `src/`, `test/`, `lib/`
- Git-friendly: meaningful diffs, branches, pull requests work naturally
- Tool-friendly: editors, grep, sed, AI agents can read/write code
- No opaque binary blobs - the filesystem IS the source of truth
- Live system and files stay in sync via tooling

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
- Encapsulation enforced: only way to interact with object is via messages
- Newspeak-style: no global namespace, all access through message chains

**Implication:** Even basic operations compile to message sends (optimized where possible).

---

## 7. Async-First, Sync When Needed

Message sends are **asynchronous by default**, returning futures (like Newspeak, unlike traditional Smalltalk).

- Default sends return immediately with a **future/promise**, not a value
- Actors don't block waiting for responses - they continue processing
- Sync sends available via explicit syntax for simple cases
- BEAM actors shouldn't block on each other - defeats fault isolation

**Implication:** All inter-actor communication is async; compiler generates futures.

---

## 8. Reflection as Primitive

**Full runtime introspection** is a language feature, not a library.

- Inspect any actor's state, mailbox, and behavior at runtime
- Query and modify methods dynamically
- The system is self-describing: code is data, data is code

**Implication:** Compiler must preserve enough metadata for full introspection.

---

## 9. Seamless BEAM Ecosystem Integration

Beamtalk is a **first-class BEAM citizen**, not an isolated language.

Interop is not an afterthought — it's essential for adoption. The BEAM ecosystem has decades of battle-tested libraries (OTP, Phoenix, Ecto, Nx). Beamtalk must use them seamlessly.

**Design principles:**
- **Call any BEAM module** — Erlang, Elixir, Gleam, LFE libraries work directly
- **Be callable by any BEAM language** — Beamtalk actors are standard `gen_server` modules
- **Share supervision trees** — Mix Beamtalk actors with Erlang/Elixir processes under one supervisor
- **Use Hex.pm** — Standard package manager, no separate ecosystem
- **OTP-native** — Actors implement OTP behaviors; appear in Observer; support distributed Erlang

**Why this matters:**
- Zero libraries means zero adoption
- Teams won't rewrite working Elixir/Erlang code
- Phoenix, Ecto, Nx are too valuable to abandon
- Gradual migration must be possible

**Implication:** Generated code follows BEAM conventions exactly. Foreign function interface is simple and low-friction. Build tools integrate with Mix/Rebar3. BEAM interop design needs an ADR.

---

## 10. Supervision is Language-Level

Fault tolerance via **declarative supervision trees**, not library patterns.

- Supervisors are actors that spawn and monitor child actors
- Restart strategies are part of actor definitions
- Embrace BEAM's "let it crash" philosophy
- No defensive programming - let the runtime handle failures

**Implication:** Actor definitions include supervision strategies; failures are expected.

---

## 11. Live Patching is Syntax

Hot code reload has **dedicated language syntax**, not just VM capability.

- `patch` expressions compile and upgrade running actors
- State preservation across code versions is automatic
- Sub-200ms from edit to running in production

**Implication:** Parser and compiler must handle `patch` expressions specially.

---

## 12. Agent Systems as Primary Use Case

Beamtalk is designed for **multi-agent AI systems**.

- Spawn swarms of LLM-powered actors (Claude, GPT, etc.)
- Live inspection of agent reasoning, mailboxes, interactions
- Edit agent prompts and behaviors while they run
- Scale to millions of concurrent agents via BEAM's lightweight processes

**Implication:** First-class support for LLM integration, tool use, agent coordination.

---

## 13. Compiler is the Language Service (TypeScript Approach)

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
- **Mandatory static typing** - Dynamic by default; optional types come later

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
