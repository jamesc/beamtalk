# ADR 0021: Stream — Universal Data Interface

## Status
Proposed (2026-02-12)

## Context

### Problem

Beamtalk has basic file I/O (`File readAll:`, `File writeAll:contents:`) and eager collection iteration (`do:`, `collect:`, `select:`), but no **universal interface for sequential data**. Every data source — files, collections, network sockets, OS processes, generators — needs its own iteration pattern today.

Without a unified Stream abstraction, Beamtalk users cannot:
1. Read files line-by-line (only `File readAll:` which slurps entire content)
2. Process large data lazily (everything is eager — full result materialized)
3. Compose data pipelines across different sources
4. Write code that works with any sequential data source (file, collection, stdin, network)

### The Insight

Smalltalk's `ReadStream`/`WriteStream` (1980) and every modern language since have converged on the same idea: **a uniform interface for sequential data**. The implementations differ — Smalltalk used mutable position state, Elixir uses closures, Rust uses traits — but the concept is identical: `select:`, `collect:`, `take:`, `do:` should work on *any* data source.

Beamtalk's opportunity: implement this idea with modern (closure-based lazy) mechanics while keeping Smalltalk's elegant message-send protocol. Smalltalk's API with Elixir's engine.

### Current State

**File I/O** (`lib/File.bt`):
- Three class methods: `exists:`, `readAll:`, `writeAll:contents:`
- Synchronous, whole-file operations via Erlang's `file` module
- Security: rejects absolute paths and directory traversal
- Structured error handling via `#beamtalk_error{}`

**TranscriptStream** (`lib/TranscriptStream.bt`):
- Actor (gen_server) with pub/sub semantics
- Methods: `show:`, `cr`, `subscribe`, `unsubscribe`, `recent`, `clear`
- Workspace singleton (ADR 0019)

**Collections** (`lib/List.bt`, `lib/Set.bt`, etc.):
- List has full eager iteration: `do:`, `collect:`, `select:`, `reject:`, `inject:into:`, `detect:`, `anySatisfy:`, `allSatisfy:`, plus `take:`, `drop:`
- String has partial iteration: `each:`, `collect:`, `select:`
- Set has only `do:`; Dictionary has only `keysAndValuesDo:`
- No lazy variants on any collection
- All operations materialize full result collections

### Constraints

1. **BEAM's I/O model** is fundamentally different from Smalltalk's — ports, processes, and message passing rather than synchronous byte streams
2. **Erlang already has** robust I/O: `file:read_line/1`, `io:get_line/1`, `gen_tcp`, `ssl`, and OTP's `gen_statem` for protocol handling
3. **Elixir's Stream module** provides lazy enumeration on BEAM — proven model we can follow
4. **Interactive-first principle** — Streams should work naturally in the REPL
5. **Smalltalk heritage** — protocol names (`select:`, `collect:`, `do:`, `inject:into:`) must be preserved

## Decision

Introduce `Stream` as Beamtalk's **universal interface for sequential data** — a single, lazy, closure-based type that unifies collection processing, file I/O, and all future data sources under one protocol.

### Class Hierarchy

```
Object
└── Stream (sealed)                ← ONE type for all sequential data
```

Stream is not abstract — it's the concrete type. Everything that produces sequential data returns a Stream:

```beamtalk
// Collections
#(1, 2, 3) stream                  // => Stream over elements
'hello' stream                     // => Stream over characters
#{#a => 1} stream                  // => Stream over Associations

// Files
File lines: 'data.csv'            // => Stream of lines (lazy, constant memory)

// Generators (pure-functional, no process needed)
Stream from: 1                    // => infinite Stream: 1, 2, 3, ...
Stream from: 1 by: [:n | n * 2]  // => infinite Stream: 1, 2, 4, 8, ...

// Stateful generators — use actors (duck-typing or future Behaviours)
fib := FibonacciGenerator spawn   // Actor that speaks Stream protocol
fib take: 10                      // => #(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

// Future integration points (not part of this ADR)
Console lines                      // => Stream of stdin lines
socket lines                       // => Stream of TCP lines
Process output: 'ls -la'          // => Stream of command output lines
```

### The Universal Protocol

Every Stream responds to the same Smalltalk-named messages. Operations are either **lazy** (return a new Stream) or **terminal** (force evaluation and return a result):

```beamtalk
// Same pipeline works on ANY data source
countErrors: aStream =>
  s := aStream select: [:line | line includes: 'ERROR']
  s inject: 0 into: [:count :line | count + 1]

countErrors: (File lines: 'app.log')        // file
countErrors: (#('ERROR: x', 'OK', 'ERROR: y') stream)  // collection
countErrors: Console lines                   // stdin (future)
```

### Stream — Lazy Pipelines

The core abstraction. Each operation wraps the previous in a closure — nothing computes until a **terminal operation** (`asList`, `do:`, `take:`, `inject:into:`) pulls elements through.

```beamtalk
// Lazy — nothing computed yet, just a recipe
s := Stream from: 1
s := s select: [:n | n isEven]
s := s collect: [:n | n * n]
s take: 5
// NOW computes: => #(4, 16, 36, 64, 100)

// From a collection — lazy wrapper, no copy
#(1, 2, 3, 4, 5) stream
  select: [:n | n > 2]
// => Stream (unevaluated)

// Terminal operations force evaluation
(#(1, 2, 3, 4, 5) stream select: [:n | n > 2]) asList
// => #(3, 4, 5)
```

**Key protocol:**

| Method | Type | Description |
|--------|------|-------------|
| `select:` | Lazy | Filter elements |
| `collect:` | Lazy | Transform elements |
| `reject:` | Lazy | Inverse filter |
| `take:` | Terminal | First N elements as List |
| `drop:` | Lazy | Skip first N elements |
| `do:` | Terminal | Iterate with side effects |
| `inject:into:` | Terminal | Fold/reduce |
| `detect:` | Terminal | First matching element |
| `asList` | Terminal | Materialize to List |
| `anySatisfy:` | Terminal | Boolean — any match? |
| `allSatisfy:` | Terminal | Boolean — all match? |

**Implementation:** Closure-based, following Elixir's proven model:
```erlang
%% Each lazy op wraps previous in a closure
%% Stream internal: #{generator => fun() -> {element, NextFun} | done}
%% select: wraps generator, skipping non-matching elements
%% collect: wraps generator, transforming each element
%% Terminal ops: pull elements until done or limit reached
```

**Error handling — misuse examples:**
```beamtalk
// Infinite stream + asList = hangs (programmer error, like 1/0)
(Stream from: 1) asList           // ⚠️ Never terminates — use take: first

// Safe: always bound infinite streams
(Stream from: 1) take: 10         // => #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// REPL inspection — Stream describes its pipeline, not its data
> s := #(1, 2, 3) stream select: [:n | n > 1]
Stream(select: [...])              // shows structure, not values
> s asList
#(2, 3)                            // terminal forces evaluation
```

### File Streaming

`File` gains a class method that returns a `Stream` of lines — no new FileStream class needed:

```beamtalk
// Read file lazily — no new class, just File + Stream
(File lines: 'data.csv') do: [:line |
  Transcript show: line
]

// Compose with Stream pipeline
headers := (File lines: 'data.csv') take: 1
data := (File lines: 'data.csv') drop: 1

// Block-scoped for explicit handle management
File open: 'data.csv' do: [:handle |
  (handle lines
    select: [:line | line includes: 'ERROR'])
    do: [:line | Transcript show: line]
]
// handle closed automatically

// Process large files in constant memory
lines := File lines: 'huge.log'
errors := lines select: [:line | line includes: 'ERROR']
errors do: [:line | Transcript show: line]
```

**Implementation:** `File lines:` opens a handle, returns a Stream whose generator calls `file:read_line/1`. When the stream is exhausted, the handle closes automatically. If the stream is abandoned without being fully consumed, the BEAM's process-linked file handle ensures cleanup when the owning process exits. Block-scoped `File open:do:` provides explicit lifecycle control for cases where deterministic cleanup matters.

**Cross-process constraint:** File-backed Streams must be consumed by the same process that created them (BEAM file handles are process-local). To pass file data to an actor, materialize first: `(File lines: 'data.csv') take: 100` returns a List that can be sent safely. Collection-backed Streams have no such restriction.

### Collection Integration

Collections gain a `stream` method that returns a lazy Stream:

```beamtalk
#(1, 2, 3, 4, 5) stream           // => Stream over list elements
'hello world' stream               // => Stream over characters
#{#a => 1, #b => 2} stream         // => Stream over Associations
```

This means collections keep their eager `do:`, `collect:`, `select:` for simple cases, but `stream` provides the lazy pipeline when needed.

### Summary

| Class | Kind | Use Case |
|-------|------|----------|
| Stream | Value type (sealed) | Lazy pipelines — the ONE stream type |
| File | Existing | `File lines:` returns a Stream of lines |

## Prior Art

### Smalltalk (Pharo/Squeak)
- `Stream` → `PositionableStream` → `ReadStream`, `WriteStream`, `ReadWriteStream`
- `FileStream` for file I/O (being replaced by `FileReference` in modern Pharo)
- Streams are stateful objects with `next`, `nextPut:`, `atEnd`, `position`
- Widely used for string building, parsing, binary data
- **Rejected:** Positional stream model — 1980s design assumes random access, doesn't generalize to lazy/infinite/file sources

### Erlang
- No stream abstraction — uses `file:read_line/1`, `io:get_line/1` directly
- File handles are process-linked (auto-close on process exit)
- Binary pattern matching for parsing (no stream needed)
- **Adopted:** Process-linked file handles, inline I/O (no actor wrapper)

### Elixir
- `Enum` module for eager collection operations (like our current `do:`, `collect:`, etc.)
- `Stream` module for lazy composition: `Stream.map/2`, `Stream.filter/2`, `Stream.take/2`
- `File.stream!/1` returns lazy stream of lines
- `IO.stream/2` for streaming I/O
- **Adopted:** Closure-based lazy streams as the core model — this is our primary inspiration

### Rust
- `Iterator` trait — lazy by default, `.map()`, `.filter()`, `.take()`, `.collect()`
- Terminal operations (`collect`, `for_each`, `count`) force evaluation
- No positional streams — iterators replaced them entirely
- **Adopted:** Lazy/terminal distinction, `collect:` as terminal materializer

### Kotlin
- `Sequence` — lazy pipeline, same API as eager collections but deferred
- `sequence { yield(value) }` for generator-based streams
- Known confusion: `List.filter {}` vs `Sequence.filter {}` — same name, different semantics, hidden by extension functions and type inference
- **Adopted:** Same protocol for eager (Collection) and lazy (Stream) — `select:`, `collect:`, `take:`
- **Key difference:** Smalltalk's explicit message-send-to-receiver makes the eager/lazy boundary visible at the call site, unlike Kotlin's extension functions

### Ruby
- `IO` class with `each_line`, `read`, `write`
- `Enumerator::Lazy` for lazy pipelines
- `File.open(path) { |f| ... }` block-scoped auto-close
- **Adopted:** Block-scoped `File open:do:` for auto-close

### Python
- File objects are iterators (line-by-line by default)
- `with open(path) as f:` context manager for auto-close
- Generator-based lazy streams
- **Adopted:** Line-by-line as default file iteration

## User Impact

### Newcomer
- `Stream` with `select:`, `collect:`, `take:` is familiar from Kotlin/Rust/Java streams
- `File lines: 'data.csv'` is intuitive — returns something you can iterate
- Block-scoped `File open:do:` prevents resource leak mistakes
- One type to learn, not a hierarchy

### Smalltalk Developer
- **Departure:** No ReadStream/WriteStream. This is the biggest break from Smalltalk tradition.
- **Migration:** `ReadStream on: collection` → `collection stream`. `WriteStream on: String new` → string concatenation or `List join`.
- Same protocol names (`select:`, `collect:`, `do:`) — but Stream versions are lazy while Collection versions remain eager
- Parsing code using `stream next` / `stream peek` needs adaptation (see `peekable` in Steelman Analysis)

### Erlang/BEAM Developer
- `File lines:` wrapping `file:read_line/1` lazily is natural
- No actor overhead for file I/O — matches idiomatic Erlang
- Closure-based laziness is lightweight (no processes spawned per stream)
- Can still use raw `file:read_line/1` via interop when needed

### Operator
- Block-scoped file access prevents handle leaks
- No extra processes per stream — predictable resource usage
- File access security inherits from `beamtalk_file.erl` path validation

## Steelman Analysis

### "Just use Erlang's file module directly via interop" (BEAM developer)
**Best argument:** Beamtalk already has BEAM interop. Erlang's `file` module is battle-tested with 30+ years of production use, zero-overhead, and covers every edge case (symlinks, encodings, permissions, large files, memory-mapped I/O). But it's not just about files — you're building an *entire lazy evaluation framework* on top of BEAM, when Erlang already has list comprehensions and Elixir (available via interop) already has `Stream`. Every Beamtalk Stream operation adds a closure layer. For a 5-line file processing task, the overhead of creating closures, wrapping generators, and pulling through a pipeline is worse than a simple `file:read_line/1` loop. You're optimizing for elegance over the pragmatism that makes BEAM great.

**Counter:** Closure overhead on BEAM is low (Elixir's `Stream` module has run in production for 12+ years) though not literally free — for small collections (<1000 elements), eager collection methods will be faster. The key value isn't performance, it's *composability*: `(File lines: 'app.log') select: [:l | l includes: 'ERROR']` in the REPL is one expression. The Erlang equivalent is 5 lines of handle management, pattern matching, and manual cleanup. For an interactive-first language, that matters. Advanced users can always drop to Erlang via interop, and eager collection methods remain the default for small-data cases.

### "We should have ReadStream/WriteStream like Smalltalk" (Smalltalk purist)
**Best argument:** Beamtalk IS a compiler — and parsers are THE classic use case for ReadStream. Sequential consumption with `peek` (lookahead without consuming) and `upTo:` (consume until delimiter) are the building blocks of every hand-written parser, tokenizer, and protocol handler. Beamtalk's own lexer does exactly this. Dropping ReadStream means anyone writing a parser in Beamtalk has to reinvent sequential-consumption-with-lookahead on top of lazy pipelines, which is awkward — lazy streams are designed for transformation pipelines, not stateful character-by-character consumption.

WriteStream is equally practical: Beamtalk's codegen builds Core Erlang source by accumulating strings. `WriteStream on: String new` with `nextPutAll:` is 50 lines of implementation and covers a real, everyday need. Why force users into `inject:into:` gymnastics when the simple, proven tool exists?

Both classes are trivial to implement (~100 lines total), carry no design risk, and every Smalltalk developer expects them. The cost of NOT having them is higher than having them.

**Counter:** The strongest argument in this ADR. Two honest responses: (1) For parsing, Stream can support a `peekable` wrapper that adds `peek` and `next` — Rust does this with `Iterator::peekable()`. It's a focused addition rather than a whole positional stream hierarchy. (2) For string building, the need is real but it's not a *stream* — it's a buffer. If the need proves acute, we add `StringBuffer` as its own focused class, not as `WriteStream` which conflates output accumulation with sequential data reading. The key principle: don't build two parallel iteration hierarchies (positional + lazy) when one (lazy + focused utilities) suffices.

### "One Stream class can't cover everything" (Type theorist)
**Best argument:** A `Stream` from `#(1,2,3) stream` and a `Stream` from `Stream from: 1` are fundamentally different objects wearing the same type. Call `asList` on the infinite one — your program hangs forever. Call `size` on a file stream — it reads the entire file just to count lines. Call `stream` again on a generator — you get a fresh sequence, not a replay. The unified type *hides critical failure modes*.

In practice, this means: a function that accepts "a Stream" cannot know if it's safe to materialize it. Library authors must document "this only works with finite streams" — which is exactly the type information that *should* be in the type, not in prose. Rust separates `Iterator` (pull-based, possibly infinite) from `ExactSizeIterator` (known length) and `Read` (I/O bytes) for exactly this reason. One type isn't simplicity — it's *lost information*.

**Counter:** Beamtalk is dynamically typed — `List` can contain integers, strings, and actors in the same list, and nobody complains. The same pragmatism applies to Stream. `asList` on an infinite stream is a programmer error, like `1/0` — the language doesn't prevent division by zero either. In practice, users know their data source. And `take:` exists precisely to make infinite streams safe: `(Stream from: 1) take: 10` always works. If Behaviours arrive later, we can formalize `FiniteStream` vs `Stream` — but building multiple classes now for a dynamically typed language is premature.

### "Lazy evaluation is premature — eager is simpler" (Incrementalist)
**Best argument:** The debugging story is the killer. When a lazy pipeline produces wrong results, *where is the bug*? In the `select:`? The `collect:`? The source generator? With eager evaluation, you inspect each intermediate collection — it exists, it's a real value, you can print it. With lazy evaluation, intermediate values *don't exist* — they only materialize at the terminal operation. Stack traces point at `asList`, not at the `select:` three steps back that had the wrong predicate.

This matters doubly for an *interactive-first* language. The REPL is your debugger. Beamtalk's whole philosophy is "inspect intermediate values." Lazy evaluation is the opposite — it *removes* intermediate values. You're undermining your own design principle.

And there's a subtler gotcha: side effects in lazy pipelines run at *terminal* time, not at *definition* time. `stream collect: [:x | Transcript show: x. x * 2]` prints nothing when you define it — it prints when you call `asList`. For newcomers, this is deeply confusing. Elixir developers learn this the hard way; do we want that learning curve?

**Counter:** This is the most legitimate objection — and it requires concrete commitments, not hand-waving. Three specific mitigations: (1) Eager collection methods (`List select:`, `List collect:`) remain the default for simple cases — most users never need `stream`. Lazy is opt-in, not forced. (2) In the REPL, terminal operations run immediately (you type `s take: 5` and see results), so interactivity is preserved — each temp variable is inspectable. (3) **Stream must ship with good `printString`** showing pipeline structure, e.g. `Stream(from: 1 | select: [:n | n isEven] | collect: [:n | n * n])`. This is a Phase 1 requirement, not a "nice to have." Without it, lazy Streams are opaque in the REPL and the interactive-first principle is violated. The side-effect timing gotcha (lazy side effects run at terminal time) is real and must be documented prominently in Stream's class documentation and the REPL tutorial.

### "This creates a confusing parallel to Collection protocol" (API designer)
**Best argument:** After this ADR, Beamtalk has TWO things that respond to `select:`, `collect:`, `do:`, `inject:into:` — Collections (eager) and Streams (lazy). Same method names, different semantics. When a newcomer reads code that says `things select: [:x | x > 0]`, they have to check whether `things` is a List or a Stream to know when the filtering actually happens. When a library accepts "something you can `collect:` on," does it work with both? Do you document that? 

Kotlin has this exact problem: `List.filter {}` vs `Sequence.filter {}` — same name, different evaluation strategy. It's a known source of confusion. You're deliberately importing that confusion into Beamtalk.

The cleaner design: make ONE of them primary. Either collections are lazy by default (like Haskell), or streams use different method names (like Elixir's `Stream.map` vs `Enum.map`).

**Counter:** Smalltalk's message-send model resolves this more cleanly than Kotlin. The Kotlin confusion arises because extension functions and type inference hide which type you're calling on — `things.filter {}` looks identical whether `things` is a `List` or `Sequence`. In Beamtalk, you're *always* sending a message to a *known receiver*:

```beamtalk
aList select: [:x | x > 0]            // I know this is a List → eager
aList stream select: [:x | x > 0]     // I explicitly opted into Stream → lazy
```

The opt-in to laziness is visible at the call site — you wrote `stream`. In the REPL, you can inspect the receiver's class at any time. Polymorphism — same name, different behavior based on receiver — is literally the *point* of Smalltalk's design. `select:` on List returns a List. `select:` on Stream returns a Stream. The receiver IS the boundary, and it's always explicit. Making collections lazy by default (Haskell) would break the simplicity of `#(1,2,3) select: [:x | x > 0]` returning a List. Using different names (Elixir's `Enum.map` vs `Stream.map`) means you can't write generic code that works with both. Same names with explicit opt-in is the right balance — and Smalltalk's paradigm makes it work better than Kotlin's.

**Chaining syntax note:** Message-send languages (Smalltalk, Newspeak, Beamtalk) have a known limitation where keyword messages cannot chain without parentheses or temporary variables. No satisfying syntax sugar has been found in the Smalltalk literature — the Pharo [Sequence](https://ceur-ws.org/Vol-3627/paper11.pdf) framework (IWST 2023) addresses this at the library level but not syntactically. Temporary variables are the pragmatic approach and align with Beamtalk's interactive-first philosophy (each step is inspectable in the REPL). Research into novel pipeline syntax is tracked in BT-506.

## Alternatives Considered

### Alternative A: Smalltalk ReadStream/WriteStream Hierarchy
Follow Smalltalk's 1980s model: `Stream` → `PositionableStream` → `ReadStream`, `WriteStream`, plus `FileStream`.

```beamtalk
// Smalltalk model
stream := ReadStream on: #(1, 2, 3)
stream next      // => 1
stream position  // => 1
stream position: 0  // reset
```

**Rejected because:** Positional streams assume random access (`position`, `position:`, `reset`) which doesn't generalize to files, network, or infinite sequences. Modern Pharo is moving away from this model (`FileReference` replacing `FileStream`). Mutable position state is un-BEAM-like. Every modern language (Elixir, Rust, Kotlin, Java 8+) converged on lazy pipelines instead.

### Alternative B: Elixir Stream Interop Only
Skip building native Beamtalk streams. Use Elixir's `Stream` and `File.stream!` via interop.

```beamtalk
// Hypothetical interop
lines := Elixir.File streamBang: 'data.csv'
```

**Rejected because:** Requires Elixir as a dependency. Syntax becomes awkward (Elixir module calls, not Smalltalk-style message sends). Misses the opportunity for `select:`, `collect:`, `do:` protocol consistency with Beamtalk collections.

### Alternative C: Iterator Protocol (Rust/Python model)
Define an `Iterable` protocol that any object can implement, similar to Rust's `Iterator` trait or Python's `__iter__`.

```beamtalk
// Hypothetical — requires Behaviours
behaviour Iterable
  next => ...    // returns {value, nextState} or #done
  
// Any class could implement Iterable
Object subclass: Range
  implements: Iterable
  next => ...
```

**Rejected because:** Requires language-level protocol/trait support (Behaviours) that Beamtalk doesn't have yet. A concrete Stream class delivers the same user value now. When Behaviours arrive, Stream naturally becomes the reference implementation of an `Iterable` behaviour — the design is forward-compatible, not locked in.

### Alternative D: FileStream as Actor
Wrap file handles in a gen_server (actor) for supervised lifecycle management.

**Rejected because:** Erlang developers do file I/O inline, not via process wrappers. The BEAM already links file handles to the calling process for auto-cleanup. A gen_server adds ~5μs overhead per call and supervision complexity for no benefit in the common case. Block-scoped `File open:do:` handles cleanup idiomatically. Users who need actor-wrapped files can build that at the application level.

### Alternative E: Do Nothing (Status Quo)
Keep the current state: `File readAll:` for files, eager collection iteration for data processing. Rely on Erlang interop for anything beyond whole-file reads.

**Rejected because:** The status quo works for small-data, simple cases — but it's a dead end. Users cannot read large files without loading them into memory. Users cannot compose data processing pipelines. Every new data source (network, stdin, generators) would need its own bespoke iteration pattern. The "do nothing" option is acceptable for 2026 if Beamtalk only targets small scripts, but not if it aims to be a general-purpose language. The investment in Stream pays off across every future I/O feature.

### Alternative F: Eager File.lines + Fill Collection Gaps Only
Add `File lines:` returning a List (eager), plus fill missing `select:`, `collect:` on Set/Dictionary/String. No lazy Stream class.

```beamtalk
File lines: 'config.txt'    // Returns a List (eager, whole file)
aSet select: [:x | x > 0]   // Now works, returns a Set
```

**Rejected because:** Handles the 80% case (small-to-medium files, consistent collection protocol) but closes the door on large-file processing and infinite sequences. If `File lines:` returns a List, a 1GB log file loads entirely into memory. The incremental cost of lazy Stream is bounded (one new class), while the cost of retrofitting laziness later is high (changing return types is a breaking change). Building Stream now, while the API surface is small, is cheaper than adding it after users depend on eager `File lines:` returning a List. However, this alternative correctly identifies that Phase 3 (collection `stream`) is lower priority than Phase 1-2.

## Consequences

### Positive
- Modern lazy-first design aligned with Elixir/Rust/Kotlin (2026, not 1980)
- One stream type instead of a class hierarchy — simple mental model
- Enables line-by-line file processing without loading entire files
- Lazy pipelines process large/infinite data in constant memory
- Collections gain `stream` for lazy mode, keep eager `do:`/`collect:` for simple cases
- Block-scoped `File open:do:` prevents resource leaks

### Negative
- Departure from Smalltalk's ReadStream/WriteStream (porting friction for parser-heavy code)
- Closure-based lazy evaluation is a bigger engineering lift than positional streams
- No dedicated string building class (use `List join` or string concatenation for now)
- Imperative yield-style generators (`generate:`) deferred — requires hiding a process inside a value type, which breaks the mental model. Stateful generators use actors instead (duck-typing the Stream protocol). This is the right pattern for BEAM but less convenient than Kotlin's `sequence { yield() }`.
- Abandoned file streams (not fully consumed, not block-scoped) rely on process exit for handle cleanup — could leak handles in long-lived processes. `File open:do:` is the safe pattern.
- **Cross-process limitation:** File-backed Streams cannot be sent as messages to actors — BEAM file handles are process-local. A file Stream consumed by a different process than the one that opened it will fail. Collection-backed and generator-backed Streams (pure closures) transfer safely. This limits the "universal" promise in actor-heavy code. Mitigation: use `File open:do:` (block-scoped, same process) or collect to List before sending.
- **Auto-await interaction:** When an actor method returns a Stream, auto-await resolves the Future but the Stream's closures still reference the actor's process context. File-backed Streams from actors will fail on the caller side. This interaction must be documented clearly; full resolution is deferred to BT-507 (Future class ADR).

### Neutral
- Existing `File readAll:` / `File writeAll:contents:` remain for simple use cases
- TranscriptStream continues unchanged (already actor-based)
- Does not affect compilation pipeline — pure runtime/stdlib addition
- String building can be addressed separately if `join` proves insufficient

## Implementation

### Phase 1: Stream Core
- Create `lib/Stream.bt` as sealed Object subclass
- Implement closure-based generator in `beamtalk_stream.erl`
- Core protocol: `select:`, `collect:`, `reject:`, `take:`, `drop:`, `do:`, `inject:into:`, `detect:`, `asList`, `anySatisfy:`, `allSatisfy:`
- Constructors: `Stream from:` (successor), `Stream from:by:` (step function), `Stream on:` (from collection)
- **Required:** `printString` showing pipeline structure, e.g. `Stream(from: 1 | select: [...])` — critical for REPL inspectability
- Register in `builtins.rs`, `beamtalk_stdlib.app.src`, `beamtalk_primitive.erl`
- Add tests in `tests/stdlib/stream.bt`
- **Components:** stdlib (lib/), runtime (primitives), codegen (builtins registration)

### Phase 2: File Streaming
- Add `File lines:` class method returning a Stream of lines
- Add `File open:do:` for block-scoped handle management
- Stream generator calls `file:read_line/1` lazily
- Path validation via existing `beamtalk_file.erl` security checks
- Add tests in `tests/stdlib/file_stream.bt`
- **Components:** stdlib (File.bt update), runtime (file line generator)

### Phase 3: Collection Integration
- Add `stream` method to List, String, Set, Dictionary
- Returns lazy Stream over elements (characters for String, associations for Dictionary)
- Note: Tuple is excluded — it serves as a Result type (`isOk`, `unwrap`), not a general collection
- **Components:** stdlib updates to existing classes

### Future Phases (separate ADRs/issues)
- **Actor-based generators** — Actors that speak the Stream protocol (`take:`, `select:`, etc.) for stateful/imperative generators. This avoids hiding a process inside a value type. When Behaviours land, formalize as `Streamable` behaviour.
- `Console lines` — Stream of stdin lines
- Network streaming — TCP/UDP socket lines as Streams
- `Process output:` — OS command output as Stream
- Behaviours-based `Streamable` protocol (when Behaviours land)

## Migration Path

### Porting Smalltalk ReadStream code
```beamtalk
// Smalltalk: ReadStream on: #(1 2 3)
// Beamtalk:
#(1, 2, 3) stream

// Smalltalk: stream next
// Beamtalk: use take: or terminal operations instead of positional next
(#(1, 2, 3) stream) take: 1   // => #(1)
```

### Porting Smalltalk WriteStream code
```beamtalk
// Smalltalk: WriteStream on: String new, then nextPutAll:
// Beamtalk: use string concatenation or List join
#('Hello', ', ', 'World') inject: '' into: [:acc :s | acc , s]
// => 'Hello, World'
```

## References
- Related ADRs: ADR 0005 (sealed classes — Stream follows this pattern), ADR 0006 (unified dispatch), ADR 0007 (compilable stdlib), ADR 0009 (OTP structure), ADR 0014 (test framework — Stream tests use terminal ops in `// =>` assertions), ADR 0016 (module naming — Stream becomes `bt@stdlib@stream`), ADR 0019 (singleton access)
- Related issues: BT-506 (pipeline chaining syntax research), BT-507 (Future class ADR)
- Existing I/O: `lib/File.bt`, `lib/TranscriptStream.bt`
- Elixir Stream module: https://hexdocs.pm/elixir/Stream.html (primary inspiration)
- Rust Iterator: https://doc.rust-lang.org/std/iter/trait.Iterator.html
- Kotlin Sequence: https://kotlinlang.org/docs/sequences.html
- Pharo Streams: https://books.pharo.org/deep-into-pharo/pdf/DeepIntoPharo.pdf (rejected model)
- Erlang file module: https://www.erlang.org/doc/apps/stdlib/file
- Pharo Sequence pipeline paper: https://ceur-ws.org/Vol-3627/paper11.pdf
