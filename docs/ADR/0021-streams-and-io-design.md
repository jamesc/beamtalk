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
- Full eager iteration: `do:`, `collect:`, `select:`, `reject:`, `inject:into:`
- No lazy variants
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

// Generators
Stream from: 1                    // => infinite Stream: 1, 2, 3, ...
Stream from: 1 by: [:n | n * 2]  // => infinite Stream: 1, 2, 4, 8, ...
Stream generate: [:yield |        // => custom generator
  yield value: 'hello'
  yield value: 'world'
]

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

### File Streaming

`File` gains a class method that returns a `Stream` of lines — no new FileStream class needed:

```beamtalk
// Read file lazily — no new class, just File + Stream
File lines: 'data.csv' do: [:line |
  Transcript show: line
]

// Compose with Stream pipeline
headers := (File lines: 'data.csv') take: 1
data := (File lines: 'data.csv') drop: 1

// Block-scoped for explicit handle management
File open: 'data.csv' do: [:handle |
  handle lines select: [:line | line includes: 'ERROR']
]
// handle closed automatically

// Process large files in constant memory
(File lines: 'huge.log') 
  select: [:line | line includes: 'ERROR']
  | do: [:line | Transcript show: line]
```

**Implementation:** `File lines:` opens a handle, returns a Stream whose generator calls `file:read_line/1`. When the stream is exhausted or garbage collected, the handle closes. Block-scoped `File open:do:` provides explicit lifecycle control.

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
- **Adopted:** Same protocol for eager (Collection) and lazy (Stream) — `select:`, `collect:`, `take:`

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
- `do:`, `collect:`, `select:` work the same — just lazy instead of eager
- Parsing code using `stream next` / `stream peek` needs adaptation

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
**Best argument:** Beamtalk already has BEAM interop. Erlang's `file` module is battle-tested with 30+ years of production use, zero-overhead, and covers every edge case (symlinks, encodings, permissions, large files). Adding Beamtalk wrappers means maintaining a second, less-tested abstraction that will always lag behind Erlang's capabilities. Experienced BEAM developers will bypass it anyway, and newcomers learning the wrapper still need to learn the Erlang module eventually for anything non-trivial. Every wrapper bug is a bug Erlang doesn't have.

**Counter:** True for advanced users, but Beamtalk's value proposition is interactive-first development with message-send syntax. `(File lines: 'data.csv') take: 5` in the REPL is discoverable; `file:read_line(Handle)` requires knowing Erlang, manual handle management, and error tuple matching. The wrapper is thin (delegates to Erlang internally) and adds real value: (1) lazy streaming without manual loop, (2) path validation security, (3) structured `#beamtalk_error{}` instead of Erlang error tuples, (4) block-scoped auto-close. Users who need raw Erlang can still use interop.

### "We should have ReadStream/WriteStream like Smalltalk" (Smalltalk purist)
**Best argument:** ReadStream and WriteStream are foundational Smalltalk classes used in virtually every Smalltalk system. Parsers use ReadStream for sequential consumption with `peek`. Code generators use WriteStream for efficient string building. Dropping them means porting Smalltalk libraries requires rewriting I/O code, not just translating syntax. Beamtalk claims Smalltalk heritage — removing core classes undermines that promise. And they're simple to implement — why not have them?

**Counter:** We're Smalltalk-*inspired*, not Smalltalk-compatible (see `docs/beamtalk-syntax-rationale.md`). ReadStream's positional model (`position`, `position:`, `reset`) assumes random access that doesn't generalize to files or infinite sequences. Modern Pharo is itself moving away from FileStream toward FileReference. The real porting friction will be actors and immutability, not streams. For parsing, Stream supports `next` as a terminal operation. For string building, `List join` and string concatenation cover the common cases. If a dedicated StringBuffer proves necessary, it can be added independently — it's not a stream.

### "One Stream class can't cover everything" (Type theorist)
**Best argument:** Lazy pipelines over collections, lazy line-by-line file reading, and infinite generators are fundamentally different things. A collection stream has a known size; a file stream has OS resource cleanup; a generator can be infinite. Cramming them into one `Stream` class means the type tells you nothing — you can't know if it's safe to call `size` or if it will ever terminate. Separate types (like Rust's `Iterator` + `Read` + `BufRead`) let the type system communicate capabilities.

**Counter:** Beamtalk is dynamically typed — the type system doesn't enforce these distinctions anyway. What matters is protocol conformance: all streams respond to `select:`, `collect:`, `take:`, `do:`. If you need to know the source, inspect it. One type is simpler to learn, simpler to document, and enables polymorphic code. Rust needs separate traits because it's statically typed; we don't.

### "Lazy evaluation is premature — eager is simpler" (Incrementalist)
**Best argument:** Beamtalk's collections already have eager `do:`, `collect:`, `select:`. Adding lazy streams means two ways to do everything — eager on collections, lazy on streams. Users have to choose, and they'll choose wrong. Lazy evaluation has gotchas: side effects run at unexpected times, debugging is harder (stack traces show the terminal operation, not the filter that's wrong), and performance can be worse for small collections due to closure overhead. Build what's needed (file line iteration) without the full lazy pipeline machinery.

**Counter:** Legitimate concern about two modes. The mitigation: eager collection methods remain the default for simple cases. `stream` is opt-in for when you need laziness (large data, infinite sequences, pipeline composition). This is exactly Kotlin's model (`List.filter` is eager, `Sequence.filter` is lazy) and it works well in practice. The lazy gotchas are real but well-understood — Elixir has used this model for 12+ years on BEAM without issues.

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

**Rejected because:** Requires language-level protocol/trait support (Behaviours) that Beamtalk doesn't have yet. Stream achieves the same result via a concrete class — when Behaviours arrive, Stream naturally becomes the reference implementation of an Iterable behaviour.

### Alternative D: FileStream as Actor
Wrap file handles in a gen_server (actor) for supervised lifecycle management.

**Rejected because:** Erlang developers do file I/O inline, not via process wrappers. The BEAM already links file handles to the calling process for auto-cleanup. A gen_server adds ~5μs overhead per call and supervision complexity for no benefit in the common case. Block-scoped `File open:do:` handles cleanup idiomatically. Users who need actor-wrapped files can build that at the application level.

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
- Constructors: `Stream from:by:` (generator), `Stream on:` (from collection)
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
- Add `stream` method to List, Tuple, String, Set, Dictionary
- Returns lazy Stream over elements
- **Components:** stdlib updates to existing classes

### Future Phases (separate ADRs/issues)
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
- Related ADRs: ADR 0007 (compilable stdlib), ADR 0009 (OTP structure), ADR 0019 (singleton access)
- Related issues: BT-506 (pipeline chaining syntax research), BT-507 (Future class ADR)
- Existing I/O: `lib/File.bt`, `lib/TranscriptStream.bt`
- Elixir Stream module: https://hexdocs.pm/elixir/Stream.html (primary inspiration)
- Rust Iterator: https://doc.rust-lang.org/std/iter/trait.Iterator.html
- Kotlin Sequence: https://kotlinlang.org/docs/sequences.html
- Pharo Streams: https://books.pharo.org/deep-into-pharo/pdf/DeepIntoPharo.pdf (rejected model)
- Erlang file module: https://www.erlang.org/doc/apps/stdlib/file
- Pharo Sequence pipeline paper: https://ceur-ws.org/Vol-3627/paper11.pdf
