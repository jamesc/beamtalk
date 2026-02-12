# ADR 0021: Streams and I/O Design

## Status
Proposed (2026-02-12)

## Context

### Problem

Beamtalk has basic file I/O (`File readAll:`, `File writeAll:contents:`) and a `TranscriptStream` actor for workspace output, but lacks the **Stream** abstraction that is foundational to Smalltalk's I/O and collection processing model.

In Smalltalk, Stream is pervasive:
- **ReadStream** / **WriteStream** for sequential access over collections
- **FileStream** for line-by-line and chunked file I/O
- String building via `WriteStream on: String new`
- Parsing via `ReadStream on: inputString`
- Lazy processing of large datasets

Without Streams, Beamtalk users cannot:
1. Read files line-by-line (only `File readAll:` which slurps entire content)
2. Build strings incrementally (no WriteStream equivalent)
3. Process large collections lazily (everything is eager)
4. Compose I/O pipelines

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
- Already demonstrates actor-based streaming pattern

**Collections** (`lib/List.bt`, `lib/Set.bt`, etc.):
- Full eager iteration: `do:`, `collect:`, `select:`, `reject:`, `inject:into:`
- No lazy variants
- All operations materialize full result collections

### Constraints

1. **BEAM's I/O model** is fundamentally different from Smalltalk's — ports, processes, and message passing rather than synchronous byte streams
2. **Erlang already has** robust I/O: `file:read_line/1`, `io:get_line/1`, `gen_tcp`, `ssl`, and OTP's `gen_statem` for protocol handling
3. **Elixir's Stream module** provides lazy enumeration on BEAM — we can interop with it
4. **Interactive-first principle** — Streams should work naturally in the REPL
5. **Actor model** — Beamtalk's core paradigm is message-passing between actors

## Decision

Implement a three-layer Stream design that aligns with both Smalltalk heritage and BEAM strengths, unified under a common abstract `Stream` superclass.

### Class Hierarchy

```
Object
└── Stream (abstract)              ← shared protocol: next, atEnd, do:, collect:, select:, take:
    ├── ReadStream (sealed)        ← Layer 1: sequential read over collection
    ├── WriteStream (sealed)       ← Layer 1: incremental collection/string building
    └── LazyStream (sealed)        ← Layer 3: closure-based, potentially infinite

Actor
└── FileStream                     ← Layer 2: process-based, wraps OS file handle
```

**Design rationale:** ReadStream, WriteStream, and LazyStream all respond to the same read protocol (`next`, `atEnd`, `do:`, `collect:`, `take:`), so they share an abstract `Stream` superclass — just as `Integer` and `Float` share `Number`. This means code that works with any `Stream` works with all three.

FileStream is an Actor (gen_server) because it wraps an OS resource with process-linked lifecycle. It implements the same stream protocol (`next`, `nextLine`, `atEnd`, `do:`, `close`) via **duck typing** — it responds to the same messages as Stream but cannot inherit from it since it extends Actor. This is idiomatic Beamtalk: shared behavior through protocol conformance, not inheritance. Code that sends `next` and `atEnd` works with both a `ReadStream` and a `FileStream`. A future Behaviours mechanism (similar to Elixir's `@behaviour` or Rust traits) could formalize this protocol conformance with compile-time verification — that would be a separate ADR affecting the whole language.

### Layer 1: ReadStream / WriteStream (Collection Streams)

Sequential access over in-memory collections, following Smalltalk's model closely. These are **value types** (Stream subclasses), not actors.

```beamtalk
// ReadStream — sequential read over a collection
stream := ReadStream on: #(1, 2, 3, 4, 5)
stream next        // => 1
stream next        // => 2
stream peek        // => 3 (doesn't advance)
stream upToEnd     // => #(3, 4, 5)
stream atEnd       // => true

// WriteStream — incremental collection building
ws := WriteStream on: List new
ws nextPut: 'hello'
ws nextPut: 'world'
ws contents        // => #('hello', 'world')

// String building (classic Smalltalk pattern)
s := WriteStream on: String new
s nextPutAll: 'Hello'
s nextPutAll: ', '
s nextPutAll: 'World'
s contents         // => 'Hello, World'
```

**Implementation:** Value types backed by collection + index. Maps to simple Erlang state:
```erlang
%% ReadStream internal: #{collection => List, position => Index}
%% WriteStream internal: #{buffer => List}  (reversed, flatten on contents)
```

### Layer 2: FileStream (File I/O Streams)

Line-by-line and chunked file access. These are **actors** — each FileStream wraps an Erlang file handle in a gen_server, enabling:
- Automatic cleanup on process exit
- Concurrent file access from multiple REPL sessions
- Backpressure via mailbox

```beamtalk
// Read file line by line
file := FileStream open: 'data.csv'
file nextLine             // => 'name,age,city'
file nextLine             // => 'Alice,30,London'
file do: [:line | Transcript show: line]  // iterate remaining lines
file close

// Write file incrementally
out := FileStream create: 'output.txt'
out nextPutAll: 'Header line'
out cr
out nextPutAll: 'Data line'
out close

// Block-scoped auto-close (like Ruby/Python with-statement)
FileStream open: 'data.csv' do: [:file |
  file do: [:line | Transcript show: line]
]
// file automatically closed here
```

**Implementation:** Actor wrapping Erlang `file:open/2` handle:
```erlang
%% FileStream is an Actor subclass (gen_server)
%% State: #{handle => IoDevice, mode => read | write}
%% On terminate: file:close(Handle)
```

### Layer 3: LazyStream (Future Work)

Lazy, composable pipelines for large/infinite data. A subclass of `Stream`, so it shares the same read protocol (`next`, `atEnd`, `do:`, `collect:`, `take:`). Deferred to a future ADR once Layer 1 and 2 prove out the design.

The recommended approach follows Elixir's proven closure-based model — each lazy operation wraps the previous in a closure, with nothing computed until a terminal operation (like `take:` or `asList`) pulls elements through.

```beamtalk
// Future — not part of this ADR
// Closure-based lazy streams (Elixir model)
// LazyStream is a Stream, so all Stream protocol works

// Infinite sequence
s := LazyStream from: 1
s := s select: [:n | n isEven]
s := s collect: [:n | n * n]
s take: 5
// => #(4, 16, 36, 64, 100)

// Works with same code as ReadStream
printFirst5: aStream => 
  (aStream take: 5) do: [:each | Transcript show: each]

// Pass either — polymorphism via shared Stream protocol
printFirst5: (ReadStream on: #(10, 20, 30, 40, 50))
printFirst5: (LazyStream from: 1)
```

**Chaining syntax note:** Message-send languages (Smalltalk, Newspeak, Beamtalk) have a known limitation where keyword messages cannot chain without parentheses or temporary variables. No satisfying syntax sugar has been found in the Smalltalk literature — the Pharo [Sequence](https://ceur-ws.org/Vol-3627/paper11.pdf) framework (IWST 2023) addresses this at the library level but not syntactically. Temporary variables are the pragmatic approach and align with Beamtalk's interactive-first philosophy (each step is inspectable in the REPL). Research into novel pipeline syntax is tracked in BT-506.

### Summary of Types

| Class | Kind | Superclass | Use Case |
|-------|------|------------|----------|
| Stream | Abstract | Object | Shared protocol: `next`, `atEnd`, `do:`, `collect:`, `take:` |
| ReadStream | Value type (sealed) | Stream | Sequential read over collections |
| WriteStream | Value type (sealed) | Stream | Build collections/strings incrementally |
| LazyStream | Value type (sealed) | Stream | Closure-based lazy/infinite sequences |
| FileStream | Actor | Actor | Line-by-line file I/O (duck-types Stream protocol) |

## Prior Art

### Smalltalk (Pharo/Squeak)
- `Stream` → `PositionableStream` → `ReadStream`, `WriteStream`, `ReadWriteStream`
- `FileStream` for file I/O (being replaced by `FileReference` in modern Pharo)
- Streams are stateful objects with `next`, `nextPut:`, `atEnd`, `position`
- Widely used for string building, parsing, binary data
- **Adopted:** ReadStream/WriteStream API for collection streams

### Erlang
- No stream abstraction — uses `file:read_line/1`, `io:get_line/1` directly
- File handles are process-linked (auto-close on process exit)
- Binary pattern matching for parsing (no stream needed)
- **Adopted:** Process-linked file handles for FileStream cleanup

### Elixir
- `Enum` module for eager collection operations (like our current `do:`, `collect:`, etc.)
- `Stream` module for lazy composition: `Stream.map/2`, `Stream.filter/2`, `Stream.take/2`
- `File.stream!/1` returns lazy stream of lines
- `IO.stream/2` for streaming I/O
- **Deferred:** Lazy stream composition to Layer 3

### Ruby
- `IO` class with `each_line`, `read`, `write`
- `Enumerator::Lazy` for lazy pipelines
- `File.open(path) { |f| ... }` block-scoped auto-close
- **Adopted:** Block-scoped `FileStream open:do:` for auto-close

### Python
- File objects are iterators (line-by-line by default)
- `with open(path) as f:` context manager for auto-close
- Generator-based lazy streams
- **Adopted:** Line-by-line as default iteration for FileStream

## User Impact

### Newcomer
- ReadStream/WriteStream are familiar from other languages (Java's Scanner/StringBuilder, Python's StringIO)
- FileStream with block-scoped auto-close prevents resource leak mistakes
- `file do: [:line | ...]` is intuitive

### Smalltalk Developer
- ReadStream/WriteStream match expectations exactly
- FileStream as actor rather than object is a departure — but fits BEAM
- String building via WriteStream is a familiar pattern

### Erlang/BEAM Developer
- FileStream wrapping `file:open/2` in gen_server is natural OTP pattern
- Process-linked handles match Erlang's cleanup model
- May prefer raw `file:read_line/1` for performance — that's fine, interop allows it

### Operator
- FileStream actors are supervised — visible in process lists
- Auto-close on process exit prevents file handle leaks
- File access security inherits from `beamtalk_file.erl` path validation

## Steelman Analysis

### "Just use Erlang's file module directly via interop" (BEAM developer)
**Best argument:** Beamtalk already has BEAM interop. Erlang's `file` module is battle-tested with 30+ years of production use, zero-overhead, and covers every edge case (symlinks, encodings, permissions, large files). Adding Beamtalk wrappers means maintaining a second, less-tested abstraction that will always lag behind Erlang's capabilities. Experienced BEAM developers will bypass it anyway, and newcomers learning the wrapper still need to learn the Erlang module eventually for anything non-trivial. Every wrapper bug is a bug Erlang doesn't have.

**Counter:** True for advanced users, but Beamtalk's value proposition is interactive-first development with message-send syntax. `file nextLine` in the REPL is discoverable; `file:read_line(Handle)` requires knowing Erlang. The wrapper is thin (delegates to Erlang internally) and adds real value: (1) path validation security, (2) actor-based lifecycle management, (3) structured `#beamtalk_error{}` instead of Erlang error tuples, (4) block-scoped auto-close. Users who need raw Erlang can still use interop.

### "ReadStream/WriteStream are unnecessary — collections have do:/collect:" (Pragmatist)
**Best argument:** Beamtalk collections already support full iteration. ReadStream adds mutable position state — fundamentally un-BEAM-like in a language targeting an immutable VM. WriteStream is just `inject:into:` with extra steps. Modern languages (Elixir, Rust, Kotlin) dropped positional streams entirely in favor of iterators and lazy pipelines. Smalltalk's Streams are a 1980s design from before people understood the iterator pattern well. We'd be adding legacy baggage that modern Smalltalk (Pharo) is actively moving away from (`FileReference` replacing `FileStream`). Nobody writes `WriteStream on: String new` when they can use string interpolation.

**Counter:** The strongest argument here. ReadStream's real value is **parsing** — sequential consumption with `peek` and `upToEnd` for building parsers, tokenizers, and protocol handlers. That's hard to replicate with `do:/collect:`. WriteStream for string building avoids O(n²) concatenation which matters for codegen and template output. But if we skip these, we should at minimum provide `StringBuffer` and evaluate whether lazy streams (Layer 3) subsume ReadStream's use cases entirely.

### "FileStream should be a value type, not an actor" (Simplicity advocate)
**Best argument:** Making FileStream an actor adds per-call gen_server overhead, supervision complexity, and debugging difficulty (file reads go through mailbox, show up in process lists, can hit mailbox overflow). A value type wrapping a file handle would be simpler, faster, and match how every other language does it. The BEAM already links file handles to the calling process — if the process dies, the handle closes. You don't need a separate actor for cleanup. And in practice, most file operations are sequential within a single function — there's no concurrency benefit from making it an actor.

**Counter:** Valid performance concern, but the gen_server overhead (~5μs per call) is negligible compared to actual I/O latency (~100μs+ per disk read). The actor model provides: (1) multiple REPL sessions can share a FileStream (pass actor ref), (2) supervision means crashed file operations don't leak handles, (3) `Workspace actors` can list open files for inspection. The real question is whether these benefits justify the complexity for the common case. A hybrid approach — value type for simple read-all, actor for streaming — may be worth considering, though `File readAll:` already covers the simple case.

### "LazyStream under the Stream hierarchy is premature" (Incrementalist)
**Best argument:** Layer 3 (LazyStream) is speculative — we don't know if Beamtalk users will need lazy streams, and designing an abstract `Stream` superclass to accommodate a feature that doesn't exist yet risks over-engineering. The abstract class might have the wrong protocol once we actually build LazyStream. ReadStream has `position`, `position:`, `reset` — none of which make sense for LazyStream. Better to build Layers 1 and 2, learn from real usage, then design the hierarchy. Premature abstraction is worse than no abstraction.

**Counter:** This is the most compelling objection. The mitigation is keeping `Stream` minimal — only protocol that genuinely applies to all streams (`next`, `atEnd`, `do:`, `collect:`, `take:`). Position-related methods stay on ReadStream only. If LazyStream proves to need a different base, we can restructure before users depend on the hierarchy. The cost of the abstract class is near-zero (a few shared methods); the benefit is polymorphic code from day one.

### "Three new classes is too much — just extend File" (Minimalist)
**Best argument:** File already exists. Add `File readLines:` returning a list, `File forEachLine:path: do:` for iteration, and `String builder` for string building. Three new classes (plus an abstract superclass, plus LazyStream planned) is a lot of surface area for a young language that should be proving its actor model, not replicating Smalltalk's collection hierarchy. Every class is documentation, tests, maintenance, and cognitive load. Keep it simple.

**Counter:** Extending File with streaming methods conflates "file system operations" (exists, read, write) with "sequential data access" (next, peek, atEnd). These are different concerns — Stream works over any data source, not just files. The Smalltalk insight that Streams are independent of their backing store (collection, file, network) has proven valuable over 40 years. Three focused classes is cleaner than one overloaded File class.

## Alternatives Considered

### Alternative A: Elixir Stream Interop Only
Skip building native Beamtalk streams entirely. Use Elixir's `Stream` and `File.stream!` via interop.

```beamtalk
// Hypothetical interop
lines := Elixir.File streamBang: 'data.csv'
lines | Elixir.Stream map: [:line | line uppercase]
      | Elixir.Enum take: 10
```

**Rejected because:** Requires Elixir as a dependency. Syntax becomes awkward (Elixir module calls, not message sends). Doesn't provide Smalltalk-familiar ReadStream/WriteStream. Doesn't integrate with Beamtalk's actor model.

### Alternative B: Iterator Protocol Instead of Streams
Define an `Iterable` protocol that any object can implement, similar to Rust's `Iterator` trait or Python's `__iter__`.

```beamtalk
// Hypothetical iterator protocol
file := File openLines: 'data.csv'
file | map: [:line | line trim] | take: 10

List new | addAll: file  // consumes iterator into list
```

**Rejected because:** Requires language-level protocol/trait support that Beamtalk doesn't have yet. Iterator protocol is a larger language design decision (ADR-worthy on its own). Can be added later as Layer 3 on top of the Stream foundation.

### Alternative C: Pure Functional Streams (Lazy Lists)
Implement streams as lazy linked lists using closures, similar to Haskell or Scheme.

```beamtalk
// Hypothetical lazy stream
naturals := LazyStream from: 1 by: [:n | n + 1]
evens := naturals select: [:n | n isEven]
evens take: 5  // => #(2, 4, 6, 8, 10)
```

**Rejected for now:** Elegant but requires careful memory management (holding head prevents GC). Better suited as Layer 3 once basic I/O streams are proven. Doesn't address file I/O needs.

## Consequences

### Positive
- Completes Smalltalk's core class hierarchy (Stream is foundational)
- Enables line-by-line file processing without loading entire files
- String building via WriteStream avoids O(n²) concatenation
- FileStream as actor provides supervised, auto-cleaning file handles
- Block-scoped `FileStream open:do:` prevents resource leaks
- Foundation for future lazy stream/pipeline support

### Negative
- ReadStream/WriteStream add mutable position state (somewhat un-BEAM-like)
- FileStream as actor has per-call gen_server overhead
- Three new classes to maintain (ReadStream, WriteStream, FileStream)
- Risk of under-use if developers prefer raw Erlang `file` module

### Neutral
- Existing `File readAll:` / `File writeAll:contents:` remain for simple use cases
- TranscriptStream continues unchanged (already actor-based)
- Does not affect compilation pipeline — pure runtime/stdlib addition

## Implementation

### Phase 1: ReadStream and WriteStream
- Create `lib/ReadStream.bt` and `lib/WriteStream.bt` as sealed Object subclasses
- Implement primitives in `beamtalk_stream.erl` (or separate modules)
- Register in `builtins.rs`, `beamtalk_stdlib.app.src`, `beamtalk_primitive.erl`
- Add tests in `tests/stdlib/streams.bt`
- **Components:** stdlib (lib/), runtime (primitives), codegen (builtins registration)

### Phase 2: FileStream
- Create `lib/FileStream.bt` as Actor subclass
- Implement as gen_server wrapping `file:open/2`
- Path validation via existing `beamtalk_file.erl` security checks
- Block-scoped `open:do:` with ensure-close
- Add tests in `tests/stdlib/file_stream.bt` (and E2E for actor lifecycle)
- **Components:** stdlib (lib/), runtime (gen_server), codegen (actor registration)

### Phase 3: Collection Integration
- Add `readStream` method to List, Tuple, String: `#(1,2,3) readStream`
- Add `writeStream` class method to List, String: `WriteStream on: List new`
- **Components:** stdlib updates to existing classes

### Future: Layer 3 — Lazy Streams
- Separate ADR when ready
- Consider process-based generators, continuation passing, or Elixir interop
- Build on top of Layers 1 and 2

## References
- Related ADRs: ADR 0007 (compilable stdlib), ADR 0009 (OTP structure), ADR 0010 (global objects), ADR 0019 (singleton access)
- Existing I/O: `lib/File.bt`, `lib/TranscriptStream.bt`
- Pharo Streams: https://books.pharo.org/deep-into-pharo/pdf/DeepIntoPharo.pdf (Chapter: Streams)
- Erlang file module: https://www.erlang.org/doc/apps/stdlib/file
- Elixir Stream module: https://hexdocs.pm/elixir/Stream.html
