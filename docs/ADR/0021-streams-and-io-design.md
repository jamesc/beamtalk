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

Implement a three-layer Stream design that aligns with both Smalltalk heritage and BEAM strengths:

### Layer 1: ReadStream / WriteStream (Collection Streams)

Sequential access over in-memory collections, following Smalltalk's model closely. These are **value types** (Object subclasses), not actors.

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

### Layer 3: Lazy Streams (Future Work)

Lazy, composable pipelines for large/infinite data. Deferred to a future ADR once Layer 1 and 2 prove out the design. The recommended approach follows Elixir's proven closure-based model — each lazy operation wraps the previous in a closure, with nothing computed until a terminal operation (like `take:` or `asList`) pulls elements through.

```beamtalk
// Future — not part of this ADR
// Closure-based lazy streams (Elixir model)
s := LazyStream from: 1
s := s select: [:n | n isEven]
s := s collect: [:n | n * n]
s take: 5
// => #(4, 16, 36, 64, 100)
```

**Chaining syntax note:** Message-send languages (Smalltalk, Newspeak, Beamtalk) have a known limitation where keyword messages cannot chain without parentheses or temporary variables. No satisfying syntax sugar has been found in the Smalltalk literature — the Pharo [Sequence](https://ceur-ws.org/Vol-3627/paper11.pdf) framework (IWST 2023) addresses this at the library level but not syntactically. Temporary variables are the pragmatic approach and align with Beamtalk's interactive-first philosophy (each step is inspectable in the REPL). Research into novel pipeline syntax is tracked in BT-506.

### Summary of Types

| Class | Kind | Superclass | Use Case |
|-------|------|------------|----------|
| ReadStream | Value type | Object | Sequential read over collections |
| WriteStream | Value type | Object | Build collections/strings incrementally |
| FileStream | Actor | Actor | Line-by-line file I/O with auto-cleanup |

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

### "Just use Erlang's file module directly via interop"
**Best argument:** Beamtalk already has BEAM interop. Erlang's `file` module is battle-tested, zero-overhead, and covers every edge case. Adding Beamtalk wrappers adds maintenance burden and potential bugs for no functional gain. Experienced BEAM developers will use Erlang directly anyway.

**Counter:** True for advanced users, but newcomers and Smalltalk developers expect `stream nextLine` not `file:read_line(Handle)`. The Beamtalk wrapper provides: (1) message-send syntax, (2) automatic path validation, (3) actor-based lifecycle management, (4) structured error handling. The wrapper is thin — it delegates to Erlang internally.

### "ReadStream/WriteStream are unnecessary — collections have do:/collect:"
**Best argument:** Beamtalk collections already support full iteration. ReadStream adds mutable position state that doesn't fit BEAM's immutable model. WriteStream is just `inject:into:` with extra steps. Modern languages (Elixir, Rust) dropped positional streams in favor of iterators.

**Counter:** Legitimate point. ReadStream is most useful for parsing (sequential consumption with lookahead), which is a real use case. WriteStream for string building is genuinely convenient and avoids O(n²) string concatenation. If we skip these, we should provide `StringBuffer` or similar.

### "FileStream should be a value type, not an actor"
**Best argument:** Making FileStream an actor adds overhead (gen_server call per read/write) and complexity (supervision, mailbox). A simple value type wrapping a file handle would be simpler and faster.

**Counter:** On BEAM, file handles are already process-linked. Making FileStream an actor means the handle's lifecycle is tied to a supervised process — if the actor crashes, the file closes cleanly. This is idiomatic OTP. The gen_server overhead is negligible compared to actual I/O latency.

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
