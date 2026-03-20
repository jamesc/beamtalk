# ADR 0069: Make String a Subclass of Binary

## Status
Proposed (2026-03-20)

## Context

Beamtalk currently models `String` and `Binary` as unrelated classes:

```text
Object
├── Binary    (sealed, class methods only)
└── Collection
    └── String  (sealed, 60 instance methods)
```

ADR 0037 established this hierarchy with `String` as a direct subclass of `Collection`, explicitly noting "no intermediate subclasses for v0.1". `Binary` sits outside the `Collection` tree entirely — it inherits from `Object` and has no instance methods.

On BEAM, strings and binaries are the **same Erlang type** (`binary()`). Erlang's `is_binary("hello")` returns `true`. There is no runtime distinction.

This mismatch causes three problems:

1. **Type friction in File I/O.** `File writeBinary:contents:` declares its parameter as `Binary`, so `File writeBinary: "data.bin" contents: "hello"` triggers a type warning — even though `"hello"` IS a binary on BEAM. Tests require `@expect type` annotations to suppress these false positives.

2. **Binary has no instance methods.** It's a sealed utility class with only four class methods (`serialize:`, `deserialize:`, `size:`, `fromIolist:`). You can't send messages to a Binary value — you must pass it back to the `Binary` class. This is un-Smalltalk: objects should respond to messages.

3. **New byte-level methods have nowhere to live.** Exdura (EventStore) needs `part:size:` (zero-copy slicing), `deserializeWithUsed:` (ETF decode returning bytes consumed), `at:` (byte access), `concat:`, `fromBytes:`, `toBytes`, and `isEmpty`. These are instance-level operations on binary data. Adding them as more class methods on `Binary` creates an awkward `Binary part: someBin from: 0 size: 10` API instead of `someBin part: 0 size: 10`.

### The core tension

String methods operate at **grapheme cluster** level (`at:` returns the nth grapheme, `length` counts graphemes). Binary methods operate at **byte** level (`at:` returns the byte value at an offset, `size` counts bytes). These are genuinely different operations on the same underlying data — the same method name, different unit of iteration.

## Decision

Make `String` a subclass of `Binary`. Binary becomes a proper class with instance methods for byte-level operations. String inherits those methods and adds text-level operations on top.

### New class hierarchy

```text
Object
└── Collection
    └── Binary   (sealed, byte-level operations)
        └── String   (sealed, text-level operations)
```

This modifies ADR 0037's hierarchy by inserting `Binary` between `Collection` and `String`.

### Collection protocol on Binary

Binary becomes a `Collection` subclass, which means it must implement the three subclass-responsibility methods from ADR 0037:

| Collection method | Binary implementation |
|---|---|
| `do: block` | Iterate over bytes (each element is an Integer 0-255) |
| `size` | Byte count (`erlang:byte_size/1`) |
| `printString` | Hex representation for non-UTF-8, quoted string for valid UTF-8 |

Binary inherits all shared Collection methods (`collect:`, `select:`, `reject:`, `inject:into:`, `isEmpty`, `isNotEmpty`, `includes:`, `detect:`, etc.) for free. This enables powerful byte-level collection operations:

```beamtalk
data := (File readBinary: "payload.bin") unwrap
data collect: [:b | b bitAnd: 0x7F]   // mask high bits
data select: [:b | b > 0]             // filter null bytes
data includes: 0xFF                    // check for byte value
```

**String overrides** `do:`, `size`, and `printString` with grapheme-level semantics (as it does today). String's `size` counts graphemes; Binary's `size` counts bytes. Since String is the subclass, its overrides win for String instances.

### Binary instance methods (new)

```beamtalk
// Collection protocol — Binary is a collection of bytes
aBinary at: 1                        // => 104 (byte value, 1-based)
aBinary size                          // => 5 (byte count)
aBinary isEmpty                       // => false (inherited: self size =:= 0)

// Byte access (0-based, Erlang-compatible alias)
aBinary byteAt: 0                     // => 104 (same byte, 0-based offset)
aBinary byteSize                      // => 5 (same as size — alias for clarity on String)

// Slicing (zero-copy on BEAM)
aBinary part: 0 size: 3              // => Binary (first 3 bytes)

// Concatenation
aBinary concat: otherBinary           // => Binary

// Byte list conversion
aBinary toBytes                       // => #(104, 101, 108, 108, 111)
Binary fromBytes: #(104, 101)         // => Binary (class method)

// UTF-8 decoding (Binary → String)
aBinary asString                      // => Result ok: "hello" | Result error: "invalid UTF-8 at byte 3"
aBinary asStringUnchecked             // => "hello" (no validation — trust the caller)
```

Binary owns `at:` and `size` — they mean "byte at 1-based index" and "byte count". This follows the Collection protocol: every Collection defines `at:` (1-based element access) and `size` (element count). For Binary, the element is a byte (Integer 0-255).

`byteAt:` (0-based) and `byteSize` are **additional** methods that provide unambiguous byte access on either Binary or String. On Binary they're aliases; on String they're escape hatches past the grapheme-level overrides. The 0-based indexing on `byteAt:` matches Erlang's `binary:at/2` for BEAM developer familiarity.

**UTF-8 decoding:** `asString` validates the binary as UTF-8 and returns a `Result` — success gives a String, failure gives an error with the byte offset of the invalid sequence. `asStringUnchecked` skips validation and returns a String directly — use when you trust the source (e.g. data you serialized yourself). On a String receiver, both are no-ops returning the string itself. The direction is intentionally asymmetric: down (String→Binary) is implicit via the type hierarchy; up (Binary→String) requires explicit validation because not all byte sequences are valid UTF-8.

### Network protocol use case

The byte/grapheme duality is essential for protocol parsing. A single data stream may contain binary framing mixed with UTF-8 text:

```beamtalk
// Parse a length-prefixed UTF-8 message from raw bytes
packet := (Socket read: connection) unwrap   // => Binary
messageType := packet at: 1                   // => byte value (Binary's at:)
length := packet at: 2
payload := packet part: 2 size: length        // => Binary (zero-copy slice)

// Decode the payload as text
text := payload asString unwrap               // => String (validated UTF-8)
text size                                     // => grapheme count (String's size)
text at: 1                                    // => first grapheme (String's at:)

// Or build a response mixing text and binary framing
header := Binary fromBytes: #(0x01, text byteSize)
response := header concat: text              // String IS-A Binary, so concat: works
```

The key: `text` started as bytes, became a String via `asString`, and was passed back to `concat:` as a Binary (implicit, via subclass relationship). No manual conversion in either direction — just validation at the byte→text boundary.

### Binary class methods (kept, some become instance)

```beamtalk
// Serialization (class methods — operate on arbitrary values)
Binary serialize: #(1, 2, 3)              // => Binary (ETF bytes)
Binary deserialize: etfBinary             // => #(1, 2, 3)
Binary deserializeWithUsed: etfBinary     // => #(value, bytesConsumed)

// Construction (class methods)
Binary fromIolist: #("hello", " ", "world")  // => Binary
Binary fromBytes: #(104, 101, 108)           // => Binary
```

### String relationship to Binary

String overrides `at:` and `size` with grapheme semantics. All other Binary methods are inherited unchanged:

```beamtalk
// String overrides — grapheme semantics (same names, different unit)
"hello" at: 1          // => "h" (first grapheme, 1-based)
"hello" size           // => 5 (grapheme count — overrides Binary's byte count)
"hello" length         // => 5 (alias for size)
"cafe\u0301" size      // => 4 (graphemes, not bytes)

// Binary inherited — byte semantics (unambiguous, always available)
"hello" byteAt: 0      // => 104 (byte value, 0-based)
"hello" byteSize       // => 5 (byte count)
"cafe\u0301" byteSize  // => 6 (bytes, not graphemes)

// Collection protocol — String iterates graphemes, Binary iterates bytes
"hello" do: [:g | g]         // g is "h", "e", "l", "l", "o" (grapheme strings)
"hello" collect: [:g | g]    // => #("h", "e", "l", "l", "o")

// Slicing — Binary's part:size: works on bytes
"hello" part: 0 size: 3  // => Binary (raw bytes, not String)
```

### Method override table

Binary defines the Collection protocol (`at:`, `size`) with byte semantics. String overrides with grapheme semantics. The `byte`-prefixed methods are always unambiguous regardless of receiver type:

| Method | On Binary | On String |
|---|---|---|
| `at: index` | byte value (1-based) | grapheme (1-based, override) |
| `size` | byte count | grapheme count (override) |
| `length` | — | alias for `size` |
| `byteAt: offset` | byte value (0-based) | inherited — byte value (0-based) |
| `byteSize` | byte count (alias for `size`) | inherited — byte count |
| `do: block` | iterate bytes | iterate graphemes (override) |
| `part: offset size: n` | byte-level slice → Binary | inherited — byte-level slice → Binary |
| `concat:` | byte concatenation → Binary | inherited — byte concatenation → Binary |
| `asString` | Result (UTF-8 validation) | no-op → self |
| `asStringUnchecked` | self (cast, no validation) | no-op → self |
| `isEmpty` | inherited from Collection | inherited from Collection |

The key insight: `at:` and `size` follow the Collection contract on both classes — "access the nth element" and "count elements." The element is a byte on Binary and a grapheme on String. This is the same override pattern as `do:` (iterate bytes vs iterate graphemes).

The existing `Binary size:` class method (taking a binary as argument) is **removed** — use `aBinary size` or `aBinary byteSize` instead.

### Return types: `concat:` and `part:size:` on String

When String inherits Binary methods, return types need attention:

- **`part:size:`** always returns `Binary`, even on a String receiver. `"hello" part: 0 size: 3` returns a Binary containing `hel`. This is intentional — byte-level slicing may split a multi-byte grapheme, producing invalid UTF-8. The result is raw bytes, not text. Users wanting text slicing should use `take:`/`drop:` (grapheme-aware).

- **`concat:`** on Binary returns `Binary`. String inherits this unchanged. For string concatenation, users should continue using `++` (which returns String). `"hello" concat: "world"` returns a Binary — this is the byte-level operation. `"hello" ++ "world"` returns a String — this is the text-level operation.

- **`collect:`** (from Collection) returns a `List` of byte integers when called on Binary, and a `List` of single-grapheme Strings when called on String. (String overrides `collect:` to return a `String` — Binary could do the same for byte-preserving transforms, but the default List behavior is acceptable for v1.)

### File I/O — no conversion needed

Beamtalk's type checker uses **nominal subtype checking** — if a parameter is typed as `Binary`, any subclass of `Binary` (including `String`) is accepted. With String as a subclass of Binary:

```beamtalk
// These all type-check cleanly — no @expect type needed
File writeBinary: "data.bin" contents: "hello"
File appendBinary: "log.bin" contents: " world"

// readBinary: returns Binary (could be non-UTF-8)
data := (File readBinary: "image.png") unwrap
data size            // => 1024 (byte count — Binary's size)
data part: 0 size: 4 // => Binary (magic bytes)
```

Text-mode I/O (`readAll:`, `writeAll:contents:`) continues to use `String` parameters and return types. The distinction is about intent and encoding assumptions, not a type barrier.

## Prior Art

### Erlang/OTP
`<<"hello">>` is a binary. Erlang historically distinguished "strings" (lists of character codepoints) from binaries, but the modern `string` module (OTP 20+) operates on UTF-8 binaries as the preferred representation. Binary pattern matching (`<<H:8, Rest/binary>>`) operates at byte level on any binary, including strings. In modern Erlang, the binary IS the string — the charlist representation is legacy.

### Elixir
`String` module operates on UTF-8 binaries. Binaries are a primitive type, not a class. `String.length("cafe\u0301")` counts graphemes; `byte_size("cafe\u0301")` counts bytes. Both work on the same value. Elixir explicitly models the bytes/graphemes duality on the same underlying type — this ADR brings that same duality to Beamtalk's class system.

### Gleam
Gleam (BEAM language) has `String` and `BitArray` as separate types with no subtype relationship. `bit_array.from_string("hello")` converts explicitly. This is the "siblings with conversion" approach that Alternative B considers — it works for Gleam because Gleam has a static type system, but Beamtalk's dynamic dispatch makes the conversion friction more painful.

### Smalltalk (ANSI / Squeak / Pharo)
`ByteArray` and `String` are both subclasses of `ArrayedCollection`. `String` is NOT a subclass of `ByteArray`. However, Squeak/Pharo have `ByteString` (a subclass of `String`) that stores one-byte-per-character, blurring the line. Crucially, Smalltalk runs on VMs where strings and byte arrays have different internal representations — the separation reflects a real implementation distinction. On BEAM, no such distinction exists.

### Ruby
`String` has both text methods (`length`, `chars`) and byte methods (`bytesize`, `bytes`, `getbyte`). Single class handles both levels. Encoding is a property of the string instance. Ruby's `bytesize`/`getbyte` naming convention inspired our `byteSize`/`byteAt:` naming for unambiguous byte access on String.

### Go
`string` and `[]byte` are distinct types with explicit conversion (`[]byte("hello")`, `string(bytes)`). Conversion copies the data. This model doesn't fit BEAM where the conversion is a no-op.

### Python
`str` and `bytes` are completely separate types (since Python 3). `"hello".encode()` / `b"hello".decode()`. This clean separation caused significant migration pain (Python 2→3). The strict separation makes sense for Python's in-memory representation but not for BEAM where they're identical.

### Newspeak
Strings are immutable sequences of characters. No separate binary type in the core language.

### Assessment
BEAM languages universally treat strings as binaries. Languages with separate string/binary types (Go, Python, Gleam) either have different in-memory representations or static type systems that make the separation low-friction. Beamtalk has neither — on BEAM they're the same type, and dynamic dispatch makes explicit conversion ceremonies painful. Our hierarchy reflects the BEAM reality: String IS-A Binary, with a text-level API on top.

## User Impact

### Newcomer
Simpler mental model — strings are a kind of binary, which matches what every BEAM tutorial teaches. `"hello" byteSize` and `"hello" byteAt: 0` are discoverable via tab completion. No confusing type errors when passing strings to binary APIs. The one downside: seeing `Binary` as String's superclass in `:help String` might be surprising if they come from Python/Go where str and bytes are separate.

### Smalltalk developer
The most unfamiliar change — Smalltalk separates String from ByteArray. But the `byteAt:`/`byteSize` naming follows Smalltalk's `ByteArray` conventions, so the method names feel familiar even if the hierarchy doesn't. The key mental shift: on BEAM, the VM doesn't have separate string and byte-array types, so the Smalltalk separation would be purely artificial.

### Erlang/BEAM developer
Natural — matches the Erlang reality they already know. `at:` and `size` are the Collection protocol; `byteAt:` (0-based) maps to `binary:at/2`. `part:size:` maps to `binary:part/3`. No artificial String/Binary barrier. Collection protocol on Binary (`collect:`, `select:`) is a bonus they wouldn't get from raw Erlang.

### Operator (Exdura)
Unblocked — `part:size:` and `deserializeWithUsed:` enable dropping the EventStore FFI layer. Binary slicing and ETF decoding work directly on values returned from `File readBinary:`. Binary owns `at:` and `size`, so EventStore code is terse: `aBinary at: 1`, `aBinary size`, `aBinary part: 0 size: 10`. Collection protocol means byte-level data processing can use familiar `select:`/`collect:` patterns.

## Steelman Analysis

### "Keep Binary as a utility class, just add more class methods"
**Best case:** This is the *least disruptive* path. Zero hierarchy changes, zero migration, zero risk of breaking String's 60-method surface area. Add 7 class methods to Binary, ship in a day, Exdura is unblocked. The API is `Binary part: bin from: 0 size: 10` — verbose, but explicit about which binary you're operating on. The File I/O type friction is a *separate* problem with a separate fix (`@expect type`, or change the param type to `Object`). Bundling them conflates urgency (Exdura is blocked now) with design purity (hierarchy aesthetics). An incremental approach — add utility methods now, reconsider hierarchy later — is lower risk.

**Why rejected:** The "ship fast, reconsider later" argument is tempting but creates tech debt we'd immediately want to repay. Utility-class methods would need to be migrated to instance methods once the hierarchy changes, doubling the work. More fundamentally, `Binary part: bin from: 0 size: 10` is not just verbose — it's un-Smalltalk. Objects should respond to messages about themselves. Every other value type in Beamtalk has instance methods; Binary being the exception is a design gap, not a feature. And the File I/O friction isn't separable — it's the same root cause (String and Binary are unrelated to the type checker despite being the same runtime type).

### "Keep String and Binary as siblings, add conversion methods"
**Best case:** Python 3's str/bytes split is widely considered one of the *best* decisions in language design history. It forced every developer to think about encoding, and that thinking prevented real bugs. The "conversion is a no-op on BEAM" argument proves too much — if the runtime doesn't distinguish them, that's exactly *why* the language should. The type system is where we ADD safety the runtime doesn't give us. Gleam targets the same BEAM and chose separation; they're not wrong. Explicit `aString asBinary` makes every byte-level operation intentional. You never accidentally treat text as bytes or bytes as text. The `@expect type` annotations in tests aren't friction — they're the type system doing its job, catching a category error.

**Why rejected:** The Python analogy breaks down on BEAM. Python 3's split works because `str` and `bytes` have genuinely different in-memory representations (UTF-32/UCS-4 vs raw bytes) — the conversion does real work. On BEAM, `aString asBinary` would be a no-op that exists solely to satisfy the type checker. Gleam makes separation work because its static type system catches mismatches at compile time with zero ceremony — no `@expect type`, no `asBinary`. Beamtalk's dynamic dispatch means the ceremony is paid at every call site, not just once in a type signature. And the category error the type system "catches" (`File writeBinary:contents: "hello"`) is not actually an error — writing a string to a file as binary data is a perfectly valid operation on BEAM.

### "Make Binary a subclass of String instead"
**Best case:** The Liskov argument in the ADR cuts both ways. Under the chosen design, `"hello" part: 0 size: 3` returns a Binary that can't respond to `length` — is that not also a substitutability violation from the user's perspective? They started with a String, called a method, and got back something that lost most of String's API. Meanwhile, Binary-subclass-of-String means every binary value can respond to `split:`, `replaceAll:with:`, `uppercase` — and on BEAM, these *work*. Erlang's `string:uppercase(SomeBinary)` doesn't crash on arbitrary bytes, it just returns bytes. Saying these operations are "nonsensical" is the language designer being paternalistic about what BEAM developers should do with their own data.

**Why rejected:** "It works on BEAM" and "it makes sense" are different claims. `string:uppercase(<<0, 255, 128>>)` does return a value without crashing — but the result is meaningless. A language that lets you `capitalize` arbitrary serialized data isn't being permissive, it's being misleading. The Liskov argument about `part:size:` is valid — and the ADR addresses it explicitly: `part:size:` is a byte-level operation that intentionally drops to Binary, just as `aList asSet` drops to Set. The API signals "you've left the text domain." Making Binary subclass String would mean `Binary serialize: 42` produces something that responds to `isEmpty` with "no, I'm not empty" and to `capitalize` with garbage — that's not empowerment, it's a trap.

### "Don't put Binary under Collection — keep it as Object subclass with instance methods"
**Best case:** Every other Collection's `select:` returns the same type: `aList select:` returns a List, `aSet select:` returns a Set, `aDictionary select:` returns a Dictionary. Binary breaks this contract — `aBinary select: [:b | b > 0]` returns a *List* of integers, not a Binary. That's not a minor ergonomic issue, it's a fundamental Collection protocol violation. Users who write generic code over Collections will be surprised when their `select:` pipeline produces a different type for one specific subclass. And "Binary is a collection of bytes" is a leaky abstraction — nobody thinks of a JPEG as a "collection" the way they think of a List or Dictionary as one. The Collection methods add API surface nobody asked for, creating discoverability noise (`:help Binary` showing `anySatisfy:`, `detect:ifNone:`, etc.).

**Why rejected:** The `select:` return type concern is the strongest argument here. In practice, every concrete Collection subclass (List, Array, String, Set) overrides `select:` to return its own type — so yes, Binary returning a List would be the odd one out. We could mitigate this by having Binary override `select:` to return a Binary (collecting bytes back into a binary), matching the pattern. But even without that optimization, the default Collection `select:` (which returns a List) is functional if not ideal. The discoverability concern is real but manageable — `:help Binary` can group inherited Collection methods separately. The deeper issue: if Binary is NOT a Collection but String IS, and String subclasses Binary, then Binary must be a Collection. Single inheritance makes this non-negotiable. The alternative — String not subclassing Binary — gives up the core benefit of the ADR.

## Alternatives Considered

### A. Add `Object` param type to File binary I/O
Change `writeBinary:contents:` from `Binary` to `Object`. Removes type errors but loses all type safety. Doesn't solve the "Binary has no instance methods" problem. Doesn't address Exdura's needs.

### B. Merge String and Binary into one class
Collapse everything into `String` with both grapheme and byte methods. Simpler hierarchy but `Binary serialize: x` returning a "String" is semantically misleading. Loses the ability to distinguish "I know this is text" from "this is opaque bytes" in method signatures.

### C. Protocol/trait-based approach
Define a `ByteSequence` protocol that both String and Binary implement. Beamtalk doesn't have protocols yet (planned but not implemented). Even when available, the BEAM reality is that they ARE the same type, so a protocol adds abstraction without reflecting the underlying truth.

### D. Binary as Object subclass with instance methods (not under Collection)
Give Binary instance methods but don't make it a Collection. String would subclass Binary directly, and Binary would subclass Object. This avoids the "is a binary a collection?" question.

Rejected because String is already a Collection — if Binary is between Collection and String in the hierarchy, it must also be a Collection. You could break the chain (Collection > String, Object > Binary > ???) but then String can't be a subclass of Binary and Collection simultaneously (single inheritance). The whole point is String IS-A Binary, and String IS-A Collection — so Binary must be a Collection too.

## Consequences

### Positive
- Type checker accepts String wherever Binary is expected — eliminates false-positive warnings
- Binary values respond to messages directly (`aBinary size` vs `Binary size: aBinary`)
- Exdura gets the methods it needs as natural instance methods
- File binary I/O works without `@expect type` annotations
- Matches BEAM reality — no artificial type barrier
- Binary owns `at:` and `size` (terse for Exdura); `byteAt:`/`byteSize` provide unambiguous byte access on String
- Collection protocol on Binary enables `collect:`, `select:`, `inject:into:` over bytes

### Negative
- String gains byte-level methods it didn't have before (`byteAt:`, `byteSize`, `part:size:`, `concat:`) — minor, they're useful
- Hierarchy change: `String` moves from `Collection > String` to `Collection > Binary > String` — any code checking `superclass` will see `Binary` instead of `Collection`
- `Binary size:` class method removed in favor of `size` instance method — breaking change for existing `Binary size: x` call sites (only 2 files affected: `stdlib/src/Binary.bt` and `stdlib/test/binary_test.bt`)
- Collection methods on Binary may surprise users: `aBinary collect: [:b | b]` returns a List of integers, not a Binary — the return type changes when going from collection back to concrete type
- Modifies ADR 0037's hierarchy — inserting a layer the original design explicitly avoided

### Neutral
- `readBinary:` return type stays `Binary` (the value might not be valid UTF-8)
- `readAll:` return type stays `String` (encoding is assumed/validated)
- `Collection` remains in the chain — `Binary` is a subclass of `Collection`, so String still IS-A Collection
- Serialization methods remain class methods on Binary (they construct, not query)
- `String fromIolist:` class method already exists alongside `Binary fromIolist:` — both continue to work, returning their respective types

## Implementation

### Phase 1: Binary class hierarchy change
- Move `Binary` under `Collection` in `generated_builtins.rs`
- Make `String` a subclass of `Binary` instead of `Collection`
- Implement Collection's three subclass-responsibility methods on Binary:
  - `do:` — iterate bytes via `beamtalk_binary:do/2`
  - `size` — byte count via `erlang:byte_size/1`
  - `printString` — hex representation or quoted if valid UTF-8
- Add instance methods to `Binary`: `at:`, `byteAt:`, `byteSize`, `part:size:`, `concat:`, `toBytes`, `asString`, `asStringUnchecked`
- Add class methods: `fromBytes:`
- Implement Erlang runtime functions in `beamtalk_binary.erl`
- Migrate `Binary size:` class method → `size` instance method (with deprecation on old class method)

### Phase 2: Serialization methods
- Add `deserializeWithUsed:` class method
- Keep `serialize:`, `deserialize:`, `fromIolist:` as class methods

### Phase 3: File I/O cleanup
- Remove `@expect type` annotations from `file_binary_io_test.bt`
- Verify type checker accepts String for Binary params throughout

### Phase 4: String method audit
- Verify String's `at:`, `size`, `length`, `do:` overrides work correctly with Binary parent (all should already be in place — String currently defines these)
- Verify `byteSize` and `byteAt:` are inherited correctly on String (no override needed — Binary's byte-level semantics are correct for byte access on strings)

## Migration Path

### Breaking changes
1. `Binary size: x` → `x size` (class method to instance method)
2. `String` superclass changes from `Collection` to `Binary` (only affects reflection)

### Migration steps
1. Add deprecation warning on `Binary size:` class method pointing to instance `size`
2. Update all `Binary size:` call sites in stdlib and examples (2 files: `Binary.bt`, `binary_test.bt`)
3. Remove deprecated `Binary size:` class method after one release cycle

## References
- Related issues: BT-1555 (File binary I/O), Exdura EventStore FFI removal
- Related ADRs: ADR 0023 (String interpolation — establishes "strings are binaries" model), ADR 0037 (Collection class hierarchy — establishes current String position)
- Documentation: `docs/beamtalk-language-features.md` (String section, Binary section)
