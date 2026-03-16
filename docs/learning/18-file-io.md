<!-- btfixture: fixtures/ch18file_io.bt -->

## File I/O

Beamtalk's `File` class provides reading, writing, directory operations, and
lazy file streams. All operations return `Result` values — check `isOk` /
`isError` or call `unwrap` to extract the value.

## Reading and writing files

Write a file with `writeAll:contents:` and read it back with `readAll:`:

```beamtalk
TestCase subclass: Ch18WriteRead
  state: tmpDir = "target/bt-doctest-tmp/ch18wr"
  setUp =>
    File mkdirAll: self.tmpDir
    self
  tearDown =>
    File deleteAll: self.tmpDir
    self

  testWriteAndRead =>
    path := self.tmpDir ++ "/greeting.txt"
    File writeAll: path contents: "Hello, Beamtalk!"
    content := (File readAll: path) unwrap
    self assert: content equals: "Hello, Beamtalk!"
```

`writeAll:contents:` creates parent directories automatically and overwrites
any existing file. `readAll:` returns a `Result` — if the file doesn't exist,
you get an error:

```beamtalk
TestCase subclass: Ch18MissingFile
  testMissing =>
    result := File readAll: "no/such/file.txt"
    self assert: result isError
```

## Checking files

```beamtalk
File exists: "docs/learning/README.md"      // => true
File isFile: "docs/learning/README.md"       // => true
File isDirectory: "docs/learning/README.md"  // => false
File isDirectory: "docs/learning"            // => true
```

## Directory operations

Create directories, list contents, and clean up:

```beamtalk
TestCase subclass: Ch18Directories
  state: tmpDir = "target/bt-doctest-tmp/ch18dirs"
  setUp =>
    File mkdirAll: self.tmpDir
    self
  tearDown =>
    File deleteAll: self.tmpDir
    self

  testMkdirAndList =>
    // Create a nested directory structure
    File mkdirAll: self.tmpDir ++ "/a/b"

    // Write some files
    File writeAll: self.tmpDir ++ "/readme.txt" contents: "hi"
    File writeAll: self.tmpDir ++ "/notes.txt" contents: "note"

    // List directory contents (returns a list of filenames)
    entries := (File listDirectory: self.tmpDir) unwrap
    self assert: (entries printString includesSubstring: "readme.txt")
    self assert: (entries printString includesSubstring: "notes.txt")

  testDeleteAndRename =>
    path := self.tmpDir ++ "/temp.txt"
    File writeAll: path contents: "temporary"
    self assert: (File exists: path)

    // Delete a file
    File delete: path
    self deny: (File exists: path)

    // Rename a file
    src := self.tmpDir ++ "/old.txt"
    dst := self.tmpDir ++ "/new.txt"
    File writeAll: src contents: "data"
    File rename: src to: dst
    self assert: (File readAll: dst) unwrap equals: "data"
```

## Useful path helpers

```beamtalk
cwd := File cwd          // => _
cwd class                 // => String
File tempDirectory class  // => String
```

## File streams

For large files, use streams instead of reading everything into memory.
`File open:do:` gives you a handle that auto-closes when the block completes:

```beamtalk
TestCase subclass: Ch18Streams
  state: tmpDir = "target/bt-doctest-tmp/ch18streams"
  state: nl = ""
  setUp =>
    self.nl := String fromCodePoint: 10
    File mkdirAll: self.tmpDir
    File writeAll: self.tmpDir ++ "/data.txt"
      contents: "one" ++ self.nl ++ "two" ++ self.nl ++ "three" ++ self.nl ++ "four" ++ self.nl ++ "five"
    self
  tearDown =>
    File deleteAll: self.tmpDir
    self

  testReadAllLines =>
    // Read all lines from a file
    lines := (File open: self.tmpDir ++ "/data.txt" do: [:h |
      h lines asList
    ]) unwrap
    self assert: lines size equals: 5
    self assert: (lines at: 1) equals: "one"
    self assert: (lines at: 5) equals: "five"

  testLazyTake =>
    // take: only reads the first N lines — lazy evaluation
    count := (File open: self.tmpDir ++ "/data.txt" do: [:h |
      (h lines take: 2) inject: 0 into: [:c :_ | c + 1]
    ]) unwrap
    self assert: count equals: 2

  testFilterLines =>
    // select: filters lazily
    result := (File open: self.tmpDir ++ "/data.txt" do: [:h |
      (h lines select: [:l | l size <= 3]) asList
    ]) unwrap
    self assert: result size equals: 3
```

Stream operations are **lazy** — `take:` and `select:` don't read the whole
file. Use `asList` to materialize results when you need them.

## Error handling

All `File` operations return `Result`. Use the standard Result methods
(see chapter 12):

```beamtalk
TestCase subclass: Ch18Errors
  testResultMethods =>
    ok := File readAll: "docs/learning/README.md"
    self assert: ok isOk

    err := File readAll: "does/not/exist.txt"
    self assert: err isError
    self deny: err isOk
```

## Security

File operations rely on OS-level permissions — there are no path restrictions.
Both absolute and relative paths are accepted (ADR 0063). Beamtalk is a trusted
developer tool; production deployments should use OS sandboxing as needed.

## Summary

**Reading & writing:**

```text
File readAll: path                    → Result<String>
File writeAll: path contents: string  → Result<ok>
File exists: path                     → Boolean
```

**Directories:**

```text
File mkdir: path          → Result (single level)
File mkdirAll: path       → Result (creates parents)
File listDirectory: path  → Result<List<String>>
File delete: path         → Result (file or empty dir)
File deleteAll: path      → Result (recursive)
File rename: src to: dst  → Result
```

**Queries:**

```text
File isFile: path       → Boolean
File isDirectory: path  → Boolean
File cwd                → String
File tempDirectory      → String
File absolutePath: path → Result<String>
```

**Streams:**

```text
File open: path do: [:handle | ...]   → Result<block return value>
File lines: path                      → Result<Stream>
handle lines                          → Stream of line Strings
stream asList                         → List
stream take: n                        → Stream (lazy)
stream select: [:l | ...]             → Stream (lazy)
stream inject: init into: [:a :l | …] → value
```

## Exercises

**1. Write and read back.** Write `"Beamtalk is fun!"` to a temporary file, read
it back, and verify the contents match. Don't forget cleanup.

<details>
<summary>Hint</summary>

```text
// In a TestCase with setUp/tearDown for temp dir:
path := tmpDir ++ "/test.txt"
File writeAll: path contents: "Beamtalk is fun!"
content := (File readAll: path) unwrap
self assert: content equals: "Beamtalk is fun!"
```
</details>

**2. File vs directory.** Use `File isFile:` and `File isDirectory:` to check
both a file path and a directory path. How do the two predicates differ from
`File exists:`?

<details>
<summary>Hint</summary>

```text
File exists: "docs/learning/README.md"       // => true
File isFile: "docs/learning/README.md"       // => true
File isDirectory: "docs/learning/README.md"  // => false
File isDirectory: "docs/learning"            // => true
```

`exists:` returns true for both files and directories. `isFile:` and
`isDirectory:` distinguish between them.
</details>

**3. Lazy line counting.** Use `File open:do:` and stream operations to count
the number of lines in a file without reading the entire contents into memory.

<details>
<summary>Hint</summary>

```text
count := (File open: path do: [:h |
  h lines inject: 0 into: [:c :_ | c + 1]
]) unwrap
```

The stream is lazy — `inject:into:` processes one line at a time.
</details>

Next: Chapter 19 — Regular Expressions
