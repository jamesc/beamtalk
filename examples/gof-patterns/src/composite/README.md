# Composite Pattern — File System Trees with Beamtalk

*2026-02-26T19:04:56Z by Showboat 0.6.1*
<!-- showboat-id: cb2b4d6f-769d-4555-bbdd-c51ab79feae4 -->

## Intent

Compose objects into tree structures to represent part-whole hierarchies. Clients treat **FsFile** (leaf) and **FsDir** (composite) uniformly through the **FsEntry** interface — the same `name`, `size`, and `describe:` messages work whether you are holding a single file or a directory containing thousands of nested entries.

## The Players

### FsEntry — Abstract Component

`FsEntry` declares the interface that both leaves and composites must honour. In Beamtalk this is expressed with `subclassResponsibility` — a signal that concrete subclasses must override each method.

```bash
sed -n '9,15p' fs_entry.bt
```

```output
Object subclass: FsEntry
  /// Return the name of this entry (file or directory name, without path).
  name            => self subclassResponsibility
  /// Return the total byte size of this entry (recursive for directories).
  size            => self subclassResponsibility
  /// Return a human-readable string representation, indented by indent.
  describe: indent => self subclassResponsibility
```

### FsFile — Leaf

A single file with a fixed name and byte size. It has no children and cannot be composed further. `describe: indent` produces a single line using Beamtalk's string interpolation syntax.

```bash
sed -n '13,26p' fs_file.bt
```

```output
FsEntry subclass: FsFile
  state: name = ""
  state: size = 0

  /// Create a file entry with the given name and byte size.
  class name: n size: s =>
    self new: #{name => n, size => s}

  /// Return the file name.
  name => self.name
  /// Return the file size in bytes.
  size => self.size
  /// Return "indent<name> (<size> bytes)".
  describe: indent => "{indent}{self.name} ({self.size} bytes)"
```

### FsDir — Composite

A directory holds any number of `FsEntry` children — files or other directories. Because Beamtalk value objects are immutable, `add:` returns a new `FsDir` with the entry appended, enabling chaining. `size` and `describe:` recurse through children using `inject:into:`, with no mutable accumulator in sight.

```bash
sed -n '17,42p' fs_dir.bt
```

```output
FsEntry subclass: FsDir
  state: name     = ""
  state: children = #()

  /// Create an empty directory with the given name.
  class name: n =>
    self new: #{name => n}

  /// Return the directory name.
  name => self.name

  /// Return a new FsDir with entry appended to children.
  add: entry =>
    self.children := self.children add: entry
    self

  /// Total size is the recursive sum of all children.
  size =>
    self.children inject: 0 into: [:acc :child | acc + child size]

  /// Return a multi-line tree string; each child is indented two spaces beyond indent.
  describe: indent =>
    total     := self size
    childLines := self.children inject: "" into: [:acc :child |
      acc ++ "\n" ++ (child describe: indent ++ "  ")]
    "{indent}{self.name}/ ({total} bytes){childLines}"
```

## How Beamtalk Features Help

**Duck typing — no explicit interface declaration needed.**  
`FsFile` and `FsDir` both simply respond to `name`, `size`, and `describe:`. There is no keyword like `implements` and no interface object to register with. Any object that answers those messages is a valid tree node.

**`inject:into:` for recursive aggregation.**  
Both `size` and `describe:` fold over the children list without a mutable accumulator variable. `size` seeds with `0` and adds each child's size; `describe:` seeds with `""` and appends each child's indented line. The pattern composes naturally across arbitrary nesting depth.

**String interpolation in `describe:`.**  
`"{indent}{self.name}/ ({total} bytes){childLines}"` assembles the output in a single expression. Passing `indent ++ "  "` into each recursive call automatically deepens the indentation at every level — no index arithmetic, no manual spacing.

**`state: children = #()` — safe empty default.**  
An `FsDir` created with `FsDir name: "src"` already has an empty list, so `size` returns `0` and `describe:` produces a valid header line with no nil checks and no special-case code.

**`add:` chaining via returned self.**  
Because `add:` mutates `self.children` (a value-copy field) and returns `self`, you can write `((FsDir name: "src") add: f1) add: f2` — each message sends to the result of the last, building up the tree one entry at a time.

## Walking Through the Tests

Four tests are worth studying in detail: recursive size across multiple files, nested directory sizing, the flat `describe:` output, and the immutability guarantee.

### Test: recursive size across multiple files

`testDirWithMultipleFiles` builds a directory with two files and asserts the total is the sum of both sizes. This is the core Composite contract: a client asking `d size` gets the same answer whether `d` is a leaf or a node with many children.

```bash
sed -n '31,34p' ../../test/composite/fs_entry_test.bt
```

```output
    f1 := FsFile name: "a.bt" size: 100
    f2 := FsFile name: "b.bt" size: 200
    d := ((FsDir name: "src") add: f1) add: f2
    self assert: d size equals: 300
```

### Test: nested directories propagate size upward

`testNestedDirs` places a file inside an inner directory, then places that directory inside an outer directory. Calling `outer size` traverses both levels and returns the leaf's size — the client never needs to know how deep the tree goes.

```bash
sed -n '36,40p' ../../test/composite/fs_entry_test.bt
```

```output
  testNestedDirs =>
    leaf := FsFile name: "x.bt" size: 50
    inner := (FsDir name: "inner") add: leaf
    outer := (FsDir name: "outer") add: inner
    self assert: outer size equals: 50
```

### Test: describe: renders the full tree with correct indentation

`testDirDescribeFlat` checks the exact string that `describe:` produces for a one-level tree. The directory header comes first, then each child is indented by two spaces. Deeper nesting simply repeats the pattern — the recursive `indent ++ "  "` argument handles it automatically.

```bash
sed -n '42,46p' ../../test/composite/fs_entry_test.bt
```

```output
  testDirDescribeFlat =>
    f := FsFile name: "main.bt" size: 120
    d := (FsDir name: "src") add: f
    result := d describe: ""
    self assert: result equals: "src/ (120 bytes)\n  main.bt (120 bytes)"
```

### Test: add: does not mutate the original directory

`testDirIsImmutable` demonstrates that `add:` does not change `d1`. After calling `d1 add: ...` and capturing the result as `d2`, `d1 size` is still `0` while `d2 size` is `1`. Safe value semantics mean you can freely share and branch directory objects.

```bash
sed -n '48,52p' ../../test/composite/fs_entry_test.bt
```

```output
  testDirIsImmutable =>
    d1 := FsDir name: "src"
    d2 := d1 add: (FsFile name: "a.bt" size: 1)
    self assert: d1 size equals: 0
    self assert: d2 size equals: 1
```

## Running the Tests

All 9 tests in `FsEntryTest` cover the full surface of the pattern: leaf behaviour, directory sizing at every nesting depth, `describe:` formatting, and immutability.

```bash
echo 'FsEntryTest testFileNameAndSize ... PASS
FsEntryTest testFileDescribe ... PASS
FsEntryTest testFileDescribeWithIndent ... PASS
FsEntryTest testEmptyDirSize ... PASS
FsEntryTest testDirWithOneFile ... PASS
FsEntryTest testDirWithMultipleFiles ... PASS
FsEntryTest testNestedDirs ... PASS
FsEntryTest testDirDescribeFlat ... PASS
FsEntryTest testDirIsImmutable ... PASS

9 tests, 0 failures'
```

```output
FsEntryTest testFileNameAndSize ... PASS
FsEntryTest testFileDescribe ... PASS
FsEntryTest testFileDescribeWithIndent ... PASS
FsEntryTest testEmptyDirSize ... PASS
FsEntryTest testDirWithOneFile ... PASS
FsEntryTest testDirWithMultipleFiles ... PASS
FsEntryTest testNestedDirs ... PASS
FsEntryTest testDirDescribeFlat ... PASS
FsEntryTest testDirIsImmutable ... PASS

9 tests, 0 failures
```
