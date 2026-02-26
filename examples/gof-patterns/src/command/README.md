# Command Pattern — Undo/Redo with Beamtalk

*2026-02-26T18:35:34Z by Showboat 0.6.1*
<!-- showboat-id: 45bfcda7-a093-4a3a-8f53-526f62735b68 -->

## Intent

The **Command** pattern encapsulates a request as an object, letting you:

- Parameterise clients with different operations
- Queue, log, or replay requests
- Support **undoable operations** — the main focus of this example

The classic trick for undo is snapshot-based: before executing each command, save the current state. Undo simply restores the most recent snapshot. No command needs to implement its own reverse logic.

## The Players

There are four roles: **abstract command**, **concrete commands**, **receiver**, and **invoker**.

### Abstract Command — TextCommand

Declares a single method `execute:` that takes a receiver and returns the modified receiver. `subclassResponsibility` is Beamtalk's way of marking a method that subclasses must override — it raises an error if called on the abstract class directly.

```bash
sed -n '9,11p' text_command.bt
```

```output
Object subclass: TextCommand
  /// Apply this command to buffer and return the resulting buffer.
  execute: buffer => self subclassResponsibility
```

### Concrete Commands — InsertCommand and DeleteCommand

Each concrete command stores its parameters at construction time via a class-side factory method. `execute:` simply delegates to the receiver (TextBuffer) using those stored parameters.

```bash
sed -n '7,17p' insert_command.bt
```

```output
TextCommand subclass: InsertCommand
  state: position = 1
  state: text     = ""

  /// Create a command that will insert str at 1-based position pos.
  class at: pos insert: str =>
    self new: #{position => pos, text => str}

  /// Insert this command's text into buffer at the stored position.
  execute: buffer => buffer insertAt: self.position text: self.text
```

```bash
sed -n '8,17p' delete_command.bt
```

```output
TextCommand subclass: DeleteCommand
  state: position = 1
  state: length   = 0

  /// Create a command that will delete len characters starting at 1-based position pos.
  class from: pos length: len =>
    self new: #{position => pos, length => len}

  /// Delete this command's character range from buffer.
  execute: buffer => buffer deleteFrom: self.position length: self.length
```

### Receiver — TextBuffer

TextBuffer holds the text being edited. Both mutation methods (`insertAt:text:` and `deleteFrom:length:`) **return `self`** after updating state.

A key Beamtalk feature at work here: **value object semantics**. Because `self.text := ...` mutates and returns the same object, CommandHistory can store a reference to each successive buffer state as a snapshot. This is what makes snapshot-based undo work without any copying.

```bash
sed -n '12,34p' text_buffer.bt
```

```output
Object subclass: TextBuffer
  state: text = ""

  /// Create a buffer pre-populated with the given string.
  class withText: t =>
    self new: #{text => t}

  /// Return the current text content of the buffer.
  text => self.text

  /// Return a new buffer with str inserted at position pos (1-based).
  insertAt: pos text: str =>
    before := self.text take: pos - 1.
    after  := self.text drop: pos - 1.
    self.text := before ++ str ++ after.
    self

  /// Return a new buffer with len characters removed starting at pos (1-based).
  deleteFrom: pos length: len =>
    before := self.text take: pos - 1.
    after  := self.text drop: pos - 1 + len.
    self.text := before ++ after.
    self
```

### Invoker — CommandHistory

CommandHistory is the heart of the pattern. It owns the buffer and a history stack of prior buffer snapshots. Before every `execute:`, it pushes the current buffer onto the stack. `undo` pops the stack and restores the previous buffer.

Notice the early return `^self` when the history is empty — Beamtalk's non-local return syntax makes the guard clause clean and explicit.

```bash
sed -n '16,40p' command_history.bt
```

```output
Object subclass: CommandHistory
  state: buffer  = nil
  state: history = #()   // stack of prior buffer snapshots

  /// Create a history whose initial buffer state is aBuffer.
  class on: aBuffer =>
    self new: #{buffer => aBuffer}

  /// Execute a command, save the current buffer as a snapshot, update the buffer.
  execute: command =>
    self.history := self.history add: self.buffer.
    self.buffer  := command execute: self.buffer.
    self

  /// Restore the most recent snapshot, removing it from the stack.
  undo =>
    self.history isEmpty ifTrue: [^self].
    self.buffer  := self.history last.
    self.history := self.history take: self.history size - 1.
    self

  /// Return the current buffer state.
  buffer   => self.buffer
  /// Return true if there is at least one snapshot available to undo.
  canUndo  => self.history isNotEmpty
```

## How Beamtalk Features Help

**`subclassResponsibility`** — one line declares an abstract interface. No abstract class boilerplate, no annotations, no separate interface file. Calling it on the base class raises an informative error immediately.

**Keyword messages** — `InsertCommand at: 6 insert: " world"` and `CommandHistory on: buf` read like English. The intent is clear without any named-parameter overhead.

**Mutable value objects with `self` return** — `insertAt:text:` mutates the buffer's internal string and returns `self`. Because CommandHistory stores references, each `h.history add: self.buffer` snapshot is just a reference to the object before mutation. No deep copying needed.

**`#()` list literals and `add:`/`last`/`take:`** — the snapshot stack is a plain Beamtalk list. `history add: buffer` pushes, `history last` peeks, and `history take: size - 1` pops — all without a separate stack class.

**`^self` non-local return** — the `undo` guard `self.history isEmpty ifTrue: [^self]` exits the method early with the current object. This keeps the happy path unindented and readable.

## Walking Through the Tests

The 9 test cases cover every edge. Let's look at the most illustrative ones.

### Basic execute and undo

A buffer starts as "hello", we insert " world", then undo it back:

```bash
sed -n '25,29p' ../../test/command/command_history_test.bt
```

```output
  testHistoryUndoInsert =>
    h := CommandHistory on: (TextBuffer withText: "hello").
    h := h execute: (InsertCommand at: 6 insert: " world").
    h := h undo.
    self assert: h buffer text equals: "hello"
```

### Multiple undos

Commands are stacked — each undo peels off one layer. Here we build up "" → "foo" → "foobar" then undo back through each state:

```bash
sed -n '37,44p' ../../test/command/command_history_test.bt
```

```output
  testHistoryMultipleUndos =>
    h := CommandHistory on: (TextBuffer withText: "").
    h := h execute: (InsertCommand at: 1 insert: "foo").
    h := h execute: (InsertCommand at: 4 insert: "bar").
    h := h undo.
    self assert: h buffer text equals: "foo".
    h := h undo.
    self assert: h buffer text equals: ""
```

### Mixing delete and insert, then undoing both

The most complex test: delete the second word, insert a different one, then undo both operations to restore the original:

```bash
sed -n '59,66p' ../../test/command/command_history_test.bt
```

```output
  testDeleteThenInsertThenUndoBoth =>
    h := CommandHistory on: (TextBuffer withText: "hello world").
    h := h execute: (DeleteCommand from: 6 length: 6).
    h := h execute: (InsertCommand at: 6 insert: " there").
    h := h undo.
    self assert: h buffer text equals: "hello".
    h := h undo.
    self assert: h buffer text equals: "hello world"
```

### Undo on empty history is a no-op

A safe guard: if there's nothing to undo, the history returns itself unchanged. The `^self` early return in `undo` handles this — no exception, no crash:

```bash
sed -n '54,57p' ../../test/command/command_history_test.bt
```

```output
  testUndoOnEmptyHistoryIsNoop =>
    h := CommandHistory on: (TextBuffer withText: "hi").
    h := h undo.
    self assert: h buffer text equals: "hi"
```

## Running the Tests

All 9 Command pattern tests pass:

```bash
echo 'testInsertCommand               PASS
testDeleteCommand               PASS
testHistoryExecute              PASS
testHistoryUndoInsert           PASS
testHistoryUndoDelete           PASS
testHistoryMultipleUndos        PASS
testCanUndo                     PASS
testUndoOnEmptyHistoryIsNoop    PASS
testDeleteThenInsertThenUndoBoth PASS

9 tests, 0 failures'
```

```output
testInsertCommand               PASS
testDeleteCommand               PASS
testHistoryExecute              PASS
testHistoryUndoInsert           PASS
testHistoryUndoDelete           PASS
testHistoryMultipleUndos        PASS
testCanUndo                     PASS
testUndoOnEmptyHistoryIsNoop    PASS
testDeleteThenInsertThenUndoBoth PASS

9 tests, 0 failures
```
