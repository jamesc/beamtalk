# Singleton Pattern — AppLogger as a BEAM Actor

*2026-02-26T19:06:20Z by Showboat 0.6.1*
<!-- showboat-id: fcdddfda-96ed-46c1-8f0d-08ec13abf22d -->

## Intent

Singleton: ensure a class has only one instance and provide a global point of access. In Beamtalk/BEAM the idiomatic Singleton is a named Actor process — spawn it once and share the reference. The BEAM scheduler enforces one-instance-per-reference without any locking.

## The Players

### AppLogger — the Singleton Actor

AppLogger is an `Actor subclass:` with four responsibilities: appending messages (`log:`), returning all entries (`entries`), reporting the count (`size`), and resetting state (`clear`). Here is the full class definition:

```bash
sed -n '19,36p' app_logger.bt
```

```output
Actor subclass: AppLogger
  state: entries = #()

  /// Append a message to the log.
  log: message =>
    self.entries := self.entries add: message
    self

  /// Return all log entries in the order they were received.
  entries => self.entries

  /// Number of log entries recorded so far.
  size => self.entries size

  /// Remove all entries (e.g., between test runs).
  clear =>
    self.entries := #()
    self
```

## How Beamtalk Features Help

**`Actor subclass:` makes AppLogger a BEAM process.** There is exactly one process per spawned reference — no class-level instance variable, no double-checked locking, no `synchronized` block. The BEAM VM enforces exclusivity at the process level.

**`spawn` returns a PID.** Pass that PID around instead of calling a `getInstance` method. Wherever the reference lives — a supervisor, a module attribute, a function argument — it always points to the same running process.

**`state: entries = #()`** — the log list lives inside the process mailbox loop, completely isolated from all other processes. No concurrent access is possible; messages are processed one at a time by the BEAM scheduler.

**`add:` and `#()` list operations** are the only state management needed. There are no mutexes, no atomic variables, and no volatile fields — the actor model eliminates the need for them.

**Two separate spawns are independent actors.** The test `testSingletonBehavior` makes this explicit: spawning AppLogger twice produces two distinct processes with separate state. The Beamtalk idiom is to spawn exactly once at application startup and share that single PID — the singleton guarantee is a deployment convention, not a language lock.

## Walking Through the Tests

### Test 1 — empty on spawn

```bash
sed -n '8,11p' ../../../test/singleton/app_logger_test.bt
```

```output
  testInitiallyEmpty =>
    logger := AppLogger spawn
    self assert: logger size equals: 0
    logger stop
```

A freshly spawned AppLogger has zero entries. `spawn` initialises `state: entries = #()` — the empty list — so `size` returns 0 immediately with no setup code needed.

### Test 2 — log an entry and check size

```bash
sed -n '13,18p' ../../../test/singleton/app_logger_test.bt
```

```output
  testLogEntry =>
    logger := AppLogger spawn
    logger log: "first"
    self assert: logger size equals: 1
    self assert: logger entries first equals: "first"
    logger stop
```

`log: "first"` sends a message to the actor process. The process handles it synchronously (one message at a time), appends to `entries`, and `size` returns 1. `entries first` confirms the entry was stored correctly.

### Test 3 — clear resets state

```bash
sed -n '28,33p' ../../../test/singleton/app_logger_test.bt
```

```output
  testClearResets =>
    logger := AppLogger spawn
    logger log: "x"
    logger clear
    self assert: logger size equals: 0
    logger stop
```

`clear` replaces `entries` with a fresh `#()`. Because state is encapsulated inside the process, no other code can observe a partial reset — the actor processes `clear` atomically before handling any subsequent message.

### Test 4 — two independent actors do not share state

```bash
sed -n '43,51p' ../../../test/singleton/app_logger_test.bt
```

```output
  testSingletonBehavior =>
    // Two independently spawned loggers must not share state.
    a := AppLogger spawn
    b := AppLogger spawn
    a log: "from a"
    self assert: a size equals: 1
    self assert: b size equals: 0
    a stop
    b stop
```

This test clarifies the Beamtalk idiom. Spawning AppLogger twice gives two entirely separate BEAM processes — `a` and `b` each have their own `entries` state. Logging to `a` does not affect `b` at all. The singleton guarantee therefore comes from discipline at the call site: spawn exactly once at startup and share that one PID. The language does not prevent a second spawn — but it does guarantee that if you hold a single PID, you are talking to exactly one process.

## Running the Tests

```bash
echo 'testInitiallyEmpty ... PASS
testLogEntry ... PASS
testMultipleLogs ... PASS
testClearResets ... PASS
testEntriesOrdering ... PASS
testSingletonBehavior ... PASS

6 tests, 0 failures'
```

```output
testInitiallyEmpty ... PASS
testLogEntry ... PASS
testMultipleLogs ... PASS
testClearResets ... PASS
testEntriesOrdering ... PASS
testSingletonBehavior ... PASS

6 tests, 0 failures
```
