<!-- btfixture: fixtures/counter.bt -->
<!-- btfixture: fixtures/bank_account.bt -->
<!-- btfixture: fixtures/id_generator.bt -->
<!-- btfixture: fixtures/ch11actors.bt -->
<!-- btfixture: fixtures/ch11bank_account.bt -->
<!-- btfixture: fixtures/ch11multiple_actors.bt -->
<!-- btfixture: fixtures/ch11cast_send.bt -->

## Actors

An actor is a concurrent object — it runs as an independent BEAM process with
its own mailbox. Actors can hold mutable state and communicate by sending
messages. This maps directly to Erlang's gen_server model.

Use `Actor subclass:` when you need:

- Mutable state shared across calls
- Concurrent access from multiple parts of your program
- Background tasks or services

Use `Value subclass:` (chapter 10) for plain data with no mutable state.

## Defining an actor

```beamtalk
Actor subclass: Counter
  state: value = 0

  // Mutate state with self.slot :=
  increment =>
    self.value := self.value + 1

  // Return the current value (^ for early return; last expression is implicit return)
  getValue =>
    self.value

  // Arguments work just like value class methods:
  incrementBy: n =>
    self.value := self.value + n

  reset =>
    self.value := 0
```

## Creating and using actors

`spawn` creates a new actor process. Unlike `new` (value classes), actors
run as independent BEAM processes.

```beamtalk
TestCase subclass: Ch11Actors

  testBasicCounterLifecycle =>
    c := Counter spawn
    self assert: c getValue equals: 0
    c increment
    self assert: c getValue equals: 1
    c increment
    c increment
    self assert: c getValue equals: 3

  testIncrementBy =>
    c := Counter spawn
    c incrementBy: 10
    self assert: c getValue equals: 10
    c incrementBy: 5
    self assert: c getValue equals: 15

  testReset =>
    c := Counter spawn
    c increment
    c increment
    c increment
    self assert: c getValue equals: 3
    c reset
    self assert: c getValue equals: 0

  testSpawnWithInitialState =>
    // spawnWith: lets you set initial slot values:
    c := Counter spawnWith: #{#value => 100}
    self assert: c getValue equals: 100
    c increment
    self assert: c getValue equals: 101
```

## Call vs cast: synchronous and asynchronous sends

By default, sending a message to an actor is **synchronous** — the sender
blocks until the actor processes the message and returns a result. This is
a `call` (maps to `gen_server:call`).

For fire-and-forget messages, append `!` to the send. This is a **cast** —
the sender continues immediately without waiting. The cast always returns
`nil` to the caller. (Maps to `gen_server:cast`.)

```beamtalk
TestCase subclass: Ch11CastSend

  testCastSendIncrements =>
    c := Counter spawn
    c increment!
    // Sync barrier: getValue forces cast to be processed first
    self assert: c getValue equals: 1

  testMultipleCastSends =>
    c := Counter spawn
    c increment!
    c increment!
    c increment!
    self assert: c getValue equals: 3

  testSyncCallReturnsValue =>
    c := Counter spawn
    c increment
    self assert: c getValue equals: 1
```

**When to use cast (`!`):**

- Logging, metrics, or notifications where you don't need the result
- Breaking potential deadlock cycles (see below)
- Improving throughput when ordering is handled by a later sync barrier

**When to use call (default):**

- You need the return value
- You need to know the operation succeeded before continuing

## Actor lifecycle

### The `initialize` method

If your actor needs setup beyond default state values, define `initialize`.
In the REPL, it runs automatically after spawn. In compiled code (including
BUnit tests), call it explicitly after spawn.

```beamtalk
Actor subclass: StackActor
  state: items = nil

  initialize =>
    self.items := #[]

  push: item =>
    self.items := self.items add: item

  size => self.items size
```

Usage in the REPL:

```text
s := StackActor spawn    // initialize runs automatically
s size                   // => 0
s push: "a"
s size                   // => 1
```

In BUnit tests, call `initialize` explicitly (see the Stack example in
chapter 13).

### Stopping actors

```beamtalk
  testIsAlive =>
    c := Counter spawn
    self assert: c isAlive
    c stop
    self deny: c isAlive

  testStopIsIdempotent =>
    c := Counter spawn
    self assert: (c stop) equals: #ok
    self assert: (c stop) equals: #ok    // second stop is a no-op

  testSendingToStoppedActorRaisesError =>
    c := Counter spawn
    c stop
    self should: [c increment] raise: #actor_dead
```

Lifecycle methods:

| Method     | Effect                                    |
|------------|-------------------------------------------|
| `spawn`    | Create actor process with default state   |
| `spawnWith:` | Create with initial slot values         |
| `initialize` | Called after spawn (override for setup) |
| `isAlive`  | Check if actor process is running         |
| `stop`     | Graceful shutdown (idempotent)            |

## Deadlock prevention

A **deadlock** occurs when two actors call each other synchronously at the
same time — each waits for the other's reply, and neither can proceed.
Beamtalk detects this as a `#timeout` error after 5 seconds.

**Prevention strategies:**

1. **Use cast (`!`) for one direction** in cyclic relationships:

```text
// A calls B synchronously, B notifies A asynchronously:
actorA doWork          // sync call to B inside
actorB notify: event!  // cast back — no deadlock
```

2. **Self-sends are always safe** — they bypass gen_server entirely.
   An actor calling its own methods within a method body dispatches directly,
   not through the mailbox.

3. **Avoid circular sync call patterns.** If A calls B and B calls A,
   restructure so one direction uses cast.

## A richer example: a bank account

```beamtalk
Actor subclass: BankAccount
  state: balance = 0
  state: owner = ""

  deposit: amount =>
    amount > 0
      ifFalse: [^self error: "Amount must be positive"]
    self.balance := self.balance + amount

  withdraw: amount =>
    amount > self.balance
      ifTrue: [^self error: "Insufficient funds"]
    self.balance := self.balance - amount

  getBalance => self.balance

  getOwner => self.owner

TestCase subclass: Ch11BankAccount

  testDepositAndWithdraw =>
    account := BankAccount spawnWith: #{#owner => "Alice"}
    account deposit: 100
    account deposit: 50
    self assert: account getBalance equals: 150
    account withdraw: 30
    self assert: account getBalance equals: 120

  testOwner =>
    account := BankAccount spawnWith: #{#owner => "Bob"}
    self assert: account getOwner equals: "Bob"

  testWithdrawInsufficientFunds =>
    account := BankAccount spawn
    account deposit: 50
    self should: [account withdraw: 100] raise: #beamtalk_error

  testDepositNegativeAmount =>
    account := BankAccount spawn
    self should: [account deposit: -10] raise: #beamtalk_error
```

## Multiple actors working together

```beamtalk
Actor subclass: IdGenerator
  state: next = 1

  nextId =>
    id := self.next
    self.next := self.next + 1
    id

TestCase subclass: Ch11MultipleActors

  testIdGeneratorProducesUniqueIds =>
    gen := IdGenerator spawn
    id1 := gen nextId
    id2 := gen nextId
    id3 := gen nextId
    self assert: id1 equals: 1
    self assert: id2 equals: 2
    self assert: id3 equals: 3

  testMultipleIndependentActors =>
    // Each spawn creates an independent process with its own state:
    c1 := Counter spawn
    c2 := Counter spawn
    c1 increment
    c1 increment
    c2 increment
    self assert: c1 getValue equals: 2
    self assert: c2 getValue equals: 1
```

## Key differences from Value classes

| Aspect         | Value class           | Actor                         |
|----------------|-----------------------|-------------------------------|
| Create         | `ClassName new`       | `ClassName spawn`             |
| Initial state  | `new: #{slot => v}`   | `spawnWith: #{slot => v}`     |
| Mutation       | Not allowed           | `self.slot := expr`           |
| Identity       | Equality by value     | Each spawn is a unique process|
| BEAM model     | Erlang map            | gen_server process            |
| Concurrent?    | Copy-safe (no state)  | Yes, message-passing safe     |
| Message style  | Always synchronous    | Sync (default) or cast (`!`) |
