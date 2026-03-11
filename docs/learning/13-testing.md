<!-- btfixture: fixtures/counter.bt -->
<!-- btfixture: fixtures/stack.bt -->
<!-- btfixture: fixtures/ch13basic_tests.bt -->
<!-- btfixture: fixtures/ch13assertions.bt -->
<!-- btfixture: fixtures/ch13setup_example.bt -->
<!-- btfixture: fixtures/stack_test.bt -->

## Testing with BUnit

Beamtalk's test framework is BUnit — a Smalltalk-style xUnit framework.
Tests live in `TestCase subclass:` definitions.

Test runner: `beamtalk test` (BUnit tests) or `beamtalk test-script` (btscript inline assertions)

## Basic structure

Any method starting with `test` is run as a test.
Use `self assert:` and friends to verify behaviour.

```beamtalk
TestCase subclass: Ch13BasicTests

  testAddition =>
    self assert: (3 + 4) equals: 7

  testString =>
    self assert: "hello" size equals: 5

  testBoolean =>
    self assert: true
    self deny: false
```

## Assertion methods

Full set of BUnit assertions:

```beamtalk
TestCase subclass: Ch13Assertions

  testAssertEquals =>
    // assert:equals: — most common assertion
    self assert: 1 + 1 equals: 2
    self assert: "hello" uppercase equals: "HELLO"

  testAssert =>
    // assert: — true if the value is truthy
    self assert: true
    self assert: (3 > 2)
    self assert: "hello" isNotEmpty

  testDeny =>
    // deny: — true if the value is falsy
    self deny: false
    self deny: (3 > 5)
    self deny: "" isNotEmpty

  testAssertNil =>
    self assert: nil equals: nil
    self assert: (nil ifNil: [nil]) equals: nil

  testDenyNil =>
    self assert: (42 isNil not)
    self assert: ("hello" isNil not)

  testShould =>
    // should:raise: — verifies an exception is raised
    self should: [1 / 0] raise: #badarith

  testShouldNotRaise =>
    result := 1 + 1
    self assert: result equals: 2

  testAssertKindOf =>
    self assert: (42 isKindOf: Integer)
    self assert: ("hello" isKindOf: String)
    self assert: (true isKindOf: Boolean)
```

### Assertion quick reference

| Assertion | Checks |
|-----------|--------|
| `self assert: expr equals: expected` | Equality |
| `self assert: boolExpr` | Truthy |
| `self deny: boolExpr` | Falsy |
| `self assertNil: expr` | Is nil |
| `self denyNil: expr` | Is not nil |
| `self should: [block] raise: #symbol` | Exception raised |
| `self shouldnt: [block] raise: #symbol` | No exception |
| `self assert: obj isKindOf: Class` | Type check |

## setUp and tearDown

`setUp` runs before each test method.
`tearDown` runs after each test method.
Instance variables must be declared as `state:` to persist across `setUp` → test.

```beamtalk
TestCase subclass: Ch13SetupExample
  state: counter = nil

  setUp =>
    self.counter := Counter spawn
    self

  tearDown =>
    self.counter isAlive ifTrue: [self.counter stop]

  testCounterStartsAtZero =>
    self assert: self.counter getValue equals: 0

  testCounterIncrements =>
    self.counter increment
    self assert: self.counter getValue equals: 1

  testCounterIsIndependentPerTest =>
    // Each test gets a fresh counter from setUp
    self assert: self.counter getValue equals: 0
    self.counter increment
    self.counter increment
    self assert: self.counter getValue equals: 2
```

### Key rules for setUp/tearDown

1. **Declare state variables** with `state: varName = default` in the class
   header. Without the declaration, `self.varName :=` in setUp doesn't
   persist to test methods.

2. **setUp must return self** — the return value is used as the test
   receiver.

3. **tearDown** is for cleanup: stop actors, close files, release resources.
   It runs even if the test fails.

4. **Each test gets a fresh fixture** — setUp runs independently before
   every test method, so tests don't affect each other.

## Organising tests

One `TestCase` per class under test is common. Methods are named
`test<What><Scenario>` for clarity.

Example: testing a full stack class:

```beamtalk
Actor subclass: Stack
  state: items = nil

  initialize =>
    self.items := #[]

  push: item =>
    self.items := self.items add: item

  pop =>
    self.items isEmpty ifTrue: [self error: "Stack underflow"]
    top := self.items last
    self.items := self.items copyFrom: 1 to: self.items size - 1
    top

  peek =>
    self.items isEmpty ifTrue: [self error: "Stack underflow"]
    self.items last

  isEmpty => self.items isEmpty

  size => self.items size

TestCase subclass: StackTest
  state: stack = nil

  setUp =>
    self.stack := Stack spawn
    self.stack initialize
    self

  testNewStackIsEmpty =>
    self assert: self.stack isEmpty

  testPushIncreasesSize =>
    self.stack push: 1
    self assert: self.stack size equals: 1
    self.stack push: 2
    self assert: self.stack size equals: 2

  testPopReturnsLastPushed =>
    self.stack push: "a"
    self.stack push: "b"
    self.stack push: "c"
    self assert: self.stack pop equals: "c"
    self assert: self.stack pop equals: "b"
    self assert: self.stack pop equals: "a"

  testPeekDoesNotRemove =>
    self.stack push: 42
    self assert: self.stack peek equals: 42
    self assert: self.stack size equals: 1

  testPopFromEmptyRaises =>
    self should: [self.stack pop] raise: #beamtalk_error

  testPeekFromEmptyRaises =>
    self should: [self.stack peek] raise: #beamtalk_error
```

### Test naming conventions

Good test names describe the scenario and expected outcome:

```text
testDepositPositiveAmount        — happy path
testWithdrawInsufficientFunds    — error case
testStopIsIdempotent             — edge case
testMultipleIndependentActors    — interaction
```

### The `@expect` directive

When a test intentionally triggers an error that would normally produce
a compiler warning (like sending an unknown message), use `@expect` to
suppress it:

```text
@expect dnu                    // Suppress does_not_understand warning
result := [42 noSuchMethod] on: RuntimeError do: [:e | #caught]
```

## Doc tests (inline assertions in `///` comments)

Methods can include doc tests in their `///` doc comments.
These are extracted and run automatically by the doc test runner.

The format is a ` ```beamtalk ` block with `// =>` assertions:

```text
Object subclass: Greeter
  /// Greet someone by name.
  ///
  /// ```beamtalk
  /// g := Greeter new
  /// g greet: "Alice"   // => Hello, Alice!
  /// ```
  greet: name => "Hello, {name}!"
```

This keeps examples right next to the code they document and ensures
they stay correct as the code evolves.

## Summary

Define a test class:

```beamtalk
TestCase subclass: MyTest
  state: fixture = nil
  setUp => ...          // runs before each test
  tearDown => ...       // runs after each test
  testSomething => ...  // any method starting with test
```

Key assertions:

```
self assert: expr equals: expected
self assert: boolExpr
self deny: boolExpr
self assertNil: expr
self denyNil: expr
self should: [block] raise: ExceptionClass
self shouldnt: [block] raise: ExceptionClass
self assert: obj isKindOf: Class
```

Run tests:

```bash
beamtalk test                     # all BUnit tests
beamtalk test test/my_test.bt     # one file
```

Next: Chapter 14 — Pattern Matching
