<!-- btfixture: fixtures/parser.bt -->
<!-- btfixture: fixtures/validator.bt -->
<!-- btfixture: fixtures/ch12exceptions.bt -->
<!-- btfixture: fixtures/ch12result.bt -->
<!-- btfixture: fixtures/ch12parser_test.bt -->
<!-- btfixture: fixtures/ch12dnu.bt -->
<!-- btfixture: fixtures/ch12validator.bt -->
<!-- btfixture: fixtures/ch12validation_error.bt -->
<!-- btfixture: fixtures/ch12custom_errors.bt -->
<!-- btfixture: fixtures/ch12ensure.bt -->

## Error Handling

Beamtalk has three error-handling mechanisms:

1. **Exceptions** — `on:do:` catches exceptions by class
2. **Result type** — explicit ok/error values (no exception thrown)
3. **DNU** — `doesNotUnderstand` fires when no method is found

All user-facing errors use `#beamtalk_error{}` records. Internal errors may
use Erlang's native exception system.

## 1. Exceptions — on:do:

Wrap potentially-failing code in a block and catch errors with `on:do:`.
The handler block receives the exception object.

```beamtalk
TestCase subclass: Ch12Exceptions

  testCatchDivisionByZero =>
    result := [10 / 0] on: Exception do: [:e | -1]
    self assert: result equals: -1

  testDoesNotUnderstand =>
    result := [42 perform: #bogusMessage] on: RuntimeError do: [:e | #caught]
    self assert: result equals: #caught

  testUncaughtExceptionPropagates =>
    // When the on:do: class doesn't match, the exception propagates:
    self should: [
      [42 perform: #bogusMessage] on: TypeError do: [:e | #wrong]
    ] raise: #does_not_understand

  testOnDoWithException =>
    r1 := [1 / 0] on: Exception do: [:e | #caught]
    self assert: r1 equals: #caught

  testErrorMessage =>
    // Exception objects expose their message:
    text := [42 perform: #bogusMessage] on: RuntimeError do: [:e | e message]
    self assert: (text isKindOf: String)
```

### The exception hierarchy

Beamtalk's exception classes form a hierarchy:

```text
Exception
  └── Error
        ├── RuntimeError  (does_not_understand, arity_mismatch, etc.)
        └── TypeError     (type mismatches)
```

Catching `Exception` catches everything. Catching `RuntimeError` catches
only runtime dispatch errors. Use the narrowest match appropriate.

### Custom exception classes

Define your own exception classes by subclassing `Error`:

```beamtalk
Error subclass: ValidationError
```

Signal (raise) a custom exception with `signal:`:

```beamtalk
TestCase subclass: Ch12CustomErrors

  testSignalCustomError =>
    result := [
      ValidationError new signal: "bad input"
    ] on: ValidationError do: [:e | e message]
    self assert: result equals: "bad input"

  testCatchByParentClass =>
    result := [
      ValidationError new signal: "oops"
    ] on: Error do: [:e | "caught by Error"]
    self assert: result equals: "caught by Error"

  testCatchByRootException =>
    result := [
      ValidationError new signal: "oops"
    ] on: Exception do: [:e | "caught by Exception"]
    self assert: result equals: "caught by Exception"

  testNonMatchingClassDoesNotCatch =>
    result := [
      [ValidationError new signal: "no"] on: TypeError do: [:e | "wrong"]
    ] on: Exception do: [:e | "correct"]
    self assert: result equals: "correct"

  testExceptionClassPreserved =>
    result := [
      ValidationError new signal: "test"
    ] on: Exception do: [:e | e class]
    self assert: result equals: ValidationError
```

### ensure: — guaranteed cleanup

`ensure:` runs its block whether the protected code succeeds or raises
an exception. It returns the value of the protected block (not the
cleanup block).

```beamtalk
TestCase subclass: Ch12Ensure

  testEnsureReturnsBodyValue =>
    result := [42] ensure: [99]
    self assert: result equals: 42

  testEnsureReturnsBodyValueOnSuccess =>
    result := ["hello"] ensure: [nil]
    self assert: result equals: "hello"
```

Use `ensure:` for cleanup that must always happen:

```text
[file := File open: path] ensure: [file close]
[connection doQuery: sql] ensure: [connection release]
```

### Error propagation patterns

Combine `on:do:` and `ensure:` for robust error handling:

```text
// Catch, clean up, and re-raise:
[riskyOperation]
  on: Error do: [:e |
    logger log: "Failed: " ++ e message.
    e signal: e message    // re-signal to propagate
  ]

// Guaranteed cleanup regardless of outcome:
[riskyOperation] ensure: [cleanup]
```

## 2. Result type — explicit ok/error values

`Result` is a value class wrapping either a success or a failure.
Use it when callers should handle errors without exceptions, or when you want
composable pipelines.

Construction:

- `Result ok: value` — success with a value
- `Result error: reason` — failure with a reason (usually a symbol)

```beamtalk
TestCase subclass: Ch12Result

  testOkResult =>
    r := Result ok: 42
    self assert: r ok
    self deny: r isError
    self assert: r value equals: 42

  testErrorResult =>
    r := Result error: #not_found
    self assert: r isError
    self deny: r ok
    self assert: r error equals: #not_found

  testValueOr =>
    // Safe extraction with default:
    self assert: ((Result ok: 42) valueOr: 0) equals: 42
    self assert: ((Result error: #x) valueOr: 0) equals: 0

  testValueOrDo =>
    // Compute the fallback with a block (gets the error reason):
    result := (Result error: #not_found) valueOrDo: [:e | "Error: {e}"]
    self assert: result equals: "Error: not_found"

  testUnwrap =>
    // unwrap returns the value or raises if it's an error:
    self assert: (Result ok: 99) unwrap equals: 99
    self should: [(Result error: #oops) unwrap] raise: #signal

  testMapOnOk =>
    // map: transforms the success value:
    r := (Result ok: 5) map: [:v | v * 2]
    self assert: r value equals: 10

  testMapOnError =>
    // map: is a no-op on errors:
    r := (Result error: #fail) map: [:v | v * 2]
    self assert: r isError
    self assert: r error equals: #fail
```

A method that returns `Result` instead of raising:

```beamtalk
Object subclass: Parser
  safeParseInt: str =>
    [Result ok: str asInteger]
      on: Error
      do: [:e | Result error: #parse_error]

TestCase subclass: Ch12ParserTest

  testSafeParseOk =>
    p := Parser new
    r := p safeParseInt: "42"
    self assert: r ok
    self assert: r value equals: 42

  testSafeParseError =>
    p := Parser new
    r := p safeParseInt: "not-a-number"
    self assert: r isError
    self assert: r error equals: #parse_error
```

### When to use Result vs exceptions

| Situation | Approach |
|-----------|----------|
| Unexpected failures (bugs) | Let exceptions propagate |
| Expected, recoverable errors | Return `Result` |
| Parsing, validation | `Result` — caller decides how to handle |
| Resource exhaustion, I/O | Either — depends on API style |

## 3. doesNotUnderstand (DNU)

When you send a message an object doesn't understand, it raises a
`doesNotUnderstand` error. This is Beamtalk's equivalent of "method not found".

You can guard against this with `respondsTo:` before sending:

```beamtalk
TestCase subclass: Ch12DNU

  testRespondsTo =>
    self assert: (42 respondsTo: #isZero)
    self deny: (42 respondsTo: #bogusMessage)

  testGuardedDispatch =>
    obj := 42
    result := (obj respondsTo: #isZero)
      ifTrue: [obj isZero printString]
      ifFalse: ["does not respond"]
    self assert: result equals: "false"

  testDNUCaught =>
    result := [42 perform: #bogusMessage] on: RuntimeError do: [:e | #caught]
    self assert: result equals: #caught
```

## 4. Raising errors from your own code

Use `self error: msg` to raise an Error from your code.
Use `self subclassResponsibility` for abstract methods.
Use `self notImplemented` for stubs.

```beamtalk
Object subclass: Validator
  validate: x =>
    x isNil ifTrue: [self error: "value must not be nil"]
    x < 0   ifTrue: [self error: "value must not be negative"]
    x

TestCase subclass: Ch12Validator

  testValidOk =>
    v := Validator new
    self assert: (v validate: 5) equals: 5

  testNilRaises =>
    v := Validator new
    self should: [v validate: nil] raise: #beamtalk_error

  testNegativeRaises =>
    v := Validator new
    self should: [v validate: -1] raise: #beamtalk_error
```

## Summary

**Exceptions:**

```
[code] on: Exception do: [:e | handler]     — catch by class
[code] on: MyCustomError do: [:e | handler]  — catch custom exception
[code] ensure: [always-runs]                 — guaranteed cleanup
self error: "message"                        — raise from your code
MyError new signal: "message"                — raise custom exception
```

**Exception hierarchy:**

```
Exception → Error → RuntimeError / TypeError / YourCustomError
```

**Result:**

```
Result ok: value        — success
Result error: reason    — failure
r ok / r isError
r value / r error
r valueOr: default
r valueOrDo: [:e | block]
r unwrap
r map: [:v | transform]
```

**DNU guard:**

```
obj respondsTo: #selector
```

Next: Chapter 13 — Testing with BUnit
