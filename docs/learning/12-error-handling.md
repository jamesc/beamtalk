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

  testBasicOnDo =>
    // Catch any exception:
    result := [1 / 0] on: Error do: [:e | "caught: {e messageText}"]
    self assert: (result startsWith: "caught:")

  testCatchDivisionByZero =>
    // ZeroDivide is a subclass of Error:
    result := [10 / 0] on: ZeroDivide do: [:e | -1]
    self assert: result equals: -1

  testDoesNotUnderstand =>
    // Sending an unknown message raises doesNotUnderstand:
    result := [42 bogusMessage] on: MessageNotUnderstood do: [:e | #caught]
    self assert: result equals: #caught

  testUncaughtExceptionPropagates =>
    // When the on:do: class doesn't match, the exception propagates:
    self should: [
      [42 bogusMessage] on: ZeroDivide do: [:e | #wrong]
    ] raise: #does_not_understand

  testOnDoWithMultipleExceptionTypes =>
    // Catch either type:
    r1 := [1 / 0] on: ZeroDivide, Error do: [:e | #caught]
    self assert: r1 equals: #caught

  testEnsure =>
    // ensure: always runs its block, even if an exception occurs:
    ran := false
    [42] ensure: [ran := true]
    self assert: ran

  testEnsureRunsOnException =>
    ran := false
    [
      [1 / 0] on: ZeroDivide do: [:e | nil]
    ] ensure: [ran := true]
    self assert: ran

  testErrorMessageText =>
    // Exception objects have messageText:
    text := [42 bogusMessage] on: MessageNotUnderstood do: [:e | e messageText]
    self assert: (text isKindOf: String)
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
    result := [42 bogusMessage] on: MessageNotUnderstood do: [:e | #caught]
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
[code] on: ExceptionClass do: [:e | handler]
[code] ensure: [always-runs]
self error: "message"
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
