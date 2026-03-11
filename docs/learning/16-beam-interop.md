<!-- btfixture: fixtures/ch16erlang_calls.bt -->
<!-- btfixture: fixtures/ch16type_mapping.bt -->
<!-- btfixture: fixtures/ch16practical_patterns.bt -->
<!-- btfixture: fixtures/ch16error_handling.bt -->

## BEAM Interop

Beamtalk is a first-class BEAM citizen. Beamtalk actors are standard OTP
gen_servers — callable from Erlang, Elixir, or Gleam without any wrapper.
Going the other way, you can call any Erlang module from Beamtalk using
the `Erlang` global object.

This chapter covers:

1. Calling Erlang functions via the Erlang proxy
2. Erlang type mapping (how BEAM values cross the boundary)
3. How Beamtalk values appear to Erlang callers
4. When to use Erlang interop vs pure Beamtalk

## 1. Calling Erlang functions

Pattern: `Erlang moduleName functionName: arg with: arg ...`

- `Erlang moduleName` — returns an `ErlangModule` proxy for that module
- `proxy msg` — calls `moduleName:msg()` or `moduleName:msg(arg)` etc.

Selector mapping:

```
Unary after proxy:      Erlang lists reverse: xs     → lists:reverse(xs)
Multi-keyword:          Erlang lists seq: 1 with: 5  → lists:seq(1, 5)
Two `with:` keywords:   Erlang math pow: 2 with: 3   → math:pow(2, 3)
```

```beamtalk
TestCase subclass: Ch16ErlangCalls

  testListsReverse =>
    result := Erlang lists reverse: #(3, 2, 1)
    self assert: result equals: #(1, 2, 3)

  testListsSeq =>
    result := Erlang lists seq: 1 with: 5
    self assert: result equals: #(1, 2, 3, 4, 5)

  testListsNth =>
    result := Erlang lists nth: 2 with: #(10, 20, 30)
    self assert: result equals: 20

  testMapsMerge =>
    result := Erlang maps merge: #{#a => 1} with: #{#b => 2}
    self assert: (result at: #a) equals: 1
    self assert: (result at: #b) equals: 2

  testErlangNode =>
    // erlang:node/0 — returns current node name as an atom:
    node := (Erlang erlang) node
    self assert: node notNil

  testCachedProxy =>
    // You can store a module proxy in a variable:
    lists := Erlang lists
    self assert: lists class equals: ErlangModule
    self assert: (lists reverse: #(1, 2, 3)) equals: #(3, 2, 1)
```

## 2. Erlang type mapping

All BEAM-native types cross the boundary transparently. No wrapping needed.

| Beamtalk value | Erlang representation |
|----------------|-----------------------|
| Integer        | integer               |
| Float          | float                 |
| String         | binary (UTF-8)        |
| Symbol         | atom                  |
| Boolean        | atom (true/false)     |
| Nil            | atom (nil)            |
| Array `#[...]` | list                  |
| Dictionary `#{}` | map                 |
| Actor          | pid (gen_server)      |

So when you call an Erlang function that expects a list, pass a Beamtalk Array.
When it returns an Erlang atom, you get a Beamtalk Symbol.

```beamtalk
TestCase subclass: Ch16TypeMapping

  testSymbolsAreAtoms =>
    // Symbols become atoms at the Erlang boundary:
    result := Erlang erlang is_atom: #hello
    self assert: result

  testStringsAreBinaries =>
    // Strings are UTF-8 binaries at the Erlang boundary:
    result := Erlang erlang is_binary: "hello"
    self assert: result

  testIntegersAreIntegers =>
    result := Erlang erlang is_integer: 42
    self assert: result

  testBooleansAreAtoms =>
    // true/false are atoms in Erlang:
    result := Erlang erlang is_atom: true
    self assert: result

  testArraysAreLists =>
    // Beamtalk Arrays become Erlang lists:
    result := Erlang erlang is_list: #[1, 2, 3]
    self assert: result

  testDictsAreMaps =>
    result := Erlang erlang is_map: #{#a => 1}
    self assert: result
```

## 3. Calling Beamtalk from Erlang

Beamtalk actors are standard gen_servers. Their module name follows this
convention: `bt_<class_name_snake_case>`

Example: an actor `MyService` compiles to module `bt_my_service`.

From Erlang:

```erlang
{ok, Pid} = gen_server:start(bt_counter, #{value => 0}, []),
1 = gen_server:call(Pid, increment),
2 = gen_server:call(Pid, increment).
```

Value types (`Value subclass:`) compile to plain Erlang maps:

```erlang
Point = bt_point:new(#{x => 3, y => 4}),
3 = maps:get(x, Point).
```

Because Beamtalk compiles to standard BEAM, there is zero FFI overhead —
calling a Beamtalk actor from Erlang is identical to calling any gen_server.

## 4. Practical patterns

```beamtalk
TestCase subclass: Ch16PracticalPatterns

  testCallingStringModule =>
    // Erlang's string module for operations not yet in Beamtalk stdlib:
    result := Erlang string uppercase: "hello"
    self assert: result equals: "HELLO"

  testCallingMathModule =>
    // math:pow/2 — floating-point power:
    result := Erlang math pow: 2.0 with: 10.0
    self assert: result equals: 1024.0

  testCallingCryptoHash =>
    // crypto:hash/2 — SHA-256 (returns binary):
    hash := Erlang crypto hash: #sha256 with: "hello"
    self assert: hash class equals: String    // binary → String

  testListsFlattening =>
    nested := #[#[1, 2], #[3, 4], #[5]]
    flat := Erlang lists flatten: nested
    self assert: flat equals: #(1, 2, 3, 4, 5)

  testMapsToList =>
    // maps:to_list/1 — convert a map to a list of {K, V} tuples:
    d := #{#a => 1, #b => 2}
    pairs := Erlang maps to_list: d
    self assert: pairs size equals: 2
```

## 5. Error handling at the Erlang boundary

Erlang exceptions (error, exit, throw) are caught as Beamtalk `RuntimeError`:

```beamtalk
TestCase subclass: Ch16ErrorHandling

  testErlangErrorCaught =>
    // Calling a non-existent function raises RuntimeError:
    result := [
      Erlang lists nonexistent_function: 42
    ] on: RuntimeError do: [:e | #caught]
    self assert: result equals: #caught

  testErlangBadargCaught =>
    // Wrong argument type raises RuntimeError:
    result := [
      Erlang lists reverse: 42    // 42 is not a list
    ] on: RuntimeError do: [:e | #caught]
    self assert: result equals: #caught
```

## 6. When to use Erlang interop

**USE** Erlang interop for:

- Battle-tested OTP libraries (crypto, ssl, mnesia, etc.)
- Erlang standard library functions not yet wrapped in Beamtalk stdlib
- Low-level system calls (`erlang:system_info`, `erlang:process_info`, etc.)
- Hex packages written in Erlang

**AVOID** Erlang interop for:

- Things already in Beamtalk stdlib (Array, String, Dictionary methods)
- New code that should be written in Beamtalk style
- Core business logic — keep it in idiomatic Beamtalk

The Erlang boundary is zero-cost (no serialization), so performance is never
a reason to avoid it — but clarity is. Prefer Beamtalk APIs where they exist.

## Summary

Call Erlang:

```
Erlang lists reverse: #[3, 2, 1]
Erlang maps merge: d1 with: d2
proxy := Erlang moduleName   -- cached proxy
```

Type mapping is transparent — no wrapping or conversion needed.

Beamtalk actors are gen_servers — callable from any BEAM language with
`gen_server:call(Pid, message)`.

Erlang exceptions → Beamtalk `RuntimeError` (catchable with `on:do:`).

Next: Chapter 17 — OTP Supervisors
