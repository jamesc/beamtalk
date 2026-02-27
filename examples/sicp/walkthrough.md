# A Metacircular Scheme Evaluator in Beamtalk

*2026-02-26T20:39:19Z by Showboat 0.6.1*
<!-- showboat-id: a603b403-cbf5-493a-bc5e-35ff9ae1ab7f -->

This project implements a **metacircular evaluator** — a Scheme interpreter written in Beamtalk, following the structure introduced in SICP Chapter 4. It can parse, evaluate, and print Scheme s-expressions including arithmetic, lambdas, closures, conditionals, `let`, `cond`, `and`, `or`, and tail-recursive definitions. The six classes map cleanly onto the classic eval/apply loop, with Beamtalk Actors providing a natural fit for mutable environments.

## The Players

Six classes make up the interpreter. They are loaded in dependency order below.

### SchemeSymbol — distinguishing names from strings

`SchemeSymbol` is a tiny value type with a single `name` string. Its only job is to be *distinguishable* from Beamtalk `String` so the evaluator can tell "look this up in the environment" from "return this string literal as-is".

```bash
sed -n '14,25p' src/scheme/symbol.bt
```

```output
/// sym =:= (SchemeSymbol withName: "define")  // => true
/// sym =:= (SchemeSymbol withName: "lambda")  // => false
/// ```
Object subclass: SchemeSymbol
  state: name = ""

  /// Create a new symbol with the given name string.
  ///
  /// ## Examples
  /// ```beamtalk
  /// SchemeSymbol withName: "car"   // => a SchemeSymbol
  /// ```
```

### SchemeReader — tokenising s-expressions

The reader uses a **functional char-list** approach: every parse method receives the remaining characters as a `List` of single-character strings and returns a 2-element `#(parsedValue, remainingChars)` pair. This avoids mutable state and composes naturally with Beamtalk blocks.

```bash
sed -n '44,51p' src/scheme/reader.bt
```

```output

  /// Drop leading whitespace characters from a char list.
  dropWs: chars =>
    chars isEmpty ifTrue: [^chars]
    (self isWs: chars first) ifFalse: [^chars]
    self dropWs: chars rest

  /// Wrap `val` and `chars` into a 2-element `#(val, remainingChars)` pair.
```

### SchemeEnv — environment as an Actor

Each environment frame is a **live Beamtalk Actor** holding a `Dictionary` of bindings and an optional parent reference. Mutation (`define`) is sent as a message and the actor updates its own state. `lookup:` walks the parent chain automatically. Because actors own their state, there is no need to thread an updated environment through every recursive call — a key advantage over a purely functional representation.

```bash
sed -n '18,43p' src/scheme/env.bt
```

```output
/// (env define: "x" value: 42) await
/// (env lookup: "x") await    // => 42
/// ```
Actor subclass: SchemeEnv
  state: bindings = #{}
  state: parent   = nil

  /// Look up `name` in this frame, then walk parent frames until found.
  /// Raises an error if the name is unbound in the entire chain.
  ///
  /// ## Examples
  /// ```beamtalk
  /// (env lookup: "x") await    // => 42 (if x was defined)
  /// (env lookup: "z") await    // raises "Unbound variable: z"
  /// ```
  lookup: name =>
    val := self.bindings at: name ifAbsent: [nil]
    val notNil ifTrue: [^val]
    self.parent notNil ifTrue: [^(self.parent lookup: name) await]
    self error: "Unbound variable: " ++ name

  /// Bind or rebind `name` to `val` in this frame.
  ///
  /// ## Examples
  /// ```beamtalk
  /// (env define: "x" value: 10) await
```

### SchemeLambda — closures as plain data

A `SchemeLambda` is an immutable value object capturing `params` (a list of name strings), `body` (the unevaluated AST), and `closureEnv` (the `SchemeEnv` actor at the point of definition). There is no code here — application is handled entirely by `SchemeEval>>apply:args:in:`.

```bash
sed -n '15,25p' src/scheme/lambda.bt
```

```output
/// lam body         // => bodyExpr
/// lam closureEnv   // => the SchemeEnv actor
/// ```
Object subclass: SchemeLambda
  state: params     = #()   // List of parameter name strings
  state: body       = nil   // Unevaluated body expression (AST)
  state: closureEnv = nil   // SchemeEnv actor at point of definition

  /// Create a new lambda capturing `p` as parameter names, `b` as the body
  /// AST, and `e` as the enclosing environment.
  ///
```

### SchemeEval — the metacircular evaluator

`SchemeEval` is the heart of the interpreter. `eval:in:` dispatches on expression type using `class =:=` guards — self-evaluating atoms return immediately, symbols trigger an environment lookup, and lists are either special forms or function application. Built-in procedures are stored as **Beamtalk Blocks** directly in the environment dictionary, so `apply:args:in:` can call them without any separate primitive dispatch table.

```bash
sed -n '48,65p' src/scheme/eval.bt
```

```output
        "/"     => [:args | (args at: 1) div: (args at: 2)],
        "="     => [:args | (args at: 1) =:= (args at: 2)],
        "<"     => [:args | (args at: 1) < (args at: 2)],
        ">"     => [:args | (args at: 1) > (args at: 2)],
        "<="    => [:args | (args at: 1) <= (args at: 2)],
        ">="    => [:args | (args at: 1) >= (args at: 2)],
        "car"   => [:args | (args at: 1) first],
        "cdr"   => [:args | (args at: 1) rest],
        "cons"  => [:args | (#() add: (args at: 1)) ++ (args at: 2)],
        "list"  => [:args | args],
        "not"   => [:args | (args at: 1) not],
        "null?" => [:args |
          v := args at: 1
          v isNil or: [v class =:= List and: [v isEmpty]]]
      },
      #parent => nil}

  /// Evaluate `expr` in `env` and return the result.
```

```bash
sed -n '112,123p' src/scheme/eval.bt
```

```output
      (sym =:= "lambda") ifTrue: [
        params := (tail at: 1) collect: [:p | p name]
        ^SchemeLambda withParams: params body: (tail at: 2) env: env]

      // (if test then else?) — conditional branch
      (sym =:= "if") ifTrue: [
        condVal := self eval: (tail at: 1) in: env
        (condVal == false or: [condVal isNil]) ifTrue: [
          ^(tail size > 2) ifTrue: [self eval: (tail at: 3) in: env] ifFalse: [nil]
        ] ifFalse: [
          ^self eval: (tail at: 2) in: env]]

```

### SchemePrinter — values back to Scheme notation

The printer is the display layer: it converts Beamtalk values back to readable Scheme strings using the same type-dispatch pattern as the evaluator. `nil` → `"()"`, `true` → `"#t"`, `List` → recursively formatted `"(1 2 3)"`.

```bash
sed -n '17,37p' src/scheme/printer.bt
```

```output
/// printer print: #(1 2 3)    // => "(1 2 3)"
/// ```
Object subclass: SchemePrinter

  /// Convert `val` to its Scheme string representation.
  ///
  /// Dispatches on `val class`: nil → `"()"`, booleans → `"#t"`/`"#f"`,
  /// integers → decimal string, strings → double-quoted, symbols → name,
  /// lambdas → `"#<lambda>"`, lists → `"(elem ...)"`.
  ///
  /// ## Examples
  /// ```beamtalk
  /// SchemePrinter new print: 0          // => "0"
  /// SchemePrinter new print: "hi"       // => "\"hi\""
  /// SchemePrinter new print: nil        // => "()"
  /// ```
  print: val =>
    val isNil ifTrue: [^"()"]
    (val class =:= True)         ifTrue: [^"#t"]
    (val class =:= False)        ifTrue: [^"#f"]
    (val class =:= Integer)      ifTrue: [^val printString]
```

## How Beamtalk Features Help

- **Actors as mutable state** — `SchemeEnv` is an `Actor`, so `define:value:` is a plain message send. The actor owns its `bindings` dictionary; mutation is automatic and safe. No need to return a new environment from every call.
- **`await` for synchronous actor calls** — `(env lookup: name) await` and `(env extend:values:) await` block until the actor replies, giving synchronous semantics without locks or shared memory.
- **Blocks as first-class built-ins** — every primitive (`+`, `-`, `car`, `cdr`, …) is stored as a Beamtalk `Block` in the environment dictionary. The `apply:args:in:` method calls `func value: args` directly — no primitive dispatch table required.
- **`class =:=` type dispatch** — Beamtalk exposes class identity as a value, so `eval:in:` and `print:` use a cascade of `ifTrue:` guards rather than a `typecase` or visitor pattern. New types can be added by appending a guard.
- **`collect:` and `inject:into:` on lists** — evaluating argument lists (`tail collect: [:a | self eval: a in: env]`) and building environment frames (`params zip: vals inject: #{} into: …`) are single expressions.
- **String concatenation with `++`** — error messages like `"Not a procedure: " ++ func printString` compose naturally without a format string DSL.

## Walking Through the Tests

`SchemeTest` exercises every layer of the interpreter. Here are five representative cases.

### testAdd — the simplest eval/apply round-trip

Parses `(+ 3 4)`, looks up `+` in the default env (a Block), evaluates `3` and `4` as self-evaluating integers, then calls the block with `#(3 4)`.

```bash
sed -n '73,75p' test/scheme_test.bt
```

```output
  testAdd =>
    self assert: (self eval: "(+ 3 4)") equals: 7

```

### testLambdaApply — creating and immediately calling a closure

`((lambda (x) (* x x)) 5)` creates a `SchemeLambda` with `params=#("x")` and `body=<ast for (* x x)>`, then applies it to `5`. `extend:values:` builds a child `SchemeEnv` with `{"x" => 5}` as its bindings, then `eval:in:` recurses into the body.

```bash
sed -n '102,103p' test/scheme_test.bt
```

```output
  testLambdaApply =>
    self assert: (self eval: "((lambda (x) (* x x)) 5)") equals: 25
```

### testClosure — closures capture their defining environment

`make-adder` returns a lambda that closes over `n`. When `(make-adder 5)` is called, `n=5` is bound in the closure env. Calling `(add5 10)` evaluates `(+ n x)` in a frame where `n=5, x=10`, finding `n` by walking up to the closure env. This exercises the full `SchemeEnv` parent-chain lookup.

```bash
sed -n '172,179p' test/scheme_test.bt
```

```output
  testClosure =>
    reader := SchemeReader new
    ev     := SchemeEval new
    env    := ev defaultEnv
    ev eval: (reader read: "(define make-adder (lambda (n) (lambda (x) (+ n x))))") in: env
    ev eval: (reader read: "(define add5 (make-adder 5))") in: env
    self assert: (ev eval: (reader read: "(add5 10)") in: env) equals: 15

```

### testFactorial — recursion via define

`fact` is defined recursively: it looks itself up by name each time it recurses. This works because `define` writes into the *same* `SchemeEnv` actor that the body evaluates against — the definition is visible to itself.

```bash
sed -n '182,188p' test/scheme_test.bt
```

```output
  testNonProcedureError =>
    result := [self eval: "(1 + 2)"] on: Error do: [:e | e message]
    self assert: result equals: "Not a procedure: 1"

  testNonProcedureErrorInteger =>
    result := [self eval: "(42 1 2)"] on: Error do: [:e | e message]
    self assert: result equals: "Not a procedure: 42"
```

```bash
sed -n '188,192p' test/scheme_test.bt
```

```output
    self assert: result equals: "Not a procedure: 42"

  // --- Recursion ---

  testFactorial =>
```

```bash
sed -n '193,193p' test/scheme_test.bt
```

```output
    reader := SchemeReader new
```

### testNonProcedureError — clear error for infix notation

`(1 + 2)` is parsed as a list with `1` as the operator. The evaluator catches this with a type guard in `apply:args:in:` and raises a meaningful `"Not a procedure: 1"` rather than leaking the raw BEAM `{badfun,1}` crash. The test catches the error with `on: Error do:` and checks the message.

```bash
sed -n '182,184p' test/scheme_test.bt
```

```output
  testNonProcedureError =>
    result := [self eval: "(1 + 2)"] on: Error do: [:e | e message]
    self assert: result equals: "Not a procedure: 1"
```

## Running the Tests

```bash
beamtalk test test/scheme_test.bt
```
