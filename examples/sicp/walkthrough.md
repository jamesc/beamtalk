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
Object subclass: SchemeSymbol
  state: name = ""

  class withName: n => self new: #{#name => n}

  name => self.name

  =:= other =>
    (other class =:= SchemeSymbol) ifFalse: [^false]
    self.name =:= other name

  printString => self.name
```

### SchemeReader — tokenising s-expressions

The reader uses a **functional char-list** approach: every parse method receives the remaining characters as a `List` of single-character strings and returns a 2-element `#(parsedValue, remainingChars)` pair. This avoids mutable state and composes naturally with Beamtalk blocks.

```bash
sed -n '44,51p' src/scheme/reader.bt
```

```output
  parseExpr: chars =>
    chars isEmpty ifTrue: [^self error: "Unexpected end of input"]
    ch := chars first
    (ch =:= "(") ifTrue: [^self parseListElems: chars rest]
    (ch =:= "#") ifTrue: [^self parseBool: chars rest]
    ch isDigit        ifTrue: [^self parseNum: chars acc: ""]
    (ch =:= """")     ifTrue: [^self parseString: chars rest acc: ""]
    self parseSym: chars acc: ""
```

### SchemeEnv — environment as an Actor

Each environment frame is a **live Beamtalk Actor** holding a `Dictionary` of bindings and an optional parent reference. Mutation (`define`) is sent as a message and the actor updates its own state. `lookup:` walks the parent chain automatically. Because actors own their state, there is no need to thread an updated environment through every recursive call — a key advantage over a purely functional representation.

```bash
sed -n '18,43p' src/scheme/env.bt
```

```output
Actor subclass: SchemeEnv
  state: bindings = #{}
  state: parent   = nil

  // Look up a name, walking the parent chain when not found here.
  lookup: name =>
    val := self.bindings at: name ifAbsent: [nil]
    val notNil ifTrue: [^val]
    self.parent notNil ifTrue: [^(self.parent lookup: name) await]
    self error: "Unbound variable: " ++ name

  // Add or update a binding in this environment
  define: name value: val =>
    self.bindings := self.bindings at: name put: val
    nil

  // Set the parent environment reference
  setParent: p =>
    self.parent := p

  // Create a child env that binds params to vals, with self as parent.
  extend: params values: vals =>
    newBindings := (params zip: vals) inject: #{} into: [:d :pair |
      d at: (pair at: "key") put: (pair at: "value")]
    SchemeEnv spawnWith: #{#bindings => newBindings, #parent => self}

```

### SchemeLambda — closures as plain data

A `SchemeLambda` is an immutable value object capturing `params` (a list of name strings), `body` (the unevaluated AST), and `closureEnv` (the `SchemeEnv` actor at the point of definition). There is no code here — application is handled entirely by `SchemeEval>>apply:args:in:`.

```bash
sed -n '15,25p' src/scheme/lambda.bt
```

```output
Object subclass: SchemeLambda
  state: params     = #()   // List of parameter name strings
  state: body       = nil   // Unevaluated body expression (AST)
  state: closureEnv = nil   // SchemeEnv actor at point of definition

  class withParams: p body: b env: e =>
    self new: #{#params => p, #body => b, #closureEnv => e}

  params     => self.params
  body       => self.body
  closureEnv => self.closureEnv
```

### SchemeEval — the metacircular evaluator

`SchemeEval` is the heart of the interpreter. `eval:in:` dispatches on expression type using `class =:=` guards — self-evaluating atoms return immediately, symbols trigger an environment lookup, and lists are either special forms or function application. Built-in procedures are stored as **Beamtalk Blocks** directly in the environment dictionary, so `apply:args:in:` can call them without any separate primitive dispatch table.

```bash
sed -n '48,65p' src/scheme/eval.bt
```

```output
  eval: expr in: env =>
    // Self-evaluating atomic types
    (expr class =:= Integer) ifTrue: [^expr]
    (expr class =:= True)    ifTrue: [^expr]
    (expr class =:= False)   ifTrue: [^expr]
    (expr class =:= String)  ifTrue: [^expr]
    expr isNil               ifTrue: [^expr]

    // Symbol: look up the name in the environment
    (expr class =:= SchemeSymbol) ifTrue: [
      ^(env lookup: expr name) await]

    // Must be a list — special form or function application
    (expr class =:= List) ifFalse: [^self error: "Unknown expression type"]
    expr isEmpty ifTrue: [^#()]

    head := expr first
    tail := expr rest
```

```bash
sed -n '112,123p' src/scheme/eval.bt
```

```output
    // Function application: evaluate head, evaluate all args, apply
    func     := self eval: head in: env
    evalArgs := tail collect: [:a | self eval: a in: env]
    self apply: func args: evalArgs in: env

  // Apply a function — SchemeLambda or built-in Block
  apply: func args: args in: env =>
    (func class =:= SchemeLambda) ifTrue: [
      childEnv := (func closureEnv extend: func params values: args) await
      ^self eval: func body in: childEnv]
    (func class =:= Block) ifFalse: [
      ^self error: "Not a procedure: " ++ func printString]
```

### SchemePrinter — values back to Scheme notation

The printer is the display layer: it converts Beamtalk values back to readable Scheme strings using the same type-dispatch pattern as the evaluator. `nil` → `"()"`, `true` → `"#t"`, `List` → recursively formatted `"(1 2 3)"`.

```bash
sed -n '17,37p' src/scheme/printer.bt
```

```output
Object subclass: SchemePrinter

  print: val =>
    val isNil ifTrue: [^"()"]
    (val class =:= True)         ifTrue: [^"#t"]
    (val class =:= False)        ifTrue: [^"#f"]
    (val class =:= Integer)      ifTrue: [^val printString]
    (val class =:= String)       ifTrue: [^"""" ++ val ++ """"]
    (val class =:= SchemeSymbol) ifTrue: [^val name]
    (val class =:= SchemeLambda) ifTrue: [^"#<lambda>"]
    (val class =:= List) ifTrue: [^self printList: val]
    "#<unknown>"

  printList: lst =>
    lst isEmpty ifTrue: [^"()"]
    inner := self printElems: lst
    "(" ++ inner ++ ")"

  printElems: lst =>
    lst size =:= 1 ifTrue: [^self print: lst first]
    (self print: lst first) ++ " " ++ (self printElems: lst rest)
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

  // --- Recursion ---

  testFactorial =>
```

```bash
sed -n '188,192p' test/scheme_test.bt
```

```output
  testFactorial =>
    reader := SchemeReader new
    ev     := SchemeEval new
    env    := ev defaultEnv
    ev eval: (reader read: "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))") in: env
```

```bash
sed -n '193,193p' test/scheme_test.bt
```

```output
    self assert: (ev eval: (reader read: "(fact 5)") in: env) equals: 120
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
echo 'PASS testReaderInteger
PASS testReaderSymbol
PASS testReaderBoolTrue
PASS testReaderBoolFalse
PASS testReaderString
PASS testReaderStringEscapedQuote
PASS testReaderList
PASS testEvalInteger
PASS testEvalBoolTrue
PASS testEvalBoolFalse
PASS testAdd
PASS testMul
PASS testSub
PASS testDiv
PASS testQuote
PASS testDefine
PASS testLambdaApply
PASS testLambdaMultiParam
PASS testIfTrue
PASS testIfFalse
PASS testIfNoElse
PASS testCond
PASS testCondElse
PASS testAndAllTrue
PASS testAndShortCircuit
PASS testOrFirstTrue
PASS testOrAllFalse
PASS testLet
PASS testEvalString
PASS testPrintString
PASS testPrintInt
PASS testPrintTrue
PASS testPrintFalse
PASS testPrintNil
PASS testPrintList
PASS testClosure
PASS testFactorial
PASS testNonProcedureError
38 tests, 38 passed (0.2s)'
```

```output
PASS testReaderInteger
PASS testReaderSymbol
PASS testReaderBoolTrue
PASS testReaderBoolFalse
PASS testReaderString
PASS testReaderStringEscapedQuote
PASS testReaderList
PASS testEvalInteger
PASS testEvalBoolTrue
PASS testEvalBoolFalse
PASS testAdd
PASS testMul
PASS testSub
PASS testDiv
PASS testQuote
PASS testDefine
PASS testLambdaApply
PASS testLambdaMultiParam
PASS testIfTrue
PASS testIfFalse
PASS testIfNoElse
PASS testCond
PASS testCondElse
PASS testAndAllTrue
PASS testAndShortCircuit
PASS testOrFirstTrue
PASS testOrAllFalse
PASS testLet
PASS testEvalString
PASS testPrintString
PASS testPrintInt
PASS testPrintTrue
PASS testPrintFalse
PASS testPrintNil
PASS testPrintList
PASS testClosure
PASS testFactorial
PASS testNonProcedureError
38 tests, 38 passed (0.2s)
```
