# Scheme Interpreter

A SICP-inspired metacircular Scheme interpreter written in Beamtalk, demonstrating:

- **Lists as s-expressions** — Beamtalk's `List` type directly represents the Scheme AST
- **Dictionaries as environments** — `Dictionary` holds variable bindings at each scope level
- **Blocks as built-in procedures** — Beamtalk closures serve as Scheme primitives
- **Actors for mutable state** — `SchemeEnv` is a BEAM actor so `define` persists naturally

## Files

| File | Class | Role |
|------|-------|------|
| `symbol.bt` | `SchemeSymbol` | Distinct symbol type for identifier lookup |
| `reader.bt` | `SchemeReader` | Tokenise + parse s-expression strings into AST |
| `env.bt` | `SchemeEnv` | Actor-based lexical environment with parent chain |
| `lambda.bt` | `SchemeLambda` | Closure: captures params, body, and defining env |
| `eval.bt` | `SchemeEval` | Metacircular evaluator with special-form dispatch |
| `printer.bt` | `SchemePrinter` | Convert evaluated values back to Scheme notation |
| `scheme_test.bt` | `SchemeTest` | BUnit test suite |

## Supported Syntax

```scheme
; Literals
42  #t  #f  "hello"  ()

; Special forms
(quote expr)
(define name expr)
(lambda (params...) body)
(if test then else?)
(cond (test expr) ... (else expr))
(and expr ...)
(or expr ...)
(let ((var expr) ...) body)

; Built-in procedures
(+ - * /)  (= < > <= >=)
(car cdr cons list)
(not null?)
```

## Usage

```beamtalk
reader  := SchemeReader new
ev      := SchemeEval new
printer := SchemePrinter new
env     := ev defaultEnv

eval := [:src |
  printer print: (ev eval: (reader read: src) in: env)]

eval value: "(* 6 7)"                          // => "42"
eval value: "(define square (lambda (x) (* x x)))"
eval value: "(square 9)"                       // => "81"
eval value: "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"
eval value: "(fact 10)"                        // => "3628800"
```

## Running Tests

```bash
beamtalk test src/scheme/scheme_test.bt
```

Or in the REPL load each file in order (symbol → reader → env → lambda → eval → printer → scheme_test) then run `SchemeTest runAll`.
