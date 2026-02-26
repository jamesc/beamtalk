# SICP Scheme Compiler

> **Coming soon** — A Scheme interpreter/compiler following the structure of
> _Structure and Interpretation of Computer Programs_ (Abelson & Sussman),
> implemented in Beamtalk.

## Planned Scope

- Scheme tokenizer and parser
- Environment model evaluator (Chapter 4)
- Metacircular evaluator
- Compilation to Beamtalk bytecode (Chapter 5)

## Starting the Workspace

```bash
cd examples/sicp
beamtalk repl
```

## What This Will Demonstrate

- Recursive descent parsing
- Environment-based interpretation
- First-class functions and closures
- Tail-call optimization on the BEAM
- Metaprogramming: a language implementing itself
