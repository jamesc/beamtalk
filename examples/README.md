# Beamtalk Examples

This directory contains example Beamtalk programs demonstrating language features.

## Using Examples in the REPL

You can load these examples into the REPL using the `:load` command:

```
$ beamtalk repl
Beamtalk v0.1.0
Type :help for available commands, :exit to quit.

Connected to REPL backend.

> :load examples/hello.bt
Loaded
> :reload
Reloaded
```

## Available Examples

### hello.bt
Simple message assignment demonstrating basic expressions and string literals.

```beamtalk
message := "Hello from loaded file!"
```

### counter.bt
Counter example with assignment and arithmetic.

```beamtalk
count := 0
count + 1
```

## Future Examples

When class definitions are added to the language, examples will include:
- Actor-based classes (Counter, Calculator, etc.)
- Message sending patterns
- Inheritance and polymorphism
- Collection operations

## Notes

- Examples currently generate actor-based gen_server modules
- Class metadata will be extracted once class definitions are added to the AST
- Loaded modules persist for the REPL session (unlike eval expressions)
- Module names are prefixed with `bt_` (e.g., `hello.bt` → `bt_hello`)
