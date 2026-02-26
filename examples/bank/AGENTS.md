# bank — Agent Guide

Actor coordination example: bank accounts, transfers, and overdraft protection.

## Workspace Structure

```
bank/
├── beamtalk.toml        # Package manifest
├── src/
│   ├── account.bt           # Account actor (balance, deposit, withdraw)
│   ├── bank.bt              # Bank actor (account registry, factory)
│   ├── transaction.bt       # Transaction value type (immutable record)
│   ├── transfer_agent.bt    # TransferAgent actor (coordinates two accounts)
│   └── typed_account.bt     # TypedAccount variant
├── test/                # BUnit tests (add your own)
├── AGENTS.md            # This file
├── .mcp.json            # MCP server config
├── .github/
│   └── copilot-instructions.md
└── .gitignore
```

## Starting the REPL

```bash
cd examples/bank
beamtalk repl
```

## Loading the Example

```
> :load account.bt
> :load transaction.bt
> :load bank.bt
> :load transfer_agent.bt
```

## Key Concepts

- **Actor as entity** — each `Account` is its own BEAM process
- **Value types vs actors** — `Transaction` is immutable data; `Account` is stateful
- **Factory pattern** — `Bank openAccount:` spawns `Account` actors
- **Error handling** — overdraft raises a structured error

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

**Prefer MCP tools over guessing.** If you're uncertain what a method returns or
whether code is correct, evaluate it directly rather than inferring from source.

| Tool | When to use |
|------|-------------|
| `evaluate` | Test expressions, explore values, prototype code snippets |
| `load_file` | Load a `.bt` file into the workspace before evaluating it |
| `reload_module` | Hot-reload a module after editing — migrates live actors |
| `list_modules` | Check what's currently loaded |
| `list_actors` | See running actors and their classes |
| `inspect` | Examine a live actor's state by PID |
| `run_tests` | Run BUnit tests (class name, or all) |
| `docs` | Look up stdlib class or method docs — primary stdlib reference |
| `info` | Get full symbol info: superclass chain, methods, source location |
| `show_codegen` | Inspect generated Core Erlang to debug compilation |
| `get_bindings` | See current REPL variable bindings |

**Stdlib reference:** use `docs` and `info` instead of guessing. The stdlib
source lives inside the beamtalk installation, not in this project. Ask the
live workspace:
- `docs: "Integer"` — all Integer methods with docs
- `docs: "Dictionary"` — Dictionary (used by Bank for account registry)
- `info: "Dictionary"` — superclass chain, method list, source path

**Typical workflow:**
1. Edit a `.bt` source file
2. `load_file` (new module) or `reload_module` (existing) to apply changes
3. `evaluate` to verify behaviour interactively
4. `run_tests` to confirm correctness

## Not Smalltalk — Common Pitfalls

Beamtalk looks like Smalltalk but has important differences. The compiler will
catch most of these, but they waste time:

| Smalltalk habit | Beamtalk equivalent | Notes |
|---|---|---|
| `\| temp \|` temp var declarations | Just use `:=` directly | No declaration syntax |
| Trailing `.` on every statement | Newline is the separator | `.` is optional; use it only to disambiguate cascades |
| `"this is a comment"` | `// this is a comment` | Double-quoted strings are data, not comments |
| `^value` on last expression | Just write `value` | `^` is early-return only; last expr is implicitly returned |
| Left-to-right binary (`2+3*4=20`) | Standard math precedence (`2+3*4=14`) | `*` binds tighter than `+` |
| `'hello', name` concatenation | `"hello {name}"` interpolation | `++` also works: `"hello" ++ name` |
| `[:x \| \|temp\| temp := x]` block locals | `[:x \| temp := x]` | No block-local declarations |

**Implicit return rule:** the last expression of a method body is always its
return value. Never write `^` on the last line — only use it for early exits
inside the method:

```beamtalk
// Correct use of ^ for early return
safeDiv: other =>
  other = 0 ifTrue: [^0].
  self / other
```

## Language Documentation

- Language features: https://jamesc.github.io/beamtalk/docs/language-features.html
- Syntax rationale: https://jamesc.github.io/beamtalk/docs/syntax-rationale.html
- Examples: see files in this directory
