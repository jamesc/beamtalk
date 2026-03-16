# sicp — Agent Guide

This workspace contains a minimal Scheme interpreter written in Beamtalk, inspired by SICP.

## Workspace Structure

```text
sicp/
├── beamtalk.toml        # Package manifest
├── src/
│   └── scheme/
│       ├── lambda.bt    # SchemeLambda — Scheme closure value type
│       ├── env.bt       # SchemeEnv — lexical environment (immutable-update)
│       └── eval.bt      # SchemeEval — tokeniser, parser, and evaluator
├── test/
│   └── scheme_test.bt   # BUnit tests for SchemeEval
├── AGENTS.md            # This file
├── .mcp.json            # MCP server config
├── .github/
│   └── copilot-instructions.md
└── .gitignore
```

## Scheme Interpreter

The interpreter supports:
- **Data types**: integers, floats, booleans (`#t`/`#f`), symbols, lists
- **Primitives**: `+`, `-`, `*`, `/`, `=`, `<`, `>`
- **Special forms**: `lambda`, `if`, `quote`
- **Error handling**: clear "Not a procedure: X" errors instead of raw BEAM crashes

### Usage

```beamtalk
eval := SchemeEval new.
eval eval: "(+ 1 2)"               // => "3"
eval eval: "(* 3 (+ 1 2))"        // => "9"
eval eval: "(lambda (x) (* x x))" // => "<procedure>"
eval eval: "(if #t 1 2)"           // => "1"
```

### Running Tests

```bash
cd examples/sicp
beamtalk test
```

## Starting the REPL

```bash
cd examples/sicp
beamtalk repl
```

## Development Workflow

The `.mcp.json` MCP server provides a persistent REPL session. Use it as
your primary development environment — not CLI commands.

**Session startup:**

1. Call `describe` to discover available operations
2. Call `load_project` with `include_tests: true` to load all source + tests
3. On a new codebase, read the language guide at https://www.beamtalk.dev/docs/language-features

**Edit → Reload → Test → Debug loop:**

1. Edit a `.bt` source file
2. `evaluate: 'Workspace load: "path"'` or `evaluate: "ClassName reload"`
   — or `load_project` again after multi-file edits
3. `test` with class name or file path — fast, no recompile
4. `evaluate` to debug failures — bindings preserved from prior calls
5. Only use CLI `beamtalk test` as a final full-suite check before committing

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

| Tool | When to use |
|------|-------------|
| `describe` | First call — discover operations and protocol version |
| `load_project` | Session startup — load all source + test files |
| `evaluate` | Test expressions, debug, call Workspace/Beamtalk APIs |
| `test` | Run tests by class name or file path |
| `complete` | Autocompletion suggestions |
| `search_examples` | Find patterns and working code (offline) |
| `show_codegen` | Inspect generated Core Erlang |
| `inspect` | Examine a live actor's state |
