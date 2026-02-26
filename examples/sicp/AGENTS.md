# sicp — Agent Guide

This workspace contains a minimal Scheme interpreter written in Beamtalk, inspired by SICP.

## Workspace Structure

```
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

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

| Tool | When to use |
|------|-------------|
| `evaluate` | Test expressions, explore values, prototype code snippets |
| `load_file` | Load a `.bt` file into the workspace before evaluating it |
| `reload_module` | Hot-reload a module after editing — migrates live actors |
| `docs` | Look up stdlib class or method docs — primary stdlib reference |
| `run_tests` | Run BUnit tests (class name, or all) |
