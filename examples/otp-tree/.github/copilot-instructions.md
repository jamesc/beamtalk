# Copilot Instructions for otp-tree

This is a [Beamtalk](https://jamesc.github.io/beamtalk) workspace demonstrating OTP supervision trees.

## Key Conventions

- Source files use `.bt` extension and live in `src/`
- Tests use BUnit (TestCase subclasses) and live in `test/`
- Build output goes to `_build/` (gitignored)
- Package manifest is `beamtalk.toml`

## OTP Patterns in This Workspace

- `Supervisor subclass:` — static supervision tree (children known at startup)
- `DynamicSupervisor subclass:` — dynamic pool (children added at runtime)
- `class supervisionPolicy =>` — per-actor restart strategy (`#permanent`, `#transient`, `#temporary`)
- `class children =>` — returns the list of child classes for the supervisor
- `class childClass =>` — the worker class for a DynamicSupervisor
- `supervise` — starts the supervisor (or returns the running instance); returns `Result(Self, Error)` — use `unwrap` for boot-style call sites or `ifOk:ifError:` / `andThen:` to handle failures
- `current` — returns the running supervisor instance by class name (Supervisor/DynamicSupervisor only)
- `startChild` / `startChild:` — spawns a new dynamic child; returns `Result(C, Error)` (child type narrowed via the `DynamicSupervisor(C)` parameter)
- `terminate:` / `terminateChild:` — returns `Result(Nil, Error)` and is idempotent on `not_found`

## Beamtalk Syntax

- Smalltalk-inspired message passing: `object message`, `object message: arg`
- Blocks are closures: `[:x | x + 1]`
- Use `//` for line comments
- Implicit returns (last expression is the return value)
- Use `^` only for early returns, never on the last expression

## Build Commands

```bash
beamtalk build    # Compile the project
beamtalk repl     # Start interactive REPL
beamtalk test     # Run tests
beamtalk run      # Start as OTP application (uses [application] supervisor in beamtalk.toml)
```

## MCP / Live Workspace

This workspace ships with a `.mcp.json` that starts `beamtalk-mcp` automatically.
Use `evaluate` to test expressions live and `load_file` to load source files.
