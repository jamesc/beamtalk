# getting-started — Agent Guide

Introductory Beamtalk examples covering core language concepts.

## Workspace Structure

```
getting-started/
├── beamtalk.toml        # Package manifest
├── src/
│   ├── hello.bt             # Minimal class
│   ├── counter.bt           # Stateful actor
│   ├── hanoi.bt             # Towers of Hanoi (recursion)
│   ├── logging_counter.bt   # Inheritance and super
│   ├── transparent_proxy.bt # Transparent proxy pattern
│   └── stream.bt            # Lazy pipelines
├── test/
│   ├── example_counter_test.bt  # BUnit tests for Counter
│   ├── hello_test.bt            # BUnit tests for Hello
│   ├── hanoi_test.bt            # BUnit tests for Hanoi
│   └── logging_counter_test.bt  # BUnit tests for LoggingCounter
├── AGENTS.md            # This file
├── .mcp.json            # MCP server config
├── .github/
│   └── copilot-instructions.md
└── .gitignore
```

## Starting the REPL

```bash
cd examples/getting-started
beamtalk repl
```

All source files in `src/` load automatically when you start the REPL.
To run the tests: `:load "test/"`

## Beamtalk Syntax Basics

```beamtalk
// Variables
x := 42
name := "hello"

// Message sends
x factorial              // unary
3 + 4                    // binary
list at: 1 put: "value"  // keyword

// Blocks (closures)
square := [:x | x * x]
square value: 5          // => 25

// Classes
Object subclass: Counter
  state: count = 0

  increment => self.count := self.count + 1
  count => self.count
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

**Prefer MCP tools over guessing.** If you're uncertain what a method returns or
whether code is correct, evaluate it directly rather than inferring from source.

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

## Essential Patterns

### Class Hierarchy

```beamtalk
// Immutable data — auto-generates getters, withX: setters, keyword constructor, equality
Value subclass: Point
  state: x = 0
  state: y = 0

// Mutable state — manual getters/setters, self.field := works
Object subclass: Config
  state: raw = nil

// Concurrent process — gen_server backed, async casts with !
Actor subclass: Server
  state: count = 0

// OTP supervision tree — for long-running services
Supervisor subclass: MyApp
  class strategy => #oneForOne
  class children => #(DatabasePool, HttpServer, Worker)
```

Rules:
- Pure data → `Value`
- Mutable but not concurrent → `Object`
- Concurrent process → `Actor`
- Long-running service with child processes → `Supervisor` with `beamtalk run`

### String Escaping

| Syntax | Result |
|--------|--------|
| `"hello {name}"` | String interpolation |
| `"literal \{ brace \}"` | Escaped braces |
| `"She said ""hello"""` | Escaped double-quote |

### Destructuring and match:

```beamtalk
// Tuple destructuring (critical for Erlang FFI)
{#ok, content} := Erlang file read_file: "path"

// Array destructuring
#[a, b] := #[10, 20]

// Map destructuring
#{#x => x, #y => y} := someDict

// match: with clauses
value match: [
  #ok -> "success";
  #error -> "failure";
  _ -> "unknown"
]
```

### Key Stdlib Classes

| Class | Purpose |
|-------|---------|
| `System` | `getEnv:`, `osPlatform`, `pid` |
| `Subprocess` | Sync subprocess with stdin/stdout |
| `ReactiveSubprocess` | Push-mode subprocess with delegate callbacks |
| `Supervisor` | OTP supervision trees for service applications |
| `HTTPClient` / `HTTPServer` | HTTP client and server |
| `File` | Filesystem operations |
| `Json` / `Yaml` | Serialization |

### Critical Gotcha — Block Mutations

```beamtalk
// WRONG on Value/Object — assignment inside block doesn't propagate
count := 0
items do: [:x | count := count + 1]  // count is still 0!

// CORRECT — use inject:into:
count := items inject: 0 into: [:acc :x | acc + 1]
```

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
| `:` for type annotations | `::` (double-colon) | `state: x :: Integer = 0`, `param :: Type -> ReturnType =>` |
| Unknown message raises error | Same — DNU raises `does_not_understand` error | Use `respondsTo:` to check before sending |

**Implicit return rule:** the last expression of a method body is always its
return value. Never write `^` on the last line — only use it for early exits
inside the method:

```beamtalk
// Wrong — redundant ^
max: other =>
  ^(self > other ifTrue: [self] ifFalse: [other])

// Correct
max: other =>
  self > other ifTrue: [self] ifFalse: [other]

// Correct use of ^ for early return
safeDiv: other =>
  other = 0 ifTrue: [^0].
  self / other
```

## Language Documentation

- **Full language reference:** https://www.beamtalk.dev/docs/language-features — read this when starting work on a new Beamtalk codebase
- Syntax rationale: https://www.beamtalk.dev/docs/syntax-rationale
- Examples: see files in this directory
