# {{project_name}} — Agent Guide

## Project Structure

```text
{{project_name}}/
├── beamtalk.toml          # Package manifest and dependencies
├── src/                   # Source files (.bt), one class per file
├── test/                  # BUnit tests (TestCase subclasses)
├── _build/                # Build output (generated, gitignored)
├── Justfile               # just build / test / fmt / lint / ci
├── AGENTS.md              # This file
├── .github/
│   ├── copilot-instructions.md
│   └── workflows/ci.yml
├── .mcp.json              # MCP server config (live REPL for agents)
├── README.md
└── .gitignore
```

Each `.bt` file holds exactly one class, named in snake_case after it:
`Counter` lives in `src/counter.bt`, `CheckRegistry` in
`src/check_registry.bt`, `CounterTest` in `test/counter_test.bt`. Putting two
classes in one file is a compile error, and so is a file name that doesn't
match its class.

## Build & Run

```bash
beamtalk build       # Compile to BEAM bytecode
beamtalk test        # Run BUnit tests
beamtalk fmt         # Format sources in place (fmt-check to verify)
beamtalk lint        # Style and redundancy checks
beamtalk repl        # Interactive development (auto-loads the package)
beamtalk run         # Run an application (projects created with --app)
just ci              # fmt + lint + build + test, as CI runs it
```

Dependencies: `beamtalk deps add <name>` (registry), `--git <url>` or
`--path <dir>`. For example, `HTTPClient`/`HTTPServer` come from the `http`
package and `Yaml` from `yaml`; they are not in the standard library.

## Beamtalk Syntax Basics

```beamtalk
// Variables: no declarations, just assign
x := 42
name := "world"
greeting := "hello {name}"      // string interpolation
both := "hello " ++ name        // concatenation

// Message sends
x factorial                     // unary
3 + 4                           // binary
x between: 1 and: 100           // keyword
#(1, 2, 3) inject: 0 into: [:sum :each | sum + each]

// Blocks (closures)
square := [:n | n * n]
square value: 5                 // => 25

// Equality: =:= strict, == loose (1 == 1.0). There is no bare `=`.
x =:= 42
```

Type annotations use `::` (double colon): `field: x :: Integer = 0`,
`add: n :: Integer -> Integer => ...`. A `typed` class
(`typed Value subclass: Point`) asks the checker to warn on missing annotations.

## Class Kinds

The data keyword states the mutability contract, and using the wrong one is a
compile error.

```beamtalk
// Value: immutable data, no process. field: slots; you get getters,
// withX: copies, a keyword constructor (Point x: 3 y: 4) and structural equality.
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0

  + other :: Point -> Point => Point x: self.x + other x y: self.y + other y
```

```beamtalk
// Actor: a BEAM process (gen_server). state: slots are mutable.
Actor subclass: Counter
  state: count :: Integer = 0

  increment -> Integer => self.count := self.count + 1
  count -> Integer => self.count
```

```beamtalk
// Object: no instance data at all (state:/field: are compile errors).
// Use it for class-method namespaces and FFI wrappers.
Object subclass: MathUtil
  class square: n :: Integer -> Integer => n * n
```

```beamtalk
// Supervisor: an OTP supervision tree; children start in order.
Supervisor subclass: AppSup
  class children => #(Counter)
```

Choosing:
- Plain data → `Value`
- Mutable state or concurrency → `Actor` (`Counter spawn`, `Counter spawnWith: #{#count => 5}`)
- Stateless helpers / FFI namespace → `Object` with class methods
- A long-running service → `Supervisor`, started with `beamtalk run` (see `[application]` in `beamtalk.toml`)

Class variables shared by all instances are declared with `classState:`.

## Development Workflow

The `.mcp.json` MCP server gives you a live REPL session; Claude Code starts
it automatically via `beamtalk-mcp --start`. Use it as your primary
environment, and check behaviour by evaluating code rather than inferring
it from source.

**Session startup:**

1. Call `describe` to discover the available tools and protocol version
2. Call `load_project` with `include_tests: true` to load all source and tests
3. On a new codebase, read the language guide at https://www.beamtalk.dev/docs/language-features

**Edit → reload → test loop:**

1. Edit a `.bt` file
2. `reload_class` (or `load_file`; `load_project` again after multi-file edits)
3. `test` with a class name or file path: fast, no full rebuild
4. `evaluate` to debug: bindings persist between calls
5. Run `beamtalk test` from the CLI as the final full-suite check before committing

**MCP tools:**

| Tools | Use for |
|-------|---------|
| `describe`, `load_project`, `load_file`, `reload_class` | Start a session; load and reload code |
| `evaluate`, `test`, `complete` | Run expressions and tests; completion |
| `docs`, `search_examples`, `search_classes`, `list_classes` | Class and method docs; idiomatic examples (offline) |
| `lint`, `diagnostic_summary` | Diagnostics without a full build |
| `try_method`, `precheck_method`, `save_method`, `save_class` | Live-patch a method, then write it back to source |
| `inspect`, `list_actors`, `supervision_tree`, `actor_stats` | Look at running actors |
| `show_codegen` | Inspect the generated Core Erlang |

`describe` lists the rest (tracing, refactoring renames, packages, nodes).

Useful `evaluate` expressions: `Beamtalk help: Counter` (class docs),
`Workspace load: "src/counter.bt"`, `Counter reload`, `Workspace classes`.

## Essential Patterns

### Errors and Results

```beamtalk
// Erlang FFI returns {ok, V} / {error, R} as a Result
(Erlang file read_file: "config.json")
  ifOk: [:content | content size]
  ifError: [:reason | 0]

// Chain fallible steps; unwrap raises on an error
size := ((Erlang file read_file: "config.json") andThen: [:c | Result ok: c size]) unwrap

// Exceptions: raise with error:, catch with on:do: or capture with tryDo:
[1 / 0] on: Error do: [:e | 0]
outcome := Result tryDo: [1 / 0]
outcome isError
```

An unknown message raises a `does_not_understand` error; use `respondsTo:` to
check first.

### Destructuring and match:

```beamtalk
// tuple (tuples come from Erlang; {ok, _}/{error, _} arrive as a Result instead)
{front, back} := Erlang lists split: 2 with: #(1, 2, 3)
#[first, second] := #[10, 20]       // array, exact size
#{#x => px} := #{#x => 1, #y => 2}  // dictionary, partial match
count := front size

count match: [
  0 -> "none";
  1 -> "one";
  _ -> "many"
]
```

### Block Mutations

Blocks given to control-flow and collection messages (`do:`, `collect:`,
`inject:into:`, `ifTrue:`/`ifNotNil:`, `and:`/`or:`, `whileTrue:`, `on:do:`,
`Result tryDo:`, `at:ifAbsent:`, ...) write the enclosing method's locals, in
Value, Actor and Object methods alike. No accumulator dict, tuple fold or
recursive helper is needed:

```beamtalk
count := 0
#(1, -2, 3) do: [:n | n > 0 ifTrue: [count := count + 1]]
count
```

Where a write cannot be handed back, the compiler rejects it rather than
dropping it silently: a block that writes a local and is passed to a
user-defined method (such as `Result andThen:`), stored, or returned is a
compile error. Return the value from the block instead.

### Actors

A message send to an actor is a synchronous `gen_server:call` (5s default
timeout). Postfix `!` sends an async cast and returns `nil`:
`counter increment!`. Two actors calling each other synchronously deadlock
until the timeout, so use `!` for notifications.

### Tests (BUnit)

```beamtalk
TestCase subclass: PointTest
  field: origin :: Point = Point new

  // TestCase is a Value: setUp returns the updated test. End it with a
  // field assignment or an explicit `self`, or the field stays nil.
  setUp =>
    self.origin := Point x: 0 y: 0
    self

  testAdd =>
    sum := self.origin + (Point x: 1 y: 2)
    self assert: sum x equals: 1
    self deny: sum y =:= 0

  testDivideByZero => self should: [1 / 0] raise: #badarith
```

## Not Smalltalk: Common Pitfalls

Beamtalk looks like Smalltalk but has important differences. The compiler
catches most of these, but they waste time:

| Smalltalk habit | Beamtalk equivalent | Notes |
|---|---|---|
| `\| temp \|` temp declarations | Just use `:=` | No declaration syntax, in methods or blocks |
| Trailing `.` on every statement | Newline separates statements | `.` is optional |
| `"a comment"` | `// a comment` | Double-quoted text is a string; `///` is a doc comment |
| `'single quotes'` | `"double quotes"` | `""` inside a string is a literal `"` |
| `a = b` | `a =:= b` (or loose `==`) | Bare `=` is not an operator |
| `^value` on the last line | Just `value` | `^` is only for early returns |
| Left-to-right binary (`2+3*4 = 20`) | Math precedence (`2+3*4 = 14`) | `*` binds tighter than `+` |
| `'hello', name` | `"hello {name}"` or `++` | |
| `:` type annotations | `::` | `field: x :: Integer = 0` |
| `instanceVariableNames:` | `field:` (Value) / `state:` (Actor) | `Object` subclasses hold no data |

`^` inside a block is a non-local return: it exits the enclosing *method*.

```beamtalk
Object subclass: Search
  class firstPositive: items :: List(Integer) -> Integer | Nil =>
    items do: [:n | n > 0 ifTrue: [^n]]
    nil
```

## Language Documentation

- Full language reference: https://www.beamtalk.dev/docs/language-features (read it when starting work on a new Beamtalk codebase)
- Syntax rationale: https://www.beamtalk.dev/docs/syntax-rationale
- The MCP `search_examples` tool finds idiomatic, working code offline
