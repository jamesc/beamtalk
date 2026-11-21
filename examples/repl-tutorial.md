# Beamtalk REPL Tutorial

Welcome to Beamtalk! This tutorial will guide you through using the interactive REPL to explore the language.

## Starting the REPL

```bash
beamtalk repl
```

You should see:

```
Beamtalk v0.5.0
Type :help for available commands, :exit to quit.

>
```

## Basic Expressions

Try some simple expressions:

```beamtalk
> 2 + 2
4

> 10 * 5
50

> "Hello, " ++ "Beamtalk!"
Hello, Beamtalk!
```

## Variables

Assign values to variables:

```beamtalk
> x := 42
42

> y := x + 10
52

> name := "Alice"
Alice
```

**Note:** In Beamtalk, `:=` is used for assignment (not `=`).

## String Interpolation

Beamtalk strings support interpolation with `{expr}` — embed any expression inside a string:

```beamtalk
> name := "Alice"
Alice

> "Hello, {name}!"
Hello, Alice!

> "2 + 2 = {2 + 2}"
2 + 2 = 4

> "{name length} characters"
5 characters
```

Non-string values are automatically converted via `printString`. To include literal braces in an interpolated string, prefix the brace characters with a backslash (write `\{` and `\}` inside the string):

```beamtalk
> "Set: \{1, 2, 3\}"
Set: {1, 2, 3}
```

## Viewing Bindings

See all your current variable bindings:

```beamtalk
> :bindings
Variable bindings:
  x = 42
  y = 52
  name = "Alice"
```

## Loading Files

Load a Beamtalk source file and compile it as a module with `Workspace load:`. It answers the classes it loaded:

```beamtalk
> Workspace load: "examples/getting-started/src/hello.bt"
#(Hello)
```

**Note:** `Workspace load:` is for loading class definitions, one file at a time (a directory is an error). The class name in the file determines the module name, not the filename. There is no `:load` command in the REPL.

## Working with Classes

After loading, you can see loaded classes:

```beamtalk
> Workspace classes
#(Hello)
```

Reload after editing: `Hello reload` recompiles one class from its source file, and `:sync` recompiles every changed file in the project (the directory must contain a `beamtalk.toml`):

```beamtalk
> Hello reload
Hello
```

## Working with Actors

Beamtalk is built on the actor model. Actors are objects that run concurrently and communicate via messages.

**Note:** To try the examples below, you'll first need to load an actor class. The Counter actor is available in the examples:

```beamtalk
> Workspace load: "examples/getting-started/src/counter.bt"
#(Counter)
```

Now you can create and interact with Counter actors:

### Spawning Actors

Create a new actor instance:

```beamtalk
> myCounter := Counter spawn
#Actor<Counter, pid=<0.123.0>>
```

### Sending Messages

Send messages to actors:

```beamtalk
> myCounter increment
1

> myCounter increment
2

> myCounter getValue
2
```

**Auto-await:** Message sends return Futures, but the REPL automatically awaits them for a synchronous experience.

## Blocks (Closures)

Blocks are Beamtalk's closures:

```beamtalk
> double := [ :x | x * 2 ]
#Block

> double value: 21
42

> numbers := #(1, 2, 3, 4, 5)
#(1, 2, 3, 4, 5)

> numbers collect: [ :n | n * n ]
#(1, 4, 9, 16, 25)
```

## Control Flow

Beamtalk uses message sends for control flow:

```beamtalk
> x := 5
5

> x > 10 ifTrue: [ "big" ] ifFalse: [ "small" ]
small

> count := 0
0

> [ count < 5 ] whileTrue: [ count := count + 1 ]
5
```

## REPL Commands

Use `:help` to see all available commands:

```beamtalk
> :help
Beamtalk REPL Commands:

  :help, :h, :?       Show help, or `:help <Class>` / `:help <Class> <selector>` for docs
  :exit, :quit, :q    Exit the REPL
  :clear              Clear the current session's local bindings
  :raw-mfa            Show every stack frame with its raw module:function/arity for this session: `:raw-mfa [on|off]`
  :bindings, :b       List the current session's local binding names
  :sync, :s           Sync the workspace with the project on disk (takes no arguments)
  :unload             Unload a class from the workspace: `:unload <ClassName>`
  :test, :t           Run tests, optionally scoped to one class: `:test [<ClassName>]`
  :show-codegen, :sc  Show generated Core Erlang for an expression: `:show-codegen <expr>`
  :interrupt, :int    Cancel a running evaluation
  :flush              Write pending ChangeLog entries to disk
  :changes            Show the workspace ChangeLog
  :dirty              Show per-class dirty (uncommitted) selectors
  :recheck            Re-check the whole live image: `:recheck image`
  :remove-method, :remove-class, :flush-destructive, :rename-class, :rename-method
                      Live-edit commands (some prompt for confirmation)
  ...
```

### :clear - Reset Bindings

```beamtalk
> x := 42
42

> :clear
✓ All bindings cleared

> x
Error: Undefined variable: x
```

### Workspace classes - List Loaded Classes

```beamtalk
> Workspace classes
#(Counter)

> Workspace testClasses
#()
```

### Reloading after an edit

After editing a file, recompile one class with `reload`, or everything that changed in the project with `:sync`:

```beamtalk
> Counter reload
Counter
```

## Streams — Lazy Data Pipelines

Beamtalk has a `Stream` class for lazy, composable data processing. Operations build up a pipeline — nothing computes until you use a **terminal** operation like `take:` or `asList`.

### Infinite Sequences

```beamtalk
> (Stream from: 1) take: 5
[1,2,3,4,5]

> (Stream from: 1 by: [:n | n * 2]) take: 6
[1,2,4,8,16,32]
```

### Pipeline Composition

Chain lazy operations to build a pipeline, then pull results with a terminal:

```beamtalk
> evens := (Stream from: 1) select: [:n | n isEven]
a Stream

> evens take: 5
[2,4,6,8,10]

> squares := evens collect: [:n | n * n]
a Stream

> squares take: 4
[4,16,36,64]
```

### Streaming Collections

Any collection responds to `stream` for lazy processing:

```beamtalk
> (#(1, 2, 3, 4, 5) stream) select: [:n | n > 2]
a Stream

> ((#(1, 2, 3, 4, 5) stream) select: [:n | n > 2]) asList
[3,4,5]

> ("hello" stream) take: 3
["h","e","l"]
```

**Eager vs Lazy:** Collection methods like `List select:` return a List immediately. Using `stream` first makes it lazy — you choose:

```beamtalk
> #(1, 2, 3) select: [:n | n > 1]
[2,3]

> (#(1, 2, 3) stream) select: [:n | n > 1]
a Stream
```

### File Streaming

Read files lazily — constant memory, safe for large files:

```beamtalk
> (File lines: "examples/getting-started/src/hello.bt") take: 3
["// Copyright 2026 James Casey","// SPDX-License-Identifier: Apache-2.0",""]

> (File lines: "examples/getting-started/src/hello.bt") inject: 0 into: [:count :line | count + 1]
15
```

Block-scoped handles close automatically:

```beamtalk
> (File open: "examples/getting-started/src/hello.bt" do: [:handle | handle lines take: 2]) unwrap
["// Copyright 2026 James Casey","// SPDX-License-Identifier: Apache-2.0"]
```

**Note:** File-backed Streams must be consumed by the same process that created them. To send file data to an actor, materialize with `take:` or `asList` first.

### Inspecting Pipelines

`printString` shows the pipeline structure without evaluating it:

```beamtalk
> (Stream from: 1) printString
Stream(from: 1)

> ((Stream from: 1) select: [:n | n isEven]) printString
Stream(from: 1) | select: [...]
```

### Terminal Operations Reference

| Method | Description |
|--------|-------------|
| `take:` | First N elements as List |
| `asList` | Materialize entire stream |
| `do:` | Iterate with side effects |
| `inject:into:` | Fold/reduce |
| `detect:` | First match or nil |
| `anySatisfy:` | Any element matches? |
| `allSatisfy:` | All elements match? |

**⚠️ Side effects are lazy:** In a pipeline like `stream collect: [:x | Transcript show: x. x * 2]`, the `show:` only runs when a terminal operation pulls elements through — not when the pipeline is defined.

## Transcript

`Transcript` is a class-side facade (no instance): inside the REPL it writes to the workspace's transcript; in compiled code, `beamtalk run` and tests it becomes a plain `Logger` notice. It works the same everywhere:

```beamtalk
> Transcript show: "hello"
hello
```

| Selector | Description |
|----------|-------------|
| `show:` / `showCr:` / `cr` | Write output |
| `recent` / `clear` | Read or empty the REPL transcript buffer (REPL only) |

Programs should use `Logger` or `Console` for output.

## Exiting the REPL

```beamtalk
> :exit
Goodbye!
```

Or use `:q` as a shortcut.

## Tips and Tricks

1. **Tab completion** - Press Tab to complete variable names and commands
2. **History** - Use ↑/↓ arrow keys to navigate command history
3. **Multi-line input** - Use Shift+Enter for multi-line expressions (coming soon)
4. **Error messages** - Errors show source location and suggestions
5. **Auto-await** - Message sends are automatically awaited for convenience

## Example Session

Here's a complete example session exploring basic expressions:

```beamtalk
$ beamtalk repl
Beamtalk v0.5.0
Type :help for available commands, :exit to quit.

> x := 42
42

> y := x + 10
52

> z := x * y
2184

> :bindings
Variable bindings:
  x = 42
  y = 52
  z = 2184

> Workspace load: "examples/getting-started/src/hello.bt"
#(Hello)

> Workspace classes
#(Hello)

> :exit
Goodbye!
```

## Next Steps

- Explore the `examples/` directory for more sample programs
- Read `docs/beamtalk-language-features.md` for full language syntax
- Check out `docs/beamtalk-principles.md` to understand the design philosophy
- Review `docs/known-limitations.md` for what's not yet supported
- Try building your own actors and experimenting with message passing!

## Getting Help

- Type `:help` in the REPL for command reference
- See `README.md` for installation and setup
- Report issues at: https://github.com/jamesc/beamtalk/issues

Happy coding! 🚀
