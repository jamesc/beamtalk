# Beamtalk REPL Tutorial

Welcome to Beamtalk! This tutorial will guide you through using the interactive REPL to explore the language.

## Starting the REPL

```bash
beamtalk repl
```

You should see:

```
Beamtalk v0.1.0
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

> "Hello, " + "Beamtalk!"
"Hello, Beamtalk!"
```

## Variables

Assign values to variables:

```beamtalk
> x := 42
42

> y := x + 10
52

> name := "Alice"
"Alice"
```

**Note:** In Beamtalk, `:=` is used for assignment (not `=`).

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

Load a Beamtalk source file and compile it as a module:

```beamtalk
> :load examples/hello.bt
Loaded Hello
```

**Note:** `:load` is for loading class definitions. The class name in the file determines the module name, not the filename.

## Working with Modules

After loading, you can see loaded modules:

```beamtalk
> :modules
Loaded modules:
  hello - examples/hello.bt (0 actors, loaded just now)
```

Reload after editing:

```beamtalk
> :reload
Reloaded examples/hello.bt
```

## Working with Actors

Beamtalk is built on the actor model. Actors are objects that run concurrently and communicate via messages.

**Note:** To try the examples below, you'll first need to load an actor class. The Counter actor is available in the examples:

```beamtalk
> :load examples/counter.bt
Loaded examples/counter.bt
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

### List Running Actors

```beamtalk
> :actors
Running actors:
  <0.123.0> - Counter (counter) - spawned 2m ago
  <0.124.0> - Counter (counter) - spawned 1m ago
```

## Blocks (Closures)

Blocks are Beamtalk's closures:

```beamtalk
> double := [ :x | x * 2 ]
#Block

> double value: 21
42

> numbers := [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]

> numbers do: [ :n | n * n ]
[1, 4, 9, 16, 25]
```

## Control Flow

Beamtalk uses message sends for control flow:

```beamtalk
> x := 5
5

> x > 10 ifTrue: [ "big" ] ifFalse: [ "small" ]
"small"

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

  :help, :h       Show this help message
  :exit, :q       Exit the REPL
  :clear          Clear all variable bindings
  :bindings       Show current variable bindings
  :load <path>    Load a .bt file
  :reload         Reload the last loaded file
  :modules        List loaded modules
  :unload <name>  Unload a module (fails if actors exist)
  :actors         List running actors
  :kill <pid>     Kill an actor by PID
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

### :modules - List Loaded Modules

```beamtalk
> :modules
Loaded modules:
  counter - examples/counter.bt (2 actors, loaded 5m ago)
```

### :reload - Reload Last File

After editing a file:

```beamtalk
> :reload
✓ Reloaded examples/counter.bt
```

### :kill - Stop an Actor

```beamtalk
> myCounter := Counter spawn
#Actor<Counter, pid=<0.125.0>>

> :kill <0.125.0>
✓ Actor <0.125.0> killed

> myCounter getValue
Error: Actor process <0.125.0> is not alive
```

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
Beamtalk v0.1.0
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

> :load examples/hello.bt
Loaded Hello

> :modules
Loaded modules:
  hello - examples/hello.bt (0 actors, loaded just now)

> :exit
Goodbye!
```

## Next Steps

- Explore the `examples/` directory for more sample programs
- Read `docs/beamtalk-language-features.md` for full language syntax
- Check out `docs/beamtalk-principles.md` to understand the design philosophy
- Try building your own actors and experimenting with message passing!

## Getting Help

- Type `:help` in the REPL for command reference
- See `README.md` for installation and setup
- Report issues at: https://github.com/jamesc/beamtalk/issues

Happy coding! 🚀
