## Workspace & Globals

When you start the Beamtalk REPL, three global objects are automatically available:
**Transcript**, **Workspace**, and **Beamtalk**. These are workspace-scoped
singletons that provide logging, introspection, and class management.

## Transcript — the shared log

`Transcript` is an actor that acts as a shared output log, similar to Smalltalk's
Transcript window. Use it for debugging and tracing:

```beamtalk
Transcript show: "Hello from Beamtalk"
Transcript show: "Step 1 complete"; cr
Transcript show: 42
```

`show:` accepts any value, converts it to a string, and appends it to an
internal buffer. `cr` adds a newline. Both return `self`, so you can
cascade them:

```beamtalk
Transcript show: "Name: "; show: "Alice"; cr; show: "Done"
```

Retrieve recent output or clear the buffer:

```beamtalk
Transcript recent   // returns the buffer contents as a list
Transcript clear     // empties the buffer
```

### Pub/sub

Processes can subscribe to Transcript output in real-time:

```beamtalk
Transcript subscribe    // subscribe the current process
Transcript unsubscribe  // unsubscribe
```

Subscribers receive messages whenever `show:` or `cr` is called. The Transcript
automatically removes dead subscribers.

## Workspace — introspection and binding management

`Workspace` is a value object (not an actor) that provides per-workspace
introspection. Use it to explore loaded classes, manage actors, and register
custom bindings.

### Exploring classes

```beamtalk
Workspace classes       // list all loaded user classes
Workspace testClasses   // list TestCase subclasses
```

### Working with actors

```beamtalk
Workspace actors              // list all live actors
Workspace actorsOf: Counter   // find actors of a specific class
```

### Loading files

```beamtalk
Workspace load: "path/to/MyClass.bt"  // compile and load a .bt file
```

### Running tests

```beamtalk
Workspace test               // run all test classes
Workspace test: MyTest       // run a specific test class
```

### Custom bindings

Register your own workspace-level names:

```beamtalk
Workspace bind: myConfig as: #Config   // register a binding
Workspace globals                       // see all bindings as a Dictionary
Workspace unbind: #Config              // remove a binding
```

The system prevents binding names that conflict with built-in globals
(`Transcript`, `Workspace`, `Beamtalk`) or loaded class names.

## Beamtalk — system reflection

`Beamtalk` provides VM-level reflection and the class registry:

```beamtalk
Beamtalk version              // the Beamtalk version string
Beamtalk allClasses           // list all registered class names
Beamtalk classNamed: #Integer // look up a class by name
Beamtalk globals              // Dictionary of all system-level names
```

### Getting help

```beamtalk
Beamtalk help: Integer                       // show class documentation
Beamtalk help: Integer selector: #factorial  // show method documentation
```

## Access from compiled code

The convenience bindings (`Transcript`, `Workspace`, `Beamtalk`) are injected
by the workspace and are only available in the REPL and btscript contexts.

In compiled code (inside `.bt` class files), use the explicit class-variable form:

```beamtalk
TranscriptStream current show: "Hello from compiled code"
WorkspaceInterface current classes
BeamtalkInterface current version
```

Each singleton class stores its instance in a `current` class variable, set
during workspace bootstrap. This works everywhere — REPL, btscript, and
compiled classes.

## Summary

**Transcript** (actor — shared log):

```text
Transcript show: value      → self (appends to buffer)
Transcript cr               → self (appends newline)
Transcript recent           → List (buffer contents)
Transcript clear            → nil (empties buffer)
Transcript subscribe        → self (subscribe to output)
Transcript unsubscribe      → self (unsubscribe)
```

**Workspace** (value object — workspace introspection):

```text
Workspace classes            → List of loaded classes
Workspace testClasses        → List of TestCase subclasses
Workspace actors             → List of live actors
Workspace actorsOf: aClass   → List of actors of that class
Workspace load: path         → compile and load a .bt file
Workspace test               → run all test classes
Workspace test: testClass    → run a specific test class
Workspace bind: val as: name → register a binding
Workspace unbind: name       → remove a binding
Workspace globals            → Dictionary of all bindings
```

**Beamtalk** (value object — system reflection):

```text
Beamtalk version                      → String
Beamtalk allClasses                   → List of class names
Beamtalk classNamed: name             → class object or nil
Beamtalk globals                      → Dictionary of system names
Beamtalk help: aClass                 → class documentation
Beamtalk help: aClass selector: sel   → method documentation
```

Next: Chapter 23 — Streams
