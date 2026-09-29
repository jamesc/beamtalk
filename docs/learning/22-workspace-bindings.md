## Workspace & Bindings

Beamtalk's system services are **class-side facades**: `Transcript`, `Workspace`
and `Beamtalk` are ordinary classes, so you send messages straight to them, in
the REPL and in compiled code alike. They provide logging, introspection and
class management. A REPL session can also hold **bindings**: names you register
yourself with `Workspace bind:as:`.

## Transcript — the shared log

`Transcript` is the REPL's shared output log, similar to Smalltalk's Transcript
window. Use it for debugging and tracing at the prompt:

```beamtalk
Transcript showCr: "Hello from Beamtalk"
Transcript show: "Step 1 complete"; cr
Transcript show: 42
```

`show:` accepts any value and appends its text (a String as-is, anything else as
its `printString`). `cr` adds a newline and `showCr:` is `show:` then `cr`. All
return `nil`; cascade them with `;`:

```beamtalk
Transcript show: "Name: "; show: "Alice"; cr; show: "Done"
```

Retrieve recent output or clear the buffer (in an interactive workspace):

```beamtalk
Transcript recent   // returns the buffer contents as a list of lines
Transcript clear    // empties the buffer
```

Outside an interactive workspace (`beamtalk run`, `beamtalk test`, a release)
there is no transcript buffer. `show:` and `showCr:` emit a plain Logger notice
instead, `cr` does nothing, and `recent` / `clear` raise `no_workspace`. Programs
should use `Logger` for diagnostics or `Console` for plain stdout/stderr;
`Transcript` is for newcomers and REPL use.

## Workspace — introspection and binding management

`Workspace` is a class-side facade over the running workspace. Use it to explore
loaded classes, load files, run tests and register custom bindings. Every
selector raises `no_workspace` when no workspace runs (for example under
`beamtalk test`); `Workspace isAvailable` asks without raising.

### Exploring classes

```beamtalk
Workspace classes       // list all loaded user classes
Workspace testClasses   // list TestCase subclasses
```

### Working with actors

Live actors are a fact about the node, so they live on `Node`:

```beamtalk
Node current actors unwrap                // list all live actors
(Node current actorsOf: Counter) unwrap   // find actors of a specific class
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
Workspace bindings                     // live view of all bindings
Workspace unbind: #Config              // remove a binding
```

A bare name in the REPL resolves in three steps: your session's local variables
first, then `Workspace` bindings, then the class registry (`Integer`,
`Counter`, `Transcript`, ...). The system refuses to bind a name that is already
a registered class. Bindings are visible only to REPL evaluations; compiled code
sees just the class registry.

## Beamtalk — system reflection

`Beamtalk` provides VM-level reflection and the class registry:

```beamtalk
Beamtalk version              // the Beamtalk version string
Beamtalk allClasses           // list all registered classes
Beamtalk classNamed: #Integer // look up a class by name
```

### Getting help

```beamtalk
Beamtalk help: Integer                       // show class documentation
Beamtalk help: Integer selector: #factorial  // show method documentation
```

## Access from compiled code

`Transcript`, `Workspace` and `Beamtalk` are ordinary classes, so compiled code
can send to them directly (`Workspace` raises `no_workspace` when no workspace
is running):

```beamtalk
Transcript showCr: "Hello from compiled code"
Workspace classes
Beamtalk version
```

## Summary

**Transcript** (class-side facade — the REPL's shared log):

```text
Transcript show: value      → nil (appends to buffer / Logger notice)
Transcript cr               → nil (appends newline / no-op)
Transcript showCr: value    → nil (show: then cr)
Transcript recent           → List (buffer contents; no_workspace outside the REPL)
Transcript clear            → nil (empties buffer; no_workspace outside the REPL)
```

**Workspace** (class-side facade — workspace operations):

```text
Workspace isAvailable        → Boolean (never raises)
Workspace classes            → List of loaded classes
Workspace testClasses        → List of TestCase subclasses
Workspace load: path         → compile and load a .bt file
Workspace test               → run all test classes
Workspace test: testClass    → run a specific test class
Workspace bind: val as: name → register a binding
Workspace unbind: name       → remove a binding
Workspace bindings           → live view of all bindings
```

**Node** (a BEAM node value — live actors):

```text
Node current actors             → Result(List of live actors)
Node current actorsOf: aClass   → Result(List of actors of that class)
```

**Beamtalk** (class-side facade — system reflection):

```text
Beamtalk version                      → String
Beamtalk allClasses                   → List of class objects
Beamtalk classNamed: name             → class object or nil
Beamtalk help: aClass                 → class documentation
Beamtalk help: aClass selector: sel   → method documentation
```

## Exercises

**1. Explore loaded classes.** In the REPL, use `Workspace classes` to see all
loaded classes. How many are there? Can you find `Integer` and `String`?

<details>
<summary>Hint</summary>

```text
classes := Workspace classes
classes size    // shows the count
// Look for specific classes:
classes includes: Integer    // likely not — classes are names
// Use Beamtalk allClasses to find by name
Beamtalk allClasses
```

`Workspace classes` lists user-loaded classes. `Beamtalk allClasses` lists
all registered classes including built-ins.
</details>

**2. Transcript cascade.** Use Transcript with cascade (`;`) to log your name,
a newline, your favorite number, and another newline — all in one expression.

<details>
<summary>Hint</summary>

```text
Transcript show: "Alice"; cr; show: 42; cr
```

Each message in the cascade goes to the same `Transcript` receiver.
</details>

**3. Help system.** Use `Beamtalk help: Integer` to explore the Integer class.
Then use `Beamtalk help: Integer selector: #factorial` to see documentation
for a specific method.

<details>
<summary>Hint</summary>

```text
Beamtalk help: Integer                     // shows class docs
Beamtalk help: Integer selector: #factorial  // shows method docs
```

The help system shows available methods, their signatures, and documentation.
</details>

Next: Chapter 23 — Streams
