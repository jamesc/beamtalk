## OTP Supervisors

Beamtalk actors are OTP gen_servers. When an actor crashes, you want
the system to restart it automatically — that's what supervisors are for.

A supervisor watches a set of actor processes. When a child crashes, the
supervisor applies a restart strategy: restart just that child, or restart
all of them, depending on the policy.

This chapter covers:

1. `Supervisor subclass:` — static child list, known at start-up
2. Restart strategies and policies
3. `DynamicSupervisor subclass:` — add children at runtime
4. Supervision in practice: a resilient counter service

## Static supervisors

A static supervisor knows its children at start-up time. Subclass `Supervisor`
and override `class children` to return the list of actor classes to supervise:

```beamtalk
TestCase subclass: Ch17StaticSupervisor

  testSupervisorClass =>
    // Supervisor subclass: defines a supervision tree.
    // class children => returns the list of actors to supervise.
    self assert: CounterApp strategy equals: #oneForOne
    self assert: CounterApp children size equals: 1

  testSupervisorIsSupervisorFlag =>
    self assert: CounterApp isSupervisor
    self deny: Counter isSupervisor
```

The `CounterApp` supervisor is defined like this (not run as a doctest —
it requires an OTP application environment):

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  value => self.value

Supervisor subclass: CounterApp
  class children => #[Counter supervisionSpec]
```

`class children` returns an array of `SupervisionSpec` values.
The simplest spec is `SomeActorClass supervisionSpec`, which uses
the actor's default restart policy (`#temporary`).

## Supervision specs

A `SupervisionSpec` describes how to start one supervised child. It is
built from an actor class using fluent setter methods:

```beamtalk
TestCase subclass: Ch17SupervisionSpecs

  testDefaultSpecHasTemporaryRestart =>
    spec := Counter supervisionSpec
    self assert: spec restart equals: #temporary

  testCustomRestartPolicy =>
    spec := Counter supervisionSpec withRestart: #permanent
    self assert: spec restart equals: #permanent

  testCustomId =>
    spec := Counter supervisionSpec withId: #mainCounter
    self assert: spec id equals: #mainCounter

  testChainedBuilders =>
    spec := Counter supervisionSpec
      withId: #primary
      withRestart: #permanent
    self assert: spec id equals: #primary
    self assert: spec restart equals: #permanent
```

## Actor restart policies

Each actor class can declare its own default restart policy. Override
`class supervisionPolicy` to change it:

```beamtalk
TestCase subclass: Ch17RestartPolicy

  testDefaultPolicyIsTemporary =>
    // Actors default to #temporary — not restarted on crash
    self assert: Counter supervisionPolicy equals: #temporary

  testSpecInheritsActorPolicy =>
    // supervisionSpec picks up the actor's policy automatically
    spec := Counter supervisionSpec
    self assert: spec restart equals: #temporary
```

Restart policies:

| Policy | Meaning |
|--------|---------|
| `#temporary` | Never restarted (default) |
| `#transient` | Restarted on abnormal termination only |
| `#permanent` | Always restarted |

To make a worker always restart, override `class supervisionPolicy` in
the actor class (not shown as a doctest — overriding class methods
requires a class definition):

```beamtalk
Actor subclass: PersistentWorker
  class supervisionPolicy => #permanent
  state: value = 0
  increment => self.value := self.value + 1
```

## Restart strategies

Override `class strategy` on your supervisor to change the strategy:

| Strategy | Meaning |
|----------|---------|
| `#oneForOne` | Only restart the crashed child (default) |
| `#oneForAll` | Restart all children when one crashes |
| `#restForOne` | Restart the crashed child and all children started after it |

```beamtalk
TestCase subclass: Ch17Strategies

  testDefaultStrategyIsOneForOne =>
    self assert: CounterApp strategy equals: #oneForOne
```

## Dynamic supervisors

A `DynamicSupervisor` starts with no children. You add children at
runtime as demand requires. This is ideal for per-request or per-connection
workers.

Subclass `DynamicSupervisor` and override `class childClass`:

```beamtalk
TestCase subclass: Ch17DynamicSupervisor

  testDynamicSupervisorIsSupervisorFlag =>
    // DynamicSupervisor subclasses are also supervisors
    self assert: WorkerPool isSupervisor

  testDynamicSupervisorHasNoStaticChildren =>
    // Dynamic supervisors don't define class children
    self assert: WorkerPool childClass equals: Counter
```

A `WorkerPool` is defined like this:

```beamtalk
DynamicSupervisor subclass: WorkerPool
  class childClass => Counter
```

At runtime, call `startChild` / `startChild:` on a running supervisor
instance to add workers:

```beamtalk
// pool := WorkerPool supervise   // start the supervisor
// pool startChild                // add a Counter child
// pool count                     // => 1
// pool startChild                // add another
// pool count                     // => 2
```

## Supervisor lifecycle

```beamtalk
TestCase subclass: Ch17Lifecycle

  testSupervisorClassMethods =>
    // supervise — starts the supervisor as an OTP process
    // current  — retrieves a running supervisor
    // isSupervisor — true for all Supervisor subclasses
    self assert: CounterApp isSupervisor
    self assert: CounterApp strategy equals: #oneForOne
    self assert: (CounterApp children size) equals: 1
```

Key class-side API:

| Message | Returns | Description |
|---------|---------|-------------|
| `MyApp supervise` | supervisor pid | Start the supervision tree |
| `MyApp current` | supervisor pid | Get the running instance |
| `MyApp isSupervisor` | Boolean | Always `true` for supervisors |
| `MyApp strategy` | Symbol | Restart strategy |
| `MyApp children` | Array | Static child specs |

Key instance-side API:

| Message | Description |
|---------|-------------|
| `sup children` | List running children |
| `sup which: Counter` | Find a specific child |
| `sup terminate: Counter` | Stop a specific child |
| `sup count` | Count running children |
| `sup stop` | Shut down the supervisor and all children |

## Summary

Static supervision tree:

```beamtalk
Supervisor subclass: MyApp
  class strategy => #oneForOne          // optional, default
  class children => #[
    SomeActor supervisionSpec,
    OtherActor supervisionSpec withRestart: #permanent
  ]
```

Dynamic supervision tree:

```beamtalk
DynamicSupervisor subclass: WorkerPool
  class childClass => Counter

// pool := WorkerPool supervise
// pool startChild        // spawn a new Counter
// pool startChild: args  // spawn with arguments
// pool count             // how many children
```

Restart policies:

```text
#temporary    never restarted (default)
#transient    restarted on abnormal exit only
#permanent    always restarted
```

Strategies:

```text
#oneForOne    restart only the crashed child (default)
#oneForAll    restart all children when any crashes
#restForOne   restart crashed child + all after it
```

Next: this is the last chapter in the guide!
See `docs/beamtalk-language-features.md` for the full language reference.
