<!--
Copyright 2026 James Casey
SPDX-License-Identifier: Apache-2.0
-->
The standard library for Beamtalk, where everything is a message send.

See each class page for full API documentation and usage examples.

## Choosing a Base Class

When defining a new class, pick one of these three base classes:

| Base class | When to use |
|------------|-------------|
| `Object`   | General-purpose reference types — containers, handles, exceptions, and anything with mutable or I/O-bearing state. |
| `Value`    | Immutable scalar value types — `Boolean`, `Number`, `DateTime`, `Regex`, `CompiledMethod`, `StackFrame`. Equality is structural; instances are interchangeable. |
| `Actor`    | Stateful concurrent objects backed by a `gen_server` process. Use when an object must own private state and handle concurrent messages safely. |
