# Beamtalk Standard Library

The standard library for Beamtalk, where everything is a message send.

## Class Hierarchy

```
ProtoObject (minimal root - identity, DNU)
  └─ Object (value types - reflection, nil testing)
       ├─ Integer, Float, String, Atom  (sealed primitives)
       ├─ True, False, Nil              (sealed primitives)
       ├─ Block                         (closures)
       ├─ Collection (abstract)
       │     ├─ SequenceableCollection
       │     │     ├─ Array    (Erlang tuple)
       │     │     └─ List     (Erlang list)
       │     ├─ Set            (ordsets)
       │     └─ Dictionary     (Erlang map)
       ├─ Beamtalk                      (system reflection)
       └─ Actor                         (process-based)
            └─ (user actors: Counter, Worker, etc.)
```

See each `.bt` file for full API documentation and usage examples.

## BEAM Mapping

| Beamtalk | Erlang/BEAM | Has Process? |
|----------|-------------|--------------|
| `ProtoObject` | Abstract root | — |
| `Object` | Value type root | ❌ |
| `Actor` | `gen_server` process | ✅ |
| `Integer` | Erlang integer | ❌ |
| `Float` | Erlang float | ❌ |
| `True` / `False` | Atoms `true` / `false` | ❌ |
| `Nil` | Atom `nil` | ❌ |
| `Atom` | Erlang atom | ❌ |
| `Block` | Anonymous fun (closure) | ❌ |
| `String` | Binary `<<"UTF-8">>` | ❌ |
| `Array` | Erlang tuple `{...}` | ❌ |
| `List` | Erlang list `[...]` | ❌ |
| `Set` | `ordsets` (sorted list) | ❌ |
| `Dictionary` | Erlang map `#{...}` | ❌ |

## Design

- [Object Model](../docs/beamtalk-object-model.md) — class hierarchy and metaclasses
- [Design Principles](../docs/beamtalk-principles.md) — philosophy and rationale
- [Language Features](../docs/beamtalk-language-features.md) — full syntax specification
