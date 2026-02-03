# ADR 0001: No Compound Assignment in Beamtalk

## Status

Proposed

## Context

Beamtalk initially planned to support compound assignment operators (`+=`, `-=`, `*=`, `/=`) as documented in various design documents.

However, compound assignment breaks Smalltalk's message-passing paradigm:

- `x + 1` is a message send (receiver: `x`, selector: `+`, argument: `1`)
- `x += 1` is NOT a message send—it's a special syntactic form that mutates in place
- In Smalltalk, everything (except assignment and return) is a message send
- Compound assignment exists "outside" the object model and cannot be overridden or customized

## Decision

**Do not support compound assignment operators (`+=`, `-=`, `*=`, `/=`) in Beamtalk.**

Instead, use explicit assignment with message sends:

```beamtalk
// Not supported:
x += 1
self.value *= 2

// Beamtalk way:
x := x + 1              // explicit message send
self.value := self.value * 2
```

## Consequences

### Positive

1. **Smalltalk purity:** Maintains message-passing paradigm. `x + 1` remains a pure message send that composes with other operations.

2. **Simpler language:** Fewer special syntactic forms. Everything (except `:=` and `^`) is a message send.

3. **Self-documenting code:** `x := x + 1` makes the operation explicit and clear.

4. **Enables Erlang operator alignment:** Frees up `/=` for use as a comparison operator (see ADR 0002).

5. **Consistency:** All operations follow the same pattern: message send, then assignment.

### Negative

1. **More verbose:** `x := x + 1` is longer than `x += 1` (but this is the Smalltalk way).

2. **Breaking change:** Need to update all existing documentation and examples that use compound assignment.

3. **Developer expectations:** Modern developers familiar with C-family languages expect compound assignment to exist.

### Neutral

1. **Field access repetition:** `self.value := self.value + 1` repeats `self.value`, but this is standard in Smalltalk.

## Migration Path

1. **Update documentation:**
   - `docs/beamtalk-syntax-rationale.md` - Remove compound assignment section
   - `docs/beamtalk-language-features.md` - Replace all `+=`, `-=`, `*=`, `/=` examples
   - All other docs with compound assignment examples

2. **Update implementation:**
   - Ensure compound assignment is not parsed (verify parser rejects it)
   - Update tests to use explicit assignment

## References

- Smalltalk-80 Blue Book: Assignment and message sending conventions
- `docs/beamtalk-syntax-rationale.md`: Original design rationale
- ADR 0002: Use Erlang comparison operators directly (depends on this decision)
- Discussion date: 2026-02-03

## Notes

This decision prioritizes:
1. **Smalltalk message-passing purity** over syntactic convenience
2. **Language simplicity** over C-family conventions

The slight increase in verbosity (`x := x + 1` vs `x += 1`) is acceptable given Beamtalk's goal of being Smalltalk-inspired with a pure message-passing model.
