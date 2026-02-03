# ADR 0001: Drop Compound Assignment for Erlang Operator Alignment

## Status

Proposed

## Context

Beamtalk initially planned to support compound assignment operators (`+=`, `-=`, `*=`, `/=`) as documented in various design documents. However, this creates a conflict:

1. **Operator conflict:** The `/=` operator is needed for compound assignment (`x /= 2` meaning `x := x / 2`), but Erlang uses `/=` for loose inequality (the negation of `==`).

2. **Erlang operator mapping:** Erlang has four comparison operators:
   - `==` - loose equality (with type coercion)
   - `/=` - loose inequality (with type coercion)
   - `=:=` - strict equality (no coercion)
   - `=/=` - strict inequality (no coercion)

3. **Current incomplete design:** Beamtalk's current design maps:
   - `==` → Erlang `==` ✅
   - `~=` → Erlang `/=` (using `~` to avoid conflict with `/=` compound assignment)
   - `=` → Erlang `=:=` ✅
   - *(missing)* → Erlang `=/=`

This creates an asymmetry (strict equality exists, but no strict inequality) and prevents full Erlang operator alignment.

4. **Philosophical misalignment:** Compound assignment breaks Smalltalk's message-passing paradigm:
   - `x + 1` is a message send (receiver: `x`, selector: `+`, argument: `1`)
   - `x += 1` is NOT a message send—it's a special syntactic form that mutates in place
   - In Smalltalk, everything (except assignment and return) is a message send
   - Compound assignment exists "outside" the object model and cannot be overridden or customized

## Decision

**Drop compound assignment operators (`+=`, `-=`, `*=`, `/=`) from Beamtalk.**

Instead, use explicit assignment:

```beamtalk
// Before (with compound assignment):
x += 1
self.value *= 2

// After (explicit assignment):
x := x + 1
self.value := self.value * 2
```

This allows us to adopt Erlang's comparison operators directly:

```beamtalk
// Full Erlang operator alignment:
x == y      // loose equality (Erlang ==)
x /= y      // loose inequality (Erlang /=)
x =:= y     // strict equality (Erlang =:=)
x =/= y     // strict inequality (Erlang =/=)
```

## Consequences

### Positive

1. **Perfect Erlang alignment:** All four Erlang comparison operators map 1:1 to Beamtalk operators. No mental translation needed.

2. **Smalltalk purity:** Maintains message-passing paradigm. `x + 1` remains a pure message send that composes with other operations.

3. **Simpler language:** Fewer special syntactic forms. Everything (except `:=` and `^`) is a message send.

4. **Symmetric operator set:** Both equality and inequality have loose and strict variants.

5. **Visual consistency:**
   - `:` means "strict/exact" (`:=` for binding, `=:=` for comparison)
   - `/` means "not" (`/=` for inequality, `=/=` for strict inequality)

6. **Self-documenting code:** `x := x + 1` makes the operation explicit and clear.

### Negative

1. **More verbose:** `x := x + 1` is longer than `x += 1` (but this is the Smalltalk way).

2. **Breaking change:** Need to update all existing documentation and examples that use compound assignment.

3. **Developer expectations:** Modern developers familiar with C-family languages expect compound assignment to exist.

### Neutral

1. **Field access repetition:** `self.value := self.value + 1` repeats `self.value`, but this is standard in Smalltalk.

## Migration Path

1. **Update documentation:**
   - `docs/beamtalk-syntax-rationale.md` - Remove compound assignment section, update operator table
   - `docs/beamtalk-language-features.md` - Replace all `+=`, `-=`, `*=`, `/=` examples
   - All other docs with compound assignment examples

2. **Update implementation:**
   - Remove compound assignment parsing (if implemented)
   - Update operator mapping: `~=` → `/=`, add `=/=` support
   - Update tests

3. **Update ProtoObject.bt:**
   - Remove `~=` method (it becomes a built-in operator)
   - Document new operator semantics

4. **Update this issue (BT-188):**
   - Change operator mappings to use `/=` and `=/=`
   - Update equality.bt test file

## References

- BT-188: Equality semantics decision
- Erlang documentation: http://erlang.org/doc/reference_manual/expressions.html#term-comparisons
- Smalltalk-80 Blue Book: Assignment and message sending conventions
- `docs/beamtalk-syntax-rationale.md`: Original design rationale
- Discussion date: 2026-02-03

## Notes

This decision prioritizes:
1. **Erlang interoperability** over C-family conventions
2. **Smalltalk message-passing purity** over syntactic convenience
3. **Language simplicity** over developer convenience

The slight increase in verbosity is acceptable given Beamtalk's goals of being Smalltalk-inspired and BEAM-first.
