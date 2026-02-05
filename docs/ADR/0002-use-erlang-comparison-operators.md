# ADR 0002: Use Erlang Comparison Operators Directly

## Status
Accepted

## Context

Beamtalk currently uses custom operator syntax for comparisons:

| **Beamtalk (current)** | **Erlang** | **Semantics** |
|------------------------|------------|---------------|
| `==` | `==` | Loose equality (with coercion) |
| `~=` | `/=` | Loose inequality (with coercion) |
| `=` | `=:=` | Strict equality (no coercion) |
| *(none)* | `=/=` | Strict inequality (no coercion) |

This design was chosen to avoid conflict with compound assignment (`/=` for "divide and assign"). However, ADR 0001 proposes removing compound assignment, which frees up `/=` and `=/=` for use as comparison operators.

### Problems with Current Design

1. **Asymmetry:** Strict equality exists (`=`) but no strict inequality operator
2. **Custom syntax:** Beamtalk-specific operators (`~=`, `=`) don't match Erlang
3. **Mental translation:** Developers must learn non-standard mappings
4. **Inconsistent negation:** Uses `~` for "not" instead of Erlang's `/` (slash through)

## Decision

**Use Erlang's comparison operators directly in Beamtalk:**

| **Beamtalk (new)** | **Erlang** | **Semantics** |
|--------------------|------------|---------------|
| `==` | `==` | Loose equality (with coercion) |
| `/=` | `/=` | Loose inequality (with coercion) |
| `=:=` | `=:=` | Strict equality (no coercion) |
| `=/=` | `=/=` | Strict inequality (no coercion) |

### Visual Logic

- **`:` means "strict/exact"**
  - `:=` for strict binding (assignment)
  - `=:=` for strict equality
- **`/` means "not"** (slash through)
  - `/=` for "not equal"
  - `=/=` for "strictly not equal"

## Consequences

### Positive

1. **Perfect Erlang alignment:** All four comparison operators map 1:1 to Erlang. No mental translation needed.

2. **Symmetric operator set:** Both equality and inequality have loose and strict variants.

3. **Consistent negation symbol:** `/` means "not" everywhere (like mathematical ≠ symbol).

4. **Self-documenting:** The colon pattern makes "strict" operations visually consistent (`:=`, `=:=`).

5. **Interoperability:** Erlang/Elixir developers feel at home.

### Negative

1. **Breaking change:** All code using `~=` and `=` must be updated.

2. **More characters:** `=:=` is 3 characters vs `=` (1 character).

3. **Unfamiliar to Smalltalk developers:** Smalltalk uses `=` and `==` with opposite meanings.

### Neutral

1. **Not Smalltalk-like:** But Beamtalk is BEAM-first, Smalltalk-inspired (not Smalltalk-exact).

## Migration Path

1. **Update implementation:**
   - Change lexer to recognize new Beamtalk comparison operator tokens in source: `/=`, `=:=`, `=/=` instead of `~=`, `=`
   - Update operator mapping in `builtins.rs` to use the new Beamtalk comparison tokens (`/=`, `=:=`, `=/=`) and drop support for the legacy `~=` and `=` comparison tokens
   - Remove the user-defined `~=` method from `ProtoObject.bt`; inequality will now be provided by the built-in `/=` operator

2. **Update tests:**
   - Replace all `~=` with `/=`
   - Replace all `=` (strict equality) with `=:=`
   - Add tests for `=/=` (strict inequality)
   - Update `equality.bt` test file

3. **Update documentation:**
   - `docs/beamtalk-syntax-rationale.md` - Update operator table
   - `lib/ProtoObject.bt` - Update equality operator documentation
   - All code examples using comparison operators

## Examples

```beamtalk
// Loose comparison (with coercion)
1.0 == 1           // true (values equal after coercion)
1.0 /= 2           // true (values not equal)

// Strict comparison (no coercion)
1.0 =:= 1.0        // true (same type and value)
1.0 =:= 1          // false (different types)
1.0 =/= 1          // true (strictly not equal - different types)

// Value types
#{x => 3} == #{x => 3}     // true (same map contents)
#{x => 3} /= #{x => 5}     // true (different values)

// Actors
c1 := Counter spawn
c2 := Counter spawn
c1 /= c2           // true (different pids)
c1 == c1           // true (same pid)
```

## References

- ADR 0001: No compound assignment in Beamtalk (prerequisite)
- BT-188: Equality semantics decision
- Erlang documentation: http://erlang.org/doc/reference_manual/expressions.html#term-comparisons
- Discussion date: 2026-02-03

## Notes

This decision prioritizes:
1. **Erlang interoperability** over Smalltalk similarity
2. **Symmetry and consistency** over brevity
3. **BEAM-first philosophy** over language-agnostic syntax

The change from single-character operators (`=`, `~=`) to multi-character operators (`=:=`, `/=`, `=/=`) is acceptable given Beamtalk's goal of being a BEAM language first, Smalltalk-inspired second.
