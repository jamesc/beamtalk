# ADR 0002: Use Erlang Comparison Operators Directly

## Status
Implemented (2026-02-08)

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

3. **Not overridable.** Codegen lowers all four operators straight to the Erlang BIF, with no class lookup. See below.

### Neutral

1. **Not Smalltalk-like:** But Beamtalk is BEAM-first, Smalltalk-inspired (not Smalltalk-exact).

## Amendment (2026-07-30, BT-2997): the equality operators are not overridable

Mapping the operators 1:1 onto Erlang's means they *are* Erlang's. `generate_binary_op` (`crates/beamtalk-core/src/codegen/core_erlang/operators.rs`) emits `call 'erlang':'=:='(L, R)` unconditionally for `=:=`, `=/=`, `==` and `/=`. There is no method-table lookup, so a class-level method for any of them is unreachable — it parses, typechecks, appears in method listings, and is compiled into the class module, but nothing can call it.

This was discovered when `Duration` shipped `=:=`/`=/=` methods whose tests passed only because Duration canonicalises to a single `millis` field, so raw term equality agreed with the override that never ran.

Note the contrast with the operators BT-2709/BT-2710 later made dispatchable. Arithmetic (`+ - * /`) and ordering (`< > <= >=`) emit a runtime guard that routes object receivers to `beamtalk_message_dispatch:send/3`, so value types can and do overload them. Equality deliberately did not follow.

### Decision

**Keep the equality operators primitive, and reject the declaration.** The semantic analyser errors on any `=:=` / `=/=` / `==` / `/=` method whose body is not a `@primitive` / `@intrinsic` pragma (a pragma body declares the built-in lowering rather than attempting to replace it, which is how `ProtoObject`, `Integer`, `String` and friends give the operators class-specific signatures). The diagnostic points at `equals:`.

The reasons dispatch is the wrong fix:

1. **The BEAM would not honour it.** The keyed containers decide identity in the VM, below any Beamtalk method: `Dictionary` by Erlang map keys, `Set` by a term-order-sorted list. The same applies to `lists:member/2`, `ets`, and receive-pattern matching. None can be taught about a Beamtalk override, so dispatch would buy `a =:= b` → `true` while a `Set` holding both still reported size 2 — a silent contradiction, strictly worse than the honest limitation.

   (Investigating this turned up a real bug: `Set` used `ordsets`, whose membership is term *order*, i.e. `==`. It collapsed `1` and `1.0` into one element while `Dictionary` kept them as two keys. Fixed under BT-2997 — both now use `=:=`. The containers agreeing with each other does not change the conclusion above; they still cannot consult a Beamtalk method.)
2. **It would break the operator's contract.** Dispatch keys on the left receiver alone, so `x =:= aFraction` and `aFraction =:= x` would disagree — equality would stop being symmetric. `=:=` is also guard-safe in Erlang; a dispatching version is not.
3. **The cost lands on the hottest operator in the language.** The `< > <= >=` guard is affordable because object comparison is rare. Equality is everywhere (`size =:= 0`, symbol dispatch, `assert:equals:`), and the static escape hatches that keep arithmetic on the bare path rarely fire for equality receivers, which are usually `Object`-typed.

### Replacement: `Object>>equals:`

`equals:` is an ordinary message send declared on `Object`, defaulting to `self =:= other`. Classes whose logical value is not their representation override it. Its limits are documented on the method: it does not and cannot affect `Dictionary`/`Set` key identity.

Where the two notions of equality are genuinely different questions rather than one being a better answer, prefer a domain-specific selector — `DateTime` keeps `equals:` structural and offers `sameInstant:`.

## Migration Path

1. **Update implementation:**
   - Change lexer to recognize new Beamtalk comparison operator tokens in source: `/=`, `=:=`, `=/=` instead of `~=`, `=`
   - Update operator mapping in `builtins.rs` to use the new Beamtalk comparison tokens (`/=`, `=:=`, `=/=`) and drop support for the legacy `~=` and `=` comparison tokens
   - Remove the user-defined `~=` method from `proto_object.bt`; inequality will now be provided by the built-in `/=` operator

2. **Update tests:**
   - Replace all `~=` with `/=`
   - Replace all `=` (strict equality) with `=:=`
   - Add tests for `=/=` (strict inequality)
   - Update `equality.bt` test file

3. **Update documentation:**
   - `docs/beamtalk-syntax-rationale.md` - Update operator table
   - `stdlib/src/proto_object.bt` - Update equality operator documentation
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
