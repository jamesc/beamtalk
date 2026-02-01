# Binary Operator Implementation Status

**Status:** Current - Tracks implementation vs. specification

Cross-reference of binary operators documented in Beamtalk specifications versus actual implementation across the codebase.

## Source Documents

- **Syntax Rationale**: `docs/beamtalk-syntax-rationale.md` (lines 117-130)
- **Language Features**: `docs/beamtalk-language-features.md` (lines 158-198)

## Documented Operator Precedence

From `../beamtalk-syntax-rationale.md`:

```
Binary operators have precedence levels:
1. `*`, `/`, `%` (highest - multiplicative)
2. `+`, `-`, `++` (additive and string concatenation)
3. `<`, `>`, `<=`, `>=` (comparison)
4. `=`, `==`, `~=` (equality - strict and loose)
```

**Note:** `&&`, `||`, `and`, `or` are **not** binary operators. They are keyword messages that take blocks for short-circuit evaluation.

## Implementation Status by Operator

| Operator | Docs | Lexer | Parser | Codegen | Runtime | REPL | Test | Notes |
|----------|------|-------|--------|---------|---------|------|------|-------|
| **Arithmetic** |
| `*` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 40 |
| `/` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 40 |
| `%` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 40, maps to `rem` |
| `+` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 30 |
| `-` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 30 |
| **String** |
| `++` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 30, string concatenation |
| **Comparison** |
| `<` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20 |
| `>` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20 |
| `<=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20, maps to `=<` |
| `>=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20 |
| **Equality** |
| `=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 10, strict equality `=:=` |
| `==` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 10, loose equality `==` |
| `~=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 10, strict inequality `=/=` |
| **Logical (Not Operators - Keyword Messages)** |
| `and:` | Partial | Keyword | Keyword | Partial | N/A | N/A | ✓ | Short-circuit via keyword message with block |
| `or:` | Partial | Keyword | Keyword | Partial | N/A | N/A | ✓ | Short-circuit via keyword message with block |
| **Removed** |
| `!=` | ❌ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | Removed - use `~=` instead |
| `**` | ❌ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | Removed - not needed |
| `&&` | ❌ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | Not an operator - use `and:` keyword message |
| `\|\|` | ❌ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | Not an operator - use `or:` keyword message |
| `==` | - | ✓ | ❌ | ✓ | N/A | N/A | ❌ | Not documented, lexer tokenizes but parser doesn't support |

## Implementation Details

### Lexer (crates/beamtalk-core/src/parse/lexer.rs)

**Implemented:**
- Lines 804-811: Single-char operators: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `~`
- Lines 821-824: Multi-char operators: `<=`, `>=`, `==`, `~=`, `++`

**Not Needed:**
- `!=` - Removed, use `~=` instead
- `**` - Removed, not needed
- `&&`, `||` - Not operators, use `and:` and `or:` keyword messages
- `and`, `or` - Keyword messages, not binary operators

### Parser (crates/beamtalk-core/src/parse/parser/mod.rs)

**Implemented** (lines 126-143, `binary_binding_power` function):
- Precedence 10: `=`, `==`, `~=` (equality)
- Precedence 20: `<`, `>`, `<=`, `>=` (comparison)
- Precedence 30: `+`, `-`, `++` (additive and concatenation)
- Precedence 40: `*`, `/`, `%` (multiplicative)

**Note:** Lower numbers = lower precedence (inverted from typical convention), but relative order is correct.

### Codegen (crates/beamtalk-core/src/codegen/core_erlang/mod.rs)

**Implemented** (lines 2457-2521, `generate_binary_op` function):
- `+`, `-`, `*`, `/`, `%` → Erlang arithmetic
- `==` → Erlang loose equality (`==`)
- `=`, `~=` → Erlang strict equality (`=:=`, `=/=`)
- `<`, `>`, `<=`, `>=` → Erlang comparison (`<`, `>`, `=<`, `>=`)
- `++` → String concatenation via `iolist_to_binary`

**Removed:**
- `**` → Exponentiation removed (not needed)
- `!=` → Removed (use `~=` instead)

**Not Operators:**
- `&&`, `||`, `and:`, `or:` - These are keyword messages with blocks, not binary operators

### Runtime (runtime/src/)

Binary operators compile to Erlang BIFs or stdlib functions. No runtime implementation needed beyond what Erlang provides.

### REPL (runtime/src/beamtalk_repl_eval.erl)

REPL uses the same codegen path, so operator support matches codegen.

### Tests (test-package-compiler/cases/)

**Tested:**
- `+`, `-`, `*`, `/`, `%` - ✓ Multiple test cases
- `<`, `>`, `<=`, `>=` - ✓ binary_operators test
- `=`, `==`, `~=` - ✓ binary_operators test
- `++` - ✓ stdlib_string, binary_operators
- `and:`, `or:` - ✓ Via stdlib_boolean test

## Summary

All binary operators are now consistently implemented across lexer, parser, codegen, and documentation:

### Fully Supported (13 operators)
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- String: `++`
- Comparison: `<`, `>`, `<=`, `>=`
- Equality: `=`, `==`, `~=`

### Not Operators (Keyword Messages)
- `and:`, `or:` - Short-circuit evaluation via keyword messages with blocks

### Removed
- `!=` - Use `~=` for strict inequality
- `**` - Exponentiation not needed
- `&&`, `||` - Use `and:` and `or:` keyword messages
