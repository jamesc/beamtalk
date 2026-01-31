# Binary Operator Implementation Status

Cross-reference of binary operators documented in Beamtalk specifications versus actual implementation across the codebase.

## Source Documents

- **Syntax Rationale**: `docs/beamtalk-syntax-rationale.md` (lines 117-124)
- **Language Features**: `docs/beamtalk-language-features.md` (line 148, 160)

## Documented Operator Precedence

From `docs/beamtalk-syntax-rationale.md`:

```
Binary operators have precedence levels:
1. `*`, `/`, `%` (highest)
2. `+`, `-`
3. `<`, `>`, `<=`, `>=`
4. `=`, `!=`
5. `&&`, `and`
6. `||`, `or` (lowest)
```

## Implementation Status by Operator

| Operator | Docs | Lexer | Parser | Codegen | Runtime | REPL | Test | Notes |
|----------|------|-------|--------|---------|---------|------|------|-------|
| **Arithmetic** |
| `*` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 40 |
| `/` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 40 |
| `%` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 40, maps to `rem` |
| `+` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 30 |
| `-` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 30 |
| **Comparison** |
| `<` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20 |
| `>` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20 |
| `<=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20, maps to `=<` |
| `>=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 20 |
| **Equality** |
| `=` | ✓ | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Precedence 10, strict equality `=:=` |
| `!=` | ✓ | ❌ | ❌ | ✓ | N/A | N/A | ❌ | **MISSING**: Codegen exists but no lexer/parser |
| **Logical (Short-circuit)** |
| `&&` | ✓ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | **MISSING**: Documented but not implemented |
| `and` | ✓ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | **MISSING**: Documented but not implemented |
| `\|\|` | ✓ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | **MISSING**: Documented but not implemented |
| `or` | ✓ | ❌ | ❌ | ❌ | N/A | N/A | ❌ | **MISSING**: Documented but not implemented |
| **Additional** |
| `~=` | - | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Not documented, but implemented (strict inequality `=/=`) |
| `++` | - | ✓ | ✓ | ✓ | N/A | N/A | ✓ | Not documented, but implemented (string concat) |
| `**` | - | ❌ | ❌ | ✓ | N/A | N/A | ❌ | Not documented, codegen exists but no lexer/parser |
| `==` | - | ✓ | ❌ | ✓ | N/A | N/A | ❌ | Not documented, lexer tokenizes but parser doesn't support |

## Implementation Details

### Lexer (crates/beamtalk-core/src/parse/lexer.rs)

**Implemented:**
- Lines 804-811: Single-char operators: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `~`
- Lines 821-824: Multi-char operators: `<=`, `>=`, `==`, `~=`
- String concatenation `++` is tokenized (in additive group)

**Missing:**
- `!=` - Neither `!` nor `!=` is in the lexer
- `**` - Not tokenized
- `&&`, `||` - Not tokenized
- `and`, `or` - These would be identifiers, not binary selectors

### Parser (crates/beamtalk-core/src/parse/parser/mod.rs)

**Implemented** (lines 126-143, `binary_binding_power` function):
- Precedence 10: `=`, `~=`
- Precedence 20: `<`, `>`, `<=`, `>=`
- Precedence 30: `+`, `-`, `++`
- Precedence 40: `*`, `/`, `%`

**Missing:**
- `!=` - Not in binding power table
- `==` - Lexer tokenizes but not in binding power table
- `**` - Not in binding power table
- `&&`, `||`, `and`, `or` - Not in binding power table

**Note:** Parser precedence matches documented levels, except:
- Doc says `=`, `!=` are precedence 4
- Parser has `=` at precedence 10 (lower number = lower precedence in parser)
- Precedence numbering is inverted but relative order is correct

### Codegen (crates/beamtalk-core/src/codegen/core_erlang/mod.rs)

**Implemented** (lines 2457-2521, `generate_binary_op` function):
- `+`, `-`, `*`, `/`, `%` → Erlang arithmetic
- `==`, `!=` → Erlang equality (`==`, `/=`)
- `=`, `~=` → Erlang strict equality (`=:=`, `=/=`)
- `<`, `>`, `<=`, `>=` → Erlang comparison (`<`, `>`, `=<`, `>=`)
- `**` → `math:pow` wrapped in `trunc`
- `++` → String concatenation via `iolist_to_binary`

**Missing:**
- `&&`, `||`, `and`, `or` - No codegen implementation

**Dead Code:**
- Lines 2469-2477: `**` operator code never reached (parser doesn't support)
- Lines 2498-2499: `==`, `!=` operator code never/partially reached

### Runtime (runtime/src/)

Binary operators compile to Erlang BIFs or stdlib functions. No runtime implementation needed beyond what Erlang provides.

### REPL (runtime/src/beamtalk_repl_eval.erl)

REPL uses the same codegen path, so operator support matches codegen.

### Tests (test-package-compiler/cases/)

**Tested:**
- `+`, `-`, `*`, `/`, `%` - ✓ Multiple test cases
- `<`, `>`, `<=`, `>=` - ✓ binary_operators test
- `=`, `~=` - ✓ binary_operators test
- `++` - ✓ stdlib_string, binary_operators

**Not Tested:**
- `==`, `!=`, `**` - Commented out in binary_operators (parser doesn't support)
- `&&`, `||`, `and`, `or` - Not implemented anywhere

## Discrepancies Summary

### 1. Documented but Not Implemented (6 operators)

| Operator | Issue |
|----------|-------|
| `!=` | Documented as precedence 4, but no lexer/parser/test support |
| `&&` | Documented as precedence 5, not implemented anywhere |
| `and` | Documented as precedence 5, not implemented anywhere |
| `\|\|` | Documented as precedence 6, not implemented anywhere |
| `or` | Documented as precedence 6, not implemented anywhere |

**Note on `and`/`or`:** These are documented as binary operators but would conflict with Smalltalk-style keyword messages. The Boolean class implements `and:` and `or:` as keyword messages with blocks (for short-circuit evaluation). This is likely the intended implementation, not binary operators.

**Recommendation:** Update documentation to clarify:
- Remove `&&`, `||` from binary operator list (not needed - use `and:` and `or:` messages)
- Change `and`, `or` from binary operators to keyword messages
- Keep or remove `!=` (add parser support or remove from docs)

### 2. Implemented but Not Documented (2 operators)

| Operator | Implementation |
|----------|----------------|
| `~=` | Fully implemented, tested, works correctly |
| `++` | Fully implemented, tested, works correctly |

**Recommendation:** Add to documentation:
- `~=` as strict inequality (precedence 4, same as `=`)
- `++` as string concatenation (precedence 2, with `+` and `-`)

### 3. Partial Implementation (2 operators)

| Operator | Status |
|----------|--------|
| `==` | Lexer tokenizes, codegen exists, but parser doesn't support |
| `**` | Codegen exists but no lexer/parser support |

**Recommendation:** Either:
- Complete implementation (add parser support), OR
- Remove from codegen (dead code)

## Recommendations

### Short-term (BT-135)

**Issue Created:** [BT-135: Add parser support for missing binary operators](https://linear.app/beamtalk/issue/BT-135)

1. **Add parser support for `==`** - Lexer already tokenizes it
2. **Remove or complete `**`** - Either add lexer/parser or remove codegen
3. **Remove `!=`** - Or decide if we want it and implement fully

### Medium-term Documentation Updates

1. **Update syntax rationale** to show actual precedence:
   ```
   1. `*`, `/`, `%` (highest)
   2. `+`, `-`, `++`
   3. `<`, `>`, `<=`, `>=`
   4. `=`, `~=` (strict equality/inequality)
   5-6. Remove `&&`, `||`, `and`, `or` from binary operators
   ```

2. **Add section on Boolean control flow**:
   ```
   true and: [expensiveCheck]    // short-circuit with block
   false or: [alternative]        // short-circuit with block
   ```

3. **Document implemented but undocumented operators**:
   - `~=` as strict inequality
   - `++` as string concatenation

### Long-term

Consider if we want both:
- `==` (standard equality) and `=` (strict equality)
- Standard practice: one equality operator is clearer
- Erlang has both because of history (lists vs binaries)
- Beamtalk could simplify to just `=` (strict) and `~=` (strict inequality)

## File References

| Component | File | Lines |
|-----------|------|-------|
| Documentation | docs/beamtalk-syntax-rationale.md | 117-124 |
| Lexer | crates/beamtalk-core/src/parse/lexer.rs | 804-824 |
| Parser | crates/beamtalk-core/src/parse/parser/mod.rs | 126-143 |
| Codegen | crates/beamtalk-core/src/codegen/core_erlang/mod.rs | 2457-2521 |
| Tests | test-package-compiler/cases/binary_operators/main.bt | Full file |
| Coverage | test-package-compiler/COVERAGE_ANALYSIS.md | 37-57 |

## Conclusion

**Documented operators:** 16 (`*`, `/`, `%`, `+`, `-`, `<`, `>`, `<=`, `>=`, `=`, `!=`, `&&`, `and`, `||`, `or`)  
**Actually parseable:** 12 (`*`, `/`, `%`, `+`, `-`, `<`, `>`, `<=`, `>=`, `=`, `~=`, `++`)  
**Overlap:** 10 operators work as documented  
**Documentation gaps:** 6 operators documented but not implemented  
**Implementation extras:** 2 operators implemented but not documented  

The test suite accurately reflects the 12 parseable operators and correctly identifies the 3 operators with codegen but no parser support.
