# Parser Architecture

This document captures the findings from the BT-109 research spike evaluating parser alternatives for Beamtalk.

## Executive Summary

**Recommendation: Keep the hand-rolled recursive descent parser, enhanced with Pratt parsing for binary operator precedence.**

The current parser is already well-structured for IDE-first development. The main improvement opportunity is consolidating binary operator precedence handling using a table-driven Pratt parsing approach, reducing code duplication and making it trivial to add new operators.

## Background

### Current Parser

The parser (`crates/beamtalk-core/src/parse/parser.rs`, ~1,642 LOC) uses recursive descent with:
- Comprehensive error recovery at statement boundaries
- Trivia preservation for IDE formatting
- Precise source spans on all AST nodes
- 50+ unit tests

### Pain Point

Adding new binary operators or precedence levels requires:
1. Creating a new `parse_*` method (~20 LOC)
2. Updating the call chain between precedence levels
3. Adding operators to the match statement

Currently, binary precedence is split across three nearly identical methods:
- `parse_comparison()`: `<`, `>`, `<=`, `>=`, `=`, `!=`
- `parse_additive()`: `+`, `-`
- `parse_multiplicative()`: `*`, `/`, `%`

## Alternatives Evaluated

### 1. Pratt Parsing (Recommended Enhancement)

Pratt parsing (top-down operator precedence parsing) uses a binding power table to handle operator precedence declaratively.

**Prototype:** `crates/beamtalk-core/src/parse/parser_pratt.rs` (~120 LOC core, ~280 LOC with tests)

**How it works:**
```rust
/// Binding power table - the ONLY place to add new operators
fn binary_binding_power(op: &str) -> Option<BindingPower> {
    match op {
        "=" | "!="           => Some(BindingPower::left_assoc(10)),  // Equality
        "<" | ">" | "<=" ... => Some(BindingPower::left_assoc(20)),  // Comparison
        "+" | "-"            => Some(BindingPower::left_assoc(30)),  // Additive
        "*" | "/" | "%"      => Some(BindingPower::left_assoc(40)),  // Multiplicative
        _ => None, // Unknown operator ends the expression
    }
}

/// Single function handles ALL binary precedence
fn parse_binary_expr(..., min_bp: u8, ...) -> Expression {
    let mut left = parse_unary();
    
    while let TokenKind::BinarySelector(op) = current_token().kind() {
        let Some(bp) = binary_binding_power(&op) else { break };
        if bp.left < min_bp { break }
        
        advance();
        let right = parse_binary_expr(..., bp.right, ...);
        left = make_message_send(left, op, right);
    }
    
    left
}
```

**Benefits:**
- **Adding operators**: 1 line in the binding power table
- **Associativity**: Built-in support (`left_assoc` vs `right_assoc`)
- **No structural changes**: Same error recovery, trivia handling, spans
- **Well-understood algorithm**: Used by rust-analyzer, TypeScript, and many production parsers

**Trade-offs:**
- Slight indirection via lookup table
- Tests needed for each new precedence level

**Test results:** 7 tests passing, covering precedence and associativity

### 2. Chumsky (Not Recommended)

Chumsky is a parser combinator library with good error recovery support.

**Evaluation:** `crates/beamtalk-core/src/parse/parser_chumsky.rs` (documentation only)

**Key finding:** Chumsky uses Pratt parsing internally for operator precedence via its `pratt()` combinator. This means adopting chumsky would provide the same algorithm as our Pratt prototype, but with additional abstraction overhead.

**Why not recommended:**

| Factor | Chumsky | Hand-rolled + Pratt |
|--------|---------|---------------------|
| Precedence handling | Uses Pratt internally | Uses Pratt directly |
| Error recovery | Combinator-based (learning curve) | Purpose-built sync points |
| Trivia handling | Requires token adapter layer | Already integrated |
| Debugging | Complex generic types, harder to step through | Simple recursive descent |
| API stability | 1.0.0-alpha (may change) | Stable |
| Dependencies | Adds crate dependency | No new deps |

The fundamental insight is that chumsky doesn't provide a different algorithm for precedence—it wraps the same Pratt parsing we can implement directly.

### 3. Parser Generators (Not Evaluated)

Tools like `lalrpop` or `pest` were not evaluated because:
- They generate parsers from grammar files
- Error recovery is typically harder to customize
- Trivia handling would require a preprocessing layer
- Our grammar (Smalltalk-style messages) doesn't map naturally to LR/PEG grammars

## Comparison Matrix

| Criterion | Current | + Pratt | Chumsky |
|-----------|---------|---------|---------|
| Binary precedence LOC | ~80 | ~40 | ~40 |
| Adding new operator | 20+ LOC, 2 files | 1 line | 1 line |
| Error recovery | Excellent | Unchanged | Learning curve |
| Trivia handling | Yes | Unchanged | Requires adapter |
| IDE integration | Yes | Unchanged | Needs span mapping |
| Debugging | Easy | Easy | Harder |
| Dependencies | None | None | +1 crate |
| API stability | N/A | N/A | Alpha |

## Implementation Status

### Phase 1: Integrate Pratt Parsing ✅ (BT-110)

**Completed in BT-110:**

1. ✅ Moved `BindingPower` struct and `binary_binding_power()` function into `parser.rs` as private helpers
2. ✅ Replaced `parse_comparison`, `parse_additive`, `parse_multiplicative` with single `parse_binary_with_pratt()` method
3. ✅ All existing tests pass unchanged (200+ tests)
4. ✅ Added edge case tests for unknown operators and single operand expressions
5. ✅ Removed `parser_pratt.rs` prototype (functionality integrated into main parser)

**Location:** `crates/beamtalk-core/src/parse/parser.rs`
- `BindingPower` struct (lines ~70-95)
- `binary_binding_power()` function (lines ~97-130)
- `parse_binary_with_pratt()` method in Parser

### Phase 2: Add New Operators (Future)

With Pratt parsing in place, adding new operators becomes trivial:

```rust
// Example: Add bitwise operators
"|"  => Some(BindingPower::left_assoc(25)),  // Between comparison and additive
"&"  => Some(BindingPower::left_assoc(26)),
"^"  => Some(BindingPower::left_assoc(24)),
"<<" => Some(BindingPower::left_assoc(27)),
">>" => Some(BindingPower::left_assoc(27)),

// Example: Add exponentiation (right-associative)
"**" => Some(BindingPower::right_assoc(50)),  // 2 ** 3 ** 4 = 2 ** (3 ** 4)
```

## Conclusion

The research confirms that our current parser architecture is sound. The main improvement is adopting Pratt parsing for binary operator precedence, which:

1. Reduces code duplication from 80 LOC to 40 LOC
2. Makes adding operators a 1-line change
3. Preserves all IDE-first qualities (error recovery, trivia, spans)
4. Uses a battle-tested algorithm without new dependencies

Chumsky was not adopted because it provides the same underlying algorithm with more complexity.

## References

- [Pratt Parsing Made Easy](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) - matklad's tutorial
- [rust-analyzer parser](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser) - Production Pratt parser example
- [Simple but Powerful Pratt Parsing](https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/) - Bob Nystrom's explanation
- Implementation: `crates/beamtalk-core/src/parse/parser.rs` (search for `binary_binding_power`)
- Chumsky evaluation: `crates/beamtalk-core/src/parse/parser_chumsky.rs`
