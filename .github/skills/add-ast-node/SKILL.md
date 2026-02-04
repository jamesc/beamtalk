---
name: add-ast-node
description: Add a new AST node to the Beamtalk compiler. Use when implementing new syntax, adding language features, or extending the parser.
---

# Adding a New AST Node

Follow this workflow when adding a new AST node type to the compiler:

## Steps

1. **Define the type** in `crates/beamtalk-core/src/ast.rs`
   - Add the new variant to the appropriate enum (e.g., `ExpressionKind`, `StatementKind`)
   - Include a `Span` field for source location
   - Derive required traits: `Debug`, `Clone`, `PartialEq`

2. **Add parsing** in `crates/beamtalk-core/src/parse/`
   - Add token recognition in the lexer if new tokens are needed
   - Implement parsing logic in the parser
   - Handle error recovery gracefully

3. **Add type checking** in `analyse.rs` (if needed)
   - Add type inference rules
   - Add validation for semantic correctness

4. **Add Core Erlang generation** in `erlang.rs`
   - Implement the codegen for the new node
   - Use fully qualified calls: `'erlang':'function'`
   - Generate unique variable names to avoid shadowing

5. **Add snapshot tests** in `test-package-compiler/cases/`
   - Create a new `.bt` file with example usage
   - Run `just test-rust` to generate snapshots
   - Review and accept snapshots with `cargo insta review`

## Key Requirements

- AST nodes MUST carry source location (`Span`) - no exceptions
- Parser MUST produce an AST even with syntax errors (error recovery)
- Never panic on malformed input
- Use `insta` for snapshot testing of parser output and codegen

## Example

```rust
// In ast.rs
pub enum ExpressionKind {
    // ... existing variants ...
    NewFeature {
        param: Box<Expression>,
        body: Box<Expression>,
    },
}

// In parser
fn parse_new_feature(&mut self) -> Result<Expression, ParseError> {
    let start = self.current_span();
    // ... parsing logic ...
    Ok(Expression {
        kind: ExpressionKind::NewFeature { param, body },
        span: start.merge(end),
    })
}
```
