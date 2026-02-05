# Semantic Analysis

This document describes the design for semantic analysis in the beamtalk compiler.

**Status**: Infrastructure Ready, Implementation Pending  
**Related Issues**: [BT-90](https://linear.app/beamtalk/issue/BT-90) (block-local variable mutation)

## Overview

Semantic analysis validates the AST after parsing, checking for errors that can't be detected syntactically. This includes:

- **Block context analysis** - Distinguishing literal blocks in control flow from stored/passed closures
- **Name resolution** - Resolving identifiers to definitions, detecting undefined variables
- **Type checking** - Validating type annotations and inferring types (future)

## Current State

| Phase | Status |
|-------|--------|
| Lexer | ✅ Complete |
| Parser | ✅ Complete |
| AST | ✅ Rich, with spans and error recovery |
| **Semantic Analysis** | 🟡 **Infrastructure in place, needs implementation** |
| Code Generation | ✅ Works for supported constructs |

### Existing Infrastructure

The semantic analysis module structure exists with type definitions:

- **Module**: `crates/beamtalk-core/src/analyse/mod.rs`
  - `AnalysisResult` - Container for diagnostics and block metadata
  - `BlockInfo` - Block context, captures, and mutations
  - `BlockContext` - Enum for control flow, stored, passed contexts
  - `CapturedVar`, `Mutation`, `MutationKind` - Analysis metadata types
  - `analyse()` - Public API (currently returns empty result)

- **Scope tracking**: `crates/beamtalk-core/src/analyse/scope.rs`
  - `Scope` - Hierarchical scope tracker with push/pop
  - `Binding` - Value object with name, span, depth, kind, and optional type annotation
  - `BindingKind` - Enum distinguishing Local, Parameter, InstanceField, ClassField
  - Variable lookup and capture detection
  - Depth tracking (module, class, method, block)
  - Full test coverage (17 tests)

- **Error types**: `crates/beamtalk-core/src/analyse/error.rs`
  - `SemanticError` and `SemanticErrorKind`
  - Error types: undefined variables, unused variables, mutated captures, escaping blocks

- **Integration**: `crates/beamtalk-core/src/queries/diagnostics.rs`
  - `compute_diagnostics()` has placeholder for semantic diagnostics
  - Ready to merge parse + semantic diagnostics

### What's Missing

The infrastructure is complete, and block context detection is implemented. Remaining work:

1. ❌ AST traversal to populate scope and detect blocks
2. ✅ Block context classification (control flow vs stored vs passed) - **DONE (BT-148)**
3. ❌ Capture and mutation detection
4. ❌ Diagnostic generation from analysis errors
5. ❌ Integration with `compute_diagnostics()`

## Motivation: BT-90

BT-90 introduces context-sensitive rules for block mutations:

| Block Context | Local Vars | Field Assignment |
|---------------|-----------|------------------|
| **Control flow** (literal, immediate) | ✅ Allowed | ✅ Allowed |
| **Stored closure** (`myBlock := [...]`) | ⚠️ Warning | ❌ Error |
| **Passed closure** (block variable in control position) | ❌ No effect | ❌ Error |

Without semantic analysis, we cannot:
1. Distinguish literal blocks from block variables
2. Detect which variables a block mutates
3. Emit appropriate errors/warnings

## Architecture

```
                    ┌─────────────────────────────────────────┐
                    │            Semantic Analysis            │
                    │                                         │
   AST ────────────►│  1. Name Resolution (build symbol table)│
                    │  2. Block Analysis (context, mutations) │
                    │  3. Validation (emit diagnostics)       │
                    │                                         │
                    └────────────────┬────────────────────────┘
                                     │
                         ┌───────────┼───────────┐
                         ▼           ▼           ▼
                    Diagnostics  Analysis    Symbol
                    (errors,     Result      Table
                     warnings)   (for        (for IDE)
                                 codegen)
```

### Current Module Structure

```
crates/beamtalk-core/src/
├── analyse/
│   ├── mod.rs           # ✅ Public API types and analyse() stub
│   ├── scope.rs         # ✅ Scope tracking with full tests
│   ├── block_context.rs # ✅ Block context classification (BT-148)
│   ├── visitor.rs       # ❌ TODO: AST visitor trait
│   └── error.rs         # ✅ Semantic error types
```

**Status**: Types, scope tracking, and block context detection implemented. Need to add visitor for full analysis.

## Key Types

Current implementation in `crates/beamtalk-core/src/analyse/mod.rs`:

```rust
/// Result of semantic analysis
pub struct AnalysisResult {
    /// Semantic diagnostics (errors + warnings)
    pub diagnostics: Vec<Diagnostic>,
    /// Block annotations for codegen
    pub block_info: HashMap<Span, BlockInfo>,
}

/// Information about a block's context
pub struct BlockInfo {
    /// Context in which the block is used
    pub context: BlockContext,
    /// Variables captured from outer scope
    pub captures: Vec<CapturedVar>,
    /// Mutations that occur within the block
    pub mutations: Vec<Mutation>,
}

/// Context in which a block is used
pub enum BlockContext {
    ControlFlow,  // Block used as control flow (if/while condition)
    Stored,       // Block stored in a variable or field
    Passed,       // Block passed as argument to a message send
    Other,        // Other known context (e.g., immediate evaluation)
    Unknown,      // Context could not be determined
}

/// A variable captured from an outer scope
pub struct CapturedVar {
    pub name: EcoString,
    pub defined_at: Span,
}

/// A mutation that occurs within a block
pub struct Mutation {
    pub kind: MutationKind,
    pub span: Span,
}

pub enum MutationKind {
    LocalVariable { name: EcoString },
    CapturedVariable { name: EcoString },
    Field { name: EcoString },
}
```

**Note**: The current implementation uses `BlockContext` enum instead of a boolean `is_control_flow` flag. This provides more granular information for better diagnostics and codegen optimization.

## Implementation Phases

### Phase 1: Block Context Analysis (Required for BT-90)

Minimum viable analysis to support BT-90:

1. **Scope tracking** - Track variable definitions as we walk the AST
2. **Block context detection** - Identify if a block is:
   - A literal block argument to a control flow message
   - Being assigned to a variable (stored closure)
   - A block variable being passed
3. **Mutation detection** - Find assignments inside blocks
4. **Diagnostic emission** - Error/warning for invalid patterns

#### Control Flow Detection

```rust
fn is_control_flow_position(receiver: &Expr, selector: &str, arg_index: usize) -> bool {
    match selector {
        "whileTrue:" | "whileFalse:" => 
            // Receiver must be literal block, arg must be literal block
            arg_index == 0,
        "timesRepeat:" | "do:" | "collect:" | "select:" | "reject:" => 
            arg_index == 0,
        "to:do:" => 
            arg_index == 1,  // Second arg is the block
        "inject:into:" => 
            arg_index == 1,
        "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNil:" | "ifNotNil:" =>
            true,  // All block args are control flow
        _ => false
    }
}
```

#### Block Classification

**Status: ✅ IMPLEMENTED (BT-148)**

The `classify_block()` function in `block_context.rs` classifies blocks based on their AST position:

```rust
enum BlockContext {
    /// Literal block in control flow: `10 timesRepeat: [x := x + 1]`
    ControlFlow,
    /// Stored in variable: `myBlock := [x := x + 1]`
    Stored,
    /// Block variable in call: `items do: myBlock`
    Passed,
    /// Other contexts (return value, etc.)
    Other,
    /// Could not determine context (error recovery)
    Unknown,
}
```

**Key Implementation Details:**

1. **Literal vs Variable Distinction**: Only literal blocks (`[...]`) in control flow positions are `ControlFlow`. Block variables in any position are always `Passed`.

2. **Control Flow Selectors**: Implemented via `is_control_flow_selector()`:
   - Loops: `whileTrue:`, `whileFalse:`, `timesRepeat:`, `do:`, `collect:`, `select:`, `reject:` (arg 0)
   - Range iteration: `to:do:`, `inject:into:` (arg 1)
   - Conditionals: `ifTrue:`, `ifFalse:`, `ifNil:`, `ifNotNil:`, `ifTrue:ifFalse:`, `ifNil:ifNotNil:` (all args)

3. **Special Cases**:
   - `whileTrue:`/`whileFalse:` receiver must also be a literal block for `ControlFlow`
   - Block variables in control flow positions are classified as `Passed`, not `ControlFlow`

4. **Test Coverage**: 22 tests covering all context types and edge cases

**Remaining Work:**
- Integration with AST visitor (Phase 1.3)
- Mutation detection within blocks (Phase 1.4)
- Diagnostic emission (Phase 1.5)

#### Control Flow Detection (Legacy Example)

The following shows the original design concept. The actual implementation is in `block_context.rs`:

```rust
fn is_control_flow_position(receiver: &Expr, selector: &str, arg_index: usize) -> bool {
    match selector {
        "whileTrue:" | "whileFalse:" => 
            // Receiver must be literal block, arg must be literal block
            arg_index == 0,
        "timesRepeat:" | "do:" | "collect:" | "select:" | "reject:" => 
            arg_index == 0,
        "to:do:" => 
            arg_index == 1,  // Second arg is the block
        "inject:into:" => 
            arg_index == 1,
        "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNil:" | "ifNotNil:" =>
            true,  // All block args are control flow
        _ => false
    }
}
```

### Phase 2: Name Resolution (Foundation)

Full symbol table with:
- Variable definitions and references
- Field declarations and accesses
- Method signatures
- Undefined identifier detection

### Phase 3: Type Checking (Future)

- Type inference
- Type annotation validation
- Message signature checking

## Design Decisions

### Decision 1: Side Tables vs. Annotated AST

**Chosen: Side Tables** ✅

Keep the AST immutable. Analysis returns a separate `AnalysisResult` with information keyed by `Span`.

Rationale:
- Matches query-based architecture from AGENTS.md
- Allows multiple analysis passes without AST mutation
- Cleaner separation of concerns
- Easier to cache/invalidate

### Decision 2: When to Run Analysis

**Chosen: On Demand** ✅

Analysis runs when:
- Diagnostics are requested (`compute_diagnostics`)
- Code generation is invoked
- IDE features need semantic info (hover, completions)

Rationale:
- IDE-first architecture prioritizes responsiveness
- Not all operations need full analysis
- Can cache results and invalidate on edit (future optimization)

### Decision 3: Block Identification

**Chosen: By Span** ✅

Use source spans to identify blocks in side tables.

Rationale:
- Spans are already unique per block
- No parser changes needed
- Simple to implement
- Can add synthetic IDs later if incremental analysis needs them

### Decision 4: Control Flow Detection Location

**Chosen: Analysis Phase** ✅

Control flow detection happens in analysis, not codegen.

Rationale:
- Single source of truth
- Better error messages (analysis knows context)
- Codegen just reads `BlockInfo.is_control_flow`
- Easier to test in isolation

### Decision 5: Error Recovery Strategy

**Chosen: Continue & Collect with Conservative Fallback** ✅

When analysis encounters errors:
1. Record the error, continue analyzing the rest of the AST
2. For blocks where context can't be determined, mark as `BlockContext::Unknown`
3. Skip mutation validation for unknown blocks (no false positives)
4. Codegen treats unknown blocks conservatively (no special optimization)

Rationale:
- Users see all errors at once (better DX)
- No false positives from cascading errors
- Matches parser's error recovery approach

## Open Questions

### Q1: How much name resolution for BT-90?

BT-90 needs to distinguish:
- Local variables (defined in current block)
- Captured variables (defined in outer scope)
- Fields (`self.x`)

**Minimal approach**: Track scope depth when variable defined vs. used.

**Full approach**: Complete symbol table with all definitions.

**Recommendation**: Start minimal, extend as needed.

### Q2: How to handle analysis errors in codegen?

If analysis fails or is incomplete:
- Codegen should still work for valid parts
- Unanalyzed blocks treated conservatively (no special codegen)
- Errors already reported by analysis

### Q3: Should analysis be incremental?

For IDE responsiveness, we may want incremental analysis:
- Only re-analyze changed methods/blocks
- Cache analysis results

**For now**: Full analysis on each request. Optimize later if needed.

## Error Messages

### Field Assignment in Closure

```
Error: Cannot assign to field 'sum' inside a stored closure.

  12 │ myBlock := [:item | self.sum := self.sum + item].
     │                     ^^^^^^^^^^^^^^^^^^^^^^^^
                           
Field assignments require immediate execution context for state threading.

Fix: Use control flow directly, or extract to a method:

  // Instead of:
  myBlock := [:item | self.sum := self.sum + item].
  items do: myBlock.
  
  // Write:
  items do: [:item | self.sum := self.sum + item].
  
  // Or use a method:
  addToSum: item => self.sum := self.sum + item
  items do: [:item | self addToSum: item].
```

### Local Mutation in Closure

```
Warning: Assignment to 'count' inside stored closure has no effect on outer scope.

  8 │ myBlock := [count := count + 1].
    │             ^^^^^^^^^^^^^^^^^
                  
Closures capture variables by value. The outer 'count' won't change.

Fix: Use control flow directly:

  // Instead of:
  myBlock := [count := count + 1].
  10 timesRepeat: myBlock.
  
  // Write:
  10 timesRepeat: [count := count + 1].
```

## References

- [BT-90: Block-local variable mutation](https://linear.app/beamtalk/issue/BT-90)
- [TypeScript Compiler Architecture](https://github.com/microsoft/TypeScript/wiki/Architectural-Overview) - query-based design inspiration
- [Rust Analyzer Architecture](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/architecture.md) - incremental analysis patterns
