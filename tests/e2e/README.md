# Beamtalk E2E Test Suite

End-to-end tests that validate the complete compilation and execution pipeline by evaluating Beamtalk expressions through the REPL.

## Feature Coverage Matrix

| Feature | Status | Test File | Notes |
|---------|--------|-----------|-------|
| **Literals** | | | |
| Integer literals | ✅ | [literals.bt](cases/literals.bt) | `42`, `0`, `1000000` |
| String literals | ✅ | [literals.bt](cases/literals.bt) | `'hello'`, `''` |
| Boolean literals | ✅ | [booleans.bt](cases/booleans.bt) | `true`, `false` |
| **Binary Operations** | | | |
| Addition | ✅ | [arithmetic.bt](cases/arithmetic.bt) | `3 + 4` |
| Subtraction | ✅ | [arithmetic.bt](cases/arithmetic.bt) | `10 - 3` |
| Multiplication | ✅ | [arithmetic.bt](cases/arithmetic.bt) | `5 * 6` |
| Division | ✅ | [arithmetic.bt](cases/arithmetic.bt) | `20 / 4` → float |
| Math precedence | ✅ | [arithmetic.bt](cases/arithmetic.bt) | `2 + 3 * 4` → 14 |
| **Unary Messages** | | | |
| Block `value` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `[42] value` |
| Integer `negated` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `5 negated` → `-5` |
| Integer `abs` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `-5 abs` → `5` |
| Integer `isZero` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `0 isZero` → `true` |
| Integer `isEven` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `4 isEven` → `true` |
| Integer `isOdd` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `5 isOdd` → `true` |
| String `length` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `'hello' length` → `5` |
| String `isEmpty` | ✅ | [unary_messages.bt](cases/unary_messages.bt) | `'' isEmpty` → `true` |
| **Keyword Messages** | | | |
| `value:` | ✅ | [keyword_messages.bt](cases/keyword_messages.bt) | `[:x | x] value: 5` |
| `value:value:` | ✅ | [keyword_messages.bt](cases/keyword_messages.bt) | `[:x :y | x + y] value: 1 value: 2` |
| `value:value:value:` | ✅ | [keyword_messages.bt](cases/keyword_messages.bt) | Three-arg blocks |
| **Blocks** | | | |
| Zero-arg blocks | ✅ | [blocks.bt](cases/blocks.bt) | `[42] value` |
| One-arg blocks | ✅ | [blocks.bt](cases/blocks.bt) | `[:x | x + 1] value: 5` |
| Two-arg blocks | ✅ | [blocks.bt](cases/blocks.bt) | `[:x :y | x + y] value: 3 value: 4` |
| Nested blocks | ✅ | [blocks.bt](cases/blocks.bt) | `[[:x | x] value: 5] value` |
| Return statements | ✅ | [blocks.bt](cases/blocks.bt) | `[:x | ^x + 10] value: 5` (explicit vs implicit) |
| **Variable Persistence** | | | |
| Simple assignment | ✅ | [variable_persistence.bt](cases/variable_persistence.bt) | `x := 42` persists |
| Variable reference | ✅ | [variable_persistence.bt](cases/variable_persistence.bt) | `x` reads back |
| Multiple variables | ✅ | [variable_persistence.bt](cases/variable_persistence.bt) | `x + y` works |
| Variable reassignment | ✅ | [variable_persistence.bt](cases/variable_persistence.bt) | `x := 100` updates |
| **Control Flow** | | | |
| Block evaluation | ✅ | [control_flow.bt](cases/control_flow.bt) | `[5 + 3] value` |
| Block with variables | ✅ | [control_flow.bt](cases/control_flow.bt) | Uses REPL bindings |
| `whileTrue:` | 🔧 | [blocks.bt](cases/blocks.bt) | Non-mutating loop (`[false] whileTrue: [42]`) works; assignments inside blocks don't persist (BT-90) |
| `whileFalse:` | 🔧 | — | Implemented but assignments inside blocks don't persist (BT-90) |
| `timesRepeat:` | ✅ | [actor_local_mutations.bt](../stdlib/actor_local_mutations.bt) | Tested in stdlib |
| `to:do:` | ✅ | [nested_to_do.bt](../stdlib/nested_to_do.bt) | Tested in stdlib |
| **Boolean Operations** | | | |
| `ifTrue:ifFalse:` | ✅ | [booleans.bt](cases/booleans.bt) | `true ifTrue: [1] ifFalse: [2]` → `1` |
| `ifTrue:` | ✅ | [booleans.bt](cases/booleans.bt) | `true ifTrue: [42]` → `42` |
| `ifFalse:` | ✅ | [booleans.bt](cases/booleans.bt) | `false ifFalse: [42]` → `42` |
| `and:` | ✅ | [booleans.bt](cases/booleans.bt) | `true and: [false]` → `false` |
| `or:` | ✅ | [booleans.bt](cases/booleans.bt) | `false or: [true]` → `true` |
| `not` | ✅ | [booleans.bt](cases/booleans.bt) | `true not` → `false` |
| **Cascades** | | | |
| Cascade syntax | 🔧 | [cascades.bt](cases/cascades.bt) | Use `// @load` + stateful tests¹ |
| **Actors** | | | |
| `spawn` | 🔧 | [actors.bt](cases/actors.bt) | Use `// @load` to load class definitions² |
| Async messages | 🔧 | [actors.bt](cases/actors.bt) | Use `// @load` + stateful tests |
| `await` | 🔧 | [actors.bt](cases/actors.bt) | Use `// @load` + stateful tests |
| **Error Handling** | | | |
| Division by zero | ✅ | [errors.bt](cases/errors.bt) | `1 / 0` → badarith |
| **Semantic Analysis** | | | |
| Stored closure field error | ✅ | [semantic_diagnostics.bt](cases/semantic_diagnostics.bt) | `@load-error` on stored closure |
| Sealed class error | ✅ | [semantic_diagnostics.bt](cases/semantic_diagnostics.bt) | `@load-error` on sealed subclass |
| Reflection validator | ✅ | [semantic_diagnostics.bt](cases/semantic_diagnostics.bt) | `respondsTo:` with non-symbol |
| Undefined variable | ✅ | [semantic_scope.bt](cases/semantic_scope.bt) | Compile-time error |
| Variable scoping | ✅ | [semantic_scope.bt](cases/semantic_scope.bt) | Block params, closures |
| Self in methods | ✅ | [semantic_scope.bt](cases/semantic_scope.bt) | Actor methods use `self` |

**Legend:**
- ✅ = Fully tested and working
- 🔄 = Implemented but needs refinement (returns future in REPL)
- 🔧 = Implemented, E2E test infrastructure ready (needs real tests)
- 📋 = Documented, implementation in progress
- — = No separate test file (documented elsewhere)

**Footnotes:**
1. Cascades send async actor messages. Use `// @load tests/e2e/fixtures/counter.bt` to load an actor class, then test cascades with stateful expressions.
2. Actor classes must be defined in files and loaded with `// @load`. See `tests/e2e/fixtures/` for example actors.

## Directory Structure

```
tests/e2e/
├── README.md              # This file
├── cases/                 # Test case files
│   ├── actors.bt          # Actor documentation (syntax examples)
│   ├── arithmetic.bt      # Arithmetic operations (+, -, *, /)
│   ├── blocks.bt          # Block/closure tests
│   ├── booleans.bt        # Boolean literals (true, false)
│   ├── cascades.bt        # Cascade documentation (syntax examples)
│   ├── control_flow.bt    # Control flow (block evaluation, variables)
│   ├── errors.bt          # Error handling tests
│   ├── semantic_diagnostics.bt # Semantic analysis error tests
│   ├── semantic_scope.bt  # Variable scope and resolution tests
│   ├── keyword_messages.bt # Keyword message sends
│   ├── literals.bt        # Integer and string literals
│   ├── unary_messages.bt  # Unary message sends
│   └── variable_persistence.bt # Variable assignment and persistence
└── fixtures/              # Actor/class definitions for stateful tests
    └── counter.bt         # Simple counter actor class
```

## Running Tests

```bash
# Run E2E tests only
just test-e2e

# Or directly via cargo (E2E tests are #[ignore]d, require --ignored flag)
cargo test --test e2e -- --ignored

# Run with verbose output
cargo test --test e2e -- --ignored --nocapture
```

**Prerequisites:**
- Erlang/OTP must be installed (for the REPL runtime)
- The project must be built (`just build`)

## Test File Format

Test files use the `.bt` extension and contain Beamtalk expressions with expected results.

### Basic Format

Each test case consists of an expression followed by a `// =>` comment with the expected result:

```smalltalk
3 + 4
// => 7

[:x | x + 1] value: 5
// => 6
```

### Comments

Regular comments (not starting with `// =>`) are ignored and can be used for documentation:

```smalltalk
// Test basic arithmetic
3 + 4
// => 7

// Test with larger numbers
1000 + 2000
// => 3000
```

### Error Testing

To test that an expression produces an error, use `ERROR:` prefix in the expected result:

```smalltalk
undefined_variable
// => ERROR: Undefined variable
```

The test passes if the error message *contains* the specified text.

See `tests/e2e/cases/errors.bt` for examples of error test cases.

### Compilation Error Testing (`@load-error`)

To test that a file fails to compile with a specific error, use the `@load-error` directive:

```smalltalk
// @load-error tests/e2e/fixtures/bad_class.bt => cannot assign to field
```

The directive attempts to load the file and expects compilation to fail. The test passes if the error message *contains* the specified substring. If the load succeeds, the test fails.

See `tests/e2e/cases/semantic_diagnostics.bt` for examples.

### Multi-line Expressions

Currently, each expression must be on a single line within the test file format.
This is a **test parser limitation**, not a language limitation - Beamtalk itself
supports multi-line expressions. The test format reads one line at a time looking
for `// =>` markers. Multi-line test format support may be added in the future.

### Stateful Tests (Actors and Cascades)

Test files support **stateful multi-expression tests** where variables persist between
expressions within the same file. This is essential for testing actors and cascades.

**Variable persistence within a file:**
```smalltalk
// Assign a variable
x := 42
// => 42

// Use it in the next expression - state persists!
x + 10
// => 52
```

**Loading actor classes:**

To test actors and cascades, first load a file containing class definitions using
the `// @load` directive:

```smalltalk
// @load tests/e2e/fixtures/counter.bt

// Now Counter class is available
counter := Counter spawn
// => <pid>

// Send messages (state persists)
counter increment
// => 1

// Cascades - send multiple messages to same receiver
counter increment; increment; getValue
// => 3
```

**Fixture files:**
- `tests/e2e/fixtures/` - Actor class definitions for E2E tests
- `tests/e2e/fixtures/counter.bt` - Simple counter actor example

**Note:** Bindings are cleared at the start of each test file, but persist across
all expressions within that file. Loaded modules also persist for the file's duration.

## Writing New Tests

1. Create a new `.bt` file in `tests/e2e/cases/`
2. Add expressions with `// =>` expected results
3. Run `just test-e2e` to verify

### Guidelines

- **One concept per file**: Group related tests (e.g., all arithmetic in `arithmetic.bt`)
- **Start simple**: Begin with basic cases before edge cases
- **Test both success and failure**: Include error cases where appropriate
- **Add comments**: Document what each test is checking
- **Keep expressions simple**: The test format doesn't support multi-line expressions

## Test Harness

The test harness (`crates/beamtalk-cli/tests/e2e.rs`) handles:

1. **REPL startup**: Starts a REPL workspace automatically (ephemeral port)
2. **REPL connection**: Uses TCP socket to communicate with REPL
3. **Test execution**: Parses `.bt` files and evaluates each expression
4. **Result comparison**: Compares actual results with expected values

### Protocol

The harness uses the same JSON protocol as the REPL CLI:

```json
// Request
{"op": "eval", "id": "1", "code": "3 + 4"}

// Success response
{"id": "1", "value": "7"}
```

## Troubleshooting

### Debugging Output

To see BEAM output during tests:

```bash
cargo test --test e2e -- --ignored --nocapture
```

### Tests timeout

The default timeout is 30 seconds per expression. If tests are timing out:
1. Check if the expression is causing an infinite loop
2. Verify the Erlang runtime is functioning correctly
3. Check for resource exhaustion
