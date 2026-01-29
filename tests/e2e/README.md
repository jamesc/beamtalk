# Beamtalk E2E Test Suite

End-to-end tests that validate the complete compilation and execution pipeline by evaluating Beamtalk expressions through the REPL.

## Directory Structure

```
tests/e2e/
├── README.md           # This file
└── cases/              # Test case files
    ├── arithmetic.bt   # Arithmetic operation tests
    └── blocks.bt       # Block/closure tests
```

## Running Tests

```bash
# Run E2E tests only
cargo test --test e2e

# Run with verbose output
cargo test --test e2e -- --nocapture

# Run all tests including E2E
cargo test --all-targets
```

**Prerequisites:**
- The daemon must be running, or the test harness will start it automatically
- Erlang/OTP must be installed (for the REPL runtime)
- The project must be built (`cargo build`)

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

### Multi-line Expressions

Currently, each expression must be on a single line. Multi-line expressions are not yet supported.

## Writing New Tests

1. Create a new `.bt` file in `tests/e2e/cases/`
2. Add expressions with `// =>` expected results
3. Run `cargo test --test e2e` to verify

### Guidelines

- **One concept per file**: Group related tests (e.g., all arithmetic in `arithmetic.bt`)
- **Start simple**: Begin with basic cases before edge cases
- **Test both success and failure**: Include error cases where appropriate
- **Add comments**: Document what each test is checking
- **Keep expressions simple**: The test format doesn't support multi-line expressions

## Test Harness

The test harness (`crates/beamtalk-cli/tests/e2e.rs`) handles:

1. **Daemon management**: Starts the daemon if not running
2. **REPL connection**: Uses TCP socket to communicate with REPL (port 9000)
3. **Test execution**: Parses `.bt` files and evaluates each expression
4. **Result comparison**: Compares actual results with expected values

### Protocol

The harness uses the same JSON protocol as the REPL CLI:

```json
// Request
{"type": "eval", "expression": "3 + 4"}

// Success response
{"type": "result", "value": 7}

// Error response
{"type": "error", "message": "Undefined variable: x"}
```

## Troubleshooting

### "Failed to connect to REPL"

1. Check if the daemon is running: `beamtalk daemon status`
2. Try starting manually: `beamtalk daemon start --foreground`
3. Check port 9000 is available: `ss -tlnp | grep 9000`

### "Daemon may not be fully started"

The REPL server may take a moment to initialize. The test harness waits up to 6 seconds for the server to become available.

### Tests timeout

The default timeout is 30 seconds per expression. If tests are timing out:
1. Check if the expression is causing an infinite loop
2. Verify the Erlang runtime is functioning correctly
3. Check for resource exhaustion

## Adding to CI

E2E tests should run as part of the CI pipeline. They require:
- Rust toolchain
- Erlang/OTP
- Network access to localhost:9000

```yaml
# Example CI step
- name: Run E2E tests
  run: |
    cargo build
    cargo test --test e2e -- --nocapture
```
