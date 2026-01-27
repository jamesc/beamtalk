# Test Package Compiler

Snapshot tests for the beamtalk compiler. This crate validates compiler output across all compilation stages: lexing, parsing, and code generation.

## Overview

Tests are organized in the `cases/` directory. Each subdirectory with a `main.bt` file becomes a test case. The build script (`build.rs`) automatically discovers test cases and generates test functions for each stage of compilation.

## Test Structure

```
test-package-compiler/
├── cases/
│   └── hello_world/
│       └── main.bt           # Test source file
├── tests/
│   ├── compiler_tests.rs     # Test harness
│   └── snapshots/            # Snapshot files (auto-generated)
├── build.rs                  # Test discovery and generation
└── README.md                 # This file
```

## Adding a New Test Case

1. Create a new directory under `cases/` with a descriptive name:
   ```bash
   mkdir -p test-package-compiler/cases/my_test_case
   ```

2. Add a `main.bt` file with your beamtalk source code:
   ```bash
   cat > test-package-compiler/cases/my_test_case/main.bt <<'EOF'
   // Copyright 2026 James Casey
   // SPDX-License-Identifier: Apache-2.0

   // Your test code here
   counter := Counter new
   counter increment
   EOF
   ```

3. Run the tests to generate snapshots:
   ```bash
   cargo test -p test-package-compiler
   ```

4. Review and accept the generated snapshots:
   ```bash
   cargo insta review
   # Or automatically accept all:
   cargo insta accept
   ```

## Test Stages

Each test case generates two snapshot tests:

### 1. Lexer Test (`test_{case_name}_lexer`)
Validates the token stream produced by the lexer, including:
- Token kinds (identifiers, keywords, literals, operators)
- Source spans for error reporting
- Leading and trailing trivia (comments, whitespace)

### 2. Parser Test (`test_{case_name}_parser`)
Validates the Abstract Syntax Tree (AST) structure:
- Expression types and nesting
- Message send structure (unary/binary/keyword)
- Blocks and closures
- Error nodes (for error recovery tests)
- Diagnostics (parse errors and warnings)

### 3. Code Generation Test (Future)
Will validate the generated Core Erlang output.

## Snapshot Management

Snapshots are stored in `tests/snapshots/` and managed by [insta](https://insta.rs/).

### Review Changes
```bash
cargo insta review
```

### Accept All (use with caution)
```bash
cargo insta accept
```

### Reject All
```bash
cargo insta reject
```

### Re-run Failed Tests
```bash
cargo insta test
```

## Test Categories

Organize test cases by category:

### Basic Syntax
- `hello_world` - Simple message send
- `literals` - All literal types (numbers, strings, symbols)
- `assignments` - Variable assignment and compound assignment

### Message Precedence (Future)
- `unary_messages` - Object-oriented method calls
- `binary_messages` - Arithmetic and comparison
- `keyword_messages` - Multi-argument messages

### Control Flow (Future)
- `blocks` - Closures and block parameters
- `returns` - Early return from methods
- `cascades` - Multiple messages to same receiver

### Error Recovery (Future)
- `missing_token` - Parser continues after missing tokens
- `invalid_syntax` - Error nodes in AST
- `unterminated_string` - Lexer error recovery

### Advanced Features (Future)
- `pattern_matching` - Destructuring and guards
- `field_access` - Direct field access
- `async_messages` - Future-based async operations

## Architecture Requirements

From [AGENTS.md](../AGENTS.md):

### Tooling-First Approach
- Tests validate that parser produces valid AST even with syntax errors
- Error recovery must preserve enough structure for IDE features
- All AST nodes include precise source spans

### Performance Regression Tests (Future)
Track compilation times to ensure they stay within targets:
- Keystroke to diagnostics: <50ms
- Single-file incremental: <50ms
- Full file diagnostics: <100ms

## Dependencies

- `beamtalk-core` - The compiler implementation
- `insta` - Snapshot testing framework
- `camino` - UTF-8 paths for cross-platform compatibility

## References

- [insta documentation](https://insta.rs/)
- [Gleam test harness](https://github.com/gleam-lang/gleam/tree/main/test-package-compiler)
- [AGENTS.md](../AGENTS.md) - Development guidelines
