# Common Development Tasks

Step-by-step guides for common tasks in the Beamtalk codebase.

## Adding a New AST Node

1. Define type in `crates/beamtalk-core/src/ast.rs`
2. Add parsing in `crates/beamtalk-core/src/source_analysis/`
3. Add type checking in `analyse.rs` if needed
4. Add Core Erlang generation in `erlang.rs`
5. Add snapshot tests in `test-package-compiler/cases/`

## Adding a CLI Command

1. Add command enum variant in `crates/beamtalk-cli/src/main.rs`
2. Implement handler function
3. Add integration test
4. Update `--help` documentation

## Adding a New Standard Library Feature

**Follow this example-driven workflow:**

1. **Define the class** in `lib/YourClass.bt`
   - Include comprehensive documentation
   - Use `@primitive intrinsicName` pragmas for methods backed by runtime dispatch (see [ADR 0007](../ADR/0007-compilable-stdlib-with-primitive-injection.md))
   - Pure Beamtalk methods need no pragma — they compile directly
   - Provide usage examples in comments

2. **Implement runtime support** (for primitive methods only)
   - Add dispatch clause in the appropriate runtime module (e.g., `runtime/src/beamtalk_integer.erl`)
   - Runtime modules provide type checking, structured errors, and extension registry fallback
   - Add intrinsic name mapping in the compiler's intrinsic registry (one line)
   - For pure Beamtalk methods, skip this step — the compiler handles them directly

3. **Create test fixtures** in `tests/e2e/fixtures/`
   - Create simple, reusable classes demonstrating the feature
   - Keep fixtures minimal and focused

4. **Write E2E tests** in `tests/e2e/cases/`
   - Test primitives first
   - Use `@load` directive to load fixtures for actor tests
   - Cover edge cases and error conditions

5. **Create runnable example** in `examples/`
   - Show how users would actually use the feature
   - Include step-by-step REPL session walkthrough
   - Explain what's happening under the hood
   - Document BEAM mapping and generated code

6. **Run E2E tests after EVERY change**
   ```bash
   cargo test e2e_language_tests
   # Must show: "X/X tests passed" (all pass)
   ```

7. **Update `lib/README.md`**
   - Add to class hierarchy or core classes section
   - Update BEAM mapping table
   - Update implementation status table

**Example from BT-176 (ProtoObject):**
- ✅ Class: `lib/ProtoObject.bt` — pragma declarations + pure Beamtalk methods
- ✅ Runtime: `runtime/src/beamtalk_primitive.erl` — dispatch for ProtoObject methods
- ✅ Fixtures: `tests/e2e/fixtures/simple_proxy.bt`
- ✅ E2E Tests: `tests/e2e/cases/protoobject.bt`, `protoobject_actors.bt`
- ✅ Example: `examples/protoobject_proxy.bt` (194 lines with walkthrough)
- ✅ Docs: Updated `lib/README.md` with class hierarchy
- ✅ Result: All 224 E2E tests pass

## Debugging Compilation

1. Use `BEAMTALK_DEBUG=1` to print intermediate representations
2. Check generated `.core` files in `build/` directory
3. Use `erlc +debug_info` for BEAM debugging
4. Inspect BEAM files with `:beam_lib.chunks/2`

## Example-Driven Development

**When implementing new language features, ALWAYS follow this pattern:**

1. **Create a runnable example** in `examples/` showing the feature in use
2. **Create test fixtures** in `tests/e2e/fixtures/` if testing classes/actors
3. **Write E2E tests** in `tests/e2e/cases/` that load and use the fixtures
4. **Run E2E tests** to verify end-to-end functionality: `cargo test e2e_language_tests`
5. **Iterate** until all tests pass

**Why this matters:**
- Examples provide immediate validation that your feature actually works
- E2E tests catch integration issues that unit tests miss
- Test fixtures can be reused across multiple test files
- Users get working examples to learn from

## Using @load for Stateful Tests

The E2E test infrastructure supports loading class definitions with the `@load` directive:

```beamtalk
// tests/e2e/cases/my_test.bt
// @load tests/e2e/fixtures/my_class.bt

instance := MyClass spawn
instance someMethod
// => expected_result
```

**How it works:**
1. Test parser extracts `// @load <path>` directives
2. REPL compiles each file via the compiler daemon
3. Classes are loaded into the REPL session
4. Test expressions can spawn and use those classes

**Best practices:**
- Put reusable classes in `tests/e2e/fixtures/`
- Keep examples in `examples/` for user learning
- Use `@load` for any test requiring actor/class definitions
- Run E2E tests after every compiler change
- **ALWAYS add `// =>` assertions** - Expressions without expected output are skipped!

## E2E Response Verification Rules

**Rule 1: Every expression MUST have a `// =>` assertion**

```beamtalk
// ✅ GOOD - Verifies response
42 + 3
// => 45

// ❌ BAD - Expression is skipped, not tested!
42 + 3
```

**Rule 2: Test deterministic values, document non-deterministic ones**

```beamtalk
// ✅ GOOD - Primitives have deterministic output
42 class
// => Integer

'hello' == 'hello'
// => true

// ⚠️ NON-DETERMINISTIC - Can't match exact PID
// Instead, test what CAN be verified:
Counter spawn           // Just verify no error
c class                 // Can't test - 'c' wasn't assigned with => assertion
```

**Rule 3: For non-deterministic objects, test their METHODS**

```beamtalk
// Can't easily test spawn result, but CAN test methods:
x := 42
// => 42

x + 1  
// => 43

// For actors: Test the behavior, not the object identity
// Use runtime tests for full stateful actor testing
```

**Rule 4: Use runtime tests for complex actor scenarios**

E2E tests are best for:
- ✅ Primitive operations (arithmetic, strings, booleans)  
- ✅ Class loading and compilation
- ✅ Message syntax and parsing
- ⚠️ Limited for: Stateful actor interactions (use `runtime/test/*.erl` instead)

Example: `runtime/test/beamtalk_actor_tests.erl` has comprehensive tests for:
- doesNotUnderstand with proxies
- Concurrent message sends
- State mutations
- Future awaiting

**Summary:** Add `// =>` assertions for every deterministic result. For actor state tests, use runtime EUnit tests where you have full control.
