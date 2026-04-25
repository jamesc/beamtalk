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

1. **Define the class** in `stdlib/src/YourClass.bt`
   - Include comprehensive documentation
   - Use `@primitive intrinsicName` pragmas for methods backed by runtime dispatch (see [ADR 0007](../ADR/0007-compilable-stdlib-with-primitive-injection.md))
   - Pure Beamtalk methods need no pragma — they compile directly
   - Provide usage examples in comments

2. **Implement runtime support** (for primitive methods only)
   - Add dispatch clause in the appropriate runtime module (e.g., `runtime/apps/beamtalk_runtime/src/beamtalk_primitive.erl`)
   - Runtime modules provide type checking, structured errors, and extension registry fallback
   - Add intrinsic name mapping in the compiler's intrinsic registry (one line)
   - For pure Beamtalk methods, skip this step — the compiler handles them directly

3. **Create test fixtures** in `tests/repl-protocol/fixtures/`
   - Create simple, reusable classes demonstrating the feature
   - Keep fixtures minimal and focused

4. **Write REPL-protocol tests** in `tests/repl-protocol/cases/`
   - Test primitives first
   - Use `@load` directive to load fixtures for actor tests
   - Cover edge cases and error conditions

5. **Create runnable example** in `examples/`
   - Show how users would actually use the feature
   - Include step-by-step REPL session walkthrough
   - Explain what's happening under the hood
   - Document BEAM mapping and generated code

6. **Run tests after EVERY change**
   ```bash
   just test-stdlib     # Bootstrap expression tests (~14s)
   just test-bunit      # BUnit TestCase tests
   just test-repl-protocol        # REPL integration tests (~50s)
   ```

7. **Update `stdlib/src/README.md`**
   - Add to class hierarchy or core classes section
   - Update BEAM mapping table
   - Update implementation status table

**Example from BT-176 (ProtoObject):**
- ✅ Class: `stdlib/src/ProtoObject.bt` — pragma declarations + pure Beamtalk methods
- ✅ Runtime: `runtime/apps/beamtalk_runtime/src/beamtalk_primitive.erl` — dispatch for ProtoObject methods
- ✅ Tests: `stdlib/test/` BUnit tests for the class
- ✅ Docs: Updated `stdlib/src/README.md` with class hierarchy

## Validating Generated Dialyzer Specs

Beamtalk generates `-spec` attributes from type annotations (e.g. `:: Integer`, `-> String`). Since Core Erlang compilation does not preserve specs in BEAM debug_info, a separate validation pipeline verifies that generated specs are well-formed.

### Quick check

```bash
just dialyzer-specs    # Compile stdlib → extract specs → Dialyzer validation
```

### How it works

1. `beamtalk build --stdlib-mode` compiles `.bt` sources to `.core` files with `'spec'` attributes
2. `scripts/validate_specs.escript` extracts those spec terms, constructs Erlang abstract forms with the actual type expressions, compiles to BEAM with `debug_info`, and runs Dialyzer
3. Dialyzer validates that all type expressions in the specs are well-formed (unknown types, malformed tuples, etc. are caught)

### Integration tests

```bash
cargo test --test spec_validation -- --ignored    # Round-trip tests + negative test
```

The integration tests in `crates/beamtalk-cli/tests/spec_validation.rs` verify:
- `.bt` with type annotations → Core Erlang → Dialyzer validation passes
- Correct type mappings (Integer→integer, String→binary, etc.)
- Invalid spec types are detected (negative test)

### Adding new type mappings

When adding a new type to `spec_codegen.rs`:

1. Add the mapping in `simple_type_to_spec()`
2. Add a unit test in the `tests` module
3. Run `just dialyzer-specs` to verify the generated spec is valid
4. Run `cargo test --test spec_validation -- --ignored` for the full round-trip

## Debugging Compilation

1. Use `BEAMTALK_DEBUG=1` to print intermediate representations
2. Check generated `.core` files in `build/` directory
3. Use `erlc +debug_info` for BEAM debugging
4. Inspect BEAM files with `:beam_lib.chunks/2`

## Example-Driven Development

**When implementing new language features, ALWAYS follow this pattern:**

1. **Create a runnable example** in `examples/` showing the feature in use
2. **Create test fixtures** in `tests/repl-protocol/fixtures/` if testing classes/actors
3. **Write REPL-protocol tests** in `tests/repl-protocol/cases/` that load and use the fixtures
4. **Run REPL-protocol tests** to verify end-to-end functionality: `just test-repl-protocol`
5. **Iterate** until all tests pass

**Why this matters:**
- Examples provide immediate validation that your feature actually works
- REPL-protocol tests catch integration issues that unit tests miss
- Test fixtures can be reused across multiple test files
- Users get working examples to learn from

## Using @load for Stateful Tests

The REPL-protocol test infrastructure supports loading class definitions with the `@load` directive:

```beamtalk
// tests/repl-protocol/cases/my_test.btscript
// @load tests/repl-protocol/fixtures/my_class.bt

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
- Put reusable classes in `tests/repl-protocol/fixtures/`
- Keep examples in `examples/` for user learning
- Use `@load` for any test requiring actor/class definitions
- Run REPL-protocol tests after every compiler change
- **ALWAYS add `// =>` assertions** - Expressions without expected output are skipped!

## REPL-Protocol Response Verification Rules

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

REPL-protocol tests are best for:
- ✅ Primitive operations (arithmetic, strings, booleans)  
- ✅ Class loading and compilation
- ✅ Message syntax and parsing
- ⚠️ Limited for: Stateful actor interactions (use `runtime/apps/beamtalk_runtime/test/*.erl` instead)

Example: `runtime/apps/beamtalk_runtime/test/beamtalk_actor_tests.erl` has comprehensive tests for:
- doesNotUnderstand with proxies
- Concurrent message sends
- State mutations
- Future awaiting

**Summary:** Add `// =>` assertions for every deterministic result. For actor state tests, use runtime EUnit tests where you have full control.

## `Nil` vs `UndefinedObject` — the two spellings

Beamtalk has **one** nil class with **two** names:

| Name              | Where it appears                                                                 |
| ----------------- | -------------------------------------------------------------------------------- |
| `UndefinedObject` | Canonical class-hierarchy spelling. Used internally by the type checker, BEAM FFI specs, protocol registry, `InferredType::known(...)`, `is_assignable_to` lookups, `Debug` output for `InferredType`, and the stdlib class definition (`stdlib/src/UndefinedObject.bt`). |
| `Nil`             | Source-sympathetic surface spelling. What users type in annotations (`:: Nil`, `-> Nil`) and read back in user-facing messages: diagnostics, hover, signature help, code-action inserts, and REPL output. (Beamtalk does not yet support union type annotations in `.bt` source — `Foo \| Nil` is how the compiler _renders_ a nullable inference, not something you can write as a type.) |

The two are aliased: the type resolver canonicalises `Nil`/`nil` → `UndefinedObject` during annotation resolution (`type_resolver.rs`), and `WellKnownClass::is_nil_class()` treats both as the nil type.

**When rendering an `InferredType`, pick the right method for the audience:**

- `InferredType::display_name()` — canonical. Use for internal bookkeeping (method-resolution keys, hierarchy enrichment) where the string must round-trip through `is_type_compatible` / `is_assignable_to`.
- `InferredType::display_for_diagnostic()` — source-sympathetic. Use for anything the user reads: diagnostic messages, hover contents, signature-help labels, code-action insert text, signature display in `display_signature()`.
- `InferredType::class_name_for_diagnostic(name)` — when you have a bare `EcoString` / `&str` (e.g., union-member names in a hint), use this helper to apply the same `UndefinedObject → Nil` rewrite.

The `Debug` derive on `InferredType` keeps the canonical `UndefinedObject` spelling so test failures and logs stay unambiguous.

**History:** The canonical-vs-surface split is enforced at the rendering boundary because renaming the class in the hierarchy would break BEAM FFI and stdlib protocols (BT-2016 documents the alias history; BT-2066 fixed the rendering regression that leaked `UndefinedObject` into diagnostics during the BT-2044 narrowing epic).
