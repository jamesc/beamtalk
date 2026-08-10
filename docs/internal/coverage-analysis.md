# Core Erlang Compilation Verification Coverage Analysis

**Status:** Test case count grows continuously — derive it rather than trust a
baked-in number (see below).

## Test Case Count

Each case generates 4 tests (lexer, parser, codegen, compiles — see the
"Compiler Snapshot Tests" section of
[testing-strategy.md](../development/testing-strategy.md)). Get the current
totals with:
```bash
cases=$(find test-package-compiler/cases -mindepth 1 -maxdepth 1 -type d | wc -l)
echo "cases=$cases tests=$((cases * 4))"
```
- **All compilation tests passing**: ✓

## Expression Types Coverage

### Literals
| Type | Covered | Test Cases |
|------|---------|------------|
| Integer | ✓ | hello_world, ws_stdlib_integer, binary_operators, many others |
| Float | ✓ | Implicitly tested in arithmetic |
| String | ✓ | hello_world, string_operations, ws_stdlib_string |
| Symbol | ✓ | map_literals, ws_stdlib_dictionary |
| Character | ✓ | Literal type exists, tested via stdlib |
| Array | ✓ | ws_stdlib_array |

### Expressions
| Type | Covered | Test Cases |
|------|---------|------------|
| Identifier | ✓ | All test cases |
| Block | ✓ | blocks_no_args, empty_blocks, nested_blocks, ws_stdlib_block |
| MessageSend (Unary) | ✓ | async_unary_message, unary_operators, cascades |
| MessageSend (Binary) | ✓ | binary_operators, ws_stdlib_integer |
| MessageSend (Keyword) | ✓ | async_keyword_message, nested_keyword_messages, multi_keyword_complex_args |
| Assignment | ✓ | actor_state_mutation, all test cases |
| FieldAccess | ✓ | actor_state_mutation |
| Return | ✓ | actor_state_mutation |
| Parenthesized | ✓ | binary_operators (nested expressions) |
| MapLiteral | ✓ | map_literals |
| Cascade | ✓ | cascades, cascade_complex |

## Binary Operators Coverage

| Operator | Erlang Mapping | Test Case | Parser Support |
|----------|----------------|-----------|----------------|
| + | erlang:'+' | actor_spawn, binary_operators | ✓ |
| - | erlang:'-' | blocks_no_args, binary_operators | ✓ |
| * | erlang:'*' | cascade_complex, binary_operators | ✓ |
| / | erlang:'/' | ws_stdlib_integer, binary_operators | ✓ |
| % | erlang:'rem' | ws_stdlib_integer, binary_operators | ✓ |
| =:= | erlang:'=:=' | ws_stdlib_integer, binary_operators | ✓ |
| == | erlang:'==' | binary_operators | ✓ |
| /= | erlang:'/=' | binary_operators | ✓ |
| =/= | erlang:'=/=' | binary_operators | ✓ |
| < | erlang:'<' | blocks_no_args, binary_operators | ✓ |
| > | erlang:'>' | blocks_no_args, binary_operators | ✓ |
| <= | erlang:'=<' | binary_operators | ✓ |
| >= | erlang:'>=' | binary_operators | ✓ |
| ** | math:pow + round | binary_operators | ✓ |
| ++ | iolist_to_binary | ws_stdlib_string, binary_operators | ✓ |

**Coverage**: 15/15 operators (100%)
- All documented operators are fully implemented and tested
- Removed: `!=` (use `/=` instead)
- Note: `and:` and `or:` are keyword messages, not binary operators

## Unary Messages/Operators Coverage

| Message | Type | Test Case |
|---------|------|-----------|
| negated | Integer | unary_operators |
| abs | Integer | unary_operators |
| isZero | Integer | ws_stdlib_integer, unary_operators |
| isEven | Integer | ws_stdlib_integer, unary_operators |
| isOdd | Integer | ws_stdlib_integer, unary_operators |
| not | Boolean | ws_stdlib_boolean, unary_operators |
| value | Block evaluation | ws_stdlib_block, unary_operators |
| repeat | Block loop | ws_stdlib_block |
| spawn | Actor creation | actor_spawn |
| await | Future resolution | async_with_await |

**Coverage**: Core unary operations tested

## Block Evaluation Messages Coverage

| Message | Parameters | Test Case |
|---------|------------|-----------|
| value | 0 args | ws_stdlib_block |
| value: | 1 arg | ws_stdlib_block |
| value:value: | 2 args | ws_stdlib_block |
| whileTrue: | 1 arg | ws_stdlib_block |
| whileFalse: | 1 arg | ws_stdlib_block |
| repeat | 0 args | ws_stdlib_block |

**Coverage**: All block evaluation patterns tested

## Boolean Control Flow Coverage

| Message | Test Case |
|---------|-----------|
| ifTrue:ifFalse: | ws_stdlib_boolean |
| ifTrue: | ws_stdlib_boolean |
| ifFalse: | ws_stdlib_boolean |
| and: | ws_stdlib_boolean |
| or: | ws_stdlib_boolean |
| not | ws_stdlib_boolean, unary_operators |

**Coverage**: Complete boolean control flow

## Dictionary/Map Operations Coverage

| Message | Test Case |
|---------|-----------|
| at: | map_literals, ws_stdlib_dictionary |
| at:put: | map_literals, ws_stdlib_dictionary |
| at:ifAbsent: | ws_stdlib_dictionary |
| includesKey: | ws_stdlib_dictionary |
| keys | ws_stdlib_dictionary |
| values | ws_stdlib_dictionary |
| size | ws_stdlib_dictionary |

**Coverage**: Core map operations tested

## Gen_Server Callbacks Coverage

All gen_server callbacks are automatically generated for every actor module.
Get the current count of cases that declare `Actor subclass:` with:
```bash
grep -l 'Actor subclass:' test-package-compiler/cases/*/main.bt | wc -l
```

| Callback | Generated | Test Coverage |
|----------|-----------|---------------|
| init/1 | ✓ | Every actor test |
| handle_cast/2 | ✓ | Every actor test |
| handle_call/3 | ✓ | Every actor test |
| code_change/3 | ✓ | Every actor test |
| terminate/2 | ✓ | Every actor test |
| spawn/0 | ✓ | actor_spawn + others |
| spawnWith:/1 | ✓ | actor_spawn_with_args |

**Coverage**: 100% - All callbacks generated and compile-tested

## Special Cases Coverage

| Feature | Test Case |
|---------|-----------|
| Error recovery | error_recovery_* (3 cases) |
| Deep nesting | boundary_deeply_nested |
| Long identifiers | boundary_long_identifiers |
| Unicode | boundary_unicode_identifiers |
| Comments | comment_handling |
| Whitespace | whitespace_handling |
| Empty structures | empty_blocks |
| Class definitions | class_definition |
| Async messages | async_keyword_message, async_unary_message |
| Futures/Await | async_with_await, future_pattern_matching |
| String operations | string_operations |

## Coverage Estimate

Based on the analysis:

1. **Expression types**: 11/11 (100%)
2. **Literal types**: 6/6 (100%)
3. **Binary operators (parseable)**: 12/12 (100%)
4. **Binary operators (with codegen)**: 12/15 (80% - 3 operators have codegen but no parser support yet)
5. **Unary operations**: 10+ core operations covered
6. **Block evaluation**: 6/6 messages (100%)
7. **Boolean control**: 6/6 operations (100%)
8. **Gen_server callbacks**: 7/7 (100%)
9. **Special features**: 12+ edge cases covered

### Code Generation Functions

Codegen has since been split from a single `mod.rs` into many files under
`crates/beamtalk-core/src/codegen/core_erlang/` (e.g. `dispatch_codegen.rs`,
`expressions.rs`, `gen_server/callbacks.rs`, `gen_server/dispatch.rs`,
`gen_server/methods.rs`, `operators.rs`, `value_type_codegen.rs`), so a single
file-local function count no longer applies. Get the current total with:
```bash
grep -rc 'fn generate_' crates/beamtalk-core/src/codegen | awk -F: '{sum+=$2} END {print sum}'
```
- Module structure: generate_module, generate_repl_module (✓)
- Actor lifecycle: generate_spawn*, generate_init, generate_start_link (✓)
- Gen_server callbacks: generate_handle_*, generate_code_change, generate_terminate (✓)
- Dispatch: generate_dispatch, generate_safe_dispatch, generate_method_dispatch (✓)
- Expressions: generate_expression and all sub-types (✓)
- Literals: generate_literal (✓)
- Blocks: generate_block*, generate_method_body* (✓)
- Message sends: generate_message_send, generate_await, generate_cascade (✓)
- Field access: generate_field_access, generate_field_assignment (✓)
- Special messages: Boolean, Block, Dictionary, Integer messages (✓)

**Estimated Coverage: >85%**

All core code generation paths are exercised. The few uncovered paths are likely:
- Error handling branches
- Edge cases in complex nesting
- Future features (pattern matching, string interpolation) marked as "future_*"

## Conclusion

The test suite provides comprehensive coverage of Core Erlang code generation:
- **All parseable binary operators**: 12/12 tested (100%)
- **All binary operators with codegen**: 12/15 (80% - 3 need parser support)
- **All unary operators**: Core set tested
- **All gen_server callbacks**: Generated and compile-tested
- **All expression types**: Covered
- **All literal types**: Covered
- **Special messages**: Block, Boolean, Dictionary, Integer - all covered

**Note on operator coverage**: The codegen module has implementations for `**`, `==`, and `!=` operators, but these cannot currently be parsed (see `generate_binary_op()` in `crates/beamtalk-core/src/codegen/core_erlang/operators.rs`). These represent dead code paths until parser support is added (tracked in BT-128).

Every test case includes a compilation verification test that runs `erlc +from_core` to ensure the generated Core Erlang is syntactically valid and compiles to BEAM bytecode.

**Target achieved**: >80% coverage of *parseable* code generation paths ✓
