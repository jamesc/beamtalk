# AGENTS.md - Beamtalk Development Guidelines

This document provides guidance for AI coding agents working on the beamtalk compiler and ecosystem.

## Repository Information

**Always use these values for GitHub API calls:**

| Property | Value |
|----------|-------|
| Owner | `jamesc` |
| Repository | `beamtalk` |
| Full name | `jamesc/beamtalk` |
| URL | `https://github.com/jamesc/beamtalk` |

Example `gh` CLI usage:
```bash
gh api repos/jamesc/beamtalk/pulls
gh pr create --repo jamesc/beamtalk
```

## Project Overview

Beamtalk is a Smalltalk/Newspeak-inspired programming language that compiles to the BEAM virtual machine. The compiler is written in Rust and generates Core Erlang, which is then compiled to BEAM bytecode via erlc.

**Key principle:** Beamtalk is an **interactive-first** language. The live environment and hot code reloading are core to the design, not afterthoughts.

**Important:** While heavily inspired by Smalltalk, Beamtalk makes pragmatic departures from Smalltalk syntax and semantics to work well with BEAM and modern development practices. We're "Smalltalk-like," not Smalltalk-compatible. See [docs/beamtalk-syntax-rationale.md](docs/beamtalk-syntax-rationale.md) for specific differences, [docs/beamtalk-principles.md](docs/beamtalk-principles.md) for design philosophy, and [docs/beamtalk-language-features.md](docs/beamtalk-language-features.md) for full language specification.

---

## Syntax Verification - Preventing Hallucinations 🚨

**CRITICAL:** AI agents must **verify all Beamtalk syntax** before using it in code, tests, or examples. Do not invent or assume syntax patterns exist.

### The Problem: Syntax Blending Hallucinations

AI agents may hallucinate plausible-looking but invalid syntax by combining real language features incorrectly.

**Example hallucination caught in BT-98:**
```beamtalk
// ❌ HALLUCINATED - This syntax does NOT exist!
Counter := Actor [
  state: value = 0.
  increment => self.value := self.value + 1.
].
```

**Why it looks plausible:**
- ✅ `:=` is real (variable assignment)
- ✅ `[...]` is real (block syntax)
- ✅ `Actor` is real (class name)
- ❌ **Combining them this way is INVALID**

**Correct syntax:**
```beamtalk
// ✅ CORRECT - Keyword message for class definition
Actor subclass: Counter
  state: value = 0
  
  increment => self.value := self.value + 1
```

### Common Hallucination Patterns

| Pattern | Example | Reality |
|---------|---------|---------|
| **Inline class syntax** | `Counter := Actor [...]` | Only `Actor subclass: Counter` works |
| **Ruby/Python-style** | `Point.new(x: 3)` | Beamtalk uses `Point new: #{x => 3}` |
| **Missing constraints** | `Counter new` on Actor | Error - actors use `spawn`, not `new` |
| **Assumed features** | `Integer subclass: MyInt` | Primitives are sealed, cannot subclass |
| **Smalltalk ported verbatim** | `Object subclass: #Counter` | Beamtalk uses identifiers, not symbols: `Object subclass: Counter` |
| **Load syntax confusion** | `@load` in REPL | Use `:load` in REPL, `@load` only in E2E test files |
| **Pragma confusion** | `@primitive blockValue` | Structural intrinsics use `@intrinsic blockValue`; `@primitive 'selector'` is for quoted selector-based dispatch only |

### E2E Test Directives vs REPL Commands

**CRITICAL:** E2E test files and REPL use different syntax for loading files.

| Syntax | Where | Purpose | Example |
|--------|-------|---------|---------|
| `@load` | E2E test files (in comments) | Test framework directive | `// @load tests/fixtures/counter.bt` |
| `:load` | REPL interactive session | REPL command | `> :load examples/counter.bt` |

**When to use which:**
- Writing E2E test case → `// @load path.bt` (test directive)
- Interactive REPL → `> :load path.bt` (REPL command)
- Documentation showing REPL session → `:load` (actual REPL command)
- Code examples in docs → `:load` (REPL usage)

**Why different?**
- `@load` is parsed by test framework before execution (meta-level instruction)
- `:load` is sent to REPL during execution (runtime command)
- Different systems, different syntax - follows test framework conventions (like `@param` in JSDoc)

### Test File Format: Parser Warns on Missing Assertions ⚠️

**CRITICAL:** In test files (`tests/stdlib/*.bt` and `tests/e2e/cases/*.bt`), expressions **without** `// =>` assertions are **skipped** and never executed. The parser emits warnings to help catch these mistakes.

**Test format:**
```beamtalk
// @load tests/fixtures/counter.bt   ← Test framework directive

Counter spawn                          ← Expression to evaluate
// => _                                 ← Required assertion (wildcard OK)

c increment                            ← Another expression
// => 1                                 ← Required assertion

count := 0                             ← Expression without assertion
                                       ← ⚠️ SILENTLY SKIPPED! Never runs!

3 timesRepeat: [count := count + 1]   ← Expression without assertion
                                       ← ⚠️ SILENTLY SKIPPED! Never runs!

count                                  ← This will fail (count was never set)
// => 3                                 ← Assertion fails because previous lines didn't run
```

**Rules for test files (stdlib and E2E):**
1. **Every expression must have `// =>` assertion** (even if `// => _` for wildcard)
2. **No assertion = no execution** (expressions are skipped)
3. **Missing assertions cause test failures** (BT-249: warnings now treated as errors)
4. **CI will fail** if any test file has expressions without assertions

**Safe pattern:**
```beamtalk
// Expression with side effect (spawn, assignment)
counter := Counter spawn
// => _                    ← Wildcard assertion, but expression RUNS

// Expression with expected value
counter increment
// => 1                    ← Specific assertion

// Expression returning nil
3 timesRepeat: [counter increment]
// => nil                  ← Explicit nil assertion, expression RUNS
```

**Dangerous pattern:**
```beamtalk
// ❌ DANGEROUS - This looks fine but DOESN'T RUN!
x := 0
3 timesRepeat: [x := x + 1]   ← No assertion, SKIPPED!
                               ← ❌ ERROR! Test will fail in CI!
x
// => 3                        ← Will fail! x was never set because previous line didn't run
```

**Verification:**
```bash
# Run tests - missing assertions will FAIL the build
just test-stdlib   # Stdlib tests
just test-e2e      # E2E tests

# Example error output:
# ⚠️  mytest.bt: Line 15: Expression will not be executed (missing // => assertion): count := 0
# E2E tests failed: 1 of 287 tests failed
```

**Error enforcement (BT-249):** Missing assertions are treated as test failures, not just warnings. CI will catch broken tests before they're merged.

### Pattern Matching in Test Assertions (BT-502)

Test assertions support **glob-style pattern matching** where `_` acts as a wildcard segment within a pattern:

| Pattern | Matches | Use Case |
|---------|---------|----------|
| `_` | Any result | Bare wildcard (run but don't check) |
| `#Actor<Counter,_>` | `#Actor<Counter,0.173.0>` | Actor spawn (ignore PID) |
| `#Actor<_,_>` | Any actor reference | Any actor class |
| `{\: Set, elements: _}` | Full Set output | Collection type check |
| `Alice` | `Alice` (exact) | Exact string match |

**Rules:**
- **Bare `_`** → full wildcard (backward compatible, matches anything)
- **`_` within a pattern** → matches any substring at that position
- **`_` between alphanumeric chars** → literal (e.g., `does_not_understand` is exact match)
- Non-`_` literal segments must appear **in order** in the actual result
- First segment must match at the start, last segment must match at the end

**Examples:**
```beamtalk
// Actor spawn — validate class name, ignore PID
counter := Counter spawn
// => #Actor<Counter,_>

// Actor method returning nil
(counter setValue: 5) await
// => nil

// Exact value match
counter getValue await
// => 5

// Collection with actors — validate type, ignore contents
room online
// => _                    ← Still bare wildcard for complex structures
```

**Prefer patterns over bare wildcards** when the result has a predictable structure (like actor class names). This catches format/class regressions that bare `_` would miss.

### Verification Checklist

Before using ANY Beamtalk syntax, verify it exists in **at least one** of these sources:

#### 1. **Language Specification (Primary Authority)**
✅ **Check first:** [docs/beamtalk-language-features.md](docs/beamtalk-language-features.md)
- Contains full language specification
- Explicitly marks implemented vs future features
- Examples for every language construct

#### 2. **Example Code (Real Usage)**
✅ **Reference directory:** `examples/*.bt`
- Real working code compiled and tested
- Demonstrates idiomatic patterns
- Shows actual syntax in use

#### 3. **Test Cases (Validated Syntax)**
✅ **Test files:**
- `tests/stdlib/*.bt` - Compiled language feature tests (ADR 0014)
- `tests/e2e/cases/*.bt` - End-to-end REPL integration tests
- `test-package-compiler/cases/*/main.bt` - Compiler test cases
- `crates/beamtalk-core/src/source_analysis/parser/mod.rs` - Parser unit tests

#### 4. **Syntax Rationale (Design Decisions)**
✅ **Deliberate choices:** [docs/beamtalk-syntax-rationale.md](docs/beamtalk-syntax-rationale.md)
- Explains divergences from Smalltalk
- Documents "why not X" decisions
- Lists rejected alternatives

### Red Flags: Probably Hallucinated

🚩 **If you're about to use syntax that doesn't appear in the sources above, STOP!**

**Ask yourself:**
1. Have I seen this exact syntax in `examples/`, `tests/stdlib/`, or `tests/e2e/cases/`?
2. Is it documented in `docs/beamtalk-language-features.md`?
3. Does the parser have test cases for it in `parser/mod.rs`?

**If no to all three → It's likely hallucinated. Ask for clarification.**

### What To Do Instead

**Option 1: Search the codebase**
```bash
# Find how Counter is actually defined
grep -r "Counter" examples/*.bt tests/stdlib/*.bt tests/e2e/cases/*.bt

# Find class definition syntax
grep -r "subclass:" examples/*.bt
```

**Option 2: Check parser tests**
```bash
# See what the parser actually accepts
grep -A10 "Actor subclass" crates/beamtalk-core/src/source_analysis/parser/mod.rs
```

**Option 3: Ask explicitly**
```markdown
I want to define a class. I found these patterns:
- `Actor subclass: Counter` in examples/counter.bt
- Is `Counter := Actor [...]` also valid?

Can you confirm which syntax is correct?
```

### Safe Code Generation Principles

✅ **DO:**
- Copy exact syntax from working examples
- Reference parser tests for valid grammar
- Use documented features from language spec
- Ask when uncertain

❌ **DON'T:**
- Blend valid features into new combinations without verification
- Assume Smalltalk syntax works as-is
- Port syntax from other languages (Ruby, Python, Swift)
- Invent "logical extensions" without checking

### Integration with DevEx Checklist

This extends the existing DevEx principle: **"Can you demonstrate the feature in 1-2 lines of REPL code?"**

If you can't find your proposed syntax in:
1. ✅ Examples that compile
2. ✅ Tests that pass
3. ✅ Documentation that describes it

Then it doesn't exist yet, and you're about to hallucinate it!

---

## Domain Driven Design (DDD)

**CRITICAL:** The beamtalk architecture is **driven by Domain Driven Design principles**. Always consider DDD when creating new code or refactoring existing code.

### Core DDD Principles

1. **Ubiquitous Language** - Use domain terms consistently throughout the codebase
   - Module names, types, and functions should reflect the domain model
   - Example: `CompletionProvider`, `DiagnosticProvider`, `HoverProvider` (not `completions`, `diagnostics`, `hover`)
   - When the domain expert says "provider", the code should say "provider"

2. **Bounded Contexts** - The codebase is organized into distinct contexts
   - **Language Service Context** - IDE features (completions, diagnostics, hover)
   - **Compilation Context** - Lexer, parser, AST, semantic analysis, codegen
   - **Runtime Context** - Erlang runtime, actors, OTP integration
   - **REPL Context** - Interactive development, live coding

3. **Domain Services** - Stateless operations that don't naturally fit in entities
   - Example: `CompletionProvider::compute_completions()` is a domain service
   - Domain services are pure functions operating on domain objects (AST, Module)

4. **Value Objects** - Immutable objects defined by their attributes
   - Example: `Span`, `Position`, `Identifier`, `Token`
   - Use newtypes for clarity: `ModuleId`, `FunctionId`, `ByteOffset`, `LineNumber`

5. **Entities** - Objects with identity that can change over time
   - Example: `Module`, `Expression`, `Statement` in the AST
   - Identity matters more than attributes

### When to Apply DDD

**Always apply DDD when:**

1. **Creating new modules** - Name them after domain concepts
   ```rust
   // ✅ GOOD - Uses domain language
   pub mod completion_provider;
   pub mod diagnostic_provider;
   
   // ❌ BAD - Generic technical terms
   pub mod completions;
   pub mod diagnostics;
   ```

2. **Refactoring** - Align code with the domain model
   - If the code structure doesn't match the domain model, refactor it
   - Example: BT-199 renamed `completions` → `completion_provider` to match LSP terminology

3. **Adding features** - Consider which bounded context it belongs to
   - Language Service feature? → `crates/beamtalk-core/src/queries/`
   - Compilation feature? → `crates/beamtalk-core/src/source_analysis/` or `src/semantic_analysis/`
   - Runtime feature? → `runtime/src/`

4. **Writing documentation** - Include DDD context annotations
   ```rust
   //! Completion provider for the language service.
   //!
   //! **DDD Context:** Language Service
   //!
   //! This domain service implements the `CompletionProvider` from the DDD model.
   ```

### DDD Resources

- **Primary reference:** [docs/beamtalk-ddd-model.md](docs/beamtalk-ddd-model.md) - Defines all bounded contexts and domain services
- **LSP alignment:** Language Service context aligns with LSP specification terminology
- **Review examples:** See BT-199 for a refactoring that improved DDD alignment

### Code Review Checklist

When reviewing code for DDD compliance:

- [ ] Module names use domain language (not generic technical terms)
- [ ] Types have clear bounded context (documented in module header)
- [ ] Functions use ubiquitous language from domain model
- [ ] Domain services are stateless and operate on domain objects
- [ ] Value objects are immutable
- [ ] Documentation includes DDD context annotations

---

## Development Architecture Principles

The beamtalk codebase follows strict architectural principles for code organization, error handling, testing, security, and dependencies. Full details in [docs/development/architecture-principles.md](docs/development/architecture-principles.md).

**Core principles:**
1. **Layered Architecture** - Dependencies flow down only (core never depends on CLI)
2. **Error Recovery** - Return partial results + diagnostics (don't stop at first error)
3. **Testing Pyramid** - Unit 60-70%, Integration 20-30%, E2E 10%
4. **Security-First** - Input validation at boundaries, no unsafe without justification
5. **Minimal Dependencies** - Prefer std library, document why each dependency exists

**Critical rules:**

❌ **NEVER:**
- `beamtalk-core` importing `beamtalk-cli` or `beamtalk-lsp`
- Panic on user input (malformed source, invalid args, missing files)
- Add dependencies without security review and commit message justification
- Use `unwrap()` on user input
- **Use bare tuple errors** - ALL errors MUST use `#beamtalk_error{}` records

✅ **ALWAYS:**
- Return `(Result, Vec<Diagnostic>)` or equivalent for user-facing operations
- Validate file paths and buffer boundaries
- Document unsafe code with `// SAFETY:` comment explaining invariants
- Run `cargo audit` before releases
- **Use structured errors** - `beamtalk_error:new/with_selector/with_hint` in all code

See full guide: [docs/development/architecture-principles.md](docs/development/architecture-principles.md)

---

## Error Handling - CRITICAL RULES

**NO bare tuple errors EVER!** All errors in the beamtalk codebase MUST use the structured `#beamtalk_error{}` system.

### Structured Error System

All errors use `#beamtalk_error{}` records defined in `runtime/include/beamtalk.hrl`:

```erlang
-record(beamtalk_error, {
    kind    :: atom(),              % does_not_understand | immutable_value | type_error | instantiation_error | ...
    class   :: atom(),              % 'Integer', 'Counter', 'Actor'  
    selector:: atom() | undefined,  % method that failed
    message :: binary(),            % human-readable explanation
    hint    :: binary() | undefined,% actionable suggestion
    details :: map()                % additional context
}).
```

### In Runtime Erlang Code

Use `beamtalk_error` module helpers:

```erlang
Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
Error1 = beamtalk_error:with_selector(Error0, 'foo'),
Error2 = beamtalk_error:with_hint(Error1, <<"Check spelling">>),
error(Error2)
```

### In Generated Core Erlang Code

Codegen MUST use `beamtalk_error` calls:

```erlang
%% ❌ WRONG - bare tuple (never do this!)
call 'erlang':'error'({'some_error', 'message'})

%% ✅ RIGHT - structured error
let Error0 = call 'beamtalk_error':'new'('instantiation_error', 'Actor') in
let Error1 = call 'beamtalk_error':'with_selector'(Error0, 'new') in
let Error2 = call 'beamtalk_error':'with_hint'(Error1, <<"Use spawn instead">>) in
call 'erlang':'error'(Error2)
```

### Error Kinds

| Kind | When | Example |
|------|------|---------|
| `does_not_understand` | Unknown method | `42 foo` |
| `immutable_value` | Mutation on primitive | `42 instVarAt:put:` |
| `type_error` | Wrong argument type | `"hello" + 42` |
| `arity_mismatch` | Wrong argument count | Missing/extra args |
| `instantiation_error` | Wrong instantiation | `Actor new` (use `spawn`) |
| `future_not_awaited` | Message to Future | `(future) size` |
| `timeout` | Operation timeout | Await exceeds deadline |

### Benefits of Structured Errors

1. **Consistent tooling** - Pattern match on kind/class/selector
2. **Better UX** - Actionable hints guide users
3. **Rich context** - Details map for debugging
4. **Future-proof** - Easy to add metadata without breaking changes

See full error taxonomy: [docs/internal/design-self-as-object.md](docs/internal/design-self-as-object.md#38-error-handling-taxonomy)

---

## Logging - CRITICAL RULES

**NO io:format for diagnostics EVER!** All logging in the runtime MUST use OTP logger **macros** (`?LOG_*`), not function calls.

### Why Macros, Not Function Calls

The runtime uses OTP logger macros for all diagnostic output. Macros automatically include caller location metadata (module, function, arity, line) which is essential for debugging via file logs.

```erlang
%% ❌ WRONG - io:format for diagnostics
io:format(standard_error, "Error: ~p~n", [Reason])

%% ❌ WRONG - function calls (no MFA metadata in log output)
logger:error("Error in method", #{selector => Selector, reason => Reason})

%% ✅ RIGHT - logger macros (includes MFA automatically)
?LOG_ERROR("Error in method", #{selector => Selector, reason => Reason})
```

**Every `.erl` file that logs MUST include:**
```erlang
-include_lib("kernel/include/logger.hrl").
```

Place the include after `-behaviour(...)` (if present) or after `-module(...)`.

### Log Levels

| Level | Macro | When | Example |
|-------|-------|------|---------|
| `?LOG_DEBUG` | `?LOG_DEBUG(Msg, Meta)` | Detailed diagnostics | JSON parse errors, protocol details |
| `?LOG_INFO` | `?LOG_INFO(Msg, Meta)` | Important events | Workspace started, REPL ready |
| `?LOG_WARNING` | `?LOG_WARNING(Msg, Meta)` | Recoverable issues | Accept error, failed cleanup |
| `?LOG_ERROR` | `?LOG_ERROR(Msg, Meta)` | Errors affecting operations | Method exceptions, timeouts |

### Structured Metadata

Always use metadata maps, not format strings:

```erlang
%% ✅ Good - structured metadata
?LOG_ERROR("Error in method", #{
    selector => Selector,
    class => Class,
    reason => Reason
})

%% ❌ Bad - format strings (harder to filter/parse)
?LOG_ERROR("Error in method ~p: ~p:~p", [Selector, Class, Reason])
```

### File Logging (BT-541)

Workspace nodes automatically write logs to `~/.beamtalk/workspaces/{workspace_id}/workspace.log`:
- **All levels** captured — the file handler sets primary logger level to `debug` at startup so all events reach the file
- **Log rotation**: 5 files × 1 MB
- **Format**: `timestamp [level] module:function/arity message`
- **Disable**: Set `BEAMTALK_NO_FILE_LOG=1` environment variable

### Benefits

1. **MFA in logs** - Every log entry shows which module/function/line emitted it
2. **File logging** - Persistent workspace logs for post-hoc debugging
3. **Clean test output** - Configure logger to suppress non-error logs during tests
4. **Structured data** - Metadata maps are easy to parse and filter
5. **Standard OTP** - Follows Erlang best practices
6. **Configurable** - Control via `sys.config` or runtime API

### Test Configuration

Tests suppress debug/info/warning logs via `runtime/test/sys.config`:

```erlang
[{kernel, [{logger_level, error}]}]
```

See full logging guidelines: [docs/development/erlang-guidelines.md](docs/development/erlang-guidelines.md#logging-with-otp-logger)

---

## Test Output - AI Agent Friendly

**Test commands are intentionally concise to avoid polluting AI agent context.**

### Output Philosophy

Tests should output:
- ✅ Summary line (X passed, Y failed, Z seconds)
- ✅ Failed test details (if any)
- ❌ NOT every passing test name
- ❌ NOT compilation progress

### Current Configuration

**Rust tests:** One line per test binary with `crate::source` label
```bash
just test-rust
# Output: ~9 lines, one per binary with pass/fail/ignore counts
# Example:
#   beamtalk_core::src/lib.rs                     429 passed; 0 failed; 2 ignored; ...
#   compiler_tests::tests/compiler_tests.rs       176 passed; 0 failed; 0 ignored; ...
```

**Erlang tests:** Use non-verbose eunit mode
```bash
just test-runtime
# Output: Summary only (~2 lines)
```

### Benefits

1. **Reduced context pollution** - 90% less output (500+ lines → 50 lines)
2. **Faster agent processing** - Less text to parse and understand
3. **Signal over noise** - See failures immediately, not buried in success spam
4. **Still debuggable** - Failures show full details with error messages

### For Debugging

When you need verbose output for investigation:

```bash
# Rust - see all test names
cargo test --all-targets

# Erlang - see all test names  
cd runtime
rebar3 eunit
```

**Remember:** Default test commands (`just test`, `just test-rust`, `just test-runtime`) are optimized for AI agent workflows with minimal output.

---

## User Experience & Developer Experience (DevEx) First

**CRITICAL:** Beamtalk is an **interactive-first** language. Every feature must be validated from the user's perspective before it's considered complete.

### Core Principle

**A feature is not done until the user can interact with it and see its value.**

Building internal infrastructure (error records, helper functions, data structures) is necessary but insufficient. Always complete the loop to user-facing integration.

### DevEx Checklist

Before marking any feature as complete, verify:

#### 1. **REPL Integration** (Most Critical)
- [ ] Does this feature appear in the REPL? How?
- [ ] Are error messages user-friendly and actionable?
- [ ] Can the user inspect results in the REPL?
- [ ] Are hints and help text available?

#### 2. **Error Messages**
- [ ] Do errors use user-facing names? (e.g., `self` not `Self`)
- [ ] Are errors actionable? (Do they explain what to do?)
- [ ] Are stack traces helpful and not overwhelming?
- [ ] Are compiler errors formatted with context and suggestions?

#### 3. **Tooling Display**
- [ ] How does this appear in the CLI? (`beamtalk repl`, `beamtalk build`)
- [ ] What does the user see when things go wrong?
- [ ] Are success/failure states clear?
- [ ] Is output formatted for human readability?

#### 4. **Examples & Testing**
- [ ] Can you demonstrate the feature in 1-2 lines of REPL code?
- [ ] Are there examples in `examples/` or docs showing real usage?
- [ ] Do tests verify the user-facing behavior, not just internals?

### Example: BT-169 Error Handling

**What we built:** `#beamtalk_error{}` records with structured fields and formatting helpers.

**Initially missed:** Integration into REPL error display! Users would have seen raw Erlang records instead of formatted messages.

**Lesson learned:** 
```erlang
% Build the infrastructure
Error = beamtalk_error:new(does_not_understand, 'Integer')

% ✅ ALSO integrate into display layer
format_error_message(#beamtalk_error{} = Error) ->
    beamtalk_error:format(Error)
```

**Always ask:**
1. "What does the user see when this runs?"
2. "How do I test this in the REPL?"
3. "What happens when this fails? Is the error helpful?"

### Common Integration Points

| Feature Type | User-Facing Integration |
|-------------|------------------------|
| **Error handling** | REPL error display, CLI error output |
| **Language features** | REPL evaluation, syntax examples |
| **Runtime features** | REPL inspection, debug output |
| **Compiler features** | Diagnostic messages, LSP integration |
| **Standard library** | REPL usage, documentation examples |

### Red Flags (Incomplete Features)

🚩 "The infrastructure is done" - but no user can see it
🚩 "Tests pass" - but no examples show real usage
🚩 "Error handling works" - but messages are cryptic tuples
🚩 "Feature implemented" - but REPL doesn't expose it
🚩 "Documented" - but no interactive examples

### Success Criteria

✅ A new developer can try the feature in the REPL immediately
✅ Error messages explain what went wrong and how to fix it
✅ Examples show real-world usage, not internal APIs
✅ Documentation includes REPL session snippets
✅ Failure modes are tested and produce helpful output

**Remember:** Beamtalk is a *live*, *interactive* language. If users can't interact with your feature in the REPL, it's not done.

---

## When to Stop and Ask for Help 🚨

**If you encounter any of these situations, STOP working and ask the user:**

### 1. **Circular Dependencies**
- Issue A blocks B, B blocks A
- Design decision requires another design decision
- "Need to implement X to test Y, but need Y to implement X"

**What to do:** Explain the circular dependency and ask which to break or how to approach incrementally.

### 2. **Specification Ambiguity**
- Acceptance criteria are unclear or contradictory
- Multiple valid interpretations of requirements
- Missing information about edge cases
- Conflicting examples in documentation

**What to do:** List the ambiguities, propose 2-3 interpretations, ask which is correct.

### 3. **Missing Design Decision**
- Feature requires architectural choice (e.g., "Should this be sync or async?")
- Multiple implementation strategies, no clear winner
- Trade-offs between performance, complexity, and maintainability
- ADR-worthy decision needed

**What to do:** Present options with pros/cons, recommend one, ask for decision.

### 4. **Repeated Test Failures**
- Same tests fail after multiple fix attempts (>3 tries)
- Test passes locally but fails in CI
- Flaky test that passes/fails randomly
- Root cause unclear after investigation

**What to do:** Share test output, explain attempts made, ask for debugging guidance.

### 5. **Breaking Change Concern**
- Change might break existing user code
- Alters public API or language semantics
- Requires migration path or deprecation cycle
- Affects BEAM interoperability

**What to do:** Describe the potential break, propose alternatives, ask if acceptable.

### 6. **Performance Cliff**
- Solution works but is O(n²) or worse
- Generates excessive memory allocations
- Requires unbounded resources (atoms, processes, memory)
- Creates hot spots in critical path

**What to do:** Present working solution, explain performance concern, ask if acceptable or needs optimization.

### 7. **Security Boundary**
- Handling untrusted user input
- Parsing external data formats
- Executing dynamic code
- Network or filesystem operations

**What to do:** Describe the security concern, propose validation strategy, request review.

### 8. **Spinning for >30 Minutes**
- Multiple solutions attempted, none work
- Stuck on same error/issue without progress
- Uncertainty about correct approach
- Debugging reveals deeper issues

**What to do:** Summarize what was tried, share current error, ask for direction.

### Better to Ask Early

**It's better to ask after 30 minutes than spin for 3 hours on wrong assumptions.**

Example good question:
```
I'm implementing BT-123 (field mutation in blocks). I've tried:
1. Threading state through block closure - fails with X
2. Using mutable reference - breaks Erlang semantics
3. Copying state back after block - loses updates

Should I:
A) Refactor codegen to use different state model?
B) Add runtime state synchronization?
C) Document as limitation and create follow-up issue?

Current error: [paste error]
```

---

## Understanding the Compilation Pipeline 🔧

**Where does your feature live?**

```
Beamtalk Source (.bt)
    ↓ Lexer (src/parse/lexer.rs)        - Converts text → tokens
    ↓ Parser (src/parse/parser/mod.rs)  - Converts tokens → AST
    ↓ Semantic Analysis (future)         - Type checking, validation
    ↓ Code Generation (src/codegen/)     - Converts AST → Core Erlang
    ↓ erlc                               - Converts Core Erlang → BEAM bytecode
    ↓ Runtime (runtime/src/)             - Executes BEAM bytecode
    ↓ REPL (runtime/src/beamtalk_repl*/) - Interactive evaluation
```

### Quick Reference: Where to Implement Features

| You're implementing... | Primary location | Secondary location | Key files |
|------------------------|------------------|--------------------|-----------|
| **New syntax** | Parser | Lexer (if new tokens needed) | `parse/parser/mod.rs`<br>`parse/lexer.rs` |
| **Type checking** | Semantic analysis (future) | N/A | Not yet implemented |
| **Code generation** | Codegen | Runtime support functions | `codegen/core_erlang/*.rs` |
| **Runtime behavior** | Runtime | Codegen for dispatch | `runtime/src/beamtalk_*.erl` |
| **Primitive operations** | Runtime | Codegen for dispatch | `runtime/src/beamtalk_integer.erl`<br>`runtime/src/beamtalk_string.erl` |
| **REPL features** | REPL | Codegen for eval context | `runtime/src/beamtalk_repl*.erl` |
| **Error messages** | All layers | Runtime for formatting | `runtime/src/beamtalk_error.erl` |
| **Class system** | Codegen + Runtime | Parser for syntax | `codegen/core_erlang/gen_server.rs`<br>`runtime/src/beamtalk_object_class.erl` |

### Examples by Layer

**Lexer changes:**
- Adding new operators (`++`, `//`)
- New literal syntax (symbols `#foo`)
- Keywords (`sealed`, `async`)

**Parser changes:**
- Message send syntax (unary, binary, keyword)
- Block syntax (`[...]`, `[:x | ...]`)
- Class definitions (`Actor subclass: Counter`)

**Codegen changes:**
- How field access compiles (`self.value`)
- Control flow structures (`ifTrue:`, `whileTrue:`)
- Message dispatch (sync vs async)

**Runtime changes:**
- Primitive method implementations (`Integer +`)
- Error handling and formatting
- Class registration and dispatch
- REPL evaluation and state management

### Debugging Tip: Inspect Intermediate Stages

```bash
# See generated Core Erlang
beamtalk build examples/counter.bt
cat examples/build/counter.core

# See BEAM assembly
cd examples/build
erlc +to_asm counter.core
cat counter.S

# Test in Erlang shell
cd examples/build
erl
1> c(counter).
2> counter:spawn().
```

### When Multiple Layers Need Changes

**Example: Adding `whileTrue:` support**

1. ✅ **Parser** - Already parses as keyword message
2. ✅ **Codegen** - Generate loop with condition check
3. ✅ **Runtime** - Block evaluation (`beamtalk_block:value/1`)
4. ✅ **Tests** - E2E test in `tests/e2e/cases/control_flow.bt`

**Start from the bottom up:**
1. Runtime support (blocks can be evaluated)
2. Codegen (generate the loop)
3. Tests (validate it works)

---

## Scope Control 📋

**When to create a follow-up issue vs fix inline:**

### ✅ Fix Inline (No New Issue)

These are part of your current work:

- **Formatting/typos** in files you're modifying
- **Test failures** caused by your changes
- **Clippy warnings** in code you wrote
- **Documentation** for features you just added
- **Compile errors** introduced by your changes

### ❌ Create Follow-up Issue

These should be separate issues:

- **Bug discovered** in unrelated code
- **Performance optimization** opportunity noticed
- **Missing tests** in other modules
- **Additional feature ideas** ("while I'm here...")
- **Refactoring** that's not required for current issue
- **Technical debt** in existing code

### Red Flags of Scope Creep

🚩 PR grows beyond 10 files changed  
🚩 "Just one more thing" appears in commits  
🚩 Acceptance criteria complete but still coding  
🚩 Testing uncovers orthogonal issues  
🚩 Refactoring unrelated code "for consistency"  
🚩 Adding features not in acceptance criteria  

### What to Do When You Find Extra Work

**Step 1:** Complete the original issue first

**Step 2:** Create new issue with context
```markdown
Title: Fix X discovered in Y

Context:
Discovered while working on BT-123. File Z has issue X.

[Details of the issue]

Acceptance Criteria:
- [ ] Fix X
- [ ] Add test for X

Dependencies:
None - independent of BT-123

References:
- Discovered in: BT-123
```

**Step 3:** Get original issue reviewed/merged

**Step 4:** Pick up follow-up if high priority

### Example: Good Scope Control

**BT-98: Field assignments in nested blocks**

✅ **In scope:**
- Implement field mutation in control flow
- Update codegen for `whileTrue:`, `timesRepeat:`
- Add E2E tests for mutations
- Fix clippy warnings in new code

❌ **Out of scope (create follow-up):**
- Optimize state threading (works but slow)
- Add `to:by:do:` iteration (different feature)
- Refactor existing control flow tests (not broken)
- Add mutation detection analysis (separate concern)

**Result:** BT-98 merged quickly. Follow-ups: BT-245 (optimization), BT-35 (to:by:do:).

### Benefits

- ✅ **Faster reviews** - Smaller, focused PRs
- ✅ **Easier debugging** - Clear what changed
- ✅ **Better history** - One issue, one fix
- ✅ **Parallelizable** - Follow-ups can be picked up independently

---

## Debugging Workflow 🔍

**Step-by-step debugging for common failures:**

### Compiler Crashes

```bash
# 1. Enable panic backtraces
RUST_BACKTRACE=1 beamtalk build failing.bt

# 2. Identify which phase failed
# Lexer error:     "unexpected character at line X, column Y"
# Parser error:    "expected X, found Y"
# Codegen error:   "failed to generate code for ..."

# 3. Create minimal repro case
echo "minimal failing code" > test.bt
beamtalk build test.bt

# 4. Add debug output in relevant layer
# For parser: add dbg!(&ast) in parse/parser/mod.rs
# For codegen: add dbg!(&expr) in codegen/core_erlang/
```

### Runtime Errors

```bash
# 1. Inspect generated Core Erlang
cat build/module_name.core | less

# Look for:
# - Function definitions ('functionName'/Arity)
# - Pattern matches (case ... of)
# - Error calls (call 'erlang':'error')

# 2. Test in Erlang shell directly
cd build
erl
1> c(module_name).
2> module_name:function_name(Args).

# 3. Enable Erlang debug traces
3> dbg:tracer().
4> dbg:p(all, c).
5> dbg:tpl(module_name, '_', []).
6> module_name:function_name(Args).
```

### Test Failures

```bash
# 1. Run single test with output
cargo test test_name -- --nocapture

# 2. Check what the test expects
# - Snapshot test: see tests/snapshots/*.snap
# - E2E test: see tests/e2e/cases/*.bt
# - Unit test: read test source

# 3. Update snapshots if intentional
cargo test test_name
# Review changes in git diff
cargo insta accept

# 4. Run all tests in module
cargo test --test module_name
```

### E2E Test Failures

```bash
# 1. Check REPL daemon logs
just test-e2e 2>&1 | tee e2e.log
grep "ERROR\|Warning\|failed" e2e.log

# 2. Test fixture manually
cd tests/e2e/fixtures
../../target/debug/beamtalk build counter.bt
cat build/counter.core

# 3. Run REPL interactively
beamtalk repl
> :load tests/e2e/fixtures/counter.bt
> Counter spawn
> c increment

# 4. Check expected output in test file
cat tests/e2e/cases/actors.bt
# Look for // => expected output comments
```

### Codegen Debugging

```bash
# 1. Generate and inspect Core Erlang
beamtalk build failing.bt
cat build/failing.core

# 2. Look for suspicious patterns:
# - Missing State/Self parameters
# - Unbound variables (StateX, State1, etc.)
# - Wrong function arities
# - Call to undefined functions

# 3. Compare with working example
beamtalk build examples/counter.bt
diff build/counter.core build/failing.core

# 4. Add codegen debug output
# Edit src/codegen/core_erlang/expressions.rs
dbg!(&expr);
// Rebuild and check output
```

### Runtime/REPL Debugging

```bash
# 1. Check if modules loaded
beamtalk repl
> Beamtalk loadedModules
> Beamtalk classNamed: #Counter

# 2. Enable verbose mode
beamtalk repl --verbose

# 3. Check actor state
> c := Counter spawn
> c class
> c respondsTo: #increment

# 4. Inspect Erlang process state
# In separate terminal:
erl -name debug@127.0.0.1 -setcookie beamtalk
(debug@127.0.0.1)1> nodes().
(debug@127.0.0.1)2> observer:start().
# Find beamtalk_repl process, inspect state
```

### Performance Debugging

```bash
# 1. Profile compilation
time beamtalk build large_file.bt

# 2. Profile runtime
beamtalk repl
> :timer.tc(fun() -> Counter spawn end).
{TimeInMicroseconds, Result}

# 3. Check memory usage
> observer:start().
# Memory tab, see allocation by process

# 4. Flame graphs (advanced)
# Enable Erlang profiling
erl -pa build
1> fprof:apply(Module, Function, Args).
2> fprof:profile().
3> fprof:analyse().
```

### When All Else Fails

1. **Simplify** - Remove code until it works, then add back
2. **Compare** - Find similar working code, diff against it
3. **Ask** - Share error + what you tried, get fresh eyes
4. **Rubber duck** - Explain the problem out loud to yourself
5. **Sleep** - Come back tomorrow with fresh perspective

---

## Work Tracking

We use **Linear** for task management. Project prefix: `BT`

### Referencing Issues

Include the Linear issue ID in commits and PR titles:
```
git commit -m "Implement lexer tokens BT-123"
```

### Issue Lifecycle

| State | Meaning |
|-------|---------|
| **Backlog** | Idea captured, not yet specified |
| **Ready** | Fully specified with acceptance criteria; agent can pick up |
| **In Progress** | Actively being worked on |
| **In Review** | Code complete, needs human verification |
| **Done** | Merged and verified |

### Labels

We use several label groups to categorize issues:

#### Agent State

Tracks workflow status:

- `agent-ready` - Task is fully specified, agent can start immediately
- `needs-spec` - Requires human to clarify requirements before work begins
- `blocked` - Waiting on external dependency or another issue
- `human-review` - Agent completed work, needs human verification
- `done` - Issue is complete and closed

#### Item Area

Identifies which component of the codebase the issue affects:

| Label | Description | Key Directories |
|-------|-------------|----------------|
| `class-system` | Class definition, parsing, codegen, and runtime | `crates/beamtalk-core/src/ast.rs`, `crates/beamtalk-core/src/source_analysis/` |
| `stdlib` | Standard library: collections, primitives, strings (compiled from `lib/*.bt` via pragmas — see [ADR 0007](../docs/ADR/0007-compilable-stdlib-with-primitive-injection.md)) | `lib/` |
| `repl` | REPL backend and CLI interaction | `runtime/src/beamtalk_repl.erl`, `crates/beamtalk-cli/src/repl/` |
| `cli` | Command-line interface and build tooling | `crates/beamtalk-cli/` |
| `codegen` | Code generation to Core Erlang/BEAM | `crates/beamtalk-core/src/erlang.rs` |
| `runtime` | Erlang runtime: actors, futures, OTP integration | `runtime/src/` |
| `parser` | Lexer, parser, AST | `crates/beamtalk-core/src/source_analysis/`, `crates/beamtalk-core/src/ast.rs` |

#### Issue Type

Categorizes the kind of work:

- `Epic` - **Large initiatives that group related issues (5+ child issues)**
- `Feature` - A chunk of customer visible work
- `Bug` - Bugs, broken tests, broken code
- `Improvement` - Incremental work on top of a feature
- `Documentation` - Words that explain things
- `Infra` - Tools, CI, dev environment configuration
- `Language Feature` - New Beamtalk language syntax/semantics
- `Refactor` - Code cleanups, tech debt
- `Research` - Research projects, code spikes
- `Samples` - Code, examples, things to help devs get started

#### Item Size

T-shirt sizing for estimates: `S`, `M`, `L`, `XL`

**When creating issues:** Always set:
1. An **Agent State** label (`agent-ready` or `needs-spec`)
2. An **Item Area** label (what part of codebase)
3. An **Issue Type** label (what kind of work)
4. An **Item Size** label (how big)

### Epics

**Epics** are large initiatives that group 5+ related issues. Use Epics to organize complex features that span multiple components or require sequential implementation.

#### Querying Epics

Query Linear directly for up-to-date Epic status — don't maintain static lists:

```bash
# Active epics (Backlog, Todo, In Progress)
streamlinear-cli search "Epic:" --state Backlog
streamlinear-cli search "Epic:" --state "In Progress"

# Completed epics
streamlinear-cli search "Epic:" --state Done

# Get details on a specific epic
streamlinear-cli get BT-XXX
```

#### Creating Epics

When creating an Epic:

1. **Title prefix** - Always use `Epic:` prefix in title (e.g., `Epic: Standard Library Core Classes`)
2. **Use Epic label** - Always add `Epic` label to Epic issues
3. **Size as XL or L** - Epics are large by definition
4. **Write comprehensive description** with:
   - Overview of initiative
   - Goals (3-5 high-level outcomes)
   - Status summary (progress percentage, completed/in-progress/planned)
   - List of child issues with status indicators
   - References to design docs
   - Next steps
4. **Link child issues** - Use Linear's "blocks" relationship to connect Epic to child issues
5. **Track progress** - Update Epic description with progress as child issues complete
6. **Include completion criteria** - Checklist of what defines "done" for the Epic

**Example Epic Titles:**
```
Epic: Block Semantics and Control Flow
Epic: Standard Library Core Classes
Epic: REPL and Interactive Development
```

**Example Epic Structure:**
```markdown
## Overview
Brief description of the initiative and why it matters.

## Goals
1. Goal 1 - Specific outcome
2. Goal 2 - Specific outcome
3. Goal 3 - Specific outcome

## Status
**Progress:** ~60% complete (6/10 issues done)

**Completed:**
- ✅ BT-X: Issue description
- ✅ BT-Y: Issue description

**In Progress:**
- 🔄 BT-Z: Issue description

**Planned:**
- ⏳ BT-A: Issue description

## Child Issues
- BT-X - Issue title ✅
- BT-Y - Issue title ✅
- BT-Z - Issue title (In Progress)
- BT-A - Issue title

## References
- Design doc: `docs/...`
- Related Epic: BT-XXX

## Next Steps
1. Complete BT-Z (in progress)
2. Start BT-A
3. Write documentation
```

#### Epic Guidelines

**When to create an Epic:**
- Feature requires 5+ related issues
- Work spans multiple components (parser + codegen + runtime)
- Implementation requires sequential phases
- High-level initiative needs progress tracking

**When NOT to create an Epic:**
- Single feature with 1-3 issues (just use issue dependencies)
- Small refactoring with <5 files
- Bug fixes (even if touching multiple areas)

**Epic Maintenance:**
- Update progress percentage monthly or when child issues complete
- Close Epic when all child issues are Done
- Add new child issues as scope clarifies
- Keep "Current Active Epics" table in AGENTS.md up to date

### Writing Agent-Ready Issues

For an issue to be `agent-ready`, include:

1. **Context** - Why this work matters, background info
2. **Acceptance Criteria** - Specific, testable requirements (checkboxes)
3. **Files to Modify** - Explicit paths to relevant files
4. **Dependencies** - Other issues that must complete first
5. **References** - Links to specs, examples, or related code
6. **Blocking Relationships** - Use Linear's "blocks" relationship for dependencies

Example:
```
Title: Implement basic lexer token types

Context:
The lexer is the first phase of compilation. It needs to tokenize
Smalltalk-style message syntax including identifiers, numbers, and keywords.

Acceptance Criteria:
- [ ] Tokenize identifiers (letters, digits, underscores)
- [ ] Tokenize integers and floats
- [ ] Tokenize single and double quoted strings
- [ ] Tokenize message keywords ending in `:`
- [ ] Tokenize block delimiters `[` `]`
- [ ] All tokens include source span

Files to Modify:
- crates/beamtalk-core/src/source_analysis/token.rs
- crates/beamtalk-core/src/source_analysis/lexer.rs

Dependencies: None

References:
- See Gleam lexer: github.com/gleam-lang/gleam/blob/main/compiler-core/src/parse/lexer.rs
```

### Creating Issue Blocking Relationships

When creating issues with dependencies, **always** set up Linear's "blocks" relationships:

```typescript
// After creating issues BT-X and BT-Y, where BT-X blocks BT-Y:
mutation {
  issueRelationCreate(input: {
    issueId: "<BT-X issue ID>"
    relatedIssueId: "<BT-Y issue ID>"
    type: blocks
  }) {
    success
  }
}
```

**Rules:**
- If issue A must be completed before issue B can start, then A "blocks" B
- Always create blocking relationships when dependencies are mentioned
- Linear automatically shows blocked/blocking status in the UI
- Use GraphQL to create relationships after issue creation
- **Set agent-state label** when creating issues:
  - `agent-ready` if fully specified with all acceptance criteria
  - `needs-spec` if human clarification needed first

**Example:** For stdlib implementation issues:
- BT-21 (API definitions) blocks BT-32, BT-33, BT-34, BT-35, BT-36, BT-37
- BT-32 (block evaluation) blocks BT-35 (iteration uses blocks) and BT-37 (collections use blocks)
- All new issues marked with `agent-ready` label

---

## Architecture Decision Records (ADRs)

We use **Architecture Decision Records (ADRs)** to document significant design and architectural decisions in `docs/ADR/`.

### When to Create an ADR

Create an ADR for decisions that:

1. **Affect language design** - Syntax, semantics, operators, control flow
2. **Change core architecture** - Module organization, compilation pipeline, runtime behavior
3. **Impact interoperability** - Erlang/Elixir compatibility, BEAM alignment
4. **Alter user-facing behavior** - Breaking changes, API changes, feature removal
5. **Establish patterns** - Coding conventions, design patterns, best practices

**Examples requiring ADRs:**
- Adding/removing language features (e.g., pattern matching, string interpolation)
- Changing operator semantics (e.g., BT-188: equality operators)
- Architectural refactoring (e.g., DDD reorganization)
- Deprecating features (e.g., ADR 0001: no compound assignment)

**Examples NOT requiring ADRs:**
- Bug fixes (unless they change behavior significantly)
- Documentation updates
- Test additions
- Dependency updates
- Minor refactoring

### ADR Structure

Each ADR must include:

```markdown
# ADR NNNN: Descriptive Title

## Status
Proposed | Accepted | Deprecated | Superseded

## Context
Background, problem statement, why this decision is needed

## Decision
The decision made (clear, concise statement)

## Consequences
### Positive
- Benefits and advantages

### Negative
- Costs, trade-offs, risks

### Neutral
- Other impacts

## References
- Related issues (BT-XXX)
- Documentation links
- Prior discussions
```

### Creating ADRs

1. **Number sequentially:** Use next available number (0001, 0002, etc.)
2. **Title format:** `NNNN-kebab-case-title.md`
3. **One decision per ADR:** Keep focused—split complex decisions into multiple ADRs
4. **Document dependencies:** If ADR B depends on ADR A, state it explicitly
5. **Update the index:** Add your ADR to `docs/ADR/README.md`

**Example workflow:**
```bash
# Create ADR file
vim docs/ADR/0003-add-pattern-matching.md

# Update index
vim docs/ADR/README.md

# Commit with reference
git commit -m "docs: add ADR 0003 - pattern matching syntax BT-XXX"
```

### ADR Best Practices

- **Write early:** Create ADR when decision is made, not months later
- **Be specific:** Include code examples, not just prose
- **Show trade-offs:** Document what you're giving up, not just what you gain
- **Link to issues:** Reference Linear issues (BT-XXX) for context
- **Update status:** Mark as Accepted/Deprecated when status changes
- **Supersede, don't delete:** If decision changes, create new ADR and mark old one as Superseded

### Current ADRs

See `docs/ADR/README.md` for the complete list. Recent examples:
- **ADR 0001:** No compound assignment in Beamtalk (Smalltalk purity)
- **ADR 0002:** Use Erlang comparison operators directly (BEAM-first)

**Reference:** [ADR best practices](https://github.com/joelparkerhenderson/architecture-decision-record)

---

## Agent Skills

This repository includes custom skills in `.github/skills/` that teach Copilot specialized workflows for this project. Skills are automatically loaded when relevant to your prompt.

### Available Skills

| Skill | Trigger | Description |
|-------|---------|-------------|
| **Coding Workflow** | | |
| `pick-issue` | `/pick-issue` | Pick up the next Linear issue from backlog |
| `done` | `/done` | Complete work, commit, push, and create PR |
| `resolve-pr` | `/resolve-pr` | Systematically address PR review comments |
| `resolve-merge` | `/resolve-merge` | Update main, merge into current branch, resolve conflicts |
| **Issue Management** | | |
| `create-issue` | "create issue" | Create Linear issues with blocking relationships |
| `refresh-issue` | `/refresh-issue BT-XX` | Refresh a Linear issue to align with current code state |
| `update-issues` | `/update-issues` | Find and update Linear issues with missing labels or metadata |
| `whats-next` | `/whats-next` | Get recommendations for what to work on next |
| **Architecture** | | |
| `draft-adr` | `/draft-adr` | Research a problem and draft an Architecture Decision Record |
| `plan-adr` | `/plan-adr` | Break an accepted ADR into implementation issues with an Epic |
| **Refactoring** | | |
| `plan-refactor` | `/plan-refactor` | Analyze repo and plan refactoring for code quality/maintainability |
| `do-refactor` | `/do-refactor` | Execute a refactoring epic: all issues on one branch, CI after each |
| **Review** | | |
| `review-code` | `/review-code` | 3-pass code review: diff, system, adversarial (different model) |
| `review-adr` | `/review-adr` | 3-pass ADR review: structural, reasoning, adversarial |

### Skill Locations

- **Project skills** (this repo): `.github/skills/<skill-name>/SKILL.md`
- **Personal skills** (all projects): `~/.copilot/skills/<skill-name>/SKILL.md`

### Creating New Skills

1. Create a directory: `.github/skills/<skill-name>/`
2. Add a `SKILL.md` file with YAML frontmatter:
   ```markdown
   ---
   name: skill-name
   description: When to use this skill. Be specific so Copilot knows when to activate it.
   ---

   # Skill Instructions

   Step-by-step instructions for Copilot to follow...
   ```

3. Optionally add scripts or resources to the skill directory

For more details, see [About Agent Skills](https://docs.github.com/en/copilot/concepts/agents/about-agent-skills).

---

## Repository Structure

```
runtime/
├── rebar.config            # Umbrella project config (ADR 0007, 0009)
├── apps/
│   ├── beamtalk_runtime/   # Core runtime OTP application (ADR 0009)
│   │   ├── src/            # Runtime source (primitives, object system, actors, futures)
│   │   ├── include/        # Header files (beamtalk.hrl)
│   │   ├── test/           # Runtime unit tests (*.erl)
│   │   ├── config/         # OTP sys.config
│   │   └── test_fixtures/  # Fixtures for runtime tests (BT-239)
│   │       ├── logging_counter.bt
│   │       ├── compile.sh  # Auto-compiles fixtures via rebar3 pre-hook
│   │       └── README.md
│   ├── beamtalk_workspace/  # Workspace and interactive development OTP application (ADR 0009) — NEW
│   │   ├── src/            # REPL source (repl eval, server, workspace supervision, session management)
│   │   └── test/           # REPL unit tests (*.erl)
│   └── beamtalk_stdlib/    # Standard library OTP application (ADR 0007)
│       ├── src/            # Application metadata (beamtalk_stdlib.app.src)
│       └── ebin/           # Generated .beam files (Actor, Block, Integer, etc.) created by build-stdlib

tests/
├── stdlib/                # Compiled language feature tests (ADR 0014)
│   ├── arithmetic.bt      # ~32 test files, ~654 assertions
│   ├── blocks.bt          # Run via `just test-stdlib`
│   └── ...
└── e2e/
    ├── cases/             # REPL integration tests (~23 files)
    └── fixtures/          # Shared test fixtures
        └── counter.bt     # CANONICAL counter implementation (BT-239)

examples/
├── counter.bt             # Simple working example for REPL
├── hello.bt              # Minimal example
└── repl-tutorial.md      # Beginner tutorial (BT-239)
```

**Dependency direction (ADR 0009):**
```
beamtalk_workspace (interactive dev)
    ↓ depends on
beamtalk_runtime (core language runtime)
    ↓ depends on
beamtalk_stdlib (compiled stdlib)
```

### Test Organization - CRITICAL DISTINCTION

⚠️ **IMPORTANT:** The test suite has multiple layers. Be precise about which tests you're referring to:

#### 1. Runtime Unit Tests
**Location:** `runtime/apps/beamtalk_runtime/test/*_tests.erl` (e.g., `beamtalk_actor_tests.erl`)
- Tests individual runtime modules in isolation
- Uses hand-written test fixtures (e.g., `test_counter.erl`)
- Calls `gen_server` protocol directly with raw pids
- Appropriate for testing low-level runtime behavior

#### 2. REPL Unit Tests
**Location:** `runtime/apps/beamtalk_workspace/test/*_tests.erl` (ADR 0009)
- Tests REPL evaluation, protocol, server, workspace management
- Includes session supervision and idle monitoring tests
- Moved from `beamtalk_runtime/test/` in BT-351

#### 3. Codegen Simulation Tests  
**Location:** `runtime/apps/beamtalk_runtime/test/beamtalk_codegen_simulation_tests.erl`
- Tests using **real compiled Beamtalk code** from `tests/e2e/fixtures/counter.bt` (unified fixture - BT-239)
- The `spawn/0` and `spawn/1` tests use `counter:spawn()` from compiled module
- Other tests use simulated state structures for complex scenarios
- **Test fixtures compile automatically** via rebar3 pre-hook (no manual step needed)
- Fixtures: `logging_counter.bt` stored in `runtime/apps/beamtalk_runtime/test_fixtures/`, `counter.bt` sourced from E2E fixtures
- Compiled by `runtime/apps/beamtalk_runtime/test_fixtures/compile.sh` (rebar3 pre-hook)
- See `docs/development/testing-strategy.md` for compilation workflow details

#### 4. Stdlib Tests (Compiled Expression Tests) — ADR 0014
**Location:** `tests/stdlib/*.bt` (~57 files)
- **Pure language feature tests** compiled directly to EUnit (no REPL needed)
- Uses same `// =>` assertion format as E2E tests
- Runs via `just test-stdlib` (fast, ~14s vs ~50s for E2E)
- Tests arithmetic, strings, blocks, closures, collections, object protocol, etc.
- Supports `@load` directives for fixture-dependent tests (actors, sealed classes)
- **Use this for any new test that doesn't need REPL/workspace features**

#### 5. BUnit Tests (TestCase Classes) — ADR 0014 Phase 2
**Location:** `test/*.bt` (project test directory)
- **SUnit-style test classes** that subclass `TestCase`
- Methods starting with `test` are auto-discovered and run with fresh instances
- `setUp`/`tearDown` lifecycle methods for test fixtures
- Assertion methods: `assert:`, `assert:equals:`, `deny:`, `should:raise:`, `fail:`
- Runs via `beamtalk test` (compiles to EUnit, fast)
- Can also run interactively in REPL: `CounterTest runAll` or `CounterTest run: #testName`
- **Use this for stateful tests, complex actor interactions, multi-assertion scenarios**

#### 6. End-to-End Tests (REPL Integration)
**Location:** `tests/e2e/cases/*.bt` (~18 files)
- Require a running REPL daemon (started automatically by test harness)
- Test workspace bindings, REPL commands, variable persistence, auto-await
- Test `ERROR:` assertion patterns and `@load-error` directives
- Runs via `just test-e2e` (~50s)
- **Only use for tests that genuinely need the REPL**

**When choosing between stdlib, BUnit, and E2E tests:**
| Test needs... | Where |
|---|---|
| Pure language features (arithmetic, strings, blocks) | `tests/stdlib/` |
| Collections (List, Dictionary, Set, Tuple) | `tests/stdlib/` |
| Actor spawn + messaging (with `@load`) | `tests/stdlib/` |
| Stateful tests with setUp/tearDown | `test/*.bt` (BUnit) |
| Complex actor interactions, multiple assertions | `test/*.bt` (BUnit) |
| Workspace bindings (Transcript, Beamtalk) | `tests/e2e/cases/` |
| REPL commands, variable persistence | `tests/e2e/cases/` |
| Auto-await, `ERROR:` assertions | `tests/e2e/cases/` |

#### Test Fixture Organization (BT-239)

**Runtime fixtures:** `runtime/apps/beamtalk_runtime/test_fixtures/`
- Colocated with runtime tests for better locality
- Compiled by `apps/beamtalk_runtime/test_fixtures/compile.sh` (rebar3 pre-hook)
- Currently: `logging_counter.bt` (super keyword tests)
- Note: `counter.bt` consolidated to E2E fixture

**E2E fixtures:** `tests/e2e/fixtures/`
- Used by E2E test cases
- `counter.bt` is the canonical Counter implementation
- Also used by runtime tests (unified fixture)

---

## Development Guidelines

Detailed coding standards and task guides are in `docs/development/`:

| Guide | Description |
|-------|-------------|
| [Rust Guidelines](docs/development/rust-guidelines.md) | Naming, traits, error handling, testing, compiler patterns |
| [Erlang Guidelines](docs/development/erlang-guidelines.md) | Code generation, OTP patterns, BEAM interop |
| [Common Tasks](docs/development/common-tasks.md) | Adding AST nodes, CLI commands, stdlib features |
| [Language Features](docs/beamtalk-language-features.md) | Full Beamtalk syntax specification |
| [Syntax Rationale](docs/beamtalk-syntax-rationale.md) | Why we keep/change Smalltalk conventions |

**Key rules (see full docs for details):**

### Static Verification (CI Commands)

This project uses **Just** for build orchestration:

```bash
just --list                  # See all available commands

# Common development commands
just build                   # Build Rust + Erlang runtime
just test                    # Run fast tests (~10s)
just test-stdlib             # Compiled language feature tests (~14s)
beamtalk test                # Run BUnit TestCase tests (not yet in CI)
just test-e2e                # Run E2E tests (~50s)
just ci                      # Run all CI checks

# Individual checks
just fmt                     # Format all code
just fmt-check               # Verify formatting
just clippy                  # Lints (warnings = errors)
just dialyzer                # Erlang type checking
just test-rust               # Rust tests only
just test-runtime            # Erlang runtime tests only

# Advanced
just test-all                # All tests (unit + stdlib + E2E + runtime)
just clean                   # Clean build artifacts (works with Docker volumes)
just coverage                # Generate coverage reports
just fuzz [DURATION]         # Fuzz parser for crash safety (default: 60s, needs nightly)
```

**Raw cargo/rebar3 commands** (if needed):
```bash
cargo fmt --all                           # Format all crates
cargo fmt --all -- --check                # Verify formatting
cargo clippy --all-targets -- -D warnings # Lints (warnings = errors)
cargo test --all-targets                  # Run all tests
cd runtime && rebar3 dialyzer             # Erlang type checking
```

**Dialyzer (Erlang Type Checker):**

Dialyzer performs static analysis on Erlang runtime code to catch type errors, incorrect specs, and unreachable code. It runs automatically in CI via `just lint`.

```bash
just dialyzer                # Run Dialyzer type checking
cd runtime && rebar3 dialyzer # Direct rebar3 command
```

**Current configuration** (`runtime/rebar.config`):
- Focuses on `error_handling` warnings (real bugs)
- Disables `underspecs` and `unmatched_returns` (noisy, not bugs)
- Future issues can address remaining spec precision improvements

**When to run:**
- Before committing Erlang runtime changes
- After modifying function signatures or specs
- CI runs automatically on every push/PR

### Workspace Commands (Planned - ADR 0004)

```bash
# Development
beamtalk repl                    # Connect to project workspace (auto-create)
beamtalk repl --session debug    # Named session, same workspace
beamtalk repl --workspace exp    # Different workspace (different code)

# Production
beamtalk attach prod@host        # Attach REPL to running production node
beamtalk run server.bt           # Run as daemon (no auto-cleanup)

# Management
beamtalk workspace list          # List active workspaces
beamtalk workspace stop <name>   # Stop a workspace
```

See [ADR 0004: Persistent Workspace Management](docs/ADR/0004-persistent-workspace-management.md) for full architecture.

### License Headers

All source files must include Apache 2.0 header:
```rust
// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
```

### Beamtalk Style (Critical)

- **Implicit returns**: Use `^` ONLY for early returns, never on last expression
- **No periods**: Newlines separate statements, not `.`
- **Comments**: Use `//` and `/* */`, not Smalltalk's `"..."`

### Code Generation — Document API Only

**CRITICAL:** All Core Erlang code generation MUST use the `Document` / `docvec!` API. **Never** use `format!()`, `write!()`, or string concatenation to build Core Erlang output.

```rust
// ❌ WRONG - string-based generation (fragile, unverifiable)
format!("call '{module}':'{function}'({args})")
write!(result, "let {var} = ").unwrap();

// ✅ RIGHT - Document API (structural, composable)
docvec![
    Document::String(format!("call '{module}':'{function}'(")),
    args_doc,
    Document::String(")".to_string()),
]
```

**Why:**
- String-based codegen has no structural guarantees — easy to get whitespace, quoting, or nesting wrong
- `Document` provides composable, pretty-printable output with correct indentation
- Structural generation is easier to review and maintain
- Snapshot tests verify output stability

**When modifying codegen files:** If you see existing `format!`/`write!` patterns, convert them to `Document` as part of your change. Don't add new string-based generation.

### Clippy Discipline

**Never suppress clippy warnings without justification.** The goal is to keep suppressions under 30 across the entire codebase.

```rust
// ❌ WRONG - blanket suppression
#[allow(clippy::too_many_lines)]
fn my_function() { ... }

// ✅ RIGHT - refactor the function instead
fn my_function() {
    let result = helper_one();
    helper_two(result)
}

// ✅ ACCEPTABLE - when suppression is truly needed, document why
#[allow(clippy::cast_sign_loss)] // arity is always non-negative from parser
let arity_u32 = arity as u32;
```

**Rules:**
1. **`too_many_lines`** — Split the function. If genuinely irreducible (e.g., one arm per AST variant), add a comment explaining why.
2. **`unnecessary_wraps`** — Fix the return type. If the signature must match a trait, document it.
3. **`dead_code`** — Remove it. If planned for future use, create an issue and remove the code.
4. **New `#[allow(...)]`** — Requires a comment explaining why suppression is necessary.

### Logging Strategy

The beamtalk CLI uses the `tracing` crate for structured logging and instrumentation.

**When to use each log level:**

| Level | Use For | Example |
|-------|---------|---------|
| `trace!` | Very detailed execution flow, function entry/exit | Function parameters, loop iterations |
| `debug!` | Internal state, diagnostics useful during development | File paths, intermediate values, compilation steps |
| `info!` | Important milestones and successful operations | "Starting build", "Compilation completed" |
| `warn!` | Recoverable issues, deprecation warnings | "Failed to clean up temp file", "Using fallback" |
| `error!` | Errors that affect user operations | Compilation errors, file not found |

**Instrumentation best practices:**

1. **Use `#[instrument]` for spans** - Add to key functions to create automatic spans:
   ```rust
   #[instrument(skip_all, fields(path = %path))]
   pub fn build(path: &str) -> Result<()> { ... }
   ```

2. **Skip sensitive data** - Use `skip` or `skip_all` to avoid logging secrets:
   ```rust
   #[instrument(skip(password))]
   fn authenticate(user: &str, password: &str) { ... }
   ```

3. **Add structured fields** - Include key-value pairs for better filtering:
   ```rust
   info!(count = files.len(), "Found source files");
   ```

4. **Keep user output separate** - Use `println!` for user-facing output, tracing for diagnostics:
   ```rust
   println!("Building {} files...", count);  // User sees this
   info!(count, "Starting build");           // Logged when RUST_LOG is set
   ```

**Enabling logging:**

```bash
# Development - see debug output
RUST_LOG=beamtalk_cli=debug beamtalk build examples/

# Production - errors and warnings only
RUST_LOG=warn beamtalk build myproject/

# Full trace for debugging
RUST_LOG=beamtalk_cli=trace beamtalk build test.bt

# Multiple modules
RUST_LOG=beamtalk_cli=debug,beamtalk_core=info beamtalk build
```

**What NOT to log:**
- User-facing output (use `println!` instead)
- Sensitive data (passwords, tokens, credentials)
- High-frequency operations in hot paths (use `trace!` sparingly)
- Duplicate information already in `println!` output

---

## File Conventions

| Extension | Description |
|-----------|-------------|
| `.bt` | Beamtalk source files |
| `.core` | Generated Core Erlang |
| `.beam` | Compiled BEAM bytecode |
| `.erl` | Erlang source (helpers, tests) |
| `.hrl` | Erlang header files |

---

## Resources

### Beamtalk Design
- [Design Principles](docs/beamtalk-principles.md) - Core philosophy guiding all decisions
- [Language Features](docs/beamtalk-language-features.md) - Planned syntax and features
- [Syntax Rationale](docs/beamtalk-syntax-rationale.md) - Why we keep/change Smalltalk conventions
- [Architecture](docs/beamtalk-architecture.md) - Compiler, runtime, and live development flow
- [Agent-Native Development](docs/beamtalk-agent-native-development.md) - AI agents as developers and live actor systems

### Rust Guidelines
- [Microsoft Rust Guidelines](https://microsoft.github.io/rust-guidelines/) - Pragmatic patterns for safety and maintainability
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/checklist.html) - Naming, traits, documentation standards
- [Rust Style Guide](https://doc.rust-lang.org/nightly/style-guide/) - Formatting conventions
- [Rust Design Patterns](https://rust-unofficial.github.io/patterns/) - Common patterns and idioms

### BEAM/Erlang
- [Core Erlang specification](https://www.it.uu.se/research/group/hipe/cerl/)
- [BEAM VM internals](https://blog.stenmans.org/theBeamBook/)

### Reference Implementations
- [Gleam compiler](https://github.com/gleam-lang/gleam) - Rust-to-BEAM reference
- [Newspeak language](https://newspeaklanguage.org/) - Module system inspiration
- [TypeScript compiler](https://github.com/microsoft/TypeScript/wiki/Architectural-Overview) - Tooling-first architecture
