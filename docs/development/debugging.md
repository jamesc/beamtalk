# Debugging Workflow

Step-by-step debugging for common failures in the Beamtalk compiler and runtime.

## Compiler Crashes

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
# For parser: add dbg!(&ast) in source_analysis/parser/mod.rs
# For codegen: add dbg!(&expr) in codegen/core_erlang/
```

## Runtime Errors

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

## Test Failures

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

## E2E Test Failures

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

## Codegen Debugging

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

## Runtime/REPL Debugging

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

## Performance Debugging

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

## When All Else Fails

1. **Simplify** — Remove code until it works, then add back
2. **Compare** — Find similar working code, diff against it
3. **Ask** — Share error + what you tried, get fresh eyes
4. **Rubber duck** — Explain the problem out loud to yourself
5. **Sleep** — Come back tomorrow with fresh perspective
