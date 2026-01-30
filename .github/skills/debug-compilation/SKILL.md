---
name: debug-compilation
description: Debug Beamtalk compilation issues. Use when troubleshooting compiler errors, investigating codegen problems, or inspecting intermediate representations.
---

# Debugging Compilation

Follow this workflow when debugging compilation issues:

## Steps

1. **Enable debug output**
   ```bash
   BEAMTALK_DEBUG=1 beamtalk compile myfile.bt
   ```
   This prints intermediate representations at each compilation phase.

2. **Check generated Core Erlang**
   - Look in `build/` directory for `.core` files
   - Core Erlang is human-readable and shows the codegen output
   - Compare against expected output

3. **Use erlc debug options**
   ```bash
   erlc +debug_info myfile.core
   ```
   This adds debug info to the BEAM file for inspection.

4. **Inspect BEAM files**
   ```erlang
   beam_lib:chunks("myfile.beam", [abstract_code]).
   beam_lib:chunks("myfile.beam", [atoms]).
   beam_lib:chunks("myfile.beam", [exports]).
   ```
   Run in `erl` shell to examine compiled bytecode.

## Common Issues

### Parser Errors
- Check lexer output: tokens being generated
- Verify precedence rules for message parsing
- Look for error recovery producing unexpected AST

### Codegen Errors
- Compare generated Core Erlang with working examples
- Verify all variables are properly scoped
- Check for missing fully-qualified calls (`'erlang':'+'` not `+`)

### Runtime Errors
- Add tracing: `io:format("~p~n", [Value])`
- Check process message queues: `process_info(Pid, messages)`
- Verify OTP behavior callbacks are correct

## Useful Commands

```bash
# Full compilation with all checks
cargo build --all-targets && cargo test

# Run single test with output
cargo test test_name -- --nocapture

# Update snapshots after intentional changes
cargo insta review

# Check Core Erlang syntax
erlc -S myfile.core

# Run generated BEAM
erl -noshell -eval 'mymodule:main(), halt().'
```

## Debugging Workflow

1. Reproduce the issue with a minimal `.bt` file
2. Run with `BEAMTALK_DEBUG=1` to see IR
3. If parser issue: check AST output
4. If codegen issue: check `.core` file
5. If runtime issue: run in `erl` shell with tracing
