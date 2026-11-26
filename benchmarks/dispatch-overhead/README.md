# Dispatch overhead per call (BT-4983)

What does one send cost in Beamtalk, and which runtime mechanisms is the cost
made of? Each row runs one kind of operation N times in a fresh process, using
the same loop shape as `benchmarks/vs-elixir` (`n timesRepeat: [last := <op>]`,
so the loop is inlined and the result stays live). Unlike vs-elixir there is no
Elixir side: this is for measuring the compiler and runtime against themselves,
so a fix to dispatch can be shown to move a number.

```bash
just bench-dispatch count            # calls per operation, per function: deterministic
just bench-dispatch time 9           # median of 9 interleaved rounds: ns/op, reductions/op
just bench-dispatch cost             # isolated cost of each leaf mechanism, ns
just bench-dispatch report 9         # time x count x cost: ns/op split by mechanism
just bench-dispatch keys p3 c5 r1    # which persistent_term key each read fetches, and who reads it
just bench-dispatch count p3 c1      # any mode takes row ids
```

or `benchmarks/dispatch-overhead/run.sh <mode> ...`. `BEAMTALK=/path/to/beamtalk`
picks the compiler (its sysroot supplies the runtime and stdlib), as in
`benchmarks/vs-elixir/run.sh`. The package is rebuilt with `--force` on every run.

## Rows

| Id | Group | Operation |
|---|---|---|
| p1-p3 | plain sends | getter on a typed sealed Value, a typed open Value, an untyped receiver |
| p4-p6 | plain sends | `Tuple at:` typed and untyped, `Dictionary at:` untyped |
| p7 | plain sends | `Actor` sync call (gen_server round trip) |
| s1-s4 | self sends | sealed Value (two forms), open Value, inside an actor method |
| c1-c3, c5 | class-side sends | `class sealed`, sealed class, open class, class held in a variable |
| f1-f6 | FFI | typed arguments (proved, direct call), untyped arguments (the `direct_call/4` proxy path) |
| e1-e2 | exceptions | `on:do:` and `ensure:` with no raise |
| r1-r5 | `^` in blocks | inlined `ifTrue:`, `do:` taken, `to:do:` taken, `do:` never taken, `^` in both branches of a tail `ifTrue:ifFalse:` (untyped receiver) |

## Methods

- **count**: `call_count` tracing of every function of every loaded module
  over the operation (all processes, so a gen_server's side counts), reported
  per operation. The mechanism columns (`persistent_term:get`,
  `function_exported`, `erlang:throw`, ...) are listed in `mechanisms/0`.
  Deterministic; identical across runs.
- **time**: the median of interleaved rounds, ns/op and reductions/op of the
  worker process, net of the `baseline` row (the bare loop). Noisy on a shared
  container: use it to compare rows within one run, and rerun on your own
  machine for absolute numbers.
- **cost**: one occurrence of each leaf mechanism in a tight loop, net of an
  empty loop.
- **report**: for each row, `occurrences x isolated cost` per mechanism next to
  the measured ns/op. `other` is the remainder: Erlang function calls, pattern
  matches and map reads around the mechanisms (see `count` for the functions).
- **keys**: traces `persistent_term:get` and prints the key and the calling
  function for 10 operations.

## Caveats

- Costs are for one core of whatever machine runs it; the numbers in the
  BT-4983 comment came from a shared 4-vCPU Xeon @ 2.1 GHz container.
- `report`'s isolated costs are best-case (hot cache, tight loop). In a large
  program a `persistent_term:get` or an `ets:lookup` can cost more.
- Actor rows (p7, s4) include a process round
  trip; their `other` column is mostly scheduling and message passing.
- Class methods that read or write the process (`put`, `erase`, `self()`) are
  rejected by the compiler in a stateless class, so the FFI rows use
  process-neutral functions.
