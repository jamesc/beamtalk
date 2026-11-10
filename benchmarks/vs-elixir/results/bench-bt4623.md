# BT-4623: per-process generation memo for tagged_class_dispatch

An untyped send to a Value (`o x`, `o area`) resolves the receiver's class
through `beamtalk_primitive:tagged_class_dispatch/1`. Before this change every
send fetched the answer from `beamtalk_class_derived_cache`: a generation read
plus an ETS lookup that copies the entry, with two closures built per call.
The answer is now memoised in the calling process's dictionary as
`{Gen, Resolved}`, tagged with the cache generation, as
`beamtalk_default_method:is_default/2` does (BT-4416). A warm send costs a
generation read, a `get/1` and the `module_loaded` purge check.

Rows 4a and 4b drop from 25.1 to 24.1 reductions and from 28 to 11 words per
send, the targets in the issue. Time drops by 14% on both rows, net of the
loop baseline.

| Row | Before ns | After ns | Before red | After red | Before words | After words |
|---|--:|--:|--:|--:|--:|--:|
| 4a `o x` | 169 | 145 | 25.1 | 24.1 | 28.0 | 11.0 |
| 4b `o area` | 167 | 143 | 25.1 | 24.1 | 28.0 | 11.0 |

Micro-bench (`beamtalk_primitive:send(P, x, [])` on the row 4a point, 1M
sends per fresh process, median of 7 processes, two runs each):

| | Run 1 | Run 2 |
|---|--:|--:|
| Before | 87.3 ns | 87.3 ns |
| After | 60.4 ns | 61.0 ns |

That is −30% on this machine (Apple M5 Max, 18 schedulers). The
issue's −45% / ≤175 ns target was set on a VM where `atomics:get` takes 33 ns
and the ETS lookup 163 ns, so the lookup was a larger share of the send there;
the saving in absolute terms is the ETS read and its copy either way.

Commands, both on the same compiler, with the runtime rebuilt from the
branch with and without the change:

```bash
BEAMTALK=$PWD/target/release/beamtalk benchmarks/vs-elixir/run.sh bench 7 4a,4b
```

## Before (main 4d259ca1d)

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 19:20  up 44 days,  5:09, 3 users, load averages: 2.55 3.49 3.76
# rounds 7
# load average after: 19:20  up 44 days,  5:10, 3 users, load averages: 2.58 3.48 3.76

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 4a | untyped receiver: o x / o.x | 169 | 25.1 | 28.0 | 1.38 | 0.00 | 0.00 | 122.4 | - | - | 25.1 | 28.0 | send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic) |
| 4b | untyped receiver: o area / protocol | 167 | 25.1 | 28.0 | 8.58 | 7.00 | 0.00 | 19.4 | 3.6 | - | 18.1 | 28.0 | send(O, area, []), same path as 4a |


## After

# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 18
# load average: 19:20  up 44 days,  5:10, 3 users, load averages: 2.58 3.48 3.76
# rounds 7
# load average after: 19:20  up 44 days,  5:10, 3 users, load averages: 2.54 3.46 3.75

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 4a | untyped receiver: o x / o.x | 145 | 24.1 | 11.0 | 1.49 | 0.00 | 0.00 | 97.3 | - | - | 24.1 | 11.0 | send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic) |
| 4b | untyped receiver: o area / protocol | 143 | 24.1 | 11.0 | 8.52 | 7.00 | 0.00 | 16.8 | 3.4 | - | 17.1 | 11.0 | send(O, area, []), same path as 4a |

