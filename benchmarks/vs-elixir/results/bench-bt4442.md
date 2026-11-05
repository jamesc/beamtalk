# BT-4442: one static call key per class

BT-4431 (`bench-bt4431.md`, section 2) measured what a metadata write on a
base class costs once its descendants' static call sites are warm:
`invalidate/1` erased one `persistent_term` key per warm call target, and each
erase waits for thread progress on every scheduler. 50 subclasses with 10
warm targets each took 122 ms.

BT-4442 keeps one key per class, the atom `'<Name> class static calls'`. Its
value is a map from each installed call-site key (`{Module, FunName, Tag}`,
`{caller_safe_raise, ...}` or `{Module, new, Tag}`, unchanged) to the class
reference. A call site reads the class's key once and matches its own target
in the map. `erase_direct_flag/1` erases at most two keys per class: the
derived flag and the map.

Measured on this branch and on main @ ccc903c with the dev build of the
compiler (`target/debug/beamtalk`) and the dev runtime tree. OTP 28 (erts
16.4), Elixir 1.20.1, 4 schedulers, a Linux VM with no other work running
(load average 1 to 3). Reductions are deterministic. ns and µs depend on the
machine. "Before" is main with this branch's harness, which also times the
re-warm (BT-4431's harness did not).

## 1. `invalidate/1` over a hierarchy

`run.sh invalidate 20`. Each round re-warms every target through
`static_call_target/4`, times one `invalidate/1` of the root, and checks that
every target is gone. The re-warm column is the time to install all N × K
targets again.

| background | N | K | targets | before median µs | after median µs | before re-warm µs | after re-warm µs |
|---|--:|--:|--:|--:|--:|--:|--:|
| idle | 0 | 0 | 0 | 15 | 16 | 0 | 0 |
| idle | 1 | 1 | 1 | 280 | 365 | 375 | 498 |
| idle | 1 | 10 | 10 | 1783 | **373** | 2732 | 2793 |
| idle | 50 | 0 | 0 | 853 | 1260 | 0 | 0 |
| idle | 50 | 1 | 50 | 23131 | 21547 | 25250 | 24317 |
| idle | 50 | 10 | 500 | 102797 | **20403** | 138283 | 127802 |
| 10000 idle processes | 1 | 10 | 10 | 1250 | **361** | 1782 | 1945 |
| 10000 idle processes | 50 | 1 | 50 | 14378 | 20511 | 17558 | 25595 |
| 10000 idle processes | 50 | 10 | 500 | 75069 | **16428** | 102570 | 115636 |
| 4 busy processes | 1 | 10 | 10 | 1600 | **334** | 1917 | 2677 |
| 4 busy processes | 50 | 1 | 50 | 15781 | 14892 | 17972 | 29118 |
| 4 busy processes | 50 | 10 | 500 | 63276 | **16992** | 107278 | 122165 |

A bare `persistent_term:erase/1` of a present key took a median of 189 µs
(before) and 213 µs (after) in the same node.

- **The erase now grows with the number of classes, not with warm targets.**
  50 × 10 falls from 103 ms to 20 ms, the cost of 50 × 1 (one present map per
  class, plus the derived flag). 1 × 10 falls from 1.8 ms to 0.4 ms.
- **Re-warming costs about the same here, but it is a dearer kind of
  update.** One `put` per target installed, as before, and a put of a new key
  already waited for thread progress. But all but a class's first install now
  replace a present value, which releases a literal area: every process's
  heap is checked for the old term, as on an erase. On this node that shows
  as a small rise with 10,000 processes (103 to 116 ms) and with busy
  schedulers (107 to 122 ms); a node with many large heaps would pay more.
  The cost moves from the writer's synchronous erase to the lazy re-warm,
  spread over the first send of each site.
- Rows with no keys (N × 0) and with one target per class are unchanged within
  noise: they made one or two updates per class before and still do.

## 2. The hit path (rows 3a-3c)

`run.sh bench 7 3a,3b,3c` (and a 15-round rerun of each). Beamtalk side, net
of the loop baseline.

| Row | before red | after red | before ns | after ns (7 rounds) | after ns (15 rounds) |
|---|--:|--:|--:|--:|--:|
| 3a | 3.00 | 3.00 | 29.2 | 22.4 | 21.9 |
| 3b | 3.00 | 3.00 | 28.5 | 22.3 | 23.0 |
| 3c | 3.00 | 3.00 | 26.8 | 28.9 | 25.9 |

Still 3 reductions. The hit path is one `persistent_term:get/2` call, then
`is_map` and `get_map_elements` with the literal call-site key, which are
instructions, not calls.

**Why the key is an atom.** The issue's first design keyed the map by the
tuple `{beamtalk_static_calls, Tag}`. Its hit path measured 37 to 41 ns on
rows 3a-3c (two runs of 15 rounds), about 10 ns slower than main. A
standalone loop of 20M reads (`persistent_term:get/2`, then the match, on the
same node) showed where the time goes:

| key | map size 1 | 3 | 10 |
|---|--:|--:|--:|
| per-target `{Module, FunName, Tag}` (main), no map | 25.8 ns | | |
| per-class `{beamtalk_static_calls, Tag}` + map match | 36.4 ns | 36.9 ns | 36.3 ns |
| per-class atom + map match | 20.8 ns | 20.7 ns | 21.0 ns |

The map match costs about nothing at these sizes. Hashing the tuple key in
`persistent_term:get/2` is the dear part. That is why BT-3700 keyed the
derived flag by the bare tag atom. The atom key makes the hit path slightly
cheaper than main's.
