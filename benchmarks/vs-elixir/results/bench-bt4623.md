# BT-4623: per-process generation memo for tagged_class_dispatch

`beamtalk_primitive:tagged_class_dispatch/1` (run on every dynamic send to a
tagged Value) now keeps a per-process `{Gen, Resolved}` memo keyed by the
derived-cache generation, with the purge check kept. A hit costs a generation
read and a `get/1` instead of the derived-cache ETS lookup.

Rows 4a/4b, 7 rounds, median, `run.sh bench 7 4a,4b`, otp 28 / 4 schedulers
(a slow-ETS VM; reductions and words are machine-independent).

| Row | Operation | Before ns | Before red | Before words | After ns | After red | After words |
|---|---|--:|--:|--:|--:|--:|--:|
| 4a | untyped receiver: `o x` | 515 | 25.1 | 28.0 | 384 | 24.1 | 11.0 |
| 4b | untyped receiver: `o area` | 487 | 25.1 | 28.0 | 396 | 24.1 | 11.0 |

Target from the issue: row 4a <= 24.1 reductions and <= 11 words. Met.

Raw runs: before = `origin/main` source of `beamtalk_primitive.erl`; after = this branch.
