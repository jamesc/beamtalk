# BT-4292: guarded static dispatch baseline

The starting point for epic BT-4291 (guarded static dispatch; ADR 0025
amendment, 2026-10-29). It records two things the epic's children are
measured against: the vs-elixir rows for the send shapes they convert, and the
dynamic-site report (`BEAMTALK_CODEGEN_DIAGNOSTICS=1 beamtalk build`) that
counts the sends still compiled to `beamtalk_message_dispatch:send/3`.

Measured at `85e9221c8` (main) plus BT-4292, with the dev build of the
compiler (`target/debug/beamtalk`). OTP 28 (erts 16.4), Elixir 1.20.1,
4 schedulers, load average about 1.0. As in the README, reductions and words
are deterministic; time is noisy on a shared machine.

## vs-elixir rows

`run.sh bench 5 1a,1b,4c,10a,10d,14a,14b`. Rows 14a and 14b are new in BT-4292.
The open-Value typed send (1b, 4c), typed `List includes:` (10d) and typed
`Dictionary at:put:` (10a) rows already existed; 1a (sealed Value, the BT-4069
guarded direct call) is the target the others are compared with.

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | +red | +words | Beamtalk compiles to | Converted by |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|---|---|
| 1a | getter on a sealed Value: p x / p.x | 10.5 | 3.00 | 0.00 | 7.13 | 0.00 | 0.00 | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069) | (target) |
| 1b | getter on an open Value: p x / p.x | 842 | 29.1 | 28.0 | 2.84 | 0.00 | 0.00 | 29.1 | 28.0 | send(P, x, []) | BT-4293 |
| 4c | typed open Value: s area / Square.area(s) | 992 | 29.1 | 28.0 | 3.89 | 2.00 | 0.00 | 27.1 | 28.0 | send(S, area, []) | BT-4293 |
| 10a | Dictionary at:put: / Map.put | 453 | 18.1 | 10.0 | 78.6 | 1.01 | 6.00 | 17.0 | 4.00 | send(D, 'at:put:', ...) | BT-4294 |
| 10d | List includes: (100 elements) / Enum.member? | 140 | 19.0 | 0.00 | 98.0 | 8.00 | 0.00 | 11.0 | 0.00 | send(L, 'includes:', [50]) | BT-4294 |
| 14a | typed Tuple at: / elem(t, 0) | 82.8 | 14.0 | 0.00 | -0.99 | 0.00 | 0.00 | 14.0 | 0.00 | send(T, 'at:', [1]) | BT-4294 |
| 14b | class reference: last := VbPoint / Point | 1061 | 17.1 | 21.0 | 0.85 | 0.00 | 0.00 | 17.1 | 21.0 | class reference built per use | BT-4295 |

## Dynamic-site report

Each dynamic send prints a hint naming the receiver's static type and the
reason it stayed dynamic (`docs/development/debugging.md`, "Dynamic dispatch
fallback"). Count by reason with:

```bash
BEAMTALK_CODEGEN_DIAGNOSTICS=1 beamtalk build --force 2>&1 \
  | grep -o 'reason: [a-z-]*' | sort | uniq -c
```

### The vs-elixir package (`benchmarks/vs-elixir/beamtalk`)

20 dynamic sites:

| Reason | Sites | Which |
|---|--:|---|
| `builtin` | 8 | `Dictionary at:put:`/`at:`, `List add:`/`includes:`, `Tuple at:`, `String isKindOf:`, `Integer to:`/`timesRepeat:` |
| `open` | 6 | `VbOpenPoint x`, `VbSquare area`, `VbSub base` (2), `VbSub viaSelf`, `Interval asList` |
| `actor` | 4 | `VbCounter count` (2), `increment`, `probe` |
| `untyped` | 2 | rows 4a and 4b (deliberately untyped) |

### Catalina

The epic's baseline (beamtalk `f5accd5`, Catalina 0.2.0) was taken with a
one-off instrumented compiler: 220 dynamic sites across 21 files — 68
built-in, 43 `Supervisor` subclasses, 9 declared `Object`, 6 open Value
(`SupervisionSpec`), 15 `Exception`/`Symbol`/`Pid`, 9 actors, 70 untyped
(epic BT-4291 has the full table). The report above replaces that
instrumentation; its reason tokens map onto the epic's rows as `builtin`,
`supervisor`, `abstract`, `open`, `exception`/`builtin`, `actor` and
`untyped`. Re-running it on Catalina and recording the per-reason counts here
is still to do: it needs a Catalina checkout and build, which this change did
not run.
