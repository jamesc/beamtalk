# vs-elixir bench
# otp 28, erts 16.4, elixir 1.20.1, schedulers 4
# load average: 03:56:38 up  2:12,  0 user,  load average: 11.50, 13.34, 15.56
# rounds 7
# load average after: 03:56:47 up  2:12,  0 user,  load average: 11.66, 13.31, 15.53

## Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 7b | actor state update: c increment / GenServer.call | 5289 | 92.3 | 152 | 3291 | 37.2 | 56.0 | 1.6 | 2.5 | 2.7 | 55.1 | 96.0 | as 7a |
| 7e | inherited self-send: sc viaSelf (VbSubCounter) / defdelegate in a GenServer | 6007 | 123 | 203 | 3303 | 39.2 | 56.0 | 1.8 | 3.2 | 3.6 | 84.3 | 147 | as 7a; VbSubCounter:dispatch/4 misses both sends and its default arm runs VbCounter's method (BT-4280: memoised inherited_target + invoke_resolved) |
| 7f | local self-send: c viaSelf / local call in a GenServer | 5202 | 97.5 | 159 | 3100 | 38.2 | 56.0 | 1.7 | 2.6 | 2.8 | 59.3 | 103 | as 7a; in the actor `self increment` is a direct VbCounter:dispatch/4 call |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 7b | `gen_update` | 3291 | 37.2 | 56.0 |
| 7e | `gen_via_self` | 3303 | 39.2 | 56.0 |
| 7f | `gen_via_self` | 3100 | 38.2 | 56.0 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 3.41 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 3.18 | 1.00 | 0.00 | 1.00 | 0.00 |
| 7b | bt | actorUpdate:c: | 5292 | 93.3 | 152 | 44.1 | 83.0 |
| 7b | ex | gen_update | 3294 | 38.2 | 56.0 | 17.1 | 33.0 |
| 7e | bt | actorViaSelfSub:c: | 6011 | 124 | 203 | 44.1 | 83.0 |
| 7e | ex | gen_via_self | 3306 | 40.2 | 56.0 | 17.2 | 33.0 |
| 7f | bt | actorViaSelf:c: | 5205 | 98.5 | 159 | 44.1 | 83.0 |
| 7f | ex | gen_via_self | 3103 | 39.2 | 56.0 | 17.2 | 33.0 |
