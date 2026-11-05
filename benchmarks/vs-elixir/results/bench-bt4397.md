# BT-4397: typed built-in sends, main vs the option 1 prototype

Rows 1a, 10a-10d and 14a, 5 rounds each, dev build, one 4-scheduler Linux
container. Reduction counts are deterministic; compare ns only within one run.

- **Main** is `7e66fcd`. Row 14a is already at 3.00 reductions, the same as
  1a, because BT-4400 inlined the in-range read inside `Tuple>>at:`. That
  meets BT-4397's target of at most 3.3. Profile: the loop, then
  `bt@stdlib@tuple:dispatch/3`, then `bt@stdlib@tuple:'at:'/2`.
- **Option 1 prototype** was not merged (follow-up BT-4481). When the class
  defines the selector itself as a pure `@primitive`, the guarded branch
  called `Mod:'sel'/N+1` directly instead of `Mod:dispatch/3`. Profile for
  14a: the loop, then `bt@stdlib@tuple:'at:'/2` (2 calls/op).

  It breaks ADR 0025 amendment §2 for stdlib built-ins. `removeSelector:`
  recompiles the module without the function, so a stale call site raises
  `undef`. A live patch can give the method a non-local-return body, whose
  `{Result, State}` result only `dispatch/3` unwraps.

| Row | Main red / words | Prototype red / words |
|---|--:|--:|
| 1a | 3.00 / 0 | 3.00 / 0 |
| 10a | 4.03 / 10.0 | 3.01 / 6.00 |
| 10b | 3.00 / 0 | 2.00 / 0 |
| 10c | 6.82 / 202 | 6.66 / 202 |
| 10d | 9.00 / 0 | 8.00 / 0 |
| 14a | 3.00 / 0 | 2.00 / 0 |

## Main (`7e66fcd`)

    # otp 28, erts 16.4, elixir 1.20.1, schedulers 4
    # load average: 16:01:23 up 16 min,  0 user,  load average: 1.98, 3.06, 2.39
    # rounds 5
    # load average after: 16:01:27 up 16 min,  0 user,  load average: 1.98, 3.04, 2.38

### Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 1a | getter on a sealed Value: p x / p.x | 16.5 | 3.00 | 0.00 | 4.19 | 0.00 | 0.00 | 3.9 | - | - | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 10a | Dictionary at:put: / Map.put | 274 | 4.03 | 10.0 | 90.6 | 1.01 | 6.00 | 3.0 | 4.0 | 1.7 | 3.03 | 4.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 30.9 | 3.00 | 0.00 | 5.60 | 3.00 | 0.00 | 5.5 | 1.0 | - | -0.00 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 1324 | 6.82 | 202 | 1068 | 4.55 | 200 | 1.2 | 1.5 | 1.0 | 2.27 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 126 | 9.00 | 0.00 | 97.5 | 8.00 | 0.00 | 1.3 | 1.1 | - | 1.00 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 14a | typed Tuple at: / elem(t, 0) | 34.7 | 3.00 | 0.00 | 1.88 | 0.00 | 0.00 | 18.5 | - | - | 3.00 | 0.00 | is_tuple-guarded direct Tuple:dispatch/3 (BT-4294); 'at:' reads an in-range element inline (BT-4400) |

### Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 1a | `getter_dot` | 4.19 | 0.00 | 0.00 |
| 1a | `getter_fun` | 5.56 | 2.00 | 0.00 |
| 1a | `getter_match` | 1.31 | 0.00 | 0.00 |
| 10a | `map_put` | 90.6 | 1.01 | 6.00 |
| 10b | `map_get` | 5.60 | 3.00 | 0.00 |
| 10c | `list_append` | 1068 | 4.55 | 200 |
| 10d | `list_member` | 97.5 | 8.00 | 0.00 |
| 14a | `tuple_elem` | 1.88 | 0.00 | 0.00 |

### Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 3.81 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 3.54 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 20.3 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1a | ex | getter_dot | 7.73 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 9.09 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 4.85 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 278 | 5.03 | 10.0 | 5.03 | 10.0 |
| 10a | ex | map_put | 94.1 | 2.01 | 6.00 | 2.01 | 6.00 |
| 10b | bt | dictAt:d: | 34.7 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10b | ex | map_get | 9.14 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 1328 | 7.82 | 202 | 7.82 | 202 |
| 10c | ex | list_append | 1072 | 5.55 | 200 | 5.55 | 200 |
| 10d | bt | listIncludes:list: | 129 | 10.0 | 0.00 | 10.0 | 0.00 |
| 10d | ex | list_member | 101 | 9.00 | 0.00 | 9.00 | 0.00 |
| 14a | bt | tupleAt:t: | 38.5 | 4.00 | 0.00 | 4.00 | 0.00 |
| 14a | ex | tuple_elem | 5.42 | 1.00 | 0.00 | 1.00 | 0.00 |

## Option 1 prototype (not merged)

The "Beamtalk compiles to" column is the harness's fixed text and still
says `dispatch/3`; the prototype's 10a, 10b, 10d and 14a sites called the
method function.

    # vs-elixir bench
    # otp 28, erts 16.4, elixir 1.20.1, schedulers 4
    # load average: 16:05:38 up 20 min,  0 user,  load average: 1.30, 2.45, 2.32
    # rounds 5
    # load average after: 16:05:41 up 20 min,  0 user,  load average: 1.27, 2.42, 2.31

### Results (median of rounds, net of each side's loop baseline)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 1a | getter on a sealed Value: p x / p.x | 18.4 | 3.00 | 0.00 | 3.63 | 0.00 | 0.00 | 5.1 | - | - | 3.00 | 0.00 | tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3 |
| 10a | Dictionary at:put: / Map.put | 109 | 3.01 | 6.00 | 82.5 | 1.01 | 6.00 | 1.3 | 3.0 | 1.0 | 2.00 | 0.00 | send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put |
| 10b | Dictionary at: / Map.get | 26.4 | 2.00 | 0.00 | 7.46 | 3.00 | 0.00 | 3.5 | 0.7 | - | -1.00 | 0.00 | send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get |
| 10c | List add: (100 elements) / l ++ [x] | 1328 | 6.66 | 202 | 1096 | 4.52 | 200 | 1.2 | 1.5 | 1.0 | 2.14 | 2.00 | send(L, 'add:', [7]) -> List primitive (++) |
| 10d | List includes: (100 elements) / Enum.member? | 107 | 8.00 | 0.00 | 105 | 8.00 | 0.00 | 1.0 | 1.0 | - | 0.00 | 0.00 | send(L, 'includes:', [50]) -> List primitive (lists:member) |
| 14a | typed Tuple at: / elem(t, 0) | 24.4 | 2.00 | 0.00 | 2.17 | 0.00 | 0.00 | 11.3 | - | - | 2.00 | 0.00 | is_tuple-guarded direct Tuple:dispatch/3 (BT-4294); 'at:' reads an in-range element inline (BT-4400) |

### Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 1a | `getter_dot` | 3.63 | 0.00 | 0.00 |
| 1a | `getter_fun` | 5.07 | 2.00 | 0.00 |
| 1a | `getter_match` | 2.47 | 0.00 | 0.00 |
| 10a | `map_put` | 82.5 | 1.01 | 6.00 |
| 10b | `map_get` | 7.46 | 3.00 | 0.00 |
| 10c | `list_append` | 1096 | 4.52 | 200 |
| 10d | `list_member` | 105 | 8.00 | 0.00 |
| 14a | `tuple_elem` | 2.17 | 0.00 | 0.00 |

### Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 2.43 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 3.26 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | bt | getterSealed:p: | 20.8 | 4.00 | 0.00 | 4.00 | 0.00 |
| 1a | ex | getter_dot | 6.89 | 1.00 | 0.00 | 1.00 | 0.00 |
| 1a | ex | getter_fun | 8.34 | 3.00 | 0.00 | 3.00 | 0.00 |
| 1a | ex | getter_match | 5.73 | 1.00 | 0.00 | 1.00 | 0.00 |
| 10a | bt | dictPut:d: | 112 | 4.01 | 6.00 | 4.01 | 6.00 |
| 10a | ex | map_put | 85.7 | 2.01 | 6.00 | 2.01 | 6.00 |
| 10b | bt | dictAt:d: | 28.9 | 3.00 | 0.00 | 3.00 | 0.00 |
| 10b | ex | map_get | 10.7 | 4.00 | 0.00 | 4.00 | 0.00 |
| 10c | bt | listAdd:list: | 1330 | 7.66 | 202 | 7.66 | 202 |
| 10c | ex | list_append | 1099 | 5.52 | 200 | 5.52 | 200 |
| 10d | bt | listIncludes:list: | 110 | 9.00 | 0.00 | 9.00 | 0.00 |
| 10d | ex | list_member | 108 | 9.00 | 0.00 | 9.00 | 0.00 |
| 14a | bt | tupleAt:t: | 26.9 | 3.00 | 0.00 | 3.00 | 0.00 |
| 14a | ex | tuple_elem | 5.43 | 1.00 | 0.00 | 1.00 | 0.00 |
