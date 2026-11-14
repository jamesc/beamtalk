# BT-4845 after: actor start-up rows 16a-16d

Branch with the registry change (`beamtalk_object_instances:register/2` writes its rows in the caller, bag row first, and casts only the monitor; the monitor and DOWN handlers use the pid index; the index repair re-checks the live bag), dev tree runtime (`BEAMTALK=target/debug/beamtalk`), same cloud container (4 cores), 7 rounds. Compare `bench-bt4845-before.md`.

Note on row 16d: an earlier run of this branch (monitor handler using `ets:match_object` on the class-keyed bag) cost 22,599 reductions per batch, up from 17,241 before. That walk is O(live instances of the class) per start and is gone in this run (16,057). Words per batch are still higher than before (36,565 vs 30,672); I have not traced where those come from.

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 16a | async actor start: spawnMonitoredAsync + continueWith: #run + stopWith: + DOWN / spawn_monitor | 58262 | 168 | 255 | 2130 | 10.0 | 17.0 | 27.4 | 16.7 | 15.0 | 158 | 238 | VbRunner spawnMonitoredAsync -> beamtalk_actor_ffi:doSpawnMonitoredAsync (spawn_opt monitor, enter_loop, initialize, handle_continue #run, stopWith:); the caller waits for the DOWN (BT-4845) |
| 16b | as 16a, spawnMonitoredAsyncWith: / spawn_monitor | 58620 | 168 | 254 | 2430 | 10.0 | 17.0 | 24.1 | 16.7 | 14.9 | 158 | 237 | as 16a through spawnMonitoredAsyncWith: |
| 16c | plain Actor spawn + stop / GenServer.start + stop | 56624 | 222 | 355 | 5739 | 73.2 | 83.0 | 9.9 | 3.0 | 4.3 | 149 | 272 | VbCounter spawn (synchronous start, init ack) + stop |
| 16d | 100 async actor starts in flight, then all DOWNs / 100 spawn_monitors | 1992668 | 16057 | 36565 | 307617 | 2286 | 1900 | 6.5 | 7.0 | 19.2 | 13771 | 34665 | as 16a, but 100 actors are started before any DOWN is awaited (one operation is the batch) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 16a | `spawn_monitor_exit` | 2130 | 10.0 | 17.0 |
| 16b | `spawn_monitor_exit` | 2430 | 10.0 | 17.0 |
| 16c | `gen_start_stop` | 5739 | 73.2 | 83.0 |
| 16d | `spawn_monitor_batch` | 307617 | 2286 | 1900 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.08 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.29 | 1.00 | 0.00 | 1.00 | 0.00 |
| 16a | bt | asyncStartRun: | 58263 | 169 | 255 | 52.3 | 53.0 |
| 16a | ex | spawn_monitor_exit | 2131 | 11.0 | 17.0 | 11.0 | 17.0 |
| 16b | bt | asyncStartRunWith: | 58621 | 169 | 254 | 52.3 | 53.0 |
| 16b | ex | spawn_monitor_exit | 2432 | 11.0 | 17.0 | 11.0 | 17.0 |
| 16c | bt | spawnStop: | 56625 | 223 | 355 | 102 | 153 |
| 16c | ex | gen_start_stop | 5741 | 74.2 | 83.0 | 74.2 | 83.0 |
| 16d | bt | asyncStartBatch: | 1992669 | 16058 | 36565 | 5371 | 5594 |
| 16d | ex | spawn_monitor_batch | 307618 | 2287 | 1900 | 2286 | 1900 |
