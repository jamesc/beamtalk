# BT-4845 after: actor start-up rows 16a-16d

Branch with the registry change (`beamtalk_object_instances:register/2` writes its rows in the caller and casts only the monitor; DOWN cleanup uses the pid index), dev tree runtime (`BEAMTALK=target/debug/beamtalk`), same cloud container (4 cores), 7 rounds. Compare `bench-bt4845-before.md`.

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 16a | async actor start: spawnMonitoredAsync + continueWith: #run + stopWith: + DOWN / spawn_monitor | 57570 | 168 | 254 | 1641 | 10.0 | 17.0 | 35.1 | 16.8 | 15.0 | 158 | 237 | VbRunner spawnMonitoredAsync -> beamtalk_actor_ffi:doSpawnMonitoredAsync (spawn_opt monitor, enter_loop, initialize, handle_continue #run, stopWith:); the caller waits for the DOWN (BT-4845) |
| 16b | as 16a, spawnMonitoredAsyncWith: / spawn_monitor | 59547 | 169 | 254 | 2108 | 10.0 | 17.0 | 28.3 | 16.8 | 14.9 | 159 | 237 | as 16a through spawnMonitoredAsyncWith: |
| 16c | plain Actor spawn + stop / GenServer.start + stop | 58143 | 225 | 352 | 5971 | 73.2 | 83.0 | 9.7 | 3.1 | 4.2 | 152 | 269 | VbCounter spawn (synchronous start, init ack) + stop |
| 16d | 100 async actor starts in flight, then all DOWNs / 100 spawn_monitors | 2251201 | 22599 | 36408 | 259841 | 2333 | 1900 | 8.7 | 9.7 | 19.2 | 20266 | 34508 | as 16a, but 100 actors are started before any DOWN is awaited (one operation is the batch) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 16a | `spawn_monitor_exit` | 1641 | 10.0 | 17.0 |
| 16b | `spawn_monitor_exit` | 2108 | 10.0 | 17.0 |
| 16c | `gen_start_stop` | 5971 | 73.2 | 83.0 |
| 16d | `spawn_monitor_batch` | 259841 | 2333 | 1900 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.27 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.35 | 1.00 | 0.00 | 1.00 | 0.00 |
| 16a | bt | asyncStartRun: | 57571 | 169 | 254 | 52.3 | 53.0 |
| 16a | ex | spawn_monitor_exit | 1643 | 11.0 | 17.0 | 11.0 | 17.0 |
| 16b | bt | asyncStartRunWith: | 59548 | 170 | 254 | 52.3 | 53.0 |
| 16b | ex | spawn_monitor_exit | 2109 | 11.0 | 17.0 | 11.0 | 17.0 |
| 16c | bt | spawnStop: | 58145 | 226 | 352 | 102 | 152 |
| 16c | ex | gen_start_stop | 5973 | 74.2 | 83.0 | 74.2 | 83.0 |
| 16d | bt | asyncStartBatch: | 2251202 | 22600 | 36408 | 5440 | 5594 |
| 16d | ex | spawn_monitor_batch | 259842 | 2334 | 1900 | 2334 | 1900 |
