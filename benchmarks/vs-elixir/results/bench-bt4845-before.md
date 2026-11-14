# BT-4845 before: actor start-up rows 16a-16d

main at 14e99d6, dev tree runtime (`BEAMTALK=target/debug/beamtalk`), cloud container (4 cores), 7 rounds: `run.sh bench 7 16a,16b,16c,16d`. Reductions and words are deterministic; ns is indicative.

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|
| 0 | loop overhead (subtracted) | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | - | - | - | 0.00 | 0.00 | letrec loop, last threaded as a loop argument |
| 16a | async actor start: spawnMonitoredAsync + continueWith: #run + stopWith: + DOWN / spawn_monitor | 56938 | 186 | 297 | 2120 | 10.0 | 17.0 | 26.9 | 18.5 | 17.5 | 176 | 280 | VbRunner spawnMonitoredAsync -> beamtalk_actor_ffi:doSpawnMonitoredAsync (spawn_opt monitor, enter_loop, initialize, handle_continue #run, stopWith:); the caller waits for the DOWN (BT-4845) |
| 16b | as 16a, spawnMonitoredAsyncWith: / spawn_monitor | 72414 | 187 | 296 | 2018 | 10.0 | 17.0 | 35.9 | 18.6 | 17.4 | 177 | 279 | as 16a through spawnMonitoredAsyncWith: |
| 16c | plain Actor spawn + stop / GenServer.start + stop | 65574 | 241 | 400 | 5797 | 73.2 | 83.0 | 11.3 | 3.3 | 4.8 | 168 | 317 | VbCounter spawn (synchronous start, init ack) + stop |
| 16d | 100 async actor starts in flight, then all DOWNs / 100 spawn_monitors | 3403021 | 17241 | 30672 | 256402 | 2162 | 1900 | 13.3 | 8.0 | 16.1 | 15079 | 28772 | as 16a, but 100 actors are started before any DOWN is awaited (one operation is the batch) |

## Elixir alternates (net ns / red / words)

| Row | Elixir | ns | red | words |
|---|---|--:|--:|--:|
| 0 | `baseline` | 0.00 | 0.00 | 0.00 |
| 16a | `spawn_monitor_exit` | 2120 | 10.0 | 17.0 |
| 16b | `spawn_monitor_exit` | 2018 | 10.0 | 17.0 |
| 16c | `gen_start_stop` | 5797 | 73.2 | 83.0 |
| 16d | `spawn_monitor_batch` | 256402 | 2162 | 1900 |

## Raw (not net): baseline and caller-only share

| Row | Side | Fun | ns | red | words | caller red | caller words |
|---|---|---|--:|--:|--:|--:|--:|
| 0 | bt | baseline: | 1.56 | 1.00 | 0.00 | 1.00 | 0.00 |
| 0 | ex | baseline | 1.20 | 1.00 | 0.00 | 1.00 | 0.00 |
| 16a | bt | asyncStartRun: | 56940 | 187 | 297 | 58.8 | 82.0 |
| 16a | ex | spawn_monitor_exit | 2121 | 11.0 | 17.0 | 11.0 | 17.0 |
| 16b | bt | asyncStartRunWith: | 72415 | 188 | 296 | 58.9 | 82.0 |
| 16b | ex | spawn_monitor_exit | 2019 | 11.0 | 17.0 | 11.0 | 17.0 |
| 16c | bt | spawnStop: | 65575 | 242 | 400 | 110 | 185 |
| 16c | ex | gen_start_stop | 5798 | 74.2 | 83.0 | 74.2 | 83.0 |
| 16d | bt | asyncStartBatch: | 3403023 | 17242 | 30672 | 4875 | 8495 |
| 16d | ex | spawn_monitor_batch | 256403 | 2163 | 1900 | 2163 | 1900 |
