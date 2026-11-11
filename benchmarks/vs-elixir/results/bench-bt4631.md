# BT-4631 bench: rows 15a/15b before/after

Median of 5 rounds, dev-tree runtime.

## before (main 68fcce0)

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
| 15a | DynamicSupervisor startChild + terminateChild / start_child + terminate_child | 32585 | 462 | 628 | 11387 | 277 | 317 | 2.9 | 1.7 | 2.0 | 185 | 311 | DynamicSupervisor:startChild -> beamtalk_supervisor:startChild (pool class + maxChildren read from the supervisor dictionary, supervisor:start_child, SupervisionChildAdded/ActorSpawned announcements skipped when the system bus has no subscriber) + terminateChild |
| 15b | capped (maxChildren: 8) DynamicSupervisor startChild + terminateChild / max_children: 8 | 35750 | 508 | 738 | 9542 | 277 | 317 | 3.7 | 1.8 | 2.3 | 231 | 421 | as 15a, with the atomic maxChildren cap: capacity token, monitor and slot release per child |
| Row | Elixir | ns | red | words |
| 15a | `pool_start_terminate` | 11387 | 277 | 317 |
| 15b | `pool_start_terminate` | 9542 | 277 | 317 |
| Row | Side | Fun | ns | red | words | caller red | caller words |
| 15a | bt | poolStartTerminate:pool: | 32587 | 463 | 628 | 253 | 313 |

## after

| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words | ns x | red x | words x | +red | +words | Beamtalk compiles to |
| 15a | DynamicSupervisor startChild + terminateChild / start_child + terminate_child | 28429 | 404 | 576 | 11228 | 277 | 317 | 2.5 | 1.5 | 1.8 | 127 | 259 | DynamicSupervisor:startChild -> beamtalk_supervisor:startChild (pool class + maxChildren read from the supervisor dictionary, supervisor:start_child, SupervisionChildAdded/ActorSpawned announcements skipped when the system bus has no subscriber) + terminateChild |
| 15b | capped (maxChildren: 8) DynamicSupervisor startChild + terminateChild / max_children: 8 | 31835 | 450 | 686 | 10118 | 276 | 317 | 3.1 | 1.6 | 2.2 | 173 | 369 | as 15a, with the atomic maxChildren cap: capacity token, monitor and slot release per child |
| Row | Elixir | ns | red | words |
| 15a | `pool_start_terminate` | 11228 | 277 | 317 |
| 15b | `pool_start_terminate` | 10118 | 276 | 317 |
| Row | Side | Fun | ns | red | words | caller red | caller words |
| 15a | bt | poolStartTerminate:pool: | 28430 | 405 | 576 | 195 | 261 |

Announce path call counts (profile): walk_collect, lookup_superclass 10 -> 0; sets:from_list 2 -> 0; subscribed_classes 4 -> 2.
