# BT-4431: what static call keys cost when a site keeps declining, and at erase time over a hierarchy

BT-4316 (`bench-bt4316.md`) made the hit path of a guarded static class send
one `persistent_term` read of a per-target static call key. Its review
accepted two costs that no row measured:

1. **A static call site that keeps declining.** The key is never installed, so
   every send misses and runs `static_call_target/4`'s unlocked check
   (`whereis_class`, `in_caller_owner/2` with its memo fetch) before it takes
   the `class_send` fallback.
2. **Erase cost over a hierarchy.** `invalidate/1` erases the static call keys
   of the class and of every descendant, and each erase of a present key is a
   global `persistent_term` release.

Measured on main @ 5f7e0d6 with the dev build of the compiler
(`target/debug/beamtalk`) and the dev runtime tree. OTP 28 (erts 16.4),
Elixir 1.20.1, 4 schedulers, an otherwise idle Linux VM (load average 1 to 2).
Reductions and words are deterministic. ns depend on the machine.

## 1. A declining static call site (rows 3g-3j)

The compiler emits a guarded direct call only when its hierarchy shows no
class variables in the chain (`ClassHierarchy::is_direct_call_eligible`).
Without class variables, row 3f compiles to a plain `class_send`. A static
site declines only when the runtime disagrees with what the compiler saw.
Rows 3g and 3i set up the two cases that cause that:

- **3g**: `VbLateStateMath` is compiled with no class state. The harness then
  publishes "declares class variables" for it at boot
  (`beamtalk_class_metadata:put_class_variables('VbLateStateMath', true)`).
  A reload that adds `classState:` without recompiling its callers makes the
  same metadata write. The site declines on the memoised owner verdict.
- **3i**: a cross-file class-side extension (`vb_shadowed_math_ext.bt`)
  shadows `VbShadowedMath`'s compiled `double:`. The site declines on
  `direct_call_ok/2`. (A same-file `X class >> sel` for a selector the class
  defines is folded into the class at compile time and does not shadow it.)

Rows 3h and 3j are the controls: the same class sent from a variable, which
skips the static check and goes straight to `class_send`.
`run.sh bench 7 3c,3d,3f,3g,3h,3i,3j`. Figures are Beamtalk-side and net of
the loop baseline.

| Row | Operation | red | words | ns | calls/op |
|---|---|--:|--:|--:|--:|
| 3c | class method, open class (key hit, reference) | 3.00 | 0 | 45 | 3 |
| 3f | class method, class with `classState:` (plain `class_send`) | 200 | 574 | 13109 | 130 |
| 3g | static site, class variables published after compile | 225 | 606 | 13380 | 146 |
| 3h | as 3g, class in a variable (control) | 204 | 588 | 12566 | 132 |
| 3i | static site, shadowed by a class-side extension | 207 | 527 | 12183 | 135 |
| 3j | as 3i, class in a variable (control) | 175 | 497 | 11282 | 114 |

**What declining costs per send:** 3g − 3h = **+21 red, +18 words**.
3i − 3j = **+32 red, +30 words**. Both fallbacks run in the class process
(`gen_server:call`, about 12 µs here). An extension on the class side routes
`class_send` through the class process too. So declining adds about 2-3% in
time and 10-18% in reductions to a send that already pays a full round trip.

From `run.sh profile 3g,3h,3i,3j`, these are the calls a declining static site
makes that the control does not, per send:

```
3g vs 3h (class variables):
  + static_call_target/4, static_call_ref/4, key_target/1 x2
  + whereis_class/1, registry_name/1, erlang:whereis/1 x2
  + in_caller_owner/2: is_process_alive/1, derived_cache:fetch/2 (memoised decline)
  + persistent_term:get/2 x2, ets:lookup/2, binary_to_atom/2
  - beamtalk_message_dispatch:send/3, is_class_name/1
3i vs 3j (extension):
  as 3g, but in_caller_owner/2 reaches direct_call_ok/2, which re-derives
  (install_direct_flag/2, derive/2) on each send: persistent_term:get/2 +5
```

**Verdict: no follow-up.** A negative cache would save about 20-30
reductions on a path that already costs about 200 reductions and a
`gen_server:call`. It would also add one more fact that every eraser has to
keep correct. Row 3i's re-derivation of the direct-call flag on each send
happens on the dynamic path as well (3j calls `derive/2` once per send). It
belongs to `direct_call_ok/2`'s caching of a `false` answer, not to static
call keys.

## 2. `invalidate/1` over a hierarchy

`run.sh invalidate 20`: a root class with N subclasses, each holding K
installed static call keys (one per class method, installed through
`static_call_target/4`, as the miss path of a static send would install it).
Each round re-warms every key and then times one
`beamtalk_class_shadow_flags:invalidate(Root)`. The bench checks that every
key is gone afterwards. It reports the median and minimum of 20 rounds.

| idle processes | subclasses N | keys per subclass K | keys erased | invalidate/1 median µs | min µs |
|--:|--:|--:|--:|--:|--:|
| 0 | 0 | 0 | 0 | 10 | 8 |
| 0 | 1 | 1 | 1 | 445 | 87 |
| 0 | 1 | 10 | 10 | 2762 | 1406 |
| 0 | 50 | 0 | 0 | 657 | 476 |
| 0 | 50 | 1 | 50 | 28331 | 25249 |
| 0 | 50 | 10 | 500 | 135817 | 89001 |
| 10000 | 0 | 0 | 0 | 15 | 9 |
| 10000 | 1 | 1 | 1 | 170 | 65 |
| 10000 | 1 | 10 | 10 | 980 | 495 |
| 10000 | 50 | 0 | 0 | 1183 | 491 |
| 10000 | 50 | 1 | 50 | 22266 | 15890 |
| 10000 | 50 | 10 | 500 | 103437 | 69912 |

A bare `persistent_term:erase/1` of a present key in the same node (no lock,
no ETS index): median 245 µs, min 133 µs.

- **Erasing costs about 0.2-0.3 ms per key, and almost all of it is the BEAM's
  own erase.** 500 keys take 136 ms against 500 × 245 µs from bare erases.
  The bookkeeping around each erase is small: walking 50 subclasses with no
  keys, each under its tag lock, costs 0.5-1.2 ms.
- **Idle processes do not add to it.** The 10,000-process runs come out the
  same or faster, within noise. The cost of a release here is waiting for the
  schedulers to pass through, not scanning the heaps of idle processes.
- **This is much more than `bench-bt4316.md` reported** (20 keys on one
  class: 0.13 ms on an M-series Mac). The per-erase price depends on the
  machine. On this 4-scheduler VM, a reload or `classState:` change on a base
  class with 50 subclasses and 10 warm static targets each blocks the writer
  for about 0.1 s.

**Verdict: large. Follow-up BT-4442.** Erasure should grow with classes, not
with warmed targets. One option is one key per class whose value maps targets
to the class reference, which keeps one read on the hit path. Another is to
erase only the descendants whose answer the write can change. Whichever design
is chosen has to keep re-warming cheap: replacing a key with `put` also
starts a release.
