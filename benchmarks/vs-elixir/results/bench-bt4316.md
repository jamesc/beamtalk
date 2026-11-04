# BT-4316: one `persistent_term` read per static class send

Before and after for the class-side rows. BT-4316 changes the hit path of the
guarded direct call BT-4301 introduced (ADR 0142 §2, amended). A static class
send to a method the class itself defines now compiles to

```erlang
let StaticSelf = case call 'persistent_term':'get'({'bt@vs_elixir_bench@vb_sealed_math', 'class_double:'}, 'false') of
    <'false'> when 'true' ->
      call 'beamtalk_class_shadow_flags':'static_call_target'(
          {'bt@vs_elixir_bench@vb_sealed_math', 'class_double:'},
          'VbSealedMath class', 'VbSealedMath', 'double:', 2)
    <StaticRef> when 'true' -> StaticRef
  end
in case StaticSelf of
  <'false'> when 'true' ->
    let ClassPid = call 'erlang':'whereis'('beamtalk_class_VbSealedMath') in
    call 'beamtalk_object_class':'class_send'(ClassPid, 'double:', [21])
  <_> when 'true' ->
    call 'bt@vs_elixir_bench@vb_sealed_math':'class_double:'(StaticSelf, 21)
end
```

The key `{Module, FunName}` holds the class reference the callee receives as
`ClassSelf`. The runtime installs it only while `direct_call_ok/2` holds, the
dynamic path's in-caller owner names this class, module and function, the
function is exported and the class process is alive. It erases it on every
metadata write that could change one of those answers, and on the class
monitor's `'DOWN'`. The direct call stays in tail position.

Measured with the dev build of the compiler (`target/debug/beamtalk`) and the
dev runtime tree. OTP 28 (erts 16.4), Elixir 1.20.1, 4 schedulers.
`run.sh bench 5 3a,3b,3c,3d,3e,3f`, load average 11 to 16 (a shared machine
running several builds). Reductions and words are deterministic; ns are not
comparable with the before column, which comes from `bench-bt4301.md` (main
@ bc17b93 re-measured the same 3a figure on 2026-11-03).

## Rows (net of the loop baseline)

| Row | Operation | before red | before words | after red | after words | after ns |
|---|---|--:|--:|--:|--:|--:|
| 3a | `class sealed` method, sealed class | 7.03 | 5 | 3.00 | 0 | 41.6 |
| 3b | class method, sealed class, not `class sealed` | 7.03 | 5 | 3.00 | 0 | 42.1 |
| 3c | class method, open class | 7.03 | 5 | 3.00 | 0 | 109 |
| 3d | class method, class in a variable (dynamic send) | 28.1 | 42 | 21.1 | 42 | 684 |
| 3e | inherited class method, open stateless subclass | 32.1 | 42 | 26.1 | 36 | 1231 |
| 3f | class method, class with `classState:` (control) | 204 | 586 | 198 | 582 | 17674 |

Rows 3a-3c meet ADR 0142's ≤ 3 target for row 3a: one reduction on top of
Elixir's `Math.double(21)` (2.00), and no allocation (the class reference is
served from the literal area). BT-4316 does not touch the dynamic path (3d,
3e, 3f); their "before" figures are BT-4301's, and the drop since then comes
from work merged to main in between (BT-4295 and others), not from this
change.

## Profile (`run.sh profile 1 3a`)

Before (BT-4301), one call each per send:

```
1.00  persistent_term:get/2
1.00  beamtalk_class_shadow_flags:direct_call_ok/2
1.00  erlang:function_exported/3
1.00  erlang:whereis/1
1.00  bt@vs_elixir_bench@vb_sealed_math:class_double:/2
```

After:

```
3.0 calls/op to 3 distinct functions
    1.00  bt@vs_elixir_bench@vb_bench:-class_classSealedMethod:/2-repeat/2-0-/3
    1.00  persistent_term:get/2
    1.00  bt@vs_elixir_bench@vb_sealed_math:class_double:/2
```

## Cost moved to the metadata write

Each `persistent_term:erase/1` of a present key scans every process's literal
references, so erasing a class's flag now costs one scan per installed static
call key on top of the class key's own. `beamtalk_class_shadow_flags:invalidate/1`
on a class with k warmed keys (median and minimum of 20 rounds):

| idle processes | static call keys | invalidate/1 median µs | min µs |
|--:|--:|--:|--:|
| 0 | 0 | 15 | 13 |
| 0 | 1 | 50 | 18 |
| 0 | 20 | 134 | 105 |
| 10000 | 0 | 23 | 18 |
| 10000 | 1 | 51 | 16 |
| 10000 | 20 | 65 | 60 |

A class with 20 warmed static call targets pays about 0.1 ms more per
metadata write (reload, `removeSelector:`, extension, runtime class method,
class-variable change), with or without 10k idle processes. That is small
next to a reload's compile and load, so the erase is not batched.
