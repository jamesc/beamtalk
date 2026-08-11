# Core Erlang Text Annotations Spike — `.bt` Source Positions via `-|`

**Issue:** BT-3119
**Date:** 2026-08-09
**Status:** Complete — recommendation: **go**
**Throwaway code:** experiments run outside the repo (scratch `.core` files, not committed); reproduction commands below.

## TL;DR

Core Erlang text annotations (`Expr -| Anno`) survive `core_scan`/`core_parse`
→ `compile:forms(..., [from_core, debug_info])` → BEAM `Line` chunk → runtime
stack traces, with **zero wire-format change** — confirming ADR 0088's status
block ("annotations can be carried via per-leaf metadata or a side-band
channel") without reopening the cerl-ETF question. The catch: the annotation
**shape** must be `[Line, {file, Filename}]` (bare integer first, then a
`{file, Name}` tuple) — not the `[{'file',...},{'line',...}]` shape the issue
sketched, which the OTP compiler frontend does not recognize as a line
annotation at all (it silently fails to attach, no error). Compile-time cost
at worst-case annotation density (every call site + every function head) is
within measurement noise on a 500-function synthetic module. **Recommendation:
proceed** — file follow-up issues for the typed-leaves API addition and the
upstream `.bt` span-propagation work.

## Questions answered

### Q1 — Do `core_scan`/`core_parse` accept `-|` annotations at expression and function positions? **Yes, with a specific shape.**

Tried three annotation shapes on OTP 28 (`erlc` compiler-9.0.6, the version
pinned in `.tool-versions`):

| Shape tried | Parses? | Recognized as line/file by the compiler backend? |
|---|---|---|
| `[{'file',"x.bt"},{'line',12}]` (the issue's sketch) | ✅ parses | ❌ **not recognized** — `sys_core_fold:get_line/1` and `beam_core_to_ssa:line_anno/1` only look for a bare integer or `{Line,Column}` tuple, so this shape is silently inert |
| `[42, {'file',"x.bt"}]` (bare line, then file tuple) | ✅ parses | ✅ recognized — this is the shape `v3_core.erl` itself emits for real `.erl` source |
| `[{42,'x.bt'}]` (mashed tuple) | did not try — first two settled the question | — |

Both expression-level (`( 'atom' -| [Anno] )`) and function-head-level
(`( fun (X) -> Body -| [Anno] )`) annotations parse. `let`-bindings need their
own wrapping parens if annotated: `( let <Y> = E1 in E2 -| [Anno] )`.

**Source of truth for the correct shape**
(`compiler-9.0.6/src/sys_core_fold.erl:2732-2735`, `beam_core_to_ssa.erl:3234-3236`):

```erlang
get_line([Line|_]) when is_integer(Line) -> Line;
get_line([{Line, _Column} | _T]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.
```

i.e. the line is *the first integer (or `{Line,Column}` pair) found in the
list*, and `{file, Name}` is looked up separately
(`beam_core_to_ssa:get_file/1`, `find_loc/3`). `{'line', N}` and `{'file', N}`
as atom-tagged-tuples-for-both is not a recognized shape — it parses (any term
list is valid Core Erlang annotation syntax) but carries no location meaning
to the compiler.

### Q2 — Do annotations survive into `debug_info` / runtime stack traces? **Yes, via the BEAM `Line` chunk — not via the `debug_info` chunk.**

Repro (OTP 28):

```erlang
% t4.core
module 't4' ['boom'/0, 'module_info'/0, 'module_info'/1]
    attributes []
'boom'/0 =
    ( fun () ->
        ( call 'erlang':'error' ('boom_reason') -| [42, {'file',"fake.bt"}] )
    -| [40, {'file',"fake.bt"}] )
'module_info'/0 = fun () -> ( call 'erlang':'get_module_info' ('t4') -| [1, {'file',"fake.bt"}] )
'module_info'/1 = fun (X) -> ( call 'erlang':'get_module_info' ('t4', X) -| [1, {'file',"fake.bt"}] )
end
```

```
$ erlc +debug_info +from_core t4.core
$ erl -noshell -pa . -eval '
    code:load_file(t4),
    try t4:boom() catch _:_:Stack -> io:format("~p~n", [Stack]) end,
    halt().'
[{t4,boom,0,[{file,"fake.bt"},{line,42}]}, ...]
```

The stack frame carries `{file,"fake.bt"},{line,42}` exactly as a normally
Erlang-compiled module would. This comes from the BEAM `"Line"` chunk (present
and populated — verified via `beam_lib:chunks("t4.beam", ["Line"])`, which
contains the literal bytes of `"fake.bt"`), *not* from the `debug_info` chunk.
The `debug_info` chunk itself
(`beam_lib:chunks(_, [debug_info])` → `{debug_info_v1, erl_abstract_code, {[],
Opts}}`) comes back with an **empty** abstract-code list for `from_core`
compiles — unsurprising, since "abstract code" means Erlang parse-tree forms,
which a Core-Erlang-text compile never produces. This has no effect on stack
traces (they read the `Line` chunk directly), but it does mean tools that walk
`debug_info`'s abstract forms specifically (not `beam_lib`/stack traces —
e.g. some `dialyzer`/`xref` modes) will see nothing for Beamtalk modules
either way, cerl-annotated or not. Not a regression versus today; just a
scope note for anyone chasing "full debug_info parity" later.

### Q3 — Typed-leaves API shape and blast radius

Sketch: a new `leaf::annotated` wrapper in
`crates/beamtalk-core/src/codegen/core_erlang/document/leaf.rs`, following the
existing helper pattern (typed constructor → `Document`, no `format!`):

```rust
/// `( Expr -| [Line, {'file', Filename}] )` — attach a `.bt` source position
/// to a Core Erlang node so it survives to the BEAM `Line` chunk and runtime
/// stack traces.
#[must_use]
pub fn annotated(expr: Document<'static>, span: BtSpan) -> Document<'static> {
    docvec![
        "( ", expr,
        " -| [", int_lit(span.line as i64), ", {'file', ", string_lit(&span.file), "}] )"
    ]
}
```

`BtSpan { file: String, line: u32 }` would need to come from the Beamtalk AST;
today source spans are tracked only partially through to codegen call sites
(same caveat ADR 0088 §*Downstream Consumers* already flags for the cerl-wire
path — this is shared upstream work, not duplicated by choosing the text
path).

Blast radius, measured against the current tree: ~27 files in
`crates/beamtalk-core/src/codegen/core_erlang/` already call
`leaf::fname`/`leaf::atom` (function heads, module attrs, `call`/`apply`
targets) and ~25 call sites construct `call `/`apply ` docs directly — these
are the natural `annotated(...)` wrap points if scoping to "function heads +
message-send sites" per the issue. That is a mechanical, incremental,
per-file migration (matches ADR 0089's typed-leaf rollout precedent) — no
flag-day required, since un-annotated nodes remain valid Core Erlang (just
without a `Line`/`file` — same as today).

### Q4 — Compile-time cost

Synthetic benchmark: 500 functions, each `fun(X) -> let Y = X+1 in Y*2`, one
version fully annotated (function head + both call sites + the `let`, 4
annotations/function = 2000 total), one plain. `erlc +from_core`, 3 runs each,
wall-clock (dominated by BEAM VM startup, ~0.5s baseline):

| Variant | Run 1 | Run 2 | Run 3 |
|---|---|---|---|
| Plain (no annotations) | 0.509s | 0.522s | 0.529s |
| Annotated (4/function) | 0.526s | 0.677s | 0.560s |

Delta is within noise at this scale (largest observed gap ~0.15s against a
~0.5s floor dominated by `erl` process startup, not parsing/codegen). No
measurable compile-time regression at annotation densities well above what
"function heads + message-send sites" would produce for realistic Beamtalk
modules (current codegen simulation fixtures are single-digit functions per
module). Not a rigorous benchmark — sufficient to clear the timebox's bar of
"any measurable cost worth worrying about."

## What NOT reopened

Per the issue's scope: ADR 0088 (cerl-ETF wire) stays as-is — ADR 0088's
Alternative 5 ("Text Wire + Annotation Side-Channel") already discusses a
*different, more complex* variant of this idea (a parallel ETF map + post-parse
re-annotation walk) and correctly notes it's brittle. This spike is simpler
than that alternative: annotations ride inline in the text `core_generator`
already emits, no side-channel, no post-parse walk — `core_parse` does the
attaching for free once the shape is right.

## Recommendation: go

1. File a follow-up issue for the typed-leaves `leaf::annotated` API +
   `BtSpan` plumbing (Q3 sketch above) — sized roughly M, since it's additive
   (existing call sites keep working un-annotated) and the ~27/~25 call-site
   inventory bounds the mechanical rollout.
2. File a follow-up issue (or fold into the same one) for the upstream
   AST → codegen span-propagation gap noted in Q3 — this is the same
   "does today only partially" caveat ADR 0088 already flags, now shared by
   both the cerl-wire and text-annotation paths, so whichever lands first
   the upstream work isn't wasted.
3. No ADR update needed to ADR 0088 itself (scope says don't reopen it), but
   its `annotations can be carried via per-leaf metadata` status-block line
   can now cite this spike as the concrete mechanism.

## Reproduction

```bash
# Minimal shape check
cat > t.core <<'EOF'
module 't' ['boom'/0, 'module_info'/0, 'module_info'/1]
    attributes []
'boom'/0 =
    ( fun () ->
        ( call 'erlang':'error' ('boom_reason') -| [42, {'file',"fake.bt"}] )
    -| [40, {'file',"fake.bt"}] )
'module_info'/0 = fun () -> ( call 'erlang':'get_module_info' ('t') -| [1, {'file',"fake.bt"}] )
'module_info'/1 = fun (X) -> ( call 'erlang':'get_module_info' ('t', X) -| [1, {'file',"fake.bt"}] )
end
EOF
erlc +debug_info +from_core t.core
erl -noshell -pa . -eval 'code:load_file(t), try t:boom() catch _:_:S -> io:format("~p~n",[S]) end, halt().'
```
