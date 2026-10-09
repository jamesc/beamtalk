# Class-variable catcher audit (ADR 0130 §4)

ADR 0130 §4 makes a protected region a transaction for class variables: when an error crosses the region's catch, every class-variable write made inside it is discarded. Compiled `on:do:` does this itself. Erlang code that catches an error around a Beamtalk block and continues must wrap the call in `beamtalk_class_vars:protect/1` (rule in [`erlang-guidelines.md`](erlang-guidelines.md) § Class variables).

BT-3728 audited every `try`/`catch` in `runtime/apps/*/src` against that rule (689 sites: 546 no-block, 98 other-process, 39 re-raises, 3 converted, 3 needing conversion, which were fixed). The result used to be a site table in this file. It is now recorded in the code, next to each site, and checked by a lint (BT-3769). This file explains that lint and the marker vocabulary.

## The lint

`just lint-class-var-catchers` (part of `just lint` and the CI fast-check step) runs `scripts/ci/lint-class-var-catchers.escript`. It parses every `runtime/apps/*/src/*.erl` with `epp_dodger`/`erl_syntax` and looks at each **catching region**: the body of a `try` that has at least one `catch` clause (not its `of` or `after` clauses), and every old-style `catch Expr`. `try ... after` with no `catch` swallows nothing and is ignored.

A region is a **site** when its body applies something it cannot see statically:

| Form | Example |
|---|---|
| A variable in operator position | `Block()`, `Fun(X)`, `Callback(Value)` |
| `apply/2`, `erlang:apply/2` | `apply(Fun, Args)` |
| `apply/3`, `erlang:apply/3` unless module and function are literal atoms | `erlang:apply(Module, FunName, Args)` |
| A remote call whose module or function is not a literal atom | `Module:'__beamtalk_meta'()`, `Mod:Fun(A, B)` |

Funs built inside the region are searched too. A call inside the argument of `beamtalk_class_vars:protect/1` is not a site: that is the conversion.

Every site must carry a marker, a full-line comment inside the enclosing function (or in the comment block directly above it), at or above the `try`/`catch`:

```erlang
%% bt-catcher-audit: <disposition> - <reason>
```

The reason may wrap onto plain comment lines below. A marker governs every site after it in the same function until the next marker.

The lint fails on:

- a site with no marker;
- an unknown disposition, or a marker with no reason;
- a **stale** marker, one that governs no site (the code it described has gone);
- a `converted` marker in a function that neither calls `beamtalk_class_vars:protect/1` or `restore/1` nor takes a `snapshot/0` that the module restores. A converted site therefore cannot silently lose its conversion.

`escript scripts/ci/lint-class-var-catchers.escript --list` prints every site with its disposition. `--self-test` runs the fixtures in `scripts/ci/fixtures/lint-class-var-catchers/`, which prove the lint fails on an unmarked `Block()` under a `catch` and on each bad-marker shape; `just lint-class-var-catchers` runs it first.

## Dispositions

To pick one, decide whether the protected region can synchronously run a Beamtalk block or other Beamtalk code **in the current process**. A `gen_server:call/cast` runs in the other process, not here.

| Disposition | Meaning |
|---|---|
| `converted` | Restores class variables: the call runs under `protect/1`, or the code takes a `snapshot/0` and `restore/1`s it before continuing. A `protect/1` site needs no marker, but marking it means the lint keeps checking it. |
| `not-applicable-no-block` | The applied value is never a Beamtalk block: an Erlang callback or reflection fun, a BIF such as `module_info/1`, a generated `__beamtalk_meta/0` getter, a compiled field-default constructor. |
| `not-applicable-reraises` | Every catch clause re-raises (or the `{error, _}` it returns is re-raised by every caller), so the next boundary restores. |
| `not-applicable-other-process` | The block runs in a spawned or worker process, an actor, or behind a `gen_server` call. Those processes have no home entry for the caller's class. |
| `deliberately-left` | Runs a block and swallows, but restoring would be wrong. Explain why in the reason. None exist today. |

`needs-conversion` was an audit finding, not a marker: convert the site instead.

The "other process" and "no home entry" reasoning rests on one fact: `beamtalk_class_vars:install/2` is called only from `beamtalk_class_dispatch.erl` (`invoke_class_method/7`, `invoke_class_extension/7`), so the home entry exists only inside a class-method invocation in a class process. Everywhere else `snapshot/0` answers `none` and `protect/1` is a pass-through.

## Converted sites

All carry a `converted` marker, so the lint fails if one loses its conversion:

- `beamtalk_class_vars:protect/1` itself.
- `beamtalk_result:'tryDo:'/1` and `beamtalk_test_case:should_raise/2` (`should:raise:`): the block runs under `protect/1`.
- `beamtalk_repl_eval:run_self_eval_module/3` (Inspector `evaluate:`): the eval runs under `protect/1` (BT-3728) and re-raises a `^` throw (BT-3735). Tests: `beamtalk_repl_eval_tests:eval_with_self_discards_class_var_writes`, `eval_with_self_reraises_nlr_and_keeps_class_var_writes`.
- `beamtalk_erlang_proxy:apply_with_coercion/5` and `maybe_retry_badarg/7`: the pre-call snapshot is restored before the `badarg` charlist retry (BT-3728). Test: `beamtalk_erlang_proxy_tests:native_call_badarg_retry_discards_first_attempt_class_var_writes_test`.

## What the lint cannot see, and watch items

The lint checks direct dynamic calls only. A catch around a static call that reaches Beamtalk code further down (for example `beamtalk_message_dispatch:send/3`, or a block handed to `lists:foreach(Block, List)`) is not a site, so the `protect/1` rule still needs a reviewer there. A marker's disposition is also a judgement the lint cannot check. The audit's watch items, none of which needs `protect/1` today:

- `beamtalk_json:encode_with_errors/2` runs `asJson` hooks in-process; every clause re-raises (`not-applicable-reraises`).
- `beamtalk_shape_chain:apply_step/3`: `migrateFromVN:` hook errors become `{error, _}`, but the callers re-raise or run in an actor (`not-applicable-reraises`). If a caller ever swallows inside a class invocation, wrap the `Invoke` fun built in `beamtalk_shape_migration:migrate/4` in `protect/1`.
- `beamtalk_process_navigation` `class_send` (two sites) can call in-process, but only for sealed stateless classes, which have no class variables. Relaxing that direct-call path to classes with `classState:` would make these sites need `protect/1`.
- `beamtalk_test_case` `run_all/1` and `run_single/2` run test code in spawned or worker processes. Called directly from inside a class-method invocation, they would need `protect/1`.
- FFI: the stdlib `.bt` sources call only `beamtalk_*` modules, so no catcher outside `runtime/apps/*/src` is reachable through the FFI.
