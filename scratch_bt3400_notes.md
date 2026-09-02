# BT-3400 investigation notes (scratch, to be removed before PR)

Root cause hypothesis (strong evidence, not yet live-reproduced):

`apps/beamtalk_stdlib/test/` has 4 EUnit fixtures that each boot the real
runtime+stdlib in the shared EUnit VM used by `rebar3 eunit --dir=apps/beamtalk_stdlib/test`:

1. beamtalk_test_case_stdlib_tests:bif_fallback_setup/0
   -> beamtalk_test_boot:boot_real_stdlib/1 (SAFE: calls
      application:ensure_all_started(beamtalk_runtime) first)
2. beamtalk_collection_tests:stdlib_setup/0 (UNSAFE ad hoc copy)
3. beamtalk_interface_tests:live_setup/0 (UNSAFE ad hoc copy)
4. beamtalk_test_runner_tests:live_setup/0 (UNSAFE ad hoc copy)

(2)-(4) are near-identical duplicated blocks:
```
case whereis(pg) of undefined -> pg:start_link(); _ -> ok end,
beamtalk_extensions:init(),
case whereis(beamtalk_bootstrap) of
    undefined -> {ok, _} = beamtalk_bootstrap:start_link();
    _ -> ok
end,
beamtalk_stdlib:init(),
```
This NEVER calls `application:ensure_all_started(beamtalk_runtime)`, so
`beamtalk_runtime_app:start/2` (which creates the `beamtalk_protocol_registry`
ETS table via `beamtalk_protocol_registry:init()`, among other tables) never
runs if one of these fixtures happens to execute first in the shared VM.

When that happens:
- beamtalk_stdlib:init() -> load_protocol_modules/0 tries to
  code:ensure_loaded the Printable/JsonRepresentable protocol modules.
  Their on_load calls beamtalk_protocol_registry:register_protocol/1, which
  does `ets:insert(beamtalk_protocol_registry, ...)` on a table that was
  never created -> badarg. This matches the CI log noise exactly
  ("Error in process ... badarg on ets:insert(beamtalk_protocol_registry,...)
  for JsonRepresentable/Printable").
- Later, when the SAFE fixture (bif_fallback_setup) runs and calls
  application:ensure_all_started(beamtalk_runtime), the application
  controller has never seen beamtalk_runtime "started" (the unsafe fixtures
  only called raw module functions, never any application:* API), so it
  genuinely invokes beamtalk_runtime_app:start/2 now — spinning up a SECOND,
  independent supervision tree (second beamtalk_bootstrap, second
  beamtalk_stdlib gen_server, etc.) because beamtalk_bootstrap:start_link/0
  never registers a name, so nothing prevents a second instance.
- The new supervised beamtalk_stdlib:init/1 re-runs
  beamtalk_module_activation:activate_module/2 for every stdlib class
  (including TestCase and its ancestors) a SECOND time.
  try_register_class/2 (beamtalk_module_activation.erl ~L471) calls
  `Module:register_class()` UNCONDITIONALLY on every activation — regardless
  of whether the module was already loaded/registered — so every class gets
  re-registered mid-VM-session, concurrently with (or just before) the
  bif_fallback fixture actually dispatching test methods against those same
  classes. This double/late (re-)initialization of shared class-registry +
  protocol-registry state is what produces the "0 tests" observation:
  whichever EUnit fixture happens to run first decides whether the runtime
  boots once cleanly, or twice messily.

Confirmed via code reading:
- runtime/apps/beamtalk_runtime/src/beamtalk_protocol_registry.erl
  register_protocol/1 does plain `ets:insert(?PROTOCOL_TABLE, ...)` — badarg
  only possible if the named table doesn't exist yet.
- runtime/apps/beamtalk_runtime/src/beamtalk_runtime_app.erl start/2 is the
  ONLY call site of beamtalk_protocol_registry:init/0.
- runtime/apps/beamtalk_test_support/src/beamtalk_test_boot.erl
  boot_real_stdlib/1 correctly calls
  application:ensure_all_started(beamtalk_runtime) FIRST — its moduledoc
  explicitly says "Guarded so it is safe to call from more than one test
  module in the same EUnit VM".
- runtime/apps/beamtalk_stdlib/test/{beamtalk_collection_tests,
  beamtalk_interface_tests,beamtalk_test_runner_tests}.erl all use the
  unsafe ad hoc sequence instead, bypassing application:ensure_all_started.
- runtime/apps/beamtalk_runtime/src/beamtalk_module_activation.erl
  activate_module/2 -> try_register_class/2 (~L471-488) calls
  `Module:register_class()` unconditionally, not gated on "was this the
  first successful load" — so re-running beamtalk_stdlib:init() is not a
  pure no-op even for already-loaded modules.

Fix plan: replace the 3 unsafe ad hoc setups with calls to the existing
beamtalk_test_boot:boot_real_stdlib/1 helper (CLAUDE.md "No duplicate
implementations" + closes the actual race by guaranteeing
application:ensure_all_started(beamtalk_runtime) always runs, and runs
before any fixture's beamtalk_stdlib:init()/on_load activity, regardless of
EUnit run order).

Next: implement, then run test-runtime (stdlib eunit dir) repeatedly (~15-20x)
to confirm the flake is gone, plus a full just test-runtime run.
