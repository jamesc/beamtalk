# Class-variable catcher audit (ADR 0130 Phase 2, BT-3728)

ADR 0130 §4 makes a protected region a transaction for class variables: when an error crosses the region's catch, every class-variable write made inside it is discarded. Compiled `on:do:` does this itself. Erlang code that catches an error around a Beamtalk block and continues must wrap the call in `beamtalk_class_vars:protect/1` (rule in [`erlang-guidelines.md`](erlang-guidelines.md) § Class variables). This document is the audit that checked every Erlang `try`/`catch` in the runtime apps against that rule.

**Snapshot:** line numbers are from `main` at commit `17efeda` (2026-10-07) and drift as files change; use the function name to find a site. Re-run the audit by grepping `try`/`catch` in `runtime/apps/*/src` and classifying each region the way the Method section describes.

## Method

For every `try ... catch/of/after` expression and old-style `catch Expr` in `runtime/apps/*/src/*.erl` (comments, `-doc` text and string literals excluded), decide whether the **protected region** can synchronously run a Beamtalk block or other Beamtalk code **in the current process** (a block value invoked directly, or an in-process class-side call). A `gen_server:call/cast` to another process runs there, not here. Then classify:

| Disposition | Meaning |
|---|---|
| converted | Already wraps the block call in `protect/1` |
| needs-conversion | Region runs a Beamtalk block in-process and the catch swallows the error and continues, so class-variable writes would wrongly survive |
| not-applicable-no-block | The region cannot run a Beamtalk block (BIFs, ETS, file I/O, port I/O, reflection) |
| not-applicable-reraises | The catch always re-raises, so the next boundary restores |
| not-applicable-other-process | The block runs in a spawned or worker process (or a gen_server call), which has no home entry for the caller's class |
| deliberately-left | Runs a block and swallows, but restoring is wrong or impossible (none found) |

The "other process" and "no home entry" conclusions rest on one fact: `beamtalk_class_vars:install/2` is called only from `beamtalk_class_dispatch.erl` (`invoke_class_method/7`, `invoke_class_extension/7`), so the home entry exists only inside a class-method invocation in a class process. Everywhere else `snapshot/0` answers `none` and `protect/1` would be a pass-through.

The classification was done by reading each region and the helpers it calls, one reviewer per slice; the three sites that needed conversion were re-checked by hand. The no-block rows were not independently re-verified.

## Results

689 sites in 5 apps.

| Disposition | Sites |
|---|---|
| not-applicable-no-block | 546 |
| not-applicable-other-process | 98 |
| not-applicable-reraises | 39 |
| converted | 3 (`beamtalk_result:'tryDo:'/1`, `beamtalk_test_case` `should:raise:`, and `protect/1` itself) |
| needs-conversion | 3 sites, 2 distinct causes (both fixed with BT-3728, below) |
| deliberately-left, unclear | 0 |

| Slice | Sites |
|---|---|
| beamtalk_runtime (two halves) | 146 + 101 |
| beamtalk_stdlib | 104 |
| beamtalk_workspace (two halves) | 162 + 75 |
| beamtalk_compiler + beamtalk_test_support | 101 |

### Sites that needed conversion (fixed)

1. `beamtalk_repl_eval:run_self_eval_module/3` (also reached through `beamtalk_inspector:eval_value/2`). Inspector `evaluate:` runs user source in the caller's process, which can be a class-invocation process, and the catch turned an error into `{error, _}`. The eval is now wrapped in `protect/1`. Test: `beamtalk_repl_eval_tests:eval_with_self_discards_class_var_writes`.
2. `beamtalk_erlang_proxy:apply_with_coercion/5`. On the `badarg` charlist retry, writes made by a block run in the failed first attempt survived into the retry. The pre-call snapshot is now restored before the retry. Test: `beamtalk_erlang_proxy_tests:native_call_badarg_retry_discards_first_attempt_class_var_writes_test`.

### Watch items (not converted, with the reason)

- `beamtalk_shape_chain` (`migrateFromVN:` hook errors): swallowed today, but its callers re-raise or run in an actor process with no home entry. Wrap the call in `protect/1` if a caller ever swallows inside a class invocation.
- `beamtalk_test_case` `run_all/1`, `beamtalk_test_runner` `run_single/2`: run in spawned or worker processes today. They would need `protect/1` only if called directly from inside a class-method invocation.
- `beamtalk_process_navigation` `class_send` (two sites): can call in-process, but only for sealed stateless classes, which have no class variables.
- `beamtalk_repl_eval:run_self_eval_module/3` (Inspector `evaluate:`): `protect/1` passes a `$bt_nlr` throw through untouched, but the enclosing catch then turns it into `{error, _}`. That swallow predates this audit, so a `^` inside an evaluated block neither reaches its home method nor rolls back its class-variable writes. Re-raising `$bt_nlr` from that catch would fix it but changes `evaluate:`'s "never raises" contract, so it is tracked separately as [BT-3735](https://linear.app/beamtalk/issue/BT-3735) rather than changed here.
- `beamtalk_compiler_server` `Module:'__beamtalk_meta'()` and `beamtalk_stderr_capture:capture/1`: no class-variable access / always re-raises. Revisit if `__beamtalk_meta/0` ever runs class-method code.
- `beamtalk_workspace_interface_primitives` `revert_method` and `beamtalk_workspace_flush` `reload_renamed_class_source`: only compile, load and edit files; checked at call level, not every transitive helper.
- FFI: the stdlib `.bt` sources call only `beamtalk_*` modules, so no catcher outside `runtime/apps/*/src` is reachable through the FFI.

### Unrelated findings

Not class-variable issues, recorded for follow-up: `beamtalk_ets` reports a block's `badarg` as a stale-table error, and `beamtalk_json` turns a stray `$bt_nlr` throw from an `asJson` hook into `type_error`.

## Site tables

The per-slice tables follow. Each row: `file:line | function/arity | what the region calls | disposition | reason`. The per-slice "needs-conversion" notes below were written before the two fixes above landed; they describe the sites as found.

### beamtalk_runtime/src, part 1 (`beamtalk_actor` .. `beamtalk_module_activation`)

#### Method

Read `beamtalk_class_vars:protect/1`, `snapshot/0`, `restore/1` and ADR 0130 §4/§5/Phase 2. Extracted every `try` (with or without `catch`/`of`/`after`) and every old-style `catch Expr` from the 40 files with a comment/`-doc`/string-literal-stripping script (146 sites; `catch` clauses are counted with their `try`). For each site I read the region and followed called helpers. Key facts used: the home entry (`'$bt_class_vars_home'`) is installed only by `beamtalk_class_dispatch:run_with_class_vars/3` (class methods and class-side extensions, in the class process); `with_snapshot/2` regions are read-only (writes raise), so nothing survives there. Therefore a block can write live class variables only in the class process during an invocation, and `protect/1` is a pass-through (snapshot `none`) in every other process (actors, spawned watchers, futures, callbacks). Instance dispatch (`beamtalk_dispatch`) runs in-process and may run a block carried from a class method; its catches convert to `{error,_}` that every in-tree caller re-raises, and the next boundary (`on:do:`/`protect`/`run_with_class_vars`, which replies the pre-call `ClassVars`) restores. Only 2 sites swallow and continue with in-process Beamtalk code that can hold a block from a live invocation.

#### Site table

| file:line | function/arity | what the region calls | disposition | reason |
|---|---|---|---|---|
| beamtalk_actor.erl:423 | register_spawned/4 | Erlang callback Mod:on_actor_spawned/4 (app env) | not-applicable-no-block | Erlang workspace callback, no Beamtalk block |
| beamtalk_actor.erl:500 | await_initialize/2 | sys:get_state/2 on actor | not-applicable-other-process | call to another process |
| beamtalk_actor.erl:834 | async_send/4 | sync_send(isRemote) -> gen_server:call | not-applicable-other-process | block would run in actor process; rejects future |
| beamtalk_actor.erl:853 | async_send/4 | gen_server:stop on actor | not-applicable-no-block | OTP stop, no block |
| beamtalk_actor.erl:910 | async_send/4 | onExit: Block(Reason) in spawned watcher | not-applicable-other-process | spawned watcher process, no class home entry (snapshot = none) |
| beamtalk_actor.erl:1157 | sync_send/3 | gen_server:stop | not-applicable-no-block | OTP stop; also re-raises other exits |
| beamtalk_actor.erl:1199 | sync_send/3 | onExit: Block(Reason) in spawned watcher | not-applicable-other-process | spawned watcher process, no class home entry (snapshot = none) |
| beamtalk_actor.erl:1256 | sync_send_remote/3 | maybe_span + gen_server:call to actor | not-applicable-other-process | method runs in actor process; catch maps exits and raises |
| beamtalk_actor.erl:1485 | sync_send/4 | maybe_span + gen_server:call to actor (timeout form) | not-applicable-other-process | method runs in actor process; catch maps exits and raises |
| beamtalk_actor.erl:1717 | is_remote/2 | AskNode() = sync_send(node) | not-applicable-other-process | gen_server:call to actor; no block runs here |
| beamtalk_actor.erl:1754 | resolve_remote_registered/2 | erpc:call whereis | not-applicable-no-block | erpc, no block |
| beamtalk_actor.erl:1939 | lookup_class/1 | ets:match on instance registry | not-applicable-no-block | ETS only |
| beamtalk_actor.erl:2334 | handle_cast/2 | dispatch/4 in actor handle_cast | not-applicable-other-process | try/after only (no catch); actor process, not a class invocation process |
| beamtalk_actor.erl:2374 | handle_cast/2 | dispatch/4 in actor handle_cast | not-applicable-other-process | try/after only (no catch); actor process, not a class invocation process |
| beamtalk_actor.erl:2445 | handle_call/3 | dispatch/4 in actor handle_call | not-applicable-other-process | try/after only (no catch); actor process, not a class invocation process |
| beamtalk_actor.erl:2811 | do_announce_actor_lifecycle/2 | beamtalk_announcements:system_announce/2 | not-applicable-no-block | gen_server cast/call to bus, no caller block here |
| beamtalk_actor.erl:3024 | dispatch/4 | beamtalk_dispatch:responds_to/2 | not-applicable-no-block | pure lookup |
| beamtalk_actor.erl:3103 | dispatch_user_method/4 | compiled method Fun/4 in actor | not-applicable-other-process | runs in actor gen_server process (no class home entry); NLR relayed, rest -> {error,..} reply |
| beamtalk_actor.erl:3124 | dispatch_user_method/4 | old-style method Fun/2 in actor | not-applicable-other-process | same as 3103 |
| beamtalk_actor.erl:3168 | call_dnu_handler/5 | DNU handler fun/3 in actor | not-applicable-other-process | actor process, no class home entry |
| beamtalk_actor.erl:3177 | call_dnu_handler/5 | DNU handler fun/2 in actor | not-applicable-other-process | actor process, no class home entry |
| beamtalk_actor.erl:3191 | dispatch_via_hierarchy/4 | beamtalk_dispatch:lookup/5 in actor | not-applicable-other-process | actor process; {error,E,State} becomes reply |
| beamtalk_actor.erl:3510 | register_name/2 | erlang:register/2 | not-applicable-no-block | BIF |
| beamtalk_actor.erl:3578 | unregister_name/1 | erlang:unregister/1 | not-applicable-no-block | BIF |
| beamtalk_actor.erl:3749 | spawn_named_scoped/4 | safe_spawn_named -> gen_server:start_link | not-applicable-other-process | init/1 runs in the new process |
| beamtalk_actor.erl:4075 | unregister/1 | erpc:call unregister on remote node | not-applicable-no-block | erpc, no block |
| beamtalk_actor.erl:4662 | remote_spawn_erpc_call/6 | erpc:call remote spawn | not-applicable-no-block | erpc, remote node |
| beamtalk_actor.erl:4708 | remote_named/3 | erpc:call remote_named_target | not-applicable-no-block | erpc, remote node |
| beamtalk_actor.erl:4761 | remote_all_registered/1 | erpc:call allRegistered | not-applicable-no-block | erpc, remote node |
| beamtalk_actor.erl:5058 | class_self_to_name_and_module/1 | beamtalk_object_class:class_name/module_name_safe (gen_server:call) | not-applicable-no-block | class process calls, no block |
| beamtalk_actor.erl:5190 | pid_class_name_remote_safe/1 | erpc:call pid_class_name | not-applicable-no-block | erpc |
| beamtalk_actor.erl:5426 | stop_global_conflict_loser/1 | gen_server:stop loser in spawned proc | not-applicable-no-block | OTP stop |
| beamtalk_actor.erl:5430 | stop_global_conflict_loser/1 | old-style catch exit(Loser, kill) | not-applicable-no-block | BIF exit/2 |
| beamtalk_actor.erl:5450 | actor_started_at/1 | erpc:call started_at_from_dictionary | not-applicable-no-block | erpc |
| beamtalk_announcements.erl:340 | system_unsubscribe/2 | ets:match_object + unsubscribe/1 (gen_server:call) | not-applicable-no-block | ETS and bus call, no block |
| beamtalk_announcements.erl:1145 | ensure_pg_scope/0 | pg:start_link | not-applicable-no-block | OTP pg |
| beamtalk_announcements.erl:1481 | dispatch_one_veneer/5 | run_handler(Handler, Event) in spawn/1 | not-applicable-other-process | handler block runs in a fresh transient process (no home entry) |
| beamtalk_behaviour_intrinsics.erl:1168 | log_extension_removal/5 | repl_eval:emit_extension_remove_change_entry (erlang:apply) | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1242 | log_local_removal/3 | repl_eval:emit_remove_change_entry | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1267 | capture_class_removal_snapshot/1 | repl_eval:capture_class_removal_snapshot | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1299 | log_class_removal/2 | repl_eval:emit_remove_class_change_entry | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1683 | class_header_span/2 | beamtalk_compiler:resolve_class_span | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1752 | method_token_span/4 | beamtalk_compiler:resolve_method_span | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1781 | current_class_source/1 | beamtalk_workspace_meta:get_class_source | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1795 | class_source_file_for/1 | beamtalk_repl_loader:class_source_file | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:1856 | rewrite_after_identity_move/4 | beamtalk_repl_eval:rewrite_sites (file rewrite + class install) | not-applicable-no-block | Erlang file rewriting; catch only maps no_workspace |
| beamtalk_behaviour_intrinsics.erl:1983 | log_class_rename/4 | repl_eval:emit_rewrite_change_entry | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:2088 | classReload/1 | beamtalk_repl_ops_load:find_project_root/maybe_recompile_native_deps | not-applicable-no-block | Erlang native recompile, no block |
| beamtalk_behaviour_intrinsics.erl:3041 | definition_selector_span_call/5 | beamtalk_compiler:find_definition_selector_spans | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:3103 | selector_send_spans_call/3 | beamtalk_compiler:find_selector_send_spans | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:3189 | log_selector_rename/6 | repl_eval:emit_rewrite_change_entry | not-applicable-no-block | workspace/compiler Erlang call via apply; no Beamtalk block |
| beamtalk_behaviour_intrinsics.erl:3524 | publish_class_removed/2 | pg:get_members | not-applicable-no-block | OTP pg |
| beamtalk_behaviour_intrinsics.erl:3582 | stop_class_actors/1 | gen_server:call(RegistryPid,{kill,Pid}) | not-applicable-no-block | call to registry process |
| beamtalk_class_builder.erl:777 | maybe_stop_builder/1 | gen_server:stop builder | not-applicable-no-block | OTP stop |
| beamtalk_class_builder.erl:796 | notify_class_loaded/1 | Mod:on_class_loaded/1 (app env callback) | not-applicable-no-block | Erlang workspace callback, no Beamtalk block |
| beamtalk_class_dispatch.erl:887 | apply_class_extension_fun/5 | class-side extension fun Fun(Args,ClassSelf) | not-applicable-reraises | -> {error,..}; self-send path (unwrap_self_dispatch_outcome) re-raises, gen_server path replies pre-call ClassVars (run_with_class_vars) |
| beamtalk_class_dispatch.erl:969 | run_with_class_vars/3 | Thunk() = apply_class_method_* | not-applicable-reraises | try/after only (no catch); installs home, replies pre-call ClassVars on error (this IS the invocation boundary) |
| beamtalk_class_dispatch.erl:1064 | apply_class_method_fun/5 | runtime class-method fun apply(Fun,[ClassSelf/Args]) | not-applicable-reraises | -> {error,..}; re-raised by self-send path or reverted by run_with_class_vars |
| beamtalk_class_dispatch.erl:1119 | apply_compiled_class_method/6 | compiled class_<sel> via erlang:apply | not-applicable-reraises | -> {error,..}; re-raised by self-send path or reverted by run_with_class_vars |
| beamtalk_class_dispatch.erl:1491 | class_send_with_recovery/3 | Action(ClassPid) = gen_server call to class | not-applicable-other-process | call to another process; only noproc exits caught, retried |
| beamtalk_class_dispatch.erl:1510 | handle_class_crash_recovery/3 | Action(NewPid) retry gen_server call | not-applicable-other-process | call to another process; re-raises structured error |
| beamtalk_class_dispatch.erl:1701 | class_name_from_pid/1 | list_to_existing_atom | not-applicable-no-block | BIF |
| beamtalk_class_instantiation.erl:71 | handle_spawn/4 | Module:spawn/0,1 (actor start_link) | not-applicable-other-process | init runs in new process; {error,E} returned to caller which raises |
| beamtalk_class_instantiation.erl:215 | handle_new_generic/2 | build_generic_instance (map merge) | not-applicable-no-block | pure map ops |
| beamtalk_class_instantiation.erl:318 | ancestor_compiled_defaults/1 | Module:new/0 (compiled field-default map) | not-applicable-no-block | field-default init, no caller-supplied block; logs and continues |
| beamtalk_class_instantiation.erl:350 | ancestor_builder_defaults/1 | gen_server:call(Pid, field_defaults) | not-applicable-other-process | call to ancestor class process |
| beamtalk_class_instantiation.erl:384 | handle_new_compiled/4 | Module:new/0,1 (compiled value-type constructor) | not-applicable-no-block | field-default init, no caller-supplied block; {error,E,_} re-raised by caller |
| beamtalk_class_instantiation.erl:600 | compute_is_constructible/2 | Module:new/0 probe | not-applicable-no-block | field-default init, no caller-supplied block |
| beamtalk_class_metadata.erl:216 | ensure_table/2 | ets:new | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:271 | insert/5 | ets:insert | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:322 | merge_identity/5 | ets:update_element | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:365 | delete/1 | ets:delete | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:523 | read_meta_detailed/1 | Module:__beamtalk_meta/0 | not-applicable-no-block | compiled metadata thunk; the shared guarded read behind read_meta/1, beamtalk_behaviour_intrinsics:meta_for_module/1 and beamtalk_class_var_abi:compiled_meta/2 (BT-3764) |
| beamtalk_class_metadata.erl:541 | merge_ancestor_map/3 | ReadOwnMapFun(Class) (live __beamtalk_meta/0 or build-time read) | not-applicable-no-block | metadata read, no block |
| beamtalk_class_metadata.erl:656 | match_subclasses/1 | ets:lookup | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:676 | foldl/2 | ets:foldl with internal Erlang Fun | not-applicable-no-block | internal Erlang fold fun, ETS |
| beamtalk_class_metadata.erl:704 | foldl_modules/2 | ets:foldl with internal Erlang Fun | not-applicable-no-block | internal Erlang fold fun, ETS |
| beamtalk_class_metadata.erl:754 | put_class_method_fun/3 | ets:insert | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:775 | put_direct_class_methods/2 | ets:update_element | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:825 | lookup_class_method_fun/2 | ets:lookup | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:845 | delete_class_method_funs/1 | ets:match_delete | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:880 | set_runtime_class_methods/2 | ets:update_element | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:903 | reset_runtime_class_methods/1 | ets:update_element | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:946 | insert_subclass_edge/2 | ets:insert | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:955 | delete_subclass_edge/2 | ets:delete_object | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:972 | field/2 | ets:lookup_element | not-applicable-no-block | ETS only |
| beamtalk_class_metadata.erl:981 | row/1 | ets:lookup | not-applicable-no-block | ETS only |
| beamtalk_class_monitor.erl:138 | init/1 | pg:get_members + class_name_for_pid | not-applicable-no-block | pg/ETS |
| beamtalk_class_monitor.erl:244 | restart_within_budget/3 | beamtalk_class_registry:restart_class/1 | not-applicable-no-block | supervisor start_child; no block |
| beamtalk_class_registry.erl:118 | live_class_entries/0 | all_classes + class_name/module_name_safe | not-applicable-no-block | class process calls |
| beamtalk_class_registry.erl:122 | live_class_entries/0 | class_name/module_name_safe (gen_server:call) | not-applicable-no-block | class process calls |
| beamtalk_class_registry.erl:153 | user_classes/0 | all_classes + class calls | not-applicable-no-block | class process calls |
| beamtalk_class_registry.erl:157 | user_classes/0 | class calls + reflection:source_file_from_module | not-applicable-no-block | class process calls, reflection |
| beamtalk_class_registry.erl:297 | ensure_class_warnings_table/0 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_registry.erl:588 | ensure_pending_errors_table/0 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_registry.erl:655 | maybe_set_heir/1 | ets:setopts heir | not-applicable-no-block | ETS |
| beamtalk_class_registry.erl:794 | ensure_pid_table/0 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_registry.erl:866 | ensure_class_state_table/0 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_registry.erl:988 | ensure_loaded_classes_table/0 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_registry.erl:1129 | ensure_backing_module_index_table/0 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_registry.erl:1278 | restart_class/1 | Module:__beamtalk_meta/0 | not-applicable-no-block | compiled metadata thunk |
| beamtalk_class_registry.erl:1296 | restart_class/1 | ets:match_delete | not-applicable-no-block | ETS |
| beamtalk_class_registry.erl:1355 | get_method_return_type/2 | gen_server:call(Pid,{get_method_return_type,..}) | not-applicable-no-block | call to class process |
| beamtalk_class_registry.erl:1396 | get_class_method_return_type/2 | gen_server:call(Pid,{get_class_method_return_type,..}) | not-applicable-no-block | call to class process |
| beamtalk_class_var_probe.erl:71 | report/6 | at_home/do_report (logging) | not-applicable-no-block | diagnostic probe, no block |
| beamtalk_class_var_probe.erl:82 | do_report/6 | logger/persistent_term/erlang:process_info | not-applicable-no-block | diagnostic probe, no block |
| beamtalk_class_var_abi.erl:69 | collect_abi_refusals/1 | ets:new | not-applicable-no-block | ETS table creation |
| beamtalk_class_var_abi.erl:74 | collect_abi_refusals/1 | Fun() (release-preflight module loads) | not-applicable-reraises | try/after only; deletes the collector table |
| beamtalk_class_var_abi.erl:200 | record_abi_refusal/2 | ets:insert | not-applicable-no-block | ETS (moved from beamtalk_class_vars, BT-3764) |
| beamtalk_class_vars.erl:291 | protect/1 | Fun() under snapshot/restore | converted | this is protect/1 itself |
| beamtalk_class_vars.erl:319 | with_snapshot/2 | Fun() in with_snapshot/2 | not-applicable-reraises | try/after only; RO region, writes raise class_state_read_only so nothing to discard |
| beamtalk_class_vars.erl:357 | tag_to_name/1 | binary_to_existing_atom | not-applicable-no-block | BIF |
| beamtalk_dispatch.erl:485 | invoke_method/6 | beamtalk_object_ops:dispatch/4 (may run Beamtalk, e.g. perform:, displayString) | not-applicable-reraises | -> {error,BtError}; in-tree callers re-raise (message_dispatch, class_dispatch) or reply (actor) |
| beamtalk_dispatch.erl:509 | invoke_method/6 | ModuleName:dispatch/4 (compiled methods, in-process) | not-applicable-reraises | -> {error,BtError}; in-tree callers re-raise (message_dispatch, class_dispatch) or reply (actor) |
| beamtalk_dispatch.erl:595 | invoke_extension/6 | extension fun in-process | not-applicable-reraises | -> {error,..}; NLR and script exit re-raised, callers re-raise |
| beamtalk_dispatch.erl:667 | check_extension/2 | beamtalk_extensions:lookup (ETS) | not-applicable-no-block | ETS only |
| beamtalk_erlang_proxy.erl:243 | apply_with_coercion/5 | erlang:apply(Module,Fun,Args): FFI call; Args may hold Beamtalk blocks run in-process | converted (BT-3728) | every class re-raises except badarg+coercible binaries; the pre-call snapshot is now restored before the charlist retry (maybe_retry_badarg/7) |
| beamtalk_erlang_proxy.erl:290 | maybe_retry_badarg/7 | retry erlang:apply with coerced args (after restoring the pre-call snapshot) | not-applicable-reraises | classify_ffi_exception always raises (badarg terminal) |
| beamtalk_erlang_proxy.erl:419 | selector_to_function/1 | list_to_existing_atom | not-applicable-no-block | BIF |
| beamtalk_erlang_proxy.erl:437 | get_exports/1 | Module:module_info(exports) | not-applicable-no-block | BIF |
| beamtalk_error.erl:181 | format_safe/1 | format/1 | not-applicable-no-block | pure formatting |
| beamtalk_error.erl:462 | maybe_enrich_dnu_hint/1 | beamtalk_dispatch:responds_to/2 | not-applicable-no-block | pure lookup |
| beamtalk_extensions.erl:416 | purge_class/1 | unregister/2 + ets:match_delete | not-applicable-no-block | ETS only |
| beamtalk_extensions.erl:573 | getSource/2 | ets:lookup | not-applicable-no-block | ETS only |
| beamtalk_extensions.erl:599 | listAllWithSource/0 | ets:foldl | not-applicable-no-block | ETS only |
| beamtalk_extensions.erl:716 | safe_xref/1 | Fun() = beamtalk_xref gen_server call | not-applicable-other-process | call to xref process; no block |
| beamtalk_future.erl:406 | execute_callback/2 | Callback(Value) in spawn/1 | not-applicable-other-process | callback runs in a fresh process (no home entry) |
| beamtalk_hot_reload.erl:242 | seed_internal_keys/4 | Module:init(#{__skip_initialize__ => true}) | not-applicable-no-block | default-state init, no caller block |
| beamtalk_hot_reload.erl:293 | try_change_code/3 | sys:suspend/change_code | not-applicable-other-process | sys calls to actor process |
| beamtalk_hot_reload.erl:297 | try_change_code/3 | sys:resume | not-applicable-other-process | sys call to actor process |
| beamtalk_inspector.erl:1054 | eval_value/2 | beamtalk_repl_eval:eval_with_self/2: compiles and runs user source in-process with self bound to inspected value (may be a block from a live invocation) | converted (BT-3728) | the swallow is in repl_eval:run_self_eval_module/3, which now wraps the eval in protect/1; this catch only sees internal failures |
| beamtalk_inspector.erl:1254 | value_string/1 | beamtalk_primitive:print_string/1 (may dispatch a user printString method in-process) | not-applicable-no-block | instance-side printString only, no caller block; falls back to ~p; class-var writes from it would be abroad (raise) |
| beamtalk_inspector.erl:1273 | value_string/2 | deep_coerce_foreign/1 | not-applicable-no-block | pure term walk |
| beamtalk_json_formatter.erl:29 | format/2 | format_json (logger formatter) | not-applicable-no-block | log formatting |
| beamtalk_json_formatter.erl:105 | format_msg_fallback/1 | io_lib:format | not-applicable-no-block | log formatting |
| beamtalk_json_formatter.erl:257 | try_format_bt_error/1 | extract/format error | not-applicable-no-block | log formatting |
| beamtalk_json_formatter.erl:388 | format_extra_value/2 | stack frame formatting | not-applicable-no-block | log formatting |
| beamtalk_logging_config.erl:165 | flush_transcript/0 | old-style catch logger_std_h:filesync/1 | not-applicable-no-block | OTP logger |
| beamtalk_logging_config.erl:704 | ensure_table/0 | ets:new | not-applicable-no-block | ETS |
| beamtalk_message_dispatch.erl:99 | send/3 | gen_server:stop(SupPid) | not-applicable-reraises | translated to structured error and re-raised |
| beamtalk_message_dispatch.erl:252 | send_number_coercion/4 | send(Right,Sel,Args): arbitrary in-process send | not-applicable-reraises | catch only matches DNU, re-raises with hint; other errors propagate |
| beamtalk_message_dispatch.erl:318 | with_known_class/3 | Fun() sync/cast send | not-applicable-reraises | try/after only (no catch) |
| beamtalk_module_activation.erl:298 | activate_module/2 | Callback({Module,Path}) (on_activate opt) | not-applicable-no-block | Erlang loader callback (typed fun, test-only users); not a Beamtalk block |
| beamtalk_module_activation.erl:415 | extract_source_path/1 | get_module_info | not-applicable-no-block | BIF |
| beamtalk_module_activation.erl:434 | extract_class_names/1 | get_module_info | not-applicable-no-block | BIF |
| beamtalk_module_activation.erl:517 | try_register_class/2 | Module:register_class/0 (compiled registration) | not-applicable-no-block | class registration via calls, no block |
| beamtalk_module_activation.erl:775 | safe_module_exports/1 | get_module_info | not-applicable-no-block | BIF |

#### NEEDS-CONVERSION and unclear sites

1. **beamtalk_erlang_proxy.erl:243 `apply_with_coercion/5`** (NEEDS-CONVERSION, low priority). `erlang:apply(Module, Fun, Args)` is the Erlang FFI call; Args may include Beamtalk blocks (e.g. `lists:foreach`) that run in the calling process (which may be a class process at home). `classify_ffi_exception` re-raises every class except `error:badarg` where binary args are coercible: there the first attempt's error is swallowed and the call is retried (`maybe_retry_badarg/6`), so class-var writes made by a block before the BIF raised raw `badarg` survive into the retry. Fix: take `beamtalk_class_vars:snapshot()` before the first apply and `restore/1` it at the top of `maybe_retry_badarg/6` (cheaper than wrapping every FFI call in `protect/1`; wrapping `erlang:apply` in `protect/1` is the simpler equivalent). Test shape (EUnit, in a class-invocation context built with `install/2`): call `apply_with_coercion` with a module/function fun that runs a fun which `put`s a class var and then raises raw `badarg` (the BIF-level shape, with a binary arg so retry is taken); assert the write is absent after the retry; assert a write made before the call is kept; assert NLR throws pass through. Honest caveat: the badarg must come from the BIF itself after running the block, so this is exotic.
2. **beamtalk_inspector.erl:1054 `eval_value/2`** (NEEDS-CONVERSION, low priority). `beamtalk_repl_eval:eval_with_self/2` compiles and runs user source in-process with `self` bound to the inspected value; if that value is a block created in a live class invocation, `evaluate: 'self value'` writes live class variables and a raise is swallowed to `{error,_}`. The first swallow is in `beamtalk_repl_eval:run_self_eval_module/3` (beamtalk_workspace, outside this file set), so a `protect/1` at the inspector catch alone would never see the error. Fix: wrap `apply(ModuleName, eval, [Bindings])` in `beamtalk_class_vars:protect/1` inside `run_self_eval_module` (workspace calls runtime, dependency direction is fine); the inspector catch then needs no change. Test shape: EUnit with a home entry installed; `eval_with_self(BlockWritingClassVar, <<"self value. 1/0">>)` returns `{error,_}` and the write is discarded; plus a BUnit test `Inspector on: [Counter bump. 1/0] evaluate: 'self value'` inside a class method of `Counter`, asserting the count is unchanged.

No site is classified `unclear` or `deliberately-left-candidate`.

#### Notes on borderline not-applicable calls (for reviewer)

- `beamtalk_class_dispatch.erl:887/1064/1119` swallow to `{error,..}` but are the invocation boundary: the self-send adapter `unwrap_self_dispatch_outcome/3` re-raises, and the gen_server entry replies with the pre-call `ClassVars`. NLR outcomes are deliberately kept (ADR 0110).
- `beamtalk_class_instantiation.erl:318/600` and `beamtalk_hot_reload.erl:242` run compiled field-default init (`Module:new/0`, `init/1`) in-process and swallow; no caller-supplied block, and instance-side code cannot write class variables at home, so classified no-block.
- `beamtalk_inspector.erl:1254` dispatches a user `printString` (instance-side) in-process and falls back to `~p`; no block from a live invocation is reachable.
- Actor-process catches (beamtalk_actor.erl:3103/3124/3168/3177/3191) run user methods in the actor gen_server, which never has a home entry.
- `beamtalk_module_activation.erl:298` `on_activate` is an Erlang callback option (only tests pass one), not a Beamtalk block.

### beamtalk_runtime/src, part 2 (`beamtalk_module_name` .. `beamtalk_xref`)

#### 1. Method

Read `beamtalk_class_vars:protect/1`, `snapshot/0`, `restore/1`. Grepped every `try`, `catch` and `after` token in the 40 assigned files (`ls | sort | tail -n +41`), discarded hits in comments, `-doc`/`-moduledoc` text, strings and `receive ... after`, then read each site and the helpers it calls. Where a region calls a fun or another module, I followed it: `beamtalk_announcements:system_announce/2` (handlers run in a spawned transient process via `dispatch_one_veneer/5`, so they are never in-process), `beamtalk_class_dispatch:class_send/3` (gen_server call to the class process, or a direct call for sealed stateless classes with `ClassSelf = nil`, which have no class vars), `beamtalk_object_class:local_call/3` (wraps in `with_snapshot/2`), `beamtalk_shape_migration:invoke_hook/3`, `beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1`, `classCanUnderstandFromName/2`, and the protocol-registry conformance helpers. 101 sites in total. None of them already uses `protect/1`, and none needs it. Eighty-three have no block in the region. The other 18 are classified individually below.

Class-var writes happen only lexically inside class methods, which run in a class-process invocation (home key present) or a `with_snapshot` read-only region (writes raise). So a region matters only if it runs Beamtalk class-method code in the process that holds a live home map. No site in these files does that and then swallows the error.

#### 2. Site table

Dispositions: NB = not-applicable-no-block, RR = not-applicable-reraises, OP = not-applicable-other-process.

| file:line | function/arity | what the region calls | disposition | reason |
|---|---|---|---|---|
| beamtalk_module_name.erl:176 | to_qualified_module_atom/2 | list_to_existing_atom | NB | atom lookup |
| beamtalk_module_name.erl:184 | to_atom/1 | list_to_existing_atom | NB | atom lookup |
| beamtalk_module_name.erl:226 | snake_to_class/1 | list_to_existing_atom | NB | atom lookup |
| beamtalk_native_docs.erl:116 | read_docs_from_beam/1 | binary_to_term | NB | term decode |
| beamtalk_native_docs.erl:125 | read_docs_from_beam/1 (nested) | binary_to_term | NB | term decode |
| beamtalk_node.erl:345 | remote_introspect/4 | erpc:call to peer node | NB | remote introspection MFA, no block |
| beamtalk_node.erl:416 | remote_shape_manifest/1 | erpc:call beamtalk_release:shape_manifest | NB | remote, pure meta read |
| beamtalk_node_monitor.erl:232 | announce/2 | beamtalk_announcements:system_announce | OP | handlers run in spawned process (`dispatch_one_veneer/5`) |
| beamtalk_node_monitor.erl:287 | check_shape_skew_on_connect/1 | shape_manifest, erpc, announce_if_skewed | NB | pure meta plus announce; handlers spawned |
| beamtalk_node_monitor.erl:328 | check_shape_skew_on_reload/2 | shape_manifest, per-peer check | NB | same as :287 |
| beamtalk_node_monitor.erl:359 | check_one_peer_class/3 | erpc:call class_shape_entry, announce | NB | remote pure meta |
| beamtalk_object_class.erl:631 | probe_local_method/4 | `Module:has_method/1`-style generated probe | NB | compiled selector-table probe, no user code |
| beamtalk_object_class.erl:1022 | announce_class_lifecycle/2 | system_announce | OP | handlers spawned |
| beamtalk_object_class.erl:1066 | safe_xref/1 | gen_server:call to beamtalk_xref (Fun is put_method) | NB | xref server call |
| beamtalk_object_class.erl:1174 | handle_call/3 (rename clause) | `catch erlang:register/2` | NB | BIF only |
| beamtalk_object_class.erl:1388 | handle_call/3 (update_class) | `Module:'__beamtalk_meta'()` | NB | generated pure meta |
| beamtalk_object_class.erl:1621 | terminate/2 | beamtalk_class_metadata:delete | NB | ETS |
| beamtalk_object_class.erl:1629 | terminate/2 | ets:delete | NB | ETS |
| beamtalk_object_class.erl:1635 | terminate/2 | pg:leave | NB | pg |
| beamtalk_object_class.erl:1645 | terminate/2 | forget_loaded_class | NB | ETS |
| beamtalk_object_class.erl:1654 | terminate/2 | forget_backing_module_entries | NB | ETS |
| beamtalk_object_class.erl:1666 | terminate/2 | forget_class_state_snapshot | NB | ETS |
| beamtalk_object_class.erl:1697 | notify_compiler_server_register/2 | beamtalk_compiler_server:register_class | NB | cast to compiler server |
| beamtalk_object_class.erl:1814 | dispatch_class_method/5 | handle_class_method_call (runs the class method) | RR | `try ... of ... after`, no catch clause; the error propagates (only restores dict keys) |
| beamtalk_object_class.erl:1963 | set_group_leader/1 | group_leader/2 | NB | BIF |
| beamtalk_object_class.erl:2040 | find_inherited_class_method/2 | gen_server:call to super class pid | NB | metadata lookup |
| beamtalk_object_class.erl:2088 | has_class_new_in_chain/3 | module_name(SuperPid), recursion | NB | metadata walk |
| beamtalk_object_class.erl:2114 | read_meta/1 | `Module:'__beamtalk_meta'()` | NB | generated pure meta |
| beamtalk_object_watch.erl:160 | is_watched/1 | ets:member | NB | ETS |
| beamtalk_object_watch.erl:188 | watched_pids/0 | ets:tab2list | NB | ETS |
| beamtalk_object_watch.erl:401 | announce_state_changed/3 | system_announce | OP | handlers spawned |
| beamtalk_package.erl:170 | package_name/1 | module_name_safe(Pid) | NB | process-dictionary read |
| beamtalk_package.erl:315 | refresh_app_classes/1 | binary_to_existing_atom | NB | atom lookup |
| beamtalk_primitive.erl:394 | process_label/1 | beamtalk_class_registry:inherits_from | NB | registry/ETS |
| beamtalk_primitive.erl:489 | is_pid_alive_safe/1 | is_process_alive | NB | BIF |
| beamtalk_primitive.erl:1119 | class_name_from_tag/1 | binary_to_existing_atom | NB | atom lookup |
| beamtalk_process_navigation.erl:350 | remote_default_snapshot/2 | erpc:call remote_snapshot_target | NB | remote, no block |
| beamtalk_process_navigation.erl:439 | status/1 | sys:get_status | NB | OTP sys call |
| beamtalk_process_navigation.erl:478 | guarded_state/2 | sys:get_state | NB | OTP sys call |
| beamtalk_process_navigation.erl:797 | safe_children_pids/2 | lists:filtermap(Filter, which_children) | NB | Filter funs are module-internal |
| beamtalk_process_navigation.erl:1100 | parse_pid/1 | list_to_pid | NB | BIF |
| beamtalk_process_navigation.erl:1328 | supervisor_strategy/1 | `class_send(ClassPid, strategy, [])` | OP | gen_server call to class process (runs there); the sealed stateless direct-call path has `ClassSelf = nil` and no class vars; no block is passed |
| beamtalk_process_navigation.erl:1345 | supervisor_restart_intensity/1 | class_send maxRestarts/restartWindow | OP | same as :1328 |
| beamtalk_process_navigation.erl:1416 | with_live_class_entries_cache/1 | Fun() (internal walk) | RR | `try ... after` only, no catch clause |
| beamtalk_process_navigation.erl:1451 | safe_child_counts/1 | gen_server:call count_children | NB | OTP supervisor call |
| beamtalk_process_navigation.erl:1475 | safe_which_children/1 | gen_server:call which_children | NB | OTP supervisor call |
| beamtalk_protocol_object.erl:81 | protocol_name_from_class_self/1 | binary_to_existing_atom | NB | atom lookup; the catch re-raises |
| beamtalk_protocol_registry.erl:415 | notify_compiler_server/2 | compiler_server:register_protocol | NB | compiler server |
| beamtalk_protocol_registry.erl:441 | notify_compiler_server_removed/1 | gen_server:cast | NB | cast |
| beamtalk_protocol_registry.erl:507 | compute_conforms_to/2 | classCanUnderstandFromName, class_has_class_method | NB | reflection via has_method/1 and metadata, no user code |
| beamtalk_protocol_registry.erl:551 | cache_lookup/1 | ets | NB | ETS |
| beamtalk_protocol_registry.erl:582 | cache_store/3 | ets | NB | ETS |
| beamtalk_protocol_registry.erl:606 | current_generation/0 | ets:update_counter | NB | ETS |
| beamtalk_protocol_registry.erl:625 | invalidate_conforms_cache/0 | ets:update_counter | NB | ETS |
| beamtalk_protocol_registry.erl:714 | conforming_classes/1 | live_class_entries, conforms_to | NB | reflection only |
| beamtalk_protocol_registry.erl:794 | register_uses/2 | ets | NB | ETS |
| beamtalk_protocol_registry.erl:822 | unregister_uses/1 | ets | NB | ETS |
| beamtalk_protocol_registry.erl:901 | resolve_class_object_safe/1 | resolve_class_object | NB | gen_server call |
| beamtalk_protocol_registry.erl:1013 | class_has_class_method_in_chain/2 | local_class_methods, superclass | NB | gen_server calls |
| beamtalk_protocol_registry.erl:1037 | check_class_extension/2 | beamtalk_extensions:has | NB | ETS |
| beamtalk_reflection.erl:138 | source_file_from_module/1 | erlang:get_module_info | NB | BIF |
| beamtalk_release.erl:168 | read_provenance/2 | json:decode | NB | JSON parse |
| beamtalk_release_shapes.erl:139 | extract_shapes/2 | activate_modules (loads modules), build_shapes_map | RR | `try ... after` only (ets:delete); the build-time extractor owns its process; no block |
| beamtalk_repl_actors.erl:126 | try_register_actor/4 | gen_server:call registry | NB | registry call |
| beamtalk_repl_actors.erl:148 | run_spawn_hook/2 | `Mod:Fun(ActorPid, ClassName)` workspace Erlang hook | NB | Erlang hook, not a Beamtalk block |
| beamtalk_repl_actors.erl:204 | object_at/1 | list_to_pid, registry | NB | BIF / registry |
| beamtalk_repl_actors.erl:430 | resolve_module/1 | whereis_class, module_name_safe | NB | registry |
| beamtalk_runtime_api.erl:222 | remove_class_from_system/1 | classRemoveFromSystemByName | NB | removal runs capability check, class stop, ETS purges; no block |
| beamtalk_script_harness.erl:55 | dispatch/3 | class_send entry method | RR | catch ends in `halt/stop_node`, never continues; the entry method runs in the class process or via direct call |
| beamtalk_script_harness.erl:77 | dispatch/3 (`catch io:put_chars`) | io:put_chars | NB | io |
| beamtalk_script_harness.erl:172 | flush_loggers/0 (`catch logger_std_h:filesync`) | logger | NB | logger |
| beamtalk_shape_chain.erl:91 | apply_step/3 | `Invoke` = invoke_hook, which calls `local_call/3` for `migrateFromVN:` (Beamtalk code, in-process) | RR | see section 3 (borderline): swallows into `{error,_}`, but the callers re-raise or run in an actor process with no home map |
| beamtalk_shape_migration.erl:236 | warn_migrations_outside_table/2 | local_class_methods | NB | gen_server call |
| beamtalk_shape_migration.erl:466 | safe_actor_init_defaults/2 | `Module:init(#{'__skip_initialize__'=>true})` (field defaults) | OP | actor init/1 is not a class-method region; class vars are unreachable from field defaults, and class self-sends are guarded |
| beamtalk_shape_migration.erl:972 | read_meta/1 | `Module:'__beamtalk_meta'()` | NB | generated pure meta |
| beamtalk_stdlib_test.erl:97 | run_one/2 (value) | `EvalMod:eval(Bindings)` | OP | test-harness (EUnit) process, not a class-invocation process; no home map |
| beamtalk_stdlib_test.erl:111 | run_one/2 (value_wildcard) | EvalMod:eval | OP | same |
| beamtalk_stdlib_test.erl:125 | run_one/2 (value_any) | EvalMod:eval | OP | same |
| beamtalk_stdlib_test.erl:135 | run_one/2 (error) | EvalMod:eval | OP | same |
| beamtalk_stdlib_test.erl:239 | format_result/1 | json:encode, term_to_json | NB | pure formatting |
| beamtalk_supervisor.erl:335 | terminateChild/2 | supervisor:terminate_child | NB | OTP call; the child dies elsewhere |
| beamtalk_supervisor.erl:954 | start_child_via_class_method/4 | factory class method in the OTP supervisor process (read-only snapshot, `call_class_method_direct`) | RR | `try ... after` only (erase keys); the error propagates and the supervisor start fails |
| beamtalk_supervisor.erl:1294 | announce_supervision/2 | system_announce | OP | handlers spawned |
| beamtalk_supervisor.erl:1329 | with_live_supervisor/3 | supervisor:* via Fun | OP | all seven callers pass supervisor:* closures; the catch only matches noproc exits; child code runs in the supervisor process |
| beamtalk_supervisor.erl:1372 | ensure_root_table/0 | ets:new | NB | ETS |
| beamtalk_trace_store.erl:142 | is_enabled/0 | persistent_term:get | NB | persistent_term |
| beamtalk_trace_store.erl:297 | max_events/0 | persistent_term:get | NB | persistent_term |
| beamtalk_trace_store.erl:315 | telemetry_attached/0 | telemetry:list_handlers | NB | telemetry |
| beamtalk_trace_store.erl:606 | ensure_counters/0 | persistent_term:get | NB | persistent_term |
| beamtalk_trace_store.erl:620 | ensure_counters/0 | persistent_term:get | NB | persistent_term |
| beamtalk_trace_store.erl:630 | init_persistent_terms/0 | persistent_term:get | NB | persistent_term |
| beamtalk_trace_store.erl:636 | init_persistent_terms/0 | persistent_term:get | NB | persistent_term |
| beamtalk_trace_store.erl:678 | allocate_counter_slot/1 | gen_server:call grow_counters, atomics, ets | NB | trace store |
| beamtalk_trace_store.erl:798 | detach_telemetry_handlers/0 | telemetry:detach | NB | telemetry |
| beamtalk_trace_store.erl:1295 | lookup_class_for_pid/1 | ets:lookup | NB | ETS |
| beamtalk_trace_store.erl:1308 | lookup_class_from_registry/1 | ets:match | NB | ETS |
| beamtalk_tracing.erl:348 | call_trace_store/1 | Fun = beamtalk_trace_store:* calls | NB | trace-store gen_server calls |
| beamtalk_tracing.erl:362 | call_trace_store_default/2 | Fun = beamtalk_trace_store:* queries | NB | trace-store gen_server calls |
| beamtalk_xref.erl:497 | send_hit_to_entry/1 | binary_to_existing_atom | NB | atom lookup |
| beamtalk_xref.erl:538 | target_module_atom/1 | binary_to_existing_atom | NB | atom lookup |
| beamtalk_xref.erl:1383 | class_defines_methods/1 | object_class:methods/local_class_methods | NB | gen_server calls |

Counts (101 sites): converted 0, NEEDS-CONVERSION 0, NB 83, RR 6 (object_class:1814, process_navigation:1416, release_shapes:139, script_harness:55, shape_chain:91, supervisor:954), OP 12, deliberately-left-candidate 0, unclear 0.

Files in scope with no try/catch sites: beamtalk_object_ops, beamtalk_object_printer, beamtalk_object_instances, beamtalk_opaque_ops, beamtalk_pid, beamtalk_reactive_subprocess_sup, beamtalk_runtime_app, beamtalk_runtime_sup, beamtalk_stack_frame, beamtalk_stdlib, beamtalk_subprocess_sup, beamtalk_tagged_map, beamtalk_test_native_counter, beamtalk_text, beamtalk_version, beamtalk_wire.

#### 3. NEEDS-CONVERSION, unclear and borderline sites

NEEDS-CONVERSION: none. Unclear: none. Deliberately-left: none.

Borderline, recorded for the reviewer (no conversion proposed):

- **beamtalk_shape_chain.erl:91 `apply_step/3` (classified RR).**
  - What it does: it runs `migrateFromVN:` hooks in-process through `invoke_hook` and `local_call/3` (which uses `with_snapshot`). It turns any exception into `{error, {Class, Reason}}`.
  - Why it is not a leak today: `with_snapshot` makes class-var writes raise `class_state_read_only` when no home key is present. The two callers of `beamtalk_shape_migration:migrate/4` are `beamtalk_hot_reload:migrate_state/3`, which runs inside the actor's gen_server `code_change` (no home map), and `beamtalk_behaviour_intrinsics:classMigrateShapeFrom/3`, which re-raises via `beamtalk_error:raise`. If a user class method calls `classMigrateShapeFrom`, the live home map is in place and an earlier step's writes would persist until the enclosing compiled `on:do:` or outer boundary restores. That is the correct outer behaviour.
  - If a future caller swallows the `{error,_}` inside a class invocation, wrap the `Invoke` fun built in `beamtalk_shape_migration:migrate/4` as `fun(S, D) -> beamtalk_class_vars:protect(fun() -> invoke_hook(Class, S, D) end) end`. Do not add a dependency from the leaf `beamtalk_shape_chain` on `beamtalk_class_vars`.
  - Test shape: an EUnit test with a class method that writes a class var, then calls `classMigrateShapeFrom` on a class whose `migrateFromV1:` writes a class var and raises. Assert the outer class-var value is unchanged after the error is caught by a compiled `on:do:`.
- **beamtalk_process_navigation.erl:1328 and :1345 (classified OP).** `class_send(strategy | maxRestarts | restartWindow)` can take the ADR 0129 Phase 0b direct-call path, which runs the user method in-process. That path is only taken for sealed, stateless classes (`ClassSelf = nil`, no class vars), so there is nothing to restore. If the direct-call eligibility is ever relaxed to classes with `classState:`, these two sites would need `protect/1`.

### beamtalk_stdlib/src

#### Method

Grepped every `try`/`catch`/`after` token in all 41 `src/*.erl` files and dropped comments, `-doc` text and string literals. Receive-`after` timeouts (`beamtalk_parallel`, `beamtalk_collection`, `beamtalk_timer`) are not `try` and are excluded. The only old-style `catch Expr` is `beamtalk_file_handle_registry:360`. That gives 104 protected regions (one row each, keyed by the `try` line). For each I read the region and the helpers it calls. Where a block is reachable I followed who supplies the fun. I also checked which process the region runs in. `beamtalk_class_vars:snapshot/0` answers `none` (so `protect/1` is a pass-through) when the process has no `'$bt_class_vars_home'` entry. No class in `stdlib/src/*.bt` declares `classState:`, and I found none that is `class sealed` in the files I checked (`TestRunner`, `TestCase`), so a stdlib-native function only sees a live home map when a user class-method invocation calls it directly in its own process (FFI or direct call). Spawned workers, test processes and gen_server callbacks never have one. Files with no `try`/`catch`: array, binary-free helpers (set, map, tuple, queue, uuid, os, platform, random, interval, time, datetime, duration, digest, file_handle, subprocess_port), and `beamtalk_program` (comment/doc mentions only).

Counts: converted 2, NEEDS-CONVERSION 0, deliberately-left-candidate 0, unclear 0, not-applicable-other-process 13, not-applicable-reraises 15, not-applicable-no-block 74. Total 104.

#### Site table

| file:line | function/arity | what the region calls | disposition | reason |
|---|---|---|---|---|
| beamtalk_result.erl:220 | 'tryDo:'/1 | `protect(Block)` | converted | uses protect/1 |
| beamtalk_test_case.erl:66 | should_raise/2 | `protect(Block)` | converted | uses protect/1 |
| beamtalk_test_case.erl:778 | run_test_method/5 (outer) | `Module:new()`, setUp dispatch, test body | not-applicable-other-process | runs in spawned test process (`spawn_test_execution:1095`), `run_class_by_name` worker, or TestRunner caller: none has a home entry, snapshot=none (see note A) |
| beamtalk_test_case.erl:801 | run_test_method/5 (test body) | `Module:dispatch(MethodName, [], Inst)`, swallows into {fail,..} | not-applicable-other-process | same test process; see note A |
| beamtalk_test_case.erl:837 | run_test_method/5 (tearDown, in `after`) | `Module:dispatch(tearDown,..)`, swallows | not-applicable-other-process | same test process; see note A |
| beamtalk_test_case.erl:944 | run_suite_lifecycle/5 (outer) | setUpOnce dispatch, `TestFun(Fixture)`, swallows into failed list | not-applicable-other-process | same test process; see note A |
| beamtalk_test_case.erl:953 | run_suite_lifecycle/5 (inner) | `TestFun(Fixture)` with `after` only | not-applicable-reraises | try/after, no catch |
| beamtalk_test_case.erl:985 | run_teardown_once/3 | tearDownOnce dispatch, swallows | not-applicable-other-process | same test process; see note A |
| beamtalk_test_case.erl:1095 | spawn_test_execution/6 | `execute_tests/5` inside `spawn(fun)` | not-applicable-other-process | spawned process, no home entry |
| beamtalk_test_case.erl:1169 | ffi_extract_class_name/1 | list_to_existing_atom | not-applicable-no-block | BIF only |
| beamtalk_test_runner.erl:155 | discover_tests/0 | registry reflection | not-applicable-no-block | reflection, runs no test code |
| beamtalk_test_runner.erl:451 | result_to_json/1 | json:encode of plain maps | not-applicable-no-block | pure Erlang |
| beamtalk_test_runner.erl:667 | class_source_matches/2 | module_name_safe gen_server call, reflection | not-applicable-no-block | gen_server/reflection only |
| beamtalk_test_runner.erl:844 | is_serial/1 | `class_send(ClassPid, serial, [])` | not-applicable-other-process | gen_server call runs in class process |
| beamtalk_test_runner.erl:936 | spawn_class_worker/3 | `run_class_by_name/1` in spawn_monitor | not-applicable-other-process | worker process, no home entry |
| beamtalk_parallel.erl:199 | run_worker/4 | `Block()`, swallows into Result error | not-applicable-other-process | block runs in spawned worker (`spawn_workers`), not the caller |
| beamtalk_stream.erl:205 | take/2 | take_loop (runs generator/blocks) | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:216 | do/2 | do_loop (block) | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:230 | inject_into/3 | inject_loop (block) | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:250 | detect/2 | detect_loop (block) | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:276 | detect_if_none/3 | detect_loop (block) | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:292 | as_list/1 | as_list_loop | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:303 | any_satisfy/2 | block loop | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:316 | all_satisfy/2 | block loop | not-applicable-reraises | try/after only |
| beamtalk_stream.erl:499 | call_finalizer/1 | `Finalizer()` | not-applicable-no-block | only finalizer is `fun() -> file:close(Fd) end` (beamtalk_file:1324); no Beamtalk block can be set (see note B) |
| beamtalk_list.erl:57 | at/2 | lists:nth | not-applicable-no-block | BIF |
| beamtalk_collection.erl:273 | to_list/1 | `send(Self,'do:',[Block])` (user do: in-process) | not-applicable-reraises | try/after only (drains mailbox) |
| beamtalk_timer.erl:71 | 'after:do:'/2 | `Block()`, swallow | not-applicable-other-process | spawn_link'd timer process |
| beamtalk_timer.erl:203 | repeat_loop/2 | `Block()`, swallow, loops | not-applicable-other-process | spawn_link'd timer process |
| beamtalk_interface.erl:594 | handle_erlang_help/1 | binary_to_existing_atom, erlang_help | not-applicable-no-block | pure reflection |
| beamtalk_interface.erl:638 | handle_erlang_help/2 | same | not-applicable-no-block | pure reflection |
| beamtalk_interface.erl:684 | handle_class_named/1 | existing-atom, class registry | not-applicable-no-block | registry lookup |
| beamtalk_interface.erl:735 | class_object_for_pid/2 | module_name_safe gen_server call | not-applicable-no-block | gen_server call |
| beamtalk_interface.erl:772 | resolve_metaclass_tag/1 | existing-atom, registry | not-applicable-no-block | registry lookup |
| beamtalk_interface.erl:800 | handle_help/1 | format_class_help (class-process calls) | not-applicable-no-block | reflection/gen_server calls |
| beamtalk_interface.erl:846 | handle_help_selector/2 | resolve_help_method (resolver, gen_server) | not-applicable-no-block | reflection/gen_server calls |
| beamtalk_interface.erl:927 | resolve_class_name/1 | class_name gen_server call | not-applicable-no-block | gen_server call |
| beamtalk_interface.erl:941 | resolve_class_name/1 | binary_to_existing_atom | not-applicable-no-block | BIF |
| beamtalk_interface.erl:955 | ensure_atom/1 | binary_to_existing_atom | not-applicable-no-block | BIF |
| beamtalk_interface.erl:1168 | own_method_categories/3 | file read, compiler port | not-applicable-no-block | file/port I/O |
| beamtalk_interface.erl:1273 | instance_selectors/1 | binary_to_existing_atom | not-applicable-no-block | BIF |
| beamtalk_subprocess.erl:203 | handle_info exit event | exec_port:close | not-applicable-no-block | port I/O |
| beamtalk_subprocess.erl:550 | cleanup | exec_port:kill_child | not-applicable-no-block | port I/O |
| beamtalk_subprocess.erl:555 | cleanup | exec_port:close | not-applicable-no-block | port I/O |
| beamtalk_reactive_subprocess.erl:278 | handle_info exit | subprocess_port:close | not-applicable-no-block | port I/O (delegate notify is outside the try) |
| beamtalk_reactive_subprocess.erl:317 | cleanup | kill_child | not-applicable-no-block | port I/O |
| beamtalk_reactive_subprocess.erl:322 | cleanup | close | not-applicable-no-block | port I/O |
| beamtalk_reactive_subprocess.erl:343 | handle_write_line/3 | iolist_to_binary | not-applicable-no-block | BIF |
| beamtalk_reactive_subprocess.erl:346 | handle_write_line/3 | write_stdin | not-applicable-no-block | port I/O |
| beamtalk_reactive_subprocess.erl:371 | handle_close/1 | kill_child | not-applicable-no-block | port I/O |
| beamtalk_reactive_subprocess.erl:376 | handle_close/1 | close | not-applicable-no-block | port I/O |
| beamtalk_system.erl:126 | hostname/0 | inet:gethostname | not-applicable-no-block | BIF |
| beamtalk_transcript_facade.erl:140 | via_stream/3 | gen_server:call (Fallback runs in the handler, outside region) | not-applicable-no-block | gen_server call |
| beamtalk_transcript_stream.erl:177 | handle_call/3 | beamtalk_dispatch:lookup in `erlang:spawn` | not-applicable-other-process | spawned helper process; gen_server is not a class process |
| beamtalk_transcript_stream.erl:267 | handle_cast/2 | same | not-applicable-other-process | spawned helper process |
| beamtalk_console.erl:194 | write/3 | io:put_chars | not-applicable-no-block | io only |
| beamtalk_console.erl:208 | read/2 | io:get_line | not-applicable-no-block | io only |
| beamtalk_exec_port.erl:187 | find_in_project/1 | find_project_root, filelib | not-applicable-no-block | file system |
| beamtalk_file.erl:389 | 'open:do:'/2 | `Block(Handle)` | not-applicable-reraises | try/after only (closes handle) |
| beamtalk_file.erl:565 | 'open:mode:do:'/3 | `Block(Handle)` | not-applicable-reraises | try/after only |
| beamtalk_ets.erl:88 | new/2 | ets:new | not-applicable-no-block | ETS |
| beamtalk_ets.erl:152 | getOrCreate-style new | ets:new | not-applicable-no-block | ETS |
| beamtalk_ets.erl:200 | lookup/2 | ets:lookup | not-applicable-no-block | ETS |
| beamtalk_ets.erl:225 | insert/3 | ets:insert | not-applicable-no-block | ETS |
| beamtalk_ets.erl:245 | lookupIfAbsent/3 | `Block()` on miss | not-applicable-reraises | catch only `error:badarg`, always raises stale_table_error (see note C) |
| beamtalk_ets.erl:261 | includesKey/2 | ets:member | not-applicable-no-block | ETS |
| beamtalk_ets.erl:272 | removeKey/2 | ets:delete | not-applicable-no-block | ETS |
| beamtalk_ets.erl:289 | keys/1 | ets:select | not-applicable-no-block | ETS |
| beamtalk_ets.erl:318 | delete/1 | ets:delete | not-applicable-no-block | ETS |
| beamtalk_file_handle_registry.erl:131 | register/2 | gen_server:call | not-applicable-no-block | gen_server call |
| beamtalk_file_handle_registry.erl:157 | unregister/1 | gen_server:call | not-applicable-no-block | gen_server call |
| beamtalk_file_handle_registry.erl:179 | open_handles/0 | gen_server:call | not-applicable-no-block | gen_server call |
| beamtalk_file_handle_registry.erl:360 | close_owned_handle/1 (`catch Expr`) | file_handle:close_handle | not-applicable-no-block | file close, gen_server callback |
| beamtalk_atomic_counter.erl:81 | new:/1 | ets:new | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:133 | increment/1 | ets:update_counter | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:144 | incrementBy/2 | ets | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:159 | decrement/1 | ets | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:170 | decrementBy/2 | ets | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:185 | value/1 | ets:lookup | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:210 | reset/1 | ets:insert | not-applicable-no-block | ETS |
| beamtalk_atomic_counter.erl:227 | delete/1 | ets:delete | not-applicable-no-block | ETS |
| beamtalk_json.erl:60 | 'parse:'/1 | json:decode, normalize_decoded | not-applicable-no-block | pure data |
| beamtalk_json.erl:93 | 'generate:'/1 | prepare_for_encode -> try_as_json_hook -> `beamtalk_message_dispatch:send(V,'asJson',[])` (user code, in-process) | not-applicable-reraises | every clause re-raises or raises type_error (see note D) |
| beamtalk_json.erl:116 | 'prettyPrint:'/1 | same asJson hook | not-applicable-reraises | every clause raises (see note D) |
| beamtalk_regex.erl:140 | matches_regex/2 | re:run | not-applicable-no-block | re |
| beamtalk_regex.erl:156 | matches_regex/3 | re:run | not-applicable-no-block | re |
| beamtalk_regex.erl:176 | first_match/2 | re:run | not-applicable-no-block | re |
| beamtalk_regex.erl:197 | all_matches/2 | re:run | not-applicable-no-block | re |
| beamtalk_regex.erl:219 | replace_regex/3 | re:replace | not-applicable-no-block | re |
| beamtalk_regex.erl:238 | replace_all_regex/3 | re:replace | not-applicable-no-block | re |
| beamtalk_regex.erl:253 | split_regex/2 | re:split | not-applicable-no-block | re |
| beamtalk_string.erl:359 | from_iolist/1 | iolist_to_binary | not-applicable-no-block | BIF |
| beamtalk_string.erl:368 | from_iolist/1 | unicode:characters_to_binary | not-applicable-no-block | BIF |
| beamtalk_string.erl:380 | from_iolist/1 | unicode conv | not-applicable-no-block | BIF |
| beamtalk_string.erl:397 | from_iolist/1 | unicode conv | not-applicable-no-block | BIF |
| beamtalk_string.erl:438 | urlDecoded/1 | uri_string:unquote | not-applicable-no-block | OTP pure |
| beamtalk_binary.erl:91 | 'deserialize:'/1 | binary_to_term | not-applicable-no-block | BIF |
| beamtalk_binary.erl:110 | 'fromIolist:'/1 | iolist_to_binary | not-applicable-no-block | BIF |
| beamtalk_binary.erl:127 | 'fromBase64:'/1 | base64:decode | not-applicable-no-block | OTP pure |
| beamtalk_binary.erl:150 | 'fromBase64Url:'/1 | base64:decode | not-applicable-no-block | OTP pure |
| beamtalk_binary.erl:169 | 'fromHex:'/1 | binary:decode_hex | not-applicable-no-block | BIF |
| beamtalk_binary.erl:277 | part/3 | binary:part | not-applicable-no-block | BIF |
| beamtalk_binary.erl:319 | from_bytes/1 | list_to_binary | not-applicable-no-block | BIF |
| beamtalk_binary.erl:401 | deserialize_with_used/1 | binary_to_term | not-applicable-no-block | BIF |

Notes referenced above:

- A. The test-harness catchers swallow test errors and continue, but the code they run is never in a class-method invocation. `TestCase runAll`/`run:` are spawned (`beamtalk_class_dispatch:is_test_execution_selector`, `spawn_test_execution`). `beamtalk_test_runner` concurrent mode uses `spawn_monitor` workers. Sequential `TestRunner runAll` runs `run_class_by_name` in the TestRunner class process, which has no `classState:` and so no home entry. Residual risk: a future class-method caller that holds a live home map and calls `run_all/1`, `run_single/2` or `run_all_structured/1` directly (the BIF-fallback exports) would keep writes. Wrapping the test body in `protect/1` would be harmless (snapshot is `none` today) if the owner wants belt-and-braces.
- B. `beamtalk_stream:make_stream/3` accepts any 0-arity finalizer, but the only in-tree producer is `beamtalk_file:make_line_stream` (closes an fd). Nothing in `stdlib/src/*.bt` or the runtime passes a Beamtalk block.
- C. Side bug, not class-var related: a block passed to `Ets lookupIfAbsent:key:block:` that raises `error:badarg` is reported as a stale-table error.
- D. `beamtalk_json:93/116` clause `_:Reason` also turns a stray `$bt_nlr` throw out of an `asJson` hook into a `type_error`. It does not continue with a swallowed value, so there is no class-var leak, and it is not a protect/1 site.

#### FFI-reachable catchers outside our src

`stdlib/src/*.bt` has no live `(Erlang <otp_module>)` call: every non-comment `Erlang <module>` / `native:` reference is to a `beamtalk_*` module, and all of those exist under `runtime/apps/*/src` (checked). The OTP names that appear (`lists`, `erlang`, `module`, `foo`) are only in `///` doc examples. So nothing is verified for this table.

| module:function | takes a block | catches around it | verified |
|---|---|---|---|
| (none found in stdlib/src) | | | |

User code can still FFI to any OTP module at run time (`docs/beamtalk-language-features.md` documents `(Erlang lists) reverse:`). That is covered by the FFI rule in `docs/development/erlang-guidelines.md`, not by this audit.

#### NEEDS-CONVERSION and unclear sites

None in `beamtalk_stdlib/src`.

Optional hardening (not required by ADR 0130):

1. `beamtalk_test_case.erl:801`, `:778`, `:944` (test-body and suite-setup catchers). Fix: wrap the dispatch in `beamtalk_class_vars:protect/1` inside the try. Test shape: EUnit that sets up a home entry (`beamtalk_class_vars` entry points), has a test method write a class var then fail, and asserts `run_test_method` returns `{fail,..}` with the pre-call map restored.
2. `beamtalk_json.erl:93/116`: no change for class vars. If wanted, rethrow `$bt_nlr` before the `_:Reason` clause. Test shape: an `asJson` hook that does `^` out of a block.

### beamtalk_workspace/src, part 1 (through `beamtalk_repl_server`)

#### Method

Enumerated every `try` and old-style `catch Expr` token with `erl_scan` (so comments, `-doc` strings and string literals are excluded) over the 28 files, then collapsed each `try ... catch/of/after` to one row keyed by its `try` line (162 regions: 159 `try`, 3 old-style `catch io:put_chars` in beamtalk_release_launcher.erl). Files with no sites: beamtalk_actor_sup, beamtalk_repl_ops, beamtalk_repl_ops_eval, beamtalk_repl_ops_session, beamtalk_repl_ops_watch, beamtalk_repl_protocol. For each region I read the body and followed called helpers where a Beamtalk block or in-process user code could be reached. Key process facts: `beamtalk_class_vars:install/2` (the only writer of `?HOME`) is called from `beamtalk_class_dispatch.erl:968`, i.e. only in a class gen_server during a class-method invocation, so `snapshot/0` returns `none` (and `protect/1` is a no-op) in every other process. REPL eval/run-entry runs in a shell-spawned eval worker (see `announce_binding_changed` doc in beamtalk_repl_eval.erl), the release launcher `eval`/`rpc` verbs run in a throwaway VM / rpc-spawned process, and op handlers run in ws/session/dist-rpc processes; none is a class-invocation process. The one exception found: `beamtalk_inspector:eval_value/2` (Inspector `evaluate:`, an FFI call from Beamtalk code) does `erlang:apply(beamtalk_repl_eval, eval_with_self, ...)` in the CALLER process, which can be a class-invocation process (e.g. a class method calling `inspector evaluate: ...`). Other Behaviour-intrinsic entry points into this app (emit_*_change_entry, rewrite_sites, validate_sites, compile_method, precheck_method, remove_method, reload_class_file, capture_class_removal_snapshot, class_source_file) also run in the caller, but none runs a Beamtalk block (compile/load/registry/store work only).

#### Sites

| file:line | function/arity | what the region calls | disposition | reason |
|---|---|---|---|---|
| beamtalk_alias_xref.erl:140 | register_class/2 | ETS/gen_server alias xref | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_alias_xref.erl:179 | register_class_additive/2 | ETS/gen_server alias xref | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_alias_xref.erl:197 | dependents_of/1 | ETS/gen_server alias xref | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_alias_xref.erl:207 | clear/0 | ETS/gen_server alias xref | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_erlang_help.erl:65 | format_function_help/2 | EEP-48 doc/code introspection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_erlang_help.erl:380 | format_exports_list/1 | EEP-48 doc/code introspection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_erlang_help.erl:409 | format_eep48_signatures_or_exports/1 | EEP-48 doc/code introspection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_erlang_help.erl:462 | format_specs_list/2 | EEP-48 doc/code introspection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_erlang_help.erl:609 | find_function_arities/2 | EEP-48 doc/code introspection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_git.erl:334 | run_git_in/3 | port/os git, binary_to_integer | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_git.erl:336 | run_git_in/3 | port/os git, binary_to_integer | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_git.erl:687 | to_int/2 | port/os git, binary_to_integer | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_idle_monitor.erl:167 | has_active_sessions/0 | session table query | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_io_capture.erl:164 | reset_captured_group_leaders/2 | IO protocol server loop | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_io_capture.erl:208 | prompt_to_binary/1 | IO protocol server loop | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_io_capture.erl:250 | handle_io_request/2 | IO protocol server loop | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_io_capture.erl:258 | handle_io_request/2 | IO protocol server loop | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_io_capture.erl:265 | handle_io_request/2 | IO protocol server loop | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:306 | trigger/4 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:360 | trigger_shape/2 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:453 | trigger_pending/5 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:515 | trigger_image/0 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:623 | trigger_leaf_change/1 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:708 | trigger_alias_change/1 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:821 | meta_type_from_binary/1 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:858 | recheck_image_class/2 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:1172 | field_accessor_atoms/1 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_recheck.erl:1661 | recheck_owner_for_leaf_change/3 | compiler/xref/ETS re-check | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_release_launcher.erl:159 | do_eval_main/3 | io:put_chars to stderr | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_release_launcher.erl:167 | do_eval_main/3 | io:put_chars to stderr | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_release_launcher.erl:216 | rpc_client_main/0 | io:put_chars to stderr | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_compiler.erl:341 | build_class_module_index/0 | compiler port/registry | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_compiler.erl:348 | build_class_module_index/0 | compiler port/registry | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_compiler.erl:905 | wrap_compiler_errors/2 | compiler port/registry | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:64 | format_class_docs/1 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:70 | format_class_docs/1 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:161 | format_method_doc/2 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:196 | format_method_doc_class_side/2 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:229 | format_class_docs_class_side/1 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:232 | format_class_docs_class_side/1 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:426 | method_doc_signature_resolved/3 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:676 | format_package_provenance/1 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_docs.erl:754 | sibling_classes/2 | class gen_server reflection/doc formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_errors.erl:95 | safe_to_existing_atom/1 | atom/binary conversion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_errors.erl:161 | ensure_structured_error/1 | atom/binary conversion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_eval.erl:368 | do_dispatch/5 | dispatch_sync/3 (run-entry Beamtalk code) | not-applicable-reraises | run-entry dispatch_sync inside; try/of/after has no catch clause (after only), errors propagate |
| beamtalk_repl_eval.erl:461 | dispatch_sync/3 | send_entry/3 (class-side dispatch to class gen_server) + future_await | not-applicable-other-process | runs entry via send_entry; swallows into {error,..}, but runs in eval worker / launcher VM / rpc process, never a class-invocation process (HOME unset, protect would no-op) |
| beamtalk_repl_eval.erl:613 | do_eval_trace/2 | apply(EvalModule, eval, [Bindings]) = user Beamtalk code | not-applicable-other-process | traced REPL eval runs generated eval/1 in the session eval worker; not a class process, HOME unset |
| beamtalk_repl_eval.erl:924 | run_self_eval_module/3 | apply(EvalModule, eval, [#{self => Self}]) = user Beamtalk code in caller process | converted (BT-3728) | Inspector evaluate: runs user source in the caller's process; the eval is now wrapped in protect/1 so a swallowed error discards its class-variable writes |
| beamtalk_repl_eval.erl:1320 | stdlib_class_module/1 | whereis_class, module_name (gen_server:call) | not-applicable-other-process | gen_server calls to class process; vanished-class exit only |
| beamtalk_repl_eval.erl:1589 | maybe_register_protocol_class/1 | ModuleName:register_class() | not-applicable-no-block | register_class/0 only registers class objects (class gen_server init runs in its own process) |
| beamtalk_repl_eval.erl:1632 | eval_loaded_module/7 | execute_and_process/5 -> apply(EvalModule, eval,[..]) user Beamtalk code | not-applicable-other-process | REPL eval worker runs user code and swallows to {error,{eval_error,..}}; worker is not a class process, HOME unset |
| beamtalk_repl_eval.erl:1730 | announce_binding_changed/3 | beamtalk_announcements:system_announce/2 | not-applicable-no-block | announcement bus cast/call, other process |
| beamtalk_repl_eval.erl:2015 | maybe_await_future/1 | beamtalk_runtime_api:future_await/2 | not-applicable-no-block | future_await is a receive; no block run |
| beamtalk_repl_json.erl:46 | parse_json/1 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:66 | format_response/1 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:83 | format_error/1 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:106 | format_response_with_warnings/2 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:127 | format_error_with_warnings/2 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:309 | term_to_json/1 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:404 | actor_label_with_fallback/2 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_json.erl:426 | is_local_process_alive/1 | JSON codec/term formatting | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:590 | register_classes/2 | ModuleName:register_class() | not-applicable-no-block | register_class/0 registers classes; class init in class process, no block |
| beamtalk_repl_loader.erl:666 | hot_reload_descendants/2 | hot_reload_class/2 -> instance code_change via other processes | not-applicable-other-process | hot_reload_class sys:change_code/suspend in instance processes; runs there, not here |
| beamtalk_repl_loader.erl:963 | emit_class_def_entry/3 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:1362 | hot_reload_class/2 | beamtalk_runtime_api:all_instances/1 | not-applicable-no-block | all_instances registry lookup; badarg only |
| beamtalk_repl_loader.erl:1396 | publish_suspended_finding/4 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:1425 | read_installed_shape_version/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:1451 | safe_binary_to_atom/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:1459 | safe_list_to_atom/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:2602 | precheck_class_shape_against/3 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:2686 | pending_reload_findings_locked/3 | pending_findings_from_temp_module/3 | not-applicable-no-block | after-only; temp-module meta read |
| beamtalk_repl_loader.erl:2815 | capture_signature_removal/3 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:3503 | emit_rewrite_change_entry/2 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:4062 | finish_rename_class_revert/1 | beamtalk_behaviour_intrinsics:install_class_rename/3 | not-applicable-no-block | install_class_rename is registry/class-gen_server mutation, no block |
| beamtalk_repl_loader.erl:5163 | do_autoflush/0 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5197 | emit_new_class_entry/3 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5292 | emit_change_entry/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5355 | capture_signature_generation/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5396 | rollback_signature_generation/4 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5467 | findings_store_clear_owner/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5508 | findings_store_put_owner_origin/3 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5604 | findings_store_get_origin/2 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5747 | prime_shape_capture/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:5800 | maybe_trigger_shape_recheck_for_class/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6001 | superclasses_losing_leaf_status/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6033 | was_leaf_class/1 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6091 | publish_leaf_change_recheck_outcome_safe/2 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6287 | publish_alias_change_recheck_outcome_safe/2 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6702 | emit_remove_change_entry/5 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6855 | emit_extension_remove_change_entry/7 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_loader.erl:6991 | emit_remove_class_change_entry/4 | stores/ETS/gen_server/file/changelog | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_modules.erl:104 | get_actor_count/3 | beamtalk_repl_ops / registry count | not-applicable-no-block | gen_server call to actor registry |
| beamtalk_repl_modules.erl:150 | resolve_source_path/1 | module registry/file | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_actors.erl:67 | handle_term/4 | sys:get_state/2, is_tagged, field_names | not-applicable-other-process | sys:get_state of actor process, other process |
| beamtalk_repl_ops_actors.erl:129 | handle_term/4 | beamtalk_repl_shell:interrupt/1 | not-applicable-other-process | interrupt is a gen_server call to the session process |
| beamtalk_repl_ops_actors.erl:141 | validate_actor_pid/1 | pid parsing/registry | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:245 | browse_classes/0 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:261 | safe_test_classes/0 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:269 | class_row/2 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1068 | browse_native_modules/0 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1225 | backing_source_file/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1286 | browse_type_aliases/0 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1316 | type_aliases_of_package/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1622 | native_meta_of/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1695 | delegate_callers_of_native_module/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1714 | delegate_rows_for_class_name/2 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1747 | dispatch_export_selector/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:1776 | optional_selector/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:2024 | resolve_module/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:2099 | validate_selector/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:2112 | resolve_class/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:2174 | category_of/1 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_browse.erl:2452 | safe_bool/1 | fun(() -> boolean()) over runtime_api reflection | not-applicable-no-block | F is an Erlang reflection fun (is_sealed etc.), not a Beamtalk block |
| beamtalk_repl_ops_browse.erl:2462 | safe_class_call/2 | registry/gen_server reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:226 | handle_term/4 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:232 | handle_term/4 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:275 | handle_term/4 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:418 | handle_term/4 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:445 | handle_term/4 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:459 | handle_term/4 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:645 | show_codegen_class_method/2 | validate_selector_if_present, compile_class_source | not-applicable-no-block | gen_server calls + compile of class source |
| beamtalk_repl_ops_dev.erl:788 | run_test_op/1 | beamtalk_test_runner:run_all/1 (runs TestCase blocks) | not-applicable-other-process | test-all op: runs Beamtalk test code and swallows to {error,..}; op handler is ws/session/dist-rpc process, not a class process |
| beamtalk_repl_ops_dev.erl:808 | run_test_op/1 | beamtalk_test_runner:run_class_by_name/1 | not-applicable-other-process | test-class op, same as above |
| beamtalk_repl_ops_dev.erl:837 | run_test_op_file/1 | beamtalk_test_runner:run_file/1 | not-applicable-other-process | test-file op, same as above |
| beamtalk_repl_ops_dev.erl:900 | class_name_completions/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:911 | class_name_completions/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1298 | tokenise_send_chain/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1433 | parse_binary_hops/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1448 | parse_binary_hops/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1574 | resolve_type_via_compiler/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1733 | classify_receiver/2 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1779 | classify_receiver/2 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1814 | maybe_class/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1830 | get_session_bindings/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1844 | get_session_alias_names/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1858 | get_workspace_bindings/0 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:1905 | collect_methods_with_fun/3 | whereis_class | not-applicable-no-block | Fun is an Erlang reflection fun over ClassPid; not a Beamtalk block |
| beamtalk_repl_ops_dev.erl:1915 | collect_methods_with_fun/3 | Fun(ClassPid) | not-applicable-no-block | Fun is an Erlang reflection fun over ClassPid (gen_server call) |
| beamtalk_repl_ops_dev.erl:1926 | collect_methods_with_fun/3 | superclass/1 | not-applicable-no-block | superclass gen_server call |
| beamtalk_repl_ops_dev.erl:1962 | is_cross_package_internal/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:2018 | collect_internal_selectors/3 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:2034 | collect_internal_selectors_for_class/2 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:2090 | read_class_meta/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_dev.erl:2486 | list_classes_session_tracker/1 | reflection/compile/completion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2091 | resolve_class_to_module/1 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2101 | resolve_class_to_module/2 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2120 | module_to_class_name_map/0 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2127 | module_to_class_name_map/0 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2307 | get_native_compile_mtime/1 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2581 | read_provenance_stamp/1 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_load.erl:2635 | current_beamtalk_version/0 | registry/file/version lookup | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_nav.erl:200 | with_selector/2 | atom conversion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_nav.erl:219 | with_class/2 | atom conversion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_nav.erl:243 | with_module/1 | atom conversion | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_nav_symbols.erl:156 | safe_all_classes/0 | registry/source-origin reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_nav_symbols.erl:164 | class_to_row/2 | registry/source-origin reflection | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_perf.erl:49 | handle_term/4 | do_handle_term/4 (beamtalk_tracing) | not-applicable-no-block | tracing/perf ops: ETS and normalise #beamtalk_error{}; no block |
| beamtalk_repl_ops_perf.erl:183 | parse_pid/1 | atom/pid parsing | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_perf.erl:205 | parse_selector/1 | atom/pid parsing | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_ops_perf.erl:227 | parse_class/1 | atom/pid parsing | not-applicable-no-block | no Beamtalk block reachable |
| beamtalk_repl_server.erl:325 | handle_protocol_request/2 | handle_op/4 dispatch to op modules | not-applicable-other-process | outer protocol handler logs and replies; ops run in ws/session handler, not class process; ops with user code run in eval worker |

Counts: not-applicable-no-block 149, not-applicable-other-process 11, not-applicable-reraises 1, NEEDS-CONVERSION 1, converted 0, deliberately-left-candidate 0, unclear 0 (total 162).

#### NEEDS-CONVERSION and unclear sites

1. **beamtalk_repl_eval.erl:924 `run_self_eval_module/3`** (NEEDS-CONVERSION; Inspector `evaluate:`).
   - Why: `apply(ModuleName, eval, [#{self => Self}])` runs compiled user Beamtalk in the calling process. The `catch Class:Reason:Stacktrace` swallows into `{error, #beamtalk_error{}}` and the caller continues. If the caller is a class-invocation process (class method calls `Inspector evaluate:`) and `Self` is, or the expression reaches, a block created in that invocation, the block writes the live home class-variable map and those writes survive the caught error. An expression compiled as a free-standing eval module cannot itself write class variables (`class_state_unreachable`), so the leak needs such a block or a sealed direct-called method; it is real but narrow.
   - Fix: wrap the `apply(...)`/`maybe_await_future` body in `beamtalk_class_vars:protect(fun() -> ... end)` inside the existing `try`, keeping the `catch` as the continuation (protect re-raises after restoring, so the outer catch still converts to `{error,_}`; `$bt_nlr` throws pass through to the outer catch as today). The `after purge_eval_module` stays outermost. Do not convert the REPL eval worker sites (461, 613, 1632): HOME is unset there, protect would be a no-op.
   - Test shape: EUnit in `beamtalk_repl_eval_self_tests` (or a BUnit test): in a process with `beamtalk_class_vars:install(Key, #{n => 0})` (home set), call `eval_with_self(Block, <<"self value">>)` where `Block` is an Erlang fun mimicking a compiled block that does `beamtalk_class_vars:put(ClassSelf, n, 1)` then `error(boom)`; assert result is `{error, _}` and `beamtalk_class_vars:get(ClassSelf, n)` is still 0. Add a control with no error asserting the write persists.

No `unclear` sites.

Notes for the maintainer (not dispositions):
- beamtalk_repl_eval.erl:461 / :613 / :1632 and beamtalk_repl_ops_dev.erl:788/808/837 run user Beamtalk code and swallow, and are correctly left alone only because the process is never a class-invocation process. If the REPL is ever changed to run eval inside a class process, these become NEEDS-CONVERSION; a conformance test asserting `beamtalk_class_vars:snapshot() =:= none` in the eval worker would pin the assumption.
- beamtalk_repl_ops_dev.erl:1905-1915 `Fun(ClassPid)` is an Erlang reflection fun, not a Beamtalk block.

### beamtalk_workspace/src, part 2 (`beamtalk_repl_shell` onward)

#### Method

Files audited: the 27 files from `beamtalk_repl_shell.erl` through `beamtalk_ws_log_handler.erl`. I located every `try`/`catch` by grep (excluding comments, `-doc` text and strings; a search for old-style `catch Expr` found none in these files), then read each region and followed called helpers (`beamtalk_announcements:system_announce`, `beamtalk_repl_loader:reload_class_file`, `beamtalk_repl_eval` revert/compile entry points, the recheck entry points) to see whether a Beamtalk block or user code could run synchronously in the catching process. Note that in a `try Expr of ... catch`, the `of` body is not protected; I only counted the `Expr`. `beamtalk_repl_shell.erl` has no `try`/`catch` (its only `after` is a `receive ... after 0`); REPL evaluation runs in `spawn_monitor` worker processes (lines 316, 327, 499, 529), and the shell handles worker crashes through `DOWN` messages. The eval worker's class-var map therefore dies with the worker, so no restore is needed in this app. Transitive depth for `revert_method` (interface_primitives:482) was checked at call level only. None of the 75 sites uses `beamtalk_class_vars:protect/1`.

#### Sites

| file:line | function/arity | what the region calls | disposition | reason |
|---|---|---|---|---|
| beamtalk_workspace_meta.erl:144 | get_metadata/N | gen_server:call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:162 | get_package_name/N | gen_server:call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:177 | update_activity/N | gen_server:cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:189 | get_last_activity/N | gen_server:call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:208 | register_actor/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:218 | unregister_actor/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:228 | supervised_actors/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:245 | register_module/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:257 | unregister_module/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:267 | loaded_modules/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:277 | set_class_source/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:289 | get_class_source/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:309 | remove_class_source/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:326 | all_class_sources/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:359 | set_protocol_source/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:369 | get_protocol_source/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:383 | remove_protocol_source/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:399 | all_protocol_sources/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:414 | set_file_mtime/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:427 | get_file_mtimes/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:440 | clear_file_mtimes/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:453 | remove_file_mtime/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:470 | get_setting/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:486 | set_setting/N | call meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:525 | set_git_toplevel/N | cast meta server (gen_server, ETS state) | not-applicable-no-block | gen_server call/cast; no block |
| beamtalk_workspace_meta.erl:564 | is_foreground_id/1 | iolist_to_binary | not-applicable-no-block | pure conversion |
| beamtalk_workspace_meta.erl:612 | prune_stale_foreground_workspaces/3 | file:list_dir, prune_if_stale (file/gen_tcp) | not-applicable-no-block | file/io only |
| beamtalk_workspace_meta.erl:644 | prune_stale_foreground_workspaces_async/3 | prune_stale... in spawned process | not-applicable-no-block | file/io; also spawned proc |
| beamtalk_workspace_meta.erl:654 | prune_if_stale/3 | file info, del_dir_r, owner check | not-applicable-no-block | file/io only |
| beamtalk_workspace_meta.erl:725 | port_file_says_dead/1 | binary_to_integer, gen_tcp:connect | not-applicable-no-block | pure/io |
| beamtalk_workspace_meta.erl:1039 | load_metadata_from_disk/1 | json:decode (`of` body not protected) | not-applicable-no-block | JSON decode only |
| beamtalk_workspace_meta.erl:1299 | safe_existing_atom/1 | binary_to_existing_atom | not-applicable-no-block | pure |
| beamtalk_repl_state.erl:396 | stdlib_alias_table/0 | lists:foldl stdlib_alias_fold over app env | not-applicable-no-block | app-env data only |
| beamtalk_repl_state.erl:438 | stdlib_alias_fold/2 | app_env_binary/doc conversions | not-applicable-no-block | pure conversion |
| beamtalk_repl_subscriptions.erl:252 | subscribe_bus/2 | beamtalk_announcements:system_unsubscribe/subscribe (registers handler only) | not-applicable-no-block | registration via gen_server; handler runs later in spawned process |
| beamtalk_repl_subscriptions.erl:270 | unsubscribe_bus/2 | beamtalk_announcements:system_unsubscribe | not-applicable-no-block | gen_server call; no block |
| beamtalk_session_primitives.erl:398 | fetch_meta/1 | beamtalk_repl_shell:get_session_meta (gen_server call) | not-applicable-other-process | call to shell process; catches exit only |
| beamtalk_session_primitives.erl:441 | mint_live_session/1 | beamtalk_repl_shell:get_session_meta (gen_server call) | not-applicable-other-process | call to shell process; catches exit only |
| beamtalk_session_primitives.erl:566 | to_binary_id/1 | unicode:characters_to_binary | not-applicable-no-block | pure |
| beamtalk_session_primitives.erl:583 | to_name_atom/1 | unicode:characters_to_binary (`of` body unprotected) | not-applicable-no-block | pure |
| beamtalk_session_primitives.erl:615 | to_existing_name_atom/1 | binary_to_existing_atom | not-applicable-no-block | pure |
| beamtalk_session_primitives.erl:621 | to_existing_name_atom/1 | binary_to_existing_atom(unicode...) | not-applicable-no-block | pure |
| beamtalk_session_sup.erl:82 | normalize_kind/1 | unicode:characters_to_binary | not-applicable-no-block | pure |
| beamtalk_session_table.erl:69 | lookup_alive/1 | ets:lookup, is_process_alive | not-applicable-no-block | ETS only |
| beamtalk_session_table.erl:94 | resolve_pid/2 | ets:lookup, is_process_alive | not-applicable-no-block | ETS only |
| beamtalk_workspace_changelog.erl:1098 | method_delta/1 | read_source_body, file:read_file, disk_method_body, body_delta | not-applicable-no-block | text/file diffing |
| beamtalk_workspace_changelog.erl:1114 | method_delta/1 | read_source_body, file:read_file, disk_class_body | not-applicable-no-block | text/file diffing |
| beamtalk_workspace_changelog.erl:1775 | parse_log/1 | entry_from_json per line | not-applicable-no-block | JSON parsing |
| beamtalk_workspace_changelog.erl:2284 | candidate_site_from_json/1 | span_from_json | not-applicable-no-block | JSON parsing |
| beamtalk_workspace_flush.erl:2686 | reload_renamed_class_source/2 | beamtalk_repl_loader:reload_class_file (file read, compiler port, code:load_binary; class on_load runs in code-server on_load process) | not-applicable-no-block | compile+load only; no block evaluated here (on_load is another process) |
| beamtalk_workspace_flush.erl:2773 | complete_flush/5 | beamtalk_workspace_changelog:mark_flushed (gen_server call) | not-applicable-other-process | call to changelog server; catches exit only |
| beamtalk_workspace_flush.erl:2837 | announce_flush_completed/2 | beamtalk_announcements:system_announce (async; handlers run in spawned transient procs) | not-applicable-other-process | handlers spawned elsewhere (dispatch_veneer_async) |
| beamtalk_workspace_interface_primitives.erl:482 | revert_method/3 | do_revert: changelog lookups, beamtalk_repl_eval compile_method/remove_method/remove_class/revert_*; class reinstall (on_load in other proc) | not-applicable-no-block | compile/load/ChangeLog only; swallows into {error,_} but runs no block (verified at call level, not every transitive helper) |
| beamtalk_workspace_interface_primitives.erl:532 | existing_selector_atom/1 | binary_to_existing_atom | not-applicable-no-block | pure |
| beamtalk_workspace_interface_primitives.erl:919 | retire_reverted_remove_class_entry/2 | changelog:mark_flushed (call) | not-applicable-other-process | gen_server call; exit then re-raised as structured error |
| beamtalk_workspace_interface_primitives.erl:982 | retire_reverted_rename_entry/2 | changelog:mark_flushed (call) | not-applicable-other-process | gen_server call; exit then re-raised as structured error |
| beamtalk_workspace_interface_primitives.erl:1070 | safe_existing_atom/1 | binary_to_existing_atom | not-applicable-no-block | pure |
| beamtalk_workspace_interface_primitives.erl:1491 | do_stop_supervisor/1 | gen_server:stop(Pid) on supervisor | not-applicable-other-process | stop runs in supervisor process; only noproc swallowed |
| beamtalk_workspace_interface_primitives.erl:1536 | dependencies/0 | beamtalk_package:named | not-applicable-no-block | package registry lookup |
| beamtalk_workspace_interface_primitives.erl:1689 | lookup_class_object/1 | beamtalk_runtime_api:module_name (call to class proc) | not-applicable-other-process | gen_server call to class process |
| beamtalk_workspace_interface_primitives.erl:1770 | ensure_bindings_table/0 | ets:new | not-applicable-no-block | ETS only |
| beamtalk_workspace_interface_primitives.erl:1969 | is_stdlib_class_name/1 | module_name_safe (call to class proc), is_stdlib_module | not-applicable-other-process | gen_server call to class process |
| beamtalk_workspace_interface_primitives.erl:2011 | base_class_name/1 | binary_to_existing_atom | not-applicable-no-block | pure |
| beamtalk_workspace_interface_primitives.erl:2026 | loaded_class_objects/1 | list_to_existing_atom only (`of` body unprotected) | not-applicable-no-block | pure; protected part is atom lookup |
| beamtalk_workspace_shape_recheck_worker.erl:140 | handle_cast/2 | beamtalk_repl_loader:maybe_trigger_leaf_change_recheck (compiler-server diagnostics round trips) | not-applicable-other-process | dedicated worker gen_server, not a class/invocation process; no block |
| beamtalk_workspace_shape_recheck_worker.erl:159 | handle_cast/2 | beamtalk_repl_loader:maybe_trigger_shape_recheck (compiler-server diagnostics round trips) | not-applicable-other-process | dedicated worker gen_server, not a class/invocation process; no block |
| beamtalk_workspace_shape_recheck_worker.erl:178 | handle_cast/2 | beamtalk_repl_loader:maybe_trigger_alias_change_recheck (compiler-server diagnostics round trips) | not-applicable-other-process | dedicated worker gen_server, not a class/invocation process; no block |
| beamtalk_workspace_shape_store.erl:292 | read_own_meta/1 | class_metadata lookup, Module:'__beamtalk_meta'() | not-applicable-no-block | generated literal meta getter; no block, no class-var access |
| beamtalk_workspace_shape_store.erl:485 | ancestor_own_field_map/2 | Module:'__beamtalk_meta'() | not-applicable-no-block | generated literal meta getter |
| beamtalk_workspace_signature_store.erl:216 | seed_from_meta/3 | Module:'__beamtalk_meta'(), map reads | not-applicable-no-block | generated literal meta getter |
| beamtalk_ws_handler.erl:389 | handle_auth/2 | json:decode (`of` body unprotected) | not-applicable-no-block | JSON decode only |
| beamtalk_ws_handler.erl:832 | actor_snapshot_frames/0 | beamtalk_repl_actors:list_actors (call to registry) | not-applicable-other-process | gen_server call; handler is WS process |
| beamtalk_ws_handler.erl:893 | normalise_files_for_push/1 | unicode:characters_to_binary | not-applicable-no-block | pure |
| beamtalk_ws_log_handler.erl:145 | log/2 | format_event (term formatting) | not-applicable-no-block | logger handler; formatting only |
| beamtalk_ws_log_handler.erl:189 | ensure_table/0 | ets:new | not-applicable-no-block | ETS only |

#### NEEDS-CONVERSION and unclear sites

None. No site in this app runs a Beamtalk block in-process and swallows the error.

Closest watch items (not flagged):

- `beamtalk_workspace_interface_primitives.erl:482` (`revert_method/3`) swallows into `{error,_}` while running compile/load/remove in the caller's process. If a future change makes any of that path evaluate class-side Beamtalk code in-process, wrap `do_revert` in `beamtalk_class_vars:protect/1` inside the `try`. Test shape: with the `do_revert` path stubbed to write a class var then raise, assert the class var is unchanged after the `{error,_}` return.
- `beamtalk_workspace_flush.erl:2686` (`reload_renamed_class_source/2`) logs and continues. Class `on_load` runs in the code-server's on_load process, so it is out of scope today. Re-check if reload ever runs class-side initialisers in-process.
- The `Module:'__beamtalk_meta'()` calls (shape_store:292, :485; signature_store:216) call generated Beamtalk module code. It returns a literal map and touches no class vars, so it is no-block.

### beamtalk_compiler/src and beamtalk_test_support/src

#### Method
Read `beamtalk_class_vars:protect/1`, `snapshot/0`, `restore/1`. Then grepped `try|catch|after` across every `src/*.erl` in both apps, and discarded comment, `-doc` and `receive ... after` hits. I opened each remaining region and followed what it calls. A site counts as needing conversion only if its body can run a Beamtalk block or in-process class-side code **and** its catch swallows the error and continues. Both apps are pure infrastructure. The compiler app talks to the Rust compiler over an Erlang port, to its own gen_server, and to beam_lib/ETS. The test-support app swaps registered names for EUnit fixtures. No region receives a Beamtalk block or fun from Beamtalk code. The only test-support regions that invoke a caller-supplied fun (`stderr_capture:capture/1`, the `Fun` in the `beamtalk_test_actor_registry` helpers) are EUnit test funs, and they re-raise or use `after`. Total sites: 101. The count is one row per try expression, plus one row per old-style `catch Expr`.

#### Table
| file:line | function/arity | what the region calls | disposition | reason |
|---|---|---|---|---|
| beamtalk_build_worker.erl:96-119 | handle_read_specs/1 | `beamtalk_spec_reader:read_specs_batch/1`, `io:put_chars` | not-applicable-no-block | beam_lib/ETS reading only; the catch logs to stderr and continues the loop, but no Beamtalk code runs |
| beamtalk_compiler_port.erl:168,268,328,390,454,517,589,688,774,856,944,1025,1122,1233,1374,1469,1546 (outer `try port_command ... of`/catch badarg) | 17 request fns (parse, compile, diagnostics, reindent, ...) | `port_command/2`, `receive` on port, `handle_*_response` pure term munging | not-applicable-no-block | pure port I/O; catch maps badarg to `{error, ...}` |
| beamtalk_compiler_port.erl:172,272,332,394,458,521,593,692,778,860,948,1029,1126,1237,1378,1473,1550 (inner `try binary_to_term([safe]) of`) | same 17 fns | `binary_to_term/2`, `handle_*_response/1` | not-applicable-no-block | decode only |
| beamtalk_compiler_port.erl:193,290,352,412,476,539,611,712,796,878,966,1047,1144,1255,1398,1491,1572 (`(try port_close(Port) catch _:_ -> ok end)`) | same 17 fns | `port_close/1` | not-applicable-no-block | closes a port; no Beamtalk code |
| beamtalk_compiler_port.erl:1295 | normalize_categories/1 | list comprehension over `normalize_category/1` (pure map reshaping) | not-applicable-no-block | pure data normalisation; catches only the tagged malformed error |
| beamtalk_compiler_port.erl:1757 | handle_resolve_response/1 | `binary_to_existing_atom/2` | not-applicable-no-block | BIF |
| beamtalk_compiler_port.erl:1889 | maybe_pretty_core/1 | `core_scan`, `core_parse`, `core_pp` | not-applicable-no-block | OTP compiler libs |
| beamtalk_compiler_server.erl:399,417,437,459,480,501,522,546,571,599,622,649,674,696,723,743,760,787,806,823,843,859,890,907 (24 sites: resolve_completion_type, register_aliases, find_*_in_source family, categorize_methods, reindent_method_source, class_state_field_defaults, get_classes/aliases/protocols, register/remove class/protocol casts, find_definition_selector_spans, resolve_class_span, ...) | public API wrappers | `gen_server:call/cast(?MODULE, ...)` | not-applicable-other-process | runs in the compiler gen_server; catch handles noproc/timeout exits |
| beamtalk_compiler_server.erl:1287 | terminate/2 | `beamtalk_compiler_port:close/1` | not-applicable-no-block | port close |
| beamtalk_compiler_server.erl:1358 | recover_from_beam_modules/0 | `Module:'__beamtalk_meta'()` on each loaded module | not-applicable-no-block | generated literal-map accessor: no block argument, no class method, no class-variable access. The catch swallows and continues, but nothing it runs touches class vars. |
| beamtalk_compiler_server.erl:1382 | open_port/0 | `beamtalk_compiler_port:open/0` | not-applicable-reraises | logs, then `error(Err)` |
| beamtalk_compiler_server.erl:1406 | send_port_request/3 | `port_command/2`, `receive` | not-applicable-no-block | port I/O |
| beamtalk_compiler_server.erl:1411 | send_port_request/3 | `binary_to_term/2` | not-applicable-no-block | decode |
| beamtalk_compiler_server.erl:1437 | send_port_request/3 | `port_close/1` | not-applicable-no-block | port close |
| beamtalk_spec_reader.erl:66 | read_specs/1 | `extract_specs_from_forms/1` (abstract-form folding) with `after clear_type_context()` | not-applicable-no-block | pure; `after` only, no catch |
| beamtalk_spec_reader.erl:103 | read_specs_batch/1 | `parallel_map/3` (spawns workers), `after ets:delete` | not-applicable-no-block | pure spec reading, and the work runs in worker processes; `after` only |
| beamtalk_spec_reader.erl:754 | resolve local type (map_type) | `map_type/1`, `after put(depth)` | not-applicable-no-block | pure; `after` only |
| beamtalk_spec_reader.erl:823 | resolve remote type | `map_type/1`, `after` restores pdict | not-applicable-no-block | pure; `after` only |
| beamtalk_spec_reader.erl:859 | remote_module_artifacts/2 | `ets:lookup`, `compute_remote_artifacts/1` | not-applicable-no-block | ETS/beam_lib; catch handles a stale-table badarg |
| beamtalk_test_support/beamtalk_stderr_capture.erl:47 | capture/1 | `Fun()` (EUnit test fun, in-process) | not-applicable-reraises | catches any exception, restores `standard_error`, then `erlang:raise/3`s the original class/reason/stack. `$bt_nlr` throws are also re-raised. Nothing is swallowed. |
| beamtalk_test_actor_registry.erl:30 | with_registry/1 | `Fun(Pid)` (test fun) | not-applicable-reraises | `try/after` with no catch clause; the exception propagates |
| beamtalk_test_actor_registry.erl:33 | with_registry/1 (old-style `catch gen_server:stop(Pid)`) | `gen_server:stop/1` | not-applicable-no-block | gen_server stop of another process |
| beamtalk_test_actor_registry.erl:42 | with_registered/2 | `Fun()` (test fun) | not-applicable-reraises | `try/after`, no catch clause |
| beamtalk_test_actor_registry.erl:45 | with_registered/2 (`catch unregister(?NAME)`) | `unregister/1` | not-applicable-no-block | BIF |
| beamtalk_test_actor_registry.erl:78 | end_isolated/0 (`catch gen_server:stop(Fresh)`) | `gen_server:stop/1` | not-applicable-no-block | other process |
| beamtalk_test_actor_registry.erl:81 | end_isolated/0 (`catch unregister`) | `unregister/1` | not-applicable-no-block | BIF |
| beamtalk_test_actor_registry.erl:84 | end_isolated/0 (`catch register`) | `register/2` | not-applicable-no-block | BIF |
| beamtalk_test_actor_registry.erl:97 | with_swapped/1 | `Fun()` (test fun) | not-applicable-reraises | `try/after`, no catch clause |
| beamtalk_test_actor_registry.erl:102 | with_swapped/1 (`catch unregister`) | `unregister/1` | not-applicable-no-block | BIF |
| beamtalk_test_actor_registry.erl:109 | with_swapped/1 (`catch register`) | `register/2` | not-applicable-no-block | BIF |

The other test_support modules (`beamtalk_test_boot`, `_corpus`, `_dynamic_class`, `_erl_forms`, `_unique`) contain no try/catch (grep hits were prose or `receive ... after`).

#### Counts
Total 101 sites. Of these, 72 are not-applicable-no-block, 24 are not-applicable-other-process, and 5 are not-applicable-reraises. The reraises are stderr_capture:47, registry:30, :42 and :97. One further row, compiler_server:1382, is also a re-raise, and it is counted in the 73 above (not-applicable-no-block). The table lists it as not-applicable-reraises, so that gives 5 reraises and 72 no-block. The numbers are 72 / 24 / 5. There are 0 converted, 0 NEEDS-CONVERSION, 0 unclear and 0 deliberately-left-candidate.

#### NEEDS-CONVERSION and unclear sites
None. Two watch items, neither needing conversion:
- `beamtalk_compiler_server.erl:1358` calls `Module:'__beamtalk_meta'()` in a swallowing catch. It is a generated literal-map accessor with no class-variable access, so no snapshot or restore applies. If a codegen change ever made `__beamtalk_meta/0` run class-method code, revisit.
- `beamtalk_stderr_capture:capture/1` runs a caller fun in-process and catches all exceptions, but it always re-raises, so no class-variable state is discarded without notice. It does not use `protect/1`. If an EUnit test wraps it around a Beamtalk class method, the class vars of the failed region are not restored. That is acceptable because the test then fails.
