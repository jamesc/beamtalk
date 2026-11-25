%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_boot).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: boot the real runtime + stdlib for EUnit suites that
need a genuinely compiled class (not a hand-written `.erl` test double) —
e.g. `beamtalk_workspace`'s `beamtalk_repl_docs_tests` (doc-formatting
integration tests) and `beamtalk_repl_ops_browse_tests` (a native-class
delegate-callers regression test), and `beamtalk_stdlib`'s
`beamtalk_test_case_stdlib_tests` (a BIF-fallback-path regression
test against a real compiled `TestCase` subclass).

Originally `beamtalk_workspace_test_boot`, living under
`beamtalk_workspace/test/` — moved here (mirroring `beamtalk_test_corpus`'s
precedent, see its `moduledoc` for the "why a standalone app" rationale) once
`beamtalk_stdlib`'s EUnit suite needed the same boot sequence too. Reaching
into a peer/dependent app's `test/` tree from another app's suite would
recreate the duplication `beamtalk_test_support` exists to avoid.

Guarded so it is safe to call from more than one test module in the same
EUnit VM: `beamtalk_bootstrap:start_link/0` is only invoked when no bootstrap
process is already registered, and `beamtalk_stdlib:init/0` is itself
idempotent (a normal stdlib load skips a module whose on_load already ran).
""".

-export([
    boot_real_stdlib/1,
    reload_stdlib_modules/0,
    wait_for_class/2,
    registry/0,
    start_app/1,
    compiler/0,
    app_owned_table/1,
    no_compiler/0,
    telemetry/0,
    load_runtime_modules/0,
    workspace/1,
    clear_compiler_classes/0
]).

-doc """
Boot `beamtalk_runtime` + real stdlib, then block until `CanaryClass` is
registered as a live class process — the caller's signal that the compiled
module backing it has finished loading (on_load → register_class/0).

Exported alongside `wait_for_class/2` so a caller can also wait on a second,
later-loaded class within the same test (e.g. one loaded lazily after the
canary), without re-booting.
""".
-spec boot_real_stdlib(atom()) -> ok.
boot_real_stdlib(CanaryClass) ->
    {ok, _} = start_app(beamtalk_runtime),
    case whereis(beamtalk_bootstrap) of
        undefined ->
            case beamtalk_bootstrap:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _ ->
            ok
    end,
    beamtalk_stdlib:init(),
    ok = repair_class_pid_index(),
    case class_registered_within(CanaryClass, 500) of
        true ->
            ok;
        false ->
            %% The stdlib modules are loaded but their class processes are gone
            %% (an earlier suite stopped the runtime or its class registry), and
            %% `stdlib:init/0` skips a module that is already loaded. Unload the
            %% stdlib modules so their `on_load` hooks register the classes (and
            %% their xref rows) again (BT-4934).
            ok = reload_stdlib_modules(),
            beamtalk_stdlib:init(),
            wait_for_class(CanaryClass, 50)
    end,
    ok.

-doc """
Make sure the boot table `Table` is owned and heir-protected the way
`beamtalk_runtime_app:start/2` leaves it. A module that loads stdlib code before
the runtime app is up creates such tables from a test or class process with no
heir (`beamtalk_ets_heir:heir_option/0` is empty then), and starting the app later
cannot add one (`maybe_set_heir/1` only works for the owner). When that happened
(a table with no heir), restart the app with the table removed so the app creates
it, and register the stdlib classes again. A no-op otherwise (BT-4962).
""".
-spec app_owned_table(atom()) -> ok.
app_owned_table(Table) ->
    case ets:info(Table, heir) of
        none ->
            _ = application:stop(beamtalk_runtime),
            try
                ets:delete(Table)
            catch
                error:badarg -> ok
            end,
            boot_real_stdlib('Integer');
        _ ->
            ok
    end.

-spec wait_for_class(atom(), non_neg_integer()) -> ok.
wait_for_class(ClassName, Retries) ->
    beamtalk_test_wait:class(ClassName, Retries * 50).

%% Stopping the runtime application stops `beamtalk_ets_heir`, which owns the
%% shared registry tables (`beamtalk_class_pids`, ...); they die with it. Class
%% processes that outlive the app are never re-recorded, so they read as
%% unregistered (`class_identity/1` -> skip) even though they are in `pg`. In a
%% release the heir never stops; here every live class is recorded again
%% (BT-4934).
repair_class_pid_index() ->
    ok = beamtalk_class_registry:ensure_pid_table(),
    lists:foreach(
        fun(Pid) ->
            try beamtalk_object_class:class_name(Pid) of
                Name when is_atom(Name) ->
                    beamtalk_class_registry:record_class_pid(Pid, Name)
            catch
                exit:_ -> ok
            end
        end,
        beamtalk_class_registry:all_classes()
    ).

class_registered_within(ClassName, TimeoutMs) ->
    try beamtalk_test_wait:class(ClassName, TimeoutMs) of
        ok -> true
    catch
        error:{class_not_registered, ClassName} -> false
    end.

-doc """
Unload every loaded `bt@stdlib@*` module so the next `beamtalk_stdlib:init/0`
runs their `on_load` hooks again (class registration and baked xref rows).
`init/0` skips a module that is already loaded, so a suite that asserts a cold
stdlib boot calls this first.
""".
-spec reload_stdlib_modules() -> ok.
reload_stdlib_modules() ->
    lists:foreach(
        fun beamtalk_test_module:unload/1,
        [M || {M, _} <- code:all_loaded(), is_stdlib_module(M)]
    ).

is_stdlib_module(Module) ->
    lists:prefix("bt@stdlib@", atom_to_list(Module)).

-doc """
Bring up just the class-registry prerequisites (the `pg` scope and every
`beamtalk_class_registry` ETS table) without starting the runtime
application — the boot ~60 suites copy by hand (BT-4677). Idempotent.

Returns a teardown handle: calling it stops `pg` only if this call started
it. (The ETS tables are heir-owned and deliberately survive teardown, as the
hand-written boots did.)
""".
-spec registry() -> fun(() -> ok).
registry() ->
    Started =
        case whereis(pg) of
            undefined ->
                %% Unlinked: `start_link` would tie `pg` to the short-lived EUnit
                %% fixture process, and when that exits `pg` dies with it, losing
                %% every class registration made in this node while the class
                %% processes and their loaded modules live on (BT-4934).
                {ok, _} = pg:start(pg),
                true;
            _ ->
                false
        end,
    beamtalk_class_metadata:new(),
    beamtalk_class_registry:ensure_class_warnings_table(),
    beamtalk_class_registry:ensure_pid_table(),
    beamtalk_class_registry:ensure_pending_errors_table(),
    beamtalk_class_registry:ensure_loaded_classes_table(),
    beamtalk_class_registry:ensure_backing_module_index_table(),
    beamtalk_class_registry:ensure_static_call_keys_table(),
    fun() ->
        case Started andalso whereis(pg) of
            Pid when is_pid(Pid) ->
                unlink(Pid),
                gen_server:stop(Pid);
            _ ->
                ok
        end
    end.

-doc """
`application:ensure_all_started(App)` for a test setup: asserts the start, and
recovers from the one failure an earlier test in the same node causes. A suite
that starts a supervised server bare (`beamtalk_xref:start_link()`,
`beamtalk_announcements`, ...) and leaves it running makes the application's
own start fail with `{failed_to_start_child, _, {already_started, Pid}}`; the
stray process is stopped and the start retried, so the app really comes up
(BT-4922, BT-4934). Any other failure crashes the setup.
""".
-spec start_app(atom()) -> {ok, [atom()]}.
start_app(App) ->
    start_app(App, 8).

start_app(App, Retries) ->
    case application:ensure_all_started(App) of
        {ok, _} = Ok when App =:= beamtalk_runtime ->
            ok = reclaim_class_owned_tables(),
            ok = ensure_bootstrap_classes(),
            ok = ensure_boot_tables(),
            Ok;
        {ok, _} = Ok ->
            Ok;
        {error, {_, {{shutdown, {failed_to_start_child, _Child, {already_started, Pid}}}, _}}} when
            is_pid(Pid), Retries > 0
        ->
            ok = beamtalk_test_proc:stop(Pid),
            start_app(App, Retries - 1);
        {error, Reason} ->
            error({app_start_failed, App, Reason})
    end.

%% Boot tables that a test process created before the runtime app was up are
%% owned by that process and vanish with it (`badarg` on `ets:lookup/2` in a later
%% module), while the app itself is "healthy" and never recreates them. The heir
%% is alive now, so the idempotent `init/0` of the registries tests have been seen
%% to lose recreates a missing table with the heir (BT-4962).
ensure_boot_tables() ->
    _ = beamtalk_protocol_registry:init(),
    ok.

%% A suite can leave the runtime application "started" but unhealthy, and a
%% second `ensure_all_started` is then a no-op, so no later suite ever sees it
%% repaired: the bootstrap `Object` class stopped on purpose
%% (`class_not_registered 'Object'`), or a supervised server (the announcement
%% bus, ...) killed and replaced by a bare `start_link` instance that is not the
%% supervisor's child, which a restarted dependent never sees. Restart the
%% application once so its children and the bootstrap classes are registered
%% again (BT-4934).
ensure_bootstrap_classes() ->
    case runtime_healthy() of
        true -> ok;
        false -> restart_runtime()
    end.

restart_runtime() ->
    ok = application:stop(beamtalk_runtime),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    wait_for_class('Object', 100),
    %% Stopping the app took every class process with it, but stdlib
    %% modules stay loaded and `stdlib:init/0` skips loaded modules:
    %% reload them so `Integer`, `Subprocess`, ... register again.
    ok = reload_stdlib_modules(),
    beamtalk_stdlib:init(),
    wait_for_class('Integer', 100),
    ok = repair_class_pid_index().

%% A stdlib class loaded before the runtime app was up (an `on_load` in a test
%% process) creates the boot tables from its class process, with no heir, and the
%% app started later keeps using them. The first test that stops that class (e.g.
%% `restart_class/2` on `Counter`) then destroys every class table with it. Stop
%% those class processes and restart the app so it owns the tables, with the heir
%% (BT-4962).
reclaim_class_owned_tables() ->
    Orphans = lists:usort([
        Owner
     || T <- ets:all(),
        is_atom(T),
        ets:info(T, heir) =:= none,
        Owner <- [ets:info(T, owner)],
        is_pid(Owner),
        beamtalk_test_class_teardown:is_class_process(Owner)
    ]),
    case Orphans of
        [] ->
            ok;
        _ ->
            %% Every class process predates the app and is stale, not only the table
            %% owners: a bare `Object` left running keeps its registered name, so the
            %% restarted stdlib's `Object` never registers.
            beamtalk_test_class_teardown:stop_pg_class_processes(),
            restart_runtime()
    end.

%% `Object` is registered, the system announcer still holds its subscription on
%% the announcement bus (a suite that wipes every subscription silences system
%% events such as `NodeShapeSkew` for everyone after it), and every supervisor
%% child that registers a name equal to its id is the process registered under
%% that name.
runtime_healthy() ->
    is_pid(beamtalk_class_registry:whereis_class('Object')) andalso
        system_announcer_subscribed() andalso
        lists:all(
            fun
                ({Id, Pid, _Type, _Mods}) when is_atom(Id), is_pid(Pid) ->
                    case whereis(Id) of
                        undefined -> true;
                        Registered -> Registered =:= Pid
                    end;
                ({_Id, _Restarting, _Type, _Mods}) ->
                    false
            end,
            supervisor:which_children(beamtalk_runtime_sup)
        ).

%% `beamtalk_announcements:subscription_count/0` is exported only under
%% `-ifdef(TEST)`; this module lives in `src/` (compiled without TEST), so
%% dialyzer cannot see it. The code that calls this runs with TEST defined.
-dialyzer({no_missing_calls, system_announcer_subscribed/0}).
system_announcer_subscribed() ->
    try
        beamtalk_announcements:subscription_count() > 0
    catch
        _:_ -> false
    end.

-doc """
Start the compiler application and its `beamtalk_compiler` port server
(tolerating an already-started one). Replaces the three-line
`ensure_all_started(compiler)` / `case ensure_all_started(beamtalk_compiler)`
sequence repeated across the integration suites.
""".
-spec compiler() -> ok.
compiler() ->
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = start_app(beamtalk_compiler),
    ok.

-doc """
Load every `beamtalk_runtime` module without starting any process. A release
loads all modules at boot (embedded mode), a bare EUnit node loads them on first
use, and each lazy load is a `code_server` call: suites that assert "no
`code_server` call" preload with this first. Idempotent.
""".
-spec load_runtime_modules() -> ok.
load_runtime_modules() ->
    case application:load(beamtalk_runtime) of
        ok -> ok;
        {error, {already_loaded, beamtalk_runtime}} -> ok
    end,
    {ok, Modules} = application:get_key(beamtalk_runtime, modules),
    lists:foreach(fun(M) -> {module, M} = code:ensure_loaded(M) end, Modules).

-doc """
Start the `telemetry` and `telemetry_poller` applications that
`beamtalk_runtime` lists as dependencies, for suites that start a supervisor
(`beamtalk_runtime_sup`, `beamtalk_trace_store`) without going through
`application:ensure_all_started(beamtalk_runtime)`. Asserts the start so a
missing dependency fails here and not as a `noproc` inside a child (BT-4934).
""".
-spec telemetry() -> ok.
telemetry() ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = application:ensure_all_started(telemetry_poller),
    ok.

-doc """
Establish "no compiler running": stop the compiler application and wait until
its server is gone. For tests of the no-compiler / `noproc` error paths: many
suites start the compiler and leave it up, so those tests must set the
precondition themselves instead of relying on suite order (BT-4934).
""".
-spec no_compiler() -> ok.
no_compiler() ->
    _ = application:stop(beamtalk_compiler),
    ok = beamtalk_test_wait:until(fun() -> whereis(beamtalk_compiler_server) =:= undefined end),
    ok.

-doc """
Boot a fresh, isolated workspace for the integration suites that drive the
real compiler: start `compiler`, `beamtalk_compiler` and `beamtalk_runtime`,
replace any running `beamtalk_workspace_meta` with a fresh one in **run
mode**, reset the xref and compiler class caches, then restart the named
singletons.

`mode => run` is load-bearing for test isolation, not tidiness: in REPL mode
(the default) `beamtalk_workspace_meta` persists `class_sources` to
`~/.beamtalk/workspaces/<workspace_id>/metadata.json` and reloads it on the
next `init/1`, so a "fresh" restart under the same id would resurrect a
previous test's `set_class_source/2` calls. Run mode skips disk entirely.

Options:
  * `workspace_id` (binary, required) — the meta process's workspace id.
  * `restart` (`[module()]`, default `[]`) — singletons restarted with
    `beamtalk_test_proc:restart/1`, in order (e.g. a findings store).
  * `clear_xref` (default `true`) — empty the `beamtalk_xref` tables.
  * `announcements` (default `false`) — ensure the SystemAnnouncer bus is up,
    so a test body's own `beamtalk_repl_subscriptions:subscribe/2` registers
    (it is silently skipped when the bus is not running yet).

Idempotent; pair with `beamtalk_test_proc:stop_registered/1` in teardown.
""".
-spec workspace(#{
    workspace_id := binary(),
    restart => [module()],
    clear_xref => boolean(),
    announcements => boolean()
}) -> ok.
workspace(#{workspace_id := WorkspaceId} = Opts) ->
    ok = compiler(),
    %% start_app/1, not a bare ensure_all_started: an earlier suite may have left a
    %% supervised server (beamtalk_xref via ensure_xref/0, ...) running bare, which
    %% makes the supervisor's own start fail with already_started (BT-4962).
    {ok, _} = start_app(beamtalk_runtime),
    ok = beamtalk_test_proc:stop_registered(beamtalk_workspace_meta),
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => undefined,
        created_at => erlang:system_time(second),
        mode => run
    }),
    case maps:get(clear_xref, Opts, true) of
        true -> beamtalk_test_repl:clear_xref();
        false -> ok
    end,
    ok = clear_compiler_classes(),
    [{ok, _} = beamtalk_test_proc:restart(Name) || Name <- maps:get(restart, Opts, [])],
    case maps:get(announcements, Opts, false) of
        true -> ok = beamtalk_announcements:ensure_started();
        false -> ok
    end,
    ok.

%% `beamtalk_compiler_server:clear_classes/0` is exported only under
%% `-ifdef(TEST)`; this module lives in `src/` (compiled without TEST), so
%% dialyzer cannot see it. The tests that call this run with TEST defined.
-dialyzer({no_missing_calls, clear_compiler_classes/0}).

-doc """
Clear the compiler server's ambient class and alias caches (ADR 0108). Only
callable from a test build, where `beamtalk_compiler_server` exports it.
""".
-spec clear_compiler_classes() -> ok.
clear_compiler_classes() ->
    beamtalk_compiler_server:clear_classes().
