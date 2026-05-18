%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_module_table).

%%% **DDD Context:** Object System Context

-moduledoc """
ETS table owner for the beamtalk_class_module table.

Encapsulates all direct ETS access to the `beamtalk_class_module` table,
which stores `{ClassName, ModuleName}` pairs for O(1) module-name lookups
that do not require a gen_server call.

## Motivation (BT-1285)

During supervisor `static_init`, the hierarchy walk in
`beamtalk_supervisor:call_inherited_class_method_direct/5` needs to resolve
the module name of ancestor classes.  Using `beamtalk_object_class:module_name/1`
(which does a `gen_server:call`) can deadlock when the ancestor is itself a
Supervisor subclass whose class gen_server is blocked inside `startLink/1`
waiting for OTP `supervisor:start_link` to return.

Storing module names in ETS lets the hierarchy walk resolve ancestor module
names without ever touching a potentially-blocked class gen_server.

## Table properties

* Type: `set` — one module name per class name.
* Access: `public`, `named_table` — any process can read; written only
  during class init/terminate/update via this module's API.
* `{read_concurrency, true}` — optimised for the frequent lookups performed
  during supervisor init and hierarchy walks.
* `{heir, beamtalk_runtime_sup, undefined}` (BT-1888 / BT-2008 pattern) —
  the table survives if the owner class process crashes, so module-name
  lookups don't degrade after an owner death.

## Concurrency

`new/0` uses a try/catch around `ets:new/2` to be safe under concurrent
first-use (TOCTOU race between `ets:info/1` and `ets:new/2`). All
read/write ops also guard against `badarg` from a TOCTOU race where the
table is deleted between an existence check and the actual ETS op
(possible during test teardown or shutdown).
""".

-export([
    new/0,
    insert/2,
    delete/1,
    lookup/1
]).

-type class_name() :: atom().

%%====================================================================
%% API
%%====================================================================

-doc """
Ensure the class module ETS table exists (idempotent).

Creates the table on first call; subsequent calls are no-ops.
Safe to call concurrently — a try/catch handles the race between
`ets:info/1` and `ets:new/2`.
""".
-spec new() -> ok.
new() ->
    case ets:info(beamtalk_class_module) of
        undefined ->
            try
                ets:new(
                    beamtalk_class_module,
                    [
                        set,
                        public,
                        named_table,
                        {read_concurrency, true}
                        | beamtalk_class_registry:heir_option()
                    ]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            beamtalk_class_registry:maybe_set_heir(beamtalk_class_module),
            ok
    end.

-doc """
Record a class and its module name in the table.

Inserts `{ClassName, ModuleName}`.  Overwrites any existing entry
(ETS `set` semantics), so re-loading a class updates the entry in place.

Calls `new/0` first to ensure the table exists.  This guards against the
rare case where the table was deleted (e.g., by test teardown) between the
class gen_server's `init/1` and a subsequent hot-reload `update_class` call.
""".
-spec insert(class_name(), module()) -> ok.
insert(ClassName, ModuleName) ->
    new(),
    try
        ets:insert(beamtalk_class_module, {ClassName, ModuleName}),
        ok
    catch
        error:badarg ->
            %% Table vanished between new/0 and ets:insert/2 — recreate and retry once.
            new(),
            ets:insert(beamtalk_class_module, {ClassName, ModuleName}),
            ok
    end.

-doc """
Remove a class entry from the table.

Called during class gen_server `terminate/2` to keep the table clean.
Safe to call even if the entry — or the table itself — does not exist.
""".
-spec delete(class_name()) -> ok.
delete(ClassName) ->
    try
        ets:delete(beamtalk_class_module, ClassName),
        ok
    catch
        error:badarg -> ok
    end.

-doc """
Look up the module name for a class.

Returns `{ok, ModuleName}` if the class is in the table,
or `not_found` otherwise.  Returns `not_found` (rather than crashing)
if the table does not exist yet (safe during bootstrap) or if it
disappears between the existence check and the lookup (TOCTOU under
teardown).
""".
-spec lookup(class_name()) -> {ok, module()} | not_found.
lookup(ClassName) ->
    case ets:info(beamtalk_class_module) of
        undefined ->
            not_found;
        _ ->
            try ets:lookup(beamtalk_class_module, ClassName) of
                [{_, Module}] -> {ok, Module};
                [] -> not_found
            catch
                error:badarg -> not_found
            end
    end.
