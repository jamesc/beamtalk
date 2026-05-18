%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_methods_table).

%%% **DDD Context:** Object System Context

-moduledoc """
ETS table owner for the beamtalk_class_methods table.

Encapsulates all direct ETS access to the `beamtalk_class_methods` table,
which stores `{ClassName, Module, ClassMethodSelectors}` triples for O(1)
lookups during inherited class-method dispatch — letting the chain walk
in `beamtalk_class_dispatch:find_class_method_in_ancestors/3` resolve
both the defining module and the local class-method selector set without
making a `gen_server:call`.

## Motivation (BT-2008)

`beamtalk_class_dispatch:class_self_dispatch/4` (BT-2007) walks the
superclass chain to route inherited class-method self-sends. Each
ancestor hop originally cost two `gen_server:call`s (one for the local
class-method map, one for the module name) — about 5–6µs per inherited
self-send for a two-level chain like `Dictionary → Collection`.

Mirroring the same data into a public ETS `set` collapses both lookups
into a single `ets:lookup/2`, dropping the inherited-dispatch cost into
the sub-microsecond range and bringing it close to the cost of a direct
`erlang:apply/3`.

The duplicated `Module` field (also stored in `beamtalk_class_module`)
is deliberate: it lets the chain walker resolve module + selectors in a
single ETS read on the hot path. Both tables are kept in sync from the
same writer (`beamtalk_object_class:init/1` and `apply_class_info/2`).

## Table properties

* Type: `set` — one entry per class name.
* Access: `public`, `named_table` — any process can read; written only
  during class init/terminate/update via this module's API.
* `{read_concurrency, true}` — optimised for the frequent reads on the
  class-dispatch hot path.

## Concurrency

`new/0` uses a try/catch around `ets:new/2` to be safe under concurrent
first-use (TOCTOU race between `ets:info/1` and `ets:new/2`).
""".

-export([
    new/0,
    insert/3,
    delete/1,
    lookup/1
]).

-type class_name() :: atom().
-type selector() :: atom().

%%====================================================================
%% API
%%====================================================================

-doc """
Ensure the class methods ETS table exists (idempotent).

Creates the table on first call; subsequent calls are no-ops.
Safe to call concurrently — a try/catch handles the race between
`ets:info/1` and `ets:new/2`.
""".
-spec new() -> ok.
new() ->
    case ets:info(beamtalk_class_methods) of
        undefined ->
            try
                ets:new(
                    beamtalk_class_methods,
                    [set, public, named_table, {read_concurrency, true}]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

-doc """
Record a class's module and local class-method selectors in the table.

Inserts `{ClassName, Module, Selectors}`. Overwrites any existing entry
(ETS `set` semantics), so re-loading a class or redefining its class
methods updates the entry in place.

Calls `new/0` first to ensure the table exists. Guards against the rare
case where the table was deleted (e.g., by test teardown) between the
class gen_server's `init/1` and a subsequent hot-reload `update_class`
call.
""".
-spec insert(class_name(), module(), [selector()]) -> ok.
insert(ClassName, Module, Selectors) when is_list(Selectors) ->
    new(),
    ets:insert(beamtalk_class_methods, {ClassName, Module, Selectors}),
    ok.

-doc """
Remove a class entry from the table.

Called during class gen_server `terminate/2` to keep the table clean.
Safe to call even if the entry does not exist.
""".
-spec delete(class_name()) -> ok.
delete(ClassName) ->
    ets:delete(beamtalk_class_methods, ClassName),
    ok.

-doc """
Look up the module and local class-method selectors for a class.

Returns `{ok, Module, Selectors}` if the class is in the table, or
`not_found` otherwise. Returns `not_found` (rather than crashing) if the
table does not exist yet — safe during bootstrap.
""".
-spec lookup(class_name()) -> {ok, module(), [selector()]} | not_found.
lookup(ClassName) ->
    case ets:info(beamtalk_class_methods) of
        undefined ->
            not_found;
        _ ->
            case ets:lookup(beamtalk_class_methods, ClassName) of
                [{_, Module, Selectors}] -> {ok, Module, Selectors};
                [] -> not_found
            end
    end.
