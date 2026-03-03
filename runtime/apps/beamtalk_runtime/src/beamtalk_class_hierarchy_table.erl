%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc ETS table owner for the beamtalk_class_hierarchy table.
%%%
%%% **DDD Context:** Runtime — Class System
%%%
%%% Encapsulates all direct ETS access to the `beamtalk_class_hierarchy` table,
%%% which stores `{ClassName, SuperclassName | none}` pairs for O(1) hierarchy
%%% queries.  No other module may reference the `beamtalk_class_hierarchy` atom
%%% directly.
%%%
%%% Extracted from `beamtalk_class_registry` (BT-1062) following the same
%%% pattern used for `beamtalk_session_table`.
%%%
%%% ## Table properties
%%%
%%% * Type: `set` — one superclass per class name.
%%% * Access: `public`, `named_table` — any process can read; written only
%%%   during class init/terminate via this module's API.
%%% * `{read_concurrency, true}` — optimised for the frequent hierarchy-walk
%%%   reads performed by `beamtalk_class_registry` and dispatch.
%%%
%%% ## Concurrency
%%%
%%% `new/0` uses a try/catch around `ets:new/2` to be safe under concurrent
%%% first-use (TOCTOU race between `ets:info/1` and `ets:new/2`).
-module(beamtalk_class_hierarchy_table).

-export([
    new/0,
    insert/2,
    delete/1,
    lookup/1,
    foldl/2,
    match_subclasses/1
]).

-type class_name() :: atom().
-type superclass() :: class_name() | none.

%%====================================================================
%% API
%%====================================================================

%% @doc Ensure the class hierarchy ETS table exists (idempotent).
%%
%% Creates the table on first call; subsequent calls are no-ops.
%% Safe to call concurrently — a try/catch handles the race between
%% `ets:info/1` and `ets:new/2`.
-spec new() -> ok.
new() ->
    case ets:info(beamtalk_class_hierarchy) of
        undefined ->
            try
                ets:new(
                    beamtalk_class_hierarchy,
                    [set, public, named_table, {read_concurrency, true}]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

%% @doc Record a class and its superclass in the hierarchy table.
%%
%% Inserts `{ClassName, Superclass}`.  Superclass may be `none` for root
%% classes.  Overwrites any existing entry (ETS `set` semantics).
-spec insert(class_name(), superclass()) -> ok.
insert(ClassName, Superclass) ->
    ets:insert(beamtalk_class_hierarchy, {ClassName, Superclass}),
    ok.

%% @doc Remove a class entry from the hierarchy table.
%%
%% Called during class gen_server `terminate/2` to keep the table clean.
%% Safe to call even if the entry does not exist.
-spec delete(class_name()) -> ok.
delete(ClassName) ->
    ets:delete(beamtalk_class_hierarchy, ClassName),
    ok.

%% @doc Look up the superclass of a class.
%%
%% Returns `{ok, Superclass}` if the class is in the hierarchy table,
%% or `not_found` otherwise.  Returns `not_found` (rather than crashing)
%% if the table does not exist yet — safe during bootstrap.
-spec lookup(class_name()) -> {ok, superclass()} | not_found.
lookup(ClassName) ->
    case ets:info(beamtalk_class_hierarchy) of
        undefined ->
            not_found;
        _ ->
            case ets:lookup(beamtalk_class_hierarchy, ClassName) of
                [{_, Super}] -> {ok, Super};
                [] -> not_found
            end
    end.

%% @doc Fold over all `{ClassName, Superclass}` entries in the table.
%%
%% Returns `Acc0` if the table does not exist (safe during bootstrap).
%% The accumulator function receives `{ClassName, Superclass}` tuples.
-spec foldl(fun(({class_name(), superclass()}, Acc) -> Acc), Acc) -> Acc.
foldl(Fun, Acc0) ->
    case ets:info(beamtalk_class_hierarchy) of
        undefined ->
            Acc0;
        _ ->
            ets:foldl(Fun, Acc0, beamtalk_class_hierarchy)
    end.

%% @doc Return all class names whose direct superclass is `ClassName`.
%%
%% Used by `beamtalk_class_registry:direct_subclasses/1`.
%% Returns `[]` if the table does not exist.
-spec match_subclasses(class_name()) -> [class_name()].
match_subclasses(ClassName) ->
    case ets:info(beamtalk_class_hierarchy) of
        undefined ->
            [];
        _ ->
            Matches = ets:match(beamtalk_class_hierarchy, {'$1', ClassName}),
            [Name || [Name] <- Matches]
    end.
