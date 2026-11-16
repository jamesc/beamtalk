%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_actors).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The workspace's actor lists, backed by the process scan (ADR 0150, option D).

The REPL `actors` op, `list-classes` actor counts, the `inspect` / `kill` /
`pid-stats` pid check and the WebSocket / LiveView connect snapshot all read
here. Each call is one `beamtalk_actor_scan` pass (or, for a single pid, one
`process_info/2`); nothing is registered, so there is no state to keep in step
with the actors themselves.

## Metadata

`#{pid, class, module, spawned_at, system}`. `module` is the backing module of
`class` (`undefined` when the class is not loaded). `spawned_at` is seconds
since the epoch, or `null` for an actor that never wrote a start time (a
`native:` actor, or one whose `init/1` has not run yet). `system` is `true` for
an actor the runtime started itself, `false` for every other.

## Which processes are listed

Every actor the scan finds, the same set `Node current actors` reports
(BT-4960, owner decision 16 Nov 2026): marked actors, and `native:` backing
gen_servers (ADR 0056), found through their `'$initial_call'`. Before ADR 0150
the retired list held only registered actors, so a `native:` actor was missing
from `:actors`; that was an artifact of registration, not a decision.

Each pid is listed once. A `native:` backing module shared by several classes
gives the scan one entry per class; the class shown is the alphabetically
first (`beamtalk_actor_scan:one_per_pid/1`, the rule `Node current actors`
uses too), so both lists agree on it.

## System actors

An actor started by the runtime itself, such as `TranscriptStream`, is a
child of `beamtalk_workspace_sup` (read from the process's own `'$ancestors'`,
so no call into the supervisor); it is listed with `system => true` (on the
wire the `system` key is present only when true). User actors, including
`native:` ones a user spawned, are `system => false`. `kill` refuses a system
actor (killing `Transcript` would drop its buffer and every subscriber);
`inspect` works.

## Read-your-writes

A scan sees an actor once its `init/1` has written the marker. A synchronous
spawn returns after that, so `:actors` right after it lists the actor. An
asynchronous spawn returns before `init/1` runs, so an immediate `:actors` may
miss it; the next call shows it. A hot reload needs no refresh step: every call
scans afresh.

## Snapshot plus feed

A scan is not a snapshot, and an incremental consumer must neither lose nor
double-count an actor that starts around it. The protocol:

1. subscribe to `ActorSpawned` / `ActorStopped` (`beamtalk_repl_subscriptions`);
2. take the snapshot (`list/0`) and `feed_new/1` from it;
3. pass each announcement through `feed_event/2`.

The feed covers marked actors only. `ActorSpawned` / `ActorStopped` come from
the lifecycle telemetry of a compiled actor's `init/1` / `terminate/2`, which a
`native:` backing gen_server does not run. A `native:` actor is in the snapshot
taken at connect, but a later start or stop of one is not announced; a client
sees it on its next `actors` call. System actors live as long as the workspace,
so this only matters for user `native:` actors.

An actor that started before the subscription is in the snapshot; one that
started after the scan is announced and not in it; one that started in between
is both, and `feed_event/2` drops the announcement the snapshot already
covers. The index lives in the subscriber, never in the runtime.
""".

-export([
    list/0,
    lookup/1,
    count_for_module/1,
    feed_new/1,
    feed_event/2
]).

-export_type([actor_metadata/0, feed/0]).

-type actor_metadata() :: #{
    pid := pid(),
    class := atom(),
    module := atom(),
    spawned_at := integer() | null,
    system := boolean()
}.

-opaque feed() :: #{pid() => true}.

-doc "Every live actor on this node: one scan, ordered by pid.".
-spec list() -> [actor_metadata()].
list() ->
    Metas = [meta(E) || E <- beamtalk_actor_scan:scan_actors_by_pid()],
    lists:sort(fun(#{pid := A}, #{pid := B}) -> A =< B end, Metas).

-doc """
The metadata of one actor, without a scan. A pid on another node, a dead
process or a non-actor is `{error, not_found}`.
""".
-spec lookup(pid()) -> {ok, actor_metadata()} | {error, not_found}.
lookup(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    case beamtalk_actor_scan:one_per_pid(beamtalk_actor_scan:info(Pid)) of
        [{Pid, _, _} = Entry] -> {ok, meta(Entry)};
        [] -> {error, not_found}
    end;
lookup(Pid) when is_pid(Pid) ->
    {error, not_found}.

-doc "How many live actors run `Module`'s code (one scan).".
-spec count_for_module(atom()) -> non_neg_integer().
count_for_module(Module) ->
    length([ok || #{module := M} <- list(), M =:= Module]).

-doc "Start a feed from a snapshot taken after subscribing.".
-spec feed_new([map()]) -> feed().
feed_new(Snapshot) ->
    maps:from_keys([Pid || #{pid := Pid} <- Snapshot], true).

-doc """
Filter one announcement against the snapshot. `drop` for the first `ActorSpawned`
of a pid the snapshot already holds; everything else is `emit`. A stop clears
the pid so a later spawn is never shadowed.
""".
-spec feed_event(map(), feed()) -> {emit | drop, feed()}.
feed_event(#{'$beamtalk_class' := 'ActorSpawned', pid := Pid}, Feed) when is_map_key(Pid, Feed) ->
    {drop, maps:remove(Pid, Feed)};
feed_event(#{'$beamtalk_class' := 'ActorStopped', pid := Pid}, Feed) ->
    {emit, maps:remove(Pid, Feed)};
feed_event(_Event, Feed) ->
    {emit, Feed}.

%%% ---------------------------------------------------------------------
%%% Internal
%%% ---------------------------------------------------------------------

-spec meta(beamtalk_actor_scan:entry()) -> actor_metadata().
meta({Pid, Class, StartedAt}) ->
    #{
        pid => Pid,
        class => Class,
        module => module_of(Class),
        spawned_at => spawned_at(StartedAt),
        system => is_system(Pid)
    }.

%% The runtime's own actors are the children of the workspace supervisor
%% (`Transcript`). Read from the process itself: `proc_lib` records the parent
%% first in `'$ancestors'` (the supervisor's registered name, or its pid when
%% unnamed). No call into the supervisor, so a busy or restarting supervisor
%% never stalls a pid check.
-spec is_system(pid()) -> boolean().
is_system(Pid) ->
    %% Keyed form: reads one entry instead of copying the whole dictionary. A
    %% dead pid answers `undefined`, a missing key `{{dictionary, _}, undefined}`.
    case erlang:process_info(Pid, {dictionary, '$ancestors'}) of
        {{dictionary, '$ancestors'}, [Parent | _]} -> is_workspace_sup(Parent);
        _ -> false
    end.

is_workspace_sup(beamtalk_workspace_sup) -> true;
is_workspace_sup(Parent) when is_pid(Parent) -> Parent =:= whereis(beamtalk_workspace_sup);
is_workspace_sup(_) -> false.

spawned_at(undefined) -> null;
spawned_at(Micros) -> Micros div 1000000.

-spec module_of(atom()) -> atom().
module_of(Class) ->
    beamtalk_class_registry:backing_module(Class).
