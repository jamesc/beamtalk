%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_scan).

%%% **DDD Context:** Actor System Context

-moduledoc """
Enumerate live actors by scanning the node's processes (ADR 0150, option D).

Under ADR 0150 actor start stops registering the actor anywhere. The readers
that need to enumerate actors (`allInstances`, hot reload,
`stop_class_actors`, the actor lists) scan with this module instead, once each
has been moved onto it. They scan with `erlang:processes_iterator/0` and one
`erlang:process_info/2` call per process reading the `'$beamtalk_actor'`
class marker and the `'$beamtalk_actor_started_at'` start time that an
actor's `init/1` writes. Cost is O(processes in the node), about 2.5-4.5 µs
per process, whatever fraction are actors.

An entry is `{Pid, Class, StartedAt}`; `StartedAt` is microseconds since the
epoch, or `undefined` when the process never wrote it (a `native:` actor, or an
actor whose `init/1` has not reached the write).

## Native actors

A `native:` backing gen_server (ADR 0056) plants no marker. The scan matches
its `'$initial_call'` through the backing-module index, via
`beamtalk_actor_registry:native_class_candidates/1` (the one implementation,
shared with `pid_class_candidates/1`). A backing module shared by several
classes yields one entry per live class, so `scan_class/1` finds the pid under
each of them while `scan_actors/0` lists the pid once per class.

## Guarantees

- A process that exits mid-scan is skipped (`process_info/2` answers
  `undefined`); the scan never crashes on one.
- A scan is not a snapshot: actors starting or stopping during it may or may
  not appear.
- Read-your-writes is weaker than a registry's. An actor is visible once its
  `init/1` has written the marker. A synchronous spawn returns after that, so
  its actor is in the next scan. An asynchronous spawn
  (`safe_spawn_monitored_async/2`) returns before `init/1` runs, so an
  immediate scan may miss it.
- Local node only (`process_info/2` raises `badarg` for a remote pid).
""".

-export([
    info/1,
    scan_actors/0,
    scan_class/1,
    scan_classes/1
]).

-export_type([entry/0]).

-type entry() :: {pid(), Class :: atom(), StartedAt :: integer() | undefined}.

-define(INFO_ITEMS, [
    {dictionary, '$beamtalk_actor'},
    {dictionary, '$beamtalk_actor_started_at'},
    {dictionary, '$initial_call'}
]).

-doc "Every live actor on this node, as `{Pid, Class, StartedAt}`.".
-spec scan_actors() -> [entry()].
scan_actors() ->
    scan(fun(Entry, Acc) -> [Entry | Acc] end, []).

-doc "The live instances of `Class` (exactly that class, no descendants) as `{Pid, StartedAt}`.".
-spec scan_class(atom()) -> [{pid(), integer() | undefined}].
scan_class(Class) when is_atom(Class) ->
    maps:get(Class, scan_classes([Class])).

-doc """
One pass over the node for several classes at once (hot reload). Every
requested class is a key of the result, with `[]` when it has no instances.
""".
-spec scan_classes([atom()]) -> #{atom() => [{pid(), integer() | undefined}]}.
scan_classes(Classes) when is_list(Classes) ->
    Wanted = maps:from_keys(Classes, []),
    scan(
        fun({Pid, Class, StartedAt}, Acc) ->
            case Acc of
                #{Class := Instances} -> Acc#{Class := [{Pid, StartedAt} | Instances]};
                _ -> Acc
            end
        end,
        Wanted
    ).

-doc """
The entries of one local pid, without a scan: `process_info/2` on the pid plus
the marker check. `[]` for a dead process or a non-actor; more than one entry
only for a `native:` backing module shared by several classes.
""".
-spec info(pid()) -> [entry()].
info(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    entries(Pid).

%%% ---------------------------------------------------------------------
%%% Internal
%%% ---------------------------------------------------------------------

-spec scan(fun((entry(), Acc) -> Acc), Acc) -> Acc when Acc :: term().
scan(Fun, Acc0) ->
    scan_loop(erlang:processes_iterator(), Fun, Acc0).

scan_loop(Iter0, Fun, Acc) ->
    case erlang:processes_next(Iter0) of
        none ->
            Acc;
        {Pid, Iter} ->
            scan_loop(Iter, Fun, lists:foldl(Fun, Acc, entries(Pid)))
    end.

%% A process that exited between the iterator and here answers `undefined`.
-spec entries(pid()) -> [entry()].
entries(Pid) ->
    case erlang:process_info(Pid, ?INFO_ITEMS) of
        [{_, Class}, {_, StartedAt}, _] when is_atom(Class), Class =/= undefined ->
            [{Pid, Class, started_at(StartedAt)}];
        [_, {_, StartedAt}, {_, InitialCall}] ->
            [
                {Pid, Class, started_at(StartedAt)}
             || Class <- beamtalk_actor_registry:native_class_candidates(InitialCall)
            ];
        _ ->
            []
    end.

started_at(T) when is_integer(T) -> T;
started_at(_) -> undefined.
