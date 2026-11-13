%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_session).

%%% **DDD Context:** Object System Context

-moduledoc """
Session-context and group-leader seeding for class-method dispatch.

A class method such as `Session current` or `Workspace currentSession` reads the
caller's session keys (`beamtalk_session_pid`, `beamtalk_session_id`,
`beamtalk_session_meta`, `beamtalk_entry_group_leader`) from the process
dictionary, but dispatch hops from the caller to the class gen_server. These
functions mirror the caller's context into the *calling* process (the class
gen_server) for the duration of one call and return a zero-arity closure that
restores the previous values.

* `seed_session_context_from/1` seeds from the context carried in the
  class-method-call message and adopts the entry group leader.
* `seed_caller_session_context/1` is the legacy fallback for an in-flight
  3-tuple message: it reads the caller's dictionary via `process_info/2`.

Both mutate the process dictionary and group leader of `self()`, so they must
only be called from the class gen_server's own process.
""".

-export([
    seed_session_context_from/1,
    seed_caller_session_context/1,
    restore_dict_key/2
]).

-doc """
Seed this class gen_server with a session context passed explicitly in the
class-method-call message, returning a zero-arity restore closure.

ADR 0081: factory class methods like `Session current` /
`Workspace currentSession` read `beamtalk_session_pid` / `beamtalk_session_id` /
`beamtalk_session_meta` from the process dictionary. Class-method dispatch hops
from the eval worker to this gen_server, so the worker now sends its context
keys in the message tuple (`beamtalk_class_dispatch:local_session_context/0`)
rather than having us copy its entire dictionary via `process_info/2`. We
`put/2` them locally and restore the previous values (or erase) on the way out.

The 4-tuple shape adds the *entry group leader* — the IO sink a
connected `beamtalk run … --connect` dispatch streams its `Console` output to.
This gen_server is long-lived, so it inherited the node's group leader at spawn
and a class method's `Console` writes miss the dispatching session's IO capture
entirely. Adopting the caller's sink for the duration of the call fixes that;
`gen_server` serialises calls, so a concurrent call from another session queues
behind this one and adopts *its own* caller's sink rather than seeing ours. The
key is only ever set by the `run-entry` op, so every other dispatch path keeps
the group leader it has today.

The 3-tuple shape carries the origin metadata (`Meta`) so `Session current kind`
reports the real client surface. The 3- and 2-tuple clauses remain for in-flight
messages sent by a pre-upgrade caller across a hot-code reload (no metadata →
`Session current` falls back to `kind => unknown`); both delegate to the 4-tuple
clause with the missing fields as `undefined`, so every clause manages the same
key set as a unit and no stale value from a previous call can leak into this one.
""".
-spec seed_session_context_from(
    {pid() | undefined, binary() | undefined, map() | undefined, pid() | undefined}
    | {pid() | undefined, binary() | undefined, map() | undefined}
    | {pid() | undefined, binary() | undefined}
    | term()
) -> fun(() -> ok).
seed_session_context_from({SessionPid, SessionId, SessionMeta, EntryGroupLeader} = Ctx) ->
    case session_context_unchanged(Ctx) of
        true ->
            %% BT-4771: the process already holds exactly this context, so the
            %% put/restore pair would write back what is there. A pid sink is
            %% excluded (see `session_context_unchanged/1`).
            fun() -> ok end;
        false ->
            seed_session_context_changed(
                SessionPid, SessionId, SessionMeta, EntryGroupLeader
            )
    end;
seed_session_context_from({SessionPid, SessionId, SessionMeta}) ->
    seed_session_context_from({SessionPid, SessionId, SessionMeta, undefined});
seed_session_context_from({SessionPid, SessionId}) ->
    %% A 2-tuple context carries no metadata, so clear any leftover `meta` for
    %% the duration of this call (and restore it after). Managing all the keys
    %% as a unit means `Session current kind` inside this call can never read a
    %% stale other-session's metadata, regardless of what ran before.
    seed_session_context_from({SessionPid, SessionId, undefined, undefined});
seed_session_context_from(_Other) ->
    %% Unrecognised context shape: seed nothing rather than crash the call.
    fun() -> ok end.

%% True when the process dictionary already holds exactly `Ctx`. Readers use
%% `get/1`, so an absent key and one holding `undefined` are the same. An
%% entry group leader that is a pid is never "unchanged": adopting it also
%% depends on the live group leader and the sink's liveness, so that path
%% always runs in full.
-spec session_context_unchanged(
    {pid() | undefined, term(), term(), term()}
) -> boolean().
session_context_unchanged({SessionPid, SessionId, SessionMeta, EntryGroupLeader}) ->
    not is_pid(EntryGroupLeader) andalso
        get(beamtalk_session_pid) =:= SessionPid andalso
        get(beamtalk_session_id) =:= SessionId andalso
        get(beamtalk_session_meta) =:= SessionMeta andalso
        get(beamtalk_entry_group_leader) =:= EntryGroupLeader.

-spec seed_session_context_changed(term(), term(), term(), term()) -> fun(() -> ok).
seed_session_context_changed(SessionPid, SessionId, SessionMeta, EntryGroupLeader) ->
    PrevPid = put(beamtalk_session_pid, SessionPid),
    PrevId = put(beamtalk_session_id, SessionId),
    PrevMeta = put(beamtalk_session_meta, SessionMeta),
    %% Keep the key in the dictionary as well as adopting the sink, so
    %% a nested class-method call made from inside this one re-emits it via
    %% `beamtalk_class_dispatch:local_session_context/0` and keeps streaming.
    PrevEntryGl = put(beamtalk_entry_group_leader, EntryGroupLeader),
    RestoreGroupLeader = adopt_entry_group_leader(EntryGroupLeader),
    fun() ->
        RestoreGroupLeader(),
        restore_dict_key(beamtalk_session_pid, PrevPid),
        restore_dict_key(beamtalk_session_id, PrevId),
        restore_dict_key(beamtalk_session_meta, PrevMeta),
        restore_dict_key(beamtalk_entry_group_leader, PrevEntryGl),
        ok
    end.

-doc """
Adopt `EntryGroupLeader` as this process's group leader for the duration of one
class-method call, returning a zero-arity closure that restores the
previous one.

A no-op — returning a no-op closure — unless the caller passed a live local pid
that differs from the group leader we already have. The liveness check matters:
`group_leader/2` happily accepts a dead pid, and a method that then writes to
`standard_io` blocks until the io call gives up, so a client that disconnected
mid-dispatch would turn every `Console` write in the entry into an error. Falling
back to the group leader we already have loses the (undeliverable) output
instead. `group_leader/2` still raises `badarg` for a non-local pid and the sink
can exit between the check and the swap, so both the swap and the restore are
wrapped: losing the output sink must never crash the class gen_server.
""".
-spec adopt_entry_group_leader(pid() | undefined | term()) -> fun(() -> ok).
adopt_entry_group_leader(EntryGroupLeader) when
    is_pid(EntryGroupLeader), node(EntryGroupLeader) =:= node()
->
    Previous = group_leader(),
    Adopt =
        EntryGroupLeader =/= Previous andalso
            is_process_alive(EntryGroupLeader) andalso
            set_group_leader(EntryGroupLeader),
    case Adopt of
        true ->
            fun() ->
                _ = set_group_leader(Previous),
                ok
            end;
        false ->
            fun() -> ok end
    end;
adopt_entry_group_leader(_Other) ->
    fun() -> ok end.

%% Point this process's group leader at `Pid`, reporting whether it took effect.
%% `false` (rather than a crash) when `group_leader/2` rejects `Pid` — the same
%% defensive shape `beamtalk_io_capture:stop/1` uses when resetting inherited
%% group leaders.
-spec set_group_leader(pid()) -> boolean().
set_group_leader(Pid) ->
    try
        group_leader(Pid, self())
    catch
        error:badarg -> false
    end.

-doc """
Mirror the calling process's session context into this class gen_server for the
duration of a class-method call, returning a zero-arity closure that restores
the prior state.

ADR 0081: `seed_session_context/3` seeds the eval *worker*, but
class-method dispatch (`Session current`, `Workspace currentSession`) hops to
this gen_server, where those keys are absent. We read them from the caller via
`process_info/2` (no message-shape change) and `put/2` them locally, restoring
the previous values (or erasing) on the way out so concurrent calls from other
sessions never see a stale context.

Deliberately leaves `beamtalk_entry_group_leader` alone: only a
legacy caller sends the 3-arity message this seeds from, and such a caller
never set that key. It cannot read a stale one either — `gen_server` serialises
calls and `seed_session_context_from/1` always restores the key before its call
returns, so the key is only ever set *during* a 4-tuple call.
""".
-spec seed_caller_session_context({pid(), term()} | term()) -> fun(() -> ok).
seed_caller_session_context({CallerPid, _Tag}) when is_pid(CallerPid) ->
    case process_info(CallerPid, dictionary) of
        {dictionary, Dict} ->
            CallerPidCtx = proplists:get_value(beamtalk_session_pid, Dict),
            CallerIdCtx = proplists:get_value(beamtalk_session_id, Dict),
            CallerMetaCtx = proplists:get_value(beamtalk_session_meta, Dict),
            PrevPid = put(beamtalk_session_pid, CallerPidCtx),
            PrevId = put(beamtalk_session_id, CallerIdCtx),
            PrevMeta = put(beamtalk_session_meta, CallerMetaCtx),
            fun() ->
                restore_dict_key(beamtalk_session_pid, PrevPid),
                restore_dict_key(beamtalk_session_id, PrevId),
                restore_dict_key(beamtalk_session_meta, PrevMeta),
                ok
            end;
        undefined ->
            %% Caller already gone (dead before we ran): nothing to mirror.
            fun() -> ok end
    end;
seed_caller_session_context(_From) ->
    fun() -> ok end.

%% Restore a process-dictionary key to its previous value, or erase it if it had
%% none. `put/2` returns `undefined` for a key that was unset, which is exactly
%% the value we erase back to.
-spec restore_dict_key(atom(), term()) -> ok.
restore_dict_key(Key, undefined) ->
    erase(Key),
    ok;
restore_dict_key(Key, Prev) ->
    put(Key, Prev),
    ok.
