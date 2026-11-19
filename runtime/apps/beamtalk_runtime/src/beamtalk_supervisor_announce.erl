%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_announce).

%%% **DDD Context:** Runtime Context

-moduledoc """
Supervision announcements: `ChildAdded`/`ChildCrashed` events published for a
supervisor's children, and the wrapping of a started child into the handle
Beamtalk code sees.

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    announce_child_added/3,
    announce_child_crashed/3,
    wrap_child/3,
    wrap_child/4
]).

-doc """
Wrap a child pid with the correct Beamtalk tuple tag.
Supervisor subclasses use {beamtalk_supervisor, ...} so follow-up sends
use the supervisor dispatch path rather than the actor/gen_server path.
""".
-spec wrap_child(atom(), module(), pid()) -> tuple().
wrap_child(ChildClass, ChildModule, ChildPid) ->
    wrap_child(ChildClass, ChildModule, unknown, ChildPid).

%% As `wrap_child/3`, with the child's spawn kind when the pool cached it at
%% `dynamic_init/3` (BT-4502): a known kind skips the superclass walk.
-spec wrap_child(atom(), module(), supervisor | actor | none | unknown, pid()) -> tuple().

wrap_child(ChildClass, ChildModule, supervisor, ChildPid) ->
    {beamtalk_supervisor, ChildClass, ChildModule, ChildPid};
wrap_child(ChildClass, ChildModule, Kind, ChildPid) when Kind =:= actor; Kind =:= none ->
    {beamtalk_object, ChildClass, ChildModule, ChildPid};
wrap_child(ChildClass, ChildModule, _Unknown, ChildPid) ->
    case beamtalk_supervisor:is_supervisor(ChildClass) of
        true -> {beamtalk_supervisor, ChildClass, ChildModule, ChildPid};
        false -> {beamtalk_object, ChildClass, ChildModule, ChildPid}
    end.

-doc """
Announce `SupervisionChildAdded` on the `SystemAnnouncer` bus after a
DynamicSupervisor successfully starts a child (ADR 0093 §2 / ADR 0092).
Best-effort and fault-isolated — see `announce_supervision/2`.
""".
-spec announce_child_added(atom(), atom(), pid()) -> ok.
announce_child_added(SupClass, ChildClass, ChildPid) ->
    announce_supervision('SupervisionChildAdded', #{
        supervisor => SupClass, childClass => ChildClass, childPid => ChildPid
    }).

-doc """
Announce `SupervisionChildCrashed` on the `SystemAnnouncer` bus when a
DynamicSupervisor child fails to start (ADR 0093 §2 / ADR 0092). The
arbitrary OTP failure `Reason` is normalised to a Symbol for the typed event
payload. Best-effort and fault-isolated — see `announce_supervision/2`.
""".
-spec announce_child_crashed(atom(), atom(), term()) -> ok.
announce_child_crashed(SupClass, ChildClass, Reason) ->
    announce_supervision('SupervisionChildCrashed', #{
        supervisor => SupClass,
        childClass => ChildClass,
        childPid => nil,
        reason => normalize_crash_reason(Reason)
    }).

-doc """
Normalise an OTP `supervisor:start_child/2` failure reason to a stable Symbol
for the `SupervisionChildCrashed` event payload: a `#beamtalk_error{}`
(e.g. from `startChild:name:`'s `'spawnAs'/3` path) yields its `kind`
(e.g. `name_registered`), a leading atom is kept (e.g. `already_present`),
anything else collapses to `child_start_failed`. Keeps the typed
`reason :: Symbol` field flat (the full reason is in the returned
`#beamtalk_error{}` for diagnostics).
""".
-spec normalize_crash_reason(term()) -> atom().
normalize_crash_reason(#beamtalk_error{kind = Kind}) -> Kind;
normalize_crash_reason(Reason) when is_atom(Reason) -> Reason;
normalize_crash_reason({Reason, _}) when is_atom(Reason) -> Reason;
normalize_crash_reason(_Reason) -> child_start_failed.

-doc """
Emit a supervision-lifecycle system event on the `SystemAnnouncer` bus.

Goes through `beamtalk_announcements:system_announce_safe/2` (the announcements
worker starts after bootstrap): announcing is a best-effort observability side
effect and must never fail or delay the supervision operation.
""".
-spec announce_supervision(atom(), map()) -> ok.
announce_supervision(EventClass, Fields) ->
    beamtalk_announcements:system_announce_safe(EventClass, Fields).
