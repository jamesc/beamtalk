%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% DynamicSupervisor `maxChildren` enforcement keys and tags shared by
%% `beamtalk_supervisor` (which installs and reads them in `dynamic_init/3`)
%% and `beamtalk_supervisor_capacity` (which enforces them). BT-3964, BT-4068.

-ifndef(BEAMTALK_SUPERVISOR_CAPACITY_HRL).
-define(BEAMTALK_SUPERVISOR_CAPACITY_HRL, true).

%% BT-3964: process-dictionary key (in the DynamicSupervisor process) holding
%% the `maxChildren` limit read by `dynamic_init/3`.
-define(MAX_CHILDREN_KEY, '$beamtalk_max_children').
%% BT-4068: process-dictionary key (in a capped DynamicSupervisor process)
%% holding the enforcement state installed by `dynamic_init/3`:
%% `{Tid, MonPid}`, the ETS counter table and the monitor process that
%% releases a child's slot when it dies. Absent on uncapped pools and on
%% pools started before BT-4068 (which keep the legacy caller-side check).
-define(MAX_CHILDREN_STATE_KEY, '$beamtalk_max_children_state').
%% BT-4068: refusal tag. `checked_dynamic_start/1..4` answers
%% `{error, {?MAX_CHILDREN_REFUSED, Max}}` when a manual start finds the pool
%% full; OTP passes it back through `supervisor:start_child/2` verbatim and
%% `beamtalk_supervisor_capacity:with_child_capacity/6` maps it to
%% `#beamtalk_error{kind = max_children}`.
-define(MAX_CHILDREN_REFUSED, beamtalk_max_children_exceeded).
%% BT-4068: manual-start token tag. `beamtalk_supervisor_capacity:with_child_capacity/6`
%% prepends `{?MAX_CHILDREN_TOKEN, Ref}` to the start args; the wrapper consumes it
%% with `ets:take/2`, so an OTP restart replay — the same args verbatim —
%% finds the token already consumed and is recognised as a restart. The tag
%% lets the wrapper strip a consumed token instead of passing it to the
%% child's start, and tells genuinely untokened starts (remote callers,
%% direct `supervisor:start_child/2`) apart from replays.
-define(MAX_CHILDREN_TOKEN, beamtalk_max_children_token).

-endif.
