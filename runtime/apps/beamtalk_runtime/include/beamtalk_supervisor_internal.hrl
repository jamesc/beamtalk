%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Macros shared by `beamtalk_supervisor` and the modules split out of it
%% (BT-5047): the process-dictionary keys set by `static_init/3` /
%% `dynamic_init/3`, the top-level start tag and the instances group.

-ifndef(BEAMTALK_SUPERVISOR_INTERNAL_HRL).
-define(BEAMTALK_SUPERVISOR_INTERNAL_HRL, true).

%% ADR 0135: process-dictionary keys (in the supervisor process) set by
%% `static_init/3` / `dynamic_init/3`. `?SUP_ARGS_KEY` holds the start args
%% (`self args`), `?CHILD_CLASS_KEY` a DynamicSupervisor's
%% `{ChildClassName, ChildModule}`, `?SUP_FLAGS_KEY` the configured
%% `#{strategy, maxRestarts, restartWindow}` (read by process navigation).
-define(SUP_ARGS_KEY, '$beamtalk_sup_args').

-define(CHILD_CLASS_KEY, '$beamtalk_child_class').

%% ADR 0135 §1: `{ClassName, Module}` of the supervisor, set by `begin_init/3`
%% (read by `named/2` to check the class of a registered supervisor).
-define(SUP_CLASS_KEY, '$beamtalk_sup_class').

%% ADR 0135 §1: the init arg of a top-level spawn-family start is wrapped in
%% `{?TOP_LEVEL_TAG, Args}`; `begin_init/3` unwraps it and joins the instance
%% to `?INSTANCES_GROUP` so `Node current supervisors` finds it.
-define(TOP_LEVEL_TAG, '$beamtalk_top_level').

-define(INSTANCES_GROUP, beamtalk_supervisor_instances).

-endif.
