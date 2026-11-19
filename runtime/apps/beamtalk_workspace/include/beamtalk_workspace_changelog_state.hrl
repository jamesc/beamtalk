%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% The ChangeLog gen_server's private state record. Included only by
%%% `beamtalk_workspace_changelog` and `beamtalk_workspace_changelog_log`
%%% (persistence, which reads/writes `changes_dir` and `log_path`); the codec
%%% and views never see it.

-record(state, {
    %% Absolute path to <workspace>/changes, or undefined in run mode
    %% (no workspace_id) — in run mode everything stays in ETS only.
    changes_dir :: string() | undefined,
    %% Path to changes.jsonl (undefined in run mode).
    log_path :: string() | undefined,
    %% Next sequence number to assign (monotonic across restarts).
    next_seq :: non_neg_integer(),
    %% Epoch for entries appended in this session (bumped each start).
    epoch :: non_neg_integer(),
    %% True once the "active entries exceed the ring bound" warning has been
    %% logged for the current overflow episode (BT-5016); cleared when it ends.
    ring_overflow_warned = false :: boolean()
}).
