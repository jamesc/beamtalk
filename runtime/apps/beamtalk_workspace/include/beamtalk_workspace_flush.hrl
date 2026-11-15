%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% Phase A / Phase B record shared by `beamtalk_workspace_flush` and the
%%% modules split out of it (`beamtalk_workspace_flush_support`,
%%% `beamtalk_workspace_flush_sites`, `beamtalk_workspace_flush_rename`).

-record(prepared, {
    %% Absolute target path on disk. For `op = move`/`move_noop` (ADR 0114)
    %% this is `new_path` — the file this operation ultimately
    %% leaves on disk — not the file Phase A read from.
    file :: binary(),
    %% The staging path Phase A produced: `<file>.bt-staging.<unique>.tmp` for
    %% `op = write` (and `move`, staged at `<new_path>.bt-staging.<unique>.tmp`; see
    %% `beamtalk_workspace_file:write_temp/2`), `<file>.tmp-delete-<epoch>-<seq>`
    %% for `op = delete` (ADR 0113), `undefined` for `op = noop`/`move_noop`.
    tmp :: string() | undefined,
    %% The entries whose patches were merged into this file's new body (or,
    %% for `op = delete` / `op = noop` / `move` / `move_noop`, the owning
    %% `'remove-class'`/`'rename-class'` entry/entries).
    entries :: [term()],
    %% Whether the target file existed prior to flush (informational for
    %% `op = write`; for `op = delete` distinguishes "this attempt performed
    %% the stage-rename" (`true`) from "a prior attempt already staged it,
    %% this run only resumed" (`false`) — see `cleanup_one/1`).
    pre_existing :: boolean(),
    %% Phase B commit action: `write` renames `tmp` into `file` (patches,
    %% `'new-class'`, `'remove-method'`); `delete` unlinks the staged `tmp`
    %% (class removal, ADR 0113); `noop` performs no I/O (the target file was
    %% already gone — external-edit soft success); `move` renames `tmp` into
    %% `file` (= `new_path`) THEN unlinks `old_file` (class rename, ADR 0114);
    %% `move_noop` performs no I/O (the move already completed in
    %% an earlier, crashed/marker-failed attempt — see the moduledoc's
    %% "Atomicity (class rename)" section).
    op = write :: write | delete | noop | move | move_noop,
    %% ADR 0114: `op = move`-only — the pre-rename path Phase B
    %% unlinks after the `tmp` -> `file` (`new_path`) rename succeeds.
    %% `undefined` for every other op.
    old_file :: binary() | undefined
}).
