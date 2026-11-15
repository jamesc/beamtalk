%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_flush_support).

%%% **DDD Context:** Workspace Context

-moduledoc """
Shared leaf helpers for the flush modules: conflict-map and error builders,
staging-file I/O (`write_tmp/2`, `cleanup_tmps/1`) and the `#prepared{}`
accessors (`seq/1`, `span_start/1`). Moved verbatim out of
`beamtalk_workspace_flush` so `..._flush_sites` and `..._flush_rename` can use
them without calling back up into the orchestrator.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_flush.hrl").

-export([
    cleanup_tmps/1,
    conflict_map/4,
    prev_source_error/2,
    seq/1,
    source_body_error/2,
    span_start/1,
    wrap_io_error/2,
    write_tmp/2
]).

%%% ----------------------------------------------------------------------------
%%% Helpers
%%% ----------------------------------------------------------------------------

%% Phase A staging write. The temp name is unique per call
%% (`beamtalk_workspace_file:write_temp/2`), not a fixed `<file>.tmp`: flush
%% runs in its caller's process (an autoflush after a live patch, or a manual
%% `Workspace flush` from another session), so two flushes can stage the same
%% file at once, and with a shared temp name one could rename the other's
%% half-written body into place.
-spec write_tmp(string(), binary()) -> {ok, string()} | {error, term()}.
write_tmp(AbsPath, Body) ->
    _ = filelib:ensure_dir(AbsPath),
    beamtalk_workspace_file:write_temp(AbsPath, Body).

%% Undo Phase A staging for every not-yet-committed `Prepared` record, so an
%% aborted flush (Phase A conflict on another file, or a Phase B failure
%% partway through) leaves no stale staging artefact behind. Dispatches by
%% `op` (ADR 0113) since `write` and `delete` staged different things:
-spec cleanup_tmps([#prepared{}]) -> ok.
cleanup_tmps(Prepared) ->
    lists:foreach(fun cleanup_one/1, Prepared),
    ok.

%% `write` (patch / new-class / remove-method): delete the freshly-written
%% `<file>.tmp` — the original `file` was never touched, so there is nothing
%% else to undo.
cleanup_one(#prepared{op = write, tmp = Tmp}) ->
    _ = file:delete(Tmp),
    ok;
%% `delete` (class removal) whose stage-rename happened *in this flush
%% attempt* (`pre_existing = true`, per `prepare_remove_class/3`): rename the
%% staged file back to its original location, undoing our own Phase A step —
%% Phase A promises "no rename happened" on a whole-flush abort.
cleanup_one(#prepared{op = delete, tmp = Tmp, file = File, pre_existing = true}) ->
    _ = file:rename(Tmp, binary_to_list(File)),
    ok;
%% `delete` whose stage-rename was already sitting on disk *before* this
%% flush attempt began (`pre_existing = false` — a resumed mid-delete-crash
%% recovery, ADR 0113): this attempt did not perform that rename, so aborting
%% must leave it exactly as found for a future flush to resume, not un-stage
%% work a different (earlier) attempt already committed to.
cleanup_one(#prepared{op = delete, pre_existing = false}) ->
    ok;
%% `noop` (already-gone soft success): no I/O happened, nothing to undo.
cleanup_one(#prepared{op = noop}) ->
    ok;
%% `move` (class rename, ADR 0114): delete the freshly-written
%% `<new_path>.tmp` — Phase A never touches `old_path` for a move (only
%% reads it), so, exactly like `write`, there is nothing else to undo.
cleanup_one(#prepared{op = move, tmp = Tmp}) ->
    _ = file:delete(Tmp),
    ok;
%% `move_noop` (the move already completed in an earlier attempt): no I/O
%% happened this round, nothing to undo.
cleanup_one(#prepared{op = move_noop}) ->
    ok.

-spec span_start(term()) -> integer().
span_start(E) ->
    case beamtalk_workspace_changelog:entry_span(E) of
        #{start := S} -> S;
        undefined -> -1
    end.

-spec seq(term()) -> non_neg_integer().
seq(E) ->
    beamtalk_workspace_changelog:entry_seq(E).

-spec conflict_map(binary(), binary(), [term()], binary()) -> map().
conflict_map(File, Reason, Entries, Detail) ->
    #{
        file => File,
        reason => Reason,
        seqs => [seq(E) || E <- Entries],
        detail => Detail
    }.

-spec source_body_error(binary() | undefined, term()) -> #beamtalk_error{}.
source_body_error(File, Reason) ->
    Err0 = beamtalk_error:new(source_body_unreadable, 'Workspace'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([
            <<"Could not read recorded patch body from ChangeLog sources/ for ">>,
            file_label(File)
        ])
    ),
    beamtalk_error:with_details(Err2, #{reason => Reason}).

-spec prev_source_error(binary() | undefined, term()) -> #beamtalk_error{}.
prev_source_error(File, Reason) ->
    Err0 = beamtalk_error:new(prev_source_unreadable, 'Workspace'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([
            <<"Could not read recorded prior body from ChangeLog sources/ for ">>,
            file_label(File)
        ])
    ),
    beamtalk_error:with_details(Err2, #{reason => Reason}).

-spec wrap_io_error({error, term()}, binary()) -> {error, #beamtalk_error{}}.
wrap_io_error({error, Reason}, File) ->
    Err0 = beamtalk_error:new(flush_io_error, 'Workspace'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([<<"I/O error writing staging file for ">>, file_label(File)])
    ),
    {error, beamtalk_error:with_details(Err2, #{reason => Reason})}.

-spec file_label(binary() | undefined) -> binary().
file_label(undefined) -> <<"<unknown>">>;
file_label(File) when is_binary(File) -> File.
