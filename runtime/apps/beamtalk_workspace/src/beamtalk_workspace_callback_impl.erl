%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_callback_impl).
-behaviour(beamtalk_workspace_callback).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The workspace's implementation of `beamtalk_workspace_callback` (BT-4895).

`beamtalk_workspace_callback:install/1` registers this module. Each callback
goes straight to the module that owns the behaviour — the loader, the rewrite
mechanism, or the ChangeLog entry builders — so no callback takes more than one
hop beyond this module. The few callbacks whose logic lives in
`beamtalk_repl_eval` (evaluate-in-context, method compile / precheck /
removal) forward there.
""".

-export([
    reload_class_file/2,
    eval_with_self/2,
    compile_method/6,
    precheck_method/4,
    remove_method/4,
    emit_remove_change_entry/5,
    rewrite_sites/2,
    validate_sites/2,
    emit_rewrite_change_entry/2,
    capture_class_removal_snapshot/1,
    emit_remove_class_change_entry/4
]).

-include("beamtalk_repl_loader.hrl").

-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    beamtalk_repl_loader_reload:reload_class_file(Path, ExpectedClassName).

-spec eval_with_self(term(), binary() | string()) -> {ok, term()} | {error, term()}.
eval_with_self(Self, Source) ->
    beamtalk_repl_eval:eval_with_self(Self, Source).

-spec compile_method(
    binary(), atom() | binary(), binary(), durable | ephemeral, binary(), human | agent
) -> {ok, binary()} | {error, term()}.
compile_method(ClassNameBin, Selector, Source, Intent, Author, AuthorKind) ->
    beamtalk_repl_eval:compile_method(
        ClassNameBin, Selector, Source, Intent, Author, AuthorKind
    ).

-spec precheck_method(binary(), atom() | binary(), binary(), instance | class) ->
    {ok, term()} | {error, term()}.
precheck_method(ClassNameBin, Selector, Source, Side) ->
    beamtalk_repl_eval:precheck_method(ClassNameBin, Selector, Source, Side).

-spec remove_method(
    binary(), atom() | binary(), instance | class, refuse_stdlib | allow_stdlib
) -> {ok, binary()} | {error, term()}.
remove_method(ClassNameBin, Selector, Side, StdlibPolicy) ->
    beamtalk_repl_eval:remove_method(ClassNameBin, Selector, Side, StdlibPolicy).

-spec emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) -> beamtalk_workspace_callback:emit_result().
emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind) ->
    ?ENTRIES:emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind).

-spec rewrite_sites(
    beamtalk_repl_loader_rewrite:rewrite_site() | undefined,
    [beamtalk_repl_loader_rewrite:rewrite_site()]
) -> {ok, beamtalk_repl_loader_rewrite:rewrite_result()} | {error, term()}.
rewrite_sites(DefinitionSite, ReferenceSites) ->
    beamtalk_repl_loader_rewrite:rewrite_sites(DefinitionSite, ReferenceSites).

-spec validate_sites(
    beamtalk_repl_loader_rewrite:rewrite_site() | undefined,
    [beamtalk_repl_loader_rewrite:rewrite_site()]
) -> ok | {error, term()}.
validate_sites(DefinitionSite, ReferenceSites) ->
    beamtalk_repl_loader_rewrite:validate_sites(DefinitionSite, ReferenceSites).

-spec emit_rewrite_change_entry(map(), beamtalk_repl_loader_rewrite:rewrite_result()) ->
    beamtalk_workspace_callback:emit_result().
emit_rewrite_change_entry(Spec, RewriteResult) ->
    ?ENTRIES:emit_rewrite_change_entry(Spec, RewriteResult).

-spec capture_class_removal_snapshot(binary()) -> map().
capture_class_removal_snapshot(ClassNameBin) ->
    ?ENTRIES:capture_class_removal_snapshot(ClassNameBin).

-spec emit_remove_class_change_entry(binary(), map(), binary(), human | agent) ->
    beamtalk_workspace_callback:emit_result().
emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind) ->
    ?ENTRIES:emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind).
