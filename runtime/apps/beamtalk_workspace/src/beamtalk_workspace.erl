%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace).

%%% **DDD Context:** Workspace Context

-moduledoc """
Workspace API for managing workspace-level operations.

This module provides a high-level API for querying workspace state.
It delegates to beamtalk_workspace_meta for workspace metadata.
""".

-export([status/0, resolve_name/2, resolve_class_reference/2, resolve_singleton_instance/1]).
-export([raise_undefined_variable/1]).

-doc """
Resolve a bare name against session locals + live workspace sources (ADR 0081).

BT-2365: the public entry point the REPL compiler's free-identifier fallthrough
targets. Delegates to `beamtalk_workspace_interface_primitives:resolve_name/2`,
which holds the single shared resolution order (locals → bind:as: → singletons →
classes → undefined_variable) shared with `Session resolve:`.
""".
-spec resolve_name(map(), atom()) -> term().
resolve_name(Locals, Name) ->
    beamtalk_workspace_interface_primitives:resolve_name(Locals, Name).

-doc """
Raise the `undefined_variable` error for `Name` directly, bypassing the workspace
resolution tiers (BT-2509).

The REPL `self` codegen uses this on a bindings-map miss: a top-level `self` has
no receiver and must be `undefined_variable`, NOT resolved through
`resolve_name/2` — whose `bind:as:` tier would let a user binding named `self`
silently shadow the reserved word.
""".
-spec raise_undefined_variable(atom()) -> no_return().
raise_undefined_variable(Name) ->
    beamtalk_error:raise(beamtalk_repl_errors:ensure_structured_error({undefined_variable, Name})).

-doc """
Resolve a capitalised class reference not found in the session locals (ADR 0081).

BT-2365: the REPL compiler emits this for a `ClassReference` after a locals miss.
Delegates to `beamtalk_workspace_interface_primitives:resolve_class_reference/2`,
which reuses resolve_name/2's singleton + class tiers but raises class_not_found
(not undefined_variable) for a genuinely unknown class.
""".
-spec resolve_class_reference(map(), atom()) -> term().
resolve_class_reference(Locals, Name) ->
    beamtalk_workspace_interface_primitives:resolve_class_reference(Locals, Name).

-doc """
Resolve a singleton binding name to its live instance, or `error` (ADR 0081).

BT-2365: the REPL compiler emits this on the miss branch of a binding-aware
class-send so a message to a singleton receiver (`Workspace bind:as:`) reaches
the live instance. Delegates to
`beamtalk_workspace_interface_primitives:resolve_singleton_instance/1`.
""".
-spec resolve_singleton_instance(atom()) -> {ok, term()} | error.
resolve_singleton_instance(Name) ->
    beamtalk_workspace_interface_primitives:resolve_singleton_instance(Name).

-doc """
Return a summary of the current workspace state.
Includes workspace ID, project path, created_at, last_activity, actor count, and loaded modules.
""".
-spec status() -> {ok, map()} | {error, term()}.
status() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, Meta} ->
            SupervisedActors = maps:get(supervised_actors, Meta, []),
            {ok, #{
                workspace_id => maps:get(workspace_id, Meta, undefined),
                project_path => maps:get(project_path, Meta, undefined),
                created_at => maps:get(created_at, Meta, undefined),
                last_activity => maps:get(last_activity, Meta, undefined),
                actor_count => length(SupervisedActors),
                loaded_modules => maps:get(loaded_modules, Meta, [])
            }};
        {error, Reason} ->
            {error, Reason}
    end.
