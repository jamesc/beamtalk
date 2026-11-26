%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_edit).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for the code-editing writes: `save-method`, `save-class`,
`revert-method` and `reload-file` (BT-5203).

Each is the one implementation of an edit that LiveView's System Browser,
the MCP `save_method` / `save_class` tools and any wire client share. They
take structured params (a class name, a selector, a source String), never a
Beamtalk expression assembled from those values, so nothing needs escaping
and no user text is ever re-parsed as Beamtalk source. Each wraps the runtime
chokepoint that `compile:source:`, `Workspace newClass:at:`, `Workspace
changes revert:` and `Workspace reload` already converge on, so validation,
ChangeLog recording and diagnostics are exactly theirs:

| Op | Chokepoint |
|----|------------|
| `save-method` | `beamtalk_repl_eval:compile_method/6` |
| `save-class` | `beamtalk_repl_loader_newclass:new_class/2` |
| `revert-method` | `beamtalk_workspace_interface_primitives:revert_method/3` |
| `reload-file` | `beamtalk_repl_eval:reload_file/1` |

`handle_term/4` returns `{value, Map}` of scalars on success, or
`{error, #beamtalk_error{}}` (compile diagnostics ride in the error as terms).
`handle/4` is the WebSocket-edge JSON wrapper. The ops ignore `SessionPid`:
they are workspace-global, like the browse ops.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

-doc "WebSocket-edge wrapper: encodes the term result to JSON.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for the editing ops.

* `save-method` (`class`, `selector`, `source`; optional `author`,
  `author_kind` = `human` | `agent`) → `{value, #{<<"class">> => _,
  <<"selector">> => _}}`. The patch is durable: installed in memory and logged
  as a keepable ChangeLog entry. Without `author` the author is the one the
  calling process is stamped with (`repl` / `human` when nothing stamps it).
* `save-class` (`source`, `path`) → `{value, #{<<"path">> => _}}`.
* `revert-method` (`class`, `selector`; optional `side` = `instance` | `class`)
  → `{value, #{<<"class">> => _, <<"selector">> => _}}`.
* `reload-file` (`path`) → `{value, #{<<"classes">> => [Name]}}`.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"save-method">>, Params, _Msg, _SessionPid) ->
    case
        fields([
            {<<"class">>, beamtalk_repl_params:nonempty_binary(Params, <<"class">>)},
            {<<"selector">>, beamtalk_repl_params:nonempty_binary(Params, <<"selector">>)},
            {<<"source">>, beamtalk_repl_params:nonempty_binary(Params, <<"source">>)}
        ])
    of
        {ok, [Class, Selector, Source]} ->
            save_method(Class, Selector, Source, Params);
        {missing, Key} ->
            missing_param(<<"save-method">>, Key)
    end;
handle_term(<<"save-class">>, Params, _Msg, _SessionPid) ->
    case
        fields([
            {<<"source">>, beamtalk_repl_params:nonempty_binary(Params, <<"source">>)},
            {<<"path">>, beamtalk_repl_params:nonempty_binary(Params, <<"path">>)}
        ])
    of
        {ok, [Source, Path]} ->
            %% A failed ChangeLog append for the new class is queued as a warning
            %% (BT-5084), not raised: collect it and return it, so the caller
            %% learns `flush` will not write the file.
            ok = beamtalk_refactor_support:begin_changelog_warnings(),
            Result =
                try
                    beamtalk_repl_loader_newclass:new_class(Source, Path)
                catch
                    Class:Reason0:Stack ->
                        %% A raise must not leave the process marked as collecting.
                        _ = beamtalk_refactor_support:end_changelog_warnings(),
                        erlang:raise(Class, Reason0, Stack)
                end,
            Warnings = beamtalk_refactor_support:end_changelog_warnings(),
            case Result of
                {ok, _ClassObjects} ->
                    Value = #{<<"path">> => Path},
                    {value,
                        case Warnings of
                            [] -> Value;
                            _ -> Value#{<<"warnings">> => Warnings}
                        end};
                {error, Reason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
            end;
        {missing, Key} ->
            missing_param(<<"save-class">>, Key)
    end;
handle_term(<<"revert-method">>, Params, _Msg, _SessionPid) ->
    case
        fields([
            {<<"class">>, beamtalk_repl_params:nonempty_binary(Params, <<"class">>)},
            {<<"selector">>, beamtalk_repl_params:nonempty_binary(Params, <<"selector">>)}
        ])
    of
        {ok, [Class, Selector]} ->
            case revert_side(maps:get(<<"side">>, Params, undefined)) of
                {ok, Side} ->
                    case
                        beamtalk_workspace_interface_primitives:revert_method(Class, Selector, Side)
                    of
                        {ok, _ClassObject} ->
                            {value, #{<<"class">> => Class, <<"selector">> => Selector}};
                        {error, Reason} ->
                            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {missing, Key} ->
            missing_param(<<"revert-method">>, Key)
    end;
handle_term(<<"reload-file">>, Params, _Msg, _SessionPid) ->
    case fields([{<<"path">>, beamtalk_repl_params:nonempty_binary(Params, <<"path">>)}]) of
        {ok, [Path]} ->
            case project_relative_bt_path(Path) of
                ok ->
                    case beamtalk_repl_eval:reload_file(binary_to_list(Path)) of
                        {ok, ClassNames} ->
                            {value, #{<<"classes">> => ClassNames}};
                        {error, Reason} ->
                            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {missing, Key} ->
            missing_param(<<"reload-file">>, Key)
    end.

%%% Internal helpers

%% `side` is absent (side-agnostic: the highest-seq entry wins) or exactly
%% `instance` / `class`. Anything else is refused rather than silently widened
%% to side-agnostic, which could revert the other side's patch.
-spec revert_side(term()) -> {ok, binary() | undefined} | {error, #beamtalk_error{}}.
revert_side(undefined) ->
    {ok, undefined};
revert_side(Side) when Side =:= <<"instance">>; Side =:= <<"class">> ->
    {ok, Side};
revert_side(_Other) ->
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            <<"revert-method: side must be \"instance\" or \"class\"">>,
            <<"Omit side to revert the newest entry for the selector.">>
        )}.

%% `reload-file` takes the project-relative `.bt` path a git operation just
%% restored: no absolute path, no `..` segment, `.bt` only.
-spec project_relative_bt_path(binary()) -> ok | {error, #beamtalk_error{}}.
project_relative_bt_path(Path) ->
    Segments = binary:split(Path, [<<"/">>, <<"\\">>], [global]),
    case
        filename:pathtype(binary_to_list(Path)) =:= relative andalso
            not lists:member(<<"..">>, Segments) andalso
            filename:extension(Path) =:= <<".bt">>
    of
        true ->
            ok;
        false ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'Workspace',
                    <<"reload-file requires a project-relative .bt path">>,
                    <<"Pass a path such as src/Counter.bt, without '..'.">>
                )}
    end.

-spec save_method(binary(), binary(), binary(), map()) -> beamtalk_repl_ops:op_result().
save_method(Class, Selector, Source, Params) ->
    case author(Params) of
        {ok, {Author, AuthorKind}} ->
            case
                beamtalk_repl_eval:compile_method(
                    Class, Selector, Source, durable, Author, AuthorKind
                )
            of
                {ok, ClassName} ->
                    {value, #{<<"class">> => ClassName, <<"selector">> => Selector}};
                {error, Reason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
            end;
        {error, _} = Err ->
            Err
    end.

%% The audit author: explicit `author` / `author_kind` params, else whatever
%% the calling process is stamped with.
-spec author(map()) -> {ok, {binary(), human | agent}} | {error, #beamtalk_error{}}.
author(Params) ->
    {DefaultAuthor, DefaultKind} = beamtalk_refactor_support:current_author_context(),
    Author =
        case maps:get(<<"author">>, Params, undefined) of
            A when is_binary(A), A =/= <<>> -> A;
            _ -> DefaultAuthor
        end,
    case maps:get(<<"author_kind">>, Params, undefined) of
        undefined ->
            {ok, {Author, DefaultKind}};
        <<"human">> ->
            {ok, {Author, human}};
        <<"agent">> ->
            {ok, {Author, agent}};
        _ ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'Workspace',
                    <<"save-method: author_kind must be \"human\" or \"agent\"">>,
                    <<"Omit author_kind to use the caller's own.">>
                )}
    end.

%% `{ok, Values}` when every required param is present, else the first missing
%% one by name.
-spec fields([{binary(), {ok, binary()} | error}]) -> {ok, [binary()]} | {missing, binary()}.
fields(Fields) ->
    fields(Fields, []).

fields([], Acc) ->
    {ok, lists:reverse(Acc)};
fields([{_Key, {ok, Value}} | Rest], Acc) ->
    fields(Rest, [Value | Acc]);
fields([{Key, error} | _Rest], _Acc) ->
    {missing, Key}.

-spec missing_param(binary(), binary()) -> {error, #beamtalk_error{}}.
missing_param(Op, Key) ->
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            iolist_to_binary([Op, <<" requires a non-empty ">>, Key]),
            iolist_to_binary([<<"Pass ">>, Key, <<" as a String.">>])
        )}.

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

-doc "Return op descriptors for the editing ops.".
-spec describe_ops() -> map().
describe_ops() ->
    beamtalk_repl_op_table:describe_for(?MODULE).
