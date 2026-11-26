%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_state).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for workspace state: `flush`, `changes` and `info` (BT-5204).

Each is the one implementation of a capability that LiveView's Changes pane,
the MCP `flush` tool and any wire client share, where each used to reach
`beamtalk_workspace_flush`, `beamtalk_workspace_changelog_views`,
`beamtalk_workspace_meta` and `beamtalk_version` on its own:

| Op | Chokepoint |
|----|------------|
| `flush` | `beamtalk_workspace_flush` (`flush/0`, `flush/2`, `flush_including_destructive/0`) |
| `changes` | `beamtalk_workspace_changelog_views:change_entries/0` |
| `info` | `beamtalk_version:get/0`, `beamtalk_workspace_meta` |

The findings snapshot is the existing `reload-findings` op.

All three answer `{value, Map}` (`handle_term/4`), wire-shaped: binary keys,
`null` for an absent value, atoms as their names. `handle/4` is the
WebSocket-edge JSON wrapper. They ignore `SessionPid`: workspace-global, like
the browse and editing ops, so `info` also answers a caller that has no
session yet (LiveView's readiness probe).

## `flush`

Optional params: at most one of `class`, `file`, `kind` (scope), and
`confirm_destructive` (a JSON boolean, default `false`). The destructive tier
(ADR 0113) is only ever applied when `confirm_destructive` is exactly `true`;
a non-boolean value is refused, never read as false or true, and no setting
can imply it. Unscoped plus `confirm_destructive` is
`beamtalk_workspace_flush:flush_including_destructive/0`. A hard runtime error
is `{error, #beamtalk_error{}}`; conflicts and skipped (Tier 2, non-flushable)
entries are in the summary, which is `{value, Summary}`:
`flushed`, `files`, `newClasses`, `removedClasses`, `skipped`, `conflicts`.

## `changes`

`{value, #{<<"entries">> => [Entry]}}`: every ChangeLog entry, oldest first,
with the `active` / `shadowed` / `clean` flags a client uses to pick the
pending view. Entry keys are `seq`, `className`, `selector`, `kind`, `side`,
`intent`, `flushable`, `authorKind`, `sourceFile`, `orphan`, `priorEpoch`,
`flushed`, `active`, `shadowed`, `clean`, `diff`, `oldClass`, `oldSelector`.

## `info`

`{value, #{<<"version">> => VersionReport, <<"node">> => _,
<<"workspace_id">> => _, <<"settings">> => #{<<"autoflush">> => Bool}}}`.
`VersionReport` is `beamtalk_version:get/0` (`runtime_version`,
`protocol_version`, `otp_release`, `erts_version`) with binary keys.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

%% Exported for white-box tests of the wire projection.
-ifdef(TEST).
-export([wire/1]).
-endif.

-doc "WebSocket-edge wrapper: encodes the term result to JSON.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc "Term-returning handler for `flush`, `changes` and `info`.".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"flush">>, Params, _Msg, _SessionPid) ->
    handle_flush(Params);
handle_term(<<"changes">>, _Params, _Msg, _SessionPid) ->
    {value, #{
        <<"entries">> => [wire(E) || E <- beamtalk_workspace_changelog_views:change_entries()]
    }};
handle_term(<<"info">>, _Params, _Msg, _SessionPid) ->
    {value, #{
        <<"version">> => wire(beamtalk_version:get()),
        <<"node">> => atom_to_binary(node(), utf8),
        <<"workspace_id">> => workspace_id(),
        <<"settings">> => #{
            <<"autoflush">> => autoflush_setting()
        }
    }}.

%%====================================================================
%% flush
%%====================================================================

-spec handle_flush(map()) -> beamtalk_repl_ops:op_result().
handle_flush(Params) ->
    case {scope(Params), confirm_destructive(Params)} of
        {{ok, Scope}, {ok, Confirm}} ->
            case run_flush(Scope, Confirm) of
                {ok, Summary} when is_map(Summary) ->
                    {value, wire(Summary)};
                {error, Reason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
            end;
        {{error, _} = Err, _} ->
            Err;
        {_, {error, _} = Err} ->
            Err
    end.

%% Unscoped: Tier 1 only, or Tier 1 and Tier 2 when confirmed. Scoped: the
%% filter form each surface already used (`Workspace flush: aClass`, a
%% Symbol, `#{ #file => ... }`).
-spec run_flush(none | {class | file | kind, term()}, boolean()) ->
    {ok, map()} | {error, term()}.
run_flush(none, false) ->
    beamtalk_workspace_flush:flush();
run_flush(none, true) ->
    beamtalk_workspace_flush:flush_including_destructive();
run_flush({class, Class}, Confirm) ->
    beamtalk_workspace_flush:flush(Class, Confirm);
run_flush({file, File}, Confirm) ->
    beamtalk_workspace_flush:flush(#{file => File}, Confirm);
run_flush({kind, Kind}, Confirm) ->
    beamtalk_workspace_flush:flush(Kind, Confirm).

-spec scope(map()) ->
    {ok, none | {class | file | kind, term()}} | {error, #beamtalk_error{}}.
scope(Params) ->
    Class = beamtalk_repl_params:nonempty_or_undefined(maps:get(<<"class">>, Params, undefined)),
    File = beamtalk_repl_params:nonempty_or_undefined(maps:get(<<"file">>, Params, undefined)),
    Kind = beamtalk_repl_params:nonempty_or_undefined(maps:get(<<"kind">>, Params, undefined)),
    Given = [
        {Name, V}
     || {Name, V} <- [{class, Class}, {file, File}, {kind, Kind}], V =/= undefined
    ],
    case Given of
        [] ->
            {ok, none};
        [{class, Class}] ->
            {ok, {class, Class}};
        [{file, File}] ->
            {ok, {file, File}};
        [{kind, Kind}] ->
            kind_atom(Kind);
        _ ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'Workspace',
                    <<"flush: class, file and kind are mutually exclusive">>,
                    <<"Pass at most one of class, file or kind.">>
                )}
    end.

%% A kind is a selector Symbol (`new-class`): resolved to an existing atom so
%% request text never creates one. A name that is no atom cannot match any
%% entry kind, so it is an invalid argument, as the Beamtalk form would fail.
-spec kind_atom(binary()) -> {ok, {kind, atom()}} | {error, #beamtalk_error{}}.
kind_atom(Kind) ->
    Bare = string:trim(Kind, both, "#'"),
    case beamtalk_text:to_existing_atom(Bare) of
        {ok, Atom} ->
            {ok, {kind, Atom}};
        error ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'Workspace',
                    iolist_to_binary([<<"flush: unknown kind ">>, Kind]),
                    <<"Use an entry kind such as new-class, instance or class.">>
                )}
    end.

%% Exactly `true` confirms. Absent is `false`; anything else is refused so a
%% malformed value can neither silently skip nor silently apply the
%% destructive tier.
-spec confirm_destructive(map()) -> {ok, boolean()} | {error, #beamtalk_error{}}.
confirm_destructive(Params) ->
    case maps:get(<<"confirm_destructive">>, Params, false) of
        Bool when is_boolean(Bool) ->
            {ok, Bool};
        _ ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'Workspace',
                    <<"flush: confirm_destructive must be a boolean">>,
                    <<"Pass true to apply pending destructive entries, or omit it.">>
                )}
    end.

%%====================================================================
%% info
%%====================================================================

-spec workspace_id() -> binary().
workspace_id() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, Meta} -> maps:get(workspace_id, Meta, <<>>);
        {error, _} -> <<>>
    end.

%% A workspace without the meta server (run mode) has no setting: `false`,
%% the default. A non-boolean stored value reads as `false` too.
-spec autoflush_setting() -> boolean().
autoflush_setting() ->
    case beamtalk_workspace_meta:get_setting(autoflush, false) of
        Flag when is_boolean(Flag) -> Flag;
        _ -> false
    end.

%%====================================================================
%% Wire projection
%%====================================================================

-doc false.
%% Project a runtime term to the wire shape: map keys and atoms become
%% binaries, `nil` / `undefined` become `null`, booleans, numbers and
%% binaries pass through, lists recurse. The class tag is internal and is
%% dropped. Anything else (a pid, a tuple) is shown as its `~p` text so a
%% reply never crashes the encoder.
-spec wire(term()) -> term().
wire(Map) when is_map(Map) ->
    maps:from_list([
        {wire_key(K), wire(V)}
     || {K, V} <- maps:to_list(Map), K =/= ?BT_CLASS_TAG_KEY
    ]);
wire(List) when is_list(List) -> [wire(E) || E <- List];
wire(Bool) when is_boolean(Bool) -> Bool;
wire(nil) ->
    null;
wire(undefined) ->
    null;
wire(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
wire(Bin) when is_binary(Bin) -> Bin;
wire(N) when is_number(N) -> N;
wire(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

-spec wire_key(term()) -> binary().
wire_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
wire_key(K) when is_binary(K) -> K;
wire_key(K) -> iolist_to_binary(io_lib:format("~p", [K])).

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

-doc "Return op descriptors for the workspace-state ops.".
-spec describe_ops() -> map().
describe_ops() ->
    beamtalk_repl_op_table:describe_for(?MODULE).
