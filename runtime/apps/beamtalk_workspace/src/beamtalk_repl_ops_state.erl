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

Optional params: at most one of `class`, `file`, `kind` (scope) or a
`selector` (a method selector such as `increment` or `at:put:`, which may be
combined with `class` to flush one method of one class; with `file` or `kind`
it is refused), and
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
-spec run_flush(
    none | {class | file | kind, term()} | {method, binary() | any, binary()}, boolean()
) ->
    {ok, map()} | {error, term()}.
run_flush(none, false) ->
    beamtalk_workspace_flush:flush();
run_flush(none, true) ->
    beamtalk_workspace_flush:flush_including_destructive();
run_flush({class, Class}, Confirm) ->
    beamtalk_workspace_flush:flush(Class, Confirm);
run_flush({method, Class, Selector}, Confirm) ->
    beamtalk_workspace_flush:flush_method(Class, Selector, Confirm);
run_flush({file, File}, Confirm) ->
    beamtalk_workspace_flush:flush(#{file => File}, Confirm);
run_flush({kind, Kind}, Confirm) ->
    beamtalk_workspace_flush:flush(Kind, Confirm).

-spec scope(map()) ->
    {ok, none | {class | file | kind, term()} | {method, binary() | any, binary()}}
    | {error, #beamtalk_error{}}.
scope(Params) ->
    case
        {
            scope_param(<<"class">>, maps:get(<<"class">>, Params, undefined)),
            scope_param(<<"file">>, maps:get(<<"file">>, Params, undefined)),
            scope_param(<<"kind">>, maps:get(<<"kind">>, Params, undefined)),
            scope_param(<<"selector">>, maps:get(<<"selector">>, Params, undefined))
        }
    of
        {{error, _} = Err, _, _, _} ->
            Err;
        {_, {error, _} = Err, _, _} ->
            Err;
        {_, _, {error, _} = Err, _} ->
            Err;
        {_, _, _, {error, _} = Err} ->
            Err;
        {{ok, Class}, {ok, File}, {ok, Kind}, {ok, Selector}} when Selector =/= undefined ->
            %% A selector narrows a class (or all classes); a file or kind is
            %% a different axis, so combining them is refused, not ignored.
            case {File, Kind} of
                {undefined, undefined} ->
                    case bare_selector(Selector) of
                        <<>> ->
                            {error,
                                invalid_scope(
                                    <<"flush: selector must not be empty">>,
                                    <<"Pass selector as a non-empty String, or omit it.">>
                                )};
                        Bare ->
                            {ok, {method, selector_class(Class), Bare}}
                    end;
                _ ->
                    {error,
                        invalid_scope(
                            <<"flush: selector combines only with class">>,
                            <<"Pass selector alone, or with class; not with file or kind.">>
                        )}
            end;
        {{ok, Class}, {ok, File}, {ok, Kind}, {ok, undefined}} ->
            case
                [{N, V} || {N, V} <- [{class, Class}, {file, File}, {kind, Kind}], V =/= undefined]
            of
                [] ->
                    {ok, none};
                [{class, V}] ->
                    {ok, {class, V}};
                [{file, V}] ->
                    {ok, {file, V}};
                [{kind, V}] ->
                    kind_atom(V);
                _ ->
                    {error,
                        invalid_scope(
                            <<"flush: class, file and kind are mutually exclusive">>,
                            <<"Pass at most one of class, file or kind.">>
                        )}
            end
    end.

-spec selector_class(binary() | undefined) -> binary() | any.
selector_class(undefined) -> any;
selector_class(Class) -> Class.

%% `#increment`, `#'at:put:'` and `increment` all name the selector as written.
-spec bare_selector(binary()) -> binary().
bare_selector(Selector) ->
    string:trim(Selector, both, "#'").

%% A scope param is a non-empty String or absent. Any other type, and the empty
%% String, is refused: reading it as absent would widen a scoped flush into an
%% unscoped one.
-spec scope_param(binary(), term()) -> {ok, binary() | undefined} | {error, #beamtalk_error{}}.
scope_param(_Key, undefined) ->
    {ok, undefined};
scope_param(Key, <<>>) ->
    {error,
        invalid_scope(
            iolist_to_binary([<<"flush: ">>, Key, <<" must not be empty">>]),
            iolist_to_binary([<<"Pass ">>, Key, <<" as a non-empty String, or omit it.">>])
        )};
scope_param(_Key, Bin) when is_binary(Bin) ->
    {ok, Bin};
scope_param(Key, _Other) ->
    {error,
        invalid_scope(
            iolist_to_binary([<<"flush: ">>, Key, <<" must be a String">>]),
            iolist_to_binary([<<"Pass ">>, Key, <<" as a non-empty String, or omit it.">>])
        )}.

-spec invalid_scope(binary(), binary()) -> #beamtalk_error{}.
invalid_scope(Message, Hint) ->
    beamtalk_repl_errors:make(invalid_argument, 'Workspace', Message, Hint).

%% A kind is one of the entry kinds `flush` selects by kind (see
%% `beamtalk_workspace_flush:is_kind_filter/1`), resolved to an existing atom so
%% request text never creates one. Anything else is refused: another name would
%% be read as a class or a method selector.
-spec kind_atom(binary()) -> {ok, {kind, atom()}} | {error, #beamtalk_error{}}.
kind_atom(Kind) ->
    Bare = string:trim(Kind, both, "#'"),
    case beamtalk_text:to_existing_atom(Bare) of
        {ok, Atom} ->
            case beamtalk_workspace_flush:is_kind_filter(Atom) of
                true -> {ok, {kind, Atom}};
                false -> unknown_kind(Kind)
            end;
        error ->
            unknown_kind(Kind)
    end.

-spec unknown_kind(binary()) -> {error, #beamtalk_error{}}.
unknown_kind(Kind) ->
    {error,
        invalid_scope(
            iolist_to_binary([<<"flush: unknown kind ">>, Kind]),
            iolist_to_binary([
                <<"Use one of ">>,
                lists:join(<<", ">>, [
                    atom_to_binary(K, utf8)
                 || K <- beamtalk_workspace_flush:kind_filters()
                ]),
                <<".">>
            ])
        )}.

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
-spec wire(term()) -> term().
wire(Term) -> beamtalk_repl_wire:project(Term).

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

-doc "Return op descriptors for the workspace-state ops.".
-spec describe_ops() -> map().
describe_ops() ->
    beamtalk_repl_op_table:describe_for(?MODULE).
