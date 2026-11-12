%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_op_table).

%%% **DDD Context:** Runtime Context

-moduledoc """
The one REPL protocol op table (BT-4673).

Every op the REPL protocol knows is declared here exactly once, as an entry
carrying its name, the module that handles it, its required and optional
params, its capability class (ADR 0125 §1.5), and optional notes. Everything
that used to be a hand-maintained copy is derived from this table:

| Consumer | Derived via |
|----------|-------------|
| `beamtalk_repl_ops:route/4` | `handler/1` |
| the `describe` op (and each ops module's `describe_ops/0`) | `describe/0`, `describe_for/1` |
| `beamtalk_capability:classify/1` for REPL ops | `capability/1` |
| Rust `RequestBuilder` conformance, `beamtalk-surface-drift`, `docs/repl-protocol.md` checks | `to_json_term/0`, pinned to `test/fixtures/repl_op_table.json` |

**Adding an op is one edit: add an entry to `ops/0`.**

The handler `beamtalk_ws_handler` marks ops served by the WebSocket
transport itself (`stdin`, `run-entry`, `subscribe-logs`, `unsubscribe-logs`);
they appear in `describe` but are not routed through `beamtalk_repl_ops`.

## Why this module lives in `beamtalk_runtime`

`beamtalk_capability` (this app) classifies REPL ops, and the router
(`beamtalk_workspace`) dispatches them. The table is the shared leaf below
both. It is pure data: it never calls a handler module.
""".

-export([
    ops/0,
    lookup/1,
    handler/1,
    capability/1,
    describe/0,
    describe_for/1,
    to_json_term/0
]).

-export_type([entry/0, capability_class/0]).

-type capability_class() :: always | compiler | workspace.

-type entry() :: #{
    op := binary(),
    handler := module(),
    params := [binary()],
    optional := [binary()],
    capability := capability_class(),
    notes => binary()
}.

-define(WS, beamtalk_ws_handler).

-doc "Every REPL protocol op, in documentation order.".
-spec ops() -> [entry()].
ops() ->
    [
        %% --- Evaluation ---
        e(<<"eval">>, beamtalk_repl_ops_eval, [<<"code">>], [<<"trace">>], compiler),
        e(<<"stdin">>, ?WS, [<<"value">>], [], always),
        %% Connected-mode `beamtalk run` (ADR 0099 §3): dispatches a class entry
        %% method with argv. Compile-free, so available in every mode.
        e(<<"run-entry">>, ?WS, [<<"class">>, <<"selector">>], [<<"args">>], always),
        e(<<"interrupt">>, beamtalk_repl_ops_actors, [], [], always),
        %% --- Editor support ---
        e(<<"complete">>, beamtalk_repl_ops_dev, [<<"code">>], [<<"cursor">>], always),
        %% Live-image hover docs; optional `before`/`suffix` resolve keyword selectors.
        e(
            <<"hover">>,
            beamtalk_repl_ops_dev,
            [<<"code">>],
            [<<"before">>, <<"suffix">>],
            always
        ),
        %% Parse-only diagnostics; `mode` is `expression` | `method`; `source_path`
        %% checks under that file's project scope.
        e(
            <<"diagnostics">>,
            beamtalk_repl_ops_dev,
            [<<"code">>],
            [<<"mode">>, <<"source_path">>],
            compiler
        ),
        e(
            <<"show-codegen">>,
            beamtalk_repl_ops_dev,
            [],
            [<<"code">>, <<"class">>, <<"selector">>],
            compiler
        ),
        e(
            <<"erlang-help">>,
            beamtalk_repl_ops_dev,
            [<<"module">>],
            [<<"function">>],
            always
        ),
        e(
            <<"erlang-complete">>,
            beamtalk_repl_ops_dev,
            [],
            [<<"prefix">>, <<"module">>],
            always
        ),
        %% --- Tests ---
        e(<<"test">>, beamtalk_repl_ops_dev, [], [<<"class">>, <<"file">>], always),
        e(<<"test-all">>, beamtalk_repl_ops_dev, [], [], always),
        e(<<"list-tests">>, beamtalk_repl_ops_dev, [], [], always),
        e(<<"load-tests">>, beamtalk_repl_ops_dev, [], [], compiler),
        %% --- Loading and workspace mutation ---
        e(<<"load-source">>, beamtalk_repl_ops_load, [<<"source">>], [], compiler),
        e(
            <<"load-project">>,
            beamtalk_repl_ops_load,
            [],
            [<<"path">>, <<"include_tests">>, <<"force">>, <<"package_name">>],
            compiler
        ),
        e(<<"unload">>, beamtalk_repl_ops_load, [<<"module">>], [], workspace),
        %% Edit, compile, reload and write back a project-owned native module.
        e(
            <<"save-native-source">>,
            beamtalk_repl_ops_load,
            [<<"module">>, <<"source">>],
            [],
            workspace
        ),
        %% Add or rename a `// === Name ===` section divider in a class's source file.
        e(
            <<"save-section">>,
            beamtalk_repl_ops_load,
            [<<"class">>, <<"new_name">>],
            [<<"old_name">>, <<"before_selector">>, <<"before_side">>],
            workspace
        ),
        %% --- Sessions ---
        e(<<"sessions">>, beamtalk_repl_ops_session, [], [], always),
        e(<<"clone">>, beamtalk_repl_ops_session, [], [], always),
        e(<<"close">>, beamtalk_repl_ops_session, [], [], always),
        e(<<"health">>, beamtalk_repl_ops_session, [], [], always),
        e(<<"shutdown">>, beamtalk_repl_ops_session, [<<"cookie">>], [], always),
        %% --- Actors ---
        e(<<"actors">>, beamtalk_repl_ops_actors, [], [], always),
        e(<<"inspect">>, beamtalk_repl_ops_actors, [<<"actor">>], [], always),
        e(<<"kill">>, beamtalk_repl_ops_actors, [<<"actor">>], [], always),
        %% ADR 0095 §5: live-Inspector process metrics, the request/response
        %% companion to the `object` push stream.
        e(
            <<"pid-stats">>,
            beamtalk_repl_ops_watch,
            [<<"actor">>],
            [],
            always,
            <<
                "Live process metrics (queue depth, memory, reductions, status) "
                "for an actor pid."
            >>
        ),
        %% --- Reflection ---
        e(<<"describe">>, beamtalk_repl_ops_dev, [], [], always),
        e(<<"methods">>, beamtalk_repl_ops_dev, [<<"class">>], [], always),
        %% Lazy per-defining-class inherited method listing for the sidebar.
        e(<<"inherited-methods">>, beamtalk_repl_ops_dev, [<<"class">>], [], always),
        e(<<"list-classes">>, beamtalk_repl_ops_dev, [], [<<"filter">>], always),
        %% Reload-induced findings snapshot read (ADR 0105 surface-parity gap).
        e(<<"reload-findings">>, beamtalk_repl_ops_dev, [], [], always),
        e(
            <<"nav-query">>,
            beamtalk_repl_ops_nav,
            [<<"kind">>],
            [
                <<"selector">>,
                <<"class">>,
                <<"module">>,
                <<"code">>,
                <<"before">>,
                <<"suffix">>
            ],
            always
        ),
        e(<<"nav-symbols">>, beamtalk_repl_ops_nav_symbols, [], [<<"scope">>], always),
        %% --- Tracing and performance (ADR 0069) ---
        e(<<"enable-tracing">>, beamtalk_repl_ops_perf, [], [], always),
        e(<<"disable-tracing">>, beamtalk_repl_ops_perf, [], [], always),
        e(
            <<"get-traces">>,
            beamtalk_repl_ops_perf,
            [],
            [
                <<"actor">>,
                <<"selector">>,
                <<"class">>,
                <<"outcome">>,
                <<"min_duration_ns">>,
                <<"limit">>
            ],
            always
        ),
        e(<<"actor-stats">>, beamtalk_repl_ops_perf, [], [<<"actor">>], always),
        e(
            <<"export-traces">>,
            beamtalk_repl_ops_perf,
            [],
            [
                <<"path">>,
                <<"actor">>,
                <<"selector">>,
                <<"class">>,
                <<"outcome">>,
                <<"min_duration_ns">>,
                <<"limit">>
            ],
            always,
            <<"Exports trace events to a JSON file with optional filters.">>
        ),
        %% --- System Browser (ADR 0095) ---
        e(<<"browse-classes">>, beamtalk_repl_ops_browse, [], [], always),
        e(<<"browse-protocols">>, beamtalk_repl_ops_browse, [<<"class">>, <<"side">>], [], always),
        e(
            <<"browse-method-source">>,
            beamtalk_repl_ops_browse,
            [<<"class">>, <<"side">>, <<"selector">>],
            [],
            always
        ),
        e(<<"browse-class-definition">>, beamtalk_repl_ops_browse, [<<"class">>], [], always),
        %% The divider-grouped ("// === Name ===") method view.
        e(<<"browse-categories">>, beamtalk_repl_ops_browse, [<<"class">>], [], always),
        %% One of `class` / `module` is required (`module` addresses a standalone
        %% native module with no backing class); `selector` narrows to a
        %% `handle_call` clause. The one-of rule is enforced by the handler.
        e(
            <<"browse-native-source">>,
            beamtalk_repl_ops_browse,
            [],
            [<<"class">>, <<"module">>, <<"selector">>],
            always,
            <<"One of `class` / `module` is required.">>
        ),
        e(<<"browse-native-modules">>, beamtalk_repl_ops_browse, [], [], always),
        %% ADR 0108 Phase 8: type-alias enumeration and source view. `package`
        %% disambiguates a same-named alias declared by more than one package.
        e(<<"browse-type-aliases">>, beamtalk_repl_ops_browse, [], [], always),
        e(
            <<"browse-alias-source">>,
            beamtalk_repl_ops_browse,
            [<<"name">>],
            [<<"package">>],
            always
        ),
        %% --- Log streaming ---
        e(<<"subscribe-logs">>, ?WS, [], [<<"level">>], always),
        e(<<"unsubscribe-logs">>, ?WS, [], [], always)
    ].

-spec e(binary(), module(), [binary()], [binary()], capability_class()) -> entry().
e(Op, Handler, Params, Optional, Capability) ->
    #{
        op => Op,
        handler => Handler,
        params => Params,
        optional => Optional,
        capability => Capability
    }.

-spec e(binary(), module(), [binary()], [binary()], capability_class(), binary()) -> entry().
e(Op, Handler, Params, Optional, Capability, Notes) ->
    (e(Op, Handler, Params, Optional, Capability))#{notes => Notes}.

-doc "The table entry for `Op`, or `error` if the op is unknown.".
-spec lookup(binary()) -> {ok, entry()} | error.
lookup(Op) ->
    case lists:search(fun(#{op := O}) -> O =:= Op end, ops()) of
        {value, Entry} -> {ok, Entry};
        false -> error
    end.

-doc "The module that handles `Op`, or `error` if the op is unknown.".
-spec handler(binary()) -> {ok, module()} | error.
handler(Op) ->
    case lookup(Op) of
        {ok, #{handler := Handler}} -> {ok, Handler};
        error -> error
    end.

-doc """
The capability class of a REPL op (ADR 0125 §1.5). An op the table does not
name needs nothing a release lacks, so it is `always`.
""".
-spec capability(binary()) -> capability_class().
capability(Op) ->
    case lookup(Op) of
        {ok, #{capability := Capability}} -> Capability;
        error -> always
    end.

-doc """
The `describe` op's `ops` map: every op in the table, keyed by name, with its
`params`, its `optional` params when it has any, and its `notes` when set.
""".
-spec describe() -> #{binary() => map()}.
describe() ->
    describe_entries(ops()).

-doc "Like `describe/0`, restricted to the ops handled by `Handler`.".
-spec describe_for(module()) -> #{binary() => map()}.
describe_for(Handler) ->
    describe_entries([E || #{handler := H} = E <- ops(), H =:= Handler]).

-spec describe_entries([entry()]) -> #{binary() => map()}.
describe_entries(Entries) ->
    maps:from_list([{Op, descriptor(E)} || #{op := Op} = E <- Entries]).

-spec descriptor(entry()) -> map().
descriptor(#{params := Params, optional := Optional} = Entry) ->
    D0 = #{<<"params">> => Params},
    D1 =
        case Optional of
            [] -> D0;
            _ -> D0#{<<"optional">> => Optional}
        end,
    case Entry of
        #{notes := Notes} -> D1#{<<"notes">> => Notes};
        _ -> D1
    end.

-doc """
The table as a JSON-encodable term: a list of objects with `op`, `handler`,
`params`, `optional`, `capability` and (when set) `notes`. This is the shape of
`test/fixtures/repl_op_table.json`, the fixture the Rust clients check
themselves against.
""".
-spec to_json_term() -> [map()].
to_json_term() ->
    [json_entry(E) || E <- ops()].

-spec json_entry(entry()) -> map().
json_entry(#{op := Op, handler := Handler, capability := Cap} = Entry) ->
    Base = #{
        <<"op">> => Op,
        <<"handler">> => atom_to_binary(Handler, utf8),
        <<"params">> => maps:get(params, Entry),
        <<"optional">> => maps:get(optional, Entry),
        <<"capability">> => atom_to_binary(Cap, utf8)
    },
    case Entry of
        #{notes := Notes} -> Base#{<<"notes">> => Notes};
        _ -> Base
    end.
