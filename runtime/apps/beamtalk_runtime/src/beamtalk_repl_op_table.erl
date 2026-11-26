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
    access/1,
    describe/0,
    describe_for/1,
    to_json_term/0
]).

-export_type([entry/0, capability_class/0, access_class/0]).

-type capability_class() :: always | compiler | workspace.

%% Who may call the op (ADR 0091 Decisions 3 and 4): `read` runs no user code,
%% so an Observer may call it; `execute` compiles, loads or runs code or changes
%% the image, the ChangeLog, the working tree or history, so it is Owner-only;
%% `admin` stops actors or the workspace. Not `capability_class()`, which says
%% what a node can do; this says what a caller may do.
-type access_class() :: read | execute | admin.

-type entry() :: #{
    op := binary(),
    handler := module(),
    params := [binary()],
    optional := [binary()],
    capability := capability_class(),
    access := access_class(),
    notes => binary()
}.

-define(WS, beamtalk_ws_handler).

-doc "Every REPL protocol op, in documentation order.".
-spec ops() -> [entry()].
ops() ->
    [
        %% --- Evaluation ---
        e(<<"eval">>, beamtalk_repl_ops_eval, [<<"code">>], [<<"trace">>], compiler, execute),
        e(<<"stdin">>, ?WS, [<<"value">>], [], always, execute),
        %% Connected-mode `beamtalk run` (ADR 0099 §3): dispatches a class entry
        %% method with argv. Compile-free, so available in every mode.
        e(<<"run-entry">>, ?WS, [<<"class">>, <<"selector">>], [<<"args">>], always, execute),
        e(<<"interrupt">>, beamtalk_repl_ops_actors, [], [], always, execute),
        %% --- Editor support ---
        e(<<"complete">>, beamtalk_repl_ops_dev, [<<"code">>], [<<"cursor">>], always, read),
        %% Live-image hover docs; optional `before`/`suffix` resolve keyword
        %% selectors; `class` + `side` (`instance` | `class`), sent together by
        %% method editors, give `self` its type.
        e(
            <<"hover">>,
            beamtalk_repl_ops_dev,
            [<<"code">>],
            [<<"before">>, <<"suffix">>, <<"class">>, <<"side">>],
            always,
            read
        ),
        %% Parse-only diagnostics; `mode` is `expression` | `method`; `source_path`
        %% checks under that file's project scope.
        e(
            <<"diagnostics">>,
            beamtalk_repl_ops_dev,
            [<<"code">>],
            [<<"mode">>, <<"source_path">>],
            compiler,
            read
        ),
        e(
            <<"show-codegen">>,
            beamtalk_repl_ops_dev,
            [],
            [<<"code">>, <<"class">>, <<"selector">>],
            compiler,
            read
        ),
        e(
            <<"erlang-help">>,
            beamtalk_repl_ops_dev,
            [<<"module">>],
            [<<"function">>],
            always,
            read
        ),
        e(
            <<"erlang-complete">>,
            beamtalk_repl_ops_dev,
            [],
            [<<"prefix">>, <<"module">>],
            always,
            read
        ),
        %% --- Tests ---
        e(<<"test">>, beamtalk_repl_ops_dev, [], [<<"class">>, <<"file">>], always, execute),
        e(<<"test-all">>, beamtalk_repl_ops_dev, [], [], always, execute),
        e(<<"list-tests">>, beamtalk_repl_ops_dev, [], [], always, read),
        e(<<"load-tests">>, beamtalk_repl_ops_dev, [], [], compiler, execute),
        %% --- Loading and workspace mutation ---
        e(<<"load-source">>, beamtalk_repl_ops_load, [<<"source">>], [], compiler, execute),
        e(
            <<"load-project">>,
            beamtalk_repl_ops_load,
            [],
            [<<"path">>, <<"include_tests">>, <<"force">>, <<"package_name">>],
            compiler,
            execute
        ),
        e(<<"unload">>, beamtalk_repl_ops_load, [<<"module">>], [], workspace, execute),
        %% Edit, compile, reload and write back a project-owned native module.
        e(
            <<"save-native-source">>,
            beamtalk_repl_ops_load,
            [<<"module">>, <<"source">>],
            [],
            workspace,
            execute
        ),
        %% Add or rename a `// === Name ===` section divider in a class's source file.
        e(
            <<"save-section">>,
            beamtalk_repl_ops_load,
            [<<"class">>, <<"new_name">>],
            [<<"old_name">>, <<"before_selector">>, <<"before_side">>],
            workspace,
            execute
        ),
        %% Code editing (BT-5203): structured-param writes shared by LiveView,
        %% MCP and wire clients; no Beamtalk source is assembled from the params.
        %% Each wraps the chokepoint `compile:source:`, `newClass:at:`,
        %% `changes revert:` and `reload` already converge on.
        e(
            <<"save-method">>,
            beamtalk_repl_ops_edit,
            [<<"class">>, <<"selector">>, <<"source">>],
            [<<"author">>, <<"author_kind">>],
            compiler,
            execute,
            <<"Durably install a method; `source` is the body right of `=>`.">>
        ),
        e(
            <<"save-class">>,
            beamtalk_repl_ops_edit,
            [<<"source">>, <<"path">>],
            [<<"author">>, <<"author_kind">>],
            compiler,
            execute,
            <<"Queue a new class at `path`; `author_kind` is `human` | `agent` (ChangeLog audit).">>
        ),
        e(
            <<"revert-method">>,
            beamtalk_repl_ops_edit,
            [<<"class">>, <<"selector">>],
            [<<"side">>],
            workspace,
            execute
        ),
        e(<<"reload-file">>, beamtalk_repl_ops_edit, [<<"path">>], [], compiler, execute),
        %% Workspace state (BT-5204): the Changes pane, flush and the workspace
        %% facts a client needs, shared by LiveView, MCP and wire clients. The
        %% findings snapshot is the `reload-findings` op below.
        e(
            <<"flush">>,
            beamtalk_repl_ops_state,
            [],
            [<<"class">>, <<"file">>, <<"kind">>, <<"selector">>, <<"confirm_destructive">>],
            workspace,
            execute,
            <<
                "Write pending ChangeLog entries to disk. At most one of `class`, `file`, "
                "`kind` scopes it (each a non-empty String; `kind` is `new-class`, "
                "`remove-method`, `remove-class`, `rename-class` or `rename-method`); "
                "`selector` (a method selector) scopes it to that method and may be combined "
                "with `class`, but not `file` or `kind`; "
                "`confirm_destructive: true` also applies Tier 2 entries."
            >>
        ),
        e(<<"changes">>, beamtalk_repl_ops_state, [], [], always, read),
        e(
            <<"info">>,
            beamtalk_repl_ops_state,
            [],
            [],
            always,
            read,
            <<"Version, node, workspace id and the settings a client needs.">>
        ),
        %% Git working tree (BT-5205): shared by LiveView's Git panel and wire
        %% clients. Every `path` is checked by `beamtalk_repl_paths` first; reads
        %% are bounded. The mutating ops change the index, working tree or
        %% history, so they are `workspace` (refused in a release).
        e(<<"git-status">>, beamtalk_repl_ops_git, [], [], always, read),
        e(<<"git-diff">>, beamtalk_repl_ops_git, [<<"path">>], [], always, read),
        e(<<"git-log">>, beamtalk_repl_ops_git, [], [<<"count">>], always, read),
        e(<<"git-stage">>, beamtalk_repl_ops_git, [<<"path">>], [], workspace, execute),
        e(<<"git-unstage">>, beamtalk_repl_ops_git, [<<"path">>], [], workspace, execute),
        e(
            <<"git-commit">>,
            beamtalk_repl_ops_git,
            [<<"message">>],
            [],
            workspace,
            execute,
            <<"Commit the staged index through system `git`, so hooks and signing apply.">>
        ),
        e(<<"git-revert-file">>, beamtalk_repl_ops_git, [<<"path">>], [], workspace, execute),
        %% --- Sessions ---
        e(<<"sessions">>, beamtalk_repl_ops_session, [], [], always, read),
        e(<<"close">>, beamtalk_repl_ops_session, [], [], always, execute),
        e(<<"health">>, beamtalk_repl_ops_session, [], [], always, read),
        e(<<"shutdown">>, beamtalk_repl_ops_session, [<<"cookie">>], [], always, admin),
        %% --- Actors ---
        e(<<"actors">>, beamtalk_repl_ops_actors, [], [], always, read),
        e(<<"inspect">>, beamtalk_repl_ops_actors, [<<"actor">>], [], always, read),
        e(<<"kill">>, beamtalk_repl_ops_actors, [<<"actor">>], [], always, admin),
        %% ADR 0095 §5: live-Inspector process metrics, the request/response
        %% companion to the `object` push stream.
        e(
            <<"pid-stats">>,
            beamtalk_repl_ops_watch,
            [<<"actor">>],
            [],
            always,
            read,
            <<
                "Live process metrics (queue depth, memory, reductions, status) "
                "for an actor pid."
            >>
        ),
        %% Process navigation (BT-5205): a pid is the node-local text or, over
        %% distribution, the live pid. `children` keeps live handles for the
        %% Inspector and encodes them as stable ids on the wire.
        e(
            <<"children">>,
            beamtalk_repl_ops_process,
            [<<"supervisor">>],
            [],
            always,
            read,
            <<"List a supervisor's children as drillable rows.">>
        ),
        e(<<"process-info">>, beamtalk_repl_ops_process, [<<"pid">>], [], always, read),
        %% --- Reflection ---
        e(<<"describe">>, beamtalk_repl_ops_dev, [], [], always, read),
        e(<<"methods">>, beamtalk_repl_ops_dev, [<<"class">>], [], always, read),
        %% Lazy per-defining-class inherited method listing for the sidebar.
        e(<<"inherited-methods">>, beamtalk_repl_ops_dev, [<<"class">>], [], always, read),
        e(<<"list-classes">>, beamtalk_repl_ops_dev, [], [<<"filter">>], always, read),
        %% Reload-induced findings snapshot read (ADR 0105 surface-parity gap).
        e(<<"reload-findings">>, beamtalk_repl_ops_dev, [], [], always, read),
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
            always,
            read
        ),
        e(<<"nav-symbols">>, beamtalk_repl_ops_nav_symbols, [], [<<"scope">>], always, read),
        %% --- Tracing and performance (ADR 0069) ---
        e(<<"enable-tracing">>, beamtalk_repl_ops_perf, [], [], always, execute),
        e(<<"disable-tracing">>, beamtalk_repl_ops_perf, [], [], always, execute),
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
            always,
            read
        ),
        e(<<"actor-stats">>, beamtalk_repl_ops_perf, [], [<<"actor">>], always, read),
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
            execute,
            <<"Exports trace events to a JSON file with optional filters.">>
        ),
        %% --- System Browser (ADR 0095) ---
        e(<<"browse-classes">>, beamtalk_repl_ops_browse, [], [], always, read),
        e(
            <<"browse-protocols">>,
            beamtalk_repl_ops_browse,
            [<<"class">>, <<"side">>],
            [],
            always,
            read
        ),
        e(
            <<"browse-method-source">>,
            beamtalk_repl_ops_browse,
            [<<"class">>, <<"side">>, <<"selector">>],
            [],
            always,
            read
        ),
        e(<<"browse-class-definition">>, beamtalk_repl_ops_browse, [<<"class">>], [], always, read),
        %% The divider-grouped ("// === Name ===") method view.
        e(<<"browse-categories">>, beamtalk_repl_ops_browse, [<<"class">>], [], always, read),
        %% One of `class` / `module` is required (`module` addresses a standalone
        %% native module with no backing class); `selector` narrows to a
        %% `handle_call` clause. The one-of rule is enforced by the handler.
        e(
            <<"browse-native-source">>,
            beamtalk_repl_ops_browse,
            [],
            [<<"class">>, <<"module">>, <<"selector">>],
            always,
            read,
            <<"One of `class` / `module` is required.">>
        ),
        e(<<"browse-native-modules">>, beamtalk_repl_ops_browse, [], [], always, read),
        %% ADR 0108 Phase 8: type-alias enumeration and source view. `package`
        %% disambiguates a same-named alias declared by more than one package.
        e(<<"browse-type-aliases">>, beamtalk_repl_ops_browse, [], [], always, read),
        e(
            <<"browse-alias-source">>,
            beamtalk_repl_ops_browse,
            [<<"name">>],
            [<<"package">>],
            always,
            read
        ),
        %% --- Log streaming ---
        e(<<"subscribe-logs">>, ?WS, [], [<<"level">>], always, read),
        e(<<"unsubscribe-logs">>, ?WS, [], [], always, read)
    ].

-spec e(binary(), module(), [binary()], [binary()], capability_class(), access_class()) ->
    entry().
e(Op, Handler, Params, Optional, Capability, Access) ->
    #{
        op => Op,
        handler => Handler,
        params => Params,
        optional => Optional,
        capability => Capability,
        access => Access
    }.

-spec e(
    binary(), module(), [binary()], [binary()], capability_class(), access_class(), binary()
) -> entry().
e(Op, Handler, Params, Optional, Capability, Access, Notes) ->
    (e(Op, Handler, Params, Optional, Capability, Access))#{notes => Notes}.

-doc "The table entry for `Op`, or `error` if the op is unknown.".
-spec lookup(binary()) -> {ok, entry()} | error.
lookup(Op) ->
    case op_map() of
        #{Op := Entry} -> {ok, Entry};
        #{} -> error
    end.

%% The op table indexed by op name, built once per module version and cached.
%% The key carries the module's md5, so a hot-loaded version rebuilds the map
%% instead of answering from the old table.
-spec op_map() -> #{binary() => entry()}.
op_map() ->
    Key = {?MODULE, op_map, ?MODULE:module_info(md5)},
    case persistent_term:get(Key, undefined) of
        undefined ->
            Map = maps:from_list([{Op, E} || #{op := Op} = E <- ops()]),
            persistent_term:put(Key, Map),
            Map;
        Map ->
            Map
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
The access class of a REPL op: `read`, `execute` or `admin`. Every entry has one
(`e/6,7` takes it), so an op cannot be added without deciding who may call it.
LiveView's `BtAttach.Facade` is checked against this (its per-op RBAC class must
equal the class of the wire op it dispatches), and an op the table does not name
is `error`, never read as permitted.
""".
-spec access(binary()) -> {ok, access_class()} | error.
access(Op) ->
    case lookup(Op) of
        {ok, #{access := Access}} -> {ok, Access};
        error -> error
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
`params`, `optional`, `capability`, `access` and (when set) `notes`. This is the shape of
`test/fixtures/repl_op_table.json`, the fixture the Rust clients check
themselves against.
""".
-spec to_json_term() -> [map()].
to_json_term() ->
    [json_entry(E) || E <- ops()].

-spec json_entry(entry()) -> map().
json_entry(#{op := Op, handler := Handler, capability := Cap, access := Access} = Entry) ->
    Base = #{
        <<"op">> => Op,
        <<"handler">> => atom_to_binary(Handler, utf8),
        <<"params">> => maps:get(params, Entry),
        <<"optional">> => maps:get(optional, Entry),
        <<"capability">> => atom_to_binary(Cap, utf8),
        <<"access">> => atom_to_binary(Access, utf8)
    },
    case Entry of
        #{notes := Notes} -> Base#{<<"notes">> => Notes};
        _ -> Base
    end.
