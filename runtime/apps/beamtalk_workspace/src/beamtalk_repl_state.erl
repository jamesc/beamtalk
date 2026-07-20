%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_state).

%%% **DDD Context:** REPL Session Context

-moduledoc """
State management for Beamtalk REPL

This module defines the REPL state record and provides utilities
for manipulating state during REPL sessions.
""".

-export([
    new/2, new/3,
    get_bindings/1,
    set_bindings/2,
    clear_bindings/1,
    get_eval_counter/1,
    increment_eval_counter/1,
    get_client_meta/1,
    get_client_kind/1,
    get_loaded_modules/1,
    add_loaded_module/2,
    get_listen_socket/1,
    get_port/1,
    get_actor_registry/1,
    set_actor_registry/2,
    get_module_tracker/1,
    set_module_tracker/2,
    %% BT-1242: pending module removals (deferred during active eval)
    get_pending_module_removals/1,
    add_pending_module_removal/2,
    clear_pending_module_removals/1,
    %% BT-2366 (ADR 0081 Phase 2): pending session-local mutations (deferred
    %% during active eval).
    get_pending_mutations/1,
    add_pending_mutation/2,
    clear_pending_mutations/1,
    %% ADR 0108 Phase 8 (BT-2902): session-local type alias table.
    get_alias_table/1,
    put_alias/3,
    known_type_alias_sources/1
]).

-export_type([state/0, mutation/0, alias_entry/0]).

-record(state, {
    listen_socket :: gen_tcp:socket() | undefined,
    port :: inet:port_number(),
    %% Origin/debug metadata for this session, set once at creation (immutable for
    %% its lifetime) and surfaced by `Workspace sessions` / `Session kind` /
    %% `Session info`. An extensible map so new debug fields are just new keys:
    %%   kind         :: binary()            -- always present; the client surface
    %%                                          (`repl`/`mcp`/`lsp`/`liveview`/…)
    %%   peer         :: binary() | undefined -- remote "host:port" (ws clients)
    %%   node         :: atom()   | undefined -- calling node (dist-attached)
    %%   user         :: binary() | undefined -- authenticated user (LiveView)
    %%   connected_at :: integer()| undefined -- system_time microsecond at start
    client_meta :: map(),
    %% BT-2365 (ADR 0081 Phase 1): holds ONLY session locals. Workspace globals
    %% (singletons + bind:as: names) are resolved lazily at eval time rather than
    %% injected here, so there is no injected_ws_keys reconciliation field.
    bindings :: map(),
    eval_counter :: non_neg_integer(),
    loaded_modules :: [atom()],
    actor_registry :: pid() | undefined,
    module_tracker :: beamtalk_repl_modules:module_tracker(),
    %% BT-1242: Modules removed via class_removed event while an eval worker
    %% is active.  Deferred here so they can be applied to the worker's returned
    %% state when eval_result arrives (prevents the worker snapshot from
    %% reinstating modules that were removed during the eval).
    pending_module_removals :: [atom()],
    %% BT-2366 (ADR 0081 Phase 2): session-local mutations issued by primitives
    %% (Session bindings at:put:/removeKey:, Session clear) while an eval worker
    %% is active.  Primitives enqueue {op, Key, Value} tuples here via a
    %% gen_server:call to the shell rather than writing directly, because the
    %% worker holds a state snapshot whose writeback would clobber a direct edit.
    %% Drained in the eval-exit clauses (apply_pending_mutations/2) in enqueue
    %% order — preserves insertion order (newest appended at the tail).
    pending_mutations :: [mutation()],
    %% ADR 0108 Phase 8 (BT-2902): type aliases declared directly at this
    %% session's REPL prompt (`type Name = ...`). Aliases erase entirely at
    %% resolution time (ADR 0108 Semantics) — there is no loaded BEAM module
    %% or process to recover them from on a later turn the way classes are
    %% recovered via `beamtalk_runtime_api`/`beamtalk_class_registry`, so
    %% this table is session state's own source of truth, not a cache.
    %% Keyed by alias name; `known_type_alias_sources/1` derives the list
    %% resent as the `known_type_aliases` request field on every subsequent
    %% turn (`beamtalk_repl_compiler:compile_expression/4`'s
    %% `KnownTypeAliasSources` argument).
    alias_table :: #{binary() => alias_entry()}
}).

-type mutation() ::
    {put, atom(), term()}
    | {remove, atom(), undefined}
    | {clear, undefined, undefined}.

-doc """
A session-visible type alias entry (ADR 0108 Phase 8, BT-2902; stdlib
seeding BT-2938).

`expansion` is the alias's unparsed `TypeAnnotation` display form (e.g.
`<<"#north | #south | #east | #west">>`) — both what `:help` renders and
what round-trips through `known_type_alias_sources/1` as a reparseable
`type Name = <expansion>` line. `doc_comment` is `undefined` when the
declaration had no `///` doc comment.

`declared_in` is `:help`'s provenance line (`format_alias_help/2` in
`beamtalk_repl_eval.erl`): `<<"REPL">>` for a `type Name = ...` this
session declared itself (exactly as returned by the compiler port's
`type_alias_definition` response), or `<<"stdlib">>` for an alias seeded
from stdlib's own compiled declarations (`stdlib_alias_table/0`) — a
session never has to declare `SupervisionStrategy` etc. itself for `:help`
or a `::` annotation to resolve it.
""".
-type alias_entry() :: #{
    expansion := binary(),
    doc_comment := binary() | undefined,
    declared_in := binary()
}.

-opaque state() :: #state{}.

-doc "Create a new REPL state.".
-spec new(gen_tcp:socket() | undefined, inet:port_number()) -> state().
new(ListenSocket, Port) ->
    new(ListenSocket, Port, #{}).

-doc """
Create a new REPL state with options.

`Options` may carry `client_meta => map()` to record session origin/debug
metadata. The stored map always has a `kind` key (defaults to
`#{kind => <<"unknown">>}` when absent or missing the key). Keys whose value is
`undefined` are dropped so `Session info` only ever surfaces fields that are
actually known (matching the documented "where known" contract — e.g. a
WebSocket peer that could not be formatted is omitted, not shown as
`undefined`).
""".
-spec new(gen_tcp:socket() | undefined, inet:port_number(), map()) -> state().
new(ListenSocket, Port, Options) ->
    Meta0 = maps:get(client_meta, Options, #{}),
    Known = maps:filter(fun(_K, V) -> V =/= undefined end, Meta0),
    Meta = maps:merge(#{kind => <<"unknown">>}, Known),
    #state{
        listen_socket = ListenSocket,
        port = Port,
        client_meta = Meta,
        bindings = #{},
        eval_counter = 0,
        loaded_modules = [],
        actor_registry = undefined,
        module_tracker = beamtalk_repl_modules:new(),
        pending_module_removals = [],
        pending_mutations = [],
        %% BT-2938: seed with stdlib's own compiled aliases so `:help
        %% <StdlibAlias>` and `::`-typed locals referencing one resolve in a
        %% fresh session — see `stdlib_alias_table/0`. A later `type Name =
        %% ...` declaration at this session's REPL prompt (`put_alias/3`)
        %% legally shadows a same-named stdlib entry (ADR 0108 Semantics'
        %% "current turn wins" precedent).
        alias_table = stdlib_alias_table()
    }.

-doc "Get current variable bindings.".
-spec get_bindings(state()) -> map().
get_bindings(#state{bindings = Bindings}) ->
    Bindings.

-doc "Set variable bindings.".
-spec set_bindings(map(), state()) -> state().
set_bindings(Bindings, State) ->
    State#state{bindings = Bindings}.

-doc "Clear all session-local variable bindings (BT-2365: locals only).".
-spec clear_bindings(state()) -> state().
clear_bindings(State) ->
    State#state{bindings = #{}}.

-doc "Get current eval counter.".
-spec get_eval_counter(state()) -> non_neg_integer().
get_eval_counter(#state{eval_counter = Counter}) ->
    Counter.

-doc "Increment eval counter and return new state.".
-spec increment_eval_counter(state()) -> state().
increment_eval_counter(State = #state{eval_counter = Counter}) ->
    State#state{eval_counter = Counter + 1}.

-doc "Get the full session origin/debug metadata map (always includes `kind`).".
-spec get_client_meta(state()) -> map().
get_client_meta(#state{client_meta = Meta}) ->
    Meta.

-doc "Get the originating client surface (the `kind` key of the metadata map).".
-spec get_client_kind(state()) -> binary().
get_client_kind(#state{client_meta = Meta}) ->
    maps:get(kind, Meta, <<"unknown">>).

-doc "Get loaded modules list.".
-spec get_loaded_modules(state()) -> [atom()].
get_loaded_modules(#state{loaded_modules = Modules}) ->
    Modules.

-doc "Add a loaded module to the state.".
-spec add_loaded_module(atom(), state()) -> state().
add_loaded_module(Module, State = #state{loaded_modules = Modules}) ->
    State#state{loaded_modules = [Module | Modules]}.

-doc "Get listen socket.".
-spec get_listen_socket(state()) -> gen_tcp:socket() | undefined.
get_listen_socket(#state{listen_socket = Socket}) ->
    Socket.

-doc "Get port number.".
-spec get_port(state()) -> inet:port_number().
get_port(#state{port = Port}) ->
    Port.

-doc "Get actor registry PID.".
-spec get_actor_registry(state()) -> pid() | undefined.
get_actor_registry(#state{actor_registry = Registry}) ->
    Registry.

-doc "Set actor registry PID.".
-spec set_actor_registry(pid() | undefined, state()) -> state().
set_actor_registry(Registry, State) ->
    State#state{actor_registry = Registry}.

-doc "Get module tracker.".
-spec get_module_tracker(state()) -> beamtalk_repl_modules:module_tracker().
get_module_tracker(#state{module_tracker = Tracker}) ->
    Tracker.

-doc "Set module tracker.".
-spec set_module_tracker(beamtalk_repl_modules:module_tracker(), state()) -> state().
set_module_tracker(Tracker, State) ->
    State#state{module_tracker = Tracker}.

-doc """
Get pending module removals (deferred during active eval).

BT-1242: Returns the list of module atoms queued for removal while an eval
worker was running.  Applied to the worker result state in eval_result handler.
""".
-spec get_pending_module_removals(state()) -> [atom()].
get_pending_module_removals(#state{pending_module_removals = Removals}) ->
    Removals.

-doc """
Add a module to the pending-removals list.

BT-1242: Called by handle_info({class_removed, ...}) when an eval worker is
active.  Deduplicates via ordsets so repeated removals are idempotent.
""".
-spec add_pending_module_removal(atom(), state()) -> state().
add_pending_module_removal(Module, #state{pending_module_removals = Removals} = State) ->
    State#state{pending_module_removals = ordsets:add_element(Module, Removals)}.

-doc """
Clear the pending-removals list.

BT-1242: Called after applying pending removals on interrupt or worker crash,
so the shell returns to idle with a clean slate.
""".
-spec clear_pending_module_removals(state()) -> state().
clear_pending_module_removals(State) ->
    State#state{pending_module_removals = []}.

-doc """
Get pending session-local mutations (deferred during active eval).

BT-2366 (ADR 0081 Phase 2): returns the queued `{op, Key, Value}` tuples in
enqueue order (oldest first).  Drained by the eval-exit clauses in
`beamtalk_repl_shell` (`apply_pending_mutations/2`).
""".
-spec get_pending_mutations(state()) -> [mutation()].
get_pending_mutations(#state{pending_mutations = Mutations}) ->
    Mutations.

-doc """
Append a session-local mutation to the pending queue.

BT-2366 (ADR 0081 Phase 2): called by `beamtalk_session_primitives` (via the
shell `enqueue_mutation` handler) when a session-scope write is issued during
an active eval.  Appends at the tail so the queue preserves enqueue order; the
fold in `apply_pending_mutations/2` then replays `put`/`remove`/`clear` in the
order the user issued them.
""".
-spec add_pending_mutation(mutation(), state()) -> state().
add_pending_mutation(Mutation, #state{pending_mutations = Mutations} = State) ->
    State#state{pending_mutations = Mutations ++ [Mutation]}.

-doc """
Clear the pending session-local mutations queue.

BT-2366 (ADR 0081 Phase 2): called after applying (or discarding) the queue on
an eval-exit path, so the shell returns to idle with an empty queue.
""".
-spec clear_pending_mutations(state()) -> state().
clear_pending_mutations(State) ->
    State#state{pending_mutations = []}.

-doc """
Get the session's type alias table (ADR 0108 Phase 8, BT-2902).

Keyed by alias name (binary). Consulted by the `:help <Alias>` interception
in `beamtalk_repl_eval:do_eval/3` — see that module for why `:help` cannot
reach this table via the ordinary `Beamtalk help: X` eval path (aliases have
no live BEAM class/process for `beamtalk_class_registry` to resolve).
""".
-spec get_alias_table(state()) -> #{binary() => alias_entry()}.
get_alias_table(#state{alias_table = AliasTable}) ->
    AliasTable.

-doc """
Register (or redefine) a session-local type alias.

BT-2902: called by `beamtalk_repl_eval:handle_type_alias_definition/3`
after the compiler port validates a `type Name = ...` declaration.
Redeclaring an existing name overwrites its entry — ADR 0108 Semantics
treats a live REPL redefinition as legal. `handle_type_alias_definition/3`
follows this call with `beamtalk_compiler_server:register_aliases/1` (keeps
the compiler port's ambient alias cache in sync) and
`beamtalk_repl_loader:spawn_alias_change_recheck/1` (ADR 0108 hot-reload
re-check trigger, BT-2899) — re-checking annotation sites that referenced
the old binding, once out of scope here, is now that trigger's job.
""".
-spec put_alias(binary(), alias_entry(), state()) -> state().
put_alias(Name, Entry, #state{alias_table = AliasTable} = State) ->
    State#state{alias_table = AliasTable#{Name => Entry}}.

-doc """
Derive the `known_type_aliases` compiler-port request field: one
reparseable `type Name = <expansion>` line per session-local alias (ADR
0108 Phase 8, BT-2902).

Deliberately omits the doc comment — round-tripping it is unnecessary
(`known_type_aliases` only feeds `resolve_type_annotation`'s structural
lookup, never `:help`, which reads `alias_table` directly) and would risk a
multi-line doc comment corrupting the single-line reparse.
""".
-spec known_type_alias_sources(state()) -> [binary()].
known_type_alias_sources(#state{alias_table = AliasTable}) ->
    [
        <<"type ", Name/binary, " = ", (maps:get(expansion, Entry))/binary>>
     || {Name, Entry} <- maps:to_list(AliasTable)
    ].

-doc """
Seed a fresh session's alias table from stdlib's own compiled `type Name =
...` declarations (BT-2938).

Reads `beamtalk_stdlib`'s `.app` `{type_aliases, [...]}` env key (ADR 0108
Phase 8/BT-2903, written by `build_stdlib.rs`'s `generate_app_file`/
`generate_app_src_file` from `app_file::AliasMetadata`) — the same durable
record `beamtalk_repl_ops_browse:package_type_aliases/1` reads for
`browse-type-aliases`. Without this, a stdlib alias like `SupervisionStrategy`
(`stdlib/src/Supervisor.bt`) has no live BEAM class/process the way a stdlib
*class* does (`beamtalk_class_registry`, populated at boot from the same
`.app`'s `{classes, [...]}` env) for a fresh session to already know about —
aliases erase entirely at compile time, so this session-state table is the
only place a bare `:help SupervisionStrategy` or a `::`-typed local
referencing it can resolve from.

Only public (non-`internal`) aliases are seeded: an `internal type Foo =
...` stdlib alias is usable only *within* stdlib itself (ADR 0108
Semantics), and a REPL session is never "inside" the stdlib package —
mirrors the seeding-boundary exclusion `beamtalk_repl_ops_browse:
alias_visible/2` applies to every non-project package's internal aliases.

Best-effort: `[]`/`#{}` (an empty seed, not a crash) if `beamtalk_stdlib`'s
env is missing/malformed or a row doesn't have the shape
`format_type_aliases_entry` always emits — a corrupted or
toolchain-mismatched `.app` file must not prevent a session from starting.
""".
-spec stdlib_alias_table() -> #{binary() => alias_entry()}.
stdlib_alias_table() ->
    try
        lists:foldl(fun stdlib_alias_fold/2, #{}, stdlib_type_aliases_env())
    catch
        _:_ -> #{}
    end.

%% `beamtalk_stdlib`'s `.app` `{type_aliases, [...]}` env key, `[]` if
%% absent (older `.app` build) or malformed.
-spec stdlib_type_aliases_env() -> [map()].
stdlib_type_aliases_env() ->
    case application:get_env(beamtalk_stdlib, type_aliases) of
        {ok, Aliases} when is_list(Aliases) -> Aliases;
        _ -> []
    end.

%% Fold one `.app`-env alias row into the session alias table being built,
%% dropping internal rows (see `stdlib_alias_table/0` doc) and any row
%% missing the `name`/`expansion` keys every `format_type_aliases_entry`
%% row carries.
%%
%% Fails **safe**, not open, on a missing (or non-boolean) `internal` key
%% (`maps:get(internal, Entry, true)` plus the catch-all `_ -> Acc` case
%% arm below — should never happen, `format_type_aliases_entry` always
%% emits a proper boolean): mirrors `beamtalk_repl_ops_browse:
%% alias_visible/2`'s explicit "no `internal` key at all... fail safe by
%% treating as internal" convention, so a row shape this reader doesn't
%% yet know about can't silently leak a should-be-internal alias into
%% every fresh REPL session instead of just being dropped. The catch-all
%% case arm also keeps this row-level-skip, not whole-table-wipe: without
%% it, a non-boolean `internal` value (e.g. a hand-edited `.app` file)
%% would throw `case_clause`, caught by `stdlib_alias_table/0`'s outer
%% `try/catch` as an empty table — discarding every other, well-formed
%% row along with this one bad row.
-spec stdlib_alias_fold(term(), #{binary() => alias_entry()}) -> #{binary() => alias_entry()}.
stdlib_alias_fold(#{name := Name, expansion := Expansion} = Entry, Acc) ->
    case maps:get(internal, Entry, true) of
        true ->
            Acc;
        false ->
            Acc#{
                app_env_binary(Name) => #{
                    expansion => app_env_binary(Expansion),
                    doc_comment => app_env_doc(maps:get(doc, Entry, undefined)),
                    declared_in => <<"stdlib">>
                }
            };
        _ ->
            Acc
    end;
stdlib_alias_fold(_Entry, Acc) ->
    Acc.

%% Normalise a `.app`-file alias field (atom | Erlang string() | binary(),
%% never JSON — `app_file.rs` emits a literal `.app` Erlang term consulted
%% via `application:get_env/2`, not a wire payload) to the binary
%% `alias_entry()` uses. Deliberately not shared with
%% `beamtalk_repl_ops_browse:alias_field_binary/1` (that helper is
%% `-ifdef(TEST)`-exported only, and maps an absent `doc` to `null`, a
%% term-op/JSON convention this module's `undefined` doesn't want).
-spec app_env_binary(atom() | string() | binary()) -> binary().
app_env_binary(V) when is_binary(V) ->
    V;
app_env_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
app_env_binary(V) when is_list(V) ->
    case unicode:characters_to_binary(V) of
        B when is_binary(B) -> B;
        _ -> list_to_binary(V)
    end.

-spec app_env_doc(undefined | atom() | string() | binary()) -> binary() | undefined.
app_env_doc(undefined) ->
    undefined;
app_env_doc(V) ->
    app_env_binary(V).
