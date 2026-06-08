%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_primitives).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Runtime primitives backing the `Session` and `BindingsView` stdlib classes
(ADR 0081 Phase 3, BT-2366).

`Session` is not an actor; its values are tagged maps minted by the factory
primitives here (the `FileHandle`/`Port` representation pattern):

```erlang
#{'$beamtalk_class' => 'Session', id => SessionId :: binary(), pid => ShellPid :: pid()}
```

`current/0` reads the calling session from the eval-worker process dictionary
(seeded in `beamtalk_repl_shell:seed_session_context/3`).  `withId/1` looks a
session up by protocol id with a liveness check, so a captured value never
carries a dead PID.

Instance operations take the `Session` value (`self`) and act on the session it
names.  Session-local **writes** are enqueued on the *calling* session's
`pending_mutations` queue (Phase 2) via a `gen_server:call` to the shell — safe
from the worker because the shell is in `noreply` while the worker runs.
Cross-session writes are rejected (`cross_session_mutation_unsupported`) because
the target shell's in-flight eval worker would clobber them.

`BindingsView` values are likewise tagged maps:

```erlang
%% session scope (read/write the named session's locals)
#{'$beamtalk_class' => 'BindingsView', scope => session, id => binary(), pid => pid()}
%% workspace scope (read singletons + bind:as: live; write through bind/2 / unbind/1)
#{'$beamtalk_class' => 'BindingsView', scope => workspace}
```

Every cross-session send (`bindingsViewFor`, `resolveFor`, `clearFor`, `idOf`,
and the view read/write primitives) liveness-checks the carried PID and raises
`#beamtalk_error{kind = session_not_found}` rather than issuing a `gen_server:call`
that would block to timeout against a dead PID.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Bounded per-shell metadata lookup so one wedged shell cannot stall the whole
%% enumeration (`liveSessions/0`) or a `withId/1` mint on the default 5s
%% gen_server:call timeout. A live, idle shell answers get_session_meta in
%% microseconds (it reads the state tuple, in any worker state), so 1s is
%% generous slack for a busy-but-responsive shell while still skipping a
%% genuinely wedged one promptly.
-define(SESSION_ID_LOOKUP_TIMEOUT, 1000).

%% Factory (class-side)
-export([current/0, withId/1, idOf/1, liveSessions/0]).
%% Session operations (instance-side, take a Session value)
-export([bindingsViewFor/1, resolveFor/2, clearFor/1, kindOf/1, infoFor/1, printStringFor/1]).
%% Workspace-globals view (not a Session method)
-export([globalsView/0]).
%% BindingsView read/write primitives
-export([
    view_at/2,
    view_at_put/3,
    view_includes_key/2,
    view_remove/2,
    view_keys/1,
    view_values/1,
    view_size/1
]).

-type session() :: #{
    '$beamtalk_class' := 'Session',
    id := binary(),
    pid := pid(),
    %% Origin/debug metadata carried from the shell (always includes `kind`).
    %% Read-only display data — `kindOf`/`infoFor`/`printStringFor` read it
    %% without a liveness check so a dead session can still be inspected.
    meta := map()
}.

-type bindings_view() ::
    #{'$beamtalk_class' := 'BindingsView', scope := session, id := binary(), pid := pid()}
    | #{'$beamtalk_class' := 'BindingsView', scope := workspace}.

-export_type([session/0, bindings_view/0]).

%%% ============================================================================
%%% Factory primitives (class-side)
%%% ============================================================================

-doc """
Return a `Session` value for the calling session, or `nil` outside an eval
context.

Reads `beamtalk_session_pid` / `beamtalk_session_id` from the process
dictionary, seeded by `beamtalk_repl_shell:seed_session_context/3` on every
eval-worker spawn.  Compiled program code (no shell-spawned worker) finds
`beamtalk_session_pid =:= undefined` and gets `nil`.
""".
-spec current() -> session() | nil.
current() ->
    case {get(beamtalk_session_pid), get(beamtalk_session_id)} of
        {Pid, Id} when is_pid(Pid), is_binary(Id) ->
            Meta =
                case get(beamtalk_session_meta) of
                    M when is_map(M) -> M;
                    _ -> #{}
                end,
            make_session(Id, Pid, Meta);
        _ ->
            nil
    end.

-doc """
Look up a session by its protocol id, returning a `Session` value or `nil`.

Uses `beamtalk_session_table:lookup_alive/1` (the `resolve_pid` liveness
discipline), so a stale ETS entry pointing at a dead PID returns `nil` rather
than a `Session` carrying a dead PID.  `aSessionId` may be a binary or a string
(list of codepoints).
""".
-spec withId(binary() | string() | term()) -> session() | nil.
withId(SessionId) ->
    case to_binary_id(SessionId) of
        {ok, Id} ->
            case beamtalk_session_table:lookup_alive(Id) of
                {ok, Pid} -> make_session(Id, Pid, fetch_meta(Pid));
                error -> nil
            end;
        error ->
            raise_id_type_error('withId:', SessionId)
    end.

-doc """
Extract the protocol id (a String) from a `Session` value.

Liveness-checked per ADR 0081 Phase 3: a captured `Session` whose backing shell
has died raises `session_not_found` rather than returning a stale id, so every
send to a `Session` value fails consistently once the session is gone.
""".
-spec idOf(session() | term()) -> binary().
idOf(Session) ->
    {Id, Pid} = session_id_pid(Session, 'idOf:'),
    ensure_alive(Pid, Id),
    Id.

-doc """
Return a `Session` value for every live shell, backing `Workspace sessions`
(ADR 0081 Phase 7, BT-2368).

Enumerates live shells from `beamtalk_session_sup` (the same live-shell source as
the `sessions` protocol op, covering CLI, WebSocket, and MCP) and mints one
`Session` value per shell the same way `withId/1` does — `{id, pid}` carrying the
shell's protocol session id (queried via `beamtalk_repl_shell:get_session_id/1`)
and its PID. The minted values therefore work with the instance reads (`id`,
`bindings`) and reject cross-session writes.

Returns `[]` when the session supervisor is not running (e.g. a bare runtime
node with no REPL server). A shell that dies or stops answering between
`which_children/1` and the per-shell id query is skipped rather than minting a
value carrying a dead PID or blocking the whole enumeration.
""".
-spec liveSessions() -> [session()].
liveSessions() ->
    case whereis(beamtalk_session_sup) of
        undefined ->
            [];
        _Sup ->
            Children = supervisor:which_children(beamtalk_session_sup),
            lists:filtermap(fun mint_live_session/1, Children)
    end.

%%% ============================================================================
%%% Session operations (instance-side)
%%% ============================================================================

-doc """
Return a `BindingsView` over the named session's locals map.

The view records the target session's id and PID so cross-session writes can be
rejected.  A read against this view (`view_at/2`, `view_keys/1`, …) goes to the
target shell's `get_bindings` (its locals-only map after BT-2365).
""".
-spec bindingsViewFor(session() | term()) -> bindings_view().
bindingsViewFor(Session) ->
    {Id, Pid} = session_id_pid(Session, 'bindingsViewFor:'),
    ensure_alive(Pid, Id),
    #{'$beamtalk_class' => 'BindingsView', scope => session, id => Id, pid => Pid}.

-doc """
Resolve a name in the named session, delegating to the shared resolver.

Reads the target session's locals (via `get_bindings`) and passes them to
`beamtalk_workspace:resolve_name/2`, so the order matches bare-name lookup:
locals → bind:as: → singletons → classes.  `aName` may be an atom, binary, or
string.  A name that resolves nowhere raises `undefined_variable` (the resolver's
terminal), matching how the name would behave as a bare identifier.
""".
-spec resolveFor(session() | term(), atom() | binary() | string() | term()) -> term().
resolveFor(Session, Name) ->
    {Id, Pid} = session_id_pid(Session, 'resolveFor:'),
    Locals = session_bindings(Pid, Id),
    NameAtom = to_name_atom(Name),
    beamtalk_workspace:resolve_name(Locals, NameAtom).

-doc """
Clear the named session's local bindings.

For the calling session, enqueues a `clear` mutation on its `pending_mutations`
queue (Phase 2) — the clear is applied on the success exit path and dropped on
error.  Against any other session, raises `cross_session_mutation_unsupported`:
the target shell's in-flight worker would clobber a direct edit, and the calling
eval's pending queue does not reach the target.
""".
-spec clearFor(session() | term()) -> nil.
clearFor(Session) ->
    {Id, Pid} = session_id_pid(Session, 'clearFor:'),
    enqueue_session_mutation(Pid, Id, {clear, undefined, undefined}, 'clearFor:'),
    nil.

-doc """
Return the originating client surface of a `Session` as a String
(`"repl"`/`"mcp"`/`"lsp"`/`"liveview"`/…, or `"unknown"`).

Reads the embedded metadata directly with **no** liveness check: kind is
immutable display data, so a dead session can still report where it came from
(useful precisely when debugging a session that has gone away).
""".
-spec kindOf(session() | term()) -> binary().
kindOf(Session) ->
    {_Id, Meta} = session_meta(Session, 'kind'),
    maps:get(kind, Meta, <<"unknown">>).

-doc """
Return a `Session`'s full origin/debug metadata as a Dictionary: its `id` plus
every metadata key (`kind`, and where known `peer`, `node`, `user`,
`connected_at`). Read-only and not liveness-checked — a primary tool for
answering "what is this session and where did it come from?".
""".
-spec infoFor(session() | term()) -> map().
infoFor(Session) ->
    {Id, Meta} = session_meta(Session, 'info'),
    Meta#{id => Id}.

-doc """
Render a `Session` for display as `a Session(<kind>: <id>)`, e.g.
`a Session(repl: session_123_ab)`.

Backs `Session>>printString`. Reads the embedded id/kind directly with no
liveness check so printing a list of sessions (`Workspace sessions`) never
raises just because one session died mid-enumeration.
""".
-spec printStringFor(session() | term()) -> binary().
printStringFor(Session) ->
    {Id, Meta} = session_meta(Session, 'printString'),
    Kind = maps:get(kind, Meta, <<"unknown">>),
    iolist_to_binary([<<"Session(">>, Kind, <<": ">>, Id, <<")">>]).

%%% ============================================================================
%%% Workspace-globals view (not a Session method)
%%% ============================================================================

-doc """
Return a `BindingsView` tagged for workspace scope.

Reads resolve live against the singleton registry + `bind:as:` ETS (via
`beamtalk_workspace_interface_primitives:get_session_bindings/0`); writes route
through `bind/2` / `unbind/1` (synchronous, protected-name conflict checks).
""".
-spec globalsView() -> bindings_view().
globalsView() ->
    #{'$beamtalk_class' => 'BindingsView', scope => workspace}.

%%% ============================================================================
%%% BindingsView read/write primitives
%%% ============================================================================

-doc """
Read a key from a `BindingsView`, returning its value or `nil`.

Reads reflect committed state: within a single eval they do not observe
mutations enqueued earlier in that same eval (those land on `pending_mutations`
and apply at eval exit).
""".
-spec view_at(bindings_view() | term(), atom() | binary() | string() | term()) -> term().
view_at(View, Key) ->
    Map = view_read_map(View, 'at:'),
    case to_existing_name_atom(Key) of
        {ok, KeyAtom} -> maps:get(KeyAtom, Map, nil);
        %% A never-interned name cannot be a binding key → absent.
        error -> nil
    end.

-doc """
Test whether a `BindingsView` contains a key, returning a boolean.

The membership test is O(1) (`maps:is_key/2`) on the resolved bindings map;
constructing that map is the same cost as any other read here (session scope
reads the locals map directly, workspace scope snapshots the bind:as: ETS via
`view_read_map/2`, which is O(n)).  Accepts atom/binary/String keys with the
same semantics as `view_at/2`: a never-interned name (via
`to_existing_name_atom/1`) cannot be a binding key, so it returns `false`
without minting an atom.
""".
-spec view_includes_key(bindings_view() | term(), atom() | binary() | string() | term()) ->
    boolean().
view_includes_key(View, Key) ->
    Map = view_read_map(View, 'includesKey:'),
    case to_existing_name_atom(Key) of
        {ok, KeyAtom} -> maps:is_key(KeyAtom, Map);
        %% A never-interned name cannot be a binding key → absent.
        error -> false
    end.

-doc """
Write a key/value into a `BindingsView`, returning the value put (Dictionary
protocol).

Session scope, calling session → enqueue a `put` mutation (deferred; the write
is not read back within the same eval).  Session scope, another session → raise
`cross_session_mutation_unsupported`.  Workspace scope → `bind/2` (synchronous,
protected-name conflict checks).
""".
-spec view_at_put(bindings_view() | term(), atom() | binary() | string() | term(), term()) ->
    term().
view_at_put(#{'$beamtalk_class' := 'BindingsView', scope := workspace}, Key, Value) ->
    KeyAtom = to_name_atom(Key),
    _ = beamtalk_workspace_interface_primitives:bind(Value, KeyAtom),
    Value;
view_at_put(
    #{'$beamtalk_class' := 'BindingsView', scope := session, id := Id, pid := Pid}, Key, Value
) ->
    KeyAtom = to_name_atom(Key),
    enqueue_session_mutation(Pid, Id, {put, KeyAtom, Value}, 'at:put:'),
    Value;
view_at_put(Other, _Key, _Value) ->
    raise_view_type_error('at:put:', Other).

-doc """
Remove a key from a `BindingsView`, returning `nil` (uniform contract — session
removals are enqueued, so the removed value is not read back synchronously).

Dispatch by scope mirrors `view_at_put/3`.
""".
-spec view_remove(bindings_view() | term(), atom() | binary() | string() | term()) -> nil.
view_remove(#{'$beamtalk_class' := 'BindingsView', scope := workspace}, Key) ->
    case to_existing_name_atom(Key) of
        %% unbind/1 raises name_not_found for unknown names; a never-interned
        %% atom is necessarily unknown, so treat it as already-absent (idempotent
        %% remove) rather than minting an atom just to fail.
        {ok, KeyAtom} -> _ = beamtalk_workspace_interface_primitives:unbind(KeyAtom);
        error -> nil
    end,
    nil;
view_remove(#{'$beamtalk_class' := 'BindingsView', scope := session, id := Id, pid := Pid}, Key) ->
    case to_existing_name_atom(Key) of
        {ok, KeyAtom} ->
            enqueue_session_mutation(Pid, Id, {remove, KeyAtom, undefined}, 'removeKey:');
        %% Never-interned name cannot be a session local → nothing to remove.
        error ->
            nil
    end,
    nil;
view_remove(Other, _Key) ->
    raise_view_type_error('removeKey:', Other).

-doc "Return the keys of a `BindingsView` as a list of atoms.".
-spec view_keys(bindings_view() | term()) -> [atom()].
view_keys(View) ->
    maps:keys(view_read_map(View, 'keys')).

-doc "Return the values of a `BindingsView` as a list.".
-spec view_values(bindings_view() | term()) -> [term()].
view_values(View) ->
    maps:values(view_read_map(View, 'values')).

-doc "Return the number of entries in a `BindingsView`.".
-spec view_size(bindings_view() | term()) -> non_neg_integer().
view_size(View) ->
    maps:size(view_read_map(View, 'size')).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Fetch a live shell's origin metadata for Session-value minting, bounded so a
%% wedged shell cannot stall `withId/1`. Falls back to an empty map (→ `kind =>
%% unknown` via make_session) if the shell exited or did not answer in time.
-spec fetch_meta(pid()) -> map().
fetch_meta(Pid) ->
    try beamtalk_repl_shell:get_session_meta(Pid, ?SESSION_ID_LOOKUP_TIMEOUT) of
        {ok, _Id, Meta} when is_map(Meta) -> Meta;
        %% Non-`{ok, _, _}` reply (e.g. `{error, unknown_request}` from an older
        %% shell during hot-code overlap) — degrade to an empty map rather than
        %% crash withId/1 with a try_clause.
        _ -> #{}
    catch
        exit:_ -> #{}
    end.

%% Extract {Id, Meta} from a Session value for display primitives, WITHOUT a
%% liveness check (display data must survive the session's death). Selector
%% names the Session method for the type-error message.
-spec session_meta(session() | term(), atom()) -> {binary(), map()}.
session_meta(#{'$beamtalk_class' := 'Session', id := Id, meta := Meta}, _Selector) when
    is_binary(Id), is_map(Meta)
->
    {Id, Meta};
session_meta(#{'$beamtalk_class' := 'Session', id := Id}, _Selector) when is_binary(Id) ->
    %% Older Session value minted before metadata existed — id but no meta.
    {Id, #{kind => <<"unknown">>}};
session_meta(Other, Selector) ->
    Err0 = beamtalk_error:new(type_error, 'Session', Selector),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Session value, got ">>, type_name(Other)])
        )
    ).

-spec make_session(binary(), pid(), map()) -> session().
make_session(Id, Pid, Meta) when is_map(Meta) ->
    %% Always carry a `kind` so display primitives never have to special-case a
    %% missing key (older shells / races default to `unknown`).
    NormMeta = maps:merge(#{kind => <<"unknown">>}, Meta),
    #{'$beamtalk_class' => 'Session', id => Id, pid => Pid, meta => NormMeta}.

%% Mint a Session value for one live shell child of beamtalk_session_sup.
%% `{false}` (filtermap drop) for non-pid children and for shells that have died
%% or stopped answering between which_children/1 and the id query, so a dead PID
%% is never minted into a Session value (mirrors withId/1's liveness discipline).
-spec mint_live_session(tuple()) -> {true, session()} | false.
mint_live_session({_ChildId, Pid, _Type, _Modules}) when is_pid(Pid) ->
    try beamtalk_repl_shell:get_session_meta(Pid, ?SESSION_ID_LOOKUP_TIMEOUT) of
        {ok, Id, Meta} when is_binary(Id), is_map(Meta) ->
            {true, make_session(Id, Pid, Meta)};
        %% Non-`{ok, _, _}` reply (e.g. `{error, unknown_request}` from an older
        %% shell during hot-code overlap) — skip rather than crash the enumeration
        %% with a try_clause.
        _ ->
            false
    catch
        %% Shell exited (noproc), or did not reply within the bounded timeout
        %% (wedged in another call) — skip rather than mint a dead PID or stall
        %% the user-facing enumeration.
        exit:_ -> false
    end;
mint_live_session(_Other) ->
    false.

%% Extract {Id, Pid} from a Session value, raising a type error if the shape is
%% wrong.  Selector names the Session method for the error message.
-spec session_id_pid(session() | term(), atom()) -> {binary(), pid()}.
session_id_pid(#{'$beamtalk_class' := 'Session', id := Id, pid := Pid}, _Selector) when
    is_binary(Id), is_pid(Pid)
->
    {Id, Pid};
session_id_pid(Other, Selector) ->
    Err0 = beamtalk_error:new(type_error, 'Session', Selector),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Session value, got ">>, type_name(Other)])
        )
    ).

%% Read the locals map of a session by PID, liveness-checked.  Raises
%% session_not_found if the shell has died.  For the calling session this is the
%% committed snapshot; for a cross-session read it is the target's get_bindings.
-spec session_bindings(pid(), binary()) -> map().
session_bindings(Pid, Id) ->
    ensure_alive(Pid, Id),
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(Pid),
    Bindings.

%% Resolve the read map for a BindingsView by scope.  Workspace reads come from
%% the live singleton + bind:as: snapshot; session reads from the target shell.
-spec view_read_map(bindings_view() | term(), atom()) -> map().
view_read_map(#{'$beamtalk_class' := 'BindingsView', scope := workspace}, _Selector) ->
    beamtalk_workspace_interface_primitives:get_session_bindings();
view_read_map(
    #{'$beamtalk_class' := 'BindingsView', scope := session, id := Id, pid := Pid}, _Selector
) ->
    session_bindings(Pid, Id);
view_read_map(Other, Selector) ->
    raise_view_type_error(Selector, Other).

%% Enqueue a session-local mutation against the *calling* session's queue.
%% Rejects cross-session writes (the target's worker would clobber them) and
%% liveness-checks the calling shell before the gen_server:call.
-spec enqueue_session_mutation(pid(), binary(), beamtalk_repl_state:mutation(), atom()) -> ok.
enqueue_session_mutation(TargetPid, TargetId, Mutation, Selector) ->
    CallingPid = get(beamtalk_session_pid),
    case TargetPid =:= CallingPid of
        true ->
            ensure_alive(TargetPid, TargetId),
            ok = beamtalk_repl_shell:enqueue_mutation(TargetPid, Mutation);
        false ->
            raise_cross_session(Selector)
    end.

%% Liveness guard: raise session_not_found rather than blocking a gen_server:call
%% to timeout against a dead shell PID.
-spec ensure_alive(pid(), binary()) -> ok.
ensure_alive(Pid, Id) ->
    case is_process_alive(Pid) of
        true ->
            ok;
        false ->
            Err0 = beamtalk_error:new(session_not_found, 'Session'),
            beamtalk_error:raise(
                beamtalk_error:with_message(
                    Err0,
                    iolist_to_binary([
                        <<"Session ">>, Id, <<" is no longer alive">>
                    ])
                )
            )
    end.

-spec raise_cross_session(atom()) -> no_return().
raise_cross_session(Selector) ->
    Err0 = beamtalk_error:new(cross_session_mutation_unsupported, 'Session', Selector),
    Err1 = beamtalk_error:with_message(
        Err0,
        <<"Cannot mutate another session's bindings; cross-session access is read-only">>
    ),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Err1,
            <<"Use Session current for writes, or read another session's bindings via withId:.">>
        )
    ).

-spec raise_view_type_error(atom(), term()) -> no_return().
raise_view_type_error(Selector, Other) ->
    Err0 = beamtalk_error:new(type_error, 'BindingsView', Selector),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a BindingsView value, got ">>, type_name(Other)])
        )
    ).

-spec raise_id_type_error(atom(), term()) -> no_return().
raise_id_type_error(Selector, Other) ->
    Err0 = beamtalk_error:new(type_error, 'Session', Selector),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a String session id, got ">>, type_name(Other)])
        )
    ).

%% Accept binary or string (list of codepoints) ids; return {ok, Binary} | error.
-spec to_binary_id(term()) -> {ok, binary()} | error.
to_binary_id(Id) when is_binary(Id) -> {ok, Id};
to_binary_id(Id) when is_list(Id) ->
    try
        {ok, unicode:characters_to_binary(Id)}
    catch
        _:_ -> error
    end;
to_binary_id(_) ->
    error.

%% Convert a name (atom | binary | string) to an atom for map keys / resolution.
-spec to_name_atom(term()) -> atom().
to_name_atom(Name) when is_atom(Name) -> Name;
to_name_atom(Name) when is_binary(Name) -> binary_to_atom(Name, utf8);
to_name_atom(Name) when is_list(Name) ->
    %% A list with invalid codepoints makes `unicode:characters_to_binary/1`
    %% return an error tuple (not a binary); feeding that to `binary_to_atom`
    %% throws a raw badarg. Funnel any such failure into the consistent
    %% type_error path instead of leaking a badarg.
    try unicode:characters_to_binary(Name) of
        Bin when is_binary(Bin) -> binary_to_atom(Bin, utf8);
        _ -> raise_name_type_error(Name)
    catch
        _:_ -> raise_name_type_error(Name)
    end;
to_name_atom(Other) ->
    raise_name_type_error(Other).

-spec raise_name_type_error(term()) -> no_return().
raise_name_type_error(Other) ->
    Err0 = beamtalk_error:new(type_error, 'BindingsView'),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Symbol name, got ">>, type_name(Other)])
        )
    ).

%% Convert a name to an EXISTING atom for read/remove lookups, returning
%% `{ok, Atom}` or `error` when the atom has never been created.
%%
%% Reads (`view_at`) and removes never need to mint a fresh atom: a name that
%% was never interned cannot be a binding key, so `error` maps to "absent".
%% This keeps introspection (`bindings at:`, `globals at:`) from being an
%% atom-table exhaustion vector when fed arbitrary user strings in a loop —
%% only genuine *writes* (`view_at_put`, which name a binding) create atoms,
%% matching the existing eval/`bind:as:` trust model.
-spec to_existing_name_atom(term()) -> {ok, atom()} | error.
to_existing_name_atom(Name) when is_atom(Name) ->
    {ok, Name};
to_existing_name_atom(Name) when is_binary(Name) ->
    try
        {ok, binary_to_existing_atom(Name, utf8)}
    catch
        error:badarg -> error
    end;
to_existing_name_atom(Name) when is_list(Name) ->
    try
        {ok, binary_to_existing_atom(unicode:characters_to_binary(Name), utf8)}
    catch
        _:_ -> error
    end;
to_existing_name_atom(Other) ->
    Err0 = beamtalk_error:new(type_error, 'BindingsView'),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Symbol name, got ">>, type_name(Other)])
        )
    ).

-spec type_name(term()) -> binary().
type_name(V) when is_binary(V) -> <<"a String">>;
type_name(V) when is_atom(V) -> <<"a Symbol">>;
type_name(V) when is_integer(V) -> <<"an Integer">>;
type_name(V) when is_map(V) -> <<"a Dictionary">>;
type_name(V) when is_list(V) -> <<"a List">>;
type_name(_) -> <<"a value of an unexpected type">>.
