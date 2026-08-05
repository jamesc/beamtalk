%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_handle_registry).
-behaviour(gen_server).

%%% **DDD Context:** Object System Context

-moduledoc """
Handle-registry for `File open:mode:` (BT-3020).

`open:mode:` hands the caller a `FileHandle` it must close itself — see
`beamtalk_file:'open:mode:'/2`. Without this registry an unclosed handle
survives the caller's death and stays open for the lifetime of the node,
because the descriptor is owned by the long-lived `File` class process
rather than the caller. This server is the reclamation and diagnostics
substrate BT-3020 adds: it tracks every outstanding `open:mode:` handle
against an *owner*, and closes an owner's handles when the owner dies.

## Ownership tiers (BT-3020 decision (a))

`beamtalk_file:'open:mode:'/2` resolves an owner via a three-tier rule before
calling `register/2`:

1. The REPL/workspace session shell pid (survives across eval-worker turns).
2. Otherwise, the calling Beamtalk actor's pid.
3. Otherwise, `undefined` — an unowned handle, registered for diagnostics
   (`open_handles/0` / `File openHandles`) but reclaimed only by an explicit
   `close` or node shutdown.

## Lifecycle

Registering a handle against a pid owner arms a monitor on that owner the
first time it appears; further handles from the same owner share it. When
the owner dies, every handle still registered under it is closed (via
`beamtalk_file_handle:close_handle/1`, already idempotent) and dropped from
the registry. `unregister/1` (called from `beamtalk_file_handle:close/1` on
every close, whatever opened the handle) removes a handle immediately, so a
closed handle never lingers and a later owner death can never double-close it.

This is a supervised `one_for_one` worker (`beamtalk_runtime_sup`, alongside
`beamtalk_object_watch` — the same monitor-owner/react-to-'DOWN' shape); if it
crashes and restarts, all bookkeeping is lost and every previously-registered
handle becomes untracked (still open, no longer reclaimed on owner death, no
longer listed). Handles opened after the restart are tracked normally.
`terminate/2` logs a warning on an abnormal exit so this loss is at least
observable, rather than a silent regression back to BT-3020's original leak.

`register/2` and `unregister/1` never let a registry failure (crashed
mid-call, or too slow to reply) propagate out of `open:mode:` / `close` — a
handle that was actually opened or closed must never be lost or turned into a
raised error just because bookkeeping failed; see the `catch exit:_` in each.
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, register/2, unregister/1, open_handles/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-type owner() :: pid() | undefined.
%% The handle's own atomics state cell (`beamtalk_file_handle:new/3`) — a
%% unique identity per handle instance, already used by the handle itself to
%% track open/closed state.
-type handle_key() :: atomics:atomics_ref().

-record(state, {
    %% HandleKey => {Handle, Owner}
    handles = #{} :: #{handle_key() => {beamtalk_file_handle:t(), owner()}},
    %% Owner pid => set of HandleKeys it owns, for O(1) cleanup on 'DOWN'.
    by_owner = #{} :: #{pid() => #{handle_key() => true}},
    %% Monitor ref => owner pid. One monitor per distinct owner, shared
    %% across every handle it owns.
    monitors = #{} :: #{reference() => pid()}
}).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Start the handle registry with a registered name.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc """
Register an outstanding `FileHandle` against its resolved `Owner`.

`Owner` is a pid (session shell or calling actor) or `undefined` (unowned,
tier 3). Synchronous: the registration is committed before this returns, so a
handle is never briefly "open but unlisted" between `open:mode:` returning and
a subsequent `File openHandles` call. A no-op returning `ok` when the registry
is not running, malformed (missing the `state` cell `beamtalk_file_handle:new/3`
always sets — never true for a real handle, only a defensive guard), or the
call itself fails (registry crashed or was too slow to reply within the
default timeout) — a bookkeeping failure must never fail the open itself, nor
leak the descriptor the caller was just handed by silently discarding it.
""".
-spec register(beamtalk_file_handle:t(), owner()) -> ok.
register(Handle, Owner) when
    is_map(Handle), is_map_key(state, Handle), Owner =:= undefined orelse is_pid(Owner)
->
    case erlang:whereis(?SERVER) of
        undefined ->
            ok;
        Server ->
            try
                gen_server:call(Server, {register, Handle, Owner})
            catch
                exit:_ -> ok
            end
    end;
register(_Handle, _Owner) ->
    ok.

-doc """
Unregister a `FileHandle`, whatever opened it.

Called from `beamtalk_file_handle:close/1` on every close, so handles never
registered here (from `open:do:` / `open:mode:do:`) simply miss — a harmless
no-op — while a registered handle is dropped immediately, closing the window
where a closed handle could still appear in `open_handles/0` or be
double-closed by a later owner `'DOWN'`. Like `register/2`, never lets a
missing/crashed/slow registry turn a handle that really did close into a
raised error from `close`.
""".
-spec unregister(beamtalk_file_handle:t()) -> ok.
unregister(Handle) when is_map(Handle), is_map_key(state, Handle) ->
    case erlang:whereis(?SERVER) of
        undefined ->
            ok;
        Server ->
            try
                gen_server:call(Server, {unregister, Handle})
            catch
                exit:_ -> ok
            end
    end;
unregister(_Handle) ->
    ok.

-doc """
List every outstanding registered handle as `{Path, Mode, Owner}`.

`Owner` is the session/actor pid for tiers 1-2, `undefined` for an unowned
(tier 3) handle. Backs `File openHandles`. Returns `[]` when the registry is
not running or fails to reply — a diagnostic read must never raise.
""".
-spec open_handles() -> [{binary(), beamtalk_file_handle:mode(), owner()}].
open_handles() ->
    case erlang:whereis(?SERVER) of
        undefined ->
            [];
        Server ->
            try
                gen_server:call(Server, open_handles)
            catch
                exit:_ -> []
            end
    end.

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

init([]) ->
    {ok, #state{}}.

handle_call({register, Handle, Owner}, _From, State) ->
    {reply, ok, do_register(Handle, Owner, State)};
handle_call({unregister, Handle}, _From, State) ->
    {reply, ok, do_unregister(Handle, State)};
handle_call(open_handles, _From, State) ->
    {reply, do_open_handles(State), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, Owner, _Reason}, State) ->
    {noreply, handle_owner_down(MonRef, Owner, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) when Reason =:= normal; Reason =:= shutdown ->
    ok;
terminate({shutdown, _}, _State) ->
    ok;
terminate(Reason, #state{handles = Handles}) ->
    %% One_for_one/permanent: the supervisor restarts this process empty.
    %% Every handle tracked here becomes untracked — reachable via `close`,
    %% but no longer reclaimed on its owner's death or listed in
    %% `File openHandles`. Log it: a safety net that fails silently is worse
    %% than one that fails loudly.
    ?LOG_WARNING(
        "beamtalk_file_handle_registry terminating abnormally — losing "
        "ownership/reclamation tracking for outstanding FileHandles",
        #{reason => Reason, tracked_handles => map_size(Handles), domain => [beamtalk, stdlib]}
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal functions
%%% ============================================================================

-spec handle_key(beamtalk_file_handle:t()) -> handle_key().
handle_key(Handle) ->
    maps:get(state, Handle).

-spec do_register(beamtalk_file_handle:t(), owner(), #state{}) -> #state{}.
do_register(Handle, Owner, State0) ->
    %% Defensive: register/2 has a single production caller (`open:mode:`,
    %% once per freshly-opened handle) and duplicate registration of the same
    %% handle should not happen — but if it ever did with a *different* Owner,
    %% not clearing the old entry first would leave a stale key in the old
    %% owner's by_owner set, which a later unrelated death would incorrectly
    %% walk. Unregistering first keeps the invariant "each handle key is
    %% indexed under at most one owner" true unconditionally.
    State = do_unregister(Handle, State0),
    do_register_new(Handle, Owner, State).

-spec do_register_new(beamtalk_file_handle:t(), owner(), #state{}) -> #state{}.
do_register_new(Handle, undefined, State) ->
    Key = handle_key(Handle),
    State#state{handles = maps:put(Key, {Handle, undefined}, State#state.handles)};
do_register_new(Handle, Owner, State) when is_pid(Owner) ->
    #state{handles = Handles, by_owner = ByOwner, monitors = Monitors} = State,
    Key = handle_key(Handle),
    Handles1 = maps:put(Key, {Handle, Owner}, Handles),
    {ByOwner1, Monitors1} =
        case maps:find(Owner, ByOwner) of
            {ok, Keys} ->
                {ByOwner#{Owner => Keys#{Key => true}}, Monitors};
            error ->
                %% First handle for this owner: arm its monitor.
                Ref = erlang:monitor(process, Owner),
                {ByOwner#{Owner => #{Key => true}}, Monitors#{Ref => Owner}}
        end,
    State#state{handles = Handles1, by_owner = ByOwner1, monitors = Monitors1}.

-spec do_unregister(beamtalk_file_handle:t(), #state{}) -> #state{}.
do_unregister(Handle, State) ->
    Key = handle_key(Handle),
    case maps:take(Key, State#state.handles) of
        {{_Handle, undefined}, Handles1} ->
            State#state{handles = Handles1};
        {{_Handle, Owner}, Handles1} ->
            {ByOwner1, Monitors1} = drop_owner_key(
                Owner, Key, State#state.by_owner, State#state.monitors
            ),
            State#state{handles = Handles1, by_owner = ByOwner1, monitors = Monitors1};
        error ->
            %% Already unregistered (or never registered) — no-op.
            State
    end.

%% Remove Key from Owner's set, demonitoring and dropping the owner entry
%% entirely once its last handle is gone.
-spec drop_owner_key(pid(), handle_key(), map(), map()) -> {map(), map()}.
drop_owner_key(Owner, Key, ByOwner, Monitors) ->
    case maps:find(Owner, ByOwner) of
        {ok, Keys0} ->
            Keys1 = maps:remove(Key, Keys0),
            case map_size(Keys1) of
                0 -> {maps:remove(Owner, ByOwner), demonitor_owner(Owner, Monitors)};
                _ -> {ByOwner#{Owner => Keys1}, Monitors}
            end;
        error ->
            {ByOwner, Monitors}
    end.

-spec demonitor_owner(pid(), map()) -> map().
demonitor_owner(Owner, Monitors) ->
    maps:filter(
        fun
            (Ref, P) when P =:= Owner ->
                erlang:demonitor(Ref, [flush]),
                false;
            (_Ref, _P) ->
                true
        end,
        Monitors
    ).

-spec handle_owner_down(reference(), pid(), #state{}) -> #state{}.
handle_owner_down(MonRef, Owner, State) ->
    #state{handles = Handles, by_owner = ByOwner, monitors = Monitors} = State,
    Keys = maps:keys(maps:get(Owner, ByOwner, #{})),
    lists:foreach(fun(Key) -> close_owned_handle(Key, Handles) end, Keys),
    State#state{
        handles = maps:without(Keys, Handles),
        by_owner = maps:remove(Owner, ByOwner),
        monitors = maps:remove(MonRef, Monitors)
    }.

-spec close_owned_handle(handle_key(), map()) -> ok.
close_owned_handle(Key, Handles) ->
    case maps:find(Key, Handles) of
        {ok, {Handle, _Owner}} ->
            %% close_handle/1 is idempotent (atomics exchange) and does not
            %% raise on I/O failure — best-effort reclamation on owner death.
            catch beamtalk_file_handle:close_handle(Handle),
            ok;
        error ->
            ok
    end.

-spec do_open_handles(#state{}) -> [{binary(), beamtalk_file_handle:mode(), owner()}].
do_open_handles(#state{handles = Handles}) ->
    [
        {maps:get(path, Handle), maps:get(mode, Handle), Owner}
     || {Handle, Owner} <- maps:values(Handles)
    ].
