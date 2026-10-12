%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ets_heir).
-behaviour(gen_server).

%%% **DDD Context:** Object System Context

-moduledoc """
Dedicated `{heir, ...}` process for every runtime ETS table (BT-3787).

An ETS table dies with its owner unless it names an heir. The runtime's tables
are created by whichever process first needs them — the application master, a
class gen_server, `beamtalk_trace_store`, `beamtalk_announcements`, or (in
tests) an EUnit process — so each one names this process as its heir and
survives its owner's death.

Why not `beamtalk_runtime_sup`: an OTP `supervisor`'s `handle_info/2` is fixed
in `supervisor.erl` and logs every unknown message, including
`{'ETS-TRANSFER', ...}`, at error level, and a supervisor can never hand a
table back. This process accepts the transfer quietly (debug level) and hands
a table back to a restarting owner through `reclaim/1`.

Started as the first child of `beamtalk_runtime_sup`, before any child that
creates a table. It holds no state of its own and does no work beyond owning
tables, so it has nothing to crash on: if it ever did die, the tables it
currently owns would go with it (and the tables naming it as heir would lose
their heir until the next `maybe_set_heir/1`).

The shared helpers — `heir_option/0`, `maybe_set_heir/1` and
`ensure_table/2` — are the one place the runtime decides a table's heir.
""".

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    heir_pid/0,
    heir_option/0,
    maybe_set_heir/1,
    adopt_owned_tables/0,
    reclaim/1,
    ensure_table/2
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Start the heir process (called by beamtalk_runtime_sup).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "The heir process's pid, or `undefined` when it is not running.".
-spec heir_pid() -> pid() | undefined.
heir_pid() ->
    whereis(?MODULE).

-doc """
Return the ETS `{heir, ...}` option for a new table: `[{heir, Pid, undefined}]`
when the heir process is running, `[]` otherwise.

Before the heir exists (early boot from `beamtalk_runtime_app:start/2`, or a
test without the runtime app) the table is created without an heir and owned
by the caller; `maybe_set_heir/1` adds the heir once the process is up.
""".
-spec heir_option() -> [{heir, pid(), undefined}].
heir_option() ->
    case heir_pid() of
        undefined -> [];
        Pid -> [{heir, Pid, undefined}]
    end.

-doc """
Retroactively set the heir on an existing table if it is not set.

Only the table owner can call `ets:setopts/2`, so this is a no-op unless the
caller owns `Table`; the owner sets it on its own next `ensure_*` call. Also a
no-op when the heir process is not running, or when the caller *is* the heir
(a process cannot be heir of its own table).
""".
-spec maybe_set_heir(ets:table()) -> ok.
maybe_set_heir(Table) ->
    case heir_pid() of
        undefined ->
            ok;
        HeirPid ->
            case ets:info(Table, heir) of
                HeirPid ->
                    ok;
                _ ->
                    Self = self(),
                    case ets:info(Table, owner) of
                        Self when Self =/= HeirPid ->
                            try
                                ets:setopts(Table, {heir, HeirPid, undefined})
                            catch
                                %% Heir died between whereis and setopts (TOCTOU).
                                error:badarg -> ok
                            end,
                            ok;
                        _ ->
                            ok
                    end
            end
    end.

-doc """
Make the heir process the heir of every ETS table the caller owns.

Called once by `beamtalk_runtime_app:start/2` after `beamtalk_runtime_sup` is
up: every table the app master created before the heir existed (class registry,
metadata, extensions, protocol registry, supervisor tables) gets the heir in
one pass, without a hand-kept list of table names to fall out of date.
""".
-spec adopt_owned_tables() -> ok.
adopt_owned_tables() ->
    Self = self(),
    lists:foreach(
        fun(Table) ->
            case ets:info(Table, owner) of
                Self -> maybe_set_heir(Table);
                _ -> ok
            end
        end,
        ets:all()
    ).

-doc """
Ask the heir to hand `Table` back to the calling process with
`ets:give_away/3`.

For an owner that restarts and expects its tables back (`beamtalk_trace_store`,
`beamtalk_announcements`). Returns `ok` once the caller owns the table — it
then receives `{'ETS-TRANSFER', Table, HeirPid, reclaimed}`. Returns
`{error, not_owner}` when the heir does not own `Table` (it was never handed
over, or another process owns it), and `{error, no_heir}` when the heir is not
running. The heir option is a table property, so it survives the hand-back.
""".
-spec reclaim(atom()) -> ok | {error, not_owner | no_heir}.
reclaim(Table) ->
    case heir_pid() of
        undefined ->
            {error, no_heir};
        HeirPid when HeirPid =:= self() ->
            {error, not_owner};
        _HeirPid ->
            try
                gen_server:call(?MODULE, {reclaim, Table})
            catch
                exit:{noproc, _} -> {error, no_heir};
                exit:{normal, _} -> {error, no_heir};
                exit:{shutdown, _} -> {error, no_heir}
            end
    end.

-doc """
Create a named table owned by the caller, or take it back from the heir.

For a restartable owner: when `Name` does not exist it is created with `Opts`
plus `heir_option/0`; when it does (it survived the previous owner's death
under the heir) the caller reclaims it, so the table follows the live owner
and the heir can inherit it again on the next crash. The heir is
(re)set either way. A table owned by some other process is left where it is —
the tables are `public`, so reads and writes still work.
""".
-spec ensure_table(atom(), [term()]) -> ok.
ensure_table(Name, Opts) ->
    case ets:whereis(Name) of
        undefined ->
            _ = ets:new(Name, Opts ++ heir_option()),
            ok;
        _Tid ->
            _ = reclaim(Name),
            maybe_set_heir(Name)
    end.

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #{}}.

handle_call({reclaim, Table}, {CallerPid, _Tag}, State) ->
    Reply =
        case ets:info(Table, owner) =:= self() of
            true ->
                try ets:give_away(Table, CallerPid, reclaimed) of
                    true ->
                        ?LOG_DEBUG("ETS heir handed table back", #{
                            table => Table, to => CallerPid
                        }),
                        ok
                catch
                    %% Caller died while the call was in flight.
                    error:badarg -> {error, not_owner}
                end;
            false ->
                {error, not_owner}
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', Table, FromPid, _HeirData}, State) ->
    ?LOG_DEBUG("ETS heir inherited table", #{table => Table, from => FromPid}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
