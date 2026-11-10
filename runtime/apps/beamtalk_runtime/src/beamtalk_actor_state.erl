%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_actor_state).
-moduledoc """
Single owner of the actor-state cell and of every access to it (ADR 0137
§1, §3, §4; the actor-side twin of `beamtalk_class_vars`, ADR 0130).

## The cell

While one of an actor's `gen_server` callbacks runs user code, the actor's
state map is installed in the calling process's dictionary under one key (the
*cell*), tagged with the owning actor: `{Owner, Map}` where `Owner` is the
`#beamtalk_object{pid}` the map belongs to. The tag lets `at_home/1` tell
apart the host actor's own state from another actor's state that nested
`super_send/5` or `call_foreign/3` machinery stored in this process (ADR 0137
§2, §5). The cell exists only inside a callback: `install/1` plants it on
entry, `take/0` reads the map back as the callback's new state on a normal
exit, and `uninstall/1` restores whatever was there before (an enclosing
callback's cell, or nothing) on every exit. Between callbacks the
`gen_server` state is the only copy.

## At home and away

A block is *at home* when a callback of its own actor is running in the
current process: `Self#beamtalk_object.pid =:= self()` with the cell
installed *and owned by that same actor*. At home `get/2`, `put/3`, `has/2`
and `clear/2` read and write the cell. Away (another process, a `Timer`,
after the callback returned, or while the cell holds another actor's state
for a nested away `super_send/5`) the 3-arity `get/3` and `has/3` answer from
the creation-time `capture/2` map, and any write raises
`actor_state_unreachable`. Pid equality is the test (not the class name)
because a restarted actor is a new process: a block from the old process is
not at home in the new one.

## Callback contract

A callback returns the cell only on a normal exit; on an exception it returns
(or stops with) the state it was called with. `adapt/1` is the adapter for
code that still answers `{reply, R, NewState}` (native methods, ADR 0066
instance-side extension funs, `beamtalk_actor:dispatch/4`): it stores
`NewState` in the cell so the callback can `take/0` it. `protect/1` is the
rollback for runtime catchers that continue after an error: it restores the
cell to its value at entry before re-raising. A `^` that reaches its actor
method commits the cell, not the copy the throw carries (`commit/2`, ADR 0146).

## Dependencies

It depends on `beamtalk_error` and `beamtalk_tagged_map`, and on
`beamtalk_dispatch` and `beamtalk_actor` only for `super_send/5`, `refresh/2`
and the test-build check in `commit/2`, which call them.
""".

-include("beamtalk.hrl").

-export([
    install/1,
    take/0,
    uninstall/1,
    snapshot/0,
    restore/2,
    installed/0,
    cell_owner/0,
    store/1,
    adapt/1,
    get/2,
    get/3,
    view/2,
    nlr_state/2,
    commit/2,
    store_unwound/2,
    enable_commit_check/1,
    init_commit_check/0,
    refresh/2,
    refresh_carried/2,
    call_foreign/3,
    super_send/5,
    put/3,
    has/2,
    has/3,
    clear/2,
    capture/2,
    protect/1
]).

-export_type([cell/0, capture/0, adapted/0]).

-define(CELL, '$bt_actor_state').
%% BT-4448: the key `nlr_state/2` stamps on the state an away closure's `^`
%% carries, holding the pid of the actor the closure belongs to. A `__local__`
%% key, so the committed state never keeps it (`beamtalk_actor:strip_local_temps/1`);
%% the `$` keeps it apart from every threaded local (`__local__<name>`).
-define(AWAY_OWNER, '__local__$nlr_away_owner').
%% ADR 0146 (BT-4535): the persistent_term switch for the test-build check in
%% `commit/2`, and the process-dictionary marker `store_unwound/2` leaves when a
%% cleanup put the cell ahead of an in-flight `^`'s carried state: the list of
%% those throws' carried states, each spent by the commit of its own throw.
-define(CHECK_COMMIT, {?MODULE, check_commit}).
-define(CELL_AHEAD, '$bt_nlr_cell_ahead').
-define(CHECK_ENV, "BEAMTALK_CHECK_NLR_COMMIT").

-type cell() :: {pid() | tuple(), map()} | undefined.
-type capture() :: none | map().
-type adapted() :: {reply, term()} | noreply | {error, term()}.

%%====================================================================
%% Callback entry points
%%====================================================================

-doc """
Install `State` as the cell, owned by the calling process's actor, and answer
what was there before (`undefined`, or an enclosing callback's cell) for
`uninstall/1`.
""".
-spec install(map()) -> cell().
install(State) when is_map(State) ->
    Old = erlang:get(?CELL),
    forget_cell_ahead(Old),
    erlang:put(?CELL, {self(), State}),
    Old.

-doc """
The cell's current map: the callback's new state on a normal exit. Raises
`actor_state_unreachable` when no cell is installed.
""".
-spec take() -> map().
take() ->
    case erlang:get(?CELL) of
        {_, State} when is_map(State) -> State;
        _ -> raise_no_cell(take)
    end.

-doc "Restore the value `install/1` returned: erase the cell, or put the enclosing one back.".
-spec uninstall(cell()) -> ok.
uninstall(undefined) ->
    erlang:erase(?CELL),
    forget_cell_ahead(undefined),
    ok;
uninstall({_, _} = Old) ->
    erlang:put(?CELL, Old),
    ok.

-doc """
The cell as it is now (`undefined` outside a callback): the catch boundary of an
`on:do:` inside a block takes one on entry (ADR 0137 §5) and `restore/2`s it when
an error is caught, so the handler sees the state from before the protected region.
""".
-spec snapshot() -> cell().
snapshot() ->
    erlang:get(?CELL).

-doc """
Put the cell back to a `snapshot/0` (an error caught inside a block discards the
writes of its region) — but only at home. Away from home the cell holds another
actor's state (or nothing): an `on:do:` in a block running there must not roll
back writes that are not its own actor's (BT-4122), so it leaves the cell alone.
The handler still sees the right fields: away it reads the block's capture.
""".
-spec restore(#beamtalk_object{}, cell()) -> ok.
restore(Self, Snapshot) ->
    case at_home(Self) of
        {true, _} -> uninstall(Snapshot);
        false -> ok
    end.

-doc "True when a callback's cell is installed in this process.".
-spec installed() -> boolean().
installed() ->
    erlang:get(?CELL) =/= undefined.

-doc """
The actor that owns the installed cell (`undefined` when no cell is
installed). Fast paths that dispatch on the cell's presence (a self-send
while already inside a callback) compare this against the receiver so a cell
holding another actor's state — stored by a nested away `super_send/5` —
never serves as the receiver's own state.
""".
-spec cell_owner() -> pid() | tuple() | undefined.
cell_owner() ->
    case erlang:get(?CELL) of
        {Owner, State} when is_map(State) -> Owner;
        _ -> undefined
    end.

-doc """
Store `State` in the cell, keeping the current owner (the actor whose state
the cell holds); with no cell installed, the calling process's actor becomes
the owner. Generated bracket stores and `call_foreign/3` go through here so a
store made while another actor's state is installed (a nested away
`super_send/5`) keeps that actor's ownership instead of claiming the state
for the host process.
""".
-spec store(map()) -> ok.
store(State) when is_map(State) ->
    Owner =
        case erlang:get(?CELL) of
            {Current, CurrentState} when is_map(CurrentState) -> Current;
            _ -> self()
        end,
    erlang:put(?CELL, {Owner, State}),
    ok.

-doc """
Adapter for code that answers `{reply, R, NewState}`, `{noreply, NewState}` or
`{error, Reason, State}` (native methods, ADR 0066 extension funs,
`beamtalk_actor:dispatch/4`). Stores the state in the cell and answers the
result without it; the callback then returns `take/0`.
""".
-spec adapt({reply, term(), map()} | {noreply, map()} | {error, term(), map()}) -> adapted().
adapt({reply, Result, NewState}) when is_map(NewState) ->
    store(NewState),
    {reply, Result};
adapt({noreply, NewState}) when is_map(NewState) ->
    store(NewState),
    noreply;
adapt({error, Reason, State}) when is_map(State) ->
    store(State),
    {error, Reason}.

%%====================================================================
%% Access helpers (Self receivers)
%%====================================================================

-doc "Read field `Name` at home; raises `actor_state_unreachable` away.".
-spec get(#beamtalk_object{}, atom()) -> term().
get(Self, Name) ->
    maps:get(Name, home_cell(Self, Name, read), nil).

-doc "Read field `Name`: the cell at home, else the creation-time `Capture` (raises when `none`).".
-spec get(#beamtalk_object{}, atom(), capture()) -> term().
get(Self, Name, Capture) ->
    case at_home(Self) of
        {true, State} -> maps:get(Name, State, nil);
        false -> maps:get(Name, away_capture(Self, Name, Capture), nil)
    end.

-doc """
The state map a block's reads see: the cell at home, else the creation-time
`Capture` (raises `actor_state_unreachable` when `none`).
""".
-spec view(#beamtalk_object{}, capture()) -> map().
view(Self, Capture) ->
    case at_home(Self) of
        {true, State} -> State;
        false -> away_capture(Self, '_', Capture)
    end.

-doc """
The state an actor closure's `^` throw carries: the cell at home, as `view/2`.
Away it is the creation-time `Capture` stamped with the pid of the closure's
own actor (BT-4448). Actor state maps hold no pid, so without the stamp the
capture of one actor equals another same-class actor's cell whenever their
fields do; `refresh_carried/2` reads the stamp so that a `^` thrown from a
closure of another actor is never resumed with this actor's cell.
""".
-spec nlr_state(#beamtalk_object{}, capture()) -> map().
nlr_state(Self, Capture) ->
    case at_home(Self) of
        {true, State} -> State;
        false -> (away_capture(Self, '_', Capture))#{?AWAY_OWNER => owner_of(Self)}
    end.

-doc """
ADR 0146: the state an actor method commits when a `^` of its own reaches its
NLR boundary (`{'reply', Value, commit(Self, Carried)}`). The cell is the one
place the actor's fields live, so at home the commit is the cell as it stands:
a cleanup's writes made while the `^` unwound are there, and the throw's own
`__local__` keys are dead at the method boundary, so none is merged back (as
`refresh/2` would). Away (an away `super_send/5` runs the method in another
process, or no callback of this actor is running here) there is no cell of this
actor's, and the commit is `Carried` unchanged, so `super_send/5`'s
illegal-write check still sees what the method answered. Ownership is
`at_home/1`'s; nothing compares two state maps to decide it.

When the test-build check is on (`enable_commit_check/1`, or the
`BEAMTALK_CHECK_NLR_COMMIT` environment variable at runtime start) a commit at
home also asserts the invariant the cell commit relies on: the cell is current
whenever a `^` starts. A `^` thrown at home stores its state to the cell (or
carries the cell) as it throws, so on reaching its boundary its carried fields
equal the cell unless something put the cell ahead on the way: a cleanup of
another method's inlined `ensure:` (`store_unwound/2`). A `^` thrown away from
home carries its creation-time capture, stamped by `nlr_state/2`, and is not
checked. Any other difference raises `actor_nlr_stale_cell`.
""".
-spec commit(#beamtalk_object{}, term()) -> term().
commit(Self, Carried) ->
    case at_home(Self) of
        {true, Cell} ->
            case persistent_term:get(?CHECK_COMMIT, false) of
                false -> Cell;
                true -> check_commit(Self, Cell, Carried)
            end;
        false ->
            Carried
    end.

-doc """
ADR 0146 R1: store the writes an inlined `ensure:` cleanup made while `Throw`, a
`^` of another method of this actor, unwound through it, as `store/1` does. The
throw is re-raised as it came, so the cell is now ahead of the state it
carries; the test-build check in `commit/2` is told so for that throw only. It
notes the throw's carried state, which only the commit of that throw (the one
carrying that exact term) spends: a nested commit of another `^` neither
consumes nor is excused by it. A note never outlives the callback it was made
in (`install/1` and `uninstall/1` drop it at a top-level callback's entry and
exit), so a throw that left by another way (an error, a REPL or class-method
boundary, the `gen_server` exception path) cannot excuse a later commit.
""".
-spec store_unwound(term(), map()) -> ok.
store_unwound(Throw, State) when is_map(State) ->
    case persistent_term:get(?CHECK_COMMIT, false) of
        false -> ok;
        true -> note_cell_ahead(Throw)
    end,
    store(State).

-spec note_cell_ahead(term()) -> ok.
note_cell_ahead(Throw) when ?IS_NLR(Throw), tuple_size(Throw) =:= 4 ->
    Carried = element(4, Throw),
    Ahead =
        case erlang:get(?CELL_AHEAD) of
            List when is_list(List) -> List;
            _ -> []
        end,
    erlang:put(?CELL_AHEAD, [Carried | Ahead]),
    ok;
note_cell_ahead(_Throw) ->
    ok.

%% Spends the note `store_unwound/2` left for the throw carrying `Carried`.
-spec take_cell_ahead(term()) -> boolean().
take_cell_ahead(Carried) ->
    case erlang:get(?CELL_AHEAD) of
        List when is_list(List) ->
            case lists:member(Carried, List) of
                true ->
                    case lists:delete(Carried, List) of
                        [] -> erlang:erase(?CELL_AHEAD);
                        Rest -> erlang:put(?CELL_AHEAD, Rest)
                    end,
                    true;
                false ->
                    false
            end;
        _ ->
            false
    end.

%% A top-level callback (no enclosing cell) starts and ends with no note: no
%% throw outlives the callback it unwound in.
-spec forget_cell_ahead(cell()) -> ok.
forget_cell_ahead(undefined) ->
    _ = erlang:erase(?CELL_AHEAD),
    ok;
forget_cell_ahead(_Enclosing) ->
    ok.

-doc """
Switch the test-build check in `commit/2` on (when the `BEAMTALK_CHECK_NLR_COMMIT`
environment variable is set to anything but `0` or the empty string) at
runtime start (`beamtalk_runtime_app`). Leaves it as it is otherwise, so no
`persistent_term` write happens in a build that does not ask for the check.
""".
-spec init_commit_check() -> ok.
init_commit_check() ->
    case os:getenv(?CHECK_ENV) of
        false -> ok;
        "" -> ok;
        "0" -> ok;
        _ -> enable_commit_check(true)
    end.

-doc """
Switch the test-build check in `commit/2` on or off. A `persistent_term`
write, so never on a hot path: at runtime start (`init_commit_check/0`) or in
a test.
""".
-spec enable_commit_check(boolean()) -> ok.
enable_commit_check(true) ->
    persistent_term:put(?CHECK_COMMIT, true);
enable_commit_check(false) ->
    _ = persistent_term:erase(?CHECK_COMMIT),
    ok.

-spec check_commit(#beamtalk_object{}, map(), term()) -> map().
check_commit(Self, Cell, Carried) ->
    Ahead = take_cell_ahead(Carried),
    case
        Ahead orelse not is_map(Carried) orelse maps:is_key(?AWAY_OWNER, Carried) orelse
            beamtalk_actor:strip_local_temps(Carried) =:= beamtalk_actor:strip_local_temps(Cell)
    of
        true -> Cell;
        false -> raise_stale_cell(Self, Cell, Carried)
    end.

-spec raise_stale_cell(#beamtalk_object{}, map(), map()) -> no_return().
raise_stale_cell(#beamtalk_object{class = Class}, Cell, Carried) ->
    Fields = beamtalk_actor:strip_local_temps(Carried),
    Stale = lists:sort([
        K
     || K <- maps:keys(maps:merge(Fields, Cell)),
        not beamtalk_tagged_map:is_local_temp_key(K),
        maps:get(K, Fields, '$absent') =/= maps:get(K, Cell, '$absent')
    ]),
    Error0 = beamtalk_error:new(actor_nlr_stale_cell, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format(
                "~s's state cell was stale when a `^` reached its method: the throw carried "
                "fields the cell did not hold (~p)",
                [Class, Stale]
            )
        )
    ),
    Error2 = beamtalk_error:with_details(Error1, #{fields => Stale}),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error2,
            <<
                "Compiler invariant (ADR 0146): the cell is current whenever a `^` starts. "
                "A ThreadedIr lowering let a `^` start without storing the method's state."
            >>
        )
    ).

-doc """
Run a `dispatch/4` exit that is not a compiled Beamtalk method (ADR 0137 §2): a
native method, an ADR 0066 extension fun, the inherited `Object`/`ProtoObject`
walk, `doesNotUnderstand:`. The code behind it is Erlang that does not bracket
what it calls, and it may run one of this actor's blocks (a stored block handed to
`perform:withArguments:`) or send back to `self`, which reads and writes the cell.
So `State` is stored to the cell first and `Fun` is applied to `Args`. Always on,
in every build.

What the answer carries is checked against the cell, never reconciled by guesswork:

  * the cell is unchanged since entry: the answer passes through;
  * the fun answers the exact state it was given and the cell changed: nothing in
    the callee advanced its own state, so the cell holds the only writes (a block it
    ran wrote them) and the answer carries the cell's state instead;
  * the fun answers a different state and the cell changed to something else: both
    changed and neither is the other's ancestor, so it raises `actor_state_conflict`
    rather than lose either set of writes (an answer equal to the cell is not a
    conflict: a compiled callee that stored and then threaded the same version).

Invariant: a compiled callee that stored to the cell stores its final `State`
on exit. Codegen guarantees it (`with_exit_store`, keyed on `cell_put_call`,
the one emitter of every cell store), so "answer equals entry, cell changed"
can only mean a block wrote the field.
""".
-spec call_foreign(map(), fun(), [term()]) -> term().
call_foreign(State, Fun, Args) when is_map(State), is_function(Fun, length(Args)) ->
    store(State),
    reflect_cell(State, apply(Fun, Args)).

-spec reflect_cell(map(), term()) -> term().
reflect_cell(Entry, {reply, Result, Answered}) when is_map(Answered) ->
    {reply, Result, reflect_state(Entry, Answered)};
reflect_cell(Entry, {noreply, Answered}) when is_map(Answered) ->
    {noreply, reflect_state(Entry, Answered)};
reflect_cell(Entry, {error, Reason, Answered}) when is_map(Answered) ->
    {error, Reason, reflect_state(Entry, Answered)};
reflect_cell(_Entry, Answer) ->
    Answer.

%% States are compared without the `nlr_state/2` stamp (BT-4448): a compiled
%% callee that caught its own away closure's `^` answers that closure's capture
%% stamped, which is the same version as an unstamped `Entry`.
-spec reflect_state(map(), map()) -> map().
reflect_state(Entry, Answered) ->
    case erlang:get(?CELL) of
        {_, Cell} when is_map(Cell) ->
            case {unstamped(Cell), unstamped(Answered), unstamped(Entry)} of
                {C, A, E} when C =/= E, C =/= A, A =:= E -> Cell;
                {C, A, E} when C =/= E, C =/= A -> raise_conflict(Entry);
                _ -> Answered
            end;
        _ ->
            Answered
    end.

-spec unstamped(map()) -> map().
unstamped(State) ->
    maps:remove(?AWAY_OWNER, State).

-spec raise_conflict(map()) -> no_return().
raise_conflict(Entry) ->
    Class = maps:get('$beamtalk_class', Entry, unknown),
    Error0 = beamtalk_error:new(actor_state_conflict, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format(
                "~s's state was changed twice by one call: a block it ran wrote a field "
                "and the method also answered a different state",
                [Class]
            )
        )
    ),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error1,
            <<
                "A native method or extension that runs a block must read the block's "
                "writes back from the cell before it answers."
            >>
        )
    ).

-doc """
`Acc` with its fields replaced by the cell, keeping the codegen-internal
`__local__` scratch keys it carries (`beamtalk_actor:strip_local_temps/1` names
them). The state an `ensure:` cleanup of a failed body starts from: the cell
holds the state at entry plus the writes the body made before the error. Away
from home (no cell) `Acc` unchanged.
""".
-spec refresh(#beamtalk_object{}, map()) -> map().
refresh(Self, Acc) ->
    case at_home(Self) of
        {true, Cell} -> cell_over(Cell, Acc);
        false -> Acc
    end.

-doc """
BT-4429: the state a `^` throw resumes with after `Block>>ensure:` reached
dynamically (`perform:`) ran its cleanup. That catch runs in the Block method,
so it cannot know what the throw carries: an actor method's or closure's state
(the cell, which the `^` stored or read just before it threw), the REPL's
bindings, or a value type's `Self`. `Snapshot` is the cell as `snapshot/0`
answered it before the cleanup ran. When the cell was this process's actor's
own and `Carried` is that cell's state (its `__local__` scratch keys aside),
answer `Carried` with its fields replaced by the cell as the cleanup left it,
as `refresh/2` does. Otherwise `Carried` holds no state a cleanup closure can
write (no closure assigns a REPL binding or a value type's field), so answer it
unchanged.

Equal fields alone do not make `Carried` the cell's (BT-4448): a closure of
another actor of the same class, run here away from home, carries its own
actor's capture, which can equal this cell. `nlr_state/2` stamps that capture
with its actor's pid, and a `Carried` stamped with any actor but the cell's
owner is answered unchanged, so it goes back to its own boundary as it came.
""".
-spec refresh_carried(cell(), term()) -> term().
refresh_carried({Owner, Before}, Carried) when Owner =:= self(), is_map(Carried) ->
    case
        maps:get(?AWAY_OWNER, Carried, Owner) =:= Owner andalso
            beamtalk_actor:strip_local_temps(Carried) =:=
                beamtalk_actor:strip_local_temps(Before)
    of
        true ->
            case erlang:get(?CELL) of
                {Owner, Cell} when is_map(Cell) -> cell_over(Cell, Carried);
                _ -> Carried
            end;
        false ->
            Carried
    end;
refresh_carried(_Snapshot, Carried) ->
    Carried.

-doc """
A `super` send from a block: run the superclass method of `Class` for `Selector`
against the cell at home (storing its new state back, answering its value);
away, against the creation-time `Capture`, where a method that changes state
raises `actor_state_unreachable` (a block abroad cannot write the actor).
""".
-spec super_send(#beamtalk_object{}, capture(), atom(), atom(), [term()]) -> term().
super_send(Self, Capture, Class, Selector, Args) ->
    case at_home(Self) of
        {true, State} ->
            %% `super_reply/5` re-raises an `{error, E}` answer, in either shape
            %% (BT-4208, BT-4592).
            {reply, Result, NewState} =
                beamtalk_dispatch:super_reply(Selector, Args, Self, State, Class),
            store(NewState),
            Result;
        false ->
            State = away_capture(Self, Selector, Capture),
            %% The superclass code stores to this process's cell (`call_foreign/3`,
            %% `SyncedCall`, exit store). Tag those stores with the block owner,
            %% so a block of the host actor running inside the call is away
            %% (its `at_home/1` sees another owner's state) and reads its
            %% creation-time capture instead of another actor's fields. The
            %% host's own cell is restored afterwards.
            Saved = snapshot(),
            erlang:put(?CELL, {owner_of(Self), State}),
            try beamtalk_dispatch:super(Selector, Args, Self, State, Class) of
                {reply, Result, State} -> Result;
                {reply, _Result, _Changed} -> raise_unreachable(Self, Selector, write);
                {error, Error} -> beamtalk_exception_handler:reraise(Error)
            after
                uninstall(Saved)
            end
    end.

-doc "Write field `Name` at home; raises `actor_state_unreachable` away.".
-spec put(#beamtalk_object{}, atom(), term()) -> ok.
put(Self, Name, Value) ->
    State = home_cell(Self, Name, write),
    store(State#{Name => Value}).

-doc "Whether field `Name` is present, at home; raises `actor_state_unreachable` away.".
-spec has(#beamtalk_object{}, atom()) -> boolean().
has(Self, Name) ->
    maps:is_key(Name, home_cell(Self, Name, read)).

-doc "`has/2` with the creation-time `Capture` fallback away from home.".
-spec has(#beamtalk_object{}, atom(), capture()) -> boolean().
has(Self, Name, Capture) ->
    case at_home(Self) of
        {true, State} -> maps:is_key(Name, State);
        false -> maps:is_key(Name, away_capture(Self, Name, Capture))
    end.

-doc "Remove field `Name` at home; raises `actor_state_unreachable` away.".
-spec clear(#beamtalk_object{}, atom()) -> ok.
clear(Self, Name) ->
    State = home_cell(Self, Name, write),
    store(maps:remove(Name, State)).

-doc """
The map a block literal binds at creation: the cell at home, else `Outer` (the
enclosing block's own capture, `none` when there is none).
""".
-spec capture(#beamtalk_object{}, capture()) -> capture().
capture(Self, Outer) ->
    case at_home(Self) of
        {true, State} -> State;
        false -> Outer
    end.

%%====================================================================
%% Rollback
%%====================================================================

-doc """
Run `Fun`; on any exception other than a `^` non-local-return throw
(`?IS_NLR`) restore the cell to its value at entry and re-raise.
""".
-spec protect(fun(() -> T)) -> T when T :: term().
protect(Fun) ->
    Snap = erlang:get(?CELL),
    %% bt-catcher-audit: not-applicable-reraises - this is the actor-state protect/1 itself: it restores the cell then re-raises every clause
    try
        Fun()
    catch
        throw:Nlr:Stack when ?IS_NLR(Nlr) ->
            erlang:raise(throw, Nlr, Stack);
        Class:Reason:Stack ->
            uninstall(Snap),
            erlang:raise(Class, Reason, Stack)
    end.

%%====================================================================
%% Internal
%%====================================================================

-spec at_home(#beamtalk_object{}) -> {true, map()} | false.
at_home(#beamtalk_object{pid = Pid}) when Pid =:= self() ->
    case erlang:get(?CELL) of
        {Owner, State} when is_map(State), Owner =:= Pid -> {true, State};
        _ -> false
    end;
at_home(#beamtalk_object{}) ->
    false.

-spec cell_over(map(), map()) -> map().
cell_over(Cell, Acc) ->
    Fields = beamtalk_actor:strip_local_temps(Acc),
    maps:merge(Cell, maps:without(maps:keys(Fields), Acc)).

-spec owner_of(#beamtalk_object{}) -> pid() | tuple().
owner_of(#beamtalk_object{pid = Pid}) -> Pid.

-spec home_cell(#beamtalk_object{}, atom(), read | write) -> map().
home_cell(Self, Name, Mode) ->
    case at_home(Self) of
        {true, State} -> State;
        false -> raise_unreachable(Self, Name, Mode)
    end.

-spec away_capture(#beamtalk_object{}, atom(), capture()) -> map().
away_capture(_Self, _Name, Capture) when is_map(Capture) -> Capture;
away_capture(Self, Name, none) -> raise_unreachable(Self, Name, read).

-spec raise_unreachable(#beamtalk_object{}, atom(), read | write) -> no_return().
raise_unreachable(#beamtalk_object{class = Class}, Name, Mode) ->
    Verb =
        case Mode of
            read -> "read";
            write -> "written"
        end,
    Message = iolist_to_binary(
        io_lib:format("~s's state field ~s cannot be ~s from this process", [Class, Name, Verb])
    ),
    Hint = iolist_to_binary(
        io_lib:format(
            "A block that accesses ~s's state ran outside ~s's own process (it was sent "
            "to another actor, a Timer or a Future, or run after the method returned). "
            "A block can read ~s's fields anywhere, as the values they had when the block "
            "was made, but can only write them inside ~s's own methods: send ~s a message "
            "instead.",
            [Class, Class, Class, Class, Class]
        )
    ),
    Error0 = beamtalk_error:new(actor_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{field => Name}),
    beamtalk_error:raise(beamtalk_error:with_hint(Error2, Hint)).

-spec raise_no_cell(atom()) -> no_return().
raise_no_cell(Operation) ->
    Error0 = beamtalk_error:new(actor_state_unreachable, unknown, Operation),
    Error1 = beamtalk_error:with_message(
        Error0, <<"no actor callback is running in this process, so there is no actor state">>
    ),
    beamtalk_error:raise(Error1).
