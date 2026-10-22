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
*cell*). The cell exists only inside a callback: `install/1` plants it on
entry, `take/0` reads it back as the callback's new state on a normal exit,
and `uninstall/1` restores whatever was there before (an enclosing callback's
cell, or nothing) on every exit. Between callbacks the `gen_server` state is
the only copy.

## At home and away

A block is *at home* when a callback of its own actor is running in the
current process: `Self#beamtalk_object.pid =:= self()` with the cell
installed. At home `get/2`, `put/3`, `has/2` and `clear/2` read and write the
cell. Away (another process, a `Timer`, after the callback returned) the
3-arity `get/3` and `has/3` answer from the creation-time `capture/2` map, and
any write raises `actor_state_unreachable`. Pid equality is the test (not the
class name) because a restarted actor is a new process: a block from the old
process is not at home in the new one.

## Callback contract

A callback returns the cell only on a normal exit; on an exception it returns
(or stops with) the state it was called with. `adapt/1` is the adapter for
code that still answers `{reply, R, NewState}` (native methods, ADR 0066
instance-side extension funs, `beamtalk_actor:dispatch/4`): it stores
`NewState` in the cell so the callback can `take/0` it. `protect/1` is the
rollback for runtime catchers that continue after an error: it restores the
cell to its value at entry before re-raising.

## Dependencies

It depends on `beamtalk_error`, and on `beamtalk_dispatch` and `beamtalk_actor`
only for `super_send/5` and `refresh/2`, which call them.
""".

-include("beamtalk.hrl").

-export([
    install/1,
    take/0,
    uninstall/1,
    snapshot/0,
    restore/1,
    installed/0,
    adapt/1,
    get/2,
    get/3,
    view/2,
    refresh/2,
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

-type cell() :: map() | undefined.
-type capture() :: none | map().
-type adapted() :: {reply, term()} | noreply | {error, term()}.

%%====================================================================
%% Callback entry points
%%====================================================================

-doc """
Install `State` as the cell and answer what was there before (`undefined`, or
an enclosing callback's state) for `uninstall/1`.
""".
-spec install(map()) -> cell().
install(State) when is_map(State) ->
    Old = erlang:get(?CELL),
    erlang:put(?CELL, State),
    Old.

-doc """
The cell's current map: the callback's new state on a normal exit. Raises
`actor_state_unreachable` when no cell is installed.
""".
-spec take() -> map().
take() ->
    case erlang:get(?CELL) of
        State when is_map(State) -> State;
        _ -> raise_no_cell(take)
    end.

-doc "Restore the value `install/1` returned: erase the cell, or put the enclosing one back.".
-spec uninstall(cell()) -> ok.
uninstall(undefined) ->
    erlang:erase(?CELL),
    ok;
uninstall(Old) when is_map(Old) ->
    erlang:put(?CELL, Old),
    ok.

-doc """
The cell as it is now (`undefined` outside a callback): the catch boundary of an
`on:do:` inside a block takes one on entry (ADR 0137 §5) and `restore/1`s it when
an error is caught, so the handler sees the state from before the protected region.
""".
-spec snapshot() -> cell().
snapshot() ->
    erlang:get(?CELL).

-doc "Put the cell back to a `snapshot/0` (an error caught inside a block discards the writes of its region).".
-spec restore(cell()) -> ok.
restore(Snapshot) ->
    uninstall(Snapshot).

-doc "True when a callback's cell is installed in this process.".
-spec installed() -> boolean().
installed() ->
    erlang:get(?CELL) =/= undefined.

-doc """
Adapter for code that answers `{reply, R, NewState}`, `{noreply, NewState}` or
`{error, Reason, State}` (native methods, ADR 0066 extension funs,
`beamtalk_actor:dispatch/4`). Stores the state in the cell and answers the
result without it; the callback then returns `take/0`.
""".
-spec adapt({reply, term(), map()} | {noreply, map()} | {error, term(), map()}) -> adapted().
adapt({reply, Result, NewState}) when is_map(NewState) ->
    erlang:put(?CELL, NewState),
    {reply, Result};
adapt({noreply, NewState}) when is_map(NewState) ->
    erlang:put(?CELL, NewState),
    noreply;
adapt({error, Reason, State}) when is_map(State) ->
    erlang:put(?CELL, State),
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
    erlang:put(?CELL, State),
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

-spec reflect_state(map(), map()) -> map().
reflect_state(Entry, Answered) ->
    case erlang:get(?CELL) of
        Cell when is_map(Cell), Cell =/= Entry, Cell =/= Answered ->
            case Answered =:= Entry of
                true -> Cell;
                false -> raise_conflict(Entry)
            end;
        _ ->
            Answered
    end.

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
        {true, Cell} ->
            Fields = beamtalk_actor:strip_local_temps(Acc),
            maps:merge(Cell, maps:without(maps:keys(Fields), Acc));
        false ->
            Acc
    end.

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
            case beamtalk_dispatch:super(Selector, Args, Self, State, Class) of
                {reply, Result, NewState} ->
                    erlang:put(?CELL, NewState),
                    Result;
                {error, Error} ->
                    beamtalk_error:raise(Error)
            end;
        false ->
            State = away_capture(Self, Selector, Capture),
            %% The superclass code stores to this process's cell (`call_foreign/3`,
            %% `SyncedCall`, exit store); that must not leave another actor's State
            %% in the cell of the process the block is running in.
            Saved = snapshot(),
            try beamtalk_dispatch:super(Selector, Args, Self, State, Class) of
                {reply, Result, State} -> Result;
                {reply, _Result, _Changed} -> raise_unreachable(Self, Selector, write);
                {error, Error} -> beamtalk_error:raise(Error)
            after
                uninstall(Saved)
            end
    end.

-doc "Write field `Name` at home; raises `actor_state_unreachable` away.".
-spec put(#beamtalk_object{}, atom(), term()) -> ok.
put(Self, Name, Value) ->
    State = home_cell(Self, Name, write),
    erlang:put(?CELL, State#{Name => Value}),
    ok.

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
    erlang:put(?CELL, maps:remove(Name, State)),
    ok.

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
        State when is_map(State) -> {true, State};
        _ -> false
    end;
at_home(#beamtalk_object{}) ->
    false.

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
