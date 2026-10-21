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

A leaf of the object system: it depends only on `beamtalk_error`.
""".

-include("beamtalk.hrl").

-export([
    install/1,
    take/0,
    uninstall/1,
    installed/0,
    adapt/1,
    get/2,
    get/3,
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
