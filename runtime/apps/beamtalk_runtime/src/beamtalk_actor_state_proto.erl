%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_actor_state_proto).
-moduledoc """
ADR 0137 Phase 0 PROTOTYPE (BT-4007): the closure-side helpers of the
"actor closures see the actor's live state" model, behind the
`BEAMTALK_ADR0137_PROTO` codegen flag (off by default).

This is NOT the Phase 1 `beamtalk_actor_state` module (BT-4008) and has a
different name on purpose. Phase 2 replaces every call to this module with the
real one; nothing outside the flagged codegen output should call it.

The cell is the `'$bt_actor_state'` process-dictionary entry every actor
callback already installs. Method bodies keep threading `State` in locals; the
flagged codegen stores the current version to the cell before a call that may
run user code and loads it back afterwards. A closure's field access goes
through these helpers: at home (a callback of its own actor is running in this
process) it reads and writes the cell; away it reads the value captured when
the block was created and a write raises `actor_state_unreachable`.
""".

-include("beamtalk.hrl").

-export([
    at_home/1,
    get_field/3,
    put_field/3,
    nlr_state/2,
    cell_get/0,
    cell_put/1
]).

-define(CELL, '$bt_actor_state').

-doc """
`true` when a callback of the actor `Self` names is running in this process:
`Self#beamtalk_object.pid =:= self()` with the cell present.
""".
-spec at_home(term()) -> boolean().
at_home(#beamtalk_object{pid = Pid}) ->
    Pid =:= self() andalso erlang:get(?CELL) =/= undefined;
at_home(_) ->
    false.

-doc "The cell (the actor's live state map), or `undefined` outside a callback.".
-spec cell_get() -> map() | undefined.
cell_get() ->
    erlang:get(?CELL).

-doc "Store a state map into the cell. Answers `ok`.".
-spec cell_put(map()) -> ok.
cell_put(State) ->
    erlang:put(?CELL, State),
    ok.

-doc """
Read field `Field` from a closure: the live cell at home, the `Captured` state
(the version current when the block literal was created) away.
""".
-spec get_field(term(), map(), atom()) -> term().
get_field(Self, Captured, Field) ->
    case at_home(Self) of
        true -> maps:get(Field, erlang:get(?CELL));
        false -> maps:get(Field, Captured)
    end.

-doc """
The `State` a `^` thrown from a closure hands back to the method that catches
it: the live cell at home (the closure's writes are in it), the closure's
captured `State` away.
""".
-spec nlr_state(term(), map()) -> map().
nlr_state(Self, Captured) ->
    case at_home(Self) of
        true -> erlang:get(?CELL);
        false -> Captured
    end.

-doc """
Write field `Field` from a closure and answer `Value`: into the cell at home;
away it raises `actor_state_unreachable`.
""".
-spec put_field(term(), atom(), term()) -> term().
put_field(Self, Field, Value) ->
    case at_home(Self) of
        true ->
            erlang:put(?CELL, maps:put(Field, Value, erlang:get(?CELL))),
            Value;
        false ->
            Class =
                case Self of
                    #beamtalk_object{class = C} -> C;
                    _ -> 'Actor'
                end,
            Error0 = beamtalk_error:new(actor_state_unreachable, Class),
            Error1 = beamtalk_error:with_selector(Error0, Field),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<
                    "A block that writes the actor's state ran outside the actor's own "
                    "process (it was sent to another actor, a Timer or a Future, or run "
                    "after the method returned). Send the actor a message instead."
                >>
            ),
            beamtalk_exception_handler:reraise(Error2)
    end.
