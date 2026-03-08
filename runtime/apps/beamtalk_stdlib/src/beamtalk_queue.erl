%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Queue class implementation — O(1) amortised FIFO queue via Erlang's :queue module.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Queue is a value-type Erlang-backed class wrapping Erlang's built-in `:queue`
%%% module. Each mutation (enqueue, dequeue) returns a new Queue map rather than
%%% modifying the receiver in place. The `:queue` module provides O(1) amortised
%%% enqueue and dequeue using a pair of lists internally.
%%%
%%% Queue objects are represented as tagged maps:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'Queue',
%%%   queue => ErlangQueue  %% opaque :queue data structure
%%% }
%%% ```
%%%
%%% ## Class Methods
%%%
%%% | Selector  | Description                |
%%% |-----------|----------------------------|
%%% | `new`     | Create an empty queue      |
%%%
%%% ## Instance Methods
%%%
%%% | Selector       | Description                                        |
%%% |----------------|----------------------------------------------------|
%%% | `enqueue:`     | Add element to back; returns new Queue             |
%%% | `dequeue`      | Remove front element; returns `{Value, NewQueue}`  |
%%% | `peek`         | Return front element without removing              |
%%% | `isEmpty`      | True if the queue has no elements                  |
%%% | `size`         | Number of elements                                 |
%%%
%%% ## References
%%%
%%% - BT-1250: stdlib: mutable collections — Queue and AtomicCounter
%%% - Erlang `:queue` module documentation

-module(beamtalk_queue).

%% Class methods
-export(['new'/0]).

%% Instance methods — no-colon forms used by the FFI proxy
-export([enqueue/2, dequeue/1, peek/1, isEmpty/1, size/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

%% @doc Create a new empty Queue.
-spec 'new'() -> map().
'new'() ->
    make_queue(queue:new()).

%%% ============================================================================
%%% Instance Methods
%%%
%%% All instance methods receive the Queue tagged map as the first argument.
%%% The FFI proxy calls these by stripping the first keyword from the selector:
%%%   `enqueue: self elem: elem` → selector `enqueue:elem:` → `enqueue(Self, Elem)`
%%% ============================================================================

%% @doc Add an element to the back of the queue. Returns a new Queue.
%%
%% O(1) amortised.
-spec enqueue(map(), term()) -> map().
enqueue(#{'$beamtalk_class' := 'Queue', queue := Q}, Elem) ->
    make_queue(queue:in(Elem, Q));
enqueue(_Self, _Elem) ->
    type_error('enqueue:').

%% @doc Remove and return the front element as `{Value, NewQueue}`.
%%
%% Returns a 2-tuple `{Value, NewQueue}` where `NewQueue` is a Queue map.
%% Raises `empty_queue` if the queue is empty.
-spec dequeue(map()) -> {term(), map()}.
dequeue(#{'$beamtalk_class' := 'Queue', queue := Q}) ->
    case queue:out(Q) of
        {empty, _} ->
            empty_queue_error('dequeue');
        {{value, Value}, NewQ} ->
            {Value, make_queue(NewQ)}
    end;
dequeue(_Self) ->
    type_error('dequeue').

%% @doc Return the front element without removing it.
%%
%% Raises `empty_queue` if the queue is empty.
-spec peek(map()) -> term().
peek(#{'$beamtalk_class' := 'Queue', queue := Q}) ->
    case queue:peek(Q) of
        empty -> empty_queue_error('peek');
        {value, Value} -> Value
    end;
peek(_Self) ->
    type_error('peek').

%% @doc Return true if the queue contains no elements.
-spec isEmpty(map()) -> boolean().
isEmpty(#{'$beamtalk_class' := 'Queue', queue := Q}) ->
    queue:is_empty(Q);
isEmpty(_Self) ->
    type_error('isEmpty').

%% @doc Return the number of elements in the queue.
-spec size(map()) -> non_neg_integer().
size(#{'$beamtalk_class' := 'Queue', queue := Q}) ->
    queue:len(Q);
size(_Self) ->
    type_error('size').

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private
%% @doc Build a Queue tagged map wrapping the given Erlang queue data structure.
-spec make_queue(term()) -> map().
make_queue(Q) ->
    #{'$beamtalk_class' => 'Queue', queue => Q}.

%% @private
%% @doc Raise a type_error for the given selector.
-spec type_error(atom()) -> no_return().
type_error(Selector) ->
    Error0 = beamtalk_error:new(type_error, 'Queue'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a Queue instance">>),
    beamtalk_error:raise(Error2).

%% @private
%% @doc Raise an empty_queue error for the given selector.
-spec empty_queue_error(atom()) -> no_return().
empty_queue_error(Selector) ->
    Error0 = beamtalk_error:new(empty_queue, 'Queue'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Cannot dequeue or peek from an empty Queue">>),
    beamtalk_error:raise(Error2).
