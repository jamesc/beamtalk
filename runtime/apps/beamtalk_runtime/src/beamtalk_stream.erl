%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime helper operations for Stream (closure-based lazy sequences).
%%
%% BT-511: Stream is Beamtalk's universal interface for sequential data.
%% Lazy operations wrap the generator in a new closure; terminal operations
%% pull elements through the chain.
%%
%% Streams are represented as tagged maps:
%%   #{'$beamtalk_class' => 'Stream', generator => fun() -> {element, NextFun} | done,
%%     description => binary()}
%%
%% where generator is a zero-arity function that returns either
%% `{element, NextFun}` (an element and the continuation) or `done`.
%%
%% **DDD Context:** Runtime Context — Domain Service
-module(beamtalk_stream).

-export([
    %% Constructors
    from/1,
    from_by/2,
    on/1,
    %% Stream construction (used by other modules, e.g. beamtalk_file)
    make_stream/2,
    make_stream/3,
    %% Lazy operations
    'select'/2,
    'collect'/2,
    'reject'/2,
    'drop'/2,
    %% Terminal operations
    take/2,
    do/2,
    inject_into/3,
    detect/2,
    as_list/1,
    any_satisfy/2,
    all_satisfy/2,
    %% Display
    print_string/1
]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @doc Create a Stream tagged map from a generator and description.
-spec make_stream(fun(() -> {term(), fun()} | done), binary()) -> map().
make_stream(Generator, Description) ->
    #{'$beamtalk_class' => 'Stream',
      generator => Generator,
      description => Description}.

%% @doc Create a Stream tagged map with an optional finalizer callback.
%% The finalizer is called when a terminal operation stops early (e.g. take:, detect:).
-spec make_stream(fun(() -> {term(), fun()} | done), binary(), fun(() -> term())) -> map().
make_stream(Generator, Description, Finalizer) when is_function(Finalizer, 0) ->
    #{'$beamtalk_class' => 'Stream',
      generator => Generator,
      description => Description,
      finalizer => Finalizer}.

%%% ============================================================================
%%% Constructors
%%% ============================================================================

%% @doc Create an infinite Stream starting from Start, incrementing by 1.
%% `Stream from: 1` => 1, 2, 3, ...
-spec from(integer()) -> map().
from(Start) when is_integer(Start) ->
    make_stream(make_successor_gen(Start), iolist_to_binary([<<"Stream(from: ">>, integer_to_binary(Start), <<")">>]));
from(_) ->
    raise_type_error('from:', <<"Expected an Integer argument">>).

%% @doc Create an infinite Stream starting from Start, applying StepFun to get next.
%% `Stream from: 1 by: [:n | n * 2]` => 1, 2, 4, 8, ...
-spec from_by(term(), fun((term()) -> term())) -> map().
from_by(Start, StepFun) when is_function(StepFun, 1) ->
    make_stream(make_step_gen(Start, StepFun), iolist_to_binary([<<"Stream(from: ">>, format_value(Start), <<" by: [...])">>]));
from_by(_, _) ->
    raise_type_error('from:by:', <<"Expected a Block as step function">>).

%% @doc Create a Stream from a collection (list, set, dictionary, or string).
%% `Stream on: #(1, 2, 3)` => 1, 2, 3
%% BT-514: Extended to support Set, Dictionary, and String.
-spec on(term()) -> map().
on(List) when is_list(List) ->
    make_stream(make_list_gen(List), <<"Stream(on: [...])">>);
on(Str) when is_binary(Str) ->
    Graphemes = beamtalk_string_ops:as_list(Str),
    make_stream(make_list_gen(Graphemes), <<"Stream(on: '...')">>);
on(Map) when is_map(Map) ->
    case beamtalk_tagged_map:class_of(Map) of
        'Set' ->
            Elements = beamtalk_set_ops:as_list(Map),
            make_stream(make_list_gen(Elements), <<"Stream(on: Set(...))">>);
        undefined ->
            %% Dictionary (plain map without $beamtalk_class tag)
            Assocs = maps:fold(
                fun(K, V, Acc) ->
                    [#{'$beamtalk_class' => 'Association', key => K, value => V} | Acc]
                end,
                [],
                Map
            ),
            make_stream(make_list_gen(lists:reverse(Assocs)), <<"Stream(on: #{...})">>);
        Other ->
            raise_type_error('on:', iolist_to_binary([
                <<"Expected a collection, got ">>,
                atom_to_binary(Other, utf8)]))
    end;
on(_) ->
    raise_type_error('on:', <<"Expected a collection (List, String, Set, or Dictionary)">>).

%%% ============================================================================
%%% Lazy Operations (return new Stream)
%%% ============================================================================

%% @doc Filter elements matching predicate.
-spec 'select'(map(), fun((term()) -> boolean())) -> map().
'select'(#{'$beamtalk_class' := 'Stream', generator := Gen, description := Desc} = Stream, Pred) when is_function(Pred, 1) ->
    NewGen = make_filter_gen(Gen, Pred),
    NewDesc = iolist_to_binary([Desc, <<" | select: [...]">>]),
    propagate_finalizer(Stream, make_stream(NewGen, NewDesc));
'select'(_, _) ->
    raise_type_error('select:', <<"Expected a Block with 1 argument">>).

%% @doc Transform each element.
-spec 'collect'(map(), fun((term()) -> term())) -> map().
'collect'(#{'$beamtalk_class' := 'Stream', generator := Gen, description := Desc} = Stream, MapFun) when is_function(MapFun, 1) ->
    NewGen = make_map_gen(Gen, MapFun),
    NewDesc = iolist_to_binary([Desc, <<" | collect: [...]">>]),
    propagate_finalizer(Stream, make_stream(NewGen, NewDesc));
'collect'(_, _) ->
    raise_type_error('collect:', <<"Expected a Block with 1 argument">>).

%% @doc Inverse filter — exclude elements matching predicate.
-spec 'reject'(map(), fun((term()) -> boolean())) -> map().
'reject'(#{'$beamtalk_class' := 'Stream', generator := Gen, description := Desc} = Stream, Pred) when is_function(Pred, 1) ->
    NewGen = make_filter_gen(Gen, fun(X) -> not Pred(X) end),
    NewDesc = iolist_to_binary([Desc, <<" | reject: [...]">>]),
    propagate_finalizer(Stream, make_stream(NewGen, NewDesc));
'reject'(_, _) ->
    raise_type_error('reject:', <<"Expected a Block with 1 argument">>).

%% @doc Skip first N elements.
-spec 'drop'(map(), non_neg_integer()) -> map().
'drop'(#{'$beamtalk_class' := 'Stream', generator := Gen, description := Desc} = Stream, N) when is_integer(N), N >= 0 ->
    NewGen = make_drop_gen(Gen, N),
    NewDesc = iolist_to_binary([Desc, <<" | drop: ">>, integer_to_binary(N)]),
    propagate_finalizer(Stream, make_stream(NewGen, NewDesc));
'drop'(_, _) ->
    raise_type_error('drop:', <<"Expected a non-negative Integer">>).

%%% ============================================================================
%%% Terminal Operations (force evaluation, return result)
%%% ============================================================================

%% @doc Return first N elements as a List.
-spec take(map(), non_neg_integer()) -> list().
take(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream, N) when is_integer(N), N >= 0 ->
    try take_loop(Gen, N, [])
    after call_finalizer(Stream)
    end;
take(_, _) ->
    raise_type_error('take:', <<"Expected a non-negative Integer">>).

%% @doc Iterate with side effects, return nil.
-spec do(map(), fun((term()) -> term())) -> nil.
do(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream, Block) when is_function(Block, 1) ->
    try do_loop(Gen, Block)
    after call_finalizer(Stream)
    end,
    nil;
do(_, _) ->
    raise_type_error('do:', <<"Expected a Block with 1 argument">>).

%% @doc Fold/reduce: inject initial value, accumulate with block.
-spec inject_into(map(), term(), fun((term(), term()) -> term())) -> term().
inject_into(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream, Initial, Block) when is_function(Block, 2) ->
    try inject_loop(Gen, Initial, Block)
    after call_finalizer(Stream)
    end;
inject_into(_, _, _) ->
    raise_type_error('inject:into:', <<"Expected a Block with 2 arguments">>).

%% @doc Return first element matching predicate, or nil if none.
-spec detect(map(), fun((term()) -> boolean())) -> term().
detect(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream, Pred) when is_function(Pred, 1) ->
    try detect_loop(Gen, Pred)
    after call_finalizer(Stream)
    end;
detect(_, _) ->
    raise_type_error('detect:', <<"Expected a Block with 1 argument">>).

%% @doc Materialize entire stream to a List.
-spec as_list(map()) -> list().
as_list(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream) ->
    try as_list_loop(Gen, [])
    after call_finalizer(Stream)
    end.

%% @doc Return true if any element satisfies predicate.
-spec any_satisfy(map(), fun((term()) -> boolean())) -> boolean().
any_satisfy(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream, Pred) when is_function(Pred, 1) ->
    try any_satisfy_loop(Gen, Pred)
    after call_finalizer(Stream)
    end;
any_satisfy(_, _) ->
    raise_type_error('anySatisfy:', <<"Expected a Block with 1 argument">>).

%% @doc Return true if all elements satisfy predicate.
-spec all_satisfy(map(), fun((term()) -> boolean())) -> boolean().
all_satisfy(#{'$beamtalk_class' := 'Stream', generator := Gen} = Stream, Pred) when is_function(Pred, 1) ->
    try all_satisfy_loop(Gen, Pred)
    after call_finalizer(Stream)
    end;
all_satisfy(_, _) ->
    raise_type_error('allSatisfy:', <<"Expected a Block with 1 argument">>).

%%% ============================================================================
%%% Display
%%% ============================================================================

%% @doc Return pipeline description string.
-spec print_string(map()) -> binary().
print_string(#{'$beamtalk_class' := 'Stream', description := Desc}) ->
    Desc.

%%% ============================================================================
%%% Generator Constructors (internal)
%%% ============================================================================

%% Successor generator: Start, Start+1, Start+2, ...
make_successor_gen(Current) ->
    fun() -> {Current, make_successor_gen(Current + 1)} end.

%% Step generator: Start, StepFun(Start), StepFun(StepFun(Start)), ...
make_step_gen(Current, StepFun) ->
    fun() -> {Current, make_step_gen(StepFun(Current), StepFun)} end.

%% List generator: elements of a list, then done.
make_list_gen([]) ->
    fun() -> done end;
make_list_gen([H | T]) ->
    fun() -> {H, make_list_gen(T)} end.

%% Filter generator: skip elements not matching predicate.
make_filter_gen(Gen, Pred) ->
    fun() -> filter_next(Gen, Pred) end.

filter_next(Gen, Pred) ->
    case Gen() of
        done -> done;
        {Elem, NextGen} ->
            case Pred(Elem) of
                true -> {Elem, make_filter_gen(NextGen, Pred)};
                false -> filter_next(NextGen, Pred)
            end
    end.

%% Map generator: transform each element.
make_map_gen(Gen, MapFun) ->
    fun() ->
        case Gen() of
            done -> done;
            {Elem, NextGen} -> {MapFun(Elem), make_map_gen(NextGen, MapFun)}
        end
    end.

%% Drop generator: skip first N elements.
make_drop_gen(Gen, 0) ->
    Gen;
make_drop_gen(Gen, N) ->
    fun() ->
        case Gen() of
            done -> done;
            {_Elem, NextGen} -> (make_drop_gen(NextGen, N - 1))()
        end
    end.

%%% ============================================================================
%%% Terminal Loops (internal)
%%% ============================================================================

take_loop(_Gen, 0, Acc) ->
    lists:reverse(Acc);
take_loop(Gen, N, Acc) ->
    case Gen() of
        done -> lists:reverse(Acc);
        {Elem, NextGen} -> take_loop(NextGen, N - 1, [Elem | Acc])
    end.

do_loop(Gen, Block) ->
    case Gen() of
        done -> ok;
        {Elem, NextGen} ->
            Block(Elem),
            do_loop(NextGen, Block)
    end.

inject_loop(Gen, Acc, Block) ->
    case Gen() of
        done -> Acc;
        {Elem, NextGen} -> inject_loop(NextGen, Block(Acc, Elem), Block)
    end.

detect_loop(Gen, Pred) ->
    case Gen() of
        done -> nil;
        {Elem, NextGen} ->
            case Pred(Elem) of
                true -> Elem;
                false -> detect_loop(NextGen, Pred)
            end
    end.

as_list_loop(Gen, Acc) ->
    case Gen() of
        done -> lists:reverse(Acc);
        {Elem, NextGen} -> as_list_loop(NextGen, [Elem | Acc])
    end.

any_satisfy_loop(Gen, Pred) ->
    case Gen() of
        done -> false;
        {Elem, NextGen} ->
            case Pred(Elem) of
                true -> true;
                false -> any_satisfy_loop(NextGen, Pred)
            end
    end.

all_satisfy_loop(Gen, Pred) ->
    case Gen() of
        done -> true;
        {Elem, NextGen} ->
            case Pred(Elem) of
                false -> false;
                true -> all_satisfy_loop(NextGen, Pred)
            end
    end.

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @doc Safely invoke a stream's finalizer if present.
%% Uses catch to handle double-close and missing finalizer gracefully.
-spec call_finalizer(map()) -> ok.
call_finalizer(#{finalizer := Finalizer}) when is_function(Finalizer, 0) ->
    catch Finalizer(),
    ok;
call_finalizer(_) ->
    ok.

%% @doc Copy finalizer from source stream to a new stream, if present.
-spec propagate_finalizer(map(), map()) -> map().
propagate_finalizer(#{finalizer := Finalizer}, NewStream) ->
    NewStream#{finalizer => Finalizer};
propagate_finalizer(_, NewStream) ->
    NewStream.

%% Format a value for description strings.
format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) when is_float(V) -> float_to_binary(V, [{decimals, 4}, compact]);
format_value(V) when is_binary(V) -> <<"'", V/binary, "'">>;
format_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_value(_) -> <<"...">>.

%% Raise a structured type error for invalid arguments.
-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'Stream'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, Hint)).
