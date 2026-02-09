%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Exception handling for Beamtalk.
%%%
%%% Provides exception wrapping, block exception handling (on:do:, ensure:),
%%% and Exception object field access. Wraps `#beamtalk_error{}` records as
%%% Exception value type objects (tagged maps).
%%%
%%% **DDD Context:** Runtime — Error Handling
%%%
%%% Exception objects are value types (tagged maps), not actors:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'Exception',
%%%   error => #beamtalk_error{kind, class, selector, message, hint, details}
%%% }
%%% ```
%%%
%%% ## Block Exception Handling
%%%
%%% ```beamtalk
%%% [1 / 0] on: Exception do: [:e | e message]
%%% [resource open] ensure: [resource close]
%%% ```

-module(beamtalk_exception_handler).
-include("beamtalk.hrl").

-export([
    on_do/3,
    ensure/2,
    wrap/1,
    dispatch/3,
    has_method/1,
    signal/1,
    signal_message/1
]).

%% @doc Execute a block with exception handling.
%%
%% Wraps block evaluation in try/catch. If a `#beamtalk_error{}` is raised,
%% wraps it as an Exception object and passes to the handler block.
%%
%% Usage: `[expr] on: Exception do: [:e | e message]`
%%
%% The ExClass parameter is reserved for future exception class matching.
%% Currently catches all `#beamtalk_error{}` exceptions.
-spec on_do(function(), atom() | map(), function()) -> term().
on_do(Block, _ExClass, Handler) when is_function(Block, 0), is_function(Handler, 1) ->
    try Block() of
        Result -> Result
    catch
        error:#beamtalk_error{} = Error ->
            ExObj = wrap(Error),
            Handler(ExObj);
        error:Reason ->
            %% Wrap non-beamtalk errors (e.g., badarith, badmatch)
            GenError = #beamtalk_error{
                kind = runtime_error,
                class = 'Exception',
                selector = undefined,
                message = iolist_to_binary(io_lib:format("~p", [Reason])),
                hint = undefined,
                details = #{reason => Reason}
            },
            ExObj = wrap(GenError),
            Handler(ExObj)
    end.

%% @doc Execute a block with guaranteed cleanup.
%%
%% Wraps block evaluation in try/after. The cleanup block always runs,
%% regardless of whether an exception occurred.
%%
%% Note: If the cleanup block raises, its error replaces the original error.
%% This matches standard Erlang try/after semantics.
%%
%% Usage: `[resource open] ensure: [resource close]`
-spec ensure(function(), function()) -> term().
ensure(Block, CleanupBlock) when is_function(Block, 0), is_function(CleanupBlock, 0) ->
    try Block()
    after CleanupBlock()
    end.

%% @doc Wrap a `#beamtalk_error{}` record as an Exception tagged map.
-spec wrap(#beamtalk_error{} | term()) -> map().
wrap(#beamtalk_error{} = Error) ->
    #{'$beamtalk_class' => 'Exception', error => Error};
wrap(Other) ->
    GenError = #beamtalk_error{
        kind = runtime_error,
        class = 'Exception',
        selector = undefined,
        message = iolist_to_binary(io_lib:format("~p", [Other])),
        hint = undefined,
        details = #{reason => Other}
    },
    #{'$beamtalk_class' => 'Exception', error => GenError}.

%% @doc Dispatch a message to an Exception object.
%%
%% Exception objects expose the underlying `#beamtalk_error{}` fields.
-spec dispatch(atom(), list(), map()) -> term().
dispatch('message', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    Error#beamtalk_error.message;
dispatch('hint', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    case Error#beamtalk_error.hint of
        undefined -> nil;
        Hint -> Hint
    end;
dispatch('kind', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    Error#beamtalk_error.kind;
dispatch('selector', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    case Error#beamtalk_error.selector of
        undefined -> nil;
        Selector -> Selector
    end;
dispatch('errorClass', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    Error#beamtalk_error.class;
dispatch('printString', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    beamtalk_error:format(Error);
dispatch('describe', [], Self) ->
    dispatch('printString', [], Self);
dispatch('class', [], #{'$beamtalk_class' := Class}) ->
    Class;
dispatch('signal', [], #{'$beamtalk_class' := 'Exception', error := Error}) ->
    error(Error);
dispatch('signal:', [Message], _Self) ->
    signal_message(Message);
dispatch(Selector, _Args, _Self) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Exception'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    error(Error1).

%% @doc Check if Exception responds to a selector.
-spec has_method(atom()) -> boolean().
has_method('message') -> true;
has_method('hint') -> true;
has_method('kind') -> true;
has_method('selector') -> true;
has_method('errorClass') -> true;
has_method('printString') -> true;
has_method('describe') -> true;
has_method('class') -> true;
has_method('signal') -> true;
has_method('signal:') -> true;
has_method(_) -> false.

%% @doc Raise a new exception with a message string.
%%
%% Usage: `Exception signal: 'something went wrong'`
-spec signal(atom() | binary()) -> no_return().
signal(Kind) when is_atom(Kind) ->
    Error = beamtalk_error:new(Kind, 'Exception'),
    error(Error).

-spec signal_message(term()) -> no_return().
signal_message(Message) when is_binary(Message) ->
    Error = #beamtalk_error{
        kind = signal,
        class = 'Exception',
        selector = undefined,
        message = Message,
        hint = undefined,
        details = #{}
    },
    error(Error);
signal_message(Message) when is_atom(Message) ->
    signal_message(atom_to_binary(Message, utf8));
signal_message(Message) ->
    %% Convert other types to binary for robustness
    signal_message(iolist_to_binary(io_lib:format("~p", [Message]))).
