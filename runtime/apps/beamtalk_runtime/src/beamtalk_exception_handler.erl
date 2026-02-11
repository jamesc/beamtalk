%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Exception handling runtime support for Beamtalk.
%%%
%%% Provides exception wrapping, class matching, and Exception object field
%%% access. Wraps `#beamtalk_error{}' records as Exception value type objects
%%% (tagged maps). Called by compiler-generated try/catch code.
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
%%% The compiler generates inline Core Erlang try/catch for `on:do:` and
%%% `ensure:` (structural intrinsics). This module provides the runtime
%%% helpers called by that generated code: `wrap/1` and `matches_class/2`.

-module(beamtalk_exception_handler).
-include("beamtalk.hrl").

-export([
    wrap/1,
    ensure_wrapped/1,
    matches_class/2,
    dispatch/3,
    has_method/1,
    signal/1,
    signal_message/1
]).

%% @doc Check if an error matches the requested exception class.
%%
%% Handles both raw `#beamtalk_error{}` records and wrapped Exception
%% tagged maps (ADR 0015). After signal-time wrapping, caught errors are
%% typically wrapped maps.
%%
%% - nil → match all (no filter)
%% - Exception class object → match all (root of hierarchy)
%% - Error class object → match all (subclass of Exception)
%% - Other class object → match by error kind atom
%% - Atom → match by error kind directly
-spec matches_class(term(), term()) -> boolean().
matches_class(nil, _Error) ->
    true;
matches_class(Filter, #{'$beamtalk_class' := 'Exception', error := Error}) ->
    matches_class(Filter, Error);
matches_class({beamtalk_object, ClassName, _, _}, Error) ->
    matches_class_name(ClassName, Error);
matches_class(ClassName, Error) when is_atom(ClassName) ->
    matches_class_name(ClassName, Error);
matches_class(_Other, _Error) ->
    %% Unknown filter type — catch all for safety
    true.

%% @private Match by class name atom.
-spec matches_class_name(atom(), #beamtalk_error{}) -> boolean().
matches_class_name('Exception class', _Error) -> true;
matches_class_name('Exception', _Error) -> true;
matches_class_name('Error class', _Error) -> true;
matches_class_name('Error', _Error) -> true;
matches_class_name(ClassName, #beamtalk_error{kind = Kind}) ->
    Kind =:= ClassName;
matches_class_name(_ClassName, _RawError) ->
    %% Raw Erlang errors don't have a kind field — can't match custom classes
    false.

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

%% @doc Idempotent exception wrapper (ADR 0015).
%%
%% Ensures the value is a wrapped Exception tagged map. If already wrapped,
%% passes through unchanged. Used in `on:do:` catch clauses to handle both
%% pre-wrapped exceptions (from raise/1) and raw Erlang exceptions.
-spec ensure_wrapped(term()) -> map().
ensure_wrapped(#{'$beamtalk_class' := _} = Already) ->
    Already;
ensure_wrapped(Other) ->
    wrap(Other).

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
    beamtalk_error:raise(Error);
dispatch('signal:', [Message], _Self) ->
    signal_message(Message);
dispatch(Selector, _Args, _Self) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Exception'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:raise(Error1).

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
    beamtalk_error:raise(Error).

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
    beamtalk_error:raise(Error);
signal_message(Message) when is_atom(Message) ->
    signal_message(atom_to_binary(Message, utf8));
signal_message(Message) ->
    %% Convert other types to binary for robustness
    signal_message(iolist_to_binary(io_lib:format("~p", [Message]))).
