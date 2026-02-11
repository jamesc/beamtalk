%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Exception handling runtime support for Beamtalk.
%%%
%%% Provides exception wrapping, class matching, and Exception object field
%%% access. Wraps `#beamtalk_error{}' records as exception hierarchy value
%%% type objects (tagged maps). Called by compiler-generated try/catch code.
%%%
%%% **DDD Context:** Runtime — Error Handling
%%%
%%% Exception objects are value types (tagged maps), not actors.
%%% The `$beamtalk_class' is set based on error kind (BT-452):
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'RuntimeError',  %% or TypeError, InstantiationError, Error
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
    signal_message/1,
    kind_to_class/1,
    is_exception_class/1
]).

%% @doc Map an error kind atom to the appropriate exception class name.
%%
%% Used by wrap/1 to set `$beamtalk_class` based on the error's kind field.
%% Falls back to 'Error' for unknown kinds (safe bootstrap default).
%%
%% NOTE: When adding new error subclasses (e.g., IOError), add a clause here.
%% is_exception_class/1 and matches_class_name/2 derive hierarchy from the
%% class system automatically (BT-475).
-spec kind_to_class(atom()) -> atom().
kind_to_class(does_not_understand) -> 'RuntimeError';
kind_to_class(arity_mismatch) -> 'RuntimeError';
kind_to_class(immutable_value) -> 'RuntimeError';
kind_to_class(runtime_error) -> 'RuntimeError';
kind_to_class(index_out_of_bounds) -> 'RuntimeError';
kind_to_class(class_not_found) -> 'RuntimeError';
kind_to_class(no_superclass) -> 'RuntimeError';
kind_to_class(class_already_exists) -> 'RuntimeError';
kind_to_class(dispatch_error) -> 'RuntimeError';
kind_to_class(callback_failed) -> 'RuntimeError';
kind_to_class(actor_dead) -> 'RuntimeError';
kind_to_class(future_not_awaited) -> 'RuntimeError';
kind_to_class(internal_error) -> 'RuntimeError';
kind_to_class(type_error) -> 'TypeError';
kind_to_class(instantiation_error) -> 'InstantiationError';
%% signal (from signal_message/1) stays Error — user decides semantics.
%% file_*/io_error/invalid_path/permission_denied stay Error — future IOError (ADR 0015 Phase 6).
kind_to_class(signal) -> 'Error';
kind_to_class(_) -> 'Error'.

%% @doc Check if a class name belongs to the exception hierarchy.
%%
%% Delegates to the class system's superclass chain (BT-475).
%% Returns true if ClassName is 'Exception' or any subclass of 'Exception'.
%% Returns false if class is not registered (safe during bootstrap).
-spec is_exception_class(atom()) -> boolean().
is_exception_class(ClassName) ->
    beamtalk_object_class:inherits_from(ClassName, 'Exception').

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
matches_class(Filter, #{'$beamtalk_class' := _, error := Error}) ->
    matches_class(Filter, Error);
matches_class({beamtalk_object, ClassName, _, _}, Error) ->
    matches_class_name(ClassName, Error);
matches_class(ClassName, Error) when is_atom(ClassName) ->
    matches_class_name(ClassName, Error);
matches_class(_Other, _Error) ->
    %% Unknown filter type — catch all for safety
    true.

%% @private Match by class name atom with hierarchy-aware matching (BT-475).
%%
%% Derives the error's class from kind_to_class/1, then uses the class
%% system's superclass chain to check if it matches the requested filter.
%% Handles both "ClassName" and "ClassName class" variants (metaclass refs).
%% Raw Erlang errors (not #beamtalk_error{}) are wrapped first.
-spec matches_class_name(atom(), term()) -> boolean().
matches_class_name(ClassName, #beamtalk_error{kind = Kind}) ->
    %% Strip " class" suffix if present (metaclass reference from class objects)
    BaseName = strip_class_suffix(ClassName),
    ErrorClass = kind_to_class(Kind),
    beamtalk_object_class:inherits_from(ErrorClass, BaseName);
matches_class_name(ClassName, RawError) when not is_map(RawError) ->
    %% Raw Erlang error (e.g. badarith) — wrap to get a kind, then match
    #{'$beamtalk_class' := _, error := Inner} = wrap(RawError),
    matches_class_name(ClassName, Inner);
matches_class_name(_ClassName, _Other) ->
    false.

%% @private Strip " class" suffix from metaclass names.
%% e.g., 'RuntimeError class' → 'RuntimeError', 'TypeError' → 'TypeError'
-spec strip_class_suffix(atom()) -> atom().
strip_class_suffix(ClassName) ->
    Str = atom_to_list(ClassName),
    case lists:suffix(" class", Str) of
        true -> list_to_atom(lists:sublist(Str, length(Str) - 6));
        false -> ClassName
    end.

%% @doc Wrap a `#beamtalk_error{}` record as an Exception tagged map.
%%
%% Sets `$beamtalk_class` based on the error kind (BT-452):
%% does_not_understand → RuntimeError, type_error → TypeError, etc.
-spec wrap(#beamtalk_error{} | term()) -> map().
wrap(#beamtalk_error{kind = Kind} = Error) ->
    Class = kind_to_class(Kind),
    #{'$beamtalk_class' => Class, error => Error};
wrap(Other) ->
    GenError = #beamtalk_error{
        kind = runtime_error,
        class = 'Exception',
        selector = undefined,
        message = iolist_to_binary(io_lib:format("~p", [Other])),
        hint = undefined,
        details = #{reason => Other}
    },
    #{'$beamtalk_class' => kind_to_class(runtime_error), error => GenError}.

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
%% Matches any exception hierarchy class (Exception, Error, RuntimeError, etc.).
-spec dispatch(atom(), list(), map()) -> term().
dispatch('message', [], #{error := Error}) ->
    Error#beamtalk_error.message;
dispatch('hint', [], #{error := Error}) ->
    case Error#beamtalk_error.hint of
        undefined -> nil;
        Hint -> Hint
    end;
dispatch('kind', [], #{error := Error}) ->
    Error#beamtalk_error.kind;
dispatch('selector', [], #{error := Error}) ->
    case Error#beamtalk_error.selector of
        undefined -> nil;
        Selector -> Selector
    end;
dispatch('errorClass', [], #{error := Error}) ->
    Error#beamtalk_error.class;
dispatch('printString', [], #{error := Error}) ->
    beamtalk_error:format(Error);
dispatch('describe', [], Self) ->
    dispatch('printString', [], Self);
dispatch('class', [], #{'$beamtalk_class' := Class}) ->
    Class;
dispatch('signal', [], #{error := Error}) ->
    beamtalk_error:raise(Error);
dispatch('signal:', [Message], _Self) ->
    signal_message(Message);
dispatch(Selector, _Args, #{'$beamtalk_class' := Class}) ->
    Error0 = beamtalk_error:new(does_not_understand, Class),
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
