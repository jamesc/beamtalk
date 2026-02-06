%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Object base class implementation (ADR 0006 Phase 1b).
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% This module provides the shared reflection, display, and utility methods
%%% inherited by all Beamtalk objects. It is registered as Object's runtime
%%% module during bootstrap, so these methods are found via hierarchy walking
%%% rather than being duplicated in every class's generated code.
%%%
%%% ## Reflection Methods
%%%
%%% | Selector          | Args          | Description                           |
%%% |-------------------|---------------|---------------------------------------|
%%% | `class`           | []            | Returns the object's class name       |
%%% | `respondsTo:`     | [Selector]    | Checks if object understands message  |
%%% | `instVarNames`    | []            | Returns list of instance variable names|
%%% | `instVarAt:`      | [Name]        | Returns instance variable value       |
%%% | `instVarAt:put:`  | [Name, Value] | Sets instance variable value          |
%%%
%%% ## Display Methods
%%%
%%% | Selector      | Args | Description                              |
%%% |---------------|------|------------------------------------------|
%%% | `printString` | []   | Human-readable string representation     |
%%% | `inspect`     | []   | Detailed inspection string               |
%%% | `describe`    | []   | Description of the object                |
%%%
%%% ## Utility Methods
%%%
%%% | Selector   | Args | Description                        |
%%% |------------|------|------------------------------------|
%%% | `yourself` | []   | Returns the receiver (Self)        |
%%% | `hash`     | []   | Returns a hash value               |
%%% | `isNil`    | []   | Returns false (non-nil objects)    |
%%% | `notNil`   | []   | Returns true (non-nil objects)     |
%%%
%%% ## Integration
%%%
%%% This module is invoked by `beamtalk_dispatch` when walking the hierarchy.
%%% The dispatch service calls `beamtalk_object:dispatch(Selector, Args, Self, State)`
%%% where State is the actor's actual state map (containing `__class__`, fields, etc.).
%%%
%%% ## References
%%%
%%% - ADR 0006: Unified Method Dispatch with Hierarchy Walking
%%% - BT-275: printString/yourself/hash protocol
%%% - BT-282: Bootstrap Object with shared reflection methods
-module(beamtalk_object).

-export([dispatch/4, has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Internal field names (filtered from instVarNames)
%%% ============================================================================

-define(INTERNAL_FIELDS, ['__class__', '__class_mod__', '__methods__', '__registry_pid__']).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to the Object base class.
%%
%% This is called by `beamtalk_dispatch` when a method is found in Object
%% during hierarchy walking. State is the actor's actual state map.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, term(), map()}.

%% --- Reflection methods ---

dispatch(class, [], _Self, State) ->
    ClassName = maps:get('__class__', State),
    {reply, ClassName, State};

dispatch('respondsTo:', [Selector], _Self, State) when is_atom(Selector) ->
    ClassName = maps:get('__class__', State),
    Result = beamtalk_dispatch:responds_to(Selector, ClassName),
    {reply, Result, State};

dispatch('instVarNames', [], _Self, State) ->
    AllKeys = maps:keys(State),
    UserFields = [K || K <- AllKeys, not lists:member(K, ?INTERNAL_FIELDS)],
    {reply, UserFields, State};

dispatch('instVarAt:', [FieldName], _Self, State) ->
    case maps:is_key(FieldName, State) of
        true ->
            Value = maps:get(FieldName, State),
            {reply, Value, State};
        false ->
            Error0 = beamtalk_error:new(does_not_understand, maps:get('__class__', State, 'Object')),
            Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:'),
            FieldBin = if is_atom(FieldName) -> atom_to_binary(FieldName);
                          true -> iolist_to_binary(io_lib:format("~p", [FieldName]))
                       end,
            Error2 = beamtalk_error:with_hint(Error1, <<"Field not found: ", FieldBin/binary>>),
            {error, Error2, State}
    end;

dispatch('instVarAt:put:', [FieldName, Value], _Self, State) ->
    case maps:is_key(FieldName, State) of
        true ->
            NewState = maps:put(FieldName, Value, State),
            {reply, Value, NewState};
        false ->
            Error0 = beamtalk_error:new(does_not_understand, maps:get('__class__', State, 'Object')),
            Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
            FieldBin = if is_atom(FieldName) -> atom_to_binary(FieldName);
                          true -> iolist_to_binary(io_lib:format("~p", [FieldName]))
                       end,
            Error2 = beamtalk_error:with_hint(Error1, <<"Field not found: ", FieldBin/binary>>),
            {error, Error2, State}
    end;

%% --- Display methods ---

dispatch('printString', [], _Self, State) ->
    ClassName = maps:get('__class__', State, 'Object'),
    Str = iolist_to_binary([<<"a ">>, atom_to_binary(ClassName)]),
    {reply, Str, State};

dispatch(inspect, [], _Self, State) ->
    ClassName = maps:get('__class__', State, 'Object'),
    %% Include user fields in inspection
    AllKeys = maps:keys(State),
    UserFields = [K || K <- AllKeys, not lists:member(K, ?INTERNAL_FIELDS)],
    FieldStrs = [io_lib:format("~p: ~p", [K, maps:get(K, State)]) || K <- UserFields],
    FieldsPart = case FieldStrs of
        [] -> <<"">>;
        _ -> iolist_to_binary([<<" (">>, lists:join(<<", ">>, FieldStrs), <<")">>])
    end,
    Str = iolist_to_binary([<<"a ">>, atom_to_binary(ClassName), FieldsPart]),
    {reply, Str, State};

dispatch(describe, [], _Self, State) ->
    ClassName = maps:get('__class__', State, 'Object'),
    Str = iolist_to_binary([<<"an instance of ">>, atom_to_binary(ClassName)]),
    {reply, Str, State};

%% --- Utility methods ---

dispatch(yourself, [], Self, State) ->
    {reply, Self, State};

dispatch(hash, [], Self, State) ->
    Hash = erlang:phash2(Self),
    {reply, Hash, State};

dispatch(isNil, [], _Self, State) ->
    {reply, false, State};

dispatch(notNil, [], _Self, State) ->
    {reply, true, State};

%% --- Fallback: method not found ---

dispatch(Selector, _Args, _Self, State) ->
    ClassName = maps:get('__class__', State, 'Object'),
    Error0 = beamtalk_error:new(does_not_understand, ClassName),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Method not found in Object">>),
    {error, Error2, State}.

%% @doc Check if Object responds to a given selector.
-spec has_method(atom()) -> boolean().
has_method(class) -> true;
has_method('respondsTo:') -> true;
has_method('instVarNames') -> true;
has_method('instVarAt:') -> true;
has_method('instVarAt:put:') -> true;
has_method('perform:') -> true;
has_method('perform:withArguments:') -> true;
has_method('printString') -> true;
has_method(inspect) -> true;
has_method(describe) -> true;
has_method(yourself) -> true;
has_method(hash) -> true;
has_method(isNil) -> true;
has_method(notNil) -> true;
has_method(_) -> false.
