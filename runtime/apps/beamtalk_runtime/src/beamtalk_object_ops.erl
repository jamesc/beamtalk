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
%%% The dispatch service calls `beamtalk_object_ops:dispatch(Selector, Args, Self, State)`
%%% where State is the actor's actual state map (containing `$beamtalk_class`, fields, etc.).
%%%
%%% ## References
%%%
%%% - ADR 0006: Unified Method Dispatch with Hierarchy Walking
%%% - BT-275: printString/yourself/hash protocol
%%% - BT-282: Bootstrap Object with shared reflection methods
-module(beamtalk_object_ops).

-export([dispatch/4, has_method/1]).

-include("beamtalk.hrl").

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
    %% BT-412: Return class as first-class object (not just an atom)
    ClassName = beamtalk_tagged_map:class_of(State),
    ClassObj = beamtalk_primitive:class_of_object_by_name(ClassName),
    {reply, ClassObj, State};

dispatch('respondsTo:', [Selector], _Self, State) when is_atom(Selector) ->
    ClassName = beamtalk_tagged_map:class_of(State),
    Result = beamtalk_dispatch:responds_to(Selector, ClassName),
    {reply, Result, State};

dispatch('instVarNames', [], _Self, State) ->
    {reply, beamtalk_reflection:field_names(State), State};

dispatch('instVarAt:', [FieldName], _Self, State) ->
    {reply, beamtalk_reflection:read_field(FieldName, State), State};

dispatch('instVarAt:put:', [FieldName, Value], _Self, State) ->
    {WrittenValue, NewState} = beamtalk_reflection:write_field(FieldName, Value, State),
    {reply, WrittenValue, NewState};

%% --- Display methods ---

dispatch('printString', [], _Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    Str = iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8)]),
    {reply, Str, State};

dispatch(inspect, [], _Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    UserFields = beamtalk_reflection:field_names(State),
    FieldStrs = [io_lib:format("~p: ~p", [K, maps:get(K, State)]) || K <- UserFields],
    FieldsPart = case FieldStrs of
        [] -> <<"">>;
        _ -> iolist_to_binary([<<" (">>, lists:join(<<", ">>, FieldStrs), <<")">>])
    end,
    Str = iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8), FieldsPart]),
    {reply, Str, State};

dispatch(describe, [], _Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    Str = iolist_to_binary([<<"an instance of ">>, atom_to_binary(ClassName, utf8)]),
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

%% --- Dynamic dispatch methods (BT-427) ---

dispatch('perform:', [TargetSelector], Self, State) when is_atom(TargetSelector) ->
    %% Dynamic message send with no arguments
    %% obj perform: #increment  => obj increment
    ClassName = beamtalk_tagged_map:class_of(State),
    Result = beamtalk_dispatch:lookup(TargetSelector, [], Self, State, ClassName),
    normalize_dispatch_result(Result, State);

dispatch('perform:withArguments:', [TargetSelector, ArgList], Self, State)
  when is_atom(TargetSelector), is_list(ArgList) ->
    %% Dynamic message send with argument list
    %% obj perform: #'at:put:' withArguments: #(1, 'x')
    ClassName = beamtalk_tagged_map:class_of(State),
    Result = beamtalk_dispatch:lookup(TargetSelector, ArgList, Self, State, ClassName),
    normalize_dispatch_result(Result, State);

dispatch('perform:withArguments:', [_TargetSelector, _ArgList], _Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    Error0 = beamtalk_error:new(type_error, ClassName),
    Error1 = beamtalk_error:with_selector(Error0, 'perform:withArguments:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Expected atom selector and list of arguments">>),
    {error, Error2, State};

%% BT-405: Abstract method contract — mirrors Object.bt pure method body
%% Runtime clause needed until compiled stdlib dispatch is wired up
dispatch(subclassResponsibility, [], _Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    Error0 = beamtalk_error:new(user_error, ClassName),
    Error1 = beamtalk_error:with_message(Error0, <<"This method is abstract and must be overridden by a subclass">>),
    {error, Error1, State};

%% --- Fallback: method not found ---

dispatch(Selector, _Args, _Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
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
has_method(subclassResponsibility) -> true;
has_method(_) -> false.

%% @private
%% @doc Normalize dispatch_result() (2-tuple error) to dispatch/4 contract (3-tuple error).
normalize_dispatch_result({error, Error}, State) ->
    {error, Error, State};
normalize_dispatch_result(Other, _State) ->
    Other.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_dispatch_result_error_test() ->
    State = #{},
    Error = beamtalk_error:new(does_not_understand, 'Test'),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}, #{}},
                 normalize_dispatch_result({error, Error}, State)).

normalize_dispatch_result_reply_test() ->
    State = #{},
    ?assertMatch({reply, 42, #{}},
                 normalize_dispatch_result({reply, 42, State}, State)).

-endif.
