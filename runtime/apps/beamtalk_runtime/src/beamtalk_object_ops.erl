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
%%% | `fieldNames`      | []            | Returns list of field names           |
%%% | `fieldAt:`        | [Name]        | Returns field value                   |
%%% | `fieldAt:put:`    | [Name, Value] | Sets field value                      |
%%%
%%% ## Display Methods
%%%
%%% | Selector      | Args | Description                              |
%%% |---------------|------|------------------------------------------|
%%% | `printString`   | []   | Developer representation (e.g. `a Counter`)  |
%%% | `displayString` | []   | User-facing representation (same for actors) |
%%% | `inspect`       | []   | Detailed inspection string                   |
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

-export([dispatch/4, has_method/1, class_name/3]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to the Object base class.
%%
%% This is called by `beamtalk_dispatch` when a method is found in Object
%% during hierarchy walking. State is the actor's actual state map.
%%
%% BT-753: When dispatched on class objects via chain fallthrough, State is
%% `#{}' (empty map). In that case, class identity is derived from Self
%% (`#beamtalk_object.class') instead of the State map.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, term(), map()}.

%% --- Reflection methods ---

dispatch(class, [], Self, State) ->
    %% BT-412: Return class as first-class object (not just an atom)
    ClassName = class_name(Self, State),
    ClassObj = beamtalk_primitive:class_of_object_by_name(ClassName),
    {reply, ClassObj, State};
dispatch('respondsTo:', [Selector], Self, State) when is_atom(Selector) ->
    ClassName = class_name_for_responds_to(Self, State),
    Result = beamtalk_dispatch:responds_to(Selector, ClassName),
    {reply, Result, State};
dispatch('fieldNames', [], _Self, State) ->
    {reply, beamtalk_reflection:field_names(State), State};
dispatch('fieldAt:', [FieldName], _Self, State) ->
    {reply, beamtalk_reflection:read_field(FieldName, State), State};
dispatch('fieldAt:put:', [FieldName, Value], _Self, State) ->
    {WrittenValue, NewState} = beamtalk_reflection:write_field(FieldName, Value, State),
    {reply, WrittenValue, NewState};
%% --- Display methods ---

dispatch('printString', [], Self, State) ->
    DisplayName = class_display_name(Self, State),
    Str = iolist_to_binary([<<"a ">>, DisplayName]),
    {reply, Str, State};
dispatch('displayString', [], Self, State) ->
    %% displayString for actors delegates to printString — same result.
    DisplayName = class_display_name(Self, State),
    Str = iolist_to_binary([<<"a ">>, DisplayName]),
    {reply, Str, State};
dispatch(inspect, [], Self, State) ->
    %% BT-753: For class objects (State is #{}), use Self for identity.
    %% For actors with state, use detailed inspect_string showing fields.
    case map_size(State) =:= 0 of
        true ->
            DisplayName = class_display_name(Self, State),
            Str = iolist_to_binary([<<"a ">>, DisplayName]),
            {reply, Str, State};
        false ->
            Str = beamtalk_reflection:inspect_string(State),
            {reply, Str, State}
    end;
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
    ClassName = class_name(Self, State),
    Result = beamtalk_dispatch:lookup(TargetSelector, [], Self, State, ClassName),
    normalize_dispatch_result(Result, State);
dispatch('perform:withArguments:', [TargetSelector, ArgList], Self, State) when
    is_atom(TargetSelector), is_list(ArgList)
->
    %% Dynamic message send with argument list
    %% obj perform: #'at:put:' withArguments: #(1, 'x')
    ClassName = class_name(Self, State),
    Result = beamtalk_dispatch:lookup(TargetSelector, ArgList, Self, State, ClassName),
    normalize_dispatch_result(Result, State);
dispatch('perform:withArguments:', [_TargetSelector, _ArgList], Self, State) ->
    ClassName = class_name(Self, State, 'Object'),
    Error0 = beamtalk_error:new(type_error, ClassName),
    Error1 = beamtalk_error:with_selector(Error0, 'perform:withArguments:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Expected atom selector and list of arguments">>),
    {error, Error2, State};
%% BT-405: Abstract method contract — mirrors Object.bt pure method body
%% Runtime clause needed until compiled stdlib dispatch is wired up
dispatch(subclassResponsibility, [], Self, State) ->
    ClassName = class_name(Self, State, 'Object'),
    Error0 = beamtalk_error:new(user_error, ClassName),
    Error1 = beamtalk_error:with_message(
        Error0, <<"This method is abstract and must be overridden by a subclass">>
    ),
    {error, Error1, State};
%% --- Fallback: method not found ---

dispatch(Selector, _Args, Self, State) ->
    ClassName = class_name(Self, State, 'Object'),
    Error0 = beamtalk_error:new(does_not_understand, ClassName),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Method not found in Object">>),
    {error, Error2, State}.

%% @doc Check if Object responds to a given selector.
-spec has_method(atom()) -> boolean().
has_method(class) -> true;
has_method('respondsTo:') -> true;
has_method('fieldNames') -> true;
has_method('fieldAt:') -> true;
has_method('fieldAt:put:') -> true;
has_method('perform:') -> true;
has_method('perform:withArguments:') -> true;
has_method('printString') -> true;
has_method('displayString') -> true;
has_method(inspect) -> true;
has_method(yourself) -> true;
has_method(hash) -> true;
has_method(isNil) -> true;
has_method(notNil) -> true;
has_method(subclassResponsibility) -> true;
has_method(_) -> false.

%% @private
%% @doc Return a display-friendly class name binary, stripping the " class" suffix.
-spec class_display_name(term(), map()) -> binary().
class_display_name(Self, State) ->
    ClassName = class_name(Self, State, 'Object'),
    beamtalk_class_registry:class_display_name(ClassName).

%% @private
%% @doc Extract class name from Self or State.
%%
%% BT-753: When dispatched on class objects via chain fallthrough, State is
%% `#{}' (empty map). In that case, derive the class from Self.
-spec class_name(term(), map()) -> atom() | undefined.
class_name(#beamtalk_object{class = Class}, _State) ->
    Class;
class_name(_Self, State) ->
    beamtalk_tagged_map:class_of(State).

%% @private
%% @doc Extract class name from Self or State, with a default fallback.
-spec class_name(term(), map(), atom()) -> atom().
class_name(#beamtalk_object{class = Class}, _State, _Default) ->
    Class;
class_name(_Self, State, Default) ->
    beamtalk_tagged_map:class_of(State, Default).

%% @private
%% @doc Return the class name to use for respondsTo: dispatch.
%%
%% BT-776: Class objects have a virtual metaclass tag (e.g., 'Counter class')
%% that is not registered in the class registry. For class objects, start the
%% responds_to chain walk from 'Class' instead.
-spec class_name_for_responds_to(term(), map()) -> atom().
class_name_for_responds_to(Self, State) when is_record(Self, beamtalk_object) ->
    case beamtalk_class_registry:is_class_object(Self) of
        true -> 'Class';
        false -> beamtalk_tagged_map:class_of(State)
    end;
class_name_for_responds_to(_Self, State) ->
    beamtalk_tagged_map:class_of(State).

%% @private
normalize_dispatch_result({error, Error}, State) ->
    {error, Error, State};
normalize_dispatch_result(Other, _State) ->
    Other.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_dispatch_result_error_test() ->
    State = #{},
    Error = beamtalk_error:new(does_not_understand, 'Test'),
    ?assertMatch(
        {error, #beamtalk_error{kind = does_not_understand}, #{}},
        normalize_dispatch_result({error, Error}, State)
    ).

normalize_dispatch_result_reply_test() ->
    State = #{},
    ?assertMatch(
        {reply, 42, #{}},
        normalize_dispatch_result({reply, 42, State}, State)
    ).

-endif.
