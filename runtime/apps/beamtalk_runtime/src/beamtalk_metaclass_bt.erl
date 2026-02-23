%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Minimal Metaclass bootstrap stub for ADR 0036 (Full Metaclass Tower Phase 1).
%%%
%% **DDD Context:** Object System
%%%
%%% This module acts as the implementation module for the 'Metaclass' class,
%%% enabling ADR 0036 Phase 1: replacing the `#Metaclass` sentinel atom with a
%%% real `#beamtalk_object{}` and wiring up metaclass dispatch through the runtime.
%%%
%%% ## Phase 1 (BT-802): Bootstrap Stub and Runtime Wiring
%%%
%%% Implements the 3 identity predicates that differ from the 'Class' chain:
%%%   - `isMeta`      → true  (overrides Behaviour's false)
%%%   - `isClass`     → false (overrides Class's true)
%%%   - `isMetaclass` → true  (not defined in superclasses)
%%%
%%% All other selectors return `does_not_understand` and fall through to the
%%% Class → Behaviour → Object → ProtoObject chain via beamtalk_dispatch.
%%%
%%% ## Future (ADR 0036 Phase 3)
%%%
%%% This stub will be replaced by a compiled Metaclass.bt stdlib module once
%%% the full Metaclass protocol (thisClass, name, superclass, printString) is
%%% implemented (BT-803 or equivalent Phase 2/3 issue).
%%%
%%% ## Metaclass Chain
%%%
%%% Metaclass objects (e.g., Counter class class) dispatch through:
%%%   Metaclass instance methods → Class instance methods → Behaviour → Object → ProtoObject
%%%
%%% This module provides the 'Metaclass' instance methods entry point.

-module(beamtalk_metaclass_bt).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([dispatch/4, has_method/1, register_class/0]).

%%% ============================================================================
%%% Instance Method Dispatch (called by beamtalk_dispatch:lookup when Self is a metaclass object)
%%% ============================================================================

%% @doc Dispatch instance messages on Metaclass objects.
%%
%% Metaclass objects (e.g., Counter class class) respond to identity predicates
%% via this module. All other selectors fall through to the Class/Behaviour chain.
%%
%% ADR 0036 Phase 1 (BT-802): Implements the 3 identity predicates that are
%% overridden from the Class chain:
%%   - isMeta      → true  (Behaviour defines isMeta → false; Metaclass overrides)
%%   - isClass     → false (Class defines isClass → true; Metaclass overrides)
%%   - isMetaclass → true  (new predicate, not in superclasses)
%%
%% Future (ADR 0036 Phase 3): This stub will be replaced by compiled Metaclass.bt.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, #beamtalk_error{}, map()}.
dispatch('isMeta', [], _Self, State) ->
    {reply, true, State};
dispatch('isClass', [], _Self, State) ->
    {reply, false, State};
dispatch('isMetaclass', [], _Self, State) ->
    {reply, true, State};
dispatch(Selector, _Args, _Self, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Metaclass'),
    Error = beamtalk_error:with_selector(Error0, Selector),
    {error, Error, State}.

%% @doc Check if Metaclass has an instance method.
%%
%% Returns true for the 3 identity predicates implemented directly in this stub.
%% All other selectors are inherited from Class/Behaviour via chain walk.
%%
%% ADR 0036 Phase 3: Will be replaced by compiled Metaclass.bt exports.
-spec has_method(atom()) -> boolean().
has_method('isMeta') -> true;
has_method('isClass') -> true;
has_method('isMetaclass') -> true;
has_method(_) -> false.

%%% ============================================================================
%%% Class Registration (called during bootstrap)
%%% ============================================================================

%% @doc Register the 'Metaclass' class in the class registry.
%%
%% Called during bootstrap (beamtalk_bootstrap) after 'Class' is registered.
%% Bootstrap order: ProtoObject → Object → Behaviour → Class → Metaclass → Actor → user modules.
%%
%% The 'Metaclass' class:
%%   - superclass: 'Class' (inherits class protocol)
%%   - module: beamtalk_metaclass_bt (this module)
%%   - instance_methods: 3 identity predicates (isMeta, isClass, isMetaclass)
%%
%% ADR 0036 Phase 1: Bootstrap stub. Full protocol in Phase 3 (Metaclass.bt).
-spec register_class() -> ok.
register_class() ->
    ClassInfo = #{
        name => 'Metaclass',
        superclass => 'Class',
        module => beamtalk_metaclass_bt,
        instance_variables => [],
        class_methods => #{},
        instance_methods => #{
            'isMeta' => #{arity => 0},
            'isClass' => #{arity => 0},
            'isMetaclass' => #{arity => 0},
            'thisClass' => #{arity => 0},
            'name' => #{arity => 0}
        }
    },
    case beamtalk_object_class:start('Metaclass', ClassInfo) of
        {ok, _Pid} ->
            ?LOG_INFO("Registered Metaclass (ADR 0036 Phase 1 stub)", #{module => ?MODULE}),
            ok;
        {error, {already_started, _}} ->
            %% Metaclass was already registered (e.g., bootstrap ran twice or a prior
            %% test registered it). Refresh the metadata to keep it consistent.
            beamtalk_object_class:update_class('Metaclass', ClassInfo),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to register Metaclass", #{reason => Reason}),
            ok
    end.
