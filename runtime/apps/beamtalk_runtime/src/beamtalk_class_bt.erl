%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Minimal Class bootstrap stub for ADR 0032 (Early Class Protocol).
%%%
%%% **DDD Context:** Object System
%%%
%%% This module acts as the implementation module for the 'Class' class,
%%% enabling ADR 0032 Phase 0 wire check: proving that class-side messages not
%%% found in user-defined class methods can dispatch through the Class chain.
%%%
%%% ## Phase 0 (BT-732): Wire Check
%%%
%%% Registers 'Class' with a temporary `testClassProtocol` probe method.
%%% This method is removed after the wire check verifies the dispatch mechanism.
%%%
%%% ## Future (ADR 0032 Phase 2)
%%%
%%% This stub will be replaced by a compiled Class.bt stdlib module once
%%% the full Behaviour/Class/Metaclass protocol is implemented.
%%%
%%% ## Class Chain
%%%
%%% Class objects (e.g., Counter as an object) dispatch through:
%%%   Counter class methods → Class instance methods → Object instance methods
%%%
%%% This module provides the 'Class' instance methods entry point.

-module(beamtalk_class_bt).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([dispatch/4, has_method/1, register_class/0]).

%%% ============================================================================
%%% Instance Method Dispatch (called by beamtalk_dispatch:lookup when Self is a class object)
%%% ============================================================================

%% @doc Dispatch instance messages on Class objects.
%%
%% Class objects (e.g., Counter, Object, Actor as first-class objects)
%% respond to these messages via the Class chain.
%%
%% ADR 0032 Phase 0 wire check confirmed (BT-732): testClassProtocol was
%% used as a probe to verify the dispatch fallthrough mechanism works.
%% The probe method has been removed; the dispatch mechanism remains.
%%
%% Future (ADR 0032 Phase 2): This stub will be replaced by a compiled
%% Class.bt stdlib module with the real Class protocol methods.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, #beamtalk_error{}, map()}.
dispatch(Selector, _Args, _Self, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Class'),
    Error = beamtalk_error:with_selector(Error0, Selector),
    {error, Error, State}.

%% @doc Check if Class has an instance method.
%%
%% Used by beamtalk_object_class:has_method/2 to detect methods before
%% looking up in the flattened table.
%%
%% ADR 0032 Phase 2: Will be replaced by compiled Class.bt exports.
-spec has_method(atom()) -> boolean().
has_method(_) -> false.

%%% ============================================================================
%%% Class Registration (called during bootstrap)
%%% ============================================================================

%% @doc Register the 'Class' class in the class registry.
%%
%% Called during bootstrap (beamtalk_bootstrap) to ensure 'Class' is
%% available before stdlib modules load. The 'Class' class:
%%   - superclass: 'Object' (inherits standard object protocol)
%%   - module: beamtalk_class_bt (this module)
%%   - No instance variables (Class state is managed by the class process)
%%
%% ADR 0032 Phase 0: Wire check registration.
-spec register_class() -> ok.
register_class() ->
    ClassInfo = #{
        name => 'Class',
        superclass => 'Object',
        module => beamtalk_class_bt,
        instance_variables => [],
        class_methods => #{
            'superclass' => #{arity => 0}
        },
        instance_methods => #{}
    },
    case beamtalk_object_class:start('Class', ClassInfo) of
        {ok, _Pid} ->
            ?LOG_INFO("Registered Class (ADR 0032 Phase 0 stub)", #{module => ?MODULE}),
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to register Class", #{reason => Reason}),
            ok
    end.
