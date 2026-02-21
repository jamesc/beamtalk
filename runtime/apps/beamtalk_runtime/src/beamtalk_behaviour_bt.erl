%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Bootstrap stub for the 'Behaviour' class (ADR 0032 Phase 2, BT-734).
%%%
%% **DDD Context:** Object System
%%%
%%% This module acts as a minimal placeholder for the 'Behaviour' class,
%%% registered during bootstrap before compiled stdlib modules load.
%%%
%%% ## Bootstrap Role
%%%
%%% 'Behaviour' must be registered in the class hierarchy before any user
%%% classes load, so that the class chain dispatch (Class → Behaviour →
%%% Object) is available from startup. This stub:
%%%   - Registers 'Behaviour' with superclass 'Object'
%%%   - Returns DNU for all method dispatches (will be replaced by compiled module)
%%%
%%% ## Replacement
%%%
%%% This stub is replaced when `bt@stdlib@behaviour.beam` loads via on_load
%%% during `beamtalk_stdlib:load_compiled_stdlib_modules()`. After that,
%%% all Behaviour methods are provided by the compiled Beamtalk module.

-module(beamtalk_behaviour_bt).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([dispatch/4, has_method/1, register_class/0]).

%%% ============================================================================
%%% Instance Method Dispatch
%%% ============================================================================

%% @doc Dispatch instance messages on Behaviour class objects.
%%
%% This stub returns DNU for all selectors. The compiled bt@stdlib@behaviour
%% module replaces this dispatch via update_class on_load.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, #beamtalk_error{}, map()}.
dispatch(Selector, _Args, _Self, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Behaviour'),
    Error = beamtalk_error:with_selector(Error0, Selector),
    {error, Error, State}.

%% @doc Check if Behaviour has an instance method (stub: always false).
%%
%% The compiled bt@stdlib@behaviour module replaces this.
-spec has_method(atom()) -> boolean().
has_method(_) -> false.

%%% ============================================================================
%%% Class Registration
%%% ============================================================================

%% @doc Register the 'Behaviour' class in the class registry.
%%
%% Called during bootstrap to ensure 'Behaviour' is available before
%% stdlib modules load. The class chain dispatch requires Behaviour to
%% be registered so that Class → Behaviour → Object lookups succeed.
-spec register_class() -> ok | {error, term()}.
register_class() ->
    ClassInfo = #{
        name => 'Behaviour',
        superclass => 'Object',
        module => beamtalk_behaviour_bt,
        instance_variables => [],
        is_abstract => true,
        class_methods => #{},
        instance_methods => #{}
    },
    case beamtalk_object_class:start('Behaviour', ClassInfo) of
        {ok, _Pid} ->
            ?LOG_INFO("Registered Behaviour (ADR 0032 Phase 2 stub)", #{module => ?MODULE}),
            ok;
        {error, {already_started, _}} ->
            case beamtalk_object_class:update_class('Behaviour', ClassInfo) of
                {ok, _} -> ok;
                {error, UpdErr} ->
                    ?LOG_ERROR("Failed to update Behaviour class", #{reason => UpdErr}),
                    {error, UpdErr}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to register Behaviour", #{reason => Reason}),
            {error, Reason}
    end.
