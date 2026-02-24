%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Minimal ClassBuilder bootstrap stub for ADR 0038 (ClassBuilder Protocol Phase 1).
%%%
%%% **DDD Context:** Object System
%%%
%%% This module acts as the implementation module for the 'ClassBuilder' class,
%%% enabling ADR 0038 Phase 1: pre-wiring ClassBuilder in the bootstrap sequence
%%% so that compiled class `on_load` hooks can find it before user modules load.
%%%
%%% ## Phase 1 (BT-835): Bootstrap Stub
%%%
%%% Registers 'ClassBuilder' during bootstrap (after Metaclass, before Actor)
%%% using the same mechanism as `beamtalk_class_bt.erl` and
%%% `beamtalk_metaclass_bt.erl`. ClassBuilder is an Actor subclass per the ADR
%%% design, but can be registered with superclass 'Actor' before Actor's own
%%% class process exists — the class system uses lazy superclass resolution.
%%%
%%% Bootstrap sequence after this change:
%%%   ProtoObject → Object → Behaviour → Class → Metaclass → ClassBuilder → Actor
%%%
%%% ## Phase 2 (Future)
%%%
%%% This stub will be replaced by a compiled ClassBuilder.bt stdlib module once
%%% the full builder protocol (name:, superclass:, fields:, methods:, register)
%%% is implemented. The stub exposes no instance methods; all dispatch returns
%%% `does_not_understand` and falls through to the Actor → Object chain.
%%%
%%% ## ClassBuilder Chain
%%%
%%% ClassBuilder instances dispatch through:
%%%   ClassBuilder instance methods → Actor instance methods → Object → ProtoObject
%%%
%%% This module provides the 'ClassBuilder' instance methods entry point (stub).
%%%
%%% ## References
%%%
%%% * ADR 0038: `docs/ADR/0038-subclass-classbuilder-protocol.md`
%%% * Pattern: `beamtalk_class_bt.erl`, `beamtalk_metaclass_bt.erl`

-module(beamtalk_class_builder_bt).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([dispatch/4, has_method/1, register_class/0]).

%%% ============================================================================
%%% Instance Method Dispatch
%%% ============================================================================

%% @doc Dispatch instance messages on ClassBuilder objects.
%%
%% Phase 1 stub: all selectors return `does_not_understand`. The full
%% ClassBuilder protocol (name:, superclass:, fields:, methods:, register) is
%% implemented in Phase 2 (ClassBuilder.bt stdlib module).
%%
%% ADR 0038 Phase 1 (BT-835): Stub only. Phase 2 replaces this with compiled
%% ClassBuilder.bt exports.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, #beamtalk_error{}, map()}.
dispatch(Selector, _Args, _Self, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'ClassBuilder'),
    Error = beamtalk_error:with_selector(Error0, Selector),
    {error, Error, State}.

%% @doc Check if ClassBuilder has an instance method.
%%
%% Phase 1 stub: returns false for all selectors. The full builder protocol
%% is implemented in Phase 2 (ClassBuilder.bt stdlib module).
%%
%% ADR 0038 Phase 2: Will be replaced by compiled ClassBuilder.bt exports.
-spec has_method(atom()) -> boolean().
has_method(_) -> false.

%%% ============================================================================
%%% Class Registration (called during bootstrap)
%%% ============================================================================

%% @doc Register the 'ClassBuilder' class in the class registry.
%%
%% Called during bootstrap (beamtalk_bootstrap) after 'Metaclass' is registered,
%% before 'Actor' loads from stdlib. The 'ClassBuilder' class:
%%   - superclass: 'Actor' (ClassBuilder instances are short-lived gen_servers)
%%   - module: beamtalk_class_builder_bt (this stub)
%%   - No instance variables (state managed by the builder gen_server)
%%   - No instance methods in Phase 1 (full protocol in Phase 2)
%%
%% Bootstrap order: ProtoObject → Object → Behaviour → Class → Metaclass →
%%                  ClassBuilder → Actor → user modules
%%
%% ADR 0038 Phase 1: Bootstrap stub. Full protocol in Phase 2 (ClassBuilder.bt).
-spec register_class() -> ok.
register_class() ->
    ClassInfo = #{
        name => 'ClassBuilder',
        superclass => 'Actor',
        module => beamtalk_class_builder_bt,
        fields => [],
        class_methods => #{},
        instance_methods => #{}
    },
    case beamtalk_object_class:start('ClassBuilder', ClassInfo) of
        {ok, _Pid} ->
            ?LOG_INFO("Registered ClassBuilder (ADR 0038 Phase 1 stub)", #{module => ?MODULE}),
            ok;
        {error, {already_started, _}} ->
            %% ClassBuilder was already registered (e.g., bootstrap ran twice or a prior
            %% test registered it). Refresh the metadata to keep it consistent.
            beamtalk_object_class:update_class('ClassBuilder', ClassInfo),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to register ClassBuilder", #{reason => Reason}),
            ok
    end.
