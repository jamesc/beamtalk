%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Hot reload domain service for state migration during code upgrades.
%%
%% **DDD Context:** Hot Reload Context
%%
%% This domain service implements the StateMigrator pattern from the DDD model.
%% It centralizes code_change/3 logic for OTP hot code upgrade, providing a
%% consistent state migration strategy across all gen_server behaviors in the
%% runtime.
%%
%% **Current Migrations:**
%% - BT-399: Rewrites `__class__` → `$beamtalk_class` tag key for actors
%%   with pre-BT-324 state maps.
%% - BT-572: Field migration — adds new fields with defaults, drops removed
%%   fields (with log warning) when Extra contains `{NewInstanceVars, Module}`.
%%
%% **References:**
%% - docs/beamtalk-ddd-model.md (Hot Reload Context, StateMigrator)
%% - http://erlang.org/doc/design_principles/appup_cookbook.html

-module(beamtalk_hot_reload).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([code_change/3, trigger_code_change/2, trigger_code_change/3]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Migrate state during hot code upgrade.
%%
%% This is the domain service implementation of OTP's code_change/3 callback.
%% Called by all gen_server behaviors in the runtime when BEAM loads a new
%% version of a module.
%%
%% **Current Migrations:**
%% - Rewrites `__class__` → `$beamtalk_class` for pre-BT-324 actor state maps.
%%   Idempotent: already-migrated state is returned unchanged.
%% - BT-572: When Extra is `{NewInstanceVars, Module}`, migrates actor state
%%   by calling the module's init to get new defaults, then merging.
%%
%% @param OldVsn The old version (either {down, Vsn} or Vsn atom/term)
%% @param State The current gen_server state
%% @param Extra Application-specific upgrade data passed via sys:change_code/4
%% @returns {ok, NewState} on success, or {error, Reason} on failure
-spec code_change(OldVsn :: term(), State :: term(), Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(_OldVsn, State, {NewInstanceVars, Module}) when is_map(State), is_list(NewInstanceVars), is_atom(Module) ->
    %% BT-572: Field migration during hot reload
    MigratedState = maybe_migrate_class_key(State),
    NewState = migrate_fields(MigratedState, NewInstanceVars, Module),
    {ok, NewState};
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, maybe_migrate_class_key(State)};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Trigger code_change for a list of actor PIDs after module reload.
%%
%% Calls sys:change_code/4 for each actor PID with empty Extra.
%%
%% @param Module The module that was reloaded
%% @param Pids List of actor PIDs to upgrade
%% @returns {ok, Upgraded, Failures} where Upgraded is count of successes
%%          and Failures is list of {Pid, Reason} tuples
-spec trigger_code_change(atom(), [pid()]) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Module, Pids) ->
    trigger_code_change(Module, Pids, []).

%% @doc Trigger code_change for a list of actor PIDs with Extra data.
%%
%% BT-572: Extra can be `{NewInstanceVars, Module}` to enable field migration.
%% Calls sys:change_code/4 for each actor PID. Failures are collected
%% but do not prevent other actors from being upgraded.
-spec trigger_code_change(atom(), [pid()], term()) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Module, Pids, Extra) ->
    lists:foldl(
        fun(Pid, {ok, Upgraded, Failures}) ->
            case try_change_code(Pid, Module, Extra) of
                ok ->
                    {ok, Upgraded + 1, Failures};
                {error, Reason} ->
                    {ok, Upgraded, [{Pid, Reason} | Failures]}
            end
        end,
        {ok, 0, []},
        Pids
    ).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Migrate old `__class__` tag key to `$beamtalk_class` (BT-399).
%%
%% Idempotent: migration may add the new key and/or remove the old key,
%% but will not change an existing `$beamtalk_class` value.
%% Only adds the new key when the legacy value is an atom (actor class names);
%% when both keys exist, the legacy key is removed regardless to complete cleanup.
%% @private
-spec maybe_migrate_class_key(map()) -> map().
maybe_migrate_class_key(State) ->
    ClassKey = beamtalk_tagged_map:class_key(),
    case {maps:find(ClassKey, State), maps:find('__class__', State)} of
        {error, {ok, Class}} when is_atom(Class) ->
            %% New key missing, old key present - migrate and remove legacy key
            maps:remove('__class__', State#{ClassKey => Class});
        {{ok, _}, {ok, _}} ->
            %% Both keys present - keep new key, remove legacy key
            maps:remove('__class__', State);
        _ ->
            State
    end.

%% @doc Migrate actor state fields during hot reload (BT-572).
%%
%% Calls the module's init(#{}) to get default state, then:
%% - Preserves all existing field values from old state
%% - Adds new fields with their default values
%% - Drops removed fields (with log warning)
%% - Preserves internal keys ($beamtalk_class, __methods__, __class_mod__)
%% @private
-spec migrate_fields(map(), [atom()], atom()) -> map().
migrate_fields(OldState, NewInstanceVars, Module) ->
    %% Get new default state by calling init with empty args
    case catch Module:init(#{}) of
        {ok, NewDefaults} when is_map(NewDefaults) ->
            %% Internal keys to always preserve from new defaults
            InternalKeys = beamtalk_tagged_map:internal_fields(),
            %% Start with internal keys from new defaults (updated method table etc.)
            BaseState = lists:foldl(
                fun(Key, Acc) ->
                    case maps:find(Key, NewDefaults) of
                        {ok, Val} -> maps:put(Key, Val, Acc);
                        error -> Acc
                    end
                end,
                #{},
                InternalKeys
            ),
            %% Add new field defaults
            NewVarSet = sets:from_list(NewInstanceVars, [{version, 2}]),
            WithDefaults = lists:foldl(
                fun(Var, Acc) ->
                    case maps:find(Var, NewDefaults) of
                        {ok, Default} -> maps:put(Var, Default, Acc);
                        error -> Acc
                    end
                end,
                BaseState,
                NewInstanceVars
            ),
            %% Overlay old state values (existing values win)
            OldInstanceVars = maps:without(InternalKeys, OldState),
            {Kept, Dropped} = maps:fold(
                fun(Key, Value, {KeepAcc, DropAcc}) ->
                    case sets:is_element(Key, NewVarSet) of
                        true -> {maps:put(Key, Value, KeepAcc), DropAcc};
                        false -> {KeepAcc, [Key | DropAcc]}
                    end
                end,
                {WithDefaults, []},
                OldInstanceVars
            ),
            %% Log warning for dropped fields
            case Dropped of
                [] -> ok;
                _ ->
                    ClassName = maps:get('$beamtalk_class', OldState, unknown),
                    ?LOG_WARNING("Hot reload dropped fields",
                                 #{class => ClassName, fields => Dropped})
            end,
            Kept;
        _ ->
            %% init failed — keep state unchanged
            OldState
    end.

%% @private
%% @doc Try to trigger code_change for a single actor via sys:change_code/4.
-spec try_change_code(pid(), atom(), term()) -> ok | {error, term()}.
try_change_code(Pid, Module, Extra) ->
    try
        ok = sys:suspend(Pid),
        try
            sys:change_code(Pid, Module, undefined, Extra)
        after
            catch sys:resume(Pid)
        end
    catch
        exit:{noproc, _} -> {error, noproc};
        exit:{timeout, _} -> {error, timeout};
        Class:Error -> {error, {Class, Error}}
    end.
