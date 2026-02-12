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
%% **Current Strategy:** Preserve state unchanged ({ok, State}).
%%
%% **Future:** Can be extended to support:
%% - Automatic field migration (add defaults, preserve unknowns)
%% - Version-specific transformations
%% - State validation during upgrade
%%
%% **References:**
%% - docs/beamtalk-ddd-model.md (Hot Reload Context, StateMigrator)
%% - http://erlang.org/doc/design_principles/appup_cookbook.html

-module(beamtalk_hot_reload).

%% Public API
-export([code_change/3, trigger_code_change/2]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Migrate state during hot code upgrade.
%%
%% This is the domain service implementation of OTP's code_change/3 callback.
%% Called by all gen_server behaviors in the runtime when BEAM loads a new
%% version of a module.
%%
%% **Current Behavior:** Returns state unchanged, supporting the OTP upgrade
%% protocol without transforming state. This preserves existing behavior while
%% centralizing the logic for future enhancement.
%%
%% @param OldVsn The old version (either {down, Vsn} or Vsn atom/term)
%% @param State The current gen_server state
%% @param Extra Application-specific upgrade data passed via sys:change_code/4
%% @returns {ok, NewState} on success, or {error, Reason} on failure
-spec code_change(OldVsn :: term(), State :: term(), Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Trigger code_change for a list of actor PIDs after module reload.
%%
%% Calls sys:change_code/4 for each actor PID. Failures are collected
%% but do not prevent other actors from being upgraded.
%%
%% @param Module The module that was reloaded
%% @param Pids List of actor PIDs to upgrade
%% @returns {ok, Upgraded, Failures} where Upgraded is count of successes
%%          and Failures is list of {Pid, Reason} tuples
-spec trigger_code_change(atom(), [pid()]) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Module, Pids) ->
    lists:foldl(
        fun(Pid, {ok, Upgraded, Failures}) ->
            case try_change_code(Pid, Module) of
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

%% @private
%% Try to trigger code_change for a single actor.
-spec try_change_code(pid(), atom()) -> ok | {error, term()}.
try_change_code(Pid, Module) ->
    try
        sys:change_code(Pid, Module, undefined, [])
    catch
        exit:{noproc, _} -> {error, noproc};
        exit:{timeout, _} -> {error, timeout};
        Class:Error -> {error, {Class, Error}}
    end.
