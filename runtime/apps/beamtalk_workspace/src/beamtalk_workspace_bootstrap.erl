%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Bootstrap worker for singleton class variables (ADR 0019 Phase 2).
%%%
%%% Sets class variables on singleton stdlib classes after workspace supervisor
%%% starts the singleton actors. Monitors singleton PIDs and re-sets class
%%% variables when children restart.
%%%
%%% **DDD Context:** Workspace
%%%
%%% Singleton mapping:
%%%   TranscriptStream.current  ← registered process 'Transcript'
%%%   SystemDictionary.current  ← registered process 'Beamtalk'
%%%   WorkspaceEnvironment.current ← registered process 'Workspace'

-module(beamtalk_workspace_bootstrap).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Singleton class ↔ registered process name ↔ binding class name mapping
-define(SINGLETONS, [
    {'TranscriptStream',    'Transcript',   'TranscriptStream'},
    {'SystemDictionary',    'Beamtalk',     'SystemDictionary'},
    {'WorkspaceEnvironment', 'Workspace',   'WorkspaceEnvironment'}
]).

-record(state, {
    monitors = #{} :: #{reference() => {ClassName :: atom(), RegName :: atom(), BindingClassName :: atom()}}
}).

%% @doc Start the bootstrap worker.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init([]) ->
    State = bootstrap_all(#state{}),
    {ok, State}.

%% @private
handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    case maps:get(MonRef, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        {ClassName, RegName, BindingClassName} ->
            Monitors = maps:remove(MonRef, State#state.monitors),
            NewState = State#state{monitors = Monitors},
            %% Re-bootstrap after a short delay to allow the supervisor
            %% to restart the child process
            erlang:send_after(100, self(), {rebootstrap, ClassName, RegName, BindingClassName, 0}),
            {noreply, NewState}
    end;
handle_info({rebootstrap, ClassName, RegName, BindingClassName, Retries}, State) when Retries < 5 ->
    case erlang:whereis(RegName) of
        undefined ->
            erlang:send_after(200, self(), {rebootstrap, ClassName, RegName, BindingClassName, Retries + 1}),
            {noreply, State};
        _Pid ->
            NewState = bootstrap_singleton(ClassName, RegName, BindingClassName, State),
            {noreply, NewState}
    end;
handle_info({rebootstrap, ClassName, RegName, _BindingClassName, _Retries}, State) ->
    ?LOG_ERROR("Bootstrap: failed to rewire singleton after retries", #{class => ClassName, name => RegName}),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%% Internal functions

%% @private Bootstrap all singletons.
bootstrap_all(State) ->
    lists:foldl(
        fun({ClassName, RegName, BindingClassName}, AccState) ->
            bootstrap_singleton(ClassName, RegName, BindingClassName, AccState)
        end,
        State,
        ?SINGLETONS
    ).

%% @private Bootstrap a single singleton: set class var and monitor.
bootstrap_singleton(ClassName, RegName, BindingClassName, State) ->
    case erlang:whereis(RegName) of
        undefined ->
            ?LOG_WARNING("Bootstrap: singleton not registered yet", #{name => RegName}),
            State;
        Pid ->
            Obj = build_object_ref(BindingClassName, Pid),
            set_class_variable(ClassName, Obj),
            MonRef = erlang:monitor(process, Pid),
            ?LOG_DEBUG("Bootstrap: wired singleton", #{class => ClassName, pid => Pid}),
            Monitors = maps:put(MonRef, {ClassName, RegName, BindingClassName}, State#state.monitors),
            State#state{monitors = Monitors}
    end.

%% @private Build the beamtalk_object reference tuple for a singleton.
build_object_ref(BindingClassName, Pid) ->
    {beamtalk_object, BindingClassName, class_module(BindingClassName), Pid}.

%% @private Map binding class name to its Erlang module.
class_module('TranscriptStream') -> beamtalk_transcript_stream;
class_module('SystemDictionary') -> beamtalk_system_dictionary;
class_module('WorkspaceEnvironment') -> beamtalk_workspace_environment;
class_module('Workspace') -> beamtalk_workspace_environment.

%% @private Set the `current` class variable on the class.
set_class_variable(ClassName, Obj) ->
    try
        beamtalk_object_class:set_class_var(ClassName, current, Obj)
    catch
        error:#beamtalk_error{kind = class_not_found} ->
            ?LOG_WARNING("Bootstrap: class not loaded yet", #{class => ClassName})
    end.
