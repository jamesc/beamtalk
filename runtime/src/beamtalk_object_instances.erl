%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Instance tracking registry for Beamtalk.
%%
%% Tracks all instances of each class to support Smalltalk-style reflection:
%% - `allInstances` - get all instances of a class
%% - `instanceCount` - count instances of a class
%% - hot-patching notifications (iterate instances when class changes)
%%
%% Each instance is registered as a `{Class, Pid}` entry in an ETS bag table.
%% A monitor process automatically cleans up entries when instances terminate.
%%
%% @see docs/beamtalk-object-model.md Part 5 "Instance Tracking"
-module(beamtalk_object_instances).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register/2,
    unregister/2,
    all/1,
    count/1,
    each/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TABLE, beamtalk_instance_registry).

-type class_name() :: atom().

-record(state, {
    monitors = #{} :: #{{class_name(), pid()} => reference()}
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the instance registry server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register an instance of a class.
%% The instance will be automatically unregistered when the process terminates.
-spec register(class_name(), pid()) -> ok.
register(Class, Pid) when is_atom(Class), is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Class, Pid}).

%% @doc Manually unregister an instance of a class.
-spec unregister(class_name(), pid()) -> ok.
unregister(Class, Pid) when is_atom(Class), is_pid(Pid) ->
    gen_server:call(?MODULE, {unregister, Class, Pid}).

%% @doc Get all live instances of a class.
%% Filters out dead processes.
%%
%% NOTE: There is a race condition where a process may die between the alive
%% check and when the caller uses the pid. Callers must handle `noproc` errors.
-spec all(class_name()) -> [pid()].
all(Class) when is_atom(Class) ->
    [Pid || {_, Pid} <- ets:lookup(?TABLE, Class), erlang:is_process_alive(Pid)].

%% @doc Count the live instances of a class.
%% More efficient than `length(all(Class))` for large instance counts.
-spec count(class_name()) -> non_neg_integer().
count(Class) when is_atom(Class) ->
    lists:foldl(
        fun({_, Pid}, Acc) ->
            case erlang:is_process_alive(Pid) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        ets:lookup(?TABLE, Class)
    ).

%% @doc Iterate all live instances of a class with a function.
%% The function is called as `Fun(Pid)` for each instance.
-spec each(class_name(), fun((pid()) -> any())) -> ok.
each(Class, Fun) when is_atom(Class), is_function(Fun, 1) ->
    lists:foreach(Fun, all(Class)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    %% Create the ETS table owned by this process so it survives
    %% until the gen_server terminates
    ?TABLE = ets:new(?TABLE, [named_table, public, bag]),
    {ok, #state{}}.

%% @private
handle_call({register, Class, Pid}, _From, State) ->
    Key = {Class, Pid},
    case maps:is_key(Key, State#state.monitors) of
        true ->
            %% Already registered
            {reply, ok, State};
        false ->
            %% Insert into ETS
            ets:insert(?TABLE, {Class, Pid}),
            %% Monitor the process for auto-cleanup
            Ref = erlang:monitor(process, Pid),
            NewMonitors = maps:put(Key, Ref, State#state.monitors),
            {reply, ok, State#state{monitors = NewMonitors}}
    end;

handle_call({unregister, Class, Pid}, _From, State) ->
    Key = {Class, Pid},
    case maps:take(Key, State#state.monitors) of
        {Ref, NewMonitors} ->
            erlang:demonitor(Ref, [flush]),
            ets:delete_object(?TABLE, {Class, Pid}),
            {reply, ok, State#state{monitors = NewMonitors}};
        error ->
            %% Not registered, but that's ok
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Find which classes this Pid was registered under
    %% This is more efficient than scanning all monitors when there are many instances
    Matches = ets:match(?TABLE, {'$1', Pid}),
    Classes = [Class || [Class] <- Matches],
    
    %% Remove from ETS
    lists:foreach(fun(Class) ->
        ets:delete_object(?TABLE, {Class, Pid})
    end, Classes),
    
    %% Remove from monitors map
    NewMonitors = lists:foldl(
        fun(Class, Monitors) ->
            maps:remove({Class, Pid}, Monitors)
        end,
        State#state.monitors,
        Classes
    ),
    {noreply, State#state{monitors = NewMonitors}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    %% ETS table is owned by this process, will be deleted automatically
    ok.

%% @private
code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).
