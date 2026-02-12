%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Module management for Beamtalk REPL
%%%
%%% Tracks loaded modules, their source files, load times, and actor usage.
%%% Provides utilities for listing, unloading, and reloading modules.

-module(beamtalk_repl_modules).

-export([new/0, add_module/3, remove_module/2, get_module_info/2,
         list_modules/1, module_exists/2, get_actor_count/3,
         format_module_info/2]).

-export_type([module_tracker/0, module_info/0]).

-record(module_info, {
    name :: atom(),
    source_file :: string() | undefined,
    load_time :: integer(),
    actor_count :: non_neg_integer()
}).

-opaque module_info() :: #module_info{}.
-type module_tracker() :: #{atom() => module_info()}.

%% @doc Create a new empty module tracker.
-spec new() -> module_tracker().
new() ->
    #{}.

%% @doc Add a module to the tracker.
%% Records the module name, source file path, and load timestamp.
-spec add_module(atom(), string() | undefined, module_tracker()) -> module_tracker().
add_module(ModuleName, SourceFile, Tracker) ->
    Info = #module_info{
        name = ModuleName,
        source_file = SourceFile,
        load_time = erlang:system_time(second),
        actor_count = 0
    },
    Tracker#{ModuleName => Info}.

%% @doc Remove a module from the tracker.
-spec remove_module(atom(), module_tracker()) -> module_tracker().
remove_module(ModuleName, Tracker) ->
    maps:remove(ModuleName, Tracker).

%% @doc Get information about a specific module.
-spec get_module_info(atom(), module_tracker()) -> {ok, module_info()} | {error, not_found}.
get_module_info(ModuleName, Tracker) ->
    case maps:find(ModuleName, Tracker) of
        {ok, Info} -> {ok, Info};
        error -> {error, not_found}
    end.

%% @doc List all tracked modules with their information.
%% Optionally updates actor counts from the actor registry.
-spec list_modules(module_tracker()) -> [{atom(), module_info()}].
list_modules(Tracker) ->
    maps:to_list(Tracker).

%% @doc Check if a module is being tracked.
-spec module_exists(atom(), module_tracker()) -> boolean().
module_exists(ModuleName, Tracker) ->
    maps:is_key(ModuleName, Tracker).

%% @doc Get the current actor count for a module.
%% Queries the actor registry to count actors using this module.
-spec get_actor_count(atom(), pid() | undefined, module_tracker()) -> non_neg_integer().
get_actor_count(ModuleName, RegistryPid, Tracker) ->
    case {maps:find(ModuleName, Tracker), RegistryPid} of
        {{ok, _Info}, Pid} when is_pid(Pid) ->
            %% Query actor registry for count
            try
                case beamtalk_repl_actors:count_actors_for_module(Pid, ModuleName) of
                    {ok, Count} -> Count;
                    {error, _} -> 0
                end
            catch
                exit:{noproc, _} -> 0;
                exit:{timeout, _} -> 0;
                _:_ -> 0
            end;
        _ ->
            0
    end.

%% @doc Format module information for display.
%% Returns a map with displayable fields.
-spec format_module_info(module_info(), non_neg_integer()) -> map().
format_module_info(#module_info{name = Name, source_file = Source, load_time = LoadTime}, ActorCount) ->
    Now = erlang:system_time(second),
    ElapsedSecs = Now - LoadTime,
    TimeAgo = format_time_ago(ElapsedSecs),
    
    #{
        name => atom_to_binary(Name, utf8),
        source_file => case Source of
            undefined -> "unknown";
            Path -> Path
        end,
        actor_count => ActorCount,
        load_time => LoadTime,
        time_ago => TimeAgo
    }.

%% @private
%% Format elapsed time as human-readable string.
-spec format_time_ago(integer()) -> string().
format_time_ago(Secs) when Secs < 60 ->
    io_lib:format("~ps ago", [Secs]);
format_time_ago(Secs) when Secs < 3600 ->
    Mins = Secs div 60,
    io_lib:format("~pm ago", [Mins]);
format_time_ago(Secs) when Secs < 86400 ->
    Hours = Secs div 3600,
    io_lib:format("~ph ago", [Hours]);
format_time_ago(Secs) ->
    Days = Secs div 86400,
    io_lib:format("~pd ago", [Days]).
