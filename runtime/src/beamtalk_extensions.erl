%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Extension method registry for Beamtalk primitives.
%%%
%%% This module provides a Pharo-style extension registry that allows libraries
%%% to add methods to existing classes (especially primitives like Integer, String).
%%% Extensions use a global namespace with conflict tracking.
%%%
%%% ## Design Philosophy
%%%
%%% Following Pharo's proven approach:
%%% - Global namespace (no import declarations needed)
%%% - Last-writer-wins conflict resolution
%%% - Conflicts are logged and visible via tooling
%%% - Provenance tracking (which library registered each method)
%%%
%%% ## ETS Table Structure
%%%
%%% Table: `beamtalk_extensions`
%%% - Type: set (unique keys)
%%% - Key: `{Class, Selector}`
%%% - Value: `{Class, Selector, Fun, Owner}`
%%%
%%% Conflicts table: `beamtalk_extension_conflicts`
%%% - Type: bag (multiple values per key)
%%% - Key: `{Class, Selector}`
%%% - Value: `{Class, Selector, Owner, Timestamp}`
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Register an extension
%%% Fun = fun(Args, Value) -> ... end,
%%% beamtalk_extensions:register('String', 'json', Fun, mylib).
%%%
%%% %% Lookup an extension
%%% case beamtalk_extensions:lookup('String', 'json') of
%%%     {ok, Fun, Owner} -> Fun([], <<"hello">>);
%%%     not_found -> error(does_not_understand)
%%% end.
%%%
%%% %% List all extensions on a class
%%% beamtalk_extensions:list('String').
%%% % => [{json, mylib}, {trim, stdlib}, ...]
%%%
%%% %% Show conflicting registrations
%%% beamtalk_extensions:conflicts().
%%% % => [{'String', json, [mylib, otherlib]}]
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section "Extension Registry Design"

-module(beamtalk_extensions).
-export([
    init/0,
    register/4,
    lookup/2,
    list/1,
    conflicts/0,
    has/2
]).

-include_lib("kernel/include/logger.hrl").

-define(EXTENSIONS_TABLE, beamtalk_extensions).
-define(CONFLICTS_TABLE, beamtalk_extension_conflicts).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Initialize the extension registry ETS tables.
%%
%% Creates two tables:
%% - beamtalk_extensions: Current method registrations
%% - beamtalk_extension_conflicts: History of conflicting registrations
%%
%% This should be called once during application startup.
-spec init() -> ok.
init() ->
    %% Create main registry table
    case ets:info(?EXTENSIONS_TABLE) of
        undefined ->
            ets:new(?EXTENSIONS_TABLE, [set, public, named_table, {read_concurrency, true}]);
        _ ->
            ok  % Already exists
    end,
    
    %% Create conflicts tracking table
    case ets:info(?CONFLICTS_TABLE) of
        undefined ->
            ets:new(?CONFLICTS_TABLE, [bag, public, named_table, {read_concurrency, true}]);
        _ ->
            ok  % Already exists
    end,
    
    ok.

%% @doc Register an extension method on a class.
%%
%% Tracks the owner (library/package name) for provenance.
%% If a method is already registered by a different owner, logs a warning
%% and overwrites (last-writer-wins policy).
%%
%% Parameters:
%% - Class: The class atom (e.g., 'Integer', 'String')
%% - Selector: The method selector atom (e.g., 'json', 'trim')
%% - Fun: The method function (signature: fun(Args, Value) -> Result)
%% - Owner: The registering library/package atom (e.g., 'mylib', 'stdlib')
%%
%% Examples:
%% ```
%% JsonFun = fun([], Str) -> jsx:encode(Str) end,
%% register('String', 'json', JsonFun, mylib).
%% ```
-spec register(atom(), atom(), function(), atom()) -> ok.
register(Class, Selector, Fun, Owner) when is_atom(Class), is_atom(Selector), 
                                            is_function(Fun), is_atom(Owner) ->
    Key = {Class, Selector},
    
    %% Check for existing registration
    case ets:lookup(?EXTENSIONS_TABLE, Key) of
        [] ->
            %% New registration
            ets:insert(?EXTENSIONS_TABLE, {Key, Fun, Owner}),
            ok;
        [{Key, _OldFun, OldOwner}] when OldOwner =:= Owner ->
            %% Same owner updating - no conflict
            ets:insert(?EXTENSIONS_TABLE, {Key, Fun, Owner}),
            ok;
        [{Key, _OldFun, OldOwner}] ->
            %% Conflict: different owner
            ?LOG_WARNING("Extension conflict: '~p' on '~p' (from '~p') overwritten by '~p'",
                        [Selector, Class, OldOwner, Owner]),
            
            %% Record conflict for tooling - record BOTH owners
            Timestamp = erlang:system_time(millisecond),
            ets:insert(?CONFLICTS_TABLE, {Key, OldOwner, Timestamp}),
            ets:insert(?CONFLICTS_TABLE, {Key, Owner, Timestamp + 1}),
            
            %% Overwrite (last-writer-wins)
            ets:insert(?EXTENSIONS_TABLE, {Key, Fun, Owner}),
            ok
    end.

%% @doc Lookup an extension method.
%%
%% Returns the currently registered function and its owner, or not_found.
%%
%% Examples:
%% ```
%% case lookup('String', 'json') of
%%     {ok, Fun, mylib} -> Fun([], <<"hello">>);
%%     not_found -> error(does_not_understand)
%% end.
%% ```
-spec lookup(atom(), atom()) -> {ok, function(), atom()} | not_found.
lookup(Class, Selector) when is_atom(Class), is_atom(Selector) ->
    Key = {Class, Selector},
    case ets:lookup(?EXTENSIONS_TABLE, Key) of
        [{Key, Fun, Owner}] -> {ok, Fun, Owner};
        [] -> not_found
    end.

%% @doc List all extensions registered on a class.
%%
%% Returns a list of {Selector, Owner} tuples for all methods
%% registered on the given class.
%%
%% Examples:
%% ```
%% list('String').
%% % => [{json, mylib}, {trim, stdlib}, {camelize, mylib}]
%% ```
-spec list(atom()) -> [{atom(), atom()}].
list(Class) when is_atom(Class) ->
    MatchPattern = {{{Class, '$1'}, '_', '$2'}},
    ets:select(?EXTENSIONS_TABLE, [{MatchPattern, [], [{{'$1', '$2'}}]}]).

%% @doc Show all methods that have been registered by multiple owners.
%%
%% Returns a list of {Class, Selector, [Owner1, Owner2, ...]} tuples
%% for methods that have conflicts in the history.
%%
%% Examples:
%% ```
%% conflicts().
%% % => [{'String', json, [mylib, otherlib]}, {'Integer', parse, [lib1, lib2]}]
%% ```
-spec conflicts() -> [{atom(), atom(), [atom()]}].
conflicts() ->
    %% Get all conflict records
    AllConflicts = ets:tab2list(?CONFLICTS_TABLE),
    
    %% Group by {Class, Selector}
    Grouped = group_conflicts(AllConflicts),
    
    %% Convert to result format
    [{Class, Selector, Owners} || {{Class, Selector}, Owners} <- Grouped, length(Owners) > 1].

%% @doc Check if an extension exists for a class and selector.
%%
%% Examples:
%% ```
%% has('String', 'json').    % => true
%% has('String', 'unknown'). % => false
%% ```
-spec has(atom(), atom()) -> boolean().
has(Class, Selector) when is_atom(Class), is_atom(Selector) ->
    Key = {Class, Selector},
    ets:member(?EXTENSIONS_TABLE, Key).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private Group conflict records by class and selector
group_conflicts(Conflicts) ->
    Dict = lists:foldl(
        fun({{Class, Selector}, Owner, _Timestamp}, Acc) ->
            Key = {Class, Selector},
            maps:update_with(Key, fun(Owners) -> lists:usort([Owner | Owners]) end, [Owner], Acc)
        end,
        #{},
        Conflicts
    ),
    maps:to_list(Dict).
