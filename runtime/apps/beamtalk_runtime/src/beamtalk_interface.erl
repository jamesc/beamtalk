%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc BeamtalkInterface actor - Beamtalk system introspection singleton.
%%%
%%% **DDD Context:** Runtime
%%%
%%% This gen_server implements the BeamtalkInterface actor that backs the
%%% `Beamtalk` global object. It provides system-wide introspection:
%%% - Class registry access (allClasses, classNamed:)
%%% - Workspace globals (globals) — immutable snapshot of the class registry
%%% - Version information (version)
%%%
%%% This is Phase 1 of ADR 0010: Global Objects and Singleton Dispatch.
%%% The actor coexists with the current beamtalk_stdlib.erl dispatch.
%%% Migration to use this actor will happen in later phases.
%%%
%%% ## Message Protocol
%%%
%%% BeamtalkInterface responds to these selectors:
%%% - `allClasses` - Returns list of all class names
%%% - `classNamed:` - Returns class info or error for given name
%%% - `globals` - Returns class registry snapshot as a Dictionary
%%% - `version` - Returns Beamtalk version string
%%%
%%% All methods are synchronous (gen_server:call) as they return values
%%% that callers need immediately.
%%%
%%% ## Example Usage
%%%
%%% ```erlang
%%% %% Start the BeamtalkInterface actor
%%% {ok, Pid} = beamtalk_interface:start_link(),
%%%
%%% %% Query all classes
%%% Classes = gen_server:call(Pid, {allClasses, []}),
%%%
%%% %% Look up a specific class
%%% ClassPid = gen_server:call(Pid, {'classNamed:', [<<"Counter">>]}),
%%%
%%% %% Get workspace globals
%%% Globals = gen_server:call(Pid, {globals, []}),
%%%
%%% %% Get version
%%% Version = gen_server:call(Pid, {version, []}).
%%% ```
-module(beamtalk_interface).
-behaviour(gen_server).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    start_link/2,
    has_method/1,
    class_info/0
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

-type selector() :: atom().

-record(beamtalk_interface_state, {
    version :: binary(),
    % Workspace name for future phases
    workspace :: atom() | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the BeamtalkInterface actor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the BeamtalkInterface actor with options.
%%
%% Options:
%%   {workspace, atom()} - Workspace name (for future phases)
%%   {version, binary()} - Version string (defaults to "0.1.0-dev")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a named BeamtalkInterface for workspace use.
%% Registers with the given name via gen_server name registration.
-spec start_link({local, atom()}, proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(ServerName, Opts) ->
    gen_server:start_link(ServerName, ?MODULE, Opts, []).

%% @doc Check if BeamtalkInterface supports a given method selector.
%%
%% Used by dispatch compatibility layer. Returns true if the selector
%% corresponds to one of the supported methods.
-spec has_method(selector()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method(version) -> true;
has_method(_) -> false.

%% @doc Return class registration metadata for BeamtalkInterface.
%%
%% Used by beamtalk_stdlib to register this singleton's class.
%% Single source of truth for class name, superclass, and method table.
-spec class_info() -> map().
class_info() ->
    #{
        name => 'BeamtalkInterface',
        module => ?MODULE,
        superclass => 'Actor',
        instance_methods => #{
            allClasses => #{arity => 0},
            'classNamed:' => #{arity => 1},
            globals => #{arity => 0},
            version => #{arity => 0}
        },
        class_methods => #{},
        fields => []
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the BeamtalkInterface actor state.
-spec init(proplists:proplist()) -> {ok, #beamtalk_interface_state{}}.
init(Opts) ->
    VersionDefault =
        case application:get_key(beamtalk_runtime, vsn) of
            {ok, Vsn} -> list_to_binary(Vsn);
            _ -> <<"0.1.0">>
        end,
    Version = proplists:get_value(version, Opts, VersionDefault),
    Workspace = proplists:get_value(workspace, Opts, undefined),

    State = #beamtalk_interface_state{
        version = Version,
        workspace = Workspace
    },
    {ok, State}.

%% @doc Handle synchronous method calls.
%%
%% Dispatches to the appropriate method implementation based on selector.
-spec handle_call(term(), {pid(), term()}, #beamtalk_interface_state{}) ->
    {reply, term(), #beamtalk_interface_state{}}.

%% allClasses - Returns list of all class names
handle_call({allClasses, []}, _From, State) ->
    Classes = handle_all_classes(),
    {reply, Classes, State};
%% classNamed: - Returns class pid or error for given class name
handle_call({'classNamed:', [ClassName]}, _From, State) ->
    Result = handle_class_named(ClassName),
    {reply, Result, State};
%% globals - Returns class registry snapshot as a Dictionary
handle_call({globals, []}, _From, State) ->
    Globals = handle_globals(),
    {reply, Globals, State};
%% version - Returns Beamtalk version string
handle_call({version, []}, _From, State) ->
    Version = State#beamtalk_interface_state.version,
    {reply, Version, State};
%% Unknown selector - return structured error
handle_call({UnknownSelector, _Args}, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"To list available selectors, use: Beamtalk methods">>
    ),
    {reply, {error, Error2}, State};
%% Unknown call format
handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_hint(Error0, <<"Expected {Selector, Args} format">>),
    Error2 = beamtalk_error:with_details(Error1, #{request => Request}),
    {reply, {error, Error2}, State}.

%% @doc Handle asynchronous messages.
%%
%% BT-374: Workspace binding dispatch uses beamtalk_actor:async_send/4 which
%% sends {Selector, Args, FuturePid} as a cast. We dispatch to the same
%% method implementations as handle_call and resolve the Future with the result.
-spec handle_cast(term(), #beamtalk_interface_state{}) -> {noreply, #beamtalk_interface_state{}}.
%% Actor protocol: {Selector, Args, FuturePid} from beamtalk_actor:async_send/4
handle_cast({allClasses, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_all_classes(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({'classNamed:', [ClassName], FuturePid}, State) when is_pid(FuturePid) ->
    case handle_class_named(ClassName) of
        {error, Err} ->
            beamtalk_future:reject(FuturePid, Err);
        Result ->
            beamtalk_future:resolve(FuturePid, Result)
    end,
    {noreply, State};
handle_cast({globals, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_globals(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({version, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = State#beamtalk_interface_state.version,
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({UnknownSelector, _Args, FuturePid}, State) when
    is_pid(FuturePid), is_atom(UnknownSelector)
->
    Error0 = beamtalk_error:new(does_not_understand, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"To list available selectors, use: Beamtalk methods">>
    ),
    beamtalk_future:reject(FuturePid, Error2),
    {noreply, State};
handle_cast(Msg, State) ->
    ?LOG_WARNING("BeamtalkInterface received unexpected cast", #{message => Msg}),
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #beamtalk_interface_state{}) -> {noreply, #beamtalk_interface_state{}}.
handle_info(Info, State) ->
    ?LOG_DEBUG("BeamtalkInterface received info", #{info => Info}),
    {noreply, State}.

%% @doc Handle process termination.
-spec terminate(term(), #beamtalk_interface_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change during hot reload.
-spec code_change(term(), #beamtalk_interface_state{}, term()) ->
    {ok, #beamtalk_interface_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal method implementations
%%====================================================================

%% @doc Get all registered class names.
%%
%% Delegates to live_class_entries/0 to get live class pid entries,
%% then extracts just the class names.
-spec handle_all_classes() -> [atom()].
handle_all_classes() ->
    [Name || {Name, _Mod, _Pid} <- live_class_entries()].

%% @doc Look up a class by name.
%%
%% Delegates to beamtalk_class_registry:whereis_class/1.
%% Returns the class as a beamtalk_object record (for dispatch), or nil if not found.
%% BT-246: Wraps result in {beamtalk_object, ClassTag, ModuleName, Pid} tuple
%% where ClassTag is 'ClassName class' (e.g., 'Point class') for is_class_object detection.
-spec handle_class_named(binary() | atom()) -> tuple() | 'nil' | {error, #beamtalk_error{}}.
handle_class_named(ClassName) when is_binary(ClassName) ->
    %% Use binary_to_existing_atom to avoid creating atoms at runtime
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_class_named(ClassAtom)
    catch
        error:badarg ->
            nil
    end;
handle_class_named(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            nil;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            %% BT-246: Use class object tag for is_class_object detection
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end;
handle_class_named(_ClassName) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_selector(Error0, 'classNamed:'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"classNamed: expects an atom or binary class name">>
    ),
    {error, Error2}.

%% @doc Get workspace global bindings as an immutable snapshot of the class registry.
%%
%% Returns a map from binary class names to class object references.
%% This provides read-only access to the class namespace at the moment of the call.
-spec handle_globals() -> map().
handle_globals() ->
    lists:foldl(
        fun({Name, ModuleName, Pid}, Acc) ->
            ClassTag = beamtalk_class_registry:class_object_tag(Name),
            ClassObj = {beamtalk_object, ClassTag, ModuleName, Pid},
            maps:put(Name, ClassObj, Acc)
        end,
        #{},
        live_class_entries()
    ).

%% @private Fetch all live class entries from the registry.
%%
%% Returns a list of {Name, ModuleName, Pid} tuples for all registered class
%% processes that are still alive. Dead processes (noproc, timeout) are silently
%% filtered out. Returns [] if the pg process group is not running.
-type class_entry() :: {atom(), module(), pid()}.
-spec live_class_entries() -> [class_entry()].
live_class_entries() ->
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    Name = beamtalk_object_class:class_name(Pid),
                    Mod = beamtalk_object_class:module_name(Pid),
                    {true, {Name, Mod, Pid}}
                catch
                    exit:{noproc, _} -> false;
                    exit:{timeout, _} -> false
                end
            end,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            ?LOG_WARNING("pg not started when fetching class entries", #{module => ?MODULE}),
            []
    end.
