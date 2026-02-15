%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc SystemDictionary actor - Beamtalk system introspection singleton.
%%%
%%% **DDD Context:** Runtime
%%%
%%% This gen_server implements the SystemDictionary actor that backs the
%%% `Beamtalk` global object. It provides system-wide introspection:
%%% - Class registry access (allClasses, classNamed:)
%%% - Workspace globals (globals)
%%% - Version information (version)
%%%
%%% This is Phase 1 of ADR 0010: Global Objects and Singleton Dispatch.
%%% The actor coexists with the current beamtalk_stdlib.erl dispatch.
%%% Migration to use this actor will happen in later phases.
%%%
%%% ## Message Protocol
%%%
%%% SystemDictionary responds to these selectors:
%%% - `allClasses` - Returns list of all class names
%%% - `classNamed:` - Returns class info or error for given name
%%% - `globals` - Returns workspace bindings map
%%% - `version` - Returns Beamtalk version string
%%%
%%% All methods are synchronous (gen_server:call) as they return values
%%% that callers need immediately.
%%%
%%% ## Example Usage
%%%
%%% ```erlang
%%% %% Start the SystemDictionary actor
%%% {ok, Pid} = beamtalk_system_dictionary:start_link(),
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
-module(beamtalk_system_dictionary).
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

-record(system_dict_state, {
    version :: binary(),
    workspace :: atom() | undefined  % Workspace name for future phases
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the SystemDictionary actor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the SystemDictionary actor with options.
%%
%% Options:
%%   {workspace, atom()} - Workspace name (for future phases)
%%   {version, binary()} - Version string (defaults to "0.1.0-dev")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a named SystemDictionary for workspace use.
%% Registers with the given name via gen_server name registration.
-spec start_link({local, atom()}, proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(ServerName, Opts) ->
    gen_server:start_link(ServerName, ?MODULE, Opts, []).

%% @doc Check if SystemDictionary supports a given method selector.
%%
%% Used by dispatch compatibility layer. Returns true if the selector
%% corresponds to one of the supported methods.
-spec has_method(selector()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method(version) -> true;
has_method(_) -> false.

%% @doc Return class registration metadata for SystemDictionary.
%%
%% Used by beamtalk_stdlib to register this singleton's class.
%% Single source of truth for class name, superclass, and method table.
-spec class_info() -> map().
class_info() ->
    #{
        name => 'SystemDictionary',
        module => ?MODULE,
        superclass => 'Actor',
        instance_methods => #{
            allClasses => #{arity => 0},
            'classNamed:' => #{arity => 1},
            globals => #{arity => 0},
            version => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the SystemDictionary actor state.
-spec init(proplists:proplist()) -> {ok, #system_dict_state{}}.
init(Opts) ->
    VersionDefault =
        case application:get_key(beamtalk_runtime, vsn) of
            {ok, Vsn} -> list_to_binary(Vsn);
            _ -> <<"0.1.0">>
        end,
    Version = proplists:get_value(version, Opts, VersionDefault),
    Workspace = proplists:get_value(workspace, Opts, undefined),
    
    State = #system_dict_state{
        version = Version,
        workspace = Workspace
    },
    {ok, State}.

%% @doc Handle synchronous method calls.
%%
%% Dispatches to the appropriate method implementation based on selector.
-spec handle_call(term(), {pid(), term()}, #system_dict_state{}) ->
    {reply, term(), #system_dict_state{}}.

%% allClasses - Returns list of all class names
handle_call({allClasses, []}, _From, State) ->
    Classes = handle_all_classes(),
    {reply, Classes, State};

%% classNamed: - Returns class pid or error for given class name
handle_call({'classNamed:', [ClassName]}, _From, State) ->
    Result = handle_class_named(ClassName),
    {reply, Result, State};

%% globals - Returns workspace bindings map
handle_call({globals, []}, _From, State) ->
    Globals = handle_globals(),
    {reply, Globals, State};

%% version - Returns Beamtalk version string
handle_call({version, []}, _From, State) ->
    Version = State#system_dict_state.version,
    {reply, Version, State};

%% Unknown selector - return structured error
handle_call({UnknownSelector, _Args}, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'SystemDictionary'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Supported methods: allClasses, classNamed:, globals, version">>),
    {reply, {error, Error2}, State};

%% Unknown call format
handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'SystemDictionary'),
    Error1 = beamtalk_error:with_hint(Error0, <<"Expected {Selector, Args} format">>),
    Error2 = beamtalk_error:with_details(Error1, #{request => Request}),
    {reply, {error, Error2}, State}.

%% @doc Handle asynchronous messages.
%%
%% BT-374: Workspace binding dispatch uses beamtalk_actor:async_send/4 which
%% sends {Selector, Args, FuturePid} as a cast. We dispatch to the same
%% method implementations as handle_call and resolve the Future with the result.
-spec handle_cast(term(), #system_dict_state{}) -> {noreply, #system_dict_state{}}.
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
    Result = State#system_dict_state.version,
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({UnknownSelector, _Args, FuturePid}, State) when is_pid(FuturePid), is_atom(UnknownSelector) ->
    Error0 = beamtalk_error:new(does_not_understand, 'SystemDictionary'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Supported methods: allClasses, classNamed:, globals, version">>),
    beamtalk_future:reject(FuturePid, Error2),
    {noreply, State};
handle_cast(Msg, State) ->
    ?LOG_WARNING("SystemDictionary received unexpected cast", #{message => Msg}),
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #system_dict_state{}) -> {noreply, #system_dict_state{}}.
handle_info(Info, State) ->
    ?LOG_DEBUG("SystemDictionary received info", #{info => Info}),
    {noreply, State}.

%% @doc Handle process termination.
-spec terminate(term(), #system_dict_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change during hot reload.
-spec code_change(term(), #system_dict_state{}, term()) -> {ok, #system_dict_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal method implementations
%%====================================================================

%% @doc Get all registered class names.
%%
%% Delegates to beamtalk_class_registry:all_classes/0 which returns a list
%% of class process pids, then extracts the class name from each pid.
%% Guards against pg not running and filters dead processes.
-spec handle_all_classes() -> [atom()].
handle_all_classes() ->
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    {true, beamtalk_object_class:class_name(Pid)}
                catch
                    exit:{noproc, _} -> false;
                    exit:{timeout, _} -> false
                end
            end,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            ?LOG_WARNING("pg not started when fetching all classes", #{module => ?MODULE}),
            []
    end.

%% @doc Look up a class by name.
%%
%% Delegates to beamtalk_class_registry:whereis_class/1.
%% Returns the class as a beamtalk_object record (for dispatch), or nil if not found.
%% BT-246: Wraps result in {beamtalk_object, ClassTag, ModuleName, Pid} tuple
%% where ClassTag is 'ClassName class' (e.g., 'Point class') for is_class_object detection.
-spec handle_class_named(binary() | atom()) -> tuple() | nil | {error, #beamtalk_error{}}.
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
    Error0 = beamtalk_error:new(type_error, 'SystemDictionary'),
    Error1 = beamtalk_error:with_selector(Error0, 'classNamed:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"classNamed: expects an atom or binary class name">>),
    {error, Error2}.

%% @doc Get workspace global bindings.
%%
%% Phase 1: Returns empty map as workspace bindings are not yet implemented.
%% Phase 2 (ADR 0010): Will query workspace state for injected bindings.
-spec handle_globals() -> map().
handle_globals() ->
    %% Phase 1: No workspace bindings yet
    %% Phase 2: Will query workspace state for bindings
    #{}.

