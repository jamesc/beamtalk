%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace actor - actor introspection singleton.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This gen_server implements the Workspace actor that provides
%%% actor introspection API for the live environment. It exposes
%%% the actor registry as Beamtalk-level methods:
%%% - `actors` - List all live actors as usable object references
%%% - `actorAt:` - Look up a specific actor by pid string
%%% - `actorsOf:` - Get all actors of a given class
%%%
%%% This is the Beamtalk equivalent of Smalltalk's `Process allInstances` -
%%% introspecting the live environment. Workspace is the right home because
%%% actors are workspace-scoped (per ADR 0004).
%%%
%%% ## Message Protocol
%%%
%%% Workspace responds to these selectors:
%%% - `actors` - Returns list of #beamtalk_object{} for all live actors
%%% - `actorAt:` - Returns #beamtalk_object{} for actor at pid, or nil
%%% - `actorsOf:` - Returns list of #beamtalk_object{} for class
%%%
%%% ## Example Usage
%%%
%%% ```beamtalk
%%% Workspace actors
%%% // => [#Actor<Counter,0.132.0>, #Actor<Counter,0.152.0>]
%%%
%%% Workspace actorAt: '<0.132.0>'
%%% // => #Actor<Counter,0.132.0>
%%%
%%% Workspace actorsOf: Counter
%%% // => [#Actor<Counter,0.132.0>]
%%% ```

-module(beamtalk_workspace_actor).
-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% API
-export([
    start_link/0,
    start_link_singleton/0,
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

-record(workspace_actor_state, {
    is_singleton :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the Workspace actor (non-singleton, for testing).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [false], []).

%% @doc Start a named Workspace singleton for workspace use.
%% Registers as 'Workspace' via register/2 and persistent_term binding.
-spec start_link_singleton() -> {ok, pid()} | {error, term()}.
start_link_singleton() ->
    gen_server:start_link({local, 'Workspace'}, ?MODULE, [true], []).

%% @doc Check if Workspace supports a given method selector.
-spec has_method(selector()) -> boolean().
has_method(actors) -> true;
has_method('actorAt:') -> true;
has_method('actorsOf:') -> true;
has_method(_) -> false.

%% @doc Return class registration metadata for Workspace.
-spec class_info() -> map().
class_info() ->
    #{
        name => 'Workspace',
        module => ?MODULE,
        superclass => 'Actor',
        instance_methods => #{
            actors => #{arity => 0},
            'actorAt:' => #{arity => 1},
            'actorsOf:' => #{arity => 1}
        },
        class_methods => #{},
        instance_variables => []
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the Workspace actor state.
-spec init([boolean()]) -> {ok, #workspace_actor_state{}}.
init([IsSingleton]) ->
    case IsSingleton of
        true ->
            persistent_term:put({beamtalk_binding, 'Workspace'},
                {beamtalk_object, 'Workspace', ?MODULE, self()}),
            %% Self-register class since we're in beamtalk_workspace app
            %% (beamtalk_stdlib in beamtalk_runtime can't reference us)
            register_class();
        false ->
            ok
    end,
    {ok, #workspace_actor_state{is_singleton = IsSingleton}}.

%% @doc Handle synchronous method calls.
-spec handle_call(term(), {pid(), term()}, #workspace_actor_state{}) ->
    {reply, term(), #workspace_actor_state{}}.

handle_call({actors, []}, _From, State) ->
    Result = handle_actors(),
    {reply, Result, State};

handle_call({'actorAt:', [PidStr]}, _From, State) ->
    Result = handle_actor_at(PidStr),
    {reply, Result, State};

handle_call({'actorsOf:', [ClassName]}, _From, State) ->
    Result = handle_actors_of(ClassName),
    {reply, Result, State};

handle_call({UnknownSelector, _Args}, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Workspace'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Supported methods: actors, actorAt:, actorsOf:">>),
    {reply, {error, Error2}, State};

handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Workspace'),
    Error1 = beamtalk_error:with_hint(Error0, <<"Expected {Selector, Args} format">>),
    Error2 = beamtalk_error:with_details(Error1, #{request => Request}),
    {reply, {error, Error2}, State}.

%% @doc Handle asynchronous messages.
%% Workspace binding dispatch uses beamtalk_actor:async_send/4 which
%% sends {Selector, Args, FuturePid} as a cast.
-spec handle_cast(term(), #workspace_actor_state{}) -> {noreply, #workspace_actor_state{}}.

handle_cast({actors, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_actors(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};

handle_cast({'actorAt:', [PidStr], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_actor_at(PidStr),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};

handle_cast({'actorsOf:', [ClassName], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_actors_of(ClassName),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};

handle_cast({UnknownSelector, _Args, FuturePid}, State) when is_pid(FuturePid), is_atom(UnknownSelector) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Workspace'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Supported methods: actors, actorAt:, actorsOf:">>),
    beamtalk_future:reject(FuturePid, Error2),
    {noreply, State};

handle_cast(Msg, State) ->
    logger:warning("Workspace received unexpected cast", #{message => Msg}),
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #workspace_actor_state{}) -> {noreply, #workspace_actor_state{}}.
handle_info(Info, State) ->
    logger:debug("Workspace received info", #{info => Info}),
    {noreply, State}.

%% @doc Handle process termination.
-spec terminate(term(), #workspace_actor_state{}) -> ok.
terminate(_Reason, #workspace_actor_state{is_singleton = true}) ->
    persistent_term:erase({beamtalk_binding, 'Workspace'}),
    ok;
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change during hot reload.
-spec code_change(term(), #workspace_actor_state{}, term()) -> {ok, #workspace_actor_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal method implementations
%%====================================================================

%% @doc Get all live actors as beamtalk_object references.
-spec handle_actors() -> [tuple()].
handle_actors() ->
    case whereis(beamtalk_actor_registry) of
        undefined -> [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(fun wrap_actor/1, Actors)
    end.

%% @doc Look up a specific actor by pid string.
%% Returns beamtalk_object or nil if not found.
-spec handle_actor_at(binary() | list()) -> tuple() | nil.
handle_actor_at(PidStr) when is_binary(PidStr) ->
    handle_actor_at(binary_to_list(PidStr));
handle_actor_at(PidStr) when is_list(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case whereis(beamtalk_actor_registry) of
            undefined -> nil;
            RegistryPid ->
                case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                    {ok, Metadata} ->
                        case wrap_actor(Metadata) of
                            {true, Obj} -> Obj;
                            false -> nil
                        end;
                    {error, not_found} -> nil
                end
        end
    catch
        error:badarg -> nil
    end;
handle_actor_at(_) -> nil.

%% @doc Get all actors of a given class.
%% Accepts a class name atom, binary, or a class object tuple.
-spec handle_actors_of(atom() | binary() | tuple()) -> [tuple()].
handle_actors_of({beamtalk_object, _ClassTag, _Module, ClassPid}) when is_pid(ClassPid) ->
    %% Class object reference — extract the actual class name
    try
        ClassName = beamtalk_object_class:class_name(ClassPid),
        handle_actors_of(ClassName)
    catch
        _:_ -> []
    end;
handle_actors_of(ClassName) when is_binary(ClassName) ->
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_actors_of(ClassAtom)
    catch
        error:badarg -> []
    end;
handle_actors_of(ClassName) when is_atom(ClassName) ->
    case whereis(beamtalk_actor_registry) of
        undefined -> [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(
                fun(#{class := Class} = Meta) when Class =:= ClassName ->
                    wrap_actor(Meta);
                   (_) ->
                    false
                end,
                Actors
            )
    end;
handle_actors_of(_) -> [].

%% @doc Wrap actor metadata into a #beamtalk_object{} tuple.
%% Filters out dead actors.
-spec wrap_actor(beamtalk_repl_actors:actor_metadata()) -> {true, tuple()} | false.
wrap_actor(#{pid := Pid, class := Class, module := Module}) ->
    case is_process_alive(Pid) of
        true ->
            {true, {beamtalk_object, Class, Module, Pid}};
        false ->
            false
    end.

%% @doc Register the Workspace class with the class registry.
%% Uses start_link which creates a link — wrapped in try/catch to avoid
%% taking down the Workspace actor if class registration fails.
-spec register_class() -> ok.
register_class() ->
    try
        ClassInfo = class_info(),
        case beamtalk_object_class:whereis_class('Workspace') of
            undefined ->
                case beamtalk_object_class:start_link('Workspace', ClassInfo) of
                    {ok, _Pid} ->
                        logger:debug("Registered Workspace class", #{});
                    {error, RegReason} ->
                        logger:warning("Failed to register Workspace class", #{reason => RegReason})
                end;
            _Pid ->
                ok
        end
    catch
        Kind:CrashReason ->
            logger:warning("Workspace class registration failed", #{kind => Kind, reason => CrashReason})
    end,
    ok.
