%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace interface - actor introspection singleton.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This gen_server implements all WorkspaceInterface methods as Erlang
%%% primitives. The Beamtalk source in stdlib/src/WorkspaceInterface.bt
%%% defines the class interface and documentation; this module provides
%%% the runtime implementation.
%%%
%%% ## Why Erlang primitives (not compiled Beamtalk dispatch)?
%%%
%%% The compiled bt@stdlib@workspace_interface:dispatch/4 internally calls
%%% beamtalk_actor:make_self(State) which uses self() — the calling process
%%% pid. Methods like actorsOf: call `self actors` which sends a message
%%% back to the gen_server. If dispatch/4 is called from the gen_server
%%% directly, this deadlocks. If called from a spawned process, make_self
%%% uses the wrong pid. Implementing methods in Erlang avoids both issues.
%%%
%%% ## Methods
%%%
%%% - `actors` - List all live actors as usable object references
%%% - `actorAt:` - Look up a specific actor by pid string
%%% - `classes` - List all loaded user classes (those with source files)
%%% - `load:` - Compile and load a .bt file
%%% - `actorsOf:` - Filter actors by class (walks inheritance)
%%% - `globals` - Dictionary snapshot of project namespace
%%% - `testClasses` - Filter classes by TestCase inheritance
%%% - `test` - Run all test classes via TestRunner
%%% - `test:` - Run a specific test class via TestRunner
%%%
%%% ## Example Usage
%%%
%%% ```beamtalk
%%% Workspace actors
%%% // => [#Actor<Counter,0.132.0>, #Actor<Counter,0.152.0>]
%%%
%%% Workspace actorAt: '<0.132.0>'
%%% // => #Actor<Counter,0.132.0>
%%% ```

-module(beamtalk_workspace_interface).

-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    has_method/1,
    class_info/0,
    dispatch/4,
    get_user_bindings/0,
    get_session_bindings/0
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

-record(workspace_interface_state, {
    user_bindings = #{} :: #{atom() => term()}
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the Workspace interface (non-singleton, for testing).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [false], []).

%% @doc Start a named Workspace interface for workspace use.
%% Registers with the given name via gen_server name registration.
-spec start_link({local, atom()}) -> {ok, pid()} | {error, term()}.
start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [true], []).

%% @doc Return workspace-level user bindings (for REPL session injection).
%% Called before each eval to merge workspace bindings into session bindings.
-spec get_user_bindings() -> #{atom() => term()}.
get_user_bindings() ->
    case whereis('Workspace') of
        undefined ->
            #{};
        Pid ->
            try
                gen_server:call(Pid, get_user_bindings, 1000)
            catch
                _:_ -> #{}
            end
    end.

%% @doc Return non-class workspace globals for session binding injection (BT-883).
%% Includes singletons (Transcript, Beamtalk, Workspace) and user-registered
%% bindings (bind:as:), but NOT class objects. Called by beamtalk_repl_shell
%% at session start to seed session bindings from Workspace globals.
-spec get_session_bindings() -> #{atom() => term()}.
get_session_bindings() ->
    case whereis('Workspace') of
        undefined ->
            #{};
        Pid ->
            try
                gen_server:call(Pid, get_session_bindings, 1000)
            catch
                _:_ -> #{}
            end
    end.

%% @doc Check if Workspace supports a given method selector.
-spec has_method(selector()) -> boolean().
has_method(actors) -> true;
has_method('actorAt:') -> true;
has_method(classes) -> true;
has_method('load:') -> true;
has_method('actorsOf:') -> true;
has_method(globals) -> true;
has_method(testClasses) -> true;
has_method(test) -> true;
has_method('test:') -> true;
has_method('bind:as:') -> true;
has_method('unbind:') -> true;
has_method(_) -> false.

%% @doc Return class registration metadata for Workspace.
-spec class_info() -> map().
class_info() ->
    #{
        name => 'WorkspaceInterface',
        module => ?MODULE,
        superclass => 'Actor',
        instance_methods => #{
            actors => #{arity => 0},
            'actorAt:' => #{arity => 1},
            classes => #{arity => 0},
            'load:' => #{arity => 1},
            'actorsOf:' => #{arity => 1},
            globals => #{arity => 0},
            testClasses => #{arity => 0},
            test => #{arity => 0},
            'test:' => #{arity => 1},
            'bind:as:' => #{arity => 2},
            'unbind:' => #{arity => 1}
        },
        class_methods => #{},
        fields => []
    }.

%%====================================================================
%% dispatch/4 — called by beamtalk_dispatch when walking the hierarchy
%%====================================================================

%% @doc Dispatch methods for WorkspaceInterface.
%% All methods are implemented as Erlang primitives.
-spec dispatch(atom(), list(), term(), map()) ->
    {reply, term(), map()} | {error, term(), map()}.
dispatch(actors, [], _Self, State) ->
    {reply, handle_actors(), State};
dispatch('actorAt:', [PidStr], _Self, State) ->
    {reply, handle_actor_at(PidStr), State};
dispatch(classes, [], _Self, State) ->
    {reply, handle_classes(), State};
dispatch('load:', [Path], _Self, State) ->
    {reply, handle_load(Path), State};
dispatch('actorsOf:', [AClass], _Self, State) ->
    {reply, handle_actors_of(AClass), State};
dispatch(globals, [], Self, State) ->
    GenServerPid = erlang:element(4, Self),
    UserBindings =
        try
            gen_server:call(GenServerPid, get_user_bindings, 1000)
        catch
            _:_ -> #{}
        end,
    {reply, handle_globals(GenServerPid, UserBindings), State};
dispatch(testClasses, [], _Self, State) ->
    {reply, handle_test_classes(), State};
dispatch(test, [], _Self, State) ->
    {reply, handle_test(), State};
dispatch('test:', [TestClass], _Self, State) ->
    {reply, handle_test_class(TestClass), State};
dispatch('bind:as:', [Value, Name], Self, State) ->
    GenServerPid = erlang:element(4, Self),
    case handle_bind(Value, Name, GenServerPid) of
        {error, Err} -> {error, Err, State};
        Result -> {reply, Result, State}
    end;
dispatch('unbind:', [Name], Self, State) ->
    GenServerPid = erlang:element(4, Self),
    case handle_unbind(Name, GenServerPid) of
        {error, Err} -> {error, Err, State};
        Result -> {reply, Result, State}
    end;
dispatch(Selector, _Args, _Self, State) ->
    Err0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    Err2 = beamtalk_error:with_hint(
        Err1, <<"To list available selectors, use: Workspace methods">>
    ),
    {error, Err2, State}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the Workspace interface state.
-spec init([boolean()]) -> {ok, #workspace_interface_state{}}.
init([true]) ->
    register_class(),
    {ok, #workspace_interface_state{}};
init([false]) ->
    {ok, #workspace_interface_state{}}.

%% @doc Handle synchronous method calls.
-spec handle_call(term(), {pid(), term()}, #workspace_interface_state{}) ->
    {reply, term(), #workspace_interface_state{}}
    | {noreply, #workspace_interface_state{}}.
handle_call({actors, []}, _From, State) ->
    {reply, handle_actors(), State};
handle_call({'actorAt:', [PidStr]}, _From, State) ->
    {reply, handle_actor_at(PidStr), State};
handle_call({classes, []}, _From, State) ->
    {reply, handle_classes(), State};
handle_call({'actorsOf:', [AClass]}, _From, State) ->
    {reply, handle_actors_of(AClass), State};
handle_call({testClasses, []}, _From, State) ->
    {reply, handle_test_classes(), State};
handle_call({globals, []}, From, State) ->
    %% globals may call handle_actors which is fast, but spawn to avoid
    %% any risk of deadlock from singleton resolution
    GenServerPid = self(),
    UserBindings = State#workspace_interface_state.user_bindings,
    spawn(fun() ->
        try handle_globals(GenServerPid, UserBindings) of
            Result -> gen_server:reply(From, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("globals handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                gen_server:reply(
                    From, {error, beamtalk_error:with_message(Err, <<"Internal error in globals">>)}
                )
        end
    end),
    {noreply, State};
handle_call({'bind:as:', [Value, Name]}, _From, State) ->
    case to_atom_name(Name) of
        {error, Err} ->
            {reply, {error, Err}, State};
        AtomName ->
            case check_bind_conflicts(AtomName) of
                ok ->
                    UserBindings = State#workspace_interface_state.user_bindings,
                    NewBindings = maps:put(AtomName, Value, UserBindings),
                    NewState = State#workspace_interface_state{user_bindings = NewBindings},
                    spawn(fun() -> maybe_warn_loaded_class(AtomName) end),
                    {reply, nil, NewState};
                {error, Err} ->
                    {reply, {error, Err}, State}
            end
    end;
handle_call({'unbind:', [Name]}, _From, State) ->
    case to_atom_name(Name) of
        {error, Err} ->
            {reply, {error, Err}, State};
        AtomName ->
            UserBindings = State#workspace_interface_state.user_bindings,
            case maps:is_key(AtomName, UserBindings) of
                true ->
                    NewBindings = maps:remove(AtomName, UserBindings),
                    NewState = State#workspace_interface_state{user_bindings = NewBindings},
                    {reply, nil, NewState};
                false ->
                    Err0 = beamtalk_error:new(name_not_found, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'unbind:'),
                    Err2 = beamtalk_error:with_message(
                        Err1,
                        iolist_to_binary([
                            atom_to_binary(AtomName, utf8),
                            <<" is not a registered workspace name">>
                        ])
                    ),
                    {reply, {error, Err2}, State}
            end
    end;
handle_call(get_user_bindings, _From, State) ->
    {reply, State#workspace_interface_state.user_bindings, State};
handle_call(get_session_bindings, _From, State) ->
    GenServerPid = self(),
    UserBindings = State#workspace_interface_state.user_bindings,
    {reply, handle_session_bindings(GenServerPid, UserBindings), State};
handle_call({'load:', [Path]}, From, State) ->
    %% load: compiles a file — spawn to avoid blocking the gen_server mailbox
    spawn(fun() ->
        try handle_load(Path) of
            Result -> gen_server:reply(From, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("load: handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                gen_server:reply(
                    From, {error, beamtalk_error:with_message(Err, <<"Internal error in load:">>)}
                )
        end
    end),
    {noreply, State};
handle_call({test, []}, From, State) ->
    %% test may take a long time — spawn to avoid blocking
    spawn(fun() ->
        try handle_test() of
            Result -> gen_server:reply(From, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("test handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                gen_server:reply(
                    From, {error, beamtalk_error:with_message(Err, <<"Internal error in test">>)}
                )
        end
    end),
    {noreply, State};
handle_call({'test:', [TestClass]}, From, State) ->
    spawn(fun() ->
        try handle_test_class(TestClass) of
            Result -> gen_server:reply(From, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("test: handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                gen_server:reply(
                    From, {error, beamtalk_error:with_message(Err, <<"Internal error in test:">>)}
                )
        end
    end),
    {noreply, State};
handle_call({Selector, Args}, From, State) when is_atom(Selector) ->
    %% Unknown selector — try hierarchy walk for inherited methods
    %% (e.g. class, respondsTo:, methods from Object/Actor)
    GenServerPid = self(),
    spawn(fun() ->
        Self = {beamtalk_object, 'WorkspaceInterface', ?MODULE, GenServerPid},
        try beamtalk_dispatch:lookup(Selector, Args, Self, #{}, 'WorkspaceInterface') of
            {reply, Value, _NewState} ->
                gen_server:reply(From, Value);
            {error, Error} ->
                gen_server:reply(From, {error, Error})
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("dispatch handler crashed", #{
                    selector => Selector, class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                gen_server:reply(
                    From,
                    {error,
                        beamtalk_error:with_selector(
                            beamtalk_error:with_message(Err, <<"Internal error">>), Selector
                        )}
                )
        end
    end),
    {noreply, State};
handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Error1 = beamtalk_error:with_hint(Error0, <<"Expected {Selector, Args} format">>),
    Error2 = beamtalk_error:with_details(Error1, #{request => Request}),
    {reply, {error, Error2}, State}.

%% @doc Handle asynchronous messages (future-based dispatch).
-spec handle_cast(term(), #workspace_interface_state{}) ->
    {noreply, #workspace_interface_state{}}.
handle_cast({actors, [], FuturePid}, State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, handle_actors()),
    {noreply, State};
handle_cast({'actorAt:', [PidStr], FuturePid}, State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, handle_actor_at(PidStr)),
    {noreply, State};
handle_cast({classes, [], FuturePid}, State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, handle_classes()),
    {noreply, State};
handle_cast({'actorsOf:', [AClass], FuturePid}, State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, handle_actors_of(AClass)),
    {noreply, State};
handle_cast({testClasses, [], FuturePid}, State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, handle_test_classes()),
    {noreply, State};
handle_cast({globals, [], FuturePid}, State) when is_pid(FuturePid) ->
    GenServerPid = self(),
    UserBindings = State#workspace_interface_state.user_bindings,
    spawn(fun() ->
        try handle_globals(GenServerPid, UserBindings) of
            Result -> beamtalk_future:resolve(FuturePid, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("globals handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                beamtalk_future:reject(
                    FuturePid, beamtalk_error:with_message(Err, <<"Internal error in globals">>)
                )
        end
    end),
    {noreply, State};
handle_cast({'bind:as:', [Value, Name], FuturePid}, State) when is_pid(FuturePid) ->
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_future:reject(FuturePid, Err),
            {noreply, State};
        AtomName ->
            case check_bind_conflicts(AtomName) of
                ok ->
                    UserBindings = State#workspace_interface_state.user_bindings,
                    NewBindings = maps:put(AtomName, Value, UserBindings),
                    NewState = State#workspace_interface_state{user_bindings = NewBindings},
                    spawn(fun() -> maybe_warn_loaded_class(AtomName) end),
                    beamtalk_future:resolve(FuturePid, nil),
                    {noreply, NewState};
                {error, Err} ->
                    beamtalk_future:reject(FuturePid, Err),
                    {noreply, State}
            end
    end;
handle_cast({'unbind:', [Name], FuturePid}, State) when is_pid(FuturePid) ->
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_future:reject(FuturePid, Err),
            {noreply, State};
        AtomName ->
            UserBindings = State#workspace_interface_state.user_bindings,
            case maps:is_key(AtomName, UserBindings) of
                true ->
                    NewBindings = maps:remove(AtomName, UserBindings),
                    NewState = State#workspace_interface_state{user_bindings = NewBindings},
                    beamtalk_future:resolve(FuturePid, nil),
                    {noreply, NewState};
                false ->
                    Err0 = beamtalk_error:new(name_not_found, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'unbind:'),
                    Err2 = beamtalk_error:with_message(
                        Err1,
                        iolist_to_binary([
                            atom_to_binary(AtomName, utf8),
                            <<" is not a registered workspace name">>
                        ])
                    ),
                    beamtalk_future:reject(FuturePid, Err2),
                    {noreply, State}
            end
    end;
handle_cast({'load:', [Path], FuturePid}, State) when is_pid(FuturePid) ->
    %% load: compiles a file — spawn to avoid blocking the gen_server mailbox
    spawn(fun() ->
        try handle_load(Path) of
            {error, Err} ->
                beamtalk_future:reject(FuturePid, Err);
            Result ->
                beamtalk_future:resolve(FuturePid, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("load: cast handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                beamtalk_future:reject(
                    FuturePid, beamtalk_error:with_message(Err, <<"Internal error in load:">>)
                )
        end
    end),
    {noreply, State};
handle_cast({test, [], FuturePid}, State) when is_pid(FuturePid) ->
    spawn(fun() ->
        try handle_test() of
            Result -> beamtalk_future:resolve(FuturePid, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("test handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                beamtalk_future:reject(
                    FuturePid, beamtalk_error:with_message(Err, <<"Internal error in test">>)
                )
        end
    end),
    {noreply, State};
handle_cast({'test:', [TestClass], FuturePid}, State) when is_pid(FuturePid) ->
    spawn(fun() ->
        try handle_test_class(TestClass) of
            Result -> beamtalk_future:resolve(FuturePid, Result)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("test: handler crashed", #{
                    class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                beamtalk_future:reject(
                    FuturePid, beamtalk_error:with_message(Err, <<"Internal error in test:">>)
                )
        end
    end),
    {noreply, State};
handle_cast({Selector, Args, FuturePid}, State) when
    is_pid(FuturePid), is_atom(Selector)
->
    %% Unknown selector — try hierarchy walk (Object/Actor methods)
    GenServerPid = self(),
    spawn(fun() ->
        Self = {beamtalk_object, 'WorkspaceInterface', ?MODULE, GenServerPid},
        try beamtalk_dispatch:lookup(Selector, Args, Self, #{}, 'WorkspaceInterface') of
            {reply, Value, _NewState} ->
                beamtalk_future:resolve(FuturePid, Value);
            {error, Error} ->
                beamtalk_future:reject(FuturePid, Error)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("dispatch handler crashed", #{
                    selector => Selector, class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                beamtalk_future:reject(
                    FuturePid,
                    beamtalk_error:with_selector(
                        beamtalk_error:with_message(Err, <<"Internal error">>), Selector
                    )
                )
        end
    end),
    {noreply, State};
handle_cast(Msg, State) ->
    ?LOG_WARNING("WorkspaceInterface received unexpected cast", #{message => Msg}),
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #workspace_interface_state{}) ->
    {noreply, #workspace_interface_state{}}.
handle_info(Info, State) ->
    ?LOG_DEBUG("WorkspaceInterface received info", #{info => Info}),
    {noreply, State}.

%% @doc Handle process termination.
-spec terminate(term(), #workspace_interface_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change during hot reload.
-spec code_change(term(), #workspace_interface_state{}, term()) ->
    {ok, #workspace_interface_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal method implementations
%%====================================================================

%% @doc Get all live actors as beamtalk_object references.
-spec handle_actors() -> [tuple()].
handle_actors() ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(fun wrap_actor/1, Actors)
    end.

%% @doc Look up a specific actor by pid string.
-spec handle_actor_at(binary() | list()) -> tuple() | 'nil'.
handle_actor_at(PidStr) when is_binary(PidStr) ->
    handle_actor_at(binary_to_list(PidStr));
handle_actor_at(PidStr) when is_list(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case whereis(beamtalk_actor_registry) of
            undefined ->
                nil;
            RegistryPid ->
                case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                    {ok, Metadata} ->
                        case wrap_actor(Metadata) of
                            {true, Obj} -> Obj;
                            false -> nil
                        end;
                    {error, not_found} ->
                        nil
                end
        end
    catch
        error:badarg -> nil
    end;
handle_actor_at(_) ->
    nil.

%% @doc Filter actors by class, walking inheritance.
%% Equivalent to: self actors select: [:a | a class includesBehaviour: aClass]
-spec handle_actors_of(term()) -> [tuple()].
handle_actors_of(AClass) ->
    AClassName = class_name_from_arg(AClass),
    Actors = handle_actors(),
    lists:filter(
        fun
            ({beamtalk_object, ActorClass, _Mod, _Pid}) ->
                beamtalk_class_registry:inherits_from(ActorClass, AClassName);
            (_) ->
                false
        end,
        Actors
    ).

%% @doc Return all loaded user classes (those with a source file recorded).
%% Excludes stdlib and ClassBuilder-created classes (they have no source file).
-spec handle_classes() -> [tuple()].
handle_classes() ->
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    ClassName = beamtalk_object_class:class_name(Pid),
                    ModuleName = beamtalk_object_class:module_name(Pid),
                    case source_file_from_module(ModuleName) of
                        nil ->
                            false;
                        _SourceFile ->
                            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
                            {true, {beamtalk_object, ClassTag, ModuleName, Pid}}
                    end
                catch
                    exit:{noproc, _} -> false;
                    exit:{timeout, _} -> false
                end
            end,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            []
    end.

%% @doc Return classes that are TestCase subclasses.
%% Equivalent to: self classes select: [:c | c inheritsFrom: TestCase]
-spec handle_test_classes() -> [tuple()].
handle_test_classes() ->
    Classes = handle_classes(),
    lists:filter(
        fun
            ({beamtalk_object, ClassTag, _Mod, _Pid}) ->
                ClassName = base_class_name(ClassTag),
                beamtalk_class_registry:inherits_from(ClassName, 'TestCase');
            (_) ->
                false
        end,
        Classes
    ).

%% @doc Return an immutable Dictionary snapshot of the project namespace.
%% Includes workspace singletons (Transcript, Beamtalk, Workspace),
%% all user-loaded classes, and user-registered bindings via bind:as:.
-spec handle_globals(pid(), map()) -> map().
handle_globals(GenServerPid, UserBindings) ->
    %% Start with non-class globals (singletons + bind:as: entries)
    Base = handle_session_bindings(GenServerPid, UserBindings),
    %% Add all user classes (highest priority — overrides same-name bind:as: entries)
    Classes = handle_classes(),
    lists:foldl(
        fun
            ({beamtalk_object, ClassTag, _Mod, _Pid} = ClassObj, Acc) ->
                ClassName = base_class_name(ClassTag),
                maps:put(ClassName, ClassObj, Acc);
            (_, Acc) ->
                Acc
        end,
        Base,
        Classes
    ).

%% @doc Return non-class entries from workspace globals for session binding injection (BT-883).
%% Includes singletons (Transcript, Beamtalk, Workspace) and user-registered
%% bind:as: names. Class objects are excluded — class name resolution continues
%% via beamtalk_class_registry (see dispatch_codegen.rs:1207).
-spec handle_session_bindings(pid(), map()) -> map().
handle_session_bindings(GenServerPid, UserBindings) ->
    WorkspaceSelf =
        {beamtalk_object, 'WorkspaceInterface', ?MODULE, GenServerPid},
    %% Start with user-registered bindings (lowest priority — overridden by singletons)
    Base0 = UserBindings,
    Base1 =
        case resolve_singleton('TranscriptStream') of
            nil -> Base0;
            TranscriptObj -> maps:put('Transcript', TranscriptObj, Base0)
        end,
    Base2 =
        case resolve_singleton('BeamtalkInterface') of
            nil -> Base1;
            BeamtalkObj -> maps:put('Beamtalk', BeamtalkObj, Base1)
        end,
    maps:put('Workspace', WorkspaceSelf, Base2).

%% @doc Load a .bt file, compiling and registering the class.
-spec handle_load(term()) -> nil | {error, #beamtalk_error{}}.
handle_load(Path) when is_binary(Path) ->
    handle_load(binary_to_list(Path));
handle_load(Path) when is_list(Path) ->
    case beamtalk_repl_eval:reload_class_file(Path) of
        ok ->
            nil;
        {error, {file_not_found, _}} ->
            Err0 = beamtalk_error:new(file_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"File not found: ">>, Path])
                )};
        {error, Reason} ->
            Err0 = beamtalk_error:new(load_error, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            Err2 = beamtalk_error:with_message(
                Err1,
                iolist_to_binary([<<"Failed to load: ">>, Path])
            ),
            Err3 = beamtalk_error:with_details(Err2, #{reason => Reason}),
            {error, Err3}
    end;
handle_load(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'load:'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([<<"load: expects a String path, got ">>, TypeName])
    ),
    {error, Err2}.

%% @doc Run all test classes via TestRunner.
-spec handle_test() -> term().
handle_test() ->
    case beamtalk_class_registry:whereis_class('TestRunner') of
        undefined ->
            {error,
                beamtalk_error:with_message(
                    beamtalk_error:new(class_not_found, 'WorkspaceInterface'),
                    <<"TestRunner class not found">>
                )};
        ClassPid ->
            beamtalk_class_dispatch:class_send(ClassPid, runAll, [])
    end.

%% @doc Run a specific test class via TestRunner.
-spec handle_test_class(term()) -> term().
handle_test_class(TestClass) ->
    case beamtalk_class_registry:whereis_class('TestRunner') of
        undefined ->
            {error,
                beamtalk_error:with_message(
                    beamtalk_error:new(class_not_found, 'WorkspaceInterface'),
                    <<"TestRunner class not found">>
                )};
        ClassPid ->
            beamtalk_class_dispatch:class_send(ClassPid, 'run:', [TestClass])
    end.

%% @doc Handle bind:as: — forward to gen_server to validate and update state.
-spec handle_bind(term(), term(), pid()) -> 'nil' | {error, #beamtalk_error{}}.
handle_bind(Value, Name, GenServerPid) ->
    gen_server:call(GenServerPid, {'bind:as:', [Value, Name]}, 5000).

%% @doc Handle unbind: — forward to gen_server to validate and update state.
-spec handle_unbind(term(), pid()) -> 'nil' | {error, #beamtalk_error{}}.
handle_unbind(Name, GenServerPid) ->
    gen_server:call(GenServerPid, {'unbind:', [Name]}, 5000).

%%====================================================================
%% Internal helpers
%%====================================================================

%% @doc Convert a name argument (binary, atom, or symbol) to an atom.
%% Returns {error, Error} for non-string/non-atom arguments.
-spec to_atom_name(term()) -> atom() | {error, #beamtalk_error{}}.
to_atom_name(Name) when is_atom(Name) -> Name;
to_atom_name(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Expected a Symbol name, got ">>, TypeName])
    ),
    {error, Err1}.

%% @doc Check if a name conflicts with Beamtalk system globals or workspace singletons.
%% Returns ok if no conflict, or {error, Error} if the name is protected.
-spec check_bind_conflicts(atom()) -> ok | {error, #beamtalk_error{}}.
check_bind_conflicts(AtomName) ->
    case is_protected_name(AtomName) of
        true ->
            Err0 = beamtalk_error:new(name_conflict, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'bind:as:'),
            Err2 = beamtalk_error:with_message(
                Err1,
                iolist_to_binary([
                    atom_to_binary(AtomName, utf8),
                    <<" is a system name and cannot be shadowed">>
                ])
            ),
            {error, Err2};
        false ->
            ok
    end.

%% @doc Check if a name is protected (class registry or workspace singleton).
-spec is_protected_name(atom()) -> boolean().
is_protected_name('Transcript') -> true;
is_protected_name('Beamtalk') -> true;
is_protected_name('Workspace') -> true;
is_protected_name(_) -> false.

%% @doc Warn if name is an existing loaded class (has source file).
-spec maybe_warn_loaded_class(atom()) -> ok.
maybe_warn_loaded_class(AtomName) ->
    Classes = handle_classes(),
    IsLoadedClass = lists:any(
        fun
            ({beamtalk_object, ClassTag, _Mod, _Pid}) ->
                base_class_name(ClassTag) =:= AtomName;
            (_) ->
                false
        end,
        Classes
    ),
    case IsLoadedClass of
        true ->
            WarningMsg = iolist_to_binary([
                <<"Warning: ">>,
                atom_to_binary(AtomName, utf8),
                <<" is a loaded class. Use reload instead.">>
            ]),
            ?LOG_WARNING("~s", [WarningMsg]),
            %% Also output to Transcript if available
            case resolve_singleton('TranscriptStream') of
                nil ->
                    ok;
                TranscriptObj ->
                    try
                        beamtalk_message_dispatch:send(TranscriptObj, 'show:', [WarningMsg])
                    catch
                        _:_ -> ok
                    end
            end;
        false ->
            ok
    end.

%% @doc Read beamtalk_source attribute from a module's attributes.
%% Uses erlang:get_module_info/2 BIF instead of Mod:module_info/1 because
%% Beamtalk modules compiled from Core Erlang do not export module_info.
%% Returns nil for stdlib/bootstrap/ClassBuilder-created classes.
-spec source_file_from_module(atom()) -> binary() | 'nil'.
source_file_from_module(ModuleName) ->
    try erlang:get_module_info(ModuleName, attributes) of
        Attrs ->
            case lists:keyfind(beamtalk_source, 1, Attrs) of
                {beamtalk_source, [Path]} when is_binary(Path) -> Path;
                {beamtalk_source, [Path]} when is_list(Path) -> list_to_binary(Path);
                _ -> nil
            end
    catch
        error:badarg -> nil
    end.

%% @doc Return a human-readable type name for an Erlang/Beamtalk value.
-spec value_type_name(term()) -> binary().
value_type_name(V) when is_integer(V) -> <<"Integer">>;
value_type_name(V) when is_float(V) -> <<"Float">>;
value_type_name(V) when is_boolean(V) -> <<"Boolean">>;
value_type_name(nil) -> <<"nil">>;
value_type_name(V) when is_atom(V) -> <<"Symbol">>;
value_type_name(V) when is_list(V) -> <<"List">>;
value_type_name(V) when is_map(V) -> <<"Dictionary">>;
value_type_name({beamtalk_object, _, _, _}) -> <<"Object">>;
value_type_name(_) -> <<"Unknown">>.

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

%% @doc Extract the base class name from a class argument.
-spec class_name_from_arg(term()) -> atom().
class_name_from_arg({beamtalk_object, ClassTag, _Mod, _Pid}) ->
    base_class_name(ClassTag);
class_name_from_arg(ClassName) when is_atom(ClassName) ->
    ClassName;
class_name_from_arg(_) ->
    undefined.

%% @doc Extract the base class name from a class tag (e.g. 'Counter class' -> 'Counter').
-spec base_class_name(atom()) -> atom().
base_class_name(Tag) ->
    Bin = beamtalk_class_registry:class_display_name(Tag),
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> Tag
    end.

%% @doc Resolve a singleton class instance (e.g. TranscriptStream current).
-spec resolve_singleton(atom()) -> tuple() | 'nil'.
resolve_singleton(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            nil;
        ClassPid ->
            try
                beamtalk_class_dispatch:class_send(ClassPid, current, [])
            catch
                _:_ -> nil
            end
    end.

%% @doc Register the WorkspaceInterface class with the class registry.
-spec register_class() -> ok.
register_class() ->
    try
        ClassInfo = class_info(),
        case beamtalk_class_registry:whereis_class('WorkspaceInterface') of
            undefined ->
                case beamtalk_object_class:start_link('WorkspaceInterface', ClassInfo) of
                    {ok, _Pid} ->
                        ?LOG_DEBUG("Registered WorkspaceInterface class", #{});
                    {error, RegReason} ->
                        ?LOG_WARNING("Failed to register WorkspaceInterface class", #{
                            reason => RegReason
                        })
                end;
            _Pid ->
                ok
        end
    catch
        Kind:CrashReason ->
            ?LOG_WARNING("WorkspaceInterface class registration failed", #{
                kind => Kind, reason => CrashReason
            })
    end,
    ok.
