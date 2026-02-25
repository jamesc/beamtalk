%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace interface - actor introspection singleton.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This gen_server implements the WorkspaceInterface that provides
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

-module(beamtalk_workspace_interface).
-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
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

-record(workspace_interface_state, {}).

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

%% @doc Check if Workspace supports a given method selector.
-spec has_method(selector()) -> boolean().
has_method(actors) -> true;
has_method('actorAt:') -> true;
has_method('actorsOf:') -> true;
has_method(classes) -> true;
has_method(testClasses) -> true;
has_method(globals) -> true;
has_method('load:') -> true;
has_method(clear) -> true;
has_method(test) -> true;
has_method('test:') -> true;
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
            'actorsOf:' => #{arity => 1},
            classes => #{arity => 0},
            testClasses => #{arity => 0},
            globals => #{arity => 0},
            'load:' => #{arity => 1},
            clear => #{arity => 0},
            test => #{arity => 0},
            'test:' => #{arity => 1}
        },
        class_methods => #{},
        fields => []
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the Workspace interface state.
-spec init([boolean()]) -> {ok, #workspace_interface_state{}}.
init([true]) ->
    %% Named instances self-register class since we're in beamtalk_workspace app
    %% (beamtalk_stdlib in beamtalk_runtime can't reference us)
    register_class(),
    {ok, #workspace_interface_state{}};
init([false]) ->
    {ok, #workspace_interface_state{}}.

%% @doc Handle synchronous method calls.
-spec handle_call(term(), {pid(), term()}, #workspace_interface_state{}) ->
    {reply, term(), #workspace_interface_state{}}.

handle_call({actors, []}, _From, State) ->
    Result = handle_actors(),
    {reply, Result, State};
handle_call({'actorAt:', [PidStr]}, _From, State) ->
    Result = handle_actor_at(PidStr),
    {reply, Result, State};
handle_call({'actorsOf:', [ClassName]}, _From, State) ->
    Result = handle_actors_of(ClassName),
    {reply, Result, State};
handle_call({classes, []}, _From, State) ->
    Result = handle_classes(),
    {reply, Result, State};
handle_call({testClasses, []}, _From, State) ->
    Result = handle_test_classes(),
    {reply, Result, State};
handle_call({globals, []}, _From, State) ->
    Result = handle_globals(),
    {reply, Result, State};
handle_call({'load:', [Path]}, _From, State) ->
    Result = handle_load(Path),
    {reply, Result, State};
handle_call({clear, []}, {FromPid, _}, State) ->
    Result = handle_clear(FromPid),
    {reply, Result, State};
handle_call({test, []}, _From, State) ->
    Result = handle_test(),
    {reply, Result, State};
handle_call({'test:', [TestClass]}, _From, State) ->
    Result = handle_test_class(TestClass),
    {reply, Result, State};
handle_call({UnknownSelector, _Args}, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"To list available selectors, use: Workspace methods">>
    ),
    {reply, {error, Error2}, State};
handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Error1 = beamtalk_error:with_hint(Error0, <<"Expected {Selector, Args} format">>),
    Error2 = beamtalk_error:with_details(Error1, #{request => Request}),
    {reply, {error, Error2}, State}.

%% @doc Handle asynchronous messages.
%% Workspace binding dispatch uses beamtalk_actor:async_send/4 which
%% sends {Selector, Args, FuturePid} as a cast.
-spec handle_cast(term(), #workspace_interface_state{}) ->
    {noreply, #workspace_interface_state{}}.

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
handle_cast({classes, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_classes(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({testClasses, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_test_classes(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({globals, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_globals(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({'load:', [Path], FuturePid}, State) when is_pid(FuturePid) ->
    case handle_load(Path) of
        {error, Err} ->
            beamtalk_future:reject(FuturePid, Err);
        Result ->
            beamtalk_future:resolve(FuturePid, Result)
    end,
    {noreply, State};
handle_cast({clear, [], FuturePid}, State) when is_pid(FuturePid) ->
    %% BT-872: Async clear cannot locate the session — reject explicitly rather
    %% than silently succeeding with nil while leaving bindings untouched.
    Err0 = beamtalk_error:new(session_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, clear),
    Err2 = beamtalk_error:with_message(
        Err1,
        <<"Workspace clear is not available via async dispatch (BT-872)">>
    ),
    beamtalk_future:reject(FuturePid, Err2),
    {noreply, State};
handle_cast({test, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_test(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({'test:', [TestClass], FuturePid}, State) when is_pid(FuturePid) ->
    case handle_test_class(TestClass) of
        {error, Err} ->
            beamtalk_future:reject(FuturePid, Err);
        Result ->
            beamtalk_future:resolve(FuturePid, Result)
    end,
    {noreply, State};
handle_cast({UnknownSelector, _Args, FuturePid}, State) when
    is_pid(FuturePid), is_atom(UnknownSelector)
->
    Error0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Error1 = beamtalk_error:with_selector(Error0, UnknownSelector),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"To list available selectors, use: Workspace methods">>
    ),
    beamtalk_future:reject(FuturePid, Error2),
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
%% Returns beamtalk_object or nil if not found.
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
        undefined ->
            [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(
                fun
                    (#{class := Class} = Meta) when Class =:= ClassName ->
                        wrap_actor(Meta);
                    (_) ->
                        false
                end,
                Actors
            )
    end;
handle_actors_of(_) ->
    [].

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

%% @doc Return TestCase subclasses as class object tuples.
-spec handle_test_classes() -> [tuple()].
handle_test_classes() ->
    try
        ClassNames = beamtalk_test_case:find_test_classes(),
        lists:filtermap(fun class_name_to_object/1, ClassNames)
    catch
        _:_ -> []
    end.

%% @doc Return an immutable snapshot of workspace-level globals.
%% Includes workspace singletons (Transcript, Beamtalk, Workspace) and
%% user-loaded classes (those with a beamtalk_source module attribute).
-spec handle_globals() -> map().
handle_globals() ->
    %% Start with workspace singletons
    Singletons = beamtalk_workspace_config:singletons(),
    Base = lists:foldl(
        fun(#{binding_name := BindingName, class_name := ClassName, module := Module}, Acc) ->
            case whereis(BindingName) of
                undefined ->
                    Acc;
                Pid ->
                    Key = atom_to_binary(BindingName, utf8),
                    Acc#{Key => {beamtalk_object, ClassName, Module, Pid}}
            end
        end,
        #{},
        Singletons
    ),
    %% Add user-loaded classes
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:foldl(
            fun(Pid, Acc) ->
                try
                    ClassName = beamtalk_object_class:class_name(Pid),
                    ModuleName = beamtalk_object_class:module_name(Pid),
                    case source_file_from_module(ModuleName) of
                        nil ->
                            Acc;
                        _SourceFile ->
                            Key = atom_to_binary(ClassName, utf8),
                            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
                            Acc#{Key => {beamtalk_object, ClassTag, ModuleName, Pid}}
                    end
                catch
                    exit:{noproc, _} -> Acc;
                    exit:{timeout, _} -> Acc
                end
            end,
            Base,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            Base
    end.

%% @doc Load a .bt file, compiling and registering the class.
%% Path must be a binary or list string. Non-string arguments return a typed error.
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

%% @doc Clear session variable bindings for the calling REPL session.
%% Uses the bt_session_pid process dictionary key set by beamtalk_repl_shell
%% in the eval worker to find the session PID.
-spec handle_clear(pid()) -> nil.
handle_clear(FromPid) ->
    SessionPid =
        case erlang:process_info(FromPid, dictionary) of
            {dictionary, Dict} ->
                proplists:get_value(bt_session_pid, Dict, undefined);
            _ ->
                undefined
        end,
    case SessionPid of
        undefined ->
            nil;
        Pid when is_pid(Pid) ->
            try
                ok = beamtalk_repl_shell:clear_bindings(Pid)
            catch
                _:_ -> ok
            end,
            nil
    end.

%% @doc Run all loaded TestCase subclasses and return a TestResult.
-spec handle_test() -> map().
handle_test() ->
    beamtalk_test_runner:run_all().

%% @doc Run a specific test class and return a TestResult.
%% Accepts a class object tuple or a class name atom/binary.
-spec handle_test_class(tuple() | atom() | binary()) -> map() | {error, #beamtalk_error{}}.
handle_test_class({beamtalk_object, _, _, _} = ClassRef) ->
    beamtalk_test_runner:run_class(ClassRef);
handle_test_class(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Err0 = beamtalk_error:new(class_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'test:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"Unknown class: ">>, atom_to_binary(ClassName, utf8)])
                )};
        Pid ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            beamtalk_test_runner:run_class({beamtalk_object, ClassTag, ModuleName, Pid})
    end;
handle_test_class(ClassName) when is_binary(ClassName) ->
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_test_class(ClassAtom)
    catch
        error:badarg ->
            Err0 = beamtalk_error:new(class_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'test:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"Unknown class: ">>, ClassName])
                )}
    end;
handle_test_class(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'test:'),
    {error,
        beamtalk_error:with_message(
            Err1,
            iolist_to_binary([<<"test: expects a class, got ">>, TypeName])
        )}.

%% @doc Convert a class name atom to a beamtalk_object class tuple.
-spec class_name_to_object(atom()) -> {true, tuple()} | false.
class_name_to_object(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            false;
        Pid ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {true, {beamtalk_object, ClassTag, ModuleName, Pid}}
    end.

%% @doc Read beamtalk_source attribute from a module's module_info(attributes).
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
        error:undef -> nil
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

%% @doc Register the WorkspaceInterface class with the class registry.
%% Uses start_link which creates a link to the class process.
%% Wrapped in try/catch to handle startup failures gracefully.
%% Note: post-start crashes of the linked class process will propagate,
%% but this matches how all other classes are registered (beamtalk_stdlib pattern).
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
