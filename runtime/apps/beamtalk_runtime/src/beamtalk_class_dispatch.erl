%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Class method dispatch for Beamtalk class objects.
%%%
%%% **DDD Context:** Object System
%%%
%% Handles class-level message sending protocol, translating Beamtalk messages
%% to gen_server calls. Also provides helpers for class method execution
%% including test execution detection and result unwrapping.
%%%
%%% Extracted from beamtalk_object_class.erl (BT-704).
-module(beamtalk_class_dispatch).

-include_lib("kernel/include/logger.hrl").

-export([
    class_send/3,
    unwrap_class_call/1,
    class_method_fun_name/1,
    is_test_execution_selector/1,
    handle_class_method_call/6
]).

-type selector() :: atom().
-type class_name() :: atom().

%% @doc Send a message to a class object synchronously (BT-246 / ADR 0013 Phase 1).
%%
%% Dispatches messages to the class gen_server, translating the Beamtalk
%% message protocol ({Selector, Args}) to the class process message format.
%% Unwraps {ok, Value} / {error, Error} results for seamless integration.
-spec class_send(pid() | undefined, atom(), list()) -> term().
class_send(undefined, Selector, _Args) ->
    Error = beamtalk_error:new(class_not_found, unknown, Selector),
    beamtalk_error:raise(Error);
class_send(ClassPid, 'new', []) ->
    unwrap_class_call(gen_server:call(ClassPid, {new, []}));
class_send(ClassPid, 'new:', [Map]) ->
    unwrap_class_call(gen_server:call(ClassPid, {new, [Map]}));
class_send(ClassPid, spawn, []) ->
    unwrap_class_call(gen_server:call(ClassPid, {spawn, []}));
class_send(ClassPid, 'spawnWith:', [Map]) ->
    unwrap_class_call(gen_server:call(ClassPid, {spawn, [Map]}));
class_send(ClassPid, methods, []) ->
    gen_server:call(ClassPid, methods);
class_send(ClassPid, superclass, []) ->
    case gen_server:call(ClassPid, superclass) of
        none -> nil;  % Beamtalk nil, not Erlang 'none'
        Super -> Super
    end;
class_send(ClassPid, class_name, []) ->
    gen_server:call(ClassPid, class_name);
class_send(ClassPid, module_name, []) ->
    gen_server:call(ClassPid, module_name);
class_send(ClassPid, 'printString', []) ->
    %% BT-477: Class objects return their display name as a string.
    ClassName = gen_server:call(ClassPid, class_name),
    atom_to_binary(ClassName, utf8);
class_send(_ClassPid, class, []) ->
    %% BT-412: Metaclass terminal — returns 'Metaclass' sentinel atom.
    'Metaclass';
class_send(ClassPid, subclasses, []) ->
    %% BT-573: Return direct subclass names as a sorted list.
    ClassName = gen_server:call(ClassPid, class_name),
    beamtalk_class_registry:direct_subclasses(ClassName);
class_send(ClassPid, allSubclasses, []) ->
    %% BT-573: Return all subclass names recursively as a sorted list.
    ClassName = gen_server:call(ClassPid, class_name),
    beamtalk_class_registry:all_subclasses(ClassName);
class_send(ClassPid, Selector, Args) ->
    %% BT-411: Try user-defined class methods before raising does_not_understand
    %% BT-440: Test execution may take a long time; use longer timeout.
    Timeout = case is_test_execution_selector(Selector) of
        true -> 300000;  % 5 minutes for test suites
        false -> 5000    % default gen_server timeout
    end,
    case gen_server:call(ClassPid, {class_method_call, Selector, Args}, Timeout) of
        {ok, Result} -> Result;
        {error, not_found} ->
            ClassName = gen_server:call(ClassPid, class_name),
            Error = beamtalk_error:new(
                does_not_understand,
                ClassName,
                Selector,
                <<"Class does not understand this message">>),
            beamtalk_error:raise(Error);
        Other -> unwrap_class_call(Other)
    end.

%% @doc Unwrap a class gen_server call result for use in class_send.
%%
%% Translates {ok, Value} → Value, {error, Error} → re-raise as exception.
%% Handles both raw #beamtalk_error{} records and already-wrapped Exception
%% maps (from raise/1 inside handle_call). Uses ensure_wrapped/1 for
%% idempotent wrapping (BT-525).
-spec unwrap_class_call(term()) -> term().
unwrap_class_call({ok, Value}) -> Value;
unwrap_class_call({error, Error}) ->
    Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
    error(Wrapped).

%% @doc Handle a class method call from the gen_server.
%%
%% BT-411: User-defined class method dispatch.
%% BT-412: Passes class variables to method and handles updates.
%% BT-440: For test execution (runAll, run:), spawns in a separate process
%% to avoid gen_server deadlock.
%%
%% Returns {reply, Result, NewState} or {noreply, State} for async test execution.
-spec handle_class_method_call(
    selector(), list(), class_name(), atom(),
    #{selector() => {class_name(), term()}},
    map()
) -> {reply, term()} | {test_spawn, fun()} | {error, not_found}.
handle_class_method_call(Selector, Args, ClassName, Module, FlatClassMethods, ClassVars) ->
    case maps:find(Selector, FlatClassMethods) of
        {ok, {DefiningClass, _MethodInfo}} ->
            %% Resolve the module for the defining class (may differ for inherited methods)
            DefiningModule = case DefiningClass of
                ClassName -> Module;
                _ ->
                    case beamtalk_class_registry:whereis_class(DefiningClass) of
                        undefined -> Module;
                        DefPid -> gen_server:call(DefPid, get_module, 5000)
                    end
            end,
            %% BT-440: For test execution (runAll, run:) inherited from TestCase,
            %% return a spawn request so the gen_server can handle noreply.
            case is_test_execution_selector(Selector) andalso
                 DefiningClass =:= 'TestCase' of
                true ->
                    {test_spawn, DefiningModule};
                false ->
                    %% Build class self object for `self` reference in class methods
                    ClassSelf = {beamtalk_object, beamtalk_class_registry:class_object_tag(ClassName), DefiningModule, self()},
                    FunName = class_method_fun_name(Selector),
                    %% BT-412: Pass class variables; handle {Result, NewClassVars} returns
                    try erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args]) of
                        {class_var_result, Result, NewClassVars} ->
                            {reply, {ok, Result}, NewClassVars};
                        Result ->
                            {reply, {ok, Result}, ClassVars}
                    catch
                        Class:Error ->
                            ?LOG_ERROR("Class method ~p:~p failed: ~p:~p",
                                         [ClassName, Selector, Class, Error]),
                            {reply, {error, Error}, ClassVars}
                    end
            end;
        error ->
            {error, not_found}
    end.

%% @doc Convert a class method selector to its module function name.
%% Class methods are generated with a 'class_' prefix, e.g.
%% `class defaultValue => 42` becomes `class_defaultValue/1`.
-spec class_method_fun_name(selector()) -> atom().
class_method_fun_name(Selector) ->
    list_to_existing_atom("class_" ++ atom_to_list(Selector)).

%% @doc Check if a class method selector is a test execution command.
%% BT-440: These selectors need special handling to avoid gen_server deadlock.
-spec is_test_execution_selector(selector()) -> boolean().
is_test_execution_selector(runAll) -> true;
is_test_execution_selector('run:') -> true;
is_test_execution_selector(_) -> false.
