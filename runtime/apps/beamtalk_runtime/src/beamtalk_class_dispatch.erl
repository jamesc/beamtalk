%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Class method dispatch for Beamtalk class objects.
%%%
%%% **DDD Context:** Object System
%%%
%%% Handles class-level message sending protocol, translating Beamtalk messages
%%% to gen_server calls. Also provides helpers for class method execution
%%% including test execution detection and result unwrapping.
%%%
%%% ADR 0032 Phase 0 (BT-732): Added class chain fallthrough.
%%% When a class-side message is not found in user-defined class methods,
%%% dispatch falls through to 'Class' instance methods via beamtalk_dispatch:lookup/5.
%%%
%%% Extracted from beamtalk_object_class.erl (BT-704).
-module(beamtalk_class_dispatch).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    class_send/3,
    metaclass_send/4,
    unwrap_class_call/1,
    class_method_fun_name/1,
    is_test_execution_selector/1,
    handle_class_method_call/6,
    handle_async_dispatch/5
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
%% BT-755: Detect gen_server self-call before it deadlocks.
%% A class method sending new/new:/spawn/spawnWith: to its own class (ClassPid == self())
%% would cause gen_server:call(self(), ...) to deadlock — OTP raises {calling_self,...}.
%% Guard all four instantiation selectors and raise a clean Beamtalk error instead.
class_send(ClassPid, 'new', _Args) when ClassPid =:= self() ->
    handle_calling_self_error(ClassPid, 'new');
class_send(ClassPid, 'new:', _Args) when ClassPid =:= self() ->
    handle_calling_self_error(ClassPid, 'new:');
class_send(ClassPid, spawn, _Args) when ClassPid =:= self() ->
    handle_calling_self_error(ClassPid, spawn);
class_send(ClassPid, 'spawnWith:', _Args) when ClassPid =:= self() ->
    handle_calling_self_error(ClassPid, 'spawnWith:');
class_send(ClassPid, 'new', []) ->
    unwrap_class_call(gen_server:call(ClassPid, {new, []}));
class_send(ClassPid, 'new:', [Map]) ->
    unwrap_class_call(gen_server:call(ClassPid, {new, [Map]}));
class_send(ClassPid, spawn, []) ->
    unwrap_class_call(gen_server:call(ClassPid, {spawn, []}));
class_send(ClassPid, 'spawnWith:', [Map]) ->
    unwrap_class_call(gen_server:call(ClassPid, {spawn, [Map]}));
%% ADR 0032 Phase 2 (BT-734): 8 of the originally planned 12 hardcoded selector
%% clauses were removed here (methods, superclass, class_name, module_name,
%% printString, class, subclasses, allSubclasses); they now dispatch through the
%% Class/Behaviour chain via the catch-all below.
%%
%% The remaining 4 instantiation selectors (new, new:, spawn, spawnWith:) retain
%% explicit clauses because they must route to the gen_server's {new, _} / {spawn, _}
%% handlers, not to class_method_call. Moving them to the Behaviour/Class chain
%% is future work (ADR 0032 Phase 4+).
class_send(ClassPid, Selector, Args) ->
    %% BT-411: Try user-defined class methods before raising does_not_understand
    %% BT-440: Test execution may take a long time; use longer timeout.
    Timeout =
        case is_test_execution_selector(Selector) of
            % 5 minutes for test suites
            true -> 300000;
            % default gen_server timeout
            false -> 5000
        end,
    case gen_server:call(ClassPid, {class_method_call, Selector, Args}, Timeout) of
        {ok, Result} ->
            Result;
        {error, not_found} ->
            ClassName = gen_server:call(ClassPid, class_name),
            %% ADR 0032 Phase 0 (BT-732): Try Class chain before raising does_not_understand.
            %% Class objects are instances of 'Class'; fall through to Class instance methods.
            ModuleName = gen_server:call(ClassPid, module_name),
            ClassSelf = #beamtalk_object{
                class = beamtalk_class_registry:class_object_tag(ClassName),
                class_mod = ModuleName,
                pid = ClassPid
            },
            case try_class_chain_fallthrough(ClassSelf, Selector, Args) of
                {ok, Result} ->
                    Result;
                not_found ->
                    Error = beamtalk_error:new(
                        does_not_understand,
                        ClassName,
                        Selector,
                        <<"Class does not understand this message">>
                    ),
                    beamtalk_error:raise(Error)
            end;
        Other ->
            unwrap_class_call(Other)
    end.

%% @doc Send a message to a metaclass object (ADR 0036 Phase 1, BT-802).
%%
%% Routes messages on metaclass objects (tagged `class='Metaclass'`) through:
%%   1. `{metaclass_method_call, Selector, Args}` to the class gen_server — returns
%%      `{error, not_found}` for the Phase 1 stub (user-defined metaclass methods
%%      are not yet supported).
%%   2. Fallthrough to `beamtalk_dispatch:lookup/5` starting at 'Metaclass', which
%%      walks the Metaclass → Class → Behaviour → Object → ProtoObject chain.
%%
%% The class pid is the same as the described class (virtual tag approach, ADR 0013).
%% Self carries `class='Metaclass'` so method dispatch receives the correct receiver.
-spec metaclass_send(pid(), atom(), list(), #beamtalk_object{}) -> term().
metaclass_send(Pid, Selector, Args, Self) ->
    %% BT-822: Try user-defined class methods first so that `self species withAll: x`
    %% (and similar dynamic class-side sends) can find methods like `withAll:` that
    %% are defined via `class withAll: ...` in Beamtalk stdlib modules.
    %% Falls through to the Metaclass chain for built-in messages (`new`, `class`, etc.).
    case gen_server:call(Pid, {class_method_call, Selector, Args}) of
        {ok, Result} ->
            Result;
        {error, not_found} ->
            case beamtalk_dispatch:lookup(Selector, Args, Self, #{}, 'Metaclass') of
                {reply, Result, _NewState} ->
                    Result;
                {error, #beamtalk_error{kind = does_not_understand}} ->
                    ClassName = gen_server:call(Pid, class_name),
                    Error = beamtalk_error:new(
                        does_not_understand,
                        ClassName,
                        Selector,
                        <<"Metaclass does not understand this message">>
                    ),
                    beamtalk_error:raise(Error);
                {error, #beamtalk_error{kind = class_not_found}} ->
                    %% 'Metaclass' process not registered — graceful fallback.
                    Error = beamtalk_error:new(does_not_understand, 'Metaclass', Selector),
                    beamtalk_error:raise(Error);
                {error, Error} ->
                    %% A real error from a method in the chain. Wrap idempotently.
                    Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
                    error(Wrapped)
            end;
        Other ->
            unwrap_class_call(Other)
    end.

%% @doc Unwrap a class gen_server call result for use in class_send.
%%
%% Translates {ok, Value} → Value, {error, Error} → re-raise as exception.
%% Handles both raw #beamtalk_error{} records and already-wrapped Exception
%% maps (from raise/1 inside handle_call). Uses ensure_wrapped/1 for
%% idempotent wrapping (BT-525).
-spec unwrap_class_call(term()) -> term().
unwrap_class_call({ok, Value}) ->
    Value;
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
%% ADR 0032 Phase 1: Receives local class_methods (not flattened table).
%% Walks the superclass chain if the method is not found locally.
%%
%% Returns {reply, Result, NewState} or test_spawn or {error, not_found}.
-spec handle_class_method_call(
    selector(),
    list(),
    class_name(),
    atom(),
    #{selector() => term()},
    map()
) -> {reply, term(), map()} | test_spawn | {error, not_found}.
handle_class_method_call(Selector, Args, ClassName, Module, LocalClassMethods, ClassVars) ->
    case maps:is_key(Selector, LocalClassMethods) of
        true ->
            %% Local class method found — invoke directly.
            invoke_class_method(Selector, Args, ClassName, Module, ClassName, Module, ClassVars);
        false ->
            %% Not found locally — walk the superclass chain for inherited class methods.
            case find_class_method_in_chain(Selector, ClassName) of
                {ok, DefiningClass, DefiningModule} ->
                    invoke_class_method(
                        Selector, Args, ClassName, Module, DefiningClass, DefiningModule, ClassVars
                    );
                not_found ->
                    {error, not_found}
            end
    end.

%% @private
%% @doc Invoke a class method (local or inherited), handling test execution specially.
-spec invoke_class_method(
    selector(),
    list(),
    class_name(),
    atom(),
    class_name(),
    atom(),
    map()
) ->
    {reply, term(), map()} | test_spawn.
invoke_class_method(Selector, Args, ClassName, _Module, DefiningClass, DefiningModule, ClassVars) ->
    %% BT-440: For test execution (runAll, run:) inherited from TestCase,
    %% return a spawn request so the gen_server can handle noreply.
    case is_test_execution_selector(Selector) andalso DefiningClass =:= 'TestCase' of
        true ->
            test_spawn;
        false ->
            %% Build class self object for `self` reference in class methods
            ClassSelf = #beamtalk_object{
                class = beamtalk_class_registry:class_object_tag(ClassName),
                class_mod = DefiningModule,
                pid = self()
            },
            FunName = class_method_fun_name(Selector),
            %% BT-412: Pass class variables; handle {Result, NewClassVars} returns
            try erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args]) of
                {class_var_result, Result, NewClassVars} ->
                    {reply, {ok, Result}, NewClassVars};
                Result ->
                    {reply, {ok, Result}, ClassVars}
            catch
                Class:Error:ST ->
                    ?LOG_ERROR(
                        "Class method ~p:~p failed: ~p:~p",
                        [ClassName, Selector, Class, Error],
                        #{
                            class => ClassName,
                            selector => Selector,
                            error_class => Class,
                            error => Error,
                            stacktrace => ST
                        }
                    ),
                    {reply, {error, Error}, ClassVars}
            end
    end.

%% @private
%% @doc Walk the superclass chain to find an inherited class method.
%%
%% Uses the ETS hierarchy table for O(1) superclass lookup per level.
%% Makes gen_server calls to each ancestor to check its local class_methods.
%% Safe to call from within a gen_server handle_call because we only walk
%% UP the hierarchy (no circular dependency possible).
-spec find_class_method_in_chain(selector(), class_name()) ->
    {ok, class_name(), atom()} | not_found.
find_class_method_in_chain(Selector, ClassName) ->
    SuperclassName = superclass_from_ets(ClassName),
    find_class_method_in_ancestors(Selector, SuperclassName, 0).

-spec find_class_method_in_ancestors(selector(), class_name() | none, non_neg_integer()) ->
    {ok, class_name(), atom()} | not_found.
find_class_method_in_ancestors(_Selector, none, _Depth) ->
    not_found;
find_class_method_in_ancestors(_Selector, AncestorName, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    ?LOG_WARNING(
        "find_class_method_in_ancestors: max hierarchy depth ~p exceeded at ~p — possible cycle",
        [?MAX_HIERARCHY_DEPTH, AncestorName]
    ),
    not_found;
find_class_method_in_ancestors(Selector, AncestorName, Depth) ->
    case beamtalk_class_registry:whereis_class(AncestorName) of
        undefined ->
            not_found;
        AncestorPid ->
            %% Query ancestor's local class_methods
            AncestorClassMethods =
                try
                    gen_server:call(AncestorPid, get_local_class_methods, 5000)
                catch
                    Class:Reason ->
                        ?LOG_WARNING(
                            "Class chain walk: failed to query ~p class_methods: ~p:~p",
                            [AncestorName, Class, Reason]
                        ),
                        #{}
                end,
            case maps:is_key(Selector, AncestorClassMethods) of
                true ->
                    AncestorModule =
                        try
                            gen_server:call(AncestorPid, module_name, 5000)
                        catch
                            Class2:Reason2 ->
                                ?LOG_WARNING(
                                    "Class chain walk: failed to get module for ~p: ~p:~p",
                                    [AncestorName, Class2, Reason2]
                                ),
                                undefined
                        end,
                    case AncestorModule of
                        undefined ->
                            %% Class has method in metadata but no module — skip and continue up.
                            Next = superclass_from_ets(AncestorName),
                            find_class_method_in_ancestors(Selector, Next, Depth + 1);
                        _ ->
                            {ok, AncestorName, AncestorModule}
                    end;
                false ->
                    Next = superclass_from_ets(AncestorName),
                    find_class_method_in_ancestors(Selector, Next, Depth + 1)
            end
    end.

%% @private
%% @doc Read the superclass name from the ETS hierarchy table (no gen_server call).
-spec superclass_from_ets(class_name()) -> class_name() | none.
superclass_from_ets(ClassName) ->
    case ets:info(beamtalk_class_hierarchy) of
        undefined ->
            none;
        _ ->
            case ets:lookup(beamtalk_class_hierarchy, ClassName) of
                [{_, Super}] -> Super;
                [] -> none
            end
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

%% @doc Handle async cast message dispatch for Future protocol.
%%
%% Dispatches class query messages asynchronously, resolving or rejecting
%% the associated Future process.
%%
%% ADR 0032 Phase 1: Receives local instance_methods (not flattened table).
%% The methods response returns local-only selectors; full hierarchy walk
%% will be done by Behaviour.methods in Phase 2.
-spec handle_async_dispatch(
    term(),
    class_name(),
    #{selector() => term()},
    class_name() | none,
    atom()
) -> ok.
handle_async_dispatch({Selector, Args, FuturePid}, ClassName, InstanceMethods, Superclass, Module) ->
    case {Selector, Args} of
        {methods, []} ->
            FuturePid ! {resolve, maps:keys(InstanceMethods)};
        {superclass, []} ->
            Resolved =
                case Superclass of
                    none -> nil;
                    _ -> Superclass
                end,
            FuturePid ! {resolve, Resolved};
        {class_name, []} ->
            FuturePid ! {resolve, ClassName};
        {module_name, []} ->
            FuturePid ! {resolve, Module};
        {{method, _MethodSelector}, _} ->
            Error = beamtalk_error:new(type_error, ClassName),
            FuturePid ! {reject, Error};
        _ ->
            Error = beamtalk_error:new(does_not_understand, ClassName, Selector),
            FuturePid ! {reject, Error}
    end,
    ok;
handle_async_dispatch(_Msg, _ClassName, _InstanceMethods, _Superclass, _Module) ->
    ok.

%%% ============================================================================
%%% Internal Functions — BT-755
%%% ============================================================================

%% @private
%% @doc Raise a clean error when a class method sends an instantiation message to its own class.
%%
%% BT-755: A class method running inside gen_server handle_call cannot call
%% gen_server:call(self(), ...) — OTP detects the self-call and crashes with
%% {calling_self,...}. Detect this upfront and surface a helpful Beamtalk error.
-spec handle_calling_self_error(pid(), atom()) -> no_return().
handle_calling_self_error(ClassPid, Selector) ->
    ClassName = class_name_from_pid(ClassPid),
    Error0 = beamtalk_error:new(instantiation_error, ClassName, Selector),
    Error1 = beamtalk_error:with_hint(
        Error0,
        <<
            "A class method cannot send 'new', 'new:', 'spawn', or 'spawnWith:' "
            "to its own class (would deadlock the class process). "
            "Delegate to a helper class method or use a factory on a superclass."
        >>
    ),
    beamtalk_error:raise(Error1).

%% @private
%% @doc Extract the Beamtalk class name from a class gen_server pid's registered name.
%%
%% Class processes are registered as 'beamtalk_class_<ClassName>'. This strips
%% that prefix to recover the class name for use in error messages.
-spec class_name_from_pid(pid()) -> atom().
class_name_from_pid(ClassPid) ->
    case erlang:process_info(ClassPid, registered_name) of
        {registered_name, RegName} ->
            RegStr = atom_to_list(RegName),
            Prefix = "beamtalk_class_",
            case lists:prefix(Prefix, RegStr) of
                true ->
                    Suffix = lists:nthtail(length(Prefix), RegStr),
                    try
                        list_to_existing_atom(Suffix)
                    catch
                        error:badarg -> unknown
                    end;
                false ->
                    unknown
            end;
        _ ->
            unknown
    end.

%%% ============================================================================
%%% Internal Functions — ADR 0032 Phase 0
%%% ============================================================================

%% @doc Try dispatching through the Class chain (ADR 0032 Phase 0, BT-732).
%%
%% When a class-side message is not found in user-defined class methods,
%% fall through to instance methods of 'Class'. This implements the
%% Smalltalk metaclass protocol: every class object is an instance of 'Class'.
%%
%% ClassSelf has the metaclass tag (e.g., 'Counter class') so methods in
%% 'Class' receive the correct self when invoked.
%%
%% Returns {ok, Result} if the method is found in the Class chain.
%% Returns not_found if 'Class' does not understand the selector (does_not_understand).
%% Re-raises any other error (type_error, arity_mismatch, etc.) from Class methods
%% so callers see real errors rather than a misleading DNU on the original class.
%%
%% NOTE: NewState from Class methods is intentionally dropped. Class has no
%% persistent instance state in Phase 0-1; if stateful Class methods are added
%% in Phase 2+, this must be revisited.
-spec try_class_chain_fallthrough(#beamtalk_object{}, atom(), list()) ->
    {ok, term()} | not_found.
try_class_chain_fallthrough(ClassSelf, Selector, Args) ->
    %% Class objects have no mutable instance state; use an empty map.
    %% The Class methods receive ClassSelf (with its metaclass tag) as self.
    case beamtalk_dispatch:lookup(Selector, Args, ClassSelf, #{}, 'Class') of
        {reply, Result, _NewState} ->
            ?LOG_DEBUG("Class chain fallthrough succeeded for ~p", [Selector]),
            {ok, Result};
        {error, #beamtalk_error{kind = does_not_understand}} ->
            %% Method not found anywhere in the Class chain — tell caller to raise DNU.
            not_found;
        {error, #beamtalk_error{kind = class_not_found}} ->
            %% 'Class' process not registered (pre-Phase 2 or process died) — graceful fallback.
            not_found;
        {error, Error} ->
            %% A real error from a Class method (type_error, arity_mismatch, etc.).
            %% Error may be a raw #beamtalk_error{} or an already-wrapped Exception map
            %% (dispatch/4 catches and returns wrapped exceptions as {error, Wrapped, State}).
            %% Use ensure_wrapped for idempotent wrapping before re-raising.
            Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
            error(Wrapped)
    end.
