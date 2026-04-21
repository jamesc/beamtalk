%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch).

%%% **DDD Context:** Object System Context

-moduledoc """
Class method dispatch for Beamtalk class objects.

Handles class-level message sending protocol, translating Beamtalk messages
to gen_server calls. Also provides helpers for class method execution
including test execution detection and result unwrapping.

ADR 0032 Phase 0 (BT-732): Added class chain fallthrough.
When a class-side message is not found in user-defined class methods,
dispatch falls through to 'Class' instance methods via beamtalk_dispatch:lookup/5.

Extracted from beamtalk_object_class.erl (BT-704).
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    class_send/3,
    class_self_dispatch/4,
    metaclass_send/4,
    unwrap_class_call/1,
    class_method_fun_name/1,
    is_test_execution_selector/1,
    handle_class_method_call/6,
    handle_async_dispatch/5
]).

-type selector() :: atom().
-type class_name() :: atom().

-doc """
Send a message to a class object synchronously (BT-246 / ADR 0013 Phase 1).

Dispatches messages to the class gen_server, translating the Beamtalk
message protocol ({Selector, Args}) to the class process message format.
Unwraps {ok, Value} / {error, Error} results for seamless integration.
""".
-spec class_send(pid() | undefined, atom(), list()) -> term().
class_send(undefined, Selector, _Args) ->
    Error = beamtalk_error:new(class_not_found, unknown, Selector),
    beamtalk_error:raise(Error);
%% BT-893: Self-call for instantiation selectors — bypass gen_server to avoid deadlock.
%% When a class method sends new/new:/spawn/spawnWith: to its own class (ClassPid == self()),
%% gen_server:call(self(), ...) would deadlock. Instead, perform instantiation directly
%% using class metadata from the process dictionary (set during init).
class_send(ClassPid, 'new', Args) when ClassPid =:= self() ->
    handle_self_instantiation(new, 'new', Args);
class_send(ClassPid, 'new:', Args) when ClassPid =:= self() ->
    handle_self_instantiation(new, 'new:', Args);
class_send(ClassPid, spawn, Args) when ClassPid =:= self() ->
    handle_self_instantiation(spawn, spawn, Args);
class_send(ClassPid, 'spawnWith:', Args) when ClassPid =:= self() ->
    handle_self_instantiation(spawn, 'spawnWith:', Args);
class_send(ClassPid, 'new', []) ->
    class_send_with_recovery(ClassPid, 'new', fun(P) ->
        unwrap_class_call(gen_server:call(P, {new, []}))
    end);
class_send(ClassPid, 'new:', [Map]) ->
    class_send_with_recovery(ClassPid, 'new:', fun(P) ->
        unwrap_class_call(gen_server:call(P, {new, [Map]}))
    end);
class_send(ClassPid, spawn, []) ->
    class_send_with_recovery(ClassPid, spawn, fun(P) ->
        unwrap_class_call(gen_server:call(P, {spawn, []}))
    end);
class_send(ClassPid, 'spawnWith:', [Map]) ->
    class_send_with_recovery(ClassPid, 'spawnWith:', fun(P) ->
        unwrap_class_call(gen_server:call(P, {spawn, [Map]}))
    end);
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
    class_send_with_recovery(ClassPid, Selector, fun(P) ->
        class_send_dispatch(P, Selector, Args)
    end).

-doc """
BT-2007: Dispatch an inherited class method from inside a class method body.

Codegen emits a call to this helper for every class-method self-send
whose selector is not a local class method, a slot constructor, an
instantiation intrinsic, or one of the auto-generated 0-arity exports.
Walks the superclass chain via `find_class_method_in_chain/2`, then
applies `DefiningModule:class_<Selector>(ClassSelf, ClassVars, Args...)`
directly in the caller's process — mirroring the gen_server path's
`invoke_class_method/7` minus the `{reply, ...}` wrapping and the
`test_spawn` branch (which only fires for `TestCase>>runAll` / `run:`
dispatched from an external caller, not from inside a class method).

`ClassSelf#beamtalk_object.class_mod` is set to the *defining* module
so Newspeak-style self-sends inside the inherited method resolve against
the class that actually contains the code (same rule as the gen_server
path at line 316).

Returns `{class_var_result, Result, NewClassVars}` when the inherited
method mutated class vars, or the plain `Result` otherwise — the shape
codegen already unwraps for local class-method self-sends. Raises
structured `does_not_understand` if no ancestor defines the selector.
""".
-spec class_self_dispatch(class_name(), selector(), map(), list()) ->
    {class_var_result, term(), map()} | term() | no_return().
class_self_dispatch(ClassName, Selector, ClassVars, Args) ->
    case find_class_method_in_chain(Selector, ClassName) of
        {ok, _DefiningClass, DefiningModule} ->
            ClassSelf = #beamtalk_object{
                class = beamtalk_class_registry:class_object_tag(ClassName),
                class_mod = DefiningModule,
                pid = self()
            },
            FunName = class_method_fun_name(Selector),
            erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args]);
        not_found ->
            raise_class_self_dnu(ClassName, Selector)
    end.

-spec raise_class_self_dnu(class_name(), selector()) -> no_return().
raise_class_self_dnu(ClassName, Selector) ->
    Hint = iolist_to_binary(
        io_lib:format(
            "Class '~s' has no class method '~s' and no ancestor defines it",
            [ClassName, Selector]
        )
    ),
    Error = beamtalk_error:new(does_not_understand, ClassName, Selector, Hint),
    beamtalk_error:raise(Error).

-doc "Dispatch a class method call (the main logic for the catch-all clause).".
-spec class_send_dispatch(pid(), selector(), list()) -> term().
class_send_dispatch(ClassPid, Selector, Args) ->
    %% BT-411: Try user-defined class methods before raising does_not_understand
    %% BT-440: Test execution may take a long time; use longer timeout.
    %% Class methods can do arbitrary I/O (HTTP, file, database), so the default
    %% must accommodate network latency.  Test selectors get 5 minutes because a
    %% full suite can run hundreds of tests sequentially.
    Timeout =
        case is_test_execution_selector(Selector) of
            % 5 minutes for test suites
            true -> 300000;
            % 60 seconds — class methods may do network I/O
            false -> 60000
        end,
    case gen_server:call(ClassPid, {class_method_call, Selector, Args}, Timeout) of
        {ok, Result} ->
            %% BT-1542 + BT-1994 (ADR 0080 Phase 0a, option 2): run the
            %% initialize: lifecycle hook in the caller's process after a
            %% supervise call returns a freshly-started supervisor tuple.
            %%
            %% After Phase 0a option 2, `beamtalk_supervisor:startLink/1`
            %% returns Result-shaped values:
            %%   {ok, {beamtalk_supervisor_new, ...}}   (fresh start)
            %%   {ok, {beamtalk_supervisor, ...}}       (already_started)
            %%   {error, #beamtalk_error{}}             (failure)
            %% which FFI coercion wraps into a `Result` tagged map. The
            %% stdlib `supervise` method currently calls `.unwrap` on that
            %% Result (preserving the pre-migration user-facing `Supervisor`
            %% type until the full Phase 1 stdlib migration lands), so by
            %% the time the class method body returns, the Result has been
            %% unwrapped to the bare inner tuple.
            %%
            %% We therefore match TWO shapes:
            %%   (a) the bare `{beamtalk_supervisor_new, ...}` tuple — the
            %%       unwrapped case, and the historical BT-1542 case;
            %%   (b) a Result tagged map wrapping the _new inner tuple —
            %%       the post-Phase-1 case where `supervise` no longer
            %%       unwraps and callers handle the Result themselves.
            %% Idempotent `{beamtalk_supervisor, ...}` returns and error
            %% branches flow through unchanged (no initialize: re-run).
            %%
            %% ARCHITECTURAL NOTE: shape (b) couples this generic dispatch
            %% layer to the Beamtalk `Result` internal tagged-map
            %% representation (`$beamtalk_class`, `isOk`, `okValue`). ADR
            %% 0080 §Phase 0a explicitly accepts this tradeoff: the
            %% alternative (option 3: helper process) was shown to
            %% deadlock on existing e2e fixtures during the BT-1994 probe.
            %% If Result's internal representation changes, update both
            %% `beamtalk_result:from_tagged_tuple/1` and this match
            %% together — they are the only two sites that encode the shape.
            case Result of
                {beamtalk_supervisor_new, CN, Mod, Pid} ->
                    SupTuple = {beamtalk_supervisor, CN, Mod, Pid},
                    beamtalk_supervisor:run_initialize(SupTuple),
                    SupTuple;
                #{
                    '$beamtalk_class' := 'Result',
                    isOk := true,
                    okValue := {beamtalk_supervisor_new, CN, Mod, Pid}
                } = ResultMap ->
                    SupTuple = {beamtalk_supervisor, CN, Mod, Pid},
                    beamtalk_supervisor:run_initialize(SupTuple),
                    ResultMap#{okValue := SupTuple};
                _ ->
                    Result
            end;
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

-doc """
Send a message to a metaclass object (ADR 0036, BT-823).

Routes messages on metaclass objects (tagged `class='Metaclass'`) through:
  1. `{metaclass_method_call, Selector, Args}` to the class gen_server via
     `gen_server:call/2` — resolves user-defined class methods (e.g. `withAll:`)
     via the superclass chain (ADR-0036 Phase 2).
  2. Fallthrough to `beamtalk_dispatch:lookup/5` starting at 'Metaclass', which
     walks the Metaclass → Class → Behaviour → Object → ProtoObject chain for
     built-in messages (`new`, `class`, etc.).

The class pid is the same as the described class (virtual tag approach, ADR 0013).
Self carries `class='Metaclass'` so method dispatch receives the correct receiver.
""".
-spec metaclass_send(pid(), atom(), list(), #beamtalk_object{}) -> term().
metaclass_send(Pid, Selector, Args, Self) ->
    %% BT-1768: Wrap in crash recovery, same as class_send.
    %% Update Self's pid on recovery so downstream dispatch uses the new process.
    class_send_with_recovery(Pid, Selector, fun(P) ->
        metaclass_send_dispatch(P, Selector, Args, Self#beamtalk_object{pid = P})
    end).

-doc "Dispatch a metaclass method call (the main logic for metaclass_send).".
-spec metaclass_send_dispatch(pid(), atom(), list(), #beamtalk_object{}) -> term().
metaclass_send_dispatch(Pid, Selector, Args, Self) ->
    %% ADR-0036 Phase 2 (BT-823): Use metaclass_method_call so that
    %% `self species withAll: x` and similar dynamic class-side sends find
    %% user-defined class methods (e.g. `withAll:`) via the proper handler.
    %% Falls through to the Metaclass chain for built-in messages (`new`, `class`, etc.).
    case gen_server:call(Pid, {metaclass_method_call, Selector, Args}, 60000) of
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

-doc """
Unwrap a class gen_server call result for use in class_send.

Translates {ok, Value} → Value, {error, Error} → re-raise as exception.
Handles both raw #beamtalk_error{} records and already-wrapped Exception
maps (from raise/1 inside handle_call). Uses ensure_wrapped/1 for
idempotent wrapping (BT-525).
""".
-spec unwrap_class_call(term()) -> term().
unwrap_class_call({ok, Value}) ->
    Value;
unwrap_class_call({error, Error}) ->
    Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
    error(Wrapped).

-doc """
Handle a class method call from the gen_server.

BT-411: User-defined class method dispatch.
BT-412: Passes class variables to method and handles updates.
BT-440: For test execution (runAll, run:), spawns in a separate process
to avoid gen_server deadlock.

ADR 0032 Phase 1: Receives local class_methods (not flattened table).
Walks the superclass chain if the method is not found locally.

Returns {reply, Result, NewState} or test_spawn or {error, not_found}.
""".
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

-doc "Invoke a class method (local or inherited), handling test execution specially.".
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
                error:undef:ST ->
                    case is_dispatch_undef(DefiningModule, FunName, ST) of
                        {true, module_not_loaded} ->
                            Hint =
                                case ClassName =:= DefiningClass of
                                    true ->
                                        iolist_to_binary(
                                            io_lib:format(
                                                "Class '~s' is not registered - the module '~s' may have failed to load",
                                                [ClassName, DefiningModule]
                                            )
                                        );
                                    false ->
                                        iolist_to_binary(
                                            io_lib:format(
                                                "Class '~s' inherits this method from '~s' - that class's module '~s' may have failed to load",
                                                [ClassName, DefiningClass, DefiningModule]
                                            )
                                        )
                                end,
                            Error = beamtalk_error:new(class_not_found, ClassName, Selector, Hint),
                            {reply, {error, Error}, ClassVars};
                        {true, method_not_found} ->
                            Hint =
                                case ClassName =:= DefiningClass of
                                    true ->
                                        iolist_to_binary(
                                            io_lib:format(
                                                "Class method '~s' not found on '~s' (arity mismatch or undefined selector)",
                                                [Selector, ClassName]
                                            )
                                        );
                                    false ->
                                        iolist_to_binary(
                                            io_lib:format(
                                                "Class method '~s' inherited from '~s' not found on '~s' (arity mismatch or undefined selector)",
                                                [Selector, DefiningClass, ClassName]
                                            )
                                        )
                                end,
                            Error = beamtalk_error:new(
                                does_not_understand, ClassName, Selector, Hint
                            ),
                            {reply, {error, Error}, ClassVars};
                        false ->
                            %% undef was raised inside the method body, not at dispatch.
                            ?LOG_ERROR(
                                "Class method ~p:~p raised undef internally",
                                [ClassName, Selector],
                                #{
                                    class => ClassName,
                                    selector => Selector,
                                    stacktrace => ST,
                                    domain => [beamtalk, runtime]
                                }
                            ),
                            {reply, {error, undef}, ClassVars}
                    end;
                ErrClass:Error:ErrST ->
                    ?LOG_ERROR(
                        "Class method ~p:~p failed",
                        [ClassName, Selector],
                        #{
                            class => ClassName,
                            selector => Selector,
                            reason => beamtalk_error:format_reason(ErrClass, Error),
                            stacktrace => ErrST,
                            domain => [beamtalk, runtime]
                        }
                    ),
                    {reply, {error, Error}, ClassVars}
            end
    end.

-doc """
Walk the superclass chain to find an inherited class method.

Uses the ETS hierarchy table for O(1) superclass lookup per level.
Makes gen_server calls to each ancestor to check its local class_methods.
Safe to call from within a gen_server handle_call because we only walk
UP the hierarchy (no circular dependency possible).
""".
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
        [?MAX_HIERARCHY_DEPTH, AncestorName],
        #{domain => [beamtalk, runtime]}
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
                    Class:Reason:Stack ->
                        ?LOG_WARNING(
                            "Class chain walk: failed to query ~p class_methods: ~p:~p",
                            [AncestorName, Class, Reason],
                            #{domain => [beamtalk, runtime], stacktrace => Stack}
                        ),
                        #{}
                end,
            case maps:is_key(Selector, AncestorClassMethods) of
                true ->
                    AncestorModule =
                        try
                            %% BT-893: Use module_name/1 (not gen_server:call directly) so
                            %% the self-call guard fires if AncestorPid happens to be self().
                            beamtalk_object_class:module_name(AncestorPid)
                        catch
                            Class2:Reason2:Stack2 ->
                                ?LOG_WARNING(
                                    "Class chain walk: failed to get module for ~p: ~p:~p",
                                    [AncestorName, Class2, Reason2],
                                    #{domain => [beamtalk, runtime], stacktrace => Stack2}
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

-doc "Read the superclass name from the hierarchy table (no gen_server call).".
-spec superclass_from_ets(class_name()) -> class_name() | none.
superclass_from_ets(ClassName) ->
    case beamtalk_class_hierarchy_table:lookup(ClassName) of
        {ok, Super} -> Super;
        not_found -> none
    end.

-doc """
Convert a class method selector to its module function name.
Class methods are generated with a 'class_' prefix, e.g.
`class defaultValue => 42` becomes `class_defaultValue/2`.
(Zero-arg selector: arity 0 + ClassSelf + ClassVars => arity 2.)

Uses list_to_atom rather than list_to_existing_atom because the class_
prefixed atom may not yet be in the atom table (e.g. when the module is
not loaded or the method is absent).  Atom exhaustion is not a concern:
selectors are bound atoms from user programs, so the class_ versions are
equally bounded.  If the resulting function does not exist in the module,
erlang:apply will raise undef, which invoke_class_method already handles.
""".
-spec class_method_fun_name(selector()) -> atom().
class_method_fun_name(Selector) ->
    % elp:fixme W0023 intentional atom creation
    list_to_atom("class_" ++ atom_to_list(Selector)).

-doc """
Check if a class method selector is a test execution command.
BT-440: These selectors need special handling to avoid gen_server deadlock.
""".
-spec is_test_execution_selector(selector()) -> boolean().
is_test_execution_selector(runAll) -> true;
is_test_execution_selector('runAll:') -> true;
is_test_execution_selector('run:') -> true;
is_test_execution_selector('run:method:') -> true;
is_test_execution_selector(_) -> false.

-doc """
Handle async cast message dispatch for Future protocol.

Dispatches class query messages asynchronously, resolving or rejecting
the associated Future process.

ADR 0032 Phase 1: Receives local instance_methods (not flattened table).
The methods response returns local-only selectors; full hierarchy walk
will be done by Behaviour.methods in Phase 2.
""".
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

-doc """
Classify an `undef` error as either a dispatch-level failure or one
raised from inside the method body.

When `erlang:apply(Module, Fun, Args)` is called and Module:Fun/Arity does
not exist, the top stacktrace frame is `{Module, Fun, ActualArgs, []}`.
If the undef originated from code inside the method, the top frame belongs
to some other module/function.

Returns:
  `{true, module_not_loaded}` — Module is not loaded; class failed to load.
  `{true, method_not_found}`  — Module is loaded but Fun/Arity is not exported.
  `false`                     — undef was raised from inside the method body.
""".
-spec is_dispatch_undef(atom(), atom(), list()) ->
    {true, module_not_loaded | method_not_found} | false.
is_dispatch_undef(Module, FunName, [{Module, FunName, _, _} | _]) ->
    case code:is_loaded(Module) of
        false -> {true, module_not_loaded};
        _ -> {true, method_not_found}
    end;
is_dispatch_undef(_Module, _FunName, _ST) ->
    false.

%%% ============================================================================
%%% Internal Functions — BT-1768 (Class Process Crash Recovery)
%%% ============================================================================

-doc """
Execute a class_send operation with automatic crash detection and recovery.

BT-1768: Wraps a gen_server call in a try/catch. If the target class process
has crashed (noproc), looks up the class name from the pid reverse index,
attempts auto-restart via `beamtalk_class_registry:restart_class/1`, and
retries the operation with the new pid. If restart fails, raises a clear error.

Only attempts recovery once to avoid infinite loops.
""".
-spec class_send_with_recovery(pid(), selector(), fun((pid()) -> term())) -> term().
class_send_with_recovery(ClassPid, Selector, Action) ->
    try
        Action(ClassPid)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            handle_class_crash_recovery(ClassPid, Selector, Action);
        exit:{noproc, _} ->
            handle_class_crash_recovery(ClassPid, Selector, Action)
    end.

-doc "Attempt to recover from a crashed class process and retry the operation.".
-spec handle_class_crash_recovery(pid(), selector(), fun((pid()) -> term())) -> term().
handle_class_crash_recovery(ClassPid, Selector, Action) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            case beamtalk_class_registry:restart_class(ClassName) of
                {ok, NewPid} ->
                    %% Retry with the restarted class process. Wrap in try/catch
                    %% so that if the restarted process crashes again immediately,
                    %% the caller gets a clean error instead of a raw noproc exit.
                    try
                        Action(NewPid)
                    catch
                        exit:{noproc, _} ->
                            Error2 = beamtalk_error:new(
                                class_not_found,
                                ClassName,
                                Selector,
                                <<"Class process crashed again immediately after restart">>
                            ),
                            beamtalk_error:raise(Error2)
                    end;
                {error, _Reason} ->
                    Error = beamtalk_error:new(
                        class_not_found,
                        ClassName,
                        Selector,
                        <<"Class process crashed and could not be restarted">>
                    ),
                    beamtalk_error:raise(Error)
            end;
        not_found ->
            %% Cannot identify the class — raise a clear error.
            Error = beamtalk_error:new(
                class_not_found,
                unknown,
                Selector,
                <<"Class process is not running (pid not found in registry)">>
            ),
            beamtalk_error:raise(Error)
    end.

%%% ============================================================================
%%% Internal Functions — BT-893 (Self-Instantiation)
%%% ============================================================================

-doc """
Perform instantiation directly when a class method self-sends new/spawn.

BT-893: Replaces BT-755's error-raising approach. Instead of raising an error
when a class method sends new/spawn to itself, we bypass gen_server and call
beamtalk_class_instantiation directly. Class name and module are read from the
process dictionary (set during beamtalk_object_class:init/1).
""".
-spec handle_self_instantiation(new | spawn, atom(), list()) -> term().
handle_self_instantiation(Type, Selector, Args) ->
    ClassName = get(beamtalk_class_name),
    Module = get(beamtalk_class_module),
    IsAbstract0 = get(beamtalk_class_is_abstract),
    case {ClassName, Module, IsAbstract0} of
        {undefined, _, _} ->
            %% Fallback: process dictionary not set — called from non-class process.
            handle_self_instantiation_error(Selector);
        {_, undefined, _} ->
            handle_self_instantiation_error(Selector);
        {_, _, undefined} ->
            handle_self_instantiation_error(Selector);
        {CN, Mod, IsAbstract} when is_boolean(IsAbstract) ->
            case Type of
                new ->
                    beamtalk_class_instantiation:class_self_new(CN, Mod, Args);
                spawn ->
                    beamtalk_class_instantiation:class_self_spawn(CN, Mod, IsAbstract, Args)
            end;
        {_, _, _} ->
            %% Corrupt/invalid is_abstract metadata — fail fast with structured error.
            handle_self_instantiation_error(Selector)
    end.

-doc """
Fallback error when process dictionary metadata is missing.

Preserves the original selector and attempts to extract the class name from
the process's registered name for diagnostic purposes.
""".
-spec handle_self_instantiation_error(atom()) -> no_return().
handle_self_instantiation_error(Selector) ->
    ClassName = class_name_from_pid(self()),
    Error0 = beamtalk_error:new(instantiation_error, ClassName, Selector),
    Error1 = beamtalk_error:with_hint(
        Error0,
        <<
            "Self-instantiation requires class metadata in the process dictionary "
            "(set during class process init). This error indicates the call "
            "originated from a non-class process or the class was not initialized correctly."
        >>
    ),
    beamtalk_error:raise(Error1).

-doc """
Extract the Beamtalk class name from a class gen_server pid's registered name.

Class processes are registered as 'beamtalk_class_<ClassName>'. This strips
that prefix to recover the class name for use in error messages.
""".
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

-doc """
Try dispatching through the Class chain (ADR 0032 Phase 0, BT-732).

When a class-side message is not found in user-defined class methods,
fall through to instance methods of 'Class'. This implements the
Smalltalk metaclass protocol: every class object is an instance of 'Class'.

ClassSelf has the metaclass tag (e.g., 'Counter class') so methods in
'Class' receive the correct self when invoked.

Returns {ok, Result} if the method is found in the Class chain.
Returns not_found if 'Class' does not understand the selector (does_not_understand).
Re-raises any other error (type_error, arity_mismatch, etc.) from Class methods
so callers see real errors rather than a misleading DNU on the original class.

NOTE: NewState from Class methods is intentionally dropped. Class has no
persistent instance state in Phase 0-1; if stateful Class methods are added
in Phase 2+, this must be revisited.
""".
-spec try_class_chain_fallthrough(#beamtalk_object{}, atom(), list()) ->
    {ok, term()} | not_found.
try_class_chain_fallthrough(ClassSelf, Selector, Args) ->
    %% Class objects have no mutable instance state; use an empty map.
    %% The Class methods receive ClassSelf (with its metaclass tag) as self.
    case beamtalk_dispatch:lookup(Selector, Args, ClassSelf, #{}, 'Class') of
        {reply, Result, _NewState} ->
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
