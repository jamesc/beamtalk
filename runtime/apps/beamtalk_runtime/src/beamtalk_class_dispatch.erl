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
    class_self_dispatch_local/4,
    metaclass_send/4,
    unwrap_class_call/1,
    class_method_fun_name/1,
    is_test_execution_selector/1,
    handle_class_method_call/6,
    handle_async_dispatch/5,
    class_understands_class_selector/2
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
%% is future work (ADR 0032 Phase 4+). BT-3071/BT-3072 lifted real class-method
%% bodies for all four onto actor.bt (`class sealed new`/`new:`/`spawn`/
%% `spawnWith:`), but deliberately left this routing untouched — those bodies
%% are the documented, xref-visible definition of dynamic-dispatch behaviour,
%% not the code path an external `Counter spawn` actually runs today.
%% BT-3018: every other selector aimed at the class's own gen_server from
%% inside that gen_server. `gen_server:call(self(), ...)` does not hang — it
%% exits with `{calling_self, {gen_server, call, [...]}}`, a raw tuple that
%% says nothing about what the caller did wrong. The commonest way to get here
%% is a block running inside a class method that messages its own class:
%%
%%     File open: p mode: #read do: [:h | File exists: q]
%%
%% Report it the way BT-2005's `handle_metaclass_self_call/2` already reports
%% the metaclass equivalent. No working path changes: the alternative was an
%% exit, not a successful call.
class_send(ClassPid, Selector, _Args) when ClassPid =:= self() ->
    handle_class_self_call(Selector);
class_send(ClassPid, Selector, Args) ->
    class_send_with_recovery(ClassPid, Selector, fun(P) ->
        class_send_dispatch(P, Selector, Args)
    end).

-doc """
Report a class-method self-send that would deadlock (BT-3018).

The four instantiation selectors are short-circuited above; anything else
sent to a class from inside that class's own process has no safe route, so
raise a structured `dispatch_error` naming the deadlock instead of letting
`gen_server:call/2` exit with an opaque `calling_self` tuple.
""".
-spec handle_class_self_call(selector()) -> no_return().
handle_class_self_call(Selector) ->
    ClassName =
        case get(beamtalk_class_name) of
            undefined -> 'the class';
            CN -> CN
        end,
    Error0 = beamtalk_error:new(dispatch_error, ClassName, Selector),
    %% Built with atom_to_binary rather than io_lib:format on purpose. A badarg
    %% escaping from *inside* error construction is especially nasty here: the
    %% FFI boundary treats it as a genuine badarg, retries the whole call with
    %% charlist-coerced arguments, and the caller ends up seeing an unrelated
    %% type_error from whichever guard the coerced arguments happen to miss.
    Error1 = beamtalk_error:with_hint(
        Error0,
        iolist_to_binary([
            <<"Sending '">>,
            atom_to_binary(Selector, utf8),
            <<"' to '">>,
            atom_to_binary(ClassName, utf8),
            <<
                "' from inside one of its own class methods would deadlock "
                "(gen_server:call to self). This usually means a block passed to a "
                "class method is messaging that same class again; read what you "
                "need before the call, or hold the resource yourself instead of "
                "using the block form."
            >>
        ])
    ),
    beamtalk_error:raise(Error1).

-doc """
BT-2007: Dispatch an inherited class method from inside a class method body.

Codegen emits a call to this helper for every class-method self-send
whose selector is not a local class method, a slot constructor, an
instantiation intrinsic, or one of the auto-generated 0-arity exports.

BT-3198: Checks the extension registry (keyed under the metaclass tag,
`class_object_tag(ClassName)`) BEFORE the superclass chain — mirroring
`handle_class_method_call/6`'s BT-3192 "extension checked before local
method table / chain" order, so a class-side extension registered on
`ClassName`'s own metaclass tag is reachable via `self extensionSel` from
inside another class method of the same class, not just via an external
`Target sel` / `Target class sel` send. Extensions are never inherited
(same rule BT-3192 established for the external paths), so only
`ClassName`'s own tag is checked here — not walked up the chain.

When the extension registry has no match, walks the superclass chain via
`find_class_method_in_chain/2`, then applies
`DefiningModule:class_<Selector>(ClassSelf, ClassVars, Args...)` directly in
the caller's process — mirroring the gen_server path's `invoke_class_method/7`
minus the `{reply, ...}` wrapping and the `test_spawn` branch (which only
fires for `TestCase>>runAll` / `run:` dispatched from an external caller, not
from inside a class method).

`ClassSelf#beamtalk_object.class_mod` is set to the *defining* module
so Newspeak-style self-sends inside the inherited method resolve against
the class that actually contains the code (same rule as the gen_server
path at line 316).

Returns `{class_var_result, Result, NewClassVars}` when the inherited
method or extension mutated class vars, or the plain `Result` otherwise —
the shape codegen already unwraps for local class-method self-sends. Raises
structured `does_not_understand` if no ancestor and no extension defines
the selector.
""".
-spec class_self_dispatch(class_name(), selector(), map(), list()) ->
    {class_var_result, term(), map()} | term() | no_return().
class_self_dispatch(ClassName, Selector, ClassVars, Args) ->
    case check_class_self_extension(ClassName, Selector, ClassVars, Args) of
        {ok, Outcome} ->
            Outcome;
        not_found ->
            class_self_dispatch_chain(ClassName, Selector, ClassVars, Args)
    end.

-doc """
BT-3198: The superclass-chain half of `class_self_dispatch/4`, factored out
so `class_self_dispatch_local/4` can fall through to it directly after its
own single extension check, instead of re-checking the (already-confirmed
absent) extension a second time via a nested `class_self_dispatch/4` call.
""".
-spec class_self_dispatch_chain(class_name(), selector(), map(), list()) ->
    {class_var_result, term(), map()} | term() | no_return().
class_self_dispatch_chain(ClassName, Selector, ClassVars, Args) ->
    case find_class_method_in_chain(Selector, ClassName) of
        {ok, DefiningClass, DefiningModule} ->
            %% Route through the same internal helper the gen_server path uses
            %% (`invoke_class_method/7`), so both paths share error classification
            %% and the TestCase `test_spawn` guard. Unwrap to raw — codegen's
            %% class_var_result pattern match handles the tuple shape — and raise
            %% structured errors on dispatch/body failures so callers see proper
            %% `#beamtalk_error{}` instead of a bare `undef`.
            unwrap_self_dispatch_outcome(
                ClassName,
                Selector,
                apply_class_method_in_context(
                    Selector, Args, ClassName, DefiningClass, DefiningModule, ClassVars
                )
            );
        not_found ->
            raise_class_self_dnu(ClassName, Selector)
    end.

-doc """
ADR 0084 / BT-2266: Dispatch a class-method self-send that may resolve to the
class's OWN runtime-installed class method.

Class-method funs created by the programmatic `ClassBuilder` (BT-2267) are
anonymous funs with no `class_<sel>` module export, so a self-send inside such a
fun cannot use the compiled direct-call path. It routes here instead.

BT-3198: checks the extension registry first (same tag/priority rule as
`class_self_dispatch/4` above — extension before local method, mirroring
BT-3192), then the class's own runtime class-method fun (the retrieval
store), and only falls back to `class_self_dispatch_chain/4` (super +
inherited, walked from the superclass) when the selector is neither.

Returns the raw `{class_var_result, Result, NewClassVars}` | plain value — the
shape the calling fun threads — and raises a structured `does_not_understand`
when no definition is found. No `gen_server` hop: the local lookup is the same
ETS read the dispatch hot path already uses (BT-2008).
""".
-spec class_self_dispatch_local(class_name(), selector(), map(), list()) ->
    {class_var_result, term(), map()} | term() | no_return().
class_self_dispatch_local(ClassName, Selector, ClassVars, Args) ->
    case check_class_self_extension(ClassName, Selector, ClassVars, Args) of
        {ok, Outcome} ->
            Outcome;
        not_found ->
            case beamtalk_class_metadata:lookup_class_method_fun(ClassName, Selector) of
                {ok, _Info} ->
                    %% Own runtime class method: dispatch with DefiningClass = ClassName
                    %% so apply_class_method_in_context resolves the fun from the store.
                    DefiningModule = self_dispatch_module(ClassName),
                    unwrap_self_dispatch_outcome(
                        ClassName,
                        Selector,
                        apply_class_method_in_context(
                            Selector, Args, ClassName, ClassName, DefiningModule, ClassVars
                        )
                    );
                error ->
                    %% Not a local runtime method — walk the chain (super + inherited),
                    %% which also resolves inherited runtime funs and compiled methods.
                    class_self_dispatch_chain(ClassName, Selector, ClassVars, Args)
            end
    end.

-doc """
BT-3198: Shared extension-registry probe for both `class_self_dispatch/4`
and `class_self_dispatch_local/4` — a `self someSelector` send from inside
another class method, checking whether `ClassName`'s own metaclass tag has a
matching class-side extension (`beamtalk_extensions`, ADR 0066) before either
function falls through to its local/inherited-method lookups. Mirrors
`handle_class_method_call/6`'s BT-3192 priority order (extension before
local method table).

Reuses `apply_class_extension_fun/6` for the same calling convention and
crash-safety as the external-dispatch path (a bad extension body must not
take down the class's own long-lived gen_server, which this self-send also
runs inside), then adapts the outcome to the self-dispatch caller shape via
`unwrap_self_dispatch_extension_outcome/3` — raw value/`{class_var_result,
...}` tuple on success, structured raises on failure.

Returns `{ok, Outcome}` when an extension matched (`Outcome` already
unwrapped and ready to return to the codegen call site), or `not_found` so
the caller proceeds to its own next lookup.
""".
-spec check_class_self_extension(class_name(), selector(), map(), list()) ->
    {ok, {class_var_result, term(), map()}} | not_found | no_return().
check_class_self_extension(ClassName, Selector, ClassVars, Args) ->
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    case beamtalk_dispatch:check_extension(ClassTag, Selector) of
        {ok, Fun} ->
            Module = self_dispatch_module(ClassName),
            ClassSelf = #beamtalk_object{class = ClassTag, class_mod = Module, pid = self()},
            {ok,
                unwrap_self_dispatch_extension_outcome(
                    ClassName,
                    Selector,
                    apply_class_extension_fun(Fun, ClassSelf, ClassVars, Args, ClassName, Selector)
                )};
        not_found ->
            not_found
    end.

-doc "The compiled module backing `ClassName`, for a `ClassSelf` built outside the gen_server state.".
-spec self_dispatch_module(class_name()) -> atom().
self_dispatch_module(ClassName) ->
    case beamtalk_class_metadata:lookup_module(ClassName) of
        {ok, M} -> M;
        not_found -> ClassName
    end.

-doc """
BT-3198: Adapt `apply_class_extension_fun/6`'s outcome to the self-dispatch
caller shape — mirroring `unwrap_self_dispatch_outcome/3` for compiled/
runtime-installed class methods, but for the `{ok, {Result, NewClassVars}}`
2-tuple `apply_extension_by_arity/4` returns rather than the plain `{ok,
Raw}` a method body returns directly. Always threads `NewClassVars` (via the
`class_var_result` tuple codegen already unwraps) since, unlike a compiled
method body, an extension fun has no ADR-0110 shadow-write mechanism to rely
on instead — the returned tuple is the only way a 3-arity actor extension's
class-var mutation gets back to the caller.
""".
-spec unwrap_self_dispatch_extension_outcome(
    class_name(),
    selector(),
    {ok, {term(), map()}}
    | {nlr_relay, term(), list()}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}
) -> {class_var_result, term(), map()} | no_return().
unwrap_self_dispatch_extension_outcome(_ClassName, _Selector, {ok, {Result, NewClassVars}}) ->
    {class_var_result, Result, NewClassVars};
unwrap_self_dispatch_extension_outcome(_ClassName, _Selector, {error, undef_in_body}) ->
    %% Preserve the `error:undef` contract — see unwrap_self_dispatch_outcome/3.
    erlang:error(undef);
unwrap_self_dispatch_extension_outcome(
    _ClassName, _Selector, {error, {raised, ErrClass, Error, ST}}
) ->
    erlang:raise(ErrClass, Error, ST);
unwrap_self_dispatch_extension_outcome(_ClassName, _Selector, {nlr_relay, Nlr, ST}) ->
    %% ADR 0110 / BT-3032: resume the non-local return unwind directly, same
    %% as unwrap_self_dispatch_outcome/3's nlr_relay clause.
    erlang:raise(throw, Nlr, ST).

-doc """
Adapt a `class_method_outcome()` from `apply_class_method_in_context/6` to the
self-dispatch caller shape: raw value on success, structured raises on failure.
Shared by `class_self_dispatch/4` and `class_self_dispatch_local/4`.
""".
-spec unwrap_self_dispatch_outcome(class_name(), selector(), class_method_outcome()) ->
    term() | no_return().
unwrap_self_dispatch_outcome(ClassName, Selector, Outcome) ->
    case Outcome of
        {ok, Raw} ->
            Raw;
        {error, #beamtalk_error{} = Error} ->
            beamtalk_error:raise(Error);
        {error, undef_in_body} ->
            %% Preserve the `error:undef` contract for callers that rely on
            %% it (e.g. user code catching `error:undef` from a method body).
            erlang:error(undef);
        {error, {raised, ErrClass, Error, ST}} ->
            erlang:raise(ErrClass, Error, ST);
        {nlr_relay, Nlr, ST} ->
            %% ADR 0110 / BT-3032: self-dispatch runs in the caller's own
            %% process, so resume the non-local return unwind directly —
            %% identical behavior to when these throws were tagged
            %% {error, {raised, throw, ...}}.
            erlang:raise(throw, Nlr, ST);
        test_spawn ->
            %% `TestCase>>runAll` / `run:` self-sent from inside another
            %% class method would run the test suite inline in the class
            %% gen_server process — the exact deadlock the `test_spawn`
            %% escape hatch was added to avoid (BT-440). Surface a
            %% structured error instead of blocking; callers should invoke
            %% test runners from outside the class body.
            raise_test_spawn_self_dispatch_unsupported(ClassName, Selector)
    end.

-spec raise_test_spawn_self_dispatch_unsupported(class_name(), selector()) -> no_return().
raise_test_spawn_self_dispatch_unsupported(ClassName, Selector) ->
    %% Atom-safe formatting — see note on raise_class_self_dnu/2.
    Hint = iolist_to_binary(
        io_lib:format(
            "Inherited test-execution selector '~ts' cannot be self-sent from "
            "inside a class method on '~ts' — it would deadlock the class "
            "gen_server. Call `~ts ~ts` from outside the class body instead.",
            [
                atom_to_list(Selector),
                atom_to_list(ClassName),
                atom_to_list(ClassName),
                atom_to_list(Selector)
            ]
        )
    ),
    Error = beamtalk_error:new(does_not_understand, ClassName, Selector, Hint),
    beamtalk_error:raise(Error).

-spec raise_class_self_dnu(class_name(), selector()) -> no_return().
raise_class_self_dnu(ClassName, Selector) ->
    %% ClassName and Selector are atoms; use ~ts (unicode string) with
    %% atom_to_list/1 rather than ~s, which expects an iolist and raises
    %% badarg on atoms — that would mask the intended does_not_understand.
    Hint = iolist_to_binary(
        io_lib:format(
            "Class '~ts' has no class method '~ts' and no ancestor defines it",
            [atom_to_list(ClassName), atom_to_list(Selector)]
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
    %% ADR 0081 / BT-2379: pass the caller's session context explicitly in the
    %% message instead of having the class gen_server copy our whole process
    %% dictionary via `process_info/2`. We read our own two keys cheaply with
    %% `get/1` here and the receiving clause seeds from the tuple.
    case
        gen_server:call(
            ClassPid, {class_method_call, Selector, Args, local_session_context()}, Timeout
        )
    of
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
        {error, {beamtalk_script_exit, _} = ScriptExit} ->
            %% BT-2691 (ADR 0099 §3): a connected `Program exit: N` raised inside
            %% the dispatched class method surfaces here as the class gen_server's
            %% error reply. Re-raise it as the `{beamtalk_script_exit, N}` *throw*
            %% in the caller's (eval/dispatch worker's) process, so that worker
            %% reports the status and ends the session — instead of
            %% unwrap_class_call/1 wrapping it as an ordinary method failure.
            throw(ScriptExit);
        {error, Nlr} when ?IS_NLR(Nlr) ->
            %% BT-3022: a `^` inside a block that this class method ran on our
            %% behalf. The matching catch frame is in *our* process — the class
            %% gen_server had no frame holding that token, so its reply is the only
            %% way the signal can get back here. Re-throw so the enclosing method
            %% unwinds as it would have without the process hop; without this the
            %% raw `{'$bt_nlr', ...}` tuple surfaces to user code as an error value.
            throw(Nlr);
        Other ->
            unwrap_class_call(Other)
    end.

-doc """
Send a message to a metaclass object (ADR 0036, BT-823).

Routes messages on metaclass objects (tagged `class='Metaclass'`) through:
  1. `{metaclass_method_call, Selector, Args, SessionCtx}` to the class
     gen_server via `gen_server:call/2` — resolves user-defined class methods
     (e.g. `withAll:`) via the superclass chain (ADR-0036 Phase 2). `SessionCtx`
     carries the caller's session context explicitly (BT-2379).
  2. Fallthrough to `beamtalk_dispatch:lookup/5` starting at 'Metaclass', which
     walks the Metaclass → Class → Behaviour → Object → ProtoObject chain for
     built-in messages (`new`, `class`, etc.).

The class pid is the same as the described class (virtual tag approach, ADR 0013).
Self carries `class='Metaclass'` so method dispatch receives the correct receiver.
""".
-spec metaclass_send(pid(), atom(), list(), #beamtalk_object{}) -> term().
%% BT-2005: `self class <msg>` inside a class method routes here with
%% Pid =:= self() (the class gen_server calling itself). A plain
%% gen_server:call on the instantiation selectors would deadlock; short-circuit
%% the spawn/new family via the process-dictionary-backed helpers used by
%% BT-893 / BT-2004, and raise a clear error for everything else.
metaclass_send(Pid, Selector, Args, _Self) when Pid =:= self() ->
    handle_metaclass_self_call(Selector, Args);
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
    %% BT-2379: carry the caller's session context explicitly (see
    %% class_send_dispatch/3) so the class gen_server need not copy our dictionary.
    case
        gen_server:call(
            Pid, {metaclass_method_call, Selector, Args, local_session_context()}, 60000
        )
    of
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
                    beamtalk_exception_handler:reraise(Error)
            end;
        {error, {beamtalk_script_exit, _} = ScriptExit} ->
            %% BT-2691 (ADR 0099 §3): re-raise a connected `Program exit:` from a
            %% metaclass-dispatched class method as the script-exit throw (parity
            %% with class_send_dispatch/3), so the worker adopts the status.
            throw(ScriptExit);
        {error, Nlr} when ?IS_NLR(Nlr) ->
            %% BT-3022: same relay as class_send_dispatch/3, and for the same
            %% reason — a metaclass-dispatched class method also runs in the class
            %% gen_server, so a `^` out of a caller-supplied block unwinds to here
            %% with no frame holding its token. Without this clause
            %% `unwrap_class_call/1` reraises it as an ordinary error and the raw
            %% `{'$bt_nlr', ...}` tuple reaches user code.
            %%
            %% Reached via a metaclass receiver — `SomeClass class class`, which
            %% `beamtalk_behaviour_intrinsics:classClass/1` tags `'Metaclass'` so
            %% `beamtalk_primitive:send/3` routes it here. A plain class literal or
            %% a single `class` send goes through `class_send/3` instead, which is
            %% why this needs its own clause rather than sharing that one.
            throw(Nlr);
        Other ->
            unwrap_class_call(Other)
    end.

-doc """
Read this process's session context (`beamtalk_session_pid` /
`beamtalk_session_id` / `beamtalk_session_meta` / `beamtalk_entry_group_leader`)
for explicit propagation into a class-method call.

ADR 0081 / BT-2379: the eval worker seeds these keys
(`beamtalk_repl_shell:seed_session_context/3`). Class-method dispatch hops to
the class gen_server, so factory methods like `Session current` need the
caller's context there — including the origin metadata so `Session current kind`
reports the real client surface rather than `unknown`. We read our own keys
cheaply with `get/1` and pass them in the message tuple, avoiding a
`process_info(CallerPid, dictionary)` full-dictionary copy on the receiving
side.

BT-2963: the fourth element is the *entry group leader* — the IO sink a
connected `beamtalk run … --connect` dispatch wants its `Console` output to
reach. It is seeded only by `beamtalk_repl_eval:do_dispatch/5` (the `run-entry`
op) and is `undefined` everywhere else, including ordinary REPL `eval`, so no
other dispatch path changes where its output goes. Carrying it alongside the
session context makes it propagate transitively: a class method that calls
*another* class method re-emits the key it was seeded with, so nested output
keeps streaming to the same client.

Returns `{undefined, undefined, undefined, undefined}` when this process has no
seeded context (the common non-REPL case).
""".
-spec local_session_context() ->
    {pid() | undefined, binary() | undefined, map() | undefined, pid() | undefined}.
local_session_context() ->
    {
        get(beamtalk_session_pid),
        get(beamtalk_session_id),
        get(beamtalk_session_meta),
        get(beamtalk_entry_group_leader)
    }.

-doc """
Unwrap a class gen_server call result for use in class_send.

Translates {ok, Value} → Value, {error, Error} → re-raise as exception.
Handles both raw #beamtalk_error{} records and already-wrapped Exception
maps (from raise/1 inside handle_call). Uses reraise/1 (which wraps
idempotently) (BT-525).
""".
-spec unwrap_class_call(term()) -> term().
unwrap_class_call({ok, Value}) ->
    Value;
unwrap_class_call({error, Error}) ->
    beamtalk_exception_handler:reraise(Error).

-doc """
Handle a class method call from the gen_server.

BT-411: User-defined class method dispatch.
BT-412: Passes class variables to method and handles updates.
BT-440: For test execution (runAll, run:), spawns in a separate process
to avoid gen_server deadlock.

ADR 0032 Phase 1: Receives local class_methods (not flattened table).
Walks the superclass chain if the method is not found locally.

BT-3192: Checks the extension registry (keyed under the metaclass tag,
`class_object_tag(ClassName)`) BEFORE the local method table / superclass
chain — mirroring `beamtalk_dispatch:lookup/5`'s own "extension checked
before local method table" order. Before this, a class-side extension
(`Target class >> sel => body`, ADR 0066) registered fine but was never read
back by ANY class-side dispatch path: neither `class_method_call` (an
ordinary `Target sel` send) nor `metaclass_method_call` (`Target class sel`)
ever consulted `beamtalk_extensions` — both funnel through this same
function (`beamtalk_object_class:dispatch_class_method/5`), so this one fix
covers both.

Scope: this covers *external* sends (`Target sel` / `Target class sel`). A
`self someSelector` send from inside another class method of the same
class — `class_self_dispatch/4` / `class_self_dispatch_local/4`, below —
goes through `check_class_self_extension/4` instead, which checks the same
registry under the same priority rule (BT-3198).

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
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    case beamtalk_dispatch:check_extension(ClassTag, Selector) of
        {ok, Fun} ->
            invoke_class_extension(Fun, Args, ClassName, ClassTag, Module, ClassVars, Selector);
        not_found ->
            case maps:is_key(Selector, LocalClassMethods) of
                true ->
                    %% Local class method found — invoke directly.
                    invoke_class_method(
                        Selector, Args, ClassName, Module, ClassName, Module, ClassVars
                    );
                false ->
                    %% Not found locally — walk the superclass chain for inherited class methods.
                    case find_class_method_in_chain(Selector, ClassName) of
                        {ok, DefiningClass, DefiningModule} ->
                            invoke_class_method(
                                Selector,
                                Args,
                                ClassName,
                                Module,
                                DefiningClass,
                                DefiningModule,
                                ClassVars
                            );
                        not_found ->
                            {error, not_found}
                    end
            end
    end.

-doc """
Invoke a class-side extension method found in the `beamtalk_extensions`
registry (BT-3192).

Reuses `beamtalk_dispatch:apply_extension_by_arity/4` for the calling
convention — the same one instance-side extensions already use: a 2-arity
fun (value/primitive target) ignores `ClassVars` and returns a plain result;
a 3-arity fun (actor target) threads `ClassVars`. `ClassSelf` mirrors the
receiver `apply_class_method_in_context/6` builds for a local/inherited
class method — `class = ClassTag` (the metaclass tag), `class_mod = Module`
(this call's own compiled module; extensions are never inherited, so there
is no separate "defining class" indirection), `pid = self()` (the class
gen_server this handler is already running inside).

Like `beamtalk_dispatch:invoke_extension/6` (BT-3199), this catches and
converts a crashing extension body rather than letting it escape — but via
`apply_class_extension_fun/6`'s own finer-grained classification
(`undef_in_body` vs. generic, plus NLR-relay / script-exit passthrough for
self-sends inside class methods) rather than the generic
`ensure_wrapped/4` instance-side dispatch uses, matching every other
class-method dispatch path (`apply_class_method_fun/6`,
`apply_compiled_class_method/7`): a bad extension body must not crash the
class's own long-lived gen_server.
""".
-spec invoke_class_extension(fun(), list(), class_name(), atom(), atom(), map(), selector()) ->
    {reply, term(), map()}.
invoke_class_extension(Fun, Args, ClassName, ClassTag, Module, ClassVars, Selector) ->
    ClassSelf = #beamtalk_object{class = ClassTag, class_mod = Module, pid = self()},
    case apply_class_extension_fun(Fun, ClassSelf, ClassVars, Args, ClassName, Selector) of
        {ok, {Result, NewClassVars}} ->
            {reply, {ok, Result}, NewClassVars};
        {nlr_relay, Nlr, _ST} ->
            %% ADR 0110 / BT-3032, adapted for extensions: same relay as
            %% invoke_class_method/7, minus the class-var shadow-key read —
            %% extension codegen never writes that key (only compiled/
            %% runtime-installed class-method bodies do), so ClassVars as-is
            %% is exactly what the shadow read would have fallen back to.
            {reply, {error, Nlr}, ClassVars};
        {error, undef_in_body} ->
            {reply, {error, undef}, ClassVars};
        {error, {raised, _ErrClass, Error, _ST}} ->
            {reply, {error, Error}, ClassVars}
    end.

-doc """
Apply a class-side extension fun, classifying the outcome the same way
`apply_class_method_fun/6` does for a runtime-installed class method
(BT-3192) — script-exit passthrough, NLR relay, `undef` classification, and
a generic catch-all — so a crashing extension body becomes a structured
`{error, ...}` reply instead of taking down the class gen_server.
""".
-spec apply_class_extension_fun(fun(), #beamtalk_object{}, map(), list(), class_name(), selector()) ->
    {ok, {term(), map()}}
    | {nlr_relay, term(), list()}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}.
apply_class_extension_fun(Fun, ClassSelf, ClassVars, Args, ClassName, Selector) ->
    try beamtalk_dispatch:apply_extension_by_arity(Fun, Args, ClassSelf, ClassVars) of
        ResultAndVars ->
            {ok, ResultAndVars}
    catch
        %% BT-2691 (ADR 0099 §3): see apply_class_method_fun/6 — pass a
        %% connected `Program exit: N` through unlogged as a control-flow
        %% signal, not a method failure.
        throw:({beamtalk_script_exit, _} = ScriptExit):ScriptST ->
            {error, {raised, throw, ScriptExit, ScriptST}};
        %% BT-3022: see apply_class_method_fun/6 — a `^` unwinding through
        %% this extension belongs to a frame in the calling process.
        throw:Nlr:NlrST when ?IS_NLR(Nlr) ->
            {nlr_relay, Nlr, NlrST};
        error:undef:ST ->
            ?LOG_ERROR(
                "Class-side extension ~p:~p raised undef internally",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    stacktrace => ST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, undef_in_body};
        ErrClass:Error:ErrST ->
            ?LOG_ERROR(
                "Class-side extension ~p:~p failed",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, {raised, ErrClass, Error, ErrST}}
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
    %% BT-2007: shares apply_class_method_in_context/6 with class_self_dispatch/4
    %% so both dispatch paths see identical error classification, class-var
    %% threading, and the BT-440 test_spawn escape hatch. This function
    %% adapts the shared outcome to the gen_server `{reply, _, State}` shape.
    %%
    %% ADR 0110 / BT-3032: the whole body runs inside try ... after so the
    %% '$bt_class_vars_shadow' process-dictionary key (written by codegen at
    %% top-level class-var mutations) never outlives a dispatch, whatever the
    %% outcome.
    %%
    %% ADR 0110 amendment (BT-3039): the key is tagged with this call's own
    %% class identity (`class_object_tag(ClassName)`, matching codegen's
    %% `element(2, ClassSelf)` write) so a mutating self-send inside a block
    %% invoked from a *different* class's process — which runs physically in
    %% *this* process but writes under its own foreign class's tag — can never
    %% be read back here. See the ADR's Codegen/Runtime change amendments.
    ShadowKey =
        {?BT_CLASS_VARS_SHADOW_KEY_ATOM, beamtalk_class_registry:class_object_tag(ClassName)},
    try
        case
            apply_class_method_in_context(
                Selector, Args, ClassName, DefiningClass, DefiningModule, ClassVars
            )
        of
            test_spawn ->
                test_spawn;
            {ok, {class_var_result, Result, NewClassVars}} ->
                {reply, {ok, Result}, NewClassVars};
            {ok, Result} ->
                {reply, {ok, Result}, ClassVars};
            {nlr_relay, Nlr, _ST} ->
                %% ADR 0110 / BT-3032: a foreign `^` relaying out of the class
                %% method is control flow, not a failure — class-var writes made
                %% before the unwind must survive. The codegen write-through
                %% (BT-3037) records them under this class's shadow key; until
                %% that lands the shadow is never set and this falls back to the
                %% pre-call ClassVars, exactly today's behavior. The reply shape
                %% is byte-identical to the historical {error, Nlr}, so the
                %% ?IS_NLR re-throw clauses in class_send_dispatch/3 and
                %% metaclass_send_dispatch/4 need no change.
                ShadowClassVars =
                    case erlang:get(ShadowKey) of
                        undefined -> ClassVars;
                        Shadow -> Shadow
                    end,
                {reply, {error, Nlr}, ShadowClassVars};
            {error, #beamtalk_error{} = Error} ->
                {reply, {error, Error}, ClassVars};
            {error, undef_in_body} ->
                {reply, {error, undef}, ClassVars};
            {error, {raised, _ErrClass, Error, _ST}} ->
                {reply, {error, Error}, ClassVars}
        end
    after
        erlang:erase(ShadowKey)
    end.

%% ADR 0110 / BT-3032: `{nlr_relay, Nlr, ST}` is a foreign `^` (non-local
%% return) relaying out of the class method — the relayed NLR tuple plus its
%% stacktrace. It is kept distinct from `{error, {raised, ...}}` so
%% `invoke_class_method/7` can preserve class-var writes recorded under the
%% `'$bt_class_vars_shadow'` process-dictionary key instead of reverting them
%% the way it does for genuine errors.
-type class_method_outcome() ::
    test_spawn
    | {ok, term()}
    | {nlr_relay, term(), list()}
    | {error, #beamtalk_error{}}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}.

-doc """
BT-2007: Shared core of class-method dispatch.

Handles the TestCase `test_spawn` guard, constructs `ClassSelf` with
`class_mod = DefiningModule`, applies the method, and classifies any
errors into the structured variants in `class_method_outcome()`. Both
`invoke_class_method/7` (gen_server path) and `class_self_dispatch/4`
(self-send path) adapt the outcome to their own caller shapes.

Does not wrap `{ok, Raw}` — callers pattern-match the raw return to
decide whether it's a plain value or `{class_var_result, R, NewCV}`.
Stacktraces are preserved in the `{raised, ...}` variant so callers
that need to re-raise (e.g. self-dispatch) get the original trace.
""".
-spec apply_class_method_in_context(
    selector(), list(), class_name(), class_name(), atom(), map()
) -> class_method_outcome().
apply_class_method_in_context(Selector, Args, ClassName, DefiningClass, DefiningModule, ClassVars) ->
    %% BT-440: For test execution (runAll, run:) inherited from TestCase,
    %% return a spawn request so the caller (gen_server) can handle noreply.
    %% The self-dispatch caller translates this to a structured error.
    case is_test_execution_selector(Selector) andalso DefiningClass =:= 'TestCase' of
        true ->
            test_spawn;
        false ->
            ClassSelf = #beamtalk_object{
                class = beamtalk_class_registry:class_object_tag(ClassName),
                class_mod = DefiningModule,
                pid = self()
            },
            %% ADR 0084 / BT-2266: a runtime-installed class-method fun shadows
            %% the compiled export. The retrieval store is keyed by the resolved
            %% DefiningClass, so this works identically for own and inherited
            %% methods, and is gated so compile-time-only classes pay no extra
            %% read on the dispatch hot path (BT-2008).
            case beamtalk_class_metadata:lookup_class_method_fun(DefiningClass, Selector) of
                {ok, #{block := Fun}} ->
                    apply_class_method_fun(Fun, ClassSelf, ClassVars, Args, ClassName, Selector);
                error ->
                    apply_compiled_class_method(
                        ClassSelf,
                        ClassVars,
                        Args,
                        ClassName,
                        DefiningClass,
                        DefiningModule,
                        Selector
                    )
            end
    end.

-doc """
Apply a runtime-installed class-method fun (ADR 0084 / BT-2266).

Same calling convention and `{class_var_result, …}` contract as the compiled
path. A fun has no module export to mismatch, so an `undef` here is always raised
from inside the fun body (`undef_in_body`); any other error becomes `{raised,…}`.
""".
-spec apply_class_method_fun(fun(), #beamtalk_object{}, map(), list(), class_name(), selector()) ->
    class_method_outcome().
apply_class_method_fun(Fun, ClassSelf, ClassVars, Args, ClassName, Selector) ->
    try apply(Fun, [ClassSelf, ClassVars | Args]) of
        Raw ->
            {ok, Raw}
    catch
        %% BT-2691 (ADR 0099 §3): a connected `Program exit: N` raised inside a
        %% class method is a control-flow signal, not a method failure. Pass it
        %% through unchanged (no error log) so class_send_dispatch/3 can re-raise
        %% it to the eval/dispatch worker that reports the status.
        throw:({beamtalk_script_exit, _} = ScriptExit):ScriptST ->
            {error, {raised, throw, ScriptExit, ScriptST}};
        %% BT-3022: a `^` non-local return that unwound through this class method
        %% belongs to a method frame in the *calling* process. Pass it through
        %% unlogged so class_send_dispatch/3 can re-throw it there.
        %% ADR 0110 / BT-3032: tagged as its own outcome variant (not
        %% {error, {raised, ...}}) so invoke_class_method/7 can keep class-var
        %% writes made before the unwind instead of reverting them.
        throw:Nlr:NlrST when ?IS_NLR(Nlr) ->
            {nlr_relay, Nlr, NlrST};
        error:undef:ST ->
            ?LOG_ERROR(
                "Runtime class method ~p:~p raised undef internally",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    stacktrace => ST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, undef_in_body};
        ErrClass:Error:ErrST ->
            ?LOG_ERROR(
                "Runtime class method ~p:~p failed",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-doc "Apply a compiled `class_<Selector>` export (the historical dispatch path).".
-spec apply_compiled_class_method(
    #beamtalk_object{}, map(), list(), class_name(), class_name(), atom(), selector()
) -> class_method_outcome().
apply_compiled_class_method(
    ClassSelf, ClassVars, Args, ClassName, DefiningClass, DefiningModule, Selector
) ->
    FunName = class_method_fun_name(Selector),
    %% BT-412: Pass class variables; the caller pattern-matches
    %% `{class_var_result, Result, NewClassVars}` vs plain return.
    try erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args]) of
        Raw ->
            {ok, Raw}
    catch
        %% BT-2691 (ADR 0099 §3): pass a connected `Program exit: N` through as a
        %% control-flow signal (no "method failed" log); class_send_dispatch/3
        %% re-raises it to the eval/dispatch worker.
        throw:({beamtalk_script_exit, _} = ScriptExit):ScriptST ->
            {error, {raised, throw, ScriptExit, ScriptST}};
        %% BT-3022: see apply_class_method_fun/6 — a non-local return unwinding out
        %% of a class method is control flow for a frame in the calling process.
        %% ADR 0110 / BT-3032: tagged {nlr_relay, ...} so class-var writes made
        %% before the unwind can be recovered rather than reverted.
        throw:Nlr:NlrST when ?IS_NLR(Nlr) ->
            {nlr_relay, Nlr, NlrST};
        error:undef:ST ->
            classify_undef(ClassName, DefiningClass, DefiningModule, Selector, FunName, ST);
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
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-spec classify_undef(class_name(), class_name(), atom(), selector(), atom(), list()) ->
    {error, #beamtalk_error{}} | {error, undef_in_body}.
classify_undef(ClassName, DefiningClass, DefiningModule, Selector, FunName, ST) ->
    case is_dispatch_undef(DefiningModule, FunName, ST) of
        {true, module_not_loaded} ->
            Hint = module_not_loaded_hint(ClassName, DefiningClass, DefiningModule),
            {error, beamtalk_error:new(class_not_found, ClassName, Selector, Hint)};
        {true, method_not_found} ->
            Hint = method_not_found_hint(ClassName, DefiningClass, Selector),
            {error, beamtalk_error:new(does_not_understand, ClassName, Selector, Hint)};
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
            {error, undef_in_body}
    end.

-spec module_not_loaded_hint(class_name(), class_name(), atom()) -> binary().
module_not_loaded_hint(ClassName, DefiningClass, DefiningModule) ->
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
    end.

-spec method_not_found_hint(class_name(), class_name(), selector()) -> binary().
method_not_found_hint(ClassName, DefiningClass, Selector) ->
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
    end.

-doc """
Walk the superclass chain to find an inherited class method.

Uses the ETS hierarchy table for O(1) superclass lookup per level and
the ETS class-methods table (BT-2008) for the per-ancestor selector +
module lookup. The hot path issues no `gen_server:call`s — both tables
are populated atomically inside `beamtalk_object_class:init/1` before
the class becomes externally dispatch-visible, so a hierarchy entry
without a matching methods entry is not reachable from normal dispatch.

Safe to call from within a gen_server `handle_call` because we only walk
UP the hierarchy (no circular dependency possible).

BT-2786: The walk itself (depth guard, cycle warning, advance-to-superclass)
is `beamtalk_hierarchy:walk_ancestors/3`; this function supplies only the
per-ancestor ETS probe.
""".
-spec find_class_method_in_chain(selector(), class_name()) ->
    {ok, class_name(), atom()} | not_found.
find_class_method_in_chain(Selector, ClassName) ->
    SuperclassName = superclass_from_ets(ClassName),
    StepFun = fun(AncestorName, _Depth) ->
        case beamtalk_class_metadata:lookup_methods(AncestorName) of
            {ok, AncestorModule, Selectors} ->
                case lists:member(Selector, Selectors) of
                    true -> {found, {AncestorName, AncestorModule}};
                    false -> {next, superclass_from_ets(AncestorName)}
                end;
            not_found ->
                not_found
        end
    end,
    case beamtalk_hierarchy:walk_ancestors(SuperclassName, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, {AncestorName, AncestorModule}} ->
            {ok, AncestorName, AncestorModule};
        not_found ->
            not_found;
        {max_depth_exceeded, CycleAncestor} ->
            ?LOG_WARNING(
                "find_class_method_in_ancestors: max hierarchy depth ~p exceeded at ~p (starting from ~p) — possible cycle",
                [?MAX_HIERARCHY_DEPTH, CycleAncestor, ClassName],
                #{domain => [beamtalk, runtime]}
            ),
            not_found
    end.

-doc "Read the superclass name from the hierarchy table (no gen_server call).".
-spec superclass_from_ets(class_name()) -> class_name() | none.
superclass_from_ets(ClassName) ->
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        {ok, Super} -> Super;
        not_found -> none
    end.

-doc """
BT-3200: Test whether `ClassName`'s own metaclass tag understands `Selector`
— a class-side extension (ADR 0066), a local or inherited class method, or a
local runtime-installed (ADR 0084) class method.

Backs `beamtalk_object_ops:responds_to_result/3`'s `respondsTo:` on a
class-object receiver. `SomeClass respondsTo: #sel` previously always
answered against the generic `'Class'` protocol only (BT-776), because a
class's metaclass tag (`'SomeClass class'`) is virtual — never registered
as its own class in `beamtalk_class_registry`/`beamtalk_class_metadata` —
so the existing hierarchy-walk reflection helper
(`beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2`) cannot be
pointed at it directly: there is no such class row to walk from. This
function instead re-implements "does dispatch find this selector" directly
against `ClassName`'s own tables, mirroring `handle_class_method_call/6`'s
priority order (extension, then local, then inherited chain) without
actually invoking anything.

Extensions are checked via the metaclass tag (matching how they are
registered, ADR 0066); local and inherited class methods are checked
against `ClassName` itself (the real registry entry) since class methods
belong to the class, not a separate metaclass registry row. Returns `false`
— not an error — for an unregistered `ClassName`, since every per-class
lookup this function makes already degrades to `not_found`/`error` for a
missing class; the generic-protocol fallback the caller applies next still
answers correctly (`Class`/`Behaviour`/`Object` messages are independent of
whether `ClassName` itself is currently registered).
""".
-spec class_understands_class_selector(class_name(), selector()) -> boolean().
class_understands_class_selector(ClassName, Selector) ->
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    case beamtalk_dispatch:check_extension(ClassTag, Selector) of
        {ok, _Fun} ->
            true;
        not_found ->
            has_local_class_method(ClassName, Selector) orelse
                (find_class_method_in_chain(Selector, ClassName) =/= not_found)
    end.

-doc """
BT-3200: Does `ClassName` itself (not an ancestor) define `Selector` as a
class method — either a compiled/static one (`lookup_methods/1`'s local
selector list) or a runtime-installed one (ADR 0084,
`lookup_class_method_fun/2`)? Mirrors the same two lookups
`class_self_dispatch_local/4` already checks for a self-send, so
`respondsTo:` agrees with what a `self sel` from inside another class
method would actually resolve.
""".
-spec has_local_class_method(class_name(), selector()) -> boolean().
has_local_class_method(ClassName, Selector) ->
    case beamtalk_class_metadata:lookup_class_method_fun(ClassName, Selector) of
        {ok, _Info} ->
            true;
        error ->
            case beamtalk_class_metadata:lookup_methods(ClassName) of
                {ok, _Module, Selectors} -> lists:member(Selector, Selectors);
                not_found -> false
            end
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

BT-3090: a selector built at runtime (e.g. via `perform:` with a
dynamically-assembled selector) can exceed Erlang's 255-byte atom limit even
though no *compile-time* selector ever does — `list_to_atom` on the raw
`"class_" ++ Selector` string would then crash with `system_limit` instead of
producing a name. This mirrors the compile-time guard in the Rust codegen's
`safe_class_method_fn_name` (`crates/beamtalk-core/src/codegen/core_erlang/selector_mangler.rs`)
byte-for-byte — same 255-byte threshold on `"class_" ++ Selector`, same
FNV-1a 64-bit hash, same `"class_kw_<16-hex-digits>"` naming scheme — so a
selector that is long at compile time and the *same* selector text
constructed at runtime via `perform:` produce the identical atom.
""".
-spec class_method_fun_name(selector()) -> atom().
class_method_fun_name(Selector) ->
    SelectorBin = atom_to_binary(Selector, utf8),
    Combined = <<"class_", SelectorBin/binary>>,
    case byte_size(Combined) =< 255 of
        true ->
            % elp:fixme W0023 intentional atom creation
            binary_to_atom(Combined, utf8);
        false ->
            Hash = fnv1a_64(SelectorBin),
            HashHex = io_lib:format("~16.16.0b", [Hash]),
            % elp:fixme W0023 intentional atom creation
            list_to_atom(lists:flatten(["class_kw_", HashHex]))
    end.

%% Deterministic FNV-1a 64-bit hash — bit-for-bit identical to the Rust
%% `fnv1a_64` in `selector_mangler.rs` (same offset basis, same prime,
%% same 64-bit wraparound). No external dependency; ~20 lines is not worth
%% pulling in a hashing library for.
-spec fnv1a_64(binary()) -> non_neg_integer().
fnv1a_64(Data) when is_binary(Data) ->
    fnv1a_64(Data, 16#cbf29ce484222325).

-spec fnv1a_64(binary(), non_neg_integer()) -> non_neg_integer().
fnv1a_64(<<>>, Hash) ->
    Hash;
fnv1a_64(<<Byte, Rest/binary>>, Hash) ->
    H1 = Hash bxor Byte,
    H2 = (H1 * 16#0100000001b3) band 16#ffffffffffffffff,
    fnv1a_64(Rest, H2).

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

BT-3072: `actor.bt` now declares real `class sealed spawn`/`spawnWith:`
bodies (`beamtalk_actor:doSpawn/1`, `doSpawnWith/2`), matching the treatment
BT-3071 gave `new`/`new:`. This function — and `class_send/3`'s explicit
`spawn`/`spawnWith:` clauses above, and `handle_metaclass_self_call/2` below
— deliberately keep routing through `class_self_spawn`/`class_self_new`
directly rather than falling through to generic inherited class-method
dispatch: doing so would mean a self-send here reaches the lifted body via
`gen_server:call(self(), ...)`, which deadlocks (the exact regression the
issue's own context warns against). Same "keep the shim, the lifted body is
the dynamic-dispatch definition, not the one every path actually calls"
choice BT-3071 made — see `beamtalk_actor:doSpawn/1`'s doc.
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
Handle `self class <selector>` self-calls from within a class method (BT-2005).

The class gen_server cannot `gen_server:call(self(), ...)` — the metaclass
dispatch path would deadlock. Short-circuit the spawn/new family to the same
helpers used by BT-893 (`self new`) and BT-2004 (`self spawnAs:`), and raise a
clear, structured error for any other selector so callers see a useful
diagnostic rather than an opaque `calling_self` timeout.
""".
-spec handle_metaclass_self_call(selector(), list()) -> term().
handle_metaclass_self_call('new', []) ->
    handle_self_instantiation(new, 'new', []);
handle_metaclass_self_call('new:', [Map]) ->
    handle_self_instantiation(new, 'new:', [Map]);
handle_metaclass_self_call(spawn, []) ->
    handle_self_instantiation(spawn, spawn, []);
handle_metaclass_self_call('spawnWith:', [Map]) ->
    handle_self_instantiation(spawn, 'spawnWith:', [Map]);
handle_metaclass_self_call('spawnAs:', [Name]) ->
    handle_metaclass_self_named_spawn('spawnAs:', #{}, Name);
handle_metaclass_self_call('spawnWith:as:', [InitArgs, Name]) ->
    handle_metaclass_self_named_spawn('spawnWith:as:', InitArgs, Name);
handle_metaclass_self_call(Selector, _Args) ->
    ClassName =
        case get(beamtalk_class_name) of
            undefined -> class_name_from_pid(self());
            CN -> CN
        end,
    Error0 = beamtalk_error:new(dispatch_error, ClassName, Selector),
    Error1 = beamtalk_error:with_hint(
        Error0,
        iolist_to_binary(
            io_lib:format(
                "'self class ~ts' inside a class method of '~ts' would deadlock "
                "(gen_server:call to self). Only spawn/new selectors can be "
                "short-circuited from the class process. Send this message "
                "from outside the class, or use `self ~ts` directly if the "
                "method is defined on the instance side.",
                [Selector, ClassName, Selector]
            )
        )
    ),
    beamtalk_error:raise(Error1).

-doc """
Perform a named spawn directly from within a class method (BT-2005 / BT-2004).

Mirrors `handle_self_instantiation/3` but routes to `class_self_spawn_as` /
`class_self_spawn_with`, which return a Beamtalk `Result` rather than raising.
""".
-spec handle_metaclass_self_named_spawn('spawnAs:' | 'spawnWith:as:', term(), term()) -> term().
handle_metaclass_self_named_spawn(Selector, InitArgs, Name) ->
    ClassName = get(beamtalk_class_name),
    Module = get(beamtalk_class_module),
    IsAbstract0 = get(beamtalk_class_is_abstract),
    case {ClassName, Module, IsAbstract0} of
        {CN, Mod, IsAbstract} when
            is_atom(CN),
            is_atom(Mod),
            is_boolean(IsAbstract),
            CN =/= undefined,
            Mod =/= undefined
        ->
            case Selector of
                'spawnAs:' ->
                    beamtalk_class_instantiation:class_self_spawn_as(CN, Mod, IsAbstract, Name);
                'spawnWith:as:' ->
                    beamtalk_class_instantiation:class_self_spawn_with(
                        CN, Mod, IsAbstract, InitArgs, Name
                    )
            end;
        _ ->
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
            %% Use reraise for idempotent wrapping before re-raising.
            beamtalk_exception_handler:reraise(Error)
    end.
