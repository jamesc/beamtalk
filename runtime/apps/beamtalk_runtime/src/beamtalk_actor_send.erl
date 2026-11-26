%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_send).

%%% **DDD Context:** Actor System Context

-moduledoc """
The body of the actor send paths, split out of `beamtalk_actor` (BT-5047).

Owns what sits behind the `beamtalk_actor:async_send/4`, `sync_send/3,4`,
`cast_send/3` and `*_known` entry points: lifecycle selectors answered
locally, the pid clauses, the `gen_server:call` wrapper (`sync_call/5`),
self-send dispatch, name-resolving proxy refs (ADR 0079), remote-node errors
(ADR 0126) and the future watcher. The entry points themselves stay in
`beamtalk_actor`, where generated code and `beamtalk_message_dispatch` call
them, so the send path costs the same number of calls as before the split.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").
-export([
    actor_dead_error_record/1,
    async_send_impl/5,
    lifecycle_exit_error/3,
    cast_send_pid/4,
    get_otel_ctx/0,
    get_propagated_ctx/0,
    node_down_error_record/2,
    resolve_proxy/1,
    resolve_proxy_or_error/2,
    sync_send_pid/4,
    sync_send_pid/5,
    sync_send_proxy/4
]).

%% Driven directly on a `$bt_nlr` result by the unit tests.
-ifdef(TEST).
-export([unwrap_dispatch_result/1, spawn_future_watcher/3]).
-endif.

%% BT-4265: the helpers an actor send runs every time are inlined, so a send
%% pays for the work, not for a chain of local calls to reach it. They live in
%% this module, with `sync_call/5` and `cast_send_pid/4`, so they stay inlined.
-compile(
    {inline, [
        self_send_kind/1,
        sync_send_remote/4,
        resolve_class/2,
        get_sync_propagated_ctx/0,
        get_propagated_ctx/0,
        get_otel_ctx/0,
        check_call_stack/2,
        wire_sync_call/5,
        local_call/3
    ]}
).

%% Longest a future watcher outlives an unsettled send (BT-5084). `future_watcher_cap_ms`
%% (application env) overrides it.
-define(FUTURE_WATCHER_CAP_MS, 3600000).

%% Lifecycle selectors answered locally with a plain value or error, shared by
%% async_send_impl/5 and sync_send_pid/4. Each selector is matched with its
%% exact arity so a wrong-arity send falls through to normal dispatch.
-define(IS_LOCAL_LIFECYCLE(S, A),
    ((A =:= [] andalso
        (S =:= isAlive orelse S =:= kill orelse S =:= delegate orelse S =:= pid orelse
            S =:= monitor)) orelse
        (S =:= 'onExit:' andalso length(A) =:= 1))
).

-spec local_lifecycle(pid(), atom(), list()) -> {value, term()} | {error, #beamtalk_error{}}.

local_lifecycle(ActorPid, isAlive, []) ->
    %% BT-5192: a remote pid's node is asked (bounded); see beamtalk_pid:ask_alive/3.
    case beamtalk_pid:ask_alive(ActorPid, unknown, isAlive) of
        {ok, Alive} -> {value, Alive};
        {error, _} = Err -> Err
    end;
local_lifecycle(ActorPid, kill, []) ->
    %% Telemetry, monitor-before-kill, and DOWN-wait are in kill_and_wait/1.
    %% If no DOWN is observed within 5 s we cannot claim success.
    case kill_and_wait(ActorPid) of
        ok -> {value, ok};
        {error, _} = Err -> Err
    end;
local_lifecycle(_ActorPid, delegate, []) ->
    %% Non-native Actors do not have a backing Erlang module.
    {error, beamtalk_actor:delegate_error(unknown)};
local_lifecycle(ActorPid, pid, []) ->
    {value, ActorPid};
local_lifecycle(ActorPid, monitor, []) ->
    {value, erlang:monitor(process, ActorPid)};
local_lifecycle(ActorPid, 'onExit:', [Block]) ->
    case start_exit_watcher(ActorPid, Block) of
        ok ->
            {value, ok};
        timeout ->
            {error,
                beamtalk_error:new(
                    timeout,
                    unknown,
                    'onExit:',
                    <<"onExit: watcher did not start within 5000ms">>
                )}
    end.

%% Spawn a watcher that monitors ActorPid and runs Block(Reason) on exit.
%% Waits (max 5 s) for the watcher's monitor to be installed before returning,
%% correlated by a unique token so stale/parallel watchers cannot confuse it.
-spec start_exit_watcher(pid(), fun((term()) -> term())) -> ok | timeout.

start_exit_watcher(ActorPid, Block) ->
    Caller = self(),
    Token = make_ref(),
    WatcherPid = spawn(fun() ->
        logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
        Ref = erlang:monitor(process, ActorPid),
        Caller ! {onExit_ready, Token},
        receive
            {'DOWN', Ref, process, ActorPid, Reason} ->
                try
                    Block(Reason)
                catch
                    Class:Err:Stack ->
                        ?LOG_WARNING("Error in onExit: callback", #{
                            domain => [beamtalk, runtime],
                            actor_pid => ActorPid,
                            exit_reason => Reason,
                            error_class => Class,
                            error => Err,
                            stacktrace => Stack
                        })
                end
        end
    end),
    receive
        {onExit_ready, Token} -> ok
    after 5000 ->
        %% Kill the watcher to prevent ghost callbacks and flush stale token.
        exit(WatcherPid, kill),
        receive
            {onExit_ready, Token} -> ok
        after 0 -> ok
        end,
        timeout
    end.

-spec async_send_impl(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()},
    atom(),
    list(),
    pid(),
    atom() | undefined
) -> ok.

%% ADR 0079 / ADR 0126: name-resolving proxy fan-out. Name-only selectors
%% answer from the proxy itself; other selectors resolve the name to the
%% currently-registered pid and re-enter the pid-based clauses below.
async_send_impl(Ref, Selector, Args, FuturePid, ClassHint) when ?IS_PROXY_REF(Ref) ->
    case proxy_route(Ref, Selector, Args) of
        {answer, Value} ->
            beamtalk_future:resolve(FuturePid, Value),
            ok;
        {pid, Pid} ->
            async_send_impl(Pid, Selector, Args, FuturePid, ClassHint);
        {error, Error} ->
            beamtalk_future:reject(FuturePid, Error),
            ok
    end;
async_send_impl(ActorPid, Selector, Args, FuturePid, _ClassHint) when
    ?IS_LOCAL_LIFECYCLE(Selector, Args)
->
    %% Local lifecycle selectors share one table with sync_send_pid/4.
    case local_lifecycle(ActorPid, Selector, Args) of
        {value, Value} -> beamtalk_future:resolve(FuturePid, Value);
        {error, Error} -> beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send_impl(ActorPid, isRemote, [], FuturePid, _ClassHint) ->
    %% isRemote must compare against the *caller's* node, so it cannot run
    %% in the actor — see is_remote/2.
    try beamtalk_actor:sync_send(ActorPid, isRemote, []) of
        Result -> beamtalk_future:resolve(FuturePid, Result)
    catch
        error:#beamtalk_error{} = Error ->
            beamtalk_future:reject(FuturePid, Error);
        error:#{error := #beamtalk_error{} = Error} ->
            beamtalk_future:reject(FuturePid, Error);
        Class:Reason ->
            Error = beamtalk_error:with_details(
                beamtalk_error:new(runtime_error, unknown, isRemote),
                #{original_class => Class, original_reason => Reason}
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send_impl(ActorPid, stop, [], FuturePid, _ClassHint) ->
    %% stop is handled locally - gracefully stops the actor process
    %% No send-site telemetry for stop — terminate/2 handles it
    %% to avoid double-counting (request + termination).
    beamtalk_actor:async_stop(ActorPid, normal, ?STOP_TIMEOUT, stop, FuturePid);
async_send_impl(ActorPid, 'stopWith:', [Reason], FuturePid, _ClassHint) ->
    %% BT-3967: stop with a caller-chosen exit reason.
    beamtalk_actor:async_stop(ActorPid, Reason, ?STOP_TIMEOUT, 'stopWith:', FuturePid);
async_send_impl(ActorPid, 'stopWith:timeout:', [Reason, Timeout], FuturePid, _ClassHint) ->
    case beamtalk_actor:is_stop_timeout(Timeout) of
        true ->
            beamtalk_actor:async_stop(ActorPid, Reason, Timeout, 'stopWith:timeout:', FuturePid);
        false ->
            beamtalk_future:reject(
                FuturePid, beamtalk_actor:stop_timeout_type_error('stopWith:timeout:')
            ),
            ok
    end;
async_send_impl(ActorPid, 'continueWith:' = Selector, [ContSelector], FuturePid, _ClassHint) ->
    %% BT-3968: only meaningful on `self`; raises in the caller otherwise.
    beamtalk_actor:async_request_continuation(ActorPid, Selector, ContSelector, none, FuturePid);
async_send_impl(
    ActorPid,
    'continueWith:withArguments:' = Selector,
    [ContSelector, ContArgs],
    FuturePid,
    _ClassHint
) ->
    beamtalk_actor:async_request_continuation(
        ActorPid, Selector, ContSelector, {args, ContArgs}, FuturePid
    );
async_send_impl(ActorPid, Selector, _Args, FuturePid, ClassHint) when
    node(ActorPid) =/= node()
->
    %% ADR 0126 (Amendment BT-4989): a remote async send is not supported. No
    %% compiled actor decodes an async wire envelope, so the future would
    %% never resolve. Settle the future and fail at once, before anything is
    %% sent, so no process is left waiting on it.
    Error = remote_async_unsupported_error(Selector, ClassHint),
    beamtalk_future:reject(FuturePid, Error),
    beamtalk_error:raise(Error);
async_send_impl(ActorPid, Selector, Args, FuturePid, ClassHint) ->
    %% Instrumented dispatch span (ADR 0069 Phase 2a; BT-4113: events opt-in).
    %% Measures dispatch-to-mailbox time for async sends.
    Class = resolve_class(ActorPid, ClassHint),
    %% Check liveness before sending, and spawn a watcher to detect
    %% actor death during message processing. The watcher monitors both the
    %% actor and future processes: if the actor dies before the future is
    %% resolved, the watcher rejects the future with a structured error.
    beamtalk_telemetry:dispatch_span(ActorPid, Class, Selector, async, fun() ->
        case beamtalk_pid:is_alive(ActorPid) of
            true ->
                PropCtx = get_propagated_ctx(),
                gen_server:cast(ActorPid, {Selector, Args, FuturePid, PropCtx}),
                spawn_future_watcher(ActorPid, FuturePid, Selector),
                {ok, ok};
            false ->
                beamtalk_future:reject(FuturePid, actor_dead_error_record(Selector)),
                {ok, error}
        end
    end),
    ok.

-spec remote_async_unsupported_error(atom(), atom() | undefined) -> #beamtalk_error{}.

remote_async_unsupported_error(Selector, ClassHint) ->
    Class =
        case ClassHint of
            undefined -> unknown;
            _ -> ClassHint
        end,
    beamtalk_error:new(
        unsupported_operation,
        Class,
        Selector,
        <<"Async sends to an actor on another node are not supported (ADR 0126); use a sync send (.) or a cast (!)">>
    ).

-spec cast_send_pid(pid(), atom(), list(), atom() | undefined) -> ok.

cast_send_pid(ActorPid, 'continueWith:' = Selector, [ContSelector], _Class) ->
    %% BT-3968: `actor continueWith: #x!` from another process is caller
    %% misuse, not a remote request — raise in the caller rather than cast.
    beamtalk_actor:request_continuation(ActorPid, Selector, ContSelector, none);
cast_send_pid(ActorPid, 'continueWith:withArguments:' = Selector, [ContSelector, ContArgs], _Class) ->
    beamtalk_actor:request_continuation(ActorPid, Selector, ContSelector, {args, ContArgs});
cast_send_pid(ActorPid, Selector, Args, ClassHint) ->
    %% Instrumented dispatch span (ADR 0069 Phase 2a; BT-4113: events opt-in).
    %% Measures dispatch-to-mailbox time for cast sends. No liveness
    %% pre-check: `gen_server:cast/2` to a dead pid is already a silent no-op,
    %% which is exactly the fire-and-forget contract.
    Class = resolve_class(ActorPid, ClassHint),
    beamtalk_telemetry:dispatch_span(ActorPid, Class, Selector, cast, fun() ->
        PropCtx = get_propagated_ctx(),
        case node(ActorPid) =/= node() of
            true ->
                case beamtalk_wire:encode(Args) of
                    {ok, WireArgs} ->
                        gen_server:cast(
                            ActorPid,
                            {?BT_WIRE_TAG, ?BT_WIRE_VERSION, cast, Selector, WireArgs, PropCtx}
                        ),
                        {ok, cast};
                    {error, Error} ->
                        %% Fire-and-forget: no caller to tell (mirrors
                        %% the receiver-side cast-skew handling, ADR
                        %% 0126 §5.2) — log and drop, matching Erlang's
                        %% fire-and-forget semantics.
                        ?LOG_WARNING("Cast argument not serialisable for remote send", #{
                            selector => Selector,
                            pid => ActorPid,
                            error => Error,
                            domain => [beamtalk, runtime, dist]
                        }),
                        {ok, cast}
                end;
            false ->
                gen_server:cast(ActorPid, {cast, Selector, Args, PropCtx}),
                {ok, cast}
        end
    end),
    ok.

-doc """
The class to report for a send to `Pid`: the caller's `ClassHint` when it
has one (BT-4113, from the `#beamtalk_object{}` record — also the only way a
remote pid's class is known, ADR 0126 §7.3), else `lookup_class/1`.
""".
-spec resolve_class(pid(), atom() | undefined) -> atom().
resolve_class(Pid, undefined) -> beamtalk_actor:lookup_class(Pid);
resolve_class(_Pid, Class) -> Class.

-doc """
The pid clauses of `sync_send/3`: lifecycle selectors answered locally, and
every other message through the Layer 1 self-send check. `ClassHint` is the
caller's class, or `undefined` to look it up (see `resolve_class/2`).
""".
-spec sync_send_pid(pid(), atom(), list(), atom() | undefined) -> term().
sync_send_pid(ActorPid, Selector, Args, _Class) when ?IS_LOCAL_LIFECYCLE(Selector, Args) ->
    case local_lifecycle(ActorPid, Selector, Args) of
        {value, Value} -> Value;
        {error, Error} -> beamtalk_exception_handler:reraise(Error)
    end;
sync_send_pid(ActorPid, isRemote, [], _Class) ->
    is_remote(ActorPid, fun() -> beamtalk_actor:sync_send(ActorPid, node, []) end);
sync_send_pid(ActorPid, stop, [], _Class) ->
    %% stop is handled locally - gracefully stops the actor process
    %% No send-site telemetry for stop — terminate/2 handles it
    %% to avoid double-counting (request + termination).
    beamtalk_actor:sync_stop(ActorPid, normal, ?STOP_TIMEOUT, stop);
sync_send_pid(ActorPid, 'stopWith:', [Reason], _Class) ->
    %% BT-3967: stop with a caller-chosen exit reason.
    beamtalk_actor:sync_stop(ActorPid, Reason, ?STOP_TIMEOUT, 'stopWith:');
sync_send_pid(ActorPid, 'stopWith:timeout:', [Reason, Timeout], _Class) ->
    case beamtalk_actor:is_stop_timeout(Timeout) of
        true ->
            beamtalk_actor:sync_stop(ActorPid, Reason, Timeout, 'stopWith:timeout:');
        false ->
            beamtalk_exception_handler:reraise(
                beamtalk_actor:stop_timeout_type_error('stopWith:timeout:')
            )
    end;
sync_send_pid(ActorPid, 'continueWith:' = Selector, [ContSelector], _Class) ->
    %% BT-3968: record a follow-up step for the current callback (self only).
    beamtalk_actor:request_continuation(ActorPid, Selector, ContSelector, none);
sync_send_pid(ActorPid, 'continueWith:withArguments:' = Selector, [ContSelector, ContArgs], _Class) ->
    beamtalk_actor:request_continuation(ActorPid, Selector, ContSelector, {args, ContArgs});
sync_send_pid(ActorPid, Selector, Args, ClassHint) ->
    %% Layer 1: Fast-path for self-sends.
    %% If ActorPid is our own process AND the installed cell holds our own
    %% state (meaning we're inside a handle_call/handle_cast dispatch),
    %% dispatch directly to avoid gen_server:call deadlock. This catches
    %% aliased self-sends like:
    %%   other := self. other fieldNames
    case self_send_kind(ActorPid) of
        self ->
            self_dispatch(Selector, Args);
        remote ->
            sync_send_remote(ActorPid, Selector, Args, ClassHint);
        foreign ->
            raise_self_send_during_away_super(ActorPid, Selector)
    end.

-doc """
Remote sync-send via gen_server:call (the normal path).
Factored out of sync_send/3 for Layer 1 self-send fast-path.

TimeoutProxy manages its own timeout on the inner (proxy→target) hop, so
the outer (caller→proxy) hop uses `infinity` so it doesn't time out before
the proxy's configured timeout expires. Every other actor uses
`gen_server:call/2`'s 5000ms default.
""".
-spec sync_send_remote(pid(), atom(), list(), atom() | undefined) -> term().
sync_send_remote(ActorPid, Selector, Args, ClassHint) ->
    Class = resolve_class(ActorPid, ClassHint),
    CallTimeout =
        case Class of
            'TimeoutProxy' -> infinity;
            _ -> default
        end,
    sync_call(ActorPid, Selector, Args, CallTimeout, Class).

-doc """
The instrumented `gen_server:call` shared by `sync_send/3` (via
`sync_send_remote/4`) and `sync_send/4`.

The dispatch span (ADR 0069 Phase 2a) measures caller-perspective round-trip
time and records the always-on aggregate; it emits telemetry events only
when dispatch events are enabled (BT-4113). A raise inside the span is
re-raised with its original class and reason, so the outer try/catch can
convert exits into structured errors.

No liveness pre-check (BT-4113): `gen_server:call` to a dead actor exits
`noproc` (or `normal`/`shutdown` if it dies mid-call), which the catch
clauses below already map to `actor_dead` — without the snapshot race a
pre-check had.

Generated handle_call/3 wraps replies as {ok, Result} or {error, Error}, and
the reply is unwrapped here so callers receive the value directly. The
{error, Error} case has three sub-forms due to the safe_dispatch layer:
  1. Error = {ErlType, Value, Stacktrace} — with captured stacktrace
  2. Error = {ErlType, Value} — backward compat: without stacktrace
  3. Error = other term — e.g. #beamtalk_error{} from dispatch_user_method
All forms are re-raised as Erlang exceptions so the caller sees them.
""".
-spec sync_call(pid(), atom(), list(), default | timeout(), atom()) -> term().
sync_call(ActorPid, Selector, Args, CallTimeout, Class) ->
    try
        beamtalk_telemetry:dispatch_span(ActorPid, Class, Selector, sync, fun() ->
            PropCtx = get_sync_propagated_ctx(),
            %% Layer 2: Check for transitive cycles before gen_server:call.
            %% If ActorPid is already in the call stack, this send would deadlock.
            check_call_stack(ActorPid, Selector),
            case wire_sync_call(ActorPid, Selector, Args, PropCtx, CallTimeout) of
                {ok, Result} ->
                    {Result, ok};
                {error, {ErlType, ErrorValue, Stacktrace}} ->
                    %% safe_dispatch caught an Erlang exception with stacktrace;
                    %% re-raise with full type/stacktrace context, attaching the
                    %% active selector/class breadcrumb so a raw error escaping
                    %% the method is classified *and* located.
                    beamtalk_exception_handler:reraise(ErlType, ErrorValue, Stacktrace, #{
                        selector => Selector, class => Class
                    });
                {error, {ErlType, ErrorValue}} ->
                    %% Backward compat: safe_dispatch without stacktrace —
                    %% still pass exception class to preserve error kind
                    beamtalk_exception_handler:reraise(ErlType, ErrorValue, []);
                {error, Nlr} when ?IS_NLR(Nlr) ->
                    %% A `^` inside a block invoked on ActorPid on our
                    %% behalf (BT-3582). The matching catch frame is in
                    %% *our* process — ActorPid had no frame holding
                    %% that token, so its reply is the only way the
                    %% signal gets back here. Re-throw so the enclosing
                    %% method unwinds as it would have without the
                    %% process hop — same relay shape as
                    %% beamtalk_class_dispatch.erl's class-method hops.
                    %% The block was away from home in ActorPid, which
                    %% `rethrow_from_away/1` tells the test-build commit
                    %% check (BT-4536).
                    beamtalk_actor_state:rethrow_from_away(Nlr);
                {error, Error} ->
                    beamtalk_exception_handler:reraise(Error);
                DirectValue ->
                    %% Backward compat: a hand-written gen_server that answers with an
                    %% unwrapped value.
                    {DirectValue, ok}
            end
        end)
    catch
        exit:Reason:Stack ->
            %% BT-4676: classification lives in beamtalk_call_failure. ADR 0126
            %% §7.1: a partitioned or never-connected node is `node_down` (the
            %% actor may be alive on the far side), not `actor_dead`.
            case beamtalk_call_failure:classify(exit, Reason) of
                dead ->
                    raise_actor_dead(Selector);
                timeout ->
                    raise_timeout(Selector);
                {unreachable, _Why} ->
                    raise_node_down(
                        beamtalk_call_failure:unreachable_node(Reason, node(ActorPid)), Selector
                    );
                other when is_tuple(Reason), tuple_size(Reason) =:= 2 ->
                    %% `{normal, _}`, `{shutdown, _}`, `{killed, _}`, custom stop
                    %% reasons: the actor died mid-call, so it is unavailable.
                    raise_actor_dead(Selector);
                other ->
                    erlang:raise(exit, Reason, Stack)
            end
    end.

-doc """
Sync `gen_server:call` for both the local and remote (ADR 0126 §5.1) paths,
called from `sync_call/5` (shared by `sync_send/3` and `sync_send/4`). `CallTimeout` is `default`
(use `gen_server:call/2`'s own 5000ms default), `infinity`, or a
non-negative integer millisecond timeout — mirroring the two call shapes the
two callers already had before this helper existed.

Remote is decided by `node(ActorPid) =/= node()` — one comparison, the only
added cost for a local call (ADR 0126 §5.1 constraint 2). A remote call
wire-encodes `Args` and tags the message so the receiver's `handle_call`
prelude recognises it, then decodes the raw reply that comes back through
the ordinary `gen_server:call` reply path — `decode_call_result/2` reraises
on a reply-direction decode failure (§5.2) rather than returning a
half-decoded term. An encode failure on `Args` is a request-direction
failure (§5.2): it never reaches the network, and is raised here in the
caller, exactly as `beamtalk_wire:encode/1`'s own doc promises.
""".
-spec wire_sync_call(pid(), atom(), list(), map(), default | timeout()) -> term().
wire_sync_call(ActorPid, Selector, Args, PropCtx, CallTimeout) ->
    case node(ActorPid) =/= node() of
        true ->
            case beamtalk_wire:encode(Args) of
                {ok, WireArgs} ->
                    Msg = {?BT_WIRE_TAG, ?BT_WIRE_VERSION, call, Selector, WireArgs, PropCtx},
                    RawResult = local_call(ActorPid, Msg, CallTimeout),
                    decode_call_result(RawResult, Selector);
                {error, EncErr} ->
                    beamtalk_exception_handler:reraise(EncErr#beamtalk_error{selector = Selector})
            end;
        false ->
            local_call(ActorPid, {Selector, Args, PropCtx}, CallTimeout)
    end.

-spec local_call(pid(), term(), default | timeout()) -> term().

local_call(ActorPid, Msg, default) ->
    gen_server:call(ActorPid, Msg);
local_call(ActorPid, Msg, Timeout) ->
    gen_server:call(ActorPid, Msg, Timeout).

-doc """
Decode a raw reply that came back from a remote `gen_server:call` (ADR 0126
§5.1's "Result path" table). A decode failure here is reply-direction skew
(§5.2): the method has already run on the callee and its state has changed,
so this raises — `direction => reply` is merged into the error's `details`
so it reads distinctly from the identically-kinded request-direction
failure, which never reaches a method body at all.
""".
-spec decode_call_result(term(), atom()) -> term().
decode_call_result(RawResult, Selector) ->
    case beamtalk_wire:decode(RawResult) of
        {ok, Decoded} ->
            Decoded;
        {error, #beamtalk_error{details = Details} = Err} ->
            beamtalk_exception_handler:reraise(Err#beamtalk_error{
                selector = Selector, details = Details#{direction => reply}
            })
    end.

-doc """
The pid clauses of `sync_send/4`; `ClassHint` as in `sync_send_pid/4`.
""".
-spec sync_send_pid(pid(), atom(), list(), timeout(), atom() | undefined) -> term().
sync_send_pid(ActorPid, isRemote, [], Timeout, _ClassHint) ->
    is_remote(ActorPid, fun() -> beamtalk_actor:sync_send(ActorPid, node, [], Timeout) end);
sync_send_pid(ActorPid, Selector, Args, _Timeout, _ClassHint) when
    Selector =:= 'continueWith:'; Selector =:= 'continueWith:withArguments:'
->
    %% BT-3968: never forwarded to the actor — a continuation can only be
    %% requested by the actor's own callback (see sync_send/3).
    beamtalk_actor:sync_send(ActorPid, Selector, Args);
sync_send_pid(ActorPid, Selector, Args, Timeout, ClassHint) when
    is_integer(Timeout), Timeout >= 0;
    Timeout =:= infinity
->
    %% Layer 1: Fast-path for self-sends (same as sync_send/3).
    case self_send_kind(ActorPid) of
        self ->
            self_dispatch(Selector, Args);
        remote ->
            sync_call(ActorPid, Selector, Args, Timeout, resolve_class(ActorPid, ClassHint));
        foreign ->
            raise_self_send_during_away_super(ActorPid, Selector)
    end;
sync_send_pid(_ActorPid, Selector, _Args, _InvalidTimeout, _ClassHint) ->
    Error = beamtalk_error:new(
        type_error,
        unknown,
        Selector,
        <<"Timeout must be a non-negative integer (milliseconds) or #infinity">>
    ),
    beamtalk_exception_handler:reraise(Error).

-doc """
How a sync send to `ActorPid` from this process should go (Layer 1).

`self` (our own pid with our own state installed) dispatches directly.
`remote` is the normal `gen_server:call` path — including our own pid with
no cell installed, exactly as before. `foreign` is our own pid while the
cell holds another actor's state (stored by a nested away `super_send/5`):
the remote path could never serve our own pid (`gen_server:call` to self
exits `calling_self`, which the catch-all would misreport as `actor_dead`),
so the send sites raise `deadlock_detected` instead.
""".
-spec self_send_kind(pid() | tuple()) -> self | remote | foreign.
self_send_kind(ActorPid) ->
    case ActorPid =:= self() of
        true ->
            case beamtalk_actor_state:cell_owner() of
                ActorPid -> self;
                undefined -> remote;
                _Foreign -> foreign
            end;
        false ->
            remote
    end.

-doc """
Raise a structured `deadlock_detected` error for a self-send issued while
another actor's state is installed in this process's cell (a nested away
`super_send/5`): our own callback is blocked in that call, so the send could
never be served.
""".
-spec raise_self_send_during_away_super(pid(), atom()) -> no_return().
raise_self_send_during_away_super(ActorPid, Selector) ->
    Class = beamtalk_actor:lookup_class(ActorPid),
    Error = beamtalk_error:new(
        deadlock_detected,
        Class,
        Selector,
        <<
            "Sync send to own actor while another actor's state is installed: ",
            "the actor is blocked in the call that carried the block away"
        >>
    ),
    Error1 = beamtalk_error:with_details(Error, #{
        target_pid => ActorPid,
        cell_owner => beamtalk_actor_state:cell_owner()
    }),
    beamtalk_exception_handler:reraise(Error1).

-doc """
Direct self-dispatch for re-entrant self-sends (Layer 1).

When sync_send detects ActorPid == self(), we dispatch directly using
the State stashed in the process dictionary by handle_call/handle_cast.
This avoids the gen_server:call deadlock for aliased self-sends like
`other := self. other fieldNames`.

Returns the unwrapped result (same as sync_send would return), or
raises an error exception on dispatch failure.
""".
-spec self_dispatch(atom(), list()) -> term().
self_dispatch(Selector, Args) ->
    State = beamtalk_actor_state:take(),
    %% Compiled actors have their own safe_dispatch/3 in the generated module;
    %% `'__class_mod__'` names it.
    ClassMod = maps:get(?BT_CLASS_MOD_KEY, State),
    DispatchResult = ClassMod:safe_dispatch(Selector, Args, State),
    unwrap_dispatch_result(DispatchResult).

-doc """
Unwrap a dispatch result, update pdict state, and return the value.
Used by self_dispatch/2 to avoid repeating the put/error pattern.

A `^` that unwound the callee (`safe_dispatch` answers `{error, Nlr,
EntryState}`) is re-thrown unchanged, with no store to the cell (ADR 0146 R3,
BT-4525): the cell already holds the callee's writes (every compiled method
stores before a call that may run the block), and storing `EntryState` would
rewind it; turning the throw into an error would stop the owner's method
boundary from catching it.
""".
-spec unwrap_dispatch_result(
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}
) -> term().
unwrap_dispatch_result({error, Nlr, _EntryState}) when ?IS_NLR(Nlr) ->
    throw(Nlr);
unwrap_dispatch_result(DispatchResult) ->
    case beamtalk_actor_state:adapt(DispatchResult) of
        {reply, Result} ->
            Result;
        noreply ->
            nil;
        {error, {ErlType, ErrorValue, Stacktrace}} ->
            %% safe_dispatch with stacktrace
            beamtalk_exception_handler:reraise(ErlType, ErrorValue, Stacktrace);
        {error, {ErlType, ErrorValue}} ->
            %% Backward compat: preserve exception class without stacktrace
            beamtalk_exception_handler:reraise(ErlType, ErrorValue, []);
        {error, Reason} ->
            beamtalk_exception_handler:reraise(Reason)
    end.

-doc """
Check call stack for transitive cycles (Layer 2).

Before gen_server:call, verify that the target ActorPid is not already
in the call chain. If it is, a sync send would deadlock (A->B->C->A).
Raises a structured deadlock_detected error with the cycle path.
""".
-spec check_call_stack(pid(), atom()) -> ok.
check_call_stack(ActorPid, Selector) ->
    case get(?BT_CALL_STACK_KEY) of
        undefined ->
            ok;
        CallStack ->
            case lists:member(ActorPid, CallStack) of
                false ->
                    ok;
                true ->
                    raise_call_cycle(ActorPid, Selector, CallStack)
            end
    end.

-spec raise_call_cycle(pid(), atom(), [pid()]) -> no_return().

raise_call_cycle(ActorPid, Selector, CallStack) ->
    Class = beamtalk_actor:lookup_class(ActorPid),
    Error = beamtalk_error:new(
        deadlock_detected,
        Class,
        Selector,
        <<"Sync send would deadlock: target actor is already in the call chain">>
    ),
    Error1 = beamtalk_error:with_details(Error, #{
        call_stack => CallStack,
        target_pid => ActorPid
    }),
    beamtalk_exception_handler:reraise(Error1).

-doc """
Raise a structured actor_dead error as an Erlang exception.

Used by sync_send/3 to signal that the actor process is not available.
The caller will see a beamtalk_error{kind = actor_dead} exception.
""".
-spec raise_actor_dead(atom()) -> no_return().
raise_actor_dead(Selector) ->
    beamtalk_exception_handler:reraise(actor_dead_error_record(Selector)).

-doc """
Raise a structured timeout error as an Erlang exception.

Used by sync_send/3 when gen_server:call exceeds the timeout (default 5000ms).
A timeout means the actor didn't respond in time, NOT that it's dead —
it may be slow, overloaded, or deadlocked.
""".
-spec raise_timeout(atom()) -> no_return().
raise_timeout(Selector) ->
    Error = beamtalk_error:actor_timeout(Selector),
    beamtalk_exception_handler:reraise(Error).

-doc "Construct a structured actor_dead error record for the given selector.".
-spec actor_dead_error_record(atom()) -> #beamtalk_error{}.

actor_dead_error_record(Selector) ->
    beamtalk_error:actor_dead(unknown, Selector).

-doc """
Raise a structured node_down error as an Erlang exception (ADR 0126 §7.1).

Used by sync_call/5 (sync_send/3,4) when gen_server:call exits with
`{nodedown, Node}` (partitioned or never-connected node), and by
spawn_future_watcher/3 for the equivalent monitor DOWN reason. Distinct
from actor_dead: the node is unreachable, but the actor may be alive and
well on the far side once the partition heals — retry logic needs to be
able to tell the two apart.
""".
-spec raise_node_down(node(), atom()) -> no_return().
raise_node_down(Node, Selector) ->
    beamtalk_exception_handler:reraise(node_down_error_record(Node, Selector)).

-doc """
Answer an actor's `isRemote` (ADR 0126 §2) from the `Node` its `node` send
returned: `true` iff that node is not the *caller's*. This is why `isRemote`
is intercepted in `sync_send/3,4` / `async_send/4` rather than dispatched to
the actor — inside the actor, `node()` is always the actor's own node. Going
through the `node` send (not `node(ActorPid)`) lets a `TimeoutProxy` report
its target's node (§6).

Falls back to the pid's own node when the actor cannot answer `node` with a
`Node` — a user class that overrides `node` for its own meaning (a graph
walker's current node, say), or a `native:` actor whose backing module does
not implement it — so `isRemote` never crashes on a well-formed actor.
""".
-spec is_remote(pid(), fun(() -> term())) -> boolean().
is_remote(ActorPid, AskNode) ->
    NodeName =
        try AskNode() of
            #{?BT_CLASS_TAG_KEY := 'Node', name := Name} when is_atom(Name) -> Name;
            _NotANode -> node(ActorPid)
        catch
            error:#beamtalk_error{kind = does_not_understand} ->
                node(ActorPid);
            error:#{error := #beamtalk_error{kind = does_not_understand}} ->
                node(ActorPid)
        end,
    NodeName =/= node().

-doc """
The error for a stop of `ActorPid` (`stop`, `stopWith:`, `stopWith:timeout:`)
that exited with `Reason` (BT-5192). Uses `beamtalk_call_failure:classify/2`,
the classification `sync_call/5` applies to a send:

- unreachable node (`nodedown`, `noconnection`) → `node_down`: the stop request
  or its `'DOWN'` was lost, and the actor may still be running;
- `timeout` → `timeout`: the actor has not finished stopping in time (the
  terminate request was sent, so it may still stop in the background);
- anything else (the actor exited with another reason first) → `actor_dead`,
  with the exit reason in `details`.

`noproc` never reaches here: an already-dead actor counts as stopped.
""".
-spec lifecycle_exit_error(pid(), atom(), term()) -> #beamtalk_error{}.
lifecycle_exit_error(ActorPid, Selector, Reason) ->
    case beamtalk_call_failure:classify(exit, Reason) of
        {unreachable, _} ->
            node_down_error_record(
                beamtalk_call_failure:unreachable_node(Reason, node(ActorPid)), Selector
            );
        timeout ->
            beamtalk_error:timeout(
                unknown,
                Selector,
                <<
                    "The actor did not finish stopping within the timeout; "
                    "it may still stop in the background"
                >>
            );
        _ ->
            beamtalk_error:with_details(actor_dead_error_record(Selector), #{exit_reason => Reason})
    end.

-doc "Construct a structured node_down error record for the given node and selector.".
-spec node_down_error_record(node(), atom()) -> #beamtalk_error{}.

node_down_error_record(Node, Selector) ->
    beamtalk_error:node_down(unknown, Selector, Node).

-doc """
ADR 0126 §3: resolve a `{registered, Name, Node}` proxy's live pid.

`Node =:= node()` degrades to the ordinary local `erlang:whereis/1` the
two-tuple `{registered, Name}` clauses already use — a node-qualified ref
that happens to name the caller's own node (e.g. `named:on:` targeting the
caller's node) needs no network hop. Otherwise resolves via `erpc:call/5`,
which also surfaces a genuinely unreachable `Node` as `node_down` rather
than letting the caller misreport it as `no_such_process` (the name may well
be registered there — the node just cannot be asked).
""".
-spec resolve_remote_registered(atom(), node()) -> pid() | undefined | node_down.
resolve_remote_registered(Name, Node) when Node =:= node() ->
    erlang:whereis(Name);
resolve_remote_registered(Name, Node) ->
    %% Every erpc-level failure (an unusual reason, or the remote
    %% erlang:whereis/1 itself somehow raising) collapses to the same
    %% node_down sentinel: this is a best-effort existence check, so "could
    %% not determine" and "node unreachable" are the same actionable fact.
    case beamtalk_call_failure:erpc(Node, erlang, whereis, [Name], ?BT_REMOTE_CALL_TIMEOUT) of
        {ok, Pid} when is_pid(Pid) -> Pid;
        {ok, undefined} -> undefined;
        {error, _} -> node_down
    end.

-doc """
BT-4676: the one resolver for name-resolving proxy refs (ADR 0079, 0126).
Answers the live pid, `undefined` when the name is not registered, or
`{node_down, Node}` when a node-qualified ref cannot be resolved because
`Node` is unreachable. Used by every send path (`async_send/4`, `cast_send/3`,
`sync_send/3,4`) so the three ref shapes are handled in exactly one place.
""".
-spec resolve_proxy(proxy_ref()) -> {ok, pid()} | undefined | {node_down, node()}.
resolve_proxy({registered, Name}) ->
    wrap_resolved(erlang:whereis(Name), undefined);
resolve_proxy({registered, Name, Node}) ->
    wrap_resolved(resolve_remote_registered(Name, Node), Node);
resolve_proxy({global, Name}) ->
    wrap_resolved(global:whereis_name(Name), undefined).

-spec wrap_resolved(pid() | undefined | node_down, node() | undefined) ->
    {ok, pid()} | undefined | {node_down, node()}.

wrap_resolved(Pid, _Node) when is_pid(Pid) -> {ok, Pid};
wrap_resolved(undefined, _Node) -> undefined;
wrap_resolved(node_down, Node) -> {node_down, Node}.

-doc """
BT-4676: route a send through a proxy ref. Name-only selectors (`isAlive`,
`isRegistered`, `registeredName`) are answered by the proxy itself; every
other selector resolves to a pid or a structured error. The caller decides
how to deliver the outcome (resolve/reject a future, or raise).
""".
-spec proxy_route(proxy_ref(), atom(), list()) ->
    {answer, term()} | {pid, pid()} | {error, #beamtalk_error{}}.
proxy_route(Ref, isAlive, []) ->
    {answer, is_pid_result(resolve_proxy(Ref))};
proxy_route(_Ref, isRegistered, []) ->
    {answer, true};
proxy_route({_Kind, Name}, registeredName, []) ->
    {answer, Name};
proxy_route({registered, Name, _Node}, registeredName, []) ->
    {answer, Name};
proxy_route(Ref, Selector, _Args) ->
    case resolve_or_error(Ref, Selector) of
        {ok, Pid} -> {pid, Pid};
        {error, _} = Error -> Error
    end.

-doc "The pid a proxy ref currently resolves to, or the structured error (for the FFI's `proxy_pid/2`).".
-spec resolve_proxy_or_error(proxy_ref(), atom()) -> {ok, pid()} | {error, #beamtalk_error{}}.

resolve_proxy_or_error(Ref, Selector) ->
    resolve_or_error(Ref, Selector).

%% The one resolve -> structured-error mapping; selector-independent.
-spec resolve_or_error(proxy_ref(), atom()) -> {ok, pid()} | {error, #beamtalk_error{}}.

resolve_or_error(Ref, Selector) ->
    case resolve_proxy(Ref) of
        {ok, Pid} -> {ok, Pid};
        undefined -> {error, no_such_process_error(Ref, Selector)};
        {node_down, Node} -> {error, node_down_error_record(Node, Selector)}
    end.

-spec is_pid_result({ok, pid()} | undefined | {node_down, node()}) -> boolean().

is_pid_result({ok, _Pid}) -> true;
is_pid_result(_) -> false.

-doc "Sync half of `proxy_route/3`: answer, raise, or continue with the resolved pid.".
-spec sync_send_proxy(proxy_ref(), atom(), list(), fun((pid()) -> term())) -> term().

sync_send_proxy(Ref, Selector, Args, WithPid) ->
    case proxy_route(Ref, Selector, Args) of
        {answer, Value} -> Value;
        {pid, Pid} -> WithPid(Pid);
        {error, Error} -> beamtalk_exception_handler:reraise(Error)
    end.

-doc "Construct a structured `no_such_process` error for the given proxy ref.".
-spec no_such_process_error(
    {registered, atom()} | {registered, atom(), node()} | {global, atom()}, atom()
) -> #beamtalk_error{}.

no_such_process_error({registered, Name}, Selector) ->
    no_such_process_error_record(Name, Selector);
no_such_process_error({registered, Name, _Node}, Selector) ->
    no_such_process_error_record(Name, Selector);
no_such_process_error({global, Name}, Selector) ->
    no_such_process_error_record(Name, Selector).

-spec no_such_process_error_record(atom(), atom()) -> #beamtalk_error{}.

no_such_process_error_record(Name, Selector) ->
    Error = beamtalk_error:new(
        no_such_process,
        unknown,
        Selector,
        beamtalk_error:chars_to_binary(
            io_lib:format(
                "No process is currently registered under name '~tp'", [Name]
            )
        )
    ),
    beamtalk_error:with_details(Error, #{name => Name}).

-doc """
Emit lifecycle telemetry, monitor `ActorPid`, send `exit(kill)`, then wait
for the `'DOWN'` message with a 5 s timeout.

Monitoring before sending the kill signal closes the TOCTOU window: if we
returned immediately after `exit/2` (which is asynchronous), a racing
`isAlive` check could still return `true` because the BEAM scheduler had
not yet processed the signal.  Waiting for `'DOWN'` guarantees the process
is gone before this function returns.  If the process is already dead, the
`'DOWN'` message arrives instantly.

A `'DOWN'` whose reason says the actor's node could not be reached
(`noconnection`, classified by `beamtalk_call_failure:classify/2` exactly as
`sync_call/5` classifies a failed send) is not a kill: the signal was lost
and the actor may still be running on the far side, so it answers
`{error, node_down}` (BT-5192). Every other `'DOWN'` reason (`killed`, an
earlier exit, `noproc` for an already-dead actor) means the actor is gone.

Returns `ok` on success, or `{error, Error}` if the node was unreachable or
the `'DOWN'` message is not received within 5 000 ms.  The error is a structured `#beamtalk_error{}` with
kind `timeout` and the message `"Actor kill timed out: did not receive DOWN
within 5000ms"`.  Building it here keeps the message in one place: callers
pass the error directly to `beamtalk_future:reject/2` (async path) or wrap it
with `beamtalk_exception_handler:ensure_wrapped/1` before raising (sync path).

Called by both `async_send/4` and `sync_send/3` for the `kill` selector.
""".
-spec kill_and_wait(pid()) -> ok | {error, #beamtalk_error{}}.
kill_and_wait(ActorPid) ->
    %% Emit lifecycle telemetry event for kill request.
    Class = beamtalk_actor:lookup_class(ActorPid),
    beamtalk_telemetry:emit(
        [beamtalk, actor, lifecycle, kill],
        #{},
        #{pid => ActorPid, class => Class}
    ),
    Ref = erlang:monitor(process, ActorPid),
    exit(ActorPid, kill),
    receive
        {'DOWN', Ref, process, ActorPid, Reason} ->
            case beamtalk_call_failure:classify(exit, Reason) of
                {unreachable, _} ->
                    {error,
                        node_down_error_record(
                            beamtalk_call_failure:unreachable_node(Reason, node(ActorPid)), kill
                        )};
                _Gone ->
                    ok
            end
    after 5000 ->
        erlang:demonitor(Ref, [flush]),
        {error,
            beamtalk_error:new(
                timeout,
                unknown,
                kill,
                <<"Actor kill timed out: did not receive DOWN within 5000ms">>
            )}
    end.

-doc """
Build a propagated context map for cross-actor message sends (ADR 0069 Phase 2b).

Returns an extensible map holding only the context that is actually present
(BT-4113), so a send from a process with no context carries `#{}`:
- `otel`: the OTel trace context, when `otel_ctx` is loaded and the current
  context is non-empty
- `trace_ctx`: the application-level trace context (set via
  set_trace_context/1), when non-empty
- `causal`: causal trace IDs, when tracing has set them
The receiver (restore_propagated_ctx/1) treats a missing key as "none" and
clears what it has, so omitting an empty key loses nothing.
""".
-spec get_propagated_ctx() -> map().
get_propagated_ctx() ->
    WithOtel =
        case get_otel_ctx() of
            undefined -> #{};
            Otel when Otel =:= #{} -> #{};
            Otel -> #{otel => Otel}
        end,
    WithTrace =
        case get('$beamtalk_trace_ctx') of
            TraceCtx when is_map(TraceCtx), map_size(TraceCtx) > 0 ->
                WithOtel#{trace_ctx => TraceCtx};
            _ ->
                WithOtel
        end,
    %% Include causal trace IDs for cross-actor linking.
    %% Only present when tracing is enabled and dispatch_span has run.
    case get('$beamtalk_trace_id') of
        undefined ->
            WithTrace;
        TraceId ->
            SpanId = get('$beamtalk_span_id'),
            WithTrace#{causal => #{trace_id => TraceId, span_id => SpanId}}
    end.

-doc """
Like get_propagated_ctx/0 but includes call_stack for cycle detection.
Only used by sync_send/3,4 — async/cast sends must NOT carry call_stack
because the sender is not blocked, so B calling A back is not a deadlock.
""".
-spec get_sync_propagated_ctx() -> map().
get_sync_propagated_ctx() ->
    ExistingStack =
        case get(?BT_CALL_STACK_KEY) of
            undefined -> [];
            CS -> CS
        end,
    (get_propagated_ctx())#{call_stack => [self() | ExistingStack]}.

-doc """
Get current OTel trace context if otel_ctx module is available.
Returns 'undefined' when OpenTelemetry is not loaded — no compile-time dependency.
""".
-spec get_otel_ctx() -> term().
get_otel_ctx() ->
    case erlang:function_exported(otel_ctx, get_current, 0) of
        true -> erlang:apply(otel_ctx, get_current, []);
        false -> undefined
    end.

-doc """
Spawn a lightweight watcher that monitors both the actor and future.
Closes the TOCTOU race in async_send — if the actor dies during
message processing (after the cast but before the future is resolved),
the watcher rejects the future with a structured error. ADR 0126 §7.1:
a `noconnection` DOWN reason (the actor's node partitioned or was never
connected — verified empirically, see erlang:monitor/2 docs) rejects with
`node_down` instead of `actor_dead`, matching the sync/timeout paths'
exit mapping. The watcher also monitors the future process so it can
clean up promptly when the future completes normally. There is no short
time cutoff (BT-5020): the watcher ends when either process dies or the future
tells it it settled, and only a hard safety cap (below) bounds it. A pending
future with no producer is itself capped by beamtalk_future (?PENDING_CAP_MS),
unless a watcher registers as its producer. This watcher does so, lifting
that cap while it lives, so it carries its own cap (?FUTURE_WATCHER_CAP_MS,
BT-5084): an actor that stays alive without settling the future has the
future rejected and the watcher ended, with an ERROR log. The cap defaults to
one hour, so an async send that really takes longer than that has its future
rejected with `future_expired` (a deliberate bound; raise it with the
`future_watcher_cap_ms` application env).
""".
-spec spawn_future_watcher(pid(), pid(), atom()) -> pid().
spawn_future_watcher(ActorPid, FuturePid, Selector) ->
    spawn(fun() ->
        ActorRef = erlang:monitor(process, ActorPid),
        FutureRef = erlang:monitor(process, FuturePid),
        %% Tell the future a live producer exists: lifts its orphan cap.
        FuturePid ! {producer, self()},
        %% BT-5084: a producer lifts the future's own cap, so an actor that stays
        %% alive and never settles would pin this watcher and the future forever.
        %% Read in the watcher, not the sender, so a send pays nothing for it.
        WatcherCap = application:get_env(
            beamtalk_runtime, future_watcher_cap_ms, ?FUTURE_WATCHER_CAP_MS
        ),
        receive
            {'DOWN', ActorRef, process, ActorPid, Reason} ->
                %% Actor died (or its node went away) — reject future
                %% (no-op if already resolved).
                Error =
                    case Reason of
                        noconnection -> node_down_error_record(node(ActorPid), Selector);
                        _ -> actor_dead_error_record(Selector)
                    end,
                beamtalk_future:reject(FuturePid, Error),
                erlang:demonitor(FutureRef, [flush]);
            {'DOWN', FutureRef, process, FuturePid, _Reason} ->
                %% Future completed and its process ended — clean up
                erlang:demonitor(ActorRef, [flush]);
            {future_settled, FuturePid} ->
                %% Future settled (resolved/rejected): nothing left to watch
                erlang:demonitor(ActorRef, [flush]),
                erlang:demonitor(FutureRef, [flush])
        after WatcherCap ->
            ?LOG_ERROR(
                "Async send still unsettled at the future watcher cap; rejecting its future",
                #{
                    actor => ActorPid,
                    future => FuturePid,
                    selector => Selector,
                    cap_ms => WatcherCap,
                    domain => [beamtalk, runtime]
                }
            ),
            beamtalk_future:reject(FuturePid, future_watcher_expired_error(Selector)),
            erlang:demonitor(ActorRef, [flush]),
            erlang:demonitor(FutureRef, [flush])
        end
    end).

-doc "Error for a future its actor never settled within the watcher cap (BT-5084).".
-spec future_watcher_expired_error(atom()) -> #beamtalk_error{}.
future_watcher_expired_error(Selector) ->
    Err0 = beamtalk_error:new(future_expired, 'Future', Selector),
    beamtalk_error:with_message(
        Err0,
        <<"The actor did not settle this future within the watcher cap; it was rejected">>
    ).
