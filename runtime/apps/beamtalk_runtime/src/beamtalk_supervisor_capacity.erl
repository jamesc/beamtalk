%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_capacity).

%%% **DDD Context:** Actor System Context

-moduledoc """
`maxChildren` capacity enforcement for a Beamtalk `DynamicSupervisor` pool.

Owns the BT-4068 enforcement state (an ETS counter table plus a monitor
process that releases dead children's slots), the atomic in-supervisor start
check (`checked_dynamic_start_n/2`), the caller-side token handshake
(`with_child_capacity/6`) and the legacy check-then-act path for pools
without enforcement state. Split out of `beamtalk_supervisor` (BT-4705); a
pure move with no behaviour change.

`checked_dynamic_start/1..4` stay exported from `beamtalk_supervisor` as thin
delegates to `checked_dynamic_start_n/2`: `wrap_capped_child_spec/3` installs
`{beamtalk_supervisor, checked_dynamic_start, [Params]}` in the OTP child
template, and OTP replays that MFA verbatim on restart, so the module and
name must not change.

## References

- BT-3964, BT-4068: maxChildren enforcement
- runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl
""".

-export([
    check_max_children/2,
    max_children/1,
    with_child_capacity/6,
    init_child_capacity/1,
    wrap_capped_child_spec/3,
    release_child_capacity/2,
    checked_dynamic_start_n/2
]).

-ifdef(TEST).
-export([max_children_error/3]).
-endif.

-include("beamtalk.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").

-doc """
Validate a DynamicSupervisor's `maxChildren` value (BT-3964).

Accepts a non-negative integer or `infinity` (Beamtalk `#infinity`); anything
else is a `type_error` carrying the offending value in `details`.
""".
-spec check_max_children(atom(), term()) ->
    {ok, non_neg_integer() | infinity} | {error, #beamtalk_error{}}.
check_max_children(_ClassName, infinity) ->
    {ok, infinity};
check_max_children(_ClassName, Max) when is_integer(Max), Max >= 0 ->
    {ok, Max};
check_max_children(ClassName, Other) ->
    {error,
        beamtalk_error:bad_value(
            ClassName,
            maxChildren,
            "maxChildren must be a non-negative Integer or #infinity, got ~tp",
            [Other],
            Other
        )}.

-spec checked_dynamic_start_n(map(), [term()]) -> {ok, pid()} | {error, term()}.
checked_dynamic_start_n(
    #{tid := Tid, max := Max, monitor := MonPid, m := M, f := F, args := StaticArgs}, Extra
) ->
    {Manual, RealExtra} =
        case Extra of
            [{?MAX_CHILDREN_TOKEN, _Ref} = Token | Rest] ->
                case ets:take(Tid, {manual, Token}) of
                    [_] ->
                        {true, Rest};
                    [] ->
                        %% A consumed token: an OTP restart replay of a
                        %% manual start. Strip the token so the child
                        %% restarts with its original args.
                        {false, Rest}
                end;
            _ ->
                %% Genuinely untokened (a remote caller, which
                %% `with_child_capacity/5` never tokens, or a direct
                %% `supervisor:start_child/2`): admit like a restart.
                {false, Extra}
        end,
    Admitted =
        case Manual of
            false ->
                _ = ets:update_counter(Tid, count, 1),
                true;
            true ->
                case ets:update_counter(Tid, count, 1) of
                    N when N =< Max -> true;
                    _Over ->
                        _ = ets:update_counter(Tid, count, -1),
                        false
                end
        end,
    case Admitted of
        true -> run_capped_start(Tid, MonPid, M, F, StaticArgs ++ RealExtra);
        false -> {error, {?MAX_CHILDREN_REFUSED, Max}}
    end.

-doc """
Apply the wrapped template MFA, tracking a started child and releasing the
slot on any failure. Runs in the supervisor process.
""".
-spec run_capped_start(ets:tid(), pid(), module(), atom(), [term()]) ->
    {ok, pid()} | {error, term()}.
run_capped_start(Tid, MonPid, M, F, Args) ->
    %% bt-catcher-audit: not-applicable-reraises - the catch clause frees the
    %% ETS slot and re-raises, so the failure still propagates to OTP.
    try apply(M, F, Args) of
        {ok, Pid} when is_pid(Pid) ->
            track_capped_child(Tid, MonPid, Pid),
            {ok, Pid};
        {ok, Pid, Extra} when is_pid(Pid) ->
            track_capped_child(Tid, MonPid, Pid),
            {ok, Pid, Extra};
        ignore ->
            _ = ets:update_counter(Tid, count, -1),
            ignore;
        {error, _} = Error ->
            _ = ets:update_counter(Tid, count, -1),
            Error;
        Other ->
            _ = ets:update_counter(Tid, count, -1),
            Other
    catch
        Class:Reason:Stack ->
            _ = ets:update_counter(Tid, count, -1),
            erlang:raise(Class, Reason, Stack)
    end.

-doc """
Record a started child in the capacity table and hand it to the monitor
process, which releases the slot when the child dies.
""".
-spec track_capped_child(ets:tid(), pid(), pid()) -> ok.
track_capped_child(Tid, MonPid, Pid) ->
    _ = ets:insert(Tid, {{child, Pid}}),
    MonPid ! {beamtalk_sup_child_started, Pid},
    ok.

-doc """
Read the `maxChildren` limit `dynamic_init/3` stashed in the supervisor's
process dictionary. A supervisor not started through `dynamic_init/3` (or one
that has already exited) has no entry and is treated as unlimited; a dead
supervisor is then reported as `stale_handle` by the `supervisor:start_child/2`
call that follows.
""".
-spec max_children(pid()) -> non_neg_integer() | infinity.
max_children(SupPid) when is_pid(SupPid), node(SupPid) =:= node() ->
    case erlang:process_info(SupPid, {dictionary, ?MAX_CHILDREN_KEY}) of
        {{dictionary, ?MAX_CHILDREN_KEY}, Max} when is_integer(Max) -> Max;
        _ -> infinity
    end;
max_children(_SupPid) ->
    infinity.

-doc """
Run `StartFun` (a `supervisor:start_child/2` call with `ExtraArgs`, plus a
capacity token when the pool counts starts itself) honouring the
DynamicSupervisor's `maxChildren` limit (BT-3964); otherwise return
`{error, #beamtalk_error{kind = max_children}}` with the limit in
`details` (`#{max_children => Max}`) so callers can back off and retry.

On a pool started with enforcement state (BT-4068, the common case) the limit
is checked atomically inside the supervisor process by
`checked_dynamic_start/1..4`: one `supervisor:start_child/2` call per
`startChild`, and concurrent starters can never overshoot the limit. The
caller's only job is to hand the wrapper a fresh token so it can tell this
manual start (refusable) from an OTP restart replay (never refused).
Supervisor-driven restarts are never refused on either path.

`StartFun` passes a `{error, {?MAX_CHILDREN_REFUSED, Max}}` refusal through
untouched; it is mapped to the structured error here, the one place the
`max_children` error is built.

Rejections log at debug: hitting the cap is expected back-pressure under load,
and logging every refused start would flood the log exactly when the system
is busiest.
""".
-spec with_child_capacity(
    atom(), atom(), pid(), non_neg_integer() | infinity, [term()], fun(([term()]) -> Result)
) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_child_capacity(SupClass, Selector, SupPid, MaxChildren, ExtraArgs, StartFun) ->
    case MaxChildren of
        infinity ->
            StartFun(ExtraArgs);
        Max ->
            case child_capacity_state(SupPid) of
                {ok, {Tid, _MonPid}} ->
                    Token = {?MAX_CHILDREN_TOKEN, make_ref()},
                    %% A dead pool's table is already gone; the
                    %% `supervisor:start_child/2` below then exits and
                    %% `with_live_supervisor/3` reports `stale_handle`.
                    _ =
                        try
                            ets:insert(Tid, {{manual, Token}})
                        catch
                            _:_ -> ok
                        end,
                    case StartFun([Token | ExtraArgs]) of
                        {error, {?MAX_CHILDREN_REFUSED, Limit}} ->
                            max_children_error(SupClass, Selector, Limit);
                        Result ->
                            Result
                    end;
                undefined ->
                    with_legacy_child_capacity(
                        SupClass, Selector, SupPid, Max, ExtraArgs, StartFun
                    )
            end
    end.

-doc """
Caller-side `maxChildren` check (BT-3964) for pools without enforcement
state: pools started before BT-4068, and test supervisors that only stash the
limit. Check-then-act (`supervisor:count_children/1`, then start), so exact
when one process starts children (e.g. a single acceptor), but concurrent
starters racing past the check can briefly exceed the limit by up to the
number of racers. Pools started through `dynamic_init/3` never take this
path.

The count is every dynamic child the supervisor tracks — what
`countChildren/1` (Beamtalk `count`) reports plus any child whose restart is
pending retry. `supervisor:count_children/1` is O(1) for `simple_one_for_one`.
Supervisor-driven restarts are never refused.
""".
-spec with_legacy_child_capacity(
    atom(), atom(), pid(), non_neg_integer(), [term()], fun(([term()]) -> Result)
) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_legacy_child_capacity(SupClass, Selector, SupPid, Max, ExtraArgs, StartFun) ->
    Counts = supervisor:count_children(SupPid),
    %% `workers` + `supervisors` is every dynamic child, including one
    %% whose supervisor-driven restart is pending retry (not yet
    %% `active`): counting it keeps a retry that later succeeds from
    %% pushing the pool past Max.
    Total =
        proplists:get_value(workers, Counts, 0) +
            proplists:get_value(supervisors, Counts, 0),
    case Total >= Max of
        false ->
            StartFun(ExtraArgs);
        true ->
            max_children_error(SupClass, Selector, Max)
    end.

-doc """
The `{error, #beamtalk_error{kind = max_children}}` return for a start refused
at the cap, on either enforcement path. The single place the error is built.
""".
-spec max_children_error(atom(), atom(), non_neg_integer()) -> {error, #beamtalk_error{}}.
max_children_error(SupClass, Selector, Max) ->
    ?LOG_DEBUG("DynamicSupervisor at maxChildren", #{
        supervisor => SupClass,
        selector => Selector,
        max_children => Max,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(
        max_children,
        SupClass,
        Selector,
        <<"Back off and retry, or free a slot with terminateChild:">>
    ),
    Error1 = beamtalk_error:with_message_fmt(
        Error0, "DynamicSupervisor is at its maxChildren limit (~b)", [Max]
    ),
    {error, beamtalk_error:with_details(Error1, #{max_children => Max})}.

-doc """
Read the BT-4068 enforcement state (`?MAX_CHILDREN_STATE_KEY`, `{Tid,
MonPid}`) from a local pool's process dictionary. `undefined` for an uncapped
pool, a pool started before BT-4068, a dead pool, or a remote pool (whose ETS
table is unusable from here; remote pools stay unenforced, as BT-3964 left
them — the wrapper admits their untokened starts the way it admits restarts).
""".
-spec child_capacity_state(pid()) -> {ok, {ets:tid(), pid()}} | undefined.
child_capacity_state(SupPid) when is_pid(SupPid), node(SupPid) =:= node() ->
    case erlang:process_info(SupPid, {dictionary, ?MAX_CHILDREN_STATE_KEY}) of
        {{dictionary, ?MAX_CHILDREN_STATE_KEY}, {Tid, MonPid}} ->
            {ok, {Tid, MonPid}};
        _ ->
            undefined
    end;
child_capacity_state(_SupPid) ->
    undefined.

-doc """
Install a capped pool's BT-4068 enforcement state. Must run in the
supervisor process (from `dynamic_init/3`): the counter table is owned by the
caller, so it dies with the pool, and the monitor exits with it.

Returns `none` for an uncapped pool, else `{Tid, MonPid}` for
`?MAX_CHILDREN_STATE_KEY`. The table holds `{count, N}` plus transient
`{{manual, Token}}` rows (one per in-flight manual start, consumed by the
wrapper) and `{{child, Pid}}` rows (one per live child, taken exactly once
when its slot is released). A token left behind by a caller that died
mid-start is inert: references are never re-minted, and the table dies with
the pool.

`init/1` can run again in the same process (a supervisor's `code_change`
re-runs it with the stored args): state that is still usable is reused, so a
re-run neither leaks a table nor resets the count of live children. If only
the monitor died, the table is reused with a fresh monitor — which cannot
resync the already-running children from in here (a `which_children/1` call
to self would deadlock), so their deaths stop releasing slots and the pool
fails closed once full.
""".
-spec init_child_capacity(infinity | non_neg_integer()) -> none | {ets:tid(), pid()}.
init_child_capacity(infinity) ->
    none;
init_child_capacity(Max) when is_integer(Max), Max >= 0 ->
    case get(?MAX_CHILDREN_STATE_KEY) of
        {Tid, MonPid} ->
            case {ets:info(Tid), is_pid(MonPid) andalso is_process_alive(MonPid)} of
                {undefined, _} ->
                    stop_child_capacity_monitor(MonPid),
                    fresh_child_capacity();
                {_, false} ->
                    {Tid, spawn_monitor_for_capacity(Tid)};
                {_, true} ->
                    {Tid, MonPid}
            end;
        _ ->
            fresh_child_capacity()
    end.

-doc """
Allocate a fresh counter table and monitor. The caller must be the pool
itself (see `init_child_capacity/1`).
""".
-spec fresh_child_capacity() -> {ets:tid(), pid()}.
fresh_child_capacity() ->
    Tid = ets:new(beamtalk_sup_child_capacity, [public, set, {write_concurrency, true}]),
    true = ets:insert(Tid, {count, 0}),
    {Tid, spawn_monitor_for_capacity(Tid)}.

-doc """
Spawn the monitor that releases dead children's capacity slots. Unlinked:
a link would deliver the monitor's exit to the pool, which must never die
for it. The monitor watches the pool instead, and exits with it.
""".
-spec spawn_monitor_for_capacity(ets:tid()) -> pid().
spawn_monitor_for_capacity(Tid) ->
    SupPid = self(),
    spawn(fun() -> child_capacity_monitor(Tid, SupPid) end).

-spec stop_child_capacity_monitor(term()) -> ok.
stop_child_capacity_monitor(MonPid) when is_pid(MonPid) ->
    _ =
        try
            exit(MonPid, kill)
        catch
            _:_ -> ok
        end,
    ok;
stop_child_capacity_monitor(_) ->
    ok.

-doc """
Rewrite a capped pool's `simple_one_for_one` child template so starts go
through `checked_dynamic_start/1..4`. Uncapped pools (`none`) keep their
template as is. Wraps any template MFA generically — worker
(`start_dynamic_child/2,3,4`) or nested-supervisor
(`start_dynamic_supervisor_child/2,3,4`) — since OTP appends the start's
extra args after the single `Params` static arg either way.
""".
-spec wrap_capped_child_spec(map(), non_neg_integer() | infinity, none | {ets:tid(), pid()}) ->
    map().
wrap_capped_child_spec(Spec, _Max, none) ->
    Spec;
wrap_capped_child_spec(#{start := {M, F, A}} = Spec, Max, {Tid, MonPid}) ->
    Params = #{tid => Tid, max => Max, monitor => MonPid, m => M, f => F, args => A},
    Spec#{start => {beamtalk_supervisor, checked_dynamic_start, [Params]}}.

-doc """
Monitor loop for one capped pool's capacity table: monitor each started
child, release its slot on death. Slot release goes through
`release_child_slot/2`, shared with the synchronous `terminateChild:` path,
so a death racing a terminate frees the slot exactly once whichever order
they land in.
""".
-spec child_capacity_monitor(ets:tid(), pid()) -> no_return().
child_capacity_monitor(Tid, SupPid) ->
    SupRef = monitor(process, SupPid),
    child_capacity_monitor_loop(Tid, SupPid, SupRef).

-spec child_capacity_monitor_loop(ets:tid(), pid(), reference()) -> no_return().
child_capacity_monitor_loop(Tid, SupPid, SupRef) ->
    receive
        {'DOWN', SupRef, process, SupPid, _Reason} ->
            exit(normal);
        {beamtalk_sup_child_started, Pid} when is_pid(Pid) ->
            _ = monitor(process, Pid),
            child_capacity_monitor_loop(Tid, SupPid, SupRef);
        {'DOWN', _Ref, process, Pid, _Reason} ->
            release_child_slot(Tid, Pid),
            child_capacity_monitor_loop(Tid, SupPid, SupRef);
        _Other ->
            child_capacity_monitor_loop(Tid, SupPid, SupRef)
    end.

-doc """
Free one child's capacity slot, exactly once: `ets:take/2` removes the
`{{child, Pid}}` membership row, and only the taker decrements. The
decrement floors at zero so a double release (which the take already rules
out) could never wedge the counter negative and fail the pool open.
""".
-spec release_child_slot(ets:tid(), pid()) -> ok.
release_child_slot(Tid, Pid) ->
    Taken =
        try
            ets:take(Tid, {child, Pid})
        catch
            _:_ -> []
        end,
    case Taken of
        [_] ->
            _ =
                try
                    ets:update_counter(Tid, count, {2, -1, 0, 0})
                catch
                    _:_ -> ok
                end,
            ok;
        _ ->
            ok
    end.

-doc """
Synchronously free a terminated child's capacity slot so a `startChild`
immediately after `terminateChild:` sees it (BT-4068): without this the
monitor's asynchronous DOWN would leave the slot occupied for a moment and
the next start would spuriously refuse. A no-op for uncapped pools, legacy
pools, and remote pools (whose table is unusable from here; their DOWN path
still releases the slot, asynchronously).
""".
-spec release_child_capacity(pid(), pid()) -> ok.
release_child_capacity(SupPid, ChildPid) ->
    case child_capacity_state(SupPid) of
        {ok, {Tid, _MonPid}} -> release_child_slot(Tid, ChildPid);
        undefined -> ok
    end.
