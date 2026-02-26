%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk Future/Promise Runtime
%%%
%%% **DDD Context:** Concurrency
%%%
%%% Implements futures (promises) as lightweight BEAM processes.
%%% Beamtalk is async-first: message sends return futures by default.
%%%
%%% Each future is a BEAM process (~2KB) that can be in one of three states:
%%% - pending: waiting for a value
%%% - resolved: has a successful value
%%% - rejected: has an error
%%%
%%% Completed futures (resolved or rejected) will automatically terminate after
%%% 5 minutes of inactivity to prevent memory leaks.
%%%
%%% ## Protocol
%%%
%%% Messages sent to a future process:
%%% - `{resolve, Value}` - Transition to resolved state with Value
%%% - `{reject, Reason}` - Transition to rejected state with Reason
%%% - `{await, Pid}` - Register Pid to be notified when future completes
%%% - `{await, Pid, Timeout}` - Register Pid with timeout (milliseconds)
%%% - `{add_callback, resolved, Fun}` - Register callback for resolved state
%%% - `{add_callback, rejected, Fun}` - Register callback for rejected state
%%%
%%% Messages received from a future process:
%%% - `{future_resolved, FuturePid, Value}` - Future was resolved
%%% - `{future_rejected, FuturePid, Reason}` - Future was rejected
%%% - `{future_timeout, FuturePid}` - Await timed out
%%%
%%% ## Example
%%%
%%% ```erlang
%%% %% Create a new future
%%% Future = beamtalk_future:new(),
%%%
%%% %% Spawn a task that will resolve the future
%%% spawn(fun() ->
%%%     Result = expensive_computation(),
%%%     beamtalk_future:resolve(Future, Result)
%%% end),
%%%
%%% %% Await the result (blocks until resolved)
%%% Value = beamtalk_future:await(Future),
%%%
%%% %% Or use a timeout
%%% case beamtalk_future:await(Future, 5000) of
%%%     {ok, Value} -> Value;
%%%     {error, timeout} -> default_value
%%% end.
%%% ```
%%%
%%% ## Callbacks
%%%
%%% ```erlang
%%% Future = beamtalk_future:new(),
%%% beamtalk_future:when_resolved(Future, fun(Value) ->
%%%     io:format("Got value: ~p~n", [Value])
%%% end),
%%% beamtalk_future:resolve(Future, 42).
%%% ```

-module(beamtalk_future).
-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").
-export([
    new/0,
    resolve/2,
    reject/2,
    await/1, await/2,
    await_forever/1,
    when_resolved/2,
    when_rejected/2,
    is_future/1,
    maybe_await/1,
    pid/1
]).

-export_type([future/0]).

%% @doc Opaque tagged tuple wrapping a future process.
%% This is an internal runtime detail — Beamtalk code never sees futures
%% directly. They are always resolved before reaching the language layer.
-type future() :: {beamtalk_future, pid()}.

%% @doc Create a new future in the pending state.
%% Returns a tagged tuple `{beamtalk_future, Pid}' that can be detected
%% by the dispatch layer for auto-awaiting in chained message sends.
-spec new() -> future().
new() ->
    {beamtalk_future, spawn(fun() -> pending([]) end)}.

%% @doc Check if a value is a future.
-spec is_future(term()) -> boolean().
is_future({beamtalk_future, Pid}) when is_pid(Pid) -> true;
is_future(_) -> false.

%% @doc Extract the raw pid from a tagged future.
%% Used by the codegen layer to obtain the underlying process pid before
%% passing it to beamtalk_actor:async_send/4, and by tests for direct
%% process inspection.
-spec pid(future()) -> pid().
pid({beamtalk_future, Pid}) -> Pid.

%% @doc Resolve a future with a value.
%% If the future is already resolved or rejected, this is a no-op.
%% Notifies all waiting processes and executes all resolved callbacks.
%% Accepts both tagged futures and raw pids (for internal actor use).
-spec resolve(future() | pid(), term()) -> ok.
resolve({beamtalk_future, Pid}, Value) ->
    Pid ! {resolve, Value},
    ok;
resolve(Pid, Value) when is_pid(Pid) ->
    Pid ! {resolve, Value},
    ok.

%% @doc Reject a future with an error reason.
%% If the future is already resolved or rejected, this is a no-op.
%% Notifies all waiting processes and executes all rejected callbacks.
%% Accepts both tagged futures and raw pids (for internal actor use).
-spec reject(future() | pid(), term()) -> ok.
reject({beamtalk_future, Pid}, Reason) ->
    Pid ! {reject, Reason},
    ok;
reject(Pid, Reason) when is_pid(Pid) ->
    Pid ! {reject, Reason},
    ok.

%% @doc Block the calling process until the future is resolved or rejected.
%% Uses a default timeout of 30 seconds to prevent indefinite blocking.
%% Returns the value if resolved, or throws {future_rejected, Reason} if rejected.
%% Throws #beamtalk_error{kind = timeout} if the future doesn't complete in time.
-spec await(future() | pid()) -> term().
await(Future) ->
    await(Future, 30000).

%% @doc Block the calling process until the future is resolved, rejected, or times out.
%% Returns the value if resolved, throws {future_rejected, Reason} if rejected,
%% or throws #beamtalk_error{kind = timeout} if the timeout expires.
%% Accepts both tagged futures and raw pids (for internal actor use).
%%
%% BT-918 / ADR 0043: With sync-by-default, actor sends return values directly
%% (not futures). To preserve backward compat during the migration period, any
%% non-future, non-pid value is returned as-is rather than crashing.
-spec await(future() | pid() | term(), timeout()) -> term().
await({beamtalk_future, Pid}, Timeout) ->
    await_pid(Pid, Timeout);
await(Pid, Timeout) when is_pid(Pid) ->
    await_pid(Pid, Timeout);
await(Value, _Timeout) ->
    %% Non-future value — already resolved, return as-is.
    Value.

%% @doc Block the calling process until the future is resolved or rejected.
%% Waits indefinitely with no timeout. Use this for operations that may take
%% an arbitrarily long time to complete.
%% Returns the value if resolved, or throws {future_rejected, Reason} if rejected.
-spec await_forever(future() | pid()) -> term().
await_forever(Future) ->
    await(Future, infinity).

%% @doc Register a callback to be executed when the future is resolved.
%% If the future is already resolved, the callback is executed immediately.
%% If the future is rejected, the callback is never executed.
-spec when_resolved(future() | pid(), fun((term()) -> term())) -> ok.
when_resolved({beamtalk_future, Pid}, Callback) ->
    Pid ! {add_callback, resolved, Callback},
    ok;
when_resolved(Pid, Callback) when is_pid(Pid) ->
    Pid ! {add_callback, resolved, Callback},
    ok.

%% @doc Register a callback to be executed when the future is rejected.
%% If the future is already rejected, the callback is executed immediately.
%% If the future is resolved, the callback is never executed.
-spec when_rejected(future() | pid(), fun((term()) -> term())) -> ok.
when_rejected({beamtalk_future, Pid}, Callback) ->
    Pid ! {add_callback, rejected, Callback},
    ok;
when_rejected(Pid, Callback) when is_pid(Pid) ->
    Pid ! {add_callback, rejected, Callback},
    ok.

%% @doc If the value is a future, await it; otherwise return as-is.
%% Used by codegen to auto-await binary op operands.
-spec maybe_await(term()) -> term().
maybe_await({beamtalk_future, _} = Future) -> await(Future);
maybe_await(Value) -> Value.

%%% Internal helpers

%% @private
%% Core await implementation operating on a raw pid.
-spec await_pid(pid(), timeout()) -> term().
await_pid(Pid, infinity) ->
    Pid ! {await, self(), infinity},
    receive
        {future_resolved, Pid, Value} ->
            Value;
        {future_rejected, Pid, Reason} ->
            throw({future_rejected, Reason})
    end;
await_pid(Pid, Timeout) ->
    Pid ! {await, self(), Timeout},
    receive
        {future_resolved, Pid, Value} ->
            Value;
        {future_rejected, Pid, Reason} ->
            throw({future_rejected, Reason});
        {future_timeout, Pid} ->
            throw(make_timeout_error())
    after Timeout ->
        %% Flush any stale {future_timeout, Pid} that may arrive just after
        %% the `after` clause fires (the future process timer fires slightly later).
        receive
            {future_timeout, Pid} -> ok
        after 0 -> ok
        end,
        throw(make_timeout_error())
    end.

%%% Internal state machine

%% @private
%% Pending state: waiting for resolve or reject
pending(Waiters) ->
    receive
        {resolve, Value} ->
            %% Notify all waiters and execute resolved callbacks
            notify_waiters(Waiters, resolved, Value),
            resolved(Value);
        {reject, Reason} ->
            %% Notify all waiters and execute rejected callbacks
            notify_waiters(Waiters, rejected, Reason),
            rejected(Reason);
        {await, Pid} ->
            %% Add to waiters list (no timeout)
            pending([{await, Pid, infinity} | Waiters]);
        {await, Pid, Timeout} ->
            %% Add to waiters list with timeout
            TRef =
                case Timeout of
                    infinity -> infinity;
                    _ -> erlang:send_after(Timeout, self(), {timeout, Pid})
                end,
            pending([{await, Pid, TRef} | Waiters]);
        {timeout, Pid} ->
            %% Send timeout message to the waiter
            Pid ! {future_timeout, self()},
            %% Remove this waiter from the list
            NewWaiters = lists:filter(
                fun
                    ({await, P, _}) -> P =/= Pid;
                    (_) -> true
                end,
                Waiters
            ),
            pending(NewWaiters);
        {add_callback, resolved, Callback} ->
            pending([{callback, resolved, Callback} | Waiters]);
        {add_callback, rejected, Callback} ->
            pending([{callback, rejected, Callback} | Waiters])
    end.

%% @private
%% Resolved state: has a value
%% Terminates after 5 minutes of inactivity to prevent memory leaks.
resolved(Value) ->
    receive
        {resolve, _} ->
            %% Already resolved, ignore
            resolved(Value);
        {reject, _} ->
            %% Already resolved, ignore
            resolved(Value);
        {await, Pid} ->
            %% Immediately notify the waiter
            Pid ! {future_resolved, self(), Value},
            resolved(Value);
        {await, Pid, _Timeout} ->
            %% Immediately notify the waiter (ignore timeout)
            Pid ! {future_resolved, self(), Value},
            resolved(Value);
        {timeout, _Pid} ->
            %% Ignore timeouts in resolved state
            resolved(Value);
        {add_callback, resolved, Callback} ->
            %% Execute callback immediately
            execute_callback(Callback, Value),
            resolved(Value);
        {add_callback, rejected, _Callback} ->
            %% Ignore rejected callback
            resolved(Value)
    after 300000 ->
        %% Terminate after 5 minutes of inactivity
        ok
    end.

%% @private
%% Rejected state: has an error reason
%% Terminates after 5 minutes of inactivity to prevent memory leaks.
rejected(Reason) ->
    receive
        {resolve, _} ->
            %% Already rejected, ignore
            rejected(Reason);
        {reject, _} ->
            %% Already rejected, ignore
            rejected(Reason);
        {await, Pid} ->
            %% Immediately notify the waiter
            Pid ! {future_rejected, self(), Reason},
            rejected(Reason);
        {await, Pid, _Timeout} ->
            %% Immediately notify the waiter (ignore timeout)
            Pid ! {future_rejected, self(), Reason},
            rejected(Reason);
        {timeout, _Pid} ->
            %% Ignore timeouts in rejected state
            rejected(Reason);
        {add_callback, rejected, Callback} ->
            %% Execute callback immediately
            execute_callback(Callback, Reason),
            rejected(Reason);
        {add_callback, resolved, _Callback} ->
            %% Ignore resolved callback
            rejected(Reason)
    after 300000 ->
        %% Terminate after 5 minutes of inactivity
        ok
    end.

%% @private
%% Notify all waiters when future completes
notify_waiters(Waiters, State, ValueOrReason) ->
    lists:foreach(
        fun
            ({await, Pid, TRef}) ->
                %% Cancel timeout timer if it exists
                case TRef of
                    infinity -> ok;
                    _ -> erlang:cancel_timer(TRef)
                end,
                %% Send notification
                case State of
                    resolved -> Pid ! {future_resolved, self(), ValueOrReason};
                    rejected -> Pid ! {future_rejected, self(), ValueOrReason}
                end;
            ({callback, CallbackState, Callback}) when CallbackState =:= State ->
                execute_callback(Callback, ValueOrReason);
            ({callback, _OtherState, _Callback}) ->
                %% Don't execute callbacks for wrong state
                ok
        end,
        Waiters
    ).

%% @private
%% Execute a callback in a separate process to avoid blocking
execute_callback(Callback, Value) ->
    spawn(fun() ->
        try
            Callback(Value)
        catch
            Class:Reason:_Stacktrace ->
                %% Log error without stack trace to avoid leaking sensitive data
                ?LOG_ERROR("Error in future callback", #{class => Class, reason => Reason})
        end
    end).

%% @private
%% Create a timeout error record with consistent message and hint
make_timeout_error() ->
    #beamtalk_error{
        kind = timeout,
        class = 'Future',
        selector = undefined,
        message = <<"Await timed out">>,
        hint = <<"Use 'await: duration' for longer timeout, or 'awaitForever' for no timeout">>,
        details = #{}
    }.
