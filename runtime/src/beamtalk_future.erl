%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk Future/Promise Runtime
%%%
%%% Implements futures (promises) as lightweight BEAM processes.
%%% Beamtalk is async-first: message sends return futures by default.
%%%
%%% Each future is a BEAM process (~2KB) that can be in one of three states:
%%% - pending: waiting for a value
%%% - resolved: has a successful value
%%% - rejected: has an error
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
-export([new/0, resolve/2, reject/2, await/1, await/2,
         when_resolved/2, when_rejected/2]).

%% @doc Create a new future in the pending state.
%% Returns the process ID of the future.
-spec new() -> pid().
new() ->
    spawn(fun() -> pending([]) end).

%% @doc Resolve a future with a value.
%% If the future is already resolved or rejected, this is a no-op.
%% Notifies all waiting processes and executes all resolved callbacks.
-spec resolve(pid(), term()) -> ok.
resolve(Future, Value) ->
    Future ! {resolve, Value},
    ok.

%% @doc Reject a future with an error reason.
%% If the future is already resolved or rejected, this is a no-op.
%% Notifies all waiting processes and executes all rejected callbacks.
-spec reject(pid(), term()) -> ok.
reject(Future, Reason) ->
    Future ! {reject, Reason},
    ok.

%% @doc Block the calling process until the future is resolved or rejected.
%% Returns the value if resolved, or throws {future_rejected, Reason} if rejected.
-spec await(pid()) -> term().
await(Future) ->
    Future ! {await, self()},
    receive
        {future_resolved, Future, Value} ->
            Value;
        {future_rejected, Future, Reason} ->
            throw({future_rejected, Reason})
    end.

%% @doc Block the calling process until the future is resolved, rejected, or times out.
%% Returns {ok, Value} if resolved, {error, Reason} if rejected, or {error, timeout}.
-spec await(pid(), timeout()) -> {ok, term()} | {error, term()}.
await(Future, Timeout) ->
    Future ! {await, self(), Timeout},
    receive
        {future_resolved, Future, Value} ->
            {ok, Value};
        {future_rejected, Future, Reason} ->
            {error, Reason};
        {future_timeout, Future} ->
            {error, timeout}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Register a callback to be executed when the future is resolved.
%% If the future is already resolved, the callback is executed immediately.
%% If the future is rejected, the callback is never executed.
-spec when_resolved(pid(), fun((term()) -> term())) -> ok.
when_resolved(Future, Callback) ->
    Future ! {add_callback, resolved, Callback},
    ok.

%% @doc Register a callback to be executed when the future is rejected.
%% If the future is already rejected, the callback is executed immediately.
%% If the future is resolved, the callback is never executed.
-spec when_rejected(pid(), fun((term()) -> term())) -> ok.
when_rejected(Future, Callback) ->
    Future ! {add_callback, rejected, Callback},
    ok.

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
            TRef = case Timeout of
                infinity -> infinity;
                _ -> erlang:send_after(Timeout, self(), {timeout, Pid})
            end,
            pending([{await, Pid, TRef} | Waiters]);
        {timeout, Pid} ->
            %% Send timeout message to the waiter
            Pid ! {future_timeout, self()},
            %% Remove this waiter from the list
            NewWaiters = lists:filter(
                fun({await, P, _}) -> P =/= Pid;
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
    end.

%% @private
%% Rejected state: has an error reason
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
    end.

%% @private
%% Notify all waiters when future completes
notify_waiters(Waiters, State, ValueOrReason) ->
    lists:foreach(
        fun({await, Pid, TRef}) ->
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
            Class:Reason:Stacktrace ->
                io:format(standard_error,
                    "Error in future callback: ~p:~p~n~p~n",
                    [Class, Reason, Stacktrace])
        end
    end).
