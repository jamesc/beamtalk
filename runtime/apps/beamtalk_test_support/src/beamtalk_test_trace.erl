%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_trace).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: observe which functions a piece of code calls, without
timing.

A process never receives trace messages for its own events, so the code under
test runs in a spawned worker traced by the test process. `calls_during/2`
returns the worker's result and the `{Module, Function}` calls it made to the
watched functions, in call order. The worker's exit is the end marker, so the
list is complete when the call returns: nothing is polled or slept on.
""".

-export([calls_during/2]).

-doc """
Run `Fun` in a traced worker and return `{Result, Calls}`: `Fun`'s result and
the `{Module, Function, Args}` of each call it made to a function in
`Watched` (a list of `{Module, Function, Arity}`), in order. Trace patterns
are removed before returning. A crash in `Fun` is re-raised in the caller.
""".
-spec calls_during(fun(() -> term()), [{module(), atom(), arity()}]) ->
    {term(), [{module(), atom(), [term()]}]}.
calls_during(Fun, Watched) ->
    Self = self(),
    Worker = spawn(fun() ->
        receive
            go -> ok
        end,
        Result =
            %% bt-catcher-audit: not-applicable-reraises - the raise is captured here and
            %% re-raised in the caller by calls_during/2.
            try
                {ok, Fun()}
            catch
                Class:Reason:Stack -> {raised, Class, Reason, Stack}
            end,
        Self ! {?MODULE, self(), Result}
    end),
    Ref = erlang:monitor(process, Worker),
    1 = erlang:trace(Worker, true, [call, {tracer, Self}]),
    [_ = erlang:trace_pattern(MFA, true, [local]) || MFA <- Watched],
    Worker ! go,
    Result =
        receive
            {?MODULE, Worker, R} -> R
        end,
    receive
        {'DOWN', Ref, process, Worker, _} -> ok
    end,
    [_ = erlang:trace_pattern(MFA, false, [local]) || MFA <- Watched],
    Calls = drain([]),
    case Result of
        {ok, Value} -> {Value, Calls};
        {raised, C, Rs, St} -> erlang:raise(C, Rs, St)
    end.

%% All trace messages are already in the mailbox: the worker is dead, and trace
%% delivery from one process is ordered ahead of its exit.
drain(Acc) ->
    receive
        {trace, _, call, {M, F, Args}} -> drain([{M, F, Args} | Acc])
    after 0 -> lists:reverse(Acc)
    end.
