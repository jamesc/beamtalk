%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bounded).

%%% **DDD Context:** Runtime Context

-moduledoc """
Run a blocking call with an upper bound on how long the caller waits (BT-5192).

Some OTP calls have no timeout of their own: `inet:getaddrs/2` waits for as
long as the resolver retries, `global:register_name/3` for as long as another
holder keeps `global`'s lock, `global:sync/0` until every node has answered.
`run/2` runs such a call in a monitored helper process and gives up waiting
after `TimeoutMs`, killing the helper. The one place this pattern lives
(AGENTS.md "No duplicate implementations"): `beamtalk_node`'s connect policy
and `global:sync/0` wait, and `beamtalk_actor_start`'s global registration.

No reply can reach the caller after `run/2` returns. The helper's reply and
its exit come from the same process, so they arrive in that order: on timeout
the caller kills the helper, waits for its `'DOWN'`, and only then takes a
reply that was already on its way (a call that finished just before the kill
counts as finished). The killed helper cannot undo what the call already did
on another process (a request already queued in `global`'s registrar still
runs); each caller documents what that means for it.
""".

-export([run/2]).

-type result(T) ::
    {ok, T}
    | {raised, error | exit | throw, term(), list()}
    | {down, term()}
    | timeout.
-export_type([result/1]).

-doc """
Run `Fun` in a monitored helper process, waiting at most `TimeoutMs`.

- `{ok, Value}` — `Fun` returned `Value` (in time, or just as it timed out).
- `{raised, Class, Reason, Stack}` — `Fun` raised; re-raise it with
  `erlang:raise/3` to keep the original exception class.
- `{down, Reason}` — the helper died without answering (killed by someone
  else).
- `timeout` — `Fun` was still running at the deadline; the helper is dead.
""".
-spec run(fun(() -> T), timeout()) -> result(T).
run(Fun, TimeoutMs) ->
    Caller = self(),
    Tag = make_ref(),
    {Helper, Ref} = spawn_monitor(fun() ->
        Result =
            try
                {ok, Fun()}
            catch
                Class:Reason:Stack -> {raised, Class, Reason, Stack}
            end,
        Caller ! {Tag, Result}
    end),
    receive
        {Tag, Result} ->
            erlang:demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Helper, Reason} ->
            {down, Reason}
    after TimeoutMs ->
        exit(Helper, kill),
        receive
            {'DOWN', Ref, process, Helper, _} -> ok
        end,
        receive
            {Tag, Late} -> Late
        after 0 -> timeout
        end
    end.
