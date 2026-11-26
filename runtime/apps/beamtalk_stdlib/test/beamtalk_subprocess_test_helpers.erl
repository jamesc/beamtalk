%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_subprocess_test_helpers).

%%% **DDD Context:** Actor System Context

-moduledoc """
`writeLine:` helpers shared by the `Subprocess` and `ReactiveSubprocess`
EUnit suites (BT-5269).
""".

-export([start_writers/3, await_write_reply/0, closed_stdin_cmd/0]).

-doc """
Start `N` callers that each send `writeLine: Chunk` to `Pid` once and send
the reply to this process as `{write_reply, Reply}`.
""".
-spec start_writers(pid(), binary(), pos_integer()) -> [pid()].
start_writers(Pid, Chunk, N) ->
    Self = self(),
    [
        spawn(fun() ->
            Self ! {write_reply, gen_server:call(Pid, {'writeLine:', [Chunk]}, 30000)}
        end)
     || _ <- lists:seq(1, N)
    ].

-doc "Receive the next reply sent by a `start_writers/3` caller.".
-spec await_write_reply() -> term().
await_write_reply() ->
    receive
        {write_reply, Reply} -> Reply
    after 20000 -> error(no_write_reply)
    end.

-doc """
`{Executable, Args}` for a child that closes its stdin, prints `closed`, and
stays up until killed (Unix only).
""".
-spec closed_stdin_cmd() -> {binary(), [binary()]}.
closed_stdin_cmd() ->
    {<<"/bin/sh">>, [<<"-c">>, <<"exec 0<&-; echo closed; exec sleep 60">>]}.
