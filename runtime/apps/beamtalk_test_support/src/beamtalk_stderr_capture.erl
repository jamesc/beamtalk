%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stderr_capture).

-moduledoc """
Shared EUnit test helper: capture text written to `standard_error' during
a function call (BT-3126).

`io:put_chars(standard_error, Chars)' resolves its target device via
`whereis(standard_error)' (see OTP's `io:request/3'), **not** via the
calling process' group leader — a `group_leader/2' swap does not intercept
it. This helper instead temporarily swaps the node-global `standard_error'
registration for a capturing loop, restoring the original on the way out
even if `Fun()' raises.

**Concurrency caveat:** this swaps a node-global registered name for the
duration of `Fun()'. Any other process in the same BEAM node that writes to
`standard_error' during that window has its output captured here instead
of reaching the real console, and any process that calls
`whereis(standard_error)'/sends to the name during the brief
unregister/register gap hits `badarg'. Low-risk for the single-threaded,
sequential-by-default EUnit suites this is written for, but do not reach
for this helper in a context with concurrent stderr writers without
reconsidering the approach (e.g. group-leader interposition for the
specific writer process, if it doesn't go through the `standard_error'
atom).
""".

-export([capture/1]).

-doc """
Runs `Fun/0', capturing everything written to the node-global
`standard_error' device during the call, and returns the captured text as
a binary. Restores the original `standard_error' registration in an
`after' block, so it is restored even if `Fun()' throws.
""".
-spec capture(fun(() -> term())) -> binary().
capture(Fun) when is_function(Fun, 0) ->
    OldStdErr = erlang:whereis(standard_error),
    Capturer = spawn(fun() -> capture_loop(<<>>) end),
    true = unregister(standard_error),
    true = register(standard_error, Capturer),
    try
        Fun()
    after
        true = unregister(standard_error),
        true = register(standard_error, OldStdErr)
    end,
    Capturer ! {get_captured, self()},
    receive
        {captured, Text} -> Text
    after 1000 -> <<>>
    end.

capture_loop(Acc) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _Encoding, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(<<Acc/binary, (iolist_to_binary(Chars))/binary>>);
        {io_request, From, ReplyAs, _Other} ->
            From ! {io_reply, ReplyAs, ok},
            capture_loop(Acc);
        {get_captured, From} ->
            From ! {captured, Acc}
    end.
