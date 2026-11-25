%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_assert).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Runtime half of the structured-error assertion macros in
`include/beamtalk_test_assert.hrl` (BT-4900).

The macros that check message / hint text cannot be written as a pure
pattern match, so they call `contains/2` here. `?awaitMessage` and
`?assertNoMessage` (BT-5038) call `await_timeout/3` / `no_message/2` to raise
a failure that names the pattern and shows the mailbox.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([contains/2, raised_error/1, await_timeout/3, no_message/2]).

-doc """
True when `Needle` occurs in `Text`.

`Text` is a `{beamtalk_string, Binary}`, a binary, or a charlist - the
shapes `#beamtalk_error{}` message and hint fields take. `undefined` (an
unset hint) never contains anything.
""".
-spec contains(term(), binary()) -> boolean().
contains({beamtalk_string, Bin}, Needle) when is_binary(Bin) ->
    contains(Bin, Needle);
contains(Bin, Needle) when is_binary(Bin) ->
    binary:match(Bin, Needle) =/= nomatch;
contains(List, Needle) when is_list(List) ->
    contains(unicode:characters_to_binary(List), Needle);
contains(_Other, _Needle) ->
    false.

-doc """
Run `Fun`; return the `#beamtalk_error{}` it raises (unwrapped from the
Exception map), or `no_error` if it returns normally. For helpers that need
the error value itself rather than an assertion.
""".
-spec raised_error(fun(() -> term())) -> #beamtalk_error{} | no_error.
raised_error(Fun) ->
    try
        _ = Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{} = Err} -> Err
    end.

-doc """
Raise the failure of a timed-out `?awaitMessage(Pattern, TimeoutMs)`: names the
source text of `Pattern` and shows the calling process's mailbox, so a test that
never received its message says what it got instead.
""".
-spec await_timeout(string(), timeout(), pos_integer()) -> no_return().
await_timeout(PatternText, TimeoutMs, Line) ->
    erlang:error(
        {await_message_timeout, [
            {pattern, PatternText},
            {timeout_ms, TimeoutMs},
            {line, Line},
            {mailbox, mailbox()}
        ]}
    ).

-doc """
Raise the failure of an `?assertNoMessage(Pattern, Ms)` that received a match.
""".
-spec no_message(string(), pos_integer()) -> no_return().
no_message(PatternText, Line) ->
    erlang:error({unexpected_message, [{pattern, PatternText}, {line, Line}]}).

-spec mailbox() -> [term()].
mailbox() ->
    {messages, Messages} = process_info(self(), messages),
    Messages.
