%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_script_harness).

%%% **DDD Context:** Runtime Context — Program Execution

-moduledoc """
Run-mode entry harness — dispatches a script's entry method and maps the
outcome to a POSIX process exit status (ADR 0099 §3).

`beamtalk run ClassName selector [args]` evaluates a one-shot `-eval` that
bootstraps a run-mode workspace and then calls `dispatch/3` here. This module
owns the **implicit exit code** contract:

* the entry method returns normally → the node exits `0`;
* the entry method raises an uncaught error → a readable message is written to
  `standard_error` and the node exits non-zero (`1`).

`Program exit: N` and `System halt: N` (ADR 0099 §3) end the node *directly*
via `erlang:halt/1` from the running process — in run-mode the program owns the
node — so they never unwind back here. This harness therefore only sees the two
*implicit* outcomes above.
""".

-export([dispatch/3]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Dispatch the entry method and halt with the implicit exit status.

Never returns: a normal return halts `0`, an uncaught error prints to stderr
and halts `1`. `Program exit:` / `System halt:` halt before reaching the catch.
""".
-spec dispatch(pid() | undefined, atom(), list()) -> no_return().
dispatch(ClassPid, Selector, Args) ->
    try beamtalk_class_dispatch:class_send(ClassPid, Selector, Args) of
        _Result -> erlang:halt(0)
    catch
        _Class:Reason:_Stacktrace ->
            report_uncaught(Reason),
            erlang:halt(1)
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Write a readable one-line(+hint) rendering of an uncaught error to stderr.".
-spec report_uncaught(term()) -> ok.
report_uncaught(#{error := #beamtalk_error{} = Error}) ->
    write_error(beamtalk_error:format(Error));
report_uncaught(#beamtalk_error{} = Error) ->
    write_error(beamtalk_error:format(Error));
report_uncaught(Other) ->
    write_error(beamtalk_error:format_safe(Other)).

-spec write_error(binary()) -> ok.
write_error(Message) ->
    io:put_chars(standard_error, [Message, $\n]).
