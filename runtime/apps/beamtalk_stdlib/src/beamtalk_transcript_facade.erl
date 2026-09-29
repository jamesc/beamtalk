%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_transcript_facade).

%%% **DDD Context:** Object System Context

-moduledoc """
Backing module for the `Transcript` class-side facade (ADR 0129 §5).

`Transcript` is `sealed typed Object subclass: Transcript native:
beamtalk_transcript_facade`; each class-side method is `=> self delegate` into
the same-named function here. Output has two routes, chosen per call:

* **Interactive workspace.** When the registered `'Transcript'` process exists
  *and* is the workspace's `TranscriptStream` (`beamtalk_transcript_stream`),
  output goes to it. A user actor that happens to be registered as
  `#Transcript` is never mistaken for the stream.
* **Elsewhere** (`beamtalk run`, `beamtalk test`, releases without a console,
  or while the stream restarts): one Logger `notice` event per `show:` /
  `showCr:` in the `[beamtalk, user, transcript]` domain. The runtime installs
  the `beamtalk_transcript_log` handler for that domain
  (`beamtalk_logging_config:install_transcript_handler/0`), so the output is
  plain text, one line per event. `cr` is a no-op on this route.

`recent` and `clear` are workspace-only and raise `no_workspace` when there is
no stream.
""".

-include_lib("kernel/include/logger.hrl").

-export([show/1, cr/0, showCr/1, recent/0, clear/0]).

-define(STREAM_NAME, 'Transcript').

-doc "`Transcript show:` — write `Value` (a String as-is, else its printString).".
-spec show(term()) -> nil.
show(Value) ->
    Text = beamtalk_primitive:display_string(Value),
    case stream() of
        {ok, Pid} ->
            via_stream(Pid, {'show:', [Text]}, fun() -> log(Text) end);
        none ->
            log(Text)
    end.

-doc "`Transcript cr` — a newline on the stream; a no-op on the Logger route.".
-spec cr() -> nil.
cr() ->
    case stream() of
        {ok, Pid} ->
            via_stream(Pid, {cr, []}, fun() -> ok end);
        none ->
            nil
    end.

-doc "`Transcript showCr:` — `show:` then `cr` (one Logger event on that route).".
-spec showCr(term()) -> nil.
showCr(Value) ->
    Text = beamtalk_primitive:display_string(Value),
    case stream() of
        {ok, Pid} ->
            via_stream(Pid, {'show:', [Text]}, fun() -> log(Text) end),
            via_stream(Pid, {cr, []}, fun() -> ok end);
        none ->
            log(Text)
    end.

-doc "`Transcript recent` — the buffered lines; `no_workspace` without a stream.".
-spec recent() -> [binary()].
recent() ->
    gen_server:call(require_stream(recent), recent).

-doc "`Transcript clear` — empty the buffer; `no_workspace` without a stream.".
-spec clear() -> nil.
clear() ->
    _ = gen_server:call(require_stream(clear), clear),
    nil.

%%% ============================================================================
%%% Internal
%%% ============================================================================

-doc """
The workspace's `TranscriptStream` process, if the registered `'Transcript'`
process is one. Identified by its `proc_lib` initial call, so an unrelated
process registered under the same name is rejected.
""".
-spec stream() -> {ok, pid()} | none.
stream() ->
    case erlang:whereis(?STREAM_NAME) of
        undefined ->
            none;
        Pid ->
            case erlang:process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    case lists:keyfind('$initial_call', 1, Dict) of
                        {'$initial_call', {beamtalk_transcript_stream, init, 1}} -> {ok, Pid};
                        _ -> none
                    end;
                undefined ->
                    none
            end
    end.

-spec require_stream(atom()) -> pid().
require_stream(Selector) ->
    case stream() of
        {ok, Pid} ->
            Pid;
        none ->
            Err0 = beamtalk_error:new(no_workspace, 'Transcript', Selector),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary(
                    io_lib:format(
                        "Transcript>>~ts needs an interactive workspace's transcript; "
                        "none is running on this node",
                        [Selector]
                    )
                )
            ),
            beamtalk_error:raise(
                beamtalk_error:with_hint(
                    Err1,
                    <<
                        "Use Logger or Console for program output; "
                        "Transcript recent and clear only work in the REPL."
                    >>
                )
            )
    end.

-doc """
Send `Request` to the stream. If the stream dies mid-call (it is restarting),
fall back to `Fallback` so output is not lost.
""".
-spec via_stream(pid(), term(), fun(() -> term())) -> nil.
via_stream(Pid, Request, Fallback) ->
    try gen_server:call(Pid, Request) of
        _ -> nil
    catch
        exit:_ ->
            _ = Fallback(),
            nil
    end.

-doc "The Logger route: one `notice` event in the transcript domain.".
-spec log(binary()) -> nil.
log(Text) ->
    ?LOG_NOTICE("~ts", [Text], #{domain => beamtalk_logging_config:transcript_domain()}),
    nil.
