%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_io_capture_tests).

-moduledoc """
EUnit tests for beamtalk_io_capture.

Covers:
- BT-1172 dead-process guard in reset_captured_group_leaders/2.
- is_stdin_request/1 edge clauses (get_chars/get_until no-encoding forms).
- prompt_to_binary/1 fallback paths (non-binary unicode result; badarg exception).
- handle_io_request/2 catch arms for {put_chars, Enc, Chars}, {put_chars, Chars},
  and {put_chars, Enc, Mod, Func, Args} variants.
- io_capture_loop/2 subscriber chunk-forwarding path.

General start/stop and io_capture_with_output coverage lives in
beamtalk_repl_eval_tests.erl.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

spawn_monitored(Fun) ->
    Pid = spawn(Fun),
    Ref = erlang:monitor(process, Pid),
    {Pid, Ref}.

await_down(Ref) ->
    receive
        {'DOWN', Ref, process, _, _} -> ok
    after 5000 ->
        error(timeout_waiting_for_down)
    end.

%%====================================================================
%% Tests: reset_captured_group_leaders — dead process guard (BT-1172)
%%====================================================================

%% BT-1172: The TOCTOU race — a process whose group_leader is CapturePid
%% dies between is_process_alive/1 returning true and group_leader/2 being
%% called. We approximate this by issuing exit(Pid, kill) and immediately
%% calling reset without waiting. The kill signal may arrive during the scan,
%% exercising the try/catch. The key invariant: no exception is raised.
reset_captured_group_leaders_toctou_race_test() ->
    OldGL = group_leader(),
    {CapturePid, CRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    {Pid, _PRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    group_leader(CapturePid, Pid),
    %% Issue kill without waiting — Pid may still appear in erlang:processes()
    exit(Pid, kill),
    try
        ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL))
    after
        exit(CapturePid, kill),
        await_down(CRef)
    end.

reset_captured_group_leaders_live_process_test() ->
    %% Live process whose group_leader is CapturePid gets reset to OldGL.
    OldGL = group_leader(),
    {CapturePid, CRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    {Pid, PRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    group_leader(CapturePid, Pid),
    try
        ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
        {group_leader, GL} = erlang:process_info(Pid, group_leader),
        ?assertEqual(OldGL, GL)
    after
        exit(Pid, kill),
        exit(CapturePid, kill),
        await_down(PRef),
        await_down(CRef)
    end.

reset_captured_group_leaders_different_gl_left_alone_test() ->
    %% Process with a different group_leader is not touched.
    OldGL = group_leader(),
    {CapturePid, CRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    {OtherPid, ORef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    %% OtherPid's GL is OldGL (default), not CapturePid
    try
        ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
        {group_leader, GL2} = erlang:process_info(OtherPid, group_leader),
        ?assertEqual(OldGL, GL2)
    after
        exit(OtherPid, kill),
        exit(CapturePid, kill),
        await_down(ORef),
        await_down(CRef)
    end.

%%====================================================================
%% Tests: is_stdin_request/1 — no-encoding protocol forms
%%====================================================================

%% {get_chars, Prompt, Count} — the 3-tuple form without an encoding arg
%% (covered by line 195–196 in the source).
is_stdin_request_get_chars_no_encoding_test() ->
    ?assertEqual(
        {true, <<">> ">>},
        beamtalk_io_capture:is_stdin_request({get_chars, <<">> ">>, 3})
    ).

%% {get_until, Prompt, Mod, Func, Args} — 5-tuple no-encoding form
%% (covered by line 199–200 in the source).
is_stdin_request_get_until_no_encoding_test() ->
    ?assertEqual(
        {true, <<"input: ">>},
        beamtalk_io_capture:is_stdin_request({get_until, <<"input: ">>, io_lib, read, []})
    ).

%%====================================================================
%% Tests: prompt_to_binary/1 — fallback paths for list input
%%====================================================================

%% unicode:characters_to_binary/2 returns {error, _, _} (not a binary) for
%% codepoints that are out-of-range for the encoding (e.g., surrogate U+D800).
%% The `_ -> <<"? ">>` branch at line 210 must be reached.
prompt_to_binary_list_non_binary_result_returns_fallback_test() ->
    %% [16#D800] is a lone surrogate — unicode:characters_to_binary returns
    %% {error, <<>>, [55296]}, which is not binary, so the fallback fires.
    ?assertEqual(<<"? ">>, beamtalk_io_capture:prompt_to_binary([16#D800])).

%% unicode:characters_to_binary/2 raises badarg for lists containing non-integer
%% elements (e.g., atoms). The `_:_ -> <<"? ">>` catch at line 212 must fire.
prompt_to_binary_list_exception_returns_fallback_test() ->
    %% [hello] is a proper list but contains an atom, causing badarg.
    ?assertEqual(<<"? ">>, beamtalk_io_capture:prompt_to_binary([hello])).

%%====================================================================
%% Tests: handle_io_request/2 — exception and non-binary fallback arms
%%====================================================================

%% {put_chars, Encoding, Chars}: unicode:characters_to_binary throws when Chars
%% contains an atom — exercises the `_:_ -> {ok, Buffer}` catch at line 254.
handle_io_request_put_chars_enc_exception_returns_buffer_test() ->
    Buf = <<"existing">>,
    ?assertEqual(
        {ok, Buf},
        beamtalk_io_capture:handle_io_request({put_chars, utf8, [hello]}, Buf)
    ).

%% {put_chars, Chars}: latin1 encoding rejects codepoints > 255, returning an
%% error tuple rather than a binary — exercises `_ -> {ok, Buffer}` at line 260.
handle_io_request_put_chars_legacy_non_binary_returns_buffer_test() ->
    Buf = <<"buf">>,
    %% unicode:characters_to_binary([300], latin1, utf8) → {error, <<>>, [300]}
    ?assertEqual(
        {ok, Buf},
        beamtalk_io_capture:handle_io_request({put_chars, [300]}, Buf)
    ).

%% {put_chars, Chars}: unicode:characters_to_binary throws for atom-in-list input
%% in the legacy form — exercises `_:_ -> {ok, Buffer}` at line 262.
handle_io_request_put_chars_legacy_exception_returns_buffer_test() ->
    Buf = <<"buf">>,
    ?assertEqual(
        {ok, Buf},
        beamtalk_io_capture:handle_io_request({put_chars, [hello]}, Buf)
    ).

%% {put_chars, Encoding, Mod, Func, Args}: apply/3 result is a surrogate list,
%% so unicode:characters_to_binary returns an error tuple, not a binary —
%% exercises `_ -> {ok, Buffer}` at line 267.
handle_io_request_put_chars_mfa_non_binary_returns_buffer_test() ->
    Buf = <<"buf">>,
    %% apply(lists, duplicate, [1, 16#D800]) → [55296];
    %% unicode:characters_to_binary([55296], utf8, utf8) → {error, <<>>, [55296]}
    ?assertEqual(
        {ok, Buf},
        beamtalk_io_capture:handle_io_request(
            {put_chars, utf8, lists, duplicate, [1, 16#D800]},
            Buf
        )
    ).

%%====================================================================
%% Tests: io_capture_loop/2 — subscriber chunk-forwarding (lines 118–123)
%%====================================================================

%% When Subscriber is a pid and new output is appended to the buffer,
%% io_capture_loop/2 must forward the new chunk as {eval_out, Chunk}.
io_capture_loop_forwards_chunk_to_subscriber_test() ->
    Self = self(),
    ReplyAs = make_ref(),
    LoopPid = spawn(fun() -> beamtalk_io_capture:io_capture_loop(<<>>, Self) end),
    try
        LoopPid ! {io_request, Self, ReplyAs, {put_chars, unicode, <<"hello">>}},
        receive
            {io_reply, ReplyAs, ok} -> ok
        after 1000 ->
            error(timeout_waiting_for_io_reply)
        end,
        receive
            {eval_out, Chunk} ->
                ?assertEqual(<<"hello">>, Chunk)
        after 1000 ->
            error(timeout_waiting_for_eval_out)
        end
    after
        exit(LoopPid, kill)
    end.
