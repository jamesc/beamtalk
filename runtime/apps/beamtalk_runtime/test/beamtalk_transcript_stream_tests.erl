%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_transcript_stream.
%%%
%%% Tests all TranscriptStream behaviours:
%%% - show: writes to buffer
%%% - cr writes newline to buffer
%%% - subscribe/unsubscribe lifecycle
%%% - subscriber receives output
%%% - dead subscriber auto-removed
%%% - recent returns buffer contents
%%% - clear empties buffer
%%% - buffer overflow drops oldest

-module(beamtalk_transcript_stream_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Helper
%%% ============================================================================

%% @private Flush all transcript_output messages from the mailbox.
flush_transcript() ->
    receive
        {transcript_output, _} -> flush_transcript()
    after 0 ->
        ok
    end.

%%% ============================================================================
%%% Tests
%%% ============================================================================

%% --- show: writes to buffer ---

show_writes_to_buffer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', <<"Hello">>}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"Hello">>], Result),
    gen_server:stop(Pid).

show_converts_integer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', 42}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"42">>], Result),
    gen_server:stop(Pid).

show_converts_atom_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', hello}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"hello">>], Result),
    gen_server:stop(Pid).

%% --- cr writes newline to buffer ---

cr_writes_newline_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, cr),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"\n">>], Result),
    gen_server:stop(Pid).

%% --- show: then cr sequence ---

show_then_cr_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', <<"Hello">>}),
    gen_server:cast(Pid, cr),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"Hello">>, <<"\n">>], Result),
    gen_server:stop(Pid).

%% --- subscribe / unsubscribe lifecycle ---

subscribe_unsubscribe_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {subscribe, self()}),
    %% Should receive output after subscribing
    gen_server:cast(Pid, {'show:', <<"sub">>}),
    receive
        {transcript_output, <<"sub">>} -> ok
    after 500 ->
        ?assert(false)
    end,
    %% Unsubscribe
    gen_server:cast(Pid, {unsubscribe, self()}),
    %% Should NOT receive output after unsubscribing
    gen_server:cast(Pid, {'show:', <<"after">>}),
    %% Sync barrier: ensures both unsubscribe and show: are processed
    _ = gen_server:call(Pid, recent),
    receive
        {transcript_output, <<"after">>} -> ?assert(false)
    after 0 ->
        ok
    end,
    gen_server:stop(Pid).

%% --- subscriber receives output ---

subscriber_receives_output_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {subscribe, self()}),
    gen_server:cast(Pid, {'show:', <<"msg1">>}),
    gen_server:cast(Pid, {'show:', <<"msg2">>}),
    receive
        {transcript_output, <<"msg1">>} -> ok
    after 500 ->
        ?assert(false)
    end,
    receive
        {transcript_output, <<"msg2">>} -> ok
    after 500 ->
        ?assert(false)
    end,
    flush_transcript(),
    gen_server:stop(Pid).

%% --- dead subscriber auto-removed ---

dead_subscriber_auto_removed_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    Sub = erlang:spawn(fun() ->
        receive stop -> ok end
    end),
    gen_server:cast(Pid, {subscribe, Sub}),
    %% Sync barrier: ensure subscribe is processed before killing
    _ = gen_server:call(Pid, recent),
    exit(Sub, kill),
    %% Wait for DOWN message to be delivered and processed
    timer:sleep(50),
    gen_server:cast(Pid, {'show:', <<"after_death">>}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"after_death">>], Result),
    gen_server:stop(Pid).

%% --- recent returns buffer contents ---

recent_returns_buffer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', <<"a">>}),
    gen_server:cast(Pid, {'show:', <<"b">>}),
    gen_server:cast(Pid, {'show:', <<"c">>}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Result),
    gen_server:stop(Pid).

recent_empty_buffer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([], Result),
    gen_server:stop(Pid).

%% --- clear empties buffer ---

clear_empties_buffer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', <<"data">>}),
    ok = gen_server:call(Pid, clear),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([], Result),
    gen_server:stop(Pid).

%% --- buffer overflow drops oldest ---

buffer_overflow_drops_oldest_test() ->
    MaxBuffer = 3,
    {ok, Pid} = beamtalk_transcript_stream:start_link(MaxBuffer),
    gen_server:cast(Pid, {'show:', <<"1">>}),
    gen_server:cast(Pid, {'show:', <<"2">>}),
    gen_server:cast(Pid, {'show:', <<"3">>}),
    gen_server:cast(Pid, {'show:', <<"4">>}),
    gen_server:cast(Pid, {'show:', <<"5">>}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"3">>, <<"4">>, <<"5">>], Result),
    gen_server:stop(Pid).

%% --- duplicate subscribe is idempotent ---

duplicate_subscribe_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {subscribe, self()}),
    gen_server:cast(Pid, {subscribe, self()}),
    gen_server:cast(Pid, {'show:', <<"once">>}),
    receive
        {transcript_output, <<"once">>} -> ok
    after 500 ->
        ?assert(false)
    end,
    %% Sync barrier, then check no duplicate message
    _ = gen_server:call(Pid, recent),
    receive
        {transcript_output, <<"once">>} -> ?assert(false)
    after 0 ->
        ok
    end,
    flush_transcript(),
    gen_server:stop(Pid).

%% --- has_method/1 ---

has_method_test() ->
    ?assert(beamtalk_transcript_stream:has_method('show:')),
    ?assert(beamtalk_transcript_stream:has_method(cr)),
    ?assert(beamtalk_transcript_stream:has_method(subscribe)),
    ?assert(beamtalk_transcript_stream:has_method(unsubscribe)),
    ?assert(beamtalk_transcript_stream:has_method(recent)),
    ?assert(beamtalk_transcript_stream:has_method(clear)),
    ?assertNot(beamtalk_transcript_stream:has_method(unknown)),
    ?assertNot(beamtalk_transcript_stream:has_method('foo:')).

%% --- spawn (unlinked) ---

spawn_test() ->
    {ok, Pid} = beamtalk_transcript_stream:spawn(),
    ?assert(is_pid(Pid)),
    gen_server:cast(Pid, {'show:', <<"spawn_test">>}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"spawn_test">>], Result),
    gen_server:stop(Pid).

spawn_with_max_buffer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:spawn(5),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

%% --- invalid max_buffer rejected ---

invalid_max_buffer_zero_test() ->
    ?assertEqual({error, {invalid_max_buffer, 0}},
                 beamtalk_transcript_stream:spawn(0)).

invalid_max_buffer_negative_test() ->
    ?assertEqual({error, {invalid_max_buffer, -1}},
                 beamtalk_transcript_stream:spawn(-1)).

%% --- non-pid subscribe is safe (no crash) ---

subscribe_non_pid_safe_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {subscribe, not_a_pid}),
    gen_server:cast(Pid, {'show:', <<"ok">>}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"ok">>], Result),
    gen_server:stop(Pid).

%% --- tuple-format calls (dispatch compatibility) ---

tuple_format_recent_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', <<"data">>}),
    Result = gen_server:call(Pid, {recent, []}),
    ?assertEqual([<<"data">>], Result),
    gen_server:stop(Pid).

tuple_format_clear_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    gen_server:cast(Pid, {'show:', <<"data">>}),
    ok = gen_server:call(Pid, {clear, []}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([], Result),
    gen_server:stop(Pid).

%%% ============================================================================
%%% Unicode / encoding edge-case tests
%%% ============================================================================

%% --- to_string: invalid charlist (non-Unicode codepoints) ---

show_invalid_charlist_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    %% A list with values > 0x10FFFF is not a valid Unicode charlist
    gen_server:cast(Pid, {'show:', [16#110000, 16#FFFFFF]}),
    [Result] = gen_server:call(Pid, recent),
    %% Should fall back to io_lib:format ~p representation, not crash
    ?assert(is_binary(Result)),
    gen_server:stop(Pid).

show_incomplete_charlist_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    %% A list ending with a high surrogate (incomplete sequence)
    gen_server:cast(Pid, {'show:', [65, 66, 16#D800]}),
    [Result] = gen_server:call(Pid, recent),
    ?assert(is_binary(Result)),
    gen_server:stop(Pid).

show_mixed_list_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    %% A list with mixed types (not a charlist at all)
    gen_server:cast(Pid, {'show:', [1, two, <<"three">>]}),
    [Result] = gen_server:call(Pid, recent),
    ?assert(is_binary(Result)),
    gen_server:stop(Pid).

%% --- to_string: non-UTF8 binaries ---

show_non_utf8_binary_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    %% 0xFF 0xFE is not valid UTF-8
    gen_server:cast(Pid, {'show:', <<255, 254, 0, 1>>}),
    [Result] = gen_server:call(Pid, recent),
    ?assert(is_binary(Result)),
    %% Must not be the raw invalid binary
    ?assertNotEqual(<<255, 254, 0, 1>>, Result),
    gen_server:stop(Pid).

show_truncated_utf8_binary_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    %% Start of a 3-byte UTF-8 sequence but truncated
    gen_server:cast(Pid, {'show:', <<16#E0, 16#A0>>}),
    [Result] = gen_server:call(Pid, recent),
    ?assert(is_binary(Result)),
    gen_server:stop(Pid).

show_valid_utf8_binary_unchanged_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    ValidUtf8 = <<"Hello, 世界!"/utf8>>,
    gen_server:cast(Pid, {'show:', ValidUtf8}),
    [Result] = gen_server:call(Pid, recent),
    ?assertEqual(ValidUtf8, Result),
    gen_server:stop(Pid).

%% --- ensure_utf8/1 directly ---

ensure_utf8_valid_test() ->
    ?assertEqual(<<"hello">>, beamtalk_transcript_stream:ensure_utf8(<<"hello">>)).

ensure_utf8_valid_multibyte_test() ->
    Bin = <<"café"/utf8>>,
    ?assertEqual(Bin, beamtalk_transcript_stream:ensure_utf8(Bin)).

ensure_utf8_invalid_bytes_test() ->
    Result = beamtalk_transcript_stream:ensure_utf8(<<255, 254>>),
    ?assert(is_binary(Result)),
    ?assertNotEqual(<<255, 254>>, Result).

ensure_utf8_incomplete_sequence_test() ->
    %% 2-byte sequence start but no continuation
    Result = beamtalk_transcript_stream:ensure_utf8(<<16#C0>>),
    ?assert(is_binary(Result)),
    ?assertNotEqual(<<16#C0>>, Result).

ensure_utf8_empty_binary_test() ->
    ?assertEqual(<<>>, beamtalk_transcript_stream:ensure_utf8(<<>>)).
