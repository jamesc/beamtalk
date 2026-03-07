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
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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
        receive
            stop -> ok
        end
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
    SelfRef = gen_server:call(Pid, clear),
    ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid}, SelfRef),
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

%% --- FFI shims ---
%%
%% The TranscriptStream FFI shims (show/2, cr/1, recent/1, clear/1, subscribe/1,
%% unsubscribe/1) delegate to dispatch/3 which uses the process dictionary.
%% They are designed to be called from INSIDE the actor's handle_call context
%% where the process dictionary belongs to the gen_server. The shims are
%% exercised end-to-end by the BUnit trace_cr_test.bt tests.
%%
%% Here we just verify that dispatch/3 integration works correctly, which is
%% the underlying mechanism.

ffi_shim_dispatch_show_test() ->
    {ok, Pid} = beamtalk_transcript_stream:spawn(),
    Self = #beamtalk_object{class = 'TranscriptStream', class_mod = 'bt@stdlib@transcript_stream', pid = Pid},
    gen_server:call(Pid, {'show:', [Self, <<"dispatch_show">>]}),
    Recent = gen_server:call(Pid, recent),
    ?assert(is_list(Recent)),
    gen_server:stop(Pid).

ffi_shim_exports_test() ->
    %% Verify the FFI shim functions are exported
    Exports = beamtalk_transcript_stream:module_info(exports),
    ?assert(lists:member({show, 2}, Exports)),
    ?assert(lists:member({cr, 1}, Exports)),
    ?assert(lists:member({recent, 1}, Exports)),
    ?assert(lists:member({clear, 1}, Exports)),
    ?assert(lists:member({subscribe, 1}, Exports)),
    ?assert(lists:member({unsubscribe, 1}, Exports)).

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
    ?assertEqual(
        {error, {invalid_max_buffer, 0}},
        beamtalk_transcript_stream:spawn(0)
    ).

invalid_max_buffer_negative_test() ->
    ?assertEqual(
        {error, {invalid_max_buffer, -1}},
        beamtalk_transcript_stream:spawn(-1)
    ).

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
    SelfRef = gen_server:call(Pid, {clear, []}),
    ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid}, SelfRef),
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

%%% ============================================================================
%%% Sync handle_call tests (BT-1163: REPL dispatch path)
%%% ============================================================================

%% The REPL sends messages via gen_server:call(Pid, {Selector, Args}).
%% These tests verify the new handle_call clauses for show:, cr, subscribe,
%% and unsubscribe that were added to support the sync-by-default dispatch.

sync_show_writes_to_buffer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    SelfRef = gen_server:call(Pid, {'show:', [<<"SyncHello">>]}),
    ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid}, SelfRef),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"SyncHello">>], Result),
    gen_server:stop(Pid).

sync_show_converts_integer_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    _ = gen_server:call(Pid, {'show:', [99]}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"99">>], Result),
    gen_server:stop(Pid).

sync_cr_writes_newline_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    SelfRef = gen_server:call(Pid, {cr, []}),
    ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid}, SelfRef),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([<<"\n">>], Result),
    gen_server:stop(Pid).

sync_subscribe_unsubscribe_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    %% Subscribe via sync call
    SelfRef = gen_server:call(Pid, {subscribe, []}),
    ?assertMatch({beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, Pid}, SelfRef),
    %% Should receive output after subscribing
    gen_server:cast(Pid, {'show:', <<"sync_sub">>}),
    receive
        {transcript_output, <<"sync_sub">>} -> ok
    after 500 ->
        ?assert(false)
    end,
    %% Unsubscribe via sync call
    _ = gen_server:call(Pid, {unsubscribe, []}),
    %% Should NOT receive output after unsubscribing
    gen_server:cast(Pid, {'show:', <<"after_unsub">>}),
    _ = gen_server:call(Pid, recent),
    receive
        {transcript_output, <<"after_unsub">>} -> ?assert(false)
    after 0 ->
        ok
    end,
    flush_transcript(),
    gen_server:stop(Pid).

sync_show_then_recent_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    _ = gen_server:call(Pid, {'show:', [<<"first">>]}),
    _ = gen_server:call(Pid, {'show:', [<<"second">>]}),
    Result = gen_server:call(Pid, {recent, []}),
    ?assertEqual([<<"first">>, <<"second">>], Result),
    gen_server:stop(Pid).

sync_show_then_clear_test() ->
    {ok, Pid} = beamtalk_transcript_stream:start_link(),
    _ = gen_server:call(Pid, {'show:', [<<"data">>]}),
    _ = gen_server:call(Pid, {clear, []}),
    Result = gen_server:call(Pid, recent),
    ?assertEqual([], Result),
    gen_server:stop(Pid).
