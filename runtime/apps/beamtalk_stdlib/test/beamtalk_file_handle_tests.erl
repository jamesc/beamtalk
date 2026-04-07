%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_handle_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_file_handle module (BT-1173, BT-1762).

Tests dispatch/3 and has_method/1 for FileHandle instances.
dispatch/3 routes 'lines' to beamtalk_file:handle_lines/1 and
delegates Object protocol selectors to beamtalk_object_ops.
Unknown selectors raise case_clause (BT-1762: catch-all removed).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

-doc "Build a FileHandle map backed by a real open file descriptor.".
with_temp_handle(Contents, Fun) ->
    TmpPath =
        "beamtalk_file_handle_test_" ++ integer_to_list(erlang:unique_integer([positive])) ++
            ".tmp",
    ok = file:write_file(TmpPath, Contents),
    {ok, Fd} = file:open(TmpPath, [read, binary]),
    Handle = #{'$beamtalk_class' => 'FileHandle', fd => Fd},
    try
        Fun(Handle)
    after
        file:close(Fd),
        file:delete(TmpPath)
    end.

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_lines_test() ->
    ?assert(beamtalk_file_handle:has_method('lines')).

has_method_class_test() ->
    ?assert(beamtalk_file_handle:has_method(class)).

has_method_responds_to_test() ->
    ?assert(beamtalk_file_handle:has_method('respondsTo:')).

has_method_printString_test() ->
    ?assert(beamtalk_file_handle:has_method('printString')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_file_handle:has_method(unknown_selector_xyz)).

has_method_readLine_test() ->
    %% readLine is not a FileHandle method — handled by dispatch only in Subprocess
    ?assertNot(beamtalk_file_handle:has_method('readLine')).

%%% ============================================================================
%%% dispatch/3 — 'lines' selector
%%% ============================================================================

dispatch_lines_returns_stream_test() ->
    with_temp_handle(<<"line1\nline2\n">>, fun(Handle) ->
        Stream = beamtalk_file_handle:dispatch('lines', [], Handle),
        ?assertMatch(#{'$beamtalk_class' := 'Stream'}, Stream)
    end).

%%% ============================================================================
%%% dispatch/3 — Object protocol selectors
%%% ============================================================================

dispatch_printString_returns_binary_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        Result = beamtalk_file_handle:dispatch('printString', [], Handle),
        ?assert(is_binary(Result))
    end).

%%% ============================================================================
%%% dispatch/3 — unknown selector
%%% BT-1762: dispatch/3 no longer has a catch-all clause.
%%% Unknown selectors are now handled by the compiled bt@stdlib@file_handle
%%% module via Object inheritance (dispatched through value_type_send).
%%% ============================================================================

dispatch_unknown_selector_raises_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assertError(
            {case_clause, false}, beamtalk_file_handle:dispatch(unknown_selector_xyz, [], Handle)
        )
    end).

dispatch_write_selector_raises_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assertError(
            {case_clause, false}, beamtalk_file_handle:dispatch('writeLine:', [<<"text">>], Handle)
        )
    end).
