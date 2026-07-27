%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_handle_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_file_handle module (BT-1173, BT-1762, BT-2975).

Tests dispatch/3 and has_method/1 for FileHandle instances, plus the
incremental I/O surface (read:/write:/seek:/sync/close/isOpen) added in
BT-2975. dispatch/3 routes FileHandle selectors to this module and
delegates Object protocol selectors to beamtalk_object_ops. Unknown
selectors raise a structured does_not_understand error.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

-doc "A unique relative temp path — never /tmp, per testing-strategy.md.".
temp_path(Tag) ->
    Tag ++ "_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tmp".

-doc "Build a FileHandle map backed by a real open file descriptor.".
with_temp_handle(Contents, Fun) ->
    with_temp_handle(Contents, read, Fun).

with_temp_handle(Contents, Mode, Fun) ->
    TmpPath = temp_path("beamtalk_file_handle_test"),
    ok = file:write_file(TmpPath, Contents),
    Options =
        case Mode of
            read -> [read, binary];
            append -> [append, binary];
            readWrite -> [read, write, binary]
        end,
    {ok, Fd} = file:open(TmpPath, Options),
    Handle = beamtalk_file_handle:new(Fd, Mode, list_to_binary(TmpPath)),
    try
        Fun(Handle)
    after
        beamtalk_file_handle:close_handle(Handle),
        file:delete(TmpPath)
    end.

-doc "Unwrap a Result ok map, failing the test if it is an error.".
ok_value(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := Value}) ->
    Value.

is_error_result(#{'$beamtalk_class' := 'Result', 'isOk' := IsOk}) ->
    not IsOk.

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

has_method_io_selectors_test() ->
    %% BT-2975: the incremental I/O surface answers respondsTo:.
    [
        ?assert(beamtalk_file_handle:has_method(S))
     || S <- [
            'read:',
            'readAll',
            'write:',
            'writeLine:',
            'position',
            'seek:',
            'flush',
            'sync',
            'close',
            'isOpen'
        ]
    ].

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
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
            beamtalk_file_handle:dispatch(unknown_selector_xyz, [], Handle)
        )
    end).

%%% ============================================================================
%%% Incremental I/O (BT-2975)
%%% ============================================================================

read_returns_requested_bytes_test() ->
    with_temp_handle(<<"0123456789">>, fun(Handle) ->
        ?assertEqual(<<"012">>, ok_value(beamtalk_file_handle:read(Handle, 3)))
    end).

read_at_eof_returns_empty_binary_test() ->
    with_temp_handle(<<"ab">>, fun(Handle) ->
        beamtalk_file_handle:read(Handle, 10),
        ?assertEqual(<<>>, ok_value(beamtalk_file_handle:read(Handle, 10)))
    end).

read_all_reads_from_current_position_test() ->
    with_temp_handle(<<"header|body">>, fun(Handle) ->
        beamtalk_file_handle:seek(Handle, 7),
        ?assertEqual(<<"body">>, ok_value(beamtalk_file_handle:readAll(Handle)))
    end).

seek_returns_new_position_test() ->
    with_temp_handle(<<"0123456789">>, fun(Handle) ->
        ?assertEqual(4, ok_value(beamtalk_file_handle:seek(Handle, 4))),
        ?assertEqual(<<"456">>, ok_value(beamtalk_file_handle:read(Handle, 3)))
    end).

position_advances_with_reads_test() ->
    with_temp_handle(<<"0123456789">>, fun(Handle) ->
        beamtalk_file_handle:read(Handle, 6),
        ?assertEqual(6, ok_value(beamtalk_file_handle:position(Handle)))
    end).

write_and_sync_append_test() ->
    with_temp_handle(<<"start">>, append, fun(Handle) ->
        ?assertEqual(nil, ok_value(beamtalk_file_handle:write(Handle, <<"-more">>))),
        ?assertEqual(nil, ok_value(beamtalk_file_handle:sync(Handle))),
        Path = maps:get(path, Handle),
        ?assertEqual({ok, <<"start-more">>}, file:read_file(Path))
    end).

write_line_appends_newline_test() ->
    with_temp_handle(<<>>, append, fun(Handle) ->
        beamtalk_file_handle:writeLine(Handle, <<"a line">>),
        Path = maps:get(path, Handle),
        ?assertEqual({ok, <<"a line\n">>}, file:read_file(Path))
    end).

flush_on_open_handle_succeeds_test() ->
    with_temp_handle(<<>>, append, fun(Handle) ->
        ?assertEqual(nil, ok_value(beamtalk_file_handle:flush(Handle)))
    end).

read_write_mode_overwrites_in_place_test() ->
    with_temp_handle(<<"0123456789">>, readWrite, fun(Handle) ->
        beamtalk_file_handle:seek(Handle, 5),
        beamtalk_file_handle:write(Handle, <<"XXXXX">>),
        Path = maps:get(path, Handle),
        ?assertEqual({ok, <<"01234XXXXX">>}, file:read_file(Path))
    end).

%%% ============================================================================
%%% close / isOpen (BT-2975)
%%% ============================================================================

is_open_is_true_before_close_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assert(beamtalk_file_handle:isOpen(Handle))
    end).

close_marks_handle_closed_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assertEqual(nil, ok_value(beamtalk_file_handle:close(Handle))),
        ?assertNot(beamtalk_file_handle:isOpen(Handle))
    end).

close_is_idempotent_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        beamtalk_file_handle:close(Handle),
        ?assertEqual(nil, ok_value(beamtalk_file_handle:close(Handle)))
    end).

closed_handle_shares_state_across_copies_test() ->
    %% The atomics cell is what makes close visible through every copy of the
    %% (otherwise immutable) handle map.
    with_temp_handle(<<"data">>, fun(Handle) ->
        Copy = Handle,
        beamtalk_file_handle:close(Handle),
        ?assertNot(beamtalk_file_handle:isOpen(Copy))
    end).

%%% ============================================================================
%%% Closed-handle and wrong-mode errors return Results, never crashes (BT-2975)
%%% ============================================================================

read_on_closed_handle_returns_error_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        beamtalk_file_handle:close(Handle),
        ?assert(is_error_result(beamtalk_file_handle:read(Handle, 1)))
    end).

write_on_closed_handle_returns_error_test() ->
    with_temp_handle(<<>>, append, fun(Handle) ->
        beamtalk_file_handle:close(Handle),
        ?assert(is_error_result(beamtalk_file_handle:write(Handle, <<"x">>)))
    end).

position_on_closed_handle_returns_error_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        beamtalk_file_handle:close(Handle),
        ?assert(is_error_result(beamtalk_file_handle:position(Handle)))
    end).

sync_on_closed_handle_returns_error_test() ->
    with_temp_handle(<<>>, append, fun(Handle) ->
        beamtalk_file_handle:close(Handle),
        ?assert(is_error_result(beamtalk_file_handle:sync(Handle)))
    end).

write_on_read_only_handle_returns_error_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assert(is_error_result(beamtalk_file_handle:write(Handle, <<"x">>)))
    end).

read_on_append_only_handle_returns_error_test() ->
    with_temp_handle(<<>>, append, fun(Handle) ->
        ?assert(is_error_result(beamtalk_file_handle:read(Handle, 1)))
    end).

dispatch_write_line_routes_to_handle_test() ->
    %% BT-2975: 'writeLine:' is a real selector now — on a read-only handle it
    %% comes back as a structured error Result, not a does_not_understand raise.
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assert(is_error_result(beamtalk_file_handle:dispatch('writeLine:', [<<"text">>], Handle)))
    end).

dispatch_read_routes_to_handle_test() ->
    with_temp_handle(<<"0123456789">>, fun(Handle) ->
        ?assertEqual(<<"01">>, ok_value(beamtalk_file_handle:dispatch('read:', [2], Handle)))
    end).

%%% ============================================================================
%%% Type errors on non-handle receivers (BT-2975)
%%% ============================================================================

read_on_non_handle_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_file_handle:read(#{}, 1)
    ).

read_with_negative_count_raises_type_error_test() ->
    with_temp_handle(<<"data">>, fun(Handle) ->
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
            beamtalk_file_handle:read(Handle, -1)
        )
    end).

write_with_non_binary_raises_type_error_test() ->
    with_temp_handle(<<>>, append, fun(Handle) ->
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
            beamtalk_file_handle:write(Handle, 42)
        )
    end).

lines_on_closed_handle_raises_test() ->
    %% BT-2975: `lines` returns a Stream, not a Result, so a closed handle
    %% raises rather than handing back a silently empty stream.
    with_temp_handle(<<"a\nb\n">>, fun(Handle) ->
        beamtalk_file_handle:close(Handle),
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}},
            beamtalk_file_handle:dispatch('lines', [], Handle)
        )
    end).
