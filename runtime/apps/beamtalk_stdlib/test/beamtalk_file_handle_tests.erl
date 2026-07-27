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

lines_on_write_only_handle_raises_test() ->
    %% BT-2975: reading a write-only descriptor crashes the file_io_server, so
    %% every later write on the handle fails silently. `lines` must refuse
    %% before touching it — and the handle must survive intact.
    with_temp_handle(<<>>, append, fun(Handle) ->
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = io_error}},
            beamtalk_file_handle:dispatch('lines', [], Handle)
        ),
        ?assert(beamtalk_file_handle:isOpen(Handle)),
        ?assertEqual(nil, ok_value(beamtalk_file_handle:writeLine(Handle, <<"still works">>))),
        Path = maps:get(path, Handle),
        ?assertEqual({ok, <<"still works\n">>}, file:read_file(Path))
    end).

is_open_false_once_descriptor_dies_test() ->
    %% A non-raw descriptor is a process: it can die without anyone calling
    %% close (owner exit, file_io_server crash). isOpen must not keep saying
    %% true, or callers get a bare `terminated` instead of a clear error.
    with_temp_handle(<<"data">>, fun(Handle) ->
        Fd = maps:get(fd, Handle),
        ok = file:close(Fd),
        ?assertNot(beamtalk_file_handle:isOpen(Handle)),
        ?assert(is_error_result(beamtalk_file_handle:read(Handle, 1)))
    end).

read_zero_bytes_is_a_no_op_test() ->
    %% `read: 0` answers <<>> without moving the position — an empty result
    %% only means end-of-file when the requested count was positive.
    with_temp_handle(<<"0123456789">>, fun(Handle) ->
        ?assertEqual(<<>>, ok_value(beamtalk_file_handle:read(Handle, 0))),
        ?assertEqual(0, ok_value(beamtalk_file_handle:position(Handle))),
        ?assertEqual(<<"01">>, ok_value(beamtalk_file_handle:read(Handle, 2)))
    end).

seek_past_eof_then_write_leaves_a_hole_test() ->
    with_temp_handle(<<"abc">>, readWrite, fun(Handle) ->
        ?assertEqual(6, ok_value(beamtalk_file_handle:seek(Handle, 6))),
        ?assertEqual(nil, ok_value(beamtalk_file_handle:write(Handle, <<"z">>))),
        Path = maps:get(path, Handle),
        ?assertEqual({ok, <<"abc", 0, 0, 0, "z">>}, file:read_file(Path))
    end).

handle_shared_across_processes_test() ->
    %% The atomics cell exists so a close through one holder is visible to
    %% every other. Exercise that across a real process boundary.
    with_temp_handle(<<"data">>, fun(Handle) ->
        Parent = self(),
        spawn(fun() ->
            beamtalk_file_handle:close(Handle),
            Parent ! closed
        end),
        receive
            closed -> ok
        after 5000 -> error(timeout)
        end,
        ?assertNot(beamtalk_file_handle:isOpen(Handle))
    end).

dispatch_and_has_method_agree_test() ->
    %% The selector table is split: dispatch/3 lives here, has_method/1
    %% delegates to beamtalk_file:handle_has_method/1. Adding a selector to one
    %% and not the other makes respondsTo: lie about a method that works (or
    %% claim one that raises). Assert both directions for every selector.
    Selectors = [
        {'read:', [1]},
        {'readAll', []},
        {'write:', [<<"x">>]},
        {'writeLine:', [<<"x">>]},
        {'position', []},
        {'seek:', [0]},
        {'flush', []},
        {'sync', []},
        {'isOpen', []},
        {'lines', []}
    ],
    with_temp_handle(<<"data">>, readWrite, fun(Handle) ->
        [
            begin
                ?assert(beamtalk_file_handle:has_method(Selector)),
                %% Routed, not a does_not_understand: the call may answer an
                %% error Result, but it must not raise DNU.
                try
                    beamtalk_file_handle:dispatch(Selector, Args, Handle),
                    ok
                catch
                    error:#{error := #beamtalk_error{kind = does_not_understand}} ->
                        ?assert(false)
                end
            end
         || {Selector, Args} <- Selectors
        ]
    end).

mode_error_hint_names_the_denied_direction_test() ->
    %% The hint should name the mode that grants what the caller was denied,
    %% not blanket-recommend #readWrite in both directions.
    Hint = fun(Result) ->
        #{'errReason' := #{error := #beamtalk_error{hint = H}}} = Result,
        H
    end,
    %% Match the mode being prescribed (`mode: #read`), not a bare `#read`:
    %% both hints mention #readWrite as the both-ways option, and `#read` is a
    %% prefix of it, so the bare form would match either hint and prove nothing.
    with_temp_handle(<<"data">>, read, fun(ReadOnly) ->
        WriteHint = Hint(beamtalk_file_handle:write(ReadOnly, <<"x">>)),
        ?assertNotEqual(nomatch, binary:match(WriteHint, <<"mode: #write">>)),
        ?assertEqual(nomatch, binary:match(WriteHint, <<"mode: #read">>))
    end),
    with_temp_handle(<<>>, append, fun(AppendOnly) ->
        ReadHint = Hint(beamtalk_file_handle:read(AppendOnly, 1)),
        ?assertNotEqual(nomatch, binary:match(ReadHint, <<"mode: #read">>)),
        ?assertEqual(nomatch, binary:match(ReadHint, <<"mode: #write">>))
    end).
