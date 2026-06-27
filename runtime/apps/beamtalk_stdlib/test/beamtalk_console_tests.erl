%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_console_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_console module.

Covers all read-side paths, pure internal helpers, and the write-side
error/exit catch — all areas missing coverage in the 28.9%-covered
source module (CI run 28260389429).

## Strategy

Read-side tests (`readLine`, `readLine:`, `readLine/1` FFI shim) use a
**mock group_leader** — a process that replies to `{io_request, ...}` with
a controlled response. `standard_io` resolves to the calling process's
group_leader, so swapping it intercepts all `io:get_line` and
`io:put_chars` calls directed at `standard_io`.

The mock is installed for the duration of each test and restored
atomically in an `after` block so a crashing test never leaks a stale
group_leader.

Pure internal helpers (`closed_device/1`, `to_binary/1`, `strip_eol/1`)
are exported under `-ifdef(TEST)` in the source module and tested
directly.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Mock group_leader helpers
%%% ============================================================================

%% Spawn a mock io server that replies to every io_request with Response.
mock_gl(Response) ->
    spawn(fun() -> mock_gl_loop(Response) end).

mock_gl_loop(Response) ->
    receive
        {io_request, From, ReplyAs, _Req} ->
            From ! {io_reply, ReplyAs, Response},
            mock_gl_loop(Response)
    end.

%% Run Fun with a mock group_leader that replies with Response to all io requests.
with_mock_gl(Response, Fun) ->
    OldGL = group_leader(),
    MockGL = mock_gl(Response),
    group_leader(MockGL, self()),
    try
        Fun()
    after
        group_leader(OldGL, self()),
        exit(MockGL, kill)
    end.

%% Run Fun with a group_leader that exits without replying to any io request.
%% Simulates a dead or disconnected io server (broken pipe / detached node).
with_dying_gl(Fun) ->
    OldGL = group_leader(),
    DyingGL = spawn(fun() ->
        receive
            {io_request, _, _, _} -> exit(terminated)
        end
    end),
    group_leader(DyingGL, self()),
    try
        Fun()
    after
        group_leader(OldGL, self())
    end.

%%% ============================================================================
%%% flush/0
%%% ============================================================================

flush_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_console:flush()).

%%% ============================================================================
%%% readLine/0 — all read/2 return paths
%%% ============================================================================

read_line_eof_returns_nil_test() ->
    with_mock_gl(eof, fun() ->
        ?assertEqual(nil, beamtalk_console:readLine())
    end).

read_line_error_terminated_returns_nil_test() ->
    with_mock_gl({error, terminated}, fun() ->
        ?assertEqual(nil, beamtalk_console:readLine())
    end).

read_line_error_ebadf_returns_nil_test() ->
    with_mock_gl({error, ebadf}, fun() ->
        ?assertEqual(nil, beamtalk_console:readLine())
    end).

read_line_error_epipe_returns_nil_test() ->
    with_mock_gl({error, epipe}, fun() ->
        ?assertEqual(nil, beamtalk_console:readLine())
    end).

read_line_genuine_error_raises_io_error_test() ->
    with_mock_gl({error, badarg}, fun() ->
        ?assertError(
            #{error := #beamtalk_error{kind = io_error}},
            beamtalk_console:readLine()
        )
    end).

read_line_binary_strips_lf_test() ->
    with_mock_gl(<<"hello\n">>, fun() ->
        ?assertEqual(<<"hello">>, beamtalk_console:readLine())
    end).

read_line_binary_strips_crlf_test() ->
    with_mock_gl(<<"hello\r\n">>, fun() ->
        ?assertEqual(<<"hello">>, beamtalk_console:readLine())
    end).

read_line_binary_no_newline_returns_as_is_test() ->
    with_mock_gl(<<"hello">>, fun() ->
        ?assertEqual(<<"hello">>, beamtalk_console:readLine())
    end).

read_line_list_data_converted_to_binary_test() ->
    %% io:get_line/2 can return a codepoint list on some io servers.
    with_mock_gl("world\n", fun() ->
        ?assertEqual(<<"world">>, beamtalk_console:readLine())
    end).

read_line_exit_from_io_server_returns_nil_test() ->
    %% A dead io server causes io:get_line to raise exit:_ — caught by read/2.
    with_dying_gl(fun() ->
        ?assertEqual(nil, beamtalk_console:readLine())
    end).

%%% ============================================================================
%%% readLine:/1 — prompt is written before reading
%%% ============================================================================

read_line_with_prompt_eof_returns_nil_test() ->
    with_mock_gl(eof, fun() ->
        ?assertEqual(nil, beamtalk_console:'readLine:'(<<"Enter: ">>))
    end).

read_line_with_prompt_returns_stripped_line_test() ->
    with_mock_gl(<<"answer\n">>, fun() ->
        ?assertEqual(<<"answer">>, beamtalk_console:'readLine:'(<<"Prompt: ">>))
    end).

read_line_with_integer_prompt_renders_and_reads_test() ->
    with_mock_gl(<<"42\n">>, fun() ->
        ?assertEqual(<<"42">>, beamtalk_console:'readLine:'(42))
    end).

%%% ============================================================================
%%% readLine/1 FFI shim
%%% ============================================================================

read_line_ffi_shim_eof_returns_nil_test() ->
    with_mock_gl(eof, fun() ->
        ?assertEqual(nil, beamtalk_console:readLine(<<"Enter: ">>))
    end).

read_line_ffi_shim_returns_line_test() ->
    with_mock_gl(<<"input\n">>, fun() ->
        ?assertEqual(<<"input">>, beamtalk_console:readLine(<<"? ">>))
    end).

%%% ============================================================================
%%% write/3 exit path — via print: with a dying group_leader
%%% ============================================================================

print_to_dying_device_returns_nil_test() ->
    %% When the io server exits without replying, io:put_chars raises exit:_,
    %% caught by write/3's `exit:_Reason -> nil` clause.
    with_dying_gl(fun() ->
        ?assertEqual(nil, beamtalk_console:'print:'(<<"hello">>))
    end).

%%% ============================================================================
%%% closed_device/1 — all four clauses (via test export)
%%% ============================================================================

closed_device_terminated_returns_true_test() ->
    ?assertEqual(true, beamtalk_console:closed_device(terminated)).

closed_device_ebadf_returns_true_test() ->
    ?assertEqual(true, beamtalk_console:closed_device(ebadf)).

closed_device_epipe_returns_true_test() ->
    ?assertEqual(true, beamtalk_console:closed_device(epipe)).

closed_device_unknown_reason_returns_false_test() ->
    ?assertEqual(false, beamtalk_console:closed_device(badarg)).

closed_device_enotsup_returns_false_test() ->
    %% enotsup is explicitly documented as NOT on the closed-device allowlist.
    ?assertEqual(false, beamtalk_console:closed_device(enotsup)).

closed_device_arbitrary_atom_returns_false_test() ->
    ?assertEqual(false, beamtalk_console:closed_device(other_reason)).

%%% ============================================================================
%%% to_binary/1 — binary passthrough and list conversion (via test export)
%%% ============================================================================

to_binary_binary_returns_same_test() ->
    ?assertEqual(<<"hello">>, beamtalk_console:to_binary(<<"hello">>)).

to_binary_empty_binary_test() ->
    ?assertEqual(<<>>, beamtalk_console:to_binary(<<>>)).

to_binary_list_ascii_test() ->
    ?assertEqual(<<"hello">>, beamtalk_console:to_binary("hello")).

to_binary_list_empty_test() ->
    ?assertEqual(<<>>, beamtalk_console:to_binary([])).

%%% ============================================================================
%%% strip_eol/1 — three pattern clauses (via test export)
%%% ============================================================================

strip_eol_removes_lf_test() ->
    ?assertEqual(<<"hello">>, beamtalk_console:strip_eol(<<"hello\n">>)).

strip_eol_removes_crlf_test() ->
    ?assertEqual(<<"hello">>, beamtalk_console:strip_eol(<<"hello\r\n">>)).

strip_eol_no_newline_unchanged_test() ->
    ?assertEqual(<<"hello">>, beamtalk_console:strip_eol(<<"hello">>)).

strip_eol_empty_unchanged_test() ->
    ?assertEqual(<<>>, beamtalk_console:strip_eol(<<>>)).

strip_eol_only_lf_returns_empty_test() ->
    ?assertEqual(<<>>, beamtalk_console:strip_eol(<<"\n">>)).

strip_eol_only_crlf_returns_empty_test() ->
    ?assertEqual(<<>>, beamtalk_console:strip_eol(<<"\r\n">>)).
