%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_transcript_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% show: Tests
%%% ============================================================================

show_string_test() ->
    %% show: returns nil (output goes to stdout)
    ?assertEqual(nil, transcript:'show:'(<<"Hello">>)).

show_integer_test() ->
    ?assertEqual(nil, transcript:'show:'(42)).

show_atom_test() ->
    ?assertEqual(nil, transcript:'show:'(true)).

show_nil_test() ->
    ?assertEqual(nil, transcript:'show:'(nil)).

%%% ============================================================================
%%% cr Tests
%%% ============================================================================

cr_test() ->
    ?assertEqual(nil, transcript:cr()).

%%% ============================================================================
%%% has_method Tests
%%% ============================================================================

has_method_show_test() ->
    ?assert(transcript:has_method('show:')).

has_method_cr_test() ->
    ?assert(transcript:has_method(cr)).

has_method_unknown_test() ->
    ?assertNot(transcript:has_method(unknown)).

%%% ============================================================================
%%% Output Capture Tests
%%% ============================================================================

show_captures_string_output_test() ->
    Output = capture_output(fun() -> transcript:'show:'(<<"Hello">>) end),
    ?assertEqual(<<"Hello">>, Output).

show_captures_integer_output_test() ->
    Output = capture_output(fun() -> transcript:'show:'(42) end),
    ?assertEqual(<<"42">>, Output).

show_captures_atom_output_test() ->
    Output = capture_output(fun() -> transcript:'show:'(true) end),
    ?assertEqual(<<"true">>, Output).

show_captures_nil_output_test() ->
    Output = capture_output(fun() -> transcript:'show:'(nil) end),
    ?assertEqual(<<"nil">>, Output).

cr_captures_newline_test() ->
    Output = capture_output(fun() -> transcript:cr() end),
    ?assertEqual(<<"\n">>, Output).

show_and_cr_test() ->
    Output = capture_output(fun() ->
        transcript:'show:'(<<"Hello">>),
        transcript:cr(),
        transcript:'show:'(<<"World">>)
    end),
    ?assertEqual(<<"Hello\nWorld">>, Output).

show_float_test() ->
    Output = capture_output(fun() -> transcript:'show:'(3.14) end),
    ?assert(binary:match(Output, <<"3.14">>) =/= nomatch).

show_map_with_class_test() ->
    Output = capture_output(fun() ->
        transcript:'show:'(#{'$beamtalk_class' => 'Counter', value => 0})
    end),
    ?assertEqual(<<"a Counter">>, Output).

show_beamtalk_object_test() ->
    Output = capture_output(fun() ->
        transcript:'show:'({beamtalk_object, 'Counter', counter, self()})
    end),
    ?assertEqual(<<"a Counter">>, Output).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @doc Capture stdout output from a function.
capture_output(Fun) ->
    %% Redirect group leader to capture output
    OldGL = group_leader(),
    {ok, StringIO} = start_string_io(),
    group_leader(StringIO, self()),
    try
        Fun(),
        get_string_io_data(StringIO)
    after
        group_leader(OldGL, self()),
        StringIO ! stop
    end.

%% @doc Start a process that collects io:put_chars output.
start_string_io() ->
    Pid = spawn_link(fun() -> string_io_loop([]) end),
    {ok, Pid}.

string_io_loop(Acc) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _Encoding, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            string_io_loop([Acc, Chars]);
        {io_request, From, ReplyAs, {put_chars, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            string_io_loop([Acc, Chars]);
        {get_data, From} ->
            From ! {string_io_data, iolist_to_binary(Acc)},
            string_io_loop(Acc);
        stop ->
            ok
    end.

get_string_io_data(Pid) ->
    Pid ! {get_data, self()},
    receive
        {string_io_data, Data} -> Data
    after 1000 ->
        error(timeout)
    end.
