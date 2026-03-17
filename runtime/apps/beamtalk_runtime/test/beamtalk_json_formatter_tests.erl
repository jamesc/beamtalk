%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Runtime Context

%%% @doc EUnit tests for beamtalk_json_formatter module.
%%%
%%% Tests JSON log formatting: field inclusion, time formatting, and
%%% metadata extraction.

-module(beamtalk_json_formatter_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

check_config_accepts_any_test() ->
    ?assertEqual(ok, beamtalk_json_formatter:check_config(#{})),
    ?assertEqual(ok, beamtalk_json_formatter:check_config(#{custom => true})).

format_returns_iodata_with_newline_test() ->
    Event = #{
        level => info,
        msg => {string, "hello world"},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Result = beamtalk_json_formatter:format(Event, #{}),
    Bin = iolist_to_binary(Result),
    %% Should end with a newline
    ?assertEqual($\n, binary:last(Bin)),
    %% Should be valid JSON (minus the trailing newline)
    JsonBin = binary:part(Bin, 0, byte_size(Bin) - 1),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assert(is_map(Decoded)).

format_includes_required_fields_test() ->
    Event = #{
        level => warning,
        msg => {string, "test message"},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"warning">>, maps:get(<<"level">>, Decoded)),
    ?assertEqual(<<"test message">>, maps:get(<<"msg">>, Decoded)),
    ?assert(maps:is_key(<<"time">>, Decoded)).

format_includes_beamtalk_class_metadata_test() ->
    Event = #{
        level => info,
        msg => {string, "actor started"},
        meta => #{
            time => erlang:system_time(microsecond),
            beamtalk_class => 'Counter'
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, Decoded)).

format_includes_beamtalk_selector_metadata_test() ->
    Event = #{
        level => debug,
        msg => {string, "dispatch"},
        meta => #{
            time => erlang:system_time(microsecond),
            beamtalk_selector => increment
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"increment">>, maps:get(<<"selector">>, Decoded)).

format_includes_domain_metadata_test() ->
    Event = #{
        level => info,
        msg => {string, "domain test"},
        meta => #{
            time => erlang:system_time(microsecond),
            domain => [beamtalk, runtime]
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"beamtalk.runtime">>, maps:get(<<"domain">>, Decoded)).

format_includes_mfa_metadata_test() ->
    Event = #{
        level => info,
        msg => {string, "mfa test"},
        meta => #{
            time => erlang:system_time(microsecond),
            mfa => {my_module, my_fun, 2}
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"my_module:my_fun/2">>, maps:get(<<"mfa">>, Decoded)).

format_includes_pid_metadata_test() ->
    Event = #{
        level => info,
        msg => {string, "pid test"},
        meta => #{
            time => erlang:system_time(microsecond),
            pid => self()
        }
    },
    Decoded = decode_event(Event),
    ?assert(maps:is_key(<<"pid">>, Decoded)).

format_omits_absent_optional_fields_test() ->
    Event = #{
        level => error,
        msg => {string, "bare event"},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertNot(maps:is_key(<<"class">>, Decoded)),
    ?assertNot(maps:is_key(<<"selector">>, Decoded)),
    ?assertNot(maps:is_key(<<"domain">>, Decoded)),
    ?assertNot(maps:is_key(<<"mfa">>, Decoded)),
    ?assertNot(maps:is_key(<<"pid">>, Decoded)).

format_handles_format_args_msg_test() ->
    Event = #{
        level => info,
        msg => {"Count is ~B", [42]},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"Count is 42">>, maps:get(<<"msg">>, Decoded)).

format_time_is_iso8601_test() ->
    Event = #{
        level => info,
        msg => {string, "time test"},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    TimeBin = maps:get(<<"time">>, Decoded),
    %% Should match ISO 8601 pattern: YYYY-MM-DDTHH:MM:SS.mmmZ
    ?assertMatch(
        <<_:4/binary, $-, _:2/binary, $-, _:2/binary, $T, _:2/binary, $:, _:2/binary, $:,
            _:2/binary, $., _:3/binary, $Z>>,
        TimeBin
    ).

%%====================================================================
%% Helpers
%%====================================================================

decode_event(Event) ->
    Result = beamtalk_json_formatter:format(Event, #{}),
    Bin = iolist_to_binary(Result),
    JsonBin = binary:part(Bin, 0, byte_size(Bin) - 1),
    jsx:decode(JsonBin, [return_maps]).
