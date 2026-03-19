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
%% OTP Report Tests
%%====================================================================

format_gen_server_terminate_report_test() ->
    Report = #{
        label => {gen_server, terminate},
        name => my_server,
        reason => {error, function_clause}
    },
    Event = #{
        level => error,
        msg => {report, Report},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertMatch(<<"gen_server my_server terminated">>, maps:get(<<"msg">>, Decoded)),
    ?assertEqual(<<"gen_server_terminate">>, maps:get(<<"report_type">>, Decoded)),
    ?assert(maps:is_key(<<"reason">>, Decoded)),
    %% Should infer domain as "otp"
    ?assertEqual(<<"otp">>, maps:get(<<"domain">>, Decoded)),
    %% Server name goes in "name" field, not "mfa"
    ?assertEqual(<<"my_server">>, maps:get(<<"name">>, Decoded)),
    ?assertNot(maps:is_key(<<"mfa">>, Decoded)).

format_supervisor_progress_report_test() ->
    Report = #{
        label => {supervisor, progress},
        supervisor => {local, my_sup},
        started => [{id, my_child}, {pid, self()}]
    },
    Event = #{
        level => info,
        msg => {report, Report},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assert(binary:match(maps:get(<<"msg">>, Decoded), <<"supervisor">>) =/= nomatch),
    ?assertEqual(<<"supervisor_progress">>, maps:get(<<"report_type">>, Decoded)),
    ?assertEqual(<<"otp">>, maps:get(<<"domain">>, Decoded)),
    %% Supervisor name goes in "name" field, not "mfa"
    ?assertEqual(<<"{local,my_sup}">>, maps:get(<<"name">>, Decoded)),
    ?assertNot(maps:is_key(<<"mfa">>, Decoded)).

format_supervisor_child_terminated_report_test() ->
    Report = #{
        label => {supervisor, child_terminated},
        supervisor => {local, my_sup},
        reason => normal
    },
    Event = #{
        level => warning,
        msg => {report, Report},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"supervisor_child_terminated">>, maps:get(<<"report_type">>, Decoded)),
    ?assertEqual(<<"otp">>, maps:get(<<"domain">>, Decoded)).

format_proc_lib_crash_report_test() ->
    Report = #{
        label => {proc_lib, crash},
        report => [
            {error_info, {exit, crashed, []}},
            {initial_call, {my_mod, init, 1}}
        ]
    },
    Event = #{
        level => error,
        msg => {report, Report},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"proc_lib_crash">>, maps:get(<<"report_type">>, Decoded)),
    ?assertEqual(<<"otp">>, maps:get(<<"domain">>, Decoded)),
    ?assert(maps:is_key(<<"initial_call">>, Decoded)).

format_explicit_domain_overrides_inference_test() ->
    %% When domain is explicitly set, it should take precedence over inference
    Report = #{label => {gen_server, terminate}, name => my_server, reason => normal},
    Event = #{
        level => error,
        msg => {report, Report},
        meta => #{
            time => erlang:system_time(microsecond),
            domain => [beamtalk, runtime]
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"beamtalk.runtime">>, maps:get(<<"domain">>, Decoded)).

format_unknown_report_no_domain_inference_test() ->
    %% Unknown reports should not get a domain inferred
    Report = #{some_key => some_value},
    Event = #{
        level => info,
        msg => {report, Report},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertNot(maps:is_key(<<"domain">>, Decoded)).

format_report_with_callback_test() ->
    %% Reports with report_cb should use the callback for message formatting
    Report = #{custom => data},
    Event = #{
        level => info,
        msg => {report, Report},
        meta => #{
            time => erlang:system_time(microsecond),
            report_cb => fun(_R) -> {"Custom: ~s", ["formatted"]} end
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"Custom: formatted">>, maps:get(<<"msg">>, Decoded)).

format_stdlib_domain_test() ->
    Event = #{
        level => info,
        msg => {string, "stdlib domain test"},
        meta => #{
            time => erlang:system_time(microsecond),
            domain => [beamtalk, stdlib]
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"beamtalk.stdlib">>, maps:get(<<"domain">>, Decoded)).

%%====================================================================
%% Extra Metadata Tests
%%====================================================================

format_includes_extra_metadata_test() ->
    Event = #{
        level => error,
        msg => {string, "handler crashed"},
        meta => #{
            time => erlang:system_time(microsecond),
            reason => badarg,
            error_class => error
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"badarg">>, maps:get(<<"reason">>, Decoded)),
    ?assertEqual(<<"error">>, maps:get(<<"error_class">>, Decoded)).

format_includes_stacktrace_metadata_test() ->
    Stack = [{my_mod, my_fun, 2, [{file, "my_mod.erl"}, {line, 42}]}],
    Event = #{
        level => error,
        msg => {string, "crashed"},
        meta => #{
            time => erlang:system_time(microsecond),
            stacktrace => Stack
        }
    },
    Decoded = decode_event(Event),
    StackBin = maps:get(<<"stacktrace">>, Decoded),
    ?assert(is_binary(StackBin)),
    ?assert(binary:match(StackBin, <<"my_mod">>) =/= nomatch).

format_handles_unicode_format_args_test() ->
    %% Unicode arrows (→) in format strings previously crashed iolist_to_binary
    Event = #{
        level => notice,
        msg => {"Migrate: load-file ~ts ~ts", [<<226, 134, 146>>, <<"reload">>]},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assert(is_binary(maps:get(<<"msg">>, Decoded))).

format_extra_meta_handles_binary_keys_test() ->
    %% User-supplied metadata (e.g. from Beamtalk Dictionary) may have binary keys
    Event = #{
        level => info,
        msg => {string, "test"},
        meta => #{
            time => erlang:system_time(microsecond),
            <<"prompt_length">> => 42
        }
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"42">>, maps:get(<<"prompt_length">>, Decoded)).

format_extra_meta_excludes_standard_keys_test() ->
    Event = #{
        level => info,
        msg => {string, "test"},
        meta => #{
            time => erlang:system_time(microsecond),
            gl => self(),
            file => "test.erl",
            line => 10
        }
    },
    Decoded = decode_event(Event),
    ?assertNot(maps:is_key(<<"gl">>, Decoded)),
    ?assertNot(maps:is_key(<<"file">>, Decoded)),
    ?assertNot(maps:is_key(<<"line">>, Decoded)).

format_falls_back_to_plain_text_on_formatter_failure_test() ->
    %% Force the normal formatter path to fail so the catch branch is exercised.
    %% A binary in the domain list makes atom_to_list/1 crash inside format_domain.
    Event = #{
        level => info,
        msg => {string, "safe msg"},
        meta => #{
            time => erlang:system_time(microsecond),
            domain => [<<"not-an-atom">>]
        }
    },
    Result = beamtalk_json_formatter:format(Event, #{}),
    Bin = iolist_to_binary(Result),
    ?assert(binary:match(Bin, <<"formatter error:">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"safe msg">>) =/= nomatch).

format_handles_format_args_with_binary_test() ->
    %% The exact pattern produced by the Logger intrinsic codegen
    Event = #{
        level => info,
        msg => {"~ts", [<<"http server started">>]},
        meta => #{time => erlang:system_time(microsecond)}
    },
    Decoded = decode_event(Event),
    ?assertEqual(<<"http server started">>, maps:get(<<"msg">>, Decoded)).

%%====================================================================
%% Helpers
%%====================================================================

decode_event(Event) ->
    Result = beamtalk_json_formatter:format(Event, #{}),
    Bin = iolist_to_binary(Result),
    JsonBin = binary:part(Bin, 0, byte_size(Bin) - 1),
    jsx:decode(JsonBin, [return_maps]).
