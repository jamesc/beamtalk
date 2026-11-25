%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_launcher_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% Covers `beamtalk_release_launcher:rpc_entry/3` — the function
%%% `rpc_client_main/0` dispatches into over distribution (BT-3573). The
%%% halting entry points (`eval_main/0`, `rpc_client_main/0`,
%%% `ping_client_main/0`, `stop_client_main/0`) call `erlang:halt/1` and are
%%% exercised by the CLI integration tests
%%% (`crates/beamtalk-cli/tests/cli/cli_release.rs`), not here.

rpc_entry_unknown_class_returns_class_not_found_error_test() ->
    Result = beamtalk_release_launcher:rpc_entry(
        <<"ThisClassDoesNotExist">>, <<"run">>, []
    ),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result).

rpc_entry_invalid_selector_returns_invalid_argument_error_test() ->
    Result = beamtalk_release_launcher:rpc_entry(
        <<"Counter">>, <<"move:to:">>, []
    ),
    ?assertMatch({error, #beamtalk_error{kind = invalid_argument}}, Result).

rpc_entry_empty_class_returns_invalid_argument_error_test() ->
    Result = beamtalk_release_launcher:rpc_entry(<<>>, <<"run">>, []),
    ?assertMatch({error, #beamtalk_error{kind = invalid_argument}}, Result).

%%% `tls_preflight/1` — the `RELEASE_DISTRIBUTION=tls` refusal (ADR 0126
%%% amendment D2, BT-5233). `tls_preflight_main/0` halts, so the launcher
%%% scripts' use of it is covered by `cli_release.rs`.

tls_preflight_test_() ->
    {setup, fun() -> beamtalk_test_fs:tmp_dir("bt5233_preflight") end, fun beamtalk_test_fs:rm_rf/1,
        fun(Dir) ->
            Good = filename:join(Dir, "good.conf"),
            ok = file:write_file(
                Good,
                "[{server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]},"
                " {client, [{verify, verify_peer}]}]."
            ),
            NoFail = filename:join(Dir, "nofail.conf"),
            ok = file:write_file(
                NoFail,
                "[{server, [{verify, verify_peer}]}, {client, [{verify, verify_peer}]}]."
            ),
            Env = fun(Overrides) ->
                Base = #{
                    "RELEASE_HOST" => "node1.example.com",
                    "RELEASE_DIST_INTERFACE" => "10.0.0.5",
                    "RELEASE_SSL_DIST_OPTFILE" => Good
                },
                Map = maps:merge(Base, Overrides),
                fun(Name) -> maps:get(Name, Map, false) end
            end,
            Problems = fun(Overrides) ->
                {error, Ps} = beamtalk_release_launcher:tls_preflight(Env(Overrides)),
                [unicode:characters_to_list(P) || P <- Ps]
            end,
            [
                ?_assertEqual(
                    {ok, #{
                        proto => "inet_tls",
                        interface => {10, 0, 0, 5},
                        host => "node1.example.com",
                        optfile => Good,
                        notes => []
                    }},
                    beamtalk_release_launcher:tls_preflight(Env(#{}))
                ),
                ?_assertMatch(
                    {ok, #{proto := "inet6_tls", interface := {16#fd00, 0, 0, 0, 0, 0, 0, 5}}},
                    beamtalk_release_launcher:tls_preflight(
                        Env(#{"RELEASE_DIST_INTERFACE" => "fd00::5"})
                    )
                ),
                %% 0.0.0.0 only when the operator names it.
                ?_assertMatch(
                    {ok, #{interface := {0, 0, 0, 0}}},
                    beamtalk_release_launcher:tls_preflight(
                        Env(#{"RELEASE_DIST_INTERFACE" => "0.0.0.0"})
                    )
                ),
                ?_assertMatch(
                    {ok, #{notes := [_]}},
                    beamtalk_release_launcher:tls_preflight(
                        Env(#{"RELEASE_DIST_INTERFACE" => "127.0.0.1"})
                    )
                ),
                ?_assertMatch(
                    ["RELEASE_SSL_DIST_OPTFILE is not set" ++ _],
                    Problems(#{"RELEASE_SSL_DIST_OPTFILE" => false})
                ),
                ?_assertMatch(
                    ["the TLS distribution option file '" ++ _],
                    Problems(#{"RELEASE_SSL_DIST_OPTFILE" => filename:join(Dir, "absent.conf")})
                ),
                ?_assertMatch(
                    ["the TLS distribution option file does not make both sides verify" ++ _],
                    Problems(#{"RELEASE_SSL_DIST_OPTFILE" => NoFail})
                ),
                ?_assertMatch(
                    ["RELEASE_DIST_INTERFACE is not set" ++ _],
                    Problems(#{"RELEASE_DIST_INTERFACE" => false})
                ),
                ?_assertMatch(
                    ["RELEASE_DIST_INTERFACE 'eth0' is not one IP address" ++ _],
                    Problems(#{"RELEASE_DIST_INTERFACE" => "eth0"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST is not set" ++ _], Problems(#{"RELEASE_HOST" => false})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST 'bad host' is not" ++ _],
                    Problems(#{"RELEASE_HOST" => "bad host"})
                ),
                ?_assertMatch(
                    {ok, #{host := "192.0.2.7"}},
                    beamtalk_release_launcher:tls_preflight(Env(#{"RELEASE_HOST" => "192.0.2.7"}))
                ),
                %% Every problem at once.
                ?_assertEqual(
                    3,
                    length(
                        Problems(#{
                            "RELEASE_HOST" => false,
                            "RELEASE_DIST_INTERFACE" => false,
                            "RELEASE_SSL_DIST_OPTFILE" => false
                        })
                    )
                )
            ]
        end}.
