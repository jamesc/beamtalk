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

%%% `tls_preflight/2` — the `RELEASE_DISTRIBUTION=tls` refusal (ADR 0126
%%% amendment D2, BT-5233). `tls_preflight_main/0` halts, so the launcher
%%% scripts' use of it is covered by `cli_release.rs`.

-define(NO_EPMD, #{
    dist_opt_words => [], extra_optfiles => [], epmd_listening => fun(_, _) -> false end
}).

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
            Preflight = fun(Overrides) ->
                beamtalk_release_launcher:tls_preflight(Env(Overrides), ?NO_EPMD)
            end,
            ProblemsWith = fun(Overrides, Args) ->
                {error, Ps} = beamtalk_release_launcher:tls_preflight(
                    Env(Overrides), maps:merge(?NO_EPMD, Args)
                ),
                [unicode:characters_to_list(P) || P <- Ps]
            end,
            Problems = fun(Overrides) -> ProblemsWith(Overrides, #{}) end,
            [
                ?_assertEqual(
                    {ok, #{
                        proto => "inet_tls",
                        interface => {10, 0, 0, 5},
                        host => "node1.example.com",
                        optfile => Good,
                        notes => []
                    }},
                    Preflight(#{})
                ),
                ?_assertMatch(
                    {ok, #{proto := "inet6_tls", interface := {16#fd00, 0, 0, 0, 0, 0, 0, 5}}},
                    Preflight(#{"RELEASE_DIST_INTERFACE" => "fd00::5"})
                ),
                %% 0.0.0.0 only when the operator names it.
                ?_assertMatch(
                    {ok, #{interface := {0, 0, 0, 0}}},
                    Preflight(#{"RELEASE_DIST_INTERFACE" => "0.0.0.0"})
                ),
                ?_assertMatch(
                    {ok, #{notes := [_]}},
                    Preflight(#{"RELEASE_DIST_INTERFACE" => "127.0.0.1"})
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
                    ["the TLS distribution options " ++ _],
                    Problems(#{"RELEASE_SSL_DIST_OPTFILE" => NoFail})
                ),
                %% A weaker -ssl_dist_opt in vm.args overrides a good file.
                ?_assertMatch(
                    ["the TLS distribution options " ++ _],
                    ProblemsWith(#{}, #{dist_opt_words => ["client_verify", "verify_none"]})
                ),
                ?_assertMatch(
                    ["vm.args (or ERL_FLAGS) also sets -ssl_dist_optfile" ++ _],
                    ProblemsWith(#{}, #{extra_optfiles => ["/etc/other.conf"]})
                ),
                %% The launcher sets -proto_dist itself; a second one is refused.
                ?_assertMatch(
                    ["vm.args (or ERL_FLAGS) also sets -proto_dist inet_tcp" ++ _],
                    ProblemsWith(#{}, #{extra_proto_dists => ["inet_tcp"]})
                ),
                ?_assertMatch(
                    [
                        "RELEASE_HOST 192.0.2.7 and RELEASE_DIST_INTERFACE fd00::5 are different" ++
                            _
                    ],
                    Problems(#{
                        "RELEASE_HOST" => "192.0.2.7", "RELEASE_DIST_INTERFACE" => "fd00::5"
                    })
                ),
                ?_assertMatch(
                    {ok, #{proto := "inet6_tls"}},
                    Preflight(#{
                        "RELEASE_HOST" => "fd00::5", "RELEASE_DIST_INTERFACE" => "fd00::5"
                    })
                ),
                ?_assertMatch(
                    ["RELEASE_SSL_DIST_OPTFILE '" ++ _],
                    Problems(#{"RELEASE_SSL_DIST_OPTFILE" => filename:join(Dir, "my!node.conf")})
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
                    ["RELEASE_DIST_INTERFACE 'fe80::5%eth0' has a %zone" ++ _],
                    Problems(#{"RELEASE_DIST_INTERFACE" => "fe80::5%eth0"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST is not set" ++ _], Problems(#{"RELEASE_HOST" => false})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST '10.1' is not" ++ _], Problems(#{"RELEASE_HOST" => "10.1"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST '010.0.0.1' is not" ++ _],
                    Problems(#{"RELEASE_HOST" => "010.0.0.1"})
                ),
                ?_assertMatch({ok, _}, Preflight(#{"RELEASE_HOST" => "node1.123.example.com"})),
                ?_assertMatch(
                    ["RELEASE_HOST 'bad host' is not" ++ _],
                    Problems(#{"RELEASE_HOST" => "bad host"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST 'fe80::1%eth0' is not" ++ _],
                    Problems(#{"RELEASE_HOST" => "fe80::1%eth0"})
                ),
                ?_assertMatch(
                    {ok, #{host := "192.0.2.7"}},
                    Preflight(#{"RELEASE_HOST" => "192.0.2.7"})
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

%% An epmd that already runs on loopback only would hide the node from other
%% hosts; one that listens on the interface (or none at all) is fine.
tls_preflight_epmd_test_() ->
    {setup, fun() -> beamtalk_test_fs:tmp_dir("bt5233_epmd") end, fun beamtalk_test_fs:rm_rf/1, fun(
            Dir
        ) ->
            Good = filename:join(Dir, "good.conf"),
            ok = file:write_file(
                Good,
                "[{server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]},"
                " {client, [{verify, verify_peer}]}]."
            ),
            Env = fun(Iface, Port) ->
                Map = #{
                    "RELEASE_HOST" => "node1.example.com",
                    "RELEASE_DIST_INTERFACE" => Iface,
                    "RELEASE_SSL_DIST_OPTFILE" => Good,
                    "ERL_EPMD_PORT" => Port
                },
                fun(Name) -> maps:get(Name, Map, false) end
            end,
            Run = fun(Iface, Listening) ->
                Self = self(),
                Probe = fun(Addr, Port) ->
                    Self ! {probed, Addr, Port},
                    lists:member(Addr, Listening)
                end,
                beamtalk_release_launcher:tls_preflight(
                    Env(Iface, "4400"),
                    #{dist_opt_words => [], extra_optfiles => [], epmd_listening => Probe}
                )
            end,
            [
                ?_assertMatch(
                    {error, [
                        "an epmd is already running on this host but does not listen on 10.0.0.5 (port 4400)" ++
                            _
                    ]},
                    (fun() ->
                        {error, Ps} = Run("10.0.0.5", [{127, 0, 0, 1}]),
                        {error, [unicode:characters_to_list(P) || P <- Ps]}
                    end)()
                ),
                ?_assertMatch({ok, _}, Run("10.0.0.5", [{127, 0, 0, 1}, {10, 0, 0, 5}])),
                ?_assertMatch({ok, _}, Run("10.0.0.5", [])),
                %% Every interface: a probe always reaches loopback, so a
                %% running epmd is a note, not a refusal.
                ?_assertMatch({ok, #{notes := [_]}}, Run("0.0.0.0", [{127, 0, 0, 1}])),
                ?_assertMatch({ok, #{notes := []}}, Run("0.0.0.0", [])),
                ?_assertMatch({ok, _}, Run("127.0.0.1", [{127, 0, 0, 1}]))
            ]
        end}.

%% Under RELEASE_DISTRIBUTION=tls the launchers put their own
%% `-kernel inet_dist_use_interface` ahead of `-args_file vm.args`, whose
%% loopback value it must replace. That holds because `erl` keeps the first
%% of a repeated application parameter; pin it on every OTP CI runs.
kernel_parameter_first_wins_test_() ->
    {timeout, 60, fun() ->
        {ok, Peer, nonode@nohost} = peer:start_link(#{
            connection => standard_io,
            args => [
                "-kernel",
                "inet_dist_use_interface",
                "{10,0,0,5}",
                "-kernel",
                "inet_dist_use_interface",
                "{127,0,0,1}"
            ],
            wait_boot => 60000
        }),
        try
            ?assertEqual(
                {ok, {10, 0, 0, 5}},
                peer:call(Peer, application, get_env, [kernel, inet_dist_use_interface])
            )
        after
            peer:stop(Peer)
        end
    end}.
