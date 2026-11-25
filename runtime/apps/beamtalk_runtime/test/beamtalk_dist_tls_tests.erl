%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_tls_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_dist_tls`: TLS distribution counts only with peer
verification on both sides and `fail_if_no_peer_cert` on the server
(ADR 0126 amendment D2, BT-5233).
""".

-include_lib("eunit/include/eunit.hrl").

-define(SERVER_OK, {server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]}).
-define(CLIENT_OK, {client, [{verify, verify_peer}]}).

%%====================================================================
%% peer_verification/2 and verified/3
%%====================================================================

complete_optfile_is_verified_test() ->
    ?assertEqual(ok, beamtalk_dist_tls:peer_verification([], [?SERVER_OK, ?CLIENT_OK])),
    ?assert(beamtalk_dist_tls:verified(["inet_tls"], [], [?SERVER_OK, ?CLIENT_OK])),
    ?assert(beamtalk_dist_tls:verified(["inet6_tls"], [], [?SERVER_OK, ?CLIENT_OK])).

inet_tls_without_settings_is_not_verified_test() ->
    ?assertNot(beamtalk_dist_tls:verified(["inet_tls"], [], [])),
    ?assertEqual(
        {error,
            {peer_verification, [
                server_verify_peer, server_fail_if_no_peer_cert, client_verify_peer
            ]}},
        beamtalk_dist_tls:peer_verification([], [])
    ).

each_missing_setting_is_named_test_() ->
    [
        ?_assertEqual(
            {error, {peer_verification, [Missing]}},
            beamtalk_dist_tls:peer_verification([], Opts)
        )
     || {Missing, Opts} <- [
            {server_verify_peer, [{server, [{fail_if_no_peer_cert, true}]}, ?CLIENT_OK]},
            {server_fail_if_no_peer_cert, [{server, [{verify, verify_peer}]}, ?CLIENT_OK]},
            {client_verify_peer, [?SERVER_OK, {client, []}]}
        ]
    ].

verify_peer_without_fail_if_no_peer_cert_is_not_verified_test() ->
    Opts = [{server, [{verify, verify_peer}]}, ?CLIENT_OK],
    ?assertNot(beamtalk_dist_tls:verified(["inet_tls"], [], Opts)).

%% Code review of BT-5233: OTP runs only the first `-proto_dist`, and listens
%% on every protocol in it, so each must be TLS.
mixed_or_tcp_first_proto_dist_is_not_verified_test() ->
    ?assertNot(beamtalk_dist_tls:verified(["inet_tcp", "inet_tls"], [], [?SERVER_OK, ?CLIENT_OK])),
    ?assert(beamtalk_dist_tls:verified(["inet_tls", "inet6_tls"], [], [?SERVER_OK, ?CLIENT_OK])).

%% A `verify_fun` or `partial_chain` can accept a peer the CA did not sign.
overriding_options_are_refused_test_() ->
    Accept = {fun(_, _, S) -> {valid, S} end, []},
    [
        ?_assertEqual(
            {error, {peer_verification, [{forbidden, server, verify_fun}]}},
            beamtalk_dist_tls:peer_verification([], [
                {server, [
                    {verify, verify_peer}, {fail_if_no_peer_cert, true}, {verify_fun, Accept}
                ]},
                ?CLIENT_OK
            ])
        ),
        ?_assertEqual(
            {error, {peer_verification, [{forbidden, client, partial_chain}]}},
            beamtalk_dist_tls:peer_verification([], [
                ?SERVER_OK,
                {client, [{verify, verify_peer}, {partial_chain, fun(_) -> unknown_ca end}]}
            ])
        ),
        ?_assertEqual(
            {error, {peer_verification, [{forbidden, client, verify_fun}]}},
            beamtalk_dist_tls:peer_verification(["client_verify_fun", "m,f,s"], [
                ?SERVER_OK, ?CLIENT_OK
            ])
        ),
        ?_assert(
            has(
                "remove verify_fun from the server list",
                beamtalk_dist_tls:format_error(
                    {peer_verification, [{forbidden, server, verify_fun}]}
                )
            )
        )
    ].

%% The launcher's check counts `-ssl_dist_opt` words (from `vm.args` or
%% `ERL_*FLAGS`) with the file: OTP puts them ahead of the file's options.
weaker_words_override_a_good_file_test() ->
    ?assertEqual(
        {error, {peer_verification, [server_verify_peer]}},
        beamtalk_dist_tls:peer_verification(["server_verify", "verify_none"], [
            ?SERVER_OK, ?CLIENT_OK
        ])
    ).

%% PR review of BT-5233: the client verbs start without `vm.args`, so
%% `-ssl_dist_opt` words there cannot stand in for a setting the file lacks.
words_cannot_complete_an_incomplete_file_test_() ->
    {setup, fun() -> beamtalk_test_fs:tmp_dir("bt5233_words") end, fun beamtalk_test_fs:rm_rf/1,
        fun(Dir) ->
            Path = filename:join(Dir, "noclient.conf"),
            ok = file:write_file(
                Path, "[{server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]}]."
            ),
            [
                ?_assertEqual(
                    {error, {peer_verification, [client_verify_peer]}},
                    beamtalk_dist_tls:check_optfile(Path, ["client_verify", "verify_peer"])
                )
            ]
        end}.

ssl_unavailable_is_explained_test() ->
    ?assert(has("ssl application", beamtalk_dist_tls:format_error(ssl_unavailable))).

plain_tcp_is_never_verified_test() ->
    ?assertNot(beamtalk_dist_tls:verified([], [], [?SERVER_OK, ?CLIENT_OK])),
    ?assertNot(beamtalk_dist_tls:verified(["inet_tcp"], [], [?SERVER_OK, ?CLIENT_OK])).

ssl_dist_opt_words_count_test() ->
    Words = [
        "server_verify",
        "verify_peer",
        "server_fail_if_no_peer_cert",
        "true",
        "client_verify",
        "verify_peer"
    ],
    ?assert(beamtalk_dist_tls:verified(["inet_tls"], Words, [])).

%% OTP reads both sources, so a weaker value in either one fails closed.
conflicting_value_fails_closed_test_() ->
    [
        ?_assertNot(
            beamtalk_dist_tls:verified(["inet_tls"], ["server_verify", "verify_none"], [
                ?SERVER_OK, ?CLIENT_OK
            ])
        ),
        ?_assertNot(
            beamtalk_dist_tls:verified(["inet_tls"], [], [
                ?SERVER_OK, ?CLIENT_OK, {client, [{verify, verify_none}]}
            ])
        ),
        ?_assertNot(
            beamtalk_dist_tls:verified(["inet_tls"], [], [
                {server, [{verify, verify_peer}, {fail_if_no_peer_cert, false}]}, ?CLIENT_OK
            ])
        )
    ].

%%====================================================================
%% read_optfile/1 and check_optfile/2
%%====================================================================

optfile_test_() ->
    {setup, fun() -> beamtalk_test_fs:tmp_dir("bt5233_optfile") end, fun beamtalk_test_fs:rm_rf/1,
        fun(Dir) ->
            Write = fun(Name, Content) ->
                Path = filename:join(Dir, Name),
                ok = file:write_file(Path, Content),
                Path
            end,
            Good = Write(
                "good.conf",
                "[{server, [{certfile, \"c.pem\"}, {verify, verify_peer},\n"
                "           {fail_if_no_peer_cert, true}]},\n"
                " {client, [{verify, verify_peer}]}].\n"
            ),
            NoFail = Write(
                "nofail.conf",
                "[{server, [{verify, verify_peer}]}, {client, [{verify, verify_peer}]}]."
            ),
            NoClient = Write(
                "noclient.conf",
                "[{server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]}]."
            ),
            Garbage = Write("garbage.conf", "[{server, ["),
            %% OTP keeps one entry per side (the last), so a repeated side
            %% could hide a weaker list behind a complete one.
            Duplicate = Write(
                "duplicate.conf",
                "[{server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]},"
                " {client, [{verify, verify_peer}]},"
                " {server, [{certfile, \"c.pem\"}]}]."
            ),
            Stray = Write(
                "stray.conf",
                "[{server, [{verify, verify_peer}, {fail_if_no_peer_cert, true}]},"
                " {client, [{verify, verify_peer}]}, {verify, verify_none}]."
            ),
            Missing = filename:join(Dir, "absent.conf"),
            [
                ?_assertEqual(ok, beamtalk_dist_tls:check_optfile(Good, [])),
                ?_assertEqual(
                    {error, {peer_verification, [server_fail_if_no_peer_cert]}},
                    beamtalk_dist_tls:check_optfile(NoFail, [])
                ),
                ?_assertEqual(
                    {error, {peer_verification, [client_verify_peer]}},
                    beamtalk_dist_tls:check_optfile(NoClient, [])
                ),
                ?_assertMatch(
                    {error, {optfile_malformed, _, _}}, beamtalk_dist_tls:check_optfile(Garbage, [])
                ),
                ?_assertEqual(
                    {error, {optfile_missing, Missing}},
                    beamtalk_dist_tls:check_optfile(Missing, [])
                ),
                ?_assertEqual(
                    {error, {optfile_malformed, Duplicate, {duplicate_side, server}}},
                    beamtalk_dist_tls:check_optfile(Duplicate, [])
                ),
                ?_assertEqual(
                    {error, {optfile_malformed, Stray, {not_a_side, {verify, verify_none}}}},
                    beamtalk_dist_tls:check_optfile(Stray, [])
                ),
                ?_assert(
                    has(
                        "{fail_if_no_peer_cert, true} to the server list",
                        beamtalk_dist_tls:format_error(
                            {peer_verification, [server_fail_if_no_peer_cert]}
                        )
                    )
                ),
                ?_assert(
                    has(
                        "RELEASE_SSL_DIST_OPTFILE",
                        beamtalk_dist_tls:format_error({optfile_missing, Missing})
                    )
                )
            ]
        end}.

has(Needle, Chardata) ->
    string:find(unicode:characters_to_list(Chardata), Needle) =/= nomatch.
