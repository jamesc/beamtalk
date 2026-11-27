%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_launcher_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Brackets the answer the probe erl prints (see collect_port/2).
-define(ERL_ANSWER_MARK, <<"@@bt-erl-answer@@">>).

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
                    ["RELEASE_SSL_DIST_OPTFILE '" ++ _],
                    Problems(#{"RELEASE_SSL_DIST_OPTFILE" => filename:join(Dir, "n\x{e9}ud.conf")})
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
                %% `-name` needs a fully qualified host: OTP refuses to connect
                %% to `x@node1` ("Hostname node1 is illegal").
                ?_assertMatch(
                    ["RELEASE_HOST 'node1' is not" ++ _], Problems(#{"RELEASE_HOST" => "node1"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST 'localhost' is not" ++ _],
                    Problems(#{"RELEASE_HOST" => "localhost"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST '010.0.0.1' is not" ++ _],
                    Problems(#{"RELEASE_HOST" => "010.0.0.1"})
                ),
                ?_assertMatch(
                    ["RELEASE_HOST '0x7f.1' is not" ++ _], Problems(#{"RELEASE_HOST" => "0x7f.1"})
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

%% BT-5260: only the distribution flags are read from a `vm.args` overlay.
vm_args_dist_flags_test_() ->
    Flags = fun(Text) -> beamtalk_release_launcher:vm_args_dist_flags(Text) end,
    [
        ?_assertEqual(
            #{ssl_dist_opt => [], ssl_dist_optfile => [], proto_dist => [], problems => []},
            Flags("-mode interactive\n-sname stray\n-detached\n-s init stop\n-eval \"x().\"\n")
        ),
        %% A stray -sname does not hide a later weaker -ssl_dist_opt.
        ?_assertEqual(
            #{
                ssl_dist_opt => ["server_verify", "verify_none"],
                ssl_dist_optfile => [],
                proto_dist => [],
                problems => []
            },
            Flags("-sname stray\n-ssl_dist_opt server_verify verify_none\n")
        ),
        %% `+` emulator flags and a non-UTF-8 comment do not leak into the words.
        ?_assertEqual(
            #{
                ssl_dist_opt => ["a"],
                ssl_dist_optfile => ["f"],
                proto_dist => ["inet_tls"],
                problems => []
            },
            Flags(
                <<"# caf", 16#E9,
                    "\n+K true\n-ssl_dist_opt a\n+A30\n-ssl_dist_optfile f\n+sbt\n-proto_dist inet_tls\n">>
            )
        ),
        ?_assertEqual(
            #{
                ssl_dist_opt => ["a", "b", "c d"],
                ssl_dist_optfile => ["/p q/o.conf"],
                proto_dist => ["inet_tls"],
                problems => []
            },
            Flags(
                "# -ssl_dist_opt commented\n-ssl_dist_opt a b\n-detached\n"
                "-ssl_dist_opt 'c d' -ssl_dist_optfile \"/p q/o.conf\"\n"
                "-proto_dist inet_tls\n-extra -proto_dist other\n"
            )
        ),
        %% A `+` flag between two -ssl_dist_opt groups must not shift their pairs.
        ?_assertMatch(
            #{ssl_dist_opt := ["server_verify", "verify_peer", "client_verify", "verify_none"]},
            Flags(
                "-ssl_dist_opt server_verify verify_peer\n+A30\n"
                "-ssl_dist_opt client_verify verify_none\n"
            )
        ),
        %% erl's own reading: `#` ends a word, `\\` escapes the next character.
        ?_assertMatch(
            #{ssl_dist_opt := ["a", "b c"], ssl_dist_optfile := ["f"]},
            Flags("-ssl_dist_opt a b\\ c#x\n-ssl_dist_optfile f # note\n")
        ),
        %% A nested argument file is reported, for the launcher to refuse.
        ?_assertMatch(
            #{problems := ["sets -args_file more.args" ++ _]}, Flags("-args_file more.args\n")
        ),
        %% Words the check cannot place exactly are refused, not guessed.
        ?_assertMatch(
            #{problems := ["has a ' or \" quote" ++ _]},
            Flags("-env OWNER O'Brien\n-ssl_dist_opt server_verify verify_none\n")
        ),
        ?_assertMatch(
            #{problems := ["has a flag that erlexec handles itself" ++ _]},
            Flags(
                "-ssl_dist_opt server_verify verify_peer +K true server_fail_if_no_peer_cert false\n"
            )
        ),
        %% `-env` and the other erlexec-handled flags are refused the same way.
        [
            ?_assertMatch(
                #{problems := ["has a flag that erlexec handles itself" ++ _]},
                Flags(
                    "-ssl_dist_opt server_verify verify_peer " ++ F ++
                        " X Y client_verify verify_none\n"
                )
            )
         || F <- ["-env", "-detached", "-emu_args", "-keep_window", "-emu_type"]
        ],
        ?_assertMatch(
            #{problems := []},
            Flags("-ssl_dist_opt a b\n+A30\n-sname x\n+K true\n-ssl_dist_opt c d\n")
        )
    ].

%% BT-5260: the reader agrees with erl's own. For each file, the words erl hands
%% `init:get_argument/1` for the three distribution flags equal ours. Skipped
%% when no `erl` is on the PATH.
vm_args_matches_erl_test_() ->
    case os:find_executable("erl") of
        false ->
            [];
        Erl ->
            {setup, fun() -> beamtalk_test_fs:tmp_dir("bt5260_vmargs") end,
                fun beamtalk_test_fs:rm_rf/1, fun(Dir) ->
                    Cases = [
                        "-mode interactive\n-sname stray\n-ssl_dist_opt a b\n",
                        "-ssl_dist_opt a b\n+A30\n-ssl_dist_opt c d\n-proto_dist inet_tls\n",
                        "-ssl_dist_opt 'a b' \"c d\" e#f\n# -ssl_dist_opt hidden\n-proto_dist x\n",
                        "-ssl_dist_opt a\\ b c # trailing\n-ssl_dist_optfile /p\n",
                        "-foo x y#don't\n-ssl_dist_opt server_verify verify_none\n",
                        "-ssl_dist_opt a b\n-extra -proto_dist other\n"
                    ],
                    [
                        ?_assertEqual(
                            erl_dist_words(Erl, Dir, N, Text),
                            ours_dist_words(Text)
                        )
                     || {N, Text} <- lists:enumerate(Cases)
                    ] ++
                        %% Why the reader refuses these: erl hands the words after an
                        %% erlexec-handled flag to the flag before it.
                        [
                            ?_assertEqual(
                                [
                                    {ssl_dist_opt, ["a", "b"]},
                                    {ssl_dist_optfile, []},
                                    {proto_dist, []}
                                ],
                                erl_dist_words(Erl, Dir, 100 + N, "-ssl_dist_opt a " ++ F ++ " b\n")
                            )
                         || {N, F} <- lists:enumerate(["-emu_args", "-keep_window", "+A30"])
                        ]
                end}
    end.

%% Reads the port until the marked answer arrives, then closes it. erlexec may
%% write around the answer: `-emu_args` prints its `Executing: ...` line, which
%% a Windows erl flushes into the output (a POSIX erl loses it at exec), and
%% `-keep_window` makes a Windows erl wait for a key after `halt()`. A newline
%% on erl's stdin, then closing it with the port, ends that wait (BT-4775).
collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            Out = iolist_to_binary([Acc, Data]),
            case marked_answer(Out) of
                {ok, Answer} ->
                    end_key_wait(Port),
                    Answer;
                more ->
                    collect_port(Port, Out)
            end;
        {Port, {exit_status, Status}} ->
            error({erl_exited_without_answer, Status, Acc})
    after 30000 -> error({erl_timeout, Acc})
    end.

%% The port may already be closed if erl exited first; either way it is done.
end_key_wait(Port) ->
    try
        port_command(Port, <<"\n">>),
        port_close(Port)
    catch
        error:badarg -> ok
    end,
    ok.

marked_answer(Out) ->
    case binary:split(Out, ?ERL_ANSWER_MARK, [global]) of
        [_Before, Answer, _After | _] -> {ok, unicode:characters_to_list(Answer)};
        _ -> more
    end.

%% erl's answer is read between two marks, so text erlexec prints around it is
%% not parsed as part of the term.
marked_answer_test_() ->
    Mark = binary_to_list(?ERL_ANSWER_MARK),
    [
        ?_assertEqual({ok, "[a]."}, marked_answer(list_to_binary(Mark ++ "[a]." ++ Mark))),
        ?_assertEqual(
            {ok, "[a]."},
            marked_answer(
                list_to_binary(
                    "Executing: C:\\erl\\erts\\bin\\erl.exe -noshell\r\n\r\n" ++
                        Mark ++ "[a]." ++ Mark ++ "\r\nPress any key to close window.\r\n"
                )
            )
        ),
        %% `-emu_args` echoes the `-eval` text, which holds the mark only split.
        ?_assertEqual(
            {ok, "[a]."},
            marked_answer(
                list_to_binary(
                    "Executing: erl.exe -eval M = \"" ++ lists:sublist(Mark, 4) ++ "\" ++ \"" ++
                        lists:nthtail(4, Mark) ++ "\", io:format(\"~s~w.~s\", [M, x, M])\r\n" ++
                        Mark ++ "[a]." ++ Mark
                )
            )
        ),
        ?_assertEqual(more, marked_answer(list_to_binary(Mark ++ "[a")))
    ].

%% A Windows erl run with `-keep_window` prints its answer, then waits on stdin
%% for a key. A stand-in that does the same returns its answer at once rather
%% than timing out. Needs `/bin/sh`, so runs on POSIX hosts; the Windows run of
%% vm_args_matches_erl_test_ covers the real erl.
collect_port_ends_a_wait_for_a_key_test_() ->
    case {os:type(), os:find_executable("sh")} of
        {{unix, _}, Sh} when is_list(Sh) ->
            {setup, fun() -> beamtalk_test_fs:tmp_dir("bt4775_keep_window") end,
                fun beamtalk_test_fs:rm_rf/1, fun(Dir) ->
                    ?_test(begin
                        Mark = binary_to_list(?ERL_ANSWER_MARK),
                        Script = filename:join(Dir, "erl_keep_window.sh"),
                        ok = file:write_file(
                            Script,
                            "printf '%s[a].%s\\nPress any key to close window.\\n' '" ++
                                Mark ++ "' '" ++ Mark ++ "'\nread key\n"
                        ),
                        Port = open_port(
                            {spawn_executable, Sh},
                            [{args, [Script]}, exit_status, stderr_to_stdout]
                        ),
                        ?assertEqual("[a].", collect_port(Port, []))
                    end)
                end};
        _ ->
            []
    end.

ours_dist_words(Text) ->
    Ours = beamtalk_release_launcher:vm_args_dist_flags(Text),
    [{K, maps:get(K, Ours)} || K <- [ssl_dist_opt, ssl_dist_optfile, proto_dist]].

%% What a real `erl -args_file` VM answers for the same three flags.
erl_dist_words(Erl, Dir, N, Text) ->
    File = filename:join(Dir, "t" ++ integer_to_list(N) ++ ".args"),
    ok = file:write_file(File, Text),
    %% The mark is built when erl runs, so the command line (which `-emu_args`
    %% echoes) never holds it whole.
    {MarkHead, MarkTail} = lists:split(4, binary_to_list(?ERL_ANSWER_MARK)),
    Eval =
        "M = \"" ++ MarkHead ++ "\" ++ \"" ++ MarkTail ++
            "\", io:format(\"~s~w.~s\", [M, [{K, case init:get_argument(K) of "
            "{ok, L} -> lists:append(L); error -> [] end} || "
            "K <- [ssl_dist_opt, ssl_dist_optfile, proto_dist]], M]), halt().",
    Port = open_port(
        {spawn_executable, Erl},
        [{args, ["-noshell", "-args_file", File, "-eval", Eval]}, exit_status, stderr_to_stdout]
    ),
    Out = collect_port(Port, []),
    {ok, Tokens, _} = erl_scan:string(Out),
    {ok, Words} = erl_parse:parse_term(Tokens),
    Words.

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

%% Naming 0.0.0.0 in ERL_EPMD_ADDRESS makes a fresh epmd exit (it then also
%% binds loopback on the same port), so a wildcard interface leaves epmd at
%% its default instead.
epmd_address_test_() ->
    [
        ?_assertEqual("all", beamtalk_release_launcher:epmd_address({0, 0, 0, 0})),
        ?_assertEqual("all", beamtalk_release_launcher:epmd_address({0, 0, 0, 0, 0, 0, 0, 0})),
        ?_assertEqual("10.0.0.5", beamtalk_release_launcher:epmd_address({10, 0, 0, 5})),
        ?_assertEqual(
            "fd00::5", beamtalk_release_launcher:epmd_address({16#fd00, 0, 0, 0, 0, 0, 0, 5})
        )
    ].

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
