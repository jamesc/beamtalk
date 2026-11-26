%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_paths_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc "The shared project-path policy (BT-5205).".

-include_lib("eunit/include/eunit.hrl").

accepts_project_relative_paths_test_() ->
    [
        ?_assertEqual(ok, beamtalk_repl_paths:check_relative(Path))
     || Path <- [
            <<"src/Counter.bt">>,
            <<"Counter.bt">>,
            <<"a b/c d.bt">>,
            <<"src/.hidden">>,
            <<"src/..foo/bar">>,
            <<"src/a..b">>,
            <<"./src/x.bt">>,
            <<"é/ü.bt"/utf8>>
        ]
    ].

refuses_absolute_paths_test_() ->
    [
        ?_assertEqual({error, absolute}, beamtalk_repl_paths:check_relative(Path))
     || Path <- [
            <<"/etc/passwd">>,
            <<"/">>,
            <<"\\windows">>,
            <<"\\\\server\\share\\x">>,
            <<"C:\\x.bt">>,
            <<"c:/x.bt">>,
            <<"Z:x.bt">>
        ]
    ].

refuses_parent_segments_test_() ->
    [
        ?_assertEqual({error, parent_segment}, beamtalk_repl_paths:check_relative(Path))
     || Path <- [
            <<"..">>,
            <<"../x">>,
            <<"a/../b">>,
            <<"a/..">>,
            <<"a\\..\\b">>,
            <<"a/..\\b">>,
            <<"/../x">>
        ]
    ].

refuses_empty_nul_and_non_strings_test_() ->
    [
        ?_assertEqual({error, empty}, beamtalk_repl_paths:check_relative(<<>>)),
        ?_assertEqual({error, nul}, beamtalk_repl_paths:check_relative(<<"a", 0, "b">>)),
        ?_assertEqual({error, not_a_string}, beamtalk_repl_paths:check_relative("src/x")),
        ?_assertEqual({error, not_a_string}, beamtalk_repl_paths:check_relative(undefined)),
        ?_assertEqual({error, not_a_string}, beamtalk_repl_paths:check_relative(7))
    ].
