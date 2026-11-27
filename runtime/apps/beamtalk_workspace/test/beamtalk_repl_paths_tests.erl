%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_paths_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc "The shared project-path policy (BT-5205).".

-include_lib("eunit/include/eunit.hrl").

-define(POSIX, {unix, linux}).
-define(WINDOWS, {win32, nt}).

accepts_project_relative_paths_test_() ->
    [
        ?_assertEqual(ok, beamtalk_repl_paths:check_relative(Path, OsType))
     || Path <- [
            <<"src/Counter.bt">>,
            <<"Counter.bt">>,
            <<"a b/c d.bt">>,
            <<"src/.hidden">>,
            <<"src/..foo/bar">>,
            <<"src/a..b">>,
            <<"./src/x.bt">>,
            <<"é/ü.bt"/utf8>>,
            %% A colon past the first segment names no drive on any OS.
            <<"src/C:/x">>
        ],
        OsType <- [?POSIX, ?WINDOWS]
    ].

%% `Z:x.bt` is an ordinary relative file name on a POSIX node. A Windows node
%% reads it as "x.bt in drive Z's current directory", which can name anything
%% outside the project, so it refuses it (BT-4775).
drive_relative_names_depend_on_the_os_test_() ->
    [
        [
            ?_assertEqual(ok, beamtalk_repl_paths:check_relative(Path, ?POSIX)),
            ?_assertEqual({error, absolute}, beamtalk_repl_paths:check_relative(Path, ?WINDOWS))
        ]
     || Path <- [<<"Z:x.bt">>, <<"a:notes.bt">>, <<"1:x.bt">>, <<"_:x">>]
    ].

%% check_relative/1 reads a path as this node's OS does, whichever OS runs the
%% test, over every first byte before `:`: each path `filename:pathtype/1` calls
%% non-relative here is refused, and on a Windows host nothing else is (a POSIX
%% host also refuses `C:` and `C:/`, which name a drive on any client).
agrees_with_the_host_pathtype_test_() ->
    Win32 = element(1, os:type()) =:= win32,
    Paths =
        [<<"src/Counter.bt">>, <<"src/C:/x">>] ++
            [
                <<B, Suffix/binary>>
             || B <- lists:seq(1, 255), Suffix <- [<<":">>, <<":x">>, <<":/">>, <<":\\">>]
            ],
    [
        ?_test(begin
            HostAbsolute = filename:pathtype(Path) =/= relative,
            Refused = beamtalk_repl_paths:check_relative(Path) =:= {error, absolute},
            case Win32 of
                true -> ?assertEqual({Path, HostAbsolute}, {Path, Refused});
                false -> ?assertEqual({Path, true}, {Path, Refused orelse not HostAbsolute})
            end
        end)
     || Path <- Paths
    ].

refuses_absolute_paths_test_() ->
    [
        ?_assertEqual({error, absolute}, beamtalk_repl_paths:check_relative(Path, OsType))
     || Path <- [
            <<"/etc/passwd">>,
            <<"/">>,
            <<"\\windows">>,
            <<"\\\\server\\share\\x">>,
            <<"C:\\x.bt">>,
            <<"c:/x.bt">>,
            <<"C:">>
        ],
        OsType <- [?POSIX, ?WINDOWS]
    ].

refuses_the_tree_root_test_() ->
    [
        ?_assertEqual({error, whole_tree}, beamtalk_repl_paths:check_relative(Path))
     || Path <- [<<".">>, <<"./">>, <<"./.">>, <<".\\">>, <<"././">>]
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
