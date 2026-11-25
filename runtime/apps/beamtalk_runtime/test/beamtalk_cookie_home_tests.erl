%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_cookie_home_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit coverage for `beamtalk_cookie_home:restore/0` (BT-5185): the private
boot home is undone and removed, the home variables come back as they were.
""".

-include_lib("eunit/include/eunit.hrl").

%% `with_tmp_home/1` points HOME at a fresh directory, standing in for the
%% private cookie home the launcher made, and restores the real HOME after.
restore_puts_home_back_and_removes_the_cookie_home_test() ->
    beamtalk_test_fs:with_tmp_home(fun(Private) ->
        Cookie = filename:join(Private, ".erlang.cookie"),
        ok = file:write_file(Cookie, <<"secret">>),
        true = os:putenv("BEAMTALK_COOKIE_HOME", Private),
        true = os:putenv("BEAMTALK_ORIG_HOME", "set:/original/home"),
        true = os:putenv("BEAMTALK_ORIG_HOMEDRIVE", "unset"),
        true = os:putenv("HOMEDRIVE", "Z:"),
        try
            ?assertEqual(restored, beamtalk_cookie_home:restore()),
            ?assertEqual("/original/home", os:getenv("HOME")),
            ?assertEqual(false, os:getenv("HOMEDRIVE")),
            ?assertEqual(false, os:getenv("BEAMTALK_COOKIE_HOME")),
            ?assertEqual(false, os:getenv("BEAMTALK_ORIG_HOME")),
            ?assertEqual(false, os:getenv("BEAMTALK_ORIG_HOMEDRIVE")),
            ?assertNot(filelib:is_file(Cookie)),
            ?assertNot(filelib:is_dir(Private)),
            %% Idempotent: nothing left to do.
            ?assertEqual(none, beamtalk_cookie_home:restore()),
            ?assertEqual("/original/home", os:getenv("HOME"))
        after
            os:unsetenv("HOMEDRIVE"),
            os:unsetenv("BEAMTALK_COOKIE_HOME")
        end
    end).

%% Only the cookie file and an empty directory are removed: anything else in
%% the directory named by the variable survives.
restore_never_removes_other_files_test() ->
    Dir = beamtalk_test_fs:tmp_dir("cookie_home"),
    Other = filename:join(Dir, "keep.txt"),
    ok = file:write_file(Other, <<"x">>),
    true = os:putenv("BEAMTALK_COOKIE_HOME", Dir),
    try
        ?assertEqual(restored, beamtalk_cookie_home:restore()),
        ?assert(filelib:is_file(Other))
    after
        os:unsetenv("BEAMTALK_COOKIE_HOME"),
        beamtalk_test_fs:rm_rf(Dir)
    end.

no_private_home_is_none_test() ->
    true = os:unsetenv("BEAMTALK_COOKIE_HOME"),
    ?assertEqual(none, beamtalk_cookie_home:restore()).
