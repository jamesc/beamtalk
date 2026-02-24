%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%% **DDD Context:** Runtime

%%% @doc EUnit tests for beamtalk_platform module.
%%%
%%% Tests platform-specific helpers: home directory, OS detection, and path utilities.

-module(beamtalk_platform_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test that home_dir/0 returns expected value based on environment
home_dir_returns_string_test() ->
    Result = beamtalk_platform:home_dir(),
    Home = os:getenv("HOME"),
    UserProfile = os:getenv("USERPROFILE"),
    case Home of
        false ->
            case UserProfile of
                false ->
                    ?assertEqual(false, Result);
                _UP ->
                    ?assertEqual(UserProfile, Result)
            end;
        "" ->
            %% Empty HOME treated as unset, should fall back
            case UserProfile of
                false ->
                    ?assertEqual(false, Result);
                "" ->
                    ?assertEqual(false, Result);
                _UP ->
                    ?assertEqual(UserProfile, Result)
            end;
        _Home ->
            ?assertEqual(Home, Result),
            ?assert(is_list(Result)),
            ?assert(length(Result) > 0)
    end.

%% Test fallback to USERPROFILE when HOME is unset
home_dir_falls_back_to_userprofile_test() ->
    OrigHome = os:getenv("HOME"),
    OrigUserProfile = os:getenv("USERPROFILE"),
    os:unsetenv("HOME"),
    os:putenv("USERPROFILE", "/mock/user/profile"),
    try
        ?assertEqual("/mock/user/profile", beamtalk_platform:home_dir())
    after
        %% Restore original state
        case OrigUserProfile of
            false -> os:unsetenv("USERPROFILE");
            UP -> os:putenv("USERPROFILE", UP)
        end,
        case OrigHome of
            false -> ok;
            Val -> os:putenv("HOME", Val)
        end
    end.

%% Test returns false when neither HOME nor USERPROFILE is set
home_dir_returns_false_when_no_env_test() ->
    OrigHome = os:getenv("HOME"),
    OrigUserProfile = os:getenv("USERPROFILE"),
    os:unsetenv("HOME"),
    os:unsetenv("USERPROFILE"),
    try
        ?assertEqual(false, beamtalk_platform:home_dir())
    after
        %% Restore original state
        case OrigUserProfile of
            false -> ok;
            UP -> os:putenv("USERPROFILE", UP)
        end,
        case OrigHome of
            false -> ok;
            H -> os:putenv("HOME", H)
        end
    end.
