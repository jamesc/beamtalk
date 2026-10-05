%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_platform_tests).

-moduledoc """
EUnit tests for beamtalk_platform module.

Tests platform-specific helpers: home directory, OS detection, and path utilities.
""".

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

%%====================================================================
%% BT-3680: shared ~/.beamtalk resolver
%%====================================================================

%% Run Fun with exactly the given env vars set (all others of
%% BEAMTALK_HOME/HOME/USERPROFILE unset), restoring afterwards.
with_env(Env, Fun) ->
    Vars = ["BEAMTALK_HOME", "HOME", "USERPROFILE"],
    Orig = [{V, os:getenv(V)} || V <- Vars],
    lists:foreach(fun(V) -> os:unsetenv(V) end, Vars),
    maps:foreach(fun(K, V) -> os:putenv(binary_to_list(K), binary_to_list(V)) end, Env),
    try
        Fun()
    after
        lists:foreach(
            fun
                ({V, false}) -> os:unsetenv(V);
                ({V, Val}) -> os:putenv(V, Val)
            end,
            Orig
        )
    end.

%% Shared Rust/Erlang conformance corpus:
%% `crates/beamtalk-workspace/tests/beamtalk_root_dir_conformance.rs` asserts
%% the same rows against `beamtalk_home::beamtalk_root_dir` /
%% `beamtalk_workspace::workspaces_base_dir`.
root_dir_conformance_matches_shared_corpus_test() ->
    Cases = beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_runtime",
        "test",
        "fixtures",
        "beamtalk_root_dir_conformance.json"
    ]),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(Case) ->
            Name = maps:get(<<"name">>, Case),
            with_env(maps:get(<<"env">>, Case), fun() ->
                ?assertEqual(
                    expected_path(maps:get(<<"root">>, Case)),
                    beamtalk_platform:beamtalk_root_dir(),
                    Name
                ),
                ?assertEqual(
                    expected_path(maps:get(<<"workspaces">>, Case)),
                    beamtalk_platform:workspaces_base_dir(),
                    Name
                )
            end)
        end,
        Cases
    ).

expected_path(null) -> undefined;
expected_path(Bin) -> binary_to_list(Bin).

workspace_dir_joins_id_test() ->
    with_env(#{<<"HOME">> => <<"/fx/home">>}, fun() ->
        ?assertEqual(
            "/fx/home/.beamtalk/workspaces/abc123",
            beamtalk_platform:workspace_dir(<<"abc123">>)
        )
    end),
    with_env(#{}, fun() ->
        ?assertEqual(undefined, beamtalk_platform:workspace_dir(<<"abc123">>))
    end).
