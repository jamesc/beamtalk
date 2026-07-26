%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Runtime Context

-module(beamtalk_version_tests).

-moduledoc """
EUnit tests for beamtalk_version module.

Tests the version-report shape used as the RPC target for the
desktop-attach readiness handshake (ADR 0097, BT-2991).
""".

-include_lib("eunit/include/eunit.hrl").

get_returns_map_test() ->
    ?assert(is_map(beamtalk_version:get())).

get_returns_expected_keys_test() ->
    Report = beamtalk_version:get(),
    ?assertEqual(
        [erts_version, otp_release, protocol_version, runtime_version],
        lists:sort(maps:keys(Report))
    ).

%% Not a cross-module guarantee: beamtalk_runtime cannot depend on
%% beamtalk_workspace (dependencies flow down only), so this only pins the
%% literal in beamtalk_version itself. Keeping the two literals in sync when
%% the wire protocol changes is a manual step (see the module doc).
get_protocol_version_is_expected_literal_test() ->
    #{protocol_version := Vsn} = beamtalk_version:get(),
    ?assertEqual(<<"2.0">>, Vsn).

get_otp_release_returns_non_empty_binary_test() ->
    #{otp_release := Release} = beamtalk_version:get(),
    ?assert(is_binary(Release)),
    ?assert(byte_size(Release) > 0).

get_erts_version_returns_non_empty_binary_test() ->
    #{erts_version := ErtsVsn} = beamtalk_version:get(),
    ?assert(is_binary(ErtsVsn)),
    ?assert(byte_size(ErtsVsn) > 0).

%%% ============================================================================
%%% runtime_version fallback (mirrors beamtalk_interface_tests:version_*)
%%%
%%% Under EUnit the beamtalk_runtime app is typically not started, so
%%% runtime_version takes the <<"unknown">> fallback. Either way it must
%%% return a binary.
%%% ============================================================================

get_runtime_version_returns_binary_test() ->
    #{runtime_version := Vsn} = beamtalk_version:get(),
    ?assert(is_binary(Vsn)).

get_runtime_version_matches_app_vsn_when_loaded_test() ->
    %% Load (not start) the runtime app so application:get_key/2 returns
    %% {ok, Vsn}, driving the {ok, Vsn} -> list_to_binary(Vsn) branch.
    _ = application:load(beamtalk_runtime),
    #{runtime_version := Vsn} = beamtalk_version:get(),
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, AppVsn} -> ?assertEqual(list_to_binary(AppVsn), Vsn);
        _ -> ?assertEqual(<<"unknown">>, Vsn)
    end.
