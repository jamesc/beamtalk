%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_logger module.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests info:/1, info:metadata:/2, warn:/1, warn:metadata:/2,
%%% error:/1, error:metadata:/2, debug:/1, debug:metadata:/2,
%%% setLevel:/1, FFI shims, and type error paths.

-module(beamtalk_logger_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% info:/1
%%% ============================================================================

info_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'info:'(<<"hello">>)).

info_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'info:'(42)
    ).

%%% ============================================================================
%%% info:metadata:/2
%%% ============================================================================

info_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'info:metadata:'(<<"hello">>, #{key => val})).

info_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'info:metadata:'(42, #{})
    ).

info_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'info:metadata:'(<<"hello">>, not_a_map)
    ).

%%% ============================================================================
%%% warn:/1
%%% ============================================================================

warn_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'warn:'(<<"caution">>)).

warn_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'warn:'(123)
    ).

%%% ============================================================================
%%% warn:metadata:/2
%%% ============================================================================

warn_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'warn:metadata:'(<<"caution">>, #{a => 1})).

warn_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'warn:metadata:'(123, #{})
    ).

warn_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'warn:metadata:'(<<"caution">>, not_a_map)
    ).

%%% ============================================================================
%%% error:/1
%%% ============================================================================

error_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'error:'(<<"failure">>)).

error_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'error:'(false)
    ).

%%% ============================================================================
%%% error:metadata:/2
%%% ============================================================================

error_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'error:metadata:'(<<"failure">>, #{reason => timeout})).

error_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'error:metadata:'(false, #{})
    ).

error_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'error:metadata:'(<<"failure">>, not_a_map)
    ).

%%% ============================================================================
%%% debug:/1
%%% ============================================================================

debug_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'debug:'(<<"trace">>)).

debug_non_binary_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'debug:'([not_binary])
    ).

%%% ============================================================================
%%% debug:metadata:/2
%%% ============================================================================

debug_metadata_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_logger:'debug:metadata:'(<<"trace">>, #{count => 1})).

debug_metadata_non_binary_msg_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'debug:metadata:'([not_binary], #{})
    ).

debug_metadata_non_map_meta_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'debug:metadata:'(<<"trace">>, not_a_map)
    ).

%%% ============================================================================
%%% setLevel:/1
%%% ============================================================================

set_level_valid_level_test() ->
    %% Save original level, set to debug, then restore
    #{level := OrigLevel} = logger:get_primary_config(),
    try
        ?assertEqual(nil, beamtalk_logger:'setLevel:'(debug)),
        #{level := debug} = logger:get_primary_config()
    after
        logger:set_primary_config(level, OrigLevel)
    end.

set_level_invalid_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = argument_error}},
        beamtalk_logger:'setLevel:'(not_a_real_level)
    ).

set_level_non_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_logger:'setLevel:'(<<"debug">>)
    ).

%%% ============================================================================
%%% FFI Shims
%%% ============================================================================

info_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:info(<<"shim info">>)).

info_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:info(<<"shim info">>, #{k => v})).

warn_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:warn(<<"shim warn">>)).

warn_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:warn(<<"shim warn">>, #{k => v})).

error_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:error(<<"shim error">>)).

error_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:error(<<"shim error">>, #{k => v})).

debug_shim_test() ->
    ?assertEqual(nil, beamtalk_logger:debug(<<"shim debug">>)).

debug_shim_metadata_test() ->
    ?assertEqual(nil, beamtalk_logger:debug(<<"shim debug">>, #{k => v})).

set_level_shim_test() ->
    #{level := OrigLevel} = logger:get_primary_config(),
    try
        ?assertEqual(nil, beamtalk_logger:setLevel(info)),
        #{level := info} = logger:get_primary_config()
    after
        logger:set_primary_config(level, OrigLevel)
    end.
