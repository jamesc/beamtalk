%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_digest_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_digest module.

Tests sha256:, sha512:, md5:, hmacSha256:key:, hmacSha512:key:, their FFI
shim aliases, and the type_error paths for non-binary inputs and keys.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% sha256:/1
%%% ============================================================================

sha256_returns_32_bytes_test() ->
    Result = beamtalk_digest:'sha256:'(<<"hello">>),
    ?assert(is_binary(Result)),
    ?assertEqual(32, byte_size(Result)).

sha256_known_value_test() ->
    %% SHA-256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    Expected = <<
        16#e3,
        16#b0,
        16#c4,
        16#42,
        16#98,
        16#fc,
        16#1c,
        16#14,
        16#9a,
        16#fb,
        16#f4,
        16#c8,
        16#99,
        16#6f,
        16#b9,
        16#24,
        16#27,
        16#ae,
        16#41,
        16#e4,
        16#64,
        16#9b,
        16#93,
        16#4c,
        16#a4,
        16#95,
        16#99,
        16#1b,
        16#78,
        16#52,
        16#b8,
        16#55
    >>,
    ?assertEqual(Expected, beamtalk_digest:'sha256:'(<<>>)).

sha256_deterministic_test() ->
    Input = <<"beamtalk">>,
    ?assertEqual(
        beamtalk_digest:'sha256:'(Input),
        beamtalk_digest:'sha256:'(Input)
    ).

sha256_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'sha256:'(not_a_binary)
    ).

%%% ============================================================================
%%% sha512:/1
%%% ============================================================================

sha512_returns_64_bytes_test() ->
    Result = beamtalk_digest:'sha512:'(<<"hello">>),
    ?assert(is_binary(Result)),
    ?assertEqual(64, byte_size(Result)).

sha512_known_value_test() ->
    %% SHA-512("") = cf83e1357eefb8bdf1542850d66d8007...
    Result = beamtalk_digest:'sha512:'(<<>>),
    ?assertEqual(64, byte_size(Result)),
    %% Verify first byte to confirm the right algorithm
    ?assertEqual(16#cf, binary:at(Result, 0)).

sha512_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'sha512:'(42)
    ).

sha512_type_error_atom_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'sha512:'(hello)
    ).

%%% ============================================================================
%%% md5:/1
%%% ============================================================================

md5_returns_16_bytes_test() ->
    Result = beamtalk_digest:'md5:'(<<"hello">>),
    ?assert(is_binary(Result)),
    ?assertEqual(16, byte_size(Result)).

md5_known_value_test() ->
    %% MD5("") = d41d8cd98f00b204e9800998ecf8427e
    Expected = <<
        16#d4,
        16#1d,
        16#8c,
        16#d9,
        16#8f,
        16#00,
        16#b2,
        16#04,
        16#e9,
        16#80,
        16#09,
        16#98,
        16#ec,
        16#f8,
        16#42,
        16#7e
    >>,
    ?assertEqual(Expected, beamtalk_digest:'md5:'(<<>>)).

md5_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'md5:'([1, 2, 3])
    ).

%%% ============================================================================
%%% hmacSha256:key:/2
%%% ============================================================================

hmac_sha256_returns_32_bytes_test() ->
    Result = beamtalk_digest:'hmacSha256:key:'(<<"message">>, <<"key">>),
    ?assert(is_binary(Result)),
    ?assertEqual(32, byte_size(Result)).

hmac_sha256_deterministic_test() ->
    Input = <<"data">>,
    Key = <<"secret">>,
    ?assertEqual(
        beamtalk_digest:'hmacSha256:key:'(Input, Key),
        beamtalk_digest:'hmacSha256:key:'(Input, Key)
    ).

hmac_sha256_type_error_input_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'hmacSha256:key:'(not_a_binary, <<"key">>)
    ).

hmac_sha256_type_error_key_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'hmacSha256:key:'(<<"input">>, not_a_binary)
    ).

%%% ============================================================================
%%% hmacSha512:key:/2
%%% ============================================================================

hmac_sha512_returns_64_bytes_test() ->
    Result = beamtalk_digest:'hmacSha512:key:'(<<"message">>, <<"key">>),
    ?assert(is_binary(Result)),
    ?assertEqual(64, byte_size(Result)).

hmac_sha512_type_error_input_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'hmacSha512:key:'(42, <<"key">>)
    ).

hmac_sha512_type_error_key_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_digest:'hmacSha512:key:'(<<"input">>, 42)
    ).

%%% ============================================================================
%%% FFI shims — verify delegation
%%% ============================================================================

sha256_ffi_shim_delegates_test() ->
    Input = <<"shim-test">>,
    ?assertEqual(
        beamtalk_digest:'sha256:'(Input),
        beamtalk_digest:sha256(Input)
    ).

sha512_ffi_shim_delegates_test() ->
    Input = <<"shim-test">>,
    ?assertEqual(
        beamtalk_digest:'sha512:'(Input),
        beamtalk_digest:sha512(Input)
    ).

md5_ffi_shim_delegates_test() ->
    Input = <<"shim-test">>,
    ?assertEqual(
        beamtalk_digest:'md5:'(Input),
        beamtalk_digest:md5(Input)
    ).

hmac_sha256_ffi_shim_delegates_test() ->
    Input = <<"data">>,
    Key = <<"key">>,
    ?assertEqual(
        beamtalk_digest:'hmacSha256:key:'(Input, Key),
        beamtalk_digest:hmacSha256(Input, Key)
    ).

hmac_sha512_ffi_shim_delegates_test() ->
    Input = <<"data">>,
    Key = <<"key">>,
    ?assertEqual(
        beamtalk_digest:'hmacSha512:key:'(Input, Key),
        beamtalk_digest:hmacSha512(Input, Key)
    ).
