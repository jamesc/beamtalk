%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_digest).

%%% **DDD Context:** Object System Context

-moduledoc """
Digest class implementation — cryptographic hashing and HMAC via `crypto`.

Thin wrapper over `crypto:hash/2` (SHA-256, SHA-512, MD5) and `crypto:mac/4`
(HMAC-SHA256, HMAC-SHA512). Every function accepts a String or Binary input
(both are raw Erlang binaries at the BEAM level) and returns a raw Binary
digest — callers convert to hex/base64 via `beamtalk_binary`.

## Class Methods

| Selector             | Description                                  |
|-----------------------|----------------------------------------------|
| `sha256:`             | SHA-256 digest (32 bytes)                    |
| `sha512:`             | SHA-512 digest (64 bytes)                    |
| `md5:`                | MD5 digest (16 bytes) — legacy/non-crypto use |
| `hmacSha256:key:`     | HMAC-SHA256 MAC (32 bytes)                   |
| `hmacSha512:key:`     | HMAC-SHA512 MAC (64 bytes)                   |
""".

%% Class methods — quoted colon-suffixed names are the real implementations.
-export(['sha256:'/1, 'sha512:'/1, 'md5:'/1, 'hmacSha256:key:'/2, 'hmacSha512:key:'/2]).

%% FFI shims for `(Erlang beamtalk_digest) sel: args` dispatch —
%% selector_to_function/1 strips the first keyword's colon (see
%% docs/beamtalk-native-erlang.md "The naming rule").
-export([sha256/1, sha512/1, md5/1, hmacSha256/2, hmacSha512/2]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "SHA-256 digest (32 bytes) of Input.".
-spec 'sha256:'(binary()) -> binary().
'sha256:'(Input) when is_binary(Input) ->
    crypto:hash(sha256, Input);
'sha256:'(_) ->
    raise_type_error('sha256:', <<"Argument must be a String or Binary">>).

-doc "SHA-512 digest (64 bytes) of Input.".
-spec 'sha512:'(binary()) -> binary().
'sha512:'(Input) when is_binary(Input) ->
    crypto:hash(sha512, Input);
'sha512:'(_) ->
    raise_type_error('sha512:', <<"Argument must be a String or Binary">>).

-doc """
MD5 digest (16 bytes) of Input.

Legacy/non-cryptographic use only — MD5 is not collision-resistant. Do not
use for signatures, password storage, or integrity checks on untrusted data.
""".
-spec 'md5:'(binary()) -> binary().
'md5:'(Input) when is_binary(Input) ->
    crypto:hash(md5, Input);
'md5:'(_) ->
    raise_type_error('md5:', <<"Argument must be a String or Binary">>).

-doc "HMAC-SHA256 message authentication code (32 bytes) of Input using Key.".
-spec 'hmacSha256:key:'(binary(), binary()) -> binary().
'hmacSha256:key:'(Input, Key) when is_binary(Input), is_binary(Key) ->
    crypto:mac(hmac, sha256, Key, Input);
'hmacSha256:key:'(Input, _Key) when is_binary(Input) ->
    raise_type_error('hmacSha256:key:', <<"Key must be a String or Binary">>);
'hmacSha256:key:'(_Input, _Key) ->
    raise_type_error('hmacSha256:key:', <<"Argument must be a String or Binary">>).

-doc "HMAC-SHA512 message authentication code (64 bytes) of Input using Key.".
-spec 'hmacSha512:key:'(binary(), binary()) -> binary().
'hmacSha512:key:'(Input, Key) when is_binary(Input), is_binary(Key) ->
    crypto:mac(hmac, sha512, Key, Input);
'hmacSha512:key:'(Input, _Key) when is_binary(Input) ->
    raise_type_error('hmacSha512:key:', <<"Key must be a String or Binary">>);
'hmacSha512:key:'(_Input, _Key) ->
    raise_type_error('hmacSha512:key:', <<"Argument must be a String or Binary">>).

%%% ============================================================================
%%% FFI Shims — (Erlang beamtalk_digest) dispatch
%%% ============================================================================

-doc "FFI alias for sha256:/1.".
-spec sha256(binary()) -> binary().
sha256(Input) -> 'sha256:'(Input).

-doc "FFI alias for sha512:/1.".
-spec sha512(binary()) -> binary().
sha512(Input) -> 'sha512:'(Input).

-doc "FFI alias for md5:/1.".
-spec md5(binary()) -> binary().
md5(Input) -> 'md5:'(Input).

-doc "FFI alias for hmacSha256:key:/2.".
-spec hmacSha256(binary(), binary()) -> binary().
hmacSha256(Input, Key) -> 'hmacSha256:key:'(Input, Key).

-doc "FFI alias for hmacSha512:key:/2.".
-spec hmacSha512(binary(), binary()) -> binary().
hmacSha512(Input, Key) -> 'hmacSha512:key:'(Input, Key).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc "Raise a Digest type_error by delegating to beamtalk_error:raise_type_error/3.".
-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    beamtalk_error:raise_type_error('Digest', Selector, Hint).
