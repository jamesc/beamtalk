%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_uuid).

%%% **DDD Context:** Object System Context

-moduledoc """
Uuid class implementation — RFC 9562 UUIDs via `crypto`/`erlang`.

Provides class-side generators (`v4`, `v7`), parsing (`fromString:`),
validation (`isValid:`), and instance methods for conversion and comparison.

Uuid objects are represented as tagged maps:
```
#{
  '\$beamtalk_class' => 'Uuid',
  bytes => <<...>> :: <<_:128>>   %% the raw 16-byte UUID, big-endian
}
```

`bytes` must stay the map's only payload key: the 16-byte binary is the
value's entire identity — two Uuids with the same `bytes` are structurally
equal maps, so the inherited `=`/`hash` from Value (raw Erlang term
comparison / `erlang:phash2`) already do the right thing. Adding any other
key (e.g. a memoized string) would silently break that — both `=` and `hash`
would then also compare/hash the extra field. No `=:=`/`=/=`/`/=` overrides
are defined (ADR 0002: those operators are hard-lowered by the compiler to
raw term comparison regardless of any class-level method, so an override
would be dead code).
""".

%% Class methods
-export(['v4'/0, 'v7'/0, 'fromString:'/1, 'isValid:'/1]).

%% Instance methods
-export(['asString'/1, 'asBinary'/1, version/1, 'printString'/1]).
-export(['<'/2, '>'/2, '=<'/2, '>='/2]).

%% FFI shims for (Erlang beamtalk_uuid) dispatch — also the functions
%% `self delegate` actually calls for keyword selectors (first keyword,
%% colon stripped; see docs/beamtalk-native-erlang.md "The naming rule").
-export([fromString/1, isValid/1]).
-export([lt/2, gt/2, lte/2, gte/2]).

-type t() :: #{'$beamtalk_class' := 'Uuid', bytes := <<_:128>>}.
-export_type([t/0]).

%% \z (not $) anchors strictly at the end of the subject — plain $ in PCRE
%% also matches immediately before a trailing "\n", which would wrongly let
%% "<uuid>\n" (e.g. read from a file/HTTP line) parse as valid.
-define(UUID_STRING_RE, <<
    "^([0-9a-fA-F]{8})-([0-9a-fA-F]{4})-([0-9a-fA-F]{4})-"
    "([0-9a-fA-F]{4})-([0-9a-fA-F]{12})\\z"
>>).

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

-doc """
Generate a random (version 4) Uuid via `crypto:strong_rand_bytes/1`.

Sets the version nibble to 4 and the variant bits to `10` per RFC 9562;
every other bit is cryptographically random.
""".
-spec 'v4'() -> t().
'v4'() ->
    <<R1:48, _:4, R2:12, _:2, R3:62>> = crypto:strong_rand_bytes(16),
    make_uuid(<<R1:48, 4:4, R2:12, 2:2, R3:62>>).

-doc """
Generate a time-ordered (version 7) Uuid.

The high 48 bits are the current Unix time in milliseconds
(`erlang:system_time(millisecond)` — BEAM's time-corrected wall clock, matching
the convention used by `beamtalk_time`), followed by the version nibble (7),
12 random bits, the variant bits (`10`), then 62 more random bits — per
RFC 9562. There is no sub-millisecond counter (RFC 9562 §6.2 "Method 1" is not
used), so ordering is only guaranteed at millisecond granularity: Uuids from
different milliseconds sort chronologically, but Uuids generated within the
same millisecond sort in an unspecified (effectively random) relative order.
""".
-spec 'v7'() -> t().
'v7'() ->
    UnixTsMs = erlang:system_time(millisecond),
    <<RandA:12, RandB:62, _:54>> = crypto:strong_rand_bytes(16),
    make_uuid(<<UnixTsMs:48, 7:4, RandA:12, 2:2, RandB:62>>).

-doc """
Parse the canonical `8-4-4-4-12` hyphenated hex string form.

Accepts upper- or lower-case hex digits. Returns `Result ok: uuid` on
success, `Result error:` if `Str` isn't a syntactically valid Uuid string.
""".
-spec 'fromString:'(binary()) -> beamtalk_result:t().
'fromString:'(Str) when is_binary(Str) ->
    case parse_uuid_string(Str) of
        {ok, Bytes} ->
            beamtalk_result:from_tagged_tuple({ok, make_uuid(Bytes)});
        error ->
            Error0 = beamtalk_error:new(parse_error, 'Uuid'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromString:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<
                    "Expected canonical UUID format: "
                    "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (hex digits)"
                >>
            ),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'fromString:'(_) ->
    beamtalk_error:raise_type_error('Uuid', 'fromString:', <<"Argument must be a String">>).

-doc """
True if `Str` is a syntactically valid canonical Uuid string.

Checks format only (hex digits and hyphen placement) — does not require the
version/variant bits to be one this class would generate.
""".
-spec 'isValid:'(binary()) -> boolean().
'isValid:'(Str) when is_binary(Str) ->
    case parse_uuid_string(Str) of
        {ok, _Bytes} -> true;
        error -> false
    end;
'isValid:'(_) ->
    beamtalk_error:raise_type_error('Uuid', 'isValid:', <<"Argument must be a String">>).

%%% ============================================================================
%%% Instance Methods — Conversion
%%% ============================================================================

-doc "Canonical lowercase hyphenated string form.".
-spec 'asString'(t()) -> binary().
'asString'(#{'$beamtalk_class' := 'Uuid', bytes := Bytes}) ->
    Hex = binary:encode_hex(Bytes, lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = Hex,
    <<A/binary, $-, B/binary, $-, C/binary, $-, D/binary, $-, E/binary>>.

-doc "Raw 16-byte binary representation.".
-spec 'asBinary'(t()) -> <<_:128>>.
'asBinary'(#{'$beamtalk_class' := 'Uuid', bytes := Bytes}) ->
    Bytes.

-doc "The RFC 9562 version nibble (bits 48-51 of the 16-byte value).".
-spec version(t()) -> 0..15.
version(#{'$beamtalk_class' := 'Uuid', bytes := <<_:48, Version:4, _:76>>}) ->
    Version.

-doc "Human-readable representation: Uuid(...).".
-spec 'printString'(t()) -> binary().
'printString'(Self) ->
    iolist_to_binary([<<"Uuid(">>, 'asString'(Self), <<")">>]).

%%% ============================================================================
%%% Instance Methods — Comparison
%%% ============================================================================
%%
%% Ordering compares the raw 16-byte binary directly — Erlang's `<`/`>` on
%% binaries is byte-wise, which is equivalent to comparing the embedded
%% millisecond timestamp (and then the random tail) for v7 Uuids.

-spec '<'(t(), t()) -> boolean().
'<'(#{'$beamtalk_class' := 'Uuid', bytes := A}, #{'$beamtalk_class' := 'Uuid', bytes := B}) ->
    A < B;
'<'(_, _) ->
    raise_type_error('<', <<"Argument must be a Uuid">>).

-spec '>'(t(), t()) -> boolean().
'>'(#{'$beamtalk_class' := 'Uuid', bytes := A}, #{'$beamtalk_class' := 'Uuid', bytes := B}) ->
    A > B;
'>'(_, _) ->
    raise_type_error('>', <<"Argument must be a Uuid">>).

-spec '=<'(t(), t()) -> boolean().
'=<'(#{'$beamtalk_class' := 'Uuid', bytes := A}, #{'$beamtalk_class' := 'Uuid', bytes := B}) ->
    A =< B;
'=<'(_, _) ->
    raise_type_error('=<', <<"Argument must be a Uuid">>).

-spec '>='(t(), t()) -> boolean().
'>='(#{'$beamtalk_class' := 'Uuid', bytes := A}, #{'$beamtalk_class' := 'Uuid', bytes := B}) ->
    A >= B;
'>='(_, _) ->
    raise_type_error('>=', <<"Argument must be a Uuid">>).

%%% ============================================================================
%%% FFI Shims — (Erlang beamtalk_uuid) dispatch
%%% ============================================================================
%%
%% selector_to_function/1 extracts the first keyword segment as the function
%% name. These shims bridge camelCase/short FFI names to the existing
%% colon-suffix and operator-named implementations.

%% `fromString:` → strips to `fromString`, arity 1
-spec fromString(binary()) -> beamtalk_result:t().
fromString(Str) -> 'fromString:'(Str).

%% `isValid:` → strips to `isValid`, arity 1
-spec isValid(binary()) -> boolean().
isValid(Str) -> 'isValid:'(Str).

%% `lt:with:` → strips to `lt`, arity 2
-spec lt(t(), t()) -> boolean().
lt(Self, Other) -> '<'(Self, Other).

%% `gt:with:` → strips to `gt`, arity 2
-spec gt(t(), t()) -> boolean().
gt(Self, Other) -> '>'(Self, Other).

%% `lte:with:` → strips to `lte`, arity 2
-spec lte(t(), t()) -> boolean().
lte(Self, Other) -> '=<'(Self, Other).

%% `gte:with:` → strips to `gte`, arity 2
-spec gte(t(), t()) -> boolean().
gte(Self, Other) -> '>='(Self, Other).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-spec make_uuid(<<_:128>>) -> t().
make_uuid(Bytes) ->
    #{'$beamtalk_class' => 'Uuid', bytes => Bytes}.

-spec parse_uuid_string(binary()) -> {ok, <<_:128>>} | error.
parse_uuid_string(Str) ->
    case re:run(Str, ?UUID_STRING_RE, [{capture, all_but_first, binary}]) of
        {match, Groups} ->
            Hex = iolist_to_binary(Groups),
            {ok, binary:decode_hex(Hex)};
        nomatch ->
            error
    end.

-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    beamtalk_error:raise_type_error('Uuid', Selector, Hint).
