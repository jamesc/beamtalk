%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_binary).

%%% **DDD Context:** Object System Context

-moduledoc """
Binary class implementation — serialization, byte operations, and
instance methods for binary data.

Binary provides class-side methods for serializing/deserializing Erlang
terms and working with byte-level binary data, plus instance methods for
iteration, slicing, and conversion (ADR 0086 Phase 1).

## Class Methods

| Selector        | Description                                      |
|-----------------|--------------------------------------------------|
| `serialize:`    | Term → binary (via `term_to_binary/1`)            |
| `deserialize:`  | Binary → term (via `binary_to_term/2`, safe mode) |
| `size:`         | Binary → byte size                                |
| `fromIolist:`   | Iolist → binary                                  |
| `fromBase64:`   | Standard base64 string → Result(Binary, Error)   |
| `fromBase64Url:` | URL-safe base64 string → Result(Binary, Error)  |
| `fromHex:`      | Hex string → Result(Binary, Error)               |

## Instance Methods

| Function              | Description                                    |
|-----------------------|------------------------------------------------|
| `do/2`                | Iterate over bytes, calling block per byte     |
| `at/2`                | 1-based byte access, returns Integer           |
| `byte_at/2`           | 0-based byte access, returns Integer           |
| `part/3`              | Zero-copy slice via binary:part/3              |
| `concat/2`            | Concatenate two binaries                       |
| `to_bytes/1`          | Binary → list of byte integers                 |
| `from_bytes/1`        | Byte list → binary                             |
| `as_string/1`         | Validate UTF-8, return Result ok:/error:        |
| `as_string_unchecked/1` | Return binary as-is (no validation)           |
| `print_string/1`      | Printable representation                       |
| `deserialize_with_used/1` | binary_to_term with byte count             |
| `asBase64:/1`         | Standard base64 encode                         |
| `asBase64Url:/1`      | URL-safe base64 encode                         |
| `asHex:/1`            | Lowercase hex encode                           |
""".

%% Class methods
-export([
    'serialize:'/1,
    'deserialize:'/1,
    'size:'/1,
    'fromIolist:'/1,
    'fromBytes:'/1,
    'deserializeWithUsed:'/1,
    'fromBase64:'/1,
    'fromBase64Url:'/1,
    'fromHex:'/1
]).
-export([serialize/1, deserialize/1, size/1, fromIolist/1, fromBytes/1, deserializeWithUsed/1]).
-export([fromBase64/1, fromBase64Url/1, fromHex/1]).

%% Instance methods (ADR 0086 Phase 1)
-export([do/2, at/2, byte_at/2, part/3, concat/2]).
-export([to_bytes/1, from_bytes/1]).
-export([as_string/1, as_string_unchecked/1, print_string/1]).
-export([deserialize_with_used/1]).

%% Shared UTF-8 validator for native text boundaries (BT-4171)
-export([check_utf8/1]).

%% Representation (BT-4170): a `Binary` is a bare BEAM `binary()`; a `String`
%% is the tagged term `{beamtalk_string, Bin}`. `String` subclasses `Binary`,
%% so Binary methods accept a `String` self (reached via `super` sends from
%% `String`): byte-level operations unwrap the tag and work on the bytes,
%% and `as_string/1` answers `ok` with the self it was handed (valid UTF-8
%% by construction). `String`-only strictness lives in `beamtalk_string`.
-type binary_or_string() :: binary() | {beamtalk_string, binary()}.
-export_type([binary_or_string/0]).

%% Instance methods — encoding
-export(['asBase64:'/1, 'asBase64Url:'/1, 'asHex:'/1]).

%% Instance methods — byte-level substring search (BT-4190)
-export([includes_substring/2, index_of/2]).
-export([asBase64/1, asBase64Url/1, asHex/1]).

%% Instance methods — bit size, shared with Bitstring (ADR 0141, BT-4139)
-export([bitSize/1]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Serialize any term to a binary via `erlang:term_to_binary/1`.".
-spec 'serialize:'(term()) -> binary().
'serialize:'(Term) ->
    erlang:term_to_binary(Term).

-doc """
Deserialize a binary back to the original term.

Uses safe mode (`[safe]`) to prevent atom table exhaustion from untrusted
input. Atoms not already in the atom table will cause an error.
""".
-spec 'deserialize:'(binary_or_string()) -> term().
'deserialize:'({beamtalk_string, Raw}) when is_binary(Raw) ->
    'deserialize:'(Raw);
'deserialize:'(Bin) when is_binary(Bin) ->
    try
        erlang:binary_to_term(Bin, [safe])
    catch
        error:badarg ->
            raise_type_error('deserialize:', <<"Binary data is invalid or contains unknown atoms">>)
    end;
'deserialize:'(_) ->
    raise_type_error('deserialize:', <<"Argument must be a Binary">>).

-doc "Return the byte size of a binary.".
-spec 'size:'(binary_or_string()) -> non_neg_integer().
'size:'({beamtalk_string, Raw}) when is_binary(Raw) ->
    erlang:byte_size(Raw);
'size:'(Bin) when is_binary(Bin) ->
    erlang:byte_size(Bin);
'size:'(_) ->
    raise_type_error('size:', <<"Argument must be a Binary">>).

-doc """
Convert an iolist to a flat binary.

BT-4170: `String` elements unwrap before assembly, alongside the raw
binaries, byte integers and nested iolists that were always accepted.
""".
-spec 'fromIolist:'(iolist()) -> binary().
'fromIolist:'(Iolist) ->
    try
        erlang:iolist_to_binary(beamtalk_primitive:deep_unwrap_strings(Iolist))
    catch
        error:badarg ->
            raise_type_error(
                'fromIolist:',
                <<"Argument must be a valid iolist (list of binaries, integers 0-255, or nested iolists)">>
            )
    end.

-doc """
Decode a standard (RFC 4648 §4) base64 string. Returns a Result: `Result ok:`
with the decoded Binary, or `Result error:` on malformed base64 (wrong
padding, invalid alphabet characters, etc.).
""".
-spec 'fromBase64:'(beamtalk_string:t()) -> beamtalk_result:t().
'fromBase64:'({beamtalk_string, Str}) when is_binary(Str) ->
    try base64:decode(Str) of
        Decoded ->
            beamtalk_result:from_tagged_tuple({ok, Decoded})
    catch
        %% base64:decode/1 signals malformed input inconsistently across
        %% cases — badarg for a bad alphabet/padding, function_clause for
        %% some too-short inputs — so this stays a catch-all rather than
        %% naming a specific reason.
        error:_ ->
            beamtalk_result:from_tagged_tuple({error, base64_decode_error('fromBase64:')})
    end;
'fromBase64:'(Bin) when is_binary(Bin) ->
    raise_type_error(
        'fromBase64:', <<"Argument must be a String; decode bytes via asString first">>
    );
'fromBase64:'(_) ->
    raise_type_error('fromBase64:', <<"Argument must be a String">>).

-doc """
Decode a URL-safe (RFC 4648 §5) base64 string (`-`/`_` in place of `+`/`/`).
Returns a Result, as `fromBase64:/1`.

Accepts both padded and unpadded input (`padding => false`) — the unpadded
form is the one commonly used in JWTs (RFC 7515 §2) and PKCE challenges.
""".
-spec 'fromBase64Url:'(beamtalk_string:t()) -> beamtalk_result:t().
'fromBase64Url:'({beamtalk_string, Str}) when is_binary(Str) ->
    try base64:decode(Str, #{mode => urlsafe, padding => false}) of
        Decoded ->
            beamtalk_result:from_tagged_tuple({ok, Decoded})
    catch
        %% Malformed input raises badarg, function_clause, or even badarith
        %% depending on exactly how it's malformed (verified empirically —
        %% not documented) — a catch-all is the only robust option.
        error:_ ->
            beamtalk_result:from_tagged_tuple({error, base64_decode_error('fromBase64Url:')})
    end;
'fromBase64Url:'(Bin) when is_binary(Bin) ->
    raise_type_error(
        'fromBase64Url:', <<"Argument must be a String; decode bytes via asString first">>
    );
'fromBase64Url:'(_) ->
    raise_type_error('fromBase64Url:', <<"Argument must be a String">>).

-doc """
Decode a hexadecimal string (upper- or lower-case digits accepted). Returns
a Result: `Result error:` on odd length or non-hex-digit characters.
""".
-spec 'fromHex:'(beamtalk_string:t()) -> beamtalk_result:t().
'fromHex:'({beamtalk_string, Str}) when is_binary(Str) ->
    try binary:decode_hex(Str) of
        Decoded ->
            beamtalk_result:from_tagged_tuple({ok, Decoded})
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(parse_error, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromHex:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Expected a string of hexadecimal digits (even length, 0-9/a-f/A-F)">>
            ),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'fromHex:'(Bin) when is_binary(Bin) ->
    raise_type_error(
        'fromHex:', <<"Argument must be a String; decode bytes via asString first">>
    );
'fromHex:'(_) ->
    raise_type_error('fromHex:', <<"Argument must be a String">>).

%%% ============================================================================
%%% Instance Methods (ADR 0086 Phase 1)
%%% ============================================================================

-doc """
Iterate over bytes in binary, calling Fun with each byte (Integer 0-255).

BT-4173: byte-level — a `String` self (e.g. via `super do:` from
`String`, which overrides `do:` with grapheme iteration) iterates its
bytes, the Binary semantics the super-send asked for.
""".
-spec do(binary() | beamtalk_string:t(), fun((non_neg_integer()) -> any())) -> nil.
do(Bin, Fun) when is_binary(Bin), is_function(Fun, 1) ->
    do_loop(Bin, 0, erlang:byte_size(Bin), Fun);
do({beamtalk_string, Raw}, Fun) when is_binary(Raw) ->
    do(Raw, Fun);
do(Bin, _Fun) when not is_binary(Bin) ->
    raise_type_error(do, <<"Receiver must be a Binary">>);
do(_Bin, _Fun) ->
    raise_type_error(do, <<"Argument must be a 1-argument block">>).

do_loop(_Bin, Pos, Size, _Fun) when Pos >= Size -> nil;
do_loop(Bin, Pos, Size, Fun) ->
    Byte = binary:at(Bin, Pos),
    Fun(Byte),
    do_loop(Bin, Pos + 1, Size, Fun).

-doc """
1-based byte access. Returns Integer 0-255.

Indexing an empty Binary raises `empty_collection`, matching
`beamtalk_list:at/2`, `beamtalk_string:at/2` and `beamtalk_array:at/2`. An
index below 1 is malformed whether or not the Binary is empty, so it stays
`index_out_of_bounds`.

BT-4173: byte-level — a `String` self (e.g. via `super at:`) answers
the byte at the index, the Binary semantics the super-send asked for.
""".
-spec at(binary() | beamtalk_string:t(), pos_integer()) -> non_neg_integer().
at({beamtalk_string, Raw}, Index) when is_binary(Raw) ->
    at(Raw, Index);
at(Bin, Index) when is_binary(Bin), is_integer(Index) ->
    Size = erlang:byte_size(Bin),
    if
        Size =:= 0 andalso Index >= 1 ->
            Error0 = beamtalk_error:new(empty_collection, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, at),
            Error2 = beamtalk_error:with_hint(
                Error1, <<"Binary is empty; guard with `isEmpty` before indexing">>
            ),
            beamtalk_error:raise(Error2);
        Index < 1 orelse Index > Size ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, at),
            Msg = io_lib:format("Index ~B is out of bounds (1..~B)", [Index, Size]),
            Error2 = beamtalk_error:with_hint(Error1, iolist_to_binary(Msg)),
            beamtalk_error:raise(Error2);
        true ->
            binary:at(Bin, Index - 1)
    end;
at(Bin, _Index) when not is_binary(Bin) ->
    raise_type_error(at, <<"Receiver must be a Binary">>);
at(_Bin, _Index) ->
    raise_type_error(at, <<"Index must be an Integer">>).

-doc """
0-based byte access (Erlang-compatible). Returns Integer 0-255.

BT-4170: inherited unchanged by `String`, so a tagged `String` receiver
unwraps here (a `String` is a `Binary` of its UTF-8 bytes).
""".
-spec byte_at(binary_or_string(), non_neg_integer()) -> non_neg_integer().
byte_at({beamtalk_string, Raw}, Index) when is_binary(Raw) ->
    byte_at(Raw, Index);
byte_at(Bin, Index) when is_binary(Bin), is_integer(Index) ->
    Size = erlang:byte_size(Bin),
    if
        Index < 0 orelse Index >= Size ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, byte_at),
            Msg =
                case Size of
                    0 ->
                        io_lib:format("Index ~B is out of bounds (empty binary)", [Index]);
                    _ ->
                        io_lib:format("Index ~B is out of bounds (0..~B)", [Index, Size - 1])
                end,
            Error2 = beamtalk_error:with_hint(Error1, iolist_to_binary(Msg)),
            beamtalk_error:raise(Error2);
        true ->
            binary:at(Bin, Index)
    end;
byte_at(Bin, _Index) when not is_binary(Bin) ->
    raise_type_error(byte_at, <<"Receiver must be a Binary">>);
byte_at(_Bin, _Index) ->
    raise_type_error(byte_at, <<"Index must be an Integer">>).

-doc "Zero-copy slice via `binary:part/3`. Pos and Len are 0-based.".
-spec part(binary_or_string(), non_neg_integer(), non_neg_integer()) -> binary().
part({beamtalk_string, Raw}, Pos, Len) when is_binary(Raw) ->
    part(Raw, Pos, Len);
part(Bin, Pos, Len) when is_binary(Bin), is_integer(Pos), is_integer(Len) ->
    try
        binary:part(Bin, Pos, Len)
    catch
        error:badarg ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    index_out_of_bounds,
                    'Binary',
                    part,
                    iolist_to_binary(
                        io_lib:format(
                            "part(~B, ~B) is out of bounds for binary of size ~B",
                            [Pos, Len, erlang:byte_size(Bin)]
                        )
                    )
                )
            )
    end;
part(Bin, _Pos, _Len) when not is_binary(Bin) ->
    raise_type_error(part, <<"Receiver must be a Binary">>);
part(_Bin, _Pos, _Len) ->
    raise_type_error(part, <<"Position and length must be Integers">>).

-doc """
Concatenate two binaries.

BT-4170: either side may be a tagged `String` (`String` is a `Binary`
of its UTF-8 bytes); the result is always a bare `Binary`.
""".
-spec concat(binary_or_string(), binary_or_string()) -> binary().
concat({beamtalk_string, RawA}, B) when is_binary(RawA) ->
    concat(RawA, B);
concat(A, {beamtalk_string, RawB}) when is_binary(A), is_binary(RawB) ->
    concat(A, RawB);
concat(A, B) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>;
concat(A, _B) when not is_binary(A) ->
    raise_type_error(concat, <<"Receiver must be a Binary">>);
concat(_A, _B) ->
    raise_type_error(concat, <<"Argument must be a Binary">>).

-doc """
Bit count of a binary: 8 times its byte count (ADR 0141). A `String`
counts its UTF-8 bytes.
""".
-spec bitSize(binary_or_string()) -> non_neg_integer().
bitSize({beamtalk_string, Raw}) when is_binary(Raw) ->
    bit_size(Raw);
bitSize(Bin) when is_binary(Bin) ->
    bit_size(Bin);
bitSize(_) ->
    raise_type_error(bitSize, <<"Receiver must be a Binary">>).

-doc "Convert binary to list of byte integers.".
-spec to_bytes(binary_or_string()) -> [non_neg_integer()].
to_bytes({beamtalk_string, Raw}) when is_binary(Raw) ->
    binary:bin_to_list(Raw);
to_bytes(Bin) when is_binary(Bin) ->
    binary:bin_to_list(Bin);
to_bytes(_) ->
    raise_type_error(to_bytes, <<"Receiver must be a Binary">>).

-doc "Create binary from list of byte integers.".
-spec from_bytes([non_neg_integer()]) -> binary().
from_bytes(Bytes) when is_list(Bytes) ->
    try
        list_to_binary(Bytes)
    catch
        error:badarg ->
            raise_type_error(
                from_bytes,
                <<"Argument must be a list of integers 0-255">>
            )
    end;
from_bytes(_) ->
    raise_type_error(from_bytes, <<"Argument must be a list of integers 0-255">>).

-doc """
Validate UTF-8 and return a real Result (BT-4169, BT-4170).

Success answers `Result ok:` with the tagged `String`; failure answers
`Result error:` with a structured `parse_error` (class `Binary`,
selector `asString`) whose message and `details` (`#{offset => N}`)
carry the byte offset of the invalid sequence — the same `parse_error`
kind the `fromBase64:`/`fromHex:` decoders use for malformed input.

`String` overrides `asString` with `self`; a tagged `String` still reaches
here through `super asString` — its bytes are valid UTF-8 by construction,
so it answers `ok` with itself.
""".
-spec as_string(binary() | beamtalk_string:t()) -> beamtalk_result:t().
as_string(Bin) when is_binary(Bin) ->
    case check_utf8(Bin) of
        {ok, Valid} ->
            beamtalk_result:from_tagged_tuple({
                ok, beamtalk_primitive:string_from_binary(Valid)
            });
        {error, Offset} ->
            beamtalk_result:from_tagged_tuple({error, invalid_utf8_error(Offset)})
    end;
as_string({beamtalk_string, _} = Str) ->
    beamtalk_result:from_tagged_tuple({ok, Str});
as_string(_) ->
    raise_type_error(as_string, <<"Receiver must be a Binary">>).

-doc """
Return these bytes as a `String`, without UTF-8 validation.

BT-4170: a bare binary wraps unchecked (trust the source); a tagged
`String` is already the answer and passes through.
""".
-spec as_string_unchecked(binary_or_string()) -> beamtalk_string:t().
as_string_unchecked({beamtalk_string, _} = Str) ->
    Str;
as_string_unchecked(Bin) when is_binary(Bin) ->
    beamtalk_primitive:string_from_binary(Bin);
as_string_unchecked(_) ->
    raise_type_error(as_string_unchecked, <<"Receiver must be a Binary">>).

-doc "Encode as a standard (RFC 4648 §4) base64 string, with `=` padding.".
-spec 'asBase64:'(binary_or_string()) -> beamtalk_string:t().
'asBase64:'({beamtalk_string, Raw}) when is_binary(Raw) ->
    'asBase64:'(Raw);
'asBase64:'(Bin) when is_binary(Bin) ->
    %% base64 output is ASCII by construction — the wrap needs no validation.
    beamtalk_primitive:string_from_binary(base64:encode(Bin));
'asBase64:'(_) ->
    raise_type_error('asBase64:', <<"Receiver must be a Binary">>).

-doc """
Encode as a URL-safe (RFC 4648 §5) base64 string, with `=` padding
(`-`/`_` in place of `+`/`/`).
""".
-spec 'asBase64Url:'(binary_or_string()) -> beamtalk_string:t().
'asBase64Url:'({beamtalk_string, Raw}) when is_binary(Raw) ->
    'asBase64Url:'(Raw);
'asBase64Url:'(Bin) when is_binary(Bin) ->
    beamtalk_primitive:string_from_binary(base64:encode(Bin, #{mode => urlsafe}));
'asBase64Url:'(_) ->
    raise_type_error('asBase64Url:', <<"Receiver must be a Binary">>).

-doc "True if Needle occurs as a byte sequence in Bin. Empty needle is found.".
-spec includes_substring(binary_or_string(), binary_or_string()) -> boolean().
includes_substring({beamtalk_string, Raw}, Needle) when is_binary(Raw) ->
    includes_substring(Raw, Needle);
includes_substring(Bin, {beamtalk_string, Raw}) when is_binary(Bin), is_binary(Raw) ->
    includes_substring(Bin, Raw);
includes_substring(Bin, Needle) when is_binary(Bin), is_binary(Needle) ->
    Needle =:= <<>> orelse binary:match(Bin, Needle) =/= nomatch;
includes_substring(Bin, _Needle) when not is_binary(Bin) ->
    raise_type_error('includesSubstring:', <<"Receiver must be a Binary">>);
includes_substring(_Bin, _Needle) ->
    raise_type_error('includesSubstring:', <<"Argument must be a Binary">>).

-doc "1-based byte index of the first occurrence of Needle, or nil. Empty needle yields nil.".
-spec index_of(binary_or_string(), binary_or_string()) -> pos_integer() | nil.
index_of({beamtalk_string, Raw}, Needle) when is_binary(Raw) ->
    index_of(Raw, Needle);
index_of(Bin, {beamtalk_string, Raw}) when is_binary(Bin), is_binary(Raw) ->
    index_of(Bin, Raw);
index_of(Bin, Needle) when is_binary(Bin), is_binary(Needle) ->
    case Needle of
        <<>> ->
            nil;
        _ ->
            case binary:match(Bin, Needle) of
                nomatch -> nil;
                {Pos, _Len} -> Pos + 1
            end
    end;
index_of(Bin, _Needle) when not is_binary(Bin) ->
    raise_type_error('indexOf:', <<"Receiver must be a Binary">>);
index_of(_Bin, _Needle) ->
    raise_type_error('indexOf:', <<"Argument must be a Binary">>).

-doc "Encode as a lowercase hexadecimal string.".
-spec 'asHex:'(binary_or_string()) -> beamtalk_string:t().
'asHex:'({beamtalk_string, Raw}) when is_binary(Raw) ->
    'asHex:'(Raw);
'asHex:'(Bin) when is_binary(Bin) ->
    beamtalk_primitive:string_from_binary(binary:encode_hex(Bin, lowercase));
'asHex:'(_) ->
    raise_type_error('asHex:', <<"Receiver must be a Binary">>).

-doc "Printable representation: hex for non-UTF-8, quoted for valid UTF-8.".
-spec print_string(binary() | beamtalk_string:t()) -> beamtalk_string:t().
print_string(Bin) when is_binary(Bin) ->
    beamtalk_primitive:string_from_binary(
        case check_utf8(Bin) of
            {ok, _} ->
                %% ADR 0138 escape mapping lives in one place.
                beamtalk_primitive:print_string(Bin);
            {error, _} ->
                iolist_to_binary([<<"<<">>, bytes_to_hex(Bin), <<">>">>])
        end
    );
print_string({beamtalk_string, Raw}) when is_binary(Raw) ->
    %% BT-4173: a `String` self (e.g. via `super printString`) renders its
    %% bytes — the Binary rendering the super-send asked for.
    print_string(Raw);
print_string(_) ->
    raise_type_error(print_string, <<"Receiver must be a Binary">>).

-doc """
Deserialize binary using binary_to_term/2 with [safe, used].
Returns {Value, BytesConsumed}.
""".
-spec deserialize_with_used(binary_or_string()) -> {term(), non_neg_integer()}.
deserialize_with_used({beamtalk_string, Raw}) when is_binary(Raw) ->
    deserialize_with_used(Raw);
deserialize_with_used(Bin) when is_binary(Bin) ->
    try
        erlang:binary_to_term(Bin, [safe, used])
    catch
        error:badarg ->
            raise_type_error(
                deserialize_with_used, <<"Binary data is invalid or contains unknown atoms">>
            )
    end;
deserialize_with_used(_) ->
    raise_type_error(deserialize_with_used, <<"Receiver must be a Binary">>).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc "Raise a Binary type_error by delegating to beamtalk_error:raise_type_error/3.".
-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    beamtalk_error:raise_type_error('Binary', Selector, Hint).

-doc """
Validate UTF-8, returning the binary on success or the byte offset of the
first invalid byte on failure. Shared core of `as_string/1` (which wraps
the outcome in a Result) and `print_string/1` (which picks a rendering).

Also the shared validator for native text boundaries (BT-4171): Json
leaf/key construction routes text through here instead of duplicating
validation logic, so the boundary constructs a validated String rather
than a binary that merely sniffs as text. Out-of-repo natives (Yaml, HTTP)
use `beamtalk_primitive:is_utf8/1` — same check, already released — until
they can depend on this export.
""".
-spec check_utf8(binary()) -> {ok, binary()} | {error, non_neg_integer()}.
check_utf8(Bin) ->
    case unicode:characters_to_binary(Bin, utf8) of
        Bin2 when is_binary(Bin2) ->
            {ok, Bin2};
        {error, _Good, _Bad} ->
            {error, find_invalid_utf8_offset(Bin, 0)};
        {incomplete, _Good, _Bad} ->
            {error, find_invalid_utf8_offset(Bin, 0)}
    end.

-doc "Build the structured parse_error for an invalid-UTF-8 byte offset.".
-spec invalid_utf8_error(non_neg_integer()) -> beamtalk_error:error().
invalid_utf8_error(Offset) ->
    Error0 = beamtalk_error:new(parse_error, 'Binary'),
    Error1 = beamtalk_error:with_selector(Error0, asString),
    Error2 = beamtalk_error:with_message(
        Error1, iolist_to_binary(io_lib:format("Invalid UTF-8 at byte offset ~B", [Offset]))
    ),
    Error3 = beamtalk_error:with_hint(
        Error2, <<"Use asStringUnchecked to skip validation">>
    ),
    beamtalk_error:with_details(Error3, #{offset => Offset}).

-doc "Find offset of first invalid UTF-8 byte.".
find_invalid_utf8_offset(<<>>, Offset) ->
    Offset;
find_invalid_utf8_offset(Bin, Offset) ->
    case Bin of
        <<_/utf8, Rest/binary>> ->
            find_invalid_utf8_offset(Rest, Offset + erlang:byte_size(Bin) - erlang:byte_size(Rest));
        _ ->
            Offset
    end.

-doc "Convert binary to hex string representation.".
bytes_to_hex(<<>>) ->
    <<>>;
bytes_to_hex(Bin) ->
    HexParts = [io_lib:format("~2.16.0B", [B]) || <<B>> <= Bin],
    iolist_to_binary(lists:join(<<" ">>, HexParts)).

-doc "Build a structured parse_error for a malformed base64 string.".
-spec base64_decode_error(atom()) -> beamtalk_error:error().
base64_decode_error(Selector) ->
    Error0 = beamtalk_error:new(parse_error, 'Binary'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:with_hint(
        Error1,
        <<"Expected a well-formed base64 string (correct alphabet and padding)">>
    ).

%%% ============================================================================
%%% FFI aliases — no-colon names for Erlang FFI dispatch
%%% ============================================================================

-doc "FFI alias for serialize:/1.".
-spec serialize(term()) -> binary().
serialize(X) -> 'serialize:'(X).

-doc "FFI alias for deserialize:/1.".
-spec deserialize(binary_or_string()) -> term().
deserialize(X) -> 'deserialize:'(X).

-doc "FFI alias for size:/1.".
-spec size(binary_or_string()) -> non_neg_integer().
size(X) -> 'size:'(X).

-doc "FFI alias for fromIolist:/1.".
-spec fromIolist(iolist()) -> binary().
fromIolist(X) -> 'fromIolist:'(X).

-doc "Class method wrapper for from_bytes/1.".
-spec 'fromBytes:'([non_neg_integer()]) -> binary().
'fromBytes:'(Bytes) -> from_bytes(Bytes).

-doc "FFI alias for fromBytes:/1.".
-spec fromBytes([non_neg_integer()]) -> binary().
fromBytes(Bytes) -> from_bytes(Bytes).

-doc "Class method wrapper for deserialize_with_used/1.".
-spec 'deserializeWithUsed:'(binary_or_string()) -> {term(), non_neg_integer()}.
'deserializeWithUsed:'(Bin) -> deserialize_with_used(Bin).

-doc "FFI alias for deserializeWithUsed:/1.".
-spec deserializeWithUsed(binary_or_string()) -> {term(), non_neg_integer()}.
deserializeWithUsed(Bin) -> deserialize_with_used(Bin).

-doc "FFI alias for fromBase64:/1.".
-spec fromBase64(beamtalk_string:t()) -> beamtalk_result:t().
fromBase64(Str) -> 'fromBase64:'(Str).

-doc "FFI alias for fromBase64Url:/1.".
-spec fromBase64Url(beamtalk_string:t()) -> beamtalk_result:t().
fromBase64Url(Str) -> 'fromBase64Url:'(Str).

-doc "FFI alias for fromHex:/1.".
-spec fromHex(beamtalk_string:t()) -> beamtalk_result:t().
fromHex(Str) -> 'fromHex:'(Str).

-doc "FFI alias for asBase64:/1.".
-spec asBase64(binary_or_string()) -> beamtalk_string:t().
asBase64(Bin) -> 'asBase64:'(Bin).

-doc "FFI alias for asBase64Url:/1.".
-spec asBase64Url(binary_or_string()) -> beamtalk_string:t().
asBase64Url(Bin) -> 'asBase64Url:'(Bin).

-doc "FFI alias for asHex:/1.".
-spec asHex(binary_or_string()) -> beamtalk_string:t().
asHex(Bin) -> 'asHex:'(Bin).
