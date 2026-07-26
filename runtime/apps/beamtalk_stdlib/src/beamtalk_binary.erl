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
| `byte_size/1`         | Byte count                                     |
| `part/3`              | Zero-copy slice via binary:part/3              |
| `concat/2`            | Concatenate two binaries                       |
| `to_bytes/1`          | Binary → list of byte integers                 |
| `from_bytes/1`        | Byte list → binary                             |
| `as_string/1`         | Validate UTF-8, return {ok,Bin}|{error,...}     |
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
-export([do/2, at/2, byte_at/2, byte_size/1, part/3, concat/2]).
-export([to_bytes/1, from_bytes/1]).
-export([as_string/1, as_string_unchecked/1, print_string/1]).
-export([deserialize_with_used/1]).

%% Instance methods — encoding
-export(['asBase64:'/1, 'asBase64Url:'/1, 'asHex:'/1]).
-export([asBase64/1, asBase64Url/1, asHex/1]).

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
-spec 'deserialize:'(term()) -> term().
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
-spec 'size:'(binary()) -> non_neg_integer().
'size:'(Bin) when is_binary(Bin) ->
    erlang:byte_size(Bin);
'size:'(_) ->
    raise_type_error('size:', <<"Argument must be a Binary">>).

-doc "Convert an iolist to a flat binary.".
-spec 'fromIolist:'(iolist()) -> binary().
'fromIolist:'(Iolist) ->
    try
        erlang:iolist_to_binary(Iolist)
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
-spec 'fromBase64:'(binary()) -> beamtalk_result:t().
'fromBase64:'(Str) when is_binary(Str) ->
    try
        beamtalk_result:from_tagged_tuple({ok, base64:decode(Str)})
    catch
        error:_ ->
            beamtalk_result:from_tagged_tuple({error, base64_decode_error('fromBase64:')})
    end;
'fromBase64:'(_) ->
    raise_type_error('fromBase64:', <<"Argument must be a String">>).

-doc """
Decode a URL-safe (RFC 4648 §5) base64 string (`-`/`_` in place of `+`/`/`).
Returns a Result, as `fromBase64:/1`.
""".
-spec 'fromBase64Url:'(binary()) -> beamtalk_result:t().
'fromBase64Url:'(Str) when is_binary(Str) ->
    try
        beamtalk_result:from_tagged_tuple({ok, base64:decode(Str, #{mode => urlsafe})})
    catch
        error:_ ->
            beamtalk_result:from_tagged_tuple({error, base64_decode_error('fromBase64Url:')})
    end;
'fromBase64Url:'(_) ->
    raise_type_error('fromBase64Url:', <<"Argument must be a String">>).

-doc """
Decode a hexadecimal string (upper- or lower-case digits accepted). Returns
a Result: `Result error:` on odd length or non-hex-digit characters.
""".
-spec 'fromHex:'(binary()) -> beamtalk_result:t().
'fromHex:'(Str) when is_binary(Str) ->
    try
        beamtalk_result:from_tagged_tuple({ok, binary:decode_hex(Str)})
    catch
        error:_ ->
            Error0 = beamtalk_error:new(parse_error, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromHex:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Expected a string of hexadecimal digits (even length, 0-9/a-f/A-F)">>
            ),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'fromHex:'(_) ->
    raise_type_error('fromHex:', <<"Argument must be a String">>).

%%% ============================================================================
%%% Instance Methods (ADR 0086 Phase 1)
%%% ============================================================================

-doc "Iterate over bytes in binary, calling Fun with each byte (Integer 0-255).".
-spec do(binary(), fun((non_neg_integer()) -> any())) -> ok.
do(Bin, Fun) when is_binary(Bin), is_function(Fun, 1) ->
    do_loop(Bin, 0, erlang:byte_size(Bin), Fun);
do(Bin, _Fun) when not is_binary(Bin) ->
    raise_type_error(do, <<"Receiver must be a Binary">>);
do(_Bin, _Fun) ->
    raise_type_error(do, <<"Argument must be a 1-argument block">>).

do_loop(_Bin, Pos, Size, _Fun) when Pos >= Size -> ok;
do_loop(Bin, Pos, Size, Fun) ->
    Byte = binary:at(Bin, Pos),
    Fun(Byte),
    do_loop(Bin, Pos + 1, Size, Fun).

-doc "1-based byte access. Returns Integer 0-255.".
-spec at(binary(), pos_integer()) -> non_neg_integer().
at(Bin, Index) when is_binary(Bin), is_integer(Index) ->
    Size = erlang:byte_size(Bin),
    if
        Index < 1 orelse Index > Size ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, at),
            Msg =
                case Size of
                    0 ->
                        io_lib:format("Index ~B is out of bounds (empty binary)", [Index]);
                    _ ->
                        io_lib:format("Index ~B is out of bounds (1..~B)", [Index, Size])
                end,
            Error2 = beamtalk_error:with_hint(Error1, iolist_to_binary(Msg)),
            beamtalk_error:raise(Error2);
        true ->
            binary:at(Bin, Index - 1)
    end;
at(Bin, _Index) when not is_binary(Bin) ->
    raise_type_error(at, <<"Receiver must be a Binary">>);
at(_Bin, _Index) ->
    raise_type_error(at, <<"Index must be an Integer">>).

-doc "0-based byte access (Erlang-compatible). Returns Integer 0-255.".
-spec byte_at(binary(), non_neg_integer()) -> non_neg_integer().
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

-doc "Return the byte count of a binary (instance version).".
-spec byte_size(binary()) -> non_neg_integer().
byte_size(Bin) when is_binary(Bin) ->
    erlang:byte_size(Bin);
byte_size(_) ->
    raise_type_error(byte_size, <<"Receiver must be a Binary">>).

-doc "Zero-copy slice via `binary:part/3`. Pos and Len are 0-based.".
-spec part(binary(), non_neg_integer(), non_neg_integer()) -> binary().
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

-doc "Concatenate two binaries.".
-spec concat(binary(), binary()) -> binary().
concat(A, B) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>;
concat(A, _B) when not is_binary(A) ->
    raise_type_error(concat, <<"Receiver must be a Binary">>);
concat(_A, _B) ->
    raise_type_error(concat, <<"Argument must be a Binary">>).

-doc "Convert binary to list of byte integers.".
-spec to_bytes(binary()) -> [non_neg_integer()].
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

-doc "Validate UTF-8 and return {ok, Binary} or {error, {invalid_utf8, Offset}}.".
-spec as_string(binary()) -> {ok, binary()} | {error, {invalid_utf8, non_neg_integer()}}.
as_string(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8) of
        Bin2 when is_binary(Bin2) ->
            {ok, Bin2};
        {error, _Good, _Bad} ->
            %% Find the offset where invalid UTF-8 starts
            {error, {invalid_utf8, find_invalid_utf8_offset(Bin, 0)}};
        {incomplete, _Good, _Bad} ->
            {error, {invalid_utf8, find_invalid_utf8_offset(Bin, 0)}}
    end;
as_string(_) ->
    raise_type_error(as_string, <<"Receiver must be a Binary">>).

-doc "Return binary as-is, without UTF-8 validation.".
-spec as_string_unchecked(binary()) -> binary().
as_string_unchecked(Bin) when is_binary(Bin) ->
    Bin;
as_string_unchecked(_) ->
    raise_type_error(as_string_unchecked, <<"Receiver must be a Binary">>).

-doc "Encode as a standard (RFC 4648 §4) base64 string, with `=` padding.".
-spec 'asBase64:'(binary()) -> binary().
'asBase64:'(Bin) when is_binary(Bin) ->
    base64:encode(Bin);
'asBase64:'(_) ->
    raise_type_error('asBase64:', <<"Receiver must be a Binary">>).

-doc """
Encode as a URL-safe (RFC 4648 §5) base64 string, with `=` padding
(`-`/`_` in place of `+`/`/`).
""".
-spec 'asBase64Url:'(binary()) -> binary().
'asBase64Url:'(Bin) when is_binary(Bin) ->
    base64:encode(Bin, #{mode => urlsafe});
'asBase64Url:'(_) ->
    raise_type_error('asBase64Url:', <<"Receiver must be a Binary">>).

-doc "Encode as a lowercase hexadecimal string.".
-spec 'asHex:'(binary()) -> binary().
'asHex:'(Bin) when is_binary(Bin) ->
    binary:encode_hex(Bin, lowercase);
'asHex:'(_) ->
    raise_type_error('asHex:', <<"Receiver must be a Binary">>).

-doc "Printable representation: hex for non-UTF-8, quoted for valid UTF-8.".
-spec print_string(binary()) -> binary().
print_string(Bin) when is_binary(Bin) ->
    case as_string(Bin) of
        {ok, _} ->
            Escaped = binary:replace(
                binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]),
                <<"\"">>,
                <<"\\\"">>,
                [global]
            ),
            iolist_to_binary([<<"\"">>, Escaped, <<"\"">>]);
        {error, _} ->
            iolist_to_binary([<<"<<">>, bytes_to_hex(Bin), <<">>">>])
    end;
print_string(_) ->
    raise_type_error(print_string, <<"Receiver must be a Binary">>).

-doc """
Deserialize binary using binary_to_term/2 with [safe, used].
Returns {Value, BytesConsumed}.
""".
-spec deserialize_with_used(term()) -> {term(), non_neg_integer()}.
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
-spec deserialize(term()) -> term().
deserialize(X) -> 'deserialize:'(X).

-doc "FFI alias for size:/1.".
-spec size(binary()) -> non_neg_integer().
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
-spec 'deserializeWithUsed:'(term()) -> {term(), non_neg_integer()}.
'deserializeWithUsed:'(Bin) -> deserialize_with_used(Bin).

-doc "FFI alias for deserializeWithUsed:/1.".
-spec deserializeWithUsed(term()) -> {term(), non_neg_integer()}.
deserializeWithUsed(Bin) -> deserialize_with_used(Bin).

-doc "FFI alias for fromBase64:/1.".
-spec fromBase64(binary()) -> beamtalk_result:t().
fromBase64(Str) -> 'fromBase64:'(Str).

-doc "FFI alias for fromBase64Url:/1.".
-spec fromBase64Url(binary()) -> beamtalk_result:t().
fromBase64Url(Str) -> 'fromBase64Url:'(Str).

-doc "FFI alias for fromHex:/1.".
-spec fromHex(binary()) -> beamtalk_result:t().
fromHex(Str) -> 'fromHex:'(Str).

-doc "FFI alias for asBase64:/1.".
-spec asBase64(binary()) -> binary().
asBase64(Bin) -> 'asBase64:'(Bin).

-doc "FFI alias for asBase64Url:/1.".
-spec asBase64Url(binary()) -> binary().
asBase64Url(Bin) -> 'asBase64Url:'(Bin).

-doc "FFI alias for asHex:/1.".
-spec asHex(binary()) -> binary().
asHex(Bin) -> 'asHex:'(Bin).
