%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bitstring).

%%% **DDD Context:** Object System Context

-moduledoc """
Bitstring class implementation — unaligned bitstrings (ADR 0141, BT-4139).

A BEAM bitstring whose bit size is a multiple of 8 is a binary and has
class `Binary` (or `String` when tagged). Any other bitstring has class
`Bitstring` (`beamtalk_primitive:class_of/1`); this module backs its
`native:` methods. The protocol is deliberately bit-level only — the byte
protocol of `Binary`/`Collection` is not understood (see
`beamtalk_primitive:send/3`'s bitstring arm for the structured error).

| Function           | Selector        | Answer                                       |
|--------------------|-----------------|----------------------------------------------|
| `bitSize/1`        | `bitSize`       | number of bits                               |
| `toBits/1`         | `toBits`        | list of 0/1, most significant bit first      |
| `concat/2`         | `concat:`       | joined bits: a Binary when byte-aligned      |
| `asBinary/1`       | `asBinary`      | `Result` error `#unaligned_bitstring`        |
| `printString/1`    | `printString`   | Erlang notation, e.g. `<<2:3>>`              |
| `displayString/1`  | `displayString` | same as `printString`                        |
""".

-export([bitSize/1, toBits/1, concat/2, asBinary/1, printString/1, displayString/1]).

%%% ============================================================================
%%% Instance methods
%%% ============================================================================

-doc "Number of bits in the bitstring (`erlang:bit_size/1`).".
-spec bitSize(bitstring()) -> non_neg_integer().
bitSize(Bits) when is_bitstring(Bits) ->
    bit_size(Bits);
bitSize(_) ->
    raise_type_error(bitSize, <<"Receiver must be a Bitstring">>).

-doc "The bits as a list of 0/1 Integers, most significant bit first.".
-spec toBits(bitstring()) -> [0 | 1].
toBits(Bits) when is_bitstring(Bits) ->
    [B || <<B:1>> <= Bits];
toBits(_) ->
    raise_type_error(toBits, <<"Receiver must be a Bitstring">>).

-doc """
Join the receiver's bits with the argument's.

The argument may be a `Bitstring`, a `Binary`, or a `String` (joined by its
UTF-8 bytes, as `Binary>>concat:` does). The answer is whatever the joined
bits are: a `Binary` when the total bit size is a multiple of 8, otherwise
a `Bitstring` — the class follows the shape, as in Erlang.
""".
-spec concat(bitstring(), bitstring() | {beamtalk_string, binary()}) -> bitstring().
concat(A, {beamtalk_string, Raw}) when is_bitstring(A), is_binary(Raw) ->
    <<A/bitstring, Raw/binary>>;
concat(A, B) when is_bitstring(A), is_bitstring(B) ->
    <<A/bitstring, B/bitstring>>;
concat(A, _B) when not is_bitstring(A) ->
    raise_type_error(concat, <<"Receiver must be a Bitstring">>);
concat(_A, _B) ->
    raise_type_error(concat, <<"Argument must be a Bitstring, Binary or String">>).

-doc """
Convert to a `Binary`, as a `Result`.

A `Bitstring` is by definition not byte-aligned, so the answer is always
`Result error:` with kind `#unaligned_bitstring`. There is no silent
padding or truncation: the hint points at the bit-level protocol instead.
""".
-spec asBinary(bitstring()) -> beamtalk_result:t().
asBinary(Bits) when is_binary(Bits) ->
    %% Unreachable through dispatch (an aligned value is a Binary), but
    %% total for direct callers.
    beamtalk_result:from_tagged_tuple({ok, Bits});
asBinary(Bits) when is_bitstring(Bits) ->
    Hint = iolist_to_binary(
        io_lib:format(
            "~B bits is not a whole number of bytes; use 'bitSize' and 'toBits' "
            "to read the bits, or concat: more bits to reach a byte boundary",
            [bit_size(Bits)]
        )
    ),
    Error0 = beamtalk_error:new(unaligned_bitstring, 'Bitstring'),
    Error1 = beamtalk_error:with_selector(Error0, asBinary),
    Error2 = beamtalk_error:with_message(
        Error1,
        iolist_to_binary(
            io_lib:format("Bitstring of ~B bits is not byte-aligned", [bit_size(Bits)])
        )
    ),
    beamtalk_result:from_tagged_tuple({error, beamtalk_error:with_hint(Error2, Hint)});
asBinary(_) ->
    raise_type_error(asBinary, <<"Receiver must be a Bitstring">>).

-doc "Erlang notation, e.g. `<<2:3>>`.".
-spec printString(bitstring()) -> beamtalk_string:t().
printString(Bits) when is_bitstring(Bits) ->
    beamtalk_primitive:string_from_binary(beamtalk_primitive:print_string(Bits));
printString(_) ->
    raise_type_error(printString, <<"Receiver must be a Bitstring">>).

-doc "Same as `printString/1`: a Bitstring has no text reading.".
-spec displayString(bitstring()) -> beamtalk_string:t().
displayString(Bits) when is_bitstring(Bits) ->
    beamtalk_primitive:string_from_binary(beamtalk_primitive:display_string(Bits));
displayString(_) ->
    raise_type_error(displayString, <<"Receiver must be a Bitstring">>).

%%% ============================================================================
%%% Internal
%%% ============================================================================

-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    beamtalk_error:raise_type_error('Bitstring', Selector, Hint).
