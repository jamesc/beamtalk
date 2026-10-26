%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_time).

%%% **DDD Context:** Object System Context

-moduledoc """
Time class implementation — current time accessors.

Time provides class-side methods for reading the system and monotonic
clocks as integers: `nowS`/`nowMs`/`nowUs` answer the system clock as a
Unix-epoch timestamp, while `monotonic`/`systemTime` answer Erlang's raw
clocks in a caller-chosen unit (native units when no unit is given) and
`convertTime` converts a time value between units.

## Methods

| Selector               | Description                        |
|------------------------|------------------------------------|
| `nowS`                 | Current time in seconds            |
| `nowMs`                | Current time in milliseconds       |
| `nowUs`                | Current time in microseconds       |
| `monotonic`            | Monotonic clock in native units    |
| `monotonic:`           | Monotonic clock in the given unit  |
| `systemTime`           | System clock in native units       |
| `systemTime:`          | System clock in the given unit     |
| `convertTime:from:to:` | Convert a time value between units |
""".

-export([nowS/0, nowMs/0, nowUs/0]).
-export([monotonic/0, monotonic/1, systemTime/0, systemTime/1, convertTime/3]).

-type time_unit() :: second | millisecond | microsecond | nanosecond | native.

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Current time in seconds since the Unix epoch.".
-spec nowS() -> integer().
nowS() ->
    erlang:system_time(second).

-doc "Current time in milliseconds since the Unix epoch.".
-spec nowMs() -> integer().
nowMs() ->
    erlang:system_time(millisecond).

-doc "Current time in microseconds since the Unix epoch.".
-spec nowUs() -> integer().
nowUs() ->
    erlang:system_time(microsecond).

-doc "Monotonic clock in Erlang native units (for measuring intervals).".
-spec monotonic() -> integer().
monotonic() ->
    erlang:monotonic_time().

-doc """
Monotonic clock in the given unit.

Unit must be one of `second`, `millisecond`, `microsecond`, `nanosecond`,
`native` (`type_error` otherwise).
""".
-spec monotonic(time_unit()) -> integer().
monotonic(Unit) ->
    erlang:monotonic_time(check_unit('monotonic:', unit, Unit)).

-doc "System clock (Unix-epoch time) in Erlang native units.".
-spec systemTime() -> integer().
systemTime() ->
    erlang:system_time().

-doc """
System clock (Unix-epoch time) in the given unit.

Unit must be one of `second`, `millisecond`, `microsecond`, `nanosecond`,
`native` (`type_error` otherwise).
""".
-spec systemTime(time_unit()) -> integer().
systemTime(Unit) ->
    erlang:system_time(check_unit('systemTime:', unit, Unit)).

-doc """
Convert a time value from one unit to another.

Precision may be lost converting to a coarser unit (1500 ms -> 1 s).
`T` must be an Integer; both units must be one of `second`,
`millisecond`, `microsecond`, `nanosecond`, `native` (`type_error`
otherwise).
""".
-spec convertTime(integer(), time_unit(), time_unit()) -> integer().
convertTime(T, _From, _To) when not is_integer(T) ->
    beamtalk_error:raise_type_error('Time', 'convertTime:from:to:', <<"t must be an Integer">>);
convertTime(T, From, To) ->
    erlang:convert_time_unit(
        T,
        check_unit('convertTime:from:to:', from, From),
        check_unit('convertTime:from:to:', to, To)
    ).

-doc """
Check a Beamtalk time-unit Symbol, answered as an atom by the delegate call.

Only the 5 canonical units are accepted — Erlang's legacy aliases
(`seconds`, `milli_seconds`, `micro_seconds`, `nano_seconds`) are rejected
with `type_error`, as is anything else.
""".
-spec check_unit(atom(), atom(), term()) -> time_unit().
check_unit(_Selector, _Arg, Unit) when
    Unit =:= second;
    Unit =:= millisecond;
    Unit =:= microsecond;
    Unit =:= nanosecond;
    Unit =:= native
->
    Unit;
check_unit(Selector, Arg, _Other) ->
    beamtalk_error:raise_type_error('Time', Selector, unit_hint(Arg)).

-doc "The `type_error` hint for a bad unit, naming the argument and valid units.".
-spec unit_hint(atom()) -> binary().
unit_hint(Arg) ->
    iolist_to_binary([
        atom_to_binary(Arg),
        " must be a Symbol: #second | #millisecond | #microsecond | #nanosecond | #native"
    ]).
