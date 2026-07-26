%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_datetime).

%%% **DDD Context:** Object System Context

-moduledoc """
DateTime class implementation — date/time via Erlang calendar/os modules.

Provides class-side constructors for creating UTC or fixed-offset datetime
values and instance methods for access, arithmetic, comparison, formatting,
and parsing.

DateTime objects are represented as tagged maps:
```
#{
  '$beamtalk_class' => 'DateTime',
  year          => integer(),
  month         => 1..12,
  day           => 1..31,
  hour          => 0..23,
  minute        => 0..59,
  second        => 0..59,
  offsetMinutes => integer()   %% UTC offset of the wall-clock fields above
}
```

`year`..`second` are the *wall-clock* fields as given (or parsed); they are
NOT normalized to UTC. `offsetMinutes` records the UTC offset that the
wall-clock fields are expressed in (0 for UTC/`Z`). The instant a DateTime
represents is `wall_clock_as_gregorian_seconds - offsetMinutes * 60`, so
comparisons and arithmetic (`asTimestamp`, `<`, `diffSeconds:`, ...) always
compare/operate on the underlying instant, never on the raw wall-clock
fields directly.

**Timezone scope decision:** only fixed UTC offsets are supported
(`offsetMinutes`, `toOffset:`). OTP ships no IANA tzdata, so named
timezones (e.g. `"America/New_York"`, with DST rules) are out of scope for
this module — callers that need a named zone's offset must supply it
(e.g. from a browser's `Intl.DateTimeFormat` or a server-side tzdata
lookup) and pass it to `toOffset:`. Revisit if/when a tzdata dependency is
adopted.
""".

%% Class methods
-export([now/0, 'monotonicNow'/0]).
-export(['year:month:day:'/3, 'year:month:day:hour:minute:second:'/6]).
-export(['year:month:day:hour:minute:second:offsetMinutes:'/7]).
-export(['fromTimestamp:'/1, 'fromString:'/1]).
-export(['parse:format:'/2]).

%% Instance methods
-export([year/1, month/1, day/1, hour/1, minute/1, second/1, 'offsetMinutes'/1]).
-export(['asTimestamp'/1, 'asString'/1, 'printString'/1]).
-export(['addSeconds:'/2, 'addDays:'/2, 'addDuration:'/2, 'diffSeconds:'/2]).
-export(['<'/2, '>'/2, '=<'/2, '>='/2, '=:='/2, '/='/2, '-'/2]).
-export(['toUtc'/1, 'toOffset:'/2, 'format:'/2]).

%% FFI shims for (Erlang beamtalk_datetime) dispatch — also the functions
%% `self delegate` actually calls for keyword selectors (first keyword,
%% colon stripped; see docs/beamtalk-native-erlang.md "The naming rule").
-export([year/3, year/6, year/7, fromTimestamp/1, fromString/1]).
-export([addSeconds/2, addDays/2, addDuration/2, diffSeconds/2, subtract/2]).
-export([lt/2, gt/2, lte/2, gte/2, eql/2, neq/2, sneq/2]).
-export([toOffset/2, format/2, parse/2]).

-type field() :: year | month | day | hour | minute | second.
-type t() :: #{'$beamtalk_class' := 'DateTime', atom() => term()}.
-export_type([t/0]).

%% Gregorian seconds at Unix epoch (1970-01-01 00:00:00)
-define(EPOCH_GREGORIAN, 62167219200).

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

-doc "Current UTC time as a DateTime.".
-spec now() -> t().
now() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    make_datetime(Y, Mo, D, H, Mi, S).

-doc "Monotonic clock value in nanoseconds (Integer, not a DateTime).".
-spec 'monotonicNow'() -> integer().
'monotonicNow'() ->
    erlang:monotonic_time(nanosecond).

-doc "Construct a DateTime from year, month, day (time defaults to 00:00:00).".
-spec 'year:month:day:'(integer(), Month :: integer(), Day :: integer()) -> t().
'year:month:day:'(Y, Mo, D) when is_integer(Y), is_integer(Mo), is_integer(D) ->
    validate_date(Y, Mo, D, 'year:month:day:'),
    make_datetime(Y, Mo, D, 0, 0, 0);
'year:month:day:'(_, _, _) ->
    raise_type_error('year:month:day:', <<"Arguments must be Integers">>).

-doc "Construct a DateTime from year, month, day, hour, minute, second.".
-spec 'year:month:day:hour:minute:second:'(
    integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer()
) -> t().
'year:month:day:hour:minute:second:'(Y, Mo, D, H, Mi, S) when
    is_integer(Y),
    is_integer(Mo),
    is_integer(D),
    is_integer(H),
    is_integer(Mi),
    is_integer(S)
->
    validate_date(Y, Mo, D, 'year:month:day:hour:minute:second:'),
    validate_time(H, Mi, S, 'year:month:day:hour:minute:second:'),
    make_datetime(Y, Mo, D, H, Mi, S);
'year:month:day:hour:minute:second:'(_, _, _, _, _, _) ->
    raise_type_error('year:month:day:hour:minute:second:', <<"Arguments must be Integers">>).

-doc """
Construct a DateTime from year, month, day, hour, minute, second, and a
fixed UTC offset (in minutes; e.g. 120 for `+02:00`, -300 for `-05:00`).

The wall-clock fields are stored as given (not normalized to UTC) — use
`toUtc` to convert to the equivalent UTC representation.

## Examples
```beamtalk
DateTime year: 2026 month: 7 day: 25 hour: 14 minute: 30 second: 0 offsetMinutes: 120
// => DateTime(2026-07-25T14:30:00+02:00)
```
""".
-spec 'year:month:day:hour:minute:second:offsetMinutes:'(
    integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer(),
    OffsetMinutes :: integer()
) -> t().
'year:month:day:hour:minute:second:offsetMinutes:'(Y, Mo, D, H, Mi, S, Off) when
    is_integer(Y),
    is_integer(Mo),
    is_integer(D),
    is_integer(H),
    is_integer(Mi),
    is_integer(S),
    is_integer(Off)
->
    validate_date(Y, Mo, D, 'year:month:day:hour:minute:second:offsetMinutes:'),
    validate_time(H, Mi, S, 'year:month:day:hour:minute:second:offsetMinutes:'),
    validate_offset(Off, 'year:month:day:hour:minute:second:offsetMinutes:'),
    make_datetime(Y, Mo, D, H, Mi, S, Off);
'year:month:day:hour:minute:second:offsetMinutes:'(_, _, _, _, _, _, _) ->
    raise_type_error(
        'year:month:day:hour:minute:second:offsetMinutes:', <<"Arguments must be Integers">>
    ).

-doc "Construct a DateTime from a Unix epoch timestamp (seconds); UTC (offsetMinutes 0).".
-spec 'fromTimestamp:'(integer()) -> t().
'fromTimestamp:'(Ts) when is_integer(Ts) ->
    from_timestamp_with_offset(Ts, 0);
'fromTimestamp:'(_) ->
    raise_type_error('fromTimestamp:', <<"Argument must be an Integer (Unix epoch seconds)">>).

-doc """
Parse a strict ISO 8601 string into a DateTime.

Accepts an optional UTC offset suffix: `Z` (UTC), or `+HH:MM`/`-HH:MM`. The
parsed wall-clock fields and offset round-trip through `asString`.

## Examples
```beamtalk
DateTime fromString: "2026-07-25T14:30:00+02:00"
// => DateTime(2026-07-25T14:30:00+02:00)
```
""".
-spec 'fromString:'(binary()) -> t().
'fromString:'(Str) when is_binary(Str) ->
    case parse_iso8601(Str) of
        {ok, Y, Mo, D, H, Mi, S, Off} ->
            make_datetime(Y, Mo, D, H, Mi, S, Off);
        error ->
            Error0 = beamtalk_error:new(type_error, 'DateTime'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromString:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Expected ISO 8601 format: YYYY-MM-DDThh:mm:ss(Z|+hh:mm|-hh:mm)">>
            ),
            beamtalk_error:raise(Error2)
    end;
'fromString:'(_) ->
    raise_type_error('fromString:', <<"Argument must be a String">>).

-doc """
Parse `Str` using the pattern language documented at `format:` (the inverse
of `format:`).

Returns `Result ok: dateTime` on success, `Result error:` if `Str` doesn't
match `Pattern` or the parsed fields form an invalid date/time (e.g. month
13). Fields absent from the pattern default to: year 0, month 1, day 1,
hour/minute/second 0. The pattern has no offset token, so the result is
always UTC (offsetMinutes 0) — this is a fallible, Result-based counterpart
to the exception-raising `fromString:`.

## Examples
```beamtalk
DateTime parse: "2026-07-25 14:30:00" format: "yyyy-MM-dd HH:mm:ss"
// => Result ok: DateTime(2026-07-25T14:30:00Z)
(DateTime parse: "not a date" format: "yyyy-MM-dd") isOk
// => false
```
""".
-spec 'parse:format:'(binary(), binary()) -> beamtalk_result:t().
'parse:format:'(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    case parse_with_pattern(Str, Pattern) of
        {ok, Y, Mo, D, H, Mi, S} ->
            beamtalk_result:from_tagged_tuple({ok, make_datetime(Y, Mo, D, H, Mi, S, 0)});
        {error, Hint} ->
            Error0 = beamtalk_error:new(parse_error, 'DateTime'),
            Error1 = beamtalk_error:with_selector(Error0, 'parse:format:'),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'parse:format:'(_, _) ->
    raise_type_error('parse:format:', <<"Both arguments must be Strings">>).

%%% ============================================================================
%%% Instance Methods — Accessors
%%% ============================================================================

-spec year(t()) -> integer().
year(#{year := V}) -> V.

-spec month(t()) -> integer().
month(#{month := V}) -> V.

-spec day(t()) -> integer().
day(#{day := V}) -> V.

-spec hour(t()) -> integer().
hour(#{hour := V}) -> V.

-spec minute(t()) -> integer().
minute(#{minute := V}) -> V.

-spec second(t()) -> integer().
second(#{second := V}) -> V.

-doc "UTC offset (in minutes) of this DateTime's wall-clock fields (0 for UTC).".
-spec 'offsetMinutes'(t()) -> integer().
'offsetMinutes'(Self) -> maps:get(offsetMinutes, Self, 0).

%%% ============================================================================
%%% Instance Methods — Conversion
%%% ============================================================================

-doc "Convert to Unix epoch timestamp (seconds) — the instant, honoring offsetMinutes.".
-spec 'asTimestamp'(t()) -> integer().
'asTimestamp'(
    #{
        year := Y,
        month := Mo,
        day := D,
        hour := H,
        minute := Mi,
        second := S
    } = Self
) ->
    Off = maps:get(offsetMinutes, Self, 0),
    GregSec = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, S}}),
    GregSec - ?EPOCH_GREGORIAN - (Off * 60).

-doc "Format as ISO 8601 string, with a `Z` suffix for UTC or a `+hh:mm`/`-hh:mm` offset suffix.".
-spec 'asString'(t()) -> binary().
'asString'(
    #{
        year := Y,
        month := Mo,
        day := D,
        hour := H,
        minute := Mi,
        second := S
    } = Self
) ->
    Off = maps:get(offsetMinutes, Self, 0),
    DatePart = io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
        [Y, Mo, D, H, Mi, S]
    ),
    iolist_to_binary([DatePart, format_offset_suffix(Off)]).

-doc "Convert to the equivalent UTC (offsetMinutes 0) DateTime, same instant.".
-spec 'toUtc'(t()) -> t().
'toUtc'(Self) ->
    from_timestamp_with_offset('asTimestamp'(Self), 0).

-doc """
Convert to an equivalent DateTime expressed at a different fixed UTC offset
(in minutes). Represents the same instant; only the wall-clock fields and
offsetMinutes change.

## Examples
```beamtalk
(DateTime fromString: "2026-07-25T12:30:00Z") toOffset: 120
// => DateTime(2026-07-25T14:30:00+02:00)
```
""".
-spec 'toOffset:'(t(), OffsetMinutes :: integer()) -> t().
'toOffset:'(Self, Off) when is_integer(Off) ->
    validate_offset(Off, 'toOffset:'),
    from_timestamp_with_offset('asTimestamp'(Self), Off);
'toOffset:'(_, _) ->
    raise_type_error('toOffset:', <<"Argument must be an Integer (offset minutes)">>).

-doc "Human-readable representation.".
-spec 'printString'(t()) -> binary().
'printString'(Self) ->
    iolist_to_binary([<<"DateTime(">>, 'asString'(Self), <<")">>]).

-doc """
Format the wall-clock fields using a documented pattern language (a small
subset of CLDR/strftime-style date-time patterns; see also `parse:format:`).

Recognized pattern letters — a run of N identical letters controls width:
- `y`/`yy`/`yyyy` — year: unpadded / last-2-digits / zero-padded to N digits.
- `M`/`MM`, `d`/`dd`, `H`/`HH`, `m`/`mm`, `s`/`ss` — month, day, hour (24h),
  minute, second: unpadded / zero-padded to N digits.
Any other character (`-`, `:`, `T`, ` `, ...) is copied through literally.

This pattern language has **no UTC-offset token** — use `asString`/
`fromString:` for offset-aware ISO 8601 formatting/parsing. For
`parse:format:`, put a literal separator between two adjacent unpadded
numeric tokens (e.g. `M/d`, not `Md`) — without one, an unpadded field's
`\\d{1,N}` regex greedily consumes digits belonging to its neighbor.

## Examples
```beamtalk
(DateTime year: 2026 month: 7 day: 25 hour: 14 minute: 30 second: 0)
  format: "yyyy-MM-dd HH:mm:ss"
// => "2026-07-25 14:30:00"
```
""".
-spec 'format:'(t(), Pattern :: binary()) -> binary().
'format:'(Self, Pattern) when is_binary(Pattern) ->
    Tokens = tokenize_pattern(Pattern),
    iolist_to_binary([render_pattern_token(Token, Self) || Token <- Tokens]);
'format:'(_, _) ->
    raise_type_error('format:', <<"Argument must be a String pattern">>).

%%% ============================================================================
%%% Instance Methods — Arithmetic
%%% ============================================================================

-doc "Add seconds, return new DateTime. Preserves self's offsetMinutes.".
-spec 'addSeconds:'(t(), Secs :: integer()) -> t().
'addSeconds:'(Self, Secs) when is_integer(Secs) ->
    Ts = 'asTimestamp'(Self) + Secs,
    Off = maps:get(offsetMinutes, Self, 0),
    from_timestamp_with_offset(Ts, Off);
'addSeconds:'(_, _) ->
    raise_type_error('addSeconds:', <<"Argument must be an Integer">>).

-doc "Add days, return new DateTime.".
-spec 'addDays:'(t(), Days :: integer()) -> t().
'addDays:'(Self, Days) when is_integer(Days) ->
    'addSeconds:'(Self, Days * 86400);
'addDays:'(_, _) ->
    raise_type_error('addDays:', <<"Argument must be an Integer">>).

-doc """
Add a Duration, return a new DateTime.

DateTime has second resolution; any sub-second milliseconds in the
Duration are truncated toward zero.
""".
-spec 'addDuration:'(t(), beamtalk_duration:t()) -> t().
'addDuration:'(Self, #{'$beamtalk_class' := 'Duration'} = D) ->
    'addSeconds:'(Self, beamtalk_duration:'asSeconds'(D));
'addDuration:'(_, _) ->
    raise_type_error('addDuration:', <<"Argument must be a Duration">>).

-doc """
Difference between two DateTimes as a Duration.

`A - B` is positive when A is later than B.
""".
-spec '-'(t(), t()) -> beamtalk_duration:t().
'-'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    beamtalk_duration:'seconds:'('diffSeconds:'(Self, Other));
'-'(_, _) ->
    raise_type_error('-', <<"Argument must be a DateTime">>).

-doc "Difference in seconds between this and another DateTime.".
-spec 'diffSeconds:'(t(), t()) -> integer().
'diffSeconds:'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) - 'asTimestamp'(Other);
'diffSeconds:'(_, _) ->
    raise_type_error('diffSeconds:', <<"Argument must be a DateTime">>).

%%% ============================================================================
%%% Instance Methods — Comparison
%%% ============================================================================

-spec '<'(t(), t()) -> boolean().
'<'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) < 'asTimestamp'(Other);
'<'(_, _) ->
    raise_type_error('<', <<"Argument must be a DateTime">>).

-spec '>'(t(), t()) -> boolean().
'>'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) > 'asTimestamp'(Other);
'>'(_, _) ->
    raise_type_error('>', <<"Argument must be a DateTime">>).

-spec '=<'(t(), t()) -> boolean().
'=<'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) =< 'asTimestamp'(Other);
'=<'(_, _) ->
    raise_type_error('=<', <<"Argument must be a DateTime">>).

-spec '>='(t(), t()) -> boolean().
'>='(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) >= 'asTimestamp'(Other);
'>='(_, _) ->
    raise_type_error('>=', <<"Argument must be a DateTime">>).

-spec '=:='(t(), t()) -> boolean().
'=:='(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) =:= 'asTimestamp'(Other);
'=:='(_, _) ->
    raise_type_error('=:=', <<"Argument must be a DateTime">>).

-spec '/='(t(), t()) -> boolean().
'/='(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) /= 'asTimestamp'(Other);
'/='(_, _) ->
    raise_type_error('/=', <<"Argument must be a DateTime">>).

%%% ============================================================================
%%% FFI Shims — (Erlang beamtalk_datetime) dispatch
%%% ============================================================================
%%
%% selector_to_function/1 extracts the first keyword segment as the function name.
%% These shims bridge camelCase/short FFI names to the existing colon-suffix
%% and operator-named implementations.

%% `year:month:day:` → strips to `year`, arity 3
-spec year(integer(), Month :: integer(), Day :: integer()) -> t().
year(Y, Mo, D) -> 'year:month:day:'(Y, Mo, D).

%% `year:month:day:hour:minute:second:` → strips to `year`, arity 6
-spec year(
    integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer()
) -> t().
year(Y, Mo, D, H, Mi, S) -> 'year:month:day:hour:minute:second:'(Y, Mo, D, H, Mi, S).

%% `year:month:day:hour:minute:second:offsetMinutes:` → strips to `year`, arity 7
-spec year(
    integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer(),
    OffsetMinutes :: integer()
) -> t().
year(Y, Mo, D, H, Mi, S, Off) ->
    'year:month:day:hour:minute:second:offsetMinutes:'(Y, Mo, D, H, Mi, S, Off).

%% `fromTimestamp:` → strips to `fromTimestamp`, arity 1
fromTimestamp(Ts) -> 'fromTimestamp:'(Ts).

%% `fromString:` → strips to `fromString`, arity 1
fromString(Str) -> 'fromString:'(Str).

%% `addSeconds:secs:` → strips to `addSeconds`, arity 2
-spec addSeconds(t(), Secs :: integer()) -> t().
addSeconds(Self, Secs) -> 'addSeconds:'(Self, Secs).

%% `addDays:days:` → strips to `addDays`, arity 2
-spec addDays(t(), Days :: integer()) -> t().
addDays(Self, Days) -> 'addDays:'(Self, Days).

%% `addDuration:with:` → strips to `addDuration`, arity 2
-spec addDuration(t(), beamtalk_duration:t()) -> t().
addDuration(Self, D) -> 'addDuration:'(Self, D).

%% `diffSeconds:with:` → strips to `diffSeconds`, arity 2
diffSeconds(Self, Other) -> 'diffSeconds:'(Self, Other).

%% `subtract:with:` → strips to `subtract`, arity 2 (DateTime difference)
-spec subtract(t(), t()) -> beamtalk_duration:t().
subtract(Self, Other) -> '-'(Self, Other).

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

%% `eql:with:` → strips to `eql`, arity 2
-spec eql(t(), t()) -> boolean().
eql(Self, Other) -> '=:='(Self, Other).

%% `neq:with:` → strips to `neq`, arity 2
-spec neq(t(), t()) -> boolean().
neq(Self, Other) -> '/='(Self, Other).

%% `sneq:with:` → strips to `sneq`, arity 2 (strict inequality)
-spec sneq(t(), t()) -> boolean().
sneq(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) =/= 'asTimestamp'(Other);
sneq(_, _) ->
    raise_type_error(sneq, <<"Argument must be a DateTime">>).

%% `toOffset:` → strips to `toOffset`, arity 2 — this is what `self delegate`
%% for `toOffset: minutes` actually calls (first keyword, colon stripped).
-spec toOffset(t(), OffsetMinutes :: integer()) -> t().
toOffset(Self, Off) -> 'toOffset:'(Self, Off).

%% `format:` → strips to `format`, arity 2 — what `self delegate` calls.
-spec format(t(), Pattern :: binary()) -> binary().
format(Self, Pattern) -> 'format:'(Self, Pattern).

%% `parse:format:` → strips to `parse`, arity 2 (class method, no self) —
%% what `self delegate` calls.
-spec parse(binary(), binary()) -> beamtalk_result:t().
parse(Str, Pattern) -> 'parse:format:'(Str, Pattern).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-spec make_datetime(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> t().
make_datetime(Y, Mo, D, H, Mi, S) ->
    make_datetime(Y, Mo, D, H, Mi, S, 0).

-spec make_datetime(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> t().
make_datetime(Y, Mo, D, H, Mi, S, Off) ->
    #{
        '$beamtalk_class' => 'DateTime',
        year => Y,
        month => Mo,
        day => D,
        hour => H,
        minute => Mi,
        second => S,
        offsetMinutes => Off
    }.

-doc "Build a DateTime for the instant `Ts` (Unix epoch seconds), expressed at `OffsetMinutes`.".
-spec from_timestamp_with_offset(integer(), integer()) -> t().
from_timestamp_with_offset(Ts, OffsetMinutes) ->
    GregSec = Ts + ?EPOCH_GREGORIAN + (OffsetMinutes * 60),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(GregSec),
    make_datetime(Y, Mo, D, H, Mi, S, OffsetMinutes).

-spec format_offset_suffix(integer()) -> binary().
format_offset_suffix(0) ->
    <<"Z">>;
format_offset_suffix(Off) ->
    Sign =
        case Off < 0 of
            true -> $-;
            false -> $+
        end,
    Abs = abs(Off),
    HH = Abs div 60,
    MM = Abs rem 60,
    iolist_to_binary(io_lib:format("~c~2..0B:~2..0B", [Sign, HH, MM])).

-spec validate_date(integer(), integer(), integer(), atom()) -> ok.
validate_date(Y, Mo, D, Selector) ->
    case calendar:valid_date(Y, Mo, D) of
        true ->
            ok;
        false ->
            Msg = iolist_to_binary(io_lib:format("Invalid date: ~p-~p-~p", [Y, Mo, D])),
            Error0 = beamtalk_error:new(type_error, 'DateTime'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(Error1, Msg),
            beamtalk_error:raise(Error2)
    end.

-spec validate_time(integer(), integer(), integer(), atom()) -> ok.
validate_time(H, Mi, S, _Selector) when
    H >= 0, H =< 23, Mi >= 0, Mi =< 59, S >= 0, S =< 59
->
    ok;
validate_time(H, Mi, S, Selector) ->
    Msg = iolist_to_binary(io_lib:format("Invalid time: ~p:~p:~p", [H, Mi, S])),
    Error0 = beamtalk_error:new(type_error, 'DateTime'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Msg),
    beamtalk_error:raise(Error2).

-spec validate_offset(integer(), atom()) -> ok.
validate_offset(Off, _Selector) when is_integer(Off), Off > -1440, Off < 1440 ->
    ok;
validate_offset(Off, Selector) ->
    Msg = iolist_to_binary(
        io_lib:format(
            "Invalid offsetMinutes: ~p (must be strictly between -1440 and 1440, i.e. within a day of UTC)",
            [Off]
        )
    ),
    Error0 = beamtalk_error:new(type_error, 'DateTime'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Msg),
    beamtalk_error:raise(Error2).

-spec parse_iso8601(binary()) ->
    {ok, integer(), integer(), integer(), integer(), integer(), integer(), integer()}
    | error.
parse_iso8601(Str) ->
    %% Accept YYYY-MM-DDThh:mm:ss with an optional Z or +hh:mm/-hh:mm offset suffix.
    case
        re:run(
            Str,
            <<"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(Z|[+-]\\d{2}:\\d{2})?$">>,
            [{capture, all_but_first, binary}]
        )
    of
        {match, [YB, MoB, DB, HB, MiB, SB | OffCaptures]} ->
            %% NOTE: re:run/PCRE drops a trailing unmatched optional capture group
            %% entirely from the result list, rather than returning an empty
            %% binary for it — so OffCaptures is [] (no offset) or [OffB].
            OffB =
                case OffCaptures of
                    [] -> <<>>;
                    [B] -> B
                end,
            Y = binary_to_integer(YB),
            Mo = binary_to_integer(MoB),
            D = binary_to_integer(DB),
            H = binary_to_integer(HB),
            Mi = binary_to_integer(MiB),
            S = binary_to_integer(SB),
            case parse_offset_suffix(OffB) of
                {ok, Off} ->
                    case calendar:valid_date(Y, Mo, D) of
                        true when H >= 0, H =< 23, Mi >= 0, Mi =< 59, S >= 0, S =< 59 ->
                            {ok, Y, Mo, D, H, Mi, S, Off};
                        _ ->
                            error
                    end;
                error ->
                    error
            end;
        nomatch ->
            error
    end.

-spec parse_offset_suffix(binary()) -> {ok, integer()} | error.
parse_offset_suffix(<<>>) ->
    {ok, 0};
parse_offset_suffix(<<"Z">>) ->
    {ok, 0};
parse_offset_suffix(<<Sign, HHB:2/binary, $:, MMB:2/binary>>) when Sign =:= $+; Sign =:= $- ->
    HH = binary_to_integer(HHB),
    MM = binary_to_integer(MMB),
    case HH =< 23 andalso MM =< 59 of
        true ->
            Total = HH * 60 + MM,
            {ok,
                case Sign of
                    $+ -> Total;
                    $- -> -Total
                end};
        false ->
            error
    end;
parse_offset_suffix(_) ->
    error.

-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    beamtalk_error:raise_type_error('DateTime', Selector, Hint).

%%% ============================================================================
%%% Internal Functions — Pattern-based format:/parse:format:
%%% ============================================================================
%%
%% Pattern language: runs of the letters y/M/d/H/m/s (see `format:` doc for
%% semantics); any other character is a literal, copied through as-is on
%% format and matched exactly on parse. Deliberately has no offset token —
%% offset-aware round-trips go through asString/fromString: instead.

-spec tokenize_pattern(binary()) -> [{field(), pos_integer()} | {lit, binary()}].
tokenize_pattern(Pattern) ->
    group_pattern_chars(unicode:characters_to_list(Pattern)).

-spec group_pattern_chars([non_neg_integer()]) -> [{field(), pos_integer()} | {lit, binary()}].
group_pattern_chars([]) ->
    [];
group_pattern_chars([C | _] = Chars) when
    C =:= $y; C =:= $M; C =:= $d; C =:= $H; C =:= $m; C =:= $s
->
    {Run, Rest} = lists:splitwith(fun(X) -> X =:= C end, Chars),
    [{pattern_field(C), length(Run)} | group_pattern_chars(Rest)];
group_pattern_chars(Chars) ->
    {Run, Rest} = lists:splitwith(
        fun(X) -> not lists:member(X, [$y, $M, $d, $H, $m, $s]) end, Chars
    ),
    [{lit, unicode:characters_to_binary(Run)} | group_pattern_chars(Rest)].

-spec pattern_field(non_neg_integer()) -> field().
pattern_field($y) -> year;
pattern_field($M) -> month;
pattern_field($d) -> day;
pattern_field($H) -> hour;
pattern_field($m) -> minute;
pattern_field($s) -> second.

-spec render_pattern_token({field(), pos_integer()} | {lit, binary()}, t()) -> iodata().
render_pattern_token({lit, Text}, _Self) ->
    Text;
render_pattern_token({year, Count}, #{year := Y}) ->
    format_year(Y, Count);
render_pattern_token({Field, Count}, Self) ->
    format_padded(maps:get(Field, Self), Count).

-spec format_year(integer(), pos_integer()) -> binary().
format_year(Y, 1) -> integer_to_binary(Y);
format_year(Y, 2) -> zero_pad(Y rem 100, 2);
format_year(Y, Count) -> zero_pad(Y, Count).

-spec format_padded(integer(), pos_integer()) -> binary().
format_padded(V, 1) -> integer_to_binary(V);
format_padded(V, Count) -> zero_pad(V, Count).

-spec zero_pad(integer(), pos_integer()) -> binary().
zero_pad(V, Width) ->
    FmtStr = "~" ++ integer_to_list(Width) ++ "..0B",
    iolist_to_binary(io_lib:format(FmtStr, [V])).

-spec parse_with_pattern(binary(), binary()) ->
    {ok, integer(), integer(), integer(), integer(), integer(), integer()} | {error, binary()}.
parse_with_pattern(Str, Pattern) ->
    Tokens = tokenize_pattern(Pattern),
    {Regex, Fields} = build_pattern_regex(Tokens),
    case re:run(Str, Regex, [{capture, all_but_first, binary}]) of
        {match, Captures} when length(Captures) =:= length(Fields) ->
            Defaults = #{year => 0, month => 1, day => 1, hour => 0, minute => 0, second => 0},
            FieldMap =
                lists:foldl(
                    fun({{Field, Count}, Cap}, Acc) ->
                        Acc#{Field => resolve_field_value(Field, Count, Cap)}
                    end,
                    Defaults,
                    lists:zip(Fields, Captures)
                ),
            #{year := Y, month := Mo, day := D, hour := H, minute := Mi, second := S} = FieldMap,
            case
                calendar:valid_date(Y, Mo, D) andalso
                    H >= 0 andalso H =< 23 andalso Mi >= 0 andalso Mi =< 59 andalso S >= 0 andalso
                    S =< 59
            of
                true -> {ok, Y, Mo, D, H, Mi, S};
                false -> {error, <<"Parsed date/time is out of range">>}
            end;
        _ ->
            {error, <<"Input does not match the given pattern">>}
    end.

-spec resolve_field_value(field(), pos_integer(), binary()) -> integer().
resolve_field_value(year, 2, Cap) -> 2000 + binary_to_integer(Cap);
resolve_field_value(_Field, _Count, Cap) -> binary_to_integer(Cap).

-spec build_pattern_regex([{field(), pos_integer()} | {lit, binary()}]) ->
    {binary(), [{field(), pos_integer()}]}.
build_pattern_regex(Tokens) ->
    {RegexParts, FieldsRev} =
        lists:foldl(
            fun
                ({lit, Text}, {RAcc, FAcc}) ->
                    {[RAcc, escape_regex(Text)], FAcc};
                ({Field, Count}, {RAcc, FAcc}) ->
                    {[RAcc, field_regex(Field, Count)], [{Field, Count} | FAcc]}
            end,
            {[], []},
            Tokens
        ),
    {iolist_to_binary([<<"^">>, RegexParts, <<"$">>]), lists:reverse(FieldsRev)}.

-spec field_regex(field(), pos_integer()) -> binary().
field_regex(year, 2) ->
    <<"(\\d{2})">>;
field_regex(year, Count) when Count >= 3 ->
    iolist_to_binary(io_lib:format("(\\d{~B})", [Count]));
field_regex(year, _Count) ->
    <<"(\\d{1,9})">>;
field_regex(_Field, Count) when Count >= 2 ->
    iolist_to_binary(io_lib:format("(\\d{~B})", [Count]));
field_regex(_Field, _Count) ->
    <<"(\\d{1,2})">>.

-spec escape_regex(binary()) -> binary().
escape_regex(Text) ->
    <<<<(escape_regex_char(C))/binary>> || <<C/utf8>> <= Text>>.

-spec escape_regex_char(non_neg_integer()) -> binary().
escape_regex_char(C) when
    C =:= $.;
    C =:= $^;
    C =:= $$;
    C =:= $*;
    C =:= $+;
    C =:= $?;
    C =:= $(;
    C =:= $);
    C =:= $[;
    C =:= $];
    C =:= ${;
    C =:= $};
    C =:= $|;
    C =:= $\\
->
    <<$\\, C/utf8>>;
escape_regex_char(C) ->
    <<C/utf8>>.
