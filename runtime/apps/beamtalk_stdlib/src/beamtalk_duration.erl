%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_duration).

%%% **DDD Context:** Object System Context

-moduledoc """
Duration class implementation — a span of time stored as total milliseconds.

Provides class-side constructors (`milliseconds:`, `seconds:`, `minutes:`,
`hours:`, `days:`, `fromString:`), integer conversions, arithmetic,
comparison, and formatting (human-readable `printString` and ISO 8601
`asString`).

Duration objects are represented as tagged maps:
```
#{
  '$beamtalk_class' => 'Duration',
  millis           => integer()
}
```

## Conversion semantics

Constructors accept an Integer or a Float; Float inputs are scaled to
milliseconds and rounded to the nearest integer (`Duration seconds: 1.5`
is exactly 1500 ms). The unit accessors (`asSeconds`, `asMinutes`,
`asHours`, `totalDays`) return Integers, truncating toward zero — the
Float alternative was rejected so deadline arithmetic stays exact.

## FFI Shims

`beamtalk_erlang_proxy:direct_call/3` derives the Erlang function name from
the first keyword of the Beamtalk selector. Operator selectors (`+`, `<`, …)
are not valid Erlang function names, so the Beamtalk operators dispatch
through named shims (`add`, `subtract`, `multiply`, `lt`, `gt`, `lte`,
`gte`, `eql`, `neq`, `sneq`).
""".

%% Class methods (canonical colon forms)
-export(['milliseconds:'/1, 'seconds:'/1, 'minutes:'/1, 'hours:'/1, 'days:'/1]).
-export(['fromString:'/1]).

%% Instance methods
-export(['asMilliseconds'/1, 'asSeconds'/1, 'asMinutes'/1, 'asHours'/1, 'totalDays'/1]).
-export(['isZero'/1, 'isNegative'/1]).
-export(['asString'/1, 'printString'/1]).
-export(['+'/2, '-'/2, '*'/2]).
-export(['<'/2, '>'/2, '=<'/2, '>='/2, '=:='/2, '/='/2]).

%% FFI shims for (Erlang beamtalk_duration) dispatch
-export([milliseconds/1, seconds/1, minutes/1, hours/1, days/1, fromString/1]).
-export([add/2, subtract/2, multiply/2]).
-export([lt/2, gt/2, lte/2, gte/2, eql/2, neq/2, sneq/2]).

%% Cross-module helper (beamtalk_timer, beamtalk_datetime)
-export([to_millis/1, is_duration/1]).

-type t() :: #{'$beamtalk_class' := 'Duration', millis := integer()}.
-export_type([t/0]).

-define(MS_PER_SECOND, 1000).
-define(MS_PER_MINUTE, 60000).
-define(MS_PER_HOUR, 3600000).
-define(MS_PER_DAY, 86400000).

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

-doc "Construct a Duration from milliseconds (Integer, or Float rounded).".
-spec 'milliseconds:'(number()) -> t().
'milliseconds:'(N) -> from_scaled(N, 1, 'milliseconds:').

-doc "Construct a Duration from seconds (Integer, or Float rounded to ms).".
-spec 'seconds:'(number()) -> t().
'seconds:'(N) -> from_scaled(N, ?MS_PER_SECOND, 'seconds:').

-doc "Construct a Duration from minutes (Integer, or Float rounded to ms).".
-spec 'minutes:'(number()) -> t().
'minutes:'(N) -> from_scaled(N, ?MS_PER_MINUTE, 'minutes:').

-doc "Construct a Duration from hours (Integer, or Float rounded to ms).".
-spec 'hours:'(number()) -> t().
'hours:'(N) -> from_scaled(N, ?MS_PER_HOUR, 'hours:').

-doc "Construct a Duration from days (Integer, or Float rounded to ms).".
-spec 'days:'(number()) -> t().
'days:'(N) -> from_scaled(N, ?MS_PER_DAY, 'days:').

-doc """
Parse an ISO 8601 duration string, e.g. `PT1H30M`, `P1DT2H`, `PT0.250S`.

Accepts an optional leading `-` for negative durations. Weeks/months/years
are not supported (calendar-dependent). Fractional seconds are supported
up to millisecond precision.
""".
-spec 'fromString:'(binary()) -> t().
'fromString:'(Str) when is_binary(Str) ->
    case parse_iso8601(Str) of
        {ok, Ms} ->
            make_duration(Ms);
        error ->
            Error0 = beamtalk_error:new(type_error, 'Duration'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromString:'),
            Error2 = beamtalk_error:with_hint(
                Error1, <<"Expected ISO 8601 duration format, e.g. PT1H30M15S or P1DT2H">>
            ),
            beamtalk_error:raise(Error2)
    end;
'fromString:'(_) ->
    raise_type_error('fromString:', <<"Argument must be a String">>).

%%% ============================================================================
%%% Instance Methods — Conversions
%%% ============================================================================

-doc "Total milliseconds (exact Integer).".
-spec 'asMilliseconds'(t()) -> integer().
'asMilliseconds'(#{'$beamtalk_class' := 'Duration', millis := Ms}) -> Ms.

-doc "Total whole seconds (Integer, truncated toward zero).".
-spec 'asSeconds'(t()) -> integer().
'asSeconds'(#{'$beamtalk_class' := 'Duration', millis := Ms}) ->
    trunc_div(Ms, ?MS_PER_SECOND).

-doc "Total whole minutes (Integer, truncated toward zero).".
-spec 'asMinutes'(t()) -> integer().
'asMinutes'(#{'$beamtalk_class' := 'Duration', millis := Ms}) ->
    trunc_div(Ms, ?MS_PER_MINUTE).

-doc "Total whole hours (Integer, truncated toward zero).".
-spec 'asHours'(t()) -> integer().
'asHours'(#{'$beamtalk_class' := 'Duration', millis := Ms}) ->
    trunc_div(Ms, ?MS_PER_HOUR).

-doc "Total whole days (Integer, truncated toward zero).".
-spec 'totalDays'(t()) -> integer().
'totalDays'(#{'$beamtalk_class' := 'Duration', millis := Ms}) ->
    trunc_div(Ms, ?MS_PER_DAY).

%%% ============================================================================
%%% Instance Methods — Predicates
%%% ============================================================================

-doc "True if this duration is exactly zero.".
-spec 'isZero'(t()) -> boolean().
'isZero'(#{'$beamtalk_class' := 'Duration', millis := Ms}) -> Ms =:= 0.

-doc "True if this duration is negative.".
-spec 'isNegative'(t()) -> boolean().
'isNegative'(#{'$beamtalk_class' := 'Duration', millis := Ms}) -> Ms < 0.

%%% ============================================================================
%%% Instance Methods — Formatting
%%% ============================================================================

-doc """
Format as an ISO 8601 duration string, e.g. `PT1H30M`, `P1DT2H`, `PT0.250S`.

Zero is `PT0S`; negative durations get a leading `-`. Round-trips through
`fromString:`.
""".
-spec 'asString'(t()) -> binary().
'asString'(#{'$beamtalk_class' := 'Duration', millis := 0}) ->
    <<"PT0S">>;
'asString'(#{'$beamtalk_class' := 'Duration', millis := Ms}) ->
    Sign =
        case Ms < 0 of
            true -> <<"-">>;
            false -> <<>>
        end,
    {D, H, Mi, S, MsRem} = decompose(abs(Ms)),
    DatePart =
        case D of
            0 -> <<>>;
            _ -> [integer_to_binary(D), <<"D">>]
        end,
    TimeUnits = [
        {H, <<"H">>},
        {Mi, <<"M">>}
    ],
    TimePart0 = [[integer_to_binary(V), U] || {V, U} <- TimeUnits, V =/= 0],
    SecondsPart = format_iso_seconds(S, MsRem),
    TimePart =
        case {TimePart0, SecondsPart} of
            {[], []} -> [];
            _ -> [<<"T">>, TimePart0, SecondsPart]
        end,
    iolist_to_binary([Sign, <<"P">>, DatePart, TimePart]).

-doc """
Human-readable representation, e.g. `1h 30m`, `1d 2h`, `1s 500ms`, `0ms`.

Zero components are omitted; negative durations get a leading `-`.
""".
-spec 'printString'(t()) -> binary().
'printString'(#{'$beamtalk_class' := 'Duration', millis := 0}) ->
    <<"0ms">>;
'printString'(#{'$beamtalk_class' := 'Duration', millis := Ms}) ->
    Sign =
        case Ms < 0 of
            true -> <<"-">>;
            false -> <<>>
        end,
    {D, H, Mi, S, MsRem} = decompose(abs(Ms)),
    Units = [
        {D, <<"d">>},
        {H, <<"h">>},
        {Mi, <<"m">>},
        {S, <<"s">>},
        {MsRem, <<"ms">>}
    ],
    Parts = [[integer_to_binary(V), U] || {V, U} <- Units, V =/= 0],
    iolist_to_binary([Sign, lists:join(<<" ">>, Parts)]).

%%% ============================================================================
%%% Instance Methods — Arithmetic
%%% ============================================================================

-doc "Sum of two Durations.".
-spec '+'(t(), t()) -> t().
'+'(#{'$beamtalk_class' := 'Duration', millis := A}, #{
    '$beamtalk_class' := 'Duration', millis := B
}) ->
    make_duration(A + B);
'+'(_, _) ->
    raise_type_error('+', <<"Argument must be a Duration">>).

-doc "Difference of two Durations (may be negative).".
-spec '-'(t(), t()) -> t().
'-'(#{'$beamtalk_class' := 'Duration', millis := A}, #{
    '$beamtalk_class' := 'Duration', millis := B
}) ->
    make_duration(A - B);
'-'(_, _) ->
    raise_type_error('-', <<"Argument must be a Duration">>).

-doc "Scale a Duration by an Integer or Float scalar (Float rounds to ms).".
-spec '*'(t(), number()) -> t().
'*'(#{'$beamtalk_class' := 'Duration', millis := A}, Scalar) when is_integer(Scalar) ->
    make_duration(A * Scalar);
'*'(#{'$beamtalk_class' := 'Duration', millis := A}, Scalar) when is_float(Scalar) ->
    make_duration(round(A * Scalar));
'*'(_, _) ->
    raise_type_error('*', <<"Argument must be an Integer or Float scalar">>).

%%% ============================================================================
%%% Instance Methods — Comparison
%%% ============================================================================

-spec '<'(t(), t()) -> boolean().
'<'(Self, Other) -> compare('<', Self, Other).

-spec '>'(t(), t()) -> boolean().
'>'(Self, Other) -> compare('>', Self, Other).

-spec '=<'(t(), t()) -> boolean().
'=<'(Self, Other) -> compare('=<', Self, Other).

-spec '>='(t(), t()) -> boolean().
'>='(Self, Other) -> compare('>=', Self, Other).

-spec '=:='(t(), t()) -> boolean().
'=:='(Self, Other) -> compare('=:=', Self, Other).

-spec '/='(t(), t()) -> boolean().
'/='(Self, Other) -> compare('/=', Self, Other).

%%% ============================================================================
%%% FFI Shims — (Erlang beamtalk_duration) dispatch
%%% ============================================================================

-doc "FFI shim: `(Erlang beamtalk_duration) milliseconds: n`".
-spec milliseconds(number()) -> t().
milliseconds(N) -> 'milliseconds:'(N).

-doc "FFI shim: `(Erlang beamtalk_duration) seconds: n`".
-spec seconds(number()) -> t().
seconds(N) -> 'seconds:'(N).

-doc "FFI shim: `(Erlang beamtalk_duration) minutes: n`".
-spec minutes(number()) -> t().
minutes(N) -> 'minutes:'(N).

-doc "FFI shim: `(Erlang beamtalk_duration) hours: n`".
-spec hours(number()) -> t().
hours(N) -> 'hours:'(N).

-doc "FFI shim: `(Erlang beamtalk_duration) days: n`".
-spec days(number()) -> t().
days(N) -> 'days:'(N).

-doc "FFI shim: `(Erlang beamtalk_duration) fromString: str`".
-spec fromString(binary()) -> t().
fromString(Str) -> 'fromString:'(Str).

%% `add:with:` → strips to `add`, arity 2
-spec add(t(), t()) -> t().
add(Self, Other) -> '+'(Self, Other).

%% `subtract:with:` → strips to `subtract`, arity 2
-spec subtract(t(), t()) -> t().
subtract(Self, Other) -> '-'(Self, Other).

%% `multiply:with:` → strips to `multiply`, arity 2
-spec multiply(t(), number()) -> t().
multiply(Self, Scalar) -> '*'(Self, Scalar).

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

%% `sneq:with:` → strips to `sneq`, arity 2 — maps to the `=/=` operator
%% (Beamtalk's strict inequality; `neq` above is `/=`)
-spec sneq(t(), t()) -> boolean().
sneq(#{'$beamtalk_class' := 'Duration', millis := A}, #{
    '$beamtalk_class' := 'Duration', millis := B
}) ->
    A =/= B;
sneq(_, _) ->
    raise_type_error(sneq, <<"Argument must be a Duration">>).

%%% ============================================================================
%%% Cross-module Helpers
%%% ============================================================================

-doc """
Normalise a timeout value to integer milliseconds.

Accepts an Integer (already milliseconds) or a Duration; anything else
returns `error`. Not currently called by `beamtalk_timer` or
`beamtalk_datetime` — both extract `millis` directly via
`asMilliseconds`/`asSeconds` — but available for future callers that
need a single normalisation point for `Duration | Integer` timeouts.
""".
-spec to_millis(term()) -> {ok, integer()} | error.
to_millis(Ms) when is_integer(Ms) -> {ok, Ms};
to_millis(#{'$beamtalk_class' := 'Duration', millis := Ms}) -> {ok, Ms};
to_millis(_) -> error.

-doc "True if the value is a Duration tagged map.".
-spec is_duration(term()) -> boolean().
is_duration(#{'$beamtalk_class' := 'Duration', millis := Ms}) -> is_integer(Ms);
is_duration(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-spec make_duration(integer()) -> t().
make_duration(Ms) when is_integer(Ms) ->
    #{'$beamtalk_class' => 'Duration', millis => Ms}.

-spec from_scaled(number(), pos_integer(), atom()) -> t().
from_scaled(N, Scale, _Selector) when is_integer(N) ->
    make_duration(N * Scale);
from_scaled(N, Scale, _Selector) when is_float(N) ->
    make_duration(round(N * Scale));
from_scaled(_, _, Selector) ->
    raise_type_error(Selector, <<"Argument must be an Integer or Float">>).

%% Integer division truncating toward zero (Erlang `div` already truncates
%% toward zero for mixed-sign operands).
-spec trunc_div(integer(), pos_integer()) -> integer().
trunc_div(A, B) -> A div B.

-spec decompose(non_neg_integer()) ->
    {non_neg_integer(), 0..23, 0..59, 0..59, 0..999}.
decompose(AbsMs) ->
    D = AbsMs div ?MS_PER_DAY,
    R1 = AbsMs rem ?MS_PER_DAY,
    H = R1 div ?MS_PER_HOUR,
    R2 = R1 rem ?MS_PER_HOUR,
    Mi = R2 div ?MS_PER_MINUTE,
    R3 = R2 rem ?MS_PER_MINUTE,
    S = R3 div ?MS_PER_SECOND,
    MsRem = R3 rem ?MS_PER_SECOND,
    {D, H, Mi, S, MsRem}.

%% Seconds part of the ISO 8601 rendering: omitted when both are zero,
%% fractional (3 digits) when sub-second milliseconds are present.
-spec format_iso_seconds(non_neg_integer(), 0..999) -> iodata().
format_iso_seconds(0, 0) ->
    [];
format_iso_seconds(S, 0) ->
    [integer_to_binary(S), <<"S">>];
format_iso_seconds(S, MsRem) ->
    [io_lib:format("~B.~3..0B", [S, MsRem]), <<"S">>].

-spec parse_iso8601(binary()) -> {ok, integer()} | error.
parse_iso8601(Str) ->
    case
        re:run(
            Str,
            <<"^(-)?P(?:(\\d+)D)?(?:T(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)(?:\\.(\\d{1,3}))?S)?)?$">>,
            [{capture, all_but_first, binary}]
        )
    of
        {match, Groups} ->
            [SignB, DB, HB, MiB, SB, FracB] = pad_groups(Groups, 6),
            case DB =:= <<>> andalso HB =:= <<>> andalso MiB =:= <<>> andalso SB =:= <<>> of
                true ->
                    %% Bare "P" / "PT" / "-P" — no components at all
                    error;
                false ->
                    Ms =
                        component(DB) * ?MS_PER_DAY +
                            component(HB) * ?MS_PER_HOUR +
                            component(MiB) * ?MS_PER_MINUTE +
                            component(SB) * ?MS_PER_SECOND +
                            frac_millis(FracB),
                    case SignB of
                        <<"-">> -> {ok, -Ms};
                        _ -> {ok, Ms}
                    end
            end;
        nomatch ->
            error
    end.

%% re:run omits trailing unmatched groups; pad to a fixed width.
-spec pad_groups([binary()], non_neg_integer()) -> [binary()].
pad_groups(Groups, N) when length(Groups) >= N -> Groups;
pad_groups(Groups, N) -> Groups ++ lists:duplicate(N - length(Groups), <<>>).

-spec component(binary()) -> non_neg_integer().
component(<<>>) -> 0;
component(B) -> binary_to_integer(B).

%% Fractional seconds → milliseconds: "2" → 200, "25" → 250, "250" → 250.
-spec frac_millis(binary()) -> 0..999.
frac_millis(<<>>) ->
    0;
frac_millis(B) ->
    Padded = pad_frac(B),
    binary_to_integer(Padded).

-spec pad_frac(binary()) -> binary().
pad_frac(B) when byte_size(B) =:= 1 -> <<B/binary, "00">>;
pad_frac(B) when byte_size(B) =:= 2 -> <<B/binary, "0">>;
pad_frac(B) -> B.

-spec compare(atom(), t(), t()) -> boolean().
compare(Op, #{'$beamtalk_class' := 'Duration', millis := A}, #{
    '$beamtalk_class' := 'Duration', millis := B
}) ->
    case Op of
        '<' -> A < B;
        '>' -> A > B;
        '=<' -> A =< B;
        '>=' -> A >= B;
        '=:=' -> A =:= B;
        '/=' -> A /= B
    end;
compare(Op, _, _) ->
    raise_type_error(Op, <<"Argument must be a Duration">>).

-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    beamtalk_error:raise_type_error('Duration', Selector, Hint).
