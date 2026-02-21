%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc DateTime class implementation — date/time via Erlang calendar/os modules.
%%%
%% **DDD Context:** Runtime Context
%%%
%%% Provides class-side constructors for creating UTC datetime values and
%%% instance methods for access, arithmetic, comparison, and formatting.
%%%
%%% DateTime objects are represented as tagged maps:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'DateTime',
%%%   year   => integer(),
%%%   month  => 1..12,
%%%   day    => 1..31,
%%%   hour   => 0..23,
%%%   minute => 0..59,
%%%   second => 0..59
%%% }
%%% ```

-module(beamtalk_datetime).

%% Class methods
-export([now/0, 'monotonicNow'/0]).
-export(['year:month:day:'/3, 'year:month:day:hour:minute:second:'/6]).
-export(['fromTimestamp:'/1, 'fromString:'/1]).

%% Instance methods
-export([year/1, month/1, day/1, hour/1, minute/1, second/1]).
-export(['asTimestamp'/1, 'asString'/1, 'printString'/1, describe/1]).
-export(['addSeconds:'/2, 'addDays:'/2, 'diffSeconds:'/2]).
-export(['<'/2, '>'/2, '=<'/2, '>='/2, '=:='/2, '/='/2]).

-export([has_method/1]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Gregorian seconds at Unix epoch (1970-01-01 00:00:00)
-define(EPOCH_GREGORIAN, 62167219200).

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

%% @doc Current UTC time as a DateTime.
-spec now() -> map().
now() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    make_datetime(Y, Mo, D, H, Mi, S).

%% @doc Monotonic clock value in nanoseconds (Integer, not a DateTime).
-spec 'monotonicNow'() -> integer().
'monotonicNow'() ->
    erlang:monotonic_time(nanosecond).

%% @doc Construct a DateTime from year, month, day (time defaults to 00:00:00).
-spec 'year:month:day:'(integer(), integer(), integer()) -> map().
'year:month:day:'(Y, Mo, D) when is_integer(Y), is_integer(Mo), is_integer(D) ->
    validate_date(Y, Mo, D, 'year:month:day:'),
    make_datetime(Y, Mo, D, 0, 0, 0);
'year:month:day:'(_, _, _) ->
    raise_type_error('year:month:day:', <<"Arguments must be Integers">>).

%% @doc Construct a DateTime from year, month, day, hour, minute, second.
-spec 'year:month:day:hour:minute:second:'(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> map().
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

%% @doc Construct a DateTime from a Unix epoch timestamp (seconds).
-spec 'fromTimestamp:'(integer()) -> map().
'fromTimestamp:'(Ts) when is_integer(Ts) ->
    GregSec = Ts + ?EPOCH_GREGORIAN,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(GregSec),
    make_datetime(Y, Mo, D, H, Mi, S);
'fromTimestamp:'(_) ->
    raise_type_error('fromTimestamp:', <<"Argument must be an Integer (Unix epoch seconds)">>).

%% @doc Parse an ISO 8601 string into a DateTime.
-spec 'fromString:'(binary()) -> map().
'fromString:'(Str) when is_binary(Str) ->
    case parse_iso8601(Str) of
        {ok, Y, Mo, D, H, Mi, S} ->
            make_datetime(Y, Mo, D, H, Mi, S);
        error ->
            Error0 = beamtalk_error:new(type_error, 'DateTime'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromString:'),
            Error2 = beamtalk_error:with_hint(
                Error1, <<"Expected ISO 8601 format: YYYY-MM-DDThh:mm:ssZ">>
            ),
            beamtalk_error:raise(Error2)
    end;
'fromString:'(_) ->
    raise_type_error('fromString:', <<"Argument must be a String">>).

%%% ============================================================================
%%% Instance Methods — Accessors
%%% ============================================================================

-spec year(map()) -> integer().
year(#{year := V}) -> V.

-spec month(map()) -> integer().
month(#{month := V}) -> V.

-spec day(map()) -> integer().
day(#{day := V}) -> V.

-spec hour(map()) -> integer().
hour(#{hour := V}) -> V.

-spec minute(map()) -> integer().
minute(#{minute := V}) -> V.

-spec second(map()) -> integer().
second(#{second := V}) -> V.

%%% ============================================================================
%%% Instance Methods — Conversion
%%% ============================================================================

%% @doc Convert to Unix epoch timestamp (seconds).
-spec 'asTimestamp'(map()) -> integer().
'asTimestamp'(#{
    year := Y,
    month := Mo,
    day := D,
    hour := H,
    minute := Mi,
    second := S
}) ->
    GregSec = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, S}}),
    GregSec - ?EPOCH_GREGORIAN.

%% @doc Format as ISO 8601 string.
-spec 'asString'(map()) -> binary().
'asString'(#{
    year := Y,
    month := Mo,
    day := D,
    hour := H,
    minute := Mi,
    second := S
}) ->
    iolist_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Y, Mo, D, H, Mi, S]
        )
    ).

%% @doc Human-readable representation.
-spec 'printString'(map()) -> binary().
'printString'(Self) ->
    iolist_to_binary([<<"a DateTime(">>, 'asString'(Self), <<")">>]).

%% @doc Describe protocol — returns ISO 8601 string.
-spec describe(map()) -> binary().
describe(Self) -> 'asString'(Self).

%%% ============================================================================
%%% Instance Methods — Arithmetic
%%% ============================================================================

%% @doc Add seconds, return new DateTime.
-spec 'addSeconds:'(map(), integer()) -> map().
'addSeconds:'(Self, Secs) when is_integer(Secs) ->
    Ts = 'asTimestamp'(Self) + Secs,
    'fromTimestamp:'(Ts);
'addSeconds:'(_, _) ->
    raise_type_error('addSeconds:', <<"Argument must be an Integer">>).

%% @doc Add days, return new DateTime.
-spec 'addDays:'(map(), integer()) -> map().
'addDays:'(Self, Days) when is_integer(Days) ->
    'addSeconds:'(Self, Days * 86400);
'addDays:'(_, _) ->
    raise_type_error('addDays:', <<"Argument must be an Integer">>).

%% @doc Difference in seconds between this and another DateTime.
-spec 'diffSeconds:'(map(), map()) -> integer().
'diffSeconds:'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) - 'asTimestamp'(Other);
'diffSeconds:'(_, _) ->
    raise_type_error('diffSeconds:', <<"Argument must be a DateTime">>).

%%% ============================================================================
%%% Instance Methods — Comparison
%%% ============================================================================

-spec '<'(map(), map()) -> boolean().
'<'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) < 'asTimestamp'(Other);
'<'(_, _) ->
    raise_type_error('<', <<"Argument must be a DateTime">>).

-spec '>'(map(), map()) -> boolean().
'>'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) > 'asTimestamp'(Other);
'>'(_, _) ->
    raise_type_error('>', <<"Argument must be a DateTime">>).

-spec '=<'(map(), map()) -> boolean().
'=<'(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) =< 'asTimestamp'(Other);
'=<'(_, _) ->
    raise_type_error('=<', <<"Argument must be a DateTime">>).

-spec '>='(map(), map()) -> boolean().
'>='(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) >= 'asTimestamp'(Other);
'>='(_, _) ->
    raise_type_error('>=', <<"Argument must be a DateTime">>).

-spec '=:='(map(), map()) -> boolean().
'=:='(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) =:= 'asTimestamp'(Other);
'=:='(_, _) ->
    raise_type_error('=:=', <<"Argument must be a DateTime">>).

-spec '/='(map(), map()) -> boolean().
'/='(Self, #{'$beamtalk_class' := 'DateTime'} = Other) ->
    'asTimestamp'(Self) /= 'asTimestamp'(Other);
'/='(_, _) ->
    raise_type_error('/=', <<"Argument must be a DateTime">>).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

-spec has_method(atom()) -> boolean().
has_method('year') -> true;
has_method('month') -> true;
has_method('day') -> true;
has_method('hour') -> true;
has_method('minute') -> true;
has_method('second') -> true;
has_method('asTimestamp') -> true;
has_method('asString') -> true;
has_method('printString') -> true;
has_method('describe') -> true;
has_method('addSeconds:') -> true;
has_method('addDays:') -> true;
has_method('diffSeconds:') -> true;
has_method('<') -> true;
has_method('>') -> true;
has_method('=<') -> true;
has_method('>=') -> true;
has_method('=:=') -> true;
has_method('/=') -> true;
has_method(_) -> false.

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
) -> map().
make_datetime(Y, Mo, D, H, Mi, S) ->
    #{
        '$beamtalk_class' => 'DateTime',
        year => Y,
        month => Mo,
        day => D,
        hour => H,
        minute => Mi,
        second => S
    }.

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
validate_time(H, Mi, S, Selector) when
    H >= 0, H =< 23, Mi >= 0, Mi =< 59, S >= 0, S =< 59
->
    ok;
validate_time(H, Mi, S, Selector) ->
    Msg = iolist_to_binary(io_lib:format("Invalid time: ~p:~p:~p", [H, Mi, S])),
    Error0 = beamtalk_error:new(type_error, 'DateTime'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Msg),
    beamtalk_error:raise(Error2).

-spec parse_iso8601(binary()) ->
    {ok, integer(), integer(), integer(), integer(), integer(), integer()}
    | error.
parse_iso8601(Str) ->
    %% Accept YYYY-MM-DDThh:mm:ssZ or YYYY-MM-DDThh:mm:ss
    case
        re:run(
            Str,
            <<"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})Z?$">>,
            [{capture, all_but_first, binary}]
        )
    of
        {match, [YB, MoB, DB, HB, MiB, SB]} ->
            Y = binary_to_integer(YB),
            Mo = binary_to_integer(MoB),
            D = binary_to_integer(DB),
            H = binary_to_integer(HB),
            Mi = binary_to_integer(MiB),
            S = binary_to_integer(SB),
            case calendar:valid_date(Y, Mo, D) of
                true when H >= 0, H =< 23, Mi >= 0, Mi =< 59, S >= 0, S =< 59 ->
                    {ok, Y, Mo, D, H, Mi, S};
                _ ->
                    error
            end;
        nomatch ->
            error
    end.

-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'DateTime'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).
