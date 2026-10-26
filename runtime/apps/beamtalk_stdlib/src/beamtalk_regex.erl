%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_regex).

%%% **DDD Context:** Object System Context

-moduledoc """
Regex class implementation — regular expressions via Erlang re module.

Provides class-side constructors for compiling patterns and instance
methods for inspection. String-side regex operations (matchesRegex:,
firstMatch:, etc.) are also in this module as helper functions called
by String's codegen.

Regex objects are represented as tagged maps:
```
#{
  '$beamtalk_class' => 'Regex',
  source => Pattern :: binary(),
  compiled => CompiledRe :: mp()
}
```
""".

-export(['from:'/1, 'from:options:'/2]).
-export([from/1, from/2]).
-export([source/1, 'printString'/1]).
-export([
    matches_regex/2,
    matches_regex_options/3,
    first_match/2,
    all_matches/2,
    replace_regex/3,
    replace_all_regex/3,
    split_regex/2
]).

-type regex_option() :: caseless | multiline | dotall | extended | ungreedy.
-type t() :: #{
    '$beamtalk_class' := 'Regex',
    source := beamtalk_string:t(),
    compiled := re:mp(),
    atom() => term()
}.
-export_type([t/0, regex_option/0]).

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

-doc """
Compile a regex pattern string into a Regex object.

Returns `Result ok: regex` on success, `Result error:` if the pattern is invalid.
""".
-spec 'from:'(beamtalk_string:t() | binary()) -> beamtalk_result:t().
%% BT-4173: a bare binary spells the same pattern (pre-flip calls stay valid).
'from:'(Pattern) when is_binary(Pattern) ->
    'from:'({beamtalk_string, Pattern});
'from:'({beamtalk_string, Pattern} = Tagged) when is_binary(Pattern) ->
    case re:compile(Pattern) of
        {ok, MP} ->
            beamtalk_result:from_tagged_tuple(
                {ok, #{
                    '$beamtalk_class' => 'Regex',
                    source => Tagged,
                    compiled => MP
                }}
            );
        {error, {ErrStr, Pos}} ->
            Msg = iolist_to_binary(io_lib:format("~s at position ~p", [ErrStr, Pos])),
            Error2 = beamtalk_error:new(regex_error, 'Regex', 'from:', Msg),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'from:'(_) ->
    beamtalk_error:raise_type_error('Regex', 'from:', <<"Pattern must be a String">>).

-doc """
Compile a regex pattern string with PCRE options.

Returns `Result ok: regex` on success, `Result error:` if the pattern is invalid.
Unknown options still raise (programming error).
""".
-spec 'from:options:'(beamtalk_string:t() | binary(), Options :: [regex_option()]) ->
    beamtalk_result:t().
'from:options:'(Pattern, Options) when is_binary(Pattern) ->
    'from:options:'({beamtalk_string, Pattern}, Options);
'from:options:'({beamtalk_string, Pattern} = Tagged, Options) when
    is_binary(Pattern), is_list(Options)
->
    ErlOpts = translate_options(Options),
    case re:compile(Pattern, ErlOpts) of
        {ok, MP} ->
            beamtalk_result:from_tagged_tuple(
                {ok, #{
                    '$beamtalk_class' => 'Regex',
                    source => Tagged,
                    compiled => MP
                }}
            );
        {error, {ErrStr, Pos}} ->
            Msg = iolist_to_binary(io_lib:format("~s at position ~p", [ErrStr, Pos])),
            Error2 = beamtalk_error:new(regex_error, 'Regex', 'from:options:', Msg),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
'from:options:'(_, _) ->
    beamtalk_error:raise_type_error(
        'Regex', 'from:options:', <<"Pattern must be a String and options a List">>
    ).

%%% ============================================================================
%%% FFI aliases — no-colon names for Erlang FFI dispatch
%%% ============================================================================

-doc "FFI alias for from:/1 — called via (Erlang beamtalk_regex) from: pattern.".
-spec from(beamtalk_string:t()) -> beamtalk_result:t().
from(Pattern) -> 'from:'(Pattern).

-doc """
FFI alias for from:options:/2 — called via (Erlang beamtalk_regex) from: p options: opts.
""".
-spec from(beamtalk_string:t(), Options :: [regex_option()]) -> beamtalk_result:t().
from(Pattern, Opts) -> 'from:options:'(Pattern, Opts).

%%% ============================================================================
%%% Instance Methods — Inspection
%%% ============================================================================

-doc "Return the original pattern source string.".
-spec source(t()) -> beamtalk_string:t().
source(#{source := Src}) -> Src.

-doc "Human-readable representation: Regex([0-9]+)".
-spec 'printString'(t()) -> beamtalk_string:t().
'printString'(#{source := {beamtalk_string, Src}}) when is_binary(Src) ->
    beamtalk_primitive:string_from_binary(
        iolist_to_binary([<<"Regex(">>, Src, <<")">>])
    ).

%%% ============================================================================
%%% String Helper Functions — called by String codegen
%%% ============================================================================

-doc "Test if string matches pattern (String or Regex).".
-spec matches_regex(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) ->
    boolean().
matches_regex(Str, Regex) when is_binary(Str), is_map(Regex) ->
    matches_regex({beamtalk_string, Str}, Regex);
matches_regex(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    matches_regex({beamtalk_string, Str}, {beamtalk_string, Pattern});
matches_regex({beamtalk_string, _} = Str, Pattern) when is_binary(Pattern) ->
    matches_regex(Str, {beamtalk_string, Pattern});
matches_regex(Str, {beamtalk_string, _} = Pattern) when is_binary(Str) ->
    matches_regex({beamtalk_string, Str}, Pattern);
matches_regex({beamtalk_string, Str}, #{'$beamtalk_class' := 'Regex', compiled := MP}) when
    is_binary(Str)
->
    re:run(Str, MP, [{capture, none}]) =:= match;
matches_regex({beamtalk_string, Str}, {beamtalk_string, Pattern}) when
    is_binary(Str), is_binary(Pattern)
->
    try
        re:run(Str, Pattern, [{capture, none}]) =:= match
    catch
        error:badarg -> raise_bad_pattern('matchesRegex:', Pattern)
    end;
matches_regex(_, _) ->
    beamtalk_error:raise_type_error(
        'String', 'matchesRegex:', <<"Receiver must be a String, argument a String or Regex">>
    ).

-doc "Test if string matches pattern with options (String only pattern).".
-spec matches_regex_options(beamtalk_string:t() | binary(), beamtalk_string:t() | binary(), list()) ->
    boolean().
matches_regex_options(Str, Pattern, Options) when is_binary(Str), is_binary(Pattern) ->
    matches_regex_options({beamtalk_string, Str}, {beamtalk_string, Pattern}, Options);
matches_regex_options({beamtalk_string, _} = Str, Pattern, Options) when is_binary(Pattern) ->
    matches_regex_options(Str, {beamtalk_string, Pattern}, Options);
matches_regex_options(Str, {beamtalk_string, _} = Pattern, Options) when is_binary(Str) ->
    matches_regex_options({beamtalk_string, Str}, Pattern, Options);
matches_regex_options({beamtalk_string, Str}, {beamtalk_string, Pattern}, Options) when
    is_binary(Str), is_binary(Pattern), is_list(Options)
->
    ErlOpts = [{capture, none} | translate_options(Options)],
    try
        re:run(Str, Pattern, ErlOpts) =:= match
    catch
        error:badarg -> raise_bad_pattern('matchesRegex:options:', Pattern)
    end;
matches_regex_options(_, _, _) ->
    beamtalk_error:raise_type_error(
        'String',
        'matchesRegex:options:',
        <<"Receiver must be a String, pattern a String, options a List">>
    ).

-doc "Find first match in string, return String or nil.".
-spec first_match(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) ->
    beamtalk_string:t() | 'nil'.
%% BT-4173: a bare binary spells the same text (pre-flip calls stay valid).
first_match(S, P) when is_binary(S) -> first_match({beamtalk_string, S}, P);
first_match(S, P) when is_binary(P) -> first_match(S, {beamtalk_string, P});
first_match({beamtalk_string, Str}, #{'$beamtalk_class' := 'Regex', compiled := MP}) when
    is_binary(Str)
->
    case re:run(Str, MP, [{capture, first, binary}]) of
        {match, [Match]} -> validated_match('firstMatch:', Match);
        nomatch -> nil
    end;
first_match({beamtalk_string, Str}, {beamtalk_string, Pattern}) when
    is_binary(Str), is_binary(Pattern)
->
    try
        case re:run(Str, Pattern, [{capture, first, binary}]) of
            {match, [Match]} -> validated_match('firstMatch:', Match);
            nomatch -> nil
        end
    catch
        error:badarg -> raise_bad_pattern('firstMatch:', Pattern)
    end;
first_match(_, _) ->
    beamtalk_error:raise_type_error(
        'String', 'firstMatch:', <<"Receiver must be a String, argument a String or Regex">>
    ).

-doc "Find all matches in string, return List of Strings.".
-spec all_matches(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) -> list().
%% BT-4173: a bare binary spells the same text (pre-flip calls stay valid).
all_matches(S, P) when is_binary(S) -> all_matches({beamtalk_string, S}, P);
all_matches(S, P) when is_binary(P) -> all_matches(S, {beamtalk_string, P});
all_matches({beamtalk_string, Str}, #{'$beamtalk_class' := 'Regex', compiled := MP}) when
    is_binary(Str)
->
    case re:run(Str, MP, [global, {capture, first, binary}]) of
        {match, Matches} -> [validated_match('allMatches:', M) || [M] <- Matches];
        nomatch -> []
    end;
all_matches({beamtalk_string, Str}, {beamtalk_string, Pattern}) when
    is_binary(Str), is_binary(Pattern)
->
    try
        case re:run(Str, Pattern, [global, {capture, first, binary}]) of
            {match, Matches} -> [validated_match('allMatches:', M) || [M] <- Matches];
            nomatch -> []
        end
    catch
        error:badarg -> raise_bad_pattern('allMatches:', Pattern)
    end;
all_matches(_, _) ->
    beamtalk_error:raise_type_error(
        'String', 'allMatches:', <<"Receiver must be a String, argument a String or Regex">>
    ).

-doc "Replace first match with replacement string.".
-spec replace_regex(
    beamtalk_string:t() | binary(),
    beamtalk_string:t() | binary() | map(),
    beamtalk_string:t() | binary()
) ->
    beamtalk_string:t().
%% BT-4173: a bare binary spells the same text (pre-flip calls stay valid).
replace_regex(S, P, R) when is_binary(S) -> replace_regex({beamtalk_string, S}, P, R);
replace_regex(S, P, R) when is_binary(P) -> replace_regex(S, {beamtalk_string, P}, R);
replace_regex(S, P, R) when is_binary(R) -> replace_regex(S, P, {beamtalk_string, R});
replace_regex(
    {beamtalk_string, Str},
    #{'$beamtalk_class' := 'Regex', compiled := MP},
    {beamtalk_string, Replacement}
) when is_binary(Str), is_binary(Replacement) ->
    validated_match(
        'replaceRegex:with:', re:replace(Str, MP, Replacement, [{return, binary}])
    );
replace_regex(
    {beamtalk_string, Str}, {beamtalk_string, Pattern}, {beamtalk_string, Replacement}
) when is_binary(Str), is_binary(Pattern), is_binary(Replacement) ->
    try
        validated_match(
            'replaceRegex:with:',
            re:replace(Str, Pattern, Replacement, [{return, binary}])
        )
    catch
        error:badarg -> raise_bad_pattern('replaceRegex:with:', Pattern)
    end;
replace_regex(_, _, _) ->
    beamtalk_error:raise_type_error(
        'String', 'replaceRegex:with:', <<"Arguments must be Strings or Regex">>
    ).

-doc "Replace all matches with replacement string.".
-spec replace_all_regex(
    beamtalk_string:t() | binary(),
    beamtalk_string:t() | binary() | map(),
    beamtalk_string:t() | binary()
) ->
    beamtalk_string:t().
%% BT-4173: a bare binary spells the same text (pre-flip calls stay valid).
replace_all_regex(S, P, R) when is_binary(S) -> replace_all_regex({beamtalk_string, S}, P, R);
replace_all_regex(S, P, R) when is_binary(P) -> replace_all_regex(S, {beamtalk_string, P}, R);
replace_all_regex(S, P, R) when is_binary(R) -> replace_all_regex(S, P, {beamtalk_string, R});
replace_all_regex(
    {beamtalk_string, Str},
    #{'$beamtalk_class' := 'Regex', compiled := MP},
    {beamtalk_string, Replacement}
) when is_binary(Str), is_binary(Replacement) ->
    validated_match(
        'replaceAllRegex:with:',
        re:replace(Str, MP, Replacement, [global, {return, binary}])
    );
replace_all_regex(
    {beamtalk_string, Str}, {beamtalk_string, Pattern}, {beamtalk_string, Replacement}
) when is_binary(Str), is_binary(Pattern), is_binary(Replacement) ->
    try
        validated_match(
            'replaceAllRegex:with:',
            re:replace(Str, Pattern, Replacement, [global, {return, binary}])
        )
    catch
        error:badarg -> raise_bad_pattern('replaceAllRegex:with:', Pattern)
    end;
replace_all_regex(_, _, _) ->
    beamtalk_error:raise_type_error(
        'String', 'replaceAllRegex:with:', <<"Arguments must be Strings or Regex">>
    ).

-doc "Split string by regex pattern.".
-spec split_regex(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) -> list().
%% BT-4173: a bare binary spells the same text (pre-flip calls stay valid).
split_regex(S, P) when is_binary(S) -> split_regex({beamtalk_string, S}, P);
split_regex(S, P) when is_binary(P) -> split_regex(S, {beamtalk_string, P});
split_regex({beamtalk_string, Str}, #{'$beamtalk_class' := 'Regex', compiled := MP}) when
    is_binary(Str)
->
    [validated_match('splitRegex:', P) || P <- re:split(Str, MP, [{return, binary}, trim])];
split_regex({beamtalk_string, Str}, {beamtalk_string, Pattern}) when
    is_binary(Str), is_binary(Pattern)
->
    try
        [
            validated_match('splitRegex:', P)
         || P <- re:split(Str, Pattern, [{return, binary}, trim])
        ]
    catch
        error:badarg -> raise_bad_pattern('splitRegex:', Pattern)
    end;
split_regex(_, _) ->
    beamtalk_error:raise_type_error(
        'String', 'splitRegex:', <<"Receiver must be a String, argument a String or Regex">>
    ).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc """
Wrap a regex-produced binary as a `String` (BT-4170).

`re` is byte-oriented here (patterns compile without the `unicode`
option), so a match or substitution boundary can split a multi-byte
character — the fragment is bytes, not text, and wrapping it unchecked
would forge a corrupt `String`. Validate first; a fragment raises
`parse_error` naming `Selector` (byte-oriented matching belongs on
`Binary`).
""".
-spec validated_match(atom(), binary()) -> beamtalk_string:t().
validated_match(Selector, Bin) when is_binary(Bin) ->
    case beamtalk_binary:check_utf8(Bin) of
        {ok, Valid} ->
            beamtalk_primitive:string_from_binary(Valid);
        {error, _} ->
            Error0 = beamtalk_error:new(parse_error, 'String'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Match boundary splits a multi-byte character; match bytes on Binary instead">>
            ),
            beamtalk_error:raise(Error2)
    end.

-doc "Raise a structured regex_error for invalid patterns in String methods.".
-spec raise_bad_pattern(atom(), binary()) -> no_return().
raise_bad_pattern(Selector, Pattern) ->
    %% Try re:compile to get a detailed error message
    Hint =
        case re:compile(Pattern) of
            {error, {ErrStr, Pos}} ->
                iolist_to_binary(
                    io_lib:format("Invalid pattern: ~s at position ~p", [ErrStr, Pos])
                );
            _ ->
                <<"Invalid regex pattern">>
        end,
    beamtalk_error:raise(beamtalk_error:new(regex_error, 'String', Selector, Hint)).

-doc "Translate Beamtalk option atoms to Erlang re compile options.".
-spec translate_options(list()) -> list().
translate_options(Opts) ->
    [translate_option(O) || O <- Opts].

-spec translate_option(atom()) -> atom().
translate_option(caseless) ->
    caseless;
translate_option(multiline) ->
    multiline;
translate_option(dotall) ->
    dotall;
translate_option(extended) ->
    extended;
translate_option(ungreedy) ->
    ungreedy;
translate_option(Other) ->
    Error0 = beamtalk_error:new(regex_error, 'Regex'),
    Hint = iolist_to_binary(
        io_lib:format(
            "Unknown option: ~p. Valid: caseless, multiline, dotall, extended, ungreedy", [Other]
        )
    ),
    Error1 = beamtalk_error:with_hint(Error0, Hint),
    beamtalk_error:raise(Error1).
