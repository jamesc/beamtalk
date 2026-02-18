%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Regex class implementation — regular expressions via Erlang re module.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Provides class-side constructors for compiling patterns and instance
%%% methods for inspection. String-side regex operations (matchesRegex:,
%%% firstMatch:, etc.) are in beamtalk_string_ops.
%%%
%%% Regex objects are represented as tagged maps:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'Regex',
%%%   source => Pattern :: binary(),
%%%   compiled => CompiledRe :: mp()
%%% }
%%% ```

-module(beamtalk_regex).

-export(['from:'/1, 'from:options:'/2]).
-export([source/1, 'printString'/1, describe/1]).
-export([has_method/1]).
-export([matches_regex/2, matches_regex_options/3,
         first_match/2, all_matches/2,
         replace_regex/3, replace_all_regex/3,
         split_regex/2]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

%% @doc Compile a regex pattern string into a Regex object.
-spec 'from:'(binary()) -> map().
'from:'(Pattern) when is_binary(Pattern) ->
    case re:compile(Pattern) of
        {ok, MP} ->
            #{'$beamtalk_class' => 'Regex',
              source => Pattern,
              compiled => MP};
        {error, {ErrStr, Pos}} ->
            Msg = iolist_to_binary(io_lib:format("~s at position ~p", [ErrStr, Pos])),
            Error0 = beamtalk_error:new(regex_error, 'Regex'),
            Error1 = beamtalk_error:with_selector(Error0, 'from:'),
            Error2 = beamtalk_error:with_hint(Error1, Msg),
            beamtalk_error:raise(Error2)
    end;
'from:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Regex'),
    Error1 = beamtalk_error:with_selector(Error0, 'from:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Pattern must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Compile a regex pattern string with PCRE options.
-spec 'from:options:'(binary(), list()) -> map().
'from:options:'(Pattern, Options) when is_binary(Pattern), is_list(Options) ->
    ErlOpts = translate_options(Options),
    case re:compile(Pattern, ErlOpts) of
        {ok, MP} ->
            #{'$beamtalk_class' => 'Regex',
              source => Pattern,
              compiled => MP};
        {error, {ErrStr, Pos}} ->
            Msg = iolist_to_binary(io_lib:format("~s at position ~p", [ErrStr, Pos])),
            Error0 = beamtalk_error:new(regex_error, 'Regex'),
            Error1 = beamtalk_error:with_selector(Error0, 'from:options:'),
            Error2 = beamtalk_error:with_hint(Error1, Msg),
            beamtalk_error:raise(Error2)
    end;
'from:options:'(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'Regex'),
    Error1 = beamtalk_error:with_selector(Error0, 'from:options:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Pattern must be a String and options a List">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Instance Methods — Inspection
%%% ============================================================================

%% @doc Return the original pattern source string.
-spec source(map()) -> binary().
source(#{source := Src}) -> Src.

%% @doc Human-readable representation: Regex([0-9]+)
-spec 'printString'(map()) -> binary().
'printString'(#{source := Src}) ->
    iolist_to_binary([<<"Regex(">>, Src, <<")">>]).

%% @doc Same as printString for describe protocol.
-spec describe(map()) -> binary().
describe(Self) -> 'printString'(Self).

%% @doc Check if Regex responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('from:') -> true;
has_method('from:options:') -> true;
has_method('source') -> true;
has_method('printString') -> true;
has_method('describe') -> true;
has_method(_) -> false.

%%% ============================================================================
%%% String Helper Functions — called by String codegen
%%% ============================================================================

%% @doc Test if string matches pattern (String or Regex).
-spec matches_regex(binary(), binary() | map()) -> boolean().
matches_regex(Str, #{compiled := MP}) when is_binary(Str) ->
    re:run(Str, MP, [{capture, none}]) =:= match;
matches_regex(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    re:run(Str, Pattern, [{capture, none}]) =:= match;
matches_regex(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'matchesRegex:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a String, argument a String or Regex">>),
    beamtalk_error:raise(Error2).

%% @doc Test if string matches pattern with options (String only pattern).
-spec matches_regex_options(binary(), binary(), list()) -> boolean().
matches_regex_options(Str, Pattern, Options) when is_binary(Str), is_binary(Pattern), is_list(Options) ->
    ErlOpts = [{capture, none} | translate_options(Options)],
    re:run(Str, Pattern, ErlOpts) =:= match;
matches_regex_options(_, _, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'matchesRegex:options:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a String, pattern a String, options a List">>),
    beamtalk_error:raise(Error2).

%% @doc Find first match in string, return String or nil.
-spec first_match(binary(), binary() | map()) -> binary() | nil.
first_match(Str, #{compiled := MP}) when is_binary(Str) ->
    case re:run(Str, MP, [{capture, first, binary}]) of
        {match, [Match]} -> Match;
        nomatch -> nil
    end;
first_match(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    case re:run(Str, Pattern, [{capture, first, binary}]) of
        {match, [Match]} -> Match;
        nomatch -> nil
    end;
first_match(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'firstMatch:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a String, argument a String or Regex">>),
    beamtalk_error:raise(Error2).

%% @doc Find all matches in string, return List of Strings.
-spec all_matches(binary(), binary() | map()) -> list().
all_matches(Str, #{compiled := MP}) when is_binary(Str) ->
    case re:run(Str, MP, [global, {capture, first, binary}]) of
        {match, Matches} -> [M || [M] <- Matches];
        nomatch -> []
    end;
all_matches(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    case re:run(Str, Pattern, [global, {capture, first, binary}]) of
        {match, Matches} -> [M || [M] <- Matches];
        nomatch -> []
    end;
all_matches(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'allMatches:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a String, argument a String or Regex">>),
    beamtalk_error:raise(Error2).

%% @doc Replace first match with replacement string.
-spec replace_regex(binary(), binary() | map(), binary()) -> binary().
replace_regex(Str, #{compiled := MP}, Replacement) when is_binary(Str), is_binary(Replacement) ->
    iolist_to_binary(re:replace(Str, MP, Replacement, [{return, binary}]));
replace_regex(Str, Pattern, Replacement) when is_binary(Str), is_binary(Pattern), is_binary(Replacement) ->
    iolist_to_binary(re:replace(Str, Pattern, Replacement, [{return, binary}]));
replace_regex(_, _, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'replaceRegex:with:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Arguments must be Strings or Regex">>),
    beamtalk_error:raise(Error2).

%% @doc Replace all matches with replacement string.
-spec replace_all_regex(binary(), binary() | map(), binary()) -> binary().
replace_all_regex(Str, #{compiled := MP}, Replacement) when is_binary(Str), is_binary(Replacement) ->
    iolist_to_binary(re:replace(Str, MP, Replacement, [global, {return, binary}]));
replace_all_regex(Str, Pattern, Replacement) when is_binary(Str), is_binary(Pattern), is_binary(Replacement) ->
    iolist_to_binary(re:replace(Str, Pattern, Replacement, [global, {return, binary}]));
replace_all_regex(_, _, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'replaceAllRegex:with:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Arguments must be Strings or Regex">>),
    beamtalk_error:raise(Error2).

%% @doc Split string by regex pattern.
-spec split_regex(binary(), binary() | map()) -> list().
split_regex(Str, #{compiled := MP}) when is_binary(Str) ->
    re:split(Str, MP, [{return, binary}, trim]);
split_regex(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    re:split(Str, Pattern, [{return, binary}, trim]);
split_regex(_, _) ->
    Error0 = beamtalk_error:new(type_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'splitRegex:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Receiver must be a String, argument a String or Regex">>),
    beamtalk_error:raise(Error2).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Translate Beamtalk option atoms to Erlang re compile options.
-spec translate_options(list()) -> list().
translate_options(Opts) ->
    lists:map(fun translate_option/1, Opts).

-spec translate_option(atom()) -> atom().
translate_option(caseless)  -> caseless;
translate_option(multiline) -> multiline;
translate_option(dotall)    -> dotall;
translate_option(extended)  -> extended;
translate_option(ungreedy)  -> ungreedy;
translate_option(Other) ->
    Error0 = beamtalk_error:new(regex_error, 'Regex'),
    Hint = iolist_to_binary(io_lib:format("Unknown option: ~p. Valid: caseless, multiline, dotall, extended, ungreedy", [Other])),
    Error1 = beamtalk_error:with_hint(Error0, Hint),
    beamtalk_error:raise(Error1).
