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

%% Exported for white-box tests only (no production caller).
-ifdef(TEST).
-export([valid_options/0]).
-endif.

-type regex_option() :: caseless | multiline | dotall | extended | ungreedy.
-type t() :: #{
    '$beamtalk_class' := 'Regex',
    source := beamtalk_string:t(),
    compiled := re:mp(),
    atom() => term()
}.
-export_type([t/0, regex_option/0]).

-define(RECEIVER_ARG_HINT, <<"Receiver must be a String, argument a String or Regex">>).

%% A pattern ready for `re:run/3` and friends: precompiled, or raw source.
-type matcher() :: {compiled, re:mp()} | {pattern, binary()}.

%%% ============================================================================
%%% Class Methods — Constructors
%%% ============================================================================

-doc """
Compile a regex pattern string into a Regex object.

Returns `Result ok: regex` on success, `Result error:` if the pattern is invalid.
""".
-spec from(beamtalk_string:t() | binary()) -> beamtalk_result:t().
%% BT-4173: a bare binary spells the same pattern (pre-flip calls stay valid).
from(Source) ->
    Pattern = beamtalk_text:unwrap_or_raise(
        Source, 'Regex', 'from:', <<"Pattern must be a String">>
    ),
    Tagged = {beamtalk_string, Pattern},
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
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format("~ts at position ~tp", [ErrStr, Pos])
            ),
            beamtalk_result:from_tagged_tuple(
                {error, compile_error('Regex', 'from:', Msg, {ErrStr, Pos})}
            )
    end.

-doc """
Compile a regex pattern string with PCRE options.

Returns `Result ok: regex` on success, `Result error:` if the pattern is invalid.
Unknown options still raise (programming error).
""".
-spec from(beamtalk_string:t() | binary(), Options :: [regex_option()]) ->
    beamtalk_result:t().
from(Source, Options) ->
    Hint = <<"Pattern must be a String and options a List">>,
    Pattern = beamtalk_text:unwrap_or_raise(Source, 'Regex', 'from:options:', Hint),
    is_list(Options) orelse beamtalk_error:raise_type_error('Regex', 'from:options:', Hint),
    Tagged = {beamtalk_string, Pattern},
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
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format("~ts at position ~tp", [ErrStr, Pos])
            ),
            beamtalk_result:from_tagged_tuple(
                {error, compile_error('Regex', 'from:options:', Msg, {ErrStr, Pos})}
            )
    end.

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
matches_regex(Str, Pattern) ->
    {Bin, Matcher} = text_and_matcher(Str, Pattern, 'matchesRegex:', ?RECEIVER_ARG_HINT),
    run_matcher(Matcher, 'matchesRegex:', fun(M) -> re:run(Bin, M, [{capture, none}]) =:= match end).

-doc "Test if string matches pattern with options (String only pattern).".
-spec matches_regex_options(beamtalk_string:t() | binary(), beamtalk_string:t() | binary(), list()) ->
    boolean().
matches_regex_options(Str, Pattern, Options) ->
    Hint = <<"Receiver must be a String, pattern a String, options a List">>,
    Bin = beamtalk_text:unwrap_or_raise(Str, 'String', 'matchesRegex:options:', Hint),
    PatternBin = beamtalk_text:unwrap_or_raise(Pattern, 'String', 'matchesRegex:options:', Hint),
    is_list(Options) orelse
        beamtalk_error:raise_type_error('String', 'matchesRegex:options:', Hint),
    CompileOpts = translate_options(Options),
    ErlOpts = [{capture, none} | CompileOpts],
    try
        re:run(Bin, PatternBin, ErlOpts) =:= match
    catch
        error:badarg ->
            raise_bad_pattern('matchesRegex:options:', PatternBin, CompileOpts)
    end.

-doc "Find first match in string, return String or nil.".
-spec first_match(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) ->
    beamtalk_string:t() | 'nil'.
first_match(Str, Pattern) ->
    {Bin, Matcher} = text_and_matcher(Str, Pattern, 'firstMatch:', ?RECEIVER_ARG_HINT),
    run_matcher(Matcher, 'firstMatch:', fun(M) ->
        case re:run(Bin, M, [{capture, first, binary}]) of
            {match, [Match]} -> validated_match('firstMatch:', Match);
            nomatch -> nil
        end
    end).

-doc "Find all matches in string, return List of Strings.".
-spec all_matches(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) -> list().
all_matches(Str, Pattern) ->
    {Bin, Matcher} = text_and_matcher(Str, Pattern, 'allMatches:', ?RECEIVER_ARG_HINT),
    run_matcher(Matcher, 'allMatches:', fun(M) ->
        case re:run(Bin, M, [global, {capture, first, binary}]) of
            {match, Matches} -> [validated_match('allMatches:', Hit) || [Hit] <- Matches];
            nomatch -> []
        end
    end).

-doc "Replace first match with replacement string.".
-spec replace_regex(
    beamtalk_string:t() | binary(),
    beamtalk_string:t() | binary() | map(),
    beamtalk_string:t() | binary()
) ->
    beamtalk_string:t().
replace_regex(Str, Pattern, Replacement) ->
    replace_with(Str, Pattern, Replacement, 'replaceRegex:with:', []).

-doc "Replace all matches with replacement string.".
-spec replace_all_regex(
    beamtalk_string:t() | binary(),
    beamtalk_string:t() | binary() | map(),
    beamtalk_string:t() | binary()
) ->
    beamtalk_string:t().
replace_all_regex(Str, Pattern, Replacement) ->
    replace_with(Str, Pattern, Replacement, 'replaceAllRegex:with:', [global]).

-doc "Split string by regex pattern.".
-spec split_regex(beamtalk_string:t() | binary(), beamtalk_string:t() | binary() | map()) -> list().
split_regex(Str, Pattern) ->
    {Bin, Matcher} = text_and_matcher(Str, Pattern, 'splitRegex:', ?RECEIVER_ARG_HINT),
    run_matcher(Matcher, 'splitRegex:', fun(M) ->
        [validated_match('splitRegex:', Part) || Part <- re:split(Bin, M, [{return, binary}, trim])]
    end).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc """
Shared body of `replace_regex/3` and `replace_all_regex/3`; `ReOpts` is
`[global]` for the all-matches variant.
""".
-spec replace_with(term(), term(), term(), atom(), [global]) -> beamtalk_string:t().
replace_with(Str, Pattern, Replacement, Selector, ReOpts) ->
    Hint = <<"Arguments must be Strings or Regex">>,
    {Bin, Matcher} = text_and_matcher(Str, Pattern, Selector, Hint),
    Repl = beamtalk_text:unwrap_or_raise(Replacement, 'String', Selector, Hint),
    run_matcher(Matcher, Selector, fun(M) ->
        validated_match(Selector, re:replace(Bin, M, Repl, ReOpts ++ [{return, binary}]))
    end).

-doc """
Normalise a `(String, String-or-Regex)` argument pair: the text to its bytes
and the pattern to a `matcher()`. Any other shape raises the `String`
`type_error` for `Selector` carrying `Hint`.
""".
-spec text_and_matcher(term(), term(), atom(), binary()) -> {binary(), matcher()}.
text_and_matcher(Str, Pattern, Selector, Hint) ->
    Bin = beamtalk_text:unwrap_or_raise(Str, 'String', Selector, Hint),
    case Pattern of
        #{'$beamtalk_class' := 'Regex', compiled := MP} ->
            {Bin, {compiled, MP}};
        _ ->
            {Bin, {pattern, beamtalk_text:unwrap_or_raise(Pattern, 'String', Selector, Hint)}}
    end.

-doc """
Run `Fun` with the compiled regex, or with the raw pattern source — in which
case `re` compiles it, and a `badarg` means the pattern is invalid and raises
the structured `regex_error` for `Selector`.
""".
-spec run_matcher(matcher(), atom(), fun((re:mp() | binary()) -> T)) -> T.
run_matcher({compiled, MP}, _Selector, Fun) ->
    Fun(MP);
run_matcher({pattern, Pattern}, Selector, Fun) ->
    %% bt-catcher-audit: not-applicable-no-block - `Fun` is an internal re:* closure, never a Beamtalk block
    try
        Fun(Pattern)
    catch
        error:badarg -> raise_bad_pattern(Selector, Pattern)
    end.

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
    raise_bad_pattern(Selector, Pattern, []).

-doc """
`raise_bad_pattern/2` for a call that compiled with `CompileOpts`: recompiling
with the same options reproduces `re:compile`'s error (and its `reason`) for a
pattern that is only invalid under them, e.g. `(#)` with `extended`.
""".
-spec raise_bad_pattern(atom(), binary(), list()) -> no_return().
raise_bad_pattern(Selector, Pattern, CompileOpts) ->
    %% Try re:compile to get a detailed error message
    case re:compile(Pattern, CompileOpts) of
        {error, {ErrStr, Pos}} ->
            Hint = beamtalk_error:chars_to_binary(
                io_lib:format("Invalid pattern: ~ts at position ~tp", [ErrStr, Pos])
            ),
            beamtalk_error:raise(compile_error('String', Selector, Hint, {ErrStr, Pos}));
        _ ->
            beamtalk_error:raise(
                beamtalk_error:new(regex_error, 'String', Selector, <<"Invalid regex pattern">>)
            )
    end.

-doc """
The `regex_error` for a pattern `re:compile` rejected (BT-4236).

`details.reason` keeps OTP's `{ErrStr, Pos}` unchanged, so `Exception>>reason`
answers the same term as the FFI call `Erlang re compile:` for that pattern.
""".
-spec compile_error(atom(), atom(), binary(), {string(), non_neg_integer()}) ->
    beamtalk_error:error().
compile_error(Class, Selector, Hint, {ErrStr, Pos}) ->
    beamtalk_error:with_details(
        beamtalk_error:new(regex_error, Class, Selector, Hint),
        #{reason => {ErrStr, Pos}}
    ).

-doc """
The PCRE compile flags `from:options:` and `matchesRegex:options:` accept.

The single runtime source for the set: `translate_option/1` and its
"Valid: …" hint both read it. `beamtalk_regex_tests` checks it equals both
the `RegexOption` alias members in `stdlib/src/regex.bt` (via the stdlib
app's `type_aliases` env) and the `regex_option()` type above, so none of
the three can drift alone.

Every member is also an `re:compile/2` option of the same name, which is why
`translate_option/1` passes it through unchanged.
""".
-spec valid_options() -> [regex_option(), ...].
valid_options() ->
    [caseless, multiline, dotall, extended, ungreedy].

-doc "Translate Beamtalk option atoms to Erlang re compile options.".
-spec translate_options(list()) -> list().
translate_options(Opts) ->
    [translate_option(O) || O <- Opts].

-spec translate_option(term()) -> regex_option().
translate_option(Option) ->
    case lists:member(Option, valid_options()) of
        true ->
            Option;
        false ->
            Error0 = beamtalk_error:new(regex_error, 'Regex'),
            Valid = lists:join(", ", [atom_to_list(O) || O <- valid_options()]),
            Hint = beamtalk_error:chars_to_binary(
                io_lib:format("Unknown option: ~tp. Valid: ~ts", [Option, Valid])
            ),
            Error1 = beamtalk_error:with_hint(Error0, Hint),
            beamtalk_error:raise(Error1)
    end.
