%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_editor_context).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Text-level editor-context analysis for the REPL read surface (complete, hover,
go-to-definition): the hand-written scanners that recover a receiver, a
completion prefix, a keyword send or a unary/binary send chain from the line
(or buffer) an editor sends up.

Everything here is a pure function of the input text, plus the class registry's
declared method return types for the chain walkers. There is no session or
binding state and no op handling: `beamtalk_repl_ops_dev` classifies the
receivers these scanners find against the session's bindings and live classes,
and shapes the op responses.

Contents:

  * `parse_receiver_and_prefix/1` — completion receiver and prefix of a line
    (including pipeline `|>` stages, ADR 0145)
  * `keyword_send_at/3` — the keyword send whose keyword ends at a hovered
    offset (hover and the `nav-query` `keyword_selector` kind)
  * `tokenise_send_chain/1`, `tokenise_binary_chain/1` — unary and mixed
    unary/binary send chains
  * `walk_chain/2`, `walk_chain_class/2`, `walk_mixed_chain/2`,
    `walk_mixed_chain_class/2` — follow a chain's declared return types
  * `is_identifier_char/1` — the completion word-boundary rule

These deliberately live beside, not on top of, the Rust compiler's lexer: the
live REPL resolves against the running image, not an AST
(`docs/development/surface-parity.md`, `complete` and `hover`). The word
boundary rule is pinned to the Rust `completion_word_start` by a shared corpus.

Extracted from beamtalk_repl_ops_dev (BT-4706).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    parse_receiver_and_prefix/1,
    keyword_send_at/3,
    tokenise_send_chain/1,
    tokenise_binary_chain/1,
    walk_chain/2,
    walk_chain_class/2,
    walk_mixed_chain/2,
    walk_mixed_chain_class/2
]).

%% Export internals for white-box testing.
-ifdef(TEST).
-export([
    %% White-box test of the completion word-boundary rule against
    %% the shared Rust/Erlang conformance corpus.
    is_identifier_char/1
]).
-endif.

-export_type([chain_hop/0]).

%% Most bytes of editor context (before + code + suffix) the hover keyword-send
%% scanner reads; the client sends a few lines either side of the hovered one.
-define(HOVER_MAX_SCAN_BYTES, 16384).

-doc """
Recover the keyword send whose keyword part ends exactly at the end of `Code`.

`Before` is the buffer text before `Code`'s line (empty, or ending in a
newline), `Code` the line up to and including the hovered token, and `Suffix`
the rest of the buffer after it. Returns:

  * `not_keyword`               — the hovered token is not a keyword part
                                  (the caller takes the unary/class path)
  * `{ok, Receiver, Selector}`  — the send's single-token receiver and its full
                                  keyword selector
  * `none`                      — a keyword-looking token was hovered but no
                                  send resolves: an expression receiver, a
                                  cascade continuation, a keyword inside a string
                                  or comment, or text we do not scan
                                  (interpolated string, oversized context)

The text is scanned into tokens (strings, character literals and comments are
skipped atomically by the `beamtalk_repl_eval` lexeme skippers). A newline is a
statement boundary unless the next line starts with a keyword part — the
language's own rule for splitting a send across lines. From the hovered keyword
we walk outward over its bracket level until a statement boundary (newline,
`.`, `;`, `^`, `:=`, a block's `|`) or the enclosing bracket; the keywords at
that level, in order, form the selector, and the tokens before the first one
are the receiver. Arguments may be any expression — keywords nested inside
brackets belong to inner sends and are skipped.

Examples (`Before` = `<<>>`):
  <<"Fetcher fetch:">>, <<" \"u\" options: #()">>
      → {ok, <<"Fetcher">>, <<"fetch:options:">>}
  <<"Fetcher fetch: \"u\" options:">>, <<" #()">>
      → {ok, <<"Fetcher">>, <<"fetch:options:">>}
  <<"x := (d at:">>, <<" 1 put: 2) size">>
      → {ok, <<"d">>, <<"at:put:">>}
  <<"Integer max">>, <<>>    → not_keyword
""".
-spec keyword_send_at(binary(), binary(), binary()) ->
    {ok, binary(), binary()} | none | not_keyword.
keyword_send_at(Before, Code, Suffix) when
    byte_size(Before) + byte_size(Code) + byte_size(Suffix) > ?HOVER_MAX_SCAN_BYTES
->
    %% Hover fires whenever the pointer rests, so bound the scan rather than
    %% stall on a huge pasted buffer.
    keyword_fallback(Code);
keyword_send_at(Before, Code, Suffix) ->
    Text = binary_to_list(<<Before/binary, Code/binary, Suffix/binary>>),
    HoverEnd = byte_size(Before) + byte_size(Code),
    Total = length(Text),
    case hover_tokens(Text, Total, Total, []) of
        unsupported ->
            %% An interpolated string makes the text unscannable here.
            keyword_fallback(Code);
        {ok, Tokens} ->
            {Left, Right} = lists:splitwith(
                fun({_Kind, _Text, End}) -> End =< HoverEnd end,
                hover_resolve_newlines(Tokens)
            ),
            case lists:reverse(Left) of
                [{keyword, _, HoverEnd} = Hovered | LeftRev] ->
                    Segment =
                        hover_walk_left(LeftRev, 0, []) ++
                            [{0, Hovered}] ++
                            hover_walk_right(Right, 0),
                    keyword_send_of(Segment);
                _ ->
                    %% Not a keyword token. A `…:` hovered inside a string or
                    %% comment must not fall back to a one-keyword lookup.
                    keyword_fallback(Code)
            end
    end.

%% No keyword send to resolve: a hovered `…:` (the client only sends one for a
%% keyword part) shows nothing (`none`); any other token keeps the unary/class
%% path, which does not need the scan.
-spec keyword_fallback(binary()) -> none | not_keyword.
keyword_fallback(Code) ->
    case binary:longest_common_suffix([Code, <<":">>]) of
        1 -> none;
        0 -> not_keyword
    end.

-spec keyword_send_of([{non_neg_integer(), hover_token()}]) ->
    {ok, binary(), binary()} | none.
keyword_send_of(Segment) ->
    IsTopKeyword = fun
        ({0, {keyword, _, _}}) -> true;
        (_) -> false
    end,
    Keywords = [Text || {0, {keyword, Text, _}} <- Segment],
    case lists:takewhile(fun(T) -> not IsTopKeyword(T) end, Segment) of
        [{0, {word, Receiver, _}}] ->
            {ok, Receiver, iolist_to_binary(Keywords)};
        _ ->
            none
    end.

%% A scanned token: kind, source text, and end offset (bytes) in the text.
-type hover_token() :: {
    keyword | word | param | open | close | sep | newline | other, binary(), non_neg_integer()
}.

%% A newline ends the statement unless the next line starts with a keyword
%% part, which continues the send above (docs/beamtalk-language-features.md
%% § Splitting a Message Send Across Lines).
-spec hover_resolve_newlines([hover_token()]) -> [hover_token()].
hover_resolve_newlines([{newline, _, _} | [{newline, _, _} | _] = Rest]) ->
    hover_resolve_newlines(Rest);
hover_resolve_newlines([{newline, _, _} | [{keyword, _, _} | _] = Rest]) ->
    hover_resolve_newlines(Rest);
hover_resolve_newlines([{newline, Text, End} | Rest]) ->
    [{sep, Text, End} | hover_resolve_newlines(Rest)];
hover_resolve_newlines([T | Rest]) ->
    [T | hover_resolve_newlines(Rest)];
hover_resolve_newlines([]) ->
    [].

%% Walk left from the hovered keyword (tokens nearest-first), tagging each with
%% its bracket depth relative to the hovered keyword; stop at the enclosing open
%% bracket or a depth-0 statement boundary. Returns tokens in source order.
-spec hover_walk_left([hover_token()], non_neg_integer(), list()) ->
    [{non_neg_integer(), hover_token()}].
hover_walk_left([], _Depth, Acc) ->
    Acc;
hover_walk_left([{close, _, _} = T | Rest], Depth, Acc) ->
    hover_walk_left(Rest, Depth + 1, [{Depth, T} | Acc]);
hover_walk_left([{open, _, _} | _], 0, Acc) ->
    Acc;
hover_walk_left([{open, _, _} = T | Rest], Depth, Acc) ->
    hover_walk_left(Rest, Depth - 1, [{Depth - 1, T} | Acc]);
hover_walk_left([{sep, _, _} | _], 0, Acc) ->
    Acc;
hover_walk_left([T | Rest], Depth, Acc) ->
    hover_walk_left(Rest, Depth, [{Depth, T} | Acc]).

%% Mirror of `hover_walk_left/3` rightwards: stop at the enclosing close
%% bracket or a depth-0 statement boundary.
-spec hover_walk_right([hover_token()], non_neg_integer()) ->
    [{non_neg_integer(), hover_token()}].
hover_walk_right([], _Depth) ->
    [];
hover_walk_right([{open, _, _} = T | Rest], Depth) ->
    [{Depth, T} | hover_walk_right(Rest, Depth + 1)];
hover_walk_right([{close, _, _} | _], 0) ->
    [];
hover_walk_right([{close, _, _} = T | Rest], Depth) ->
    [{Depth - 1, T} | hover_walk_right(Rest, Depth - 1)];
hover_walk_right([{sep, _, _} | _], 0) ->
    [];
hover_walk_right([T | Rest], Depth) ->
    [{Depth, T} | hover_walk_right(Rest, Depth)].

%% Scan text (a byte list) into hover tokens. `Len` is the length of the
%% remaining text and `Total` that of the whole, so a token ending with `Len'`
%% bytes left ends at offset `Total - Len'`. Returns `unsupported` when an
%% interpolated string makes the text unscannable.
-spec hover_tokens(string(), non_neg_integer(), non_neg_integer(), [hover_token()]) ->
    {ok, [hover_token()]} | unsupported.
hover_tokens([], _Len, _Total, Acc) ->
    {ok, lists:reverse(Acc)};
hover_tokens([$\n | _] = Cs, Len, Total, Acc) ->
    hover_push(newline, Cs, 1, Len, Total, Acc);
hover_tokens([C | Rest], Len, Total, Acc) when C =:= $\s; C =:= $\t; C =:= $\r ->
    hover_tokens(Rest, Len - 1, Total, Acc);
hover_tokens([$/, $/ | _] = Cs, _Len, Total, Acc) ->
    hover_skip(beamtalk_repl_eval:skip_line_comment(Cs), Total, Acc);
hover_tokens([$/, $* | _] = Cs, _Len, Total, Acc) ->
    hover_skip(beamtalk_repl_eval:skip_block_comment(Cs), Total, Acc);
hover_tokens([$" | _] = Cs, Len, Total, Acc) ->
    case beamtalk_repl_eval:skip_string_literal(Cs) of
        {ok, Rest} -> hover_push(word, Cs, Len - length(Rest), Len, Total, Acc);
        unsupported -> unsupported
    end;
hover_tokens([$$ | _] = Cs, Len, Total, Acc) ->
    Rest = beamtalk_repl_eval:skip_character_literal(Cs),
    hover_push(word, Cs, Len - length(Rest), Len, Total, Acc);
hover_tokens([$#, C | _] = Cs, Len, Total, Acc) when C =:= $(; C =:= $[; C =:= ${ ->
    hover_push(open, Cs, 2, Len, Total, Acc);
hover_tokens([$#, $' | Rest] = Cs, Len, Total, Acc) ->
    %% Quoted symbol `#'…'` (no escapes inside symbol literals).
    {Body, Close} = lists:splitwith(fun(C) -> C =/= $' end, Rest),
    hover_push(word, Cs, 2 + length(Body) + min(length(Close), 1), Len, Total, Acc);
hover_tokens([$# | Rest] = Cs, Len, Total, Acc) ->
    %% Symbol literal `#foo` / `#at:put:` — colons included, so a symbol's
    %% keywords are never mistaken for a send's.
    {Sym, _} = lists:splitwith(fun is_identifier_char/1, Rest),
    hover_push(word, Cs, 1 + length(Sym), Len, Total, Acc);
hover_tokens([C | _] = Cs, Len, Total, Acc) when C =:= $(; C =:= $[; C =:= ${ ->
    hover_push(open, Cs, 1, Len, Total, Acc);
hover_tokens([C | _] = Cs, Len, Total, Acc) when C =:= $); C =:= $]; C =:= $} ->
    hover_push(close, Cs, 1, Len, Total, Acc);
hover_tokens([$:, $= | _] = Cs, Len, Total, Acc) ->
    hover_push(sep, Cs, 2, Len, Total, Acc);
hover_tokens([$:, $: | _] = Cs, Len, Total, Acc) ->
    hover_push(other, Cs, 2, Len, Total, Acc);
hover_tokens([$:, C | Rest] = Cs, Len, Total, Acc) when
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C =:= $_
->
    %% Block parameter `:x`.
    {Name, _} = lists:splitwith(fun is_name_char/1, Rest),
    hover_push(param, Cs, 2 + length(Name), Len, Total, Acc);
hover_tokens([$., C | _] = Cs, Len, Total, Acc) when
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C =:= $_
->
    %% `self.count` field access, not a statement-ending `.`.
    hover_push(other, Cs, 1, Len, Total, Acc);
hover_tokens([$| | _] = Cs, Len, Total, [{param, _, _} | _] = Acc) ->
    %% The `|` closing a block's parameters (`[:x | …]`). Elsewhere `|` is
    %% the binary `or` and stays inside the expression.
    hover_push(sep, Cs, 1, Len, Total, Acc);
hover_tokens([C | _] = Cs, Len, Total, Acc) when C =:= $.; C =:= $;; C =:= $^ ->
    hover_push(sep, Cs, 1, Len, Total, Acc);
hover_tokens([C | Rest] = Cs, Len, Total, Acc) when C >= $0, C =< $9 ->
    hover_push(word, Cs, Len - length(hover_skip_number(Rest)), Len, Total, Acc);
hover_tokens([C | Rest] = Cs, Len, Total, Acc) when
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C =:= $_
->
    {Name, After} = lists:splitwith(fun is_name_char/1, Rest),
    NameLen = 1 + length(Name),
    case After of
        [$:, N | _] when N =:= $:; N =:= $= ->
            hover_push(word, Cs, NameLen, Len, Total, Acc);
        [$: | _] ->
            hover_push(keyword, Cs, NameLen + 1, Len, Total, Acc);
        _ ->
            hover_push(word, Cs, NameLen, Len, Total, Acc)
    end;
hover_tokens([_ | _] = Cs, Len, Total, Acc) ->
    %% Binary-operator characters or any other byte: one `other` token each.
    %% Only keyword/word/bracket/boundary tokens drive send recovery, so finer
    %% splitting is unnecessary.
    hover_push(other, Cs, 1, Len, Total, Acc).

%% Emit the `TokLen`-byte token at the head of `Cs` and continue after it.
-spec hover_push(
    atom(), string(), pos_integer(), non_neg_integer(), non_neg_integer(), [hover_token()]
) ->
    {ok, [hover_token()]} | unsupported.
hover_push(Kind, Cs, TokLen, Len, Total, Acc) ->
    {TokChars, Rest} = lists:split(TokLen, Cs),
    RestLen = Len - TokLen,
    hover_tokens(Rest, RestLen, Total, [
        {Kind, list_to_binary(TokChars), Total - RestLen} | Acc
    ]).

%% Continue after a skipped comment (no token emitted).
-spec hover_skip(string(), non_neg_integer(), [hover_token()]) ->
    {ok, [hover_token()]} | unsupported.
hover_skip(Rest, Total, Acc) ->
    hover_tokens(Rest, length(Rest), Total, Acc).

%% Number literal tail: digits/letters/`_` (radix `16rFF`, exponents) and a `.`
%% only when a digit follows (`3.14`, not the statement-ending `3.`).
-spec hover_skip_number(string()) -> string().
hover_skip_number([$., D | Rest]) when D >= $0, D =< $9 ->
    hover_skip_number(Rest);
hover_skip_number([C | Rest]) when
    (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse C =:= $_
->
    hover_skip_number(Rest);
hover_skip_number(Rest) ->
    Rest.

%% An identifier character other than `:` (see `is_identifier_char/1`).
-spec is_name_char(char()) -> boolean().
is_name_char($:) -> false;
is_name_char(C) -> is_identifier_char(C).

-doc """
Parse the line up to the cursor into a receiver and prefix.

Returns:
  {undefined, Prefix}                — no receiver (bare prefix, e.g. <<"s">>)
  {ReceiverToken, Prefix}            — single-token receiver (e.g. <<"Integer">>, <<"42">>)
  {expression, ReceiverExpr, Prefix} — multi-token receiver expression (e.g. <<"\"hello\" size">>)

Examples:
  <<"Integer s">>       → {<<"Integer">>, <<"s">>}
  <<"Integer ">>        → {<<"Integer">>, <<>>}
  <<"42 s">>            → {<<"42">>, <<"s">>}
  <<"\"hello\" size c">>→ {expression, <<"\"hello\" size">>, <<"c">>}
  <<"s">>               → {undefined, <<"s">>}
  <<>>                  → {undefined, <<>>}
  <<"\"hi\" size |> ab">> → {expression, <<"\"hi\" size">>, <<"ab">>}

After a pipeline `|>` (ADR 0145) the receiver is the value before the last
`|>`, so `x m1 |> pre` completes `pre` against `x m1`, as the LSP does
(`pipeline_receiver_end` in `completion_provider.rs`). Both are pinned to the
shared corpus `test/fixtures/completion_pipeline_receiver_corpus.json`.
""".
-spec parse_receiver_and_prefix(binary()) ->
    {binary() | undefined, binary()} | {expression, binary(), binary()}.
parse_receiver_and_prefix(<<>>) ->
    {undefined, <<>>};
parse_receiver_and_prefix(Line) when is_binary(Line) ->
    Str = binary_to_list(Line),
    RevStr = lists:reverse(Str),
    %% Extract trailing identifier characters (the completion prefix)
    {PrefixCharsRev, Rest} = lists:splitwith(fun is_identifier_char/1, RevStr),
    Prefix = list_to_binary(lists:reverse(PrefixCharsRev)),
    %% Skip whitespace before the prefix
    {SpaceChars, ReceiverPartRev} = lists:splitwith(
        fun(C) -> C =:= $\s orelse C =:= $\t end, Rest
    ),
    case {ReceiverPartRev, SpaceChars} of
        {[$>, $| | BeforePipeRev], _} ->
            %% `value |> prefix`: completing a pipeline stage (ADR 0145).
            pipeline_stage_receiver(BeforePipeRev, Prefix);
        {_, []} ->
            %% No space before prefix — single token, no receiver
            {undefined, Prefix};
        _ ->
            %% Extract the token immediately before the space (the receiver)
            {ReceiverCharsRev, Tail} = lists:splitwith(
                fun(C) -> C =/= $\s andalso C =/= $\t end, ReceiverPartRev
            ),
            case ReceiverCharsRev of
                [] ->
                    {undefined, Prefix};
                _ ->
                    ReceiverToken = list_to_binary(lists:reverse(ReceiverCharsRev)),
                    %% Check if Tail contains non-whitespace — only then is it multi-token.
                    %% Leading indentation (e.g. "  Integer s") must not trigger the
                    %% expression path; Tail would be all-whitespace in that case.
                    case
                        lists:any(
                            fun(C) -> C =/= $\s andalso C =/= $\t end, Tail
                        )
                    of
                        false ->
                            %% Single-token receiver (possibly with leading whitespace)
                            {ReceiverToken, Prefix};
                        true ->
                            %% Multi-token receiver expression
                            ReceiverExpr = list_to_binary(lists:reverse(ReceiverPartRev)),
                            {expression, ReceiverExpr, Prefix}
                    end
            end
    end.

-doc """
The receiver and prefix when completing a pipeline stage: `Prefix` follows
the last `|>`, and `BeforePipeRev` is the reversed text before that `|>`.
The receiver is whatever `parse_receiver_and_prefix/1` finds at the end of
that text, as if the cursor sat after it.
""".
-spec pipeline_stage_receiver(string(), binary()) ->
    {binary() | undefined, binary()} | {expression, binary(), binary()}.
pipeline_stage_receiver(BeforePipeRev, Prefix) ->
    case parse_receiver_and_prefix(list_to_binary(lists:reverse(BeforePipeRev, " "))) of
        {expression, ReceiverExpr, <<>>} -> {expression, ReceiverExpr, Prefix};
        {Receiver, <<>>} -> {Receiver, Prefix}
    end.

-spec is_identifier_char(char()) -> boolean().
is_identifier_char(C) ->
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $_ orelse
        %% Colons are identifier chars so keyword selectors like `ifTrue:` and
        %% `ifTrue:ifFalse:` complete as a unit.  The rule is the Rust
        %% `completion_word_start` in
        %% crates/beamtalk-core/src/source_analysis/mod.rs; the shared corpus
        %% test/fixtures/completion_word_boundary_corpus.json pins both.
        C =:= $: orelse
        %% @ is an identifier char so `json@Parser` is treated as a
        %% single token for completions and receiver parsing.
        C =:= $@.

%%% Chain tokenisers

-doc """
Parse a binary expression into a receiver token and a list of unary selectors.

Accepts only simple unary send chains: whitespace-separated tokens where the first
is a receiver (literal, class name, or variable) and the rest are unary selectors.
Returns `error` for keyword sends mid-chain, parenthesised subexpressions, or `>>`.

Examples:
  <<"\"hello\" size">>    → {ok, <<"\"hello\"">>, [size]}
  <<"counter getValue">>  → {ok, <<"counter">>, [getValue]}
  <<"\"hello\" size abs">>→ {ok, <<"\"hello\"">>, [size, abs]}
  <<"\"hello\" |> size abs">> → {ok, <<"\"hello\"">>, [size, abs]}
  <<"inject: 0 into:">>   → error  (keyword send)
  <<"(myList size)">>     → error  (paren)
""".
-spec tokenise_send_chain(binary()) -> {ok, binary(), [atom()]} | error.
tokenise_send_chain(<<>>) ->
    error;
tokenise_send_chain(Expr) when is_binary(Expr) ->
    Parts = chain_tokens(Expr),
    case Parts of
        [] ->
            error;
        [_] ->
            %% Single token — no sends, nothing to chain-walk
            error;
        [ReceiverToken | Selectors] ->
            case validate_chain_tokens(ReceiverToken, Selectors) of
                ok ->
                    %% Use binary_to_existing_atom to avoid atom table exhaustion from
                    %% user input.  Valid selectors are already loaded as method atoms;
                    %% unknown selectors mean the chain will break anyway.
                    try
                        SelectorAtoms = [binary_to_existing_atom(S, utf8) || S <- Selectors],
                        {ok, ReceiverToken, SelectorAtoms}
                    catch
                        error:badarg -> error
                    end;
                error ->
                    error
            end
    end.

-doc """
Split a receiver expression into whitespace-separated tokens for the chain
tokenisers, dropping each pipeline `|>` token: in a chain of unary and binary
sends walked left to right, `a m1 |> m2` is the same chain as `a m1 m2`
(ADR 0145).
""".
-spec chain_tokens(binary()) -> [binary()].
chain_tokens(Expr) ->
    [
        list_to_binary(T)
     || T <- string:tokens(binary_to_list(Expr), " \t"),
        T =/= "|>"
    ].

-spec validate_chain_tokens(binary(), [binary()]) -> ok | error.
validate_chain_tokens(ReceiverToken, Selectors) ->
    AllTokens = [ReceiverToken | Selectors],
    HasInvalid = lists:any(
        fun(T) ->
            binary:match(T, <<"(">>) =/= nomatch orelse
                binary:match(T, <<")">>) =/= nomatch orelse
                binary:match(T, <<">>">>) =/= nomatch
        end,
        AllTokens
    ),
    case HasInvalid of
        true ->
            error;
        false ->
            case lists:all(fun is_valid_unary_selector/1, Selectors) of
                true -> ok;
                false -> error
            end
    end.

-doc """
A valid unary selector starts with a letter or underscore and contains no colon.
""".
-spec is_valid_unary_selector(binary()) -> boolean().
is_valid_unary_selector(<<>>) ->
    false;
is_valid_unary_selector(<<H, _/binary>> = Sel) when
    (H >= $a andalso H =< $z) orelse (H >= $A andalso H =< $Z) orelse H =:= $_
->
    binary:match(Sel, <<":">>) =:= nomatch;
is_valid_unary_selector(_) ->
    false.

-doc """
Returns true if the first character of a token is a binary selector character.

Binary selector characters: + - * / < > = ~ % & ? , \
These are the same characters recognised by the Beamtalk lexer.
""".
-spec is_binary_selector_token(binary()) -> boolean().
is_binary_selector_token(<<H, _/binary>>) ->
    lists:member(H, "+-*/<>=~%&?,\\");
is_binary_selector_token(<<>>) ->
    false.

-doc """
Returns true if the token contains characters that make it un-parseable
as part of a simple chain (parentheses or the `>>` method-reference operator).
""".
-spec has_invalid_chain_chars(binary()) -> boolean().
has_invalid_chain_chars(T) ->
    binary:match(T, <<"(">>) =/= nomatch orelse
        binary:match(T, <<")">>) =/= nomatch orelse
        binary:match(T, <<">>">>) =/= nomatch.

-doc """
Parse a whitespace-separated expression into a receiver token and a list
of mixed unary/binary hops.

Extends `tokenise_send_chain/1` to handle binary message sends mid-chain:
each binary operator token consumes the following token as its argument.
Binary hops are tagged `{binary, Selector}` and unary hops `{unary, Selector}`.

Returns `error` for:
- Empty or single-token expressions (no chain to walk)
- Tokens containing parentheses or `>>` (complex sub-expressions)
- Binary operators with no following argument token
- Keyword sends (tokens containing `:`)

Examples:
  <<"counter value + 1">>  → {ok, <<"counter">>, [{unary, value}, {binary, '+'}]}
  <<"\"foo\" , \"bar\"">>  → {ok, <<"\"foo\"">>, [{binary, ','}]}
  <<"myList size + offset">>→ {ok, <<"myList">>, [{unary, size}, {binary, '+'}]}
  <<"x + 1 * 2">>          → {ok, <<"x">>, [{binary, '+'}, {binary, '*'}]}
""".
-type chain_hop() :: {unary, atom()} | {binary, atom()}.
-spec tokenise_binary_chain(binary()) -> {ok, binary(), [chain_hop()]} | error.
tokenise_binary_chain(<<>>) ->
    error;
tokenise_binary_chain(Expr) when is_binary(Expr) ->
    Parts = chain_tokens(Expr),
    case Parts of
        [] ->
            error;
        [_] ->
            %% Single token — no sends to walk
            error;
        [ReceiverToken | HopTokens] ->
            case has_invalid_chain_chars(ReceiverToken) of
                true ->
                    error;
                false ->
                    case parse_binary_hops(HopTokens) of
                        error -> error;
                        {ok, []} -> error;
                        {ok, Hops} -> {ok, ReceiverToken, Hops}
                    end
            end
    end.

-spec parse_binary_hops([binary()]) -> {ok, [chain_hop()]} | error.
parse_binary_hops([]) ->
    {ok, []};
parse_binary_hops([Token | Rest]) ->
    case has_invalid_chain_chars(Token) of
        true ->
            error;
        false ->
            case is_binary_selector_token(Token) of
                true ->
                    %% Binary op: consume the following token as the argument
                    case Rest of
                        [] ->
                            %% Binary operator with no argument — malformed
                            error;
                        [ArgToken | Rest2] ->
                            case
                                has_invalid_chain_chars(ArgToken) orelse
                                    binary:match(ArgToken, <<":">>) =/= nomatch
                            of
                                true ->
                                    error;
                                false ->
                                    try
                                        Sel = binary_to_existing_atom(Token, utf8),
                                        case parse_binary_hops(Rest2) of
                                            {ok, MoreHops} -> {ok, [{binary, Sel} | MoreHops]};
                                            error -> error
                                        end
                                    catch
                                        error:badarg -> error
                                    end
                            end
                    end;
                false ->
                    %% Must be a valid unary selector (no colon, starts with letter/_)
                    case is_valid_unary_selector(Token) of
                        true ->
                            try
                                Sel = binary_to_existing_atom(Token, utf8),
                                case parse_binary_hops(Rest) of
                                    {ok, MoreHops} -> {ok, [{unary, Sel} | MoreHops]};
                                    error -> error
                                end
                            catch
                                error:badarg -> error
                            end;
                        false ->
                            %% Keyword send or other unrecognised token
                            error
                    end
            end
    end.

-doc """
Walk an instance-side send chain containing mixed unary and binary hops.

For each `{unary, Sel}` hop: looks up `get_method_return_type(ClassName, Sel)`.
For each `{binary, Sel}` hop: looks up `get_method_return_type(ClassName, Sel)`.
Returns `undefined` (graceful fallback) when any hop lacks a return-type annotation.
""".
-spec walk_mixed_chain(atom(), [chain_hop()]) -> {ok, atom(), instance | class} | undefined.
walk_mixed_chain(ClassName, Hops) ->
    walk_mixed_chain(ClassName, Hops, 0).

-spec walk_mixed_chain(atom(), [chain_hop()], non_neg_integer()) ->
    {ok, atom(), instance | class} | undefined.
walk_mixed_chain(ClassName, [], _Depth) ->
    {ok, ClassName, instance};
walk_mixed_chain(_ClassName, _Hops, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    undefined;
walk_mixed_chain(ClassName, [{unary, class} | Rest], _Depth) ->
    %% `instance class` transitions to the class side.
    walk_mixed_chain_class(ClassName, Rest);
walk_mixed_chain(ClassName, [{_Kind, Sel} | Rest], Depth) ->
    case beamtalk_class_registry:get_method_return_type(ClassName, Sel) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_mixed_chain(NextClass, Rest, Depth + 1);
        %% ADR 0068: Tagged tuple return types (type_param, generic) cannot be
        %% resolved without caller annotation context — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

-doc """
Walk a chain starting from the class side with mixed unary/binary hops.

The first hop uses `get_class_method_return_type`; subsequent hops transition to
instance-side via `walk_mixed_chain/3`.
""".
-spec walk_mixed_chain_class(atom(), [chain_hop()]) -> {ok, atom(), instance | class} | undefined.
walk_mixed_chain_class(ClassName, []) ->
    {ok, ClassName, class};
walk_mixed_chain_class(ClassName, [{unary, class} | Rest]) ->
    %% `ClassName class` → metaclass; stay on the class side for completions.
    walk_mixed_chain_class(ClassName, Rest);
walk_mixed_chain_class(ClassName, [{_Kind, Sel} | Rest]) ->
    case beamtalk_class_registry:get_class_method_return_type(ClassName, Sel) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_mixed_chain(NextClass, Rest, 1);
        %% ADR 0068: Tagged tuple return types — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

%%% Chain walkers

-doc """
Walk an instance-side send chain, following method return types at each hop.

Returns `{ok, FinalClassName}` when every hop has a known return type,
or `undefined` when any hop breaks (annotation absent or non-Simple).
""".
-spec walk_chain(atom(), [atom()]) -> {ok, atom(), instance | class} | undefined.
walk_chain(ClassName, Selectors) ->
    walk_chain(ClassName, Selectors, 0).

-spec walk_chain(atom(), [atom()], non_neg_integer()) -> {ok, atom(), instance | class} | undefined.
walk_chain(ClassName, [], _Depth) ->
    {ok, ClassName, instance};
walk_chain(_ClassName, _Selectors, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    undefined;
walk_chain(ClassName, [class | Rest], _Depth) ->
    %% `instance class` transitions to the class side — offer class methods.
    walk_chain_class(ClassName, Rest);
walk_chain(ClassName, [Selector | Rest], Depth) ->
    case beamtalk_runtime_api:get_method_return_type(ClassName, Selector) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_chain(NextClass, Rest, Depth + 1);
        %% ADR 0068: Tagged tuple return types — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

-doc """
Walk a chain starting from the class side.

The first hop uses `class_method_return_types`; subsequent hops transition to the
instance side via `walk_chain/2`.

Special case: `class` is an instance method on ProtoObject (not annotated with a
return type). When a class object receives `class`, it returns its metaclass, which
has the same class-side methods for completion purposes. We stay on the class side
rather than failing the chain.
""".
-spec walk_chain_class(atom(), [atom()]) -> {ok, atom(), instance | class} | undefined.
walk_chain_class(ClassName, []) ->
    {ok, ClassName, class};
walk_chain_class(ClassName, [class | Rest]) ->
    %% `ClassName class` → metaclass; for completions treat as still on the class side.
    walk_chain_class(ClassName, Rest);
walk_chain_class(ClassName, [Selector | Rest]) ->
    case beamtalk_runtime_api:get_class_method_return_type(ClassName, Selector) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_chain(NextClass, Rest, 1);
        %% ADR 0068: Tagged tuple return types — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.
