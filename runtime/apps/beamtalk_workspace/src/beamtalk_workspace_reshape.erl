%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_reshape).

%%% **DDD Context:** Workspace Context

-moduledoc """
Pure-Erlang method-source reshaping shared by the ChangeLog install hook
(`beamtalk_repl_loader`) and flush (`beamtalk_workspace_flush`).

`reindent_method_source/2` is a byte-for-byte mirror of the Rust
`beamtalk_core::unparse::reindent_method_source` (the canonical reference and
test oracle). It is kept in Erlang so the install-hook reshape — which runs on
every live method patch — has **no transport-level failure path**: the former
`beamtalk_compiler:reindent_method_source/2` routed this pure string transform
through the compiler port (gen_server → port), giving it `port_error` / `noproc`
/ `timeout` failure modes it does not have as plain Erlang. A transient port
hiccup during install could downgrade a perfectly valid ChangeEntry to
`flushable => false`; a pure-Erlang reshape removes that path entirely (BT-2592).

The `beamtalk_workspace_reshape_tests` suite asserts byte-identical parity with
the Rust implementation across the stdlib+examples corpus.

It also holds the trailing-newline / leading-whitespace helpers that the install
hook and flush both need, so they live in one place rather than copied verbatim.
""".

-export([
    reindent_method_source/2,
    strip_trailing_newlines/1,
    ensure_trailing_newline/1,
    leading_ws/1
]).

%% Re-indent a canonical (column-0) method source so its least-indented line
%% sits at `BaseIndent', preserving relative indentation — a byte-for-byte
%% mirror of Rust `reindent_method_source(BaseIndent, Source)' (BT-2584/2592).
%%
%% Semantics, matching the on-disk byte-span slice the reshaped body must be a
%% drop-in for:
%%
%%   * The least leading-whitespace width across **non-blank** lines (normally 0
%%     for canonical output) is stripped from every line, then `BaseIndent' is
%%     prepended — relative indentation inside the body is preserved.
%%   * **Blank lines** (whitespace-only, including empty) are emitted empty: no
%%     `BaseIndent', no trailing whitespace.
%%   * A trailing newline (the span includes the body's terminating newline) is
%%     preserved: a final empty segment after the last `\n' stays empty.
%%
%% `BaseIndent' is expected to be a run of spaces/tabs (the leading whitespace of
%% the on-disk definition's first line); an empty `BaseIndent' makes this the
%% identity transform for canonical input. Unlike the retired
%% `beamtalk_compiler:reindent_method_source/2' port call, this is total — it
%% always returns a binary and never fails.
-spec reindent_method_source(binary(), binary()) -> binary().
reindent_method_source(Source, BaseIndent) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    MinIndent = min_non_blank_indent(Lines),
    Reshaped = [reshape_line(Line, BaseIndent, MinIndent) || Line <- Lines],
    iolist_to_binary(lists:join(<<"\n">>, Reshaped)).

%% The minimum leading-whitespace width across non-blank lines, or 0 when every
%% line is blank (mirrors Rust's `.min().unwrap_or(0)').
-spec min_non_blank_indent([binary()]) -> non_neg_integer().
min_non_blank_indent(Lines) ->
    Widths = [leading_ws_len(Line) || Line <- Lines, not is_blank_line(Line)],
    case Widths of
        [] -> 0;
        _ -> lists:min(Widths)
    end.

%% Reshape a single line: blank lines become empty; non-blank lines have
%% `MinIndent' leading bytes stripped (always whitespace) and `BaseIndent'
%% prepended.
-spec reshape_line(binary(), binary(), non_neg_integer()) -> binary().
reshape_line(Line, BaseIndent, MinIndent) ->
    case is_blank_line(Line) of
        true ->
            <<>>;
        false ->
            Stripped = binary:part(Line, MinIndent, byte_size(Line) - MinIndent),
            <<BaseIndent/binary, Stripped/binary>>
    end.

%% The number of leading space/tab bytes in `Line'.
-spec leading_ws_len(binary()) -> non_neg_integer().
leading_ws_len(Line) -> leading_ws_len(Line, 0).

leading_ws_len(Line, N) ->
    case Line of
        <<_:N/binary, C, _/binary>> when C =:= $\s; C =:= $\t ->
            leading_ws_len(Line, N + 1);
        _ ->
            N
    end.

%% Whether `Line' is blank: empty or only spaces/tabs.
-spec is_blank_line(binary()) -> boolean().
is_blank_line(Line) -> leading_ws_len(Line) =:= byte_size(Line).

%% The leading run of spaces/tabs of the first line of `Body' — the base
%% indentation of the on-disk method definition a span covers.
-spec leading_ws(binary()) -> binary().
leading_ws(Body) ->
    %% `binary:split/2' always returns a non-empty list (the whole binary when
    %% the delimiter is absent), so the first element is the first line.
    [First | _] = binary:split(Body, <<"\n">>),
    binary:part(First, 0, leading_ws_len(First)).

-spec strip_trailing_newlines(binary()) -> binary().
strip_trailing_newlines(<<>>) ->
    <<>>;
strip_trailing_newlines(Bin) ->
    case binary:last(Bin) of
        $\n -> strip_trailing_newlines(binary:part(Bin, 0, byte_size(Bin) - 1));
        _ -> Bin
    end.

-spec ensure_trailing_newline(binary()) -> binary().
ensure_trailing_newline(<<>>) ->
    <<"\n">>;
ensure_trailing_newline(Bin) ->
    case binary:last(Bin) of
        $\n -> Bin;
        _ -> <<Bin/binary, "\n">>
    end.
