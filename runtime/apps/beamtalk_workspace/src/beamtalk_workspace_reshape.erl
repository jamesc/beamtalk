%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_reshape).

%%% **DDD Context:** Workspace Context

-moduledoc """
String helpers for the ChangeLog install hook (`beamtalk_repl_loader`) and
flush (`beamtalk_workspace_flush`): leading-whitespace extraction and
trailing-newline normalisation, kept in one place rather than copied verbatim
into both callers.

Method-source *reshaping* proper — re-indenting the compiler's canonical
column-0 `unparse_method` body to the on-disk indentation — is done by the
compiler port (`beamtalk_compiler:reindent_method_source/2`). The port
re-lays-out the body so width-sensitive lines re-break at the target indent,
matching what `bt fmt` produces on disk; a pure-Erlang whitespace shift cannot
re-break a line and so would reformat such methods on flush (BT-2594). The
install hook already calls the port to compile the method, so the reshape adds
no new port round-trip.
""".

-export([
    strip_trailing_newlines/1,
    ensure_trailing_newline/1,
    leading_ws/1
]).

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
