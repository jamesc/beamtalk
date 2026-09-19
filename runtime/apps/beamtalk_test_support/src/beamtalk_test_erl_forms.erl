%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_erl_forms).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: parse Erlang source text into abstract forms suitable
for `compile:forms/2`.

Extracted from `beamtalk_behaviour_intrinsics_tests`'s own `parse_forms/1`,
once `beamtalk_shape_migration_tests` needed the identical helper — same
"copied once, extracted on the second use" precedent `beamtalk_test_corpus`'s
own moduledoc documents (CLAUDE.md's no-duplicate-implementations rule).
""".

-export([parse_forms/1]).

-doc """
Parse a list of complete top-level Erlang source forms (each ending in `.`)
into abstract forms suitable for `compile:forms/2`.
""".
-spec parse_forms([string()]) -> [erl_parse:abstract_form()].
parse_forms(SourceLines) ->
    lists:map(
        fun(Line) ->
            {ok, Tokens, _} = erl_scan:string(Line),
            {ok, Form} = erl_parse:parse_form(Tokens),
            Form
        end,
        SourceLines
    ).
