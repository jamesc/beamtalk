%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_spec_reader (ADR 0075, Phase 1).
%%
%% Tests spec extraction from `.beam` files, Erlang→Beamtalk type mapping,
%% multi-clause spec handling, and batch processing.

-module(beamtalk_spec_reader_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ---------------------------------------------------------------
%%% read_specs/1 — reading specs from real OTP .beam files
%%% ---------------------------------------------------------------

%% Reading specs from lists.beam succeeds and returns non-empty results.
read_specs_lists_test() ->
    BeamFile = code:which(lists),
    ?assertNotEqual(non_existing, BeamFile),
    {ok, Specs} = beamtalk_spec_reader:read_specs(BeamFile),
    ?assert(is_list(Specs)),
    ?assert(length(Specs) > 0),
    %% Verify reverse/1 is present with correct structure
    ReverseSpecs = [
        S
     || S <- Specs,
        maps:get(name, S) =:= <<"reverse">>,
        maps:get(arity, S) =:= 1
    ],
    ?assert(length(ReverseSpecs) > 0),
    [ReverseSpec | _] = ReverseSpecs,
    ?assertEqual(1, length(maps:get(params, ReverseSpec))),
    ?assertEqual(<<"List">>, maps:get(return_type, ReverseSpec)).

%% Reading specs from maps.beam succeeds and returns non-empty results.
read_specs_maps_test() ->
    BeamFile = code:which(maps),
    ?assertNotEqual(non_existing, BeamFile),
    {ok, Specs} = beamtalk_spec_reader:read_specs(BeamFile),
    ?assert(is_list(Specs)),
    ?assert(length(Specs) > 0),
    %% Verify keys/1 is present
    KeysSpecs = [
        S
     || S <- Specs,
        maps:get(name, S) =:= <<"keys">>,
        maps:get(arity, S) =:= 1
    ],
    ?assert(length(KeysSpecs) > 0).

%% Verify that lists:seq/2 has meaningful parameter names from spec variables.
read_specs_param_names_test() ->
    BeamFile = code:which(lists),
    {ok, Specs} = beamtalk_spec_reader:read_specs(BeamFile),
    SeqSpecs = [
        S
     || S <- Specs,
        maps:get(name, S) =:= <<"seq">>,
        maps:get(arity, S) =:= 2
    ],
    ?assert(length(SeqSpecs) > 0),
    [SeqSpec | _] = SeqSpecs,
    Params = maps:get(params, SeqSpec),
    ?assertEqual(2, length(Params)),
    %% Both params should have Integer type (from spec constraints)
    lists:foreach(
        fun(P) ->
            ?assertEqual(<<"Integer">>, maps:get(type, P))
        end,
        Params
    ).

%% Reading specs from a nonexistent file returns an error.
read_specs_nonexistent_file_test() ->
    Result = beamtalk_spec_reader:read_specs("/nonexistent/path/fake.beam"),
    ?assertMatch({error, {beam_lib, _}}, Result).

%%% ---------------------------------------------------------------
%%% map_type/1 — Erlang type to Beamtalk type mapping
%%% ---------------------------------------------------------------

map_type_integer_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({type, 0, integer, []})).

map_type_float_test() ->
    ?assertEqual(<<"Float">>, beamtalk_spec_reader:map_type({type, 0, float, []})).

map_type_number_test() ->
    ?assertEqual(<<"Number">>, beamtalk_spec_reader:map_type({type, 0, number, []})).

map_type_binary_test() ->
    ?assertEqual(<<"String">>, beamtalk_spec_reader:map_type({type, 0, binary, []})).

map_type_boolean_test() ->
    ?assertEqual(<<"Boolean">>, beamtalk_spec_reader:map_type({type, 0, boolean, []})).

map_type_atom_test() ->
    ?assertEqual(<<"Symbol">>, beamtalk_spec_reader:map_type({type, 0, atom, []})).

map_type_list_test() ->
    ?assertEqual(<<"List">>, beamtalk_spec_reader:map_type({type, 0, list, []})).

map_type_list_with_element_type_test() ->
    ElemType = {type, 0, integer, []},
    ?assertEqual(<<"List">>, beamtalk_spec_reader:map_type({type, 0, list, [ElemType]})).

map_type_tuple_test() ->
    ?assertEqual(<<"Tuple">>, beamtalk_spec_reader:map_type({type, 0, tuple, any})).

map_type_map_test() ->
    ?assertEqual(<<"Dictionary">>, beamtalk_spec_reader:map_type({type, 0, map, any})).

map_type_pid_test() ->
    ?assertEqual(<<"Pid">>, beamtalk_spec_reader:map_type({type, 0, pid, []})).

map_type_fun_test() ->
    ?assertEqual(<<"Block">>, beamtalk_spec_reader:map_type({type, 0, 'fun', []})).

map_type_true_test() ->
    ?assertEqual(<<"True">>, beamtalk_spec_reader:map_type({atom, 0, true})).

map_type_false_test() ->
    ?assertEqual(<<"False">>, beamtalk_spec_reader:map_type({atom, 0, false})).

map_type_nil_test() ->
    ?assertEqual(<<"Nil">>, beamtalk_spec_reader:map_type({atom, 0, nil})).

map_type_term_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, term, []})).

map_type_any_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, any, []})).

map_type_non_neg_integer_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({type, 0, non_neg_integer, []})).

map_type_pos_integer_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({type, 0, pos_integer, []})).

map_type_no_return_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, no_return, []})).

map_type_var_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({var, 0, 'T'})).

map_type_atom_literal_test() ->
    ?assertEqual(<<"Symbol">>, beamtalk_spec_reader:map_type({atom, 0, ok})).

map_type_iodata_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, iodata, []})).

map_type_iolist_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, iolist, []})).

map_type_node_test() ->
    ?assertEqual(<<"Symbol">>, beamtalk_spec_reader:map_type({type, 0, node, []})).

map_type_module_test() ->
    ?assertEqual(<<"Symbol">>, beamtalk_spec_reader:map_type({type, 0, module, []})).

map_type_char_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({type, 0, char, []})).

map_type_byte_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({type, 0, byte, []})).

map_type_string_test() ->
    ?assertEqual(<<"List">>, beamtalk_spec_reader:map_type({type, 0, string, []})).

map_type_range_test() ->
    ?assertEqual(
        <<"Integer">>,
        beamtalk_spec_reader:map_type({type, 0, range, [{integer, 0, 1}, {integer, 0, 10}]})
    ).

map_type_remote_type_test() ->
    ?assertEqual(
        <<"Dynamic">>,
        beamtalk_spec_reader:map_type({remote_type, 0, [{atom, 0, sets}, {atom, 0, set}, []]})
    ).

map_type_user_type_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({user_type, 0, my_type, []})).

map_type_annotated_test() ->
    ?assertEqual(
        <<"Integer">>,
        beamtalk_spec_reader:map_type(
            {ann_type, 0, [{var, 0, 'X'}, {type, 0, integer, []}]}
        )
    ).

map_type_integer_literal_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({integer, 0, 42})).

map_type_neg_integer_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:map_type({type, 0, neg_integer, []})).

map_type_nonempty_list_test() ->
    ?assertEqual(
        <<"List">>,
        beamtalk_spec_reader:map_type({type, 0, nonempty_list, [{type, 0, integer, []}]})
    ).

%% Union types — non-ok/error unions map each branch
map_type_union_test() ->
    ?assertEqual(
        <<"Integer | Float">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [{type, 0, integer, []}, {type, 0, float, []}]}
        )
    ).

%% Union types — dedup identical mapped types
map_type_union_dedup_test() ->
    %% integer() | non_neg_integer() both map to Integer
    ?assertEqual(
        <<"Integer">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [{type, 0, integer, []}, {type, 0, non_neg_integer, []}]}
        )
    ).

%%% ---------------------------------------------------------------
%%% Union types — ok/error Result recognition (ADR 0076 Phase 2)
%%% ---------------------------------------------------------------

%% {ok, binary()} | {error, posix()} → Result(String, Symbol)
map_type_result_ok_error_test() ->
    ?assertEqual(
        <<"Result(String, Symbol)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, ok}, {type, 0, binary, []}]},
                {type, 0, tuple, [{atom, 0, error}, {type, 0, atom, []}]}
            ]}
        )
    ).

%% {ok, pid()} | {error, term()} → Result(Pid, Dynamic)
map_type_result_pid_dynamic_test() ->
    ?assertEqual(
        <<"Result(Pid, Dynamic)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, ok}, {type, 0, pid, []}]},
                {type, 0, tuple, [{atom, 0, error}, {type, 0, term, []}]}
            ]}
        )
    ).

%% ok | {error, E} → Result(Nil, E) (bare ok in union)
map_type_result_bare_ok_test() ->
    ?assertEqual(
        <<"Result(Nil, Symbol)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {atom, 0, ok},
                {type, 0, tuple, [{atom, 0, error}, {type, 0, atom, []}]}
            ]}
        )
    ).

%% {ok, T} without error branch → Result(T, Dynamic)
map_type_result_ok_only_test() ->
    %% {ok, T} | other — ok branch present, no error branch
    ?assertEqual(
        <<"Result(String, Dynamic) | Integer">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, ok}, {type, 0, binary, []}]},
                {type, 0, integer, []}
            ]}
        )
    ).

%% {ok, T} as the sole branch in a union with no error branch
map_type_result_ok_only_pure_test() ->
    ?assertEqual(
        <<"Result(String, Dynamic)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, ok}, {type, 0, binary, []}]}
            ]}
        )
    ).

%% {error, E} without ok branch → Result(Dynamic, E)
map_type_result_error_only_test() ->
    %% {error, E} | other — error branch present, no ok branch
    ?assertEqual(
        <<"Result(Dynamic, Symbol) | Integer">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, error}, {type, 0, atom, []}]},
                {type, 0, integer, []}
            ]}
        )
    ).

%% {error, E} as the sole branch in a union with no ok branch
map_type_result_error_only_pure_test() ->
    ?assertEqual(
        <<"Result(Dynamic, Symbol)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, error}, {type, 0, atom, []}]}
            ]}
        )
    ).

%% {ok, T} | {error, E} | Other → Result(T, E) | Other
map_type_result_three_branch_test() ->
    ?assertEqual(
        <<"Result(String, Symbol) | Integer">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, ok}, {type, 0, binary, []}]},
                {type, 0, tuple, [{atom, 0, error}, {type, 0, atom, []}]},
                {type, 0, integer, []}
            ]}
        )
    ).

%% term()/any() return type (no ok/error structure) → Dynamic (unchanged)
map_type_result_term_unchanged_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, term, []})),
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({type, 0, any, []})).

%% Bare error atom in union
map_type_result_bare_error_test() ->
    ?assertEqual(
        <<"Result(String, Nil)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {type, 0, tuple, [{atom, 0, ok}, {type, 0, binary, []}]},
                {atom, 0, error}
            ]}
        )
    ).

%% Both bare ok and bare error
map_type_result_both_bare_test() ->
    ?assertEqual(
        <<"Result(Nil, Nil)">>,
        beamtalk_spec_reader:map_type(
            {type, 0, union, [
                {atom, 0, ok},
                {atom, 0, error}
            ]}
        )
    ).

%%% ---------------------------------------------------------------
%%% Real OTP module verification (ADR 0076 Phase 2)
%%% ---------------------------------------------------------------

%% file:read_file/1 → Result(String, ...) return type
verify_file_read_file_result_test() ->
    BeamFile = code:which(file),
    ?assertNotEqual(non_existing, BeamFile),
    {ok, Specs} = beamtalk_spec_reader:read_specs(BeamFile),
    ReadFileSpecs = [
        S
     || S <- Specs,
        maps:get(name, S) =:= <<"read_file">>,
        maps:get(arity, S) =:= 1
    ],
    ?assert(length(ReadFileSpecs) > 0),
    [Spec | _] = ReadFileSpecs,
    RetType = maps:get(return_type, Spec),
    %% Should start with "Result(" — the ok type is String from binary()
    ?assertMatch(<<"Result(String,", _/binary>>, RetType).

%% timer:send_after/2 → Result return type
verify_timer_send_after_result_test() ->
    BeamFile = code:which(timer),
    ?assertNotEqual(non_existing, BeamFile),
    {ok, Specs} = beamtalk_spec_reader:read_specs(BeamFile),
    SendAfterSpecs = [
        S
     || S <- Specs,
        maps:get(name, S) =:= <<"send_after">>,
        maps:get(arity, S) =:= 2
    ],
    ?assert(length(SendAfterSpecs) > 0),
    [Spec | _] = SendAfterSpecs,
    RetType = maps:get(return_type, Spec),
    %% Should be a Result type
    ?assertMatch(<<"Result(", _/binary>>, RetType).

%% application:start/1 → Result(Nil, ...) (bare ok in union)
verify_application_start_result_test() ->
    BeamFile = code:which(application),
    ?assertNotEqual(non_existing, BeamFile),
    {ok, Specs} = beamtalk_spec_reader:read_specs(BeamFile),
    StartSpecs = [
        S
     || S <- Specs,
        maps:get(name, S) =:= <<"start">>,
        maps:get(arity, S) =:= 1
    ],
    ?assert(length(StartSpecs) > 0),
    [Spec | _] = StartSpecs,
    RetType = maps:get(return_type, Spec),
    %% Should be Result(Nil, ...) because of bare ok atom
    ?assertMatch(<<"Result(Nil,", _/binary>>, RetType).

%%% ---------------------------------------------------------------
%%% Bounded fun with ok/error union (constraint resolution into Result)
%%% ---------------------------------------------------------------

%% Simulate: -spec read_file(Filename) -> {ok, Binary} | {error, Reason}
%%           when Filename :: binary(), Binary :: binary(), Reason :: atom().
%% This tests constraint resolution inside union branches.
bounded_fun_result_with_constraints_test() ->
    Forms = [
        {attribute, 1, spec,
            {{read_file, 1}, [
                {type, 2, bounded_fun, [
                    {type, 2, 'fun', [
                        {type, 2, product, [{var, 2, 'Filename'}]},
                        {type, 2, union, [
                            {type, 2, tuple, [{atom, 2, ok}, {var, 2, 'Binary'}]},
                            {type, 2, tuple, [{atom, 2, error}, {var, 2, 'Reason'}]}
                        ]}
                    ]},
                    [
                        {type, 3, constraint, [
                            {atom, 3, is_subtype},
                            [{var, 3, 'Filename'}, {type, 3, binary, []}]
                        ]},
                        {type, 4, constraint, [
                            {atom, 4, is_subtype},
                            [{var, 4, 'Binary'}, {type, 4, binary, []}]
                        ]},
                        {type, 5, constraint, [
                            {atom, 5, is_subtype},
                            [{var, 5, 'Reason'}, {type, 5, atom, []}]
                        ]}
                    ]
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    [Spec] = Result,
    ?assertEqual(<<"read_file">>, maps:get(name, Spec)),
    %% The union's type variables (Binary, Reason) must be resolved via constraints
    %% before Result recognition, yielding Result(String, Symbol)
    ?assertEqual(<<"Result(String, Symbol)">>, maps:get(return_type, Spec)).

%%% ---------------------------------------------------------------
%%% extract_param_names/1 — parameter name extraction
%%% ---------------------------------------------------------------

extract_param_names_annotated_test() ->
    ArgTypes = [
        {ann_type, 0, [{var, 0, 'From'}, {type, 0, integer, []}]},
        {ann_type, 0, [{var, 0, 'To'}, {type, 0, integer, []}]}
    ],
    Result = beamtalk_spec_reader:extract_param_names(ArgTypes),
    ?assertEqual(2, length(Result)),
    [P1, P2] = Result,
    ?assertEqual(<<"from">>, maps:get(name, P1)),
    ?assertEqual(<<"Integer">>, maps:get(type, P1)),
    ?assertEqual(<<"to">>, maps:get(name, P2)),
    ?assertEqual(<<"Integer">>, maps:get(type, P2)).

extract_param_names_bare_var_test() ->
    ArgTypes = [{var, 0, 'T'}],
    Result = beamtalk_spec_reader:extract_param_names(ArgTypes),
    ?assertEqual(1, length(Result)),
    [P1] = Result,
    ?assertEqual(<<"t">>, maps:get(name, P1)),
    ?assertEqual(<<"Dynamic">>, maps:get(type, P1)).

extract_param_names_no_var_test() ->
    ArgTypes = [{type, 0, integer, []}],
    Result = beamtalk_spec_reader:extract_param_names(ArgTypes),
    ?assertEqual(1, length(Result)),
    [P1] = Result,
    ?assertEqual(<<"arg">>, maps:get(name, P1)),
    ?assertEqual(<<"Integer">>, maps:get(type, P1)).

%%% ---------------------------------------------------------------
%%% extract_specs_from_forms/1 — processing abstract forms
%%% ---------------------------------------------------------------

extract_specs_from_forms_simple_test() ->
    Forms = [
        {attribute, 1, module, test_mod},
        {attribute, 2, spec,
            {{foo, 1}, [
                {type, 3, 'fun', [
                    {type, 3, product, [{type, 3, integer, []}]},
                    {type, 3, boolean, []}
                ]}
            ]}},
        {eof, 4}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    ?assertEqual(1, length(Result)),
    [Spec] = Result,
    ?assertEqual(<<"foo">>, maps:get(name, Spec)),
    ?assertEqual(1, maps:get(arity, Spec)),
    ?assertEqual(<<"Boolean">>, maps:get(return_type, Spec)),
    [Param] = maps:get(params, Spec),
    ?assertEqual(<<"Integer">>, maps:get(type, Param)).

extract_specs_from_forms_bounded_fun_test() ->
    %% Simulate: -spec bar(X) -> X when X :: integer().
    Forms = [
        {attribute, 1, spec,
            {{bar, 1}, [
                {type, 2, bounded_fun, [
                    {type, 2, 'fun', [
                        {type, 2, product, [{var, 2, 'X'}]},
                        {var, 2, 'X'}
                    ]},
                    [
                        {type, 2, constraint, [
                            {atom, 2, is_subtype},
                            [{var, 2, 'X'}, {type, 2, integer, []}]
                        ]}
                    ]
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    ?assertEqual(1, length(Result)),
    [Spec] = Result,
    ?assertEqual(<<"bar">>, maps:get(name, Spec)),
    ?assertEqual(<<"Integer">>, maps:get(return_type, Spec)),
    [Param] = maps:get(params, Spec),
    ?assertEqual(<<"x">>, maps:get(name, Param)),
    ?assertEqual(<<"Integer">>, maps:get(type, Param)).

extract_specs_skips_non_spec_forms_test() ->
    Forms = [
        {attribute, 1, module, test_mod},
        {attribute, 2, export, [{foo, 1}]},
        {function, 3, foo, 1, [{clause, 3, [{var, 3, '_X'}], [], [{atom, 3, ok}]}]},
        {eof, 4}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    ?assertEqual([], Result).

%%% ---------------------------------------------------------------
%%% Multi-clause specs — union of return types
%%% ---------------------------------------------------------------

%% Multi-clause spec: two clauses with different return types produce a union.
multi_clause_different_returns_test() ->
    %% -spec baz(integer()) -> boolean()
    %%         ; (float()) -> atom().
    Forms = [
        {attribute, 1, spec,
            {{baz, 1}, [
                {type, 2, 'fun', [
                    {type, 2, product, [{type, 2, integer, []}]},
                    {type, 2, boolean, []}
                ]},
                {type, 3, 'fun', [
                    {type, 3, product, [{type, 3, float, []}]},
                    {type, 3, atom, []}
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    ?assertEqual(1, length(Result)),
    [Spec] = Result,
    ?assertEqual(<<"baz">>, maps:get(name, Spec)),
    ?assertEqual(1, maps:get(arity, Spec)),
    %% Return type is union of both clauses
    ?assertEqual(<<"Boolean | Symbol">>, maps:get(return_type, Spec)),
    %% Params come from first clause
    [Param] = maps:get(params, Spec),
    ?assertEqual(<<"Integer">>, maps:get(type, Param)).

%% Multi-clause spec: same return types collapse to a single type.
multi_clause_same_returns_test() ->
    %% -spec qux(integer()) -> boolean()
    %%         ; (float()) -> boolean().
    Forms = [
        {attribute, 1, spec,
            {{qux, 1}, [
                {type, 2, 'fun', [
                    {type, 2, product, [{type, 2, integer, []}]},
                    {type, 2, boolean, []}
                ]},
                {type, 3, 'fun', [
                    {type, 3, product, [{type, 3, float, []}]},
                    {type, 3, boolean, []}
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    [Spec] = Result,
    %% Duplicate return types are deduped
    ?assertEqual(<<"Boolean">>, maps:get(return_type, Spec)).

%% Multi-clause with bounded_fun and plain fun clauses.
multi_clause_mixed_bounded_test() ->
    %% -spec mixed(X) -> X when X :: integer()
    %%           ; (binary()) -> boolean().
    Forms = [
        {attribute, 1, spec,
            {{mixed, 1}, [
                {type, 2, bounded_fun, [
                    {type, 2, 'fun', [
                        {type, 2, product, [{var, 2, 'X'}]},
                        {var, 2, 'X'}
                    ]},
                    [
                        {type, 2, constraint, [
                            {atom, 2, is_subtype},
                            [{var, 2, 'X'}, {type, 2, integer, []}]
                        ]}
                    ]
                ]},
                {type, 3, 'fun', [
                    {type, 3, product, [{type, 3, binary, []}]},
                    {type, 3, boolean, []}
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    [Spec] = Result,
    ?assertEqual(<<"Integer | Boolean">>, maps:get(return_type, Spec)),
    %% Params from first (bounded) clause
    [Param] = maps:get(params, Spec),
    ?assertEqual(<<"x">>, maps:get(name, Param)).

%% All unrecognizable clauses fall back to positional params + Dynamic.
multi_clause_all_unrecognized_test() ->
    Forms = [
        {attribute, 1, spec,
            {{weird, 2}, [
                {unknown_clause_form, 2},
                {another_unknown, 3}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    [Spec] = Result,
    ?assertEqual(<<"weird">>, maps:get(name, Spec)),
    ?assertEqual(2, maps:get(arity, Spec)),
    ?assertEqual(<<"Dynamic">>, maps:get(return_type, Spec)),
    Params = maps:get(params, Spec),
    ?assertEqual(2, length(Params)),
    [P1, P2] = Params,
    ?assertEqual(<<"arg1">>, maps:get(name, P1)),
    ?assertEqual(<<"arg2">>, maps:get(name, P2)).

%% Zero-arity function spec.
zero_arity_spec_test() ->
    Forms = [
        {attribute, 1, spec,
            {{now, 0}, [
                {type, 2, 'fun', [
                    {type, 2, product, []},
                    {type, 2, integer, []}
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    [Spec] = Result,
    ?assertEqual(<<"now">>, maps:get(name, Spec)),
    ?assertEqual(0, maps:get(arity, Spec)),
    ?assertEqual(<<"Integer">>, maps:get(return_type, Spec)),
    ?assertEqual([], maps:get(params, Spec)).

%%% ---------------------------------------------------------------
%%% merge_return_types/1
%%% ---------------------------------------------------------------

merge_return_types_empty_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:merge_return_types([])).

merge_return_types_single_test() ->
    ?assertEqual(<<"Integer">>, beamtalk_spec_reader:merge_return_types([<<"Integer">>])).

merge_return_types_multiple_distinct_test() ->
    ?assertEqual(
        <<"Integer | Boolean">>,
        beamtalk_spec_reader:merge_return_types([<<"Integer">>, <<"Boolean">>])
    ).

merge_return_types_dedup_test() ->
    ?assertEqual(
        <<"Integer">>,
        beamtalk_spec_reader:merge_return_types([<<"Integer">>, <<"Integer">>])
    ).

merge_return_types_dedup_preserves_order_test() ->
    ?assertEqual(
        <<"Integer | Boolean">>,
        beamtalk_spec_reader:merge_return_types([<<"Integer">>, <<"Boolean">>, <<"Integer">>])
    ).

merge_return_types_three_types_test() ->
    ?assertEqual(
        <<"Integer | Boolean | String">>,
        beamtalk_spec_reader:merge_return_types([<<"Integer">>, <<"Boolean">>, <<"String">>])
    ).

%%% ---------------------------------------------------------------
%%% read_specs_batch/1 — batch processing
%%% ---------------------------------------------------------------

%% Batch processing multiple OTP modules succeeds.
read_specs_batch_multiple_test() ->
    ListsBeam = code:which(lists),
    MapsBeam = code:which(maps),
    ?assertNotEqual(non_existing, ListsBeam),
    ?assertNotEqual(non_existing, MapsBeam),
    Results = beamtalk_spec_reader:read_specs_batch([ListsBeam, MapsBeam]),
    ?assertEqual(2, length(Results)),
    %% Check structure of each result
    lists:foreach(
        fun({ModName, {ok, Specs}}) ->
            ?assert(is_binary(ModName)),
            ?assert(is_list(Specs)),
            ?assert(length(Specs) > 0)
        end,
        Results
    ).

%% Batch processing with a nonexistent file returns error for that file.
read_specs_batch_with_error_test() ->
    ListsBeam = code:which(lists),
    ?assertNotEqual(non_existing, ListsBeam),
    Results = beamtalk_spec_reader:read_specs_batch([
        ListsBeam, "/nonexistent/fake.beam"
    ]),
    ?assertEqual(2, length(Results)),
    %% First should succeed
    {<<"lists">>, {ok, ListsSpecs}} = lists:nth(1, Results),
    ?assert(length(ListsSpecs) > 0),
    %% Second should fail
    {<<"fake">>, {error, _}} = lists:nth(2, Results).

%% Batch processing empty list returns empty list.
read_specs_batch_empty_test() ->
    Results = beamtalk_spec_reader:read_specs_batch([]),
    ?assertEqual([], Results).

%% Batch result module names are correct binaries.
read_specs_batch_module_names_test() ->
    ListsBeam = code:which(lists),
    Results = beamtalk_spec_reader:read_specs_batch([ListsBeam]),
    [{ModName, _}] = Results,
    ?assertEqual(<<"lists">>, ModName).

%%% ---------------------------------------------------------------
%%% Remote spec form (module:function/arity)
%%% ---------------------------------------------------------------

extract_specs_remote_spec_test() ->
    %% Remote spec: {attribute, _, spec, {{Module, Name, Arity}, Clauses}}
    Forms = [
        {attribute, 1, spec,
            {{other_mod, remote_fn, 1}, [
                {type, 2, 'fun', [
                    {type, 2, product, [{type, 2, integer, []}]},
                    {type, 2, atom, []}
                ]}
            ]}}
    ],
    Result = beamtalk_spec_reader:extract_specs_from_forms(Forms),
    ?assertEqual(1, length(Result)),
    [Spec] = Result,
    ?assertEqual(<<"remote_fn">>, maps:get(name, Spec)),
    ?assertEqual(1, maps:get(arity, Spec)),
    ?assertEqual(<<"Symbol">>, maps:get(return_type, Spec)).

%%% ---------------------------------------------------------------
%%% Graceful fallback — .beam without debug_info
%%% ---------------------------------------------------------------

read_specs_no_debug_info_test() ->
    %% Compile a simple module without debug_info, then try to read specs.
    %% We compile to a temp directory.
    TmpDir = make_tmp_dir(),
    try
        Forms = [
            {attribute, erl_anno:new(1), module, test_no_debug},
            {attribute, erl_anno:new(1), export, [{hello, 0}]},
            {function, erl_anno:new(1), hello, 0, [
                {clause, erl_anno:new(1), [], [], [{atom, erl_anno:new(1), world}]}
            ]},
            {eof, erl_anno:new(1)}
        ],
        %% Compile WITHOUT debug_info
        {ok, test_no_debug, BeamBin} = compile:forms(Forms, []),
        BeamFile = filename:join(TmpDir, "test_no_debug.beam"),
        ok = file:write_file(BeamFile, BeamBin),
        Result = beamtalk_spec_reader:read_specs(BeamFile),
        ?assertEqual({error, no_debug_info}, Result)
    after
        cleanup_tmp_dir(TmpDir)
    end.

%%% ---------------------------------------------------------------
%%% Edge cases from ADR 0075
%%% ---------------------------------------------------------------

%% opaque types map to Dynamic
map_type_opaque_via_user_type_test() ->
    %% Opaque types appear as user_type in abstract code
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({user_type, 0, opaque_type, []})).

%% Typed map (#{key := Type}) maps to Dictionary
map_type_typed_map_test() ->
    ?assertEqual(
        <<"Dictionary">>,
        beamtalk_spec_reader:map_type(
            {type, 0, map, [
                {type, 0, map_field_exact, [{atom, 0, key}, {type, 0, integer, []}]}
            ]}
        )
    ).

%% Unknown/unrecognized type forms map to Dynamic
map_type_unknown_form_test() ->
    ?assertEqual(<<"Dynamic">>, beamtalk_spec_reader:map_type({unknown_form, 0, something})).

%%% ---------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------

make_tmp_dir() ->
    TmpBase =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    TmpDir = filename:join(
        TmpBase,
        "bt_spec_reader_test_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(TmpDir, "x")),
    TmpDir.

cleanup_tmp_dir(TmpDir) ->
    lists:foreach(
        fun(F) -> file:delete(F) end,
        filelib:wildcard(filename:join(TmpDir, "*"))
    ),
    file:del_dir(TmpDir).
