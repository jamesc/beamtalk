%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_spec_reader (ADR 0075, Phase 0 spike).
%%
%% Tests spec extraction from `.beam` files and Erlang→Beamtalk type mapping.

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
