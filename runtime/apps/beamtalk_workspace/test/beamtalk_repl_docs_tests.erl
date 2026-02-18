%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_docs module
%%%
%%% Tests EEP-48 doc chunk extraction, formatting, and error handling.
%%% Uses compiled stdlib .beam files as real fixtures for doc chunk tests.

-module(beamtalk_repl_docs_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% safe_to_existing_atom tests
%%====================================================================

safe_to_existing_atom_known_atom_test() ->
    %% 'ok' is always a known atom
    ?assertEqual({ok, ok}, beamtalk_repl_docs:safe_to_existing_atom(<<"ok">>)).

safe_to_existing_atom_empty_binary_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_docs:safe_to_existing_atom(<<>>)).

safe_to_existing_atom_unknown_atom_test() ->
    %% Random string unlikely to be an existing atom
    ?assertEqual({error, badarg},
                 beamtalk_repl_docs:safe_to_existing_atom(<<"xyzzy_not_an_atom_29384756">>)).

safe_to_existing_atom_plus_operator_test() ->
    %% '+' should be a known atom (used in Erlang)
    ?assertEqual({ok, '+'}, beamtalk_repl_docs:safe_to_existing_atom(<<"+">>)).

%%====================================================================
%% format_superclass tests
%%====================================================================

format_superclass_none_test() ->
    ?assertEqual([], beamtalk_repl_docs:format_superclass(none)).

format_superclass_with_class_test() ->
    Result = iolist_to_binary(beamtalk_repl_docs:format_superclass('Object')),
    ?assertEqual(<<" < Object">>, Result).

%%====================================================================
%% group_by_class tests
%%====================================================================

group_by_class_empty_test() ->
    ?assertEqual([], beamtalk_repl_docs:group_by_class([])).

group_by_class_single_class_test() ->
    Input = [{foo, 'Object'}, {bar, 'Object'}],
    Expected = [{'Object', [bar, foo]}],
    ?assertEqual(Expected, beamtalk_repl_docs:group_by_class(Input)).

group_by_class_multiple_classes_test() ->
    Input = [{foo, 'Object'}, {bar, 'Number'}, {baz, 'Object'}],
    Result = beamtalk_repl_docs:group_by_class(Input),
    ?assertEqual([{'Number', [bar]}, {'Object', [baz, foo]}], Result).

group_by_class_sorted_output_test() ->
    Input = [{z, 'B'}, {a, 'A'}, {m, 'B'}],
    Result = beamtalk_repl_docs:group_by_class(Input),
    %% Classes sorted alphabetically, selectors sorted within each class
    ?assertEqual([{'A', [a]}, {'B', [m, z]}], Result).

%%====================================================================
%% format_class_output tests (pure formatting, no runtime needed)
%%====================================================================

format_class_output_minimal_test() ->
    %% Class with no docs, no methods
    Result = beamtalk_repl_docs:format_class_output(
        'Empty', none, none, [], []),
    ?assert(binary:match(Result, <<"== Empty ==">>)       =/= nomatch),
    ?assert(binary:match(Result, <<":help">>)             =/= nomatch).

format_class_output_with_superclass_test() ->
    Result = beamtalk_repl_docs:format_class_output(
        'Counter', 'Actor', none, [], []),
    ?assert(binary:match(Result, <<"== Counter < Actor ==">>)  =/= nomatch).

format_class_output_with_module_doc_test() ->
    Result = beamtalk_repl_docs:format_class_output(
        'Integer', 'Number', <<"Whole number arithmetic.">>, [], []),
    ?assert(binary:match(Result, <<"== Integer < Number ==">>)      =/= nomatch),
    ?assert(binary:match(Result, <<"Whole number arithmetic.">>)    =/= nomatch).

format_class_output_with_own_methods_test() ->
    OwnDocs = [
        {'+', <<"+ other">>, <<"Add two numbers.">>},
        {'-', <<"- other">>, none}
    ],
    Result = beamtalk_repl_docs:format_class_output(
        'Integer', 'Number', none, OwnDocs, []),
    ?assert(binary:match(Result, <<"Instance methods:">>)  =/= nomatch),
    ?assert(binary:match(Result, <<"+ other">>)            =/= nomatch),
    ?assert(binary:match(Result, <<"- other">>)            =/= nomatch).

format_class_output_with_inherited_methods_test() ->
    InheritedGrouped = [{'Object', [class, respondsTo]}],
    Result = beamtalk_repl_docs:format_class_output(
        'Integer', 'Number', none, [], InheritedGrouped),
    ?assert(binary:match(Result, <<"Inherited from Object (2 methods)">>)  =/= nomatch),
    ?assert(binary:match(Result, <<"class, respondsTo">>)                  =/= nomatch).

format_class_output_inherited_truncation_test() ->
    %% More than 5 inherited methods should show "... (N more)"
    InheritedGrouped = [{'Object', [a, b, c, d, e, f, g]}],
    Result = beamtalk_repl_docs:format_class_output(
        'MyClass', none, none, [], InheritedGrouped),
    ?assert(binary:match(Result, <<"7 methods">>)    =/= nomatch),
    ?assert(binary:match(Result, <<"4 more)">>)      =/= nomatch).

format_class_output_inherited_five_or_fewer_test() ->
    %% Exactly 5 methods should show all, no truncation
    InheritedGrouped = [{'Object', [a, b, c, d, e]}],
    Result = beamtalk_repl_docs:format_class_output(
        'MyClass', none, none, [], InheritedGrouped),
    ?assert(binary:match(Result, <<"5 methods">>)         =/= nomatch),
    ?assert(binary:match(Result, <<"a, b, c, d, e">>)     =/= nomatch),
    ?assertEqual(nomatch, binary:match(Result, <<"more)">>)).

%%====================================================================
%% format_method_output tests (pure formatting)
%%====================================================================

format_method_output_own_method_test() ->
    Result = beamtalk_repl_docs:format_method_output(
        'Integer', <<"+">>, 'Integer', {<<"+ other">>, <<"Adds numbers.">>}),
    ?assert(binary:match(Result, <<"Integer >> +">>)     =/= nomatch),
    ?assert(binary:match(Result, <<"+ other">>)          =/= nomatch),
    ?assert(binary:match(Result, <<"Adds numbers.">>)    =/= nomatch),
    %% Should NOT show "inherited from" since defining class == queried class
    ?assertEqual(nomatch, binary:match(Result, <<"inherited">>)).

format_method_output_inherited_method_test() ->
    Result = beamtalk_repl_docs:format_method_output(
        'Integer', <<"class">>, 'Object', {<<"class">>, <<"Returns the class.">>}),
    ?assert(binary:match(Result, <<"Integer >> class">>)            =/= nomatch),
    ?assert(binary:match(Result, <<"(inherited from Object)">>)    =/= nomatch),
    ?assert(binary:match(Result, <<"Returns the class.">>)         =/= nomatch).

format_method_output_no_doc_test() ->
    Result = beamtalk_repl_docs:format_method_output(
        'Counter', <<"increment">>, 'Counter', {<<"increment">>, none}),
    ?assert(binary:match(Result, <<"Counter >> increment">>)  =/= nomatch),
    ?assert(binary:match(Result, <<"increment">>)             =/= nomatch).

%%====================================================================
%% Metaclass documentation tests (BT-618)
%%====================================================================

format_metaclass_docs_test() ->
    Result = beamtalk_repl_docs:format_metaclass_docs(),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"== Metaclass ==">>)          =/= nomatch),
    ?assert(binary:match(Result, <<"terminal sentinel">>)        =/= nomatch),
    ?assert(binary:match(Result, <<"new">>)                      =/= nomatch),
    ?assert(binary:match(Result, <<"spawn">>)                    =/= nomatch),
    ?assert(binary:match(Result, <<"spawnWith:">>)               =/= nomatch),
    ?assert(binary:match(Result, <<"describe">>)                 =/= nomatch).

format_class_docs_metaclass_test() ->
    ?assertMatch({ok, _}, beamtalk_repl_docs:format_class_docs('Metaclass')),
    {ok, Result} = beamtalk_repl_docs:format_class_docs('Metaclass'),
    ?assert(binary:match(Result, <<"== Metaclass ==">>)  =/= nomatch).

format_metaclass_method_doc_known_test() ->
    {ok, Result} = beamtalk_repl_docs:format_metaclass_method_doc(<<"spawn">>),
    ?assert(binary:match(Result, <<"Metaclass >> spawn">>)  =/= nomatch),
    ?assert(binary:match(Result, <<"actor">>)               =/= nomatch).

format_metaclass_method_doc_new_test() ->
    {ok, Result} = beamtalk_repl_docs:format_metaclass_method_doc(<<"new">>),
    ?assert(binary:match(Result, <<"Metaclass >> new">>)       =/= nomatch),
    ?assert(binary:match(Result, <<"new instance">>)           =/= nomatch).

format_metaclass_method_doc_unknown_test() ->
    ?assertMatch(
        {error, {method_not_found, 'Metaclass', <<"unknownMethod">>}},
        beamtalk_repl_docs:format_metaclass_method_doc(<<"unknownMethod">>)
    ).

format_method_doc_metaclass_test() ->
    ?assertMatch({ok, _}, beamtalk_repl_docs:format_method_doc('Metaclass', <<"spawn">>)),
    {ok, Result} = beamtalk_repl_docs:format_method_doc('Metaclass', <<"spawn">>),
    ?assert(binary:match(Result, <<"Metaclass >> spawn">>)  =/= nomatch).

format_method_doc_metaclass_unknown_test() ->
    ?assertMatch(
        {error, {method_not_found, 'Metaclass', <<"unknownMethod">>}},
        beamtalk_repl_docs:format_method_doc('Metaclass', <<"unknownMethod">>)
    ).

%%====================================================================
%% EEP-48 doc chunk tests (require stdlib .beam files)
%%====================================================================

%% Setup: ensure stdlib code path is available
stdlib_setup() ->
    case code:lib_dir(beamtalk_stdlib, ebin) of
        StdlibEbin when is_list(StdlibEbin) ->
            case filelib:is_dir(StdlibEbin) of
                true ->
                    code:add_pathz(StdlibEbin),
                    ok;
                false ->
                    stdlib_setup_fallback()
            end;
        {error, _} ->
            stdlib_setup_fallback()
    end.

stdlib_setup_fallback() ->
    RelPath = "apps/beamtalk_stdlib/ebin",
    case filelib:is_dir(RelPath) of
        true ->
            code:add_pathz(RelPath),
            ok;
        false ->
            {skip, stdlib_not_built}
    end.

get_module_doc_existing_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"get_module_doc returns doc for Integer",
       fun() ->
           Result = beamtalk_repl_docs:get_module_doc('bt@stdlib@integer'),
           ?assertMatch(B when is_binary(B), Result),
           ?assert(byte_size(Result) > 0)
       end}
     ]}.

get_module_doc_missing_module_test() ->
    ?assertEqual(none, beamtalk_repl_docs:get_module_doc('nonexistent_module_xyz_123')).

get_doc_entries_existing_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"get_doc_entries returns entries for Integer",
       fun() ->
           Entries = beamtalk_repl_docs:get_doc_entries('bt@stdlib@integer'),
           ?assert(is_list(Entries)),
           ?assert(length(Entries) > 0)
       end}
     ]}.

get_doc_entries_missing_module_test() ->
    ?assertEqual([], beamtalk_repl_docs:get_doc_entries('nonexistent_module_xyz_123')).

find_doc_entry_found_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"find_doc_entry finds + selector in Integer docs",
       fun() ->
           Entries = beamtalk_repl_docs:get_doc_entries('bt@stdlib@integer'),
           Result = beamtalk_repl_docs:find_doc_entry('+', Entries),
           ?assertMatch({ok, _, _}, Result),
           {ok, Sig, DocText} = Result,
           ?assert(is_binary(Sig)),
           ?assertMatch(B when is_binary(B), DocText)
       end}
     ]}.

find_doc_entry_not_found_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"find_doc_entry returns not_found for unknown selector",
       fun() ->
           Entries = beamtalk_repl_docs:get_doc_entries('bt@stdlib@integer'),
           ?assertEqual(not_found, beamtalk_repl_docs:find_doc_entry('nonexistent_xyz', Entries))
       end}
     ]}.

find_doc_entry_empty_entries_test() ->
    ?assertEqual(not_found, beamtalk_repl_docs:find_doc_entry('+', [])).

get_method_signatures_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"get_method_signatures returns signatures for known selectors",
       fun() ->
           Result = beamtalk_repl_docs:get_method_signatures('bt@stdlib@integer', ['+', '-']),
           ?assertEqual(2, length(Result)),
           %% Each entry is {Selector, Signature, Doc}
           [{PlusSel, PlusSig, _PlusDoc}, {MinusSel, MinusSig, _MinusDoc}] = Result,
           ?assertEqual('+', PlusSel),
           ?assertEqual('-', MinusSel),
           ?assert(is_binary(PlusSig)),
           ?assert(is_binary(MinusSig))
       end}
     ]}.

get_method_signatures_unknown_selector_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"get_method_signatures falls back to atom name for unknown selector",
       fun() ->
           Result = beamtalk_repl_docs:get_method_signatures('bt@stdlib@integer', [unknownMethod]),
           ?assertEqual(1, length(Result)),
           [{unknownMethod, Sig, none}] = Result,
           ?assertEqual(<<"unknownMethod">>, Sig)
       end}
     ]}.

get_method_doc_found_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"get_method_doc returns signature and doc text",
       fun() ->
           {Sig, DocText} = beamtalk_repl_docs:get_method_doc('bt@stdlib@integer', '+'),
           ?assert(is_binary(Sig)),
           ?assertMatch(B when is_binary(B), DocText)
       end}
     ]}.

get_method_doc_not_found_test_() ->
    {setup, fun stdlib_setup/0, fun(_) -> ok end,
     [{"get_method_doc falls back for unknown selector",
       fun() ->
           {Sig, DocText} = beamtalk_repl_docs:get_method_doc('bt@stdlib@integer', 'xyzzyNotReal'),
           ?assertEqual(<<"xyzzyNotReal">>, Sig),
           ?assertEqual(none, DocText)
       end}
     ]}.

%%====================================================================
%% Integration tests (require running runtime with bootstrap)
%%====================================================================

integration_setup() ->
    case stdlib_setup() of
        {skip, _} = Skip -> Skip;
        ok ->
            case code:which('bt@stdlib@integer') of
                non_existing ->
                    {skip, stdlib_not_built};
                _ ->
                    application:ensure_all_started(beamtalk_runtime),
                    case whereis(beamtalk_bootstrap) of
                        undefined ->
                            case beamtalk_bootstrap:start_link() of
                                {ok, _} -> ok;
                                {error, {already_started, _}} -> ok
                            end;
                        _ -> ok
                    end,
                    beamtalk_stdlib:init(),
                    %% Wait for Integer class to be registered
                    wait_for_class('Integer', 50),
                    ok
            end
    end.

wait_for_class(ClassName, 0) ->
    error({class_not_registered, ClassName});
wait_for_class(ClassName, Retries) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            timer:sleep(50),
            wait_for_class(ClassName, Retries - 1);
        _Pid -> ok
    end.

format_class_docs_integer_test_() ->
    {setup, fun integration_setup/0, fun(_) -> ok end,
     [{"format_class_docs returns docs for Integer",
       fun() ->
           {ok, Result} = beamtalk_repl_docs:format_class_docs('Integer'),
           ?assert(is_binary(Result)),
           ?assert(binary:match(Result, <<"== Integer">>)         =/= nomatch),
           ?assert(binary:match(Result, <<"Instance methods:">>)  =/= nomatch),
           ?assert(binary:match(Result, <<":help">>)              =/= nomatch)
       end},
      {"format_class_docs returns error for unknown class",
       fun() ->
           ?assertMatch({error, {class_not_found, 'NoSuchClass12345'}},
                        beamtalk_repl_docs:format_class_docs('NoSuchClass12345'))
       end}
     ]}.

format_method_doc_known_test_() ->
    {setup, fun integration_setup/0, fun(_) -> ok end,
     [{"format_method_doc returns docs for Integer +",
       fun() ->
           {ok, Result} = beamtalk_repl_docs:format_method_doc('Integer', <<"+">>),
           ?assert(is_binary(Result)),
           ?assert(binary:match(Result, <<"Integer >> +">>)  =/= nomatch)
       end},
      {"format_method_doc returns error for unknown class",
       fun() ->
           ?assertMatch({error, {class_not_found, 'NoSuchClass12345'}},
                        beamtalk_repl_docs:format_method_doc('NoSuchClass12345', <<"foo">>))
       end},
      {"format_method_doc returns error for unknown method",
       fun() ->
           ?assertMatch({error, {method_not_found, 'Integer', <<"xyzzy_not_a_method">>}},
                        beamtalk_repl_docs:format_method_doc('Integer', <<"xyzzy_not_a_method">>))
       end}
     ]}.
