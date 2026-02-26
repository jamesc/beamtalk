%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_compiler module

-module(beamtalk_repl_compiler_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% format_formatted_diagnostics/1
%%====================================================================

format_diagnostics_empty_test() ->
    ?assertEqual(<<"Compilation failed">>, beamtalk_repl_compiler:format_formatted_diagnostics([])).

format_diagnostics_single_test() ->
    ?assertEqual(
        <<"Unexpected token">>,
        beamtalk_repl_compiler:format_formatted_diagnostics([<<"Unexpected token">>])
    ).

format_diagnostics_multiple_test() ->
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([<<"Error 1">>, <<"Error 2">>]),
    ?assertEqual(<<"Error 1\n\nError 2">>, Result).

format_diagnostics_three_test() ->
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([<<"A">>, <<"B">>, <<"C">>]),
    ?assert(binary:match(Result, <<"A">>) =/= nomatch),
    ?assert(binary:match(Result, <<"B">>) =/= nomatch),
    ?assert(binary:match(Result, <<"C">>) =/= nomatch).

%%====================================================================
%% is_internal_key/1
%%====================================================================

is_internal_key_double_underscore_test() ->
    ?assert(beamtalk_repl_compiler:is_internal_key('__repl_actor_registry__')).

is_internal_key_double_underscore_prefix_only_test() ->
    ?assert(beamtalk_repl_compiler:is_internal_key('__workspace_user_bindings__')).

is_internal_key_single_underscore_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key('_error')).

is_internal_key_regular_atom_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key(x)).

is_internal_key_empty_atom_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key('')).

is_internal_key_normal_name_test() ->
    ?assertNot(beamtalk_repl_compiler:is_internal_key(myVar)).

%%====================================================================
%% build_class_superclass_index/0
%%====================================================================

build_class_superclass_index_missing_table_test() ->
    %% When the ETS table doesn't exist, returns empty map
    Result = beamtalk_repl_compiler:build_class_superclass_index(),
    ?assert(is_map(Result)).

%%====================================================================
%% format_core_error/1
%%====================================================================

format_core_error_atom_test() ->
    Result = beamtalk_repl_compiler:format_core_error(badarg),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"Core Erlang compile error">>) =/= nomatch).

format_core_error_tuple_test() ->
    Result = beamtalk_repl_compiler:format_core_error({error, some_reason}),
    ?assert(is_binary(Result)).

format_core_error_string_test() ->
    Result = beamtalk_repl_compiler:format_core_error("something went wrong"),
    ?assert(is_binary(Result)).

%%====================================================================
%% apply_module_name_override/2
%%====================================================================

apply_module_name_override_undefined_test() ->
    Opts = #{stdlib_mode => false},
    ?assertEqual(Opts, beamtalk_repl_compiler:apply_module_name_override(Opts, undefined)).

apply_module_name_override_with_name_test() ->
    Opts = #{stdlib_mode => false},
    Result = beamtalk_repl_compiler:apply_module_name_override(Opts, <<"my_module">>),
    ?assertEqual(<<"my_module">>, maps:get(module_name, Result)).

apply_module_name_override_preserves_existing_test() ->
    Opts = #{stdlib_mode => true, other => value},
    Result = beamtalk_repl_compiler:apply_module_name_override(Opts, <<"override">>),
    ?assertEqual(true, maps:get(stdlib_mode, Result)),
    ?assertEqual(value, maps:get(other, Result)),
    ?assertEqual(<<"override">>, maps:get(module_name, Result)).

%%====================================================================
%% apply_source_path/2
%%====================================================================

apply_source_path_undefined_test() ->
    Opts = #{stdlib_mode => false},
    ?assertEqual(Opts, beamtalk_repl_compiler:apply_source_path(Opts, undefined)).

apply_source_path_with_slash_test() ->
    Opts = #{},
    Result = beamtalk_repl_compiler:apply_source_path(Opts, "/home/user/src/Foo.bt"),
    ?assertEqual(<<"/home/user/src/Foo.bt">>, maps:get(source_path, Result)).

apply_source_path_with_bt_extension_test() ->
    Opts = #{},
    Result = beamtalk_repl_compiler:apply_source_path(Opts, "Foo.bt"),
    ?assertEqual(<<"Foo.bt">>, maps:get(source_path, Result)).

apply_source_path_plain_expression_test() ->
    %% "1 + 1" has no path markers, should not add source_path
    Opts = #{},
    Result = beamtalk_repl_compiler:apply_source_path(Opts, "1 + 1"),
    ?assertNot(maps:is_key(source_path, Result)).

apply_source_path_windows_path_test() ->
    Opts = #{},
    Result = beamtalk_repl_compiler:apply_source_path(Opts, "C:\\Users\\src\\Foo.bt"),
    ?assertEqual(<<"C:\\Users\\src\\Foo.bt">>, maps:get(source_path, Result)).

apply_source_path_non_list_ignored_test() ->
    Opts = #{},
    Result = beamtalk_repl_compiler:apply_source_path(Opts, 42),
    ?assertEqual(Opts, Result).

%%====================================================================
%% assemble_class_result/5
%%====================================================================

assemble_class_result_error_trailing_test() ->
    %% When trailing result is an error, returns the error
    Result = beamtalk_repl_compiler:assemble_class_result(
        <<"binary">>, my_module, [], [], {error, <<"trailing failed">>}
    ),
    ?assertEqual({error, <<"trailing failed">>}, Result).

assemble_class_result_no_trailing_test() ->
    %% With trailing=none, produces base info
    Result = beamtalk_repl_compiler:assemble_class_result(
        <<"bin">>, my_mod, [#{name => <<"Foo">>}], [<<"warn">>], none
    ),
    ?assertMatch(
        {ok, class_definition, #{binary := <<"bin">>, module_name := my_mod}, [<<"warn">>]}, Result
    ).

assemble_class_result_with_trailing_test() ->
    %% With trailing={ok, TrailingBin, ModName}, adds trailing keys
    Result = beamtalk_repl_compiler:assemble_class_result(
        <<"bin">>, my_mod, [], [], {ok, <<"trailing">>, trail_mod}
    ),
    ?assertMatch(
        {ok, class_definition,
            #{trailing_binary := <<"trailing">>, trailing_module_name := trail_mod}, []},
        Result
    ).

assemble_class_result_warnings_preserved_test() ->
    Warnings = [<<"w1">>, <<"w2">>],
    {ok, class_definition, _Info, ResultWarnings} = beamtalk_repl_compiler:assemble_class_result(
        <<"bin">>, m, [], Warnings, none
    ),
    ?assertEqual(Warnings, ResultWarnings).

%%====================================================================
%% compile_trailing_expressions/2
%%====================================================================

compile_trailing_expressions_none_test() ->
    %% No trailing_core_erlang key → returns none
    ClassInfo = #{core_erlang => <<"...">>, module_name => <<"m">>, classes => [], warnings => []},
    Result = beamtalk_repl_compiler:compile_trailing_expressions(ClassInfo, my_mod),
    ?assertEqual(none, Result).

%%====================================================================
%% compile_expression_via_port/3 — error paths (no compiler running)
%%====================================================================

compile_expression_via_port_noproc_test() ->
    %% With no compiler server, hits exit:{noproc,_} → {error, _}
    Result = beamtalk_repl_compiler:compile_expression_via_port("1 + 1", test_mod, #{}),
    ?assertMatch({error, _}, Result).

compile_expression_via_port_with_bindings_test() ->
    %% Bindings are filtered; still hits noproc
    Result = beamtalk_repl_compiler:compile_expression_via_port(
        "x + 1", test_mod, #{x => 42, '__internal__' => true}
    ),
    ?assertMatch({error, _}, Result).

compile_expression_via_port_error_message_test() ->
    {error, Msg} = beamtalk_repl_compiler:compile_expression_via_port("1", m, #{}),
    ?assert(is_binary(Msg)).

%%====================================================================
%% compile_file_via_port/4 — error paths (no compiler running)
%%====================================================================

compile_file_via_port_noproc_test() ->
    Result = beamtalk_repl_compiler:compile_file_via_port("x := 1", "/test.bt", false, undefined),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_file_via_port_stdlib_mode_test() ->
    Result = beamtalk_repl_compiler:compile_file_via_port(
        "Object subclass: Foo [\n]\n", "/stdlib/src/Foo.bt", true, undefined
    ),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_file_via_port_with_override_test() ->
    Result = beamtalk_repl_compiler:compile_file_via_port(
        "Object subclass: Bar [\n]\n", "/src/Bar.bt", false, <<"bt@pkg@bar">>
    ),
    ?assertMatch({error, {compile_error, _}}, Result).

%%====================================================================
%% compile_expression/3 (public API — same error path as via_port)
%%====================================================================

compile_expression_noproc_test() ->
    Result = beamtalk_repl_compiler:compile_expression("1 + 1", some_mod, #{}),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% compile_file/4 (public API — same error path as via_port)
%%====================================================================

compile_file_noproc_test() ->
    Result = beamtalk_repl_compiler:compile_file(
        "Object subclass: X [\n]\n", "/x.bt", false, undefined
    ),
    ?assertMatch({error, {compile_error, _}}, Result).

%%====================================================================
%% compile_for_codegen/3 (public API)
%%====================================================================

compile_for_codegen_noproc_test() ->
    Result = beamtalk_repl_compiler:compile_for_codegen(<<"1 + 1">>, <<"test_mod">>, []),
    ?assertMatch({error, {compile_error, _}}, Result).

%%====================================================================
%% compile_for_method_reload/2 (public API)
%%====================================================================

compile_for_method_reload_noproc_test() ->
    Result = beamtalk_repl_compiler:compile_for_method_reload(<<"Object subclass: X [\n]\n">>, #{}),
    ?assertMatch({error, {compile_error, _}}, Result).

%%====================================================================
%% compile_standard_expression/2
%%====================================================================

compile_standard_expression_bad_core_test() ->
    %% Invalid core erlang → compile_core_erlang fails → {error, _}
    Result = beamtalk_repl_compiler:compile_standard_expression(<<"not valid core erlang">>, []),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% compile_file_core/3
%%====================================================================

compile_file_core_bad_core_test() ->
    Result = beamtalk_repl_compiler:compile_file_core(<<"not core erlang">>, my_mod, []),
    ?assertMatch({error, {core_compile_error, _}}, Result).

compile_file_core_class_extraction_test() ->
    %% Even if compile fails, the class list transform logic is pure — test error path
    Result = beamtalk_repl_compiler:compile_file_core(<<"garbage">>, my_mod, [
        #{name => <<"Foo">>, superclass => <<"Object">>}
    ]),
    %% Core erlang compile will fail, but we're just verifying it handles the error
    ?assertMatch({error, _}, Result).
