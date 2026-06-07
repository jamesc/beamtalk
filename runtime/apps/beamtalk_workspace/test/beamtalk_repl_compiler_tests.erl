%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_compiler_tests).

-moduledoc "Unit tests for beamtalk_repl_compiler module".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% format_formatted_diagnostics/1
%%====================================================================

format_diagnostics_empty_test() ->
    ?assertEqual(<<"Compilation failed">>, beamtalk_repl_compiler:format_formatted_diagnostics([])).

format_diagnostics_single_binary_test() ->
    ?assertEqual(
        <<"Unexpected token">>,
        beamtalk_repl_compiler:format_formatted_diagnostics([<<"Unexpected token">>])
    ).

format_diagnostics_multiple_binary_test() ->
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([<<"Error 1">>, <<"Error 2">>]),
    ?assertEqual(<<"Error 1\n\nError 2">>, Result).

format_diagnostics_three_binary_test() ->
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([<<"A">>, <<"B">>, <<"C">>]),
    ?assert(binary:match(Result, <<"A">>) =/= nomatch),
    ?assert(binary:match(Result, <<"B">>) =/= nomatch),
    ?assert(binary:match(Result, <<"C">>) =/= nomatch).

format_diagnostics_map_message_only_test() ->
    %% BT-1235: structured diagnostic map with line but no hint
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([
        #{message => <<"Unused variable `x`">>, line => 3}
    ]),
    ?assertEqual(<<"Line 3: Unused variable `x`">>, Result).

format_diagnostics_map_no_line_test() ->
    %% BT-1235: structured diagnostic map without line or hint
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([
        #{message => <<"Unused variable `x`">>}
    ]),
    ?assertEqual(<<"Unused variable `x`">>, Result).

format_diagnostics_map_with_hint_test() ->
    %% BT-1235: structured diagnostic map with line and hint
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([
        #{message => <<"Unused variable `x`">>, line => 3, hint => <<"prefix with _x">>}
    ]),
    ?assertEqual(<<"Line 3: Unused variable `x`\nHint: prefix with _x">>, Result).

format_diagnostics_map_multiple_test() ->
    %% BT-1235: multiple structured diagnostics joined with double newline
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([
        #{message => <<"Error 1">>, line => 1},
        #{message => <<"Error 2">>, line => 2}
    ]),
    ?assertEqual(<<"Line 1: Error 1\n\nLine 2: Error 2">>, Result).

format_diagnostics_unexpected_type_test() ->
    %% BT-1235: unexpected diagnostic types are formatted via ~p fallback
    Result = beamtalk_repl_compiler:format_formatted_diagnostics([some_atom]),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

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
%% known_vars/1
%%====================================================================
%%
%% Regression guard for the recurring "Unresolved class `Workspace`" bug:
%% after ADR 0081 Phase 1 the workspace singletons (Transcript/Beamtalk/
%% Workspace) are resolved lazily and are no longer in the eval bindings map,
%% so known_vars/1 must add them back from beamtalk_workspace_config — otherwise
%% the structural validator flags `Workspace classes` as an unresolved class.

%% The three workspace singletons are always known vars, even with no bindings.
known_vars_includes_singletons_test() ->
    KnownVars = beamtalk_repl_compiler:known_vars(#{}),
    ?assert(lists:member(<<"Workspace">>, KnownVars)),
    ?assert(lists:member(<<"Transcript">>, KnownVars)),
    ?assert(lists:member(<<"Beamtalk">>, KnownVars)).

%% The singleton list is derived from the workspace config single source of
%% truth, not hardcoded in the compiler — assert parity so the two can't drift.
known_vars_singletons_match_config_test() ->
    KnownVars = beamtalk_repl_compiler:known_vars(#{}),
    ConfigNames = [atom_to_binary(N, utf8) || N <- beamtalk_workspace_config:binding_names()],
    %% Bidirectional parity: with no user bindings, known_vars/1 returns exactly
    %% the (sorted, deduped) config singletons — extra names would also fail.
    ?assertEqual(lists:usort(ConfigNames), KnownVars).

%% User variable bindings (session locals + bind:as: globals) are included.
known_vars_includes_user_bindings_test() ->
    KnownVars = beamtalk_repl_compiler:known_vars(#{x => 1, myVar => 2}),
    ?assert(lists:member(<<"x">>, KnownVars)),
    ?assert(lists:member(<<"myVar">>, KnownVars)).

%% Internal `__`-prefixed keys are excluded (they are not source identifiers).
known_vars_excludes_internal_keys_test() ->
    KnownVars = beamtalk_repl_compiler:known_vars(#{
        '__workspace_user_bindings__' => #{}, '__repl_actor_registry__' => self()
    }),
    ?assertNot(lists:member(<<"__workspace_user_bindings__">>, KnownVars)),
    ?assertNot(lists:member(<<"__repl_actor_registry__">>, KnownVars)).

%% The result is sorted and de-duplicated (a binding shadowing a singleton name
%% must not appear twice).
known_vars_dedups_and_sorts_test() ->
    KnownVars = beamtalk_repl_compiler:known_vars(#{'Workspace' => some_value}),
    WorkspaceCount = length([N || N <- KnownVars, N =:= <<"Workspace">>]),
    ?assertEqual(1, WorkspaceCount),
    ?assertEqual(lists:usort(KnownVars), KnownVars).

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

%%====================================================================
%% Success-path tests (require the beamtalk_compiler OTP app + port)
%%
%% These exercise the compile pipeline end-to-end: Beamtalk source ->
%% Core Erlang (Rust port) -> BEAM bytecode. They cover the cold success
%% branches of compile_expression/compile_file/compile_for_codegen/etc.
%% that the noproc error-path tests above cannot reach.
%%====================================================================

setup_compiler() ->
    application:ensure_all_started(compiler),
    %% Start the runtime so the class-hierarchy ETS table is populated; this
    %% lets build_class_superclass_index/0 and build_class_module_index/0
    %% return non-empty maps and add_class_indexes/1 take its populated branches.
    application:ensure_all_started(beamtalk_runtime),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Give the runtime a moment to register its bootstrap classes.
    timer:sleep(300),
    ok.

teardown_compiler(_) ->
    %% Stop the compiler app so later test modules in the shared EUnit node see
    %% the baseline "compiler not running" state (noproc error-path tests in
    %% beamtalk_repl_eval_tests depend on it).
    _ = application:stop(beamtalk_compiler),
    ok.

compiler_success_test_() ->
    {setup, fun setup_compiler/0, fun teardown_compiler/1, [
        {"compile_expression standard expr succeeds", fun compile_expression_standard_ok/0},
        {"compile_expression with known vars succeeds", fun compile_expression_known_vars_ok/0},
        {"compile_expression class definition succeeds", fun compile_expression_class_def_ok/0},
        {"compile_expression method definition succeeds", fun compile_expression_method_def_ok/0},
        {"compile_expression invalid returns diagnostics", fun compile_expression_invalid/0},
        {"compile_expression_via_port standard succeeds", fun compile_via_port_standard_ok/0},
        {"compile_expression_trace standard succeeds", fun compile_trace_standard_ok/0},
        {"compile_expression_trace invalid returns error", fun compile_trace_invalid/0},
        {"compile_file class succeeds", fun compile_file_ok/0},
        {"compile_file invalid returns compile_error", fun compile_file_invalid/0},
        {"compile_file/5 with prebuilt indexes succeeds", fun compile_file_prebuilt_ok/0},
        {"compile_for_codegen standard succeeds", fun compile_for_codegen_ok/0},
        {"compile_for_codegen class succeeds", fun compile_for_codegen_class_ok/0},
        {"compile_for_codegen method def rejected", fun compile_for_codegen_method_rejected/0},
        {"compile_for_codegen invalid returns error", fun compile_for_codegen_invalid/0},
        {"compile_file_for_codegen succeeds", fun compile_file_for_codegen_ok/0},
        {"compile_file_for_codegen invalid returns error", fun compile_file_for_codegen_invalid/0},
        {"compile_for_method_reload succeeds", fun compile_for_method_reload_ok/0},
        {"compile_for_method_reload invalid returns error",
            fun compile_for_method_reload_invalid/0},
        {"compile_standard_expression valid core succeeds", fun compile_standard_expression_ok/0},
        {"compile_file_core valid core succeeds", fun compile_file_core_ok/0},
        {"build_class_indexes returns map", fun build_class_indexes_ok/0},
        {"build_class_module_index returns map", fun build_class_module_index_ok/0},
        {"compile_expression protocol definition succeeds", fun compile_expression_protocol_ok/0},
        {"compile_for_codegen protocol rejected", fun compile_for_codegen_protocol_rejected/0},
        {"compile_expression_trace with known vars succeeds", fun compile_trace_known_vars_ok/0}
    ]}.

compile_expression_standard_ok() ->
    Result = beamtalk_repl_compiler:compile_expression("1 + 2", expr_std_mod, #{}),
    {ok, Binary, _ResultExpr, Warnings} = Result,
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0),
    ?assert(is_list(Warnings)).

compile_expression_known_vars_ok() ->
    %% Bindings keys are forwarded as known vars (internal keys filtered out).
    Result = beamtalk_repl_compiler:compile_expression(
        "x + 1", expr_kv_mod, #{x => 41, '__internal__' => skip}
    ),
    ?assertMatch({ok, _Binary, _ResultExpr, _Warnings}, Result).

compile_expression_class_def_ok() ->
    Source = "Actor subclass: ReplCompClassDef\n  value => 42",
    Result = beamtalk_repl_compiler:compile_expression(Source, expr_classdef_mod, #{}),
    {ok, class_definition, ClassInfo, _Warnings} = Result,
    ?assert(is_binary(maps:get(binary, ClassInfo))),
    ?assert(is_atom(maps:get(module_name, ClassInfo))),
    ?assert(is_list(maps:get(classes, ClassInfo))).

compile_expression_method_def_ok() ->
    %% `ClassName >> selector => body` is a standalone method definition.
    Source = "Object >> doubled => self * 2",
    Result = beamtalk_repl_compiler:compile_expression(Source, expr_methoddef_mod, #{}),
    {ok, method_definition, MethodInfo, _Warnings} = Result,
    ?assert(is_map(MethodInfo)),
    ?assert(maps:is_key(selector, MethodInfo)).

compile_expression_invalid() ->
    Result = beamtalk_repl_compiler:compile_expression("+++", expr_bad_mod, #{}),
    %% Invalid expression returns structured diagnostics (a list of maps/binaries).
    ?assertMatch({error, _}, Result),
    {error, Diagnostics} = Result,
    ?assert(is_list(Diagnostics)).

compile_via_port_standard_ok() ->
    Result = beamtalk_repl_compiler:compile_expression_via_port("3 * 4", via_port_ok_mod, #{}),
    ?assertMatch({ok, _Binary, _ResultExpr, _Warnings}, Result).

compile_trace_standard_ok() ->
    Result = beamtalk_repl_compiler:compile_expression_trace("1. 2. 3", trace_ok_mod, #{}),
    {ok, Binary, _ResultExpr, Warnings} = Result,
    ?assert(is_binary(Binary)),
    ?assert(is_list(Warnings)).

compile_trace_invalid() ->
    Result = beamtalk_repl_compiler:compile_expression_trace("+++", trace_bad_mod, #{}),
    ?assertMatch({error, _}, Result).

compile_file_ok() ->
    Source = "Actor subclass: ReplCompFile\n  value => 7",
    Result = beamtalk_repl_compiler:compile_file(Source, "/src/ReplCompFile.bt", false, undefined),
    {ok, Binary, Classes, ModuleName} = Result,
    ?assert(is_binary(Binary)),
    ?assert(is_list(Classes)),
    ?assert(is_atom(ModuleName)),
    %% Class metadata is transformed into #{name, superclass} string maps.
    [#{name := Name, superclass := Super} | _] = Classes,
    ?assert(is_list(Name)),
    ?assert(is_list(Super)).

compile_file_invalid() ->
    Result = beamtalk_repl_compiler:compile_file("+++ not valid", "/src/Bad.bt", false, undefined),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_file_prebuilt_ok() ->
    Source = "Actor subclass: ReplCompPrebuilt\n  value => 9",
    %% Empty prebuilt index map (not use_runtime_indexes) exercises the merge branch.
    Result = beamtalk_repl_compiler:compile_file(
        Source, "/src/ReplCompPrebuilt.bt", false, undefined, #{}
    ),
    ?assertMatch({ok, _Binary, _Classes, _ModuleName}, Result).

compile_for_codegen_ok() ->
    Result = beamtalk_repl_compiler:compile_for_codegen(<<"1 + 2">>, <<"codegen_ok_mod">>, []),
    {ok, CoreErlang, Warnings} = Result,
    ?assert(is_binary(CoreErlang)),
    ?assert(byte_size(CoreErlang) > 0),
    ?assert(is_list(Warnings)).

compile_for_codegen_class_ok() ->
    Source = <<"Actor subclass: ReplCodegenClass\n  value => 1">>,
    Result = beamtalk_repl_compiler:compile_for_codegen(Source, <<"codegen_class_mod">>, []),
    {ok, CoreErlang, _Warnings} = Result,
    ?assert(is_binary(CoreErlang)).

compile_for_codegen_method_rejected() ->
    %% Standalone method definitions are not supported by show-codegen.
    Source = <<"Object >> tripled => self * 3">>,
    Result = beamtalk_repl_compiler:compile_for_codegen(Source, <<"codegen_method_mod">>, []),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_for_codegen_invalid() ->
    Result = beamtalk_repl_compiler:compile_for_codegen(<<"+++">>, <<"codegen_bad_mod">>, []),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_file_for_codegen_ok() ->
    Source = <<"Actor subclass: ReplFileCodegen\n  value => 2">>,
    Result = beamtalk_repl_compiler:compile_file_for_codegen(Source, "/src/ReplFileCodegen.bt"),
    {ok, CoreErlang, Warnings} = Result,
    ?assert(is_binary(CoreErlang)),
    ?assert(is_list(Warnings)).

compile_file_for_codegen_invalid() ->
    Result = beamtalk_repl_compiler:compile_file_for_codegen(<<"+++ bad">>, "/src/Bad.bt"),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_for_method_reload_ok() ->
    Source = <<"Actor subclass: ReplMethodReload\n  value => 5">>,
    Result = beamtalk_repl_compiler:compile_for_method_reload(Source, #{}),
    {ok, Binary, ModName, Classes, Warnings} = Result,
    ?assert(is_binary(Binary)),
    ?assert(is_atom(ModName)),
    ?assert(is_list(Classes)),
    ?assert(is_list(Warnings)).

compile_for_method_reload_invalid() ->
    Result = beamtalk_repl_compiler:compile_for_method_reload(<<"+++ bad">>, #{}),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_standard_expression_ok() ->
    %% Generate real Core Erlang from the compiler, then feed it back to
    %% compile_standard_expression/2 to exercise the success branch.
    {ok, CoreErlang, _} =
        beamtalk_compiler:compile_expression(<<"40 + 2">>, <<"std_expr_core">>, []),
    Result = beamtalk_repl_compiler:compile_standard_expression(CoreErlang, [<<"w">>]),
    {ok, Binary, {port_compiled}, Warnings} = Result,
    ?assert(is_binary(Binary)),
    ?assertEqual([<<"w">>], Warnings).

compile_file_core_ok() ->
    {ok, CoreErlang, _} =
        beamtalk_compiler:compile_expression(<<"1 + 1">>, <<"file_core_ok">>, []),
    %% binary_to_atom of the module name in the Core Erlang would mismatch, but
    %% compile_file_core only needs a valid atom + the class metadata transform.
    Result = beamtalk_repl_compiler:compile_file_core(CoreErlang, file_core_ok_mod, [
        #{name => <<"Foo">>, superclass => <<"Object">>}
    ]),
    {ok, Binary, ClassNames, ModuleName} = Result,
    ?assert(is_binary(Binary)),
    ?assertEqual(file_core_ok_mod, ModuleName),
    ?assertEqual([#{name => "Foo", superclass => "Object"}], ClassNames).

build_class_indexes_ok() ->
    Result = beamtalk_repl_compiler:build_class_indexes(),
    ?assert(is_map(Result)).

build_class_module_index_ok() ->
    Result = beamtalk_repl_compiler:build_class_module_index(),
    ?assert(is_map(Result)).

compile_expression_protocol_ok() ->
    %% `Protocol define: Name` compiles to a protocol_definition result, exercising
    %% compile_protocol_definition_result/1 (Core Erlang -> BEAM for the protocol module).
    Result = beamtalk_repl_compiler:compile_expression(
        "Protocol define: ReplCompProto", proto_ok_mod, #{}
    ),
    {ok, protocol_definition, ProtocolInfo, _Warnings} = Result,
    ?assert(is_binary(maps:get(binary, ProtocolInfo))),
    ?assert(is_atom(maps:get(module_name, ProtocolInfo))),
    ?assert(is_list(maps:get(protocols, ProtocolInfo))).

compile_for_codegen_protocol_rejected() ->
    %% show-codegen does not support protocol definitions.
    Result = beamtalk_repl_compiler:compile_for_codegen(
        <<"Protocol define: ReplCodegenProto">>, <<"codegen_proto_mod">>, []
    ),
    ?assertMatch({error, {compile_error, _}}, Result).

compile_trace_known_vars_ok() ->
    %% Non-internal atom binding keys are forwarded as known vars to the trace
    %% compiler; internal `__`-prefixed keys are filtered out.
    Result = beamtalk_repl_compiler:compile_expression_trace(
        "x + 1", trace_kv_mod, #{x => 10, '__skip__' => true}
    ),
    ?assertMatch({ok, _Binary, _ResultExpr, _Warnings}, Result).
