%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_tests).

-moduledoc "Unit tests for beamtalk_repl_loader module".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

temp_dir() ->
    case os:getenv("TMPDIR") of
        false ->
            case os:getenv("TEMP") of
                false -> "/tmp";
                Dir -> Dir
            end;
        Dir ->
            Dir
    end.

%%====================================================================
%% is_stdlib_path/1
%%====================================================================

is_stdlib_path_relative_test() ->
    ?assert(beamtalk_repl_loader:is_stdlib_path("stdlib/src/Integer.bt")).

is_stdlib_path_relative_nested_test() ->
    ?assert(beamtalk_repl_loader:is_stdlib_path("stdlib/src/collections/Array.bt")).

is_stdlib_path_absolute_test() ->
    ?assert(beamtalk_repl_loader:is_stdlib_path("/workspace/project/stdlib/src/Integer.bt")).

is_stdlib_path_not_stdlib_src_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("src/MyClass.bt")).

is_stdlib_path_absolute_not_stdlib_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("/workspace/project/src/MyClass.bt")).

is_stdlib_path_no_trailing_slash_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("stdlib/src")).

is_stdlib_path_wrong_subdir_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("stdlib/srcs/Integer.bt")).

is_stdlib_path_empty_test() ->
    ?assertNot(beamtalk_repl_loader:is_stdlib_path("")).

is_stdlib_path_deep_absolute_test() ->
    ?assert(beamtalk_repl_loader:is_stdlib_path("/home/user/beamtalk/stdlib/src/String.bt")).

%%====================================================================
%% to_snake_case/1
%%====================================================================

to_snake_case_empty_test() ->
    ?assertEqual([], beamtalk_repl_loader:to_snake_case([])).

to_snake_case_lowercase_test() ->
    ?assertEqual("counter", beamtalk_repl_loader:to_snake_case("counter")).

to_snake_case_uppercase_first_test() ->
    ?assertEqual("counter", beamtalk_repl_loader:to_snake_case("Counter")).

to_snake_case_two_words_test() ->
    ?assertEqual("scheme_symbol", beamtalk_repl_loader:to_snake_case("SchemeSymbol")).

to_snake_case_three_words_test() ->
    ?assertEqual("my_counter_actor", beamtalk_repl_loader:to_snake_case("MyCounterActor")).

to_snake_case_acronym_no_underscore_test() ->
    %% Consecutive uppercase → no underscore inserted within them
    ?assertEqual("httprouter", beamtalk_repl_loader:to_snake_case("HTTPRouter")).

to_snake_case_already_snake_test() ->
    ?assertEqual("already_snake", beamtalk_repl_loader:to_snake_case("already_snake")).

to_snake_case_with_digits_test() ->
    ?assertEqual("app2", beamtalk_repl_loader:to_snake_case("App2")).

to_snake_case_all_uppercase_test() ->
    ?assertEqual("abc", beamtalk_repl_loader:to_snake_case("ABC")).

%%====================================================================
%% verify_class_present/3
%%====================================================================

verify_class_present_undefined_test() ->
    ?assertEqual(
        ok, beamtalk_repl_loader:verify_class_present(undefined, [#{name => "Foo"}], "/path.bt")
    ).

verify_class_present_found_test() ->
    Classes = [#{name => "Counter"}, #{name => "Timer"}],
    ?assertEqual(ok, beamtalk_repl_loader:verify_class_present('Counter', Classes, "/path.bt")).

verify_class_present_not_found_test() ->
    Classes = [#{name => "OtherClass"}],
    Result = beamtalk_repl_loader:verify_class_present('Counter', Classes, "/path.bt"),
    ?assertEqual({error, {class_not_found, 'Counter', "/path.bt", ["OtherClass"]}}, Result).

verify_class_present_empty_list_test() ->
    Result = beamtalk_repl_loader:verify_class_present('Foo', [], "/path.bt"),
    ?assertEqual({error, {class_not_found, 'Foo', "/path.bt", []}}, Result).

verify_class_present_multiple_classes_test() ->
    Classes = [#{name => "A"}, #{name => "B"}, #{name => "C"}],
    ?assertEqual(ok, beamtalk_repl_loader:verify_class_present('B', Classes, "/path.bt")).

%%====================================================================
%% normalize_class_source_key/1
%%====================================================================

normalize_class_source_key_binary_test() ->
    ?assertEqual(<<"Foo">>, beamtalk_repl_loader:normalize_class_source_key(<<"Foo">>)).

normalize_class_source_key_atom_test() ->
    ?assertEqual(<<"counter">>, beamtalk_repl_loader:normalize_class_source_key(counter)).

normalize_class_source_key_list_test() ->
    ?assertEqual(<<"Counter">>, beamtalk_repl_loader:normalize_class_source_key("Counter")).

%%====================================================================
%% extract_trailing_info/1
%%====================================================================

extract_trailing_info_no_trailing_test() ->
    ClassInfo = #{binary => <<"bin">>, module_name => my_mod, classes => []},
    ?assertEqual(no_trailing, beamtalk_repl_loader:extract_trailing_info(ClassInfo)).

extract_trailing_info_with_trailing_test() ->
    ClassInfo = #{
        binary => <<"bin">>,
        module_name => my_mod,
        trailing_binary => <<"trail_bin">>,
        trailing_module_name => trail_mod
    },
    Result = beamtalk_repl_loader:extract_trailing_info(ClassInfo),
    ?assertEqual({trailing, trail_mod, <<"trail_bin">>}, Result).

%%====================================================================
%% resolve_class_name/1
%%====================================================================

resolve_class_name_binary_existing_test() ->
    %% 'lists' is a known atom
    Result = beamtalk_repl_loader:resolve_class_name(#{name => <<"lists">>}),
    ?assertEqual(lists, Result).

resolve_class_name_binary_unknown_test() ->
    Result = beamtalk_repl_loader:resolve_class_name(#{name => <<"xyzzy_nonexistent_class_99991">>}),
    ?assertEqual(undefined, Result).

resolve_class_name_atom_test() ->
    Result = beamtalk_repl_loader:resolve_class_name(#{name => lists}),
    ?assertEqual(lists, Result).

resolve_class_name_list_existing_test() ->
    Result = beamtalk_repl_loader:resolve_class_name(#{name => "lists"}),
    ?assertEqual(lists, Result).

resolve_class_name_list_unknown_test() ->
    Result = beamtalk_repl_loader:resolve_class_name(#{name => "xyzzy_nonexistent_9999"}),
    ?assertEqual(undefined, Result).

resolve_class_name_no_name_key_test() ->
    Result = beamtalk_repl_loader:resolve_class_name(#{}),
    ?assertEqual(undefined, Result).

resolve_class_name_undefined_name_test() ->
    Result = beamtalk_repl_loader:resolve_class_name(#{name => undefined}),
    ?assertEqual(undefined, Result).

%%====================================================================
%% safe_binary_to_atom/1
%%====================================================================

safe_binary_to_atom_known_test() ->
    ?assertEqual(lists, beamtalk_repl_loader:safe_binary_to_atom(<<"lists">>)).

safe_binary_to_atom_unknown_test() ->
    ?assertEqual(
        undefined, beamtalk_repl_loader:safe_binary_to_atom(<<"xyzzy_nonexistent_atom_111">>)
    ).

%%====================================================================
%% safe_list_to_atom/1
%%====================================================================

safe_list_to_atom_known_test() ->
    ?assertEqual(lists, beamtalk_repl_loader:safe_list_to_atom("lists")).

safe_list_to_atom_unknown_test() ->
    ?assertEqual(undefined, beamtalk_repl_loader:safe_list_to_atom("xyzzy_nonexistent_atom_222")).

%%====================================================================
%% safe_atom_result/1
%%====================================================================

safe_atom_result_ok_test() ->
    ?assertEqual({true, lists}, beamtalk_repl_loader:safe_atom_result({ok, lists})).

safe_atom_result_error_test() ->
    ?assertEqual(false, beamtalk_repl_loader:safe_atom_result({error, badarg})).

%%====================================================================
%% register_classes/2
%%====================================================================

register_classes_no_function_exported_test() ->
    %% lists module has no register_class/0 → ok
    ?assertEqual(ok, beamtalk_repl_loader:register_classes([], lists)).

register_classes_empty_classes_test() ->
    ?assertEqual(ok, beamtalk_repl_loader:register_classes([], some_nonexistent_module)).

%%====================================================================
%% trigger_hot_reload/2
%%====================================================================

trigger_hot_reload_empty_test() ->
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, [])).

trigger_hot_reload_unknown_class_binary_test() ->
    Classes = [#{name => <<"xyzzy_nonexistent_class_88881">>}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_unknown_class_list_test() ->
    Classes = [#{name => "xyzzy_nonexistent_class_88882"}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_unknown_class_atom_test() ->
    Classes = [#{name => xyzzy_nonexistent_class_88883}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_undefined_name_test() ->
    Classes = [#{name => undefined}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_no_name_key_test() ->
    Classes = [#{}],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

trigger_hot_reload_multiple_classes_test() ->
    Classes = [
        #{name => <<"xyzzy_class_a_1">>},
        #{name => "xyzzy_class_b_1"},
        #{}
    ],
    ?assertEqual(ok, beamtalk_repl_loader:trigger_hot_reload(some_module, Classes)).

%%====================================================================
%% activate_module/2
%%====================================================================

activate_module_existing_module_test() ->
    %% lists has no register_class/0, no hot reload needed — should complete ok
    ?assertEqual(ok, beamtalk_repl_loader:activate_module(lists, [])).

%%====================================================================
%% handle_load/2 — error paths
%%====================================================================

handle_load_not_found_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertMatch(
        {error, {file_not_found, _}, _},
        beamtalk_repl_loader:handle_load("/nonexistent/x.bt", State)
    ).

handle_load_directory_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertMatch({error, {read_error, _}, _}, beamtalk_repl_loader:handle_load(temp_dir(), State)).

handle_load_compile_error_test() ->
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(temp_dir(), io_lib:format("loader_test_~p.bt", [UniqueId])),
    ok = file:write_file(TempFile, <<"invalid syntax @@@">>),
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_loader:handle_load(TempFile, State),
    file:delete(TempFile),
    ?assertMatch({error, {compile_error, _}, _}, Result).

%%====================================================================
%% handle_load/3 — error paths (no compiler/runtime needed)
%%====================================================================

handle_load_3_not_found_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertMatch(
        {error, {file_not_found, _}, _},
        beamtalk_repl_loader:handle_load("/nonexistent/y.bt", State, #{})
    ).

handle_load_3_directory_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertMatch(
        {error, {read_error, _}, _},
        beamtalk_repl_loader:handle_load(temp_dir(), State, #{})
    ).

handle_load_3_compile_error_test() ->
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(temp_dir(), io_lib:format("loader_h3_~p.bt", [UniqueId])),
    ok = file:write_file(TempFile, <<"invalid syntax @@@">>),
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_loader:handle_load(TempFile, State, #{}),
    file:delete(TempFile),
    ?assertMatch({error, _, _}, Result).

%%====================================================================
%% handle_load_source/3 — error path
%%====================================================================

handle_load_source_compile_error_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_loader:handle_load_source(
        <<"invalid @@@ syntax">>, "inline-bad", State
    ),
    ?assertMatch({error, _, _}, Result).

%%====================================================================
%% reload_class_file/1,2 — error paths
%%====================================================================

reload_class_file_not_found_test() ->
    ?assertEqual(
        {error, {file_not_found, "/nonexistent/x.bt"}},
        beamtalk_repl_loader:reload_class_file("/nonexistent/x.bt")
    ).

reload_class_file_2_not_found_test() ->
    ?assertEqual(
        {error, {file_not_found, "/nonexistent/x.bt"}},
        beamtalk_repl_loader:reload_class_file("/nonexistent/x.bt", 'MyClass')
    ).

reload_class_file_directory_test() ->
    ?assertMatch({error, {read_error, _}}, beamtalk_repl_loader:reload_class_file(temp_dir())).

reload_class_file_compile_error_test() ->
    UniqueId = erlang:unique_integer([positive]),
    TempFile = filename:join(temp_dir(), io_lib:format("loader_reload_~p.bt", [UniqueId])),
    ok = file:write_file(TempFile, <<"invalid @@@ syntax">>),
    Result = beamtalk_repl_loader:reload_class_file(TempFile),
    file:delete(TempFile),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% compute_package_module_name/1
%%====================================================================

compute_package_module_name_no_metadata_test() ->
    %% Without workspace_meta, returns undefined so the compiler port
    %% derives the module name from the class name.  This preserves hot
    %% reload semantics: hot_counter.bt and hot_counter_v2.bt both define
    %% HotCounter → same module bt@hot_counter.
    Result = beamtalk_repl_loader:compute_package_module_name("/some/path/src/Foo.bt"),
    ?assertEqual(undefined, Result).

compute_package_module_name_no_metadata_camel_case_test() ->
    %% Same: no metadata → undefined (class-name-based naming).
    Result = beamtalk_repl_loader:compute_package_module_name("/some/path/MyCounter.bt"),
    ?assertEqual(undefined, Result).

%%====================================================================
%% resolve_package_module/4
%%====================================================================

resolve_package_module_src_match_test() ->
    %% File under ProjectRoot/src → bt@pkg@module
    %% Use file:get_cwd() as the root so paths are drive-letter-consistent on Windows.
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "myproject"),
    AbsPath = filename:join([ProjectRoot, "src", "Counter.bt"]),
    OrigPath = filename:join([ProjectRoot, "src", "Counter.bt"]),
    Result = beamtalk_repl_loader:resolve_package_module(
        AbsPath, ProjectRoot, <<"mypkg">>, OrigPath
    ),
    ?assertEqual(<<"bt@mypkg@counter">>, Result).

resolve_package_module_test_match_test() ->
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "myproject"),
    AbsPath = filename:join([ProjectRoot, "test", "CounterTest.bt"]),
    OrigPath = AbsPath,
    Result = beamtalk_repl_loader:resolve_package_module(
        AbsPath, ProjectRoot, <<"mypkg">>, OrigPath
    ),
    ?assertEqual(<<"bt@mypkg@test@counter_test">>, Result).

resolve_package_module_no_match_test() ->
    %% File outside src/ and test/ falls back to bt@{stem_snake_case}
    %% Use different basenames for AbsPath and OrigPath to prove the
    %% fallback derives from OrigPath, not AbsPath.
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "myproject"),
    AbsPath = filename:join([Cwd, "other", "project", "Foo.bt"]),
    OrigPath = filename:join([Cwd, "examples", "MyCounter.bt"]),
    Result = beamtalk_repl_loader:resolve_package_module(
        AbsPath, ProjectRoot, <<"pkg">>, OrigPath
    ),
    ?assertEqual(<<"bt@my_counter">>, Result).

%%====================================================================
%% try_package_relative/3
%%====================================================================

try_package_relative_match_test() ->
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "project"),
    AbsPath = filename:join([ProjectRoot, "src", "collections", "Array.bt"]),
    Result = beamtalk_repl_loader:try_package_relative(AbsPath, ProjectRoot, "src"),
    ?assertMatch({ok, _}, Result),
    {ok, Parts} = Result,
    ?assertEqual(iolist_to_binary(Parts), <<"collections@array">>).

try_package_relative_no_match_test() ->
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "project"),
    AbsPath = filename:join([Cwd, "other", "src", "Foo.bt"]),
    Result = beamtalk_repl_loader:try_package_relative(AbsPath, ProjectRoot, "src"),
    ?assertEqual(undefined, Result).

try_package_relative_simple_file_test() ->
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "project"),
    AbsPath = filename:join([ProjectRoot, "src", "MyClass.bt"]),
    Result = beamtalk_repl_loader:try_package_relative(AbsPath, ProjectRoot, "src"),
    ?assertMatch({ok, _}, Result),
    {ok, Parts} = Result,
    ?assertEqual(iolist_to_binary(Parts), <<"my_class">>).

%%====================================================================
%% maybe_add_loaded_module/2
%%====================================================================

maybe_add_loaded_module_not_present_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    NewState = beamtalk_repl_loader:maybe_add_loaded_module(lists, State),
    ?assert(lists:member(lists, beamtalk_repl_state:get_loaded_modules(NewState))).

maybe_add_loaded_module_already_present_test() ->
    State0 = beamtalk_repl_state:new(undefined, 0),
    State1 = beamtalk_repl_loader:maybe_add_loaded_module(lists, State0),
    State2 = beamtalk_repl_loader:maybe_add_loaded_module(lists, State1),
    %% Should appear exactly once
    Modules = beamtalk_repl_state:get_loaded_modules(State2),
    ?assertEqual(1, length([M || M <- Modules, M =:= lists])).

%%====================================================================
%% store_file_class_sources/3
%%====================================================================

store_file_class_sources_single_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => "MyClass", superclass => "Object"}],
    %% Source is written to workspace_meta (not started, so no-op); State returned unchanged.
    NewState = beamtalk_repl_loader:store_file_class_sources(
        Classes, "Object subclass: MyClass [\n]\n", State
    ),
    ?assertEqual(State, NewState).

store_file_class_sources_multiple_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => "A", superclass => "Object"}, #{name => "B", superclass => "Object"}],
    NewState = beamtalk_repl_loader:store_file_class_sources(Classes, "source", State),
    ?assertEqual(State, NewState).

store_file_class_sources_empty_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    NewState = beamtalk_repl_loader:store_file_class_sources([], "source", State),
    ?assertEqual(State, NewState).

%%====================================================================
%% store_class_sources/4
%%====================================================================

store_class_sources_empty_classes_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    {Name, NewState} = beamtalk_repl_loader:store_class_sources([], my_fallback_mod, "expr", State),
    %% Falls back to module name as class name; State is unchanged (source goes to workspace_meta).
    ?assertEqual(<<"my_fallback_mod">>, Name),
    ?assertEqual(State, NewState).

store_class_sources_with_classes_binary_name_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => <<"Counter">>, superclass => <<"Object">>}],
    {Name, NewState} = beamtalk_repl_loader:store_class_sources(Classes, some_mod, "expr", State),
    ?assertEqual(<<"Counter">>, Name),
    ?assertEqual(State, NewState).

store_class_sources_with_classes_list_name_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => "MyActor", superclass => "Actor"}],
    {Name, NewState} = beamtalk_repl_loader:store_class_sources(Classes, some_mod, "expr", State),
    ?assertEqual(<<"MyActor">>, Name),
    ?assertEqual(State, NewState).

%%====================================================================
%% is_path_inside/2 (ADR 0082 Phase 1, BT-2283)
%%====================================================================

is_path_inside_direct_child_test() ->
    ?assert(beamtalk_repl_loader:is_path_inside("/proj", "/proj/src/Counter.bt")).

is_path_inside_root_itself_test() ->
    ?assert(beamtalk_repl_loader:is_path_inside("/proj", "/proj")).

is_path_inside_sibling_prefix_not_inside_test() ->
    %% "/proj-other" must NOT count as inside "/proj" — component-wise prefix,
    %% not a raw string prefix.
    ?assertNot(beamtalk_repl_loader:is_path_inside("/proj", "/proj-other/src/X.bt")).

is_path_inside_outside_tree_test() ->
    ?assertNot(beamtalk_repl_loader:is_path_inside("/proj", "/elsewhere/X.bt")).

%%====================================================================
%% method_source_binary/1 (ADR 0082 Phase 1, BT-2283)
%%====================================================================

method_source_binary_prefers_method_source_test() ->
    Info = #{method_source => <<"increment => 1">>, expression => <<"Counter >> increment => 1">>},
    ?assertEqual(<<"increment => 1">>, beamtalk_repl_loader:method_source_binary(Info)).

method_source_binary_accepts_list_test() ->
    Info = #{method_source => "doubled => 2"},
    ?assertEqual(<<"doubled => 2">>, beamtalk_repl_loader:method_source_binary(Info)).

method_source_binary_falls_back_to_expression_test() ->
    Info = #{expression => <<"Counter >> noop => self">>},
    ?assertEqual(
        <<"Counter >> noop => self">>, beamtalk_repl_loader:method_source_binary(Info)
    ).

method_source_binary_empty_when_absent_test() ->
    ?assertEqual(<<>>, beamtalk_repl_loader:method_source_binary(#{})).

%%====================================================================
%% patch_side/1 (ADR 0082 Phase 1, BT-2283)
%%====================================================================

patch_side_instance_test() ->
    ?assertEqual(instance, beamtalk_repl_loader:patch_side(false)).

patch_side_class_test() ->
    ?assertEqual(class, beamtalk_repl_loader:patch_side(true)).

%%====================================================================
%% span_error_entry/3 (ADR 0082 Phase 1, BT-2283)
%%====================================================================

span_error_entry_other_error_downgrades_test() ->
    %% A genuine resolution failure (ambiguous, port down) downgrades to
    %% memory-only with a reason. The brand-new-method (`selector_not_found')
    %% case no longer routes here — it is handled by new_method_entry/3 (BT-2583).
    Base = #{class => <<"Counter">>},
    Entry = beamtalk_repl_loader:span_error_entry(Base, <<"src/counter.bt">>, ambiguous),
    ?assertEqual(false, maps:get(flushable, Entry)),
    ?assertEqual(<<"span_unresolved:ambiguous">>, maps:get(not_flushable_reason, Entry)).

%%====================================================================
%% sibling_method_indent/1 (BT-2583)
%%
%% Pure base-indent derivation: the leading whitespace of the first indented,
%% non-comment, non-blank line of the class body — the sibling-method step a
%% brand-new method is reshaped to before flush appends it.
%%====================================================================

sibling_method_indent_two_space_member_test() ->
    %% A 2-space-indented method/field is the typical stdlib convention.
    Disk = <<"Object subclass: Counter\n  value => 0\n  step => 1\n">>,
    ?assertEqual(<<"  ">>, beamtalk_repl_loader:sibling_method_indent(Disk)).

sibling_method_indent_skips_leading_comments_test() ->
    %% Doc/line comments above the first member do not set the indentation step;
    %% the first indented *member* line does.
    Disk = <<
        "Object subclass: Counter\n"
        "  /// A counter.\n"
        "  value => 0\n"
    >>,
    %% The comment line is itself indented 2 spaces, so the step is still 2 —
    %% but the helper must reach a member line, not stop on the class header.
    ?assertEqual(<<"  ">>, beamtalk_repl_loader:sibling_method_indent(Disk)).

sibling_method_indent_four_space_member_test() ->
    %% A non-standard 4-space body is honoured (we copy the sibling, not a
    %% hard-coded 2).
    Disk = <<"Object subclass: Counter\n    value => 0\n">>,
    ?assertEqual(<<"    ">>, beamtalk_repl_loader:sibling_method_indent(Disk)).

sibling_method_indent_empty_body_falls_back_test() ->
    %% A class with no indented member yet falls back to the 2-space default.
    Disk = <<"Object subclass: Counter\n">>,
    ?assertEqual(<<"  ">>, beamtalk_repl_loader:sibling_method_indent(Disk)).

sibling_method_indent_skips_unindented_comment_test() ->
    %% A column-0 line comment is not a member; the indented member below it sets
    %% the step.
    Disk = <<"// header\nObject subclass: Counter\n  value => 0\n">>,
    ?assertEqual(<<"  ">>, beamtalk_repl_loader:sibling_method_indent(Disk)).

%%====================================================================
%% declared_class_name/1 (ADR 0082 Phase 1, BT-2285)
%%====================================================================

declared_class_name_single_class_test() ->
    Classes = [#{name => "Greeter", superclass => "Object"}],
    ?assertEqual({ok, <<"Greeter">>}, beamtalk_repl_loader:declared_class_name(Classes)).

declared_class_name_no_class_is_error_test() ->
    {error, Err} = beamtalk_repl_loader:declared_class_name([]),
    ?assertEqual(no_class_declared, Err#beamtalk_error.kind),
    ?assertEqual('newClass:at:', Err#beamtalk_error.selector).

declared_class_name_multiple_classes_is_error_test() ->
    %% One class per file (ADR 0040): a source with two classes is rejected, and
    %% the message names both so the user knows what to split.
    Classes = [
        #{name => "Foo", superclass => "Object"},
        #{name => "Bar", superclass => "Object"}
    ],
    {error, Err} = beamtalk_repl_loader:declared_class_name(Classes),
    ?assertEqual(multiple_classes_declared, Err#beamtalk_error.kind),
    Msg = Err#beamtalk_error.message,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Foo">>)),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Bar">>)).

%%====================================================================
%% validate_new_class/3 (ADR 0082 Phase 1, BT-2285)
%%====================================================================

validate_new_class_matching_name_not_loaded_ok_test() ->
    %% Declared name == basename(path) and not already loaded → ok.
    ?assertEqual(
        ok,
        beamtalk_repl_loader:validate_new_class(<<"Greeter">>, "src/greeter.bt", false)
    ).

validate_new_class_name_mismatch_is_error_test() ->
    %% (c) declared class name must match the path basename.
    {error, Err} = beamtalk_repl_loader:validate_new_class(
        <<"Greeter">>, "src/welcomer.bt", false
    ),
    ?assertEqual(class_name_mismatch, Err#beamtalk_error.kind),
    ?assertEqual('newClass:at:', Err#beamtalk_error.selector),
    Msg = Err#beamtalk_error.message,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Greeter">>)),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"welcomer">>)).

validate_new_class_already_loaded_is_error_test() ->
    %% (d) a class of that name is already loaded → error even when the basename
    %% matches.
    {error, Err} = beamtalk_repl_loader:validate_new_class(
        <<"Greeter">>, "src/greeter.bt", true
    ),
    ?assertEqual(class_already_loaded, Err#beamtalk_error.kind),
    ?assertEqual('newClass:at:', Err#beamtalk_error.selector).

validate_new_class_name_mismatch_takes_precedence_over_loaded_test() ->
    %% Name mismatch is checked before the loaded check, so a mismatched name
    %% surfaces the mismatch error regardless of load state.
    {error, Err} = beamtalk_repl_loader:validate_new_class(
        <<"Greeter">>, "src/other.bt", true
    ),
    ?assertEqual(class_name_mismatch, Err#beamtalk_error.kind).

%% Any existing filesystem entry (regular file *or* directory) at the target
%% path must be reported as target_exists; otherwise newClass:at: would log a
%% durable ChangeEntry that later fails to flush with eisdir.
validate_target_path_existing_directory_is_target_exists_test() ->
    Tmp = unicode:characters_to_list(beamtalk_file:'tempDirectory'()),
    Dir =
        Tmp ++ "/bt_new_class_dir_test_" ++
            integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    try
        {error, Err} = beamtalk_repl_loader:validate_target_path(Dir),
        ?assertEqual(target_exists, Err#beamtalk_error.kind),
        ?assertEqual('newClass:at:', Err#beamtalk_error.selector)
    after
        file:del_dir(Dir)
    end.

%%====================================================================
%% new_class/2 type-error guard (pure — no runtime needed)
%%====================================================================

new_class_non_string_args_type_error_test() ->
    %% Neither argument is a String/list, so the catch-all clause raises a
    %% type_error #beamtalk_error{} without touching the filesystem or compiler.
    {error, Err} = beamtalk_repl_loader:new_class(42, 99),
    ?assertEqual(type_error, Err#beamtalk_error.kind),
    ?assertEqual('newClass:at:', Err#beamtalk_error.selector).

%%====================================================================
%% method_source_binary/1 — list expression fallback (pure)
%%====================================================================

method_source_binary_falls_back_to_list_expression_test() ->
    %% No method_source key; expression is a string (list) — converted to binary.
    Info = #{expression => "Counter >> tick => self"},
    ?assertEqual(<<"Counter >> tick => self">>, beamtalk_repl_loader:method_source_binary(Info)).

%%====================================================================
%% Integration fixture: real compile + load + activate
%%
%% Starts the beamtalk_compiler port backend and the beamtalk_runtime
%% application (class registry) so the full success paths of handle_load,
%% handle_load_source, load_class_module, reload_class_file,
%% reload_method_definition and new_class can be exercised end-to-end.
%%====================================================================

loader_setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    Tmp = unicode:characters_to_list(beamtalk_file:'tempDirectory'()),
    Proj = Tmp ++ "/bt_loader_proj_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Proj),
    %% A beamtalk.toml lets workspace_meta auto-detect a package name, which in
    %% turn exercises compute_package_module_name/1's package-qualified branch.
    ok = file:write_file(
        filename:join(Proj, "beamtalk.toml"),
        <<"[package]\nname = \"loaderpkg\"\n">>
    ),
    %% A workspace_meta with a real project_path makes classify_source_file/1
    %% treat files under Proj as flushable (in-project).
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"loader_test_ws">>,
        project_path => list_to_binary(Proj),
        created_at => erlang:system_time(second)
    }),
    %% A live ChangeLog server lets do_emit_change_entry / emit_new_class_entry
    %% append successfully instead of taking only their best-effort catch path.
    case whereis(beamtalk_workspace_changelog) of
        undefined ->
            {ok, _} = beamtalk_workspace_changelog:start_link(#{
                workspace_id => <<"loader_test_ws">>
            });
        _ ->
            ok
    end,
    Proj.

loader_teardown(Proj) ->
    %% Stop the singleton servers this fixture started so later test modules
    %% (e.g. beamtalk_workspace_changelog_tests, which start_link their own
    %% registered server with a {ok, Pid} match) see a clean slate.
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        ClPid -> gen_server:stop(ClPid)
    end,
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    %% Stop the compiler app so later test modules in the shared EUnit node see
    %% the baseline "compiler not running" state (noproc error-path tests in
    %% beamtalk_repl_eval_tests depend on it).
    _ = application:stop(beamtalk_compiler),
    %% Best-effort recursive cleanup of the temp project directory.
    rm_rf(Proj),
    ok.

rm_rf(Path) ->
    case filelib:is_dir(Path) of
        true ->
            case file:list_dir(Path) of
                {ok, Entries} ->
                    lists:foreach(fun(E) -> rm_rf(filename:join(Path, E)) end, Entries),
                    file:del_dir(Path);
                {error, _} ->
                    ok
            end;
        false ->
            file:delete(Path)
    end.

%% Write a .bt file inside the project src dir and return its absolute path.
write_bt(Proj, Name, Source) ->
    Path = filename:join(Proj, Name),
    ok = file:write_file(Path, Source),
    Path.

%% Write a .bt file under <Proj>/<SubDir>/ so it gets a package-qualified module
%% name (`bt@loaderpkg@<mod>' for src/, `bt@loaderpkg@test@<mod>' for test/).
write_bt_under(Proj, SubDir, Name, Source) ->
    Dir = filename:join(Proj, SubDir),
    ok = filelib:ensure_dir(filename:join(Dir, "anchor")),
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Source),
    Path.

%% The BEAM module a class is currently registered to (what a patch must keep).
current_module_of(ClassName) ->
    Pid = beamtalk_class_registry:whereis_class(ClassName),
    ?assert(is_pid(Pid)),
    beamtalk_object_class:module_name(Pid).

%% The live `__source__' of a method, as the IDE reads it back after a save
%% (same channel `browse_method_source' uses: a `{method, Selector}' class call).
stored_method_source(ClassName, Selector) ->
    Pid = beamtalk_class_registry:whereis_class(ClassName),
    ?assert(is_pid(Pid)),
    #{'__source__' := Src} = gen_server:call(Pid, {method, Selector}, 5000),
    Src.

t_install_method_keeps_package_and_source(_Proj) ->
    Proj = live_project_dir(),
    %% BT-2553 follow-up (bug 2): patching a method must NOT drop the class's
    %% package-qualified module name or its on-disk source attribution — that
    %% degradation is what broke flush/revert (a project class became a
    %% stem-named, source-less `bt@<mod>').
    Path = write_bt_under(
        Proj,
        "src",
        "InstallPkg.bt",
        <<"Actor subclass: InstallPkg\n  state: v = 1\n\n  value -> Integer =>\n    self.v\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    %% Fresh load → package-qualified module WITH a beamtalk_source attribute.
    Mod0 = current_module_of('InstallPkg'),
    ?assertEqual('bt@loaderpkg@install_pkg', Mod0),
    ?assert(is_binary(beamtalk_reflection:source_file_from_module(Mod0))),
    %% Patch a new method via the structured install path.
    Result = beamtalk_repl_loader:install_method(
        <<"InstallPkg">>,
        <<"doubled">>,
        <<"doubled -> Integer =>\n  self.v * 2">>,
        durable,
        <<"test">>,
        human,
        [],
        State1
    ),
    ?assertMatch({ok, _, _, _, _}, Result),
    %% Module name AND source attribution are PRESERVED across the patch.
    Mod1 = current_module_of('InstallPkg'),
    ?assertEqual('bt@loaderpkg@install_pkg', Mod1),
    ?assert(is_binary(beamtalk_reflection:source_file_from_module(Mod1))).

t_install_method_preserves_comments(_Proj) ->
    Proj = live_project_dir(),
    %% Bug 1: a method's leading `// --- … ---' section banner + multi-line `///'
    %% doc block must survive the save AND repeated saves (idempotent — the bug
    %% eroded one comment line per save).
    Path = write_bt_under(
        Proj,
        "src",
        "InstallDoc.bt",
        <<"Actor subclass: InstallDoc\n  state: v = 1\n\n  value -> Integer =>\n    self.v\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    MethodSrc =
        <<
            "// --- Section ---\n\n/// First doc line.\n/// Second doc line.\n"
            "bumped -> Integer =>\n  self.v + 1"
        >>,
    ?assertMatch(
        {ok, _, _, _, _},
        beamtalk_repl_loader:install_method(
            <<"InstallDoc">>, <<"bumped">>, MethodSrc, durable, <<"test">>, human, [], State1
        )
    ),
    Src1 = stored_method_source('InstallDoc', bumped),
    ?assert(binary:match(Src1, <<"// --- Section ---">>) =/= nomatch),
    ?assert(binary:match(Src1, <<"/// First doc line.">>) =/= nomatch),
    ?assert(binary:match(Src1, <<"/// Second doc line.">>) =/= nomatch),
    %% Save again (feeding back the stored source). Source stays byte-stable —
    %% no per-save erosion of the leading comment block.
    ?assertMatch(
        {ok, _, _, _, _},
        beamtalk_repl_loader:install_method(
            <<"InstallDoc">>, <<"bumped">>, Src1, durable, <<"test">>, human, [], State1
        )
    ),
    Src2 = stored_method_source('InstallDoc', bumped),
    ?assertEqual(Src1, Src2).

%% BT-2567: `browse-method-source`'s `disk_differs` re-reads the on-disk class
%% file *live* each browse, so an out-of-band rewrite (an external editor, or
%% another session flushing) is detected even though the image body is
%% unchanged. A pre-BT-2567 diff against the load-time `workspace_meta` snapshot
%% would stay `false` here — the snapshot still matches the image body.
t_disk_differs_reflects_live_disk(_Proj) ->
    Proj = live_project_dir(),
    Path = write_bt_under(
        Proj,
        "src",
        "DiskDiff.bt",
        <<"Actor subclass: DiskDiff\n  state: v = 1\n\n  value -> Integer => self.v\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, _State1} = beamtalk_repl_loader:handle_load(Path, State0),
    %% A fresh file load gives the module a `beamtalk_source` attribute, which is
    %% how `disk_differs` finds the file to re-read.
    ?assert(is_binary(beamtalk_reflection:source_file_from_module(current_module_of('DiskDiff')))),
    %% (1) Image == disk: the live method body appears verbatim on disk → not
    %% diverged. Capture the image body the diff compares against.
    Value0 = browse_method_value('DiskDiff'),
    ImageSource = maps:get(<<"source">>, Value0),
    ?assert(is_binary(ImageSource)),
    ?assertEqual(false, maps:get(<<"disk_differs">>, Value0)),
    {ok, OnDisk0} = file:read_file(Path),
    ?assertNotEqual(nomatch, binary:match(OnDisk0, ImageSource)),
    %% (2) Rewrite the file out-of-band (no reload): the image body is now absent
    %% from disk. A live re-read must report divergence; the load-time snapshot
    %% (unchanged) would not.
    NewFile = <<"Actor subclass: DiskDiff\n  state: v = 1\n\n  value -> Integer => self.w\n">>,
    ?assertEqual(nomatch, binary:match(NewFile, ImageSource)),
    ok = file:write_file(Path, NewFile),
    Value1 = browse_method_value('DiskDiff'),
    ?assertEqual(true, maps:get(<<"disk_differs">>, Value1)).

%% browse-method-source for the instance `value' selector, term form (the same
%% path the System Browser drives). `Msg' is unused by this op.
browse_method_value(Class) ->
    {value, V} = beamtalk_repl_ops_browse:handle_term(
        <<"browse-method-source">>,
        #{
            <<"class">> => atom_to_binary(Class, utf8),
            <<"side">> => <<"instance">>,
            <<"selector">> => <<"value">>
        },
        {protocol_msg, <<"browse">>, <<"t1">>, <<"s1">>, #{}, false},
        self()
    ),
    V.

t_install_method_accumulation_preserves_siblings(_Proj) ->
    Proj = live_project_dir(),
    %% Round-trip fidelity across saves: the stored class source accumulates via
    %% unparse, so patching one method must NOT erode OTHER methods (or their
    %% comments) or the class's state. Save methodOne, then methodTwo, then read
    %% BOTH back from the image.
    Path = write_bt_under(
        Proj,
        "src",
        "InstallAccum.bt",
        <<
            "Actor subclass: InstallAccum\n"
            "  state: v :: Integer = 1\n\n"
            "  /// Doc for one.\n"
            "  methodOne -> Integer =>\n    self.v + 1\n\n"
            "  /// Doc for two.\n"
            "  methodTwo -> Integer =>\n    self.v + 2\n"
        >>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    %% Patch methodOne (with a leading banner), then patch methodTwo.
    ?assertMatch(
        {ok, _, _, _, _},
        beamtalk_repl_loader:install_method(
            <<"InstallAccum">>,
            <<"methodOne">>,
            <<"// --- one ---\n/// Doc for one (edited).\nmethodOne -> Integer =>\n  self.v + 10">>,
            durable,
            <<"test">>,
            human,
            [],
            State1
        )
    ),
    ?assertMatch(
        {ok, _, _, _, _},
        beamtalk_repl_loader:install_method(
            <<"InstallAccum">>,
            <<"methodTwo">>,
            <<"methodTwo -> Integer =>\n  self.v + 20">>,
            durable,
            <<"test">>,
            human,
            [],
            State1
        )
    ),
    %% After patching methodTwo, methodOne (and its edited doc/banner) must still
    %% be intact — accumulation did not drop or erode the sibling method.
    One = stored_method_source('InstallAccum', methodOne),
    ?assert(binary:match(One, <<"// --- one ---">>) =/= nomatch),
    ?assert(binary:match(One, <<"Doc for one (edited).">>) =/= nomatch),
    ?assert(binary:match(One, <<"self.v + 10">>) =/= nomatch),
    Two = stored_method_source('InstallAccum', methodTwo),
    ?assert(binary:match(Two, <<"self.v + 20">>) =/= nomatch),
    %% The class state survived the accumulation (still has the `v` ivar).
    Pid = beamtalk_class_registry:whereis_class('InstallAccum'),
    ?assert(lists:member(v, beamtalk_runtime_api:instance_variables(Pid))).

t_revert_recovers_prev_from_disk(_Proj) ->
    Proj = live_project_dir(),
    %% A ChangeEntry with a known on-disk source_file but NO recorded prev_source
    %% (e.g. recorded before source attribution was preserved). find_revert_target
    %% must reconstruct the pre-patch body from disk rather than failing.
    Path = write_bt_under(
        Proj,
        "src",
        "RevertDisk.bt",
        <<"Actor subclass: RevertDisk\n  state: v = 1\n\n  bump -> Integer =>\n    self.v + 1\n">>
    ),
    {ok, _} = beamtalk_workspace_changelog:append(#{
        class => <<"RevertDisk">>,
        selector => <<"bump">>,
        kind => instance,
        source => <<"bump -> Integer => self.v + 99">>,
        intent => durable,
        flushable => true,
        author => <<"test">>,
        author_kind => human,
        source_file => list_to_binary(Path),
        span => #{start => 0, 'end' => 1}
    }),
    Result = beamtalk_workspace_changelog:find_revert_target(<<"RevertDisk">>, bump),
    ?assertMatch({ok, _Body, _Entry}, Result),
    {ok, Body, _} = Result,
    %% The recovered body is the CURRENT on-disk method (the pre-patch state),
    %% not the patched `self.v + 99'.
    ?assert(binary:match(Body, <<"self.v + 1">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Body, <<"99">>)).

t_install_method_test_dir_keeps_package(_Proj) ->
    Proj = live_project_dir(),
    %% A class under test/ gets `bt@loaderpkg@test@<mod>' — a patch must keep it.
    Path = write_bt_under(
        Proj,
        "test",
        "InstallSpec.bt",
        <<"Actor subclass: InstallSpec\n  state: v = 1\n\n  value -> Integer =>\n    self.v\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    ?assertEqual('bt@loaderpkg@test@install_spec', current_module_of('InstallSpec')),
    ?assertMatch(
        {ok, _, _, _, _},
        beamtalk_repl_loader:install_method(
            <<"InstallSpec">>,
            <<"doubled">>,
            <<"doubled -> Integer =>\n  self.v * 2">>,
            durable,
            <<"test">>,
            human,
            [],
            State1
        )
    ),
    ?assertEqual('bt@loaderpkg@test@install_spec', current_module_of('InstallSpec')).

loader_integration_test_() ->
    {setup, fun loader_setup/0, fun loader_teardown/1, fun(Proj) ->
        [
            {"handle_load/2 success", fun() -> t_handle_load_success(Proj) end},
            {"handle_load/3 prebuilt indexes success", fun() ->
                t_handle_load_prebuilt_success(Proj)
            end},
            {"handle_load_source/3 success", fun() -> t_handle_load_source_success() end},
            {"load_class_module/3 success", fun() -> t_load_class_module_success(Proj) end},
            {"reload_class_file/1 success", fun() -> t_reload_class_file_success(Proj) end},
            {"reload_class_file/2 matching class", fun() ->
                t_reload_class_file_2_match(Proj)
            end},
            {"reload_class_file/2 class mismatch", fun() ->
                t_reload_class_file_2_mismatch(Proj)
            end},
            {"reload_method_definition success", fun() ->
                t_reload_method_definition_success(Proj)
            end},
            {"reload_method_definition no source", fun() ->
                t_reload_method_definition_no_source()
            end},
            {"reload_method_definition compile error", fun() ->
                t_reload_method_definition_compile_error(Proj)
            end},
            {"install_method keeps package module + source attr (bug 2)", fun() ->
                t_install_method_keeps_package_and_source(Proj)
            end},
            {"install_method preserves leading comments, idempotently (bug 1)", fun() ->
                t_install_method_preserves_comments(Proj)
            end},
            {"disk_differs re-reads the on-disk file live (BT-2567)", fun() ->
                t_disk_differs_reflects_live_disk(Proj)
            end},
            {"install_method on test/ class keeps package", fun() ->
                t_install_method_test_dir_keeps_package(Proj)
            end},
            {"install_method accumulation preserves sibling methods + state", fun() ->
                t_install_method_accumulation_preserves_siblings(Proj)
            end},
            {"revert recovers prior body from disk when prev_source absent", fun() ->
                t_revert_recovers_prev_from_disk(Proj)
            end},
            {"new_class/2 success", fun() -> t_new_class_success(Proj) end},
            {"new_class/2 target exists", fun() -> t_new_class_target_exists(Proj) end},
            {"new_class/2 outside project", fun() -> t_new_class_outside_project() end},
            {"new_class/2 name mismatch", fun() -> t_new_class_name_mismatch(Proj) end},
            {"new_class/2 already loaded", fun() -> t_new_class_already_loaded(Proj) end},
            {"new_class/2 compile error", fun() -> t_new_class_compile_error(Proj) end},
            {"compute_package_module_name with metadata", fun() ->
                t_compute_package_module_name(Proj)
            end},
            {"compute_package_module_name test dir", fun() ->
                t_compute_package_module_name_test_dir(Proj)
            end},
            {"compute_package_module_name outside subdirs", fun() ->
                t_compute_package_module_name_outside_subdirs(Proj)
            end},
            {"new_class/2 multiple classes", fun() -> t_new_class_multiple_classes(Proj) end},
            {"new_class/2 agent author", fun() -> t_new_class_agent_author(Proj) end},
            {"handle_load/2 protocol", fun() -> t_handle_load_protocol(Proj) end},
            {"handle_load/3 protocol", fun() -> t_handle_load_3_protocol(Proj) end},
            {"new_class/2 author_kind only", fun() -> t_new_class_author_kind_only(Proj) end},
            {"handle_load_source/3 protocol", fun() -> t_handle_load_source_protocol() end},
            {"reload_class_file/1 protocol", fun() -> t_reload_class_file_protocol(Proj) end},
            {"reload_method_definition existing method span", fun() ->
                t_reload_method_definition_existing(Proj)
            end},
            {"reload_method_definition autoflush", fun() ->
                t_reload_method_definition_autoflush(Proj)
            end},
            {"new method flushes at class body indentation (BT-2583)", fun() ->
                t_new_method_appends_indented(Proj)
            end}
        ]
    end}.

t_handle_load_success(Proj) ->
    Path = write_bt(Proj, "LoaderOne.bt", <<"Object subclass: LoaderOne\n  value => 42\n">>),
    State = beamtalk_repl_state:new(undefined, 0),
    {ok, ClassNames, NewState} = beamtalk_repl_loader:handle_load(Path, State),
    ?assertEqual([#{name => "LoaderOne", superclass => "Object"}], ClassNames),
    %% The compiled module is recorded in the loaded-modules list.
    ?assert(length(beamtalk_repl_state:get_loaded_modules(NewState)) >= 1).

t_handle_load_prebuilt_success(Proj) ->
    Path = write_bt(Proj, "LoaderTwo.bt", <<"Object subclass: LoaderTwo\n  v => 1\n">>),
    State = beamtalk_repl_state:new(undefined, 0),
    %% Empty prebuilt indexes still exercise the /3 arity path.
    {ok, ClassNames, _NewState} = beamtalk_repl_loader:handle_load(Path, State, #{}),
    ?assertEqual([#{name => "LoaderTwo", superclass => "Object"}], ClassNames).

t_handle_load_source_success() ->
    State = beamtalk_repl_state:new(undefined, 0),
    {ok, ClassNames, _NewState} = beamtalk_repl_loader:handle_load_source(
        <<"Object subclass: LoaderInline\n  v => 9\n">>, "inline", State
    ),
    ?assertEqual([#{name => "LoaderInline", superclass => "Object"}], ClassNames).

t_load_class_module_success(Proj) ->
    %% Compile a class to obtain a real binary + module name, then drive
    %% load_class_module/3 directly with a ClassInfo map.
    Path = write_bt(Proj, "LoaderThree.bt", <<"Object subclass: LoaderThree\n  v => 3\n">>),
    {ok, Source} = file:read_file(Path),
    {ok, Binary, ClassNames, ModuleName} = beamtalk_repl_compiler:compile_file(
        binary_to_list(Source), Path, false, undefined
    ),
    ClassInfo = #{binary => Binary, module_name => ModuleName, classes => ClassNames},
    State = beamtalk_repl_state:new(undefined, 0),
    {ok, ClassName, Trailing, _NewState} = beamtalk_repl_loader:load_class_module(
        ClassInfo, "Object subclass: LoaderThree", State
    ),
    ?assertEqual(<<"LoaderThree">>, ClassName),
    ?assertEqual(no_trailing, Trailing).

t_reload_class_file_success(Proj) ->
    Path = write_bt(Proj, "LoaderReload.bt", <<"Object subclass: LoaderReload\n  v => 7\n">>),
    Result = beamtalk_repl_loader:reload_class_file(Path),
    ?assertMatch({ok, [#{name := "LoaderReload"}]}, Result).

t_reload_class_file_2_match(Proj) ->
    Path = write_bt(Proj, "LoaderRl2.bt", <<"Object subclass: LoaderRl2\n  v => 1\n">>),
    Result = beamtalk_repl_loader:reload_class_file(Path, 'LoaderRl2'),
    ?assertMatch({ok, [#{name := "LoaderRl2"}]}, Result).

t_reload_class_file_2_mismatch(Proj) ->
    Path = write_bt(Proj, "LoaderRl3.bt", <<"Object subclass: LoaderRl3\n  v => 1\n">>),
    %% Expected class name does not appear in the compiled class list → error.
    Result = beamtalk_repl_loader:reload_class_file(Path, 'NotThere'),
    ?assertMatch({error, {class_not_found, 'NotThere', _, ["LoaderRl3"]}}, Result).

t_reload_method_definition_success(Proj) ->
    %% Load the class first so its source is recorded in workspace_meta, then
    %% patch a new method. This exercises recompile_with_method,
    %% load_recompiled_method, emit_change_entry and the flushability helpers.
    Path = write_bt(Proj, "LoaderMethod.bt", <<"Object subclass: LoaderMethod\n  v => 1\n">>),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    MethodInfo = #{class_name => <<"LoaderMethod">>, selector => <<"doubled">>},
    Result = beamtalk_repl_loader:reload_method_definition(
        MethodInfo, [], "LoaderMethod >> doubled => self.v * 2", State1
    ),
    {ok, Display, EmptyBin, _Warnings, _NewState} = Result,
    ?assertEqual(<<"LoaderMethod>>doubled">>, Display),
    ?assertEqual(<<>>, EmptyBin).

t_reload_method_definition_no_source() ->
    %% No class source recorded for this name → compile_error, empty binary.
    MethodInfo = #{class_name => <<"NoSuchClassForReload9999">>, selector => <<"foo">>},
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_loader:reload_method_definition(
        MethodInfo, [<<"w">>], "foo => 1", State
    ),
    ?assertMatch({error, {compile_error, _}, <<>>, [<<"w">>], _}, Result).

t_reload_method_definition_compile_error(Proj) ->
    %% A class with recorded source, but a method body that fails to recompile,
    %% takes the {error, Reason} branch of recompile_with_method.
    Path = write_bt(Proj, "LoaderBadM.bt", <<"Object subclass: LoaderBadM\n  v => 1\n">>),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    MethodInfo = #{class_name => <<"LoaderBadM">>, selector => <<"broken">>},
    Result = beamtalk_repl_loader:reload_method_definition(
        MethodInfo, [], "LoaderBadM >> broken => @@@ invalid @@@", State1
    ),
    ?assertMatch({error, _, <<>>, [], _}, Result).

%% Read the live workspace project_path so new-class targets land inside the
%% project tree that classify_source_file/1 actually checks (robust against any
%% workspace_meta restart between setup and test execution).
live_project_dir() ->
    {ok, #{project_path := PP}} = beamtalk_workspace_meta:get_metadata(),
    binary_to_list(PP).

t_new_class_success(_Proj) ->
    Proj = live_project_dir(),
    Name = "NewClassOk" ++ integer_to_list(erlang:unique_integer([positive])),
    Path = filename:join(Proj, Name ++ ".bt"),
    file:delete(Path),
    Src = "Object subclass: " ++ Name ++ "\n  greet => 1\n",
    Result = beamtalk_repl_loader:new_class(list_to_binary(Src), list_to_binary(Path)),
    ?assertMatch({ok, [_ | _]}, Result).

t_new_class_target_exists(_Proj) ->
    Proj = live_project_dir(),
    Path = filename:join(Proj, "NewClassExists.bt"),
    ok = file:write_file(Path, <<"placeholder">>),
    {error, Err} = beamtalk_repl_loader:new_class(
        <<"Object subclass: NewClassExists">>, list_to_binary(Path)
    ),
    ?assertEqual(target_exists, Err#beamtalk_error.kind).

t_new_class_outside_project() ->
    %% A path outside the workspace project_path is rejected before compiling.
    Tmp = unicode:characters_to_list(beamtalk_file:'tempDirectory'()),
    Path =
        Tmp ++ "/bt_outside_" ++ integer_to_list(erlang:unique_integer([positive])) ++
            "/Outsider.bt",
    {error, Err} = beamtalk_repl_loader:new_class(
        <<"Object subclass: Outsider">>, list_to_binary(Path)
    ),
    ?assertEqual(target_outside_project, Err#beamtalk_error.kind).

t_new_class_name_mismatch(_Proj) ->
    Proj = live_project_dir(),
    %% Declared class name does not match the file basename.
    Path = filename:join(Proj, "MismatchFile.bt"),
    file:delete(Path),
    {error, Err} = beamtalk_repl_loader:new_class(
        <<"Object subclass: SomethingElse\n  v => 1\n">>, list_to_binary(Path)
    ),
    ?assertEqual(class_name_mismatch, Err#beamtalk_error.kind).

t_new_class_already_loaded(_Proj) ->
    Proj = live_project_dir(),
    %% Create a class, then attempt to create it again at a fresh path → the
    %% name is already loaded in the registry.
    Name = "NewClassDup" ++ integer_to_list(erlang:unique_integer([positive])),
    Path1 = filename:join(Proj, Name ++ ".bt"),
    file:delete(Path1),
    Src = "Object subclass: " ++ Name ++ "\n  v => 1\n",
    {ok, _} = beamtalk_repl_loader:new_class(list_to_binary(Src), list_to_binary(Path1)),
    %% Second attempt at a different in-project path whose basename equals the
    %% class name (so the name check passes) reaches the already-loaded branch.
    Path3 = filename:join(filename:join(Proj, "sub"), Name ++ ".bt"),
    ok = filelib:ensure_dir(Path3),
    file:delete(Path3),
    {error, Err} = beamtalk_repl_loader:new_class(list_to_binary(Src), list_to_binary(Path3)),
    ?assertEqual(class_already_loaded, Err#beamtalk_error.kind).

t_new_class_compile_error(_Proj) ->
    Proj = live_project_dir(),
    Path = filename:join(Proj, "BadNewClass.bt"),
    file:delete(Path),
    {error, Err} = beamtalk_repl_loader:new_class(
        <<"@@@ not valid beamtalk @@@">>, list_to_binary(Path)
    ),
    %% Compile failure is surfaced as a structured #beamtalk_error{}.
    ?assert(is_record(Err, beamtalk_error)).

t_compute_package_module_name(_Proj) ->
    %% With workspace_meta carrying a project_path AND a package name (from the
    %% beamtalk.toml written in setup), a file under src/ resolves to a
    %% package-qualified module name bt@loaderpkg@widget.
    Proj = live_project_dir(),
    Path = filename:join([Proj, "src", "Widget.bt"]),
    Result = beamtalk_repl_loader:compute_package_module_name(Path),
    ?assertEqual(<<"bt@loaderpkg@widget">>, Result).

t_compute_package_module_name_test_dir(_Proj) ->
    %% A file under test/ resolves to bt@loaderpkg@test@<module>.
    Proj = live_project_dir(),
    Path = filename:join([Proj, "test", "WidgetTest.bt"]),
    Result = beamtalk_repl_loader:compute_package_module_name(Path),
    ?assertEqual(<<"bt@loaderpkg@test@widget_test">>, Result).

t_compute_package_module_name_outside_subdirs(_Proj) ->
    %% A file in the project root but outside src/ and test/ falls back to the
    %% bt@<stem> form via stem_module_name/1.
    Proj = live_project_dir(),
    Path = filename:join([Proj, "Widget.bt"]),
    Result = beamtalk_repl_loader:compute_package_module_name(Path),
    ?assertEqual(<<"bt@widget">>, Result).

t_new_class_agent_author(_Proj) ->
    %% When the submission boundary stamps $beamtalk_author / $beamtalk_author_kind
    %% into the process dictionary, emit_new_class_entry records them. This drives
    %% new_class_author/0 and new_class_author_kind/0's agent branches.
    Proj = live_project_dir(),
    Name = "AgentClass" ++ integer_to_list(erlang:unique_integer([positive])),
    Path = filename:join(Proj, Name ++ ".bt"),
    file:delete(Path),
    erlang:put('$beamtalk_author', <<"agent">>),
    erlang:put('$beamtalk_author_kind', agent),
    try
        Src = "Object subclass: " ++ Name ++ "\n  v => 1\n",
        Result = beamtalk_repl_loader:new_class(list_to_binary(Src), list_to_binary(Path)),
        ?assertMatch({ok, [_ | _]}, Result)
    after
        erlang:erase('$beamtalk_author'),
        erlang:erase('$beamtalk_author_kind')
    end.

t_handle_load_protocol(Proj) ->
    %% A protocol definition file routes through load_protocol_module/3 instead
    %% of the class path (the {ok, protocol_definition, ...} compile result).
    Path = write_bt(
        Proj, "GreetableProto.bt", <<"Protocol define: GreetableProto\n  greet -> String\n">>
    ),
    State = beamtalk_repl_state:new(undefined, 0),
    {ok, ClassNames, NewState} = beamtalk_repl_loader:handle_load(Path, State),
    ?assertEqual([#{name => "GreetableProto", superclass => "Object"}], ClassNames),
    ?assert(length(beamtalk_repl_state:get_loaded_modules(NewState)) >= 1).

t_handle_load_3_protocol(Proj) ->
    %% Protocol routing through the /3 (prebuilt-indexes) arity.
    Path = write_bt(
        Proj, "Greetable3Proto.bt", <<"Protocol define: Greetable3Proto\n  hi -> String\n">>
    ),
    State = beamtalk_repl_state:new(undefined, 0),
    {ok, ClassNames, _NewState} = beamtalk_repl_loader:handle_load(Path, State, #{}),
    ?assertEqual([#{name => "Greetable3Proto", superclass => "Object"}], ClassNames).

t_new_class_author_kind_only(_Proj) ->
    %% Only $beamtalk_author_kind is set (no explicit $beamtalk_author): the
    %% default-author path maps the agent kind to <<"agent">> (line in
    %% new_class_default_author/0).
    Proj = live_project_dir(),
    Name = "KindOnly" ++ integer_to_list(erlang:unique_integer([positive])),
    Path = filename:join(Proj, Name ++ ".bt"),
    file:delete(Path),
    erlang:put('$beamtalk_author_kind', agent),
    try
        Src = "Object subclass: " ++ Name ++ "\n  v => 1\n",
        Result = beamtalk_repl_loader:new_class(list_to_binary(Src), list_to_binary(Path)),
        ?assertMatch({ok, [_ | _]}, Result)
    after
        erlang:erase('$beamtalk_author_kind')
    end.

t_handle_load_source_protocol() ->
    State = beamtalk_repl_state:new(undefined, 0),
    {ok, ClassNames, _NewState} = beamtalk_repl_loader:handle_load_source(
        <<"Protocol define: InlineProto\n  tick -> Integer\n">>, "inline-proto", State
    ),
    ?assertEqual([#{name => "InlineProto", superclass => "Object"}], ClassNames).

t_reload_class_file_protocol(Proj) ->
    %% Stateless reload of a protocol file → load_protocol_module_stateless/2.
    Path = write_bt(
        Proj, "ReloadProto.bt", <<"Protocol define: ReloadProto\n  ping -> Boolean\n">>
    ),
    Result = beamtalk_repl_loader:reload_class_file(Path),
    ?assertMatch({ok, [#{name := "ReloadProto"}]}, Result).

t_new_class_multiple_classes(_Proj) ->
    %% A source declaring two classes is rejected by declared_class_name/1,
    %% surfacing through new_class_validate_and_install's NameErr passthrough.
    Proj = live_project_dir(),
    Path = filename:join(Proj, "MultiClass.bt"),
    file:delete(Path),
    Src = "Object subclass: MultiClass\n  v => 1\nObject subclass: SecondClass\n  w => 2\n",
    Result = beamtalk_repl_loader:new_class(list_to_binary(Src), list_to_binary(Path)),
    ?assertMatch({error, _}, Result).

t_reload_method_definition_existing(_Proj) ->
    Proj = live_project_dir(),
    %% Patch a method that already exists on disk. The flushability path then
    %% reads the disk source and resolves the method span (prev_source + span),
    %% exercising resolve_span_entry's success branch.
    Path = write_bt(
        Proj,
        "LoaderExisting.bt",
        <<"Object subclass: LoaderExisting\n  value => 1\n  doubled => self.value * 2\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    MethodInfo = #{class_name => <<"LoaderExisting">>, selector => <<"doubled">>},
    Result = beamtalk_repl_loader:reload_method_definition(
        MethodInfo, [], "LoaderExisting >> doubled => self.value * 3", State1
    ),
    ?assertMatch({ok, <<"LoaderExisting>>doubled">>, <<>>, _, _}, Result).

t_reload_method_definition_autoflush(Proj) ->
    %% With autoflush enabled, a durable method patch triggers do_autoflush/0.
    %% The flush is best-effort; we only assert the patch itself succeeds.
    ok = beamtalk_workspace_meta:set_setting(autoflush, true),
    try
        Path = write_bt(
            Proj, "LoaderFlush.bt", <<"Object subclass: LoaderFlush\n  value => 1\n">>
        ),
        State0 = beamtalk_repl_state:new(undefined, 0),
        {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
        MethodInfo = #{class_name => <<"LoaderFlush">>, selector => <<"tripled">>},
        Result = beamtalk_repl_loader:reload_method_definition(
            MethodInfo, [], "LoaderFlush >> tripled => self.value * 3", State1
        ),
        ?assertMatch({ok, <<"LoaderFlush>>tripled">>, <<>>, _, _}, Result)
    after
        beamtalk_workspace_meta:set_setting(autoflush, false)
    end.

%% BT-2583: a brand-new method (no prior on-disk span) flushed to a 2-space
%% indented class body is written at that indentation, NOT at column 0. The
%% install hook reshapes the compiler's canonical column-0 body to the class's
%% sibling-method indentation at store time (new_method_entry/3), so flush's
%% verbatim append lands an indented method.
t_new_method_appends_indented(_Proj) ->
    %% Use the live workspace project_path so the patched class's sourceFile is
    %% classified flushable (robust against any workspace_meta restart between
    %% setup and execution — same convention as t_reload_method_definition_existing).
    Proj = live_project_dir(),
    %% Start from a clean ChangeLog so flush/0 only acts on this test's entry
    %% (the integration fixture's log is shared across sub-tests).
    ok = beamtalk_workspace_changelog:clear(),
    %% A 2-space-indented class body with one sibling method. The brand-new
    %% method must be appended at that 2-space indentation, not at column 0.
    Path = write_bt_under(
        Proj,
        "src",
        "IndentAppend.bt",
        <<"Actor subclass: IndentAppend\n  state: v = 1\n\n  value -> Integer =>\n    self.v\n">>
    ),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _, State1} = beamtalk_repl_loader:handle_load(Path, State0),
    %% `bumped' does not exist on disk → selector_not_found → new_method_entry/3.
    %% The structured install path carries a real method body (unlike the bare
    %% `>>' path), so the reshaped, appended source is non-empty.
    {ok, _, _, _, _} = beamtalk_repl_loader:install_method(
        <<"IndentAppend">>,
        <<"bumped">>,
        <<"bumped -> Integer =>\n  self.v + 1">>,
        durable,
        <<"test">>,
        human,
        [],
        State1
    ),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(Path),
    Lines = binary:split(Final, <<"\n">>, [global]),
    %% The appended method's signature line must carry the class body's 2-space
    %% indent — i.e. the column-0 bug is gone.
    SigLines = [L || L <- Lines, contains(L, <<"bumped -> Integer">>)],
    ?assertMatch([_ | _], SigLines),
    lists:foreach(
        fun(L) ->
            ?assertMatch(<<"  bumped", _/binary>>, L),
            %% And not be at column 0.
            ?assertNotMatch(<<"bumped", _/binary>>, L)
        end,
        SigLines
    ).

%% True iff Needle occurs anywhere in Haystack.
contains(Haystack, Needle) ->
    binary:match(Haystack, Needle) =/= nomatch.
