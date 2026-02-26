%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_loader module

-module(beamtalk_repl_loader_tests).
-include_lib("eunit/include/eunit.hrl").

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
    %% Without workspace_meta started, returns undefined
    Result = beamtalk_repl_loader:compute_package_module_name("/some/path/src/Foo.bt"),
    ?assertEqual(undefined, Result).

%%====================================================================
%% resolve_package_module/3
%%====================================================================

resolve_package_module_src_match_test() ->
    %% File under ProjectRoot/src → bt@pkg@module
    %% Use file:get_cwd() as the root so paths are drive-letter-consistent on Windows.
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "myproject"),
    AbsPath = filename:join([ProjectRoot, "src", "Counter.bt"]),
    Result = beamtalk_repl_loader:resolve_package_module(AbsPath, ProjectRoot, <<"mypkg">>),
    ?assertEqual(<<"bt@mypkg@counter">>, Result).

resolve_package_module_test_match_test() ->
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "myproject"),
    AbsPath = filename:join([ProjectRoot, "test", "CounterTest.bt"]),
    Result = beamtalk_repl_loader:resolve_package_module(AbsPath, ProjectRoot, <<"mypkg">>),
    ?assertEqual(<<"bt@mypkg@test@counter_test">>, Result).

resolve_package_module_no_match_test() ->
    {ok, Cwd} = file:get_cwd(),
    ProjectRoot = filename:join(Cwd, "myproject"),
    AbsPath = filename:join([Cwd, "other", "project", "Foo.bt"]),
    Result = beamtalk_repl_loader:resolve_package_module(AbsPath, ProjectRoot, <<"pkg">>),
    ?assertEqual(undefined, Result).

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
    NewState = beamtalk_repl_loader:store_file_class_sources(
        Classes, "Object subclass: MyClass [\n]\n", State
    ),
    ?assertNotEqual(undefined, beamtalk_repl_state:get_class_source(<<"MyClass">>, NewState)).

store_file_class_sources_multiple_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => "A", superclass => "Object"}, #{name => "B", superclass => "Object"}],
    NewState = beamtalk_repl_loader:store_file_class_sources(Classes, "source", State),
    ?assertNotEqual(undefined, beamtalk_repl_state:get_class_source(<<"A">>, NewState)),
    ?assertNotEqual(undefined, beamtalk_repl_state:get_class_source(<<"B">>, NewState)).

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
    %% Falls back to module name as class name
    ?assertEqual(<<"my_fallback_mod">>, Name),
    ?assertNotEqual(
        undefined, beamtalk_repl_state:get_class_source(<<"my_fallback_mod">>, NewState)
    ).

store_class_sources_with_classes_binary_name_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => <<"Counter">>, superclass => <<"Object">>}],
    {Name, NewState} = beamtalk_repl_loader:store_class_sources(Classes, some_mod, "expr", State),
    ?assertEqual(<<"Counter">>, Name),
    ?assertNotEqual(undefined, beamtalk_repl_state:get_class_source(<<"Counter">>, NewState)).

store_class_sources_with_classes_list_name_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Classes = [#{name => "MyActor", superclass => "Actor"}],
    {Name, NewState} = beamtalk_repl_loader:store_class_sources(Classes, some_mod, "expr", State),
    ?assertEqual(<<"MyActor">>, Name),
    ?assertNotEqual(undefined, beamtalk_repl_state:get_class_source(<<"MyActor">>, NewState)).
