%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_tests).

-moduledoc """
Tests for beamtalk_compiler (ADR 0022, Phase 1).

Tests the public API, backend dispatch, in-memory Core Erlang compilation,
and the compile (file) command.
""".

-include_lib("eunit/include/eunit.hrl").

%%% Setup / Teardown

%% Start the compiler application for integration tests.
setup() ->
    %% Ensure compiler app and dependencies are started
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

teardown(_) ->
    ok.

compiler_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"compile_expression succeeds", fun compile_expression_succeeds/0},
        {"compile_expression with known vars", fun compile_expression_known_vars/0},
        {"compile_expression with invalid source returns error", fun compile_expression_error/0},
        {"compile_expression with empty source returns error", fun compile_expression_empty/0},
        {"compile file succeeds", fun compile_file_succeeds/0},
        {"compile file with stdlib mode", fun compile_file_stdlib_mode/0},
        {"compile file with invalid source returns error", fun compile_file_error/0},
        {"compile file with no class returns error", fun compile_file_no_class/0},
        {"diagnostics returns structured diagnostics", fun diagnostics_succeeds/0},
        {"diagnostics for invalid source includes errors", fun diagnostics_errors/0},
        {"diagnostics method mode accepts a bare method body",
            fun diagnostics_method_mode_accepts_bare_body/0},
        {"diagnostics method mode reports a broken body",
            fun diagnostics_method_mode_reports_broken_body/0},
        {"version returns binary", fun version_succeeds/0},
        {"compile_core_erlang in memory", fun compile_core_erlang_in_memory/0},
        {"compile_core_erlang with invalid source", fun compile_core_erlang_invalid/0},
        {"compile_core_erlang scan error", fun compile_core_erlang_scan_error/0},
        {"compile_core_erlang parse error", fun compile_core_erlang_parse_error/0},
        {"backend defaults to port", fun backend_default_port/0},
        {"multiple compiles on same server", fun multiple_compiles/0},
        {"compile file with workspace_mode=false", fun compile_file_workspace_mode/0},
        {"compile_expression with class definition", fun compile_expression_class_def/0},
        {"compiler app module callbacks", fun compiler_app_callbacks/0},
        {"resolve_method_span instance method", fun resolve_method_span_instance/0},
        {"resolve_method_span class method", fun resolve_method_span_class/0},
        {"resolve_method_span selector not found", fun resolve_method_span_not_found/0},
        {"resolve_class_span header only", fun resolve_class_span_header_only/0},
        {"resolve_class_span class not found", fun resolve_class_span_not_found/0},
        {"categorize_methods class with no dividers is one unnamed category",
            fun categorize_methods_no_dividers/0},
        {"categorize_methods groups by divider", fun categorize_methods_groups_by_divider/0},
        {"categorize_methods class not found", fun categorize_methods_not_found/0},
        {"command vocabulary corpus is recognized (BT-3095)",
            fun command_vocabulary_corpus_is_recognized/0}
    ]}.

%%% Tests

compile_expression_succeeds() ->
    {ok, CoreErlang, []} =
        beamtalk_compiler:compile_expression(<<"1 + 2">>, <<"test_mod">>, []),
    ?assert(is_binary(CoreErlang)),
    ?assert(byte_size(CoreErlang) > 0),
    %% The compiler emits `call 'erlang':'+'\n\t\t    (1, 2)`. Check both
    %% the qualified BIF and the literal operand pair so that operand-order
    %% regressions are still caught.
    ?assert(binary:match(CoreErlang, <<"'erlang':'+'">>) =/= nomatch),
    ?assert(binary:match(CoreErlang, <<"1, 2">>) =/= nomatch).

compile_expression_known_vars() ->
    {ok, CoreErlang, []} =
        beamtalk_compiler:compile_expression(<<"x + 1">>, <<"test_mod">>, [<<"x">>]),
    ?assert(is_binary(CoreErlang)),
    %% ADR 0081: REPL free identifiers now look up via `maps:find` with a
    %% `beamtalk_workspace:resolve_name/2` fallthrough (locals-first, then the
    %% lazy global resolver) instead of a bare `maps:get`. Assert the new
    %% lookup call and the `'x'` key so the test still guards variable lookup.
    ?assert(binary:match(CoreErlang, <<"'maps':'find'">>) =/= nomatch),
    ?assert(binary:match(CoreErlang, <<"'beamtalk_workspace':'resolve_name'">>) =/= nomatch),
    ?assert(binary:match(CoreErlang, <<"'x'">>) =/= nomatch).

compile_expression_error() ->
    {error, Diagnostics} =
        beamtalk_compiler:compile_expression(<<"+++">>, <<"test_mod">>, []),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0).

compile_expression_empty() ->
    {error, _} =
        beamtalk_compiler:compile_expression(<<"">>, <<"test_mod">>, []).

compile_file_succeeds() ->
    Source =
        <<"Actor subclass: TestCounter\n  count => self.count\n  increment => self.count := self.count + 1">>,
    {ok, Result} = beamtalk_compiler:compile(Source, #{}),
    ?assert(is_map(Result)),
    ?assert(is_binary(maps:get(core_erlang, Result))),
    ?assert(is_binary(maps:get(module_name, Result))),
    ?assert(is_list(maps:get(classes, Result))),
    %% Module name should follow ADR 0016 naming
    ModuleName = maps:get(module_name, Result),
    ?assert(binary:match(ModuleName, <<"bt@">>) =/= nomatch),
    %% Classes list should contain TestCounter
    Classes = maps:get(classes, Result),
    ?assert(length(Classes) > 0).

compile_file_stdlib_mode() ->
    Source = <<"Object subclass: StdlibTest\n  value => 42">>,
    {ok, Result} = beamtalk_compiler:compile(Source, #{stdlib_mode => true}),
    ModuleName = maps:get(module_name, Result),
    %% Stdlib mode should use bt@stdlib@ prefix
    ?assert(binary:match(ModuleName, <<"bt@stdlib@">>) =/= nomatch).

compile_file_error() ->
    {error, Diagnostics} =
        beamtalk_compiler:compile(<<"+++">>, #{}),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0).

compile_file_no_class() ->
    %% Source with no class definition
    {error, Diagnostics} =
        beamtalk_compiler:compile(<<"1 + 2">>, #{}),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0).

diagnostics_succeeds() ->
    {ok, Diagnostics} = beamtalk_compiler:diagnostics(<<"1 + 2">>),
    ?assert(is_list(Diagnostics)).

diagnostics_errors() ->
    {ok, Diagnostics} = beamtalk_compiler:diagnostics(<<"+++">>),
    ?assert(is_list(Diagnostics)),
    ?assert(length(Diagnostics) > 0),
    %% Check structured format
    [First | _] = Diagnostics,
    ?assert(is_map(First)),
    ?assert(is_binary(maps:get(message, First))),
    ?assert(is_binary(maps:get(severity, First))).

diagnostics_method_mode_accepts_bare_body() ->
    %% BT-2569: a bare method body is a false parse error under the default
    %% (expression) grammar — `=>` is not a valid top-level token — but parses
    %% clean in method mode.
    Body = <<"decrement => self.value := self.value - 1">>,
    {ok, ExprDiags} = beamtalk_compiler:diagnostics(Body, <<"expression">>),
    ?assert(length(ExprDiags) > 0),
    {ok, MethodDiags} = beamtalk_compiler:diagnostics(Body, <<"method">>),
    ?assertEqual([], MethodDiags).

diagnostics_method_mode_reports_broken_body() ->
    %% The parse-only method path still reports genuine errors (here `:=` with no
    %% right-hand side), so it is not a no-op.
    {ok, Diags} = beamtalk_compiler:diagnostics(<<"decrement => self.value :=">>, <<"method">>),
    ?assert(length(Diags) > 0).

version_succeeds() ->
    {ok, Version} = beamtalk_compiler:version(),
    ?assert(is_binary(Version)),
    ?assert(byte_size(Version) > 0).

compile_core_erlang_in_memory() ->
    %% First, get some Core Erlang from the compiler
    {ok, CoreErlang, []} =
        beamtalk_compiler:compile_expression(<<"42">>, <<"test_in_memory">>, []),
    %% Compile it in memory
    {ok, ModuleName, Binary} = beamtalk_compiler:compile_core_erlang(CoreErlang),
    ?assert(is_atom(ModuleName)),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0),
    %% Load and execute the module
    {module, ModuleName} = code:load_binary(ModuleName, "", Binary),
    Result = ModuleName:eval(#{}),
    %% Clean up
    code:purge(ModuleName),
    code:delete(ModuleName),
    ?assertMatch({42, _}, Result).

compile_core_erlang_invalid() ->
    Result = beamtalk_compiler:compile_core_erlang(<<"this is not core erlang">>),
    ?assertMatch({error, _}, Result).

compile_core_erlang_scan_error() ->
    %% Binary that produces core_scan error
    Result = beamtalk_compiler:compile_core_erlang(<<"\x00\x01">>),
    ?assertMatch({error, _}, Result).

compile_core_erlang_parse_error() ->
    %% Scannable but invalid Core Erlang structure
    Result = beamtalk_compiler:compile_core_erlang(<<"module 'x' []\n">>),
    ?assertMatch({error, {core_parse_error, _}}, Result).

backend_default_port() ->
    Original = os:getenv("BEAMTALK_COMPILER"),
    try
        os:unsetenv("BEAMTALK_COMPILER"),
        ?assertEqual(port, beamtalk_compiler_backend:backend())
    after
        case Original of
            false -> ok;
            Value -> os:putenv("BEAMTALK_COMPILER", Value)
        end
    end.

multiple_compiles() ->
    {ok, _, []} = beamtalk_compiler:compile_expression(<<"1 + 2">>, <<"m1">>, []),
    {ok, _, []} = beamtalk_compiler:compile_expression(<<"3 * 4">>, <<"m2">>, []),
    {ok, _, []} = beamtalk_compiler:compile_expression(<<"5 - 1">>, <<"m3">>, []).

compile_file_workspace_mode() ->
    Source = <<"Actor subclass: WsModeTest\n  value => 42">>,
    {ok, Result} = beamtalk_compiler:compile(Source, #{workspace_mode => false}),
    ?assert(is_map(Result)),
    ?assert(is_binary(maps:get(core_erlang, Result))).

compile_expression_class_def() ->
    %% Inline class definition should return class_definition tuple
    Source = <<"Actor subclass: InlineClassTest\n  value => 42">>,
    Result = beamtalk_compiler:compile_expression(Source, <<"inline_test">>, []),
    ?assertMatch({ok, class_definition, _}, Result).

compiler_app_callbacks() ->
    %% stop/1 returns ok
    ?assertEqual(ok, beamtalk_compiler_app:stop(undefined)).

%% --- resolve_method_span (ADR 0082 Phase 1, BT-2283) ---

%% Shared fixture: a small class with an instance and a class-side method.
span_fixture() ->
    <<
        "Object subclass: SpanCounter\n"
        "\n"
        "  increment => self.value := self.value + 1\n"
        "\n"
        "  class new => self basicNew\n"
    >>.

resolve_method_span_instance() ->
    Source = span_fixture(),
    {ok, #{start := Start, 'end' := End}, PrevSource} =
        beamtalk_compiler:resolve_method_span(Source, <<"SpanCounter">>, <<"increment">>, instance),
    %% Splicing PrevSource back over the span is a no-op — the load-bearing
    %% property of the byte-span splice strategy.
    ?assertEqual(PrevSource, binary:part(Source, Start, End - Start)),
    ?assert(binary:match(PrevSource, <<"increment =>">>) =/= nomatch).

resolve_method_span_class() ->
    Source = span_fixture(),
    {ok, _Span, PrevSource} =
        beamtalk_compiler:resolve_method_span(Source, <<"SpanCounter">>, <<"new">>, class),
    ?assert(binary:match(PrevSource, <<"class new =>">>) =/= nomatch).

resolve_method_span_not_found() ->
    Source = span_fixture(),
    Result =
        beamtalk_compiler:resolve_method_span(Source, <<"SpanCounter">>, <<"nope">>, instance),
    ?assertMatch({error, selector_not_found, _}, Result).

%% --- resolve_class_span (ADR 0082 extension, BT-3248) ---

resolve_class_span_header_only() ->
    Source = span_fixture(),
    {ok, #{start := Start, 'end' := End}, PrevSource} =
        beamtalk_compiler:resolve_class_span(Source, <<"SpanCounter">>),
    %% Splicing PrevSource back over the span is a no-op, same load-bearing
    %% property as the method-span resolver.
    ?assertEqual(PrevSource, binary:part(Source, Start, End - Start)),
    %% span_fixture()'s SpanCounter has no state declarations, so the span is
    %% just its header line — deliberately excluding every method (BT-3248:
    %% a class-def entry must never be able to reach a method's bytes).
    ?assertEqual(<<"Object subclass: SpanCounter\n">>, PrevSource),
    ?assertEqual(nomatch, binary:match(PrevSource, <<"increment =>">>)),
    ?assertEqual(nomatch, binary:match(PrevSource, <<"class new =>">>)).

resolve_class_span_not_found() ->
    Source = span_fixture(),
    Result = beamtalk_compiler:resolve_class_span(Source, <<"NoSuchClass">>),
    ?assertMatch({error, class_not_found, _}, Result).

%% --- categorize_methods (BT-3238) ---

%% span_fixture()'s SpanCounter has no `// === Name ===` dividers, so it must
%% come back as a single, unnamed (implicit leading) category — the
%% `has_dividers` gate every consumer uses to fall back to a flat rendering.
categorize_methods_no_dividers() ->
    Source = span_fixture(),
    {ok, Categories} = beamtalk_compiler:categorize_methods(Source, <<"SpanCounter">>),
    ?assertMatch([#{name := undefined}], Categories).

divider_fixture() ->
    <<
        "Object subclass: SpanCounter\n"
        "\n"
        "  increment => self.value := self.value + 1\n"
        "\n"
        "  // === Construction ===\n"
        "\n"
        "  class new => self basicNew\n"
    >>.

categorize_methods_groups_by_divider() ->
    Source = divider_fixture(),
    {ok, Categories} = beamtalk_compiler:categorize_methods(Source, <<"SpanCounter">>),
    ?assertMatch(
        [
            #{name := undefined, methods := [#{selector := <<"increment">>, side := instance}]},
            #{
                name := <<"Construction">>,
                divider_span := #{start := _, 'end' := _},
                methods := [#{selector := <<"new">>, side := class}]
            }
        ],
        Categories
    ).

categorize_methods_not_found() ->
    Source = span_fixture(),
    Result = beamtalk_compiler:categorize_methods(Source, <<"NoSuchClass">>),
    ?assertMatch({error, class_not_found, _}, Result).

%%% ---------------------------------------------------------------
%%% Command-vocabulary conformance corpus (BT-3095)
%%% ---------------------------------------------------------------

%% BT-3095 conformance: every command in the shared wire-vocabulary corpus
%% must dispatch successfully through `beamtalk_compiler`'s public API,
%% against the REAL compiled `beamtalk-compiler-port` binary (via
%% `beamtalk_compiler_server`'s port, opened by this file's `setup/0`). The
%% corpus is the single source of truth both implementations are pinned to;
%% the Rust side asserts the identical list against `handle_request`'s
%% dispatch in `tests::handle_request_recognizes_shared_command_vocabulary_corpus`
%% (`crates/beamtalk-compiler-port/src/main.rs`) — see that function's doc
%% comment, and `handle_request`'s, for the full rationale. A command
%% missing from either side therefore fails a build-time test instead of
%% surfacing only as a runtime "Unknown command" error wherever it's
%% invoked.
command_vocabulary_corpus_is_recognized() ->
    Corpus = load_command_vocabulary_corpus(),
    ?assert(length(Corpus) > 0),
    lists:foreach(fun assert_command_recognized/1, Corpus).

%% Exercise one corpus command through a minimal-but-successful call to
%% `beamtalk_compiler`'s public API. Each clause asserts an unambiguous
%% success shape — not just "didn't error" — so an "Unknown command"
%% mismatch can't hide behind a legitimate not-found/empty result.
%% `resolve_completion_type` needs an *exact* expected value rather than
%% just `{ok, _}`: its Erlang API collapses every failure shape (a
%% legitimate not-found result *and* an unrecognized command) into the same
%% `{error, type_unknown}`, so only a guaranteed-resolvable expression proves
%% the command actually dispatched.
assert_command_recognized(<<"compile_expression">>) ->
    ?assertMatch(
        {ok, _, _}, beamtalk_compiler:compile_expression(<<"1 + 2">>, <<"bt3095_ce">>, [])
    );
assert_command_recognized(<<"compile_expression_trace">>) ->
    ?assertMatch(
        {ok, _, _},
        beamtalk_compiler:compile_expression_trace(<<"1 + 2">>, <<"bt3095_cet">>, [])
    );
assert_command_recognized(<<"compile">>) ->
    Source = <<"Object subclass: Bt3095CompileTarget\n  value => 42">>,
    ?assertMatch({ok, _}, beamtalk_compiler:compile(Source, #{}));
assert_command_recognized(<<"compile_method">>) ->
    ClassSource = <<"Object subclass: Bt3095MethodTarget\n  greet => 42">>,
    MethodSource = <<"greet => 43">>,
    ?assertMatch({ok, _}, beamtalk_compiler:compile_method(ClassSource, MethodSource, #{}));
assert_command_recognized(<<"diagnostics">>) ->
    ?assertMatch({ok, _}, beamtalk_compiler:diagnostics(<<"1 + 2">>));
assert_command_recognized(<<"version">>) ->
    ?assertMatch({ok, _}, beamtalk_compiler:version());
assert_command_recognized(<<"resolve_completion_type">>) ->
    %% "42" resolves deterministically to 'Integer' — see
    %% `completion_provider::tests::resolve_expression_type_integer_literal`.
    ?assertEqual({ok, 'Integer'}, beamtalk_compiler:resolve_completion_type(<<"42">>));
assert_command_recognized(<<"find_senders_in_source">>) ->
    ?assertMatch({ok, _}, beamtalk_compiler:find_senders_in_source(<<"x printNl">>, printNl));
assert_command_recognized(<<"find_all_sends_in_source">>) ->
    ?assertMatch({ok, _}, beamtalk_compiler:find_all_sends_in_source(<<"x printNl">>));
assert_command_recognized(<<"find_references_to_in_source">>) ->
    ?assertMatch(
        {ok, _},
        beamtalk_compiler:find_references_to_in_source(<<"x := MyClass new">>, 'MyClass')
    );
assert_command_recognized(<<"find_field_readers_in_source">>) ->
    ?assertMatch(
        {ok, _}, beamtalk_compiler:find_field_readers_in_source(<<"^ self.value">>, value)
    );
assert_command_recognized(<<"find_field_writers_in_source">>) ->
    ?assertMatch(
        {ok, _},
        beamtalk_compiler:find_field_writers_in_source(<<"self.value := 42">>, <<"value">>)
    );
assert_command_recognized(<<"find_ffi_sites_in_source">>) ->
    ?assertMatch(
        {ok, _},
        beamtalk_compiler:find_ffi_sites_in_source(
            <<"(Erlang lists) reverse: x">>, lists, reverse, any
        )
    );
assert_command_recognized(<<"find_announce_sites_in_source">>) ->
    ?assertMatch({ok, _}, beamtalk_compiler:find_announce_sites_in_source(<<"x printNl">>));
assert_command_recognized(<<"resolve_method_span">>) ->
    ?assertMatch(
        {ok, _, _},
        beamtalk_compiler:resolve_method_span(
            span_fixture(), <<"SpanCounter">>, <<"increment">>, instance
        )
    );
assert_command_recognized(<<"reindent_method_source">>) ->
    ?assertMatch({ok, _}, beamtalk_compiler:reindent_method_source(<<"greet => 42">>, <<"  ">>));
assert_command_recognized(<<"resolve_class_span">>) ->
    ?assertMatch(
        {ok, _, _},
        beamtalk_compiler:resolve_class_span(span_fixture(), <<"SpanCounter">>)
    );
assert_command_recognized(<<"categorize_methods">>) ->
    ?assertMatch(
        {ok, _},
        beamtalk_compiler:categorize_methods(span_fixture(), <<"SpanCounter">>)
    );
assert_command_recognized(Command) ->
    %% A corpus entry with no dispatch clause is a test-authoring gap, not a
    %% vocabulary mismatch — fail loudly rather than silently skipping it.
    error({no_dispatch_table_entry_for_corpus_command, Command}).

%% Load the shared compiler-port command-vocabulary conformance corpus
%% (BT-3095) from the repo tree. `beamtalk_test_corpus` (BT-3099) walks up
%% from the test CWD to the project root (the dir holding `Cargo.toml`),
%% then reads the fixture both surfaces share. `beamtalk_test_support` is a
%% test-only peer app (ADR 0022 — `beamtalk_compiler` has no dependency on
%% `beamtalk_runtime`), never listed in `beamtalk_compiler.app.src`.
load_command_vocabulary_corpus() ->
    beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_compiler",
        "test",
        "fixtures",
        "compiler_port_command_vocabulary_corpus.json"
    ]).
