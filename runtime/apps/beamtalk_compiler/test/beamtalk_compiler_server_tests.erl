%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_server_tests).

-moduledoc """
Tests for beamtalk_compiler_server internal functions.

Tests response handlers, compile_core_erlang,
and gen_server edge cases (unknown call, cast, info).
Integration tests (compile_expression, compile, diagnostics, version)
are covered in beamtalk_compiler_tests.erl.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ---------------------------------------------------------------
%%% compile_core_erlang/1 — direct coverage of the server's function
%%% ---------------------------------------------------------------

compile_core_erlang_valid_test() ->
    CoreErlang = valid_core_erlang(),
    {ok, test_server_mod, Binary} = beamtalk_compiler_server:compile_core_erlang(CoreErlang),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

compile_core_erlang_with_warnings_test() ->
    %% A valid module should compile even if compile:forms returns warnings
    CoreErlang = valid_core_erlang(),
    Result = beamtalk_compiler_server:compile_core_erlang(CoreErlang),
    ?assertMatch({ok, _, _}, Result).

compile_core_erlang_scan_error_test() ->
    %% Use a character that core_scan cannot tokenize
    Result = beamtalk_compiler_server:compile_core_erlang(<<"\x00\x01\x02">>),
    ?assertMatch({error, _}, Result).

compile_core_erlang_parse_error_test() ->
    Result = beamtalk_compiler_server:compile_core_erlang(<<"module 'x' []\n">>),
    ?assertMatch({error, {core_parse_error, _}}, Result).

compile_core_erlang_empty_test() ->
    Result = beamtalk_compiler_server:compile_core_erlang(<<>>),
    ?assertMatch({error, _}, Result).

%%% ---------------------------------------------------------------
%%% handle_compile_response/1 — internal response handler
%%% ---------------------------------------------------------------

handle_compile_response_ok_test() ->
    Response = #{
        status => ok,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        classes => [<<"Foo">>],
        warnings => []
    },
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch(
        {ok, #{
            core_erlang := <<"core">>,
            module_name := <<"mod">>,
            classes := [<<"Foo">>],
            warnings := []
        }},
        Result
    ).

%% ADR 0108 hot-reload re-check trigger (BT-2899): a class-defining
%% compile's `referenced_aliases` field must survive this response
%% reshaping, defaulting to `[]` for an older compiler-port binary that
%% omits the key.
handle_compile_response_ok_forwards_referenced_aliases_test() ->
    Response = #{
        status => ok,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        classes => [<<"Foo">>],
        warnings => [],
        referenced_aliases => [<<"Direction">>]
    },
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch(
        {ok, #{referenced_aliases := [<<"Direction">>]}},
        Result
    ).

%% BT-2917 (BT-2899 follow-up): the protocol_definition clause is a
%% *separate* reshaping match arm from the class-definition one above — it
%% must forward `referenced_aliases` too. Confirmed missing by inspection
%% before this fix: the port response carried the field, but this reshaping
%% step silently dropped it (rebuilds the reply map by hand instead of
%% forwarding `Response` verbatim), so nothing downstream ever saw it, even
%% though `crates/beamtalk-compiler-port/src/main.rs`'s
%% `protocol_definition_ok_response` had already started sending it.
handle_compile_response_protocol_definition_forwards_referenced_aliases_test() ->
    Response = #{
        status => ok,
        kind => protocol_definition,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        protocols => [<<"Directional">>],
        warnings => [],
        referenced_aliases => [<<"Direction">>]
    },
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch(
        {ok, protocol_definition, #{referenced_aliases := [<<"Direction">>]}},
        Result
    ).

%% Defensive default: a protocol_definition response omitting the field
%% entirely (an older compiler-port binary predating BT-2917) must still
%% decode, with `referenced_aliases => []` rather than crashing.
handle_compile_response_protocol_definition_defaults_referenced_aliases_test() ->
    Response = #{
        status => ok,
        kind => protocol_definition,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        protocols => [<<"Directional">>],
        warnings => []
    },
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch(
        {ok, protocol_definition, #{referenced_aliases := []}},
        Result
    ).

handle_compile_response_error_test() ->
    Response = #{status => error, diagnostics => [<<"parse error">>]},
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertEqual({error, [<<"parse error">>]}, Result).

handle_compile_response_unexpected_test() ->
    Response = #{status => unknown_status, data => <<"garbage">>},
    Result = beamtalk_compiler_server:handle_compile_response(Response),
    ?assertMatch(
        {error, [#{message := <<"Unexpected compiler response">>}]},
        Result
    ).

%%% ---------------------------------------------------------------
%%% handle_diagnostics_response/1 — internal response handler
%%% ---------------------------------------------------------------

handle_diagnostics_response_ok_test() ->
    Response = #{status => ok, diagnostics => [#{message => <<"warn">>}]},
    Result = beamtalk_compiler_server:handle_diagnostics_response(Response),
    ?assertEqual({ok, [#{message => <<"warn">>}]}, Result).

handle_diagnostics_response_error_test() ->
    Response = #{status => error, diagnostics => [<<"syntax error">>]},
    Result = beamtalk_compiler_server:handle_diagnostics_response(Response),
    ?assertEqual({error, [<<"syntax error">>]}, Result).

handle_diagnostics_response_unexpected_test() ->
    Response = #{bogus => true},
    Result = beamtalk_compiler_server:handle_diagnostics_response(Response),
    ?assertMatch(
        {error, [#{message := <<"Unexpected compiler response">>}]},
        Result
    ).

%%% ---------------------------------------------------------------
%%% handle_version_response/1 — internal response handler
%%% ---------------------------------------------------------------

handle_version_response_ok_test() ->
    Response = #{status => ok, version => <<"1.0.0">>},
    Result = beamtalk_compiler_server:handle_version_response(Response),
    ?assertEqual({ok, <<"1.0.0">>}, Result).

handle_version_response_unexpected_test() ->
    Response = #{status => error, message => <<"fail">>},
    Result = beamtalk_compiler_server:handle_version_response(Response),
    ?assertEqual({error, unexpected_response}, Result).

%%% ---------------------------------------------------------------
%%% ADR 0050 Phase 3: Class cache (register_class, clear_classes)
%%% ---------------------------------------------------------------

class_cache_test_() ->
    {setup, fun start_compiler/0, fun stop_compiler/1, [
        {"register → class visible in state", fun register_class_visible/0},
        {"register twice → overwrites", fun register_class_overwrites/0},
        {"clear_classes → cache emptied", fun clear_classes_empties/0},
        {"crash recovery → no builtins in cache", fun crash_recovery_populates/0},
        {"register_class when server down → no crash", fun register_when_down/0}
    ]}.

register_class_visible() ->
    beamtalk_compiler_server:clear_classes(),
    Meta = #{class => 'TestFoo', superclass => 'Object', fields => []},
    beamtalk_compiler_server:register_class('TestFoo', Meta),
    %% gen_server mailbox ordering: the synchronous get_classes/0 call is
    %% guaranteed to be processed after the preceding cast.
    Classes = beamtalk_compiler_server:get_classes(),
    ?assertMatch(#{class := 'TestFoo'}, maps:get('TestFoo', Classes)).

register_class_overwrites() ->
    beamtalk_compiler_server:clear_classes(),
    Meta1 = #{class => 'TestBar', superclass => 'Object', fields => []},
    Meta2 = #{class => 'TestBar', superclass => 'Actor', fields => [x]},
    beamtalk_compiler_server:register_class('TestBar', Meta1),
    beamtalk_compiler_server:register_class('TestBar', Meta2),
    Classes = beamtalk_compiler_server:get_classes(),
    ?assertMatch(#{superclass := 'Actor', fields := [x]}, maps:get('TestBar', Classes)).

clear_classes_empties() ->
    Meta = #{class => 'TestBaz', superclass => 'Object', fields => []},
    beamtalk_compiler_server:register_class('TestBaz', Meta),
    ok = beamtalk_compiler_server:clear_classes(),
    Classes = beamtalk_compiler_server:get_classes(),
    ?assertEqual(#{}, Classes).

crash_recovery_populates() ->
    %% Restart the server and check that recovery excludes builtins.
    %% Note: if no non-builtin Beamtalk classes are loaded, recovery correctly
    %% returns #{} — the assertion checks the invariant "no builtins in cache".
    application:stop(beamtalk_compiler),
    application:start(beamtalk_compiler),
    %% application:start/1 is synchronous — init/1 (including recovery) has
    %% completed before start/1 returns, so no sleep is needed.
    Classes = beamtalk_compiler_server:get_classes(),
    BuiltinSet = sets:from_list(beamtalk_class_metadata:all_builtins()),
    ?assert(
        maps:fold(
            fun(ClassName, _Meta, Acc) ->
                Acc andalso not sets:is_element(ClassName, BuiltinSet)
            end,
            true,
            Classes
        )
    ).

register_when_down() ->
    %% Calling register_class/2 when the server is not running must not crash.
    application:stop(beamtalk_compiler),
    ?assertEqual(ok, beamtalk_compiler_server:register_class('TestDown', #{class => 'TestDown'})),
    application:start(beamtalk_compiler).

%%% ---------------------------------------------------------------
%%% ADR 0108 hot-reload re-check trigger (BT-2899): ambient alias cache
%%% (register_aliases, get_aliases)
%%% ---------------------------------------------------------------

alias_cache_test_() ->
    {setup, fun start_compiler/0, fun stop_compiler/1, [
        {"register → alias visible in state", fun register_aliases_visible/0},
        {"redefinition overwrites by name", fun register_aliases_redefinition_overwrites/0},
        {"two sessions' aliases merge, neither wipes the other",
            fun register_aliases_concurrent_sessions_merge/0},
        {"clear_classes → alias cache emptied too", fun clear_classes_empties_aliases/0},
        {"register_aliases when server down → no crash", fun register_aliases_when_down/0},
        {"compile_expression/3 backstops the ambient alias cache (BT-2956)",
            fun compile_expression_ambient_alias_backstop/0}
    ]}.

register_aliases_visible() ->
    beamtalk_compiler_server:clear_classes(),
    ok = beamtalk_compiler_server:register_aliases([<<"type Direction = #north | #south">>]),
    ?assertEqual(
        [<<"type Direction = #north | #south">>], beamtalk_compiler_server:get_aliases()
    ).

register_aliases_redefinition_overwrites() ->
    beamtalk_compiler_server:clear_classes(),
    ok = beamtalk_compiler_server:register_aliases([<<"type Direction = #north | #south">>]),
    ok = beamtalk_compiler_server:register_aliases([
        <<"type Direction = #north | #south | #east | #west">>
    ]),
    ?assertEqual(
        [<<"type Direction = #north | #south | #east | #west">>],
        beamtalk_compiler_server:get_aliases()
    ).

%% ADR 0108 adversarial review finding: `beamtalk_compiler_server` is a
%% single node-global process shared by every concurrent REPL session
%% (`beamtalk_session_sup`). Session A registering its own alias table must
%% not wipe session B's — the ambient cache merges by alias name rather
%% than replacing wholesale (see the `aliases` field's doc).
register_aliases_concurrent_sessions_merge() ->
    beamtalk_compiler_server:clear_classes(),
    %% Session A's own current table (just `Foo`).
    ok = beamtalk_compiler_server:register_aliases([<<"type Foo = Integer">>]),
    %% Session B's own, entirely disjoint table (just `Bar`) — must merge
    %% in alongside Foo, not replace it.
    ok = beamtalk_compiler_server:register_aliases([<<"type Bar = String">>]),
    Aliases = lists:sort(beamtalk_compiler_server:get_aliases()),
    ?assertEqual(
        lists:sort([<<"type Foo = Integer">>, <<"type Bar = String">>]),
        Aliases
    ).

clear_classes_empties_aliases() ->
    ok = beamtalk_compiler_server:register_aliases([<<"type Baz = Symbol">>]),
    ok = beamtalk_compiler_server:clear_classes(),
    ?assertEqual([], beamtalk_compiler_server:get_aliases()).

register_aliases_when_down() ->
    application:stop(beamtalk_compiler),
    ?assertEqual(ok, beamtalk_compiler_server:register_aliases([<<"type Down = Integer">>])),
    application:start(beamtalk_compiler).

%% BT-2956: `compile_expression/3` (no explicit `known_type_aliases`) must
%% still resolve an earlier-turn alias via the ambient cache — proving the
%% `{compile_expression}` handler now defaults `known_type_aliases` the same
%% way `{compile}`/`{compile_method}` already do, instead of only ever
%% seeing whatever (if anything) the caller happened to pass in `Options`.
compile_expression_ambient_alias_backstop() ->
    beamtalk_compiler_server:clear_classes(),
    ok = beamtalk_compiler_server:register_aliases([<<"type Direction = #north | #south">>]),
    {ok, protocol_definition, ProtocolInfo} = beamtalk_compiler_server:compile_expression(
        <<"Protocol define: Directional\n  heading: d :: Direction -> Boolean\n">>,
        <<"bt@ambient_backstop">>,
        []
    ),
    ?assertEqual([<<"Direction">>], maps:get(referenced_aliases, ProtocolInfo)).

%%% ---------------------------------------------------------------
%%% gen_server edge cases (via running server)
%%% ---------------------------------------------------------------

gen_server_edge_test_() ->
    {setup, fun start_compiler/0, fun stop_compiler/1, [
        {"unknown call returns error", fun unknown_call/0},
        {"cast is ignored gracefully", fun cast_ignored/0},
        {"unknown info is ignored gracefully", fun unknown_info/0}
    ]}.

unknown_call() ->
    Result = gen_server:call(beamtalk_compiler_server, {bogus_request, 42}, 5000),
    ?assertEqual({error, unknown_request}, Result).

cast_ignored() ->
    %% cast should not crash the server
    gen_server:cast(beamtalk_compiler_server, {bogus_cast}),
    timer:sleep(50),
    %% Server is still alive
    ?assert(is_pid(whereis(beamtalk_compiler_server))).

unknown_info() ->
    %% Sending random message should not crash the server
    beamtalk_compiler_server ! {unexpected_message, 123},
    timer:sleep(50),
    ?assert(is_pid(whereis(beamtalk_compiler_server))).

%%% ---------------------------------------------------------------
%%% BT-2832: inject_diagnostics_failure/1 (test-only fault injection)
%%%
%%% Direct, isolated coverage of the mechanism itself — one-shot consumption
%%% and self-clearing — independent of `beamtalk_repl_loader_recheck_tests`'s
%%% higher-level integration tests (which exercise it as a means to reach
%%% `beamtalk_recheck`'s otherwise-unreachable `failed` outcome).
%%% ---------------------------------------------------------------

diagnostics_fault_injection_test_() ->
    {setup, fun start_compiler/0, fun stop_compiler/1, [
        {"armed fault fails the next diagnostics call", fun fault_fails_next_call/0},
        {"fault self-clears after being consumed", fun fault_self_clears/0},
        {"unarmed diagnostics call reaches the real port", fun no_fault_reaches_port/0}
    ]}.

fault_fails_next_call() ->
    ok = beamtalk_compiler_server:inject_diagnostics_failure(<<"BT-2832 test fault">>),
    Result = beamtalk_compiler_server:diagnostics(<<"1 + 2">>),
    ?assertMatch({error, [#{message := <<"BT-2832 test fault">>}]}, Result).

fault_self_clears() ->
    ok = beamtalk_compiler_server:inject_diagnostics_failure(<<"BT-2832 test fault">>),
    FaultedResult = beamtalk_compiler_server:diagnostics(<<"1 + 2">>),
    ?assertMatch({error, _}, FaultedResult),
    %% The fault was consumed by the call above — this one must reach the
    %% real port and succeed normally, proving the flag is one-shot, not
    %% "stuck on" for the rest of the process's life.
    RecoveredResult = beamtalk_compiler_server:diagnostics(<<"1 + 2">>),
    ?assertMatch({ok, _}, RecoveredResult).

no_fault_reaches_port() ->
    %% No fault armed — an ordinary diagnostics call is unaffected.
    Result = beamtalk_compiler_server:diagnostics(<<"1 + 2">>),
    ?assertMatch({ok, _}, Result).

%%% ---------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------

%%% ---------------------------------------------------------------
%%% Public API integration — drive each server call through to the live
%%% compiler port (covers the API wrappers + handle_call clauses + the
%%% do_compile / do_diagnostics / do_version / send_port_request plumbing).
%%% ---------------------------------------------------------------

server_api_test_() ->
    {setup, fun start_compiler/0, fun stop_compiler/1, [
        {"compile_expression/3", fun api_compile_expression/0},
        {"compile_expression/4 with options", fun api_compile_expression_opts/0},
        {"compile_expression_trace/3", fun api_compile_trace/0},
        {"compile_expression_trace/4 with options", fun api_compile_trace_opts/0},
        {"compile/2 class definition", fun api_compile/0},
        {"diagnostics/1", fun api_diagnostics/0},
        {"version/0", fun api_version/0},
        {"resolve_completion_type/1", fun api_resolve_completion_type/0},
        {"find_senders_in_source/2", fun api_find_senders/0},
        {"find_all_sends_in_source/1", fun api_find_all_sends/0},
        {"find_references_to_in_source/2", fun api_find_references/0},
        {"find_field_readers_in_source/2", fun api_find_field_readers/0},
        {"find_field_writers_in_source/2", fun api_find_field_writers/0},
        {"find_ffi_sites_in_source/4", fun api_find_ffi_sites/0},
        {"resolve_method_span/4", fun api_resolve_method_span/0}
    ]}.

api_compile_expression() ->
    {ok, Core, []} = beamtalk_compiler_server:compile_expression(<<"1 + 2">>, <<"m">>, []),
    ?assert(binary:match(Core, <<"'erlang':'+'">>) =/= nomatch).

api_compile_expression_opts() ->
    Options = #{class_module_index => #{<<"Counter">> => <<"bt@app@counter">>}},
    {ok, Core, _} =
        beamtalk_compiler_server:compile_expression(<<"1 + 2">>, <<"m">>, [], Options),
    ?assert(is_binary(Core)).

api_compile_trace() ->
    {ok, Core, []} =
        beamtalk_compiler_server:compile_expression_trace(<<"1 + 2">>, <<"m">>, []),
    ?assert(is_binary(Core)).

api_compile_trace_opts() ->
    Options = #{class_hierarchy => #{'Counter' => #{superclass => 'Object'}}},
    {ok, Core, _} =
        beamtalk_compiler_server:compile_expression_trace(<<"1 + 2">>, <<"m">>, [], Options),
    ?assert(is_binary(Core)).

api_compile() ->
    Source = <<"Object subclass: TmpServerC\n\n  foo => 1\n">>,
    Result = beamtalk_compiler_server:compile(Source, #{}),
    ?assertMatch({ok, _}, Result).

api_diagnostics() ->
    Result = beamtalk_compiler_server:diagnostics(<<"1 + 2">>),
    ?assertMatch({ok, _}, Result).

api_version() ->
    Result = beamtalk_compiler_server:version(),
    ?assertMatch({ok, _}, Result).

api_resolve_completion_type() ->
    Result = beamtalk_compiler_server:resolve_completion_type(<<"42">>),
    ?assert(
        Result =:= {error, type_unknown} orelse
            (is_tuple(Result) andalso element(1, Result) =:= ok)
    ).

api_find_senders() ->
    {ok, Lines} = beamtalk_compiler_server:find_senders_in_source(<<"x printNl">>, printNl),
    ?assert(lists:member(1, Lines)).

api_find_all_sends() ->
    {ok, Sends} = beamtalk_compiler_server:find_all_sends_in_source(<<"x printNl">>),
    ?assert(lists:any(fun(S) -> maps:get(selector, S) =:= <<"printNl">> end, Sends)).

api_find_references() ->
    {ok, Lines} =
        beamtalk_compiler_server:find_references_to_in_source(<<"x := MyClass new">>, 'MyClass'),
    ?assert(lists:member(1, Lines)).

api_find_field_readers() ->
    {ok, Lines} =
        beamtalk_compiler_server:find_field_readers_in_source(<<"^ self.value">>, value),
    ?assert(lists:member(1, Lines)).

api_find_field_writers() ->
    {ok, Lines} =
        beamtalk_compiler_server:find_field_writers_in_source(<<"self.value := 42">>, value),
    ?assert(lists:member(1, Lines)).

api_find_ffi_sites() ->
    {ok, Lines} =
        beamtalk_compiler_server:find_ffi_sites_in_source(
            <<"(Erlang lists) reverse: x">>, lists, reverse, any
        ),
    ?assert(is_list(Lines)).

api_resolve_method_span() ->
    Source = <<
        "Object subclass: SpanServerC\n"
        "\n"
        "  increment => self.value := self.value + 1\n"
    >>,
    {ok, #{start := Start, 'end' := End}, Prev} =
        beamtalk_compiler_server:resolve_method_span(
            Source, <<"SpanServerC">>, <<"increment">>, instance
        ),
    ?assertEqual(Prev, binary:part(Source, Start, End - Start)).

start_compiler() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

stop_compiler(_) ->
    ok.

valid_core_erlang() ->
    <<
        "module 'test_server_mod' ['hello'/0]\n"
        "  attributes []\n"
        "  'hello'/0 = fun () -> 'world'\n"
        "end\n"
    >>.
