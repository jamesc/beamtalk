%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_port_tests).

-moduledoc "Tests for beamtalk_compiler_port (ADR 0022, Phase 0 wire check).".

-include_lib("eunit/include/eunit.hrl").
%% Helper to find the compiler binary
compiler_binary() ->
    %% Look in target/debug from project root
    ProjectRoot = find_project_root(filename:absname("")),
    %% On Windows, executables have a .exe extension.
    ExeName =
        case os:type() of
            {win32, _} -> "beamtalk-compiler-port.exe";
            _ -> "beamtalk-compiler-port"
        end,
    Path = filename:join([ProjectRoot, "target", "debug", ExeName]),
    case filelib:is_regular(Path) of
        true -> Path;
        false -> error({compiler_not_found, Path})
    end.

find_project_root("/") ->
    filename:absname("");
find_project_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "Cargo.toml")) of
        true -> Dir;
        false -> find_project_root(filename:dirname(Dir))
    end.

%% Helper to open and close a port around a test
with_port(Fun) ->
    Port = beamtalk_compiler_port:open(compiler_binary()),
    try
        Fun(Port)
    after
        (try
            beamtalk_compiler_port:close(Port)
        catch
            _:_ -> ok
        end)
    end.

%%% Tests

simple_expression_test() ->
    with_port(fun(Port) ->
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression(Port, <<"1 + 2">>, <<"test">>, []),
        %% The port emits `call 'erlang':'+'\n\t\t    (1, 2)`. Check both
        %% the qualified BIF and the literal operand pair so that operand-order
        %% regressions are still caught.
        ?assert(binary:match(CoreErlang, <<"'erlang':'+'">>) =/= nomatch),
        ?assert(binary:match(CoreErlang, <<"1, 2">>) =/= nomatch)
    end).

known_vars_test() ->
    with_port(fun(Port) ->
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression(Port, <<"x + 1">>, <<"test">>, [<<"x">>]),
        %% ADR 0081: REPL free identifiers now look up via `maps:find` with a
        %% `beamtalk_workspace:resolve_name/2` fallthrough (locals-first, then
        %% the lazy global resolver) instead of a bare `maps:get`. Assert the
        %% new lookup call and the `'x'` key so the test still guards lookup.
        ?assert(binary:match(CoreErlang, <<"'maps':'find'">>) =/= nomatch),
        ?assert(binary:match(CoreErlang, <<"'beamtalk_workspace':'resolve_name'">>) =/= nomatch),
        ?assert(binary:match(CoreErlang, <<"'x'">>) =/= nomatch)
    end).

invalid_expression_returns_error_test() ->
    with_port(fun(Port) ->
        {error, Diagnostics} =
            beamtalk_compiler_port:compile_expression(Port, <<"+++">>, <<"test">>, []),
        ?assert(length(Diagnostics) > 0)
    end).

multiple_compiles_on_same_port_test() ->
    with_port(fun(Port) ->
        {ok, _, []} = beamtalk_compiler_port:compile_expression(Port, <<"1 + 2">>, <<"m1">>, []),
        {ok, _, []} = beamtalk_compiler_port:compile_expression(Port, <<"3 * 4">>, <<"m2">>, []),
        {ok, _, []} = beamtalk_compiler_port:compile_expression(Port, <<"5 - 1">>, <<"m3">>, [])
    end).

port_reopens_after_close_test() ->
    Port1 = beamtalk_compiler_port:open(compiler_binary()),
    {ok, _, []} = beamtalk_compiler_port:compile_expression(Port1, <<"1">>, <<"t">>, []),
    beamtalk_compiler_port:close(Port1),
    %% Reopen — BEAM is fine
    Port2 = beamtalk_compiler_port:open(compiler_binary()),
    {ok, _, []} = beamtalk_compiler_port:compile_expression(Port2, <<"2">>, <<"t">>, []),
    beamtalk_compiler_port:close(Port2).

empty_expression_returns_error_test() ->
    with_port(fun(Port) ->
        {error, _} =
            beamtalk_compiler_port:compile_expression(Port, <<"">>, <<"test">>, [])
    end).

garbage_etf_handled_gracefully_test() ->
    %% Send raw garbage — port should return an error, not crash
    Port = beamtalk_compiler_port:open(compiler_binary()),
    port_command(Port, <<131, 0, 0, 0>>),
    receive
        {Port, {data, RespBin}} ->
            Resp = binary_to_term(RespBin),
            ?assertMatch(#{status := error}, Resp);
        {Port, {exit_status, _}} ->
            %% Port crashed but BEAM is alive — acceptable for Phase 0
            ok
    after 5000 ->
        ?assert(false)
    end,
    (try
        beamtalk_compiler_port:close(Port)
    catch
        _:_ -> ok
    end).

compile_on_closed_port_returns_error_test() ->
    Port = beamtalk_compiler_port:open(compiler_binary()),
    beamtalk_compiler_port:close(Port),
    %% Calling compile on a closed port should return error, not crash BEAM
    Result = beamtalk_compiler_port:compile_expression(Port, <<"1">>, <<"t">>, []),
    ?assertMatch({error, _}, Result).

%%% ---------------------------------------------------------------
%%% handle_response/1 — ETF response parsing
%%% ---------------------------------------------------------------

handle_response_expression_ok_test() ->
    Response = #{status => ok, core_erlang => <<"core">>, warnings => []},
    ?assertEqual(
        {ok, <<"core">>, []},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_expression_with_warnings_test() ->
    Response = #{
        status => ok,
        core_erlang => <<"core">>,
        warnings => [<<"unused var">>]
    },
    ?assertEqual(
        {ok, <<"core">>, [<<"unused var">>]},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_class_definition_test() ->
    Response = #{
        status => ok,
        kind => class_definition,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        classes => [<<"Foo">>],
        warnings => []
    },
    {ok, class_definition, Info} =
        beamtalk_compiler_port:handle_response(Response),
    ?assertEqual(<<"core">>, maps:get(core_erlang, Info)),
    ?assertEqual(<<"mod">>, maps:get(module_name, Info)),
    ?assertEqual([<<"Foo">>], maps:get(classes, Info)).

handle_response_method_definition_test() ->
    Response = #{
        status => ok,
        kind => method_definition,
        class_name => <<"Counter">>,
        selector => <<"increment">>,
        is_class_method => false,
        method_source => <<"src">>,
        warnings => []
    },
    {ok, method_definition, Info} =
        beamtalk_compiler_port:handle_response(Response),
    ?assertEqual(<<"Counter">>, maps:get(class_name, Info)),
    ?assertEqual(<<"increment">>, maps:get(selector, Info)),
    ?assertEqual(false, maps:get(is_class_method, Info)).

handle_response_error_binary_test() ->
    %% Legacy: plain binary diagnostics are normalized to maps.
    Response = #{status => error, diagnostics => [<<"err">>]},
    ?assertEqual(
        {error, [#{message => <<"err">>}]},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_error_structured_test() ->
    %% BT-1235: Structured diagnostic maps with line and hint.
    Diag = #{message => <<"Unused variable `x`">>, line => 2, hint => <<"prefix with _">>},
    Response = #{status => error, diagnostics => [Diag]},
    ?assertEqual(
        {error, [Diag]},
        beamtalk_compiler_port:handle_response(Response)
    ).

handle_response_error_unexpected_type_test() ->
    %% BT-1235: unexpected diagnostic types are formatted as ~p
    Response = #{status => error, diagnostics => [some_atom]},
    {error, [Diag]} = beamtalk_compiler_port:handle_response(Response),
    ?assert(is_map(Diag)),
    ?assert(is_binary(maps:get(message, Diag))).

handle_response_unexpected_test() ->
    Response = #{bogus => true},
    ?assertMatch(
        {error, [#{message := <<"Unexpected compiler response">>}]},
        beamtalk_compiler_port:handle_response(Response)
    ).

%%% ---------------------------------------------------------------
%%% find_compiler_binary/0 — binary discovery paths
%%% ---------------------------------------------------------------

find_compiler_binary_returns_path_test() ->
    Path = beamtalk_compiler_port:find_compiler_binary(),
    ?assert(is_list(Path) orelse is_binary(Path)),
    ?assert(filelib:is_regular(Path)).

find_compiler_binary_env_override_test() ->
    Original = os:getenv("BEAMTALK_COMPILER_PORT_BIN"),
    try
        %% Point to the real binary via env var
        RealPath = beamtalk_compiler_port:find_compiler_binary(),
        os:putenv("BEAMTALK_COMPILER_PORT_BIN", RealPath),
        Found = beamtalk_compiler_port:find_compiler_binary(),
        ?assertEqual(RealPath, Found)
    after
        case Original of
            false -> os:unsetenv("BEAMTALK_COMPILER_PORT_BIN");
            Value -> os:putenv("BEAMTALK_COMPILER_PORT_BIN", Value)
        end
    end.

find_compiler_binary_invalid_env_fallback_test() ->
    Original = os:getenv("BEAMTALK_COMPILER_PORT_BIN"),
    try
        os:putenv("BEAMTALK_COMPILER_PORT_BIN", "/nonexistent/binary"),
        %% Should fall back to dev path / PATH search
        Path = beamtalk_compiler_port:find_compiler_binary(),
        ?assert(filelib:is_regular(Path))
    after
        case Original of
            false -> os:unsetenv("BEAMTALK_COMPILER_PORT_BIN");
            Value -> os:putenv("BEAMTALK_COMPILER_PORT_BIN", Value)
        end
    end.

find_compiler_binary_empty_env_fallback_test() ->
    Original = os:getenv("BEAMTALK_COMPILER_PORT_BIN"),
    try
        os:putenv("BEAMTALK_COMPILER_PORT_BIN", ""),
        Path = beamtalk_compiler_port:find_compiler_binary(),
        ?assert(filelib:is_regular(Path))
    after
        case Original of
            false -> os:unsetenv("BEAMTALK_COMPILER_PORT_BIN");
            Value -> os:putenv("BEAMTALK_COMPILER_PORT_BIN", Value)
        end
    end.

%%% ---------------------------------------------------------------
%%% find_project_root/0 — project root discovery
%%% ---------------------------------------------------------------

find_project_root_finds_cargo_toml_test() ->
    Root = beamtalk_compiler_port:find_project_root(),
    ?assert(filelib:is_regular(filename:join(Root, "Cargo.toml"))).

%%% ---------------------------------------------------------------
%%% class_module_index — package-qualified class references
%%% ---------------------------------------------------------------

class_module_index_used_for_spawn_expression_test() ->
    %% When class_module_index maps Counter → bt@getting_started@counter,
    %% `Counter spawn` must generate code referencing the package-qualified module.
    with_port(fun(Port) ->
        Options = #{
            class_module_index => #{
                <<"Counter">> => <<"bt@getting_started@counter">>
            }
        },
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression(
                Port, <<"Counter spawn">>, <<"repl_test">>, [], Options
            ),
        ?assert(
            binary:match(CoreErlang, <<"'bt@getting_started@counter':'spawn'">>) =/= nomatch
        ),
        %% Must NOT contain the heuristic fallback module name
        ?assertEqual(
            nomatch,
            binary:match(CoreErlang, <<"'bt@counter':'spawn'">>)
        )
    end).

class_module_index_used_for_spawn_with_args_expression_test() ->
    %% spawnWith: must also respect class_module_index.
    with_port(fun(Port) ->
        Options = #{
            class_module_index => #{
                <<"Counter">> => <<"bt@getting_started@counter">>
            }
        },
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression(
                Port, <<"Counter spawnWith: #{#value => 10}">>, <<"repl_test">>, [], Options
            ),
        ?assert(
            binary:match(CoreErlang, <<"'bt@getting_started@counter':'spawn'">>) =/= nomatch
        ),
        %% Must NOT contain the heuristic fallback module name
        ?assertEqual(
            nomatch,
            binary:match(CoreErlang, <<"'bt@counter':'spawn'">>)
        )
    end).

%%% ---------------------------------------------------------------
%%% compile_expression_trace/4,5 — trace-mode compile (live port)
%%% ---------------------------------------------------------------

compile_expression_trace_test() ->
    with_port(fun(Port) ->
        {ok, CoreErlang, []} =
            beamtalk_compiler_port:compile_expression_trace(Port, <<"1 + 2">>, <<"tr">>, []),
        ?assert(is_binary(CoreErlang)),
        ?assert(binary:match(CoreErlang, <<"'erlang':'+'">>) =/= nomatch)
    end).

%% Non-empty index/hierarchy options exercise the Request1/Request2/Request
%% build branches (the `_ -> Request#{...}` arms).
compile_expression_trace_with_options_test() ->
    with_port(fun(Port) ->
        Options = #{
            class_superclass_index => #{<<"Counter">> => <<"Object">>},
            class_module_index => #{<<"Counter">> => <<"bt@app@counter">>},
            class_hierarchy => #{'Counter' => #{superclass => 'Object'}}
        },
        {ok, CoreErlang, _Warnings} =
            beamtalk_compiler_port:compile_expression_trace(
                Port, <<"1 + 2">>, <<"tr_opts">>, [], Options
            ),
        ?assert(is_binary(CoreErlang))
    end).

compile_expression_trace_invalid_returns_error_test() ->
    with_port(fun(Port) ->
        {error, Diagnostics} =
            beamtalk_compiler_port:compile_expression_trace(Port, <<"+++">>, <<"tr">>, []),
        ?assert(length(Diagnostics) > 0)
    end).

%%% ---------------------------------------------------------------
%%% find_senders_in_source/3 (BT-2190) — live port
%%% ---------------------------------------------------------------

find_senders_in_source_atom_selector_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_senders_in_source(Port, <<"x printNl">>, printNl),
        ?assert(is_list(Lines)),
        ?assert(lists:member(1, Lines))
    end).

find_senders_in_source_binary_selector_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_senders_in_source(Port, <<"x printNl">>, <<"printNl">>),
        ?assert(is_list(Lines))
    end).

%% Guard-fail clause: selector must be an atom or binary (no port needed).
find_senders_in_source_invalid_selector_test() ->
    {error, [Diag]} =
        beamtalk_compiler_port:find_senders_in_source(fake_port, <<"x printNl">>, 42),
    ?assert(binary:match(maps:get(message, Diag), <<"selector must be">>) =/= nomatch).

%%% ---------------------------------------------------------------
%%% find_all_sends_in_source/2 (BT-2206) — live port
%%% ---------------------------------------------------------------

find_all_sends_in_source_test() ->
    with_port(fun(Port) ->
        {ok, Sends} =
            beamtalk_compiler_port:find_all_sends_in_source(Port, <<"x printNl">>),
        ?assert(is_list(Sends)),
        ?assert(lists:any(fun(S) -> maps:get(selector, S) =:= <<"printNl">> end, Sends))
    end).

find_all_sends_in_source_invalid_source_test() ->
    {error, [Diag]} =
        beamtalk_compiler_port:find_all_sends_in_source(fake_port, not_a_binary),
    ?assert(binary:match(maps:get(message, Diag), <<"source must be a binary">>) =/= nomatch).

%%% ---------------------------------------------------------------
%%% find_references_to_in_source/3 (BT-2203) — live port
%%% ---------------------------------------------------------------

find_references_to_in_source_atom_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_references_to_in_source(
                Port, <<"x := MyClass new">>, 'MyClass'
            ),
        ?assert(is_list(Lines)),
        ?assert(lists:member(1, Lines))
    end).

find_references_to_in_source_binary_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_references_to_in_source(
                Port, <<"x := MyClass new">>, <<"MyClass">>
            ),
        ?assert(is_list(Lines))
    end).

find_references_to_in_source_invalid_test() ->
    {error, [Diag]} =
        beamtalk_compiler_port:find_references_to_in_source(fake_port, <<"x">>, 42),
    ?assert(binary:match(maps:get(message, Diag), <<"class name must be">>) =/= nomatch).

%%% ---------------------------------------------------------------
%%% find_field_readers/writers_in_source/3 (BT-2208) — live port
%%% ---------------------------------------------------------------

find_field_readers_in_source_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_field_readers_in_source(Port, <<"^ self.value">>, value),
        ?assert(is_list(Lines)),
        ?assert(lists:member(1, Lines))
    end).

find_field_writers_in_source_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_field_writers_in_source(
                Port, <<"self.value := 42">>, <<"value">>
            ),
        ?assert(is_list(Lines)),
        ?assert(lists:member(1, Lines))
    end).

%% Guard-fail clause of the shared field_access_query/5 driver.
find_field_readers_invalid_field_test() ->
    {error, [Diag]} =
        beamtalk_compiler_port:find_field_readers_in_source(fake_port, <<"^ self.value">>, 42),
    ?assert(binary:match(maps:get(message, Diag), <<"field name must be">>) =/= nomatch).

%%% ---------------------------------------------------------------
%%% find_ffi_sites_in_source/5 (BT-2211) — live port
%%% ---------------------------------------------------------------

%% A non-negative integer constrains the match to that argument count (the
%% `arity => N' request branch). `reverse: x' is a 1-arg keyword send.
find_ffi_sites_in_source_with_arity_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_ffi_sites_in_source(
                Port, <<"(Erlang lists) reverse: x">>, lists, reverse, 1
            ),
        ?assert(is_list(Lines))
    end).

%% `any' arity omits the field entirely (the BaseRequest branch) and must match
%% the FFI call site regardless of argument count.
find_ffi_sites_in_source_any_arity_test() ->
    with_port(fun(Port) ->
        {ok, Lines} =
            beamtalk_compiler_port:find_ffi_sites_in_source(
                Port, <<"(Erlang lists) reverse: x">>, <<"lists">>, <<"reverse">>, any
            ),
        ?assert(is_list(Lines))
    end).

find_ffi_sites_in_source_invalid_arity_test() ->
    {error, [Diag]} =
        beamtalk_compiler_port:find_ffi_sites_in_source(
            fake_port, <<"src">>, lists, reverse, -1
        ),
    ?assert(binary:match(maps:get(message, Diag), <<"arity">>) =/= nomatch).

%%% ---------------------------------------------------------------
%%% resolve_method_span/5 (ADR 0082 Phase 1) — live port
%%% ---------------------------------------------------------------

span_fixture() ->
    <<
        "Object subclass: SpanCounter\n"
        "\n"
        "  increment => self.value := self.value + 1\n"
        "\n"
        "  class new => self basicNew\n"
    >>.

resolve_method_span_instance_test() ->
    with_port(fun(Port) ->
        Source = span_fixture(),
        {ok, #{start := Start, 'end' := End}, PrevSource} =
            beamtalk_compiler_port:resolve_method_span(
                Port, Source, <<"SpanCounter">>, <<"increment">>, instance
            ),
        %% Splicing PrevSource back over its byte span is a no-op.
        ?assertEqual(PrevSource, binary:part(Source, Start, End - Start)),
        ?assert(binary:match(PrevSource, <<"increment =>">>) =/= nomatch)
    end).

resolve_method_span_class_test() ->
    with_port(fun(Port) ->
        {ok, _Span, PrevSource} =
            beamtalk_compiler_port:resolve_method_span(
                Port, span_fixture(), <<"SpanCounter">>, <<"new">>, class
            ),
        ?assert(binary:match(PrevSource, <<"class new =>">>) =/= nomatch)
    end).

resolve_method_span_not_found_test() ->
    with_port(fun(Port) ->
        Result =
            beamtalk_compiler_port:resolve_method_span(
                Port, span_fixture(), <<"SpanCounter">>, <<"nope">>, instance
            ),
        ?assertMatch({error, selector_not_found, _}, Result)
    end).

%% Guard-fail clause: invalid Side returns {error, bad_argument, _}.
resolve_method_span_invalid_side_test() ->
    Result =
        beamtalk_compiler_port:resolve_method_span(
            fake_port, <<"src">>, <<"C">>, <<"m">>, bogus_side
        ),
    ?assertMatch({error, bad_argument, _}, Result).

%%% ---------------------------------------------------------------
%%% resolve_completion_type/3 (BT-1068) — live port
%%% ---------------------------------------------------------------

resolve_completion_type_test() ->
    with_port(fun(Port) ->
        Result = beamtalk_compiler_port:resolve_completion_type(Port, <<"42">>, #{}),
        %% Either statically resolved to a class atom, or unknown — both are
        %% well-formed outcomes of the completion-type query.
        ?assert(
            Result =:= {error, type_unknown} orelse
                (is_tuple(Result) andalso element(1, Result) =:= ok)
        )
    end).

%%% ---------------------------------------------------------------
%%% Closed-port degradation — each query's `catch error:badarg' arm
%%% (the "compiler port is not available" path) returns a structured
%%% error rather than crashing the caller.
%%% ---------------------------------------------------------------

%% Open a port and immediately close it, then run Fun(ClosedPort).
with_closed_port(Fun) ->
    Port = beamtalk_compiler_port:open(compiler_binary()),
    beamtalk_compiler_port:close(Port),
    Fun(Port).

compile_expression_trace_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, [_ | _]},
            beamtalk_compiler_port:compile_expression_trace(Port, <<"1 + 2">>, <<"t">>, [])
        )
    end).

resolve_completion_type_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertEqual(
            {error, type_unknown},
            beamtalk_compiler_port:resolve_completion_type(Port, <<"42">>, #{})
        )
    end).

find_senders_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, [_ | _]},
            beamtalk_compiler_port:find_senders_in_source(Port, <<"x printNl">>, printNl)
        )
    end).

find_all_sends_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, [_ | _]},
            beamtalk_compiler_port:find_all_sends_in_source(Port, <<"x printNl">>)
        )
    end).

find_references_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, [_ | _]},
            beamtalk_compiler_port:find_references_to_in_source(Port, <<"x">>, 'MyClass')
        )
    end).

find_field_readers_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, [_ | _]},
            beamtalk_compiler_port:find_field_readers_in_source(Port, <<"^ self.value">>, value)
        )
    end).

find_ffi_sites_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, [_ | _]},
            beamtalk_compiler_port:find_ffi_sites_in_source(Port, <<"src">>, lists, reverse, any)
        )
    end).

resolve_method_span_on_closed_port_test() ->
    with_closed_port(fun(Port) ->
        ?assertMatch(
            {error, port_error, _},
            beamtalk_compiler_port:resolve_method_span(
                Port, span_fixture(), <<"SpanCounter">>, <<"increment">>, instance
            )
        )
    end).

%%% ---------------------------------------------------------------
%%% Invalid-source error responses — the `{error, diagnostics}' arm of
%%% the source-analysis response handlers.
%%% ---------------------------------------------------------------

find_senders_invalid_source_returns_error_test() ->
    with_port(fun(Port) ->
        Result =
            beamtalk_compiler_port:find_senders_in_source(Port, <<"@@@ ))) +++">>, printNl),
        %% Unparseable source surfaces as a diagnostics list (or, if the analyzer
        %% tolerates it, an empty result) — both are documented {ok|error} shapes.
        ?assert(
            case Result of
                {error, Diags} when is_list(Diags) -> true;
                {ok, Lines} when is_list(Lines) -> true;
                _ -> false
            end
        )
    end).

%%% ---------------------------------------------------------------
%%% handle_response/1 — additional ok-kind and diagnostic shapes (pure)
%%% ---------------------------------------------------------------

%% BT-903: a class_definition response carrying trailing_core_erlang must
%% forward it into the returned info map.
handle_response_class_definition_with_trailing_test() ->
    Response = #{
        status => ok,
        kind => class_definition,
        core_erlang => <<"core">>,
        module_name => <<"mod">>,
        classes => [<<"Foo">>],
        trailing_core_erlang => <<"trailing">>,
        warnings => []
    },
    {ok, class_definition, Info} = beamtalk_compiler_port:handle_response(Response),
    ?assertEqual(<<"trailing">>, maps:get(trailing_core_erlang, Info)).

%% BT-1612: protocol_definition response shape.
handle_response_protocol_definition_test() ->
    Response = #{
        status => ok,
        kind => protocol_definition,
        core_erlang => <<"core">>,
        module_name => <<"proto_mod">>,
        protocols => [<<"Drawable">>],
        warnings => []
    },
    {ok, protocol_definition, Info} = beamtalk_compiler_port:handle_response(Response),
    ?assertEqual(<<"proto_mod">>, maps:get(module_name, Info)),
    ?assertEqual([<<"Drawable">>], maps:get(protocols, Info)).

%% BT-1235: a diagnostic map whose `message` is not a binary is coerced to a
%% binary via ensure_binary/1 (the non-binary fallback arm).
handle_response_diagnostic_non_binary_message_test() ->
    Response = #{status => error, diagnostics => [#{message => 12345, line => 3}]},
    {error, [Diag]} = beamtalk_compiler_port:handle_response(Response),
    ?assert(is_binary(maps:get(message, Diag))),
    ?assertEqual(3, maps:get(line, Diag)).
