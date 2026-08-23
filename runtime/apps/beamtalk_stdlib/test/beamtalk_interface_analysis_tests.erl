%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interface_analysis_tests).

%%% **DDD Context:** Object System Context

-moduledoc ""
"\n"
"EUnit tests for beamtalk_interface: erlangHelp paths and source-analysis\n"
"degradation paths (BT-2248).\n"
"\n"
"Tests cover:\n"
"- erlangHelp/1,2 and dispatch('erlangHelp:', ...) — happy path and error paths\n"
"- findSendersIn/2 — type guard and compiler-unavailable degradation\n"
"- allSendsIn/1   — type guard and compiler-unavailable degradation\n"
"- findReferencesToIn/2 — type guard and compiler-unavailable degradation\n"
"- findFieldReadersIn/2 / findFieldWritersIn/2 — type guard and degradation\n"
"- ffiSitesIn/4 — type guard and compiler-unavailable degradation\n"
"- log_compiler_diagnostics/2 — port-unavailable (ERROR) and parse-warn (WARNING) paths\n"
"".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Fake Self — BeamtalkInterface primitives ignore Self entirely.
fake_self() ->
    {beamtalk_object, 'BeamtalkInterface class', 'bt@stdlib@beamtalk_interface', self()}.

with_compiler_stopped(Fun) ->
    WasStarted = lists:keymember(beamtalk_compiler, 1, application:which_applications()),
    stop_compiler_if_started(WasStarted),
    try
        Fun()
    after
        start_compiler_if_was_started(WasStarted)
    end.

stop_compiler_if_started(true) ->
    case application:stop(beamtalk_compiler) of
        ok -> ok;
        {error, {not_started, beamtalk_compiler}} -> ok
    end;
stop_compiler_if_started(false) ->
    ok.

start_compiler_if_was_started(true) ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end;
start_compiler_if_was_started(false) ->
    ok.

%%====================================================================
%% erlangHelp: dispatch — happy path
%%====================================================================

erlang_help_dispatch_known_module_test() ->
    Result = beamtalk_interface:dispatch('erlangHelp:', [<<"lists">>], fake_self()),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"lists">>)).

erlang_help_dispatch_known_module_selector_atom_test() ->
    Result = beamtalk_interface:dispatch(
        'erlangHelp:selector:', [<<"lists">>, reverse], fake_self()
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"reverse">>)).

erlang_help_dispatch_known_module_selector_binary_test() ->
    Result = beamtalk_interface:dispatch(
        'erlangHelp:selector:', [<<"lists">>, <<"reverse">>], fake_self()
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"reverse">>)).

%%====================================================================
%% erlangHelp: dispatch — error paths
%%====================================================================

erlang_help_dispatch_non_binary_module_raises_type_error_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:', [42], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

erlang_help_dispatch_unknown_module_raises_not_found_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:', [<<"no_such_module_xyz99">>], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(not_found, Err#beamtalk_error.kind)
    end.

erlang_help_selector_dispatch_non_binary_module_raises_type_error_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:selector:', [42, reverse], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

erlang_help_selector_dispatch_integer_selector_raises_type_error_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:selector:', [<<"lists">>, 42], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% erlangHelp/1 direct export
%%====================================================================

erlang_help_1_known_module_test() ->
    Result = beamtalk_interface:erlangHelp(<<"lists">>),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"lists">>)).

erlang_help_1_non_binary_raises_type_error_test() ->
    try
        beamtalk_interface:erlangHelp(lists),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

erlang_help_1_unknown_module_raises_not_found_test() ->
    try
        beamtalk_interface:erlangHelp(<<"no_such_module_xyz99">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(not_found, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% erlangHelp/2 direct export
%%====================================================================

erlang_help_2_atom_selector_test() ->
    Result = beamtalk_interface:erlangHelp(<<"lists">>, reverse),
    ?assert(is_binary(Result)).

erlang_help_2_binary_selector_test() ->
    Result = beamtalk_interface:erlangHelp(<<"lists">>, <<"reverse">>),
    ?assert(is_binary(Result)).

erlang_help_2_non_binary_module_raises_type_error_test() ->
    try
        beamtalk_interface:erlangHelp(42, reverse),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

erlang_help_2_integer_selector_raises_type_error_test() ->
    try
        beamtalk_interface:erlangHelp(<<"lists">>, 42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

erlang_help_2_unknown_function_raises_not_found_test() ->
    try
        beamtalk_interface:erlangHelp(<<"lists">>, no_such_function_xyz),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(not_found, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% findSendersIn/2 — compiler-unavailable degradation
%%
%% When beamtalk_compiler_server is not running (as in unit-test context),
%% beamtalk_compiler_server catches the noproc exit and returns
%% {error, [#{message => <<"Compiler server is not available">>}]}.
%% beamtalk_interface:findSendersIn/2 must then log and return [].
%%====================================================================

find_senders_in_binary_source_atom_selector_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findSendersIn(<<"x printNl">>, send),
        ?assertEqual([], Result)
    end).

find_senders_in_binary_source_binary_selector_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findSendersIn(<<"x printNl">>, <<"send">>),
        ?assertEqual([], Result)
    end).

find_senders_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findSendersIn(42, <<"sel">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

find_senders_in_integer_selector_raises_type_error_test() ->
    try
        beamtalk_interface:findSendersIn(<<"source">>, 42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% allSendsIn/1 — compiler-unavailable degradation
%%====================================================================

all_sends_in_binary_source_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:allSendsIn(<<"x printNl">>),
        ?assertEqual([], Result)
    end).

all_sends_in_non_binary_raises_type_error_test() ->
    try
        beamtalk_interface:allSendsIn(42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

%%====================================================================
%% findReferencesToIn/2 — compiler-unavailable degradation
%%====================================================================

find_references_to_in_atom_class_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findReferencesToIn(<<"x := MyClass new">>, 'MyClass'),
        ?assertEqual([], Result)
    end).

find_references_to_in_binary_class_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findReferencesToIn(<<"x := MyClass new">>, <<"MyClass">>),
        ?assertEqual([], Result)
    end).

find_references_to_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findReferencesToIn(42, <<"MyClass">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% findFieldReadersIn/2 — compiler-unavailable degradation
%%====================================================================

find_field_readers_in_atom_field_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findFieldReadersIn(<<"^ self.value">>, value),
        ?assertEqual([], Result)
    end).

find_field_readers_in_binary_field_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findFieldReadersIn(<<"^ self.value">>, <<"value">>),
        ?assertEqual([], Result)
    end).

find_field_readers_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findFieldReadersIn(42, <<"field">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% findFieldWritersIn/2 — compiler-unavailable degradation
%%====================================================================

find_field_writers_in_atom_field_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findFieldWritersIn(<<"self.value := 42">>, value),
        ?assertEqual([], Result)
    end).

find_field_writers_in_binary_field_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:findFieldWritersIn(<<"self.value := 42">>, <<"value">>),
        ?assertEqual([], Result)
    end).

find_field_writers_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findFieldWritersIn(42, <<"field">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% ffiSitesIn/4 — compiler-unavailable degradation
%%====================================================================

ffi_sites_in_integer_arity_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:ffiSitesIn(<<"(Erlang lists) reverse: x">>, lists, reverse, 1),
        ?assertEqual([], Result)
    end).

ffi_sites_in_binary_any_arity_degrades_to_empty_test() ->
    with_compiler_stopped(fun() ->
        Result = beamtalk_interface:ffiSitesIn(
            <<"(Erlang lists) reverse: x">>, <<"lists">>, <<"reverse">>, any
        ),
        ?assertEqual([], Result)
    end).

ffi_sites_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:ffiSitesIn(42, lists, reverse, 1),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

ffi_sites_in_negative_arity_raises_type_error_test() ->
    try
        beamtalk_interface:ffiSitesIn(<<"source">>, lists, reverse, -1),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% log_compiler_diagnostics/2 — TEST export (BT-2219)
%%
%% Verifies the function returns ok without crashing on all diagnostic shapes.
%% The actual log level (ERROR vs WARNING) is not asserted — we verify
%% classification by confirming the call completes without exception.
%%====================================================================

log_compiler_diagnostics_port_unavailable_returns_ok_test() ->
    Diag = #{message => <<"Compiler server is not available">>},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_timed_out_returns_ok_test() ->
    Diag = #{message => <<"Compiler server timed out">>},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_parse_warning_returns_ok_test() ->
    Diag = #{message => <<"parse error on line 3">>},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_empty_list_returns_ok_test() ->
    Result = beamtalk_interface:log_compiler_diagnostics([], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_multiple_diagnostics_returns_ok_test() ->
    Diags = [
        #{message => <<"parse error">>},
        #{message => <<"undefined variable x">>}
    ],
    Result = beamtalk_interface:log_compiler_diagnostics(Diags, 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_non_binary_message_returns_ok_test() ->
    Diag = #{message => some_atom},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

%%====================================================================
%% instance_selectors/1, validated_own_categories/2, method_sig_line/2
%% (BT-3239) — grouped `Instance methods:` rendering for `:help`/`docs`.
%%====================================================================

instance_selectors_filters_to_instance_side_test() ->
    Methods = [
        #{selector => <<"foo">>, side => instance},
        #{selector => <<"new">>, side => class},
        #{selector => <<"bar">>, side => instance}
    ],
    ?assertEqual([foo, bar], beamtalk_interface:instance_selectors(Methods)).

instance_selectors_drops_unknown_atom_selector_test() ->
    %% A selector that isn't a live atom (shouldn't happen in practice —
    %% every selector comes from the class's own compiled method table) is
    %% dropped rather than crashing.
    Methods = [
        #{selector => <<"foo">>, side => instance},
        #{selector => <<"bt3239_definitely_not_an_existing_atom_xyz">>, side => instance}
    ],
    ?assertEqual([foo], beamtalk_interface:instance_selectors(Methods)).

validated_own_categories_groups_when_selectors_match_test() ->
    Categories = [
        #{name => <<"Construction">>, methods => [#{selector => <<"foo">>, side => instance}]},
        #{name => <<"Arithmetic">>, methods => [#{selector => <<"bar">>, side => instance}]}
    ],
    SealedMap = #{foo => false, bar => false},
    ?assertEqual(
        [{<<"Construction">>, [foo]}, {<<"Arithmetic">>, [bar]}],
        beamtalk_interface:validated_own_categories(Categories, SealedMap)
    ).

validated_own_categories_drops_class_side_methods_test() ->
    %% `format_class_help/2`'s "Instance methods:" section is instance-only
    %% (BT-3087 `collect_flattened_methods` never walks class-side) —
    %% grouping must preserve that scope even though the Rust categorizer
    %% interleaves both sides.
    Categories = [
        #{
            name => <<"Construction">>,
            methods => [
                #{selector => <<"new">>, side => class},
                #{selector => <<"foo">>, side => instance}
            ]
        }
    ],
    SealedMap = #{foo => false},
    ?assertEqual(
        [{<<"Construction">>, [foo]}],
        beamtalk_interface:validated_own_categories(Categories, SealedMap)
    ).

validated_own_categories_no_dividers_falls_back_to_undefined_test() ->
    %% A single implicit, unnamed category (no dividers in the source at
    %% all) must render exactly as it did before BT-3239 — no lone
    %% "(uncategorized)" header.
    Categories = [
        #{methods => [#{selector => <<"foo">>, side => instance}]}
    ],
    SealedMap = #{foo => false},
    ?assertEqual(
        undefined,
        beamtalk_interface:validated_own_categories(Categories, SealedMap)
    ).

validated_own_categories_single_named_category_is_kept_test() ->
    %% Unlike the no-dividers case, a *single real* divider is still real
    %% grouping information and should render with its header.
    Categories = [
        #{name => <<"OnlySection">>, methods => [#{selector => <<"foo">>, side => instance}]}
    ],
    SealedMap = #{foo => false},
    ?assertEqual(
        [{<<"OnlySection">>, [foo]}],
        beamtalk_interface:validated_own_categories(Categories, SealedMap)
    ).

validated_own_categories_selector_mismatch_falls_back_to_undefined_test() ->
    %% The on-disk source and the live image have diverged (e.g. an
    %% unflushed `>>` patch) — a partial grouped view could silently hide a
    %% method, so this must degrade to the always-correct flat rendering.
    Categories = [
        #{name => <<"Section">>, methods => [#{selector => <<"foo">>, side => instance}]}
    ],
    SealedMap = #{foo => false, bar => false},
    ?assertEqual(
        undefined,
        beamtalk_interface:validated_own_categories(Categories, SealedMap)
    ).

validated_own_categories_empty_category_is_omitted_test() ->
    %% A category with no instance-side methods left after filtering (e.g.
    %% it only had class-side methods) contributes no header at all.
    Categories = [
        #{name => <<"ClassSideOnly">>, methods => [#{selector => <<"new">>, side => class}]},
        #{name => <<"Real">>, methods => [#{selector => <<"foo">>, side => instance}]}
    ],
    SealedMap = #{foo => false},
    ?assertEqual(
        [{<<"Real">>, [foo]}],
        beamtalk_interface:validated_own_categories(Categories, SealedMap)
    ).

method_sig_line_sealed_appends_suffix_test() ->
    ?assertEqual(
        <<"foo [sealed]">>, beamtalk_interface:method_sig_line(<<"foo">>, true)
    ).

method_sig_line_unsealed_is_unchanged_test() ->
    ?assertEqual(<<"foo">>, beamtalk_interface:method_sig_line(<<"foo">>, false)).

%%====================================================================
%% render_own_methods_part/2 (BT-3239) — the actual `Instance methods:`
%% string `format_class_help/2` emits, deterministic and independent of a
%% live class or compiler-port round trip (a dropped indentation prefix in
%% either branch would fail one of these directly, unlike a live end-to-end
%% check that depends on the eunit environment's stdlib .beam artifacts
%% actually carrying a `beamtalk_source` attribute, which they may not).
%%====================================================================

render_own_methods_part_empty_own_docs_is_empty_test() ->
    ?assertEqual(<<>>, beamtalk_interface:render_own_methods_part([], undefined)).

render_own_methods_part_flat_indents_every_line_test() ->
    OwnDocs = [{bar, <<"bar">>, false}, {foo, <<"foo">>, false}],
    Result = beamtalk_interface:render_own_methods_part(OwnDocs, undefined),
    ?assertEqual(<<"\nInstance methods:\n  bar\n  foo">>, Result).

render_own_methods_part_flat_sealed_suffix_is_indented_too_test() ->
    %% Regression guard (BT-3239 review): the flat-fallback branch must
    %% indent every rendered line, sealed or not — a prior version of this
    %% function rendered the flat branch flush at column 0.
    OwnDocs = [{foo, <<"foo">>, true}],
    Result = beamtalk_interface:render_own_methods_part(OwnDocs, undefined),
    ?assertEqual(<<"\nInstance methods:\n  foo [sealed]">>, Result),
    ?assertEqual(nomatch, binary:match(Result, <<"\nfoo">>)).

render_own_methods_part_grouped_indents_header_and_methods_test() ->
    OwnDocs = [{bar, <<"bar">>, false}, {foo, <<"foo">>, false}],
    Categories = [{<<"Section">>, [foo, bar]}],
    Result = beamtalk_interface:render_own_methods_part(OwnDocs, Categories),
    ?assertEqual(
        <<"\nInstance methods:\n  === Section ===\n  foo\n  bar">>, Result
    ).

render_own_methods_part_grouped_uncategorized_leading_bucket_test() ->
    OwnDocs = [{bar, <<"bar">>, false}, {foo, <<"foo">>, false}],
    Categories = [{undefined, [foo]}, {<<"Section">>, [bar]}],
    Result = beamtalk_interface:render_own_methods_part(OwnDocs, Categories),
    ?assertEqual(
        <<"\nInstance methods:\n  (uncategorized)\n  foo\n  === Section ===\n  bar">>,
        Result
    ).

render_own_methods_part_grouped_sealed_method_is_indented_test() ->
    OwnDocs = [{foo, <<"foo">>, true}],
    Categories = [{<<"Section">>, [foo]}],
    Result = beamtalk_interface:render_own_methods_part(OwnDocs, Categories),
    ?assertEqual(<<"\nInstance methods:\n  === Section ===\n  foo [sealed]">>, Result).
