%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_nav_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_nav (BT-2239).

Covers the pure validation/serialisation surface that does not require a
running workspace: query construction + injection guard (build_query/2,
is_safe_arg/1) and the wire-shaping helpers (encode_site/1,
encode_implementor/1). The end-to-end eval path is exercised by the BT-2240
references-delegation work.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% build_query/2 — validation + expression construction
%%====================================================================

build_query_senders_test() ->
    ?assertEqual(
        {ok, "SystemNavigation default sendersOf: #increment", sites},
        beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<"increment">>)
    ).

build_query_implementors_test() ->
    ?assertEqual(
        {ok, "SystemNavigation default implementorsOf: #asString", implementors},
        beamtalk_repl_ops_nav:build_query(<<"implementorsOf">>, <<"asString">>)
    ).

build_query_references_uses_bareword_class_test() ->
    ?assertEqual(
        {ok, "SystemNavigation default referencesTo: Counter", sites},
        beamtalk_repl_ops_nav:build_query(<<"referencesTo">>, <<"Counter">>)
    ).

build_query_keyword_selector_test() ->
    ?assertEqual(
        {ok, "SystemNavigation default sendersOf: #at:put:", sites},
        beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<"at:put:">>)
    ).

build_query_binary_selector_test() ->
    ?assertEqual(
        {ok, "SystemNavigation default sendersOf: #+", sites},
        beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<"+">>)
    ).

build_query_unknown_kind_errors_test() ->
    ?assertMatch(
        {error, _}, beamtalk_repl_ops_nav:build_query(<<"bogusKind">>, <<"foo">>)
    ).

build_query_empty_arg_errors_test() ->
    ?assertMatch({error, _}, beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<>>)),
    ?assertMatch({error, _}, beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, undefined)).

build_query_unsafe_arg_errors_test() ->
    %% A space, quote, or paren must be rejected so the arg cannot break out of
    %% the evaluated expression.
    ?assertMatch({error, _}, beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<"a b">>)),
    ?assertMatch({error, _}, beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<"x\"y">>)),
    ?assertMatch({error, _}, beamtalk_repl_ops_nav:build_query(<<"sendersOf">>, <<"x)">>)).

%%====================================================================
%% is_safe_arg/1
%%====================================================================

is_safe_arg_accepts_selectors_and_classes_test() ->
    ?assert(beamtalk_repl_ops_nav:is_safe_arg(<<"foo">>)),
    ?assert(beamtalk_repl_ops_nav:is_safe_arg(<<"at:put:">>)),
    ?assert(beamtalk_repl_ops_nav:is_safe_arg(<<"+">>)),
    ?assert(beamtalk_repl_ops_nav:is_safe_arg(<<"Counter">>)),
    %% Package-qualified class name (BT-1659).
    ?assert(beamtalk_repl_ops_nav:is_safe_arg(<<"json@Parser">>)).

is_safe_arg_rejects_breakout_chars_test() ->
    ?assertNot(beamtalk_repl_ops_nav:is_safe_arg(<<"a b">>)),
    ?assertNot(beamtalk_repl_ops_nav:is_safe_arg(<<"x\"y">>)),
    ?assertNot(beamtalk_repl_ops_nav:is_safe_arg(<<"x.y">>)),
    ?assertNot(beamtalk_repl_ops_nav:is_safe_arg(<<"(x)">>)).

%%====================================================================
%% encode_site/1 and encode_implementor/1 — wire shaping
%%====================================================================

encode_site_shapes_record_test() ->
    %% term_to_json passes binaries through, so a pre-resolved class name
    %% binary lets us assert the shape without constructing a class object.
    Site = #{class => <<"Counter">>, selector => increment, line => 7},
    ?assertEqual(
        #{<<"class">> => <<"Counter">>, <<"selector">> => <<"increment">>, <<"line">> => 7},
        beamtalk_repl_ops_nav:encode_site(Site)
    ).

encode_implementor_instance_side_test() ->
    ?assertEqual(
        #{<<"class">> => <<"Integer">>, <<"meta">> => false},
        beamtalk_repl_ops_nav:encode_implementor(<<"Integer">>)
    ).

encode_implementor_class_side_sets_meta_test() ->
    ?assertEqual(
        #{<<"class">> => <<"Counter class">>, <<"meta">> => true},
        beamtalk_repl_ops_nav:encode_implementor(<<"Counter class">>)
    ).
