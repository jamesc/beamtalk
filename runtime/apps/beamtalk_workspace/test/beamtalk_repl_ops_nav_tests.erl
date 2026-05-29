%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_nav_tests).

%%% **DDD Context:** REPL Session Context (Navigation bridge)

-moduledoc """
EUnit tests for beamtalk_repl_ops_nav (BT-2311, BT-2309).

Covers: describe_ops/0 map shape, all validate_params/1 error branches,
success paths via handle/4 for senders, implementors, and references
queries against a live beamtalk_xref server seeded with a minimal fixture,
line-field population from xref data, reference-row shape/ownership, and
atom-safety sentinel invariants under selector/class floods.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% Construct a minimal non-legacy protocol_msg record.
%% The tuple form mirrors #protocol_msg{op, id, session, params, legacy}.
make_msg() ->
    {protocol_msg, <<"nav-query">>, <<"t1">>, <<"s1">>, #{}, false}.

%% Decode a JSON binary and assert it represents an error response.
assert_error_response(Response) ->
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>, <<"error">>]}, Decoded),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    Decoded.

%%====================================================================
%% describe_ops/0
%%====================================================================

describe_ops_returns_map_with_nav_query_key_test() ->
    Ops = beamtalk_repl_ops_nav:describe_ops(),
    ?assertMatch(#{<<"nav-query">> := _}, Ops).

describe_ops_nav_query_requires_kind_param_test() ->
    #{<<"nav-query">> := Info} = beamtalk_repl_ops_nav:describe_ops(),
    ?assertEqual([<<"kind">>], maps:get(<<"params">>, Info)).

describe_ops_nav_query_optional_includes_selector_and_class_test() ->
    #{<<"nav-query">> := Info} = beamtalk_repl_ops_nav:describe_ops(),
    Optional = maps:get(<<"optional">>, Info),
    ?assert(lists:member(<<"selector">>, Optional)),
    ?assert(lists:member(<<"class">>, Optional)).

%%====================================================================
%% handle/4 — validation error paths (no xref dependency)
%%====================================================================

handle_missing_kind_returns_error_mentioning_kind_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(<<"nav-query">>, #{}, Msg, self()),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"kind">>)).

handle_unknown_kind_returns_error_mentioning_the_value_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"bogus">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"bogus">>)).

handle_kind_not_a_string_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => 42}, Msg, self()
    ),
    assert_error_response(Response).

handle_kind_as_map_returns_error_test() ->
    %% Non-binary kind variants other than numbers — JSON-shaped objects also
    %% hit the catch-all clause in validate_params/1.
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => #{<<"nested">> => <<"value">>}}, Msg, self()
    ),
    assert_error_response(Response).

handle_kind_as_list_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => [<<"senders">>]}, Msg, self()
    ),
    assert_error_response(Response).

handle_senders_missing_selector_returns_error_mentioning_selector_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"senders">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"selector">>)).

handle_senders_empty_selector_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"senders">>, <<"selector">> => <<>>},
        Msg,
        self()
    ),
    assert_error_response(Response).

handle_implementors_missing_selector_returns_error_mentioning_selector_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"implementors">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"selector">>)).

handle_implementors_empty_selector_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"implementors">>, <<"selector">> => <<>>},
        Msg,
        self()
    ),
    assert_error_response(Response).

handle_references_missing_class_returns_error_mentioning_class_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"references">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"class">>)).

handle_references_empty_class_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"references">>, <<"class">> => <<>>},
        Msg,
        self()
    ),
    assert_error_response(Response).

%%====================================================================
%% handle/4 — success paths (require a live beamtalk_xref)
%%====================================================================

xref_setup() ->
    Pid =
        case whereis(beamtalk_xref) of
            undefined ->
                {ok, P} = beamtalk_xref:start_link(),
                P;
            P ->
                P
        end,
    %% Start each test from a clean slate to avoid bleed from other suites.
    clear_xref_tables(),
    Pid.

xref_cleanup(_Pid) ->
    clear_xref_tables(),
    ok.

clear_xref_tables() ->
    try
        sys:replace_state(beamtalk_xref, fun(S) ->
            ets:delete_all_objects(beamtalk_xref_methods),
            ets:delete_all_objects(beamtalk_xref_senders),
            ets:delete_all_objects(beamtalk_xref_references),
            ets:delete_all_objects(xref_class_gen),
            S
        end)
    catch
        _:_ -> ok
    end.

nav_query_test_() ->
    {setup, fun xref_setup/0, fun xref_cleanup/1, fun nav_tests/1}.

nav_tests(_Pid) ->
    [
        {"senders of non-existent atom returns empty sites list", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            %% Atom does not exist in VM → binary_to_existing_atom catches badarg
            %% → uses '__nav_query_unknown__' sentinel → senders returns []
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"__does_not_exist_nav__">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
            ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
        end},
        {"senders of known selector returns non-empty sites list", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            %% 'increment' sends '+', so senders_of('+') should return a site
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"+">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
            #{<<"value">> := #{<<"sites">> := Sites}} = Decoded,
            ?assertNotEqual([], Sites)
        end},
        {"site row has all required fields", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"+">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assert(maps:is_key(<<"class">>, Row)),
            ?assert(maps:is_key(<<"class_side">>, Row)),
            ?assert(maps:is_key(<<"method">>, Row)),
            ?assert(maps:is_key(<<"line">>, Row)),
            ?assert(maps:is_key(<<"source_file">>, Row))
        end},
        {"site row class field matches registered class name", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"+">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assertEqual(<<"NavTestClass">>, maps:get(<<"class">>, Row))
        end},
        {"implementors of known selector returns non-empty list", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"implementors">>, <<"selector">> => <<"increment">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
            #{<<"value">> := #{<<"sites">> := Sites}} = Decoded,
            ?assertNotEqual([], Sites)
        end},
        {"implementors of unknown selector returns empty list", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"implementors">>, <<"selector">> => <<"__no_such_method__">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
        end},
        {"implementor row has all required fields", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"implementors">>, <<"selector">> => <<"increment">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assert(maps:is_key(<<"class">>, Row)),
            ?assert(maps:is_key(<<"class_side">>, Row)),
            ?assert(maps:is_key(<<"method">>, Row)),
            ?assert(maps:is_key(<<"line">>, Row)),
            ?assert(maps:is_key(<<"source_file">>, Row))
        end},
        {"implementor row method field matches queried selector", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"implementors">>, <<"selector">> => <<"increment">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assertEqual(<<"increment">>, maps:get(<<"method">>, Row))
        end},
        {"references to known class returns non-empty sites list", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            %% nav_test_xref references 'Integer' in increment method
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"references">>, <<"class">> => <<"Integer">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
            #{<<"value">> := #{<<"sites">> := Sites}} = Decoded,
            ?assertNotEqual([], Sites)
        end},
        {"references to non-existent class returns empty sites list", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"references">>, <<"class">> => <<"__no_such_class__">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
        end},
        {"source_file field is null when class has no backing process", fun() ->
            %% NavTestClass is registered in xref only, not in the class registry,
            %% so source_file_of/1 returns null for it.
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"+">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assertEqual(null, maps:get(<<"source_file">>, Row))
        end},
        {"senders row line field is populated from xref `sends` line", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"+">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            %% nav_test_xref records the '+' send on line 8.
            ?assertEqual(8, maps:get(<<"line">>, Row))
        end},
        {"implementor row line field is populated from xref method_info", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"implementors">>, <<"selector">> => <<"increment">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            %% nav_test_xref records the 'increment' method header on line 7.
            ?assertEqual(7, maps:get(<<"line">>, Row))
        end},
        {"references row has all required fields", fun() ->
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"references">>, <<"class">> => <<"Integer">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assert(maps:is_key(<<"class">>, Row)),
            ?assert(maps:is_key(<<"class_side">>, Row)),
            ?assert(maps:is_key(<<"method">>, Row)),
            ?assert(maps:is_key(<<"line">>, Row)),
            ?assert(maps:is_key(<<"source_file">>, Row))
        end},
        {"references row class field is the owner of the reference site", fun() ->
            %% references_to/1 returns sites whose `owner` is the referencing
            %% class — here NavTestClass references Integer, so the row's
            %% `class` should be NavTestClass (not Integer).
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"references">>, <<"class">> => <<"Integer">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assertEqual(<<"NavTestClass">>, maps:get(<<"class">>, Row))
        end},
        {"atom table stable under flood of unknown selectors", fun() ->
            %% Atom-safety sentinel (BT-2239 follow-up): a flood of unique,
            %% never-seen-before selectors must not grow the VM atom table,
            %% because the handler uses binary_to_existing_atom/2 and falls
            %% back to a single shared sentinel atom on badarg.
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            %% Warm up: ensure all involved modules and code paths are loaded
            %% before measuring, so lazy-loading atoms don't skew the baseline.
            _ = code:ensure_loaded(beamtalk_repl_ops_nav),
            _ = code:ensure_loaded(json),
            _ = json:decode(beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"senders">>, <<"selector">> => <<"warmup">>},
                Msg,
                self()
            )),
            Before = erlang:system_info(atom_count),
            lists:foreach(
                fun(N) ->
                    Bin = iolist_to_binary([
                        <<"__nav_query_flood_">>,
                        integer_to_binary(N),
                        <<"_">>,
                        integer_to_binary(erlang:unique_integer([positive]))
                    ]),
                    Response = beamtalk_repl_ops_nav:handle(
                        <<"nav-query">>,
                        #{<<"kind">> => <<"senders">>, <<"selector">> => Bin},
                        Msg,
                        self()
                    ),
                    Decoded = json:decode(Response),
                    ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
                end,
                lists:seq(1, 100)
            ),
            After = erlang:system_info(atom_count),
            ?assertEqual(Before, After)
        end},
        {"atom table stable under flood of unknown classes", fun() ->
            %% Same invariant for the `references` kind, which routes through
            %% with_class/1 → binary_to_existing_atom/2.
            ok = beamtalk_xref:register_class('NavTestClass', nav_test_xref()),
            Msg = make_msg(),
            %% Warm up: ensure all involved modules and code paths are loaded
            %% before measuring, so lazy-loading atoms don't skew the baseline.
            _ = code:ensure_loaded(beamtalk_repl_ops_nav),
            _ = code:ensure_loaded(json),
            _ = json:decode(beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"references">>, <<"class">> => <<"WarmupClass">>},
                Msg,
                self()
            )),
            Before = erlang:system_info(atom_count),
            lists:foreach(
                fun(N) ->
                    Bin = iolist_to_binary([
                        <<"__NavQueryFloodClass_">>,
                        integer_to_binary(N),
                        <<"_">>,
                        integer_to_binary(erlang:unique_integer([positive]))
                    ]),
                    Response = beamtalk_repl_ops_nav:handle(
                        <<"nav-query">>,
                        #{<<"kind">> => <<"references">>, <<"class">> => Bin},
                        Msg,
                        self()
                    ),
                    Decoded = json:decode(Response),
                    ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
                end,
                lists:seq(1, 100)
            ),
            After = erlang:system_info(atom_count),
            ?assertEqual(Before, After)
        end}
    ].

%%====================================================================
%% Fixtures
%%====================================================================

%% Minimal xref payload: one instance-side method 'increment' that sends '+'
%% and references 'Integer'. Mirrors the shape codegen will emit (BT-2239).
nav_test_xref() ->
    [
        #{
            class_side => false,
            selector => 'increment',
            line => 7,
            sends => [
                #{selector => '+', line => 8, recv_kind => self_recv}
            ],
            references => [
                #{class => 'Integer', line => 7}
            ],
            source_status => indexed,
            provenance => class_body
        }
    ].
