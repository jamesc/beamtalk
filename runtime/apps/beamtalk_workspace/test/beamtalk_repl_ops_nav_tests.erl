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
    ?assert(lists:member(<<"class">>, Optional)),
    %% BT-2669: callers_of_native_module takes a `module` param.
    ?assert(lists:member(<<"module">>, Optional)).

%%====================================================================
%% handle/4 — callers_of_native_module validation (BT-2669)
%%====================================================================

handle_callers_missing_module_returns_error_mentioning_module_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"callers_of_native_module">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"module">>)).

handle_callers_empty_module_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"callers_of_native_module">>, <<"module">> => <<>>},
        Msg,
        self()
    ),
    assert_error_response(Response).

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
%% handle/4 — protocol kinds validation (BT-2639)
%%====================================================================

handle_required_methods_missing_class_returns_error_mentioning_class_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"required_methods">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"class">>)).

handle_conforming_classes_missing_class_returns_error_mentioning_class_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>, #{<<"kind">> => <<"conforming_classes">>}, Msg, self()
    ),
    Decoded = assert_error_response(Response),
    ErrBin = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrBin, <<"class">>)).

handle_required_methods_empty_class_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"required_methods">>, <<"class">> => <<>>},
        Msg,
        self()
    ),
    assert_error_response(Response).

handle_conforming_classes_empty_class_returns_error_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"conforming_classes">>, <<"class">> => <<>>},
        Msg,
        self()
    ),
    assert_error_response(Response).

%% An unknown protocol name resolves to an empty site list (not a validation
%% error), mirroring senders/implementors degradation for unknown selectors.
handle_required_methods_unknown_protocol_returns_empty_sites_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"required_methods">>, <<"class">> => <<"__no_such_protocol_nav__">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
    ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded).

handle_conforming_classes_unknown_protocol_returns_empty_sites_test() ->
    Msg = make_msg(),
    Response = beamtalk_repl_ops_nav:handle(
        <<"nav-query">>,
        #{<<"kind">> => <<"conforming_classes">>, <<"class">> => <<"__no_such_protocol_nav__">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
    ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded).

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
            _ =
                json:decode(
                    beamtalk_repl_ops_nav:handle(
                        <<"nav-query">>,
                        #{<<"kind">> => <<"senders">>, <<"selector">> => <<"warmup">>},
                        Msg,
                        self()
                    )
                ),
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
        {"callers_of_native_module returns the calling sites", fun() ->
            ok = beamtalk_xref:register_class('NavFfiCaller', nav_ffi_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"callers_of_native_module">>, <<"module">> => <<"lists">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
            #{<<"value">> := #{<<"sites">> := [Row | _] = Sites}} = Decoded,
            ?assertNotEqual([], Sites),
            ?assertEqual(<<"NavFfiCaller">>, maps:get(<<"class">>, Row)),
            ?assertEqual(<<"reverseList:">>, maps:get(<<"method">>, Row)),
            ?assert(maps:is_key(<<"class_side">>, Row)),
            ?assert(maps:is_key(<<"line">>, Row)),
            ?assert(maps:is_key(<<"source_file">>, Row))
        end},
        {"callers_of_native_module is empty for a module with no callers", fun() ->
            ok = beamtalk_xref:register_class('NavFfiCaller', nav_ffi_xref()),
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"callers_of_native_module">>, <<"module">> => <<"maps">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
        end},
        {"callers_of_native_module unknown module returns empty sites list", fun() ->
            ok = beamtalk_xref:register_class('NavFfiCaller', nav_ffi_xref()),
            Msg = make_msg(),
            %% A never-loaded module name has no existing atom → sentinel → [].
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{
                    <<"kind">> => <<"callers_of_native_module">>,
                    <<"module">> => <<"__no_such_native_mod_nav__">>
                },
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"value">> := #{<<"sites">> := []}}, Decoded)
        end},
        {"atom table stable under flood of unknown native modules", fun() ->
            ok = beamtalk_xref:register_class('NavFfiCaller', nav_ffi_xref()),
            Msg = make_msg(),
            _ = code:ensure_loaded(beamtalk_repl_ops_nav),
            _ = code:ensure_loaded(json),
            _ =
                json:decode(
                    beamtalk_repl_ops_nav:handle(
                        <<"nav-query">>,
                        #{
                            <<"kind">> => <<"callers_of_native_module">>,
                            <<"module">> => <<"warmup_native_mod">>
                        },
                        Msg,
                        self()
                    )
                ),
            Before = erlang:system_info(atom_count),
            lists:foreach(
                fun(N) ->
                    Bin = iolist_to_binary([
                        <<"__nav_query_flood_mod_">>,
                        integer_to_binary(N),
                        <<"_">>,
                        integer_to_binary(erlang:unique_integer([positive]))
                    ]),
                    Response = beamtalk_repl_ops_nav:handle(
                        <<"nav-query">>,
                        #{<<"kind">> => <<"callers_of_native_module">>, <<"module">> => Bin},
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
            _ =
                json:decode(
                    beamtalk_repl_ops_nav:handle(
                        <<"nav-query">>,
                        #{<<"kind">> => <<"references">>, <<"class">> => <<"WarmupClass">>},
                        Msg,
                        self()
                    )
                ),
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
%% handle/4 — protocol kinds success paths (BT-2639)
%%====================================================================

protocol_setup() ->
    ok = beamtalk_protocol_registry:init(),
    %% A protocol with one instance-side and one class-side requirement so the
    %% class-side `class ` prefix split is exercised.
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'NavTestProtocol',
        module => nav_test_protocol_mod,
        required_methods => [#{selector => 'printOn:', arity => 1}],
        required_class_methods => [#{selector => 'fromString:', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    ok.

protocol_cleanup(_) ->
    catch ets:delete(beamtalk_protocol_registry, 'NavTestProtocol'),
    ok.

protocol_query_test_() ->
    {setup, fun protocol_setup/0, fun protocol_cleanup/1, fun protocol_tests/1}.

protocol_tests(_) ->
    [
        {"required_methods returns the protocol's required selectors", fun() ->
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"required_methods">>, <<"class">> => <<"NavTestProtocol">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := Sites}} = Decoded,
            Methods = [maps:get(<<"method">>, R) || R <- Sites],
            ?assert(lists:member(<<"printOn:">>, Methods)),
            %% The class-side requirement is stripped of its `class ` prefix.
            ?assert(lists:member(<<"fromString:">>, Methods))
        end},
        {"required_methods class-side requirement carries class_side=true", fun() ->
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"required_methods">>, <<"class">> => <<"NavTestProtocol">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := Sites}} = Decoded,
            [ClassRow] = [R || R <- Sites, maps:get(<<"method">>, R) =:= <<"fromString:">>],
            ?assertEqual(true, maps:get(<<"class_side">>, ClassRow)),
            %% Instance-side requirement is not class-side.
            [InstRow] = [R || R <- Sites, maps:get(<<"method">>, R) =:= <<"printOn:">>],
            ?assertEqual(false, maps:get(<<"class_side">>, InstRow))
        end},
        {"required_methods row carries the owning protocol as `class`", fun() ->
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"required_methods">>, <<"class">> => <<"NavTestProtocol">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            #{<<"value">> := #{<<"sites">> := [Row | _]}} = Decoded,
            ?assertEqual(<<"NavTestProtocol">>, maps:get(<<"class">>, Row)),
            ?assert(maps:is_key(<<"source_file">>, Row)),
            ?assert(maps:is_key(<<"line">>, Row))
        end},
        {"conforming_classes returns an (possibly empty) sites list", fun() ->
            %% No live classes are registered in this unit suite, so the result is
            %% an empty list — the key invariant is the wire shape, not membership.
            Msg = make_msg(),
            Response = beamtalk_repl_ops_nav:handle(
                <<"nav-query">>,
                #{<<"kind">> => <<"conforming_classes">>, <<"class">> => <<"NavTestProtocol">>},
                Msg,
                self()
            ),
            Decoded = json:decode(Response),
            ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
            #{<<"value">> := #{<<"sites">> := Sites}} = Decoded,
            ?assert(is_list(Sites))
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

%% Minimal xref payload with an Erlang-FFI send into the `lists` module
%% (BT-2669). Mirrors what codegen emits for `(Erlang lists) reverse: xs`.
nav_ffi_xref() ->
    [
        #{
            class_side => false,
            selector => 'reverseList:',
            line => 12,
            sends => [
                #{
                    selector => 'reverse:',
                    line => 13,
                    recv_kind => erlang_ffi,
                    target_module => 'lists'
                }
            ],
            references => [],
            source_status => indexed,
            provenance => class_body
        }
    ].
