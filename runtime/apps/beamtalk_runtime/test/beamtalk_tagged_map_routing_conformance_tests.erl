%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_tagged_map_routing_conformance_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4200: Erlang half of the tagged-map routing conformance fixture.

The compiler's guarded direct dispatch for sealed-Value-typed sends
(`dispatch_codegen.rs`: `NON_TAGGED_MAP_CLASSES` + the `Exception` superclass
walk) must exclude exactly the classes whose dynamic routing is *not* "tagged
map -> compiled class module": raw Erlang terms, the untagged-map `Dictionary`,
`#beamtalk_object{}` records, the `ErlangModule` native proxy, and the
`Exception` family. Both sides read
`test/fixtures/tagged_map_routing_conformance.json`; the Rust side asserts its
exclusion set equals the fixture's, and this module asserts the runtime really
routes each fixture class the way the fixture says
(`beamtalk_primitive:tagged_map_dispatch/1`) and, the other way round, that
every fixed-module route the runtime has (`beamtalk_primitive:proxy_routes/0`,
BT-4206) is in the fixture. Changing either side alone fails one of the tests.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

fixture() ->
    beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_runtime",
        "test",
        "fixtures",
        "tagged_map_routing_conformance.json"
    ]).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

routing_conformance_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"raw-term classes are never tagged maps", fun raw_terms_are_not_maps/0},
        {"untagged maps dispatch to Dictionary", fun untagged_map_is_dictionary/0},
        {"proxy-routed tagged classes dispatch to the native proxy", fun proxy_routed/0},
        {"every proxy route of the runtime is in the fixture", fun proxy_routes_exhaustive/0},
        {"record classes' values are #beamtalk_object{} records", fun record_classes/0},
        {"the exception root and its subclasses dispatch to the exception module",
            fun exception_family_routed/0}
    ]}.

raw_terms_are_not_maps() ->
    #{<<"raw_term">> := Entries} = fixture(),
    lists:foreach(
        fun(#{<<"class">> := ClassBin, <<"sample">> := Sample}) ->
            case Sample of
                null ->
                    ok;
                Expr ->
                    Value = eval(binary_to_list(Expr)),
                    ?assertNot(is_map(Value), ClassBin),
                    ?assertEqual(
                        binary_to_atom(ClassBin, utf8),
                        beamtalk_primitive:class_of(Value)
                    )
            end
        end,
        Entries
    ).

untagged_map_is_dictionary() ->
    #{<<"untagged_map">> := Classes} = fixture(),
    ?assertEqual([<<"Dictionary">>], Classes),
    ?assertEqual({module, 'bt@stdlib@dictionary'}, beamtalk_primitive:tagged_map_dispatch(#{})),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{a => 1})).

proxy_routed() ->
    #{<<"proxy_routed">> := Classes} = fixture(),
    lists:foreach(
        fun(ClassBin) ->
            Map = #{'$beamtalk_class' => binary_to_atom(ClassBin, utf8)},
            ?assertEqual(
                {module, beamtalk_erlang_proxy}, beamtalk_primitive:tagged_map_dispatch(Map)
            )
        end,
        Classes
    ).

%% BT-4206: the other direction — every class the runtime routes to a fixed
%% native module is one the fixture (and so the compiler's exclusion set)
%% names, so a new route cannot be added on the runtime side alone.
proxy_routes_exhaustive() ->
    #{<<"proxy_routed">> := Classes} = fixture(),
    ?assertEqual(
        lists:sort([binary_to_atom(C, utf8) || C <- Classes]),
        lists:sort(maps:keys(beamtalk_primitive:proxy_routes()))
    ).

%% BT-4206: a `record` class's values are `#beamtalk_object{}` records, never
%% tagged maps: the class object of each is a record whose class is its
%% metaclass.
record_classes() ->
    #{<<"record">> := Classes} = fixture(),
    ?assertNotEqual([], Classes),
    lists:foreach(
        fun(ClassBin) ->
            Class = binary_to_atom(ClassBin, utf8),
            ClassObj = beamtalk_class_registry:resolve_class_object(Class),
            ?assert(is_record(ClassObj, beamtalk_object), ClassBin),
            ?assertNot(is_map(ClassObj), ClassBin),
            ?assert(beamtalk_class_registry:is_class_object(ClassObj), ClassBin),
            ?assertEqual(
                binary_to_atom(<<ClassBin/binary, " class">>, utf8),
                beamtalk_primitive:class_of(ClassObj)
            )
        end,
        Classes
    ).

exception_family_routed() ->
    #{<<"exception_root">> := RootBin} = fixture(),
    Root = binary_to_atom(RootBin, utf8),
    lists:foreach(
        fun(Class) ->
            ?assert(beamtalk_exception_handler:is_exception_class(Class)),
            ?assertEqual(
                {module, 'bt@stdlib@exception'},
                beamtalk_primitive:tagged_map_dispatch(#{'$beamtalk_class' => Class})
            )
        end,
        [Root, 'Error', 'RuntimeError']
    ).

eval(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.
