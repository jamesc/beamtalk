%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_tagged_map_routing_conformance_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4200, reshaped by BT-4292: Erlang half of the representation routing
fixture `test/fixtures/tagged_map_routing_conformance.json`.

The compiler reads every representation fact from a table generated from
that fixture (`crates/beamtalk-codegen/src/core_erlang/routing/table.rs`):
the type-pattern guards, the classes a class-tag guard must never target,
the reserved tuple tags. This module checks the runtime really behaves as
each row says, through the dynamic path:

- every row with a sample dispatches to its `target_module` (observed with
  call tracing, not inferred), and `target_module:dispatch/3` answers every
  probe send exactly as `beamtalk_message_dispatch:send/3` does — the
  routing-level differential a guarded direct call relies on;
- every special selector is really answered outside `target_module`, and the
  runtime's own special-selector sets equal the fixture's;
- every `dynamic_only` reason that can be observed still holds;
- the reserved tuple tags are never `Tuple`s, a `'__class_mod__'` map is
  never a `Dictionary`, and the fixed-module routes, record classes and the
  exception family are routed as the fixture says.

Changing the runtime or the fixture alone fails a test here or in the Rust
drift test `routing_table_is_generated_from_fixture`.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% A selector no class defines.
-define(NO_SUCH, bt4292NoSuchSelector).

%% Stands in for a compiled actor module behind a `'__class_mod__'` key.
-export([class_name/0]).

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
        {"a '__class_mod__' map is not a Dictionary", fun class_mod_map_is_not_dictionary/0},
        {"proxy-routed tagged classes dispatch to the native proxy", fun proxy_routed/0},
        {"every proxy route of the runtime is in the fixture", fun proxy_routes_exhaustive/0},
        {"record classes' values are #beamtalk_object{} records", fun record_classes/0},
        {"the exception root and its subclasses dispatch to the exception module",
            fun exception_family_routed/0},
        {"every sample reaches its target module", fun samples_reach_target_module/0},
        {"a pinned class-module row's live class module is its target module",
            fun pinned_class_modules_are_live_class_modules/0},
        {"target_module:dispatch/3 answers like the dynamic send",
            fun target_dispatch_matches_dynamic_send/0},
        {"String special selectors equal the runtime's shared-selector set",
            fun string_special_selectors_match_runtime/0},
        {"String special selectors bypass the String module",
            fun string_special_selectors_bypass_target/0},
        {"Pid special selectors equal the runtime's future protocol",
            fun pid_special_selectors_match_runtime/0},
        {"Pid special selectors bypass the Pid module", fun pid_special_selectors_bypass_target/0},
        {"supervisor special selectors skip the hierarchy walk",
            fun supervisor_special_selectors_skip_lookup/0},
        {"observable dynamic_only reasons still hold", fun dynamic_only_reasons_hold/0},
        {"reserved tuple tags are never Tuples", fun reserved_tuples_are_not_tuples/0},
        {"isKindOfBuiltin treats only String among reserved tuples as a plain value",
            fun reserved_tuples_are_not_plain_values/0},
        {"with no override, isKindOfBuiltin answers the test for plain values only",
            fun is_kind_of_builtin_answers_test_without_override/0},
        {"every row's type-test guard agrees with Object>>isKindOf: for every sample",
            fun guards_agree_with_is_kind_of/0},
        {"a deliberately broken guard is caught by the agreement check",
            fun broken_guard_is_caught/0},
        {"an isKindOf: override below Object makes isKindOfBuiltin send",
            fun is_kind_of_override_makes_builtin_send/0},
        {"rewritten selectors are rewritten before classification",
            fun rewritten_selectors_are_rewritten/0}
    ]}.

%%% ============================================================================
%%% Fixture access
%%% ============================================================================

rows() ->
    #{<<"rows">> := Rows} = fixture(),
    Rows.

rows_of(Representation) ->
    [Row || #{<<"representation">> := R} = Row <- rows(), R =:= Representation].

row(Class) ->
    [Row] = [R || #{<<"class">> := C} = R <- rows(), C =:= Class],
    Row.

class_atom(#{<<"class">> := Class}) -> binary_to_atom(Class, utf8).

atoms(Bins) -> [binary_to_atom(B, utf8) || B <- Bins].

special_selectors(Row) -> atoms(maps:get(<<"special_selectors">>, Row)).

%% Rows with a sample and a target module, as {Row, Sample, TargetModule}.
routed_samples() ->
    [
        {Row, eval(binary_to_list(Sample)), binary_to_atom(Target, utf8)}
     || #{<<"sample">> := Sample, <<"target_module">> := Target} = Row <- rows(),
        Sample =/= null,
        Target =/= null
    ].

%% Every module a fixture row names as a target: the modules a send could be
%% misrouted to.
target_modules() ->
    lists:usort([
        binary_to_atom(T, utf8)
     || #{<<"target_module">> := T} <- rows(), T =/= null
    ]).

%%% ============================================================================
%%% Representation
%%% ============================================================================

raw_terms_are_not_maps() ->
    lists:foreach(
        fun
            (#{<<"sample">> := null}) ->
                ok;
            (#{<<"class">> := ClassBin, <<"sample">> := Expr}) ->
                Value = eval(binary_to_list(Expr)),
                ?assertNot(is_map(Value), ClassBin),
                ?assertEqual(binary_to_atom(ClassBin, utf8), beamtalk_primitive:class_of(Value))
        end,
        rows_of(<<"raw_term">>)
    ).

untagged_map_is_dictionary() ->
    ?assertEqual([<<"Dictionary">>], [C || #{<<"class">> := C} <- rows_of(<<"untagged_map">>)]),
    ?assertEqual({module, 'bt@stdlib@dictionary'}, beamtalk_primitive:tagged_map_dispatch(#{})),
    ?assertEqual('Dictionary', beamtalk_primitive:class_of(#{a => 1})).

%% BT-4292: `beamtalk_tagged_map:class_of/1` reads a class from
%% `'__class_mod__'` when there is no `'$beamtalk_class'` tag, so the
%% `untagged_map` guard must reject such a map.
class_mod_map_is_not_dictionary() ->
    Map = #{'__class_mod__' => ?MODULE, total => 1},
    ?assertEqual(class_name(), beamtalk_primitive:class_of(Map)).

%% A compiled actor module's `class_name/0`, which
%% `beamtalk_tagged_map:class_of/1` calls through `'__class_mod__'`.
class_name() -> 'Bt4292ClassModProbe'.

proxy_routed() ->
    lists:foreach(
        fun(#{<<"target_module">> := Target} = Row) ->
            Map = #{'$beamtalk_class' => class_atom(Row)},
            ?assertEqual(
                {module, binary_to_atom(Target, utf8)},
                beamtalk_primitive:tagged_map_dispatch(Map)
            )
        end,
        rows_of(<<"proxy_routed">>)
    ).

%% BT-4206: the other direction — every class the runtime routes to a fixed
%% native module is one the fixture (and so the compiler's exclusion set)
%% names, so a new route cannot be added on the runtime side alone.
proxy_routes_exhaustive() ->
    ?assertEqual(
        lists:sort([
            {class_atom(Row), binary_to_atom(T, utf8)}
         || #{<<"target_module">> := T} = Row <- rows_of(<<"proxy_routed">>)
        ]),
        lists:sort(maps:to_list(beamtalk_primitive:proxy_routes()))
    ).

%% BT-4206: a `record` class's values are `#beamtalk_object{}` records, never
%% tagged maps: the class object of each is a record whose class is its
%% metaclass.
record_classes() ->
    Rows = rows_of(<<"record">>),
    ?assertNotEqual([], Rows),
    lists:foreach(
        fun(#{<<"class">> := ClassBin} = Row) ->
            ClassObj = beamtalk_class_registry:resolve_class_object(class_atom(Row)),
            ?assert(is_record(ClassObj, beamtalk_object), ClassBin),
            ?assertNot(is_map(ClassObj), ClassBin),
            ?assert(beamtalk_class_registry:is_class_object(ClassObj), ClassBin),
            ?assertEqual(
                binary_to_atom(<<ClassBin/binary, " class">>, utf8),
                beamtalk_primitive:class_of(ClassObj)
            )
        end,
        Rows
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

reserved_tuples_are_not_tuples() ->
    #{<<"reserved_tuple_tags">> := Tags} = fixture(),
    lists:foreach(
        fun(#{<<"tag">> := TagBin, <<"size">> := Size}) ->
            Tag = binary_to_atom(TagBin, utf8),
            Sample = reserved_sample(Tag),
            ?assertEqual(Size, tuple_size(Sample)),
            ?assertNotEqual('Tuple', beamtalk_primitive:class_of(Sample), TagBin)
        end,
        Tags
    ).

%% BT-4473: `beamtalk_behaviour_intrinsics:isKindOfBuiltin/3` answers the
%% compiler's representation test only for a plain value; every other reserved
%% shape (an actor or class object, a supervisor handle, a future) must send,
%% and so must a tagged map. A new reserved tag in the fixture fails here until
%% `plain_value/1` decides it.
reserved_tuples_are_not_plain_values() ->
    #{<<"reserved_tuple_tags">> := Tags} = fixture(),
    lists:foreach(
        fun(#{<<"tag">> := TagBin}) ->
            Tag = binary_to_atom(TagBin, utf8),
            ?assertEqual(
                Tag =:= beamtalk_string,
                beamtalk_behaviour_intrinsics:plain_value(reserved_sample(Tag)),
                TagBin
            )
        end,
        Tags
    ),
    ?assertNot(beamtalk_behaviour_intrinsics:plain_value(#{'$beamtalk_class' => 'Point'})),
    %% Whatever the runtime treats as tagged is not plain.
    ?assertNot(beamtalk_behaviour_intrinsics:plain_value(#{'__class_mod__' => 1, foo => 1})),
    %% A reserved head of any size is not plain (`class_of/1` does not route
    %% it to `Tuple`).
    ?assertNot(beamtalk_behaviour_intrinsics:plain_value({beamtalk_object, 1, 2})),
    ?assert(beamtalk_behaviour_intrinsics:plain_value({a, 1})),
    ?assert(beamtalk_behaviour_intrinsics:plain_value(#{a => 1})).

%% BT-4473: the branch the BUnit run may never reach (another test registers
%% an `isKindOf:` extension node-wide): with the flag down, a plain value gets
%% the compiler's test answer, which agrees with the send for every routed
%% sample; a non-plain value (a class object) still sends.
is_kind_of_builtin_answers_test_without_override() ->
    lists:foreach(
        fun({#{<<"class">> := ClassBin}, Sample, _Target}) ->
            Class = binary_to_atom(ClassBin, utf8),
            K = beamtalk_class_registry:resolve_class_object(Class),
            Sent = beamtalk_message_dispatch:send(Sample, 'isKindOf:', [K]),
            ?assertEqual(
                Sent,
                beamtalk_behaviour_intrinsics:kind_of_builtin(Sample, K, Sent, false),
                ClassBin
            ),
            ?assertEqual(
                not Sent,
                beamtalk_behaviour_intrinsics:kind_of_builtin(Sample, K, not Sent, false),
                ClassBin
            )
        end,
        routed_samples()
    ),
    IntegerClass = beamtalk_class_registry:resolve_class_object('Integer'),
    ?assertEqual(
        false,
        beamtalk_behaviour_intrinsics:kind_of_builtin(IntegerClass, IntegerClass, true, false)
    ).

%% BT-4473: once any class below `Object` defines `isKindOf:` (here an
%% extension on `Port`), `isKindOfBuiltin/3` stops answering the compiler's
%% test and sends, so the override runs. The flag is node-wide and never
%% lowered, so this test leaves it raised for the rest of the run, which only
%% sends more.
is_kind_of_override_makes_builtin_send() ->
    Port = hd(erlang:ports()),
    PortClass = beamtalk_class_registry:resolve_class_object('Port'),
    Ext = fun(_Args, _Self) -> overridden end,
    ok = beamtalk_extensions:register('Port', 'isKindOf:', Ext, bt4473_test),
    try
        ?assert(beamtalk_default_method:overridden_anywhere('isKindOf:')),
        ?assertEqual(
            overridden,
            beamtalk_behaviour_intrinsics:isKindOfBuiltin(Port, PortClass, true)
        )
    after
        beamtalk_extensions:unregister('Port', 'isKindOf:')
    end,
    %% Unwatched selectors always count as overridden; `Object` itself never
    %% raises the flag.
    ?assert(beamtalk_default_method:overridden_anywhere('printString')),
    ok = beamtalk_default_method:note_override('Object', 'isKindOf:').

%% BT-4495: the lowering of `x isKindOf: C` to C's routing-fixture type test is
%% sound only if the guard equals `Object>>isKindOf:` for every value. Check
%% it here, against the dynamic send (never the lowered literal or
%% `kind_of_builtin/4`), so it runs on every run whatever `isKindOf:`
%% extensions other tests registered.
guards_agree_with_is_kind_of() ->
    ?assertEqual([], guard_disagreements(rows())).

%% The check has teeth: each broken guard below must be reported.
broken_guard_is_caught() ->
    Broken = fun(Class, Guard) ->
        [Row] = [R#{<<"guard">> := Guard} || R <- rows(), maps:get(<<"class">>, R) =:= Class],
        guard_disagreements([Row])
    end,
    ?assertNotEqual(
        [], Broken(<<"Integer">>, #{<<"kind">> => <<"bif">>, <<"bif">> => <<"is_float">>})
    ),
    ?assertNotEqual(
        [], Broken(<<"Binary">>, #{<<"kind">> => <<"bif">>, <<"bif">> => <<"is_binary">>})
    ),
    ?assertNotEqual(
        [], Broken(<<"Symbol">>, #{<<"kind">> => <<"bif">>, <<"bif">> => <<"is_atom">>})
    ),
    ?assertNotEqual(
        [], Broken(<<"Tuple">>, #{<<"kind">> => <<"bif">>, <<"bif">> => <<"is_tuple">>})
    ).

%% {Class, Sample, Guard answer, send answer} for every guarded row and sample
%% where the two differ.
guard_disagreements(Rows) ->
    Samples = agreement_samples(),
    [
        {ClassBin, Sample, Guarded, Sent}
     || #{<<"class">> := ClassBin, <<"guard">> := Guard} <- Rows,
        Guard =/= null,
        Sample <- Samples,
        begin
            K = beamtalk_class_registry:resolve_class_object(binary_to_atom(ClassBin, utf8)),
            Sent = beamtalk_message_dispatch:send(Sample, 'isKindOf:', [K]),
            Guarded = guard_holds(Guard, Sample),
            Guarded =/= Sent
        end
    ].

%% Every routed sample, plus values the fixture has no row for: empty
%% containers, every reserved tuple head, a class object.
agreement_samples() ->
    %% A future is awaited before `isKindOf:` runs (the send answers for its
    %% value) and `plain_value/1` keeps it out of the lowered test, so it is no
    %% sample here.
    Reserved = [
        reserved_sample(binary_to_atom(T, utf8))
     || #{<<"tag">> := T} <- reserved_tuples(), T =/= <<"beamtalk_future">>
    ],
    [S || {_Row, S, _Target} <- routed_samples()] ++
        Reserved ++
        [
            {},
            [],
            <<>>,
            <<"abc">>,
            true,
            false,
            nil,
            #{},
            'a',
            {a, 1},
            {a, b, c, d},
            beamtalk_class_registry:resolve_class_object('Integer')
        ].

reserved_tuples() ->
    #{<<"reserved_tuple_tags">> := Tags} = fixture(),
    Tags.

%% Interprets a fixture guard (see `_fields.guard` in the fixture).
guard_holds(#{<<"kind">> := <<"bif">>, <<"bif">> := Bif}, V) ->
    erlang:apply(erlang, binary_to_atom(Bif, utf8), [V]);
guard_holds(#{<<"kind">> := <<"string_tag">>}, V) ->
    is_string_tag(V);
guard_holds(#{<<"kind">> := <<"binary_or_string_tag">>}, V) ->
    is_binary(V) orelse is_string_tag(V);
guard_holds(#{<<"kind">> := <<"unaligned_bitstring">>}, V) ->
    is_bitstring(V) andalso not is_binary(V);
guard_holds(#{<<"kind">> := <<"symbol">>}, V) ->
    is_atom(V) andalso V =/= true andalso V =/= false andalso V =/= nil;
guard_holds(#{<<"kind">> := <<"exact_atom">>, <<"atom">> := A}, V) ->
    V =:= binary_to_atom(A, utf8);
guard_holds(#{<<"kind">> := <<"untagged_map">>}, V) ->
    is_map(V) andalso not maps:is_key('$beamtalk_class', V) andalso
        not maps:is_key('__class_mod__', V);
guard_holds(#{<<"kind">> := <<"unreserved_tuple">>}, V) ->
    is_tuple(V) andalso
        (tuple_size(V) =:= 0 orelse
            not lists:any(fun(T) -> element(1, V) =:= T end, reserved_heads())).

is_string_tag({beamtalk_string, _}) -> true;
is_string_tag(_) -> false.

reserved_heads() ->
    atoms([T || #{<<"tag">> := T} <- reserved_tuples()]).

reserved_sample(beamtalk_string) ->
    {beamtalk_string, <<"s">>};
reserved_sample(beamtalk_future) ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, 42),
    Future;
reserved_sample(beamtalk_object) ->
    beamtalk_class_registry:resolve_class_object('Integer');
reserved_sample(beamtalk_supervisor) ->
    {beamtalk_supervisor, 'Supervisor', 'bt@stdlib@supervisor', dead_pid()}.

%%% ============================================================================
%%% Routing: target module, special selectors, dynamic_only
%%% ============================================================================

samples_reach_target_module() ->
    lists:foreach(
        fun({#{<<"class">> := ClassBin}, Sample, Target}) ->
            Calls = traced_calls(
                [{M, dispatch, 3} || M <- target_modules()],
                fun() -> beamtalk_message_dispatch:send(Sample, printString, []) end
            ),
            ?assertMatch([{Target, dispatch, [printString, [], Sample]} | _], Calls, ClassBin)
        end,
        routed_samples()
    ).

%% BT-4411: a class literal naming a row whose `target_module` is the class's
%% own module bakes that module into the class object instead of reading it
%% with `module_name_safe/1`. That is sound only while the live class's
%% module is the fixture's: check it for every such row, so the baked object
%% is `=:=` to the one the run-time lookup builds. A `proxy_routed` row's
%% target is the native proxy, not its class module, and is never baked.
pinned_class_modules_are_live_class_modules() ->
    Rows = [
        {binary_to_atom(C, utf8), binary_to_atom(T, utf8)}
     || #{<<"class">> := C, <<"target_module">> := T, <<"representation">> := R} <- rows(),
        T =/= null,
        R =/= <<"proxy_routed">>
    ],
    ?assertNotEqual([], Rows),
    lists:foreach(
        fun({Class, Target}) ->
            Pid = beamtalk_class_registry:whereis_class(Class),
            ?assert(is_pid(Pid), Class),
            ?assertEqual(Target, beamtalk_object_class:module_name_safe(Pid), Class)
        end,
        Rows
    ).

%% The routing-level differential send: for a value the row's guard accepts
%% (its sample), `target_module:dispatch/3` — what a guarded direct call runs
%% — gives the same result or the same error shape as the dynamic send, for
%% every probe outside the row's special selectors.
target_dispatch_matches_dynamic_send() ->
    lists:foreach(
        fun({#{<<"class">> := ClassBin} = Row, Sample, Target}) ->
            case maps:get(<<"dynamic_only">>, Row) of
                null ->
                    Special = special_selectors(Row),
                    lists:foreach(
                        fun({Selector, Args}) ->
                            ?assertEqual(
                                outcome(fun() ->
                                    beamtalk_message_dispatch:send(Sample, Selector, Args)
                                end),
                                outcome(fun() -> Target:dispatch(Selector, Args, Sample) end),
                                {ClassBin, Selector}
                            )
                        end,
                        [P || {Sel, _} = P <- probes(), not lists:member(Sel, Special)]
                    );
                _ ->
                    ok
            end
        end,
        routed_samples()
    ).

probes() ->
    [
        {printString, []},
        {class, []},
        {?NO_SUCH, []},
        {'bt4292NoSuch:selector:', [1, 2]},
        {'perform:', [printString]},
        {'perform:', [?NO_SUCH]},
        {'perform:withArguments:', [printString, []]},
        {'respondsTo:', [printString]},
        {'doesNotUnderstand:args:', [?NO_SUCH, []]}
    ].

string_special_selectors_match_runtime() ->
    Special = lists:sort(special_selectors(row(<<"String">>))),
    {ok, BinarySelectors} = beamtalk_class_metadata:lookup_instance_methods('Binary'),
    ?assertEqual(
        Special,
        lists:sort([
            S
         || S <- lists:usort(BinarySelectors ++ Special),
            beamtalk_primitive:is_string_binary_shared_selector(S)
        ])
    ).

string_special_selectors_bypass_target() ->
    Row = row(<<"String">>),
    Sample = eval(binary_to_list(maps:get(<<"sample">>, Row))),
    lists:foreach(
        fun(Selector) ->
            Args = lists:duplicate(arity(Selector), 0),
            Calls = traced_calls(
                [{M, dispatch, 3} || M <- target_modules()],
                fun() -> beamtalk_message_dispatch:send(Sample, Selector, Args) end
            ),
            ?assertMatch([{'bt@stdlib@binary', dispatch, [Selector | _]} | _], Calls, Selector)
        end,
        special_selectors(Row)
    ).

pid_special_selectors_match_runtime() ->
    ?assertEqual(
        lists:sort(special_selectors(row(<<"Pid">>))),
        lists:sort(beamtalk_primitive:future_selectors())
    ).

%% `await` and `awaitForever` on a bare pid block until it answers, so only
%% the selectors that return on a pid that is not a future are sent.
pid_special_selectors_bypass_target() ->
    Pid = spawn(fun() -> ok end),
    Block = fun(_) -> ok end,
    Sends = [{'await:', [0]}, {'whenResolved:', [Block]}, {'whenRejected:', [Block]}],
    ?assertEqual([], [S || {S, _} <- Sends, not lists:member(S, special_selectors(row(<<"Pid">>)))]),
    lists:foreach(
        fun({Selector, Args}) ->
            Calls = traced_calls(
                [{'bt@stdlib@pid', dispatch, 3}],
                fun() -> beamtalk_message_dispatch:send(Pid, Selector, Args) end
            ),
            ?assertEqual([], Calls, Selector)
        end,
        Sends
    ).

supervisor_special_selectors_skip_lookup() ->
    lists:foreach(
        fun(#{<<"class">> := ClassBin} = Row) ->
            Handle = {beamtalk_supervisor, class_atom(Row), 'bt@stdlib@supervisor', dead_pid()},
            ?assertEqual(class_atom(Row), beamtalk_primitive:class_of(Handle)),
            lists:foreach(
                fun(Selector) ->
                    Calls = traced_calls(
                        [{beamtalk_dispatch, lookup, 5}],
                        fun() ->
                            beamtalk_message_dispatch:send(
                                Handle, Selector, lists:duplicate(arity(Selector), 0)
                            )
                        end
                    ),
                    ?assertEqual([], Calls, {ClassBin, Selector})
                end,
                special_selectors(Row)
            ),
            %% Any other selector takes the hierarchy walk (ADR 0059).
            ?assertMatch(
                [{beamtalk_dispatch, lookup, _} | _],
                traced_calls(
                    [{beamtalk_dispatch, lookup, 5}],
                    fun() -> beamtalk_message_dispatch:send(Handle, ?NO_SUCH, []) end
                ),
                ClassBin
            )
        end,
        rows_of(<<"supervisor_tuple">>)
    ).

%% The `dynamic_only` reasons a test can observe: Bitstring's rewrapped DNU
%% and Boolean's two modules. The others are representation facts checked
%% above (Class/Metaclass records, the proxy route, supervisor handles) or a
%% codegen fact (Character).
dynamic_only_reasons_hold() ->
    Bits = eval(binary_to_list(maps:get(<<"sample">>, row(<<"Bitstring">>)))),
    BitsModule = binary_to_atom(maps:get(<<"target_module">>, row(<<"Bitstring">>)), utf8),
    ?assertNotEqual(null, maps:get(<<"dynamic_only">>, row(<<"Bitstring">>))),
    ?assertNotEqual(
        outcome(fun() -> BitsModule:dispatch(?NO_SUCH, [], Bits) end),
        outcome(fun() -> beamtalk_message_dispatch:send(Bits, ?NO_SUCH, []) end)
    ),
    Boolean = row(<<"Boolean">>),
    ?assertEqual(null, maps:get(<<"target_module">>, Boolean)),
    ?assertNotEqual(
        maps:get(<<"target_module">>, row(<<"True">>)),
        maps:get(<<"target_module">>, row(<<"False">>))
    ).

rewritten_selectors_are_rewritten() ->
    #{<<"rewritten_selectors">> := Rewritten} = fixture(),
    ?assertEqual(['perform:withArguments:timeout:'], atoms(Rewritten)),
    Calls = traced_calls(
        [{'bt@stdlib@integer', dispatch, 3}],
        fun() ->
            beamtalk_message_dispatch:send(
                1, 'perform:withArguments:timeout:', [printString, [], 1000]
            )
        end
    ),
    ?assertMatch([{'bt@stdlib@integer', dispatch, [printString, [], 1]} | _], Calls).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% The calls to `MFAs` made while `Fun` runs, in order, as
%% `{Module, Function, Args}`. `Fun` runs in a fresh process (a process
%% tracing itself receives no trace messages); its result or exception is
%% discarded.
traced_calls(MFAs, Fun) ->
    lists:foreach(fun({M, _, _}) -> {module, M} = code:ensure_loaded(M) end, MFAs),
    flush_trace(),
    Parent = self(),
    Worker = spawn(fun() ->
        receive
            go -> ok
        end,
        _ = outcome(Fun),
        Parent ! {traced_done, self()}
    end),
    _ = [erlang:trace_pattern(MFA, true, [global]) || MFA <- MFAs],
    1 = erlang:trace(Worker, true, [call]),
    Worker ! go,
    receive
        {traced_done, Worker} -> ok
    end,
    _ = [erlang:trace_pattern(MFA, false, [global]) || MFA <- MFAs],
    %% Trace messages are delivered asynchronously: wait for all of them.
    Ref = erlang:trace_delivered(Worker),
    receive
        {trace_delivered, Worker, Ref} -> ok
    end,
    collect_trace([]).

collect_trace(Acc) ->
    receive
        {trace, _, call, MFA} -> collect_trace([MFA | Acc])
    after 0 -> lists:reverse(Acc)
    end.

flush_trace() ->
    receive
        {trace, _, call, _} -> flush_trace()
    after 0 -> ok
    end.

%% The observable outcome of `Fun`: its value, or the structured shape of the
%% error it raised.
outcome(Fun) ->
    try
        {returned, Fun()}
    catch
        error:#{error := #beamtalk_error{} = E} -> {raised, error_shape(E)};
        error:#beamtalk_error{} = E -> {raised, error_shape(E)};
        Class:Reason -> {raised, Class, Reason}
    end.

error_shape(#beamtalk_error{kind = K, class = C, selector = S, message = M, hint = H}) ->
    {K, C, S, M, H}.

arity(Selector) ->
    length([C || C <- atom_to_list(Selector), C =:= $:]).

dead_pid() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive
        {'DOWN', Ref, process, Pid, _} -> Pid
    end.

eval(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.
