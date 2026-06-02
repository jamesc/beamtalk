%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_xref_tests).

-moduledoc """
EUnit tests for `beamtalk_xref` (BT-2297 / ADR 0087 Phase 1).

Covers: insert via register_class/2, query via the four read APIs,
purge via purge_class/1, per-class generation increments, put_method/4
isolation, and concurrent reader during a write.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

setup() ->
    %% Use an already-running supervised server when one exists (the runtime
    %% app starts xref under the supervisor); otherwise stand a fresh one up
    %% for tests that run outside the application.
    Pid =
        case whereis(beamtalk_xref) of
            undefined ->
                {ok, P} = beamtalk_xref:start_link(),
                P;
            P ->
                P
        end,
    %% Start each test from a clean slate. This protects against bleed-through
    %% when EUnit groups multiple test_ generators under one fixture and from
    %% a stale supervised process surviving an earlier failure.
    clear_all_tables(),
    Pid.

cleanup(_Pid) ->
    %% Don't stop the server — supervised in the runtime app. Clear all rows
    %% so the next test starts empty even if it picks up the same gen_server.
    clear_all_tables(),
    ok.

clear_all_tables() ->
    %% The tables are `protected` (owned by the gen_server, only the owner can
    %% write), so route the clears through the owning process. Phase 1 doesn't
    %% expose a public clear API yet (lands with the Phase 4 sweep); for tests
    %% we ride sys:replace_state to mutate from inside the gen_server.
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
    end,
    ok.

%%====================================================================
%% Fixtures
%%====================================================================

counter_xref() ->
    %% Hand-rolled method_xref payload mimicking what codegen will emit in
    %% Phase 2 — gives the read APIs something to match against.
    [
        #{
            class_side => false,
            selector => 'increment',
            line => 14,
            sends => [
                #{selector => '+', line => 17, recv_kind => self_recv}
            ],
            references => [
                #{class => 'Integer', line => 16}
            ],
            source_status => indexed,
            provenance => class_body
        },
        #{
            class_side => false,
            selector => 'value',
            line => 22,
            sends => [],
            references => [],
            source_status => indexed,
            provenance => class_body
        },
        #{
            class_side => true,
            selector => 'new',
            line => 8,
            sends => [
                #{selector => 'basicNew', line => 9, recv_kind => super_recv}
            ],
            references => [],
            source_status => indexed,
            provenance => class_body
        }
    ].

point_xref() ->
    [
        #{
            class_side => false,
            selector => 'distanceTo:',
            line => 30,
            sends => [
                #{selector => '+', line => 32, recv_kind => self_recv},
                #{selector => 'sqrt', line => 33, recv_kind => other}
            ],
            references => [
                #{class => 'Integer', line => 31}
            ]
        }
    ].

%%====================================================================
%% register_class / read APIs
%%====================================================================

register_and_read_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),

                %% defined_selectors
                Instance = lists:sort(beamtalk_xref:defined_selectors('Counter', false)),
                ?assertEqual(['increment', 'value'], Instance),
                ?assertEqual(['new'], beamtalk_xref:defined_selectors('Counter', true)),
                ?assertEqual([], beamtalk_xref:defined_selectors('NoSuchClass', false)),

                %% implementors_of
                ?assertEqual([{'Counter', false}], beamtalk_xref:implementors_of('increment')),
                ?assertEqual([{'Counter', true}], beamtalk_xref:implementors_of('new')),
                ?assertEqual([], beamtalk_xref:implementors_of('noSuchSelector')),

                %% senders_of
                PlusSites = beamtalk_xref:senders_of('+'),
                ?assertEqual(1, length(PlusSites)),
                [PlusSite] = PlusSites,
                ?assertEqual('Counter', maps:get(owner, PlusSite)),
                ?assertEqual('increment', maps:get(method, PlusSite)),
                ?assertEqual(false, maps:get(class_side, PlusSite)),
                ?assertEqual(self_recv, maps:get(recv_kind, PlusSite)),
                ?assertEqual(17, maps:get(line, PlusSite)),
                ?assertEqual(1, maps:get(gen, PlusSite)),

                BasicNewSites = beamtalk_xref:senders_of('basicNew'),
                ?assertEqual(1, length(BasicNewSites)),
                [BasicNewSite] = BasicNewSites,
                ?assertEqual(true, maps:get(class_side, BasicNewSite)),
                ?assertEqual(super_recv, maps:get(recv_kind, BasicNewSite)),

                %% references_to
                IntRefs = beamtalk_xref:references_to('Integer'),
                ?assertEqual(1, length(IntRefs)),
                [IntRef] = IntRefs,
                ?assertEqual('Counter', maps:get(owner, IntRef)),
                ?assertEqual('increment', maps:get(method, IntRef)),
                ?assertEqual([], beamtalk_xref:references_to('NoSuchClass'))
            end)
        ]
    end}.

multi_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                ok = beamtalk_xref:register_class('Point', point_xref()),

                %% Both classes contribute to senders_of('+').
                PlusSites = beamtalk_xref:senders_of('+'),
                ?assertEqual(2, length(PlusSites)),
                Owners = lists:sort([maps:get(owner, S) || S <- PlusSites]),
                ?assertEqual(['Counter', 'Point'], Owners),

                %% Both classes reference Integer.
                IntRefs = beamtalk_xref:references_to('Integer'),
                ?assertEqual(2, length(IntRefs)),

                %% implementors_of is class-specific.
                ?assertEqual([{'Point', false}], beamtalk_xref:implementors_of('distanceTo:'))
            end)
        ]
    end}.

%%====================================================================
%% purge_class
%%====================================================================

purge_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                ok = beamtalk_xref:register_class('Point', point_xref()),

                ?assertEqual(2, length(beamtalk_xref:senders_of('+'))),

                ok = beamtalk_xref:purge_class('Counter'),

                %% Counter rows are gone, Point rows remain.
                ?assertEqual([], beamtalk_xref:defined_selectors('Counter', false)),
                ?assertEqual([], beamtalk_xref:defined_selectors('Counter', true)),
                ?assertEqual([], beamtalk_xref:implementors_of('increment')),
                ?assertEqual([], beamtalk_xref:implementors_of('new')),

                PlusSites = beamtalk_xref:senders_of('+'),
                ?assertEqual(1, length(PlusSites)),
                [Site] = PlusSites,
                ?assertEqual('Point', maps:get(owner, Site)),

                IntRefs = beamtalk_xref:references_to('Integer'),
                ?assertEqual(1, length(IntRefs)),
                [IntRef] = IntRefs,
                ?assertEqual('Point', maps:get(owner, IntRef)),

                %% Purging an unknown class is a no-op.
                ok = beamtalk_xref:purge_class('NoSuchClass')
            end)
        ]
    end}.

%%====================================================================
%% Generation counter
%%====================================================================

generation_increments_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                Gen1 = current_gen('Counter'),
                ?assertEqual(1, Gen1),

                %% Inspect the gen embedded in a senders row.
                [Site1] = beamtalk_xref:senders_of('basicNew'),
                ?assertEqual(1, maps:get(gen, Site1)),

                %% Re-register bumps the gen.
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                ?assertEqual(2, current_gen('Counter')),

                %% put_method also bumps the gen.
                NewEntry = #{
                    class_side => false,
                    selector => 'increment',
                    line => 14,
                    sends => [],
                    references => [],
                    source_status => indexed,
                    provenance => put_method
                },
                ok = beamtalk_xref:put_method('Counter', false, 'increment', NewEntry),
                ?assertEqual(3, current_gen('Counter')),

                %% A fresh class starts at gen 1.
                ok = beamtalk_xref:register_class('Point', point_xref()),
                ?assertEqual(1, current_gen('Point'))
            end)
        ]
    end}.

%%====================================================================
%% put_method isolation
%%====================================================================

put_method_replaces_one_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),

                %% Sanity: the sibling 'value' method is present and 'increment'
                %% sends '+'.
                ?assert(lists:member('value', beamtalk_xref:defined_selectors('Counter', false))),
                ?assertEqual(1, length(beamtalk_xref:senders_of('+'))),

                %% Replace `increment` with a version that sends `-` instead.
                NewEntry = #{
                    class_side => false,
                    selector => 'increment',
                    line => 14,
                    sends => [
                        #{selector => '-', line => 17, recv_kind => self_recv}
                    ],
                    references => [
                        #{class => 'Float', line => 16}
                    ],
                    source_status => indexed,
                    provenance => put_method
                },
                ok = beamtalk_xref:put_method('Counter', false, 'increment', NewEntry),

                %% Sibling `value` untouched.
                ?assert(lists:member('value', beamtalk_xref:defined_selectors('Counter', false))),
                %% Class-side `new` untouched.
                ?assertEqual(['new'], beamtalk_xref:defined_selectors('Counter', true)),
                %% Class-side `basicNew` site preserved (was on `new`, not `increment`).
                ?assertEqual(1, length(beamtalk_xref:senders_of('basicNew'))),

                %% Old `+` send from `increment` is gone, new `-` send is present.
                ?assertEqual([], beamtalk_xref:senders_of('+')),
                MinusSites = beamtalk_xref:senders_of('-'),
                ?assertEqual(1, length(MinusSites)),
                [MinusSite] = MinusSites,
                ?assertEqual('increment', maps:get(method, MinusSite)),

                %% References updated: Integer dropped, Float added.
                ?assertEqual([], beamtalk_xref:references_to('Integer')),
                FloatRefs = beamtalk_xref:references_to('Float'),
                ?assertEqual(1, length(FloatRefs))
            end)
        ]
    end}.

put_method_normalises_selector_and_side_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),

                %% Even if the entry omits class_side/selector or has stale
                %% values, the call args win — the entry is addressed by
                %% {Class, ClassSide, Selector}.
                BareEntry = #{
                    line => 20,
                    sends => [#{selector => 'foo', line => 21}],
                    references => [],
                    source_status => indexed,
                    provenance => put_method
                },
                ok = beamtalk_xref:put_method('Counter', false, 'value', BareEntry),

                %% The row lands under 'value', not under whatever was in the map.
                ?assert(lists:member('value', beamtalk_xref:defined_selectors('Counter', false))),
                FooSites = beamtalk_xref:senders_of('foo'),
                ?assertEqual(1, length(FooSites)),
                [FooSite] = FooSites,
                ?assertEqual('value', maps:get(method, FooSite)),
                ?assertEqual(false, maps:get(class_side, FooSite))
            end)
        ]
    end}.

%%====================================================================
%% method_info reads — current generation wins on stale bag rows
%%====================================================================

method_info_picks_highest_generation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% `?METHODS_TABLE` is a bag; `register_class` does NOT purge
                %% before inserting under the new gen, so re-registering a
                %% class without an intervening `purge_class/1` accumulates
                %% rows from older generations. `method_info/3` must pick the
                %% row carrying the highest `gen` so live patches always win.
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                Info1 = beamtalk_xref:method_info('Counter', false, 'increment'),
                ?assertEqual(1, maps:get(gen, Info1)),
                ?assertEqual(14, maps:get(line, Info1)),

                %% Re-register with the same selector at a different line; the
                %% old row at line 8 (gen 1) stays in the bag alongside the new
                %% gen-2 row at line 42. `method_info/3` must return the gen-2
                %% row, not whichever row ETS picks first.
                Counter2 = lists:map(
                    fun
                        (#{selector := 'increment'} = E) ->
                            E#{line => 42};
                        (E) ->
                            E
                    end,
                    counter_xref()
                ),
                ok = beamtalk_xref:register_class('Counter', Counter2),
                Info2 = beamtalk_xref:method_info('Counter', false, 'increment'),
                ?assertEqual(2, maps:get(gen, Info2)),
                ?assertEqual(42, maps:get(line, Info2)),

                %% Sanity: the bag really does still hold both generations
                %% — the read picks the higher one rather than relying on a
                %% prior purge.
                Rows = ets:lookup(beamtalk_xref_methods, {'Counter', false, 'increment'}),
                ?assertEqual(2, length(Rows)),

                %% Unknown method → undefined.
                ?assertEqual(
                    undefined,
                    beamtalk_xref:method_info('Counter', false, 'no_such_method')
                ),
                %% Unknown class → undefined.
                ?assertEqual(
                    undefined,
                    beamtalk_xref:method_info('NoSuchClass', false, 'increment')
                )
            end)
        ]
    end}.

%%====================================================================
%% Concurrent reader during write
%%====================================================================

concurrent_reader_during_write_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 30,
                ?_test(begin
                    %% Seed the table so the reader always has something to
                    %% observe — even before any writer's first call lands.
                    ok = beamtalk_xref:register_class('Counter', counter_xref()),

                    Parent = self(),
                    Writers = 4,
                    Iterations = 50,

                    %% Spawn writers that repeatedly register_class — each call
                    %% bumps the per-class gen and re-inserts rows.
                    WriterPids = [
                        spawn_link(fun() ->
                            lists:foreach(
                                fun(_) ->
                                    ok = beamtalk_xref:register_class('Counter', counter_xref())
                                end,
                                lists:seq(1, Iterations)
                            ),
                            Parent ! {writer_done, self()}
                        end)
                     || _ <- lists:seq(1, Writers)
                    ],

                    %% Reader runs concurrently and must see a non-empty,
                    %% well-formed view at every observation. Phase 1
                    %% guarantees `register_class` always leaves rows in the
                    %% table (delete-then-reinsert under a single gen_server
                    %% call would expose a gap; we instead just insert under
                    %% a new gen — old rows stay until purge), so the reader
                    %% never sees an empty Counter view between writes.
                    ReaderPid = spawn_link(fun() -> reader_loop(Parent) end),

                    lists:foreach(
                        fun(_) ->
                            receive
                                {writer_done, _} -> ok
                            after 10000 ->
                                ?assert(false)
                            end
                        end,
                        WriterPids
                    ),

                    %% Tell the reader the writers are done; it should also
                    %% finish soon after.
                    ReaderPid ! stop,
                    receive
                        {reader_done, Count, Errors} ->
                            ?assert(Count > 0),
                            ?assertEqual([], Errors)
                    after 10000 ->
                        ?assert(false)
                    end,

                    %% Final consistency check: the table is non-empty and gen
                    %% is at least 1 (in fact >= Writers*Iterations + 1, but
                    %% we don't assume that to keep the test robust).
                    ?assert(current_gen('Counter') >= 1),
                    ?assertNotEqual([], beamtalk_xref:defined_selectors('Counter', false))
                end)}
        ]
    end}.

reader_loop(Parent) ->
    reader_loop(Parent, 0, []).

reader_loop(Parent, Count, Errors) ->
    receive
        stop ->
            Parent ! {reader_done, Count, lists:reverse(Errors)}
    after 0 ->
        NewErrors =
            case check_counter_view() of
                ok -> Errors;
                {error, Reason} -> [Reason | Errors]
            end,
        reader_loop(Parent, Count + 1, NewErrors)
    end.

check_counter_view() ->
    %% Snapshot the four read APIs and assert they agree internally.
    %% The Phase 1 invariant: once a class is registered, subsequent
    %% writers under that class never observe an empty view because each
    %% register_class call appends new rows under a higher gen before
    %% the supervisor sweep (Phase 4) runs.
    InstSels = beamtalk_xref:defined_selectors('Counter', false),
    ClassSels = beamtalk_xref:defined_selectors('Counter', true),
    Impl = beamtalk_xref:implementors_of('increment'),
    PlusSites = beamtalk_xref:senders_of('+'),
    IntRefs = beamtalk_xref:references_to('Integer'),

    HasIncrement = lists:member('increment', InstSels),
    HasValue = lists:member('value', InstSels),
    HasNew = lists:member('new', ClassSels),
    HasImpl = lists:member({'Counter', false}, Impl),
    HasPlusSite = lists:any(
        fun(S) -> maps:get(owner, S, undefined) =:= 'Counter' end,
        PlusSites
    ),
    HasIntRef = lists:any(
        fun(S) -> maps:get(owner, S, undefined) =:= 'Counter' end,
        IntRefs
    ),

    case
        HasIncrement andalso HasValue andalso HasNew andalso
            HasImpl andalso HasPlusSite andalso HasIntRef
    of
        true ->
            ok;
        false ->
            {error, #{
                instance_selectors => InstSels,
                class_selectors => ClassSels,
                implementors => Impl,
                plus_sites_count => length(PlusSites),
                int_refs_count => length(IntRefs)
            }}
    end.

%%====================================================================
%% purge_method isolation (BT-2301)
%%====================================================================

purge_method_removes_one_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),

                %% Sanity: increment is present and sends '+'; sibling 'value' present.
                ?assert(
                    lists:member('increment', beamtalk_xref:defined_selectors('Counter', false))
                ),
                ?assertEqual(1, length(beamtalk_xref:senders_of('+'))),
                ?assertEqual(1, length(beamtalk_xref:references_to('Integer'))),

                %% Purge just `increment`.
                ok = beamtalk_xref:purge_method('Counter', false, 'increment'),

                %% `increment` gone — method row, its '+' send, and its Integer ref.
                ?assertEqual([], beamtalk_xref:implementors_of('increment')),
                ?assertNot(
                    lists:member('increment', beamtalk_xref:defined_selectors('Counter', false))
                ),
                ?assertEqual([], beamtalk_xref:senders_of('+')),
                ?assertEqual([], beamtalk_xref:references_to('Integer')),

                %% Siblings untouched: instance `value`, class-side `new` + its
                %% `basicNew` send all survive.
                ?assert(
                    lists:member('value', beamtalk_xref:defined_selectors('Counter', false))
                ),
                ?assertEqual(['new'], beamtalk_xref:defined_selectors('Counter', true)),
                ?assertEqual(1, length(beamtalk_xref:senders_of('basicNew')))
            end)
        ]
    end}.

purge_method_unknown_is_noop_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                %% Purging an unknown method / class is a harmless no-op.
                ?assertEqual(ok, beamtalk_xref:purge_method('Counter', false, 'no_such_method')),
                ?assertEqual(ok, beamtalk_xref:purge_method('NoSuchClass', false, 'increment')),
                %% Existing rows intact.
                ?assert(
                    lists:member('increment', beamtalk_xref:defined_selectors('Counter', false))
                )
            end)
        ]
    end}.

%%====================================================================
%% build_method_entry/5 — runtime xref-entry construction (BT-2301)
%%====================================================================

build_method_entry_sourceless_test_() ->
    [
        ?_test(begin
            %% A sourceless (`unindexed_runtime_fun`) entry carries empty sends
            %% and references, regardless of whether the compiler app is loaded.
            Entry = beamtalk_xref:build_method_entry(
                false, 'doubled', <<>>, unindexed_runtime_fun, extension
            ),
            ?assertEqual(false, maps:get(class_side, Entry)),
            ?assertEqual('doubled', maps:get(selector, Entry)),
            ?assertEqual([], maps:get(sends, Entry)),
            ?assertEqual([], maps:get(references, Entry)),
            ?assertEqual(unindexed_runtime_fun, maps:get(source_status, Entry)),
            ?assertEqual(extension, maps:get(provenance, Entry))
        end)
    ].

build_method_entry_indexed_shape_test_() ->
    [
        ?_test(begin
            %% An `indexed` entry always has the canonical shape. When the
            %% compiler app is present the sends are parsed from source; when it
            %% is absent the call degrades to empty sends rather than failing.
            Entry = beamtalk_xref:build_method_entry(
                true, 'increment', <<"increment => self count + 1">>, indexed, put_method
            ),
            ?assertEqual(true, maps:get(class_side, Entry)),
            ?assertEqual('increment', maps:get(selector, Entry)),
            ?assertEqual(1, maps:get(line, Entry)),
            ?assertEqual(indexed, maps:get(source_status, Entry)),
            ?assertEqual(put_method, maps:get(provenance, Entry)),
            ?assert(is_list(maps:get(sends, Entry))),
            ?assertEqual([], maps:get(references, Entry))
        end)
    ].

%%====================================================================
%% Internal helpers
%%====================================================================

current_gen(Class) ->
    case ets:lookup(xref_class_gen, Class) of
        [] -> 0;
        [{_, Gen}] -> Gen
    end.
