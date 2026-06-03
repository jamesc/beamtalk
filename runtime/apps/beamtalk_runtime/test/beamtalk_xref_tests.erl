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

%% Logger handler callback used by the miss-policy test to capture warnings.
-export([log/2]).

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

%% ADR 0087 Phase 6 (BT-2304): a value class's `method_xref` payload as codegen
%% now emits it — the hand-written method plus the compiler-generated
%% auto-accessors for the `value` slot. The synthetic getter / setter carry
%% `source_status => synthetic` and a `synthetic_origin` line pointing at the
%% generating slot declaration, and ride the same write path as the indexed row.
value_class_with_synthetic_xref() ->
    [
        #{
            class_side => false,
            selector => 'doubled',
            line => 5,
            sends => [#{selector => '+', line => 6, recv_kind => self_recv}],
            references => [],
            source_status => indexed,
            provenance => class_body
        },
        #{
            class_side => false,
            selector => 'value',
            line => 2,
            sends => [],
            references => [#{class => 'Integer', line => 2}],
            source_status => synthetic,
            synthetic_origin => 2,
            provenance => class_body
        },
        #{
            class_side => false,
            selector => 'withValue:',
            line => 2,
            sends => [],
            references => [#{class => 'Integer', line => 2}],
            source_status => synthetic,
            synthetic_origin => 2,
            provenance => class_body
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
%% ADR 0087 Phase 6 (BT-2304): synthetic auto-accessor parity
%%====================================================================

synthetic_accessor_visibility_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class(
                    'Boxed', value_class_with_synthetic_xref()
                ),

                %% Parity exception: the synthetic `value` getter IS an
                %% implementor of `#value` — non-empty where a source-scan query
                %% returned empty pre-index (the auto-accessor has no source).
                ?assertEqual([{'Boxed', false}], beamtalk_xref:implementors_of('value')),
                ?assertEqual(
                    [{'Boxed', false}], beamtalk_xref:implementors_of('withValue:')
                ),

                %% Synthetic accessors are defined selectors like any other.
                Defined = lists:sort(beamtalk_xref:defined_selectors('Boxed', false)),
                ?assertEqual(['doubled', 'value', 'withValue:'], Defined),

                %% The synthetic rows are tagged `synthetic` (the filterable
                %% parity marker), distinct from the hand-written `indexed` row.
                ValueInfo = beamtalk_xref:method_info('Boxed', false, 'value'),
                ?assertEqual(synthetic, maps:get(source_status, ValueInfo)),
                SetterInfo = beamtalk_xref:method_info('Boxed', false, 'withValue:'),
                ?assertEqual(synthetic, maps:get(source_status, SetterInfo)),
                DoubledInfo = beamtalk_xref:method_info('Boxed', false, 'doubled'),
                ?assertEqual(indexed, maps:get(source_status, DoubledInfo)),

                %% The synthetic getter's `line` is the derived origin (the slot
                %% declaration line) so LSP / System Browser can navigate to it.
                ?assertEqual(2, maps:get(line, ValueInfo)),

                %% The slot type reference (`Integer`) rides along: the synthetic
                %% accessor contributes a reference exactly like a hand-written
                %% typed accessor would.
                IntRefs = beamtalk_xref:references_to('Integer'),
                IntMethods = lists:sort([maps:get(method, R) || R <- IntRefs]),
                ?assertEqual(['value', 'withValue:'], IntMethods),

                %% Synthetic accessors delegate to runtime map primitives, so they
                %% contribute no senders — `value` / `withValue:` send nothing.
                ?assertEqual([], beamtalk_xref:senders_of('value'))
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

                %% put_method is a surgical single-method patch and must NOT
                %% bump the class generation (Phase 4 / BT-2300) — a bump would
                %% strand the unbumped sibling methods behind the reader's
                %% current-gen filter. The patched method joins the class's
                %% current generation, leaving it unchanged at 2.
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
                ?assertEqual(2, current_gen('Counter')),

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

method_info_picks_current_generation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% `?METHODS_TABLE` is a bag; `register_class` inserts under a
                %% new gen and publishes it before the async old-gen sweep
                %% runs, so a window exists where rows from two generations
                %% coexist. `method_info/3` must return the row carrying the
                %% class's *current* gen, never a stale one (Phase 4 / BT-2300).
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                Info1 = beamtalk_xref:method_info('Counter', false, 'increment'),
                ?assertEqual(1, maps:get(gen, Info1)),
                ?assertEqual(14, maps:get(line, Info1)),

                %% Re-register the same selector at a different line. Before the
                %% async sweep lands, the old line-14 (gen 1) row can still sit
                %% in the bag beside the new line-42 (gen 2) row. `method_info/3`
                %% must return the gen-2 row regardless of ETS bag order.
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

                %% After the async sweep drains, only the current-gen row
                %% survives in the bag (memory hygiene, ADR 0087 §Atomicity
                %% step 4). Flush the gen_server mailbox so the sweep cast has
                %% certainly been processed before asserting.
                flush_xref(),
                Rows = ets:lookup(beamtalk_xref_methods, {'Counter', false, 'increment'}),
                ?assertEqual(1, length(Rows)),
                [{_, OnlyInfo}] = Rows,
                ?assertEqual(2, maps:get(gen, OnlyInfo)),
                ?assertEqual(42, maps:get(line, OnlyInfo)),

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
%% Generation filtering on the read path (Phase 4 / BT-2300)
%%====================================================================

%% A re-register that drops a method/send/reference must not leave the
%% dropped item visible through any list reader, even before the async
%% old-gen sweep has run. Correctness comes from the reader's current-gen
%% filter, not from the sweep — so this test deliberately does NOT flush.
read_path_filters_stale_generation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Gen 1: increment sends '+' and references Integer; value and
                %% class-side new also present.
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                ?assertEqual(1, length(beamtalk_xref:senders_of('+'))),
                ?assertEqual(1, length(beamtalk_xref:references_to('Integer'))),
                ?assertEqual([{'Counter', false}], beamtalk_xref:implementors_of('value')),
                ?assert(lists:member('value', beamtalk_xref:defined_selectors('Counter', false))),

                %% Gen 2: re-register WITHOUT the `value` method, with
                %% `increment` now sending '-' (not '+') and referencing Float
                %% (not Integer). The gen-1 rows linger in the bag until the
                %% async sweep, but no reader may surface them.
                Gen2Xref = [
                    #{
                        class_side => false,
                        selector => 'increment',
                        line => 14,
                        sends => [#{selector => '-', line => 17, recv_kind => self_recv}],
                        references => [#{class => 'Float', line => 16}],
                        source_status => indexed,
                        provenance => class_body
                    },
                    #{
                        class_side => true,
                        selector => 'new',
                        line => 8,
                        sends => [#{selector => 'basicNew', line => 9, recv_kind => super_recv}],
                        references => [],
                        source_status => indexed,
                        provenance => class_body
                    }
                ],
                ok = beamtalk_xref:register_class('Counter', Gen2Xref),

                %% senders_of: the dropped '+' send is gone; the new '-' is live.
                ?assertEqual([], beamtalk_xref:senders_of('+')),
                MinusSites = beamtalk_xref:senders_of('-'),
                ?assertEqual(1, length(MinusSites)),
                [MinusSite] = MinusSites,
                ?assertEqual(2, maps:get(gen, MinusSite)),

                %% references_to: Integer dropped, Float live.
                ?assertEqual([], beamtalk_xref:references_to('Integer')),
                ?assertEqual(1, length(beamtalk_xref:references_to('Float'))),

                %% implementors_of: the removed `value` no longer reports Counter.
                ?assertEqual([], beamtalk_xref:implementors_of('value')),
                ?assertEqual([{'Counter', false}], beamtalk_xref:implementors_of('increment')),

                %% defined_selectors: `value` dropped, `increment` retained.
                InstSels = beamtalk_xref:defined_selectors('Counter', false),
                ?assertEqual(['increment'], InstSels),
                ?assertEqual(['new'], beamtalk_xref:defined_selectors('Counter', true))
            end)
        ]
    end}.

%% The async sweep eventually reclaims the stale-gen rows from every table.
%% Filtering already hides them; this asserts the memory-hygiene step.
async_sweep_reclaims_old_generation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                ok = beamtalk_xref:register_class('Counter', counter_xref()),
                ok = beamtalk_xref:register_class('Counter', counter_xref()),

                %% Drain the sweep casts.
                flush_xref(),

                %% Only the current generation (3) survives in every table.
                MethodRows = ets:tab2list(beamtalk_xref_methods),
                CounterMethodGens = [
                    maps:get(gen, Info)
                 || {{'Counter', _, _}, Info} <- MethodRows
                ],
                ?assertEqual([3], lists:usort(CounterMethodGens)),

                SenderGens = [
                    maps:get(gen, Site)
                 || {_, Site} <- ets:tab2list(beamtalk_xref_senders),
                    maps:get(owner, Site) =:= 'Counter'
                ],
                ?assertEqual([3], lists:usort(SenderGens)),

                RefGens = [
                    maps:get(gen, Site)
                 || {_, Site} <- ets:tab2list(beamtalk_xref_references),
                    maps:get(owner, Site) =:= 'Counter'
                ],
                ?assertEqual([3], lists:usort(RefGens)),

                %% The view is still complete after the sweep.
                ?assertEqual(1, length(beamtalk_xref:senders_of('+'))),
                ?assert(lists:member('value', beamtalk_xref:defined_selectors('Counter', false)))
            end)
        ]
    end}.

%% A never-registered class patched via put_method/4 establishes generation 1
%% and the patched method is immediately visible through the read path — the
%% reader's current-gen filter must not hide it.
put_method_on_fresh_class_is_visible_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Entry = #{
                    class_side => false,
                    selector => 'ping',
                    line => 3,
                    sends => [#{selector => 'pong', line => 4, recv_kind => self_recv}],
                    references => [#{class => 'Boolean', line => 5}],
                    source_status => indexed,
                    provenance => put_method
                },
                ok = beamtalk_xref:put_method('Fresh', false, 'ping', Entry),

                ?assertEqual(1, current_gen('Fresh')),
                ?assert(lists:member('ping', beamtalk_xref:defined_selectors('Fresh', false))),
                ?assertEqual([{'Fresh', false}], beamtalk_xref:implementors_of('ping')),
                ?assertEqual(1, length(beamtalk_xref:senders_of('pong'))),
                ?assertEqual(1, length(beamtalk_xref:references_to('Boolean'))),
                #{owner := 'Fresh'} = beamtalk_xref:method_info('Fresh', false, 'ping')
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
%% Miss-policy fallback (ADR 0087 Phase 3, BT-2299)
%%====================================================================

%% A registry-loaded class that is artificially purged from the index must
%% (1) surface in the `fallback_classes` partition of `senders_of_bt/1` so the
%% BT layer source-scans it, and (2) emit exactly one `xref_miss` warning for
%% that class. Runs against the live runtime app so `beamtalk_class_registry`
%% reports the bootstrap stub classes as loaded.
miss_policy_fallback_test_() ->
    {setup, fun setup_app/0, fun cleanup_app/1, fun(_) ->
        {timeout, 30,
            ?_test(begin
                %% Pick a loaded class that actually defines methods — the miss
                %% policy only flags such classes (empty base/protocol classes have
                %% nothing to scan). At least one method-bearing class must be
                %% loaded to exercise the policy.
                Class = first_method_bearing_class(),
                ?assertNotEqual(undefined, Class),

                %% Ensure the chosen class is indexed (re-register a stub row so the
                %% test does not depend on cross-fixture table state). While indexed
                %% it must *not* appear as a fallback class.
                StubRows = [
                    #{
                        class_side => false,
                        selector => 'isMeta',
                        line => 1,
                        sends => [],
                        references => [],
                        source_status => unindexed_runtime_fun,
                        provenance => class_body
                    }
                ],
                ok = beamtalk_xref:register_class(Class, StubRows),
                #{fallback_classes := Fallback0} = beamtalk_xref:senders_of_bt('isMeta'),
                ?assertNot(lists:member(Class, Fallback0)),

                %% Artificially purge the chosen class from the index — it is now
                %% loaded but unindexed, the exact miss the policy must catch.
                ok = beamtalk_xref:purge_class(Class),

                HandlerId = install_capture_handler(),
                try
                    #{fallback_classes := Fallback1} = beamtalk_xref:senders_of_bt('isMeta'),
                    %% (1) The purged-but-loaded class is reported for source-scan.
                    ?assert(lists:member(Class, Fallback1)),

                    %% (2) Exactly one matching xref_miss warning for that class.
                    Misses = collect_xref_misses(Class),
                    ?assertEqual(1, length(Misses)),
                    [Meta] = Misses,
                    ?assertEqual(xref_miss, maps:get(reason, Meta)),
                    ?assertEqual(sendersOf, maps:get(query, Meta)),
                    ?assertEqual([beamtalk, runtime], maps:get(domain, Meta))
                after
                    remove_capture_handler(HandlerId),
                    %% Restore the purged class so later tests sharing this VM do
                    %% not inherit the artificially broken (loaded-but-unindexed)
                    %% state and start seeing fallback scans / xref_miss warnings.
                    ok = beamtalk_xref:register_class(Class, StubRows)
                end
            end)}
    end}.

%%====================================================================
%% Miss-policy fallback for referencesTo: / implementorsOf: /
%% selectorsMatching: (ADR 0087 Phase 5, BT-2302)
%%====================================================================

%% The three navigation queries migrated in BT-2302 share the `senders_of_bt/1`
%% partition shape: while a method-bearing class is indexed its rows come from
%% the index and it never appears as a fallback class; once artificially purged
%% (loaded-but-unindexed) it falls back and emits exactly one `xref_miss`
%% warning tagged with the right query name. This exercises all three new
%% `_bt/0,1` read APIs against the live runtime app.
nav_query_miss_policy_test_() ->
    {setup, fun setup_app/0, fun cleanup_app/1, fun(_) ->
        {timeout, 30,
            ?_test(begin
                Class = first_method_bearing_class(),
                ?assertNotEqual(undefined, Class),

                %% Stub rows: one instance-side method that references `Integer`,
                %% plus one class-side method. Gives every query something to
                %% match while indexed: a reference (referencesTo:), implementors
                %% on both sides (implementorsOf:), and named selectors
                %% (selectorsMatching:).
                StubRows = [
                    #{
                        class_side => false,
                        selector => 'navProbeInstance',
                        line => 1,
                        sends => [],
                        references => [#{class => 'Integer', line => 1}],
                        source_status => unindexed_runtime_fun,
                        provenance => class_body
                    },
                    #{
                        class_side => true,
                        selector => 'navProbeClassSide',
                        line => 1,
                        sends => [],
                        references => [],
                        source_status => unindexed_runtime_fun,
                        provenance => class_body
                    }
                ],
                ok = beamtalk_xref:register_class(Class, StubRows),

                %% --- While indexed: rows come from the index, no fallback. ---
                #{indexed := RefIndexed, fallback_classes := RefFb0} =
                    beamtalk_xref:references_to_bt('Integer'),
                ?assertNot(lists:member(Class, RefFb0)),
                ?assert(
                    lists:any(
                        fun(R) ->
                            maps:get(owner, R) =:= Class andalso
                                maps:get(method, R) =:= 'navProbeInstance'
                        end,
                        RefIndexed
                    )
                ),

                #{indexed := ImplInst, fallback_classes := ImplFb0} =
                    beamtalk_xref:implementors_of_bt('navProbeInstance'),
                ?assertNot(lists:member(Class, ImplFb0)),
                ?assert(
                    lists:member(#{owner => Class, class_side => false}, ImplInst)
                ),
                #{indexed := ImplCls} =
                    beamtalk_xref:implementors_of_bt('navProbeClassSide'),
                ?assert(
                    lists:member(#{owner => Class, class_side => true}, ImplCls)
                ),

                #{indexed := SelRows, fallback_classes := SelFb0} =
                    beamtalk_xref:defined_selectors_bt(),
                ?assertNot(lists:member(Class, SelFb0)),
                ?assert(
                    lists:member(
                        #{owner => Class, class_side => false, selector => 'navProbeInstance'},
                        SelRows
                    )
                ),

                %% --- Artificially purge: now loaded-but-unindexed. ---
                ok = beamtalk_xref:purge_class(Class),

                HandlerId = install_capture_handler(),
                try
                    #{fallback_classes := RefFb1} =
                        beamtalk_xref:references_to_bt('Integer'),
                    ?assert(lists:member(Class, RefFb1)),
                    assert_single_miss(Class, referencesTo),

                    #{fallback_classes := ImplFb1} =
                        beamtalk_xref:implementors_of_bt('navProbeInstance'),
                    ?assert(lists:member(Class, ImplFb1)),
                    assert_single_miss(Class, implementorsOf),

                    #{fallback_classes := SelFb1} =
                        beamtalk_xref:defined_selectors_bt(),
                    ?assert(lists:member(Class, SelFb1)),
                    assert_single_miss(Class, selectorsMatching)
                after
                    remove_capture_handler(HandlerId),
                    %% Restore so later tests in the same VM see a clean index.
                    ok = beamtalk_xref:register_class(Class, StubRows)
                end
            end)}
    end}.

%% The CodeRabbit finding deferred from BT-2299 to BT-2300: the `indexed`
%% partition of senders_of_bt/1 must be filtered to the live generation so a
%% re-register never exposes a stale sender row. Re-register a loaded class
%% twice with a *different* send selector each generation and assert the
%% partition only ever reflects the current generation.
senders_of_bt_indexed_is_gen_filtered_test_() ->
    {setup, fun setup_app/0, fun cleanup_app/1, fun(_) ->
        {timeout, 30,
            ?_test(begin
                Class = first_method_bearing_class(),
                ?assertNotEqual(undefined, Class),

                Gen1 = [
                    #{
                        class_side => false,
                        selector => 'someSel',
                        line => 1,
                        sends => [#{selector => 'genOneSend', line => 2, recv_kind => other}],
                        references => [],
                        source_status => indexed,
                        provenance => class_body
                    }
                ],
                ok = beamtalk_xref:register_class(Class, Gen1),
                try
                    %% Gen 1 send is visible in the indexed partition.
                    #{indexed := Idx1} = beamtalk_xref:senders_of_bt('genOneSend'),
                    ?assert(lists:any(fun(R) -> maps:get(owner, R) =:= Class end, Idx1)),

                    %% Re-register the class WITHOUT the gen-1 send (it now sends
                    %% `genTwoSend` instead). The gen-1 sender row lingers in the
                    %% bag until the async sweep, but the gen filter inside
                    %% senders_of/1 must keep it out of the indexed partition.
                    Gen2 = [
                        #{
                            class_side => false,
                            selector => 'someSel',
                            line => 1,
                            sends => [
                                #{selector => 'genTwoSend', line => 2, recv_kind => other}
                            ],
                            references => [],
                            source_status => indexed,
                            provenance => class_body
                        }
                    ],
                    ok = beamtalk_xref:register_class(Class, Gen2),

                    %% Stale gen-1 send no longer surfaces for this class...
                    #{indexed := Idx2} = beamtalk_xref:senders_of_bt('genOneSend'),
                    ?assertNot(lists:any(fun(R) -> maps:get(owner, R) =:= Class end, Idx2)),
                    %% ...and the current gen-2 send does.
                    #{indexed := Idx3} = beamtalk_xref:senders_of_bt('genTwoSend'),
                    ?assert(lists:any(fun(R) -> maps:get(owner, R) =:= Class end, Idx3))
                after
                    %% Drop the synthetic rows so sibling tests are unaffected.
                    ok = beamtalk_xref:purge_class(Class)
                end
            end)}
    end}.

%%====================================================================
%% Composite-query aggregate readers: all_sends_bt/0 + all_sent_selectors_bt/0
%% (ADR 0087 Phase 5, BT-2303)
%%====================================================================

%% The two composite queries (`unimplementedSelectors` / `unusedSelectors`) read
%% the whole-universe aggregate readers. While a class is indexed its sends come
%% from the index (carrying the sent selector, the containing-method site, and
%% the FFI-facing recv tag); once artificially purged it falls back and emits one
%% `xref_miss` per reader tagged with the right query name. Erlang-FFI sends are
%% dropped from the sent-selector set but kept (tagged) in the send-row stream.
composite_query_aggregate_readers_test_() ->
    {setup, fun setup_app/0, fun cleanup_app/1, fun(_) ->
        {timeout, 30,
            ?_test(begin
                Class = first_method_bearing_class(),
                ?assertNotEqual(undefined, Class),

                %% One instance method that sends a normal selector (self-recv),
                %% an erlang-ffi "send", and a super-recv send. The send-row
                %% stream must carry all three with mapped recv tags; the
                %% sent-selector set must include the normal + super ones but
                %% NOT the FFI one.
                StubRows = [
                    #{
                        class_side => false,
                        selector => 'navAggMethod',
                        line => 1,
                        sends => [
                            #{selector => 'navAggNormalSend', line => 2, recv_kind => self_recv},
                            #{selector => 'navAggFfiSend', line => 3, recv_kind => erlang_ffi},
                            #{selector => 'navAggSuperSend', line => 4, recv_kind => super_recv}
                        ],
                        references => [],
                        source_status => indexed,
                        provenance => class_body
                    }
                ],
                ok = beamtalk_xref:register_class(Class, StubRows),
                try
                    %% --- all_sends_bt/0: every send carried with site + recv. ---
                    #{indexed := SendRows, fallback_classes := SendFb0} =
                        beamtalk_xref:all_sends_bt(),
                    ?assertNot(lists:member(Class, SendFb0)),
                    OurSends = [
                        R
                     || R <- SendRows, maps:get(owner, R) =:= Class
                    ],
                    %% The normal send: sent selector + site fields + mapped recv.
                    ?assert(
                        lists:any(
                            fun(R) ->
                                maps:get(sent, R) =:= 'navAggNormalSend' andalso
                                    maps:get(method, R) =:= 'navAggMethod' andalso
                                    maps:get(class_side, R) =:= false andalso
                                    maps:get(line, R) =:= 2 andalso
                                    maps:get(recv, R) =:= self
                            end,
                            OurSends
                        )
                    ),
                    %% The FFI send is present and tagged #erlang_ffi (BT applies
                    %% the exclusion).
                    ?assert(
                        lists:any(
                            fun(R) ->
                                maps:get(sent, R) =:= 'navAggFfiSend' andalso
                                    maps:get(recv, R) =:= erlang_ffi
                            end,
                            OurSends
                        )
                    ),
                    %% The super send maps super_recv -> super.
                    ?assert(
                        lists:any(
                            fun(R) ->
                                maps:get(sent, R) =:= 'navAggSuperSend' andalso
                                    maps:get(recv, R) =:= super
                            end,
                            OurSends
                        )
                    ),

                    %% --- all_sent_selectors_bt/0: distinct set, FFI excluded. ---
                    #{indexed := SentSels, fallback_classes := SentFb0} =
                        beamtalk_xref:all_sent_selectors_bt(),
                    ?assertNot(lists:member(Class, SentFb0)),
                    ?assert(lists:member('navAggNormalSend', SentSels)),
                    ?assert(lists:member('navAggSuperSend', SentSels)),
                    %% The FFI send's "selector" is an Erlang function name, not a
                    %% Beamtalk selector — it must not appear in the sent set.
                    ?assertNot(lists:member('navAggFfiSend', SentSels)),

                    %% --- Artificially purge: now loaded-but-unindexed. ---
                    ok = beamtalk_xref:purge_class(Class),

                    HandlerId = install_capture_handler(),
                    try
                        #{fallback_classes := SendFb1} = beamtalk_xref:all_sends_bt(),
                        ?assert(lists:member(Class, SendFb1)),
                        assert_single_miss(Class, unimplementedSelectors),

                        #{fallback_classes := SentFb1} =
                            beamtalk_xref:all_sent_selectors_bt(),
                        ?assert(lists:member(Class, SentFb1)),
                        assert_single_miss(Class, unusedSelectors)
                    after
                        remove_capture_handler(HandlerId)
                    end
                after
                    %% Drop the synthetic rows so sibling tests are unaffected.
                    ok = beamtalk_xref:purge_class(Class)
                end
            end)}
    end}.

%% The aggregate readers' indexed partitions must be gen-filtered: a re-register
%% under a new generation must not leak a stale send row (mirrors the deferred
%% CodeRabbit finding for senders_of_bt/1).
all_sends_bt_indexed_is_gen_filtered_test_() ->
    {setup, fun setup_app/0, fun cleanup_app/1, fun(_) ->
        {timeout, 30,
            ?_test(begin
                Class = first_method_bearing_class(),
                ?assertNotEqual(undefined, Class),

                Gen1 = [
                    #{
                        class_side => false,
                        selector => 'aggGenSel',
                        line => 1,
                        sends => [#{selector => 'aggGenOneSend', line => 2, recv_kind => other}],
                        references => [],
                        source_status => indexed,
                        provenance => class_body
                    }
                ],
                ok = beamtalk_xref:register_class(Class, Gen1),
                try
                    #{indexed := S1} = beamtalk_xref:all_sends_bt(),
                    ?assert(
                        lists:any(
                            fun(R) ->
                                maps:get(owner, R) =:= Class andalso
                                    maps:get(sent, R) =:= 'aggGenOneSend'
                            end,
                            S1
                        )
                    ),

                    Gen2 = [
                        #{
                            class_side => false,
                            selector => 'aggGenSel',
                            line => 1,
                            sends => [
                                #{selector => 'aggGenTwoSend', line => 2, recv_kind => other}
                            ],
                            references => [],
                            source_status => indexed,
                            provenance => class_body
                        }
                    ],
                    ok = beamtalk_xref:register_class(Class, Gen2),

                    #{indexed := S2} = beamtalk_xref:all_sends_bt(),
                    %% Stale gen-1 send is filtered out...
                    ?assertNot(
                        lists:any(
                            fun(R) ->
                                maps:get(owner, R) =:= Class andalso
                                    maps:get(sent, R) =:= 'aggGenOneSend'
                            end,
                            S2
                        )
                    ),
                    %% ...and the current gen-2 send is present.
                    ?assert(
                        lists:any(
                            fun(R) ->
                                maps:get(owner, R) =:= Class andalso
                                    maps:get(sent, R) =:= 'aggGenTwoSend'
                            end,
                            S2
                        )
                    ),

                    %% The sent-selector set agrees with the gen filter.
                    #{indexed := SentSels} = beamtalk_xref:all_sent_selectors_bt(),
                    ?assertNot(lists:member('aggGenOneSend', SentSels)),
                    ?assert(lists:member('aggGenTwoSend', SentSels))
                after
                    ok = beamtalk_xref:purge_class(Class)
                end
            end)}
    end}.

%% Assert exactly one xref_miss warning for `Class` tagged with `Query`. Drains
%% the capture mailbox of misses for the class and checks the metadata. Each
%% call drains independently, so the three queries above are checked in turn.
assert_single_miss(Class, Query) ->
    Misses = [M || M <- collect_xref_misses(Class), maps:get(query, M) =:= Query],
    ?assertEqual(1, length(Misses)),
    [Meta] = Misses,
    ?assertEqual(xref_miss, maps:get(reason, Meta)),
    ?assertEqual(Query, maps:get(query, Meta)),
    ?assertEqual([beamtalk, runtime], maps:get(domain, Meta)).
setup_app() ->
    %% The miss policy needs both a live class registry (to report a class as
    %% loaded) and a running xref server. Bring up the full runtime app when we
    %% can; an earlier fixture in the same VM may have started xref bare, which
    %% makes the supervised app start fail with `already_started` and skips
    %% bootstrap's class registration. In every case, end by guaranteeing the
    %% bootstrap stub classes are registered so at least one class is loaded.
    _ = application:ensure_all_started(beamtalk_runtime),
    ensure_xref_running(),
    ensure_pg_running(),
    register_stub_classes(),
    %% Give registration a beat to land in pg.
    timer:sleep(200),
    ok.

ensure_xref_running() ->
    case whereis(beamtalk_xref) of
        undefined ->
            {ok, _} = beamtalk_xref:start_link(),
            ok;
        _ ->
            ok
    end.

ensure_pg_running() ->
    case whereis(pg) of
        undefined ->
            {ok, _} = pg:start_link(),
            ok;
        _ ->
            ok
    end.

%% Register the hand-coded bootstrap stub classes (the same calls
%% `beamtalk_bootstrap:init/1` makes). Idempotent — re-registering a live class
%% is a no-op at the registry level.
register_stub_classes() ->
    catch beamtalk_class_bt:register_class(),
    catch beamtalk_metaclass_bt:register_class(),
    ok.

cleanup_app(_) ->
    ok.

%% First loaded class that defines at least one method, or `undefined`.
first_method_bearing_class() ->
    Entries = beamtalk_class_registry:live_class_entries(),
    case [N || {N, _Mod, Pid} <- Entries, defines_methods(Pid)] of
        [] -> undefined;
        [First | _] -> First
    end.

defines_methods(Pid) ->
    try
        beamtalk_object_class:methods(Pid) =/= [] orelse
            beamtalk_object_class:local_class_methods(Pid) =/= []
    catch
        _:_ -> false
    end.

%% Drain the capture mailbox and return the metadata maps of every xref_miss
%% warning whose `class` field matches `Class`.
collect_xref_misses(Class) ->
    receive
        {captured_log, #{level := warning, msg := {report, Report}}} ->
            case Report of
                #{event := xref_miss, class := Class} ->
                    [Report | collect_xref_misses(Class)];
                _ ->
                    collect_xref_misses(Class)
            end;
        {captured_log, _Other} ->
            collect_xref_misses(Class)
    after 200 ->
        []
    end.

install_capture_handler() ->
    HandlerId = beamtalk_xref_test_capture,
    Self = self(),
    %% Ensure the handler module's callback is exported and loaded before the
    %% logger validates it at add time.
    _ = code:ensure_loaded(?MODULE),
    %% The test sys.config pins the primary logger level to `error` for clean
    %% output, which drops warnings before any handler sees them. Open the gate
    %% to `all` for the duration of the capture, remembering the old level so it
    %% can be restored.
    #{level := OldLevel} = logger:get_primary_config(),
    put(saved_primary_level, OldLevel),
    ok = logger:set_primary_config(level, all),
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        level => all,
        filter_default => log,
        capture_pid => Self
    }),
    HandlerId.

remove_capture_handler(HandlerId) ->
    logger:remove_handler(HandlerId),
    case get(saved_primary_level) of
        undefined -> ok;
        OldLevel -> logger:set_primary_config(level, OldLevel)
    end,
    ok.

%% Logger handler callback — forwards log events to the capture pid.
log(LogEvent, #{capture_pid := Pid}) ->
    Pid ! {captured_log, LogEvent},
    ok.

%%====================================================================
%% Internal helpers
%%====================================================================

current_gen(Class) ->
    case ets:lookup(xref_class_gen, Class) of
        [] -> 0;
        [{_, Gen}] -> Gen
    end.

%% Barrier that guarantees every async sweep cast enqueued by a prior write
%% has been processed. A `sys:get_state/1` is a synchronous call into the
%% gen_server, so it cannot be handled until the gen_server has drained all
%% earlier mailbox messages (the sweep casts) ahead of it.
flush_xref() ->
    _ = sys:get_state(beamtalk_xref),
    ok.
