%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_inspector_tests).

-moduledoc """
Unit tests for `beamtalk_inspector` — the ADR 0095 Phase 1 inspector shim.

Coverage:
- kind classification (`#value` for a tagged Value map, `#actor` for a pid)
- `#value` field derivation in ADR-0094 sort order, with drillability
- `at:` drilling into a child cursor (parent/path wired) and the
  `no_such_field` error Result on a miss
- `refresh` returning a fresh cursor (immutable original)
- the shared `beamtalk_process_navigation:guarded_state/1` guard: a live
  `sys`-compliant process yields `{ok, State}`; a dead pid yields `unavailable`
- an `#actor` over a live gen_server snapshots its state; a dead actor degrades
  to the single `#status => #unavailable` diagnostic field (never a crash)
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% A minimal sys-compliant gen_server whose state is a tagged Value-shaped map,
%% so the inspector's actor snapshot path can read user fields.
-export([init/1, handle_call/3, handle_cast/2]).

%%====================================================================
%% Test gen_server
%%====================================================================

%% Mark the process as a Beamtalk actor (BT-2503): classification keys on a
%% `'$beamtalk_actor'` process-dictionary entry, distinguishing a real actor
%% (`#actor`) from a foreign OTP process (`#foreign`). A `{foreign, State}` arg
%% starts without the marker, standing in for a foreign OTP gen_server.
init({foreign, State}) ->
    {ok, State};
init(State) ->
    erlang:put('$beamtalk_actor', 'TestActor'),
    {ok, State}.
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

%% Start a sys-compliant process, marked as a Beamtalk actor, whose state is the
%% given tagged map.
start_actor(State) ->
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    Pid.

%% Start a sys-compliant process that is NOT marked as a Beamtalk actor — a
%% stand-in for a foreign OTP gen_server (`#foreign`).
start_foreign(State) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {foreign, State}, []),
    Pid.

%% A Value-shaped tagged map (Point with x/y).
point(X, Y) ->
    #{'$beamtalk_class' => 'Point', x => X, y => Y}.

%%====================================================================
%% Kind classification
%%====================================================================

value_kind_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    ?assertEqual(value, beamtalk_inspector:kindOf(I)).

actor_kind_test() ->
    Pid = start_actor(point(1, 2)),
    I = beamtalk_inspector:on(Pid),
    ?assertEqual(actor, beamtalk_inspector:kindOf(I)),
    gen_server:stop(Pid).

root_cursor_has_no_parent_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    ?assertEqual(nil, beamtalk_inspector:parentOf(I)),
    ?assertEqual([], beamtalk_inspector:pathOf(I)).

%%====================================================================
%% #value field derivation (ADR 0094 sort order)
%%====================================================================

value_fields_sorted_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    Fields = beamtalk_inspector:fieldsOf(I),
    Names = [maps:get(name, F) || F <- Fields],
    ?assertEqual([x, y], Names).

value_field_values_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    [XF, YF] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(3, maps:get(value, XF)),
    ?assertEqual(4, maps:get(value, YF)).

scalar_field_not_drillable_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    [XF | _] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(false, maps:get(drillable, XF)).

nested_value_field_drillable_test() ->
    Nested = #{'$beamtalk_class' => 'Pair', a => point(1, 2), b => 9},
    I = beamtalk_inspector:on(Nested),
    Fields = beamtalk_inspector:fieldsOf(I),
    [AF] = [F || F <- Fields, maps:get(name, F) =:= a],
    ?assertEqual(true, maps:get(drillable, AF)).

field_records_tagged_inspectorfield_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    [XF | _] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual('InspectorField', maps:get('$beamtalk_class', XF)),
    ?assertEqual(slot, maps:get(kind, XF)).

%%====================================================================
%% Navigation: at: / parent / path
%%====================================================================

%% `inspector/2` returns the raw `{ok, Child} | {error, Err}` tuple; the FFI
%% boundary (`coerce_result/1`) lifts it to a Beamtalk `Result` at the call site.
at_drills_into_child_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {ok, Child} = beamtalk_inspector:inspector(I, x),
    ?assertEqual(3, beamtalk_inspector:subjectOf(Child)),
    ?assertEqual([x], beamtalk_inspector:pathOf(Child)).

at_child_parent_is_cursor_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {ok, Child} = beamtalk_inspector:inspector(I, x),
    ?assertEqual(I, beamtalk_inspector:parentOf(Child)).

at_miss_returns_no_such_field_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {error, Err} = beamtalk_inspector:inspector(I, nonesuch),
    ?assertEqual(no_such_field, Err#beamtalk_error.kind).

%%====================================================================
%% Refresh
%%====================================================================

refresh_value_returns_cursor_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    R = beamtalk_inspector:refresh(I),
    ?assertEqual(value, beamtalk_inspector:kindOf(R)),
    ?assertEqual(point(3, 4), beamtalk_inspector:subjectOf(R)).

%%====================================================================
%% Shared guarded_state/1 (ADR 0095 §4)
%%====================================================================

guarded_state_live_test() ->
    Pid = start_actor(point(5, 6)),
    ?assertEqual({ok, point(5, 6)}, beamtalk_process_navigation:guarded_state(Pid)),
    gen_server:stop(Pid).

guarded_state_dead_test() ->
    Pid = start_actor(point(5, 6)),
    gen_server:stop(Pid),
    ?assertEqual(unavailable, beamtalk_process_navigation:guarded_state(Pid)).

guarded_state_non_pid_test() ->
    ?assertEqual(unavailable, beamtalk_process_navigation:guarded_state(not_a_pid)).

%%====================================================================
%% #actor snapshot + unavailable degradation
%%====================================================================

actor_fields_from_snapshot_test() ->
    Pid = start_actor(point(7, 8)),
    I = beamtalk_inspector:on(Pid),
    Fields = beamtalk_inspector:fieldsOf(I),
    Names = lists:sort([maps:get(name, F) || F <- Fields]),
    ?assertEqual([x, y], Names),
    gen_server:stop(Pid).

dead_actor_fields_unavailable_test() ->
    Pid = start_actor(point(7, 8)),
    gen_server:stop(Pid),
    %% Inspecting a dead actor must not crash — single status field.
    I = beamtalk_inspector:on(Pid),
    [StatusF] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(status, maps:get(name, StatusF)),
    ?assertEqual(unavailable, maps:get(value, StatusF)),
    ?assertEqual(false, maps:get(drillable, StatusF)).

%%====================================================================
%% #collection — windowing, size, page, direct-index at: (ADR 0095 §6)
%%====================================================================

%% A bare Erlang list (a Beamtalk List) classifies as #collection.
list_kind_collection_test() ->
    I = beamtalk_inspector:on([10, 20, 30]),
    ?assertEqual(collection, beamtalk_inspector:kindOf(I)).

%% size is the cheap full count, not the (capped) window length.
collection_size_is_full_count_test() ->
    Big = lists:seq(1, 1000),
    I = beamtalk_inspector:on(Big),
    ?assertEqual(1000, beamtalk_inspector:sizeOf(I)).

%% fields returns only the first window (page size 50), even for a large list.
collection_fields_windowed_test() ->
    Big = lists:seq(1, 1000),
    I = beamtalk_inspector:on(Big),
    Fields = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(50, length(Fields)),
    %% Ordered elements are #element fields keyed by 1-based absolute index.
    First = hd(Fields),
    ?assertEqual(1, maps:get(name, First)),
    ?assertEqual(element, maps:get(kind, First)),
    ?assertEqual(1, maps:get(value, First)).

%% page/2 (1-based) returns a new cursor on the next window.
collection_page_next_window_test() ->
    Big = lists:seq(1, 1000),
    I = beamtalk_inspector:on(Big),
    {ok, P2} = beamtalk_inspector:page(I, 2),
    Fields = beamtalk_inspector:fieldsOf(P2),
    ?assertEqual(50, length(Fields)),
    First = hd(Fields),
    %% Window 2 starts at absolute index 51 (value 51).
    ?assertEqual(51, maps:get(name, First)),
    ?assertEqual(51, maps:get(value, First)).

%% A page past the end yields an empty window, not an error.
collection_page_past_end_empty_test() ->
    I = beamtalk_inspector:on(lists:seq(1, 10)),
    {ok, P5} = beamtalk_inspector:page(I, 5),
    ?assertEqual([], beamtalk_inspector:fieldsOf(P5)).

%% page/2 on a non-collection cursor is a not_a_collection error.
page_on_non_collection_errors_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {error, Err} = beamtalk_inspector:page(I, 2),
    ?assertEqual(not_a_collection, Err#beamtalk_error.kind).

%% at: indexes a list directly (1-based), reaching beyond the first window.
collection_at_direct_index_test() ->
    Big = lists:seq(1, 1000),
    I = beamtalk_inspector:on(Big),
    {ok, Child} = beamtalk_inspector:inspector(I, 73),
    ?assertEqual(73, beamtalk_inspector:subjectOf(Child)).

%% at: out of range is a no_such_field error.
collection_at_out_of_range_test() ->
    I = beamtalk_inspector:on([1, 2, 3]),
    {error, Err} = beamtalk_inspector:inspector(I, 99),
    ?assertEqual(no_such_field, Err#beamtalk_error.kind).

%% A Dictionary (plain map) classifies as #collection with #association fields.
dictionary_associations_test() ->
    Dict = #{a => 1, b => 2},
    I = beamtalk_inspector:on(Dict),
    ?assertEqual(collection, beamtalk_inspector:kindOf(I)),
    ?assertEqual(2, beamtalk_inspector:sizeOf(I)),
    Fields = beamtalk_inspector:fieldsOf(I),
    ?assertEqual([association, association], [maps:get(kind, F) || F <- Fields]),
    %% at: looks up an association by key.
    {ok, Child} = beamtalk_inspector:inspector(I, b),
    ?assertEqual(2, beamtalk_inspector:subjectOf(Child)).

%% A Dictionary with integer keys drills by key (association lookup), never by
%% ordered position — so `at: 5` reaches the entry keyed 5, not the 5th element.
dictionary_integer_key_drills_by_key_test() ->
    Dict = #{5 => fifty, 9 => ninety},
    I = beamtalk_inspector:on(Dict),
    {ok, Child} = beamtalk_inspector:inspector(I, 5),
    ?assertEqual(fifty, beamtalk_inspector:subjectOf(Child)),
    %% A missing key is a no_such_field miss, not an out-of-range index slip.
    {error, Err} = beamtalk_inspector:inspector(I, 1),
    ?assertEqual(no_such_field, Err#beamtalk_error.kind).

%% `at:` on a Dictionary uses exact (`=:=`) keys via `maps:find/2`, matching
%% `Dictionary >> at:` (`beamtalk_map:at:`): an exact key drills; a numerically-
%% equal-but-not-identical key (`at: 1` against a `1.0` entry) misses cleanly —
%% no crash, no `==` false hit (BT-2507; replaces the BT-2503 `==`-keyfind path).
dictionary_numeric_key_exact_match_test() ->
    Dict = #{1.0 => one_point_oh},
    I = beamtalk_inspector:on(Dict),
    {ok, Child} = beamtalk_inspector:inspector(I, 1.0),
    ?assertEqual(one_point_oh, beamtalk_inspector:subjectOf(Child)),
    {error, Err} = beamtalk_inspector:inspector(I, 1),
    ?assertEqual(no_such_field, Err#beamtalk_error.kind).

%% Dictionary `size` is the user-key count — `maps:size/1` minus the class tag,
%% for both tagged and plain maps (BT-2507, cheap on the render hot path).
dictionary_size_excludes_tag_test() ->
    Tagged = #{'$beamtalk_class' => 'Dictionary', a => 1, b => 2, c => 3},
    ?assertEqual(3, beamtalk_inspector:sizeOf(beamtalk_inspector:on(Tagged))),
    ?assertEqual(2, beamtalk_inspector:sizeOf(beamtalk_inspector:on(#{x => 1, y => 2}))).

%% A large Array windows via `array:get/2` over the page's index range (not a
%% whole-array `array:to_list/1`), with correct absolute indices (BT-2507).
large_array_windowed_test() ->
    Arr = #{'$beamtalk_class' => 'Array', data => array:from_list(lists:seq(1, 1000))},
    I = beamtalk_inspector:on(Arr),
    ?assertEqual(1000, beamtalk_inspector:sizeOf(I)),
    Fields = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(50, length(Fields)),
    ?assertEqual(1, maps:get(name, hd(Fields))),
    ?assertEqual(1, maps:get(value, hd(Fields))),
    {ok, P2} = beamtalk_inspector:page(I, 2),
    [P2First | _] = beamtalk_inspector:fieldsOf(P2),
    ?assertEqual(51, maps:get(name, P2First)),
    ?assertEqual(51, maps:get(value, P2First)).

%% An improper list (routine in foreign OTP process state) is *not* a collection:
%% it degrades to a #value cursor instead of crashing the windowing paths
%% (length/1, lists:sublist/3, lists:nth/2) (BT-2503 regression).
improper_list_degrades_to_value_test() ->
    I = beamtalk_inspector:on([1 | 2]),
    ?assertEqual(value, beamtalk_inspector:kindOf(I)),
    %% Field derivation and rendering must not raise on the improper list.
    ?assertEqual([], beamtalk_inspector:fieldsOf(I)),
    ?assert(is_binary(beamtalk_inspector:printString(I, 1))).

%%====================================================================
%% #foreign — process_info + guarded state (ADR 0095 §3–§4)
%%====================================================================

%% A non-Beamtalk OTP process classifies as #foreign.
foreign_kind_test() ->
    Pid = start_foreign(point(1, 2)),
    I = beamtalk_inspector:on(Pid),
    ?assertEqual(foreign, beamtalk_inspector:kindOf(I)),
    gen_server:stop(Pid).

%% Foreign fields are best-effort process_info, tagged #processInfo, including a
%% guarded sys:get_state `state` field for a sys-compliant process.
foreign_fields_process_info_test() ->
    Pid = start_foreign(point(5, 6)),
    I = beamtalk_inspector:on(Pid),
    Fields = beamtalk_inspector:fieldsOf(I),
    Kinds = lists:usort([maps:get(kind, F) || F <- Fields]),
    ?assertEqual([processInfo], Kinds),
    StateFields = [F || F <- Fields, maps:get(name, F) =:= state],
    ?assertMatch([_], StateFields),
    [StateF] = StateFields,
    ?assertEqual(point(5, 6), maps:get(value, StateF)),
    gen_server:stop(Pid).

%% A dead foreign process degrades to the single #status => #unavailable field.
dead_foreign_unavailable_test() ->
    Pid = start_foreign(point(5, 6)),
    gen_server:stop(Pid),
    I = beamtalk_inspector:on(Pid),
    [StatusF] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(status, maps:get(name, StatusF)),
    ?assertEqual(unavailable, maps:get(value, StatusF)).

%% A pid on another node must not crash classification — `is_beamtalk_actor/1`,
%% `is_process_alive/1`, and `process_info/2` all raise `badarg` for a remote pid.
%% It degrades to the single #status => #unavailable foreign field (BT-2508).
remote_pid_degrades_to_unavailable_test() ->
    RemotePid = a_remote_pid(),
    ?assert(node(RemotePid) =/= node()),
    I = beamtalk_inspector:on(RemotePid),
    ?assertEqual(foreign, beamtalk_inspector:kindOf(I)),
    %% fieldsOf must not raise; it yields only the unavailable status field.
    [StatusF] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(status, maps:get(name, StatusF)),
    ?assertEqual(unavailable, maps:get(value, StatusF)),
    %% printString and refresh must not raise on a remote pid either.
    ?assert(is_binary(beamtalk_inspector:printString(I, 1))),
    ?assertEqual(foreign, beamtalk_inspector:kindOf(beamtalk_inspector:refresh(I))).

%% A deterministic pid on a *non-local* node, built via the external term format
%% (`NEW_PID_EXT`) so the test needs no distribution / peer node — `binary_to_term`
%% does not require the encoded node to exist or be connected.
a_remote_pid() ->
    NodeBin = atom_to_binary('inspector_remote@nohost', utf8),
    Len = byte_size(NodeBin),
    binary_to_term(<<131, 88, 119, Len:16, NodeBin/binary, 1:32, 0:32, 0:32>>).

%%====================================================================
%% evaluate: — actor/foreign return actor_eval_unsupported (ADR 0095 §7)
%%====================================================================

actor_evaluate_unsupported_test() ->
    Pid = start_actor(point(1, 2)),
    I = beamtalk_inspector:on(Pid),
    {error, Err} = beamtalk_inspector:evaluate(I, <<"self">>),
    ?assertEqual(actor_eval_unsupported, Err#beamtalk_error.kind),
    gen_server:stop(Pid).

foreign_evaluate_unsupported_test() ->
    Pid = start_foreign(point(1, 2)),
    I = beamtalk_inspector:on(Pid),
    {error, Err} = beamtalk_inspector:evaluate(I, <<"self">>),
    ?assertEqual(actor_eval_unsupported, Err#beamtalk_error.kind),
    gen_server:stop(Pid).

%%====================================================================
%% Edge cases (BT-2509)
%%====================================================================

%% The empty list `#()` is a leaf `#value`, not a `#collection` — consistent with
%% a drilled empty-list field (which is also a leaf).
empty_list_is_value_leaf_test() ->
    I = beamtalk_inspector:on([]),
    ?assertEqual(value, beamtalk_inspector:kindOf(I)),
    %% A tagged value whose slot holds `#()` renders that slot as a non-drillable
    %% leaf field.
    V = #{'$beamtalk_class' => 'Holder', items => []},
    HF = hd([
        F
     || F <- beamtalk_inspector:fieldsOf(beamtalk_inspector:on(V)),
        maps:get(name, F) =:= items
    ]),
    ?assertEqual(false, maps:get(drillable, HF)).

%% `refresh` of an `#actor` whose process has died stays `#actor` (unavailable),
%% rather than re-classifying as `#foreign` and losing the actor kind.
dead_actor_refresh_stays_actor_test() ->
    Pid = start_actor(point(1, 2)),
    I = beamtalk_inspector:on(Pid),
    ?assertEqual(actor, beamtalk_inspector:kindOf(I)),
    gen_server:stop(Pid),
    R = beamtalk_inspector:refresh(I),
    ?assertEqual(actor, beamtalk_inspector:kindOf(R)),
    [StatusF] = beamtalk_inspector:fieldsOf(R),
    ?assertEqual(status, maps:get(name, StatusF)),
    ?assertEqual(unavailable, maps:get(value, StatusF)).

%% A forged/malformed collection-tagged map (wrong-typed `data`/`elements`/`counts`)
%% must not crash `array:size/1`, `length/1`, etc. — it degrades to an empty view.
forged_collection_maps_never_crash_test() ->
    Array = #{'$beamtalk_class' => 'Array', data => 42},
    IA = beamtalk_inspector:on(Array),
    ?assertEqual(collection, beamtalk_inspector:kindOf(IA)),
    ?assertEqual(0, beamtalk_inspector:sizeOf(IA)),
    ?assertEqual([], beamtalk_inspector:fieldsOf(IA)),
    ?assert(is_binary(beamtalk_inspector:printString(IA, 1))),
    Set = #{'$beamtalk_class' => 'Set', elements => not_a_list},
    ?assertEqual(0, beamtalk_inspector:sizeOf(beamtalk_inspector:on(Set))),
    Bag = #{'$beamtalk_class' => 'Bag', counts => not_a_map},
    ?assertEqual(0, beamtalk_inspector:sizeOf(beamtalk_inspector:on(Bag))).
