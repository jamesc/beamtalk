%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime benchmark — method-level state-threading overhead
%%%
%%% @doc Erlang simulations of Beamtalk's method-level state threading.
%%%
%%% Unlike block/loop state threading (bench_block_threading.erl), this module
%%% benchmarks straight-line code in method bodies:
%%%   - Sequential field mutations: self.x := 1. self.y := 2. self.z := 3
%%%   - Field read-after-write: self.x := 5. self.y := self.x + 1
%%%   - Mixed local vars + field mutations
%%%
%%% In Beamtalk's codegen, each field assignment emits:
%%%   let Val = <rhs> in let StateN = call 'maps':'put'('field', Val, StateN-1) in ...
%%%
%%% Local variable assignments are zero-overhead (standard let bindings).
%%%
%%% All functions take a pre-existing State map as input (simulating the
%%% gen_server state already in scope), so we benchmark only the mutation
%%% overhead, not map construction.
%%%
%%% Findings (2026-03-17):
%%%   - Small-map overhead: ~1.5-1.9x vs native map update syntax
%%%   - Large-map overhead: 3-8x vs small-map equivalents (HAMT penalty)
%%%   - Batched map update syntax (State#{...}) is NOT inherently faster
%%%     than sequential maps:put on BEAM's JIT — roughly neutral
%%%   - Shadow locals (eliminating maps:get for just-written fields) give
%%%     ~1.1-1.3x on method body execution, but this is negligible vs
%%%     gen_server dispatch overhead (~2000ns per call)
%%%   - Loop-level optimizations (BT-1275/1276/1342) remain the priority
%%%     due to N-iteration amplification

-module(bench_method_threading).

-export([
    %% State constructors (called once per benchmark, not per iteration)
    small_state/0,
    big_state/0,
    %% Sequential field mutations (State -> State1 -> State2 -> ...)
    field_assign_native_3/1,
    field_assign_maps_3/1,
    field_assign_native_6/1,
    field_assign_maps_6/1,
    field_assign_native_10/1,
    field_assign_maps_10/1,
    %% Field read-after-write (maps:get after maps:put in the chain)
    field_raw_native_5/1,
    field_raw_maps_5/1,
    field_raw_native_10/1,
    field_raw_maps_10/1,
    %% Mixed locals + fields (locals are free, fields cost maps:put)
    mixed_native_10/1,
    mixed_maps_10/1,
    %% Large actor state (>32 keys — past BEAM small-map threshold)
    field_assign_bigmap_3/1,
    field_assign_bigmap_6/1,
    field_assign_bigmap_10/1,
    field_raw_bigmap_5/1,
    field_raw_bigmap_10/1
]).

%%====================================================================
%% State constructors
%%====================================================================

%% @doc Small actor state (~13 keys, within BEAM small-map JIT threshold).
-spec small_state() -> map().
small_state() ->
    #{'$beamtalk_class' => 'MyActor', '__class_mod__' => test,
      '__methods__' => #{},
      x => 0, y => 0, z => 0, a => 0, b => 0, c => 0,
      d => 0, e => 0, f => 0, g => 0}.

%% @doc Large actor state (37 keys, past BEAM small-map threshold).
-spec big_state() -> map().
big_state() ->
    lists:foldl(
        fun(I, Acc) ->
            Key = list_to_atom("field_" ++ integer_to_list(I)),
            maps:put(Key, 0, Acc)
        end,
        #{'$beamtalk_class' => 'BigActor', '__class_mod__' => test,
          '__methods__' => #{},
          x => 0, y => 0, z => 0, a => 0, b => 0, c => 0,
          d => 0, e => 0, f => 0, g => 0},
        lists:seq(1, 24)).

%%====================================================================
%% Sequential field mutations — 3 fields
%%
%% Beamtalk source:
%%   doStuff =>
%%     self.x := 1.
%%     self.y := 2.
%%     self.z := 3
%%
%% Native Erlang baseline: direct map update syntax.
%% Maps codegen: chain of maps:put calls (State -> State1 -> State2 -> State3).
%%====================================================================

-spec field_assign_native_3(map()) -> map().
field_assign_native_3(State) ->
    State#{x := 1, y := 2, z := 3}.

-spec field_assign_maps_3(map()) -> {reply, integer(), map()}.
field_assign_maps_3(State) ->
    %% let _Val1 = 1 in let State1 = call 'maps':'put'('x', _Val1, State) in
    State1 = maps:put(x, 1, State),
    %% let _Val2 = 2 in let State2 = call 'maps':'put'('y', _Val2, State1) in
    State2 = maps:put(y, 2, State1),
    %% let _Val3 = 3 in let State3 = call 'maps':'put'('z', _Val3, State2) in
    State3 = maps:put(z, 3, State2),
    {reply, 3, State3}.

%%====================================================================
%% Sequential field mutations — 6 fields
%%====================================================================

-spec field_assign_native_6(map()) -> map().
field_assign_native_6(State) ->
    State#{x := 1, y := 2, z := 3, a := 4, b := 5, c := 6}.

-spec field_assign_maps_6(map()) -> {reply, integer(), map()}.
field_assign_maps_6(State) ->
    State1 = maps:put(x, 1, State),
    State2 = maps:put(y, 2, State1),
    State3 = maps:put(z, 3, State2),
    State4 = maps:put(a, 4, State3),
    State5 = maps:put(b, 5, State4),
    State6 = maps:put(c, 6, State5),
    {reply, 6, State6}.

%%====================================================================
%% Sequential field mutations — 10 fields
%%====================================================================

-spec field_assign_native_10(map()) -> map().
field_assign_native_10(State) ->
    State#{x := 1, y := 2, z := 3, a := 4, b := 5,
           c := 6, d := 7, e := 8, f := 9, g := 10}.

-spec field_assign_maps_10(map()) -> {reply, integer(), map()}.
field_assign_maps_10(State) ->
    State1 = maps:put(x, 1, State),
    State2 = maps:put(y, 2, State1),
    State3 = maps:put(z, 3, State2),
    State4 = maps:put(a, 4, State3),
    State5 = maps:put(b, 5, State4),
    State6 = maps:put(c, 6, State5),
    State7 = maps:put(d, 7, State6),
    State8 = maps:put(e, 8, State7),
    State9 = maps:put(f, 9, State8),
    State10 = maps:put(g, 10, State9),
    {reply, 10, State10}.

%%====================================================================
%% Field read-after-write — 5 dependent assignments
%%
%% Beamtalk source:
%%   compute =>
%%     self.x := 1.
%%     self.y := self.x + 1.
%%     self.z := self.y + 1.
%%     self.a := self.z + 1.
%%     self.b := self.a + 1
%%
%% Each RHS reads a field that was just written, requiring maps:get on
%% the latest state version. This is the worst case for the maps:put
%% chain since every assignment depends on the previous one.
%%====================================================================

-spec field_raw_native_5(map()) -> map().
field_raw_native_5(State) ->
    X = 1,
    Y = X + 1,
    Z = Y + 1,
    A = Z + 1,
    B = A + 1,
    State#{x := X, y := Y, z := Z, a := A, b := B}.

-spec field_raw_maps_5(map()) -> {reply, integer(), map()}.
field_raw_maps_5(State) ->
    %% self.x := 1
    State1 = maps:put(x, 1, State),
    %% self.y := self.x + 1  — needs maps:get('x', State1)
    V2 = maps:get(x, State1) + 1,
    State2 = maps:put(y, V2, State1),
    %% self.z := self.y + 1
    V3 = maps:get(y, State2) + 1,
    State3 = maps:put(z, V3, State2),
    %% self.a := self.z + 1
    V4 = maps:get(z, State3) + 1,
    State4 = maps:put(a, V4, State3),
    %% self.b := self.a + 1
    V5 = maps:get(a, State4) + 1,
    State5 = maps:put(b, V5, State4),
    {reply, V5, State5}.

%%====================================================================
%% Field read-after-write — 10 dependent assignments
%%====================================================================

-spec field_raw_native_10(map()) -> map().
field_raw_native_10(State) ->
    X = 1,
    Y = X + 1,
    Z = Y + 1,
    A = Z + 1,
    B = A + 1,
    C = B + 1,
    D = C + 1,
    E = D + 1,
    F = E + 1,
    G = F + 1,
    State#{x := X, y := Y, z := Z, a := A, b := B,
           c := C, d := D, e := E, f := F, g := G}.

-spec field_raw_maps_10(map()) -> {reply, integer(), map()}.
field_raw_maps_10(State) ->
    State1 = maps:put(x, 1, State),
    V2 = maps:get(x, State1) + 1,
    State2 = maps:put(y, V2, State1),
    V3 = maps:get(y, State2) + 1,
    State3 = maps:put(z, V3, State2),
    V4 = maps:get(z, State3) + 1,
    State4 = maps:put(a, V4, State3),
    V5 = maps:get(a, State4) + 1,
    State5 = maps:put(b, V5, State4),
    V6 = maps:get(b, State5) + 1,
    State6 = maps:put(c, V6, State5),
    V7 = maps:get(c, State6) + 1,
    State7 = maps:put(d, V7, State6),
    V8 = maps:get(d, State7) + 1,
    State8 = maps:put(e, V8, State7),
    V9 = maps:get(e, State8) + 1,
    State9 = maps:put(f, V9, State8),
    V10 = maps:get(f, State9) + 1,
    State10 = maps:put(g, V10, State9),
    {reply, V10, State10}.

%%====================================================================
%% Mixed locals + fields — 10 operations (5 locals, 5 field mutations)
%%
%% Beamtalk source:
%%   compute =>
%%     tmp1 := 1.
%%     self.x := tmp1.
%%     tmp2 := tmp1 + 1.
%%     self.y := tmp2.
%%     tmp3 := tmp2 + 1.
%%     self.z := tmp3.
%%     tmp4 := tmp3 + 1.
%%     self.a := tmp4.
%%     tmp5 := tmp4 + 1.
%%     self.b := tmp5
%%
%% Locals are free (let bindings). Only field mutations cost maps:put.
%% This pattern is common: compute something, store it in a field.
%%====================================================================

-spec mixed_native_10(map()) -> map().
mixed_native_10(State) ->
    Tmp1 = 1,
    Tmp2 = Tmp1 + 1,
    Tmp3 = Tmp2 + 1,
    Tmp4 = Tmp3 + 1,
    Tmp5 = Tmp4 + 1,
    State#{x := Tmp1, y := Tmp2, z := Tmp3, a := Tmp4, b := Tmp5}.

-spec mixed_maps_10(map()) -> {reply, integer(), map()}.
mixed_maps_10(State) ->
    %% tmp1 := 1  — local, just a let binding
    Tmp1 = 1,
    %% self.x := tmp1
    State1 = maps:put(x, Tmp1, State),
    %% tmp2 := tmp1 + 1  — local, just a let binding (reads local, not field)
    Tmp2 = Tmp1 + 1,
    %% self.y := tmp2
    State2 = maps:put(y, Tmp2, State1),
    %% tmp3 := tmp2 + 1
    Tmp3 = Tmp2 + 1,
    %% self.z := tmp3
    State3 = maps:put(z, Tmp3, State2),
    %% tmp4 := tmp3 + 1
    Tmp4 = Tmp3 + 1,
    %% self.a := tmp4
    State4 = maps:put(a, Tmp4, State3),
    %% tmp5 := tmp4 + 1
    Tmp5 = Tmp4 + 1,
    %% self.b := tmp5
    State5 = maps:put(b, Tmp5, State4),
    {reply, Tmp5, State5}.

%%====================================================================
%% Large-map variants — past BEAM small-map threshold (>32 keys)
%%
%% BEAM JIT uses flat tuples for small maps (<=32 keys) making maps:put
%% effectively O(1). Above 32 keys, maps switch to HAMT where maps:put
%% is O(log32 N). These variants show real-world cost for actors with
%% many fields.
%%====================================================================

-spec field_assign_bigmap_3(map()) -> {reply, integer(), map()}.
field_assign_bigmap_3(State) ->
    State1 = maps:put(x, 1, State),
    State2 = maps:put(y, 2, State1),
    State3 = maps:put(z, 3, State2),
    {reply, 3, State3}.

-spec field_assign_bigmap_6(map()) -> {reply, integer(), map()}.
field_assign_bigmap_6(State) ->
    State1 = maps:put(x, 1, State),
    State2 = maps:put(y, 2, State1),
    State3 = maps:put(z, 3, State2),
    State4 = maps:put(a, 4, State3),
    State5 = maps:put(b, 5, State4),
    State6 = maps:put(c, 6, State5),
    {reply, 6, State6}.

-spec field_assign_bigmap_10(map()) -> {reply, integer(), map()}.
field_assign_bigmap_10(State) ->
    State1 = maps:put(x, 1, State),
    State2 = maps:put(y, 2, State1),
    State3 = maps:put(z, 3, State2),
    State4 = maps:put(a, 4, State3),
    State5 = maps:put(b, 5, State4),
    State6 = maps:put(c, 6, State5),
    State7 = maps:put(d, 7, State6),
    State8 = maps:put(e, 8, State7),
    State9 = maps:put(f, 9, State8),
    State10 = maps:put(g, 10, State9),
    {reply, 10, State10}.

-spec field_raw_bigmap_5(map()) -> {reply, integer(), map()}.
field_raw_bigmap_5(State) ->
    State1 = maps:put(x, 1, State),
    V2 = maps:get(x, State1) + 1,
    State2 = maps:put(y, V2, State1),
    V3 = maps:get(y, State2) + 1,
    State3 = maps:put(z, V3, State2),
    V4 = maps:get(z, State3) + 1,
    State4 = maps:put(a, V4, State3),
    V5 = maps:get(a, State4) + 1,
    State5 = maps:put(b, V5, State4),
    {reply, V5, State5}.

-spec field_raw_bigmap_10(map()) -> {reply, integer(), map()}.
field_raw_bigmap_10(State) ->
    State1 = maps:put(x, 1, State),
    V2 = maps:get(x, State1) + 1,
    State2 = maps:put(y, V2, State1),
    V3 = maps:get(y, State2) + 1,
    State3 = maps:put(z, V3, State2),
    V4 = maps:get(z, State3) + 1,
    State4 = maps:put(a, V4, State3),
    V5 = maps:get(a, State4) + 1,
    State5 = maps:put(b, V5, State4),
    V6 = maps:get(b, State5) + 1,
    State6 = maps:put(c, V6, State5),
    V7 = maps:get(c, State6) + 1,
    State7 = maps:put(d, V7, State6),
    V8 = maps:get(d, State7) + 1,
    State8 = maps:put(e, V8, State7),
    V9 = maps:get(e, State8) + 1,
    State9 = maps:put(f, V9, State8),
    V10 = maps:get(f, State9) + 1,
    State10 = maps:put(g, V10, State9),
    {reply, V10, State10}.
