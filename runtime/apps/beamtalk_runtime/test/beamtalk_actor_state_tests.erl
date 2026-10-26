%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_actor_state_tests).
-moduledoc "EUnit tests for beamtalk_actor_state (ADR 0137 Phase 1).".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

self_obj() ->
    #beamtalk_object{class = 'ActorStateTestClass', class_mod = astc, pid = self()}.

foreign_obj() ->
    #beamtalk_object{class = 'ActorStateTestClass', class_mod = astc, pid = spawn(fun() -> ok end)}.

with_cell(State, Fun) ->
    Old = beamtalk_actor_state:install(State),
    try
        Fun()
    after
        beamtalk_actor_state:uninstall(Old)
    end.

kind_of(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{kind = Kind}} -> Kind
    end.

%%% install / take / uninstall

install_take_uninstall_test() ->
    ?assertEqual(false, beamtalk_actor_state:installed()),
    Old = beamtalk_actor_state:install(#{a => 1}),
    ?assertEqual(undefined, Old),
    ?assert(beamtalk_actor_state:installed()),
    ?assertEqual(#{a => 1}, beamtalk_actor_state:take()),
    ok = beamtalk_actor_state:uninstall(Old),
    ?assertEqual(false, beamtalk_actor_state:installed()).

nested_install_restores_enclosing_cell_test() ->
    with_cell(#{a => outer}, fun() ->
        Inner = beamtalk_actor_state:install(#{a => inner}),
        ?assertEqual({self(), #{a => outer}}, Inner),
        ?assertEqual(#{a => inner}, beamtalk_actor_state:take()),
        beamtalk_actor_state:uninstall(Inner),
        ?assertEqual(#{a => outer}, beamtalk_actor_state:take())
    end).

take_without_cell_raises_test() ->
    ?assertEqual(actor_state_unreachable, kind_of(fun beamtalk_actor_state:take/0)).

%%% adapt

adapt_stores_state_and_strips_it_test() ->
    with_cell(#{a => 0}, fun() ->
        ?assertEqual({reply, r}, beamtalk_actor_state:adapt({reply, r, #{a => 1}})),
        ?assertEqual(#{a => 1}, beamtalk_actor_state:take()),
        ?assertEqual(noreply, beamtalk_actor_state:adapt({noreply, #{a => 2}})),
        ?assertEqual(#{a => 2}, beamtalk_actor_state:take()),
        ?assertEqual({error, boom}, beamtalk_actor_state:adapt({error, boom, #{a => 3}})),
        ?assertEqual(#{a => 3}, beamtalk_actor_state:take())
    end).

%%% access at home

home_get_put_has_clear_test() ->
    Self = self_obj(),
    with_cell(#{x => 1}, fun() ->
        ?assertEqual(1, beamtalk_actor_state:get(Self, x)),
        ?assertEqual(nil, beamtalk_actor_state:get(Self, y)),
        ok = beamtalk_actor_state:put(Self, x, 2),
        ?assertEqual(2, beamtalk_actor_state:get(Self, x)),
        ?assert(beamtalk_actor_state:has(Self, x)),
        ?assertNot(beamtalk_actor_state:has(Self, y)),
        ok = beamtalk_actor_state:clear(Self, x),
        ?assertNot(beamtalk_actor_state:has(Self, x)),
        ?assertEqual(#{}, beamtalk_actor_state:take())
    end).

home_ignores_capture_test() ->
    with_cell(#{x => live}, fun() ->
        ?assertEqual(live, beamtalk_actor_state:get(self_obj(), x, #{x => captured})),
        ?assertEqual(#{x => live}, beamtalk_actor_state:capture(self_obj(), none))
    end).

%%% access away

no_cell_is_away_test() ->
    Self = self_obj(),
    ?assertEqual(actor_state_unreachable, kind_of(fun() -> beamtalk_actor_state:get(Self, x) end)),
    ?assertEqual(
        actor_state_unreachable, kind_of(fun() -> beamtalk_actor_state:put(Self, x, 1) end)
    ),
    ?assertEqual(
        actor_state_unreachable, kind_of(fun() -> beamtalk_actor_state:clear(Self, x) end)
    ),
    ?assertEqual(none, beamtalk_actor_state:capture(Self, none)).

foreign_pid_is_away_even_with_cell_test() ->
    with_cell(#{x => 1}, fun() ->
        Other = foreign_obj(),
        ?assertEqual(
            actor_state_unreachable,
            kind_of(fun() -> beamtalk_actor_state:put(Other, x, 2) end)
        ),
        ?assertEqual(captured, beamtalk_actor_state:get(Other, x, #{x => captured})),
        ?assert(beamtalk_actor_state:has(Other, x, #{x => captured})),
        ?assertEqual(#{x => outer}, beamtalk_actor_state:capture(Other, #{x => outer})),
        ?assertEqual(#{x => 1}, beamtalk_actor_state:take())
    end).

%% A cell holding another actor's state (stored by a nested away
%% `super_send/5`) never counts as home, even for the process that runs in:
%% the host actor reads its capture, and its writes raise.
foreign_owned_cell_is_away_for_the_host_test() ->
    with_cell(#{x => host}, fun() ->
        Host = self_obj(),
        %% Simulate the away `super_send/5` entry: another actor's state,
        %% owned by that actor, installed in this process.
        ForeignPid = spawn(fun() -> ok end),
        erlang:put('$bt_actor_state', {ForeignPid, #{x => foreign}}),
        try
            ?assertEqual(ForeignPid, beamtalk_actor_state:cell_owner()),
            ?assertEqual(
                captured, beamtalk_actor_state:get(Host, x, #{x => captured})
            ),
            ?assertEqual(
                actor_state_unreachable,
                kind_of(fun() -> beamtalk_actor_state:put(Host, x, 1) end)
            ),
            %% A store keeps the foreign owner: the host must not claim it.
            beamtalk_actor_state:store(#{x => written}),
            ?assertEqual(ForeignPid, beamtalk_actor_state:cell_owner()),
            ?assertEqual(
                captured, beamtalk_actor_state:get(Host, x, #{x => captured})
            )
        after
            %% Restore the host's own cell for `with_cell`'s teardown.
            erlang:put('$bt_actor_state', {self(), #{x => host}})
        end
    end).

store_without_a_cell_claims_the_caller_test() ->
    ?assertEqual(undefined, beamtalk_actor_state:cell_owner()),
    beamtalk_actor_state:store(#{x => 1}),
    try
        ?assertEqual(self(), beamtalk_actor_state:cell_owner()),
        ?assertEqual(#{x => 1}, beamtalk_actor_state:take())
    after
        erlang:erase('$bt_actor_state')
    end.

away_read_without_capture_raises_test() ->
    Other = foreign_obj(),
    ?assertEqual(
        actor_state_unreachable,
        kind_of(fun() -> beamtalk_actor_state:get(Other, x, none) end)
    ).

unreachable_error_shape_test() ->
    try
        beamtalk_actor_state:put(self_obj(), x, 1),
        ?assert(false)
    catch
        error:#{error := #beamtalk_error{kind = K, class = C, hint = H, details = D}} ->
            ?assertEqual(actor_state_unreachable, K),
            ?assertEqual('ActorStateTestClass', C),
            ?assert(is_binary(H)),
            ?assertEqual(#{field => x}, D)
    end.

%%% snapshot / restore (BT-4122: restore only rolls back at home)

restore_puts_the_cell_back_to_the_snapshot_at_home_test() ->
    Self = self_obj(),
    with_cell(#{x => 0}, fun() ->
        Snap = beamtalk_actor_state:snapshot(),
        beamtalk_actor_state:put(Self, x, 9),
        ?assertEqual(#{x => 9}, beamtalk_actor_state:take()),
        ok = beamtalk_actor_state:restore(Self, Snap),
        ?assertEqual(#{x => 0}, beamtalk_actor_state:take())
    end).

restore_away_leaves_the_other_actors_cell_alone_test() ->
    %% An actor A block with on:do: running inside actor B's callback: the
    %% region's snapshot is B's cell, and B may have committed writes after
    %% it. Rolling back would discard B's writes, so restore/2 is a no-op.
    Foreign = foreign_obj(),
    with_cell(#{b => 1}, fun() ->
        Snap = beamtalk_actor_state:snapshot(),
        beamtalk_actor_state:store(#{b => 2}),
        ok = beamtalk_actor_state:restore(Foreign, Snap),
        ?assertEqual(#{b => 2}, beamtalk_actor_state:take())
    end).

restore_with_no_cell_is_a_noop_test() ->
    ?assertEqual(undefined, beamtalk_actor_state:snapshot()),
    ok = beamtalk_actor_state:restore(self_obj(), undefined),
    ?assertEqual(false, beamtalk_actor_state:installed()).

%%% protect

protect_restores_cell_on_exception_test() ->
    Self = self_obj(),
    with_cell(#{x => 0}, fun() ->
        ?assertError(
            boom,
            beamtalk_actor_state:protect(fun() ->
                beamtalk_actor_state:put(Self, x, 9),
                error(boom)
            end)
        ),
        ?assertEqual(#{x => 0}, beamtalk_actor_state:take())
    end).

protect_keeps_writes_on_normal_exit_test() ->
    Self = self_obj(),
    with_cell(#{x => 0}, fun() ->
        ?assertEqual(
            ok, beamtalk_actor_state:protect(fun() -> beamtalk_actor_state:put(Self, x, 5) end)
        ),
        ?assertEqual(#{x => 5}, beamtalk_actor_state:take())
    end).

protect_passes_nlr_through_test() ->
    Self = self_obj(),
    Nlr = {'$bt_nlr', make_ref(), v},
    with_cell(#{x => 0}, fun() ->
        ?assertEqual(
            {caught, Nlr},
            try
                beamtalk_actor_state:protect(fun() ->
                    beamtalk_actor_state:put(Self, x, 5),
                    throw(Nlr)
                end)
            catch
                throw:T -> {caught, T}
            end
        ),
        ?assertEqual(#{x => 5}, beamtalk_actor_state:take())
    end).

%%% Callback wrapper (observable behaviour of the runtime-path actor)

wrapper_installs_cell_and_erases_it_test() ->
    ?assertEqual(false, beamtalk_actor_state:installed()),
    State = #{
        '$beamtalk_class' => 'Wrapped',
        '__methods__' => #{
            probe => fun(_Sel, [], _Self, S) -> {reply, beamtalk_actor_state:installed(), S} end,
            bump => fun(_Sel, [], _Self, S) -> {reply, ok, S#{n => 1}} end,
            fail => fun(_Sel, [], _Self, S) -> {error, oops, S} end
        }
    },
    ?assertMatch(
        {reply, true, _},
        beamtalk_actor:handle_call({probe, []}, {self(), make_ref()}, State)
    ),
    ?assertMatch(
        {reply, ok, #{n := 1}},
        beamtalk_actor:handle_call({bump, []}, {self(), make_ref()}, State)
    ),
    {reply, {error, oops}, Kept} = beamtalk_actor:handle_call(
        {fail, []}, {self(), make_ref()}, State
    ),
    ?assertEqual(State, Kept),
    ?assertEqual(false, beamtalk_actor_state:installed()).

%%% view / refresh / call_foreign / super_send (ADR 0137 Phase 2b)

view_is_the_cell_at_home_test() ->
    with_cell(#{a => 1}, fun() ->
        ?assertEqual(#{a => 1}, beamtalk_actor_state:view(self_obj(), none)),
        %% at home the capture is ignored
        ?assertEqual(#{a => 1}, beamtalk_actor_state:view(self_obj(), #{a => 99}))
    end).

view_away_is_the_creation_time_capture_test() ->
    ?assertEqual(#{a => 7}, beamtalk_actor_state:view(foreign_obj(), #{a => 7})).

view_away_without_a_capture_raises_test() ->
    ?assertEqual(
        actor_state_unreachable,
        kind_of(fun() -> beamtalk_actor_state:view(foreign_obj(), none) end)
    ).

refresh_takes_the_cells_fields_and_keeps_local_scratch_test() ->
    Acc = #{a => 1, b => 2, '__local__x' => 5},
    with_cell(#{a => 10, b => 20}, fun() ->
        ?assertEqual(
            #{a => 10, b => 20, '__local__x' => 5},
            beamtalk_actor_state:refresh(self_obj(), Acc)
        )
    end).

refresh_away_answers_the_accumulator_unchanged_test() ->
    Acc = #{a => 1, '__local__x' => 5},
    ?assertEqual(Acc, beamtalk_actor_state:refresh(foreign_obj(), Acc)).

call_foreign_stores_the_state_and_answers_the_funs_answer_test() ->
    State = #{n => 1},
    Fun = fun(Arg) -> {reply, {saw, beamtalk_actor_state:take(), Arg}, State} end,
    try
        ?assertEqual(
            {reply, {saw, State, x}, State},
            beamtalk_actor_state:call_foreign(State, Fun, [x])
        )
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_passes_an_error_answer_through_test() ->
    try
        ?assertEqual(
            {error, boom},
            beamtalk_actor_state:call_foreign(#{n => 1}, fun() -> {error, boom} end, [])
        )
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_accepts_a_native_that_reads_a_blocks_writes_back_test() ->
    State = #{n => 1},
    %% a native that ran a block (the block wrote the cell) answers the cell's state
    Fun = fun() ->
        beamtalk_actor_state:store(#{n => 2}),
        {reply, ok, beamtalk_actor_state:take()}
    end,
    try
        ?assertEqual({reply, ok, #{n => 2}}, beamtalk_actor_state:call_foreign(State, Fun, []))
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_adopts_the_cell_when_the_answer_is_the_entry_state_test() ->
    State = #{n => 1},
    %% a block the callee ran wrote the cell, but the callee answers the state it was
    %% given: the cell holds the only writes
    Fun = fun() ->
        beamtalk_actor_state:store(#{n => 2}),
        {reply, ok, State}
    end,
    try
        ?assertEqual({reply, ok, #{n => 2}}, beamtalk_actor_state:call_foreign(State, Fun, []))
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_adopts_the_cell_for_other_answer_shapes_test() ->
    State = #{n => 1},
    Write = fun() -> beamtalk_actor_state:store(#{n => 2}) end,
    try
        ?assertEqual(
            {noreply, #{n => 2}},
            beamtalk_actor_state:call_foreign(
                State,
                fun() ->
                    Write(),
                    {noreply, State}
                end,
                []
            )
        ),
        ?assertEqual(
            {error, boom, #{n => 2}},
            beamtalk_actor_state:call_foreign(
                State,
                fun() ->
                    Write(),
                    {error, boom, State}
                end,
                []
            )
        )
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_passes_the_answer_through_when_the_cell_is_unchanged_test() ->
    State = #{n => 1},
    try
        ?assertEqual(
            {reply, ok, #{n => 3}},
            beamtalk_actor_state:call_foreign(State, fun() -> {reply, ok, #{n => 3}} end, [])
        ),
        ?assertEqual(
            {reply, ok, State},
            beamtalk_actor_state:call_foreign(State, fun() -> {reply, ok, State} end, [])
        )
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_accepts_an_answer_equal_to_the_cell_test() ->
    State = #{n => 1},
    %% a compiled callee that stored a version and threaded the same version
    Fun = fun() ->
        beamtalk_actor_state:store(#{n => 2}),
        {reply, ok, #{n => 2}}
    end,
    try
        ?assertEqual({reply, ok, #{n => 2}}, beamtalk_actor_state:call_foreign(State, Fun, []))
    after
        erlang:erase('$bt_actor_state')
    end.

call_foreign_raises_a_conflict_when_both_the_answer_and_the_cell_changed_test() ->
    State = #{n => 1},
    Fun = fun() ->
        beamtalk_actor_state:store(#{n => 2}),
        {reply, ok, #{n => 3}}
    end,
    try
        ?assertEqual(
            actor_state_conflict,
            kind_of(fun() -> beamtalk_actor_state:call_foreign(State, Fun, []) end)
        )
    after
        erlang:erase('$bt_actor_state')
    end.

super_send_away_without_a_capture_raises_test() ->
    ?assertEqual(
        actor_state_unreachable,
        kind_of(fun() ->
            beamtalk_actor_state:super_send(foreign_obj(), none, 'NoSuchClass', bump, [])
        end)
    ).
