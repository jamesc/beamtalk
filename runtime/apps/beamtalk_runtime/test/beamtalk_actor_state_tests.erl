%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_actor_state_tests).
-moduledoc "EUnit tests for beamtalk_actor_state (ADR 0137 Phase 1).".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% BT-4536: the deprecated `nlr_state/2` shim is tested here until it goes.
-compile({nowarn_deprecated_function, [{beamtalk_actor_state, nlr_state, 2}]}).

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
            ?assertMatch({beamtalk_string, _}, H),
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

%% BT-4536: `nlr_state/2` is a deprecated alias of `view/2`, kept one release
%% for closures compiled before it; it no longer stamps an away capture.
nlr_state_is_view_at_home_test() ->
    with_cell(#{n => 1}, fun() ->
        ?assertEqual(
            beamtalk_actor_state:view(self_obj(), #{n => 0}),
            beamtalk_actor_state:nlr_state(self_obj(), #{n => 0})
        ),
        ?assertEqual(#{n => 1}, beamtalk_actor_state:nlr_state(self_obj(), #{n => 0}))
    end).

nlr_state_is_view_away_test() ->
    Away = foreign_obj(),
    ?assertEqual(#{n => 0}, beamtalk_actor_state:nlr_state(Away, #{n => 0})),
    ?assertEqual(
        actor_state_unreachable,
        kind_of(fun() -> beamtalk_actor_state:nlr_state(Away, none) end)
    ).

nlr_state_is_view_under_another_owners_cell_test() ->
    Other = spawn(fun() -> ok end),
    Old = beamtalk_actor_state:snapshot(),
    try
        ok = beamtalk_actor_state:uninstall({Other, #{n => 7}}),
        ?assertEqual(#{n => 0}, beamtalk_actor_state:nlr_state(self_obj(), #{n => 0}))
    after
        beamtalk_actor_state:uninstall(Old)
    end.

%% ADR 0146 (BT-4535, BT-4536): an actor method's `^` commits the cell at home
%% and the carried state away. Whose cell it is is decided by identity (the
%% calling process and the cell's owner), never by comparing state maps.
commit_for_its_own_pid_and_its_own_cell_answers_the_cell_test() ->
    with_commit_check(false, fun() ->
        with_cell(#{n => 5, '__local__i' => 1}, fun() ->
            %% The throw's own `__local__` keys are dead: not merged back.
            ?assertEqual(
                #{n => 5, '__local__i' => 1},
                beamtalk_actor_state:commit(self_obj(), #{n => 0, '__local__j' => 2})
            )
        end)
    end).

commit_for_its_own_pid_under_another_owners_cell_answers_the_carried_state_test() ->
    %% A nested away `super_send/5` installed another actor's state here: even
    %% when its fields equal the carried state, it is not this actor's cell.
    Other = spawn(fun() -> ok end),
    Old = beamtalk_actor_state:snapshot(),
    try
        ok = beamtalk_actor_state:uninstall({Other, #{n => 7}}),
        ?assertEqual(#{n => 0}, beamtalk_actor_state:commit(self_obj(), #{n => 0})),
        ok = beamtalk_actor_state:uninstall({Other, #{n => 0}}),
        Carried = #{n => 0, '__local__i' => 1},
        ?assertEqual(Carried, beamtalk_actor_state:commit(self_obj(), Carried))
    after
        beamtalk_actor_state:uninstall(Old)
    end.

commit_for_another_pid_answers_the_carried_state_test() ->
    with_cell(#{n => 5}, fun() ->
        ?assertEqual(#{n => 0}, beamtalk_actor_state:commit(foreign_obj(), #{n => 0}))
    end),
    %% Equal fields do not make this process's cell another actor's.
    with_cell(#{n => 0}, fun() ->
        Carried = #{n => 0, '__local__i' => 1},
        ?assertEqual(Carried, beamtalk_actor_state:commit(foreign_obj(), Carried))
    end),
    ?assertEqual(nil, beamtalk_actor_state:commit(foreign_obj(), nil)).

commit_without_a_cell_answers_the_carried_state_test() ->
    ?assertEqual(#{n => 0}, beamtalk_actor_state:commit(self_obj(), #{n => 0})).

%% Runs `Fun` with the `commit/2` test-build check on (or off), restoring
%% what it was: the Justfile exports `BEAMTALK_CHECK_NLR_COMMIT`, so a runtime
%% started by the test run may already have it on.
with_commit_check(Fun) -> with_commit_check(true, Fun).

with_commit_check(Enabled, Fun) ->
    Was = persistent_term:get({beamtalk_actor_state, check_commit}, false),
    beamtalk_actor_state:enable_commit_check(Enabled),
    try
        Fun()
    after
        beamtalk_actor_state:enable_commit_check(Was),
        erlang:erase('$bt_nlr_commit_excused')
    end.

commit_check_passes_a_current_cell_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 5}, fun() ->
            ?assertEqual(
                #{n => 5}, beamtalk_actor_state:commit(self_obj(), #{n => 5, '__local__i' => 3})
            )
        end)
    end).

commit_check_raises_on_a_stale_cell_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 5}) end)
            )
        end)
    end).

commit_check_skips_a_cell_a_foreign_cleanup_put_ahead_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:store_unwound(nlr_throw(#{n => 0}), #{n => 40}),
            ?assertEqual(#{n => 40}, beamtalk_actor_state:commit(self_obj(), #{n => 0})),
            %% The note is spent by the commit it was left for.
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 0}) end)
            )
        end)
    end).

%% BT-4536: `Block>>ensure:` reached by `perform:` runs its closure cleanup,
%% which writes the cell in place, and re-raises the throw as it came.
commit_check_skips_a_cell_a_performed_ensure_put_ahead_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:put(self_obj(), n, 9),
            ok = beamtalk_actor_state:note_unwound(nlr_throw(#{n => 0})),
            ?assertEqual(#{n => 9}, beamtalk_actor_state:commit(self_obj(), #{n => 0}))
        end)
    end).

%% A `^` through two cleanups is noted once: once its commit spends the note,
%% nothing is left to excuse a later commit of an equal carried state.
commit_check_note_is_deduplicated_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            Throw = nlr_throw(#{n => 0}),
            ok = beamtalk_actor_state:store_unwound(Throw, #{n => 40}),
            ok = beamtalk_actor_state:note_unwound(Throw),
            ?assertEqual(#{n => 40}, beamtalk_actor_state:commit(self_obj(), #{n => 0})),
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 0}) end)
            )
        end)
    end).

%% BT-4459/BT-4536: this actor's block, run by another actor, carries its
%% creation-time capture, which the cell has moved past. It reaches this
%% process through `rethrow_from_away/1`: recognised as away by process
%% identity, with nothing in the state map.
commit_check_skips_a_throw_from_another_process_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 5}, fun() ->
            Throw = nlr_throw(#{n => 0}),
            ?assertEqual(
                {thrown, Throw},
                try beamtalk_actor_state:rethrow_from_away(Throw) of
                    _ -> not_thrown
                catch
                    throw:Thrown -> {thrown, Thrown}
                end
            ),
            ?assertEqual(#{n => 5}, beamtalk_actor_state:commit(self_obj(), #{n => 0})),
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 0}) end)
            )
        end)
    end).

%% The same capture thrown at home (no hop) is a stale cell: the away
%% half of the check is decided by where the throw came from, not by its state.
commit_check_does_not_skip_an_equal_capture_thrown_here_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 5}, fun() ->
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 0}) end)
            )
        end)
    end).

rethrow_from_away_with_the_check_off_notes_nothing_test() ->
    with_commit_check(false, fun() ->
        with_cell(#{n => 5}, fun() ->
            Throw = nlr_throw(#{n => 0}),
            ?assertThrow(Throw, beamtalk_actor_state:rethrow_from_away(Throw)),
            ?assertEqual(undefined, erlang:get('$bt_nlr_commit_excused'))
        end)
    end).

note_unwound_outside_a_callback_notes_nothing_test() ->
    with_commit_check(fun() ->
        ok = beamtalk_actor_state:note_unwound(nlr_throw(#{n => 0})),
        ?assertEqual(undefined, erlang:get('$bt_nlr_commit_excused'))
    end).

note_unwound_ignores_anything_but_a_state_carrying_throw_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:note_unwound({'$bt_nlr', make_ref(), 1}),
            ok = beamtalk_actor_state:note_unwound(an_error),
            ?assertEqual(undefined, erlang:get('$bt_nlr_commit_excused'))
        end)
    end).

commit_check_off_never_raises_test() ->
    with_commit_check(false, fun() ->
        with_cell(#{n => 0}, fun() ->
            ?assertEqual(#{n => 0}, beamtalk_actor_state:commit(self_obj(), #{n => 5}))
        end)
    end).

nlr_throw(Carried) -> {'$bt_nlr', make_ref(), 1, Carried}.

%% The note is the throw's own: a nested commit of another `^` (a self-send's
%% boundary reached while the throw is in flight) neither spends it nor is
%% excused by it.
commit_check_note_is_spent_only_by_its_own_throw_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:store_unwound(nlr_throw(#{n => 0}), #{n => 40}),
            %% Another throw, carrying the cell: passes, spends nothing.
            ?assertEqual(#{n => 40}, beamtalk_actor_state:commit(self_obj(), #{n => 40})),
            %% Another throw, stale: not excused by the first throw's note.
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 7}) end)
            ),
            %% The first throw's own commit is still excused.
            ?assertEqual(#{n => 40}, beamtalk_actor_state:commit(self_obj(), #{n => 0}))
        end)
    end).

%% A note left for a throw that never reached its boundary (it left the
%% callback by another way) does not outlive the callback: a later callback's
%% stale cell is still caught.
commit_check_note_does_not_cross_callbacks_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:store_unwound(nlr_throw(#{n => 0}), #{n => 40})
        end),
        with_cell(#{n => 40}, fun() ->
            ?assertEqual(
                actor_nlr_stale_cell,
                kind_of(fun() -> beamtalk_actor_state:commit(self_obj(), #{n => 0}) end)
            )
        end)
    end).

%% A nested callback (an enclosing cell installed) keeps the outer one's note.
commit_check_note_survives_a_nested_install_test() ->
    with_commit_check(fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:store_unwound(nlr_throw(#{n => 0}), #{n => 40}),
            Old = beamtalk_actor_state:install(#{n => 40}),
            ok = beamtalk_actor_state:uninstall(Old),
            ?assertEqual(#{n => 40}, beamtalk_actor_state:commit(self_obj(), #{n => 0}))
        end)
    end).

store_unwound_stores_like_store_test() ->
    with_commit_check(false, fun() ->
        with_cell(#{n => 0}, fun() ->
            ok = beamtalk_actor_state:store_unwound(nlr_throw(#{n => 0}), #{n => 3}),
            ?assertEqual(#{n => 3}, beamtalk_actor_state:take()),
            ?assertEqual(undefined, erlang:get('$bt_nlr_commit_excused'))
        end)
    end).

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
