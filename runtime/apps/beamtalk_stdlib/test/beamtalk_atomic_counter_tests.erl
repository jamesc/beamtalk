%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_atomic_counter module (BT-1250).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests cover:
%%% - new:/1 — counter creation (success, already_exists, type error)
%%% - named:/1 — counter lookup (success, not_found, type error)
%%% - increment/1 — atomic add 1 (success, stale, type error)
%%% - incrementBy/2 — atomic add N (success, type error)
%%% - decrement/1 — atomic subtract 1 (success, type error)
%%% - decrementBy/2 — atomic subtract N (success, type error)
%%% - value/1 — read current value (success, type error)
%%% - reset/1 — set to 0 (success, type error)
%%% - delete/1 — destroy table (success, type error)
%%% - FFI shims: new/1, named/1
%%% - concurrent increment test

-module(beamtalk_atomic_counter_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Test Helpers
%%% ============================================================================

%% @doc Create a fresh counter with a unique name, run Fun(Counter), then clean up.
with_counter(Name, Fun) ->
    %% Delete any leftover table from a previous failed run
    catch ets:delete(Name),
    Counter = beamtalk_atomic_counter:'new:'(Name),
    try
        Fun(Counter)
    after
        catch ets:delete(Name)
    end.

%%% ============================================================================
%%% new:/1
%%% ============================================================================

new_returns_counter_map_test() ->
    catch ets:delete(bt_ac_test_new),
    C = beamtalk_atomic_counter:'new:'(bt_ac_test_new),
    ?assertMatch(#{'$beamtalk_class' := 'AtomicCounter', table := bt_ac_test_new}, C),
    ets:delete(bt_ac_test_new).

new_starts_at_zero_test() ->
    catch ets:delete(bt_ac_test_zero),
    C = beamtalk_atomic_counter:'new:'(bt_ac_test_zero),
    try
        ?assertEqual(0, beamtalk_atomic_counter:value(C))
    after
        catch ets:delete(bt_ac_test_zero)
    end.

new_already_exists_test() ->
    catch ets:delete(bt_ac_test_dup),
    ets:new(bt_ac_test_dup, [set, named_table, public]),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = already_exists, class = 'AtomicCounter'}
            },
            beamtalk_atomic_counter:'new:'(bt_ac_test_dup)
        )
    after
        ets:delete(bt_ac_test_dup)
    end.

new_type_error_not_atom_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:'new:'(<<"not_an_atom">>)
    ).

%%% ============================================================================
%%% named:/1
%%% ============================================================================

named_success_test() ->
    catch ets:delete(bt_ac_test_named),
    _C = beamtalk_atomic_counter:'new:'(bt_ac_test_named),
    try
        C2 = beamtalk_atomic_counter:'named:'(bt_ac_test_named),
        ?assertMatch(#{'$beamtalk_class' := 'AtomicCounter', table := bt_ac_test_named}, C2)
    after
        catch ets:delete(bt_ac_test_named)
    end.

named_not_found_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = not_found, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:'named:'(bt_ac_test_no_such_xyz)
    ).

named_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:'named:'(<<"not_an_atom">>)
    ).

%%% ============================================================================
%%% increment/1
%%% ============================================================================

increment_returns_new_value_test() ->
    with_counter(bt_ac_test_incr, fun(C) ->
        ?assertEqual(1, beamtalk_atomic_counter:increment(C)),
        ?assertEqual(2, beamtalk_atomic_counter:increment(C)),
        ?assertEqual(3, beamtalk_atomic_counter:increment(C))
    end).

increment_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:increment(not_a_counter)
    ).

%%% ============================================================================
%%% incrementBy/2
%%% ============================================================================

incrementBy_returns_new_value_test() ->
    with_counter(bt_ac_test_incrby, fun(C) ->
        ?assertEqual(5, beamtalk_atomic_counter:incrementBy(C, 5)),
        ?assertEqual(8, beamtalk_atomic_counter:incrementBy(C, 3))
    end).

incrementBy_type_error_receiver_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:incrementBy(not_a_counter, 5)
    ).

incrementBy_type_error_non_integer_test() ->
    with_counter(bt_ac_test_incrby_type, fun(C) ->
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
            },
            beamtalk_atomic_counter:incrementBy(C, 1.5)
        )
    end).

%%% ============================================================================
%%% decrement/1
%%% ============================================================================

decrement_returns_new_value_test() ->
    with_counter(bt_ac_test_decr, fun(C) ->
        ?assertEqual(-1, beamtalk_atomic_counter:decrement(C)),
        ?assertEqual(-2, beamtalk_atomic_counter:decrement(C))
    end).

decrement_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:decrement(not_a_counter)
    ).

%%% ============================================================================
%%% decrementBy/2
%%% ============================================================================

decrementBy_returns_new_value_test() ->
    with_counter(bt_ac_test_decrby, fun(C) ->
        ?assertEqual(-3, beamtalk_atomic_counter:decrementBy(C, 3)),
        ?assertEqual(-8, beamtalk_atomic_counter:decrementBy(C, 5))
    end).

decrementBy_type_error_receiver_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:decrementBy(not_a_counter, 3)
    ).

decrementBy_type_error_non_integer_test() ->
    with_counter(bt_ac_test_decrby_type, fun(C) ->
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
            },
            beamtalk_atomic_counter:decrementBy(C, 1.5)
        )
    end).

%%% ============================================================================
%%% value/1
%%% ============================================================================

value_returns_current_test() ->
    with_counter(bt_ac_test_value, fun(C) ->
        ?assertEqual(0, beamtalk_atomic_counter:value(C)),
        beamtalk_atomic_counter:increment(C),
        ?assertEqual(1, beamtalk_atomic_counter:value(C))
    end).

value_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:value(not_a_counter)
    ).

%%% ============================================================================
%%% reset/1
%%% ============================================================================

reset_sets_to_zero_test() ->
    with_counter(bt_ac_test_reset, fun(C) ->
        beamtalk_atomic_counter:incrementBy(C, 10),
        ?assertEqual(nil, beamtalk_atomic_counter:reset(C)),
        ?assertEqual(0, beamtalk_atomic_counter:value(C))
    end).

reset_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:reset(not_a_counter)
    ).

%%% ============================================================================
%%% delete/1
%%% ============================================================================

delete_destroys_table_test() ->
    catch ets:delete(bt_ac_test_delete),
    C = beamtalk_atomic_counter:'new:'(bt_ac_test_delete),
    ?assertEqual(nil, beamtalk_atomic_counter:delete(C)),
    ?assertEqual(undefined, ets:whereis(bt_ac_test_delete)).

delete_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:delete(not_a_counter)
    ).

%%% ============================================================================
%%% FFI shims: new/1, named/1
%%% ============================================================================

ffi_new_shim_test() ->
    catch ets:delete(bt_ac_test_ffi_new),
    C = beamtalk_atomic_counter:new(bt_ac_test_ffi_new),
    ?assertMatch(#{'$beamtalk_class' := 'AtomicCounter', table := bt_ac_test_ffi_new}, C),
    ets:delete(bt_ac_test_ffi_new).

ffi_named_shim_success_test() ->
    catch ets:delete(bt_ac_test_ffi_named),
    _C = beamtalk_atomic_counter:'new:'(bt_ac_test_ffi_named),
    try
        C2 = beamtalk_atomic_counter:named(bt_ac_test_ffi_named),
        ?assertMatch(#{'$beamtalk_class' := 'AtomicCounter', table := bt_ac_test_ffi_named}, C2)
    after
        catch ets:delete(bt_ac_test_ffi_named)
    end.

ffi_named_shim_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = not_found}},
        beamtalk_atomic_counter:named(bt_ac_test_ffi_no_such_xyz)
    ).

%%% ============================================================================
%%% Stale counter errors
%%% ============================================================================

stale_counter_increment_test() ->
    catch ets:delete(bt_ac_test_stale_incr),
    C = beamtalk_atomic_counter:'new:'(bt_ac_test_stale_incr),
    beamtalk_atomic_counter:delete(C),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = stale_counter, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:increment(C)
    ).

stale_counter_value_test() ->
    catch ets:delete(bt_ac_test_stale_value),
    C = beamtalk_atomic_counter:'new:'(bt_ac_test_stale_value),
    ets:delete(bt_ac_test_stale_value),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = stale_counter, class = 'AtomicCounter'}
        },
        beamtalk_atomic_counter:value(C)
    ).

%%% ============================================================================
%%% Concurrent increment test
%%% ============================================================================

concurrent_increment_test() ->
    %% Spawn N processes, each incrementing the counter M times.
    %% The final value must equal N * M.
    NumProcs = 20,
    Increments = 100,
    Expected = NumProcs * Increments,
    catch ets:delete(bt_ac_test_concurrent),
    C = beamtalk_atomic_counter:'new:'(bt_ac_test_concurrent),
    try
        Parent = self(),
        Pids = [
            spawn(fun() ->
                lists:foreach(
                    fun(_) -> beamtalk_atomic_counter:increment(C) end,
                    lists:seq(1, Increments)
                ),
                Parent ! done
            end)
         || _ <- lists:seq(1, NumProcs)
        ],
        %% Wait for all workers to finish
        lists:foreach(
            fun(_) ->
                receive
                    done -> ok
                end
            end,
            Pids
        ),
        ?assertEqual(Expected, beamtalk_atomic_counter:value(C))
    after
        catch ets:delete(bt_ac_test_concurrent)
    end.
