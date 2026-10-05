%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_vars_tests).
-moduledoc "EUnit tests for beamtalk_class_vars (ADR 0130 Phase 2).".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HOME, '$bt_class_vars_home').
-define(C, 'ClassVarsTestClass').

%%% Helpers

clean() ->
    erlang:erase(beamtalk_class_vars:key(?C)),
    erlang:erase(beamtalk_class_vars:key('OtherClass')),
    erlang:erase(?HOME),
    ok.

with_clean(Fun) ->
    clean(),
    try
        Fun()
    after
        clean()
    end.

%% Run Fun, return the beamtalk_error kind it raised (or `no_error`).
raised_kind(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{kind = Kind}} -> Kind
    end.

raised_error(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{} = E} -> E
    end.

%% Register a process as class ?C's class process with a mirror snapshot.
start_fake_class(Map) ->
    Self = self(),
    Pid = spawn(fun() ->
        true = erlang:register(beamtalk_class_registry:registry_name(?C), self()),
        beamtalk_class_registry:record_class_state_snapshot(self(), Map),
        Self ! {ready, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {ready, Pid} -> Pid
    after 2000 -> error(fake_class_timeout)
    end.

stop_fake_class(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 2000 -> error(stop_timeout)
    end,
    beamtalk_class_registry:forget_class_state_snapshot(Pid).

%%% key / install / uninstall / assert_absent

key_test() ->
    ?assertEqual({'$bt_class_vars', 'Foo'}, beamtalk_class_vars:key('Foo')).

install_uninstall_test() ->
    with_clean(fun() ->
        ok = beamtalk_class_vars:install(?C, #{a => 1}),
        Key = beamtalk_class_vars:key(?C),
        ?assertEqual(#{a => 1}, erlang:get(Key)),
        ?assertEqual(Key, erlang:get(?HOME)),
        ok = beamtalk_class_vars:uninstall(?C),
        ?assertEqual(undefined, erlang:get(Key)),
        ?assertEqual(undefined, erlang:get(?HOME))
    end).

assert_absent_ok_test() ->
    with_clean(fun() -> ?assertEqual(ok, beamtalk_class_vars:assert_absent(?C)) end).

assert_absent_rejects_key_test() ->
    with_clean(fun() ->
        erlang:put(beamtalk_class_vars:key(?C), #{}),
        ?assertEqual(
            internal_error, raised_kind(fun() -> beamtalk_class_vars:assert_absent(?C) end)
        )
    end).

assert_absent_rejects_home_test() ->
    with_clean(fun() ->
        erlang:put(?HOME, beamtalk_class_vars:key('OtherClass')),
        ?assertEqual(
            internal_error, raised_kind(fun() -> beamtalk_class_vars:assert_absent(?C) end)
        )
    end).

%%% get / get_late / put / clear / has

get_put_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{count => 0}),
        ?assertEqual(0, beamtalk_class_vars:get(?C, count)),
        ?assertEqual(5, beamtalk_class_vars:put(?C, count, 5)),
        ?assertEqual(5, beamtalk_class_vars:get(?C, count))
    end).

get_undeclared_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{count => 0}),
        ?assertEqual(
            undeclared_class_variable,
            raised_kind(fun() -> beamtalk_class_vars:get(?C, nope) end)
        ),
        ?assertEqual(
            undeclared_class_variable,
            raised_kind(fun() -> beamtalk_class_vars:has(?C, nope) end)
        )
    end).

get_late_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{late => nil, set => 3}),
        ?assertEqual(3, beamtalk_class_vars:get_late(?C, set)),
        ?assertEqual(
            uninitialized_state_error,
            raised_kind(fun() -> beamtalk_class_vars:get_late(?C, late) end)
        ),
        ?assertEqual(
            uninitialized_state_error,
            raised_kind(fun() -> beamtalk_class_vars:get_late(?C, missing) end)
        )
    end).

clear_and_has_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{x => 1}),
        ?assert(beamtalk_class_vars:has(?C, x)),
        ?assertEqual(nil, beamtalk_class_vars:clear(?C, x)),
        ?assertNot(beamtalk_class_vars:has(?C, x)),
        ?assertEqual(nil, beamtalk_class_vars:get(?C, x))
    end).

put_without_key_unreachable_test() ->
    with_clean(fun() ->
        E = raised_error(fun() -> beamtalk_class_vars:put(?C, x, 1) end),
        ?assertMatch(
            #beamtalk_error{
                kind = class_state_unreachable, details = #{class_variable := x}, hint = H
            } when is_binary(H),
            E
        ),
        ?assertEqual(undefined, erlang:get(beamtalk_class_vars:key(?C)))
    end).

get_without_key_unreachable_test() ->
    with_clean(fun() ->
        ?assertEqual(
            class_state_unreachable, raised_kind(fun() -> beamtalk_class_vars:get(?C, x) end)
        )
    end).

nil_receiver_test() ->
    Fns = [
        fun() -> beamtalk_class_vars:key(nil) end,
        fun() -> beamtalk_class_vars:get(nil, x) end,
        fun() -> beamtalk_class_vars:get_late(nil, x) end,
        fun() -> beamtalk_class_vars:put(nil, x, 1) end,
        fun() -> beamtalk_class_vars:clear(nil, x) end,
        fun() -> beamtalk_class_vars:has(nil, x) end,
        fun() -> beamtalk_class_vars:get(nil, x, #{}) end,
        fun() -> beamtalk_class_vars:capture(nil, #{}) end
    ],
    [?assertEqual(internal_error, raised_kind(F)) || F <- Fns].

%%% captured fallback forms / capture

captured_fallback_test() ->
    with_clean(fun() ->
        Captured = #{a => 1, l => nil},
        ?assertEqual(1, beamtalk_class_vars:get(?C, a, Captured)),
        ?assert(beamtalk_class_vars:has(?C, a, Captured)),
        ?assertNot(beamtalk_class_vars:has(?C, l, Captured)),
        ?assertEqual(1, beamtalk_class_vars:get_late(?C, a, Captured)),
        ?assertEqual(
            uninitialized_state_error,
            raised_kind(fun() -> beamtalk_class_vars:get_late(?C, l, Captured) end)
        ),
        %% A live key wins over the captured map.
        beamtalk_class_vars:install(?C, #{a => 99}),
        ?assertEqual(99, beamtalk_class_vars:get(?C, a, Captured))
    end).

capture_test() ->
    with_clean(fun() ->
        ?assertEqual(#{o => 1}, beamtalk_class_vars:capture(?C, #{o => 1})),
        beamtalk_class_vars:install(?C, #{live => 2}),
        ?assertEqual(#{live => 2}, beamtalk_class_vars:capture(?C, #{o => 1}))
    end).

%%% snapshot / restore / protect

snapshot_restore_test() ->
    with_clean(fun() ->
        ?assertEqual(none, beamtalk_class_vars:snapshot()),
        beamtalk_class_vars:install(?C, #{a => 1}),
        Snap = beamtalk_class_vars:snapshot(),
        ?assertEqual({beamtalk_class_vars:key(?C), #{a => 1}}, Snap),
        beamtalk_class_vars:put(?C, a, 2),
        ok = beamtalk_class_vars:restore(Snap),
        ?assertEqual(1, beamtalk_class_vars:get(?C, a))
    end).

restore_none_plants_nothing_test() ->
    with_clean(fun() ->
        Before = lists:sort(erlang:get_keys()),
        ok = beamtalk_class_vars:restore(none),
        ?assertEqual(Before, lists:sort(erlang:get_keys()))
    end).

protect_success_keeps_writes_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{a => 1}),
        ?assertEqual(
            done,
            beamtalk_class_vars:protect(fun() ->
                beamtalk_class_vars:put(?C, a, 2),
                done
            end)
        ),
        ?assertEqual(2, beamtalk_class_vars:get(?C, a))
    end).

protect_restores_on_exception_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{a => 1}),
        ?assertError(
            boom,
            beamtalk_class_vars:protect(fun() ->
                beamtalk_class_vars:put(?C, a, 2),
                error(boom)
            end)
        ),
        ?assertEqual(1, beamtalk_class_vars:get(?C, a)),
        ?assertThrow(
            plain,
            beamtalk_class_vars:protect(fun() ->
                beamtalk_class_vars:put(?C, a, 3),
                throw(plain)
            end)
        ),
        ?assertEqual(1, beamtalk_class_vars:get(?C, a))
    end).

protect_passes_nlr_without_restore_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{a => 1}),
        Nlr3 = {'$bt_nlr', tok, val},
        Nlr4 = {'$bt_nlr', tok, val, state},
        [
            begin
                ?assertThrow(
                    Nlr,
                    beamtalk_class_vars:protect(fun() ->
                        beamtalk_class_vars:put(?C, a, 7),
                        throw(Nlr)
                    end)
                ),
                ?assertEqual(7, beamtalk_class_vars:get(?C, a)),
                beamtalk_class_vars:put(?C, a, 1)
            end
         || Nlr <- [Nlr3, Nlr4]
        ]
    end).

protect_without_home_plants_nothing_test() ->
    with_clean(fun() ->
        Before = lists:sort(erlang:get_keys()),
        ?assertError(boom, beamtalk_class_vars:protect(fun() -> error(boom) end)),
        ?assertEqual(Before, lists:sort(erlang:get_keys())),
        ?assertEqual(ok, beamtalk_class_vars:protect(fun() -> ok end))
    end).

%%% with_snapshot

with_snapshot_reads_mirror_by_name_test() ->
    with_clean(fun() ->
        Pid = start_fake_class(#{n => 10}),
        try
            Result = beamtalk_class_vars:with_snapshot(?C, fun() ->
                {
                    beamtalk_class_vars:get(?C, n),
                    beamtalk_class_vars:has(?C, n),
                    beamtalk_class_vars:capture(?C, #{})
                }
            end),
            ?assertEqual({10, true, #{n => 10}}, Result),
            ?assertEqual(undefined, erlang:get(beamtalk_class_vars:key(?C))),
            ?assertEqual(undefined, erlang:get(?HOME))
        after
            stop_fake_class(Pid)
        end
    end).

with_snapshot_write_raises_test() ->
    with_clean(fun() ->
        Pid = start_fake_class(#{n => 10}),
        try
            E = raised_error(fun() ->
                beamtalk_class_vars:with_snapshot(?C, fun() -> beamtalk_class_vars:put(?C, n, 1) end)
            end),
            ?assertMatch(
                #beamtalk_error{
                    kind = class_state_read_only, details = #{class_variable := n}, hint = H
                } when is_binary(H),
                E
            ),
            ?assertEqual(
                class_state_read_only,
                raised_kind(fun() ->
                    beamtalk_class_vars:with_snapshot(?C, fun() ->
                        beamtalk_class_vars:clear(?C, n)
                    end)
                end)
            )
        after
            stop_fake_class(Pid)
        end
    end).

with_snapshot_erases_on_raise_test() ->
    with_clean(fun() ->
        ?assertError(
            boom, beamtalk_class_vars:with_snapshot(?C, fun() -> error(boom) end)
        ),
        ?assertEqual(undefined, erlang:get(beamtalk_class_vars:key(?C)))
    end).

with_snapshot_resolves_after_restart_test() ->
    with_clean(fun() ->
        Pid1 = start_fake_class(#{n => 1}),
        try
            beamtalk_class_vars:with_snapshot(?C, fun() ->
                ?assertEqual(1, beamtalk_class_vars:get(?C, n)),
                %% Simulate a class-process restart: new pid, new mirror.
                stop_fake_class(Pid1),
                Pid2 = start_fake_class(#{n => 2}),
                try
                    ?assertEqual(2, beamtalk_class_vars:get(?C, n))
                after
                    stop_fake_class(Pid2)
                end
            end)
        after
            catch stop_fake_class(Pid1)
        end
    end).

with_snapshot_leaves_outer_live_key_alone_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?C, #{a => 1}),
        Key = beamtalk_class_vars:key(?C),
        beamtalk_class_vars:with_snapshot(?C, fun() ->
            %% Outer live key still live: writes succeed.
            beamtalk_class_vars:put(?C, a, 2)
        end),
        ?assertEqual(#{a => 2}, erlang:get(Key)),
        ?assertEqual(Key, erlang:get(?HOME))
    end).

with_snapshot_never_touches_home_test() ->
    with_clean(fun() ->
        OtherKey = beamtalk_class_vars:key('OtherClass'),
        erlang:put(?HOME, OtherKey),
        erlang:put(OtherKey, #{z => 1}),
        beamtalk_class_vars:with_snapshot(?C, fun() ->
            ?assertEqual(OtherKey, erlang:get(?HOME))
        end),
        ?assertEqual(OtherKey, erlang:get(?HOME)),
        ?assertEqual(#{z => 1}, erlang:get(OtherKey))
    end).
