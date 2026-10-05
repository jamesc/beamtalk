%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_vars_tests).
-moduledoc "EUnit tests for beamtalk_class_vars (ADR 0130 Phase 2).".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HOME, '$bt_class_vars_home').
-define(C, 'ClassVarsTestClass').
-define(KEY, {'$bt_class_vars', 'ClassVarsTestClass class'}).

%%% Helpers

self_obj() ->
    #beamtalk_object{class = 'ClassVarsTestClass class', class_mod = cvtc, pid = self()}.

clean() ->
    erlang:erase(?KEY),
    erlang:erase({'$bt_class_vars', 'OtherClass class'}),
    erlang:erase(?HOME),
    ok.

with_clean(Fun) ->
    clean(),
    try
        Fun()
    after
        clean()
    end.

%% Run Fun with declared class variables `x`, `late` (and `a`) mocked for ?C.
with_declared(Fun) ->
    meck:new(beamtalk_behaviour_intrinsics, [passthrough, no_link]),
    meck:expect(beamtalk_behaviour_intrinsics, classAllClassVarKindsByName, fun
        (?C) -> #{x => eager, a => eager, late => late};
        (_) -> #{}
    end),
    try
        with_clean(Fun)
    after
        meck:unload(beamtalk_behaviour_intrinsics)
    end.

raised_error(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{} = E} -> E
    end.

raised_kind(Fun) ->
    case raised_error(Fun) of
        #beamtalk_error{kind = Kind} -> Kind;
        no_error -> no_error
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
    ?assertEqual(?KEY, beamtalk_class_vars:key(?C)).

install_uninstall_test() ->
    with_clean(fun() ->
        ok = beamtalk_class_vars:install(?KEY, #{a => 1}),
        ?assertEqual(#{a => 1}, erlang:get(?KEY)),
        ?assertEqual(?KEY, erlang:get(?HOME)),
        ok = beamtalk_class_vars:uninstall(?KEY),
        ?assertEqual(undefined, erlang:get(?KEY)),
        ?assertEqual(undefined, erlang:get(?HOME))
    end).

assert_absent_ok_test() ->
    with_clean(fun() -> ?assertEqual(ok, beamtalk_class_vars:assert_absent(?KEY)) end).

assert_absent_rejects_key_test() ->
    with_clean(fun() ->
        erlang:put(?KEY, #{}),
        ?assertEqual(
            internal_error, raised_kind(fun() -> beamtalk_class_vars:assert_absent(?KEY) end)
        )
    end).

assert_absent_rejects_home_test() ->
    with_clean(fun() ->
        erlang:put(?HOME, {'$bt_class_vars', 'OtherClass class'}),
        ?assertEqual(
            internal_error, raised_kind(fun() -> beamtalk_class_vars:assert_absent(?KEY) end)
        )
    end).

%%% get / get_late / put / clear / has

get_put_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{count => 0}),
        ?assertEqual(0, beamtalk_class_vars:get(self_obj(), count)),
        ?assertEqual(5, beamtalk_class_vars:put(self_obj(), count, 5)),
        ?assertEqual(5, beamtalk_class_vars:get(self_obj(), count))
    end).

get_undeclared_test() ->
    with_declared(fun() ->
        beamtalk_class_vars:install(?KEY, #{x => 0}),
        ?assertEqual(
            undeclared_class_variable,
            raised_kind(fun() -> beamtalk_class_vars:get(self_obj(), nope) end)
        ),
        %% `hasField:` never raises on the name.
        ?assertEqual(false, beamtalk_class_vars:has(self_obj(), nope)),
        ?assertEqual(false, beamtalk_class_vars:has(self_obj(), nope, #{}))
    end).

%% No metadata (ClassBuilder / dynamic class) or no live class: kinds are
%% unknown, so a read of an absent name answers nil like get_class_var.
get_without_metadata_reads_nil_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{}),
        ?assertEqual(nil, beamtalk_class_vars:get(self_obj(), anything)),
        ?assertEqual(nil, beamtalk_class_vars:get(self_obj(), anything, #{}))
    end).

cleared_then_read_test() ->
    with_declared(fun() ->
        beamtalk_class_vars:install(?KEY, #{x => 1}),
        beamtalk_class_vars:clear(self_obj(), x),
        ?assertEqual(nil, beamtalk_class_vars:get(self_obj(), x))
    end),
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{x => 1}),
        beamtalk_class_vars:clear(self_obj(), x),
        ?assertEqual(nil, beamtalk_class_vars:get(self_obj(), x))
    end).

declared_but_absent_reads_nil_test() ->
    with_declared(fun() ->
        beamtalk_class_vars:install(?KEY, #{}),
        ?assertEqual(nil, beamtalk_class_vars:get(self_obj(), x)),
        ?assertNot(beamtalk_class_vars:has(self_obj(), x))
    end).

get_late_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{late => nil, set => 3}),
        ?assertEqual(3, beamtalk_class_vars:get_late(self_obj(), set)),
        E1 = raised_error(fun() -> beamtalk_class_vars:get_late(self_obj(), late) end),
        ?assertMatch(
            #beamtalk_error{kind = uninitialized_state_error, selector = undefined, hint = H} when
                is_binary(H),
            E1
        ),
        ?assertEqual(
            uninitialized_state_error,
            raised_kind(fun() -> beamtalk_class_vars:get_late(self_obj(), missing) end)
        )
    end).

clear_and_has_test() ->
    with_declared(fun() ->
        beamtalk_class_vars:install(?KEY, #{x => 1}),
        ?assert(beamtalk_class_vars:has(self_obj(), x)),
        ?assertEqual(self_obj(), beamtalk_class_vars:clear(self_obj(), x)),
        ?assertNot(beamtalk_class_vars:has(self_obj(), x)),
        ?assertEqual(nil, beamtalk_class_vars:get(self_obj(), x))
    end).

put_without_key_unreachable_test() ->
    with_clean(fun() ->
        E = raised_error(fun() -> beamtalk_class_vars:put(self_obj(), x, 1) end),
        ?assertMatch(
            #beamtalk_error{
                kind = class_state_unreachable,
                message =
                    <<"ClassVarsTestClass's class variable x cannot be written from this process">>,
                details = #{class_variable := x},
                hint = H
            } when is_binary(H),
            E
        ),
        ?assertEqual(undefined, erlang:get(?KEY))
    end).

get_without_key_unreachable_test() ->
    with_clean(fun() ->
        ?assertEqual(
            class_state_unreachable,
            raised_kind(fun() -> beamtalk_class_vars:get(self_obj(), x) end)
        ),
        %% Captured `none` (method-level capture taken abroad) also raises.
        ?assertEqual(
            class_state_unreachable,
            raised_kind(fun() -> beamtalk_class_vars:get(self_obj(), x, none) end)
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
        fun() -> beamtalk_class_vars:capture(nil, #{}) end,
        fun() -> beamtalk_class_vars:with_snapshot(nil, fun() -> ok end) end
    ],
    [?assertEqual(internal_error, raised_kind(F)) || F <- Fns].

non_class_receiver_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{a => 1}),
        %% Instance receiver (tag without ` class` suffix): internal error, map untouched.
        Instance = #beamtalk_object{class = ?C, class_mod = cvtc, pid = self()},
        ?assertEqual(
            internal_error, raised_kind(fun() -> beamtalk_class_vars:put(Instance, a, 2) end)
        ),
        ?assertEqual(#{a => 1}, erlang:get(?KEY)),
        %% Unknown base atom: structured error, not badarg.
        Unknown = #beamtalk_object{
            class = 'NoSuchClassAtomBT3706Xq class', class_mod = cvtc, pid = self()
        },
        ?assertEqual(
            internal_error, raised_kind(fun() -> beamtalk_class_vars:get(Unknown, a) end)
        )
    end).

%%% captured fallback forms / capture

captured_fallback_test() ->
    with_clean(fun() ->
        Captured = #{a => 1, l => nil},
        ?assertEqual(1, beamtalk_class_vars:get(self_obj(), a, Captured)),
        ?assert(beamtalk_class_vars:has(self_obj(), a, Captured)),
        ?assertEqual(1, beamtalk_class_vars:get_late(self_obj(), a, Captured)),
        ?assertEqual(
            uninitialized_state_error,
            raised_kind(fun() -> beamtalk_class_vars:get_late(self_obj(), l, Captured) end)
        ),
        %% A live key wins over the captured map.
        beamtalk_class_vars:install(?KEY, #{a => 99}),
        ?assertEqual(99, beamtalk_class_vars:get(self_obj(), a, Captured))
    end).

capture_test() ->
    with_clean(fun() ->
        ?assertEqual(#{o => 1}, beamtalk_class_vars:capture(self_obj(), #{o => 1})),
        ?assertEqual(none, beamtalk_class_vars:capture(self_obj(), none)),
        beamtalk_class_vars:install(?KEY, #{live => 2}),
        ?assertEqual(#{live => 2}, beamtalk_class_vars:capture(self_obj(), #{o => 1}))
    end).

%%% snapshot / restore / protect

snapshot_restore_test() ->
    with_clean(fun() ->
        ?assertEqual(none, beamtalk_class_vars:snapshot()),
        beamtalk_class_vars:install(?KEY, #{a => 1}),
        Snap = beamtalk_class_vars:snapshot(),
        ?assertEqual({?KEY, #{a => 1}}, Snap),
        beamtalk_class_vars:put(self_obj(), a, 2),
        ok = beamtalk_class_vars:restore(Snap),
        ?assertEqual(1, beamtalk_class_vars:get(self_obj(), a))
    end).

restore_none_plants_nothing_test() ->
    with_clean(fun() ->
        Before = lists:sort(erlang:get_keys()),
        ok = beamtalk_class_vars:restore(none),
        ?assertEqual(Before, lists:sort(erlang:get_keys()))
    end).

protect_success_keeps_writes_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{a => 1}),
        ?assertEqual(
            done,
            beamtalk_class_vars:protect(fun() ->
                beamtalk_class_vars:put(self_obj(), a, 2),
                done
            end)
        ),
        ?assertEqual(2, beamtalk_class_vars:get(self_obj(), a))
    end).

protect_restores_on_exception_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{a => 1}),
        ?assertError(
            boom,
            beamtalk_class_vars:protect(fun() ->
                beamtalk_class_vars:put(self_obj(), a, 2),
                error(boom)
            end)
        ),
        ?assertEqual(1, beamtalk_class_vars:get(self_obj(), a)),
        ?assertThrow(
            plain,
            beamtalk_class_vars:protect(fun() ->
                beamtalk_class_vars:put(self_obj(), a, 3),
                throw(plain)
            end)
        ),
        ?assertEqual(1, beamtalk_class_vars:get(self_obj(), a))
    end).

protect_passes_nlr_without_restore_test() ->
    with_clean(fun() ->
        beamtalk_class_vars:install(?KEY, #{a => 1}),
        [
            begin
                ?assertThrow(
                    Nlr,
                    beamtalk_class_vars:protect(fun() ->
                        beamtalk_class_vars:put(self_obj(), a, 7),
                        throw(Nlr)
                    end)
                ),
                ?assertEqual(7, beamtalk_class_vars:get(self_obj(), a)),
                beamtalk_class_vars:put(self_obj(), a, 1)
            end
         || Nlr <- [{'$bt_nlr', tok, val}, {'$bt_nlr', tok, val, state}]
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
            Result = beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
                {
                    beamtalk_class_vars:get(self_obj(), n),
                    beamtalk_class_vars:has(self_obj(), n),
                    beamtalk_class_vars:capture(self_obj(), none)
                }
            end),
            ?assertEqual({10, true, #{n => 10}}, Result),
            ?assertEqual(undefined, erlang:get(?KEY)),
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
                beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
                    beamtalk_class_vars:put(self_obj(), n, 1)
                end)
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
                    beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
                        beamtalk_class_vars:clear(self_obj(), n)
                    end)
                end)
            ),
            ?assertEqual(undefined, erlang:get(?KEY))
        after
            stop_fake_class(Pid)
        end
    end).

with_snapshot_no_live_class_test() ->
    with_clean(fun() ->
        E = raised_error(fun() ->
            beamtalk_class_vars:with_snapshot(self_obj(), fun() -> ok end)
        end),
        ?assertMatch(#beamtalk_error{kind = class_state_unreachable}, E),
        ?assertEqual(undefined, erlang:get(?KEY))
    end).

mirror_read_with_dead_class_unreachable_test() ->
    with_clean(fun() ->
        Pid = start_fake_class(#{n => 1}),
        try
            beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
                stop_fake_class(Pid),
                ?assertEqual(
                    class_state_unreachable,
                    raised_kind(fun() -> beamtalk_class_vars:get(self_obj(), n) end)
                ),
                ?assertEqual(
                    class_state_unreachable,
                    raised_kind(fun() -> beamtalk_class_vars:capture(self_obj(), none) end)
                )
            end)
        after
            catch stop_fake_class(Pid)
        end
    end).

with_snapshot_erases_on_raise_test() ->
    with_clean(fun() ->
        Pid = start_fake_class(#{}),
        try
            ?assertError(
                boom, beamtalk_class_vars:with_snapshot(self_obj(), fun() -> error(boom) end)
            ),
            ?assertEqual(undefined, erlang:get(?KEY))
        after
            stop_fake_class(Pid)
        end
    end).

with_snapshot_resolves_after_restart_test() ->
    with_clean(fun() ->
        Pid1 = start_fake_class(#{n => 1}),
        try
            beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
                ?assertEqual(1, beamtalk_class_vars:get(self_obj(), n)),
                %% Simulate a class-process restart: new pid, new mirror.
                stop_fake_class(Pid1),
                Pid2 = start_fake_class(#{n => 2}),
                try
                    ?assertEqual(2, beamtalk_class_vars:get(self_obj(), n))
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
        beamtalk_class_vars:install(?KEY, #{a => 1}),
        beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
            %% Outer live key still live: writes succeed.
            beamtalk_class_vars:put(self_obj(), a, 2)
        end),
        ?assertEqual(#{a => 2}, erlang:get(?KEY)),
        ?assertEqual(?KEY, erlang:get(?HOME))
    end).

with_snapshot_never_touches_home_test() ->
    with_clean(fun() ->
        Pid = start_fake_class(#{n => 1}),
        try
            OtherKey = {'$bt_class_vars', 'OtherClass class'},
            erlang:put(?HOME, OtherKey),
            erlang:put(OtherKey, #{z => 1}),
            beamtalk_class_vars:with_snapshot(self_obj(), fun() ->
                ?assertEqual(OtherKey, erlang:get(?HOME))
            end),
            ?assertEqual(OtherKey, erlang:get(?HOME)),
            ?assertEqual(#{z => 1}, erlang:get(OtherKey))
        after
            stop_fake_class(Pid)
        end
    end).
