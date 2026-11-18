%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_remove_supervised_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4936: `removeFromSystem` refuses (structured error, nothing killed, nothing
removed) while any supervisor holds a child of the class — static spec,
dynamic (`simple_one_for_one`) child, nested tree, or a normally exited
`transient` child whose spec remains. Raw OTP supervisors stand in for
Beamtalk ones; they join the same `pg` instance group that
`beamtalk_supervisor:instance_pids/0` reads.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([init/1]).

-define(GROUP, beamtalk_supervisor_instances).

%%% ============================================================================
%%% Fixtures
%%% ============================================================================

setup() ->
    beamtalk_test_boot:registry(),
    _ = pg:start(pg),
    ok = beamtalk_capability:set(#{mode => workspace, include_compiler => true}),
    ok.

teardown(_) ->
    ok = beamtalk_capability:clear().

removal_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun refuses_static_permanent_child/0,
        fun crash_after_refusal_keeps_supervisor_alive/0,
        fun refuses_dynamic_child/0,
        fun refuses_nested_tree/0,
        fun refuses_transient_child_that_exited_normally/0,
        fun removal_succeeds_once_child_is_deleted/0,
        fun refuses_factory_child_matched_by_pid/0,
        {timeout, 30, fun refuses_when_supervisor_cannot_be_queried/0},
        fun unsupervised_class_still_removed/0
    ]}.

%%% ============================================================================
%%% Tests
%%% ============================================================================

%% The BT-4936 reproduction, as it now behaves: a permanent supervised child.
%% Before the fix removeFromSystem returned nil, left a zombie on a deleted
%% module, and the child's next crash killed the supervisor.
refuses_static_permanent_child() ->
    with_fixture(permanent, static, fun(#{class := C, sup := Sup, child := Child}) ->
        Err = remove_error(C),
        ?assertMatch(#beamtalk_error{kind = runtime_error}, Err),
        ?assertNotEqual(nomatch, binary:match(msg(Err), <<"supervisor">>)),
        ?assertMatch(#{child_id := child1}, Err#beamtalk_error.details),
        %% Nothing killed, nothing removed.
        ?assert(is_process_alive(Child)),
        ?assert(is_process_alive(Sup)),
        ?assertNotEqual(undefined, beamtalk_class_registry:whereis_class(C))
    end).

crash_after_refusal_keeps_supervisor_alive() ->
    with_fixture(permanent, static, fun(#{class := C, sup := Sup, child := Child}) ->
        _ = remove_error(C),
        exit(Child, kill),
        %% Wait for the supervisor to restart the child.
        ok = beamtalk_test_wait:until(fun() ->
            case supervisor:which_children(Sup) of
                [{child1, P, _, _}] -> is_pid(P) andalso P =/= Child;
                _ -> false
            end
        end),
        ?assert(is_process_alive(Sup)),
        [{child1, NewChild, _, _}] = supervisor:which_children(Sup),
        ?assert(is_pid(NewChild))
    end).

refuses_dynamic_child() ->
    with_fixture(permanent, dynamic, fun(#{class := C, sup := Sup, child := Child}) ->
        ?assertMatch(#beamtalk_error{}, remove_error(C)),
        ?assert(is_process_alive(Child)),
        ?assert(is_process_alive(Sup))
    end).

refuses_nested_tree() ->
    with_fixture(permanent, nested, fun(#{class := C, child := Child}) ->
        ?assertMatch(#beamtalk_error{}, remove_error(C)),
        ?assert(is_process_alive(Child))
    end).

%% A transient child that exits normally is not restarted, but its spec stays
%% in the supervisor, so a later restart would hit the deleted module.
refuses_transient_child_that_exited_normally() ->
    with_fixture(transient, static, fun(#{class := C, sup := Sup, child := Child}) ->
        Child ! stop_normal,
        ok = beamtalk_test_wait:dead(Child),
        ?assertNot(is_process_alive(Child)),
        ?assertMatch(#beamtalk_error{}, remove_error(C)),
        ?assert(is_process_alive(Sup))
    end).

removal_succeeds_once_child_is_deleted() ->
    with_fixture(permanent, static, fun(#{class := C, sup := Sup, module := M}) ->
        ?assertMatch(#beamtalk_error{}, remove_error(C)),
        ok = supervisor:terminate_child(Sup, child1),
        ok = supervisor:delete_child(Sup, child1),
        ?assertEqual(nil, beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(C)),
        ?assertEqual(undefined, beamtalk_class_registry:whereis_class(C)),
        ?assertEqual(false, code:is_loaded(M) =/= false)
    end).

%% A factory child can carry a module other than the class's; the class's live
%% actor pid under a supervisor still counts.
refuses_factory_child_matched_by_pid() ->
    with_fixture(permanent, factory, fun(#{class := C, child := Child}) ->
        ?assertMatch(#beamtalk_error{}, remove_error(C)),
        ?assert(is_process_alive(Child))
    end).

%% Fail closed: a supervisor that does not answer must refuse, not be skipped.
refuses_when_supervisor_cannot_be_queried() ->
    with_fixture(permanent, static, fun(#{class := C, sup := Sup, child := Child}) ->
        true = erlang:suspend_process(Sup),
        try
            ?assertMatch(#beamtalk_error{}, remove_error(C))
        after
            erlang:resume_process(Sup)
        end,
        ?assert(is_process_alive(Child)),
        ?assertNotEqual(undefined, beamtalk_class_registry:whereis_class(C))
    end).

unsupervised_class_still_removed() ->
    {C, M} = load_class(unsup),
    ?assertEqual(nil, beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(C)),
    ?assertEqual(undefined, beamtalk_class_registry:whereis_class(C)),
    _ = M,
    ok.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

remove_error(Class) ->
    #beamtalk_error{} =
        beamtalk_test_assert:raised_error(fun() ->
            beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(Class)
        end).

msg(#beamtalk_error{message = {beamtalk_string, M}}) -> M.

%% Loads a disposable class + child module, starts a supervisor tree of the
%% requested shape (registered in the instances group), runs Fun, tears down.
with_fixture(Restart, Shape, Fun) ->
    {Class, Mod} = load_class(Shape),
    {ok, Sup} = start_sup(Shape, Restart, Mod),
    try
        Child = first_child(Sup, Shape),
        Fun(#{class => Class, module => Mod, sup => Sup, child => Child})
    after
        try
            unlink(Sup),
            exit(Sup, shutdown)
        catch
            _:_ -> ok
        end,
        ok = beamtalk_test_wait:dead(Sup),
        case beamtalk_class_registry:whereis_class(Class) of
            undefined -> ok;
            P -> beamtalk_test_proc:stop(P)
        end,
        beamtalk_test_module:unload(Mod)
    end.

load_class(Tag) ->
    Mod = list_to_atom("bt4936_child_" ++ atom_to_list(Tag)),
    Class = list_to_atom("BT4936Class_" ++ atom_to_list(Tag)),
    M = atom_to_list(Mod),
    Forms = beamtalk_test_erl_forms:parse_forms([
        "-module(" ++ M ++ ").",
        "-export([start_link/0, init/1, loop/0]).",
        "start_link() -> proc_lib:start_link(" ++ M ++ ", init, [self()]).",
        "init(Parent) -> proc_lib:init_ack(Parent, {ok, self()}), loop().",
        "loop() -> receive stop_normal -> ok after infinity -> " ++ M ++ ":loop() end."
    ]),
    ok = beamtalk_test_module:load(Mod, Forms),
    {ok, _} = beamtalk_object_class:start(Class, #{
        name => Class,
        module => Mod,
        superclass => 'Actor',
        instance_methods => #{},
        class_methods => #{}
    }),
    {Class, Mod}.

child_spec(Id, Restart, Mod) ->
    #{id => Id, start => {Mod, start_link, []}, restart => Restart, modules => [Mod]}.

start_sup(static, Restart, Mod) ->
    start_sup_with([child_spec(child1, Restart, Mod)]);
start_sup(factory, Restart, Mod) ->
    %% modules deliberately do not name the class's module.
    start_sup_with([(child_spec(child1, Restart, Mod))#{modules => [other_mod]}]);
start_sup(dynamic, Restart, Mod) ->
    {ok, Sup} = supervisor:start_link(?MODULE, {dynamic, Restart, Mod}),
    pg:join(?GROUP, Sup),
    {ok, _} = supervisor:start_child(Sup, []),
    {ok, Sup};
start_sup(nested, Restart, Mod) ->
    Inner = #{
        id => inner,
        start => {supervisor, start_link, [?MODULE, {static, [child_spec(child1, Restart, Mod)]}]},
        restart => permanent,
        type => supervisor,
        modules => [?MODULE]
    },
    {ok, Outer} = supervisor:start_link(?MODULE, {static, [Inner]}),
    pg:join(?GROUP, Outer),
    %% Only the outer supervisor is a registered instance in some paths; also
    %% register the inner one as Beamtalk's init does for every supervisor.
    [{inner, InnerPid, _, _}] = supervisor:which_children(Outer),
    pg:join(?GROUP, InnerPid),
    {ok, Outer};
start_sup(unsup, _, _) ->
    {ok, spawn(fun() -> ok end)}.

start_sup_with(Specs) ->
    {ok, Sup} = supervisor:start_link(?MODULE, {static, Specs}),
    pg:join(?GROUP, Sup),
    {ok, Sup}.

first_child(Sup, nested) ->
    [{inner, Inner, _, _}] = supervisor:which_children(Sup),
    first_child(Inner, static);
first_child(Sup, _) ->
    [{_, Pid, _, _} | _] = supervisor:which_children(Sup),
    Pid.

%% supervisor callback for the raw supervisors above
init({static, Specs}) ->
    {ok, {#{strategy => one_for_one, intensity => 3, period => 5}, Specs}};
init({dynamic, Restart, Mod}) ->
    {ok,
        {#{strategy => simple_one_for_one, intensity => 3, period => 5}, [
            (child_spec(undefined, Restart, Mod))#{id => dyn}
        ]}}.
