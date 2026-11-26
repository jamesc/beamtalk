%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_class_call_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_supervisor_class_call` error paths that lacked coverage
per the nightly erlang-coverage-reports artifact (56% / 11 of 25 lines uncovered,
run 44908043238, 2026-11-21).

Covered here:
- Line 69  — `false` branch of `function_exported` in `call_class_method_direct/5`
             triggers `call_inherited_class_method_direct/5`.
- Lines 81  — depth > 30 guard: `error({supervisor_init_method_not_found, …})`.
- Lines 83–86 — main clause of `call_inherited_class_method_direct/5`: `lookup_superclass`
             returns `not_found` → same error.
- Line 94  — `lookup_module` returns `not_found` for a known superclass → recurse.
- Lines 97, 101, 103, 105 — `lookup_module` returns `{ok, Module}`:
             method found → call it (lines 97, 101);
             method absent → recurse (lines 103, 105).
- Line 152 — `not_found` branch of `child_class_name/1`: unregistered module atom
             returned as-is.

No Beamtalk runtime is started.  `beamtalk_class_metadata:field/2` safely returns
`undefined` (catching `badarg`) when the ETS table is absent, so tests that need
the hierarchy walk create the table via `beamtalk_class_metadata:insert/5` (which
calls `new/0` internally).  `beamtalk_class_derived_cache:fetch/2` computes
directly when no generation counter exists in `persistent_term`, so the module-to-
class map lookup is safe without a full runtime.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% Fake ancestor class method used by test 4 (call_inherited_calls_ancestor_module).
-export([class_ancestor_method_bt5099/1]).

%%====================================================================
%% Tests: actor_child_start_error/2 — child_class_name not_found branch (line 152)
%%====================================================================

%% When the child module is not registered in the class registry,
%% `child_class_name/1` falls through to `not_found -> ChildModule` (line 152)
%% and returns the module atom itself.  `actor_child_start_error/2` with `timeout`
%% exercises that path and builds an `instantiation_error` for the atom.
%%
%% No ETS or runtime setup required: `beamtalk_class_metadata:foldl_modules/2`
%% returns its empty accumulator when the ETS table is absent, and
%% `beamtalk_class_derived_cache:fetch/2` computes directly (no generation counter).
actor_child_start_error_timeout_uses_module_as_class_name_test() ->
    Module = bt_unregistered_module_bt5099,
    Result = beamtalk_supervisor_class_call:actor_child_start_error(Module, timeout),
    ?assertMatch(
        {initialize_failed, #beamtalk_error{kind = instantiation_error, class = Module}}, Result
    ).

%%====================================================================
%% Tests: call_class_method_direct/5 — false branch + no superclass (lines 69, 83–86)
%%====================================================================

%% When the given module does not export the requested function, `call_class_method_direct`
%% falls into the `false` branch (line 69) and calls `call_inherited_class_method_direct`.
%% With no superclass registered for the class, `lookup_superclass` returns `not_found`
%% (lines 83–86) and the function raises `{supervisor_init_method_not_found, FunName}`.
%%
%% No ETS rows are needed: `beamtalk_class_metadata:field/2` catches `badarg` for a
%% missing table (or missing key) and returns `undefined`, which `lookup_superclass/1`
%% maps to `not_found`.
call_class_method_direct_missing_method_no_superclass_raises_test() ->
    ClassName = 'BtCallTestNoSup',
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    ClassSelf = #beamtalk_object{class = ClassTag, class_mod = ?MODULE, pid = self()},
    ?assertError(
        {supervisor_init_method_not_found, bt_nonexistent_fun_bt5099},
        beamtalk_supervisor_class_call:call_class_method_direct(
            ClassName, ?MODULE, bt_nonexistent_fun_bt5099, ClassSelf, []
        )
    ).

%%====================================================================
%% Tests: call_inherited_class_method_direct/5 — superclass with no module (line 94)
%%====================================================================

%% When a superclass is registered but its module is absent from ETS,
%% `lookup_module` returns `not_found` (line 94) and the walk recurses upward
%% until `lookup_superclass` returns `not_found`, then raises.
call_class_method_direct_ancestor_without_module_raises_test() ->
    ClassName = 'BtCallTestNoMod',
    ParentName = 'BtCallTestNoModParent',
    %% Insert the child with a known superclass, but do NOT insert the parent:
    %% lookup_module(ParentName) will return not_found.
    beamtalk_class_metadata:insert(
        ClassName, nonexistent_child_mod_bt5099, [], ParentName, undefined
    ),
    try
        ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
        ClassSelf = #beamtalk_object{
            class = ClassTag, class_mod = nonexistent_child_mod_bt5099, pid = self()
        },
        ?assertError(
            {supervisor_init_method_not_found, bt_nonexistent_fun_bt5099},
            beamtalk_supervisor_class_call:call_class_method_direct(
                ClassName, nonexistent_child_mod_bt5099, bt_nonexistent_fun_bt5099, ClassSelf, []
            )
        )
    after
        beamtalk_class_metadata:delete(ClassName)
    end.

%%====================================================================
%% Tests: call_inherited_class_method_direct/5 — method found in ancestor (lines 97, 101)
%%====================================================================

%% `class_ancestor_method_bt5099/1` is exported by this module so that the
%% inheritance walk can find and invoke it as the ancestor's class method.
class_ancestor_method_bt5099(ClassSelf) ->
    {ok, ancestor_called, ClassSelf}.

%% When an ancestor module IS registered and exports the method, `call_inherited`
%% finds it via `lookup_module` (line 97) and calls it (line 101).
call_class_method_direct_finds_and_calls_ancestor_method_test() ->
    ChildName = 'BtCallTestAncestor',
    ParentName = 'BtCallTestAncestorParent',
    %% Parent is registered with this test module, which exports class_ancestor_method_bt5099/1.
    beamtalk_class_metadata:insert(
        ChildName, nonexistent_child_mod2_bt5099, [], ParentName, undefined
    ),
    beamtalk_class_metadata:insert(ParentName, ?MODULE, [], undefined, undefined),
    try
        ChildTag = beamtalk_class_registry:class_object_tag(ChildName),
        ClassSelf = #beamtalk_object{
            class = ChildTag, class_mod = nonexistent_child_mod2_bt5099, pid = self()
        },
        Result = beamtalk_supervisor_class_call:call_class_method_direct(
            ChildName, nonexistent_child_mod2_bt5099, class_ancestor_method_bt5099, ClassSelf, []
        ),
        ?assertMatch({ok, ancestor_called, _}, Result)
    after
        beamtalk_class_metadata:delete(ChildName),
        beamtalk_class_metadata:delete(ParentName)
    end.

%%====================================================================
%% Tests: call_inherited_class_method_direct/5 — module found, method absent,
%%        recurse to grandparent (lines 103–105)
%%====================================================================

%% When an intermediate ancestor's module IS registered but does NOT export
%% the method, the walk continues upward (lines 103–105).
call_class_method_direct_ancestor_module_lacks_method_continues_test() ->
    ChildName = 'BtCallTestMid',
    MidName = 'BtCallTestMidParent',
    %% Insert the child with MidName as superclass.  MidName's module is a
    %% nonexistent atom, so function_exported returns false → recurse.
    %% MidName has no superclass → lookup_superclass returns not_found → error.
    beamtalk_class_metadata:insert(
        ChildName, nonexistent_child_mod3_bt5099, [], MidName, undefined
    ),
    beamtalk_class_metadata:insert(MidName, nonexistent_mid_mod_bt5099, [], undefined, undefined),
    try
        ChildTag = beamtalk_class_registry:class_object_tag(ChildName),
        ClassSelf = #beamtalk_object{
            class = ChildTag, class_mod = nonexistent_child_mod3_bt5099, pid = self()
        },
        ?assertError(
            {supervisor_init_method_not_found, bt_nonexistent_fun_bt5099},
            beamtalk_supervisor_class_call:call_class_method_direct(
                ChildName, nonexistent_child_mod3_bt5099, bt_nonexistent_fun_bt5099, ClassSelf, []
            )
        )
    after
        beamtalk_class_metadata:delete(ChildName),
        beamtalk_class_metadata:delete(MidName)
    end.

%%====================================================================
%% Tests: call_inherited_class_method_direct/5 — depth > 30 guard (line 81)
%%====================================================================

%% When the inheritance walk exceeds depth 30, the depth-guard clause fires
%% (line 81) and raises `{supervisor_init_method_not_found, FunName}`.
%% A two-class cycle (A → B → A) triggers this after 31 recursive calls.
call_class_method_direct_deep_hierarchy_hits_depth_limit_test() ->
    CycleA = 'BtCallTestCycleA',
    CycleB = 'BtCallTestCycleB',
    beamtalk_class_metadata:insert(CycleA, nonexistent_cycle_mod_a_bt5099, [], CycleB, undefined),
    beamtalk_class_metadata:insert(CycleB, nonexistent_cycle_mod_b_bt5099, [], CycleA, undefined),
    try
        ATag = beamtalk_class_registry:class_object_tag(CycleA),
        ClassSelf = #beamtalk_object{
            class = ATag, class_mod = nonexistent_cycle_mod_a_bt5099, pid = self()
        },
        ?assertError(
            {supervisor_init_method_not_found, bt_nonexistent_fun_bt5099},
            beamtalk_supervisor_class_call:call_class_method_direct(
                CycleA, nonexistent_cycle_mod_a_bt5099, bt_nonexistent_fun_bt5099, ClassSelf, []
            )
        )
    after
        beamtalk_class_metadata:delete(CycleA),
        beamtalk_class_metadata:delete(CycleB)
    end.
