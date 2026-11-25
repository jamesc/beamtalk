%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_var_home_removed_tests).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
Pins for ADR 0153 Phase 3b (BT-5160): the class process no longer stores
class variables or installs a class-variable home around a class method.

- The class-variable API of the class process is gone: `set_class_var/3`
  (`beamtalk_object_class`, `beamtalk_runtime_api`), the reflection-send
  helpers, and the `ClassVars` argument of `handle_class_method_call`.
- A class method run in the class process sees no home: neither the
  receiver's class-variable key nor a live home key is set.
- `ClassBuilder register` ignores a `classState` key: the class process's
  classVars snapshot row stays empty.
- `is_field_reflection_selector/1` names exactly the four instance-field
  reflection selectors that a class object refuses (ADR 0153 decision 7).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HELPER, beamtalk_class_dispatch_test_helper).

exported(Module, Fun, Arity) ->
    {module, Module} = code:ensure_loaded(Module),
    erlang:function_exported(Module, Fun, Arity).

removed_exports_test_() ->
    [
        {
            atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++ " is gone",
            ?_assertNot(exported(M, F, A))
        }
     || {M, F, A} <- [
            {beamtalk_runtime_api, set_class_var, 3},
            {beamtalk_object_class, set_class_var, 3},
            {beamtalk_object_class, class_var_reflection_op, 2},
            {beamtalk_object_class, is_class_var_reflection_send, 2},
            {beamtalk_object_class, is_class_var_reflection_selector, 1},
            {beamtalk_class_dispatch, handle_class_method_call, 6}
        ]
    ] ++
        [
            {"handle_class_method_call/5 replaces /6",
                ?_assert(exported(beamtalk_class_dispatch, handle_class_method_call, 5))}
        ].

field_reflection_selector_test_() ->
    [
        ?_assert(beamtalk_object_class:is_field_reflection_selector(S))
     || S <- ['hasField:', 'clearField:', 'fieldAt:', 'fieldAt:put:']
    ] ++
        [
            ?_assertNot(beamtalk_object_class:is_field_reflection_selector(S))
         || S <- [fieldAt, 'fieldAt:at:', 'fieldNames', 'instVarAt:', new, 'hasField']
        ].

with_runtime(Fun) ->
    {setup,
        fun() ->
            {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
            ok
        end,
        fun(_) -> ok end, Fun}.

class_method_runs_without_home_test_() ->
    with_runtime(fun() ->
        Name = 'BT5160NoHome',
        Pid = beamtalk_test_class:start(Name, #{
            superclass => 'Object',
            module => ?HELPER,
            class_methods => #{testObserveHome => #{arity => 0}},
            %% A stale `class_state` key in ClassInfo is ignored.
            class_state => #{count => 1}
        }),
        try
            %% The class-process path (`invoke_class_method/6`): no key for
            %% the receiver and no live home while the method runs.
            ?assertEqual(
                {ok, {undefined, undefined}},
                gen_server:call(Pid, {class_method_call, testObserveHome, []})
            ),
            %% The snapshot row is the empty map, not the ClassInfo value.
            ?assertEqual(
                {ok, #{}}, beamtalk_class_registry:class_state_snapshot_lookup(Pid)
            )
        after
            beamtalk_test_class:stop(Name)
        end
    end).

class_builder_ignores_class_state_test_() ->
    with_runtime(fun() ->
        Name = 'BT5160BuilderClassState',
        Builder = #{
            className => Name,
            superclassRef => 'Object',
            fieldSpecs => #{},
            methodSpecs => #{},
            classState => #{count => 7}
        },
        {ok, Pid} = beamtalk_class_builder:register(Builder),
        try
            ?assertEqual(
                {ok, #{}}, beamtalk_class_registry:class_state_snapshot_lookup(Pid)
            )
        after
            beamtalk_test_class:stop(Name)
        end
    end).
