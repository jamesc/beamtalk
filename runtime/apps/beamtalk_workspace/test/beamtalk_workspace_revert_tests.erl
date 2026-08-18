%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_revert_tests).

-moduledoc """
End-to-end tests for the ChangeLog revert completeness set (ADR 0082):

  - BT-2663: reverting a newly-added *method* removes it from the live image.
  - BT-2664: reverting a newly-added *class* removes the class from the live image.
  - BT-2665: reverting a *class-side* method works symmetrically with instance
    side — a modify re-installs the prior class-side body; an add removes it.

Also covers the stdlib policy `beamtalk_repl_eval:remove_method/3,4` gates on
(ADR 0112 Phase 1, BT-3184): `remove_method/3` (the revert-of-an-add path
above) keeps refusing stdlib classes, while `remove_method/4` with
`allow_stdlib` reaches them — the mechanism `removeSelector:` (ADR 0112 Phase
2, BT-3186) drives.

BT-3206 (ADR 0113 Phase 1): also covers `"remove-class"` ChangeLog entries
appended by `beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1` —
an ordinary in-project class's removal (flushable, sourceFile + prev_source
recovered) and a ClassBuilder (dynamic) class's removal
(`not_flushable_reason: "dynamic"`). Needs this module's full-stack scaffolding
for the same reason the `removeSelector:` coverage above does: the append
routes through `beamtalk_repl_eval`/`beamtalk_workspace_changelog`, both of
which are no-ops without a running workspace.

BT-3186: also covers `beamtalk_behaviour_intrinsics:classRemoveSelector/2` /
`classRemoveSelectorIfAbsent/3` themselves for every scenario that needs this
module's live-workspace scaffolding — the local-method-table removal branch
routes through `beamtalk_repl_eval:remove_method/4` exactly like the revert
tests above, so it needs the identical `beamtalk_workspace_meta` +
real-on-disk-source setup and cannot run under plain BUnit (no workspace app
running there — the same reason `compile:source:`/`tryCompile:source:` have
no BUnit coverage either, only `tests/repl-protocol/cases/*.btscript`).
Functional re-exposure (an actual dispatch after removal), extension
removal, `removeSelector:ifAbsent:`'s block execution context, and the full
Beamtalk-syntax surface are covered by
`tests/repl-protocol/cases/remove_selector.btscript` instead; the tests here
are the structural/error-path checks that are more naturally expressed
against the Erlang primitives directly (mirroring `remove_method_*` above),
plus the stdlib-class scenario that needs `define_stdlib_class/3`'s
`stdlib/src/`-pathed scaffolding — the same reason BT-3184's stdlib coverage
lives here rather than in a `.bt`/`.btscript` file.

BT-3208 (ADR 0113 Phase 3): also covers `revert:` itself for `"remove-class"`
entries — recompile+reinstall the whole class from `prev_source_ref`, reusing
the `newClass:at:` install path (`find_revert_target/3`'s `reinstall_class`
outcome, `beamtalk_workspace_interface_primitives:reinstall_reverted_class/3`,
`beamtalk_repl_loader:revert_remove_class/2`) — and the post-flush "nothing to
revert" degrade for both `"remove-method"` and `"remove-class"` entries (an
entry drops out of the active ChangeLog view once flushed, matching ADR
0082/0113's documented "best-effort, pre-flush semantics only" contract).

These boot the full stack (compiler port + runtime + workspace_meta + changelog)
against an isolated, in-project temp tree so the live install / remove and the
flushable-with-prev-source recording paths run for real, then drive revert
through the public `revert_method/2` / `changeLogRevert/1` surface.

Each test uses a UNIQUE workspace_id + temp project dir and cleans up (workspace
servers, loaded modules, temp tree) in an `after`/teardown block so a failed
assertion cannot leak state into another run (a fixed-id reuse pollutes the
persisted ChangeLog and produces misleading results).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

revert_e2e_test_() ->
    {setup, fun suite_setup/0, fun suite_teardown/1,
        {foreach, fun case_setup/0, fun case_teardown/1, [
            fun revert_added_instance_method_removes_it/1,
            fun revert_modified_instance_method_restores_prior_body/1,
            fun revert_added_class_side_method_removes_it/1,
            fun revert_modified_class_side_method_restores_prior_body/1,
            fun revert_new_class_removes_the_class/1,
            fun remove_method_default_still_refuses_stdlib/1,
            fun remove_method_allow_stdlib_removes_from_a_stdlib_class/1,
            fun remove_method_unknown_policy_fails_closed/1,
            fun remove_selector_instance_side_removes_local_method/1,
            fun remove_selector_class_side_removes_local_method/1,
            fun remove_selector_absent_raises_selector_not_found/1,
            fun remove_selector_if_absent_returns_block_value_on_absence/1,
            fun remove_selector_if_absent_returns_receiver_on_success/1,
            fun remove_selector_allow_stdlib_removes_from_a_stdlib_class/1,
            fun remove_selector_contested_extension_re_exposes_local_method/1,
            %% ADR 0112 Phase 3 (BT-3187): "remove-method" ChangeLog entries +
            %% the (class, selector, side) flush-shadow-key / revert-side-
            %% resolution required fix.
            fun remove_selector_instance_side_logs_remove_method_changelog_entry/1,
            fun remove_selector_class_side_logs_remove_method_changelog_entry/1,
            fun remove_selector_extension_logs_remove_method_changelog_entry/1,
            %% BT-3206 (ADR 0113 Phase 1): "remove-class" ChangeLog entries
            %% for `removeFromSystem`.
            fun remove_from_system_logs_remove_class_changelog_entry/1,
            fun remove_from_system_dynamic_class_logs_not_flushable_dynamic/1,
            fun remove_selector_and_instance_patch_do_not_shadow_across_sides/1,
            fun revert_remove_method_entry_restores_removed_instance_method/1,
            fun revert_remove_method_entry_restores_removed_class_side_method/1,
            fun revert_selects_correct_side_entry_when_both_sides_have_entries/1,
            fun revert_method_selects_correct_side_entry_when_both_sides_have_entries/1,
            fun revert_method_side_agnostic_fallback_still_picks_highest_seq/1,
            fun recover_prev_from_disk_resolves_remove_method_entry_via_entry_side/1,
            %% BT-3208 (ADR 0113 Phase 3): `revert:` extended to `"remove-class"`
            %% entries, and the post-flush "nothing to revert" degrade for both
            %% `"remove-method"` and `"remove-class"`.
            fun revert_remove_class_entry_reinstalls_the_class/1,
            fun revert_remove_class_entry_leaves_no_phantom_pending_entry/1,
            fun revert_remove_class_entry_dynamic_class_has_no_source_to_reinstall_from/1,
            fun revert_remove_class_entry_recovers_prev_source_from_disk_when_changelog_copy_missing/1,
            fun revert_remove_method_entry_after_flush_is_unsupported/1,
            fun revert_remove_class_entry_after_flush_is_unsupported/1
        ]}}.

%% Start the heavy, node-global apps once for the whole suite (the compiler port
%% + runtime). Stopping/restarting them per case is slow and flaky.
suite_setup() ->
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Let the runtime register bootstrap classes before compiling user code.
    timer:sleep(300),
    ok.

suite_teardown(_) ->
    _ = application:stop(beamtalk_compiler),
    ok.

%% Per-case: an isolated workspace (unique id + temp HOME/project) plus the
%% changelog and meta gen_servers, with `project_path` pointing at the temp tree
%% so files written there classify as in-project (flushable + revertable).
case_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("revert-e2e-" ++ Unique),
    Tmp = filename:join(temp_dir(), "bt-revert-e2e-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    stop_existing(beamtalk_workspace_changelog),
    stop_existing(beamtalk_workspace_meta),
    {ok, ClogPid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => list_to_binary(Tmp),
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }),
    #{
        clog_pid => ClogPid,
        meta_pid => MetaPid,
        workspace_id => WorkspaceId,
        tmp => Tmp,
        unique => Unique,
        old_home => OldHome,
        classes => []
    }.

case_teardown(#{clog_pid := ClogPid, meta_pid := MetaPid, tmp := Tmp, old_home := OldHome}) ->
    stop_proc(MetaPid),
    stop_proc(ClogPid),
    restore_home(OldHome),
    _ = file:del_dir_r(Tmp),
    ok.

%%====================================================================
%% BT-2663 — added instance method reverts as a removal
%%====================================================================

revert_added_instance_method_removes_it(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevAddInst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n"]
    ),
    %% Add a brand-new instance method live.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"added">>, <<"^ 42">>, durable, <<"sess">>, human
    ),
    AfterAdd = beamtalk_runtime_api:local_instance_methods(Pid),
    %% Revert the add → the method must be gone.
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"added">>),
    AfterRevert = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assert(lists:member(added, AfterAdd)),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assertNot(lists:member(added, AfterRevert)),
        %% The pre-existing method is untouched.
        ?_assert(lists:member(base, AfterRevert))
    ].

%%====================================================================
%% BT-2290 regression — modified instance method reverts to prior body
%%====================================================================

revert_modified_instance_method_restores_prior_body(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevModInst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  value => 1\n"]
    ),
    %% Modify the existing instance method.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"value">>, <<"^ 2">>, durable, <<"sess">>, human
    ),
    %% Revert the modify → the method still exists (the prior body is re-installed,
    %% not removed).
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"value">>),
    AfterRevert = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(value, AfterRevert))
    ].

%%====================================================================
%% BT-2665 — class-side add reverts as a removal
%%====================================================================

revert_added_class_side_method_removes_it(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevAddCls" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n"]
    ),
    %% Add a brand-new class-side method live (side = class).
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"make">>, <<"^ self new">>, durable, <<"sess">>, human, class
    ),
    AfterAdd = beamtalk_runtime_api:local_class_methods(Pid),
    %% Revert the class-side add → the class-side method must be gone.
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"make">>),
    AfterRevert = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assert(lists:member(make, AfterAdd)),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assertNot(lists:member(make, AfterRevert))
    ].

%%====================================================================
%% BT-2665 — class-side modify reverts to the prior class-side body
%%====================================================================

revert_modified_class_side_method_restores_prior_body(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevModCls" ++ U),
    %% Define the class WITH a class-side method on disk so the modify has a
    %% recoverable prior class-side body.
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  class build => 1\n"]
    ),
    %% Modify the existing class-side method.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"build">>, <<"^ 2">>, durable, <<"sess">>, human, class
    ),
    %% Revert the class-side modify → the class-side method still exists (prior
    %% class-side body re-installed, not removed).
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"build">>),
    AfterRevert = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(build, AfterRevert))
    ].

%%====================================================================
%% BT-2664 — new-class reverts as a class removal
%%====================================================================

revert_new_class_removes_the_class(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("RevNewCls" ++ U),
    TargetPath = filename:join(Tmp, binary_to_list(ClassName) ++ ".bt"),
    Source = iolist_to_binary([
        <<"Actor subclass: ">>, ClassName, <<"\n  v => 1\n">>
    ]),
    {ok, _Objs} = beamtalk_repl_eval:new_class(Source, list_to_binary(TargetPath)),
    LoadedBefore = beamtalk_runtime_api:whereis_class(binary_to_atom(ClassName, utf8)),
    %% Revert the new-class entry via the FFI surface (new-class rows carry
    %% selector = nil, mapped to the `new-class` placeholder).
    Entry = #{
        '$beamtalk_class' => 'ChangeEntry',
        className => binary_to_atom(ClassName, utf8),
        selector => nil,
        kind => 'new-class'
    },
    _ = beamtalk_workspace_interface_primitives:changeLogRevert(Entry),
    LoadedAfter = beamtalk_runtime_api:whereis_class(binary_to_atom(ClassName, utf8)),
    [
        ?_assert(is_pid(LoadedBefore)),
        ?_assertEqual(undefined, LoadedAfter)
    ].

%%====================================================================
%% BT-3184 — remove_method/3,4 stdlib policy (ADR 0112 Phase 1)
%%====================================================================

%% remove_method/3 (the arity the revert-of-an-add path above uses) must keep
%% refusing stdlib classes exactly as before this issue — a real, already-
%% loaded stdlib class (ErlangModule) is used directly since this path never
%% reaches the recompile step, so nothing about the live class is mutated.
remove_method_default_still_refuses_stdlib(_) ->
    ?assert(beamtalk_runtime_api:whereis_class('ErlangModule') =/= undefined),
    Result3 = beamtalk_repl_eval:remove_method(
        <<"ErlangModule">>, <<"doesNotUnderstand:args:">>, instance
    ),
    Result4Refuse = beamtalk_repl_eval:remove_method(
        <<"ErlangModule">>, <<"doesNotUnderstand:args:">>, instance, refuse_stdlib
    ),
    [
        ?_assertMatch({error, #beamtalk_error{kind = runtime_error}}, Result3),
        ?_assertMatch({error, #beamtalk_error{kind = runtime_error}}, Result4Refuse)
    ].

%% remove_method/4 with `allow_stdlib` must reach a stdlib class instead of
%% refusing it up front. Defines a throwaway class under a `stdlib/src/`
%% subdirectory of the isolated temp tree (real file, `reload_class_file`-
%% loaded — see `define_stdlib_class/3`) so `is_stdlib_path/1`'s path check
%% genuinely classifies it stdlib-mode, giving it a real `bt@stdlib@...`
%% module name — the exact condition `stdlib_class_module/1` checks for. This
%% never touches a real production stdlib class.
remove_method_allow_stdlib_removes_from_a_stdlib_class(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3184StdlibProbe" ++ U),
    {ok, Pid} = define_stdlib_class(
        Tmp, ClassName, ["  class keep => 1\n", "  class probe => 2\n"]
    ),
    AfterAdd = beamtalk_runtime_api:local_class_methods(Pid),
    Result = beamtalk_repl_eval:remove_method(ClassName, <<"probe">>, class, allow_stdlib),
    AfterRemove = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assert(lists:member(probe, AfterAdd)),
        ?_assertMatch({ok, _}, Result),
        ?_assertNot(lists:member(probe, AfterRemove)),
        %% The pre-existing method is untouched.
        ?_assert(lists:member(keep, AfterRemove))
    ].

%% An unrecognised `StdlibPolicy` atom must fail loudly (no matching case
%% clause) rather than silently falling through to the permissive
%% `allow_stdlib` path — guards against a future caller's typo reaching a real
%% stdlib class unintentionally.
remove_method_unknown_policy_fails_closed(_) ->
    ?assert(beamtalk_runtime_api:whereis_class('ErlangModule') =/= undefined),
    [
        ?_assertError(
            {case_clause, _},
            beamtalk_repl_eval:remove_method(
                <<"ErlangModule">>, <<"doesNotUnderstand:args:">>, instance, bogus_policy
            )
        )
    ].

%%====================================================================
%% BT-3186 — classRemoveSelector/2, classRemoveSelectorIfAbsent/3
%% (ADR 0112 Phase 2)
%%====================================================================

%% Instance-side: a project class's own local method disappears from its
%% local method table after removal. Functional re-exposure (an actual
%% dispatch to the inherited implementation) is covered by
%% remove_selector.btscript, which can spawn and message a real instance;
%% structurally this is the same "local table" assertion
%% revert_added_instance_method_removes_it/1 above already makes.
remove_selector_instance_side_removes_local_method(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186Inst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n", "  greet => 2\n"]
    ),
    Self = instance_self(ClassName, Pid),
    BeforeRemove = beamtalk_runtime_api:local_instance_methods(Pid),
    Result = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, greet),
    AfterRemove = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assert(lists:member(greet, BeforeRemove)),
        ?_assertEqual(Self, Result),
        ?_assertNot(lists:member(greet, AfterRemove)),
        ?_assert(lists:member(base, AfterRemove))
    ].

%% Class-side: `Counter class removeSelector: #foo` (Self tagged 'Metaclass')
%% touches class_methods, not instance_methods.
remove_selector_class_side_removes_local_method(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186Cls" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  class keep => 1\n", "  class build => 2\n"]
    ),
    Self = metaclass_self(Pid),
    BeforeRemove = beamtalk_runtime_api:local_class_methods(Pid),
    Result = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, build),
    AfterRemove = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assert(lists:member(build, BeforeRemove)),
        ?_assertEqual(Self, Result),
        ?_assertNot(lists:member(build, AfterRemove)),
        ?_assert(lists:member(keep, AfterRemove))
    ].

%% Bare `removeSelector:` on a selector that is neither a local method nor an
%% extension raises `selector_not_found` — not `does_not_understand` (ADR
%% 0112 § Error behaviour on absent selector).
remove_selector_absent_raises_selector_not_found(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186Absent" ++ U),
    {ok, Pid} = define_project_class(Tmp, ClassName, ["  base => 1\n"]),
    Self = instance_self(ClassName, Pid),
    [
        ?_assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = selector_not_found}},
            beamtalk_behaviour_intrinsics:classRemoveSelector(Self, bogus)
        )
    ].

%% `removeSelector:ifAbsent:` runs the block and returns its value instead of
%% raising when the selector is absent.
remove_selector_if_absent_returns_block_value_on_absence(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186IfAbsMiss" ++ U),
    {ok, Pid} = define_project_class(Tmp, ClassName, ["  base => 1\n"]),
    Self = instance_self(ClassName, Pid),
    Result = beamtalk_behaviour_intrinsics:classRemoveSelectorIfAbsent(
        Self, bogus, fun() -> not_found_marker end
    ),
    [?_assertEqual(not_found_marker, Result)].

%% `removeSelector:ifAbsent:` returns the receiver (not the block's value) on
%% success — the block never runs.
remove_selector_if_absent_returns_receiver_on_success(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186IfAbsHit" ++ U),
    {ok, Pid} = define_project_class(Tmp, ClassName, ["  gone => 1\n"]),
    Self = instance_self(ClassName, Pid),
    Result = beamtalk_behaviour_intrinsics:classRemoveSelectorIfAbsent(
        Self, gone, fun() -> error(block_must_not_run) end
    ),
    AfterRemove = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assertEqual(Self, Result),
        ?_assertNot(lists:member(gone, AfterRemove))
    ].

%% ADR 0112 "Flushability, not refusal": removeSelector: installs
%% unconditionally, including on stdlib classes — unlike removeFromSystem,
%% there is no receiver-side stdlib refusal. Mirrors
%% remove_method_allow_stdlib_removes_from_a_stdlib_class/1 above, one layer
%% up (through the classRemoveSelector primitive rather than remove_method/4
%% directly), against the same throwaway `stdlib/src/`-pathed class.
remove_selector_allow_stdlib_removes_from_a_stdlib_class(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186Stdlib" ++ U),
    {ok, Pid} = define_stdlib_class(
        Tmp, ClassName, ["  class keep => 1\n", "  class probe => 2\n"]
    ),
    Self = metaclass_self(Pid),
    BeforeRemove = beamtalk_runtime_api:local_class_methods(Pid),
    Result = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, probe),
    AfterRemove = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assert(lists:member(probe, BeforeRemove)),
        ?_assertEqual(Self, Result),
        ?_assertNot(lists:member(probe, AfterRemove)),
        ?_assert(lists:member(keep, AfterRemove))
    ].

%% ADR 0112 § Extension methods, case 3: a local method shadowed by a
%% same-named extension. Removal must drop only the dispatch-winning
%% extension (mirroring beamtalk_dispatch:lookup/5's extension-first order),
%% re-exposing the local method; a second removeSelector: call removes that
%% too. Constructed directly via beamtalk_extensions:register/4 rather than
%% `>>` syntax — targeting an already-locally-defined selector on a known
%% class with `>>` patches the local method in place instead of registering a
%% foreign extension, not the contested state this case needs.
remove_selector_contested_extension_re_exposes_local_method(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3186Contested" ++ U),
    {ok, Pid} = define_project_class(Tmp, ClassName, ["  greet => 1\n"]),
    ClassAtom = binary_to_atom(ClassName, utf8),
    ExtFun = fun(_Args, _Self) -> ext_greet end,
    ok = beamtalk_extensions:register(ClassAtom, greet, ExtFun, bt3186_test_owner),
    Self = instance_self(ClassName, Pid),
    HasExtBefore = beamtalk_extensions:has(ClassAtom, greet),
    LocalBefore = beamtalk_runtime_api:local_instance_methods(Pid),
    %% First removal: drops the extension only; the local `greet` is untouched.
    Result1 = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, greet),
    HasExtAfterFirst = beamtalk_extensions:has(ClassAtom, greet),
    LocalAfterFirst = beamtalk_runtime_api:local_instance_methods(Pid),
    %% Second removal: the now-dispatch-winning local method is removed too.
    Result2 = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, greet),
    LocalAfterSecond = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assert(HasExtBefore),
        ?_assert(lists:member(greet, LocalBefore)),
        ?_assertEqual(Self, Result1),
        ?_assertNot(HasExtAfterFirst),
        ?_assert(lists:member(greet, LocalAfterFirst)),
        ?_assertEqual(Self, Result2),
        ?_assertNot(lists:member(greet, LocalAfterSecond))
    ].

%%====================================================================
%% BT-3187 (ADR 0112 Phase 3) — "remove-method" ChangeLog entries; the
%% (class, selector, side) flush-shadow-key and revert-side-resolution
%% required fix to ADR 0082's shipped flush/revert logic.
%%====================================================================

%% A successful instance-side removeSelector: appends a "remove-method" entry
%% with side = instance, flushable = true (an ordinary in-project class), and
%% sourceFile pointing at the class's own .bt file.
remove_selector_instance_side_logs_remove_method_changelog_entry(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187LogInst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n", "  greet => 2\n"]
    ),
    Self = instance_self(ClassName, Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, greet),
    Entry = only_remove_method_entry(ClassName, <<"greet">>),
    [
        ?_assertEqual('remove-method', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(instance, beamtalk_workspace_changelog:entry_side(Entry)),
        ?_assert(beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            project_class_path(Tmp, ClassName),
            beamtalk_workspace_changelog:entry_source_file(Entry)
        )
    ].

%% Same as above for the class side: `Counter class removeSelector: #foo`
%% logs side = class.
remove_selector_class_side_logs_remove_method_changelog_entry(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187LogCls" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  class keep => 1\n", "  class build => 2\n"]
    ),
    Self = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, build),
    Entry = only_remove_method_entry(ClassName, <<"build">>),
    [
        ?_assertEqual('remove-method', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(class, beamtalk_workspace_changelog:entry_side(Entry)),
        ?_assert(beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            project_class_path(Tmp, ClassName),
            beamtalk_workspace_changelog:entry_source_file(Entry)
        )
    ].

%% Extension-side counterpart (ADR 0112 § Extension methods): removing a
%% genuine EXTENSION method — registered via `beamtalk_extensions:register/5`
%% with real source text, not the local class table — also logs a
%% `"remove-method"` entry, but is always `flushable: false,
%% not_flushable_reason: "extension"` (no reliable byte-span resolver for a
%% foreign `TargetClass >> selector` definition exists in this codebase — see
%% `emit_extension_remove_change_entry/7`'s doc). The extension's source text
%% is still captured as `prev_source` for the audit trail even though nothing
%% can act on it yet.
remove_selector_extension_logs_remove_method_changelog_entry(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187Ext" ++ U),
    {ok, Pid} = define_project_class(Tmp, ClassName, ["  base => 1\n"]),
    ClassAtom = binary_to_atom(ClassName, utf8),
    ExtFun = fun(_Args, _Self) -> ext_shout end,
    ExtSource = <<"shout => ^ 'loud'">>,
    ok = beamtalk_extensions:register(ClassAtom, shout, ExtFun, bt3187_test_owner, ExtSource),
    Self = instance_self(ClassName, Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, shout),
    Entry = only_remove_method_entry(ClassName, <<"shout">>),
    PrevBody = beamtalk_workspace_changelog:read_prev_source_body(Entry),
    [
        ?_assertEqual('remove-method', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(instance, beamtalk_workspace_changelog:entry_side(Entry)),
        ?_assertNot(beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            <<"extension">>, beamtalk_workspace_changelog:entry_not_flushable_reason(Entry)
        ),
        ?_assertEqual({ok, ExtSource}, PrevBody)
    ].

%%====================================================================
%% BT-3206 (ADR 0113 Phase 1) — "remove-class" ChangeLog entries for
%% `removeFromSystem`.
%%====================================================================

%% A successful `removeFromSystem` on an ordinary in-project class appends a
%% "remove-class" entry: no selector/side (class-level, not method-level),
%% flushable = true, sourceFile pointing at the class's own .bt file, and
%% prev_source_ref recovering the exact source that was on disk before the
%% removal (captured before class_removed/2's teardown purged it).
remove_from_system_logs_remove_class_changelog_entry(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3206RmClass" ++ U),
    {ok, _Pid} = define_project_class(Tmp, ClassName, ["  base => 1\n"]),
    {ok, ExpectedSource} = file:read_file(project_class_path(Tmp, ClassName)),
    ClassAtom = binary_to_atom(ClassName, utf8),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassAtom),
    Entry = only_remove_class_entry(ClassName),
    PrevBody = beamtalk_workspace_changelog:read_prev_source_body(Entry),
    [
        ?_assertEqual('remove-class', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(undefined, beamtalk_workspace_changelog:entry_selector(Entry)),
        ?_assertEqual(undefined, beamtalk_workspace_changelog:entry_side(Entry)),
        ?_assertEqual(durable, beamtalk_workspace_changelog:entry_intent(Entry)),
        ?_assert(beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            project_class_path(Tmp, ClassName),
            beamtalk_workspace_changelog:entry_source_file(Entry)
        ),
        ?_assertEqual({ok, ExpectedSource}, PrevBody),
        %% The class is really gone — not just logged.
        ?_assertEqual(undefined, beamtalk_class_registry:whereis_class(ClassAtom))
    ].

%% A dynamically-created class (no backing `.bt` file — same shape
%% `beamtalk_behaviour_intrinsics_tests:bt1982_class_remove_success_when_registry_absent_test_/0`
%% uses: a freshly-compiled, freshly-loaded module registered directly via
%% `beamtalk_object_class:start/2` rather than compiled from a `.bt` file, so
%% `code:delete/1` succeeds during teardown but no `beamtalk_source` module
%% attribute — and therefore no sourceFile — ever exists) logs `flushable:
%% false, not_flushable_reason: "dynamic"` — never "stdlib" (removeFromSystem's
%% stdlib refusal, unchanged by this issue, means that case never reaches the
%% append point) and never a byte-span `sourceFile` that does not exist.
remove_from_system_dynamic_class_logs_not_flushable_dynamic(_Ctx) ->
    {ClassName, ClassNameBin} = define_dynamic_class("Bt3206RmDynamic"),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassName),
    Entry = only_remove_class_entry(ClassNameBin),
    [
        ?_assertEqual('remove-class', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertNot(beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            <<"dynamic">>, beamtalk_workspace_changelog:entry_not_flushable_reason(Entry)
        ),
        ?_assertEqual(undefined, beamtalk_workspace_changelog:entry_source_file(Entry))
    ].

%% The required fix itself: an instance-side durable patch of `Counter >>
%% #foo` and a class-side `Counter class removeSelector: #foo` share
%% `(class, selector)` but target different method tables — the
%% `(class, selector, side)` shadow key must keep both as survivors (neither
%% shadows the other), unlike the pre-ADR-0112 `(class, selector)`-only key
%% which would have collapsed them to one.
remove_selector_and_instance_patch_do_not_shadow_across_sides(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187Shadow" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  foo => 1\n", "  class foo => 2\n"]
    ),
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"foo">>, <<"^ 99">>, durable, <<"sess">>, human, instance
    ),
    Self = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, foo),
    Targeted = [
        E
     || E <- beamtalk_workspace_changelog:entries(),
        beamtalk_workspace_changelog:entry_class(E) =:= ClassName,
        beamtalk_workspace_changelog:entry_selector(E) =:= <<"foo">>
    ],
    {Applied, Shadowed} = beamtalk_workspace_flush:shadow_duplicates(Targeted),
    [
        ?_assertEqual(2, length(Targeted)),
        ?_assertEqual(2, length(Applied)),
        ?_assertEqual([], Shadowed)
    ].

%% Reverting a "remove-method" ChangeEntry re-installs the removed body on the
%% SAME side it was removed from — exercising `revert_side/1`'s
%% `entry_side/1`-based resolution (which no longer only understands
%% `instance`/`class` kinds) against a real `'remove-method'` entry, not just
%% the legacy instance/class-kind patches the pre-existing revert tests above
%% already cover.
revert_remove_method_entry_restores_removed_instance_method(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187RevertInst" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n", "  greet => 2\n"]
    ),
    Self = instance_self(ClassName, Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, greet),
    AfterRemove = beamtalk_runtime_api:local_instance_methods(Pid),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"greet">>),
    AfterRevert = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        ?_assertNot(lists:member(greet, AfterRemove)),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(greet, AfterRevert))
    ].

%% Class-side counterpart: the removed method is restored as a class-side
%% method, not accidentally re-installed instance-side.
revert_remove_method_entry_restores_removed_class_side_method(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187RevertCls" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  class keep => 1\n", "  class build => 2\n"]
    ),
    Self = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, build),
    AfterRemove = beamtalk_runtime_api:local_class_methods(Pid),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"build">>),
    AfterRevert = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assertNot(lists:member(build, AfterRemove)),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(build, AfterRevert))
    ].

%% The required fix itself, on the revert side: an instance-side durable patch
%% of `Counter >> #foo` (P1) and a later class-side `Counter class
%% removeSelector: #foo` (R1, same selector name, higher seq) are
%% indistinguishable to `find_revert_target/2`'s `(class, selector)`-only key
%% — it would pick R1 (the higher-seq candidate) regardless of which entry the
%% caller actually meant to revert. `Workspace changes revert: p1Entry`
%% (`changeLogRevert/1`) must resolve the entry to revert by
%% `(class, selector, side)`, using the `side` field carried on the
%% ChangeEntry the caller passed in — reverting the instance-side patch must
%% NOT touch the (unrelated, still-live) class-side removal.
revert_selects_correct_side_entry_when_both_sides_have_entries(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187RevertPickSide" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  foo => 1\n", "  class foo => 2\n"]
    ),
    %% P1: instance-side durable patch to #foo.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"foo">>, <<"^ 99">>, durable, <<"sess">>, human, instance
    ),
    %% R1: class-side removal of #foo, logged after P1 (higher seq).
    ClassSelf = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(ClassSelf, foo),
    BeforeRevertClassMethods = beamtalk_runtime_api:local_class_methods(Pid),
    BeforeRevertInstanceMethods = beamtalk_runtime_api:local_instance_methods(Pid),
    %% Revert P1 via the ChangeEntry-based surface (`Workspace changes revert:
    %% p1Entry`), passing the instance side explicitly — exactly the map shape
    %% `entry_to_value/2` builds for a ChangeEntry row.
    ClassAtom = binary_to_atom(ClassName, utf8),
    P1Entry = #{
        '$beamtalk_class' => 'ChangeEntry',
        className => ClassAtom,
        selector => foo,
        side => instance
    },
    _ = beamtalk_workspace_interface_primitives:changeLogRevert(P1Entry),
    AfterRevertClassMethods = beamtalk_runtime_api:local_class_methods(Pid),
    AfterRevertInstanceMethods = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        %% Sanity on the fixture: class-side foo is gone, instance-side foo is
        %% still there (patched to 99) before the revert.
        ?_assertNot(lists:member(foo, BeforeRevertClassMethods)),
        ?_assert(lists:member(foo, BeforeRevertInstanceMethods)),
        %% The class-side removal (R1) must be untouched by reverting P1 — the
        %% bug this regression guards against re-installed R1's class-side foo
        %% instead, because side-blind selection picked the higher-seq entry.
        ?_assertNot(lists:member(foo, AfterRevertClassMethods)),
        %% The instance-side method is still present (P1's revert re-installs
        %% its prior body in place, it does not remove the method).
        ?_assert(lists:member(foo, AfterRevertInstanceMethods))
    ].

%% ADR 0112 (BT-3187) required fix: `revert_method/2` — the LiveView "Workspace
%% changes" ChangeLog viewer's *only* revert entry point
%% (`BtAttach.Workspace.revert/2,3` calls `revert_method/3`) — must resolve the
%% same way the ChangeEntry-based `changeLogRevert/1` surface above does when
%% given an explicit side, not stay side-blind. Same fixture as
%% `revert_selects_correct_side_entry_when_both_sides_have_entries/1` above
%% (an instance-side durable patch to `#foo`, then a higher-seq class-side
%% `removeSelector: #foo`), but driven through `revert_method/3` with the
%% *binary* side a `phx-value-side` attribute arrives as (not a ChangeEntry
%% map) — exactly the LiveView row → RPC path.
revert_method_selects_correct_side_entry_when_both_sides_have_entries(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187RevertMethodPickSide" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  foo => 1\n", "  class foo => 2\n"]
    ),
    %% P1: instance-side durable patch to #foo.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"foo">>, <<"^ 99">>, durable, <<"sess">>, human, instance
    ),
    %% R1: class-side removal of #foo, logged after P1 (higher seq).
    ClassSelf = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(ClassSelf, foo),
    BeforeRevertClassMethods = beamtalk_runtime_api:local_class_methods(Pid),
    BeforeRevertInstanceMethods = beamtalk_runtime_api:local_instance_methods(Pid),
    %% Revert P1 via `revert_method/3` with `<<"instance">>` — the (class,
    %% selector, side) triple the LiveView row's revert button now sends,
    %% instead of the old side-blind `revert_method/2`.
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(
        ClassName, <<"foo">>, <<"instance">>
    ),
    AfterRevertClassMethods = beamtalk_runtime_api:local_class_methods(Pid),
    AfterRevertInstanceMethods = beamtalk_runtime_api:local_instance_methods(Pid),
    [
        %% Sanity on the fixture: class-side foo is gone, instance-side foo is
        %% still there (patched to 99) before the revert.
        ?_assertNot(lists:member(foo, BeforeRevertClassMethods)),
        ?_assert(lists:member(foo, BeforeRevertInstanceMethods)),
        ?_assertMatch({ok, _}, RevertResult),
        %% The class-side removal (R1) must be untouched by reverting P1 — the
        %% wrong-side bug this regression guards against re-installed R1's
        %% class-side foo instead, because side-blind selection picked the
        %% higher-seq entry regardless of which row was clicked.
        ?_assertNot(lists:member(foo, AfterRevertClassMethods)),
        %% The instance-side method is still present (P1's revert re-installs
        %% its prior body in place, it does not remove the method).
        ?_assert(lists:member(foo, AfterRevertInstanceMethods))
    ].

%% `revert_method/2` (no side argument) stays side-agnostic on purpose — it is
%% `revert_method/3(Class, Selector, undefined)` — for callers with no side
%% information. Same fixture as the two tests above, but asserts the
%% *intentional* highest-seq fallback: with no side given, `revert_method/2`
%% picks R1 (the class-side removal, higher seq) over P1 (the instance-side
%% patch), re-installing the class-side `foo` R1 removed rather than restoring
%% P1's prior instance-side body. This documents the still-supported
%% side-agnostic contract so a future change doesn't silently alter it.
revert_method_side_agnostic_fallback_still_picks_highest_seq(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3187RevertMethodNoSide" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  foo => 1\n", "  class foo => 2\n"]
    ),
    %% P1: instance-side durable patch to #foo.
    {ok, _} = beamtalk_repl_eval:compile_method(
        ClassName, <<"foo">>, <<"^ 99">>, durable, <<"sess">>, human, instance
    ),
    %% R1: class-side removal of #foo, logged after P1 (higher seq).
    ClassSelf = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(ClassSelf, foo),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"foo">>),
    AfterRevertClassMethods = beamtalk_runtime_api:local_class_methods(Pid),
    [
        ?_assertMatch({ok, _}, RevertResult),
        %% Side-agnostic fallback picked R1 (highest seq) — the class-side
        %% removal is undone, re-installing class-side foo.
        ?_assert(lists:member(foo, AfterRevertClassMethods))
    ].

%% ADR 0112 (BT-3187) required fix: `recover_prev_from_disk/1` — the fallback
%% `find_revert_target/3` reaches when an entry's recorded `prev_source_ref`
%% body file cannot be read (a rotation/cleanup race; here simulated by
%% deleting it out from under a genuine `'remove-method'` entry) — must resolve
%% the on-disk method span using `entry_side(Entry)`, not the entry's raw
%% `kind`. `kind` for a `'remove-method'` entry is the atom `'remove-method'`,
%% which `beamtalk_compiler:resolve_method_span/4` rejects outright (its `Side`
%% guard only accepts `instance`/`class`), so pre-fix this always failed with
%% `bad_argument` — surfacing as `{error, no_prev_source}` — even though the
%% class-side method `build` is still sitting right there on disk (the removal
%% was never flushed).
recover_prev_from_disk_resolves_remove_method_entry_via_entry_side(#{
    tmp := Tmp, unique := U, workspace_id := WorkspaceId
}) ->
    ClassName = list_to_binary("Bt3187RecoverDisk" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  class keep => 1\n", "  class build => 2\n"]
    ),
    Self = metaclass_self(Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, build),
    Entry = only_remove_method_entry(ClassName, <<"build">>),
    %% Sanity on the fixture: the entry recorded a real prior body (the normal
    %% flow — `resolve_removal_span_entry/6` found `build` on disk and
    %% captured it), so this test exercises the "body file unreadable" fallback
    %% specifically, not the (already-covered) "never recorded" one.
    PrevRef = beamtalk_workspace_changelog:entry_prev_source_ref(Entry),
    true = is_binary(PrevRef),
    %% Simulate the rotation/cleanup race: the recorded prior-body file is gone,
    %% but the class's own on-disk source (still containing `build`, since the
    %% removal was never flushed) is untouched.
    SourcesDir = filename:join(beamtalk_workspace_changelog:changes_dir(WorkspaceId), "sources"),
    PrevBodyPath = filename:join(SourcesDir, binary_to_list(PrevRef)),
    ok = file:delete(PrevBodyPath),
    Result = beamtalk_workspace_changelog:find_revert_target(ClassName, <<"build">>),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"build">>),
    AfterRevert = beamtalk_runtime_api:local_class_methods(Pid),
    [
        %% Recovered from disk (the class-side kind resolved via entry_side/1),
        %% not the `{error, no_prev_source}` the raw-`kind` bug produced.
        ?_assertMatch({ok, _Body, _Entry}, Result),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(lists:member(build, AfterRevert))
    ].

%%====================================================================
%% BT-3208 (ADR 0113 Phase 3) — `revert:` extended to `"remove-class"`
%% entries, and both `"remove-method"`/`"remove-class"` revert degrading to
%% "nothing to revert" once flushed.
%%====================================================================

%% Reverting a pending "remove-class" ChangeEntry recompiles and reinstalls
%% the whole class from its recorded `prev_source_ref`, reusing the
%% `newClass:at:` install path (same `new-class` placeholder selector
%% `revert_new_class_removes_the_class/1` above reaches a `'new-class'` entry
%% with, since a `'remove-class'` entry is equally selector-less/sideless).
%% The class comes back with both its methods intact and is live (dispatch-
%% capable) again — not just present in the ChangeLog.
revert_remove_class_entry_reinstalls_the_class(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3208RevertClass" ++ U),
    {ok, _Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n", "  greet => 2\n"]
    ),
    ClassAtom = binary_to_atom(ClassName, utf8),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassAtom),
    LoadedAfterRemove = beamtalk_class_registry:whereis_class(ClassAtom),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(
        ClassName, <<"new-class">>
    ),
    LoadedAfterRevert = beamtalk_class_registry:whereis_class(ClassAtom),
    AfterRevertMethods =
        case LoadedAfterRevert of
            undefined -> [];
            Pid -> beamtalk_runtime_api:local_instance_methods(Pid)
        end,
    [
        %% Sanity on the fixture: the class is really gone before the revert.
        ?_assertEqual(undefined, LoadedAfterRemove),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(is_pid(LoadedAfterRevert)),
        ?_assert(lists:member(base, AfterRevertMethods)),
        ?_assert(lists:member(greet, AfterRevertMethods))
    ].

%% Review fix (ADR 0113, BT-3208): reverting a "remove-class" entry must not
%% leave the class permanently stuck as a phantom pending "new-class" — the
%% reverted file was never deleted, so its content already matches disk. Before
%% the fix, `new_class_install/7` unconditionally emitted a fresh 'new-class'
%% ChangeEntry on every revert, which `method_delta/1` hardcodes as
%% always-pending and `beamtalk_workspace_flush:prepare_new_class/3` treats any
%% existing file as an unresolvable `target_exists` conflict — so the class
%% could never again flush cleanly. Verifies both: no active entry remains for
%% the class, and an ordinary flush reports nothing pending/no conflicts.
revert_remove_class_entry_leaves_no_phantom_pending_entry(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3208RevertNoPhantom" ++ U),
    {ok, _Pid} = define_project_class(Tmp, ClassName, ["  base => 1\n"]),
    ClassAtom = binary_to_atom(ClassName, utf8),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassAtom),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(
        ClassName, <<"new-class">>
    ),
    ?assertMatch({ok, _}, RevertResult),
    ActiveForClass = [
        E
     || E <- beamtalk_workspace_changelog:active_entries(),
        beamtalk_workspace_changelog:entry_class(E) =:= ClassName
    ],
    {ok, FlushSummary} = beamtalk_workspace_flush:flush(),
    FlushSkippedForClass = [
        S
     || S <- maps:get(skipped, FlushSummary),
        maps:get(class, S, undefined) =:= ClassName
    ],
    FlushConflictsForClass = [
        C
     || C <- maps:get(conflicts, FlushSummary),
        maps:get(class, C, undefined) =:= ClassName
    ],
    [
        ?_assertEqual([], ActiveForClass),
        ?_assertEqual([], FlushSkippedForClass),
        ?_assertEqual([], FlushConflictsForClass)
    ].

%% A dynamically-created class's `"remove-class"` entry has no recorded
%% `sourceFile` (BT-3206: `flushable: false, not_flushable_reason: "dynamic"`)
%% — there is nothing on disk to recompile+reinstall from, so revert fails
%% loudly with a structured error rather than silently no-op'ing.
revert_remove_class_entry_dynamic_class_has_no_source_to_reinstall_from(_Ctx) ->
    {ClassName, ClassNameBin} = define_dynamic_class("Bt3208RmDynamic"),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassName),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(
        ClassNameBin, <<"new-class">>
    ),
    [
        ?_assertMatch({error, #beamtalk_error{}}, RevertResult)
    ].

%% Mirrors `recover_prev_from_disk_resolves_remove_method_entry_via_entry_side/1`
%% for a class-level target: when the recorded `prev_source_ref` body for a
%% "remove-class" entry is unreadable (ChangeLog rotation pruned `sources/`,
%% or a rare fs race), revert falls back to reading the class's own on-disk
%% `sourceFile` directly (`recover_class_prev_from_disk/1`) — no span to
%% resolve, unlike the method case, since a class removal excises the whole
%% file, not a byte range within it.
revert_remove_class_entry_recovers_prev_source_from_disk_when_changelog_copy_missing(#{
    tmp := Tmp, unique := U, workspace_id := WorkspaceId
}) ->
    ClassName = list_to_binary("Bt3208RecoverDisk" ++ U),
    {ok, _Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n", "  greet => 2\n"]
    ),
    ClassAtom = binary_to_atom(ClassName, utf8),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassAtom),
    Entry = only_remove_class_entry(ClassName),
    %% Sanity on the fixture: the entry recorded a real prior body (the normal
    %% flow), so this test exercises the "body file unreadable" fallback
    %% specifically, not the (already-covered) "never recorded" one.
    PrevRef = beamtalk_workspace_changelog:entry_prev_source_ref(Entry),
    true = is_binary(PrevRef),
    %% Simulate the rotation/cleanup race: the recorded prior-body file is
    %% gone, but the class's own on-disk source (untouched, since the removal
    %% was never flushed) is still there.
    SourcesDir = filename:join(beamtalk_workspace_changelog:changes_dir(WorkspaceId), "sources"),
    PrevBodyPath = filename:join(SourcesDir, binary_to_list(PrevRef)),
    ok = file:delete(PrevBodyPath),
    Result = beamtalk_workspace_changelog:find_revert_target(ClassName, <<"new-class">>),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(
        ClassName, <<"new-class">>
    ),
    LoadedAfterRevert = beamtalk_class_registry:whereis_class(ClassAtom),
    [
        %% Recovered from disk, not the `{error, no_prev_source}` a missing
        %% fallback would have produced.
        ?_assertMatch({reinstall_class, _Body, _Entry}, Result),
        ?_assertMatch({ok, _}, RevertResult),
        ?_assert(is_pid(LoadedAfterRevert))
    ].

%% Once a "remove-method" entry has been flushed (Tier 1 — an ordinary
%% `flush/0` already applies pending "remove-method" entries, ADR 0113), it
%% drops out of the active ChangeLog view (`is_active/1`) and `revert:`
%% reports "nothing to revert" instead of attempting to reconstruct or
%% reinstall anything — ADR 0082/0113's documented "best-effort, pre-flush
%% semantics only" contract.
revert_remove_method_entry_after_flush_is_unsupported(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3208FlushedMethod" ++ U),
    {ok, Pid} = define_project_class(
        Tmp, ClassName, ["  base => 1\n", "  greet => 2\n"]
    ),
    Self = instance_self(ClassName, Pid),
    _ = beamtalk_behaviour_intrinsics:classRemoveSelector(Self, greet),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(ClassName, <<"greet">>),
    [
        ?_assertMatch({error, #beamtalk_error{}}, RevertResult)
    ].

%% Same "nothing to revert" degrade for a `"remove-class"` entry once
%% `flushIncludingDestructive/0` (Tier 2) has actually deleted the file —
%% post-flush undo is out of scope by design (git, or a fresh corrective
%% operation from the archived `prev_source_ref`), not a revert responsibility.
revert_remove_class_entry_after_flush_is_unsupported(#{tmp := Tmp, unique := U}) ->
    ClassName = list_to_binary("Bt3208FlushedClass" ++ U),
    {ok, _Pid} = define_project_class(Tmp, ClassName, ["  base => 1\n"]),
    ClassAtom = binary_to_atom(ClassName, utf8),
    _ = beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassAtom),
    {ok, _Summary} = beamtalk_workspace_flush:flush_including_destructive(),
    RevertResult = beamtalk_workspace_interface_primitives:revert_method(
        ClassName, <<"new-class">>
    ),
    [
        ?_assertMatch({error, #beamtalk_error{}}, RevertResult)
    ].

%%====================================================================
%% Helpers
%%====================================================================

%% Define a class backed by a real in-project `.bt` file and load it into the
%% live image so its source is recorded (flushable + revertable) and its method
%% spans resolve against disk. `Methods` is a list of method-line iolists (each
%% already indented). Returns `{ok, ClassPid}`.
define_project_class(Tmp, ClassNameBin, Methods) ->
    Path = filename:join(Tmp, binary_to_list(ClassNameBin) ++ ".bt"),
    Source = iolist_to_binary([
        <<"Actor subclass: ">>, ClassNameBin, <<"\n">>, Methods
    ]),
    ok = file:write_file(Path, Source),
    {ok, _ClassNames} = beamtalk_repl_loader:reload_class_file(Path),
    %% Record the class source in workspace_meta so live patches resolve their
    %% span (the stateful REPL load path does this; the stateless reload does not).
    ok = beamtalk_workspace_meta:set_class_source(ClassNameBin, binary_to_list(Source)),
    ClassAtom = binary_to_atom(ClassNameBin, utf8),
    Pid = wait_for_class(ClassAtom, 50),
    {ok, Pid}.

%% BT-3206/BT-3208: a dynamically-created class with no backing `.bt` file — a
%% freshly-compiled, freshly-loaded module registered directly via
%% `beamtalk_object_class:start/2` rather than compiled from a `.bt` file, so
%% `code:delete/1` succeeds during teardown but no `beamtalk_source` module
%% attribute — and therefore no sourceFile — ever exists (same shape
%% `beamtalk_behaviour_intrinsics_tests:
%% bt1982_class_remove_success_when_registry_absent_test_/0` uses). Shared by
%% the BT-3206 "logs not_flushable: dynamic" test and the BT-3208 "nothing to
%% reinstall from" revert test below. Returns `{ClassName, ClassNameBin}`.
define_dynamic_class(NamePrefix) ->
    Unique = erlang:unique_integer([positive]),
    ModAtom = list_to_atom("bt_rm_dynamic_mod_" ++ integer_to_list(Unique)),
    Forms = [
        {attribute, 1, module, ModAtom},
        {attribute, 2, export, []}
    ],
    {ok, ModAtom, Bin} = compile:forms(Forms, [return_errors]),
    %% An empty filename (rather than a fake ".erl" path) is what makes
    %% `code:which/1` answer `[]` — the "loaded from binary with no file"
    %% signal `no_source_reason/1` reads as `"dynamic"` rather than "stdlib"
    %% (see that function's doc).
    {module, ModAtom} = code:load_binary(ModAtom, "", Bin),
    ClassName = list_to_atom(NamePrefix ++ integer_to_list(Unique)),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    ClassInfo = #{
        name => ClassName,
        module => ModAtom,
        superclass => none,
        instance_methods => #{},
        class_methods => #{}
    },
    {ok, _Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
    {ClassName, ClassNameBin}.

%% BT-3186: the instance-side #beamtalk_object{} for a class object built by
%% define_project_class/3 / define_stdlib_class/3 — the receiver shape
%% `classRemoveSelector/2` expects for `Counter removeSelector: #foo`. Mirrors
%% `beamtalk_class_registry:atom_to_class_object/1`'s construction.
instance_self(ClassNameBin, Pid) ->
    ClassAtom = binary_to_atom(ClassNameBin, utf8),
    Tag = beamtalk_class_registry:class_object_tag(ClassAtom),
    Module = beamtalk_object_class:module_name_safe(Pid),
    #beamtalk_object{class = Tag, class_mod = Module, pid = Pid}.

%% BT-3186: the metaclass-tagged #beamtalk_object{} for a class built by
%% define_project_class/3 / define_stdlib_class/3 — the receiver shape
%% `classRemoveSelector/2` expects for `Counter class removeSelector: #foo`.
%% Mirrors `beamtalk_behaviour_intrinsics:classClass/1`'s construction.
metaclass_self(Pid) ->
    #beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid}.

%% BT-3187: the absolute `.bt` path `define_project_class/3` wrote
%% `ClassNameBin` to, as the binary a ChangeEntry's `entry_source_file/1`
%% is expected to record.
project_class_path(Tmp, ClassNameBin) ->
    list_to_binary(filename:join(Tmp, binary_to_list(ClassNameBin) ++ ".bt")).

%% BT-3187: the sole `"remove-method"` ChangeLog entry logged for
%% `(ClassNameBin, SelectorBin)` in the current (fresh, per-test) workspace.
%% Pattern-matches to exactly one — a test scenario that produces more (or
%% zero) indicates a bug in the scenario itself, not a case to degrade
%% gracefully for.
only_remove_method_entry(ClassNameBin, SelectorBin) ->
    [Entry] = [
        E
     || E <- beamtalk_workspace_changelog:entries(),
        beamtalk_workspace_changelog:entry_kind(E) =:= 'remove-method',
        beamtalk_workspace_changelog:entry_class(E) =:= ClassNameBin,
        beamtalk_workspace_changelog:entry_selector(E) =:= SelectorBin
    ],
    Entry.

%% BT-3206: the sole "remove-class" ChangeLog entry logged for ClassNameBin in
%% the current (fresh, per-test) workspace.
only_remove_class_entry(ClassNameBin) ->
    [Entry] = [
        E
     || E <- beamtalk_workspace_changelog:entries(),
        beamtalk_workspace_changelog:entry_kind(E) =:= 'remove-class',
        beamtalk_workspace_changelog:entry_class(E) =:= ClassNameBin
    ],
    Entry.

%% Like define_project_class/3, but writes the file under a `stdlib/src/`
%% subdirectory of the temp tree instead of directly in it. `is_stdlib_path/1`
%% (`beamtalk_repl_loader`) matches on the `/stdlib/src/` path substring alone,
%% so this drives the real compiler through its stdlib-mode branch and yields
%% a genuine `bt@stdlib@...` module name (BT-3184) — without writing anything
%% into the real `stdlib/src/` tree or touching a real stdlib class.
define_stdlib_class(Tmp, ClassNameBin, Methods) ->
    Dir = filename:join([Tmp, "stdlib", "src"]),
    ok = filelib:ensure_path(Dir),
    Path = filename:join(Dir, binary_to_list(ClassNameBin) ++ ".bt"),
    Source = iolist_to_binary([
        <<"Object subclass: ">>, ClassNameBin, <<"\n">>, Methods
    ]),
    ok = file:write_file(Path, Source),
    {ok, _ClassNames} = beamtalk_repl_loader:reload_class_file(Path),
    ok = beamtalk_workspace_meta:set_class_source(ClassNameBin, binary_to_list(Source)),
    ClassAtom = binary_to_atom(ClassNameBin, utf8),
    Pid = wait_for_class(ClassAtom, 50),
    {ok, Pid}.

%% Poll for the class registration (reload is synchronous, but registration can
%% lag a few ms behind module load).
wait_for_class(_ClassAtom, 0) ->
    error(class_not_registered);
wait_for_class(ClassAtom, N) ->
    case beamtalk_runtime_api:whereis_class(ClassAtom) of
        Pid when is_pid(Pid) -> Pid;
        _ ->
            timer:sleep(20),
            wait_for_class(ClassAtom, N - 1)
    end.

stop_existing(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> stop_proc(Pid)
    end.

stop_proc(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ok
            end;
        false ->
            ok
    end;
stop_proc(_) ->
    ok.

restore_home(false) -> os:unsetenv("HOME");
restore_home(OldHome) -> os:putenv("HOME", OldHome).

temp_dir() ->
    unicode:characters_to_list(beamtalk_file:'tempDirectory'()).
