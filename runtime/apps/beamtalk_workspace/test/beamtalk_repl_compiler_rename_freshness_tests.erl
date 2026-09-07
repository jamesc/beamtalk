%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_compiler_rename_freshness_tests).

-moduledoc """
Regression test for BT-3443 (ADR 0119 Open Questions, on ADR 0114's live
`renameTo:`): pins the "the very next request already sees a live rename"
invariant that `beamtalk_repl_compiler:build_class_module_index/0` relies
on having, with no invalidation hook of its own.

`build_class_module_index/0` rebuilds its `class_name -> module_name` map
fresh from `beamtalk_runtime_api:all_classes/0` (the live class registry)
on every call — there is no cross-request cache for a rename to go stale
in. This test renames a live class via
`beamtalk_behaviour_intrinsics:classRenameTo/2` (ADR 0114, BT-3278) and
asserts the map `build_class_module_index/0` returns immediately afterward
already reflects the rename: the OLD name is gone, the NEW name is
present. See `crates/beamtalk-core/src/semantic_analysis/
class_module_registry.rs`'s own invariant doc for the Rust-side half of
this same "always fresh, never cached" contract — that side has no live
registry to poll at all, so its own regression test is a from-scratch
per-instance independence check instead of this rename-freshness check.

Uses a freestanding dynamic class (`beamtalk_class_builder:register/1`,
mirroring `beamtalk_behaviour_intrinsics_rename_to_tests.erl`'s own
`register_dynamic_class/1` helper) rather than a real `.bt` file: this test
only cares whether `build_class_module_index/0`'s snapshot is fresh, not
about `classRenameTo/2`'s reference-site-rewrite machinery (already
covered by that other module), so no `beamtalk_workspace_meta`/project
directory is needed here.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixture: one freestanding dynamic class, no source file, no project.
%%====================================================================

setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    {ClassObj, _Pid} = register_dynamic_class('Bt3443RenameFreshnessSource'),
    ClassObj.

teardown(_ClassObj) ->
    lists:foreach(
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    catch gen_server:stop(Pid, normal, 5000)
            end
        end,
        ['Bt3443RenameFreshnessSource', 'Bt3443RenameFreshnessTarget']
    ),
    ok.

register_dynamic_class(ClassName) ->
    State = #{
        className => ClassName,
        superclassRef => 'Object',
        fieldSpecs => #{},
        methodSpecs => #{}
    },
    {ok, Pid} = beamtalk_class_builder:register(State),
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    Module = beamtalk_object_class:module_name(Pid),
    ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = Pid},
    {ClassObj, Pid}.

%%====================================================================
%% The invariant itself.
%%====================================================================

rename_is_visible_in_class_module_index_on_next_call_test_() ->
    {setup, fun setup/0, fun teardown/1,
        fun rename_is_visible_in_class_module_index_on_next_call/1}.

rename_is_visible_in_class_module_index_on_next_call(ClassObj) ->
    _ = beamtalk_behaviour_intrinsics:classRenameTo(ClassObj, 'Bt3443RenameFreshnessTarget'),
    Index = beamtalk_repl_compiler:build_class_module_index(),
    [
        %% No stale cache to invalidate: this call rebuilt the map fresh
        %% from the live class registry, so the OLD name must already be
        %% gone from it.
        ?_assertNot(maps:is_key(<<"Bt3443RenameFreshnessSource">>, Index)),
        %% ...and the NEW name must already be present, on this very next
        %% call — no rebuild-on-mutation hook needed because nothing here
        %% caches an instance across requests in the first place.
        ?_assert(maps:is_key(<<"Bt3443RenameFreshnessTarget">>, Index))
    ].
