%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_lifecycle_tests).

-moduledoc """
EUnit tests for beamtalk_class_lifecycle:class_removed/2 (BT-3105).

Exercises each of the five derived-registry purges directly against
`class_removed/2` — xref, extensions (both instance- and class-side),
protocol registry, the compiler server's ambient class cache, and workspace
metadata's class_sources map. The end-to-end path through
`beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1` (extension
dispatch, redefinition-does-not-resurrect) is covered separately in
`beamtalk_behaviour_intrinsics_tests.erl`.
""".
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Setup / Teardown
%%% ============================================================================

%% Stand up (idempotently) every registry class_removed/2 touches, and clear
%% the ones scoped to this module's app so tests don't see each other's rows.
setup() ->
    case pg:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    beamtalk_extensions:init(),
    beamtalk_protocol_registry:init(),
    case whereis(beamtalk_xref) of
        undefined -> {ok, _} = beamtalk_xref:start_link();
        _ -> ok
    end,
    ok.

teardown(_) ->
    ok.

%% True when beamtalk_compiler_server is reachable — some standalone EUnit
%% runs of just this app may not have started the beamtalk_compiler
%% application. Compiler-cache assertions gate on this so they don't fail
%% spuriously outside the full `--app=beamtalk_runtime,beamtalk_workspace,
%% beamtalk_compiler` run.
compiler_server_available() ->
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% Stop any pre-existing beamtalk_workspace_meta process so each test that
%% needs one starts from a clean slate and stops its own — mirrors
%% beamtalk_activity_tracking_tests.erl / beamtalk_workspace_meta_tests.erl's
%% convention. Leaving a stray instance running would break other test
%% modules that assert on the "no workspace started" error path (e.g.
%% beamtalk_logging_config_tests.erl).
stop_workspace_meta_if_running() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%% Start a throwaway workspace_meta (repl => false, so it never touches
%% disk) for the duration of `Fun/1`, passing whether it actually started —
%% `false` on a standalone EUnit run where beamtalk_workspace is unreachable.
%% Always stops the process it started before returning, even on failure.
with_workspace_meta(Fun) ->
    stop_workspace_meta_if_running(),
    case
        beamtalk_workspace_meta:start_link(#{
            workspace_id => <<"bt3105-lifecycle-test">>,
            created_at => erlang:system_time(second),
            repl => false
        })
    of
        {ok, Pid} ->
            try
                Fun(true)
            after
                gen_server:stop(Pid)
            end;
        {error, _} ->
            Fun(false)
    end.

%%% ============================================================================
%%% class_removed/2 — per-registry purges
%%% ============================================================================

class_removed_purges_xref_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassName = 'BT3105LifecycleXref',
                Entry = beamtalk_xref:build_method_entry(
                    false, 'foo', <<"foo => 1">>, indexed, class_body
                ),
                ok = beamtalk_xref:register_class(ClassName, [Entry]),
                ?assertEqual(
                    [{ClassName, false}], beamtalk_xref:implementors_of('foo')
                ),

                ok = beamtalk_class_lifecycle:class_removed(ClassName, bt3105_lifecycle_mod),

                ?assertEqual([], beamtalk_xref:implementors_of('foo'))
            end)
        ]
    end}.

class_removed_purges_instance_and_class_side_extensions_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassName = 'BT3105LifecycleExt',
                ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
                Fun = fun(_Args, _Self) -> ok end,
                ok = beamtalk_extensions:register(ClassName, 'instSel', Fun, mylib),
                ok = beamtalk_extensions:register(ClassTag, 'classSel', Fun, mylib),
                ?assert(beamtalk_extensions:has(ClassName, 'instSel')),
                ?assert(beamtalk_extensions:has(ClassTag, 'classSel')),

                ok = beamtalk_class_lifecycle:class_removed(ClassName, bt3105_lifecycle_mod2),

                ?assertNot(beamtalk_extensions:has(ClassName, 'instSel')),
                ?assertNot(beamtalk_extensions:has(ClassTag, 'classSel'))
            end)
        ]
    end}.

class_removed_unregisters_protocol_by_module_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Module = bt3105_lifecycle_proto_mod,
                beamtalk_protocol_registry:register_protocol(#{
                    name => 'BT3105LifecycleProto',
                    module => Module,
                    required_methods => [],
                    type_params => [],
                    extending => undefined
                }),
                ?assert(beamtalk_protocol_registry:is_protocol('BT3105LifecycleProto')),

                ok = beamtalk_class_lifecycle:class_removed('BT3105LifecycleProto', Module),

                ?assertNot(beamtalk_protocol_registry:is_protocol('BT3105LifecycleProto'))
            end)
        ]
    end}.

class_removed_drops_compiler_cache_entry_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                case compiler_server_available() of
                    false ->
                        %% Compiler app unreachable in this standalone run — skip.
                        ok;
                    true ->
                        ClassName = 'BT3105LifecycleCompiler',
                        beamtalk_compiler_server:register_class(
                            ClassName, #{class => ClassName}
                        ),
                        ?assert(
                            maps:is_key(ClassName, beamtalk_compiler_server:get_classes())
                        ),

                        ok = beamtalk_class_lifecycle:class_removed(
                            ClassName, bt3105_lifecycle_mod3
                        ),
                        %% remove_class is a cast; get_classes/0 is a synchronous
                        %% call to the same gen_server mailbox, so it is
                        %% guaranteed to observe the preceding cast.
                        ?assertNot(
                            maps:is_key(ClassName, beamtalk_compiler_server:get_classes())
                        )
                end
            end)
        ]
    end}.

class_removed_drops_workspace_class_source_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(
                with_workspace_meta(fun
                    (false) ->
                        %% Workspace app unreachable in this standalone run — skip.
                        ok;
                    (true) ->
                        ClassName = 'BT3105LifecycleWorkspace',
                        ClassNameBin = atom_to_binary(ClassName, utf8),
                        ok = beamtalk_workspace_meta:set_class_source(
                            ClassNameBin, "Object subclass: BT3105LifecycleWorkspace []"
                        ),
                        ?assertNotEqual(
                            undefined, beamtalk_workspace_meta:get_class_source(ClassNameBin)
                        ),

                        ok = beamtalk_class_lifecycle:class_removed(
                            ClassName, bt3105_lifecycle_mod4
                        ),
                        timer:sleep(50),

                        ?assertEqual(
                            undefined, beamtalk_workspace_meta:get_class_source(ClassNameBin)
                        )
                end)
            )
        ]
    end}.

%% class_removed/2 purges every registry in one call — a class with rows in
%% all five, then a single call, then every registry reports it gone.
class_removed_purges_all_five_registries_together_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(
                with_workspace_meta(fun(WorkspaceOk) ->
                    ClassName = 'BT3105LifecycleAll',
                    Module = bt3105_lifecycle_all_mod,
                    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),

                    Entry = beamtalk_xref:build_method_entry(
                        false, 'bar', <<"bar => 2">>, indexed, class_body
                    ),
                    ok = beamtalk_xref:register_class(ClassName, [Entry]),

                    Fun = fun(_Args, _Self) -> ok end,
                    ok = beamtalk_extensions:register(ClassName, 'ext1', Fun, mylib),
                    ok = beamtalk_extensions:register(ClassTag, 'ext2', Fun, mylib),

                    beamtalk_protocol_registry:register_protocol(#{
                        name => ClassName,
                        module => Module,
                        required_methods => [],
                        type_params => [],
                        extending => undefined
                    }),

                    CompilerOk = compiler_server_available(),
                    case CompilerOk of
                        true ->
                            beamtalk_compiler_server:register_class(
                                ClassName, #{class => ClassName}
                            );
                        false ->
                            ok
                    end,

                    ClassNameBin = atom_to_binary(ClassName, utf8),
                    case WorkspaceOk of
                        true -> ok = beamtalk_workspace_meta:set_class_source(ClassNameBin, "src");
                        false -> ok
                    end,

                    ok = beamtalk_class_lifecycle:class_removed(ClassName, Module),

                    ?assertEqual([], beamtalk_xref:implementors_of('bar')),
                    ?assertNot(beamtalk_extensions:has(ClassName, 'ext1')),
                    ?assertNot(beamtalk_extensions:has(ClassTag, 'ext2')),
                    ?assertNot(beamtalk_protocol_registry:is_protocol(ClassName)),
                    case CompilerOk of
                        true ->
                            ?assertNot(
                                maps:is_key(ClassName, beamtalk_compiler_server:get_classes())
                            );
                        false ->
                            ok
                    end,
                    case WorkspaceOk of
                        true ->
                            timer:sleep(50),
                            ?assertEqual(
                                undefined, beamtalk_workspace_meta:get_class_source(ClassNameBin)
                            );
                        false ->
                            ok
                    end
                end)
            )
        ]
    end}.
