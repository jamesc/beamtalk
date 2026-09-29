%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_config_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
Unit tests for beamtalk_workspace_config.

Covers all 4 exported pure functions: singletons/0, value_singletons/0,
binding_names/0, and binding_name_for_class/1.
""".
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% singletons/0
%%% ============================================================================

singletons_returns_list_test() ->
    Result = beamtalk_workspace_config:singletons(),
    ?assert(is_list(Result)).

singletons_has_one_entry_test() ->
    Result = beamtalk_workspace_config:singletons(),
    ?assertEqual(1, length(Result)).

singletons_transcript_binding_name_test() ->
    [Singleton] = beamtalk_workspace_config:singletons(),
    ?assertEqual('Transcript', maps:get(binding_name, Singleton)).

singletons_transcript_class_name_test() ->
    [Singleton] = beamtalk_workspace_config:singletons(),
    ?assertEqual('TranscriptStream', maps:get(class_name, Singleton)).

singletons_transcript_module_test() ->
    [Singleton] = beamtalk_workspace_config:singletons(),
    ?assertEqual(beamtalk_transcript_stream, maps:get(module, Singleton)).

singletons_transcript_start_args_test() ->
    [Singleton] = beamtalk_workspace_config:singletons(),
    ?assertEqual([1000], maps:get(start_args, Singleton)).

%%% ============================================================================
%%% value_singletons/0
%%% ============================================================================

value_singletons_returns_list_test() ->
    Result = beamtalk_workspace_config:value_singletons(),
    ?assert(is_list(Result)).

value_singletons_has_one_entry_test() ->
    Result = beamtalk_workspace_config:value_singletons(),
    ?assertEqual(1, length(Result)).

value_singletons_workspace_binding_test() ->
    [Workspace] = beamtalk_workspace_config:value_singletons(),
    ?assertEqual('Workspace', maps:get(binding_name, Workspace)),
    ?assertEqual('WorkspaceInterface', maps:get(class_name, Workspace)).

%%% ============================================================================
%%% binding_names/0
%%% ============================================================================

binding_names_returns_two_test() ->
    Names = beamtalk_workspace_config:binding_names(),
    ?assertEqual(2, length(Names)).

binding_names_exact_order_test() ->
    Names = beamtalk_workspace_config:binding_names(),
    ?assertEqual(['Transcript', 'Workspace'], Names).

binding_names_contains_transcript_test() ->
    Names = beamtalk_workspace_config:binding_names(),
    ?assert(lists:member('Transcript', Names)).

binding_names_excludes_beamtalk_test() ->
    Names = beamtalk_workspace_config:binding_names(),
    ?assertNot(lists:member('Beamtalk', Names)).

binding_names_contains_workspace_test() ->
    Names = beamtalk_workspace_config:binding_names(),
    ?assert(lists:member('Workspace', Names)).

%%% ============================================================================
%%% binding_name_for_class/1
%%% ============================================================================

binding_name_for_class_transcript_stream_test() ->
    ?assertEqual(
        {ok, 'Transcript'},
        beamtalk_workspace_config:binding_name_for_class('TranscriptStream')
    ).

binding_name_for_class_workspace_interface_test() ->
    ?assertEqual(
        {ok, 'Workspace'},
        beamtalk_workspace_config:binding_name_for_class('WorkspaceInterface')
    ).

binding_name_for_class_unknown_returns_undefined_test() ->
    ?assertEqual(undefined, beamtalk_workspace_config:binding_name_for_class('Counter')).

binding_name_for_class_object_returns_undefined_test() ->
    ?assertEqual(undefined, beamtalk_workspace_config:binding_name_for_class('Object')).

binding_name_for_class_nonexistent_atom_test() ->
    ?assertEqual(undefined, beamtalk_workspace_config:binding_name_for_class('NoSuchClass')).
