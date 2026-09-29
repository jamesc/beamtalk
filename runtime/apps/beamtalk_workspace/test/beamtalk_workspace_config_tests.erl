%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_config_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
Unit tests for beamtalk_workspace_config.

Covers the exported pure functions: singletons/0 and value_singletons/0.
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

singletons_transcript_registered_name_test() ->
    [Singleton] = beamtalk_workspace_config:singletons(),
    ?assertEqual('Transcript', maps:get(registered_name, Singleton)),
    %% ADR 0129: the process keeps its name, but is not a REPL binding.
    ?assertNot(maps:is_key(binding_name, Singleton)).

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

%% ADR 0129: `Beamtalk` and `Workspace` are class-side facades, not singletons.
value_singletons_is_empty_test() ->
    ?assertEqual([], beamtalk_workspace_config:value_singletons()).
