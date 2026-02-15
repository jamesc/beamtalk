%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Single source of truth for workspace singleton configuration.
%%%
%%% Centralises the mapping between binding names (Transcript, Beamtalk, Workspace),
%%% class names (TranscriptStream, SystemDictionary, WorkspaceEnvironment),
%%% and Erlang modules (beamtalk_transcript_stream, etc.).
%%%
%%% Used by:
%%% - beamtalk_workspace_sup — to build supervisor child specs
%%% - beamtalk_workspace_bootstrap — to wire class variables
%%% - beamtalk_repl_shell — to inject REPL convenience bindings
%%% - beamtalk_workspace.hrl — to filter workspace binding names
%%%
%%% **DDD Context:** Workspace

-module(beamtalk_workspace_config).

-export([singletons/0, binding_names/0]).

-type singleton_config() :: #{
    binding_name := atom(),
    class_name := atom(),
    module := module(),
    start_args := [term()]
}.

-export_type([singleton_config/0]).

%% @doc Return the workspace singleton definitions.
%%
%% Each entry defines a workspace singleton with:
%% - binding_name: the REPL convenience name (e.g. 'Transcript')
%% - class_name: the Beamtalk class name (e.g. 'TranscriptStream')
%% - module: the Erlang implementation module
%% - start_args: extra arguments after the registration tuple for start_link
-spec singletons() -> [singleton_config()].
singletons() ->
    [
        #{binding_name => 'Transcript',
          class_name   => 'TranscriptStream',
          module       => beamtalk_transcript_stream,
          start_args   => [1000]},
        #{binding_name => 'Beamtalk',
          class_name   => 'SystemDictionary',
          module       => beamtalk_system_dictionary,
          start_args   => [[]]},
        #{binding_name => 'Workspace',
          class_name   => 'WorkspaceEnvironment',
          module       => beamtalk_workspace_environment,
          start_args   => []}
    ].

%% @doc Return the list of workspace binding names.
%% Used to filter these from :bindings display.
-spec binding_names() -> [atom()].
binding_names() ->
    [maps:get(binding_name, S) || S <- singletons()].
