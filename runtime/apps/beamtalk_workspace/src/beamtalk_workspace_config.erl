%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Single source of truth for workspace singleton configuration.
%%%
%%% Centralises the mapping between binding names (Transcript, Beamtalk, Workspace),
%%% class names (TranscriptStream, BeamtalkInterface, WorkspaceInterface),
%%% and Erlang modules (beamtalk_transcript_stream, etc.).
%%%
%%% Singletons are split into two categories:
%%% - Actor singletons: `singletons/0` — started as gen_server children by the
%%%   workspace supervisor, registered under their binding_name.
%%% - Value singletons: `value_singletons/0` — `sealed Object subclass:` instances
%%%   (tagged maps, no process). Bootstrapped by `beamtalk_workspace_bootstrap`
%%%   via `Module:new()`.
%%%
%%% Used by:
%%% - beamtalk_workspace_sup — to build supervisor child specs (actor singletons only)
%%% - beamtalk_workspace_bootstrap — to wire class variables (both kinds)
%%% - beamtalk_repl_ops_eval / beamtalk_repl_ops_dev — to filter workspace binding names
%%%
%%% **DDD Context:** Workspace Context

-module(beamtalk_workspace_config).

-export([singletons/0, value_singletons/0, binding_names/0]).

-type singleton_config() :: #{
    binding_name := atom(),
    class_name := atom(),
    module := module(),
    start_args := [term()]
}.

-type value_singleton_config() :: #{
    binding_name := atom(),
    class_name := atom(),
    module := module()
}.

-export_type([singleton_config/0, value_singleton_config/0]).

%% @doc Return the actor workspace singleton definitions.
%%
%% Each entry defines a workspace singleton backed by a gen_server:
%% - binding_name: the REPL convenience name (e.g. 'Transcript')
%% - class_name: the Beamtalk class name (e.g. 'TranscriptStream')
%% - module: the Erlang implementation module
%% - start_args: extra arguments after the registration tuple for start_link
%%
%% Order matters: the supervisor starts children in list order, and
%% actor registry interleaving is managed by beamtalk_workspace_sup.
-spec singletons() -> [singleton_config()].
singletons() ->
    [
        #{
            binding_name => 'Transcript',
            class_name => 'TranscriptStream',
            module => beamtalk_transcript_stream,
            start_args => [1000]
        }
    ].

%% @doc Return value singleton definitions (sealed Object subclass:, no process).
%%
%% Each entry defines a singleton that is a value type (tagged map, no gen_server).
%% Bootstrapped by calling `Module:new()` and setting the class variable `current`.
%% - binding_name: the REPL convenience name (e.g. 'Beamtalk')
%% - class_name: the Beamtalk class name
%% - module: the compiled Erlang module (provides new/0)
-spec value_singletons() -> [value_singleton_config()].
value_singletons() ->
    [
        #{
            binding_name => 'Beamtalk',
            class_name => 'BeamtalkInterface',
            module => 'bt@stdlib@beamtalk_interface'
        },
        #{
            binding_name => 'Workspace',
            class_name => 'WorkspaceInterface',
            module => 'bt@stdlib@workspace_interface'
        }
    ].

%% @doc Return the list of all workspace binding names (actor + value singletons).
%% Used to filter these from :bindings display.
-spec binding_names() -> [atom()].
binding_names() ->
    ActorNames = [maps:get(binding_name, S) || S <- singletons()],
    ValueNames = [maps:get(binding_name, S) || S <- value_singletons()],
    ActorNames ++ ValueNames.
