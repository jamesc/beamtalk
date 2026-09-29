%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_config).

%%% **DDD Context:** Workspace Context

-moduledoc """
Single source of truth for workspace singleton configuration.

Maps the supervised singleton process (`'Transcript'`, the REPL stream) to its
class (`TranscriptStream`) and Erlang module (`beamtalk_transcript_stream`).

Since ADR 0129 no workspace singleton is a REPL binding: `Transcript` is a
class-side facade (`beamtalk_transcript_facade`) that routes to the registered
process when it is the workspace's `TranscriptStream`. The process keeps its
registered name; nothing here is injected into REPL scope.

Singletons are split into two categories:
- Actor singletons: `singletons/0` — started as gen_server children by the
  workspace supervisor (REPL server modes only), registered under their
  `registered_name`.
- Value singletons: `value_singletons/0` — `sealed Object subclass:` instances
  (tagged maps, no process). Empty since ADR 0129; removed in Phase 4.

Used by:
- beamtalk_workspace_sup — to build supervisor child specs (actor singletons only)
- beamtalk_workspace_bootstrap — to wire value singleton class variables
""".

-export([singletons/0, value_singletons/0]).

-type singleton_config() :: #{
    registered_name := atom(),
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

-doc """
Return the actor workspace singleton definitions.

Each entry defines a workspace singleton backed by a gen_server:
- registered_name: the process's registered name (e.g. 'Transcript')
- class_name: the Beamtalk class name (e.g. 'TranscriptStream')
- module: the Erlang implementation module
- start_args: extra arguments after the registration tuple for start_link

Order matters: the supervisor starts children in list order, and
actor registry interleaving is managed by beamtalk_workspace_sup.
""".
-spec singletons() -> [singleton_config()].
singletons() ->
    [
        #{
            registered_name => 'Transcript',
            class_name => 'TranscriptStream',
            module => beamtalk_transcript_stream,
            start_args => [1000]
        }
    ].

-doc """
Return value singleton definitions (sealed Object subclass:, no process).

Each entry defines a singleton that is a value type (tagged map, no gen_server).
Bootstrapped by calling `Module:new()` and setting the class variable `current`.
- binding_name: the REPL convenience name
- class_name: the Beamtalk class name
- module: the compiled Erlang module (provides new/0)
""".
-spec value_singletons() -> [value_singleton_config()].
value_singletons() ->
    %% Empty since ADR 0129: `Beamtalk` and `Workspace` are class-side facades
    %% (no instance, no `current`, no injected binding).
    [].
