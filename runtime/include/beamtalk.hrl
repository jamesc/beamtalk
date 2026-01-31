%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk shared record definitions.
%%%
%%% This header file defines records used by the Beamtalk runtime modules.
%%% Runtime Erlang modules that need these records should include it with:
%%%   -include("beamtalk.hrl").
%%%
%%% Note: Generated Core Erlang modules do not emit this include directive.
%%% The record is currently used only by runtime support modules.

%% @doc Object reference record (planned).
%%
%% This record is the target representation for Beamtalk object references,
%% bundling class metadata with the actor pid:
%% - class: The class name atom (e.g., 'Counter')
%% - class_mod: The class module atom (e.g., 'beamtalk_counter_class')
%% - pid: The actor process pid
%%
%% This will enable reflection (obj#beamtalk_object.class) and proper
%% message routing through the class module.
%%
%% NOTE: Currently, generated code returns raw pids from spawn/0 and spawn/1.
%% Integration with this record is deferred work (requires BT-95 class runtime).
%%
%% Following LFE Flavors' #flavor-instance{} pattern.
-record(beamtalk_object, {
    class :: atom(),           % Class name (e.g., 'Counter')
    class_mod :: atom(),       % Class module (e.g., 'beamtalk_counter_class')
    pid :: pid()               % The actor process
}).
