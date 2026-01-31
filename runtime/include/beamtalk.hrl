%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk shared record definitions.
%%%
%%% This header file defines records used across the Beamtalk runtime.
%%% Include it in generated modules with: -include("beamtalk.hrl").

%% @doc Object reference record.
%%
%% Instead of passing raw pids, Beamtalk passes this record which bundles:
%% - class: The class name atom (e.g., 'Counter')
%% - class_mod: The class module atom (e.g., 'beamtalk_counter_class')
%% - pid: The actor process pid
%%
%% This enables reflection (obj#beamtalk_object.class) and proper
%% message routing through the class module.
%%
%% Following LFE Flavors' #flavor-instance{} pattern.
-record(beamtalk_object, {
    class :: atom(),           % Class name (e.g., 'Counter')
    class_mod :: atom(),       % Class module (e.g., 'beamtalk_counter_class')
    pid :: pid()               % The actor process
}).
