%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Formatting helpers for opaque BEAM types (Pid, Port, Reference).
%%
%%% **DDD Context:** Runtime
%%
%% These types are opaque Erlang values with no direct binary conversion
%% BIF. We use `erlang:pid_to_list/1` etc. and convert to binary with
%% a Beamtalk-style prefix (#Pid, #Port, #Ref).
-module(beamtalk_opaque_ops).

-export([pid_to_string/1, port_to_string/1, ref_to_string/1]).

%% @doc Format a pid as `#Pid<X.Y.Z>`.
-spec pid_to_string(pid()) -> binary().
pid_to_string(Pid) ->
    %% erlang:pid_to_list/1 returns "<X.Y.Z>"
    List = erlang:pid_to_list(Pid),
    %% Strip leading '<' and trailing '>', wrap with #Pid<...>
    Inner = lists:sublist(List, 2, length(List) - 2),
    iolist_to_binary([<<"#Pid<">>, Inner, <<">">>]).

%% @doc Format a port as `#Port<X.Y>`.
-spec port_to_string(port()) -> binary().
port_to_string(Port) ->
    %% erlang:port_to_list/1 returns "#Port<X.Y>"
    List = erlang:port_to_list(Port),
    list_to_binary(List).

%% @doc Format a reference as `#Ref<X.Y.Z.W>`.
-spec ref_to_string(reference()) -> binary().
ref_to_string(Ref) ->
    %% erlang:ref_to_list/1 returns "#Ref<X.Y.Z.W>"
    List = erlang:ref_to_list(Ref),
    list_to_binary(List).
