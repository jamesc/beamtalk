%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Association runtime helpers.
%%%
%%% **DDD Context:** Runtime Context — Value Object
%%%
%%% Provides formatting for Association tagged maps.
%%% Association is represented as `#{$beamtalk_class => 'Association', key => Key, value => Value}`.
%%%
%%% @see beamtalk_primitive For dispatch routing.
-module(beamtalk_association).
-export([format_string/1]).

%% @doc Format an Association for display.
%%
%% Returns a binary like `<<"#name -> James">>`.
-spec format_string(map()) -> binary().
format_string(Assoc) when is_map(Assoc) ->
    Key = maps:get(key, Assoc),
    Value = maps:get(value, Assoc),
    KeyStr = beamtalk_primitive:print_string(Key),
    ValueStr = beamtalk_primitive:print_string(Value),
    iolist_to_binary([KeyStr, <<" -> ">>, ValueStr]).
