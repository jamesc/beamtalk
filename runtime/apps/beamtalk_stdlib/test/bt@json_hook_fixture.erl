%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Minimal stub mimicking a compiled Beamtalk Value class that implements
%% the `asJson` JSON conversion hook (BT-2818).
%%
%% Named 'bt@json_hook_fixture' so beamtalk_primitive:class_name_to_module/1
%% resolves class 'JsonHookFixture' to this module via the bt@<snake_case>
%% naming convention. Instances are tagged maps carrying a `mode` field that
%% selects the asJson behaviour under test.
-module('bt@json_hook_fixture').

-export([new/1, has_method/1, dispatch/3]).

-spec new(atom()) -> map().
new(Mode) ->
    #{'$beamtalk_class' => 'JsonHookFixture', mode => Mode}.

-spec has_method(atom()) -> boolean().
has_method(asJson) -> true;
has_method(_) -> false.

-spec dispatch(atom(), list(), map()) -> term().
dispatch('respondsTo:', [Selector], _Self) ->
    %% Real compiled modules answer respondsTo: via generated reflection
    %% branches; the stub mirrors that against its has_method/1.
    has_method(Selector);
dispatch(asJson, [], #{mode := self_return} = Self) ->
    %% Pathological hook: returns the receiver unchanged. beamtalk_json must
    %% raise a type error instead of recursing forever.
    Self;
dispatch(asJson, [], #{mode := nested}) ->
    %% The hook result itself contains another asJson object.
    #{<<"inner">> => new(plain)};
dispatch(asJson, [], #{mode := mutual_a}) ->
    %% Not a literal self-return: a cycle discovered one level down, via a
    %% distinct partner object whose own asJson embeds this one back.
    #{<<"partner">> => new(mutual_b)};
dispatch(asJson, [], #{mode := mutual_b}) ->
    #{<<"partner">> => new(mutual_a)};
dispatch(asJson, [], #{mode := plain}) ->
    #{<<"kind">> => <<"fixture">>, <<"ok">> => true}.
