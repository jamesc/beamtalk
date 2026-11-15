%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_kinds_tests).

-include_lib("eunit/include/eunit.hrl").

wire_name_is_atom_name_test() ->
    [
        ?assertEqual(atom_to_binary(K, utf8), beamtalk_workspace_kinds:wire_name(K))
     || K <- beamtalk_workspace_kinds:all()
    ].

decode_roundtrip_test() ->
    [
        ?assertEqual(
            {ok, K}, beamtalk_workspace_kinds:decode(beamtalk_workspace_kinds:wire_name(K))
        )
     || K <- beamtalk_workspace_kinds:persisted()
    ],
    ?assertEqual(error, beamtalk_workspace_kinds:decode(<<"unknown">>)),
    ?assertEqual(error, beamtalk_workspace_kinds:decode(<<"bogus">>)).

persisted_excludes_unknown_test() ->
    ?assertNot(lists:member(unknown, beamtalk_workspace_kinds:persisted())),
    ?assert(lists:member(unknown, beamtalk_workspace_kinds:all())).

tier_test() ->
    Tier2 = ['remove-class', 'rename-class', 'rename-method'],
    [
        ?assertEqual(
            case lists:member(K, Tier2) of
                true -> tier2;
                false -> tier1
            end,
            beamtalk_workspace_kinds:tier(K)
        )
     || K <- beamtalk_workspace_kinds:all()
    ].

rename_test() ->
    ?assertEqual(class, beamtalk_workspace_kinds:rename('rename-class')),
    ?assertEqual(method, beamtalk_workspace_kinds:rename('rename-method')),
    ?assertEqual(none, beamtalk_workspace_kinds:rename(instance)),
    ?assert(beamtalk_workspace_kinds:is_rename('rename-method')),
    ?assertNot(beamtalk_workspace_kinds:is_rename('remove-class')).

filter_symbol_test() ->
    ?assertEqual(<<"#instance">>, beamtalk_workspace_kinds:filter_symbol(instance)),
    ?assertEqual(<<"#'new-class'">>, beamtalk_workspace_kinds:filter_symbol('new-class')).
