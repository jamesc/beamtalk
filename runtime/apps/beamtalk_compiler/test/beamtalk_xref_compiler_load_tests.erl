%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_xref_compiler_load_tests).

%%% **DDD Context:** Compilation Context

-moduledoc """
`beamtalk_xref` indexes the sends of a live patch through the
`beamtalk_compiler` facade (BT-4938). In interactive code-loading mode that
module is not loaded until something calls it, and the guard
`erlang:function_exported/3` is false for an unloaded module, so a patch made
before the first compiler call used to index no sends. The facade must be
loaded on demand.
""".

-include_lib("eunit/include/eunit.hrl").

sends_are_indexed_when_the_compiler_facade_is_not_loaded_test_() ->
    {setup, fun() -> beamtalk_test_boot:compiler() end, fun(_) -> ok end, fun() ->
        %% Simulate interactive mode before the first compiler call.
        _ = code:purge(beamtalk_compiler),
        true = code:delete(beamtalk_compiler),
        _ = code:purge(beamtalk_compiler),
        ?assertNot(erlang:function_exported(beamtalk_compiler, find_all_sends_in_source, 1)),
        %% Selectors are mapped with `binary_to_existing_atom`, so the atom has
        %% to exist; naming it here does that.
        Selector = 'printNl',
        Entry = beamtalk_xref:build_method_entry(
            false, 'run', <<"x printNl">>, indexed, put_method
        ),
        ?assertMatch([#{selector := Selector} | _], maps:get(sends, Entry))
    end}.
