%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Fixture for scripts/ci/lint-class-var-catchers.escript --self-test (BT-3769).
%% A form epp_dodger cannot parse could hide a site, so the lint must report it
%% rather than skip it. The second function is deliberately malformed.
%%
%% expect: parse_error 14
%% expect: unmarked 18
-module(unparsed).

ok() -> ok.

broken(Block) ->
    try Block() catch _:_ -> ok end end end.

still_checked(Block) ->
    try
        Block()
    catch
        _:_ -> ok
    end.
