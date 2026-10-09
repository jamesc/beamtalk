%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Fixture for scripts/ci/lint-class-var-catchers.escript --self-test (BT-3769).
%% Every catching region here runs a dynamic call with no marker and no
%% protect/1, so each must be reported as `unmarked` on its try/catch line.
%%
%% expect: unmarked 18
%% expect: unmarked 25
%% expect: unmarked 32
%% expect: unmarked 39
%% expect: unmarked 46
%% expect: unmarked 53
-module(unmarked).

%% The acceptance-criteria case: a bare Block() under a catch.
block_under_catch(Block) ->
    try
        Block()
    catch
        _:_ -> ok
    end.

apply_two(Fun, Args) ->
    try
        apply(Fun, Args)
    catch
        error:_ -> error
    end.

erlang_apply_three(Module, Args) ->
    try
        erlang:apply(Module, run, Args)
    catch
        _:_ -> error
    end.

dynamic_remote(Module) ->
    try
        Module:run()
    catch
        _:_ -> error
    end.

nested_fun(Block, List) ->
    try
        lists:foreach(fun(X) -> Block(X) end, List)
    catch
        _:_ -> error
    end.

old_style_catch(Block) ->
    catch Block().
