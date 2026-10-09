%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Fixture for scripts/ci/lint-class-var-catchers.escript --self-test (BT-3769).
%% Every reported region here runs a dynamic call with no marker of its own and
%% no protect/1, so each must be reported as `unmarked` on its try/catch line.
%%
%% expect: unmarked 19
%% expect: unmarked 26
%% expect: unmarked 33
%% expect: unmarked 40
%% expect: unmarked 47
%% expect: unmarked 54
%% expect: unmarked 67
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

%% A marker governs only the first catching region below it, so a second
%% region added later in the same function is unmarked.
second_region_under_marker(Fun) ->
    %% bt-catcher-audit: not-applicable-no-block - Fun is an Erlang reflection fun
    A =
        try
            Fun()
        catch
            _:_ -> a
        end,
    B =
        try
            Fun()
        catch
            _:_ -> b
        end,
    {A, B}.
