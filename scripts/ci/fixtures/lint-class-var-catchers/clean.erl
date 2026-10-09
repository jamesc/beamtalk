%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Fixture for scripts/ci/lint-class-var-catchers.escript --self-test (BT-3769).
%% No expect lines: this module must lint clean. It covers every shape the
%% lint accepts without a complaint.
-module(clean).

%% Converted: the block runs under protect/1, so it is not a site at all.
protected(Block) ->
    try
        beamtalk_class_vars:protect(Block)
    catch
        _:_ -> error
    end.

%% A converted marker on a protect/1 site is checked, not stale.
protected_marked(Block) ->
    %% bt-catcher-audit: converted - the block runs under protect/1
    try beamtalk_class_vars:protect(Block) of
        V -> {ok, V}
    catch
        _:_ -> error
    end.

%% Converted by hand: snapshot here, restored in a helper of the same module.
snapshot_then_retry(Block) ->
    Snap = beamtalk_class_vars:snapshot(),
    %% bt-catcher-audit: converted - the snapshot is restored before the retry
    try
        Block()
    catch
        _:_ -> retry(Block, Snap)
    end.

retry(Block, Snap) ->
    beamtalk_class_vars:restore(Snap),
    Block().

%% A marker in the comment block above the function governs its sites,
%% and an em dash separator is accepted.
%% bt-catcher-audit: not-applicable-other-process — runs in a spawned process
marked_above(Block) ->
    spawn(fun() ->
        try
            Block()
        catch
            _:_ -> ok
        end
    end).

%% Each catching region carries its own marker, even when the reason repeats;
%% a nested region is marked inside its parent.
three_sites(Fun, Module) ->
    %% bt-catcher-audit: not-applicable-no-block - Fun is an Erlang reflection fun
    A =
        try
            Fun(),
            %% bt-catcher-audit: not-applicable-no-block - Fun is an Erlang reflection fun
            catch Fun()
        catch
            _:_ -> a
        end,
    %% bt-catcher-audit: not-applicable-no-block - Fun is an Erlang reflection fun
    B = (catch Fun()),
    %% bt-catcher-audit: not-applicable-reraises - every clause re-raises
    C =
        try
            Module:run()
        catch
            Class:Reason:Stack -> erlang:raise(Class, Reason, Stack)
        end,
    {A, B, C}.

%% Not sites: try/after with no catch, static applies, and calls made in the
%% `of` clauses rather than the protected body.
not_sites(Block, Args) ->
    try
        Block()
    after
        ok
    end,
    try
        erlang:apply(lists, reverse, Args)
    catch
        _:_ -> error
    end,
    try lists:reverse(Args) of
        _ -> Block()
    catch
        _:_ -> error
    end.
