%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Fixture for scripts/ci/lint-class-var-catchers.escript --self-test (BT-3769).
%% Markers that must be rejected.
%%
%% expect: bad_disposition 18
%% expect: stale_marker 27
%% expect: missing_reason 35
%% expect: converted_without_restore 43
%% expect: stale_marker 53
-module(bad_markers).

%% `needs-conversion` was an audit finding, not a marker: convert instead.
%% The site is still governed by the (invalid) marker, so it is not also
%% reported as unmarked.
needs_conversion(Block) ->
    %% bt-catcher-audit: needs-conversion - swallows a block error
    try
        Block()
    catch
        _:_ -> error
    end.

%% A marker on a region that runs no dynamic call is stale.
stale(List) ->
    %% bt-catcher-audit: not-applicable-no-block - nothing here runs a block
    try
        lists:reverse(List)
    catch
        _:_ -> error
    end.

missing_reason(Block) ->
    %% bt-catcher-audit: not-applicable-reraises
    try
        Block()
    catch
        C:R:S -> erlang:raise(C, R, S)
    end.

converted_without_restore(Block) ->
    %% bt-catcher-audit: converted - claims a conversion that is not there
    try
        Block()
    catch
        _:_ -> error
    end.

%% A marker below the last site of its file governs nothing.
stranded_marker() ->
    ok.
    %% bt-catcher-audit: not-applicable-no-block - stranded after the last site
