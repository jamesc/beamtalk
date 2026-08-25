%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_rewrite_sites_tests).

-moduledoc """
Tests for the shared multi-site rewrite mechanism (ADR 0114, BT-3270):
`beamtalk_repl_loader:rewrite_sites/2` and `emit_rewrite_change_entry/2`.

Integration tests against the real compiler port + a real
`beamtalk_workspace_meta`, mirroring `beamtalk_repl_loader_precheck_tests.erl`'s
fixture pattern — real `.bt` files on disk, loaded through the real
file-compile path (`beamtalk_repl_loader:handle_load/2`), so `rewrite_sites/2`
exercises its actual `compile_reload_source/4` / `install_reload_result/2`
call chain rather than a hand-rolled double.

## Fixture graph

Mirrors the shape ADR 0114's own Phase 2 acceptance criteria name verbatim:
"a class with 3 in-project references across 2 files".

`counter.bt`:
```
Value subclass: Counter
  increment -> Integer => 1
  incrementTwice -> Integer => self increment + self increment
```

`sub_counter.bt`:
```
Counter subclass: SubCounter
  bump -> Integer => super increment
```

The word `increment` (word-bounded, so `incrementTwice` never matches) occurs
four times total: the definition (`counter.bt`), two `self increment` sends
inside `incrementTwice` (`counter.bt` — same class AND same file as the
definition, exercising the "two sites merge into one recompile" invariant),
and one `super increment` send (`sub_counter.bt` — a different class in a
different file). That is one definition site plus three reference sites
across two files.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixture sources
%%====================================================================

counter_source() ->
    <<
        "Value subclass: Counter\n"
        "  increment -> Integer => 1\n"
        "  incrementTwice -> Integer => self increment + self increment"
    >>.

sub_counter_source() ->
    <<
        "Counter subclass: SubCounter\n"
        "  bump -> Integer => super increment"
    >>.

%%====================================================================
%% Integration fixture: real compiler port + workspace_meta, real files
%%====================================================================

setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    Unique = integer_to_list(erlang:unique_integer([positive])),
    ProjDir = filename:join(temp_dir(), "bt-rewrite-sites-" ++ Unique),
    ok = filelib:ensure_path(ProjDir),
    CounterPath = filename:join(ProjDir, "counter.bt"),
    SubCounterPath = filename:join(ProjDir, "sub_counter.bt"),
    ok = file:write_file(CounterPath, counter_source()),
    ok = file:write_file(SubCounterPath, sub_counter_source()),
    %% `repl => false` for the same test-isolation reason
    %% `beamtalk_repl_loader_precheck_tests.erl` uses it. `project_path` is
    %% set to the fixture directory so `classify_source_file/1` (used by
    %% `emit_rewrite_change_entry/2`'s flushability derivation) classifies
    %% our fixture files as in-project/flushable rather than "dependency".
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"rewrite_sites_test_ws">>,
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        repl => false
    }),
    beamtalk_compiler_server:clear_classes(),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _CounterClasses, _State1} = beamtalk_repl_loader:handle_load(CounterPath, State0),
    {ok, _SubCounterClasses, _State2} = beamtalk_repl_loader:handle_load(SubCounterPath, State0),
    #{proj_dir => ProjDir, counter_path => CounterPath, sub_counter_path => SubCounterPath}.

teardown(_) ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    ok.

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

%%====================================================================
%% Word-boundary span helper (test-only — mirrors what a real site-discovery
%% step, out of scope for BT-3270, would eventually compute).
%%====================================================================

%% Every byte-offset span of the exact word `Word` in `Source` — bounded by
%% non-word characters on both sides, so "increment" never matches inside
%% "incrementTwice".
word_spans(Source, Word) ->
    Len = byte_size(Word),
    [
        #{start => Start, 'end' => Start + Len}
     || {Start, _} <- binary:matches(Source, Word),
        not is_word_byte(Source, Start - 1),
        not is_word_byte(Source, Start + Len)
    ].

is_word_byte(_Source, Pos) when Pos < 0 -> false;
is_word_byte(Source, Pos) when Pos >= byte_size(Source) -> false;
is_word_byte(Source, Pos) ->
    <<_:Pos/binary, C, _/binary>> = Source,
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $_.

%%====================================================================
%% Atomic success: rewrite `increment` -> `incrementBy` across all 4 sites
%%====================================================================

rewrite_sites_success_test_() ->
    {setup, fun setup/0, fun teardown/1, fun rewrite_sites_success/1}.

rewrite_sites_success(#{counter_path := CounterPath, sub_counter_path := SubCounterPath}) ->
    CounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [DefSpan, RefSpan1, RefSpan2] = word_spans(CounterSource, <<"increment">>),
    [SubRefSpan] = word_spans(SubCounterSource, <<"increment">>),

    DefinitionSite = #{
        class => <<"Counter">>,
        source_file => list_to_binary(CounterPath),
        span => DefSpan,
        new_text => <<"incrementBy">>
    },
    ReferenceSites = [
        #{
            class => <<"Counter">>,
            source_file => list_to_binary(CounterPath),
            span => RefSpan1,
            new_text => <<"incrementBy">>
        },
        #{
            class => <<"Counter">>,
            source_file => list_to_binary(CounterPath),
            span => RefSpan2,
            new_text => <<"incrementBy">>
        },
        #{
            class => <<"SubCounter">>,
            source_file => list_to_binary(SubCounterPath),
            span => SubRefSpan,
            new_text => <<"incrementBy">>
        }
    ],

    Result = beamtalk_repl_loader:rewrite_sites(DefinitionSite, ReferenceSites),
    {ok, #{definition := Definition, sites := InstalledRefSites}} = Result,

    NewCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    NewSubCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [
        %% The rewrite result reports the definition site with its correct
        %% before/after text.
        ?_assertEqual(<<"Counter">>, maps:get(class, Definition)),
        ?_assertEqual(<<"increment">>, maps:get(prev_source, Definition)),
        ?_assertEqual(<<"incrementBy">>, maps:get(source, Definition)),
        %% All three reference sites are reported too, each correctly.
        ?_assertEqual(3, length(InstalledRefSites)),
        ?_assert(
            lists:all(
                fun(S) ->
                    maps:get(prev_source, S) =:= <<"increment">> andalso
                        maps:get(source, S) =:= <<"incrementBy">>
                end,
                InstalledRefSites
            )
        ),
        %% Every occurrence of the OLD selector is gone from both files' live
        %% tracked source, and the NEW selector appears everywhere it should
        %% (2 in Counter: definition + incrementTwice's two self-sends = 3
        %% total; 1 in SubCounter).
        ?_assertEqual([], word_spans(NewCounterSource, <<"increment">>)),
        ?_assertEqual(3, length(word_spans(NewCounterSource, <<"incrementBy">>))),
        ?_assertEqual([], word_spans(NewSubCounterSource, <<"increment">>)),
        ?_assertEqual(1, length(word_spans(NewSubCounterSource, <<"incrementBy">>))),
        %% The rewrite is genuinely live, not just tracked-source bookkeeping:
        %% the recompiled+hot-reloaded Counter module now answers to the new
        %% selector and no longer understands the old one.
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Counter new incrementBy", beamtalk_repl_state:new(undefined, 0)
            )
        ),
        ?_assertMatch(
            {error, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Counter new increment", beamtalk_repl_state:new(undefined, 0)
            )
        )
    ].

%%====================================================================
%% ChangeLog entry construction (`emit_rewrite_change_entry/2`) — produces a
%% `sites`-shaped entry using the `rename-method` schema (ADR 0114, BT-3269).
%%====================================================================

emit_rewrite_change_entry_test_() ->
    {setup, fun setup_with_changelog/0, fun teardown_with_changelog/1,
        fun emit_rewrite_change_entry_case/1}.

setup_with_changelog() ->
    Fixture = setup(),
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    %% A real (non-run-mode) ChangeLog needs a `workspace_id` AND a HOME it
    %% can resolve `<home>/.beamtalk/workspaces/<id>/changes/` under — mirrors
    %% `beamtalk_workspace_changelog_tests:fresh_workspace/0`'s isolation
    %% pattern (a unique id + a temp HOME) so `store_site_body/1` actually
    %% persists a ref file instead of degrading to `undefined` (run mode).
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("bt-rewrite-sites-changelog-" ++ Unique),
    ChangelogHome = filename:join(temp_dir(), "bt-rewrite-sites-changelog-home-" ++ Unique),
    ok = filelib:ensure_path(ChangelogHome),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", ChangelogHome),
    {ok, _} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    Fixture#{old_home => OldHome}.

teardown_with_changelog(#{old_home := OldHome} = Fixture) ->
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    case OldHome of
        false -> os:unsetenv("HOME");
        _ -> os:putenv("HOME", OldHome)
    end,
    teardown(Fixture).

emit_rewrite_change_entry_case(#{counter_path := CounterPath, sub_counter_path := SubCounterPath}) ->
    CounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [DefSpan, RefSpan1, RefSpan2] = word_spans(CounterSource, <<"increment">>),
    [SubRefSpan] = word_spans(SubCounterSource, <<"increment">>),
    DefinitionSite = #{
        class => <<"Counter">>,
        source_file => list_to_binary(CounterPath),
        span => DefSpan,
        new_text => <<"incrementBy">>
    },
    ReferenceSites = [
        #{
            class => <<"Counter">>,
            source_file => list_to_binary(CounterPath),
            span => RefSpan1,
            new_text => <<"incrementBy">>
        },
        #{
            class => <<"Counter">>,
            source_file => list_to_binary(CounterPath),
            span => RefSpan2,
            new_text => <<"incrementBy">>
        },
        #{
            class => <<"SubCounter">>,
            source_file => list_to_binary(SubCounterPath),
            span => SubRefSpan,
            new_text => <<"incrementBy">>
        }
    ],
    {ok, RewriteResult} = beamtalk_repl_loader:rewrite_sites(DefinitionSite, ReferenceSites),
    Spec = #{
        kind => 'rename-method',
        class => <<"Counter">>,
        selector => <<"incrementBy">>,
        old_selector => <<"increment">>,
        side => instance,
        intent => durable,
        author => <<"sess-1">>,
        author_kind => agent
    },
    ok = beamtalk_repl_loader:emit_rewrite_change_entry(Spec, RewriteResult),
    [Entry] = beamtalk_workspace_changelog:entries(),
    Sites = beamtalk_workspace_changelog:entry_sites(Entry),
    [DefinitionSiteValue | _] = Sites,
    [
        ?_assertEqual('rename-method', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(<<"Counter">>, beamtalk_workspace_changelog:entry_class(Entry)),
        ?_assertEqual(<<"incrementBy">>, beamtalk_workspace_changelog:entry_selector(Entry)),
        ?_assertEqual(<<"increment">>, beamtalk_workspace_changelog:entry_old_selector(Entry)),
        ?_assertEqual(instance, beamtalk_workspace_changelog:entry_side(Entry)),
        ?_assertEqual(true, beamtalk_workspace_changelog:entry_flushable(Entry)),
        %% sites[0] is the definition, sites[1..] are the 3 reference sites —
        %% ADR 0114's schema (`source_ref`/`prev_source_ref` are TOP-level
        %% `undefined` for a multi-site kind; each site carries its own).
        ?_assertEqual(4, length(Sites)),
        ?_assert(is_binary(maps:get(source_ref, DefinitionSiteValue))),
        ?_assert(is_binary(maps:get(prev_source_ref, DefinitionSiteValue))),
        ?_assertEqual(
            {ok, <<"incrementBy">>},
            beamtalk_workspace_changelog:read_source_file(
                maps:get(source_ref, DefinitionSiteValue)
            )
        ),
        ?_assertEqual(
            {ok, <<"increment">>},
            beamtalk_workspace_changelog:read_source_file(
                maps:get(prev_source_ref, DefinitionSiteValue)
            )
        )
    ].

%%====================================================================
%% Forced validation failure: the whole batch aborts, no partial mutation
%%====================================================================

rewrite_sites_validation_failure_test_() ->
    {setup, fun setup/0, fun teardown/1, fun rewrite_sites_validation_failure/1}.

rewrite_sites_validation_failure(#{counter_path := CounterPath, sub_counter_path := SubCounterPath}) ->
    CounterSourceBefore = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceBefore = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [DefSpan, RefSpan1, RefSpan2] = word_spans(CounterSourceBefore, <<"increment">>),
    [SubRefSpan] = word_spans(SubCounterSourceBefore, <<"increment">>),

    %% Counter's own definition + both same-class references are ordinary,
    %% valid rewrites — only SubCounter's single reference site is corrupted
    %% with unparseable replacement text, forcing that one class-group's
    %% compile to fail while Counter's own group would otherwise validate
    %% cleanly on its own.
    DefinitionSite = #{
        class => <<"Counter">>,
        source_file => list_to_binary(CounterPath),
        span => DefSpan,
        new_text => <<"incrementBy">>
    },
    ReferenceSites = [
        #{
            class => <<"Counter">>,
            source_file => list_to_binary(CounterPath),
            span => RefSpan1,
            new_text => <<"incrementBy">>
        },
        #{
            class => <<"Counter">>,
            source_file => list_to_binary(CounterPath),
            span => RefSpan2,
            new_text => <<"incrementBy">>
        },
        #{
            class => <<"SubCounter">>,
            source_file => list_to_binary(SubCounterPath),
            span => SubRefSpan,
            new_text => <<"!!! not a valid selector or expression (((">>
        }
    ],

    Result = beamtalk_repl_loader:rewrite_sites(DefinitionSite, ReferenceSites),

    CounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [
        ?_assertMatch({error, {validation_failed, [{<<"SubCounter">>, _}]}}, Result),
        %% Counter's group validated successfully on its own, but the whole
        %% batch still aborted before ANY install ran — its tracked source
        %% (and therefore its live compiled module) is untouched.
        ?_assertEqual(CounterSourceBefore, CounterSourceAfter),
        ?_assertEqual(SubCounterSourceBefore, SubCounterSourceAfter),
        ?_assertEqual(3, length(word_spans(CounterSourceAfter, <<"increment">>))),
        ?_assertEqual(0, length(word_spans(CounterSourceAfter, <<"incrementBy">>))),
        %% Live dispatch still answers to the OLD selector on both classes —
        %% no half-applied rewrite reached the running image either.
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Counter new increment", beamtalk_repl_state:new(undefined, 0)
            )
        ),
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval("SubCounter new bump", beamtalk_repl_state:new(undefined, 0))
        )
    ].
