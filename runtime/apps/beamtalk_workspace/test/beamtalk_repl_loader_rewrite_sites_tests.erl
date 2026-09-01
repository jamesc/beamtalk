%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_rewrite_sites_tests).

-moduledoc """
Tests for the shared multi-site rewrite mechanism (ADR 0114, BT-3270):
`beamtalk_repl_loader:rewrite_sites/2`, its validate-only counterpart
`validate_sites/2` (BT-3278 review follow-up), and `emit_rewrite_change_entry/2`.

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

## `meck` (BT-3280)

The `partial_install_failure` and `stale_snapshot` cases below are the first
use of `meck` anywhere in this codebase's test suite. Both need to inject a
fault or a race at one specific, otherwise-untriggerable point inside
`rewrite_sites/2`'s install pass — `partial_install_failure` needs
`install_reload_result/2` (load + hot-reload) to fail on a binary that just
compiled successfully (which essentially never happens naturally — see that
function's own doc), and `stale_snapshot` needs a second writer's edit to
land in the exact gap between one class-group's validation and that same
group's own install turn, which a real concurrent process can never be
reliably scheduled into inside a deterministic test. `meck:new(?MODULE,
[passthrough])` against `beamtalk_repl_loader` itself, mocking only
`install_reload_result/2` (`beamtalk_repl_loader.erl`'s `install_rewrite_group/7`
calls it as `?MODULE:install_reload_result/2` specifically so this can
intercept it — a local call would bypass meck's module-replacement
entirely), was chosen deliberately over the alternatives: mocking a shared
system module like `code` or `file` would risk destabilizing every other
test in the same `rebar3 eunit` run (those modules are used everywhere,
including by EUnit itself), and a hand-rolled test double for
`beamtalk_repl_loader` would mean these tests no longer exercise the real
`compile_reload_source/4` → `install_reload_result/2` call chain this
module's other tests specifically exist to cover (see this moduledoc's own
opening paragraph). Scoping the mock to this one module's one function, with
`meck:passthrough/1` for every other call, keeps that real-chain coverage
for everything except the single point under test.

BT-2962 spike (OTP 29 native records): on this branch, the
`meck:new(beamtalk_repl_loader, ...)` calls below crash — `meck` rebuilds a
mock module's attributes from `Mod:module_info(attributes)`, which
list-wraps `-import_record`'s value even for a single occurrence, and
`erl_lint:import_native_record/3` has no clause for that shape. See the
BT-2962 Linear issue for the full writeup.
""".

-include_lib("eunit/include/eunit.hrl").

%% BT-2962 spike: `meck:new(beamtalk_repl_loader, ...)` crashes the
%% compiler on this branch (see moduledoc above). Throwing turns that
%% crash into a clean, deliberate "context setup failed" skip in the eunit
%% report instead of an unreadable compiler stack dump — eunit has no
%% primitive that both skips a test AND keeps the overall run's exit code
%% zero, so this doesn't turn `just test` green, just legible.
meck_broken_by_bt_2962() ->
    throw(
        {skip,
            "BT-2962: meck:new/2 crashes under OTP 29 native records "
            "(erl_lint:import_native_record/3 has no clause for the "
            "list-wrapped attribute module_info(attributes) produces) — "
            "see the BT-2962 Linear issue"}
    ).

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
    %% Cross-invocation-unique (BT-3281) — see `beamtalk_test_unique:id/0`.
    Unique = beamtalk_test_unique:id(),
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
%% Same-start tie: a zero-length insertion sharing a `start` with a
%% same-position replacement, in both caller-supplied orders (ADR 0114,
%% BT-3270 — review feedback on PR #3522). `validate_no_overlaps/3`
%% deterministically accepts this pair; `apply_site_splices/2` must
%% therefore apply them in a well-defined order (the larger-`end` span
%% first) regardless of which order the caller lists them in, or one of
%% the two orders would splice the zero-length site's ORIGINAL-source-
%% relative span against an already-shifted accumulator and corrupt the
%% result. Two separate test cases — one per caller-list order — each
%% asserting the correct merged text, not merely "no crash".
%%====================================================================

%% A zero-length insertion of "XX" immediately before the definition site's
%% "increment", sharing that site's `start`. The only byte-correct merge of
%% these two sites replaces the shared span's original text with
%% "XXincrementBy" and leaves everything else untouched — computed directly
%% from `CounterSource` (not re-derived via the mechanism under test) so this
%% is an independent expected value, not a tautology.
same_start_tie_sites_and_expected(CounterSource) ->
    [DefSpan | _] = word_spans(CounterSource, <<"increment">>),
    #{start := Start, 'end' := End} = DefSpan,
    ZeroLengthInsert = #{
        class => <<"Counter">>,
        source_file => undefined,
        span => #{start => Start, 'end' => Start},
        new_text => <<"XX">>
    },
    Replacement = #{
        class => <<"Counter">>,
        source_file => undefined,
        span => DefSpan,
        new_text => <<"incrementBy">>
    },
    <<Before:Start/binary, _Old:(End - Start)/binary, After/binary>> = CounterSource,
    Expected = <<Before/binary, "XXincrementBy", After/binary>>,
    {ZeroLengthInsert, Replacement, Expected}.

rewrite_sites_same_start_tie_insert_first_test_() ->
    {setup, fun setup/0, fun teardown/1, fun rewrite_sites_same_start_tie_insert_first/1}.

rewrite_sites_same_start_tie_insert_first(_Fixture) ->
    CounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    {ZeroLengthInsert, Replacement, Expected} = same_start_tie_sites_and_expected(CounterSource),
    Result = beamtalk_repl_loader:rewrite_sites(undefined, [ZeroLengthInsert, Replacement]),
    NewCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    [
        ?_assertMatch({ok, _}, Result),
        ?_assertEqual(Expected, NewCounterSource)
    ].

rewrite_sites_same_start_tie_replacement_first_test_() ->
    {setup, fun setup/0, fun teardown/1, fun rewrite_sites_same_start_tie_replacement_first/1}.

rewrite_sites_same_start_tie_replacement_first(_Fixture) ->
    CounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    {ZeroLengthInsert, Replacement, Expected} = same_start_tie_sites_and_expected(CounterSource),
    %% Same two sites, opposite caller-list order — must produce the
    %% identical, correct merge, not a different (and possibly corrupted)
    %% result.
    Result = beamtalk_repl_loader:rewrite_sites(undefined, [Replacement, ZeroLengthInsert]),
    NewCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    [
        ?_assertMatch({ok, _}, Result),
        ?_assertEqual(Expected, NewCounterSource)
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
    %%
    %% Cross-invocation-unique (BT-3281) — see `beamtalk_test_unique:id/0`.
    Unique = beamtalk_test_unique:id(),
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

%%====================================================================
%% validate_sites/2 (BT-3278 review follow-up): rewrite_sites/2's own
%% validate-only prefix, exposed standalone. Same fixture and same forced
%% failure as rewrite_sites_validation_failure_test_ above, but asserting
%% NOTHING is ever installed even on success — that's the whole point of a
%% validate-only call, not merely the validation-failure case's existing
%% "aborted before install" guarantee.
%%====================================================================

validate_sites_success_test_() ->
    {setup, fun setup/0, fun teardown/1, fun validate_sites_success/1}.

validate_sites_success(#{counter_path := CounterPath, sub_counter_path := SubCounterPath}) ->
    CounterSourceBefore = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceBefore = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [DefSpan, RefSpan1, RefSpan2] = word_spans(CounterSourceBefore, <<"increment">>),
    [SubRefSpan] = word_spans(SubCounterSourceBefore, <<"increment">>),

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

    Result = beamtalk_repl_loader:validate_sites(DefinitionSite, ReferenceSites),

    CounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [
        ?_assertEqual(ok, Result),
        %% Validation says this WOULD succeed, but nothing was installed:
        %% both classes' tracked source is byte-for-byte unchanged, and live
        %% dispatch still answers to the OLD selector.
        ?_assertEqual(CounterSourceBefore, CounterSourceAfter),
        ?_assertEqual(SubCounterSourceBefore, SubCounterSourceAfter),
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Counter new increment", beamtalk_repl_state:new(undefined, 0)
            )
        )
    ].

validate_sites_validation_failure_test_() ->
    {setup, fun setup/0, fun teardown/1, fun validate_sites_validation_failure/1}.

validate_sites_validation_failure(#{counter_path := CounterPath, sub_counter_path := SubCounterPath}) ->
    CounterSourceBefore = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceBefore = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [DefSpan, RefSpan1, RefSpan2] = word_spans(CounterSourceBefore, <<"increment">>),
    [SubRefSpan] = word_spans(SubCounterSourceBefore, <<"increment">>),

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

    Result = beamtalk_repl_loader:validate_sites(DefinitionSite, ReferenceSites),

    CounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [
        ?_assertMatch({error, {validation_failed, [{<<"SubCounter">>, _}]}}, Result),
        ?_assertEqual(CounterSourceBefore, CounterSourceAfter),
        ?_assertEqual(SubCounterSourceBefore, SubCounterSourceAfter),
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Counter new increment", beamtalk_repl_state:new(undefined, 0)
            )
        )
    ].

%%====================================================================
%% Shared site-builder for "rename increment -> incrementBy across all 4
%% sites" (BT-3280) — the exact rename `rewrite_sites_success_test_` above
%% also builds inline; extracted here rather than copied a further time
%% (CLAUDE.md's no-duplicate-implementations rule) since both new test cases
%% below need it.
%%====================================================================

rename_increment_sites(#{counter_path := CounterPath, sub_counter_path := SubCounterPath}) ->
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
    {DefinitionSite, ReferenceSites}.

teardown_with_meck(Fixture) ->
    meck:unload(beamtalk_repl_loader),
    teardown(Fixture).

%%====================================================================
%% partial_install_failure path (BT-3280): validation passes for EVERY
%% group, but the later install_reload_result/2 call for one specific group
%% fails — see this moduledoc's "meck" section for why meck is used here at
%% all, and why it is scoped to this one module and function.
%%====================================================================

rewrite_sites_partial_install_failure_test_() ->
    {setup, fun setup_with_install_fault/0, fun teardown_with_meck/1,
        fun rewrite_sites_partial_install_failure/1}.

setup_with_install_fault() ->
    Fixture = setup(),
    meck_broken_by_bt_2962(),
    meck:new(beamtalk_repl_loader, [passthrough]),
    %% Fail only SubCounter's own group install — Counter's group (processed
    %% first; see group_sites_by_class/1's first-seen-class-order doc) still
    %% installs for real via meck:passthrough/1, so this batch genuinely
    %% partially applies rather than failing on its very first group.
    meck:expect(beamtalk_repl_loader, install_reload_result, fun(Compiled, LoadPath) ->
        case filename:basename(LoadPath) of
            "sub_counter.bt" -> {error, {injected_fault, load_binary_anomaly}};
            _ -> meck:passthrough([Compiled, LoadPath])
        end
    end),
    Fixture.

rewrite_sites_partial_install_failure(Fixture) ->
    {DefinitionSite, ReferenceSites} = rename_increment_sites(Fixture),
    Result = beamtalk_repl_loader:rewrite_sites(DefinitionSite, ReferenceSites),
    NewCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [
        ?_assertMatch(
            {error, {partial_install_failure, <<"SubCounter">>, _, [<<"Counter">>]}}, Result
        ),
        %% Counter's group installed successfully BEFORE SubCounter's own
        %% install call failed — its tracked source (and live module) already
        %% reflect the rename. This is the documented, bounded partial-
        %% application case, not equivalent to a clean validation-phase abort.
        ?_assertEqual(3, length(word_spans(NewCounterSource, <<"incrementBy">>))),
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Counter new incrementBy", beamtalk_repl_state:new(undefined, 0)
            )
        ),
        %% SubCounter's own group never installed — its tracked source is
        %% byte-for-byte untouched (no "incrementBy" landed in it)...
        ?_assertEqual(0, length(word_spans(SubCounterSourceAfter, <<"incrementBy">>))),
        %% ...and its live `bump` method (still `super increment`) now fails,
        %% since Counter no longer understands the old selector — exactly the
        %% half-applied state `partial_install_failure`'s own doc describes,
        %% not a silently-swallowed error.
        ?_assertMatch(
            {error, _, _, _, _},
            beamtalk_repl_eval:do_eval("SubCounter new bump", beamtalk_repl_state:new(undefined, 0))
        )
    ].

%%====================================================================
%% Stale-snapshot detection (BT-3280): a concurrent writer's edit to a
%% class's tracked source lands in the window between `build_class_group/2`'s
%% own snapshot of that class (taken up front, before ANY group in this
%% batch validates) and that SAME class's own turn to install, later in this
%% same install loop. Reuses the `partial_install_failure` test's meck seam
%% purely as a hook point to inject the concurrent write at the right moment
%% (right after Counter's own group installs, immediately before
%% SubCounter's own install turn) — Counter's own install call still passes
%% through for real, nothing about it is faulted.
%%====================================================================

rewrite_sites_stale_snapshot_test_() ->
    {setup, fun setup_with_concurrent_write/0, fun teardown_with_meck/1,
        fun rewrite_sites_stale_snapshot/1}.

%% The "concurrent session's" own edit to SubCounter — deliberately NOT
%% derived from this test's own rename (a genuinely independent change, the
%% same way an unrelated session's edit would be).
concurrent_sub_counter_source() ->
    <<
        "Counter subclass: SubCounter\n"
        "  bump -> Integer => super increment\n"
        "  // edited by a concurrent session between this batch's own\n"
        "  // validation and SubCounter's own install turn (BT-3280)"
    >>.

setup_with_concurrent_write() ->
    Fixture = setup(),
    meck_broken_by_bt_2962(),
    meck:new(beamtalk_repl_loader, [passthrough]),
    meck:expect(beamtalk_repl_loader, install_reload_result, fun(Compiled, LoadPath) ->
        Result = meck:passthrough([Compiled, LoadPath]),
        case filename:basename(LoadPath) of
            "counter.bt" ->
                %% Simulate a second session's independent, successful edit to
                %% SubCounter's tracked source landing right after Counter's
                %% own group installs but before SubCounter's own group gets
                %% its turn — exactly the race rewrite_sites/2's doc (point 4)
                %% describes. Not a fault injection: this write itself
                %% succeeds; it is `install_rewrite_groups/3`'s own
                %% `class_source_unchanged/1` check against SubCounter's now-
                %% stale snapshot that must catch it.
                beamtalk_workspace_meta:set_class_source(
                    <<"SubCounter">>, binary_to_list(concurrent_sub_counter_source())
                );
            _ ->
                ok
        end,
        Result
    end),
    Fixture.

rewrite_sites_stale_snapshot(Fixture) ->
    {DefinitionSite, ReferenceSites} = rename_increment_sites(Fixture),
    Result = beamtalk_repl_loader:rewrite_sites(DefinitionSite, ReferenceSites),
    NewCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    SubCounterSourceAfter = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"SubCounter">>)
    ),
    [
        %% A distinct error from `partial_install_failure` — a caller (and
        %% its ChangeLog bookkeeping) must be able to tell "cleanly rejected,
        %% safely retryable" apart from "a documented bounded partial
        %% application happened".
        ?_assertMatch({error, {stale_snapshot, <<"SubCounter">>}}, Result),
        %% Counter's own group is unaffected by the race on SubCounter and
        %% still installed for real.
        ?_assertEqual(3, length(word_spans(NewCounterSource, <<"incrementBy">>))),
        %% SubCounter's tracked source is EXACTLY the concurrent session's own
        %% edit — this batch's own precomputed (now-stale) new_source for
        %% SubCounter was never written over it. This is the "no silent
        %% overwrite / no lost update" guarantee BT-3280 exists to add: a
        %% pre-fix implementation would have clobbered this with
        %% "...bump -> Integer => super incrementBy" instead, silently
        %% discarding the concurrent session's own edit.
        ?_assertEqual(concurrent_sub_counter_source(), SubCounterSourceAfter)
    ].

%%====================================================================
%% Stale-snapshot detection, class-removed variant (BT-3280): the OTHER way
%% a class's tracked source can stop matching its snapshot — not edited but
%% REMOVED entirely (e.g. a concurrent `removeFromSystem`), which
%% `beamtalk_workspace_meta:get_class_source/1` reports as `undefined`
%% rather than a changed binary. `class_source_unchanged/1` has a dedicated
%% clause for this (`undefined -> false`) that the edited-source variant
%% above never exercises, since that test's concurrent write always leaves
%% SOME source behind — this is the only test in this suite that reaches it.
%%====================================================================

rewrite_sites_stale_snapshot_removed_test_() ->
    {setup, fun setup_with_concurrent_removal/0, fun teardown_with_meck/1,
        fun rewrite_sites_stale_snapshot_removed/1}.

setup_with_concurrent_removal() ->
    Fixture = setup(),
    meck_broken_by_bt_2962(),
    meck:new(beamtalk_repl_loader, [passthrough]),
    meck:expect(beamtalk_repl_loader, install_reload_result, fun(Compiled, LoadPath) ->
        Result = meck:passthrough([Compiled, LoadPath]),
        case filename:basename(LoadPath) of
            "counter.bt" ->
                %% Simulate a concurrent `removeFromSystem` on SubCounter
                %% landing in the same gap the edited-source variant's own
                %% setup describes — this time removing its tracked source
                %% outright rather than replacing it.
                beamtalk_workspace_meta:remove_class_source(<<"SubCounter">>);
            _ ->
                ok
        end,
        Result
    end),
    Fixture.

rewrite_sites_stale_snapshot_removed(Fixture) ->
    {DefinitionSite, ReferenceSites} = rename_increment_sites(Fixture),
    Result = beamtalk_repl_loader:rewrite_sites(DefinitionSite, ReferenceSites),
    NewCounterSource = unicode:characters_to_binary(
        beamtalk_workspace_meta:get_class_source(<<"Counter">>)
    ),
    [
        %% Same distinct, cleanly-detected error as the edited-source variant
        %% — a removed class is exactly as unsafe to install over as a
        %% changed one, not a crash or a silent no-op.
        ?_assertMatch({error, {stale_snapshot, <<"SubCounter">>}}, Result),
        %% Counter's own group is unaffected and still installed for real.
        ?_assertEqual(3, length(word_spans(NewCounterSource, <<"incrementBy">>))),
        %% SubCounter has no tracked source at all — confirming the batch's
        %% own stale `new_source` was never written back into existence
        %% under it.
        ?_assertEqual(undefined, beamtalk_workspace_meta:get_class_source(<<"SubCounter">>))
    ].
