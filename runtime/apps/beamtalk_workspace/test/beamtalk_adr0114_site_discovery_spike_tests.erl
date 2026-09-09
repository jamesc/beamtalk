%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_adr0114_site_discovery_spike_tests).

%%% **DDD Context:** Runtime Context (spike / validation harness)

-moduledoc """
BT-3268 — ADR 0114 Phase 1 validation spike.

`renameTo:`'s planned site-discovery mechanism (not yet implemented — see ADR
0114 § Decision) is the union of `SystemNavigation default referencesTo:
aClass` (ADR 0087) and `beamtalk_class_registry:direct_subclasses/1`. This
module is the spike the ADR's Phase 1 row calls for: confirm that union is
exhaustive against real, compiled stdlib code before any rename primitive is
built on top of it, and measure the one known, accepted gap (Constraint 4 —
`beamtalk_xref:build_method_entry/5` hard-codes `references => []` for every
live-patched method).

Two concerns, two groups of tests:

1. **Corpus accuracy** (`corpus_accuracy_test_/0`) — boots the real, compiled
   stdlib (`beamtalk_test_boot:boot_real_stdlib/1`, the same helper
   `beamtalk_repl_ops_browse_tests` and `beamtalk_repl_docs_tests` use for a
   genuinely-compiled-not-hand-written corpus) and runs
   `beamtalk_class_registry:direct_subclasses/1` /
   `beamtalk_xref:references_to/1` against a representative sample of real
   `stdlib/src/*.bt` classes, diffing each result against a reference list
   computed by hand-reading the actual source (recorded inline in each test's
   comment, with the exact source lines cited so the audit is independently
   checkable — not just against a second regex).

2. **Live-patch gap reproduction** (`live_patch_gap_test_/0`) — patches a
   throwaway dynamic class via `beamtalk_object_class:put_method/4`, one of
   `beamtalk_xref:build_method_entry/5`'s three real call sites (the other
   two are `beamtalk_extensions:register/5` and `beamtalk_class_builder.erl`'s
   `methodSource:` install — see the corrected note below). Confirms
   `references => []` reproduces exactly as Constraint 4 describes and
   measures the blast radius on a method carrying two syntactically distinct
   reference forms (a type annotation and a constructor send) to the same
   class.

**Spike finding, correcting the ADR's own text:** Constraint 4 states this
gap is reachable via "every `>>`/`compile:source:` live patch". A live E2E
check (`tests/repl-protocol/cases/adr_0114_live_patch_gap.btscript`) found
this is NOT true of the current implementation — both `>>` and
`compile:source:` route through `beamtalk_repl_loader:install_method/9` /
`reload_method_definition/4`, which recompile the **whole class** via
`beamtalk_repl_compiler:compile_method_reload/3` and `code:load_binary/3`,
producing a complete compiler-computed `method_xref` (references included).
Neither surface calls `beamtalk_object_class:put_method/3,4` — a repo-wide
grep confirms zero non-test call sites of that function today. The gap IS
reachable, confirmed live, via `beamtalk_extensions:register/5` (ADR 0066
source-bearing extensions) — the `.btscript` fixture above exercises that
real surface instead. See the findings doc for the full account.

Findings are written up in `docs/development/adr-0114-site-discovery-spike-findings.md`.

No production code changes — this module is test-only, per the issue's scope.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Group 1 — corpus accuracy: referencesTo: / direct_subclasses/1 against
%% the real, compiled stdlib corpus.
%%====================================================================

%% Boots the real stdlib once for every test in this generator. Idempotent
%% and safe alongside other test modules doing the same in this EUnit VM
%% (`beamtalk_test_boot`'s own moduledoc) — no global process-group wipe here,
%% unlike `beamtalk_object_class_tests`'s heavier setup/teardown, since this
%% module must not disturb real stdlib class processes booted for its own or
%% sibling suites.
corpus_setup() ->
    beamtalk_test_boot:boot_real_stdlib('Duration').

corpus_accuracy_test_() ->
    {setup, fun corpus_setup/0, fun(_) -> ok end, [
        {
            "direct_subclasses/1 matches a hand audit of every `Announcement subclass:` "
            "declaration in stdlib/src/*.bt",
            fun direct_subclasses_announcement/0
        },
        {
            "direct_subclasses/1 matches a hand audit of every `Error subclass:` "
            "declaration in stdlib/src/*.bt",
            fun direct_subclasses_error/0
        },
        {
            "references_to/1 matches a hand audit of every real (non-doc-comment) "
            "`Duration` mention in stdlib/src/*.bt",
            fun references_to_duration/0
        }
    ]}.

%% Hand audit (`grep -n "Announcement subclass:" stdlib/src/*.bt`, 2026-08-25):
%% actor_spawned.bt:18, actor_stopped.bt:17, binding_changed.bt:26,
%% class_loaded.bt:17, class_removed.bt:17, flush_completed.bt:19,
%% object_state_changed.bt:26, supervision_child_added.bt:18,
%% supervision_child_crashed.bt:17 — nine direct subclasses, no more, no fewer.
%% Asserted as a subset, not exact equality: this module runs in ordinary
%% CI, and an unrelated future PR adding another `Announcement subclass:`
%% declaration must not fail a spike test whose validation purpose (this
%% hand audit, dated 2026-08-25) is already served — see the Claude review
%% bot's finding on PR #3519. The hand-audited set must still be present
%% (that's the actual accuracy check); growth beyond it is expected and
%% fine.
direct_subclasses_announcement() ->
    Expected = lists:sort([
        'ActorSpawned',
        'ActorStopped',
        'BindingChanged',
        'ClassLoaded',
        'ClassRemoved',
        'FlushCompleted',
        'ObjectStateChanged',
        'SupervisionChildAdded',
        'SupervisionChildCrashed'
    ]),
    Actual = lists:sort(beamtalk_class_registry:direct_subclasses('Announcement')),
    ?assert(ordsets:is_subset(ordsets:from_list(Expected), ordsets:from_list(Actual))).

%% Hand audit (`grep -n "Error subclass:" stdlib/src/*.bt`, 2026-08-25):
%% beamerror.bt:20, instantiation_error.bt:13, runtime_error.bt:14,
%% type_error.bt:12 — four direct subclasses. `ExitError`/`ThrowError`
%% subclass `BEAMError`, not `Error` directly, so they are correctly excluded
%% here (direct_subclasses/1 is not transitive).
%% Subset, not exact equality — see direct_subclasses_announcement/0's note.
direct_subclasses_error() ->
    Expected = lists:sort(['BEAMError', 'InstantiationError', 'RuntimeError', 'TypeError']),
    Actual = lists:sort(beamtalk_class_registry:direct_subclasses('Error')),
    ?assert(ordsets:is_subset(ordsets:from_list(Expected), ordsets:from_list(Actual))).

%% Hand audit (`grep -n '\bDuration\b' stdlib/src/*.bt`, 2026-08-25, real code
%% lines only — every other hit is inside a `///` doc comment and must NOT be
%% indexed, matching `find_references_to_in_source`'s own documented
%% doc-comment-body-vs-example-code distinction). This audit was corrected
%% once against a live run (see finding #3 in the write-up): a first pass
%% counted only *other* classes' mentions of `Duration` (8) and missed that
%% `referencesTo:` also — correctly — reports a class's own self-references
%% (`Duration`'s own arithmetic/comparison operators and constructors
%% mention `Duration` in their own type signatures), which is required for
%% `renameTo:` to be exhaustive: those signatures need rewriting too.
%%
%% Cross-class mentions:
%%   actor.bt:294    withTimeout: ms :: Timeout | Duration -> TimeoutProxy =>
%%   actor.bt:297      (ms isKindOf: Duration) ifTrue: [ ...            (2 rows: different lines)
%%   date_time.bt:255  addDuration: d :: Duration -> DateTime => self delegate
%%   date_time.bt:280  - other :: DateTime -> Duration =>
%%   parallel.bt:73   class sealed all: ... timeout: ms :: Integer | Duration -> List(Result) =>
%%   timer.bt:36/48/58  after:do: / every:do: / sleep: (each `Integer | Duration`)
%% Self-mentions in duration.bt itself:
%%   instance-side: + - < <= > >= * (lines 184/193/225/243/234/252/203 — each
%%     signature mentions `Duration` in its param and/or return type, but
%%     ends up as ONE row per method: when both mentions land on the same
%%     source line, e.g. `+ other :: Duration -> Duration =>`, they produce
%%     two identical `#{class => 'Duration', line => L}` entries at codegen
%%     time, which collapse to a single ETS `bag` row keyed on the full
%%     `{owner, class_side, method, line}` site tuple — a real, minor
%%     ETS-semantics footgun for anyone hand-predicting a raw occurrence
%%     count from source, NOT a dropped reference: the site (this line, this
%%     method) is still correctly present exactly once.)
%%   class-side: milliseconds: seconds: minutes: hours: days: fromString:
%%     (lines 40/49/57/65/73/87 — return-type-only mentions, one row each)
%%
%% 21 rows total across 5 owner classes.
%% Asserted as a subset, not exact equality — see
%% direct_subclasses_announcement/0's note: a future PR adding a method
%% elsewhere in the stdlib that mentions `Duration` in its signature must
%% not fail this spike test. The hand-audited 21-tuple set (this module's
%% actual accuracy evidence) must still be present in full.
references_to_duration() ->
    Sites = beamtalk_xref:references_to('Duration'),
    OwnerMethodTally = lists:sort([
        {maps:get(owner, S), maps:get(class_side, S), maps:get(method, S)}
     || S <- Sites
    ]),
    ExpectedTally = lists:sort([
        %% Cross-class mentions.
        {'Actor', false, 'withTimeout:'},
        {'Actor', false, 'withTimeout:'},
        {'DateTime', false, 'addDuration:'},
        {'DateTime', false, '-'},
        {'Parallel', true, 'all:timeout:'},
        {'Timer', true, 'after:do:'},
        {'Timer', true, 'every:do:'},
        {'Timer', true, 'sleep:'},
        %% Duration's own self-references (instance-side operators).
        {'Duration', false, '+'},
        {'Duration', false, '-'},
        {'Duration', false, '*'},
        {'Duration', false, '<'},
        {'Duration', false, '<='},
        {'Duration', false, '>'},
        {'Duration', false, '>='},
        %% Duration's own self-references (class-side constructors).
        {'Duration', true, 'milliseconds:'},
        {'Duration', true, 'seconds:'},
        {'Duration', true, 'minutes:'},
        {'Duration', true, 'hours:'},
        {'Duration', true, 'days:'},
        {'Duration', true, 'fromString:'}
    ]),
    ?assert(multiset_subset(ExpectedTally, OwnerMethodTally)).

%%====================================================================
%% Group 2 — live-patch gap reproduction (ADR 0114 Constraint 4).
%%
%% `AtomicCounter` is a real, always-loaded stdlib class (native-backed,
%% `stdlib/src/atomic_counter.bt`) used purely as "some other real class name"
%% for the patched method to reference — any loaded class name would do.
%%====================================================================

live_patch_gap_test_() ->
    {setup, fun live_patch_setup/0, fun live_patch_teardown/1, fun live_patch_gap/1}.

live_patch_setup() ->
    %% Boots the real runtime (idempotent alongside corpus_setup/0 within the
    %% same EUnit VM) so `AtomicCounter` is a real, indexed class to reference,
    %% then spawns one throwaway dynamic class of our own — never touching any
    %% real stdlib class process, so no global pg/class-registry cleanup is
    %% needed beyond stopping our own class and purging its own xref rows.
    ok = beamtalk_test_boot:boot_real_stdlib('AtomicCounter'),
    %% `beamtalk_xref:build_method_entry/5`'s `sends` channel (used by the
    %% "send channel does NOT have the same gap" assertion below) walks the
    %% patched source via `beamtalk_compiler:find_all_sends_in_source/1`,
    %% which round-trips through `beamtalk_compiler_server` — not started by
    %% `beamtalk_runtime` alone (`beamtalk_runtime.app.src` does not list
    %% `beamtalk_compiler` as a dependency). Started explicitly here so this
    %% test is deterministic in isolation, rather than relying on some other
    %% suite in the same EUnit VM having started it first.
    {ok, _} = application:ensure_all_started(beamtalk_compiler),
    ClassInfo = #{
        name => 'BT3268LiveFixture',
        module => bt3268_live_fixture,
        instance_methods => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link('BT3268LiveFixture', ClassInfo),
    Pid.

live_patch_teardown(Pid) ->
    gen_server:stop(Pid),
    beamtalk_xref:purge_class('BT3268LiveFixture'),
    ok.

live_patch_gap(Pid) ->
    %% One method, two syntactically distinct reference forms to the same
    %% class — a type annotation (return type) and a constructor send —
    %% mirroring the ADR's own examples of what `referencesTo:` normally
    %% catches (Decision § "renameTo: rewrites cross-file references").
    Source =
        <<
            "makeCounter -> AtomicCounter =>\n"
            "  AtomicCounter new: #bt3268LiveFixture"
        >>,
    NewFun = fun() -> ok end,
    ok = beamtalk_object_class:put_method(Pid, makeCounter, NewFun, Source),

    [
        {
            "build_method_entry/5 (the function Constraint 4 names) hard-codes "
            "references => [] for a live-patched method, even though this "
            "method's source carries two real references to AtomicCounter",
            fun() ->
                Entry = beamtalk_xref:build_method_entry(
                    false, makeCounter, Source, indexed, put_method
                ),
                %% Independent count of the real references in the fixture source
                %% (not derived from the compiler's own walker): the literal
                %% "AtomicCounter" substring appears exactly twice.
                ActualReferenceCount = count_occurrences(<<"AtomicCounter">>, Source),
                ?assertEqual(2, ActualReferenceCount),
                ?assertEqual([], maps:get(references, Entry))
            end
        },

        {
            "the gap reproduces end-to-end: references_to/1 never surfaces the "
            "live-patched class as a referencing site for AtomicCounter, "
            "confirmed against the real runtime index (not just the entry "
            "builder in isolation)",
            fun() ->
                Sites = beamtalk_xref:references_to('AtomicCounter'),
                OwnedByFixture = [S || S <- Sites, maps:get(owner, S) =:= 'BT3268LiveFixture'],
                ?assertEqual([], OwnedByFixture)
            end
        },

        {
            "the send channel does NOT have the same gap (ADR 0087's documented "
            "asymmetry, Constraint 4's contrast with sendersOf:): the "
            "constructor send's selector IS indexed for the live-patched method",
            fun() ->
                Sites = beamtalk_xref:senders_of('new:'),
                OwnedByFixture = [S || S <- Sites, maps:get(owner, S) =:= 'BT3268LiveFixture'],
                ?assertEqual(1, length(OwnedByFixture)),
                [Site] = OwnedByFixture,
                ?assertEqual(makeCounter, maps:get(method, Site))
            end
        }
    ].

%% Count non-overlapping occurrences of `Pattern` in `Bin` — a plain
%% substring tally, independent of any compiler/xref machinery, used as the
%% "actual blast radius" ground truth for the live-patch gap test above.
-spec count_occurrences(binary(), binary()) -> non_neg_integer().
count_occurrences(Pattern, Bin) ->
    case binary:matches(Bin, Pattern) of
        Matches when is_list(Matches) -> length(Matches)
    end.

%% True iff every element of `Expected` occurs in `Actual` at least as many
%% times (a multiset subset, not a plain set subset — some hand-audited
%% tuples above are legitimately duplicated, e.g. a method mentioning
%% `Duration` on two distinct source lines). Used so the corpus-accuracy
%% tests keep proving the hand audit's evidence is present without failing
%% when an unrelated future stdlib change adds more of the same shape.
-spec multiset_subset([term()], [term()]) -> boolean().
multiset_subset(Expected, Actual) ->
    ExpectedCounts = tally(Expected),
    ActualCounts = tally(Actual),
    maps:fold(
        fun(Key, Count, Ok) -> Ok andalso Count =< maps:get(Key, ActualCounts, 0) end,
        true,
        ExpectedCounts
    ).

tally(List) ->
    lists:foldl(fun(X, Acc) -> maps:update_with(X, fun(C) -> C + 1 end, 1, Acc) end, #{}, List).
