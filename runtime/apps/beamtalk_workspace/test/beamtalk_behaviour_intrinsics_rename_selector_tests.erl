%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_intrinsics_rename_selector_tests).

-moduledoc """
Integration tests for `beamtalk_behaviour_intrinsics:classRenameSelector/3`
and `classRenameSelectorIfAbsent/4` (ADR 0114 Phase 3, BT-3279) against real,
in-project fixture graphs.

Mirrors `beamtalk_behaviour_intrinsics_rename_to_tests.erl`'s fixture
pattern exactly (real `.bt` files on disk, loaded through the real
file-compile path so `beamtalk_xref`'s senders/implementors indexes are
actually populated — `senders_of/1`/`implementors_of/1` only see what was
compiled, not hand-constructed maps). Each scenario below uses its own
`Bt3279<Scenario>*` class-name prefix so the scenarios never share
xref/registry state.

## Scenarios

- `rename_selector_success_test_`: the basic happy path — an unoverridden
  selector with self-sends inside its own defining class AND a subclass;
  both confirmed and rewritten, in-memory dispatch to the new name works.
- `rename_selector_cross_hierarchy_test_`: an UNRELATED class (no
  inheritance relation) self-sending the same selector name lands in
  `candidate_sites`, never `sites`.
- `rename_selector_override_shadowing_test_`: a subclass overriding the
  renamed selector, with a further subclass sending `self` to it — EVERY
  self/super site for that selector, INCLUDING ones owned by the target
  class itself, lands in `candidate_sites`; only the definition itself is
  still renamed (ADR 0114 § ChangeLog schema: "sites[0] is always the
  definition site").
- `rename_selector_collision_test_`: `NewSelector` already locally defined
  raises with the ADR's exact hint text.
- `rename_selector_if_absent_test_`: the `ifAbsent:` escape hatch runs its
  block when `OldSelector` is absent, without touching the class.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Shared harness (mirrors beamtalk_behaviour_intrinsics_rename_to_tests.erl)
%%====================================================================

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

start_fixture(Prefix, Files) ->
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
    ProjDir = filename:join(temp_dir(), Prefix ++ "-" ++ Unique),
    ok = filelib:ensure_path(ProjDir),
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => list_to_binary(Prefix ++ "_ws"),
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        repl => false
    }),
    beamtalk_compiler_server:clear_classes(),
    State0 = beamtalk_repl_state:new(undefined, 0),
    lists:foreach(
        fun({FileName, Source}) ->
            Path = filename:join(ProjDir, FileName),
            ok = file:write_file(Path, Source),
            {ok, _Classes, _State} = beamtalk_repl_loader:handle_load(Path, State0)
        end,
        Files
    ),
    #{proj_dir => ProjDir}.

stop_classes(ClassNames) ->
    lists:foreach(
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    catch gen_server:stop(Pid, normal, 5000)
            end
        end,
        ClassNames
    ).

stop_meta() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end.

class_object(ClassName) ->
    beamtalk_class_registry:class_object_from_pid(
        beamtalk_class_registry:whereis_class(ClassName)
    ).

tracked_source(ClassNameBin) ->
    unicode:characters_to_binary(beamtalk_workspace_meta:get_class_source(ClassNameBin)).

%%====================================================================
%% Happy path: unoverridden selector, self-sends in the defining class AND
%% a subclass — both confirmed, rewritten, and live post-rename.
%%====================================================================

happy_base_source() ->
    <<
        "Value subclass: Bt3279HappyBase\n"
        "  bump -> String => \"bumped\"\n"
        "  increment -> String => self bump"
    >>.

happy_sub_source() ->
    <<
        "Bt3279HappyBase subclass: Bt3279HappySub\n"
        "  extra -> String => self bump"
    >>.

rename_selector_success_test_() ->
    {setup, fun setup_happy/0, fun teardown_happy/1, fun rename_selector_success/1}.

setup_happy() ->
    start_fixture("bt3279-happy", [
        {"bt3279_happy_base.bt", happy_base_source()},
        {"bt3279_happy_sub.bt", happy_sub_source()}
    ]).

teardown_happy(_Fixture) ->
    stop_classes(['Bt3279HappyBase', 'Bt3279HappySub']),
    stop_meta().

rename_selector_success(_Fixture) ->
    Result = beamtalk_behaviour_intrinsics:classRenameSelector(
        class_object('Bt3279HappyBase'), bump, boost
    ),
    [
        %% Method rename never changes the class's own identity.
        ?_assertMatch(#beamtalk_object{class = 'Bt3279HappyBase class'}, Result),
        %% Definition site rewritten (own class).
        ?_assertEqual(
            <<
                "Value subclass: Bt3279HappyBase\n"
                "  boost -> String => \"bumped\"\n"
                "  increment -> String => self boost"
            >>,
            tracked_source(<<"Bt3279HappyBase">>)
        ),
        %% Confirmed reference site: the subclass's own self-send.
        ?_assertEqual(
            <<
                "Bt3279HappyBase subclass: Bt3279HappySub\n"
                "  extra -> String => self boost"
            >>,
            tracked_source(<<"Bt3279HappySub">>)
        ),
        %% Genuinely live: the recompiled+hot-reloaded subclass now
        %% dispatches its self-send under the new name.
        ?_assertMatch(
            {ok, _, _, _, _},
            beamtalk_repl_eval:do_eval(
                "Bt3279HappySub new extra", beamtalk_repl_state:new(undefined, 0)
            )
        )
    ].

%%====================================================================
%% Cross-hierarchy false positive: an unrelated class (no inheritance
%% relation) defining/sending the same selector name lands in
%% `candidate_sites`, never `sites`.
%%====================================================================

xclass_base_source() ->
    <<
        "Value subclass: Bt3279XClassBase\n"
        "  bump -> String => \"bumped\"\n"
        "  increment -> String => self bump"
    >>.

xclass_unrelated_source() ->
    <<
        "Value subclass: Bt3279XClassUnrelated\n"
        "  ping -> String => self bump"
    >>.

rename_selector_cross_hierarchy_test_() ->
    {setup, fun setup_xclass/0, fun teardown_xclass/1, fun rename_selector_cross_hierarchy/1}.

setup_xclass() ->
    Fixture = start_fixture("bt3279-xclass", [
        {"bt3279_xclass_base.bt", xclass_base_source()},
        {"bt3279_xclass_unrelated.bt", xclass_unrelated_source()}
    ]),
    start_changelog(Fixture, "bt3279-xclass-cl").

teardown_xclass(Fixture) ->
    stop_changelog(Fixture),
    stop_classes(['Bt3279XClassBase', 'Bt3279XClassUnrelated']),
    stop_meta().

rename_selector_cross_hierarchy(_Fixture) ->
    _ = beamtalk_behaviour_intrinsics:classRenameSelector(
        class_object('Bt3279XClassBase'), bump, boost
    ),
    [Entry] = beamtalk_workspace_changelog:entries(),
    CandidateSourceFiles = [
        maps:get(source_file, C)
     || C <- beamtalk_workspace_changelog:entry_candidate_sites(Entry)
    ],
    [
        %% Own definition + own self-send confirmed and rewritten.
        ?_assertEqual(
            <<
                "Value subclass: Bt3279XClassBase\n"
                "  boost -> String => \"bumped\"\n"
                "  increment -> String => self boost"
            >>,
            tracked_source(<<"Bt3279XClassBase">>)
        ),
        %% The unrelated class's self-send is NEVER rewritten...
        ?_assertEqual(
            <<
                "Value subclass: Bt3279XClassUnrelated\n"
                "  ping -> String => self bump"
            >>,
            tracked_source(<<"Bt3279XClassUnrelated">>)
        ),
        %% ...it is reported as a candidate instead.
        ?_assert(
            lists:any(
                fun(F) -> binary:match(F, <<"bt3279_xclass_unrelated">>) =/= nomatch end,
                CandidateSourceFiles
            )
        )
    ].

%%====================================================================
%% Intra-hierarchy override-shadowing: a subclass overrides the renamed
%% selector; a further subclass sends `self` to it. EVERY self/super site
%% for that selector — including ones owned by the renamed class itself —
%% must land in candidate_sites, not just sites downstream of the override.
%% Only the definition itself is still renamed (ChangeLog schema:
%% "sites[0] is always the definition site").
%%====================================================================

override_base_source() ->
    <<
        "Value subclass: Bt3279OverrideBase\n"
        "  bump -> String => \"bumped\"\n"
        "  increment -> String => self bump"
    >>.

override_sub_source() ->
    <<
        "Bt3279OverrideBase subclass: Bt3279OverrideSub\n"
        "  bump -> String => \"overridden\""
    >>.

override_deep_source() ->
    <<
        "Bt3279OverrideSub subclass: Bt3279OverrideDeep\n"
        "  deeper -> String => self bump"
    >>.

rename_selector_override_shadowing_test_() ->
    {setup, fun setup_override/0, fun teardown_override/1,
        fun rename_selector_override_shadowing/1}.

setup_override() ->
    Fixture = start_fixture("bt3279-override", [
        {"bt3279_override_base.bt", override_base_source()},
        {"bt3279_override_sub.bt", override_sub_source()},
        {"bt3279_override_deep.bt", override_deep_source()}
    ]),
    start_changelog(Fixture, "bt3279-override-cl").

teardown_override(Fixture) ->
    stop_changelog(Fixture),
    stop_classes(['Bt3279OverrideBase', 'Bt3279OverrideSub', 'Bt3279OverrideDeep']),
    stop_meta().

rename_selector_override_shadowing(_Fixture) ->
    _ = beamtalk_behaviour_intrinsics:classRenameSelector(
        class_object('Bt3279OverrideBase'), bump, boost
    ),
    [Entry] = beamtalk_workspace_changelog:entries(),
    CandidateSourceFiles = [
        maps:get(source_file, C)
     || C <- beamtalk_workspace_changelog:entry_candidate_sites(Entry)
    ],
    [
        %% The definition itself IS still renamed — sites[0] is always the
        %% definition site, unconditionally.
        ?_assertEqual(
            <<
                "Value subclass: Bt3279OverrideBase\n"
                "  boost -> String => \"bumped\"\n"
                "  increment -> String => self bump"
            >>,
            tracked_source(<<"Bt3279OverrideBase">>)
        ),
        %% ...but its OWN self-send inside `increment` is NOT rewritten —
        %% override-freedom fails for the WHOLE selector, so even a site
        %% owned by the renamed class itself is a candidate, never confirmed.
        ?_assert(
            lists:any(
                fun(F) -> binary:match(F, <<"bt3279_override_base">>) =/= nomatch end,
                CandidateSourceFiles
            )
        ),
        %% The override itself is untouched (still named `bump`).
        ?_assertEqual(
            <<
                "Bt3279OverrideBase subclass: Bt3279OverrideSub\n"
                "  bump -> String => \"overridden\""
            >>,
            tracked_source(<<"Bt3279OverrideSub">>)
        ),
        %% The deep subclass's self-send is also a candidate, not rewritten.
        ?_assertEqual(
            <<
                "Bt3279OverrideSub subclass: Bt3279OverrideDeep\n"
                "  deeper -> String => self bump"
            >>,
            tracked_source(<<"Bt3279OverrideDeep">>)
        ),
        ?_assert(
            lists:any(
                fun(F) -> binary:match(F, <<"bt3279_override_deep">>) =/= nomatch end,
                CandidateSourceFiles
            )
        )
    ].

%%====================================================================
%% Collision refusal: `NewSelector` already locally defined raises with
%% the ADR's exact hint text.
%%====================================================================

collision_source() ->
    <<
        "Value subclass: Bt3279CollisionClass\n"
        "  foo -> String => \"foo\"\n"
        "  bar -> String => \"bar\""
    >>.

rename_selector_collision_test_() ->
    {setup, fun setup_collision/0, fun teardown_collision/1, fun rename_selector_collision/1}.

setup_collision() ->
    start_fixture("bt3279-collision", [
        {"bt3279_collision_class.bt", collision_source()}
    ]).

teardown_collision(_Fixture) ->
    stop_classes(['Bt3279CollisionClass']),
    stop_meta().

rename_selector_collision(_Fixture) ->
    try
        beamtalk_behaviour_intrinsics:classRenameSelector(
            class_object('Bt3279CollisionClass'), foo, bar
        ),
        [?_assert(false)]
    catch
        error:#{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = Kind, message = Message, hint = Hint}
        } ->
            [
                ?_assertEqual(selector_already_exists, Kind),
                ?_assert(binary:match(Message, <<"already defines #bar locally">>) =/= nomatch),
                ?_assert(binary:match(Message, <<"refusing to overwrite">>) =/= nomatch),
                ?_assert(binary:match(Hint, <<"removeSelector: #bar">>) =/= nomatch)
            ]
    end.

%%====================================================================
%% `renameSelector:to:ifAbsent:` runs the block when OldSelector is absent.
%%====================================================================

absent_source() ->
    <<
        "Value subclass: Bt3279AbsentClass\n"
        "  onlyThis -> String => \"here\""
    >>.

rename_selector_if_absent_test_() ->
    {setup, fun setup_absent/0, fun teardown_absent/1, fun rename_selector_if_absent/1}.

setup_absent() ->
    start_fixture("bt3279-absent", [
        {"bt3279_absent_class.bt", absent_source()}
    ]).

teardown_absent(_Fixture) ->
    stop_classes(['Bt3279AbsentClass']),
    stop_meta().

rename_selector_if_absent(_Fixture) ->
    Result = beamtalk_behaviour_intrinsics:classRenameSelectorIfAbsent(
        class_object('Bt3279AbsentClass'),
        missingSelector,
        newName,
        fun() -> not_found_marker end
    ),
    [
        ?_assertEqual(not_found_marker, Result),
        %% The class is untouched.
        ?_assertEqual(
            <<
                "Value subclass: Bt3279AbsentClass\n"
                "  onlyThis -> String => \"here\""
            >>,
            tracked_source(<<"Bt3279AbsentClass">>)
        )
    ].

%%====================================================================
%% ChangeLog harness helper (mirrors
%% beamtalk_behaviour_intrinsics_rename_to_tests.erl's setup_with_changelog/0)
%%====================================================================

start_changelog(Fixture, Prefix) ->
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    Unique = os:getpid() ++ "-" ++ integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary(Prefix ++ "-" ++ Unique),
    ChangelogHome = filename:join(temp_dir(), Prefix ++ "-home-" ++ Unique),
    ok = filelib:ensure_path(ChangelogHome),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", ChangelogHome),
    {ok, _} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    Fixture#{old_home => OldHome}.

stop_changelog(#{old_home := OldHome}) ->
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    case OldHome of
        false -> os:unsetenv("HOME");
        _ -> os:putenv("HOME", OldHome)
    end,
    ok.
