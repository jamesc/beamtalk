%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_move_class_tests).

-moduledoc """
Integration tests for `beamtalk_repl_loader:move_class/2` (ADR 0114 Phase 2,
BT-3272; `Workspace moveClass:to:`) against a real, in-project fixture.

`Workspace` (the singleton) is not available in the BUnit test context
(`workspace_mode: false` — see `stdlib/test/workspace_interface_test.bt`'s
own header comment), so `Workspace moveClass:to:` cannot be exercised there
at all — BUnit only verifies the selector is present on the facade
(`testIncludesSelectorMoveClassTo`). This module is where `moveClass:to:`'s
actual behavior — the byte-identical declaration-site rewrite, the
`'rename-class'` ChangeLog entry it produces, and the real file move once
`Workspace flushIncludingDestructive` replays that entry (BT-3271) — is
exercised end-to-end, mirroring `beamtalk_behaviour_intrinsics_rename_to_tests.erl`'s
identical fixture pattern (real `.bt` files on disk, `project_path` set so
`classify_source_file/1` classifies them as flushable).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixture sources
%%====================================================================

gadget_source() ->
    <<
        "Value subclass: Bt3272Gadget\n"
        "  greet -> String => \"hi\""
    >>.

%%====================================================================
%% Integration fixture: real compiler port + workspace_meta, real files
%%====================================================================

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

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
    ProjDir = filename:join(temp_dir(), "bt-move-class-" ++ Unique),
    ok = filelib:ensure_path(ProjDir),
    GadgetPath = filename:join(ProjDir, "bt3272_gadget.bt"),
    ok = file:write_file(GadgetPath, gadget_source()),
    %% `project_path` set to the fixture directory so `classify_source_file/1`
    %% classifies this file as in-project/flushable rather than "dependency"
    %% — the same reason `beamtalk_behaviour_intrinsics_rename_to_tests.erl`
    %% needs it.
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"move_class_test_ws">>,
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        repl => false
    }),
    beamtalk_compiler_server:clear_classes(),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _GadgetClasses, _State1} = beamtalk_repl_loader:handle_load(GadgetPath, State0),
    #{proj_dir => ProjDir, gadget_path => GadgetPath}.

teardown(_) ->
    lists:foreach(
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    catch gen_server:stop(Pid, normal, 5000)
            end
        end,
        ['Bt3272Gadget']
    ),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    ok.

%% Same "real ChangeLog needs a resolvable HOME" wrinkle
%% `beamtalk_behaviour_intrinsics_rename_to_tests.erl`'s own
%% `setup_with_changelog/0` documents (`os:getpid()` closes the
%% `erlang:unique_integer/1`-alone gap across separate `rebar3 eunit` runs).
setup_with_changelog() ->
    Fixture = setup(),
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    Unique = os:getpid() ++ "-" ++ integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("bt-move-class-changelog-" ++ Unique),
    ChangelogHome = filename:join(temp_dir(), "bt-move-class-changelog-home-" ++ Unique),
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

%%====================================================================
%% Full success path: move (in memory) + ChangeLog entry + real disk flush
%%====================================================================

move_class_success_test_() ->
    {setup, fun setup_with_changelog/0, fun teardown_with_changelog/1, fun move_class_success/1}.

move_class_success(#{proj_dir := ProjDir, gadget_path := GadgetPath}) ->
    NewPath = filename:join([ProjDir, "moved", "bt3272_gadget.bt"]),
    NewPathBin = list_to_binary(NewPath),
    OldPid = beamtalk_class_registry:whereis_class('Bt3272Gadget'),

    Result = beamtalk_repl_loader:move_class('Bt3272Gadget', NewPathBin),

    NewPid = beamtalk_class_registry:whereis_class('Bt3272Gadget'),
    [Entry] = beamtalk_workspace_changelog:entries(),
    Sites = beamtalk_workspace_changelog:entry_sites(Entry),

    {ok, Summary} = beamtalk_workspace_flush:flush_including_destructive(),

    [
        %% move_class/2 itself: no reinstall, no identity change — the SAME
        %% pid still answers to the SAME name.
        ?_assertEqual(ok, Result),
        ?_assertEqual(OldPid, NewPid),
        ?_assert(is_pid(NewPid)),
        %% The tracked source is byte-identical — moveClass:to: never rewrites
        %% the declaration text, only where the file lives.
        ?_assertEqual(
            gadget_source(),
            unicode:characters_to_binary(
                beamtalk_workspace_meta:get_class_source(<<"Bt3272Gadget">>)
            )
        ),
        %% ChangeLog entry: `'rename-class'`, `old_class == class` (same
        %% identity, different path), single site (the declaration only —
        %% no reference rewrite is ever attempted for a pure move).
        ?_assertEqual('rename-class', beamtalk_workspace_changelog:entry_kind(Entry)),
        ?_assertEqual(<<"Bt3272Gadget">>, beamtalk_workspace_changelog:entry_class(Entry)),
        ?_assertEqual(<<"Bt3272Gadget">>, beamtalk_workspace_changelog:entry_old_class(Entry)),
        ?_assertEqual(
            list_to_binary(GadgetPath), beamtalk_workspace_changelog:entry_old_path(Entry)
        ),
        ?_assertEqual(NewPathBin, beamtalk_workspace_changelog:entry_new_path(Entry)),
        ?_assertEqual(true, beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(1, length(Sites)),
        %% Flush relocates the file: old path gone, new path holds the exact
        %% same (unrewritten) declaration.
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(false, filelib:is_regular(GadgetPath)),
        ?_assertEqual(true, filelib:is_regular(NewPath)),
        ?_assertEqual({ok, gadget_source()}, file:read_file(NewPath)),
        %% The class is still live and answers under its own (unchanged) name
        %% after the flush, from its (unchanged) pid.
        ?_assertEqual(NewPid, beamtalk_class_registry:whereis_class('Bt3272Gadget'))
    ].

%%====================================================================
%% Refusal: a dynamic (ClassBuilder) class has no backing file to move at
%% all — `no_source_file`, deliberately stricter than `classRenameTo/2`'s
%% permissive `flushable: false` treatment of the identical classification
%% (see `move_class/2`'s own doc for why).
%%====================================================================

move_class_dynamic_no_source_file_test_() ->
    {setup, fun setup_dynamic/0, fun teardown_dynamic/1, fun move_class_dynamic_no_source_file/1}.

setup_dynamic() ->
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
    ProjDir = filename:join(temp_dir(), "bt-move-class-dyn-" ++ Unique),
    ok = filelib:ensure_path(ProjDir),
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"move_class_dyn_test_ws">>,
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        repl => false
    }),
    beamtalk_compiler_server:clear_classes(),
    State = #{
        className => 'Bt3272DynGadget',
        superclassRef => 'Object',
        fieldSpecs => #{},
        methodSpecs => #{}
    },
    {ok, _Pid} = beamtalk_class_builder:register(State),
    #{proj_dir => ProjDir}.

teardown_dynamic(_Fixture) ->
    case beamtalk_class_registry:whereis_class('Bt3272DynGadget') of
        undefined -> ok;
        Pid when is_pid(Pid) -> catch gen_server:stop(Pid, normal, 5000)
    end,
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    ok.

move_class_dynamic_no_source_file(#{proj_dir := ProjDir}) ->
    NewPath = list_to_binary(filename:join(ProjDir, "dyn_gadget.bt")),
    Result = beamtalk_repl_loader:move_class('Bt3272DynGadget', NewPath),
    [
        ?_assertMatch({error, #beamtalk_error{kind = no_source_file}}, Result)
    ].

%%====================================================================
%% Refusal: a stdlib class's `.bt` source lives outside any project this
%% workspace could ever move a file into — same reasoning `classRenameTo/2`
%% already applies (ADR 0114 § Refusal vs flushability).
%%====================================================================

move_class_stdlib_refusal_test_() ->
    {setup, fun setup_dynamic/0, fun teardown_dynamic/1, fun move_class_stdlib_refusal/1}.

move_class_stdlib_refusal(#{proj_dir := ProjDir}) ->
    NewPath = list_to_binary(filename:join(ProjDir, "object.bt")),
    Result = beamtalk_repl_loader:move_class('Object', NewPath),
    [
        ?_assertMatch({error, #beamtalk_error{kind = runtime_error}}, Result)
    ].
