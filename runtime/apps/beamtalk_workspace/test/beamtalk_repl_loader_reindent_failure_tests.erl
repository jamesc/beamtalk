%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_reindent_failure_tests).

-moduledoc """
Fault-injection coverage (BT-3335) for the `reindent_failed` flushability
downgrade shared by `new_method_entry/3` (a brand-new method, no prior
on-disk span) and `store_disk_shaped_entry/4` (a patch to a method that
already has one) in `beamtalk_repl_loader.erl`: when the store-time
compiler-port reshape (`beamtalk_compiler:reindent_method_source/2`) fails
on an otherwise-successful method install/patch, the ChangeLog entry must
downgrade to memory-only (`not_flushable_reason = "reindent_failed"`) rather
than store a column-0 body flush would splice into an indented region and
corrupt the file (BT-2594's own reasoning for keeping this fallback at all).

Reuses `beamtalk_repl_loader_rewrite_sites_tests.erl`'s `meck` precedent
(BT-3280) — see that module's moduledoc for the general reasoning against
mocking a shared system module. This case is simpler: `reindent_method_
source/2` is called on `beamtalk_compiler` — a DIFFERENT module from the one
under test — so an ordinary `meck:new(beamtalk_compiler, [passthrough])`
intercepts the real, externally-qualified call (`beamtalk_repl_loader.erl`
always calls it as `beamtalk_compiler:reindent_method_source/2`, never
locally), with no need for the self-mock-the-module-under-test workaround
that seam required. `[passthrough]` keeps every other `beamtalk_compiler`
function — including the real compile call `install_method/8` itself makes —
running for real; only `reindent_method_source/2` is faulted, and only for
the lifetime of one `{setup, ...}` fixture (`meck:unload/1` in teardown), so
no other test in the same `rebar3 eunit` run ever sees a faulted compiler.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixture: real compiler port + workspace_meta/changelog, one class with an
%% existing method (`value`) to patch, and a brand-new selector (`bumped`)
%% to append — covers both `reindent_failed` call sites with one fixture.
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
    %% Same "real ChangeLog needs a resolvable HOME" wrinkle
    %% `beamtalk_behaviour_intrinsics_rename_to_tests.erl`'s own
    %% `setup_with_changelog/0` documents. Cross-invocation-unique (BT-3281)
    %% — see `beamtalk_test_unique:id/0`.
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    Unique = beamtalk_test_unique:id(),
    ProjDir = filename:join(temp_dir(), "bt-reindent-fail-" ++ Unique),
    ChangelogHome = filename:join(temp_dir(), "bt-reindent-fail-home-" ++ Unique),
    ok = filelib:ensure_path(ProjDir),
    ok = filelib:ensure_path(ChangelogHome),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", ChangelogHome),
    GadgetPath = filename:join(ProjDir, "reindent_gadget.bt"),
    ok = file:write_file(
        GadgetPath,
        <<"Actor subclass: ReindentGadget\n  state: v = 1\n\n  value -> Integer =>\n    self.v\n">>
    ),
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"reindent_fail_test_ws">>,
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        repl => false
    }),
    {ok, _} = beamtalk_workspace_changelog:start_link(#{
        workspace_id => <<"reindent_fail_test_ws">>
    }),
    beamtalk_compiler_server:clear_classes(),
    State0 = beamtalk_repl_state:new(undefined, 0),
    {ok, _Classes, State1} = beamtalk_repl_loader:handle_load(GadgetPath, State0),
    #{proj_dir => ProjDir, gadget_path => GadgetPath, state => State1, old_home => OldHome}.

teardown(#{old_home := OldHome}) ->
    case beamtalk_class_registry:whereis_class('ReindentGadget') of
        undefined -> ok;
        Pid when is_pid(Pid) -> catch gen_server:stop(Pid, normal, 5000)
    end,
    case whereis(beamtalk_workspace_changelog) of
        undefined -> ok;
        LogPid -> gen_server:stop(LogPid)
    end,
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    case OldHome of
        false -> os:unsetenv("HOME");
        _ -> os:putenv("HOME", OldHome)
    end,
    ok.

setup_with_fault() ->
    Fixture = setup(),
    meck:new(beamtalk_compiler, [passthrough]),
    meck:expect(beamtalk_compiler, reindent_method_source, fun(_Source, _BaseIndent) ->
        {error, injected_reindent_fault, <<"injected by BT-3335 fault-injection test">>}
    end),
    Fixture.

teardown_with_fault(Fixture) ->
    meck:unload(beamtalk_compiler),
    teardown(Fixture).

%%====================================================================
%% new_method_entry/3's `reindent_failed` branch: a brand-new method (no
%% prior on-disk span — `selector_not_found`) whose store-time reshape of
%% the compiler's column-0 canonical body to the class's sibling-method
%% indentation fails.
%%====================================================================

new_method_reindent_failed_test_() ->
    {setup, fun setup_with_fault/0, fun teardown_with_fault/1, fun new_method_reindent_failed/1}.

new_method_reindent_failed(#{state := State}) ->
    Result = beamtalk_repl_loader:install_method(
        <<"ReindentGadget">>,
        <<"bumped">>,
        <<"bumped -> Integer =>\n  self.v + 1">>,
        durable,
        <<"test">>,
        human,
        [],
        State
    ),
    [Entry] = [
        E
     || E <- beamtalk_workspace_changelog:active_entries(),
        beamtalk_workspace_changelog:entry_selector(E) =:= <<"bumped">>
    ],
    [
        %% The install itself still succeeds — a store-time reshape failure
        %% only downgrades the ChangeLog entry's flushability, never the
        %% live install that already happened.
        ?_assertMatch({ok, _, _, _, _}, Result),
        ?_assertEqual(false, beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            <<"reindent_failed">>, beamtalk_workspace_changelog:entry_not_flushable_reason(Entry)
        )
    ].

%%====================================================================
%% store_disk_shaped_entry/4's `reindent_failed` branch: patching a method
%% that ALREADY has an on-disk span, whose store-time reshape of the
%% compiler's column-0 canonical body to the span's own on-disk indentation
%% fails.
%%====================================================================

existing_method_reindent_failed_test_() ->
    {setup, fun setup_with_fault/0, fun teardown_with_fault/1,
        fun existing_method_reindent_failed/1}.

existing_method_reindent_failed(#{state := State}) ->
    Result = beamtalk_repl_loader:install_method(
        <<"ReindentGadget">>,
        <<"value">>,
        <<"value -> Integer =>\n  self.v * 2">>,
        durable,
        <<"test">>,
        human,
        [],
        State
    ),
    [Entry] = [
        E
     || E <- beamtalk_workspace_changelog:active_entries(),
        beamtalk_workspace_changelog:entry_selector(E) =:= <<"value">>
    ],
    [
        ?_assertMatch({ok, _, _, _, _}, Result),
        ?_assertEqual(false, beamtalk_workspace_changelog:entry_flushable(Entry)),
        ?_assertEqual(
            <<"reindent_failed">>, beamtalk_workspace_changelog:entry_not_flushable_reason(Entry)
        )
    ].
