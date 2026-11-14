%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_protocol).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Protocol reload fan-out and live protocol patching for the Beamtalk REPL
(ADR 0127 §11).

Owns the atomic protocol reload (`reload_protocol_fanout/3,4`: recompile every
class that `uses:` a reloaded protocol, install them all, or roll everything
back) and the live protocol patch/remove paths
(`patch_protocol_provision/6`, `remove_protocol_provision/3`) that edit one
provision in a protocol's source and fan the result out. The two share the
fan-out machinery, which is why they live together.

Extracted from `beamtalk_repl_loader` (BT-4702). It is a client of the
loader's compile/install primitives (`compile_reload_source/4,5`,
`install_reload_result/2`, …), which stay in `beamtalk_repl_loader` because
the single-site class paths share them; the loader calls into this module from
`reload_compile_and_load/4`, `install_method`, `reload_method_definition` and
`remove_method`.
""".

-export([
    reload_protocol_fanout/3,
    reload_protocol_fanout/4,
    is_protocol_name/1,
    patch_protocol_provision/6,
    remove_protocol_provision/3
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).

%%% ----------------------------------------------------------------------------
%%% ADR 0127 §11 / BT-3593: atomic protocol reload fan-out
%%% ----------------------------------------------------------------------------

-doc """
Reload a protocol file and atomically fan the edit out to every loaded,
source-backed, non-stdlib user (ADR 0127 §11, "Live patching is a message
send"; BT-3593's reload-mechanics half).

`ProtocolResult` is `compile_reload_source/4`'s already-successful
`{ok, protocol_definition, ProtocolInfo}` for the just-recompiled protocol
file at `Path`; `Source` is that file's raw text. Called from
`reload_compile_and_load/4` in place of `install_reload_result/2` — a
protocol reload can never stop at installing just the protocol module the
way a class reload installs just that class, because every loaded user's
`uses:`-flattened body is baked from the protocol's OLD provisions until it,
too, recompiles.

Refuses outright, before compiling a single user, when `Path` is a stdlib
file — stdlib protocols are read-only in the workspace, mirroring
`beamtalk_repl_eval:stdlib_method_read_only_error/2`'s class-level refusal
for the same reason.

Otherwise runs two stages, per the ADR's "all-or-nothing" requirement:

1. **Compile.** Every class `beamtalk_protocol_registry:users_of/1` names for
   one of this file's protocols, that ALSO has a `beamtalk_workspace_meta`
   tracked source, is recompiled from that tracked source — `protocol_sources`
   (this protocol's just-compiled `Source`, keyed by name) merged into
   `beamtalk_repl_compiler:build_class_indexes/0`'s ordinary superclass/module
   indexes, so `uses:` flattening sees the NEW provisions (ADR 0127 §10a) —
   but NOTHING is installed yet, mirroring `beamtalk_repl_loader_rewrite:rewrite_sites/2`'s own
   compile-then-install split. A user with no tracked source (no backing
   file, or a class the workspace never loaded from one) is not a failure:
   it is recorded under `skipped` and warned about via `?LOG_WARNING`, per
   the ADR's "users without workspace source keep their old code" allowance.
   If ANY user's recompile fails, the whole reload is rejected —
   `{error, #beamtalk_error{}}` naming every failing user with its own
   diagnostic — and nothing from this call installs, not even the protocol
   itself.
2. **Install.** Only once every user has compiled does anything load: the
   protocol module first, then each user in turn, both via the same
   `install_reload_result/2` chokepoint an ordinary single-class reload
   uses — so ADR 0105's shape re-check, `beamtalk_alias_xref` refresh, and
   actor hot-reload all run exactly as they would for a ordinary reload (see
   `activate_module/4`'s own doc). Failing here is expected to be rare
   (every module reaching this stage already compiled successfully — the
   same reasoning `beamtalk_repl_loader_rewrite:rewrite_sites/2`'s doc gives for its own install pass),
   but is still handled: if install fails partway through, every module
   THIS call already installed in stage 2 — protocol included — is rolled
   back to its previous code before the error is returned, so a
   partial-install failure can never leave some users on the new provisions
   and others on the old ones. "Previous code" is recompiled from the
   previously-tracked source (`beamtalk_workspace_meta`), NOT a binary
   captured via `code:get_object_code/1` — that call reliably returns
   `error` for every REPL-loaded class (see `install_protocol_fanout`'s own
   comment). A module with no previously-tracked source (its first-ever
   load) has nothing to roll back to; it is left on the code this call just
   installed, since there is no "before" state.

Returns `{ok, AllClassNames}` — the protocol's own pseudo-class entries
followed by every successfully-installed user's, in the same
`[#{name := string(), superclass := string()}]` shape `reload_class_file/1`
already promises its callers (`:reload`, `Counter reload`, `Workspace
flush`) — so this is a drop-in superset of the single-protocol reload it
replaces, not a breaking return-shape change.
""".
-spec reload_protocol_fanout({ok, protocol_definition, map()}, string(), string()) ->
    {ok, [map()]} | {error, #beamtalk_error{}}.
reload_protocol_fanout(ProtocolResult, Path, Source) ->
    reload_protocol_fanout(ProtocolResult, Path, Source, #{}).

%% `reload_protocol_fanout/3` inside a `load_project` batch: `BatchOpts`
%% rides each user's stage-1 recompile (BT-4394). The rollback recompile
%% stays untagged: it runs only on a failed install, where re-verifying the
%% project scope on the next file costs nothing that matters.
-spec reload_protocol_fanout({ok, protocol_definition, map()}, string(), string(), map()) ->
    {ok, [map()]} | {error, #beamtalk_error{}}.
reload_protocol_fanout(
    {ok, protocol_definition, ProtocolInfo} = ProtocolResult, Path, Source, BatchOpts
) ->
    case beamtalk_repl_loader:is_stdlib_path(Path) of
        true ->
            {error, stdlib_protocol_reload_read_only_error(ProtocolInfo)};
        false ->
            ProtocolAtoms = protocol_atoms_from_info(ProtocolInfo),
            {TargetAtoms, SkippedNameBins} = discover_fanout_targets(ProtocolAtoms),
            %% This reload's own protocol(s) map to their fresh (not-yet-
            %% installed) `Source` — merged OVER the ambient, possibly-stale
            %% `all_protocol_sources/0` snapshot (still tracking the OLD
            %% source for this same name until stage 2 installs the new
            %% one) so a fan-out user using more than one protocol still
            %% resolves every OTHER one it needs, not just the one being
            %% reloaded — see `with_ambient_protocol_sources/1`'s doc.
            ProtocolSources = maps:merge(
                beamtalk_workspace_meta:all_protocol_sources(),
                build_protocol_sources_map(ProtocolAtoms, Source)
            ),
            %% BT-4828: this reload's protocol(s) live at `Path`; every other
            %% tracked protocol keeps the path it was loaded from.
            ProtocolSourcePaths = maps:merge(
                beamtalk_workspace_meta:all_protocol_source_paths(),
                maps:from_list([
                    {atom_to_binary(ProtocolAtom, utf8), Path}
                 || ProtocolAtom <- ProtocolAtoms
                ])
            ),
            FanoutOpts = BatchOpts#{protocol_source_paths => ProtocolSourcePaths},
            warn_skipped_fanout_users(ProtocolAtoms, SkippedNameBins),
            case compile_fanout_users(TargetAtoms, ProtocolSources, FanoutOpts) of
                {ok, CompiledUsers} ->
                    install_protocol_fanout(ProtocolResult, Path, Source, CompiledUsers);
                {error, Failures} ->
                    {error, protocol_reload_rejected_error(ProtocolAtoms, Failures)}
            end
    end.

%% This file's own protocol names, as existing atoms — `compile_reload_source/4`
%% only reaches this point after the compiler itself accepted these as valid
%% protocol names, and a RELOAD (as opposed to a first-ever load) means each
%% one was already registered by an earlier load, so the atom already exists.
-spec protocol_atoms_from_info(map()) -> [atom()].
protocol_atoms_from_info(#{protocols := ProtocolNameBins}) ->
    lists:filtermap(
        fun(NameBin) ->
            beamtalk_repl_loader:safe_atom_result(
                beamtalk_repl_server:safe_to_existing_atom(NameBin)
            )
        end,
        ProtocolNameBins
    ).

%% Every loaded user of any of `ProtocolAtoms`, partitioned into
%% `{TargetAtoms, SkippedNameBins}` — a target has a `beamtalk_workspace_meta`
%% tracked source to recompile from; a skip does not (no backing file, or a
%% class the workspace never tracked source for at all). `lists:usort/1`
%% both de-duplicates a class using more than one of this file's protocols
%% and gives a deterministic (alphabetical) install order.
-spec discover_fanout_targets([atom()]) -> {[atom()], [binary()]}.
discover_fanout_targets(ProtocolAtoms) ->
    UserAtoms = lists:usort(
        lists:flatmap(fun beamtalk_protocol_registry:users_of/1, ProtocolAtoms)
    ),
    {TargetsRev, SkippedRev} = lists:foldl(
        fun(ClassAtom, {TargetsAcc, SkippedAcc}) ->
            ClassNameBin = beamtalk_repl_loader:normalize_class_source_key(ClassAtom),
            case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
                undefined -> {TargetsAcc, [ClassNameBin | SkippedAcc]};
                _Source -> {[ClassAtom | TargetsAcc], SkippedAcc}
            end
        end,
        {[], []},
        UserAtoms
    ),
    {lists:reverse(TargetsRev), lists:reverse(SkippedRev)}.

%% `protocol_sources` context (ADR 0127 §10a) for every fanned-out user's own
%% recompile: this file's protocol name(s) mapped to its own just-compiled
%% `Source` text, so `uses:` flattening sees the NEW provisions rather than
%% nothing. Every protocol this one file defines maps to the SAME source
%% text (a file may define more than one protocol) — `compile.rs`'s decode
%% side re-parses each entry and only keeps the definition matching its own
%% key, so an entry irrelevant to a given user's `uses:` line is simply
%% never matched, not a correctness risk.
-spec build_protocol_sources_map([atom()], string()) -> #{binary() => binary()}.
build_protocol_sources_map(ProtocolAtoms, Source) ->
    SourceBin = unicode:characters_to_binary(Source),
    maps:from_list([
        {atom_to_binary(ProtocolAtom, utf8), SourceBin}
     || ProtocolAtom <- ProtocolAtoms
    ]).

%% Best-effort warning for every fan-out user this reload will leave on its
%% old code because it has no workspace-tracked source (ADR 0127 §11's "users
%% without workspace source keep their old code" allowance). Never fails the
%% reload — a class with no source to recompile from was ALREADY going to
%% keep running its old (still perfectly valid) code either way.
-spec warn_skipped_fanout_users([atom()], [binary()]) -> ok.
warn_skipped_fanout_users(_ProtocolAtoms, []) ->
    ok;
warn_skipped_fanout_users(ProtocolAtoms, SkippedNameBins) ->
    ?LOG_WARNING(
        "Protocol reload: ~p user(s) of ~p have no workspace-tracked source and keep their old code",
        [length(SkippedNameBins), ProtocolAtoms],
        #{
            domain => [beamtalk, runtime],
            skipped_users => SkippedNameBins,
            protocols => ProtocolAtoms
        }
    ).

%% Stage 1: compile (never install) every fan-out target from its own
%% tracked source, with `protocol_sources` context. All-or-nothing — a
%% single user's compile failure rejects the whole batch, mirroring
%% `validate_rewrite_groups/1`'s identical shape for the multi-site rewrite
%% protocol. `BatchOpts` is the `load_project` batch's compile option (or
%% `#{}`), merged into every recompile — see `reload_class_file/3`.
-spec compile_fanout_users([atom()], #{binary() => binary()}, map()) ->
    {ok, [{atom(), string(), term()}]} | {error, [{atom(), term()}]}.
compile_fanout_users(TargetAtoms, ProtocolSources, BatchOpts) ->
    Results = [
        {ClassAtom, compile_fanout_user(ClassAtom, ProtocolSources, BatchOpts)}
     || ClassAtom <- TargetAtoms
    ],
    case [{ClassAtom, Reason} || {ClassAtom, {_Path, {error, Reason}}} <- Results] of
        [] -> {ok, [{ClassAtom, Path, Compiled} || {ClassAtom, {Path, Compiled}} <- Results]};
        Failures -> {error, Failures}
    end.

%% Recompile one fan-out target from its OWN tracked source (never the just-
%% edited protocol's) — the class's own body did not change; only what its
%% `uses:` line flattens in did.
-spec compile_fanout_user(atom(), #{binary() => binary()}, map()) ->
    {string(), {ok, compiled, binary(), [map()], atom()} | {error, term()}}.
compile_fanout_user(ClassAtom, ProtocolSources, BatchOpts) ->
    ClassNameBin = beamtalk_repl_loader:normalize_class_source_key(ClassAtom),
    %% Present by construction: `discover_fanout_targets/1` only lists a
    %% class here after confirming `get_class_source/1` returns a value.
    Source = beamtalk_workspace_meta:get_class_source(ClassNameBin),
    SourceFileBin = beamtalk_repl_loader:class_source_file(ClassNameBin),
    Path =
        case SourceFileBin of
            nil -> "";
            _ -> binary_to_list(SourceFileBin)
        end,
    ModuleNameOverride = beamtalk_repl_loader:compute_package_module_name(Path),
    PrebuiltIndexes = maps:merge(
        beamtalk_repl_compiler:build_class_indexes(),
        BatchOpts#{protocol_sources => ProtocolSources}
    ),
    Compiled = beamtalk_repl_loader:compile_reload_source(
        Source, Path, ModuleNameOverride, ClassAtom, PrebuiltIndexes
    ),
    {Path, Compiled}.

%% Stage 2: install the protocol, then every already-compiled fan-out user,
%% in order — rolling every module this call installed back to its previous
%% code if any install fails partway through. See `reload_protocol_fanout/3`'s
%% own doc for the full contract.
%%
%% "Previous binary" here means recompiled-and-reinstalled from the
%% previously-tracked SOURCE, not a cached raw BEAM binary —
%% `code:get_object_code/1` (the obvious candidate) only finds a module via
%% the code-path SEARCH mechanism, which a Beamtalk class's dynamically
%% `code:load_binary/3`-installed module was never placed on; it reliably
%% returns `error` for every class this REPL ever loads. Recompiling from
%% `beamtalk_workspace_meta`'s already-tracked previous source (the same
%% source of truth `class_source_unchanged/1` already trusts elsewhere in
%% this module) is the mechanism that actually works here, and produces the
%% exact same observable code the module had before this reload — since
%% nothing about a user's own `.bt` text changes in a protocol reload, only
%% what its `uses:` line flattens in, "recompile the user against the OLD
%% protocol source" and "restore the user's previous binary" are the same
%% operation.
-spec install_protocol_fanout(
    {ok, protocol_definition, map()}, string(), string(), [{atom(), string(), term()}]
) -> {ok, [map()]} | {error, #beamtalk_error{}}.
install_protocol_fanout(
    {ok, protocol_definition, ProtocolInfo} = ProtocolResult, Path, Source, CompiledUsers
) ->
    #{protocols := ProtocolNameBins} = ProtocolInfo,
    %% Snapshot BEFORE install — `track_protocol_source/2` below overwrites
    %% these the moment the new protocol module goes live.
    OldProtocolSources = maps:from_list([
        {NameBin, beamtalk_workspace_meta:get_protocol_source(NameBin)}
     || NameBin <- ProtocolNameBins
    ]),
    %% A remote call into `beamtalk_repl_loader` (where `install_reload_result/2`
    %% lives) so this install can be intercepted by `meck` — see
    %% `beamtalk_repl_loader_tests.erl`'s
    %% `t_protocol_reload_rollback_merges_ambient_protocol_sources`.
    case beamtalk_repl_loader:install_reload_result(ProtocolResult, Path) of
        {error, Reason} ->
            {error, protocol_reload_install_failed_error(protocol, Reason)};
        {ok, ProtocolClassNames} ->
            %% Now that the NEW protocol module is actually live, this
            %% reload's own edit becomes the tracked source for any LATER
            %% cross-file `uses:` — see `track_protocol_source/2`'s doc.
            beamtalk_repl_loader:track_protocol_source(ProtocolInfo, Source, Path),
            ProtocolInstalled = {protocol, Path, ProtocolClassNames},
            case install_fanout_users(CompiledUsers, [ProtocolInstalled]) of
                {ok, InstalledRev} ->
                    AllClassNames = lists:flatmap(
                        fun({_Who, _Path2, ClassNames}) -> ClassNames end,
                        lists:reverse(InstalledRev)
                    ),
                    {ok, AllClassNames};
                {error, FailedWho, Reason, InstalledRev} ->
                    rollback_fanout_installs(InstalledRev, OldProtocolSources),
                    {error, protocol_reload_install_failed_error(FailedWho, Reason)}
            end
    end.

-spec install_fanout_users([{atom(), string(), term()}], [tuple()]) ->
    {ok, [tuple()]} | {error, atom(), term(), [tuple()]}.
install_fanout_users([], InstalledRev) ->
    {ok, InstalledRev};
install_fanout_users(
    [{ClassAtom, Path, {ok, compiled, Binary, ClassNames, ModuleName}} | Rest], InstalledRev
) ->
    %% Remote call into the loader — see `install_protocol_fanout/4`'s own call for why.
    case
        beamtalk_repl_loader:install_reload_result(
            {ok, compiled, Binary, ClassNames, ModuleName}, Path
        )
    of
        {ok, InstalledClassNames} ->
            Installed = {ClassAtom, Path, InstalledClassNames},
            install_fanout_users(Rest, [Installed | InstalledRev]);
        {error, Reason} ->
            {error, ClassAtom, Reason, InstalledRev}
    end.

%% Roll back every module in `InstalledRev` (oldest-installed last, since
%% each was prepended as it installed), most-recently-installed first — an
%% arbitrary but deterministic choice, since these installs are independent
%% modules with no ordering dependency on one another once each is captured.
-spec rollback_fanout_installs([tuple()], #{binary() => string() | undefined}) -> ok.
rollback_fanout_installs(InstalledRev, OldProtocolSources) ->
    lists:foreach(
        fun(Installed) -> rollback_one_install(Installed, OldProtocolSources) end,
        InstalledRev
    ).

%% Roll the just-installed protocol module back to whichever of its
%% (possibly several) protocol names had a previously-tracked source. A name
%% with `undefined` (this reload was its first-ever load) has no "before"
%% state to restore — left on the code this call just installed, same
%% accepted-limitation shape this module's other rollback paths already
%% document for a first-ever install.
-spec rollback_one_install(tuple(), #{binary() => string() | undefined}) -> ok.
rollback_one_install({protocol, Path, _ClassNames}, OldProtocolSources) ->
    lists:foreach(
        fun
            ({_NameBin, undefined}) ->
                ok;
            ({_NameBin, OldSource}) ->
                ModuleNameOverride = beamtalk_repl_loader:compute_package_module_name(Path),
                case
                    beamtalk_repl_loader:compile_reload_source(
                        OldSource, Path, ModuleNameOverride, undefined
                    )
                of
                    {ok, protocol_definition, OldProtocolInfo} ->
                        case
                            beamtalk_repl_loader:install_reload_result(
                                {ok, protocol_definition, OldProtocolInfo}, Path
                            )
                        of
                            {ok, _} ->
                                beamtalk_repl_loader:track_protocol_source(
                                    OldProtocolInfo, OldSource, Path
                                );
                            {error, Reason} ->
                                log_rollback_failure(protocol, Reason)
                        end;
                    {error, Reason} ->
                        log_rollback_failure(protocol, Reason)
                end
        end,
        maps:to_list(OldProtocolSources)
    );
rollback_one_install({ClassAtom, Path, _ClassNames}, OldProtocolSources) ->
    ClassNameBin = beamtalk_repl_loader:normalize_class_source_key(ClassAtom),
    case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
        undefined ->
            %% Source vanished from tracking mid-batch (a concurrent
            %% `removeFromSystem`) — nothing sound to recompile from; leave
            %% the just-installed module as-is, same "no before state"
            %% shape as an `undefined` protocol source above.
            ok;
        UserSource ->
            ModuleNameOverride = beamtalk_repl_loader:compute_package_module_name(Path),
            %% Merge over the ambient snapshot the same way the forward-compile
            %% path (`reload_protocol_fanout/3`) does — `OldProtocolSources`
            %% only carries the name(s) *this reloaded file* defines, so a
            %% user `uses:`ing a second, unrelated protocol would otherwise
            %% fail this rollback recompile with an "unknown protocol"
            %% diagnostic, leaving it stuck on its new (post-reload) code
            %% while its siblings correctly roll back — exactly the
            %% half-applied state the two-stage design exists to prevent.
            PrebuiltIndexes = maps:merge(
                beamtalk_repl_compiler:build_class_indexes(),
                #{
                    protocol_sources => maps:merge(
                        beamtalk_workspace_meta:all_protocol_sources(),
                        filter_defined(OldProtocolSources)
                    )
                }
            ),
            case
                beamtalk_repl_loader:compile_reload_source(
                    UserSource, Path, ModuleNameOverride, ClassAtom, PrebuiltIndexes
                )
            of
                {ok, compiled, Binary, ClassNames2, ModuleName2} ->
                    case
                        beamtalk_repl_loader:install_reload_result(
                            {ok, compiled, Binary, ClassNames2, ModuleName2}, Path
                        )
                    of
                        {ok, _} -> ok;
                        {error, Reason} -> log_rollback_failure(ClassAtom, Reason)
                    end;
                {error, Reason} ->
                    log_rollback_failure(ClassAtom, Reason)
            end
    end.

%% Drop `undefined` entries (a protocol never tracked before this reload)
%% before handing a source map to a compile's `protocol_sources` option,
%% which expects only real source text per entry.
-spec filter_defined(#{binary() => string() | undefined}) -> #{binary() => binary()}.
filter_defined(Sources) ->
    maps:from_list([
        {NameBin, unicode:characters_to_binary(Source)}
     || {NameBin, Source} <- maps:to_list(Sources), Source =/= undefined
    ]).

%% Best-effort logging for a rollback step that itself failed — see
%% `rollback_signature_generation/4`'s identical "a rollback failure must
%% never surface as the user-visible error" contract elsewhere in this
%% module. `Who` is left on whatever code this reload most recently
%% installed for it; a further-degraded image from here is the same accepted
%% residual this module's other rollback paths already carry.
-spec log_rollback_failure(protocol | atom(), term()) -> ok.
log_rollback_failure(Who, Reason) ->
    ?LOG_ERROR(
        "Failed to roll back ~p to its previous source after a partial protocol-reload install failure",
        [Who],
        #{domain => [beamtalk, runtime], who => Who, reason => Reason}
    ),
    ok.

%% Structured refusal for reloading a stdlib protocol file — mirrors
%% `beamtalk_repl_eval:stdlib_method_read_only_error/2`'s class-level
%% message/hint shape for the same reason (built-in protocols ship with the
%% standard library; edit them in the source tree and rebuild).
-spec stdlib_protocol_reload_read_only_error(map()) -> #beamtalk_error{}.
stdlib_protocol_reload_read_only_error(#{protocols := ProtocolNameBins}) ->
    NamesBin = iolist_to_binary(lists:join(<<", ">>, ProtocolNameBins)),
    %% `Class` here is nominal only (`beamtalk_error:new/2` requires an
    %% atom) — a protocol reload has no single class this error is "about"
    %% the way a method patch is about the class it targets, so the atom
    %% chosen carries no further meaning beyond satisfying the API.
    Err0 = beamtalk_error:new(runtime_error, 'Protocol'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            <<"Cannot reload stdlib protocol '">>,
            NamesBin,
            <<"': built-in protocols are read-only in the workspace">>
        ])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Built-in (stdlib) protocols ship with the standard library; edit them in the Beamtalk source tree and rebuild, not from the workspace.">>
    ).

%% Structured rejection for a stage-1 compile failure in the fan-out: names
%% every failing user with its own diagnostic, so a rejected reload's error
%% message satisfies the ADR's "report every failing user with its
%% diagnostic and fix" requirement directly, without the caller needing to
%% unpack a raw `Failures` list itself.
-spec protocol_reload_rejected_error([atom()], [{atom(), term()}]) -> #beamtalk_error{}.
protocol_reload_rejected_error(ProtocolAtoms, Failures) ->
    ProtocolNamesBin = iolist_to_binary(
        lists:join(<<", ">>, [atom_to_binary(P, utf8) || P <- ProtocolAtoms])
    ),
    FailureLines = [format_fanout_failure(ClassAtom, Reason) || {ClassAtom, Reason} <- Failures],
    Err0 = beamtalk_error:new(runtime_error, 'Protocol'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            <<"Protocol reload rejected for '">>,
            ProtocolNamesBin,
            <<"': ">>,
            integer_to_binary(length(Failures)),
            <<" user(s) failed to recompile; nothing was loaded:\n">>,
            lists:join(<<"\n">>, FailureLines)
        ])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Fix every listed user's compile error and reload the protocol again; none of this edit's classes were installed.">>
    ).

-spec format_fanout_failure(atom(), term()) -> binary().
format_fanout_failure(ClassAtom, Reason) ->
    #beamtalk_error{message = TaggedMsg} = beamtalk_repl_errors:ensure_structured_error(
        Reason
    ),
    Msg = beamtalk_text:to_binary(TaggedMsg),
    iolist_to_binary([<<"  - ">>, atom_to_binary(ClassAtom, utf8), <<": ">>, Msg]).

%% Structured failure for a (rare) stage-2 install failure — `Who` is either
%% `protocol` or the failing user's class name atom.
-spec protocol_reload_install_failed_error(protocol | atom(), term()) -> #beamtalk_error{}.
protocol_reload_install_failed_error(Who, Reason) ->
    WhoBin =
        case Who of
            protocol -> <<"the protocol module">>;
            ClassAtom -> atom_to_binary(ClassAtom, utf8)
        end,
    #beamtalk_error{message = TaggedReason} = beamtalk_repl_errors:ensure_structured_error(
        Reason
    ),
    ReasonMsg = beamtalk_text:to_binary(TaggedReason),
    Err0 = beamtalk_error:new(runtime_error, 'Protocol'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            <<"Protocol reload failed while installing ">>,
            WhoBin,
            <<": ">>,
            ReasonMsg,
            <<"; every module this reload already installed was rolled back to its previous code.">>
        ])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"This is unexpected; every failing module already compiled successfully. Please retry; if it persists, file a bug.">>
    ).

%%% ----------------------------------------------------------------------------
%%% Live protocol patching (ADR 0127 §11, BT-3630)
%%% ----------------------------------------------------------------------------

-doc """
True iff `NameBin` names a registered protocol (ADR 0068/0127) — the routing
test `reload_method_definition/4`, `install_method/9` and `remove_method/3`
share to send a protocol target through the two-stage protocol edit below
instead of the class recompile path. Uses `safe_to_existing_atom/1`, so an
arbitrary user-typed name never mints an atom.
""".
-spec is_protocol_name(binary()) -> boolean().
is_protocol_name(NameBin) when is_binary(NameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(NameBin) of
        {ok, Name} -> beamtalk_protocol_registry:is_protocol(Name);
        {error, _} -> false
    end.

-doc """
Live-patch one provision of protocol `ProtocolBin` (`P >> sel => …` at the REPL,
IDE save, MCP `save_method`) — ADR 0127 §11.

Adds `MethodSource` as a provision of the protocol's tracked source, or
replaces the same-selector one, then runs the WHOLE edited source through
`apply_protocol_edit/5`: the same two-stage compile-everything-then-install
fan-out a protocol file reload uses (`reload_protocol_fanout/3`), so a patch a
user cannot accept (a conflict, an unmet requirement, a self-send outside the
required/provided set) is rejected as a whole and nothing changes. On success
it is recorded in the ChangeLog against the PROTOCOL (`class` = the protocol
name, resolved to the protocol's file — never a user class's file) so
`Workspace flush` splices it into the protocol file (ADR 0113).

Refuses stdlib protocols (read-only, like stdlib classes), class-side
provisions (not supported in v1, ADR 0127 §13), and a protocol with no tracked
source (defined inline at the REPL rather than loaded from a file).
""".
-spec patch_protocol_provision(
    binary(),
    binary(),
    binary(),
    map(),
    [beamtalk_repl_protocol:warning()],
    beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
patch_protocol_provision(ProtocolBin, SelectorBin, MethodSource, Opts, Warnings, State) ->
    case patch_protocol_provision_edit(ProtocolBin, SelectorBin, MethodSource, Opts) of
        ok ->
            Intent = maps:get(intent, Opts, durable),
            ?ENTRIES:emit_change_entry(#{
                class_name => ProtocolBin,
                selector => SelectorBin,
                is_class_method => false,
                method_source => MethodSource,
                intent => Intent,
                author => maps:get(author, Opts, <<"repl">>),
                author_kind => maps:get(author_kind, Opts, human)
            }),
            beamtalk_repl_loader:maybe_autoflush(Intent),
            Result = <<ProtocolBin/binary, ">>", SelectorBin/binary>>,
            {ok, Result, <<>>, Warnings, State};
        {error, Reason} ->
            {error, Reason, <<>>, Warnings, State}
    end.

-spec patch_protocol_provision_edit(binary(), binary(), binary(), map()) -> ok | {error, term()}.
patch_protocol_provision_edit(ProtocolBin, SelectorBin, MethodSource, Opts) ->
    case maps:get(is_class_method, Opts, false) of
        true ->
            {error, protocol_class_side_provision_error(ProtocolBin, SelectorBin)};
        false ->
            case protocol_edit_target(ProtocolBin, SelectorBin, <<"recompile">>) of
                {error, _} = Err ->
                    Err;
                {ok, Path, Source} ->
                    case patched_protocol_source(Source, ProtocolBin, SelectorBin, MethodSource) of
                        {error, _} = Err ->
                            Err;
                        {ok, NewSource} ->
                            Captures = protocol_user_captures(ProtocolBin, fun(UserBin) ->
                                beamtalk_repl_loader_recheck:capture_signature_generation(#{
                                    class_name => UserBin,
                                    selector => SelectorBin,
                                    is_class_method => false,
                                    return_type => maps:get(return_type, Opts, <<"Dynamic">>),
                                    param_types => maps:get(param_types, Opts, [])
                                })
                            end),
                            apply_protocol_edit(
                                ProtocolBin, Path, NewSource, Captures, SelectorBin
                            )
                    end
            end
    end.

-doc """
Remove provision `SelectorBin` from protocol `ProtocolBin` (`P removeSelector:
#sel`, ADR 0112 / ADR 0127 §11) and re-expand every user through the two-stage
fan-out of `apply_protocol_edit/5`. Rejected — nothing changes — when a user can
no longer compile without it (e.g. it relied on the provision to satisfy
another protocol's requirement, §5). The caller (`removeSelector:`'s primitive)
records the `remove-method` ChangeLog entry, exactly as for a class method.
Only the instance side exists for provisions.
""".
-spec remove_protocol_provision(binary(), binary(), instance | class) ->
    {ok, binary()} | {error, term()}.
remove_protocol_provision(ProtocolBin, SelectorBin, class) ->
    {error, protocol_class_side_provision_error(ProtocolBin, SelectorBin)};
remove_protocol_provision(ProtocolBin, SelectorBin, instance) ->
    case protocol_edit_target(ProtocolBin, SelectorBin, <<"remove">>) of
        {error, _} = Err ->
            Err;
        {ok, Path, Source} ->
            SourceBin = unicode:characters_to_binary(Source),
            case
                beamtalk_compiler:resolve_method_span(
                    SourceBin, ProtocolBin, SelectorBin, instance
                )
            of
                {ok, Span, _Body} ->
                    NewSource = unicode:characters_to_list(
                        beamtalk_repl_loader:splice_out_span(SourceBin, Span)
                    ),
                    Captures = protocol_user_captures(ProtocolBin, fun(UserBin) ->
                        beamtalk_repl_loader:capture_signature_removal(
                            UserBin, SelectorBin, instance
                        )
                    end),
                    case apply_protocol_edit(ProtocolBin, Path, NewSource, Captures, SelectorBin) of
                        ok -> {ok, ProtocolBin};
                        {error, _} = Err -> Err
                    end;
                {error, Reason, _Msg} ->
                    {error, {method_not_found, Reason}}
            end
    end.

%% The protocol's tracked source + file path, or the structured refusal. `Verb`
%% (`recompile` | `remove`) only shapes the stdlib refusal message.
-spec protocol_edit_target(binary(), binary(), binary()) ->
    {ok, string(), string()} | {error, term()}.
protocol_edit_target(ProtocolBin, SelectorBin, Verb) ->
    Path =
        case beamtalk_repl_loader:class_source_file(ProtocolBin) of
            nil -> "";
            FileBin -> binary_to_list(FileBin)
        end,
    case beamtalk_workspace_meta:get_protocol_source(ProtocolBin) of
        undefined ->
            case ?ENTRIES:no_source_reason(ProtocolBin) of
                <<"stdlib">> ->
                    {error, stdlib_protocol_patch_read_only_error(ProtocolBin, SelectorBin, Verb)};
                _ ->
                    {error,
                        {compile_error,
                            <<"Protocol source not available for ", ProtocolBin/binary,
                                " (it must be loaded from a file to be patched)">>}}
            end;
        Source ->
            case beamtalk_repl_loader:is_stdlib_path(Path) of
                true ->
                    {error, stdlib_protocol_patch_read_only_error(ProtocolBin, SelectorBin, Verb)};
                false ->
                    {ok, Path, Source}
            end
    end.

%% Splice `MethodSource` into the protocol's `Source` (as text): replace the
%% same-selector provision in place, or append after a blank line — the same
%% convention `Workspace flush` uses for a brand-new method. The result is
%% only ever a candidate: `apply_protocol_edit/5` compiles it before anything
%% is installed, and the selector is re-resolved on it here so a body declaring
%% a different selector than asked for is rejected loudly.
-spec patched_protocol_source(string(), binary(), binary(), binary()) ->
    {ok, string()} | {error, term()}.
patched_protocol_source(Source, ProtocolBin, SelectorBin, MethodSource) ->
    SourceBin = unicode:characters_to_binary(Source),
    {Span, Indent, PrevSource} =
        case beamtalk_compiler:resolve_method_span(SourceBin, ProtocolBin, SelectorBin, instance) of
            {ok, S, Prev} -> {S, beamtalk_workspace_reshape:leading_ws(Prev), Prev};
            {error, _, _} -> {undefined, ?ENTRIES:sibling_method_indent(SourceBin), undefined}
        end,
    case beamtalk_compiler:reindent_method_source(MethodSource, Indent) of
        {ok, Reindented} ->
            NewBin =
                case Span of
                    undefined ->
                        Trimmed = beamtalk_workspace_reshape:strip_trailing_newlines(SourceBin),
                        Added = beamtalk_workspace_reshape:ensure_trailing_newline(Reindented),
                        <<Trimmed/binary, "\n\n", Added/binary>>;
                    _ ->
                        beamtalk_repl_loader:splice_replace(
                            SourceBin, Span, ?ENTRIES:match_trailing_newline(Reindented, PrevSource)
                        )
                end,
            case
                beamtalk_compiler:resolve_method_span(NewBin, ProtocolBin, SelectorBin, instance)
            of
                {ok, _, _} ->
                    {ok, unicode:characters_to_list(NewBin)};
                {error, _, _} ->
                    {error,
                        {compile_error,
                            <<"Method selector mismatch: asked to patch '", SelectorBin/binary,
                                "' but the source defines a different selector">>}}
            end;
        {error, _Reason, Msg} ->
            {error, {compile_error, Msg}}
    end.

%% Compile the edited protocol source and run it through the atomic fan-out
%% (`reload_protocol_fanout/3`: compile protocol + every source-backed user, and
%% install only if all compiled; rollback on a partial install failure).
%% `Captures` are ADR 0105 signature-generation captures taken BEFORE the
%% install for every user (see `protocol_user_captures/2`): on success each
%% user's known dependents are re-checked exactly as for a class method patch —
%% `activate_module/4`'s own shape re-check only covers field-shape changes, so
%% this is what makes a changed/removed provision's signature re-check the
%% senders of that selector on every user; on failure each capture is undone.
-spec apply_protocol_edit(
    binary(),
    string(),
    string(),
    [{binary(), beamtalk_repl_loader_recheck:capture_outcome()}],
    binary()
) -> ok | {error, term()}.
apply_protocol_edit(ProtocolBin, Path, NewSource, Captures, SelectorBin) ->
    ModuleNameOverride = beamtalk_repl_loader:compute_package_module_name(Path),
    PrebuiltIndexes = beamtalk_repl_loader:with_ambient_protocol_sources(
        beamtalk_repl_compiler:build_class_indexes()
    ),
    Result =
        case
            beamtalk_repl_loader:compile_reload_source(
                NewSource, Path, ModuleNameOverride, undefined, PrebuiltIndexes
            )
        of
            {ok, protocol_definition, _Info} = ProtocolResult ->
                reload_protocol_fanout(ProtocolResult, Path, NewSource);
            {ok, _Tag, _, _, _} ->
                {error,
                    {compile_error,
                        <<"Edited source for ", ProtocolBin/binary,
                            " no longer defines a protocol">>}};
            {error, _} = Err ->
                Err
        end,
    case Result of
        {ok, _ClassNames} ->
            lists:foreach(
                fun({UserBin, Capture}) ->
                    beamtalk_repl_loader_recheck:maybe_trigger_recheck(
                        UserBin, SelectorBin, instance, Capture
                    )
                end,
                Captures
            );
        {error, Reason} ->
            lists:foreach(
                fun({UserBin, Capture}) ->
                    beamtalk_repl_loader_recheck:rollback_signature_generation(
                        UserBin, SelectorBin, instance, Capture
                    )
                end,
                Captures
            ),
            {error, Reason}
    end.

%% `{UserClassBin, Capture}` for every loaded, source-backed user of the
%% protocol — the same set `discover_fanout_targets/1` will recompile — where
%% `Capture` is `CaptureFun(UserClassBin)`.
-spec protocol_user_captures(binary(), fun(
    (binary()) -> beamtalk_repl_loader_recheck:capture_outcome()
)) ->
    [{binary(), beamtalk_repl_loader_recheck:capture_outcome()}].
protocol_user_captures(ProtocolBin, CaptureFun) ->
    case beamtalk_repl_server:safe_to_existing_atom(ProtocolBin) of
        {ok, Protocol} ->
            {Targets, _Skipped} = discover_fanout_targets([Protocol]),
            [
                {UserBin, CaptureFun(UserBin)}
             || User <- Targets, UserBin <- [beamtalk_repl_loader:normalize_class_source_key(User)]
            ];
        {error, _} ->
            []
    end.

-spec protocol_class_side_provision_error(binary(), binary()) -> #beamtalk_error{}.
protocol_class_side_provision_error(ProtocolBin, SelectorBin) ->
    Err0 = beamtalk_error:new(runtime_error, 'Protocol'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            <<"Cannot patch class-side '">>,
            SelectorBin,
            <<"' on protocol '">>,
            ProtocolBin,
            <<"': protocols provide instance-side methods only">>
        ])
    ),
    beamtalk_error:with_hint(
        Err1, <<"Class-side provisions are not supported yet (ADR 0127 section 13).">>
    ).

%% Structured refusal for a live patch/removal against a stdlib protocol —
%% the message ADR 0127 §11 approved, mirroring
%% `beamtalk_repl_eval:stdlib_method_read_only_error/2`'s class-level one.
-spec stdlib_protocol_patch_read_only_error(binary(), binary(), binary()) -> #beamtalk_error{}.
stdlib_protocol_patch_read_only_error(ProtocolBin, SelectorBin, Verb) ->
    Err0 = beamtalk_error:new(runtime_error, 'Protocol'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            <<"Cannot ">>,
            Verb,
            <<" '">>,
            SelectorBin,
            <<"' on stdlib protocol '">>,
            ProtocolBin,
            <<"': built-in protocols are read-only in the workspace">>
        ])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Built-in (stdlib) protocols ship with the standard library; edit them in the Beamtalk source tree and rebuild, not from the workspace.">>
    ).
