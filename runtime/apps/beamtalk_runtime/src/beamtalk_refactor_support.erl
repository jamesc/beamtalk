%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_refactor_support).

%%% **DDD Context:** Object System Context

-moduledoc """
Shared guards and change-log helpers for the live-refactoring primitives.

Leaf module: depends on nothing in `beamtalk_behaviour_intrinsics`,
`beamtalk_class_refactor` or `beamtalk_selector_refactor`, all of which call
into it. It holds only what two or more of those modules need:

* reentrancy guards: `refuse_in_own_class_process/2`,
  `require_class_responsive/3` (BT-4262, BT-4290);
* capability guards: `require_class_capability/3`,
  `require_method_capability/3`;
* removal change-log helpers: `has_runtime_class_fun/2`,
  `capture_class_removal_snapshot/1`, `log_class_removal/2`,
  `current_author_context/0`;
* the rename pipeline shared by class rename (`beamtalk_class_refactor`) and
  method rename (`beamtalk_selector_refactor`): `raise_if_colliding/2`,
  `rename_collision_error/3`, `rename_rewrite_failed_error/5` and
  `log_rename_change/2`, each parameterised by class-or-selector.

Extracted from `beamtalk_behaviour_intrinsics` (BT-4703); the code moved
verbatim.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    refuse_in_own_class_process/2,
    require_class_responsive/3,
    require_class_capability/3,
    require_method_capability/3,
    has_runtime_class_fun/2,
    capture_class_removal_snapshot/1,
    log_class_removal/2,
    current_author_context/0,
    %% Shared by the class-rename and method-rename pipelines (BT-4703).
    raise_if_colliding/2,
    rename_collision_error/3,
    rename_rewrite_failed_error/5,
    log_rename_change/2
]).

%% BT-4262: a primitive that changes the receiving class has to message it
%% (or stop it) to do so, which cannot happen from inside that class's own
%% process. Refuse up front, with the structured `dispatch_error` every other
%% send back to the running class raises, before any side effect: otherwise
%% `removeFromSystem` would stop the class's actors and only then fail on
%% `gen_server:stop/1`. Reached from a class method's `self removeFromSystem`
%% (the class-object protocol, BT-4252) or a block sending to its own class
%% (`beamtalk_class_dispatch:class_send_from_own_process/2`).
-spec refuse_in_own_class_process(pid(), atom()) -> ok.
refuse_in_own_class_process(ClassPid, Selector) when ClassPid =:= self() ->
    beamtalk_class_dispatch:handle_class_self_call(Selector);
refuse_in_own_class_process(_ClassPid, _Selector) ->
    ok.

%% BT-4290: `removeFromSystem` and `renameTo:` end in `gen_server:stop/1`,
%% whose timeout is `infinity`. When the class process is itself blocked
%% waiting on the caller, which `refuse_in_own_class_process/2` cannot see
%% (a block from one of its class methods, run by another class or actor
%% while the class waits on it, sends to it), the stop never returns. A
%% process blocked in `gen_server:call/3` sits in `gen:do_call/4` and
%% monitors the process it waits on, so follow that wait chain from the
%% class, through other classes and actors (bounded). If it reaches the
%% caller, ask the class to answer, bounded, before any side effect, and
%% raise a structured `timeout` if it does not (the probe guards against a
%% monitor held for another reason). A class that is merely busy is not
%% waiting on the caller, so removal waits for it as before.
%%
%% Limits, each failing before any side effect or no worse than before:
%% the walk follows every monitor of a waiting process, so a busy class
%% whose wait chain passes a process that monitors the caller for another
%% reason is probed and times out if it stays busy past the probe; and the
%% check is one-shot, so a class that starts waiting on the caller after it
%% can still hold `gen_server:stop/1` until its own class call times out
%% (60s, `beamtalk_class_dispatch:class_method_call/3`).
%%
%% `doc:` and the other primitives that message the class use a bounded
%% `gen_server:call/2` before any side effect, so they already fail rather
%% than hang.
-define(CLASS_RESPONSIVE_TIMEOUT, 5000).
-define(WAIT_CHAIN_MAX_PROCESSES, 64).

-spec require_class_responsive(pid(), atom(), atom()) -> ok.
require_class_responsive(ClassPid, ClassName, Selector) ->
    case waits_on([ClassPid], self(), #{ClassPid => true}, ?WAIT_CHAIN_MAX_PROCESSES) of
        false -> ok;
        true -> probe_class(ClassPid, ClassName, Selector)
    end.

%% Whether the processes in `Queue` are blocked in a `gen_server:call/3`
%% chain that reaches `Target`: a breadth-first walk with one visited set,
%% examining at most `Budget` processes. Local processes only.
-spec waits_on([pid()], pid(), #{pid() => true}, non_neg_integer()) -> boolean().
waits_on([], _Target, _Seen, _Budget) ->
    false;
waits_on(_Queue, _Target, _Seen, 0) ->
    false;
waits_on([Pid | Rest], Target, Seen, Budget) ->
    Callees = callees(Pid),
    case lists:member(Target, Callees) of
        true ->
            true;
        false ->
            New = [Callee || Callee <- Callees, not maps:is_key(Callee, Seen)],
            Seen1 = maps:merge(Seen, maps:from_keys(New, true)),
            waits_on(Rest ++ New, Target, Seen1, Budget - 1)
    end.

-spec callees(pid()) -> [pid()].
callees(Pid) when node(Pid) =:= node() ->
    case erlang:process_info(Pid, [current_function, monitors]) of
        [{current_function, {gen, do_call, 4}}, {monitors, Monitors}] ->
            [Callee || {process, Callee} <- Monitors, is_pid(Callee)];
        _ ->
            []
    end;
callees(_Pid) ->
    [].

-spec probe_class(pid(), atom(), atom()) -> ok.
probe_class(ClassPid, ClassName, Selector) ->
    try gen_server:call(ClassPid, class_name, ?CLASS_RESPONSIVE_TIMEOUT) of
        _ -> ok
    catch
        exit:{timeout, _} ->
            Error0 = beamtalk_error:new(timeout, ClassName, Selector),
            Error1 = beamtalk_error:with_message(
                Error0,
                iolist_to_binary([
                    <<"Class '">>,
                    atom_to_binary(ClassName, utf8),
                    <<"' is waiting on this process and did not answer within ">>,
                    integer_to_binary(?CLASS_RESPONSIVE_TIMEOUT div 1000),
                    <<" seconds; ">>,
                    atom_to_binary(Selector, utf8),
                    <<" was not started and nothing was changed">>
                ])
            ),
            beamtalk_error:raise(
                beamtalk_error:with_hint(
                    Error1,
                    <<
                        "A block from one of its class methods, run by another class or actor "
                        "while the class waits on it, cannot change that class. Change it from "
                        "outside."
                    >>
                )
            );
        exit:_ ->
            %% The class exited before or during the probe: nothing is
            %% waiting on the caller any more, so carry on and let the
            %% removal or rename report the missing class as before.
            ok
    end.

%% BT-4438: whether `ClassName` itself (not an ancestor) has an ADR 0084
%% runtime class-method fun for `Selector` -- the store dispatch reads.
-spec has_runtime_class_fun(atom(), atom()) -> boolean().
has_runtime_class_fun(ClassName, Selector) ->
    beamtalk_class_metadata:lookup_class_method_fun(ClassName, Selector) =/= error.

%% best-effort snapshot of a class's current full source and its
%% on-disk flushability classification, taken by
%% classRemoveFromSystemByName/1 before its teardown begins (see that call
%% site's comment for why the ordering matters). Routed via
%% `beamtalk_workspace_callback`. Degrades to a not-flushable/`"dynamic"`
%% snapshot on ANY failure — not just `{error, no_workspace}` (no workspace
%% running) — because unlike that helper's post-mutation logging, this
%% call runs BEFORE teardown starts: an uncaught exception here (e.g. a
%% transient `gen_server:call` failure against `beamtalk_workspace_meta`)
%% would abort the removal itself, not just degrade its audit trail. A
%% snapshot capture failure must never block the removal it is only trying to
%% describe.
-spec capture_class_removal_snapshot(binary()) -> map().
capture_class_removal_snapshot(ClassNameBin) ->
    try beamtalk_workspace_callback:capture_class_removal_snapshot(ClassNameBin) of
        Snapshot when is_map(Snapshot) ->
            Snapshot;
        {error, no_workspace} ->
            %% Expected when no workspace is running (e.g. plain runtime, no
            %% live ChangeLog) — not a bug, so no log line.
            #{flushable => false, not_flushable_reason => <<"dynamic">>}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "capture_class_removal_snapshot failed unexpectedly for ~p — logging removal as dynamic",
                [ClassNameBin],
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            #{flushable => false, not_flushable_reason => <<"dynamic">>}
    end.

%% Best-effort ChangeLog append after a successful class removal,
%% called at classRemoveFromSystemByName/1's existing success point
%% (immediately after publish_class_removed/2) with the snapshot captured
%% before teardown. Mirrors log_local_removal/3's placement and
%% self-swallowing failure handling: the removal is already complete and
%% irreversible in memory by this point, so a logging failure must never
%% surface to the caller.
-spec log_class_removal(binary(), map()) -> ok.
log_class_removal(ClassNameBin, Snapshot) ->
    {Author, AuthorKind} = current_author_context(),
    %% `{error, no_workspace}` (no workspace running) is ignored: no live
    %% ChangeLog to append to.
    _ = beamtalk_workspace_callback:emit_remove_class_change_entry(
        ClassNameBin, Snapshot, Author, AuthorKind
    ),
    ok.

%% ADR 0125 §1.5: refuse a method-level compile or workspace operation on a
%% node that cannot perform it (a release without a compiler, or any release
%% for a workspace mutation), naming the method as `Class >> selector`.
%% The subject is built lazily (only on refusal) and formats a non-atom
%% selector rather than crashing, so the primitive's own argument validation
%% still reports a bad selector on nodes where the operation is allowed.
-spec require_method_capability(beamtalk_capability:operation(), atom(), term()) -> ok.
require_method_capability(Op, ClassName, Selector) ->
    beamtalk_capability:require(Op, ClassName, fun() ->
        SelectorFormat =
            case is_atom(Selector) orelse is_binary(Selector) of
                true -> "~ts";
                false -> "~tp"
            end,
        beamtalk_error:chars_to_binary(
            io_lib:format("~ts >> " ++ SelectorFormat, [ClassName, Selector])
        )
    end).

%% `Class <Suffix>` subject for a class-level capability refusal, built lazily.
-spec require_class_capability(beamtalk_capability:operation(), atom(), iodata()) -> ok.
require_class_capability(Op, ClassName, Suffix) ->
    beamtalk_capability:require(Op, ClassName, fun() ->
        iolist_to_binary([atom_to_binary(ClassName, utf8), <<" ">>, Suffix])
    end).

%% Resolve the audit author for the current patch. ADR 0082 distinguishes
%% `human' (REPL / interactive) from `agent' (MCP `save_method' / `try_method').
%% The submission boundary stamps the kind into the process dictionary before
%% dispatching; absent that stamp (e.g. a direct REPL `compile:source:' call) we
%% default to `human'/`repl'. Returns `{Author, AuthorKind}'.
-spec current_author_context() -> {binary(), human | agent}.
current_author_context() ->
    case erlang:get('$beamtalk_author_kind') of
        agent ->
            Author =
                case erlang:get('$beamtalk_author') of
                    A when is_binary(A) -> A;
                    _ -> <<"agent">>
                end,
            {Author, agent};
        _ ->
            Author =
                case erlang:get('$beamtalk_author') of
                    A when is_binary(A) -> A;
                    _ -> <<"repl">>
                end,
            {Author, human}
    end.

%%====================================================================
%% Rename pipeline shared by class rename and method rename (ADR 0114)
%%====================================================================

%% Collision refusal (ADR 0114 § Decision): raises the error `MkError`
%% builds when the rename target already exists, otherwise `ok`. `MkError` is
%% a fun so the error (and anything it looks up) is only built on a collision.
-spec raise_if_colliding(boolean(), fun(() -> #beamtalk_error{})) -> ok.
raise_if_colliding(false, _MkError) ->
    ok;
raise_if_colliding(true, MkError) ->
    beamtalk_error:raise(MkError()).

%% `renameTo: #Existing` when `Existing` already names a loaded class
%% (`class`, `class_already_exists` — a class-identity collision), or
%% `renameSelector: #a to: #b` when `#b` is already locally defined on the
%% same side (`selector`, `selector_already_exists` — a NEW kind, not a reuse
%% of `class_already_exists`, mirroring `selector_not_found`'s selector-level
%% vs class-level split, ADR 0112). Exact hint text from ADR 0114's worked
%% examples.
-spec rename_collision_error(class | selector, atom(), atom()) -> #beamtalk_error{}.
rename_collision_error(class, OldName, NewName) ->
    OldBin = atom_to_binary(OldName, utf8),
    NewBin = atom_to_binary(NewName, utf8),
    Error0 = beamtalk_error:new(class_already_exists, OldName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"cannot rename ">>,
            OldBin,
            <<" to ">>,
            NewBin,
            <<" — "/utf8>>,
            NewBin,
            <<" already exists">>
        ])
    ),
    beamtalk_error:with_hint(Error1, <<"remove or rename the existing class first">>);
rename_collision_error(selector, ClassName, NewSelector) ->
    ClassBin = atom_to_binary(ClassName, utf8),
    NewBin = atom_to_binary(NewSelector, utf8),
    Error0 = beamtalk_error:new(selector_already_exists, ClassName, NewSelector),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            ClassBin,
            <<" already defines #">>,
            NewBin,
            <<" locally — refusing to overwrite"/utf8>>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"removeSelector: #">>,
            NewBin,
            <<" first, or choose a different target name">>
        ])
    ).

%% The `rewrite_class_sites/5` failure shared by `renameTo:` and
%% `renameSelector:to:`. `Op` names the message in the "requires a running
%% workspace" text, `Noun` (`<<"class">>` | `<<"selector">>`) the generic
%% "Could not rename ..." text, and `NoSitesMsg` the (caller-specific) text for
%% a classification with neither a definition nor any reference site.
-spec rename_rewrite_failed_error(atom(), binary(), binary(), binary(), term()) ->
    #beamtalk_error{}.
rename_rewrite_failed_error(ErrClass, Op, _Noun, _NoSitesMsg, workspace_unavailable) ->
    Error0 = beamtalk_error:new(runtime_error, ErrClass),
    beamtalk_error:with_message(
        Error0,
        <<"Workspace not available; ", Op/binary, " requires a running workspace">>
    );
rename_rewrite_failed_error(ErrClass, _Op, _Noun, NoSitesMsg, no_sites) ->
    Error0 = beamtalk_error:new(runtime_error, ErrClass),
    beamtalk_error:with_message(Error0, NoSitesMsg);
rename_rewrite_failed_error(ErrClass, _Op, Noun, _NoSitesMsg, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, ErrClass),
    Msg = beamtalk_error:chars_to_binary(
        io_lib:format("Could not rename ~ts: ~tp", [Noun, Reason])
    ),
    beamtalk_error:with_message(Error0, Msg).

%% Best-effort ChangeLog append after a successful rename (ADR 0114 §
%% ChangeLog schema): `Fields` carries the kind-specific entries
%% (`rename-class` or `rename-method`); the durable intent and author are
%% common. The rename is already live in memory by this point, so a logging
%% failure must never surface to the caller.
-spec log_rename_change(map(), map()) -> ok.
log_rename_change(Fields, RewriteResult) ->
    {Author, AuthorKind} = current_author_context(),
    Spec = Fields#{intent => durable, author => Author, author_kind => AuthorKind},
    _ = beamtalk_workspace_callback:emit_rewrite_change_entry(Spec, RewriteResult),
    ok.
