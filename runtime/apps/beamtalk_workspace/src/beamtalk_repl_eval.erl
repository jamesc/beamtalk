%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_eval).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Expression evaluation orchestration for the Beamtalk REPL

Orchestrates compilation, module loading, execution, and result processing
for REPL expressions. Delegates compilation to beamtalk_repl_compiler and
module loading to beamtalk_repl_loader (BT-863).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    do_eval/2, do_eval/3,
    do_eval_trace/2,
    do_dispatch/5,
    do_show_codegen/2,
    handle_load/2, handle_load/3,
    handle_load_source/3
]).
%% BT-845: ADR 0040 Phase 2 — stateless class reload (called via erlang:apply from beamtalk_runtime)
-export([reload_class_file/1, reload_class_file/2]).

%% BT-2598 — cockpit reload-from-disk after a content-mutating git op. Reloads a
%% reverted `.bt` file into the live image (image == disk) and repopulates the
%% workspace_meta class-source cache. Called via RPC from the LiveView client.
-export([reload_file/1]).

%% ADR 0082 Phase 1 (BT-2285) — new-class creation backing `Workspace newClass:at:'.
-export([new_class/2]).

%% ADR 0095 Phase 2 (BT-2503) — stateless evaluate-in-context for the Inspector's
%% value `evaluate:`. Called via erlang:apply from beamtalk_inspector
%% (beamtalk_runtime) so the runtime keeps no compile-time dep on beamtalk_workspace.
-export([eval_with_self/2]).

%% ADR 0082 Phase 1 (BT-2283) — stateless live method patch backing
%% `Behaviour compile:source:' / `tryCompile:source:'. Called via erlang:apply
%% from beamtalk_behaviour_intrinsics so beamtalk_runtime keeps no compile-time
%% dependency on beamtalk_workspace.
-export([compile_method/4, compile_method/6, compile_method/7]).

%% ADR 0105 Phase 3 (BT-2782) — stateless pre-save advisory precheck backing
%% `Behaviour precheckCompile:source:'. Called via erlang:apply from
%% beamtalk_behaviour_intrinsics for the same compile-time-dependency reason
%% as compile_method/4,6,7.
-export([precheck_method/4]).

%% ADR 0082 revert completeness (BT-2663/BT-2665) — remove a live method (the
%% *add* revert case) and remove a live class (new-class revert, BT-2664).
-export([remove_method/3, remove_class/1]).

%% BT-2531 — workspace binding-mutation announcement with an explicit session id.
%% Called from `beamtalk_repl_shell` for the clear / pending put/remove paths,
%% whose shell gen_server process does not carry `beamtalk_session_id` in its
%% process dictionary (that is seeded only in eval workers).
-export([announce_binding_changed/3]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    announce_binding_changed/2,
    extract_assignment/1,
    maybe_await_future/1,
    rebuild_bindings_from_steps/2,
    should_purge_module/2,
    strip_internal_bindings/1,
    inject_output/3,
    handle_class_definition/7,
    handle_method_definition/4,
    handle_protocol_definition/3,
    handle_type_alias_definition/3,
    maybe_help_for_alias/2,
    format_alias_help/2,
    wrap_load_err/3,
    normalize_method_source/2,
    resolve_entry/2,
    is_keyword_selector/1
]).
-endif.

-define(INTERNAL_REGISTRY_KEY, '__repl_actor_registry__').
-define(WORKSPACE_BINDINGS_KEY, '__workspace_user_bindings__').

-doc """
Result of evaluating an expression. The `script_exit` shape (BT-2688) carries the
status of a connected-session `Program exit: Code` so the shell can report it and
terminate the session; `ok`/`error` are the ordinary value/failure shapes.
""".
-type eval_result() ::
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {script_exit, non_neg_integer(), binary(), [binary()], beamtalk_repl_state:state()}.

-export_type([eval_result/0]).

%%% Public API

-doc "Evaluate a Beamtalk expression.".
-spec do_eval(string(), beamtalk_repl_state:state()) -> eval_result().
do_eval(Expression, State) ->
    do_eval(Expression, State, undefined).

-doc "Evaluate with optional streaming subscriber (BT-696).".
-spec do_eval(string(), beamtalk_repl_state:state(), pid() | undefined) -> eval_result().
do_eval(Expression, State, Subscriber) ->
    %% ADR 0108 Phase 8 (BT-2902): `:help <Alias>` short-circuits before
    %% compilation — see maybe_help_for_alias/2 for why the ordinary
    %% `Beamtalk help: X` eval path cannot see session-local aliases.
    case maybe_help_for_alias(Expression, State) of
        {ok, HelpText} ->
            {ok, HelpText, <<>>, [], State};
        not_found ->
            do_eval_expression(Expression, State, Subscriber)
    end.

-spec do_eval_expression(string(), beamtalk_repl_state:state(), pid() | undefined) ->
    eval_result().
do_eval_expression(Expression, State, Subscriber) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    % elp:fixme W0023 intentional atom creation
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),

    SessionBindings = beamtalk_repl_state:get_bindings(State),
    %% BT-881: Merge workspace user bindings into session bindings.
    WorkspaceUserBindings = beamtalk_workspace_interface_primitives:get_user_bindings(),
    WorkspaceOnlyBindings = maps:without(maps:keys(SessionBindings), WorkspaceUserBindings),
    Bindings0 = maps:merge(WorkspaceUserBindings, SessionBindings),
    Bindings = Bindings0#{?WORKSPACE_BINDINGS_KEY => WorkspaceOnlyBindings},

    RegistryPid = beamtalk_repl_state:get_actor_registry(State),

    %% ADR 0108 Phase 8 (BT-2902): forward this session's earlier-turn
    %% alias declarations so `::` annotations in Expression resolve them.
    KnownTypeAliasSources = beamtalk_repl_state:known_type_alias_sources(State),

    case
        beamtalk_repl_compiler:compile_expression(
            Expression, ModuleName, Bindings, KnownTypeAliasSources
        )
    of
        %% BT-571: Inline class definition
        {ok, class_definition, ClassInfo, Warnings} ->
            handle_class_definition(
                ClassInfo, Warnings, Expression, Bindings, NewState, RegistryPid, Subscriber
            );
        %% BT-571: Standalone method definition
        {ok, method_definition, MethodInfo, Warnings} ->
            handle_method_definition(MethodInfo, Warnings, Expression, NewState);
        %% BT-1612: Protocol definition
        {ok, protocol_definition, ProtocolInfo, Warnings} ->
            handle_protocol_definition(ProtocolInfo, Warnings, NewState);
        %% ADR 0108 Phase 8 (BT-2902): type alias definition
        {ok, type_alias_definition, AliasInfo, Warnings} ->
            handle_type_alias_definition(AliasInfo, Warnings, NewState);
        {ok, Binary, _ResultExpr, Warnings} ->
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    eval_loaded_module(
                        ModuleName,
                        Expression,
                        Bindings,
                        RegistryPid,
                        Subscriber,
                        Warnings,
                        NewState
                    );
                {error, Reason} ->
                    wrap_load_err(Reason, Warnings, NewState)
            end;
        {error, Reason} ->
            wrap_compile_err(Reason, NewState)
    end.

-doc """
Intercept `:help <Alias>` before compilation (ADR 0108 Phase 8, BT-2902).

`:help <Name>` is client-side sugar (`crates/beamtalk-cli/src/commands/repl/mod.rs`'s
`handle_help_topic`) for evaluating `Beamtalk help: <Name>`, ordinarily routed
through `beamtalk_interface:handle_help/1` — which only resolves *global*
`beamtalk_class_registry` entries. A REPL-session-declared type alias has no
live BEAM class or process to register there (aliases erase entirely at
resolution time — ADR 0108 Semantics), so that path can never see one.
Intercepting here, where `State` (and therefore the session's `alias_table`)
is directly available, answers a bare `Beamtalk help: <Name>` naming a known
alias without reaching the compiler/eval pipeline at all. Anything else — a
class/protocol name, or the `selector:`/`class` forms `:help` also builds —
does not match the exact-bare-identifier pattern and falls through to the
ordinary eval path unchanged.
""".
-spec maybe_help_for_alias(string(), beamtalk_repl_state:state()) -> {ok, binary()} | not_found.
maybe_help_for_alias(Expression, State) ->
    case
        re:run(Expression, "^Beamtalk help: ([A-Za-z][A-Za-z0-9_]*)$", [
            {capture, all_but_first, binary}
        ])
    of
        {match, [Name]} ->
            case maps:find(Name, beamtalk_repl_state:get_alias_table(State)) of
                {ok, Entry} -> {ok, format_alias_help(Name, Entry)};
                error -> not_found
            end;
        _ ->
            not_found
    end.

-doc """
Render `:help <Alias>` output (ADR 0108 Phase 8, BT-2902; stdlib
provenance BT-2938).

`type Name = <expansion>`, then — only when a doc comment is present — a
blank line and the indented doc text, then a blank line and
`Declared in: <declared_in>`. `declared_in` is `<<"REPL">>` for a
session-declared alias (this issue's chosen convention, flagged in the PR
description for maintainer sign-off per CLAUDE.md's REPL-output rule,
alongside the `type` declaration's own echo-value decision) or
`<<"stdlib">>` for one seeded from stdlib's own compiled declarations
(`beamtalk_repl_state:stdlib_alias_table/0`) — same sign-off flag, chosen to
match `package => 'stdlib'`'s existing convention elsewhere (e.g.
`format_stdlib_class_entry`) rather than a full `source_file` path.
""".
-spec format_alias_help(binary(), beamtalk_repl_state:alias_entry()) -> binary().
format_alias_help(Name, #{
    expansion := Expansion, doc_comment := DocComment, declared_in := DeclaredIn
}) ->
    Header = <<"type ", Name/binary, " = ", Expansion/binary>>,
    CommentBlock =
        case DocComment of
            undefined -> <<>>;
            Doc -> <<"\n\n  ", Doc/binary>>
        end,
    <<Header/binary, CommentBlock/binary, "\n\nDeclared in: ", DeclaredIn/binary>>.

-doc "Handle a `type Name = ...` declaration (ADR 0108 Phase 8, BT-2902).".
-spec handle_type_alias_definition(map(), [binary()], beamtalk_repl_state:state()) ->
    eval_result().
handle_type_alias_definition(AliasInfo, Warnings, State) ->
    #{alias_name := Name, expansion := Expansion, doc_comment := DocComment} = AliasInfo,
    Entry = #{expansion => Expansion, doc_comment => DocComment, declared_in => <<"REPL">>},
    NewState = beamtalk_repl_state:put_alias(Name, Entry, State),
    %% ADR 0108 hot-reload re-check trigger (BT-2899): keep the compiler
    %% server's ambient alias cache in sync with session state so a later
    %% `diagnostics/3` re-check round trip (`beamtalk_recheck.erl`) — and
    %% every subsequent `compile`/`compile_method` this session — resolves
    %% `::` annotations against this (possibly just-redefined) alias table.
    %% Then trigger the dependent-site re-check: a redefinition of `Name`
    %% invalidates any annotation-resolution or exhaustiveness proof
    %% computed against its previous expansion — this is a no-op query
    %% (`beamtalk_alias_xref:dependents_of/1` returns `[]`) for a brand-new
    %% alias name with no recorded dependents yet.
    beamtalk_compiler_server:register_aliases(
        beamtalk_repl_state:known_type_alias_sources(NewState)
    ),
    beamtalk_repl_loader:spawn_alias_change_recheck([Name]),
    %% REPL display-value decision (flagged in the PR description for
    %% maintainer sign-off per CLAUDE.md's REPL-output rule): echo the
    %% declared name as a plain binary, mirroring the class-declaration
    %% convention (`Actor subclass: Counter` displays `Counter`) rather
    %% than the protocol-declaration convention (`Protocol Foo defined`) —
    %% a `type` declaration introduces a *name*, closer in spirit to a
    %% class name than to a side-effecting "defined" announcement. Unlike
    %% a class name, this never needs to become an atom (no BEAM module to
    %% load under it), so the binary is used directly — `term_to_json`
    %% (`beamtalk_repl_json.erl`) renders atoms and binaries identically.
    {ok, Name, <<>>, Warnings, NewState}.

-doc """
Dispatch a class entry method (`ClassName selector [argv]`) in this connected
session (BT-2691, ADR 0099 §3 / Phase 5).

The connected-mode counterpart of run-mode's `beamtalk_script_harness:dispatch/3`:
`beamtalk run ClassName selector [args] --connect` sends a `run-entry` op, which
lands here on a session eval worker. The class is resolved from the live image,
then `beamtalk_class_dispatch:class_send/3` runs the entry on **this worker's own
synchronous call chain** — so a `Program exit: N` raised by the entry unwinds as
`{beamtalk_script_exit, N}` and is caught here exactly like the `do_eval` path,
letting the shell report the status and end the session while the shared node
stays up.

`SelectorBin` carries the trailing `:` for the arity-1 keyword (`main:`) form, in
which case `Argv` (the program arguments as a `List(String)`, i.e. a list of
UTF-8 binaries) is passed as the single method argument; a unary entry (`run`)
receives no arguments. `Subscriber` is the streaming output sink (the connecting
client) — `Console` writes reach it via the captured group leader, mirroring the
streaming `eval` path.

Returns the shared `eval_result()` shape (`{ok, …}` / `{error, …}` /
`{script_exit, …}`) so the shell's existing `eval_result` handling applies
unchanged. A class that is not loaded, or a selector the class does not
understand, is returned as a structured `#beamtalk_error{}` error rather than
raised.
""".
-spec do_dispatch(
    binary(), binary(), [binary()], pid() | undefined, beamtalk_repl_state:state()
) -> eval_result().
do_dispatch(ClassNameBin, SelectorBin, Argv, Subscriber, State) ->
    case resolve_entry(ClassNameBin, SelectorBin) of
        {ok, ClassPid, Selector} ->
            %% A unary entry takes no arguments; the arity-1 keyword form
            %% (`main:`) receives the whole argv list as its single
            %% `List(String)` argument (parity with run-mode's dispatch list).
            DispatchArgs =
                case is_keyword_selector(SelectorBin) of
                    true -> [Argv];
                    false -> []
                end,
            CaptureRef = beamtalk_io_capture:start(Subscriber),
            EvalResult =
                try beamtalk_class_dispatch:class_send(ClassPid, Selector, DispatchArgs) of
                    RawResult ->
                        case maybe_await_future(RawResult) of
                            {future_rejected, FutureReason} ->
                                FutExObj = beamtalk_exception_handler:ensure_wrapped(FutureReason),
                                {error, FutExObj, State};
                            Value ->
                                {ok, Value, State}
                        end
                catch
                    throw:{beamtalk_script_exit, Code} ->
                        %% BT-2691: `Program exit: Code` from the dispatched entry —
                        %% same connected-exit handling as the `do_eval` path.
                        {script_exit, Code, State};
                    Class:Reason:Stacktrace ->
                        CaughtExObj = beamtalk_exception_handler:ensure_wrapped(
                            Class, Reason, Stacktrace
                        ),
                        {error, {eval_error, Class, CaughtExObj}, State}
                after
                    %% Unlike the eval path there is no transient eval module to
                    %% purge — the entry runs in already-loaded class code.
                    ok
                end,
            Output = beamtalk_io_capture:stop(CaptureRef),
            inject_output(EvalResult, Output, []);
        {error, Err} ->
            {error, Err, <<>>, [], State}
    end.

%% Resolve a `(ClassName, Selector)` entry against the live image for
%% `do_dispatch/5`. Both names must already exist (the class is loaded; the
%% selector names one of its methods), so we use existing-atom lookups and never
%% grow the atom table from client-supplied strings. A missing class or selector
%% becomes a structured error the caller surfaces verbatim.
-spec resolve_entry(binary(), binary()) ->
    {ok, pid(), atom()} | {error, #beamtalk_error{}}.
resolve_entry(ClassNameBin, SelectorBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_runtime_api:whereis_class(ClassName) of
                undefined ->
                    {error, dispatch_class_not_found_error(ClassNameBin)};
                ClassPid ->
                    case beamtalk_repl_errors:safe_to_existing_atom(SelectorBin) of
                        {ok, Selector} ->
                            {ok, ClassPid, Selector};
                        {error, _} ->
                            {error, dispatch_dnu_error(ClassNameBin, SelectorBin)}
                    end
            end;
        {error, _} ->
            {error, dispatch_class_not_found_error(ClassNameBin)}
    end.

%% True when the selector is the arity-1 keyword form (`main:`), i.e. it ends in
%% a single trailing colon — the CLI validates the shape, so a non-empty binary
%% ending in `:` is the keyword entry and anything else is the unary entry.
-spec is_keyword_selector(binary()) -> boolean().
is_keyword_selector(<<>>) -> false;
is_keyword_selector(SelectorBin) -> binary:last(SelectorBin) =:= $:.

-spec dispatch_class_not_found_error(binary()) -> #beamtalk_error{}.
dispatch_class_not_found_error(ClassNameBin) ->
    Err0 = beamtalk_error:new(class_not_found, 'Program'),
    Err1 = beamtalk_error:with_message(
        Err0,
        <<"Class '", ClassNameBin/binary,
            "' is not loaded in this workspace, so it cannot be run connected.">>
    ),
    beamtalk_error:with_hint(
        Err1,
        <<
            "Load the project into the running workspace first (open it with `beamtalk repl` "
            "or start it with `beamtalk run .`), or omit `--connect` to run it in a fresh node."
        >>
    ).

-spec dispatch_dnu_error(binary(), binary()) -> #beamtalk_error{}.
dispatch_dnu_error(ClassNameBin, SelectorBin) ->
    Err0 = beamtalk_error:new(does_not_understand, 'Program'),
    beamtalk_error:with_message(
        Err0,
        <<"Class '", ClassNameBin/binary, "' has no entry method '", SelectorBin/binary, "'.">>
    ).

-doc """
Evaluate a Beamtalk expression in trace mode (BT-1238).

Returns `{ok, Steps, Output, Warnings, State}' where
`Steps = [{SourceBin, Value}]' — one entry per top-level statement —
or `{error, Reason, Output, Warnings, State}' on failure.
""".
-spec do_eval_trace(string(), beamtalk_repl_state:state()) ->
    {ok, [{binary(), term()}], binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {script_exit, non_neg_integer(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval_trace(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    % elp:fixme W0023 intentional atom creation
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),

    SessionBindings = beamtalk_repl_state:get_bindings(State),
    WorkspaceUserBindings = beamtalk_workspace_interface_primitives:get_user_bindings(),
    WorkspaceOnlyBindings = maps:without(maps:keys(SessionBindings), WorkspaceUserBindings),
    Bindings0 = maps:merge(WorkspaceUserBindings, SessionBindings),
    Bindings = Bindings0#{?WORKSPACE_BINDINGS_KEY => WorkspaceOnlyBindings},

    RegistryPid = beamtalk_repl_state:get_actor_registry(State),

    case beamtalk_repl_compiler:compile_expression_trace(Expression, ModuleName, Bindings) of
        {ok, Binary, _ResultExpr, Warnings} ->
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    BindingsWithRegistry =
                        case RegistryPid of
                            undefined -> Bindings;
                            _ -> Bindings#{?INTERNAL_REGISTRY_KEY => RegistryPid}
                        end,
                    CaptureRef = beamtalk_io_capture:start(undefined),
                    EvalResult =
                        try
                            {RawSteps, UpdatedBindings} = apply(ModuleName, eval, [
                                BindingsWithRegistry
                            ]),
                            CleanBindings = strip_internal_bindings(UpdatedBindings),
                            %% BT-1238: Await each step's value. If any future rejects, propagate
                            %% as a top-level error (consistent with the non-trace eval path).
                            AwaitedSteps = [
                                {Src, maybe_await_future(Val)}
                             || {Src, Val} <- RawSteps
                            ],
                            case
                                lists:search(
                                    fun
                                        ({_Src, {future_rejected, _}}) -> true;
                                        (_) -> false
                                    end,
                                    AwaitedSteps
                                )
                            of
                                {value, {_Src, {future_rejected, FutureReason}}} ->
                                    %% Mirror process_eval_result/4: wrap and store in '_error'.
                                    FutExObj = beamtalk_exception_handler:ensure_wrapped(
                                        FutureReason
                                    ),
                                    FutBindings = CleanBindings#{'_error' => FutExObj},
                                    ErrState = beamtalk_repl_state:set_bindings(
                                        FutBindings, NewState
                                    ),
                                    {error, FutExObj, ErrState};
                                false ->
                                    %% BT-1261: Rebuild bindings from awaited step values so that
                                    %% variable assignments whose RHS was a future are stored with
                                    %% the resolved value rather than the raw future handle.
                                    FinalBindings = rebuild_bindings_from_steps(
                                        AwaitedSteps, CleanBindings
                                    ),
                                    FinalState = beamtalk_repl_state:set_bindings(
                                        FinalBindings, NewState
                                    ),
                                    {ok, AwaitedSteps, FinalState}
                            end
                        catch
                            throw:{beamtalk_script_exit, Code} ->
                                %% BT-2688: `Program exit: Code` inside a traced eval.
                                %% Honour it like the non-trace path
                                %% (eval_loaded_module) instead of letting the generic
                                %% clause wrap it as an error, so the shell reports the
                                %% status and terminates the session consistently.
                                {script_exit, Code, NewState};
                            Class:Reason:Stacktrace ->
                                CaughtExObj = beamtalk_exception_handler:ensure_wrapped(
                                    Class, Reason, Stacktrace
                                ),
                                CaughtBindings = Bindings#{'_error' => CaughtExObj},
                                CaughtState = beamtalk_repl_state:set_bindings(
                                    CaughtBindings, NewState
                                ),
                                {error, {eval_error, Class, CaughtExObj}, CaughtState}
                        after
                            cleanup_module(ModuleName, RegistryPid)
                        end,
                    Output = beamtalk_io_capture:stop(CaptureRef),
                    case EvalResult of
                        {ok, Steps2, FinalState2} ->
                            {ok, Steps2, Output, Warnings, FinalState2};
                        {script_exit, ExitCode, ScState} ->
                            {script_exit, ExitCode, Output, Warnings, ScState};
                        {error, ErrorReason, ErrorState} ->
                            WrappedReason = beamtalk_repl_errors:ensure_structured_error(
                                ErrorReason
                            ),
                            {error, WrappedReason, Output, Warnings, ErrorState}
                    end;
                {error, Reason} ->
                    wrap_load_err(Reason, Warnings, NewState)
            end;
        {error, Reason} ->
            wrap_compile_err(Reason, NewState)
    end.

-doc "Compile a Beamtalk expression and return Core Erlang source (BT-700).".
-spec do_show_codegen(string(), beamtalk_repl_state:state()) ->
    {ok, binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), [binary()], beamtalk_repl_state:state()}.
do_show_codegen(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    SessionBindings = beamtalk_repl_state:get_bindings(State),
    WorkspaceUserBindings = beamtalk_workspace_interface_primitives:get_user_bindings(),
    Bindings = maps:merge(WorkspaceUserBindings, SessionBindings),
    SourceBin = list_to_binary(Expression),
    ModNameBin = iolist_to_binary(["beamtalk_repl_codegen_", integer_to_list(Counter)]),
    KnownVars = beamtalk_repl_compiler:known_vars(Bindings),
    case beamtalk_repl_compiler:compile_for_codegen(SourceBin, ModNameBin, KnownVars) of
        {ok, CoreErlang, Warnings} ->
            {ok, CoreErlang, Warnings, NewState};
        {error, Reason} ->
            {error, Reason, [], NewState}
    end.

-doc "Load a Beamtalk file and register its classes.".
-spec handle_load(string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State) ->
    beamtalk_repl_loader:handle_load(Path, State).

-doc "Load a Beamtalk file with pre-built class indexes (BT-1543).".
-spec handle_load(string(), beamtalk_repl_state:state(), map()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State, PrebuiltIndexes) ->
    beamtalk_repl_loader:handle_load(Path, State, PrebuiltIndexes).

-doc "Load Beamtalk source from an inline binary string (no file path).".
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    beamtalk_repl_loader:handle_load_source(SourceBin, Label, State).

-doc "Compile and load a source file without REPL session state (BT-845).".
-spec reload_class_file(string()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path) ->
    beamtalk_repl_loader:reload_class_file(Path).

-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    beamtalk_repl_loader:reload_class_file(Path, ExpectedClassName).

-doc """
Reload a `.bt` file from disk into the live image, returning the names of the
classes it defines (BT-2598).

The clean-returning entry the cockpit uses after a content-mutating git op (e.g.
`git restore -- <path>`): the on-disk working tree has just changed, so the live
image is re-installed from disk to keep image == disk (git-first / disk-as-
source-of-truth, BT-2585). The hot redefinition fires a `ClassLoaded`
announcement on the `classes` stream, so subscribed surfaces (the cockpit) refresh
their open windows.

Unlike the bare `reload_class_file/1`, this also repopulates the workspace_meta
class-source cache from the freshly-read file, mirroring the `Workspace load:`
post-step (`beamtalk_workspace_interface_primitives:handle_load_after_native/1`),
so a subsequent `Class >> selector` method patch diffs against the reverted body
rather than a stale pre-revert snapshot.

Returns `{ok, [ClassName :: binary()]}` (the reloaded class names) or
`{error, Reason}` (a structured reload error: file not found, compile failure,
etc.). `Path` is a project-relative `.bt` path (the same form git restored).
""".
-spec reload_file(string()) -> {ok, [binary()]} | {error, term()}.
reload_file(Path) ->
    case beamtalk_repl_loader:reload_class_file(Path) of
        {ok, ClassNames} ->
            repopulate_class_sources(Path, ClassNames),
            {ok, class_name_binaries(ClassNames)};
        {error, Reason} ->
            {error, Reason}
    end.

%% BT-2598: repopulate the workspace_meta class-source cache from the just-read
%% file, mirroring `handle_load_after_native/1`. A read failure is non-fatal —
%% the reload itself already succeeded — so the cache is simply left untouched.
-spec repopulate_class_sources(string(), [map()]) -> ok.
repopulate_class_sources(Path, ClassNames) ->
    case file:read_file(Path) of
        {ok, SourceBin} ->
            SourceStr = binary_to_list(SourceBin),
            lists:foreach(
                fun(Entry) ->
                    case class_name_binary(Entry) of
                        undefined -> ok;
                        Bin -> beamtalk_workspace_meta:set_class_source(Bin, SourceStr)
                    end
                end,
                ClassNames
            );
        {error, _} ->
            ok
    end.

-spec class_name_binaries([map()]) -> [binary()].
class_name_binaries(ClassNames) ->
    [Bin || Entry <- ClassNames, (Bin = class_name_binary(Entry)) =/= undefined].

%% Extract a class name as a binary from a reload payload entry, mirroring the
%% defensive shape-matching in `handle_load_after_native/1` (atom | binary |
%% string name keys). Returns `undefined` for an unrecognised shape.
-spec class_name_binary(map()) -> binary() | undefined.
class_name_binary(#{name := Atom}) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
class_name_binary(#{name := Bin}) when is_binary(Bin) -> Bin;
class_name_binary(#{name := Str}) when is_list(Str) -> list_to_binary(Str);
class_name_binary(_) -> undefined.

-doc """
Create a brand-new class from a source String at a target path (ADR 0082 Phase 1,
BT-2285). Delegates to `beamtalk_repl_loader:new_class/2`; see there for the
validation contract. Returns `{ok, [ClassObject]}` or `{error, #beamtalk_error{}}`.
""".
-spec new_class(binary() | string(), binary() | string()) ->
    {ok, [tuple()]} | {error, term()}.
new_class(Source, TargetPath) ->
    beamtalk_repl_loader:new_class(Source, TargetPath).

-doc """
Evaluate `Source` (a Beamtalk expression) with `self` bound to `Self`, returning
`{ok, Value}` or `{error, #beamtalk_error{}}` (ADR 0095 §1, BT-2503).

Stateless evaluate-in-context for the Inspector's value `evaluate:`: the
expression is compiled with `self` as a known free variable (resolved from the
bindings map in REPL codegen, BT-2503) and evaluated in this worker with
`#{self => Self}` as the only binding. No session state is touched, no workspace
bindings are merged, and the eval module is purged afterwards. Compile and runtime
failures are returned as structured `#beamtalk_error{}` — never raised — so the
Inspector lifts them to a `Result error:` at the FFI boundary.

Called via `erlang:apply` from `beamtalk_inspector` (beamtalk_runtime) so the
runtime keeps no compile-time dependency on beamtalk_workspace.
""".
-spec eval_with_self(term(), binary() | string()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
eval_with_self(Self, Source) ->
    SourceStr = unicode:characters_to_list(Source),
    %% Reuse a per-process module name (minted once, cached in the process
    %% dictionary) instead of a fresh atom per call, so a hot `evaluate:` loop in
    %% one process cannot exhaust the never-reclaimed atom table (BT-2503).
    ModuleName = eval_module_name(),
    Bindings = #{self => Self},
    %% BT-2956: use the no-registration variant — every definition-shaped
    %% result below is rejected immediately via eval_not_an_expression_error/0,
    %% so there is no reader for a beamtalk_alias_xref edge (or even a wasted
    %% bytecode compile) that compile_expression/3 would otherwise trigger.
    case
        beamtalk_repl_compiler:compile_expression_no_registration(SourceStr, ModuleName, Bindings)
    of
        {ok, class_definition, _Info, _Warnings} ->
            eval_not_an_expression_error();
        {ok, method_definition, _Info, _Warnings} ->
            eval_not_an_expression_error();
        {ok, protocol_definition, _Info, _Warnings} ->
            eval_not_an_expression_error();
        {ok, type_alias_definition, _Info, _Warnings} ->
            eval_not_an_expression_error();
        {ok, Binary, _ResultExpr, _Warnings} ->
            run_self_eval_module(ModuleName, Binary, Bindings);
        {error, Reason} ->
            {error, eval_with_self_error(Reason)}
    end.

%% The error for `evaluate:` given a class/method/protocol definition rather than
%% a value expression.
-spec eval_not_an_expression_error() -> {error, #beamtalk_error{}}.
eval_not_an_expression_error() ->
    {error,
        beamtalk_error:new(
            eval_failed,
            'Inspector',
            'evaluate:',
            <<"evaluate: expects an expression, not a class/method definition">>
        )}.

%% Load the compiled eval module, run its `eval/1` with the self-binding, and
%% purge it. Any throw/error/exit is captured into a structured error so the
%% Inspector never sees a raise.
-spec run_self_eval_module(atom(), binary(), map()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
run_self_eval_module(ModuleName, Binary, Bindings) ->
    %% Fully unload any module left in this process's recycled eval slot before
    %% loading the new one, so a prior call's code never lingers.
    purge_eval_module(ModuleName),
    case code:load_binary(ModuleName, "", Binary) of
        {module, ModuleName} ->
            try
                {RawResult, _UpdatedBindings} = apply(ModuleName, eval, [Bindings]),
                case maybe_await_future(RawResult) of
                    {future_rejected, FutureReason} ->
                        %% An awaited future that rejected/timed out is an error,
                        %% not a success — honour the structured-error contract
                        %% rather than leaking the internal `{future_rejected, _}`
                        %% tuple through `{ok, _}` (mirrors process_eval_result/4).
                        FutExObj = beamtalk_exception_handler:ensure_wrapped(FutureReason),
                        {error, beamtalk_repl_errors:ensure_structured_error(FutExObj)};
                    Value ->
                        {ok, Value}
                end
            catch
                Class:Reason:Stacktrace ->
                    ExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stacktrace),
                    {error, beamtalk_repl_errors:ensure_structured_error(ExObj)}
            after
                %% `evaluate:` is values-only and stateless, and the module name
                %% is private to this process (never shared with an actor), so the
                %% transient module is always fully unloaded.
                purge_eval_module(ModuleName)
            end;
        {error, LoadReason} ->
            {error, eval_with_self_error({load_error, LoadReason})}
    end.

%% The per-process module name for stateless `evaluate:` compilation. Minted once
%% per process and cached in the process dictionary so repeated `evaluate:` calls
%% (e.g. a tight loop in one worker) reuse a single atom rather than leaking one
%% never-reclaimed atom per call. The name is private to the calling process, so
%% recycling it is race-free: only sequential same-process calls share it, and no
%% other process can be executing its code when it is purged.
-spec eval_module_name() -> atom().
eval_module_name() ->
    case get('$beamtalk_inspector_eval_module') of
        undefined ->
            Unique = erlang:unique_integer([positive]),
            % elp:fixme W0023 one recycled atom per process, not per call
            ModuleName = list_to_atom("beamtalk_inspector_eval_" ++ integer_to_list(Unique)),
            put('$beamtalk_inspector_eval_module', ModuleName),
            ModuleName;
        ModuleName ->
            ModuleName
    end.

%% Fully unload a transient `evaluate:` module. `code:delete/1` moves the current
%% code to old; `code:purge/1` then reclaims it. The order matters: `purge` before
%% `delete` is a no-op for a module with no *old* code, so a freshly loaded module
%% would otherwise stay resident forever.
-spec purge_eval_module(atom()) -> ok.
purge_eval_module(ModuleName) ->
    code:delete(ModuleName),
    code:purge(ModuleName),
    ok.

%% Wrap a compile/load failure from `eval_with_self/2` as a structured error.
-spec eval_with_self_error(term()) -> #beamtalk_error{}.
eval_with_self_error(Reason) ->
    Message = iolist_to_binary(io_lib:format("evaluate: failed: ~tp", [Reason])),
    Err = beamtalk_error:new(eval_failed, 'Inspector', 'evaluate:'),
    beamtalk_error:with_message(Err, Message).

-doc """
Install a live method patch from a `(ClassName, Selector, Source, Intent)' tuple
without REPL session state (ADR 0082 Phase 1, BT-2283).

Equivalent to `compile_method/6' with the author defaulting to `<<"repl">>' /
`human' — used by callers (e.g. tests) that do not carry audit metadata.
""".
-spec compile_method(binary(), atom() | binary(), binary(), durable | ephemeral) ->
    {ok, binary()} | {error, term()}.
compile_method(ClassNameBin, Selector, Source, Intent) ->
    compile_method(ClassNameBin, Selector, Source, Intent, <<"repl">>, human).

-doc """
Install a live method patch, threading the caller's audit metadata
(ADR 0082 Phase 1, BT-2283).

Backs `Behaviour compile:source:' (`Intent = durable') and
`tryCompile:source:' (`Intent = ephemeral'). `Source' is the method definition
String exactly as the caller supplied it. The accepted forms are:

  - A **full method definition** that names its own parameters, for any selector
    arity: unary (`increment => body'), keyword (`at: k put: v => body'), or
    binary (`+ other => body'). This is the form tools should always send.
  - A **bare body** with no header — only meaningful for a **unary** selector,
    where the canonical `selector => ' header is prepended. A bare body cannot
    supply parameter names for a keyword/binary selector, so for those a full
    definition is required (a bare body would fail to compile).

Either way the synthesized `>>' expression (`ClassName >> definition') is
recompiled to recover the canonical MethodInfo.

`Author' / `AuthorKind' identify the caller for the ChangeLog audit trail:
agent-driven patches (MCP `save_method' / `try_method') pass `AuthorKind = agent';
the human REPL path passes `human'. They are stamped onto the MethodInfo so the
install chokepoint records them verbatim instead of defaulting to `human'.

The patch compiles and installs in memory, and the install chokepoint
(`beamtalk_repl_loader:load_recompiled_method/7') attempts (best-effort) to emit
a ChangeLog entry tagged with `Intent' and the author metadata. Returns
`{ok, ClassName}' on success or `{error, Reason}' on compile/install failure
(memory unchanged — the all-or-nothing contract for the install).
""".
-spec compile_method(
    binary(), atom() | binary(), binary(), durable | ephemeral, binary(), human | agent
) ->
    {ok, binary()} | {error, term()}.
compile_method(ClassNameBin, Selector, Source, Intent, Author, AuthorKind) ->
    compile_method(ClassNameBin, Selector, Source, Intent, Author, AuthorKind, instance).

-doc """
Install a live method patch on a chosen side (BT-2665).

`Side` is `instance` (the `compile:source:` / IDE-save default) or `class` (a
class-side / static method). Backs class-side revert re-installs, which must
recompile the class with the prior class-side body. Otherwise equivalent to
`compile_method/6`.
""".
-spec compile_method(
    binary(),
    atom() | binary(),
    binary(),
    durable | ephemeral,
    binary(),
    human | agent,
    instance | class
) ->
    {ok, binary()} | {error, term()}.
compile_method(ClassNameBin, Selector, Source, Intent, Author, AuthorKind, Side) ->
    SelectorBin = beamtalk_repl_protocol:to_binary(Selector),
    %% Obey project mode: a built-in (stdlib) class is compiled in stdlib mode and
    %% its bodies may use `@intrinsic'/`@primitive', which the workspace's
    %% project-mode recompile cannot reproduce. Refuse the patch up front with a
    %% clear error rather than attempt a recompile that cannot succeed.
    case stdlib_class_module(ClassNameBin) of
        {ok, _Module} ->
            {error, stdlib_method_read_only_error(ClassNameBin, SelectorBin)};
        none ->
            do_compile_method(ClassNameBin, SelectorBin, Source, Intent, Author, AuthorKind, Side)
    end.

-doc """
Pre-save advisory precheck (ADR 0105 Phase 3, BT-2782): compile a pending
method edit and report would-be-stale dependents without installing.

Backs `Behaviour precheckCompile:source:'. `Selector`/`Source` mirror
`compile_method/6`'s arguments (`Source` is the method body String, with the
same bare-body-vs-full-definition normalization); there is no `Intent` /
`Author` / `AuthorKind` because nothing is recorded to the ChangeLog — a
precheck is read-only. `Side` selects instance vs. class-side, matching
`compile_method/7`.

Refuses stdlib classes (their methods are read-only in the workspace) the
same way `compile_method/6,7` does. Returns `{ok, beamtalk_recheck:result()}`
or `{error, Reason}`.
""".
-spec precheck_method(binary(), atom() | binary(), binary(), instance | class) ->
    {ok, beamtalk_recheck:result()} | {error, term()}.
precheck_method(ClassNameBin, Selector, Source, Side) ->
    SelectorBin = beamtalk_repl_protocol:to_binary(Selector),
    case stdlib_class_module(ClassNameBin) of
        {ok, _Module} ->
            {error, stdlib_method_read_only_error(ClassNameBin, SelectorBin)};
        none ->
            MethodSource = normalize_method_source(SelectorBin, Source),
            IsClassMethod = (Side =:= class),
            beamtalk_repl_loader:precheck_method(
                ClassNameBin, SelectorBin, MethodSource, IsClassMethod
            )
    end.

-doc """
Remove a live method from a class (BT-2663/BT-2665 *add* revert case).

`Side` (`instance | class`) selects which side's method to drop. Recompiles the
class without the method and hot-reloads it; the live image is unchanged on error.
Returns `{ok, ClassNameBin}` or `{error, Reason}`. Refuses stdlib classes (their
methods are read-only in the workspace).
""".
-spec remove_method(binary(), atom() | binary(), instance | class) ->
    {ok, binary()} | {error, term()}.
remove_method(ClassNameBin, Selector, Side) ->
    SelectorBin = beamtalk_repl_protocol:to_binary(Selector),
    case stdlib_class_module(ClassNameBin) of
        {ok, _Module} ->
            {error, stdlib_method_read_only_error(ClassNameBin, SelectorBin)};
        none ->
            beamtalk_repl_loader:remove_method(ClassNameBin, SelectorBin, Side)
    end.

-doc """
Remove a live class from the system (BT-2664 new-class revert case).

Delegates to `beamtalk_runtime_api:remove_class_from_system/1`, which unregisters
the class, purges its module, and cleans up runtime state. Returns
`{ok, Module} | {error, #beamtalk_error{}}`.
""".
-spec remove_class(binary()) -> {ok, module()} | {error, #beamtalk_error{}}.
remove_class(ClassNameBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            beamtalk_runtime_api:remove_class_from_system(ClassName);
        {error, _} ->
            {error, remove_class_unknown_error(ClassNameBin)}
    end.

-spec remove_class_unknown_error(binary()) -> #beamtalk_error{}.
remove_class_unknown_error(ClassNameBin) ->
    Err0 = beamtalk_error:new(class_not_found, 'WorkspaceInterface'),
    beamtalk_error:with_message(
        Err0,
        <<"Cannot remove class '", ClassNameBin/binary, "': no such class is loaded">>
    ).

-spec do_compile_method(
    binary(), binary(), binary(), durable | ephemeral, binary(), human | agent, instance | class
) ->
    {ok, binary()} | {error, term()}.
do_compile_method(ClassNameBin, SelectorBin, Source, Intent, Author, AuthorKind, Side) ->
    %% `compile:source:` / MCP `save_method` pass the method BODY only (no
    %% `selector => ' header); the IDE save passes a full method definition.
    %% `normalize_method_source/2' synthesises the header for the bare-body case
    %% and leaves a full (possibly comment-led) definition untouched, so the
    %% structured install path always receives a complete method definition. This
    %% is header *synthesis*, not the old `Class >>` text-wrap — the backend then
    %% parses the result standalone, so comments/formatting still round-trip
    %% exactly and the patched class keeps its package-qualified module name +
    %% on-disk source attribution (flushable + revertable).
    MethodSource = normalize_method_source(SelectorBin, Source),
    State = beamtalk_repl_state:new(undefined, 0),
    IsClassMethod = (Side =:= class),
    case
        beamtalk_repl_loader:install_method(
            ClassNameBin,
            SelectorBin,
            MethodSource,
            Intent,
            Author,
            AuthorKind,
            [],
            State,
            IsClassMethod
        )
    of
        {ok, _Result, _Output, _W, _S} -> {ok, ClassNameBin};
        {error, Reason, _Output, _W, _S} -> {error, Reason}
    end.

%% `{ok, Module}' when `ClassNameBin' names a loaded built-in (stdlib) class,
%% `none' otherwise. Stdlib classes load from `bt@stdlib@' modules
%% (`beamtalk_class_registry:is_stdlib_module/1'); an unknown / not-yet-loaded
%% class is `none' so the normal compile path surfaces its own error.
-spec stdlib_class_module(binary()) -> {ok, module()} | none.
stdlib_class_module(ClassNameBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            try
                case beamtalk_runtime_api:whereis_class(ClassName) of
                    undefined ->
                        none;
                    Pid ->
                        Module = beamtalk_runtime_api:module_name(Pid),
                        case beamtalk_class_registry:is_stdlib_module(Module) of
                            true -> {ok, Module};
                            false -> none
                        end
                end
            catch
                %% The class process can die between whereis_class/1 and the
                %% module_name/1 gen_server:call — treat a vanished class as "not
                %% stdlib" and let the normal compile path surface its own error.
                exit:_ -> none
            end;
        {error, _} ->
            none
    end.

%% Structured rejection for a method patch against a built-in class. Mirrors the
%% stdlib-protection error raised by `classRemoveFromSystemByName' so both
%% "can't touch stdlib" refusals read consistently.
-spec stdlib_method_read_only_error(binary(), binary()) -> #beamtalk_error{}.
stdlib_method_read_only_error(ClassNameBin, SelectorBin) ->
    %% stdlib_class_module/1 found the class, so the atom already exists.
    ClassName = binary_to_existing_atom(ClassNameBin, utf8),
    Err0 = beamtalk_error:new(runtime_error, ClassName),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            <<"Cannot recompile '">>,
            SelectorBin,
            <<"' on stdlib class '">>,
            ClassNameBin,
            <<"': built-in methods are read-only in the workspace">>
        ])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Built-in (stdlib) classes ship with the standard library; edit them in the Beamtalk source tree and rebuild, not from the workspace.">>
    ).

%% A `compile:source:' / MCP `save_method' body may be a full method definition
%% or just the body (the selector is supplied separately). When it is a bare
%% body, prepend the canonical `selector => ' header so the structured install
%% path receives a complete method definition. We only treat the source as
%% already-headed when it begins with the selector token *and* that token is
%% followed by a header form — args / arg names / whitespace up to the `=>'
%% arrow. A bare expression such as `incremented + 1' for selector `increment'
%% shares no such header and is correctly prefixed.
%%
%% Leading doc comments (`///') and line comments (`//') precede the header in
%% stored method source, so we look past them before testing for a header; a
%% documented full definition is left intact. NOTE: this is header *synthesis*,
%% not the removed `Class >>` text-wrap — its output is parsed standalone by the
%% backend, so comments round-trip exactly.
-spec normalize_method_source(binary(), binary()) -> binary().
normalize_method_source(SelectorBin, Source) ->
    Body = skip_leading_comments(Source),
    Head = first_token(SelectorBin),
    case has_method_header(Body, Head) of
        true ->
            Source;
        false ->
            PrefixLen = byte_size(Source) - byte_size(Body),
            <<Prefix:PrefixLen/binary, _/binary>> = Source,
            %% A leading `//' comment with no trailing newline (a comment-only
            %% source) would otherwise glue the injected `selector => ' onto the
            %% comment line, swallowing the header. Insert a newline when the
            %% prefix ends mid-comment so the header starts fresh.
            Sep = header_separator(Prefix),
            <<Prefix/binary, Sep/binary, SelectorBin/binary, " => ", Body/binary>>
    end.

%% Separator between a consumed comment/whitespace prefix and the injected
%% `selector => ' header. Empty when the prefix already ends at a line or
%% statement boundary (newline / whitespace / no prefix); a newline only when the
%% prefix ends in comment text (a `//' comment without a trailing newline).
-spec header_separator(binary()) -> binary().
header_separator(<<>>) ->
    <<>>;
header_separator(Prefix) ->
    case binary:last(Prefix) of
        $\n -> <<>>;
        $\s -> <<>>;
        $\t -> <<>>;
        $\r -> <<>>;
        _ -> <<"\n">>
    end.

%% Skip leading whitespace and full-line `//'/`///' comments, returning the first
%% suffix of `Source' that begins a real expression or method header. `Body' is
%% always a suffix of `Source', so the consumed prefix can be recovered by length.
-spec skip_leading_comments(binary()) -> binary().
skip_leading_comments(Source) ->
    Trimmed = string:trim(Source, leading),
    case Trimmed of
        <<"//", _/binary>> ->
            case binary:split(Trimmed, <<"\n">>) of
                [_Comment, Rest] -> skip_leading_comments(Rest);
                [_Comment] -> <<>>
            end;
        _ ->
            Trimmed
    end.

%% True iff `Trimmed' starts with the selector token `Head' followed by a valid
%% method header: the token, then anything (args / arg names / whitespace), then
%% the `=>' arrow, before any other top-level statement boundary. We require both
%% the leading token *and* an arrow so a bare body that merely happens to begin
%% with the selector name (e.g. `increment + 1') is not mistaken for a header.
-spec has_method_header(binary(), binary()) -> boolean().
has_method_header(<<"class ", Rest/binary>>, Head) ->
    %% A class-side method definition leads with the `class ' modifier (e.g.
    %% `class make => ...'). Skip it and test the header on the rest — this keeps
    %% a complete class-side definition (such as a recovered prior body from a
    %% class-side revert, BT-2665) intact instead of re-wrapping it under a
    %% synthesised instance-side header.
    has_method_header(trim_leading_ws(Rest), Head);
has_method_header(Trimmed, Head) ->
    HeadSize = byte_size(Head),
    case Trimmed of
        <<Head:HeadSize/binary, Rest/binary>> ->
            %% The char right after the token must be a header delimiter, not a
            %% continuation of a longer identifier (so `increments' is not read
            %% as the `increment' header) — `:' for keyword selectors, or
            %% whitespace, or an operator/`=>' for unary/binary selectors.
            header_after_token(Rest);
        _ ->
            false
    end.

%% Drop leading spaces/tabs, returning a binary (string:trim/2 returns chardata,
%% which the binary-match clauses above cannot pattern-match against).
-spec trim_leading_ws(binary()) -> binary().
trim_leading_ws(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t -> trim_leading_ws(Rest);
trim_leading_ws(Bin) -> Bin.

-spec header_after_token(binary()) -> boolean().
header_after_token(<<>>) ->
    false;
%% Identifier continuation: not a header boundary (e.g. `increments').
header_after_token(<<C, _/binary>>) when
    (C >= $a andalso C =< $z);
    (C >= $A andalso C =< $Z);
    (C >= $0 andalso C =< $9);
    C =:= $_
->
    false;
header_after_token(Rest) ->
    %% A keyword/unary/binary header reaches the `=>' arrow before the next
    %% top-level statement separator (`.' or newline).
    binary_has_arrow_before_break(Rest).

%% True iff `Bin' contains a `=>' arrow before the first top-level statement
%% break (newline or `.'), i.e. the leading token really opens a method header.
-spec binary_has_arrow_before_break(binary()) -> boolean().
binary_has_arrow_before_break(<<"=>", _/binary>>) ->
    true;
binary_has_arrow_before_break(<<$\n, _/binary>>) ->
    false;
binary_has_arrow_before_break(<<$., _/binary>>) ->
    false;
binary_has_arrow_before_break(<<_, Rest/binary>>) ->
    binary_has_arrow_before_break(Rest);
binary_has_arrow_before_break(<<>>) ->
    false.

%% First whitespace/`:'-delimited token of a (possibly keyword) selector binary,
%% e.g. `at:put:' -> `at', `increment' -> `increment', `+' -> `+'.
-spec first_token(binary()) -> binary().
first_token(SelectorBin) ->
    case binary:split(SelectorBin, [<<":">>, <<" ">>]) of
        [Head | _] when byte_size(Head) > 0 -> Head;
        _ -> SelectorBin
    end.

%%% Internal functions

-doc "Handle inline class definition: load class module, eval trailing expressions.".
-spec handle_class_definition(
    map(),
    [binary()],
    string(),
    map(),
    beamtalk_repl_state:state(),
    pid() | undefined,
    pid() | undefined
) ->
    eval_result().
handle_class_definition(
    ClassInfo, Warnings, Expression, MergedBindings, State, RegistryPid, Subscriber
) ->
    case beamtalk_repl_loader:load_class_module(ClassInfo, Expression, State) of
        {ok, ClassName, no_trailing, NewState2} ->
            {ok, ClassName, <<>>, Warnings, NewState2};
        {ok, _ClassName, {trailing, TrailingModName, TrailingBinary}, NewState2} ->
            %% BT-885: eval trailing expressions after class load
            case code:load_binary(TrailingModName, "", TrailingBinary) of
                {module, TrailingModName} ->
                    eval_loaded_module(
                        TrailingModName,
                        Expression,
                        MergedBindings,
                        RegistryPid,
                        Subscriber,
                        Warnings,
                        NewState2
                    );
                {error, LoadReason} ->
                    wrap_load_err(LoadReason, Warnings, NewState2)
            end;
        {error, Reason, NewState2} ->
            Err = beamtalk_repl_errors:ensure_structured_error(Reason),
            {error, Err, <<>>, Warnings, NewState2}
    end.

-doc "Handle standalone method definition: recompile and reload target class.".
-spec handle_method_definition(map(), [binary()], string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
handle_method_definition(MethodInfo, Warnings, Expression, State) ->
    beamtalk_repl_loader:reload_method_definition(MethodInfo, Warnings, Expression, State).

-doc "Handle protocol definition: load module and register protocol (BT-1612).".
-spec handle_protocol_definition(map(), [binary()], beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
handle_protocol_definition(ProtocolInfo, Warnings, State) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    case code:load_binary(ModuleName, "", Binary) of
        {module, ModuleName} ->
            %% Call register_class/0 to register the protocol with the runtime
            case maybe_register_protocol_class(ModuleName) of
                ok ->
                    NewState = beamtalk_repl_state:add_loaded_module(ModuleName, State),
                    %% Build display string: "Protocol Foo defined" or "Protocols Foo, Bar defined"
                    ProtocolNames = [binary_to_list(P) || P <- Protocols],
                    DisplayStr =
                        case ProtocolNames of
                            [Single] ->
                                list_to_binary("Protocol " ++ Single ++ " defined");
                            Multiple ->
                                list_to_binary(
                                    "Protocols " ++ string:join(Multiple, ", ") ++ " defined"
                                )
                        end,
                    {ok, DisplayStr, <<>>, Warnings, NewState};
                {error, RegError} ->
                    %% Registration failed — clean up the loaded module to avoid
                    %% leaving it resident but untracked in State.
                    RegistryPid = beamtalk_repl_state:get_actor_registry(State),
                    cleanup_module(ModuleName, RegistryPid),
                    {error, RegError, <<>>, Warnings, State}
            end;
        {error, Reason} ->
            wrap_load_err(Reason, Warnings, State)
    end.

-doc """
Attempt to call register_class/0 on a protocol module.
Returns ok if successful or not exported, or {error, #beamtalk_error{}} on failure.
""".
-spec maybe_register_protocol_class(atom()) -> ok | {error, #beamtalk_error{}}.
maybe_register_protocol_class(ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            try ModuleName:register_class() of
                {error, RegReason} ->
                    ?LOG_ERROR(
                        "Protocol register_class/0 returned error for ~p: ~p",
                        [ModuleName, RegReason],
                        #{domain => [beamtalk, runtime]}
                    ),
                    Err = beamtalk_repl_errors:ensure_structured_error(
                        {registration_error, {ModuleName, RegReason}}
                    ),
                    {error, Err};
                _Ok ->
                    ok
            catch
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR(
                        "Protocol register_class/0 failed for ~p: ~p:~p~n~p",
                        [ModuleName, Class, Reason, Stacktrace],
                        #{domain => [beamtalk, runtime]}
                    ),
                    Err = beamtalk_repl_errors:ensure_structured_error(
                        {registration_error, {ModuleName, Reason}}
                    ),
                    {error, Err}
            end;
        false ->
            ok
    end.

-doc "Evaluate a loaded module: capture IO, execute, process result, cleanup.".
-spec eval_loaded_module(
    atom(),
    string(),
    map(),
    pid() | undefined,
    pid() | undefined,
    [binary()],
    beamtalk_repl_state:state()
) ->
    eval_result().
eval_loaded_module(ModuleName, Expression, Bindings, RegistryPid, Subscriber, Warnings, State) ->
    CaptureRef = beamtalk_io_capture:start(Subscriber),
    EvalResult =
        try
            execute_and_process(ModuleName, Expression, Bindings, RegistryPid, State)
        catch
            throw:{beamtalk_script_exit, Code} ->
                %% BT-2688: `Program exit: Code` evaluated in a connected session
                %% (ADR 0099 §3 / Phase 5). This is the job-level exit signal raised
                %% by `beamtalk_program:'exit:'/1`, not a user error — surface the
                %% status so the shell reports it and terminates the session. Caught
                %% ahead of the generic clause so it is never wrapped as an error.
                {script_exit, Code, State};
            Class:Reason:Stacktrace ->
                CaughtExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stacktrace),
                CaughtBindings = Bindings#{'_error' => CaughtExObj},
                CaughtState = beamtalk_repl_state:set_bindings(CaughtBindings, State),
                {error, {eval_error, Class, CaughtExObj}, CaughtState}
        after
            cleanup_module(ModuleName, RegistryPid)
        end,
    Output = beamtalk_io_capture:stop(CaptureRef),
    inject_output(EvalResult, Output, Warnings).

-doc "Execute module and process the result (assignment/future handling).".
-spec execute_and_process(atom(), string(), map(), pid() | undefined, beamtalk_repl_state:state()) ->
    {ok, term(), beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
execute_and_process(ModuleName, Expression, Bindings, RegistryPid, State) ->
    BindingsWithRegistry =
        case RegistryPid of
            undefined -> Bindings;
            _ -> Bindings#{?INTERNAL_REGISTRY_KEY => RegistryPid}
        end,
    {RawResult, UpdatedBindings} = apply(ModuleName, eval, [BindingsWithRegistry]),
    CleanBindings = strip_internal_bindings(UpdatedBindings),
    Result = maybe_await_future(RawResult),
    process_eval_result(Result, Expression, CleanBindings, State).

-doc "Process evaluation result: handle rejected futures and assignments.".
-spec process_eval_result(term(), string(), map(), beamtalk_repl_state:state()) ->
    {ok, term(), beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
process_eval_result({future_rejected, ErrorReason}, _Expression, CleanBindings, State) ->
    FutExObj = beamtalk_exception_handler:ensure_wrapped(ErrorReason),
    FutBindings = CleanBindings#{'_error' => FutExObj},
    FinalState = beamtalk_repl_state:set_bindings(FutBindings, State),
    {error, FutExObj, FinalState};
process_eval_result(Result, Expression, CleanBindings, State) ->
    case extract_assignment(Expression) of
        {ok, VarName} ->
            NewBindings = CleanBindings#{VarName => Result},
            %% ADR 0093 §2 (BT-2445): a workspace assignment (`x := ...`) is a
            %% BindingChanged system event. Announced after the new binding map is
            %% built; best-effort and fault-isolated (see announce_binding_changed/2).
            announce_binding_changed(VarName, Result),
            {ok, Result, beamtalk_repl_state:set_bindings(NewBindings, State)};
        none ->
            {ok, Result, beamtalk_repl_state:set_bindings(CleanBindings, State)}
    end.

-doc """
Announce `BindingChanged` on the `SystemAnnouncer` bus after a workspace variable
is assigned (ADR 0093 §2, BT-2445).

The payload carries `sessionId` — the protocol session id of the evaluating
session, read from the eval worker's process dictionary (seeded by
`beamtalk_repl_shell:seed_session_context/3`) — so a multi-session consumer can
attribute the change without a round-trip (BT-2530). `nil` when the eval runs
outside a shell-spawned worker.

Guarded by a `whereis` check (the announcements worker may be absent on a
minimal runtime) and wrapped in try/catch: announcing is a best-effort
observability side effect and must never fail or delay the eval reply.
""".
-spec announce_binding_changed(atom(), term()) -> ok.
announce_binding_changed(VarName, Value) ->
    SessionId =
        case get(beamtalk_session_id) of
            undefined -> nil;
            Id -> Id
        end,
    announce_binding_changed(VarName, Value, SessionId).

-doc """
As `announce_binding_changed/2`, but with an explicit `SessionId` rather than
reading it from the process dictionary (BT-2531).

Used by `beamtalk_repl_shell` for the workspace binding-mutation paths — `Session
clear` (`handle_call(clear_bindings, …)`) and the pending `put`/`remove`/`clear`
edits applied after a worker returns. Those run in the shell gen_server process,
which does **not** carry `beamtalk_session_id` (it is seeded only in eval
workers), so the session id is threaded from the shell's state tuple. This keeps
the `bindings` push stream refreshing on every binding change, not just on `:=`
assignment, restoring the coarse refresh the retired `beamtalk_bindings_events`
channel provided — now as a typed, session-scoped `BindingChanged`.
""".
-spec announce_binding_changed(atom(), term(), binary() | nil) -> ok.
announce_binding_changed(VarName, Value, SessionId) ->
    case erlang:whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _Pid ->
            try
                beamtalk_announcements:system_announce('BindingChanged', #{
                    name => VarName, value => Value, sessionId => SessionId
                })
            catch
                _:_ -> ok
            end
    end,
    ok.

-doc "Purge eval module if no living actors reference it.".
-spec cleanup_module(atom(), pid() | undefined) -> ok.
cleanup_module(ModuleName, RegistryPid) ->
    case should_purge_module(ModuleName, RegistryPid) of
        true ->
            case code:purge(ModuleName) of
                true -> code:delete(ModuleName);
                false -> ok
            end;
        false ->
            ok
    end,
    ok.

-doc """
Rebuild bindings by extracting variable assignments from awaited trace steps (BT-1261).

For each step whose source text is a simple assignment (`VarName := Expr`), the
binding is updated with the awaited (resolved) value from the step.  This ensures
that if the RHS was a future the session binding holds the final value, not the
raw `{beamtalk_future, Pid}` handle stored by the evaluator before awaiting.
""".
-spec rebuild_bindings_from_steps([{binary(), term()}], map()) -> map().
rebuild_bindings_from_steps(Steps, Bindings) ->
    lists:foldl(
        fun({Src, AwaitedVal}, Acc) ->
            SrcStr = binary_to_list(Src),
            case extract_assignment(SrcStr) of
                {ok, VarName} -> Acc#{VarName => AwaitedVal};
                none -> Acc
            end
        end,
        Bindings,
        Steps
    ).

-doc "Extract variable name from assignment expression.".
-spec extract_assignment(string()) -> {ok, atom()} | none.
extract_assignment(Expression) ->
    case re:run(Expression, "\\.\\s+\\S", []) of
        {match, _} ->
            none;
        nomatch ->
            case
                re:run(
                    Expression,
                    "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=",
                    [{capture, [1], list}]
                )
            of
                % elp:fixme W0023 intentional atom creation
                {match, [VarName]} -> {ok, list_to_atom(VarName)};
                nomatch -> none
            end
    end.

-doc "Auto-await a Future if the result is a tagged future tuple (BT-840).".
-spec maybe_await_future(term()) -> term().
maybe_await_future({beamtalk_future, _} = Future) ->
    try beamtalk_runtime_api:future_await(Future, 30000) of
        AwaitedValue -> AwaitedValue
    catch
        throw:#beamtalk_error{kind = timeout} = TimeoutError ->
            {future_rejected, TimeoutError};
        throw:{future_rejected, #beamtalk_error{} = Reason} ->
            {future_rejected, Reason};
        throw:{future_rejected, Reason} ->
            {future_rejected, Reason}
    end;
maybe_await_future(Value) ->
    Value.

-doc "Check if a module should be purged (no living actors reference it).".
-spec should_purge_module(atom(), pid() | undefined) -> boolean().
should_purge_module(_ModuleName, undefined) ->
    true;
should_purge_module(ModuleName, RegistryPid) ->
    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    not lists:any(fun(#{module := ActorModule}) -> ActorModule =:= ModuleName end, Actors).

-doc "Strip internal plumbing keys from bindings map (BT-153).".
-spec strip_internal_bindings(map()) -> map().
strip_internal_bindings(Bindings) ->
    %% BT-881: Strip workspace-only binding keys injected by do_eval.
    Stripped0 =
        case maps:find(?WORKSPACE_BINDINGS_KEY, Bindings) of
            {ok, WorkspaceOnlyBindings} when is_map(WorkspaceOnlyBindings) ->
                WithoutMeta = maps:remove(?WORKSPACE_BINDINGS_KEY, Bindings),
                maps:fold(
                    fun(Key, OriginalValue, Acc) ->
                        % elp:fixme W0032 maps:find with complex branch logic
                        case maps:find(Key, Acc) of
                            {ok, OriginalValue} -> maps:remove(Key, Acc);
                            _ -> Acc
                        end
                    end,
                    WithoutMeta,
                    WorkspaceOnlyBindings
                );
            _ ->
                Bindings
        end,
    maps:remove(?INTERNAL_REGISTRY_KEY, Stripped0).

-doc "Inject captured output and warnings into an eval result tuple.".
-spec inject_output(
    {ok, term(), beamtalk_repl_state:state()}
    | {error, term(), beamtalk_repl_state:state()}
    | {script_exit, non_neg_integer(), beamtalk_repl_state:state()},
    binary(),
    [binary()]
) -> eval_result().
inject_output({ok, Result, State}, Output, Warnings) ->
    {ok, Result, Output, Warnings, State};
inject_output({error, Reason, State}, Output, Warnings) ->
    {error, Reason, Output, Warnings, State};
%% BT-2688: connected-session `Program exit:` — carry the status alongside any
%% output captured before the exit signal fired.
inject_output({script_exit, Code, State}, Output, Warnings) ->
    {script_exit, Code, Output, Warnings, State}.

-doc "Wrap a compile error as a structured #beamtalk_error{} result tuple.".
-spec wrap_compile_err(term(), beamtalk_repl_state:state()) ->
    {error, #beamtalk_error{}, binary(), [binary()], beamtalk_repl_state:state()}.
wrap_compile_err(Reason, State) ->
    Err = beamtalk_repl_errors:ensure_structured_error({compile_error, Reason}),
    {error, Err, <<>>, [], State}.

-doc "Wrap a load error as a structured #beamtalk_error{} result tuple.".
-spec wrap_load_err(term(), [binary()], beamtalk_repl_state:state()) ->
    {error, #beamtalk_error{}, binary(), [binary()], beamtalk_repl_state:state()}.
wrap_load_err(Reason, Warnings, State) ->
    Err = beamtalk_repl_errors:ensure_structured_error({load_error, Reason}),
    {error, Err, <<>>, Warnings, State}.
