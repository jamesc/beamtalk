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
    do_show_codegen/2,
    handle_load/2, handle_load/3,
    handle_load_source/3
]).
%% BT-845: ADR 0040 Phase 2 — stateless class reload (called via erlang:apply from beamtalk_runtime)
-export([reload_class_file/1, reload_class_file/2]).

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
-export([compile_method/4, compile_method/6]).

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
    wrap_load_err/3
]).
-endif.

-define(INTERNAL_REGISTRY_KEY, '__repl_actor_registry__').
-define(WORKSPACE_BINDINGS_KEY, '__workspace_user_bindings__').

%%% Public API

-doc "Evaluate a Beamtalk expression.".
-spec do_eval(string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State) ->
    do_eval(Expression, State, undefined).

-doc "Evaluate with optional streaming subscriber (BT-696).".
-spec do_eval(string(), beamtalk_repl_state:state(), pid() | undefined) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State, Subscriber) ->
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

    case beamtalk_repl_compiler:compile_expression(Expression, ModuleName, Bindings) of
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
Evaluate a Beamtalk expression in trace mode (BT-1238).

Returns `{ok, Steps, Output, Warnings, State}' where
`Steps = [{SourceBin, Value}]' — one entry per top-level statement —
or `{error, Reason, Output, Warnings, State}' on failure.
""".
-spec do_eval_trace(string(), beamtalk_repl_state:state()) ->
    {ok, [{binary(), term()}], binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
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
    case beamtalk_repl_compiler:compile_expression(SourceStr, ModuleName, Bindings) of
        {ok, class_definition, _Info, _Warnings} ->
            eval_not_an_expression_error();
        {ok, method_definition, _Info, _Warnings} ->
            eval_not_an_expression_error();
        {ok, protocol_definition, _Info, _Warnings} ->
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
    SelectorBin = beamtalk_repl_protocol:to_binary(Selector),
    %% Obey project mode: a built-in (stdlib) class is compiled in stdlib mode and
    %% its bodies may use `@intrinsic'/`@primitive', which the workspace's
    %% project-mode recompile cannot reproduce. Refuse the patch up front with a
    %% clear error rather than attempt a recompile that cannot succeed.
    case stdlib_class_module(ClassNameBin) of
        {ok, _Module} ->
            {error, stdlib_method_read_only_error(ClassNameBin, SelectorBin)};
        none ->
            do_compile_method(ClassNameBin, SelectorBin, Source, Intent, Author, AuthorKind)
    end.

-spec do_compile_method(
    binary(), binary(), binary(), durable | ephemeral, binary(), human | agent
) ->
    {ok, binary()} | {error, term()}.
do_compile_method(ClassNameBin, SelectorBin, Source, Intent, Author, AuthorKind) ->
    %% Structured single-method install. `Source' is the BARE method body — the
    %% backend parses it standalone (no `Class >>' text wrap, no
    %% `normalize_method_source' header-sniffing), so comments and formatting
    %% round-trip exactly and the patched class keeps its package-qualified module
    %% name + on-disk source attribution (flushable + revertable). A `Source' that
    %% is not a single method definition comes back as a structured compile error.
    State = beamtalk_repl_state:new(undefined, 0),
    case
        beamtalk_repl_loader:install_method(
            ClassNameBin, SelectorBin, Source, Intent, Author, AuthorKind, [], State
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
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
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
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
eval_loaded_module(ModuleName, Expression, Bindings, RegistryPid, Subscriber, Warnings, State) ->
    CaptureRef = beamtalk_io_capture:start(Subscriber),
    EvalResult =
        try
            execute_and_process(ModuleName, Expression, Bindings, RegistryPid, State)
        catch
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
-spec inject_output(tuple(), binary(), [binary()]) -> tuple().
inject_output({ok, Result, State}, Output, Warnings) ->
    {ok, Result, Output, Warnings, State};
inject_output({error, Reason, State}, Output, Warnings) ->
    {error, Reason, Output, Warnings, State}.

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
