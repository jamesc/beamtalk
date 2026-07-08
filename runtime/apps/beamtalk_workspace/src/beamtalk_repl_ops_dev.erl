%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_dev).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for complete, describe, and show-codegen operations.

Extracted from beamtalk_repl_server (BT-705).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    handle/4,
    handle_term/4,
    get_completions/1,
    get_context_completions/1,
    get_context_completions/2,
    parse_receiver_and_prefix/1,
    tokenise_send_chain/1,
    tokenise_binary_chain/1,
    resolve_chain_type/2,
    walk_chain/2,
    walk_chain_class/2,
    walk_mixed_chain/2,
    walk_mixed_chain_class/2,
    make_class_not_found_error/1,
    base_protocol_response/1,
    list_class_methods_for_ws/1,
    resolve_qualified_class_name/1
]).

-include_lib("kernel/include/logger.hrl").

%% BT-1045: Export internals for white-box testing of the binding-lookup pipeline.
-ifdef(TEST).
-export([
    get_session_bindings/1,
    validate_selector_if_present/4,
    %% BT-2572: white-box test of the diagnostics `mode` normalisation that
    %% mirrors the Elixir facade at the Erlang op boundary.
    normalize_diagnostics_mode/1
]).
-endif.

%% Methods inherited from Object that are internal implementation protocol and
%% should not appear in user-facing completions. Long-term, reflection methods
%% (fieldNames, fieldAt:, fieldAt:put:) should move to Mirror classes (BT-1049).
-define(COMPLETION_HIDDEN_METHODS, [
    %% Abstract-class stubs — only meaningful inside a class body
    subclassResponsibility,
    notImplemented,
    %% Message-not-understood handler — internal dispatch protocol
    'doesNotUnderstand:args:',
    %% Reflection intrinsics — long-term these should move to a Mirror class
    fieldNames,
    'fieldAt:',
    'fieldAt:put:',
    %% Object constructors — valid on class objects but confusing on instances
    %% ("foo" new dispatches via basicNew which most classes don't support directly)
    new,
    'new:',
    %% Dynamic dispatch — advanced/meta protocol, rarely called directly
    'perform:',
    'perform:withArguments:'
]).

-doc """
Handle complete/describe/show-codegen/methods/list-classes/test/test-all/
erlang-help/erlang-complete ops for the WebSocket transport — encodes the term
result to JSON at the edge (BT-2402).
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for the developer read-surface ops (BT-2402, ADR 0085).

Returns `{completions, [binary()]}`, `{docs, binary()}`,
`{codegen, CoreErlang, Warnings}`, `{methods, Methods, StateVars}`,
`{class_list, [ClassInfo]}`, `{test_results, TestResult}`,
`{describe, Ops, Versions}`, or `{error, #beamtalk_error{}}` — no JSON in this
path.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"complete">>, Params, Msg, SessionPid) ->
    Code = maps:get(<<"code">>, Params, <<>>),
    %% BT-783: New protocol includes "cursor" field — Code is the full line up to cursor.
    %% Old protocol omits "cursor" — Code is a bare prefix (backward compat).
    Completions =
        case maps:is_key(<<"cursor">>, Params) of
            true ->
                %% Completions run on a separate WebSocket session with no user bindings.
                %% If the client passes its main session ID, resolve bindings from that session
                %% so instance-method completions work for bound actor variables.
                %% BT-1045: session is decoded into Msg by the protocol layer and stripped
                %% from Params — use get_session(Msg), NOT maps:get(<<"session">>, Params).
                BindingPid = beamtalk_session_table:resolve_pid(
                    beamtalk_repl_protocol:get_session(Msg), SessionPid
                ),
                SessionBindings = get_session_bindings(BindingPid),
                WorkspaceBindings = get_workspace_bindings(),
                %% Session bindings take priority over workspace globals (e.g. Transcript)
                Bindings = maps:merge(WorkspaceBindings, SessionBindings),
                get_context_completions(Code, Bindings);
            false ->
                get_completions(Code)
        end,
    {completions, Completions};
handle_term(<<"hover">>, Params, Msg, SessionPid) ->
    %% BT-2555: live-image hover for the cockpit CodeMirror editors. `code` is
    %% the editor line up to (and including) the hovered token. We resolve the
    %% token against the LIVE class registry — a bare class name → its docs, a
    %% `Receiver selector` pair → that method's docs — and format signature +
    %% doc-comment markdown via beamtalk_repl_docs, the SAME live doc engine the
    %% REPL `:help` uses (no forked hover engine; the static LSP path in
    %% hover_provider.rs reads on-disk ASTs and so cannot see REPL-defined or
    %% live-patched classes — this can). Pure reflection, runs no user code, so
    %% it is a `:read` op (the Observer may hover).
    Code = maps:get(<<"code">>, Params, <<>>),
    case Code of
        <<>> ->
            {docs, <<>>};
        _ ->
            %% Resolve bindings the same way `complete` does so an instance
            %% receiver that is a bound variable classifies (BT-1045). Hover is
            %% answered on a side WebSocket session with no user bindings, so we
            %% resolve the caller's main session via the protocol-decoded id.
            BindingPid = beamtalk_session_table:resolve_pid(
                beamtalk_repl_protocol:get_session(Msg), SessionPid
            ),
            SessionBindings = get_session_bindings(BindingPid),
            WorkspaceBindings = get_workspace_bindings(),
            Bindings = maps:merge(WorkspaceBindings, SessionBindings),
            {docs, hover_docs(Code, Bindings)}
    end;
handle_term(<<"diagnostics">>, Params, _Msg, _SessionPid) ->
    %% BT-2556: parse-only diagnostics for the cockpit CodeMirror editors. `code`
    %% is the FULL editor buffer (not a line prefix). We run the compiler's
    %% side-effect-free `diagnostics/2` path — parse + semantic check via the
    %% Rust port's `diagnostics` command — which compiles for DIAGNOSIS ONLY: it
    %% emits no module, installs nothing, never touches the image or the
    %% ChangeLog, and runs no user code. That makes it safe to fire on every
    %% keystroke and a `:read` op (the Observer may see diagnostics). Each entry
    %% carries byte-offset `start`/`end` spans + a `severity` + a `message`; the
    %% client maps spans to editor positions and severities to squiggles.
    %% `mode` (BT-2569) selects the parse grammar: <<"expression">> (default,
    %% top-level script — the Workspace + REPL editors) or <<"method">> (a bare
    %% method body — the System Browser method editor, where the `=>` body
    %% separator is not a valid top-level token).
    Code = maps:get(<<"code">>, Params, <<>>),
    %% BT-2572: normalise an unknown-binary `mode` to <<"expression">> here, at
    %% the Erlang op boundary, mirroring the Elixir `BtAttach.Facade` (which maps
    %% anything but "method" to "expression"). Without this, an unknown binary
    %% (e.g. <<"foo">>) flowed straight to the Rust port, which only special-cases
    %% "method" and treats everything else as expression mode — so the runtime
    %% behaviour was already correct, but the Erlang boundary was more permissive
    %% than the facade. A non-binary `mode` is passed through unchanged so it
    %% still degrades to `[]` via the `diagnostics_for/2` catch-all (BT-2569).
    Mode = normalize_diagnostics_mode(maps:get(<<"mode">>, Params, <<"expression">>)),
    {diagnostics, diagnostics_for(Code, Mode)};
handle_term(<<"erlang-complete">>, Params, _Msg, _SessionPid) ->
    %% BT-1903: Tab completion for `:h Erlang <module>` and `:h Erlang <mod> <fn>`.
    Prefix = maps:get(<<"prefix">>, Params, <<>>),
    ModuleBin = nonempty_or_undefined(maps:get(<<"module">>, Params, undefined)),
    Completions =
        case ModuleBin of
            undefined ->
                %% Complete module names
                All = beamtalk_erlang_help:available_modules(),
                [M || M <- All, binary:match(M, Prefix) =:= {0, byte_size(Prefix)}];
            _ ->
                %% Complete function names within a module.
                %% Use beamtalk_erlang_help to avoid triggering module autoload during completion.
                try binary_to_existing_atom(ModuleBin, utf8) of
                    Module ->
                        %% get_object_code avoids autoload and on_load side effects.
                        case code:get_object_code(Module) of
                            {Module, _Binary, _Filename} ->
                                %% Module is already loaded; safe to get exports.
                                try Module:module_info(exports) of
                                    Exports ->
                                        Filtered = [
                                            atom_to_binary(F, utf8)
                                         || {F, _A} <- Exports,
                                            F =/= module_info
                                        ],
                                        Unique = lists:usort(Filtered),
                                        [
                                            F
                                         || F <- Unique,
                                            binary:match(F, Prefix) =:= {0, byte_size(Prefix)}
                                        ]
                                catch
                                    _:_ -> []
                                end;
                            error ->
                                %% Module not loaded; fall back to empty completions
                                %% to avoid triggering autoload during a read-only operation.
                                []
                        end
                catch
                    error:badarg -> []
                end
        end,
    {completions, Completions};
handle_term(<<"erlang-help">>, Params, _Msg, _SessionPid) ->
    %% BT-1852: `:help Erlang <module>` and `:help Erlang <module> <function>`
    %% Delegates to beamtalk_erlang_help for formatting.
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    FunctionBin = nonempty_or_undefined(maps:get(<<"function">>, Params, undefined)),
    case ModuleBin of
        <<>> ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'REPL',
                    <<"Module name required">>,
                    <<"Usage: :help Erlang <module>">>
                )};
        _ ->
            %% Use binary_to_existing_atom to prevent atom table exhaustion
            %% from repeated queries with typos (atoms are never GC'd).
            try binary_to_existing_atom(ModuleBin, utf8) of
                Module ->
                    Result =
                        case FunctionBin of
                            undefined ->
                                beamtalk_erlang_help:format_module_help(Module);
                            _ ->
                                beamtalk_erlang_help:format_function_help(Module, FunctionBin)
                        end,
                    case Result of
                        {ok, Text} ->
                            {docs, Text};
                        {error, not_found} ->
                            What =
                                case FunctionBin of
                                    undefined ->
                                        iolist_to_binary([
                                            <<"Erlang module '">>, ModuleBin, <<"'">>
                                        ]);
                                    _ ->
                                        iolist_to_binary([
                                            ModuleBin, <<":">>, FunctionBin
                                        ])
                                end,
                            Hint =
                                case FunctionBin of
                                    undefined ->
                                        <<"Check the module name and ensure it is available on the code path.">>;
                                    _ ->
                                        iolist_to_binary([
                                            <<"Use :help Erlang ">>,
                                            ModuleBin,
                                            <<" to see available functions and types.">>
                                        ])
                                end,
                            erlang_not_found_error(What, Hint)
                    end
            catch
                error:badarg ->
                    erlang_not_found_error(
                        iolist_to_binary([
                            <<"Erlang module '">>, ModuleBin, <<"'">>
                        ]),
                        <<"Check the module name and ensure it is available on the code path.">>
                    )
            end
    end;
handle_term(<<"show-codegen">>, Params, _Msg, SessionPid) ->
    %% BT-700: Compile expression and return Core Erlang source without evaluating.
    %% BT-1236: Also accepts class+selector to inspect a loaded class method.
    %% `class` takes priority when both are present; empty string is treated as absent.
    ClassBin = nonempty_or_undefined(maps:get(<<"class">>, Params, undefined)),
    CodeBin = maps:get(<<"code">>, Params, undefined),
    SelectorBin = nonempty_or_undefined(maps:get(<<"selector">>, Params, undefined)),
    %% Reject orphaned selector at the Erlang boundary, mirroring the Rust MCP guard,
    %% so behaviour is consistent regardless of which entry point is used.
    case {SelectorBin, ClassBin} of
        {SB, undefined} when SB =/= undefined ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument,
                    'REPL',
                    <<"'selector' requires 'class' to be specified">>,
                    <<"Provide 'class' along with 'selector' to inspect a specific method.">>
                )};
        _ ->
            case {ClassBin, CodeBin} of
                {CB, _} when CB =/= undefined ->
                    show_codegen_class_method(CB, SelectorBin);
                {undefined, CB} when CB =/= undefined ->
                    Code = binary_to_list(CB),
                    case Code of
                        [] ->
                            {error,
                                beamtalk_repl_errors:make(
                                    empty_expression,
                                    'REPL',
                                    <<"Empty expression">>,
                                    <<"Enter an expression to compile.">>
                                )};
                        _ ->
                            case beamtalk_repl_shell:show_codegen(SessionPid, Code) of
                                {ok, CoreErlang, Warnings} ->
                                    {codegen, CoreErlang, Warnings};
                                {error, ErrorReason, Warnings} ->
                                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(
                                        ErrorReason
                                    ),
                                    {error, WrappedReason, <<>>, Warnings}
                            end
                    end;
                {undefined, undefined} ->
                    {error,
                        beamtalk_repl_errors:make(
                            empty_expression,
                            'REPL',
                            <<"Missing required parameter">>,
                            <<"Provide 'code' to compile an expression, or 'class' to inspect a loaded class.">>
                        )}
            end
    end;
handle_term(<<"methods">>, Params, _Msg, _SessionPid) ->
    %% BT-1026: Return instance and class-side methods for a loaded class.
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    Methods = list_class_methods_for_ws(ClassBin),
    StateVars = list_state_vars_for_ws(ClassBin),
    {methods, Methods, StateVars};
handle_term(<<"list-classes">>, Params, _Msg, SessionPid) ->
    %% BT-1404: List all available classes with one-line descriptions.
    %% BT-2091: Now also returns `source_file` and `actor_count` so editors
    %% (VS Code, LSP) can drive class navigation without the deprecated
    %% `modules` op. `source_file` is resolved from workspace metadata;
    %% `actor_count` is session-scoped and is 0 when no session is provided.
    RawFilter = maps:get(<<"filter">>, Params, undefined),
    %% Validate filter upfront: resolve superclass name to atom if needed
    Filter = validate_list_classes_filter(RawFilter),
    case Filter of
        {error, BadFilter} ->
            Error = #beamtalk_error{
                kind = argument_error,
                class = 'Object',
                selector = undefined,
                message = iolist_to_binary([
                    <<"Unknown filter: '">>,
                    BadFilter,
                    <<"'. Use 'stdlib', 'user', or a class name like 'Value' or 'Actor'.">>
                ]),
                hint = undefined,
                details = #{}
            },
            {error, Error};
        _ ->
            case
                try
                    {ok, beamtalk_runtime_api:all_classes()}
                catch
                    _:AllClassesErr ->
                        {error, #beamtalk_error{
                            kind = runtime_error,
                            class = 'Object',
                            selector = undefined,
                            message = iolist_to_binary(
                                io_lib:format("Failed to list classes: ~p", [AllClassesErr])
                            ),
                            hint = <<"Is the runtime started?">>,
                            details = #{}
                        }}
                end
            of
                {error, Error} ->
                    {error, Error};
                {ok, ClassPids} ->
                    %% BT-2091: Fetch session-scoped data once for all classes.
                    %% Falls back to an empty tracker when SessionPid is undefined
                    %% or the session is gone, so list-classes still answers in
                    %% editor / non-session contexts (actor_count = 0).
                    Tracker = list_classes_session_tracker(SessionPid),
                    RegistryPid = whereis(beamtalk_actor_registry),
                    ClassInfos = lists:filtermap(
                        fun(Pid) ->
                            try
                                Name = beamtalk_runtime_api:class_name(Pid),
                                Super = beamtalk_runtime_api:superclass(Pid),
                                Doc =
                                    case gen_server:call(Pid, get_doc, 5000) of
                                        none -> null;
                                        D when is_binary(D) -> first_line(D);
                                        _ -> null
                                    end,
                                IsSealed = beamtalk_runtime_api:is_sealed(Pid),
                                IsAbstract = beamtalk_runtime_api:is_abstract(Pid),
                                ModName = beamtalk_runtime_api:module_name(Pid),
                                %% ADR 0071 Phase 5: Include visibility in class listing
                                IsInternal =
                                    try
                                        beamtalk_runtime_api:is_internal(Pid)
                                    catch
                                        _:_ -> false
                                    end,
                                case should_include_class(Name, Super, ModName, Filter) of
                                    true ->
                                        SourceFile = list_classes_source_file(ModName),
                                        ActorCount = beamtalk_repl_modules:get_actor_count(
                                            ModName, RegistryPid, Tracker
                                        ),
                                        {true, #{
                                            <<"name">> => atom_to_binary(Name, utf8),
                                            <<"superclass">> =>
                                                case Super of
                                                    none -> null;
                                                    S -> atom_to_binary(S, utf8)
                                                end,
                                            <<"doc">> => Doc,
                                            <<"sealed">> => IsSealed,
                                            <<"abstract">> => IsAbstract,
                                            <<"internal">> => IsInternal,
                                            <<"source_file">> => SourceFile,
                                            <<"actor_count">> => ActorCount
                                        }};
                                    false ->
                                        false
                                end
                            catch
                                exit:{noproc, _} ->
                                    false;
                                exit:{timeout, _} ->
                                    false;
                                Class:Reason ->
                                    ?LOG_WARNING(
                                        "list-classes: skipping class ~p: ~p:~p",
                                        [Pid, Class, Reason],
                                        #{domain => [beamtalk, runtime]}
                                    ),
                                    false
                            end
                        end,
                        ClassPids
                    ),
                    Sorted = lists:sort(
                        fun(A, B) -> maps:get(<<"name">>, A) =< maps:get(<<"name">>, B) end,
                        ClassInfos
                    ),
                    {class_list, Sorted}
            end
    end;
handle_term(<<"test">>, Params, _Msg, _SessionPid) ->
    ClassName = maps:get(<<"class">>, Params, undefined),
    FilePath = maps:get(<<"file">>, Params, undefined),
    case {ClassName, FilePath} of
        {CN, FP} when CN =/= undefined, FP =/= undefined ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument, 'TestRunner', <<"'class' and 'file' are mutually exclusive">>
                )};
        {undefined, FP} when FP =/= undefined, not is_binary(FP) ->
            {error,
                beamtalk_repl_errors:make(
                    invalid_argument, 'TestRunner', <<"'file' must be a binary path">>
                )};
        {undefined, FP} when FP =/= undefined ->
            run_test_op_file(FP);
        _ ->
            run_test_op(ClassName)
    end;
handle_term(<<"test-all">>, _Params, _Msg, _SessionPid) ->
    run_test_op(undefined);
handle_term(<<"list-tests">>, _Params, _Msg, _SessionPid) ->
    %% BT-2557: discover loaded TestCase subclasses + their selectors for the
    %% cockpit's test-runner pane. Pure reflection over the class registry — runs
    %% NO test code — so it is a `:read` op (the Observer may list tests). The
    %% discovery maps are projected to the binary-keyed wire shape so the result
    %% travels as a `{value, _}` term (consumed live over distribution, or encoded
    %% as JSON identity at the WebSocket edge).
    Discovered = beamtalk_test_runner:discover_tests(),
    Classes = [
        #{
            <<"class">> => maps:get(class, T),
            <<"selectors">> => maps:get(selectors, T)
        }
     || T <- Discovered
    ],
    {value, #{<<"classes">> => Classes}};
handle_term(<<"load-tests">>, _Params, _Msg, _SessionPid) ->
    %% BT-2557: load the project's `test/` files into the live image so the
    %% cockpit test-runner pane (and the System Browser's "Tests" group) surface
    %% TestCase subclasses without a CLI `beamtalk test` round-trip. Plain
    %% `load-project` defaults to include_tests=false, so opening a project never
    %% loads test files and the runner shows an empty catalogue (the gap this op
    %% closes). Delegates to the shared `sync_project/2` with include_tests=true,
    %% scoped to the workspace cwd (the project root, as `Workspace sync` assumes).
    %% Compiles + loads user test code (mutating the image), so the facade gates
    %% it `:execute` (Owner-only), mirroring `run_tests`/`eval`.
    %%
    %% No `session_pid` is threaded: this op is workspace-global (like `list-tests`
    %% / `test-all`), and the dist-attached caller passes its OWN pid as the
    %% `dispatch/4` SessionPid placeholder — a remote pid that is not a
    %% `beamtalk_repl_shell` gen_server. Passing it would route the load through
    %% the session-tracking path (`load_files_sequential` → `beamtalk_repl_shell`)
    %% against the wrong process. Omitting it selects the stateless load path
    %% (`load_files_stateless` → `beamtalk_repl_loader`), exactly as the
    %% `Workspace sync` primitive does.
    case
        beamtalk_repl_ops_load:sync_project(".", #{
            include_tests => true
        })
    of
        {ok, Result} ->
            {value, #{
                <<"classes">> => maps:get(classes, Result, []),
                <<"errors">> => maps:get(errors, Result, []),
                <<"summary">> => maps:get(summary, Result, <<>>)
            }};
        {error, Err} ->
            {error, Err}
    end;
handle_term(<<"describe">>, _Params, _Msg, _SessionPid) ->
    Ops = describe_ops(),
    BeamtalkVsnBin =
        case application:get_key(beamtalk_workspace, vsn) of
            {ok, Vsn} when is_list(Vsn) -> list_to_binary(Vsn);
            {ok, Vsn} when is_binary(Vsn) -> Vsn;
            _ -> <<"unknown">>
        end,
    Versions = #{
        %% BT-2091: Bumped to 2.0 — removed deprecated ops docs/load-file/reload/modules.
        <<"protocol">> => <<"2.0">>,
        <<"beamtalk">> => BeamtalkVsnBin
    },
    {describe, Ops, Versions}.

-doc """
Handle show-codegen for a loaded class method (BT-1236).

Looks up the class in the runtime, validates the selector (if given),
retrieves the authoritative live source from workspace metadata (falling back
to the on-disk file), re-compiles for codegen, and returns Core Erlang.

All gen_server calls on ClassPid are guarded against TOCTOU races: if the
class is unloaded between the whereis_class/1 lookup and subsequent calls,
the noproc exit is translated to a structured class_not_found error.
""".
-spec show_codegen_class_method(
    binary(), binary() | undefined
) -> beamtalk_repl_ops:op_result().
show_codegen_class_method(ClassBin, SelectorBin) ->
    %% BT-1659: Support package-qualified class names (e.g. "json@Parser")
    case resolve_qualified_class_name(ClassBin) of
        {error, badarg} ->
            {error, make_class_not_found_error(ClassBin)};
        {ok, ClassAtom} ->
            case beamtalk_runtime_api:whereis_class(ClassAtom) of
                undefined ->
                    {error, make_class_not_found_error(ClassBin)};
                ClassPid ->
                    %% Guard all ClassPid gen_server calls against unload/reload races.
                    try
                        case
                            validate_selector_if_present(
                                ClassBin, ClassAtom, ClassPid, SelectorBin
                            )
                        of
                            {error, Err} ->
                                {error, Err};
                            ok ->
                                compile_class_source(ClassBin, ClassAtom, ClassPid)
                        end
                    catch
                        exit:{noproc, _} ->
                            {error, make_class_not_found_error(ClassBin)};
                        exit:{timeout, _} ->
                            {error,
                                beamtalk_repl_errors:make(
                                    runtime_error,
                                    ClassAtom,
                                    iolist_to_binary([
                                        <<"Class '">>,
                                        ClassBin,
                                        <<"' is not responding (may be under heavy load)">>
                                    ])
                                )}
                    end
            end
    end.

-doc """
Retrieve the authoritative source for a loaded class and compile it for codegen.

Reads from workspace_meta first (updated by :load and method patching), falling
back to the on-disk file recorded in the beamtalk_source module attribute.
""".
-spec compile_class_source(
    binary(), atom(), pid()
) -> beamtalk_repl_ops:op_result().
compile_class_source(ClassBin, ClassAtom, ClassPid) ->
    ModuleName = beamtalk_runtime_api:module_name(ClassPid),
    SourcePath = beamtalk_repl_modules:resolve_source_path(ModuleName),
    %% Prefer the live in-memory source over the on-disk file; the metadata is
    %% updated by handle_load and method-patching so it reflects the current state.
    CompilePath =
        if
            SourcePath =:= "unknown" -> undefined;
            true -> SourcePath
        end,
    SourceResult =
        case beamtalk_workspace_meta:get_class_source(ClassBin) of
            undefined when SourcePath =:= "unknown" ->
                {error, no_source};
            undefined ->
                file:read_file(SourcePath);
            SrcStr when is_list(SrcStr) ->
                {ok, list_to_binary(SrcStr)}
        end,
    case SourceResult of
        {error, no_source} ->
            {error,
                beamtalk_repl_errors:make(
                    runtime_error,
                    ClassAtom,
                    iolist_to_binary([
                        <<"No source for ">>,
                        ClassBin,
                        <<". Class may have been defined inline.">>
                    ])
                )};
        {ok, SourceBin} ->
            case beamtalk_repl_compiler:compile_file_for_codegen(SourceBin, CompilePath) of
                {ok, CoreErlang, Warnings} ->
                    {codegen, CoreErlang, Warnings};
                {error, ErrorReason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(ErrorReason)}
            end;
        {error, Reason} ->
            {error,
                beamtalk_repl_errors:make(
                    runtime_error,
                    ClassAtom,
                    iolist_to_binary(
                        io_lib:format("Cannot read source file ~s: ~p", [SourcePath, Reason])
                    )
                )}
    end.

-doc """
Validate that SelectorBin exists on the class (instance-side or class-side).
Returns ok when selector is undefined (no validation needed) or when found.
Returns `{error, #beamtalk_error{}}` when the selector is unknown (BT-2402: a
structured error term rather than a pre-encoded JSON binary).
""".
-spec validate_selector_if_present(
    binary(), atom(), pid(), binary() | undefined
) -> ok | {error, #beamtalk_error{}}.
validate_selector_if_present(_ClassBin, _ClassAtom, _ClassPid, undefined) ->
    ok;
validate_selector_if_present(ClassBin, ClassAtom, ClassPid, SelectorBin) ->
    InstanceMethods = beamtalk_runtime_api:local_instance_methods(ClassPid),
    ClassMethods = beamtalk_runtime_api:local_class_methods(ClassPid),
    AllMethods = InstanceMethods ++ ClassMethods,
    SelectorFound =
        case beamtalk_repl_errors:safe_to_existing_atom(SelectorBin) of
            {error, badarg} -> false;
            {ok, SelectorAtom} -> lists:member(SelectorAtom, AllMethods)
        end,
    case SelectorFound of
        true ->
            ok;
        false ->
            {error,
                beamtalk_repl_errors:make(
                    not_found,
                    ClassAtom,
                    iolist_to_binary([
                        <<"Selector '">>, SelectorBin, <<"' not found on ">>, ClassBin
                    ]),
                    iolist_to_binary([
                        <<"Use :help ">>, ClassBin, <<" to see available selectors.">>
                    ])
                )}
    end.

-doc """
Return the value if it is a non-empty binary, otherwise return undefined.
Used to normalise optional params so that "" is treated the same as absent.
""".
-spec nonempty_or_undefined(binary() | undefined) -> binary() | undefined.
nonempty_or_undefined(<<>>) -> undefined;
nonempty_or_undefined(undefined) -> undefined;
nonempty_or_undefined(Bin) when is_binary(Bin) -> Bin.

%%% Test op helpers

-doc """
Execute a test run and encode the result.

When ClassName is undefined, runs all discovered TestCase subclasses.
When ClassName is a binary, runs tests for that specific class.
""".
-spec run_test_op(binary() | undefined) -> beamtalk_repl_ops:op_result().
run_test_op(undefined) ->
    try
        TestResult = beamtalk_test_runner:run_all(0),
        {test_results, TestResult}
    catch
        error:#{error := #beamtalk_error{} = Err} ->
            {error, Err};
        _Class:Reason ->
            ?LOG_ERROR("test-all op failed: ~p", [Reason], #{domain => [beamtalk, runtime]}),
            {error,
                beamtalk_repl_errors:make(
                    runtime_error,
                    'TestRunner',
                    iolist_to_binary(io_lib:format("Test run failed: ~p", [Reason]))
                )}
    end;
run_test_op(ClassName) when is_binary(ClassName) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassName) of
        {error, badarg} ->
            {error, make_class_not_found_error(ClassName)};
        {ok, ClassAtom} ->
            try
                TestResult = beamtalk_test_runner:run_class_by_name(ClassAtom),
                {test_results, TestResult}
            catch
                error:#{error := #beamtalk_error{} = Err} ->
                    {error, Err};
                _Class:Reason ->
                    ?LOG_ERROR("test op failed for ~s: ~p", [ClassName, Reason], #{
                        domain => [beamtalk, runtime]
                    }),
                    {error,
                        beamtalk_repl_errors:make(
                            runtime_error,
                            'TestRunner',
                            iolist_to_binary(
                                io_lib:format("Test run failed for ~s: ~p", [ClassName, Reason])
                            )
                        )}
            end
    end.

-doc """
Execute a file-scoped test run and return the result term.

Discovers all TestCase subclasses whose beamtalk_source attribute matches
FilePath (by path suffix) and runs them. Returns an aggregated TestResult.
""".
-spec run_test_op_file(binary()) -> beamtalk_repl_ops:op_result().
run_test_op_file(FilePath) ->
    try
        TestResult = beamtalk_test_runner:run_file(FilePath),
        {test_results, TestResult}
    catch
        error:#{error := #beamtalk_error{} = Err} ->
            {error, Err};
        _Class:Reason ->
            ?LOG_ERROR("test file op failed for ~s: ~p", [FilePath, Reason], #{
                domain => [beamtalk, runtime]
            }),
            {error,
                beamtalk_repl_errors:make(
                    runtime_error,
                    'TestRunner',
                    iolist_to_binary(
                        io_lib:format("Test run failed for file ~s: ~p", [FilePath, Reason])
                    )
                )}
    end.

%%% Internal helpers

-spec base_protocol_response(term()) -> map().
base_protocol_response(Msg) ->
    Id = beamtalk_repl_protocol:get_id(Msg),
    Session = beamtalk_repl_protocol:get_session(Msg),
    M0 = #{},
    M1 =
        case Id of
            undefined -> M0;
            _ -> M0#{<<"id">> => Id}
        end,
    case Session of
        undefined -> M1;
        _ -> M1#{<<"session">> => Session}
    end.

-spec get_completions(binary()) -> [binary()].
get_completions(<<>>) ->
    [];
get_completions(Prefix) when is_binary(Prefix) ->
    PrefixStr = binary_to_list(Prefix),
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    %% ADR 0071 Phase 5: Filter internal classes from cross-package completions.
    %% The REPL runs in an implicit package (nil), so internal classes from any
    %% named package (e.g. stdlib) are excluded. Internal classes with no package
    %% (user-loaded) are kept since they share the REPL's nil package.
    ClassNames = lists:filtermap(
        fun(Pid) ->
            try
                Name = beamtalk_runtime_api:class_name(Pid),
                case is_cross_package_internal(Pid) of
                    true -> false;
                    false -> {true, atom_to_binary(Name, utf8)}
                end
            catch
                _:_ -> false
            end
        end,
        ClassPids
    ),
    [Name || Name <- ClassNames, binary:match(Name, Prefix) =:= {0, byte_size(Prefix)}] ++
        [
            atom_to_binary(B, utf8)
         || B <-
                try
                    beamtalk_workspace_config:binding_names()
                catch
                    _:_ -> []
                end,
            binary:match(atom_to_binary(B, utf8), Prefix) =:= {0, byte_size(Prefix)}
        ] ++
        [
            Kw
         || Kw <- builtin_keywords(),
            binary:match(Kw, Prefix) =:= {0, byte_size(Prefix)},
            PrefixStr =/= ""
        ].

-doc """
Context-aware completion: parses the line to find a receiver and returns
matching method selectors (BT-783).  Falls back to get_completions/1 when
no receiver is detected.  Wrapper with no binding context.
""".
-spec get_context_completions(binary()) -> [binary()].
get_context_completions(Line) ->
    get_context_completions(Line, #{}).

-doc "Context-aware completion with session bindings for variable lookup.".
-spec get_context_completions(binary(), map()) -> [binary()].
get_context_completions(<<>>, _Bindings) ->
    [];
get_context_completions(Line, Bindings) when is_binary(Line) ->
    case parse_receiver_and_prefix(Line) of
        {undefined, Prefix} ->
            %% No receiver — use standard prefix completion
            get_completions(Prefix);
        {expression, ReceiverExpr, Prefix} ->
            %% Multi-token receiver expression — resolve via chain type inference (BT-1006)
            case resolve_chain_type(ReceiverExpr, Bindings) of
                {ok, ClassName, class} -> complete_class_methods(ClassName, Prefix);
                {ok, ClassName, instance} -> complete_instance_methods(ClassName, Prefix);
                undefined -> []
            end;
        {Receiver, <<>>} ->
            %% Empty prefix with receiver (e.g., "Integer ") — return all methods
            MethodSelectors = get_methods_for_receiver(Receiver, Bindings),
            lists:usort([atom_to_binary(S, utf8) || S <- MethodSelectors]);
        {Receiver, Prefix} ->
            %% Receiver with prefix — look up methods filtered by prefix
            MethodSelectors = get_methods_for_receiver(Receiver, Bindings),
            lists:usort([
                atom_to_binary(S, utf8)
             || S <- MethodSelectors,
                binary:match(atom_to_binary(S, utf8), Prefix) =:= {0, byte_size(Prefix)}
            ])
    end.

-doc """
Compute parse-only diagnostics for an editor buffer (BT-2556).

`Code` is the full buffer source; `Mode` selects the parse grammar
(`<<"expression">>` for a top-level script, `<<"method">>` for a bare method
body). Delegates to `beamtalk_compiler:diagnostics/2` — the side-effect-free
parse + semantic-check path (the Rust port's `diagnostics` command): it never
generates code, installs a module, mutates the image, or appends to the
ChangeLog. Each diagnostic is a map with `message`, `severity`, and byte-offset
`start`/`end` keys.

An empty buffer short-circuits to `[]` (nothing to diagnose). A compiler-port
failure (`{error, _}` — port down / timed out) also degrades to `[]`: diagnostics
are advisory and fire on every keystroke, so a transient port hiccup must not
surface an error to the editor.
""".
-spec diagnostics_for(binary(), binary()) -> [map()].
diagnostics_for(<<>>, _Mode) ->
    [];
diagnostics_for(Code, Mode) when is_binary(Code), is_binary(Mode) ->
    case beamtalk_compiler:diagnostics(Code, Mode) of
        {ok, Diagnostics} when is_list(Diagnostics) -> Diagnostics;
        {error, _Reason} -> []
    end;
%% A non-binary `code`/`mode` (a raw TCP/MCP client can send either as a JSON
%% number/bool, not just the LiveView surface) degrades to `[]` rather than
%% crashing the session — diagnostics are advisory and fire on every keystroke,
%% so this matches the `{error, _}` degradation above and keeps the `binary()`
%% spec honest at the Erlang boundary (BT-2569).
diagnostics_for(_, _) ->
    [].

-doc """
Normalise the `diagnostics` `mode` param at the Erlang op boundary (BT-2572).

Mirrors the Elixir `BtAttach.Facade` (`invoke(:diagnostics, ...)`): a binary
`mode` of <<"method">> is preserved; any other binary (e.g. <<"foo">>) is
normalised to the safe default <<"expression">>. A non-binary `mode` is passed
through unchanged so `diagnostics_for/2`'s catch-all still degrades it to `[]`
(the existing BT-2569 behaviour) rather than silently treating it as expression
mode.
""".
-spec normalize_diagnostics_mode(term()) -> term().
normalize_diagnostics_mode(<<"method">>) ->
    <<"method">>;
normalize_diagnostics_mode(Mode) when is_binary(Mode) ->
    <<"expression">>;
normalize_diagnostics_mode(Mode) ->
    Mode.

-doc """
Resolve hover documentation for the hovered token (BT-2555).

`Code` is the editor line up to and including the hovered token; we reuse
`parse_receiver_and_prefix/1` (the completion parser) so the trailing token is
the hovered word and the token before it (if any) is its receiver:

  * bare class name (`Counter`)            → that class's live docs
  * `Receiver selector` (`42 factorial`)   → that method's live docs
  * multi-token receiver expression        → class-name hover only (full
                                             receiver inference is future work)

Returns the formatted markdown binary, or `<<>>` when nothing resolves (the
client shows no tooltip): a missing class/selector is a `{error, _}` return that
degrades to `<<>>`, since hover is advisory. This does not *catch* exceptions —
an unexpected throw in `beamtalk_repl_docs` propagates and surfaces to the
`rpc`-attached client as `{badrpc, _}`, which the Elixir layer maps to an empty
hover, so the system still degrades cleanly.
""".
-spec hover_docs(binary(), map()) -> binary().
hover_docs(Code, Bindings) ->
    case parse_receiver_and_prefix(Code) of
        {expression, _ReceiverExpr, Word} ->
            %% Multi-token receiver: only offer hover when the hovered token is
            %% itself a class name. Selector hover on an inferred expression
            %% receiver is deferred (full receiver inference).
            hover_class(Word, Bindings);
        {_Receiver, <<>>} ->
            %% Empty trailing token (line ends in whitespace). Defensive: the JS
            %% `wordAt` always ends `code` at the last char of an identifier, so
            %% this is unreachable from the client path — nothing to hover.
            <<>>;
        {undefined, Word} ->
            hover_class(Word, Bindings);
        {Receiver, Word} ->
            hover_method(Receiver, Word, Bindings)
    end.

-doc "Hover docs for a bare token that is a class name; `<<>>` otherwise.".
-spec hover_class(binary(), map()) -> binary().
hover_class(<<>>, _Bindings) ->
    <<>>;
hover_class(Word, Bindings) ->
    case classify_receiver(Word, Bindings) of
        {class, ClassName} ->
            hover_format(fun() -> beamtalk_repl_docs:format_class_docs(ClassName) end);
        %% A bare lowercase token (a binding/instance) is intentionally NOT
        %% hovered in v1: hover targets class names + explicit receiver→selector.
        _ ->
            <<>>
    end.

-doc """
Hover docs for a `Receiver selector` pair, picking the instance-side or
class-side doc lookup from how the receiver classifies (mirroring how
`complete` offers class-side vs instance-side selectors for a receiver).
""".
-spec hover_method(binary(), binary(), map()) -> binary().
hover_method(Receiver, Selector, Bindings) ->
    case classify_receiver(Receiver, Bindings) of
        {class, ClassName} ->
            hover_format(fun() ->
                beamtalk_repl_docs:format_method_doc_class_side(ClassName, Selector)
            end);
        {instance, ClassName} ->
            hover_format(fun() ->
                beamtalk_repl_docs:format_method_doc(ClassName, Selector)
            end);
        undefined ->
            <<>>
    end.

-doc "Run a `{ok, Bin} | {error, _}` doc lookup, mapping anything but ok to `<<>>`.".
-spec hover_format(fun(() -> {ok, binary()} | {error, term()})) -> binary().
hover_format(Lookup) ->
    case Lookup() of
        {ok, Docs} when is_binary(Docs) -> Docs;
        _ -> <<>>
    end.

-doc """
Parse the line up to the cursor into a receiver and prefix.

Returns:
  {undefined, Prefix}                — no receiver (bare prefix, e.g. <<"s">>)
  {ReceiverToken, Prefix}            — single-token receiver (e.g. <<"Integer">>, <<"42">>)
  {expression, ReceiverExpr, Prefix} — multi-token receiver expression (e.g. <<"\"hello\" size">>) (BT-1006)

Examples:
  <<"Integer s">>       → {<<"Integer">>, <<"s">>}
  <<"Integer ">>        → {<<"Integer">>, <<>>}
  <<"42 s">>            → {<<"42">>, <<"s">>}
  <<"\"hello\" size c">>→ {expression, <<"\"hello\" size">>, <<"c">>}
  <<"s">>               → {undefined, <<"s">>}
  <<>>                  → {undefined, <<>>}
""".
-spec parse_receiver_and_prefix(binary()) ->
    {binary() | undefined, binary()} | {expression, binary(), binary()}.
parse_receiver_and_prefix(<<>>) ->
    {undefined, <<>>};
parse_receiver_and_prefix(Line) when is_binary(Line) ->
    Str = binary_to_list(Line),
    RevStr = lists:reverse(Str),
    %% Extract trailing identifier characters (the completion prefix)
    {PrefixCharsRev, Rest} = lists:splitwith(fun is_identifier_char/1, RevStr),
    Prefix = list_to_binary(lists:reverse(PrefixCharsRev)),
    %% Skip whitespace before the prefix
    {SpaceChars, ReceiverPartRev} = lists:splitwith(
        fun(C) -> C =:= $\s orelse C =:= $\t end, Rest
    ),
    case SpaceChars of
        [] ->
            %% No space before prefix — single token, no receiver
            {undefined, Prefix};
        _ ->
            %% Extract the token immediately before the space (the receiver)
            {ReceiverCharsRev, Tail} = lists:splitwith(
                fun(C) -> C =/= $\s andalso C =/= $\t end, ReceiverPartRev
            ),
            case ReceiverCharsRev of
                [] ->
                    {undefined, Prefix};
                _ ->
                    ReceiverToken = list_to_binary(lists:reverse(ReceiverCharsRev)),
                    %% Check if Tail contains non-whitespace — only then is it multi-token.
                    %% Leading indentation (e.g. "  Integer s") must not trigger the
                    %% expression path; Tail would be all-whitespace in that case.
                    case
                        lists:any(
                            fun(C) -> C =/= $\s andalso C =/= $\t end, Tail
                        )
                    of
                        false ->
                            %% Single-token receiver (possibly with leading whitespace)
                            {ReceiverToken, Prefix};
                        true ->
                            %% Multi-token receiver expression (BT-1006)
                            ReceiverExpr = list_to_binary(lists:reverse(ReceiverPartRev)),
                            {expression, ReceiverExpr, Prefix}
                    end
            end
    end.

-spec is_identifier_char(char()) -> boolean().
is_identifier_char(C) ->
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $_ orelse
        %% Colons are identifier chars so keyword selectors like `ifTrue:` and
        %% `ifTrue:ifFalse:` complete as a unit.  Must stay in sync with
        %% word_start in crates/beamtalk-cli/src/commands/repl/helper.rs.
        C =:= $: orelse
        %% BT-1659: @ is an identifier char so `json@Parser` is treated as a
        %% single token for completions and receiver parsing.
        C =:= $@.

%%% Chain resolution (BT-1006)

-doc """
Parse a binary expression into a receiver token and a list of unary selectors.

Accepts only simple unary send chains: whitespace-separated tokens where the first
is a receiver (literal, class name, or variable) and the rest are unary selectors.
Returns `error` for keyword sends mid-chain, parenthesised subexpressions, or `>>`.

Examples:
  <<"\"hello\" size">>    → {ok, <<"\"hello\"">>, [size]}
  <<"counter getValue">>  → {ok, <<"counter">>, [getValue]}
  <<"\"hello\" size abs">>→ {ok, <<"\"hello\"">>, [size, abs]}
  <<"inject: 0 into:">>   → error  (keyword send)
  <<"(myList size)">>     → error  (paren)
""".
-spec tokenise_send_chain(binary()) -> {ok, binary(), [atom()]} | error.
tokenise_send_chain(<<>>) ->
    error;
tokenise_send_chain(Expr) when is_binary(Expr) ->
    Parts = [list_to_binary(T) || T <- string:tokens(binary_to_list(Expr), " \t")],
    case Parts of
        [] ->
            error;
        [_] ->
            %% Single token — no sends, nothing to chain-walk
            error;
        [ReceiverToken | Selectors] ->
            case validate_chain_tokens(ReceiverToken, Selectors) of
                ok ->
                    %% Use binary_to_existing_atom to avoid atom table exhaustion from
                    %% user input.  Valid selectors are already loaded as method atoms;
                    %% unknown selectors mean the chain will break anyway.
                    try
                        SelectorAtoms = [binary_to_existing_atom(S, utf8) || S <- Selectors],
                        {ok, ReceiverToken, SelectorAtoms}
                    catch
                        error:badarg -> error
                    end;
                error ->
                    error
            end
    end.

-spec validate_chain_tokens(binary(), [binary()]) -> ok | error.
validate_chain_tokens(ReceiverToken, Selectors) ->
    AllTokens = [ReceiverToken | Selectors],
    HasInvalid = lists:any(
        fun(T) ->
            binary:match(T, <<"(">>) =/= nomatch orelse
                binary:match(T, <<")">>) =/= nomatch orelse
                binary:match(T, <<">>">>) =/= nomatch
        end,
        AllTokens
    ),
    case HasInvalid of
        true ->
            error;
        false ->
            case lists:all(fun is_valid_unary_selector/1, Selectors) of
                true -> ok;
                false -> error
            end
    end.

-doc """
A valid unary selector starts with a letter or underscore and contains no colon.
""".
-spec is_valid_unary_selector(binary()) -> boolean().
is_valid_unary_selector(<<>>) ->
    false;
is_valid_unary_selector(<<H, _/binary>> = Sel) when
    (H >= $a andalso H =< $z) orelse (H >= $A andalso H =< $Z) orelse H =:= $_
->
    binary:match(Sel, <<":">>) =:= nomatch;
is_valid_unary_selector(_) ->
    false.

-doc """
Returns true if the first character of a token is a binary selector character.

Binary selector characters: + - * / < > = ~ % & ? , \
These are the same characters recognised by the Beamtalk lexer.
""".
-spec is_binary_selector_token(binary()) -> boolean().
is_binary_selector_token(<<H, _/binary>>) ->
    lists:member(H, "+-*/<>=~%&?,\\");
is_binary_selector_token(<<>>) ->
    false.

-doc """
Returns true if the token contains characters that make it un-parseable
as part of a simple chain (parentheses or the `>>` method-reference operator).
""".
-spec has_invalid_chain_chars(binary()) -> boolean().
has_invalid_chain_chars(T) ->
    binary:match(T, <<"(">>) =/= nomatch orelse
        binary:match(T, <<")">>) =/= nomatch orelse
        binary:match(T, <<">>">>) =/= nomatch.

-doc """
Parse a whitespace-separated expression into a receiver token and a list
of mixed unary/binary hops (BT-1071).

Extends `tokenise_send_chain/1` to handle binary message sends mid-chain:
each binary operator token consumes the following token as its argument.
Binary hops are tagged `{binary, Selector}` and unary hops `{unary, Selector}`.

Returns `error` for:
- Empty or single-token expressions (no chain to walk)
- Tokens containing parentheses or `>>` (complex sub-expressions)
- Binary operators with no following argument token
- Keyword sends (tokens containing `:`)

Examples:
  <<"counter value + 1">>  → {ok, <<"counter">>, [{unary, value}, {binary, '+'}]}
  <<"\"foo\" , \"bar\"">>  → {ok, <<"\"foo\"">>, [{binary, ','}]}
  <<"myList size + offset">>→ {ok, <<"myList">>, [{unary, size}, {binary, '+'}]}
  <<"x + 1 * 2">>          → {ok, <<"x">>, [{binary, '+'}, {binary, '*'}]}
""".
-type chain_hop() :: {unary, atom()} | {binary, atom()}.
-spec tokenise_binary_chain(binary()) -> {ok, binary(), [chain_hop()]} | error.
tokenise_binary_chain(<<>>) ->
    error;
tokenise_binary_chain(Expr) when is_binary(Expr) ->
    Parts = [list_to_binary(T) || T <- string:tokens(binary_to_list(Expr), " \t")],
    case Parts of
        [] ->
            error;
        [_] ->
            %% Single token — no sends to walk
            error;
        [ReceiverToken | HopTokens] ->
            case has_invalid_chain_chars(ReceiverToken) of
                true ->
                    error;
                false ->
                    case parse_binary_hops(HopTokens) of
                        error -> error;
                        {ok, []} -> error;
                        {ok, Hops} -> {ok, ReceiverToken, Hops}
                    end
            end
    end.

-spec parse_binary_hops([binary()]) -> {ok, [chain_hop()]} | error.
parse_binary_hops([]) ->
    {ok, []};
parse_binary_hops([Token | Rest]) ->
    case has_invalid_chain_chars(Token) of
        true ->
            error;
        false ->
            case is_binary_selector_token(Token) of
                true ->
                    %% Binary op: consume the following token as the argument
                    case Rest of
                        [] ->
                            %% Binary operator with no argument — malformed
                            error;
                        [ArgToken | Rest2] ->
                            case
                                has_invalid_chain_chars(ArgToken) orelse
                                    binary:match(ArgToken, <<":">>) =/= nomatch
                            of
                                true ->
                                    error;
                                false ->
                                    try
                                        Sel = binary_to_existing_atom(Token, utf8),
                                        case parse_binary_hops(Rest2) of
                                            {ok, MoreHops} -> {ok, [{binary, Sel} | MoreHops]};
                                            error -> error
                                        end
                                    catch
                                        error:badarg -> error
                                    end
                            end
                    end;
                false ->
                    %% Must be a valid unary selector (no colon, starts with letter/_)
                    case is_valid_unary_selector(Token) of
                        true ->
                            try
                                Sel = binary_to_existing_atom(Token, utf8),
                                case parse_binary_hops(Rest) of
                                    {ok, MoreHops} -> {ok, [{unary, Sel} | MoreHops]};
                                    error -> error
                                end
                            catch
                                error:badarg -> error
                            end;
                        false ->
                            %% Keyword send or other unrecognised token
                            error
                    end
            end
    end.

-doc """
Walk an instance-side send chain containing mixed unary and binary hops (BT-1071).

For each `{unary, Sel}` hop: looks up `get_method_return_type(ClassName, Sel)`.
For each `{binary, Sel}` hop: looks up `get_method_return_type(ClassName, Sel)`.
Returns `undefined` (graceful fallback) when any hop lacks a return-type annotation.
""".
-spec walk_mixed_chain(atom(), [chain_hop()]) -> {ok, atom(), instance | class} | undefined.
walk_mixed_chain(ClassName, Hops) ->
    walk_mixed_chain(ClassName, Hops, 0).

-spec walk_mixed_chain(atom(), [chain_hop()], non_neg_integer()) ->
    {ok, atom(), instance | class} | undefined.
walk_mixed_chain(ClassName, [], _Depth) ->
    {ok, ClassName, instance};
walk_mixed_chain(_ClassName, _Hops, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    undefined;
walk_mixed_chain(ClassName, [{unary, class} | Rest], _Depth) ->
    %% `instance class` transitions to the class side.
    walk_mixed_chain_class(ClassName, Rest);
walk_mixed_chain(ClassName, [{_Kind, Sel} | Rest], Depth) ->
    case beamtalk_class_registry:get_method_return_type(ClassName, Sel) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_mixed_chain(NextClass, Rest, Depth + 1);
        %% ADR 0068: Tagged tuple return types (type_param, generic) cannot be
        %% resolved without caller annotation context — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

-doc """
Walk a chain starting from the class side with mixed unary/binary hops (BT-1071).

The first hop uses `get_class_method_return_type`; subsequent hops transition to
instance-side via `walk_mixed_chain/3`.
""".
-spec walk_mixed_chain_class(atom(), [chain_hop()]) -> {ok, atom(), instance | class} | undefined.
walk_mixed_chain_class(ClassName, []) ->
    {ok, ClassName, class};
walk_mixed_chain_class(ClassName, [{unary, class} | Rest]) ->
    %% `ClassName class` → metaclass; stay on the class side for completions.
    walk_mixed_chain_class(ClassName, Rest);
walk_mixed_chain_class(ClassName, [{_Kind, Sel} | Rest]) ->
    case beamtalk_class_registry:get_class_method_return_type(ClassName, Sel) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_mixed_chain(NextClass, Rest, 1);
        %% ADR 0068: Tagged tuple return types — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

-doc """
Resolve the type at the end of a send chain using static return-type metadata.

Tokenises the expression, classifies the receiver, then walks the chain by
looking up each send's return type in `method_return_types` on the class registry.

When the unary tokenizer fails, tries the binary/mixed tokenizer (BT-1071).
When both tokenizers fail (e.g. parenthesised subexpressions, keyword sends
mid-chain), falls back to compiler-based type resolution (BT-1068, ADR 0045 Option C).
""".
-spec resolve_chain_type(binary(), map()) -> {ok, atom(), instance | class} | undefined.
resolve_chain_type(Expr, Bindings) ->
    case tokenise_send_chain(Expr) of
        {ok, ReceiverToken, Selectors} ->
            case classify_receiver(ReceiverToken, Bindings) of
                {instance, ClassName} -> walk_chain(ClassName, Selectors);
                {class, ClassName} -> walk_chain_class(ClassName, Selectors);
                undefined -> undefined
            end;
        error ->
            %% BT-1071: try binary/mixed chain tokenizer before the compiler port.
            case tokenise_binary_chain(Expr) of
                {ok, ReceiverToken, Hops} ->
                    case classify_receiver(ReceiverToken, Bindings) of
                        {instance, ClassName} ->
                            walk_mixed_chain(ClassName, Hops);
                        {class, ClassName} ->
                            walk_mixed_chain_class(ClassName, Hops);
                        undefined ->
                            %% Receiver parsed but not classifiable — fall back to compiler.
                            resolve_type_via_compiler(Expr)
                    end;
                error ->
                    %% BT-1068: tokeniser can't parse the expression — try the compiler port.
                    resolve_type_via_compiler(Expr)
            end
    end.

-doc """
Compiler-based type resolution fallback for complex expressions (BT-1068).

Sends the expression to the Rust compiler via the port. The compiler parses
it fully, runs type inference, and returns the type of the last expression.
Falls back to `undefined' if the compiler is unavailable or the type is unknown.
""".
-spec resolve_type_via_compiler(binary()) -> {ok, atom(), instance | class} | undefined.
resolve_type_via_compiler(Expr) ->
    try beamtalk_compiler:resolve_completion_type(Expr) of
        {ok, ClassName} -> {ok, ClassName, instance};
        {error, type_unknown} -> undefined
    catch
        _:_ -> undefined
    end.

-doc """
Walk an instance-side send chain, following method return types at each hop.

Returns `{ok, FinalClassName}` when every hop has a known return type,
or `undefined` when any hop breaks (annotation absent or non-Simple).
""".
-spec walk_chain(atom(), [atom()]) -> {ok, atom(), instance | class} | undefined.
walk_chain(ClassName, Selectors) ->
    walk_chain(ClassName, Selectors, 0).

-spec walk_chain(atom(), [atom()], non_neg_integer()) -> {ok, atom(), instance | class} | undefined.
walk_chain(ClassName, [], _Depth) ->
    {ok, ClassName, instance};
walk_chain(_ClassName, _Selectors, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    undefined;
walk_chain(ClassName, [class | Rest], _Depth) ->
    %% `instance class` transitions to the class side — offer class methods.
    walk_chain_class(ClassName, Rest);
walk_chain(ClassName, [Selector | Rest], Depth) ->
    case beamtalk_runtime_api:get_method_return_type(ClassName, Selector) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_chain(NextClass, Rest, Depth + 1);
        %% ADR 0068: Tagged tuple return types — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

-doc """
Walk a chain starting from the class side.

The first hop uses `class_method_return_types`; subsequent hops transition to the
instance side via `walk_chain/2`.

Special case: `class` is an instance method on ProtoObject (not annotated with a
return type). When a class object receives `class`, it returns its metaclass, which
has the same class-side methods for completion purposes. We stay on the class side
rather than failing the chain.
""".
-spec walk_chain_class(atom(), [atom()]) -> {ok, atom(), instance | class} | undefined.
walk_chain_class(ClassName, []) ->
    {ok, ClassName, class};
walk_chain_class(ClassName, [class | Rest]) ->
    %% `ClassName class` → metaclass; for completions treat as still on the class side.
    walk_chain_class(ClassName, Rest);
walk_chain_class(ClassName, [Selector | Rest]) ->
    case beamtalk_runtime_api:get_class_method_return_type(ClassName, Selector) of
        {ok, NextClass} when is_atom(NextClass) ->
            walk_chain(NextClass, Rest, 1);
        %% ADR 0068: Tagged tuple return types — graceful fallback.
        {ok, _TaggedTuple} ->
            undefined;
        {error, not_found} ->
            undefined
    end.

-doc """
Remove methods that are internal Object protocol and should not appear
in user-facing completions. See ?COMPLETION_HIDDEN_METHODS.
""".
-spec filter_hidden_methods([atom()]) -> [atom()].
filter_hidden_methods(Selectors) ->
    Hidden = sets:from_list(?COMPLETION_HIDDEN_METHODS, [{version, 2}]),
    [S || S <- Selectors, not sets:is_element(S, Hidden)].

-doc """
Return all instance methods of a class filtered by the given prefix.
ADR 0071 Phase 5: Also filters internal methods from cross-package classes.
""".
-spec complete_instance_methods(atom(), binary()) -> [binary()].
complete_instance_methods(ClassName, Prefix) ->
    MethodSelectors = filter_hidden_methods(collect_all_methods(ClassName, 0)),
    Filtered = filter_internal_methods(MethodSelectors, ClassName),
    filter_by_prefix(Filtered, Prefix).

-doc """
Return all class-side methods of a class filtered by the given prefix.

Includes class methods plus ProtoObject instance methods (e.g. `class`,
`respondsTo:`) since class objects are also objects.
ADR 0071 Phase 5: Also filters internal methods from cross-package classes.
""".
-spec complete_class_methods(atom(), binary()) -> [binary()].
complete_class_methods(ClassName, Prefix) ->
    ProtoObjMethods = filter_hidden_methods(collect_all_methods('ProtoObject', 0)),
    ClassMethods = collect_all_class_methods(ClassName, 0),
    %% Filter internal class methods from the target class
    FilteredClassMethods = filter_internal_methods(ClassMethods, ClassName, class),
    filter_by_prefix(FilteredClassMethods ++ ProtoObjMethods, Prefix).

-doc "Filter method selectors by a binary prefix, returning sorted binaries.".
-spec filter_by_prefix([atom()], binary()) -> [binary()].
filter_by_prefix(MethodSelectors, Prefix) ->
    All = [atom_to_binary(S, utf8) || S <- MethodSelectors],
    case Prefix of
        <<>> ->
            lists:usort(All);
        _ ->
            lists:usort([
                M
             || M <- All,
                binary:match(M, Prefix) =:= {0, byte_size(Prefix)}
            ])
    end.

-doc """
Get method selectors for a given receiver token.
For class-name receivers (uppercase), returns only class-side methods (via hierarchy
walk). Instance methods are excluded — they cannot be called on the class object.
For instance receivers (literals, bindings), returns instance methods.
""".
-spec get_methods_for_receiver(binary(), map()) -> [atom()].
get_methods_for_receiver(Receiver, Bindings) when is_binary(Receiver) ->
    case classify_receiver(Receiver, Bindings) of
        undefined ->
            [];
        {class, ClassName} ->
            %% Class-object receiver: class-side methods plus the ProtoObject instance
            %% methods that are genuinely callable on any object (including class objects).
            %% Most instance methods are excluded — they can't be called on the class object
            %% and would cause misleading completions that always error.
            %% ProtoObject methods (e.g. `class`) are included because class objects ARE
            %% objects and `ClassName class` is meaningful (returns the metaclass).
            ProtoObjMethods = filter_hidden_methods(collect_all_methods('ProtoObject', 0)),
            %% ADR 0071 Phase 5: Filter internal class methods from cross-package classes
            FilteredClassMethods = filter_internal_methods(
                collect_all_class_methods(ClassName, 0), ClassName, class
            ),
            FilteredClassMethods ++ ProtoObjMethods;
        {instance, ClassName} ->
            %% ADR 0071 Phase 5: Filter internal instance methods from cross-package classes
            Selectors = filter_hidden_methods(collect_all_methods(ClassName, 0)),
            filter_internal_methods(Selectors, ClassName)
    end.

-doc """
Classify a receiver token as a class object or instance, with optional binding lookup.
Returns {class, ClassName} for class-object receivers (uppercase class names),
{instance, ClassName} for instance receivers (literals, bindings), or undefined.
""".
-spec classify_receiver(binary(), map()) -> {class, atom()} | {instance, atom()} | undefined.
classify_receiver(<<>>, _Bindings) ->
    undefined;
classify_receiver(<<H, _/binary>> = Receiver, Bindings) when H >= $A, H =< $Z ->
    %% Starts with uppercase — first try class registry, then fall back to bindings.
    %% The fallback handles uppercase global bindings like `Transcript` that are
    %% workspace actors, not class names.
    case beamtalk_repl_errors:safe_to_existing_atom(Receiver) of
        {ok, ClassName} ->
            case
                try
                    beamtalk_runtime_api:whereis_class(ClassName)
                catch
                    _:_ -> undefined
                end
            of
                undefined ->
                    %% Not a class — check if it's a named binding (e.g. Transcript)
                    classify_by_binding(ClassName, Bindings);
                _Pid ->
                    {class, ClassName}
            end;
        {error, _} ->
            undefined
    end;
classify_receiver(<<H, _/binary>> = Receiver, _Bindings) when H >= $0, H =< $9 ->
    %% Only treat as Integer when the whole token is digits (guards against
    %% float literals like "3.14" being misclassified as Integer).
    case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Receiver)) of
        true ->
            case maybe_class('Integer') of
                undefined -> undefined;
                ClassName -> {instance, ClassName}
            end;
        false ->
            undefined
    end;
classify_receiver(<<$", _/binary>>, _Bindings) ->
    %% String literal — complete String instance methods
    case maybe_class('String') of
        undefined -> undefined;
        ClassName -> {instance, ClassName}
    end;
classify_receiver(Receiver, Bindings) ->
    %% BT-1659: Check for package-qualified class name (e.g. "json@Parser")
    case binary:match(Receiver, <<"@">>) of
        nomatch ->
            %% Lowercase identifier — look up in bindings to find the class
            case beamtalk_repl_errors:safe_to_existing_atom(Receiver) of
                {ok, VarAtom} -> classify_by_binding(VarAtom, Bindings);
                {error, _} -> undefined
            end;
        _ ->
            case resolve_qualified_class_name(Receiver) of
                {ok, ClassName} ->
                    case
                        try
                            beamtalk_runtime_api:whereis_class(ClassName)
                        catch
                            _:_ -> undefined
                        end
                    of
                        undefined -> undefined;
                        _Pid -> {class, ClassName}
                    end;
                {error, _} ->
                    undefined
            end
    end.

-doc """
Classify a receiver by looking it up in the bindings map.
Uses beamtalk_runtime_api:primitive_class_of/1 as the canonical type classifier,
which handles actors, tagged maps, primitives, symbols, blocks, nil, etc.
""".
-spec classify_by_binding(atom(), map()) -> {instance, atom()} | undefined.
classify_by_binding(VarAtom, Bindings) ->
    case maps:find(VarAtom, Bindings) of
        {ok, Value} ->
            ClassName = beamtalk_runtime_api:primitive_class_of(Value),
            case maybe_class(ClassName) of
                undefined -> undefined;
                _ -> {instance, ClassName}
            end;
        error ->
            undefined
    end.

-spec maybe_class(atom()) -> atom() | undefined.
maybe_class(ClassName) ->
    case
        try
            beamtalk_runtime_api:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined -> undefined;
        _Pid -> ClassName
    end.

-doc """
Get the session bindings map from a session PID.
Returns empty map if session is unavailable.
""".
-spec get_session_bindings(pid()) -> map().
get_session_bindings(SessionPid) ->
    try
        {ok, Bindings} = beamtalk_repl_shell:get_bindings(SessionPid),
        Bindings
    catch
        _:_ -> #{}
    end.

-doc """
Get workspace-level global bindings (e.g. Transcript, Beamtalk, Workspace).
Uses get_session_bindings/0 to include singletons as well as bind:as: entries.
Returns empty map if workspace interface is unavailable.
""".
-spec get_workspace_bindings() -> map().
get_workspace_bindings() ->
    try
        beamtalk_workspace_interface_primitives:get_session_bindings()
    catch
        _:_ -> #{}
    end.

-doc """
Collect all class-side method selectors for a class by walking the superclass chain.
Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
""".
-spec collect_all_class_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_class_methods(ClassName, Depth) ->
    collect_methods_with_fun(ClassName, Depth, fun beamtalk_runtime_api:local_class_methods/1).

-doc """
Collect all instance method selectors for a class by walking the superclass chain.
Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
""".
-spec collect_all_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_methods(ClassName, Depth) ->
    collect_methods_with_fun(ClassName, Depth, fun beamtalk_runtime_api:class_methods/1).

-doc "Walk the superclass chain collecting methods via a caller-supplied getter fun.".
-spec collect_methods_with_fun(atom(), non_neg_integer(), fun((pid()) -> [atom()])) -> [atom()].
collect_methods_with_fun(_ClassName, Depth, _Fun) when Depth > ?MAX_HIERARCHY_DEPTH ->
    [];
collect_methods_with_fun(ClassName, Depth, Fun) ->
    case
        try
            beamtalk_runtime_api:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined ->
            [];
        ClassPid ->
            LocalMethods =
                try
                    Fun(ClassPid)
                catch
                    _:_ -> []
                end,
            Superclass =
                try
                    beamtalk_runtime_api:superclass(ClassPid)
                catch
                    _:_ -> none
                end,
            InheritedMethods =
                case Superclass of
                    none -> [];
                    Super -> collect_methods_with_fun(Super, Depth + 1, Fun)
                end,
            LocalMethods ++ InheritedMethods
    end.

-doc """
ADR 0071 Phase 5: Check if a class is internal and belongs to a different
package than the REPL's implicit nil package. Returns true for internal classes
from named packages (e.g. stdlib). Returns false for public classes and for
internal classes with no package (user-loaded in REPL, same nil package).
""".
-spec is_cross_package_internal(pid()) -> boolean().
is_cross_package_internal(Pid) ->
    try
        case beamtalk_runtime_api:is_internal(Pid) of
            false ->
                false;
            true ->
                %% Internal class — check if it belongs to a named package
                %% using the ETS module table for fast lookup (avoids gen_server
                %% calls that can timeout on mock class processes in tests).
                ClassName = beamtalk_runtime_api:class_name(Pid),
                case beamtalk_class_metadata:lookup_module(ClassName) of
                    not_found ->
                        false;
                    {ok, Mod} ->
                        case extract_package_from_module_name(Mod) of
                            nil -> false;
                            _ -> true
                        end
                end
        end
    catch
        _:_ -> false
    end.

-doc """
ADR 0071 Phase 5: Filter internal methods from a list of method selectors.
Reads method visibility from __beamtalk_meta/0 and removes internal methods
when the class belongs to a different package than the REPL.

Walks the class hierarchy to also filter inherited internal methods from
ancestor classes. Checks method_info for instance side and class_method_info
for class side.

Uses the class hierarchy table (ETS) for module lookup to avoid gen_server
calls that can timeout on mock class processes in tests.
""".
-spec filter_internal_methods([atom()], atom()) -> [atom()].
filter_internal_methods(Selectors, ClassName) ->
    filter_internal_methods(Selectors, ClassName, instance).

-spec filter_internal_methods([atom()], atom(), instance | class) -> [atom()].
filter_internal_methods(Selectors, ClassName, Side) ->
    %% Collect all internal selectors from the class and its ancestors.
    InternalSet = collect_internal_selectors(ClassName, Side, 0),
    case maps:size(InternalSet) of
        0 -> Selectors;
        _ -> [S || S <- Selectors, not maps:is_key(S, InternalSet)]
    end.

-doc "Walk the hierarchy collecting internal selectors from cross-package classes.".
-spec collect_internal_selectors(atom(), instance | class, non_neg_integer()) -> map().
collect_internal_selectors(_ClassName, _Side, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    #{};
collect_internal_selectors(ClassName, Side, Depth) ->
    Local = collect_internal_selectors_for_class(ClassName, Side),
    Super =
        case
            try
                beamtalk_class_metadata:lookup_superclass(ClassName)
            catch
                _:_ -> not_found
            end
        of
            not_found -> #{};
            {ok, none} -> #{};
            {ok, SuperName} -> collect_internal_selectors(SuperName, Side, Depth + 1)
        end,
    maps:merge(Super, Local).

-doc "Get internal selectors from a single class's meta (if cross-package).".
-spec collect_internal_selectors_for_class(atom(), instance | class) -> map().
collect_internal_selectors_for_class(ClassName, Side) ->
    case
        try
            beamtalk_class_metadata:lookup_module(ClassName)
        catch
            _:_ -> not_found
        end
    of
        not_found ->
            #{};
        {ok, Mod} ->
            case extract_package_from_module_name(Mod) of
                nil ->
                    #{};
                _ ->
                    Meta = read_class_meta(Mod),
                    InfoKey =
                        case Side of
                            instance -> method_info;
                            class -> class_method_info
                        end,
                    MethodInfo = maps:get(InfoKey, Meta, #{}),
                    maps:filter(
                        fun
                            (_Sel, Info) when is_map(Info) ->
                                maps:get(visibility, Info, public) =:= internal;
                            (_Sel, _Info) ->
                                false
                        end,
                        MethodInfo
                    )
            end
    end.

-doc """
Extract the package name from a BEAM module name.
Returns a binary package name or nil.
""".
-spec extract_package_from_module_name(atom()) -> binary() | nil.
extract_package_from_module_name(ModuleName) when is_atom(ModuleName) ->
    ModStr = atom_to_list(ModuleName),
    case string:split(ModStr, "@", all) of
        %% Qualified module name: bt@{pkg}@{class}[...]
        ["bt", Pkg, _Class | _Rest] when Pkg =/= [] ->
            list_to_binary(Pkg);
        %% Unqualified or non-standard names (bt@foo) have no explicit package.
        _ ->
            nil
    end.

-doc """
Read __beamtalk_meta/0 from a compiled module.
Returns the meta map or #{} if unavailable.
""".
-spec read_class_meta(atom()) -> map().
read_class_meta(Module) ->
    case erlang:function_exported(Module, '__beamtalk_meta', 0) of
        true ->
            try Module:'__beamtalk_meta'() of
                M when is_map(M) -> M;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        false ->
            #{}
    end.

-spec builtin_keywords() -> [binary()].
builtin_keywords() ->
    [
        <<"self">>,
        <<"super">>,
        <<"true">>,
        <<"false">>,
        <<"nil">>,
        <<"ifTrue:">>,
        <<"ifFalse:">>,
        <<"ifTrue:ifFalse:">>,
        <<"whileTrue:">>,
        <<"timesRepeat:">>,
        <<"subclass:">>,
        <<"spawn">>,
        <<"new">>
    ].

-doc """
Describe available protocol operations.

The deprecated ops `docs`, `load-file`, `reload`, and `modules` were removed
in BT-2091 (protocol 2.0). See the module doc for migration guidance.
""".
-spec describe_ops() -> map().
describe_ops() ->
    BaseOps = base_ops(),
    %% Merge ops from other modules (dynamic discovery, BT-1622)
    PerfOps = beamtalk_repl_ops_perf:describe_ops(),
    %% BT-2239: structured navigation queries.
    NavOps = beamtalk_repl_ops_nav:describe_ops(),
    %% BT-2244: bulk class+method outline (`nav-symbols`).
    NavSymbolsOps = beamtalk_repl_ops_nav_symbols:describe_ops(),
    %% ADR 0095 / BT-2488: System Browser browse facade (four browse-* ops).
    BrowseOps = beamtalk_repl_ops_browse:describe_ops(),
    %% maps:merge(A, B) gives B's value when keys collide — we want the
    %% per-module op descriptors to win over BaseOps (the keysets are
    %% disjoint today, but this preserves the override direction the
    %% original `maps:merge(BaseOps, PerfOps)` line documented).
    maps:merge(
        BaseOps,
        maps:merge(maps:merge(maps:merge(PerfOps, NavOps), NavSymbolsOps), BrowseOps)
    ).

-doc "Core ops defined in this module and beamtalk_repl_server.".
-spec base_ops() -> map().
base_ops() ->
    #{
        <<"eval">> => #{<<"params">> => [<<"code">>], <<"optional">> => [<<"trace">>]},
        <<"stdin">> => #{<<"params">> => [<<"value">>]},
        <<"complete">> => #{<<"params">> => [<<"code">>], <<"optional">> => [<<"cursor">>]},
        %% BT-2555: live-image hover docs for the cockpit editors.
        <<"hover">> => #{<<"params">> => [<<"code">>]},
        %% BT-2556: parse-only diagnostics for the cockpit editors. BT-2569:
        %% optional `mode` (<<"expression">> | <<"method">>) selects the grammar.
        <<"diagnostics">> => #{
            <<"params">> => [<<"code">>], <<"optional">> => [<<"mode">>]
        },
        <<"test">> => #{<<"params">> => [], <<"optional">> => [<<"class">>, <<"file">>]},
        <<"test-all">> => #{<<"params">> => []},
        %% BT-2557: discover TestCase subclasses for the cockpit test-runner pane.
        <<"list-tests">> => #{<<"params">> => []},
        %% BT-2557: load the project's test/ files so the runner/browser see them.
        <<"load-tests">> => #{<<"params">> => []},
        <<"load-source">> => #{<<"params">> => [<<"source">>]},
        <<"load-project">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"path">>, <<"include_tests">>, <<"force">>]
        },
        <<"sessions">> => #{<<"params">> => []},
        <<"clone">> => #{<<"params">> => []},
        <<"close">> => #{<<"params">> => []},
        <<"interrupt">> => #{<<"params">> => []},
        <<"actors">> => #{<<"params">> => []},
        <<"inspect">> => #{<<"params">> => [<<"actor">>]},
        <<"kill">> => #{<<"params">> => [<<"actor">>]},
        <<"unload">> => #{<<"params">> => [<<"module">>]},
        <<"health">> => #{<<"params">> => []},
        <<"methods">> => #{<<"params">> => [<<"class">>]},
        <<"list-classes">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"filter">>]
        },
        <<"show-codegen">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"code">>, <<"class">>, <<"selector">>]
        },
        <<"describe">> => #{<<"params">> => []},
        <<"erlang-help">> => #{
            <<"params">> => [<<"module">>],
            <<"optional">> => [<<"function">>]
        },
        <<"erlang-complete">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"prefix">>, <<"module">>]
        },
        <<"shutdown">> => #{<<"params">> => [<<"cookie">>]}
    }.

-doc """
Return a list of method descriptors for a class by name (BT-1026).

Collects local instance methods and local class-side methods for the named
class. Returns an empty list if the class name is unknown or not loaded.
Each entry is a map with <<"name">>, <<"selector">>, and <<"side">> keys.
""".
-spec list_class_methods_for_ws(binary()) -> [map()].
list_class_methods_for_ws(ClassBin) when is_binary(ClassBin) ->
    %% BT-1659: Support package-qualified class names (e.g. "json@Parser")
    case resolve_qualified_class_name(ClassBin) of
        {error, badarg} ->
            [];
        {ok, ClassName} ->
            case beamtalk_runtime_api:whereis_class(ClassName) of
                undefined ->
                    [];
                Pid ->
                    InstanceSelectors = lists:sort(
                        beamtalk_runtime_api:local_instance_methods(Pid)
                    ),
                    ClassSelectors = lists:sort(beamtalk_runtime_api:local_class_methods(Pid)),
                    InstanceEntries = [
                        #{
                            <<"name">> => atom_to_binary(S, utf8),
                            <<"selector">> => atom_to_binary(S, utf8),
                            <<"side">> => <<"instance">>
                        }
                     || S <- InstanceSelectors
                    ],
                    ClassEntries = [
                        #{
                            <<"name">> => atom_to_binary(S, utf8),
                            <<"selector">> => atom_to_binary(S, utf8),
                            <<"side">> => <<"class">>
                        }
                     || S <- ClassSelectors
                    ],
                    InstanceEntries ++ ClassEntries
            end
    end.

-spec list_state_vars_for_ws(binary()) -> [binary()].
list_state_vars_for_ws(ClassBin) when is_binary(ClassBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassBin) of
        {error, badarg} ->
            [];
        {ok, ClassName} ->
            case beamtalk_runtime_api:whereis_class(ClassName) of
                undefined ->
                    [];
                Pid ->
                    IVars = beamtalk_runtime_api:instance_variables(Pid),
                    lists:sort([atom_to_binary(V, utf8) || V <- IVars])
            end
    end.

-doc """
Resolve a class name that may be package-qualified (ADR 0070 Phase 6, BT-1659).

Parses `<<"json@Parser">>` into `{ok, 'Parser'}` by looking up the
package-qualified BEAM module name (`bt@json@parser`) in the class registry.
Plain class names (`<<"Counter">>`) are resolved directly via `safe_to_existing_atom`.

Returns `{ok, ClassAtom}` on success, or `{error, badarg}` if the class
is not found (whether qualified or unqualified).
""".
-spec resolve_qualified_class_name(binary()) -> {ok, atom()} | {error, badarg}.
resolve_qualified_class_name(ClassBin) when is_binary(ClassBin) ->
    case binary:match(ClassBin, <<"@">>) of
        nomatch ->
            %% Plain class name — existing behavior
            beamtalk_repl_errors:safe_to_existing_atom(ClassBin);
        {Pos, _Len} ->
            %% Package-qualified: "json@Parser" → package=json, class=Parser
            PkgBin = binary:part(ClassBin, 0, Pos),
            ClassNameBin = binary:part(ClassBin, Pos + 1, byte_size(ClassBin) - Pos - 1),
            %% Convert class name to snake_case module name: bt@{pkg}@{snake_case}
            SnakeCase = camel_to_snake(binary_to_list(ClassNameBin)),
            ModNameStr = "bt@" ++ binary_to_list(PkgBin) ++ "@" ++ SnakeCase,
            try list_to_existing_atom(ModNameStr) of
                _ModAtom ->
                    %% Module name atom exists — now resolve the class name atom
                    beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin)
            catch
                error:badarg ->
                    {error, badarg}
            end
    end.

-doc """
CamelCase string to snake_case string conversion.
Mirrors beamtalk_primitive:camel_to_snake/1 for use in REPL ops.
""".
-spec camel_to_snake(string()) -> string().
camel_to_snake(Str) ->
    camel_to_snake(Str, false, []).

camel_to_snake([], _PrevWasLower, Acc) ->
    lists:reverse(Acc);
camel_to_snake([H | T], PrevWasLower, Acc) when H >= $A, H =< $Z ->
    Lower = H + 32,
    case PrevWasLower of
        true -> camel_to_snake(T, false, [Lower, $_ | Acc]);
        false -> camel_to_snake(T, false, [Lower | Acc])
    end;
camel_to_snake([H | T], _PrevWasLower, Acc) ->
    camel_to_snake(T, (H >= $a andalso H =< $z), [H | Acc]).

-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin = beamtalk_repl_protocol:to_binary(ClassName),
    beamtalk_repl_errors:make(
        class_not_found,
        'REPL',
        iolist_to_binary([<<"Unknown class: ">>, NameBin]),
        <<"Use Workspace classes to see loaded classes.">>
    ).

-doc """
Extract the first line of a binary string (up to the first newline).
BT-1404: Used to produce one-line class descriptions from full doc strings.
""".
-spec first_line(binary()) -> binary().
first_line(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<"\n">>) of
        [First | _] -> First;
        _ -> Bin
    end.

-doc """
BT-2091: Resolve the per-class source file path for list-classes.

Returns a binary path when workspace metadata knows the class's source,
or `null` when it does not (e.g. bootstrap-only classes that haven't been
file-loaded). `beamtalk_repl_modules:resolve_source_path/1` returns the
literal string `"unknown"` in that case; we normalise to JSON null.
""".
-spec list_classes_source_file(atom()) -> binary() | null.
list_classes_source_file(ModName) ->
    case beamtalk_repl_modules:resolve_source_path(ModName) of
        "unknown" -> null;
        Path -> list_to_binary(Path)
    end.

-doc """
BT-2091: Fetch a session module-tracker for list-classes if a session is
available, falling back to an empty tracker otherwise.

Editor / non-session callers don't have a SessionPid; in that case we
return an empty tracker so `get_actor_count/3` reports 0 without crashing.
""".
-spec list_classes_session_tracker(pid() | undefined) ->
    beamtalk_repl_modules:module_tracker().
list_classes_session_tracker(SessionPid) when is_pid(SessionPid) ->
    try beamtalk_repl_shell:get_module_tracker(SessionPid) of
        {ok, Tracker} -> Tracker
    catch
        exit:{noproc, _} -> #{};
        exit:{timeout, _} -> #{};
        _:_ -> #{}
    end;
list_classes_session_tracker(_) ->
    #{}.

-doc """
Validate and normalize the list-classes filter parameter.
Returns the filter in a form ready for should_include_class/4:
  undefined     → pass through
  <<"stdlib">>  → pass through
  <<"user">>    → pass through
  Other binary  → resolve to {superclass, Atom} or {error, FilterBin}
""".
-spec validate_list_classes_filter(term()) ->
    undefined | binary() | {superclass, atom()} | {error, binary()}.
validate_list_classes_filter(undefined) ->
    undefined;
validate_list_classes_filter(<<"stdlib">>) ->
    <<"stdlib">>;
validate_list_classes_filter(<<"user">>) ->
    <<"user">>;
validate_list_classes_filter(FilterBin) when is_binary(FilterBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(FilterBin) of
        {ok, Atom} -> {superclass, Atom};
        {error, badarg} -> {error, FilterBin}
    end;
validate_list_classes_filter(Other) ->
    {error, iolist_to_binary(io_lib:format("~p", [Other]))}.

-doc """
Filter predicate for list-classes op (BT-1404).
Uses the pre-validated filter from validate_list_classes_filter/1.
""".
-spec should_include_class(
    atom(),
    atom() | none,
    atom(),
    undefined | binary() | {superclass, atom()}
) -> boolean().
should_include_class(_Name, _Super, _ModName, undefined) ->
    true;
should_include_class(_Name, _Super, ModName, <<"stdlib">>) ->
    beamtalk_class_registry:is_stdlib_module(ModName);
should_include_class(_Name, _Super, ModName, <<"user">>) ->
    not beamtalk_class_registry:is_stdlib_module(ModName);
should_include_class(Name, _Super, _ModName, {superclass, FilterAtom}) ->
    beamtalk_runtime_api:inherits_from(Name, FilterAtom).

%%% ============================================================================
%%% Erlang FFI Help (BT-1852)
%%% ============================================================================

-doc "Build a \"not found\" error term for Erlang help lookups (BT-2402).".
-spec erlang_not_found_error(binary(), binary()) -> {error, #beamtalk_error{}}.
erlang_not_found_error(What, Hint) ->
    {error,
        beamtalk_repl_errors:make(
            does_not_understand, 'Erlang', iolist_to_binary([What, <<" not found">>]), Hint
        )}.
