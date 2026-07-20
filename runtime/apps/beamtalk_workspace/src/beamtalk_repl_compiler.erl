%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_compiler).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Beamtalk compilation pipeline for the REPL

Handles Beamtalk source → Core Erlang → BEAM bytecode compilation,
including expression compilation, file compilation, and codegen display.
Uses the beamtalk_compiler OTP application (ADR 0022) exclusively.
""".

-export([
    compile_expression/3,
    compile_expression/4,
    compile_expression_no_registration/3,
    compile_expression_trace/3,
    compile_file/4,
    compile_file/5,
    compile_for_codegen/3,
    compile_file_for_codegen/2,
    compile_for_method_reload/2,
    compile_method_reload/3,
    apply_module_name_override/2,
    apply_source_path/2,
    format_formatted_diagnostics/1,
    is_internal_key/1,
    known_vars/1,
    build_class_superclass_index/0,
    build_class_module_index/0,
    build_class_indexes/0
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    compile_expression_via_port/3,
    compile_expression_via_port/4,
    compile_file_via_port/4,
    compile_file_via_port/5,
    assemble_class_result/5,
    compile_trailing_expressions/2,
    compile_standard_expression/2,
    compile_file_core/4,
    format_core_error/1
]).
-endif.

%%% Public API

-doc "Compile a Beamtalk expression to bytecode.".
-spec compile_expression(string(), atom(), map()) ->
    {ok, binary(), term(), [binary()]}
    | {ok, class_definition, map(), [binary()]}
    | {ok, method_definition, map(), [binary()]}
    | {ok, protocol_definition, map(), [binary()]}
    | {ok, type_alias_definition, map(), [binary()]}
    | {error, term()}.
compile_expression(Expression, ModuleName, Bindings) ->
    compile_expression_via_port(Expression, ModuleName, Bindings).

-doc """
Compile a Beamtalk expression to bytecode, with earlier-turn type aliases.

`KnownTypeAliasSources' (ADR 0108 Phase 8, BT-2902) is the reparseable
`type Name = <expansion>' list from `beamtalk_repl_state:known_type_alias_sources/1'
— aliases declared in earlier turns of the same REPL session, forwarded so
`::' annotations in `Expression' resolve names this session already knows.
""".
-spec compile_expression(string(), atom(), map(), [binary()]) ->
    {ok, binary(), term(), [binary()]}
    | {ok, class_definition, map(), [binary()]}
    | {ok, method_definition, map(), [binary()]}
    | {ok, protocol_definition, map(), [binary()]}
    | {ok, type_alias_definition, map(), [binary()]}
    | {error, term()}.
compile_expression(Expression, ModuleName, Bindings, KnownTypeAliasSources) ->
    compile_expression_via_port(Expression, ModuleName, Bindings, KnownTypeAliasSources).

-doc """
Compile a Beamtalk expression the same way `compile_expression/3` does, but
without the `beamtalk_alias_xref` registration (or the bytecode compile that
triggers it) `compile_class_definition_result/2`/
`compile_protocol_definition_result/2` normally perform for a
`class_definition`/`protocol_definition` result (BT-2956).

CONTRACT WARNING: the `class_definition`/`method_definition`/
`protocol_definition`/`type_alias_definition` payload maps and warnings
lists returned here are always empty (`#{}`/`[]`), never the real compiled
payload. That is safe ONLY because the current, sole caller —
`beamtalk_repl_eval:eval_with_self/2` (`evaluate:`) — always rejects those
four result shapes outright and never inspects the payload; skipping
alias_xref registration and the bytecode compile that would normally
populate them is pure side-effect avoidance for a result about to be
discarded. Do NOT reuse this function for a new caller that reads the
payload of a definition-shaped result — call `compile_expression/3,4`
instead, or extend this function's registration behaviour first.
""".
-spec compile_expression_no_registration(string(), atom(), map()) ->
    {ok, binary(), term(), [binary()]}
    | {ok, class_definition, map(), [binary()]}
    | {ok, method_definition, map(), [binary()]}
    | {ok, protocol_definition, map(), [binary()]}
    | {ok, type_alias_definition, map(), [binary()]}
    | {error, term()}.
compile_expression_no_registration(Expression, ModuleName, Bindings) ->
    SourceBin = list_to_binary(Expression),
    ModNameBin = atom_to_binary(ModuleName, utf8),
    KnownVars = known_vars(Bindings),
    CompileOpts = add_class_indexes(#{}),
    wrap_compiler_errors(
        fun() ->
            case
                beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars, CompileOpts)
            of
                {ok, class_definition, _ClassInfo} ->
                    {ok, class_definition, #{}, []};
                {ok, method_definition, _MethodInfo} ->
                    {ok, method_definition, #{}, []};
                {ok, protocol_definition, _ProtocolInfo} ->
                    {ok, protocol_definition, #{}, []};
                {ok, type_alias_definition, _AliasInfo} ->
                    {ok, type_alias_definition, #{}, []};
                {ok, CoreErlang, Warnings} ->
                    compile_standard_expression(CoreErlang, Warnings);
                {error, Diagnostics} ->
                    {error, Diagnostics}
            end
        end,
        direct
    ).

-doc """
Compile a Beamtalk expression in trace mode (BT-1238).

Like `compile_expression/3' but the generated module's `eval/1' returns
`{[{<<"src0">>, Val0}, ...], FinalState}' — one step per top-level statement.
""".
-spec compile_expression_trace(string(), atom(), map()) ->
    {ok, binary(), term(), [binary()]} | {error, term()}.
compile_expression_trace(Expression, ModuleName, Bindings) ->
    SourceBin = list_to_binary(Expression),
    ModNameBin = atom_to_binary(ModuleName, utf8),
    KnownVars = known_vars(Bindings),
    CompileOpts = add_class_indexes(#{}),
    wrap_compiler_errors(
        fun() ->
            case
                beamtalk_compiler:compile_expression_trace(
                    SourceBin, ModNameBin, KnownVars, CompileOpts
                )
            of
                {ok, CoreErlang, Warnings} ->
                    compile_standard_expression(CoreErlang, Warnings);
                {error, Diagnostics} ->
                    {error, Diagnostics}
            end
        end,
        direct
    ).

-doc "Compile a Beamtalk file to bytecode.".
-spec compile_file(string(), string(), boolean(), binary() | undefined) ->
    {ok, binary(), [#{name := string(), superclass := string()}], atom()}
    | {ok, protocol_definition, map(), [binary()]}
    | {error, term()}.
compile_file(Source, Path, StdlibMode, ModuleNameOverride) ->
    compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride).

-doc """
Compile a Beamtalk file to bytecode with pre-built class indexes (BT-1543).

Like `compile_file/4' but accepts pre-built class indexes to avoid
redundant class registry scans during batch loads.
""".
-spec compile_file(string(), string(), boolean(), binary() | undefined, map()) ->
    {ok, binary(), [#{name := string(), superclass := string()}], atom()}
    | {ok, protocol_definition, map(), [binary()]}
    | {error, term()}.
compile_file(Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes) ->
    compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes).

-doc """
Compile a Beamtalk expression and return Core Erlang source (for show-codegen).
Does NOT compile to bytecode.
""".
-spec compile_for_codegen(binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]} | {error, term()}.
compile_for_codegen(SourceBin, ModNameBin, KnownVars) ->
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars) of
                {ok, class_definition, ClassInfo} ->
                    #{core_erlang := CoreErlang, warnings := Warnings} = ClassInfo,
                    {ok, CoreErlang, Warnings};
                {ok, method_definition, _MethodInfo} ->
                    {error,
                        {compile_error,
                            <<"show-codegen does not support standalone method definitions">>}};
                %% BT-1612: Protocol definitions have no Core Erlang to show.
                {ok, protocol_definition, _ProtocolInfo} ->
                    {error,
                        {compile_error, <<"show-codegen does not support protocol definitions">>}};
                {ok, CoreErlang, Warnings} ->
                    {ok, CoreErlang, Warnings};
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

-doc """
Compile a Beamtalk source file and return Core Erlang source (for show-codegen).
Does NOT compile to bytecode. Used when inspecting codegen for a loaded class.
""".
-spec compile_file_for_codegen(binary(), string() | undefined) ->
    {ok, binary(), [binary()]} | {error, term()}.
compile_file_for_codegen(SourceBin, Path) ->
    Options0 = #{},
    Options1 = apply_source_path(Options0, Path),
    Options = add_class_indexes(Options1),
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok, #{core_erlang := CoreErlang} = CR} ->
                    Warnings = maps:get(warnings, CR, []),
                    {ok, CoreErlang, Warnings};
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

-doc """
Format a list of diagnostics as a human-readable binary string.
Handles both structured diagnostic maps (BT-1235) and plain binaries (legacy).
""".
-spec format_formatted_diagnostics(list()) -> binary().
format_formatted_diagnostics([]) ->
    <<"Compilation failed">>;
format_formatted_diagnostics(FormattedList) ->
    Messages = [format_diagnostic_text(D) || D <- FormattedList],
    iolist_to_binary(lists:join(<<"\n\n">>, Messages)).

-doc "Format a single diagnostic for human-readable display.".
-spec format_diagnostic_text(term()) -> binary().
format_diagnostic_text(D) when is_map(D) ->
    Msg = maps:get(message, D, <<"Unknown error">>),
    LinePrefix =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(line, D) of
            {ok, Line} when is_integer(Line) ->
                [<<"Line ">>, integer_to_binary(Line), <<": ">>];
            _ ->
                []
        end,
    HintSuffix =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(hint, D) of
            {ok, Hint} -> [<<"\nHint: ">>, Hint];
            error -> []
        end,
    iolist_to_binary([LinePrefix, Msg, HintSuffix]);
format_diagnostic_text(D) when is_binary(D) ->
    D;
format_diagnostic_text(D) ->
    iolist_to_binary(io_lib:format("~p", [D])).

-doc """
Build the `KnownVars' list passed to the compiler port for a REPL eval.

This is the set of names the structural validator must treat as already
defined so it does not flag them as unresolved classes / undefined variables.
It must mirror what the runtime name resolver
(`beamtalk_workspace_interface_primitives:resolve_name/2') can resolve as a
free identifier:

- user variable bindings — session locals plus `bind:as:' globals, both already
  present as atom keys in `Bindings' (internal `__'-prefixed keys excluded);
- workspace singleton binding names (`Transcript'/`Beamtalk'/`Workspace'),
  derived from `beamtalk_workspace_config:binding_names/0' — the same single
  source of truth the resolver's singleton tier uses.

ADR 0081 Phase 1 stopped eagerly injecting the singletons into the session
bindings map (they are now resolved lazily), so they are no longer in
`Bindings' and must be added back here — otherwise `Workspace classes' at the
REPL reports a spurious `Unresolved class Workspace` warning.

Class names (`Counter', `Integer', …) are not added here; the compiler already
knows them via the class hierarchy.
""".
-spec known_vars(map()) -> [binary()].
known_vars(Bindings) ->
    UserVars = [
        atom_to_binary(K, utf8)
     || K <- maps:keys(Bindings),
        is_atom(K),
        not is_internal_key(K)
    ],
    SingletonVars = [
        atom_to_binary(Name, utf8)
     || Name <- beamtalk_workspace_config:binding_names()
    ],
    lists:usort(UserVars ++ SingletonVars).

-doc "Check if a binding key is internal (not a user variable).".
-spec is_internal_key(atom()) -> boolean().
is_internal_key(Key) when is_atom(Key) ->
    case atom_to_list(Key) of
        "__" ++ _ -> true;
        _ -> false
    end.

-doc """
Build a class→superclass index from the Beamtalk class hierarchy ETS table.

BT-905: Used when compiling new source via the REPL to inform the compiler
which already-loaded classes are value objects vs actors.
""".
-spec build_class_superclass_index() -> #{binary() => binary()}.
build_class_superclass_index() ->
    FoldFun = fun
        ({_Class, none}, Acc) ->
            Acc;
        ({Class, Superclass}, Acc) when is_atom(Class), is_atom(Superclass) ->
            Acc#{atom_to_binary(Class, utf8) => atom_to_binary(Superclass, utf8)};
        (_, Acc) ->
            Acc
    end,
    beamtalk_runtime_api:hierarchy_foldl(FoldFun, #{}).

-doc """
Build a class→module index from all registered class gen-servers.

When a class lives in a subdirectory (e.g. src/singleton/app_logger.bt),
the Rust compiler's user_package_prefix loses the subdirectory segment. By passing
a full class→module map, compiled_module_name/2 uses the correct module name
(e.g. bt@gang_of_four@singleton@app_logger) instead of guessing bt@gang_of_four@app_logger.
""".
-spec build_class_module_index() -> #{binary() => binary()}.
build_class_module_index() ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    lists:foldl(
        fun(Pid, Acc) ->
            try
                ClassName = beamtalk_runtime_api:class_name(Pid),
                ModuleName = beamtalk_runtime_api:module_name(Pid),
                case {ClassName, ModuleName} of
                    {CN, MN} when is_atom(CN), is_atom(MN) ->
                        Acc#{atom_to_binary(CN, utf8) => atom_to_binary(MN, utf8)};
                    _ ->
                        Acc
                end
            catch
                _:_ -> Acc
            end
        end,
        #{},
        ClassPids
    ).

-doc """
Build both class indexes as a single options map (BT-1543).

Returns a map suitable for merging into compile options, containing
`class_superclass_index' and/or `class_module_index' keys when non-empty.
Intended to be built once before a batch of compilations.
""".
-spec build_class_indexes() -> map().
build_class_indexes() ->
    add_class_indexes(#{}).

-doc """
Compile Beamtalk source and Core Erlang for method reload (BT-911).

Wraps both beamtalk_compiler:compile/2 and compile_core_erlang/1 inside
wrap_compiler_errors so that a compiler crash (exit, throw, error) returns
{error, {compile_error, Msg}} instead of propagating a fatal exit that would
kill the REPL process.
""".
-spec compile_for_method_reload(binary(), map()) ->
    {ok, binary(), atom(), list(), [binary()]} | {error, term()}.
compile_for_method_reload(SourceBin, Options) ->
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok,
                    #{core_erlang := CoreErlang, module_name := ModNameBin, classes := Classes} =
                        CR} ->
                    Warnings = maps:get(warnings, CR, []),
                    % elp:fixme W0023 intentional atom creation
                    ModName = binary_to_atom(ModNameBin, utf8),
                    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                        {ok, _CompiledMod, Binary} ->
                            {ok, Binary, ModName, Classes, Warnings};
                        {error, Reason} ->
                            {error, {compile_error, format_core_error(Reason)}}
                    end;
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

-doc """
Structured single-method compile + Core Erlang for the live-image write-surface.

The rock-solid replacement for `compile_for_method_reload/2`'s textual
`Class >> <source>` wrap: `MethodSource' is the BARE method body, parsed
standalone (comments and all) and merged into `ClassSource''s class by the
backend. Returns a map with the compiled `binary', `module_name' (atom),
`classes', the recovered `selector', `is_class_method', the canonical
`method_source' (byte-stable round-trip), and `warnings'.

Wrapped in `wrap_compiler_errors' like `compile_for_method_reload/2' so a
compiler crash becomes `{error, {compile_error, Msg}}'.
""".
-spec compile_method_reload(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
compile_method_reload(ClassSource, MethodSource, Options) ->
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile_method(ClassSource, MethodSource, Options) of
                {ok, #{core_erlang := CoreErlang, module_name := ModNameBin} = CR} ->
                    % elp:fixme W0023 intentional atom creation
                    ModName = binary_to_atom(ModNameBin, utf8),
                    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                        {ok, _CompiledMod, Binary} ->
                            Classes = maps:get(classes, CR, []),
                            %% ADR 0108 hot-reload re-check trigger (BT-2899).
                            %% `additive` (BT-2955 follow-up): `ClassSource`
                            %% here is the class's own reconstructed source —
                            %% `beamtalk_workspace_meta:get_class_source/1` is
                            %% keyed by class name and stores only that
                            %% class's subclass-definition text, not the
                            %% originating file's other top-level
                            %% declarations. A `type Name = ...` alias
                            %% declared as a *sibling* of this class in the
                            %% same source file (the exact `stdlib/src/Ets.bt`
                            %% shape BT-2955 fixed for the other REPL-inline
                            %% paths) is therefore just as invisible to this
                            %% compile as it is to `compile_class_definition_result/2`
                            %% — `replace` here would silently clobber a real
                            %% edge a prior `:load` registered.
                            ReferencedAliases = maps:get(referenced_aliases, CR, []),
                            register_alias_xref_for_classes(Classes, ReferencedAliases, additive),
                            {ok, #{
                                binary => Binary,
                                module_name => ModName,
                                classes => Classes,
                                selector => maps:get(selector, CR),
                                is_class_method => maps:get(is_class_method, CR, false),
                                method_source => maps:get(method_source, CR),
                                merged_class_source => maps:get(merged_class_source, CR),
                                %% ADR 0105 Phase 1 (BT-2777): declared signature,
                                %% forwarded so the signature-generation store can
                                %% capture it before the patch installs.
                                return_type => maps:get(return_type, CR, <<"Dynamic">>),
                                param_types => maps:get(param_types, CR, []),
                                warnings => maps:get(warnings, CR, [])
                            }};
                        {error, Reason} ->
                            {error, {compile_error, format_core_error(Reason)}}
                    end;
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

%%% Internal functions

%% Compile expression via beamtalk_compiler OTP app (port backend).
compile_expression_via_port(Expression, ModuleName, Bindings) ->
    compile_expression_via_port(Expression, ModuleName, Bindings, []).

%% Compile expression via beamtalk_compiler OTP app (port backend), forwarding
%% earlier-turn type aliases (ADR 0108 Phase 8, BT-2902).
compile_expression_via_port(Expression, ModuleName, Bindings, KnownTypeAliasSources) ->
    SourceBin = list_to_binary(Expression),
    ModNameBin = atom_to_binary(ModuleName, utf8),
    KnownVars = known_vars(Bindings),
    CompileOpts0 = add_class_indexes(#{}),
    CompileOpts =
        case KnownTypeAliasSources of
            [] -> CompileOpts0;
            _ -> CompileOpts0#{known_type_aliases => KnownTypeAliasSources}
        end,
    wrap_compiler_errors(
        fun() ->
            case
                beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars, CompileOpts)
            of
                {ok, class_definition, ClassInfo} ->
                    compile_class_definition_result(ClassInfo, ModuleName);
                {ok, method_definition, MethodInfo} ->
                    Warnings = maps:get(warnings, MethodInfo, []),
                    %% ADR 0105 Phase 1 (BT-2777): return_type/param_types ride
                    %% along so the signature-generation store can capture the
                    %% declared signature before the patch installs.
                    {ok, method_definition,
                        maps:with(
                            [
                                class_name,
                                selector,
                                is_class_method,
                                method_source,
                                return_type,
                                param_types
                            ],
                            MethodInfo
                        ),
                        Warnings};
                %% BT-1612: Protocol definition — compile Core Erlang to BEAM.
                %% `additive` (BT-2955): this is the REPL-inline path, which
                %% may be missing file-local alias context.
                {ok, protocol_definition, ProtocolInfo} ->
                    compile_protocol_definition_result(ProtocolInfo, additive);
                %% ADR 0108 Phase 8 (BT-2902): type alias definition — no
                %% Core Erlang/bytecode step (aliases erase entirely).
                {ok, type_alias_definition, AliasInfo} ->
                    Warnings = maps:get(warnings, AliasInfo, []),
                    {ok, type_alias_definition,
                        maps:with([alias_name, expansion, doc_comment], AliasInfo), Warnings};
                {ok, CoreErlang, Warnings} ->
                    compile_standard_expression(CoreErlang, Warnings);
                {error, Diagnostics} ->
                    %% BT-1235: Return structured diagnostics (maps with message/line/hint)
                    %% so callers can surface line numbers and hints to MCP clients.
                    {error, Diagnostics}
            end
        end,
        direct
    ).

-doc """
Compile a protocol definition result to BEAM bytecode (BT-1612).

ADR 0108 hot-reload re-check trigger (BT-2899 follow-up, BT-2917, BT-2952):
also registers `ProtocolInfo`'s `referenced_aliases` into `beamtalk_alias_xref`
for the compiled protocols — see `register_alias_xref_for_protocols/3`'s doc.

## History: why this used to be split into two functions

This function is shared by *two* callers: `compile_file_via_port/5` (a real
file compile) and `compile_expression_via_port/4` (a REPL-typed `Protocol
define: ...`). Before BT-2952, only the file-compile path's
`referenced_aliases` was the compiler-port's genuinely-computed set — the
REPL-inline path's was hardcoded `[]` (the Rust side didn't compute one for
that path yet), so registering unconditionally here would have let a
REPL-inline recompile of an already-`:load`ed protocol silently *clobber*
the real edge with `[]` (`beamtalk_alias_xref:register_class/2` is
whole-set replacement, not a delta — see its own doc). Registration used to
live only in a `compile_protocol_definition_result_for_file/1` wrapper the
file-compile path alone called.

BT-2952 made the compiler port compute a genuine `referenced_aliases` set
for the REPL-inline path too (mirroring `compile_class_definition_result/2`,
classes' REPL-inline equivalent, which registers directly for the same
reason), so both callers now carry trustworthy data for *aliases declared in
the REPL session*.

## File-local aliases (BT-2955): the `Mode` parameter

"Trustworthy" above does not extend to an alias declared inside the SAME
file as the protocol/class (e.g. `stdlib/src/Ets.bt`'s `type EtsTableType =
...` alongside `Ets`'s own method signatures) — a REPL-inline redefinition
of that protocol/class has no way to see a file-local alias it didn't also
declare in-session, so its `referenced_aliases` silently omits it, and an
unconditional whole-set-replace registration would clobber whatever a prior
`:load` of that file registered. `Mode` disambiguates this function's two
callers again (mirroring the pre-BT-2952 split, but for registration
semantics rather than whether to register at all): `compile_file_via_port/5`
passes `replace` (a file compile always has the complete picture for its own
file, so `beamtalk_alias_xref:register_class/2`'s whole-set-replace stays
correct and is still the only way to ever retire a stale edge);
`compile_expression_via_port/4` passes `additive`
(`beamtalk_alias_xref:register_class_additive/2` — see that function's doc
for why never removing an edge is the safe default for a REPL-inline compile
that might be missing file-local context).
""".
-spec compile_protocol_definition_result(map(), replace | additive) ->
    {ok, protocol_definition, map(), [binary()]} | {error, binary()}.
compile_protocol_definition_result(ProtocolInfo, Mode) ->
    #{
        core_erlang := CoreErlang,
        module_name := ModuleNameBin,
        protocols := Protocols,
        warnings := Warnings
    } = ProtocolInfo,
    % elp:fixme W0023 intentional atom creation
    ModuleName = binary_to_atom(ModuleNameBin, utf8),
    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
        {ok, _CompiledMod, Binary} ->
            ReferencedAliases = maps:get(referenced_aliases, ProtocolInfo, []),
            register_alias_xref_for_protocols(Protocols, ReferencedAliases, Mode),
            {ok, protocol_definition,
                #{
                    binary => Binary,
                    module_name => ModuleName,
                    protocols => Protocols
                },
                Warnings};
        {error, Reason} ->
            ErrMsg = iolist_to_binary(
                io_lib:format("Failed to compile protocol module ~s: ~p", [ModuleNameBin, Reason])
            ),
            {error, ErrMsg}
    end.

-doc """
Compile a class definition result including optional trailing expressions.

ADR 0108 hot-reload re-check trigger (BT-2899 / BT-2952 follow-up): also
registers `ClassInfo`'s `referenced_aliases` into `beamtalk_alias_xref` for
every class this REPL-inline definition installs — see
`register_alias_xref_for_classes/3`'s doc for why the same flat set is
registered against every class in a multi-class source. Previously this
never registered anything (only the file-compile path's `compile_file_core/4`
did) because the compiler port hardcoded an empty `referenced_aliases` set
for REPL-inline class definitions; now that the port computes a genuine set
(BT-2952) for aliases declared in the REPL session, registering here is as
safe as `compile_file_core/4` doing it for the file-compile path for aliases
declared *at the REPL* — but this function is exclusively the REPL-inline
call site (unlike `compile_protocol_definition_result/2`, `compile_file_core/4`
handles classes for the file-compile path directly), so it always registers
`additive` (BT-2955): a class whose alias reference was declared *inside the
same file* the class originally came from, not in the REPL session, is
invisible to this compile's `referenced_aliases`, and an unconditional
whole-set-replace registration would silently clobber a real edge a prior
`:load` registered. See `beamtalk_alias_xref:register_class_additive/2`'s doc.
""".
-spec compile_class_definition_result(map(), atom()) ->
    {ok, class_definition, map(), [binary()]} | {error, binary()}.
compile_class_definition_result(ClassInfo, ModuleName) ->
    #{
        core_erlang := CoreErlang,
        module_name := ClassModNameBin,
        classes := Classes,
        warnings := Warnings
    } = ClassInfo,
    % elp:fixme W0023 intentional atom creation
    ClassModName = binary_to_atom(ClassModNameBin, utf8),
    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
        {ok, _CompiledMod, Binary} ->
            ReferencedAliases = maps:get(referenced_aliases, ClassInfo, []),
            register_alias_xref_for_classes(Classes, ReferencedAliases, additive),
            TrailingResult = compile_trailing_expressions(ClassInfo, ModuleName),
            assemble_class_result(Binary, ClassModName, Classes, Warnings, TrailingResult);
        {error, Reason} ->
            {error, format_core_error(Reason)}
    end.

%% Compile trailing expressions attached to a class definition (BT-885).
-spec compile_trailing_expressions(map(), atom()) ->
    {ok, binary(), atom()} | {error, binary()} | none.
compile_trailing_expressions(ClassInfo, ModuleName) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(trailing_core_erlang, ClassInfo) of
        {ok, TrailingCoreErlang} ->
            case beamtalk_compiler:compile_core_erlang(TrailingCoreErlang) of
                {ok, _TCompiledMod, TBinary} ->
                    {ok, TBinary, ModuleName};
                {error, Reason} ->
                    {error, format_core_error(Reason)}
            end;
        error ->
            none
    end.

%% Assemble the final class definition result map.
-spec assemble_class_result(binary(), atom(), list(), [binary()], term()) ->
    {ok, class_definition, map(), [binary()]} | {error, binary()}.
assemble_class_result(_Binary, _ClassModName, _Classes, _Warnings, {error, Msg}) ->
    {error, Msg};
assemble_class_result(Binary, ClassModName, Classes, Warnings, TrailingResult) ->
    BaseInfo = #{binary => Binary, module_name => ClassModName, classes => Classes},
    ClassInfo2 =
        case TrailingResult of
            {ok, TrailingBinary, TrailingModName} ->
                BaseInfo#{
                    trailing_binary => TrailingBinary,
                    trailing_module_name => TrailingModName
                };
            none ->
                BaseInfo
        end,
    {ok, class_definition, ClassInfo2, Warnings}.

%% Compile a standard (non-class, non-method) expression.
-spec compile_standard_expression(binary(), [binary()]) ->
    {ok, binary(), term(), [binary()]} | {error, binary()}.
compile_standard_expression(CoreErlang, Warnings) ->
    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
        {ok, _CompiledMod, Binary} ->
            {ok, Binary, {port_compiled}, Warnings};
        {error, Reason} ->
            {error, format_core_error(Reason)}
    end.

%% Compile file via beamtalk_compiler OTP app (port backend).
compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride) ->
    compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride, use_runtime_indexes).

%% Compile file via beamtalk_compiler with pre-built class indexes (BT-1543).
%% Pass `use_runtime_indexes` to derive indexes from the runtime registry.
compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes) ->
    SourceBin = list_to_binary(Source),
    Options0 = #{stdlib_mode => StdlibMode},
    Options1 = apply_module_name_override(Options0, ModuleNameOverride),
    Options2 = apply_source_path(Options1, Path),
    Options =
        case PrebuiltIndexes of
            use_runtime_indexes -> add_class_indexes(Options2);
            _ -> maps:merge(Options2, PrebuiltIndexes)
        end,
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok,
                    #{
                        core_erlang := CoreErlang,
                        module_name := ModNameBin,
                        classes := Classes
                    } = CR} ->
                    % elp:fixme W0023 intentional atom creation
                    ModuleName = binary_to_atom(ModNameBin, utf8),
                    ReferencedAliases = maps:get(referenced_aliases, CR, []),
                    compile_file_core(CoreErlang, ModuleName, Classes, ReferencedAliases);
                %% BT-1950: Protocol definitions from the compile path — compile
                %% Core Erlang to BEAM and return a protocol_definition result.
                %% BT-2917/BT-2952: `compile_protocol_definition_result/2`
                %% itself registers `beamtalk_alias_xref` edges from
                %% `ProtocolInfo`'s `referenced_aliases` — see that
                %% function's doc. `replace`: this is the file-compile path,
                %% which always has the complete alias picture for its own
                %% file (BT-2955).
                {ok, protocol_definition, ProtocolInfo} ->
                    compile_protocol_definition_result(ProtocolInfo, replace);
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

%% Compile the Core Erlang generated for a file and extract class metadata.
%%
%% ADR 0108 hot-reload re-check trigger (BT-2899): also registers
%% `ReferencedAliases` into `beamtalk_alias_xref` for every class this
%% compile installs — see `register_alias_xref_for_classes/3`'s doc for why
%% the same flat set is registered against every class in a multi-class
%% source, and why registering slightly before `code:load_binary/3` actually
%% installs the class is an accepted, harmless imprecision (this index is
%% advisory-only, matching every other ADR 0105/0108 store's risk
%% tolerance). `replace`: this is the file-compile path, which always has
%% the complete alias picture for its own file (BT-2955) — it remains the
%% sole authority that can retire a stale edge.
-spec compile_file_core(binary(), atom(), list(), [binary()]) ->
    {ok, binary(), list(), atom()} | {error, term()}.
compile_file_core(CoreErlang, ModuleName, Classes, ReferencedAliases) ->
    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
        {ok, _CompiledMod, Binary} ->
            ClassNames = [
                #{
                    name => binary_to_list(maps:get(name, C, <<"">>)),
                    superclass => binary_to_list(maps:get(superclass, C, <<"Object">>))
                }
             || C <- Classes
            ],
            register_alias_xref_for_classes(Classes, ReferencedAliases, replace),
            {ok, Binary, ClassNames, ModuleName};
        {error, Reason} ->
            {error, {core_compile_error, Reason}}
    end.

-doc """
Register `ReferencedAliases` into `beamtalk_alias_xref` for every class name
in `Classes` (the raw compiler-port `#{name := ..., superclass := ...}`
maps, ADR 0108 hot-reload re-check trigger, BT-2899).

`ReferencedAliases` is a **flat, whole-module** set
(`AnalysisResult::referenced_aliases`) — for a multi-class source (REPL
inline multi-class, a dependency file with several classes), a class that
itself referenced no alias still gets registered against every alias a
*sibling* class in the same source referenced. This is an accepted
over-approximation, not a bug: it means that sibling is occasionally an
unnecessary re-check candidate (comes back clean) on a later redefinition of
that alias, never a *missed* one — the same "advisory noise, not
correctness gap" tolerance every other ADR 0105/0108 mechanism accepts.
Per-class precision would need per-class alias-dependency tracking on the
Rust side, which BT-2899 deliberately scopes to a flat per-compile set (see
`resolve_type_annotation_with_alias_deps`'s doc).

`Mode` (BT-2955) selects which `beamtalk_alias_xref` registration primitive
to use: `replace` for a call site with the complete alias picture for its
source (`beamtalk_alias_xref:register_class/2`, whole-set-replace — can
retire a stale edge); `additive` for a call site that might be missing
file-local alias context, e.g. a REPL-inline redefinition
(`beamtalk_alias_xref:register_class_additive/2` — never removes an edge).
See those two functions' docs.
""".
-spec register_alias_xref_for_classes([map()], [binary()], replace | additive) -> ok.
register_alias_xref_for_classes(Classes, ReferencedAliases, Mode) ->
    RegisterFun = alias_xref_register_fun(Mode),
    lists:foreach(
        fun(C) ->
            case maps:get(name, C, <<>>) of
                <<>> -> ok;
                ClassNameBin -> RegisterFun(ClassNameBin, ReferencedAliases)
            end
        end,
        Classes
    ).

-doc """
Register `ReferencedAliases` into `beamtalk_alias_xref` for every protocol
name in `Protocols` (ADR 0108 hot-reload re-check trigger, BT-2899 follow-up,
BT-2917).

Protocol-shaped adapter over `beamtalk_alias_xref:register_class/2`/
`register_class_additive/2` (both themselves name-shape-agnostic — see their
docs — they just index `binary() -> [binary()]`): `Protocols` here is a
plain `[binary()]` of protocol names (`protocol_definition_ok_response`'s
`protocols` field, see `crates/beamtalk-compiler-port/src/main.rs`), unlike
`Classes`' `#{name := ..., superclass := ...}` maps, so this can't reuse
`register_alias_xref_for_classes/3` directly.

`Mode` — see `register_alias_xref_for_classes/3`'s doc.
""".
-spec register_alias_xref_for_protocols([binary()], [binary()], replace | additive) -> ok.
register_alias_xref_for_protocols(Protocols, ReferencedAliases, Mode) ->
    RegisterFun = alias_xref_register_fun(Mode),
    lists:foreach(
        fun
            %% Mirrors register_alias_xref_for_classes/3's empty-name guard —
            %% belt-and-braces against a malformed/empty protocol name
            %% binary reaching the xref index.
            (<<>>) ->
                ok;
            (ProtocolNameBin) ->
                RegisterFun(ProtocolNameBin, ReferencedAliases)
        end,
        Protocols
    ).

%% Resolve `Mode` to the `beamtalk_alias_xref` registration primitive it
%% names — see `register_alias_xref_for_classes/3`'s doc.
-spec alias_xref_register_fun(replace | additive) -> fun((binary(), [binary()]) -> ok).
alias_xref_register_fun(replace) -> fun beamtalk_alias_xref:register_class/2;
alias_xref_register_fun(additive) -> fun beamtalk_alias_xref:register_class_additive/2.

%% Build and merge class superclass and module indexes into a compile options map.
%%
%% Both indexes are conditionally included only when non-empty, keeping the
%% protocol backward-compatible with older port binaries (BT-905, BT-907).
-spec add_class_indexes(map()) -> map().
add_class_indexes(Opts) ->
    SuperclassIndex = build_class_superclass_index(),
    Opts1 =
        case map_size(SuperclassIndex) of
            0 -> Opts;
            _ -> Opts#{class_superclass_index => SuperclassIndex}
        end,
    ModuleIndex = build_class_module_index(),
    case map_size(ModuleIndex) of
        0 -> Opts1;
        _ -> Opts1#{class_module_index => ModuleIndex}
    end.

%% Apply module name override to options (BT-775).
-spec apply_module_name_override(map(), binary() | undefined) -> map().
apply_module_name_override(Options, undefined) ->
    Options;
apply_module_name_override(Options, ModuleNameOverride) ->
    Options#{module_name => ModuleNameOverride}.

%% Apply source path to options when available (BT-845/BT-860).
-spec apply_source_path(map(), string() | undefined) -> map().
apply_source_path(Options, undefined) ->
    Options;
apply_source_path(Options, Path) when is_list(Path) ->
    IsFilePath =
        lists:member($/, Path) orelse
            lists:member($\\, Path) orelse
            filename:extension(Path) =:= ".bt",
    case IsFilePath of
        true -> Options#{source_path => list_to_binary(Path)};
        false -> Options
    end;
apply_source_path(Options, _) ->
    Options.

-doc """
Wrap compiler calls with shared error handling for exit/error/throw.
ErrorStyle controls error wrapping:
  direct  - returns {error, Message} directly (for expression compilation)
  wrapped - wraps as {error, {compile_error, Message}} (for file compilation)
""".
-spec wrap_compiler_errors(fun(() -> term()), direct | wrapped) -> term().
wrap_compiler_errors(Fun, ErrorStyle) ->
    try
        Fun()
    catch
        exit:{noproc, _} ->
            wrap_error(
                <<"Compiler not available. Ensure beamtalk_compiler application is started.">>,
                ErrorStyle
            );
        exit:{timeout, _} ->
            wrap_error(<<"Compilation timed out.">>, ErrorStyle);
        exit:{Reason, _} ->
            wrap_error(iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason])), ErrorStyle);
        error:Reason ->
            wrap_error(iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason])), ErrorStyle);
        throw:Reason ->
            wrap_error(iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason])), ErrorStyle)
    end.

-spec wrap_error(binary(), direct | wrapped) -> {error, term()}.
wrap_error(Msg, direct) -> {error, Msg};
wrap_error(Msg, wrapped) -> {error, {compile_error, Msg}}.

-spec format_core_error(term()) -> binary().
format_core_error(Reason) ->
    iolist_to_binary(io_lib:format("Core Erlang compile error: ~p", [Reason])).
