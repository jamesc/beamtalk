%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk compilation pipeline for the REPL
%%%
%%% **DDD Context:** REPL
%%%
%%% Handles Beamtalk source → Core Erlang → BEAM bytecode compilation,
%%% including expression compilation, file compilation, and codegen display.
%%% Uses the beamtalk_compiler OTP application (ADR 0022) exclusively.

-module(beamtalk_repl_compiler).

-include_lib("kernel/include/logger.hrl").

-export([
    compile_expression/3,
    compile_file/4,
    compile_for_codegen/3,
    compile_for_method_reload/2,
    format_formatted_diagnostics/1,
    is_internal_key/1,
    build_class_superclass_index/0,
    build_class_module_index/0
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    compile_expression_via_port/3,
    compile_file_via_port/4
]).
-endif.

%%% Public API

%% @doc Compile a Beamtalk expression to bytecode.
-spec compile_expression(string(), atom(), map()) ->
    {ok, binary(), term(), [binary()]}
    | {ok, class_definition, map(), [binary()]}
    | {ok, method_definition, map(), [binary()]}
    | {error, term()}.
compile_expression(Expression, ModuleName, Bindings) ->
    compile_expression_via_port(Expression, ModuleName, Bindings).

%% @doc Compile a Beamtalk file to bytecode.
-spec compile_file(string(), string(), boolean(), binary() | undefined) ->
    {ok, binary(), [#{name := string(), superclass := string()}], atom()} | {error, term()}.
compile_file(Source, Path, StdlibMode, ModuleNameOverride) ->
    compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride).

%% @doc Compile a Beamtalk expression and return Core Erlang source (for show-codegen).
%% Does NOT compile to bytecode.
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
                {ok, CoreErlang, Warnings} ->
                    {ok, CoreErlang, Warnings};
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

%% @doc Format pre-formatted diagnostics (miette output).
-spec format_formatted_diagnostics(list()) -> binary().
format_formatted_diagnostics([]) ->
    <<"Compilation failed">>;
format_formatted_diagnostics(FormattedList) ->
    iolist_to_binary(lists:join(<<"\n\n">>, FormattedList)).

%% @doc Check if a binding key is internal (not a user variable).
-spec is_internal_key(atom()) -> boolean().
is_internal_key(Key) when is_atom(Key) ->
    case atom_to_list(Key) of
        "__" ++ _ -> true;
        _ -> false
    end.

%% @doc Build a class→superclass index from the Beamtalk class hierarchy ETS table.
%%
%% BT-905: Used when compiling new source via the REPL to inform the compiler
%% which already-loaded classes are value objects vs actors.
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
    try
        ets:foldl(FoldFun, #{}, beamtalk_class_hierarchy)
    catch
        error:badarg -> #{}
    end.

%% @doc Build a class→module index from all registered class gen-servers.
%%
%% When a class lives in a subdirectory (e.g. src/singleton/app_logger.bt),
%% the Rust compiler's user_package_prefix loses the subdirectory segment. By passing
%% a full class→module map, compiled_module_name/2 uses the correct module name
%% (e.g. bt@gang_of_four@singleton@app_logger) instead of guessing bt@gang_of_four@app_logger.
-spec build_class_module_index() -> #{binary() => binary()}.
build_class_module_index() ->
    ClassPids =
        try
            beamtalk_class_registry:all_classes()
        catch
            _:_ -> []
        end,
    lists:foldl(
        fun(Pid, Acc) ->
            try
                ClassName = gen_server:call(Pid, class_name, 1000),
                ModuleName = gen_server:call(Pid, module_name, 1000),
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

%% @doc Compile Beamtalk source and Core Erlang for method reload (BT-911).
%%
%% Wraps both beamtalk_compiler:compile/2 and compile_core_erlang/1 inside
%% wrap_compiler_errors so that a compiler crash (exit, throw, error) returns
%% {error, {compile_error, Msg}} instead of propagating a fatal exit that would
%% kill the REPL process.
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

%%% Internal functions

%% Compile expression via beamtalk_compiler OTP app (port backend).
compile_expression_via_port(Expression, ModuleName, Bindings) ->
    SourceBin = list_to_binary(Expression),
    ModNameBin = atom_to_binary(ModuleName, utf8),
    KnownVars = [
        atom_to_binary(K, utf8)
     || K <- maps:keys(Bindings),
        is_atom(K),
        not is_internal_key(K)
    ],
    SuperclassIndex = build_class_superclass_index(),
    CompileOpts0 =
        case map_size(SuperclassIndex) of
            0 -> #{};
            _ -> #{class_superclass_index => SuperclassIndex}
        end,
    ModuleIndex = build_class_module_index(),
    CompileOpts =
        case map_size(ModuleIndex) of
            0 -> CompileOpts0;
            _ -> CompileOpts0#{class_module_index => ModuleIndex}
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
                    {ok, method_definition,
                        maps:with(
                            [class_name, selector, is_class_method, method_source], MethodInfo
                        ),
                        Warnings};
                {ok, CoreErlang, Warnings} ->
                    compile_standard_expression(CoreErlang, Warnings);
                {error, Diagnostics} ->
                    {error, format_formatted_diagnostics(Diagnostics)}
            end
        end,
        direct
    ).

%% Compile a class definition result including optional trailing expressions.
-spec compile_class_definition_result(map(), atom()) ->
    {ok, class_definition, map(), [binary()]} | {error, binary()}.
compile_class_definition_result(ClassInfo, ModuleName) ->
    #{
        core_erlang := CoreErlang,
        module_name := ClassModNameBin,
        classes := Classes,
        warnings := Warnings
    } = ClassInfo,
    ClassModName = binary_to_atom(ClassModNameBin, utf8),
    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
        {ok, _CompiledMod, Binary} ->
            TrailingResult = compile_trailing_expressions(ClassInfo, ModuleName),
            assemble_class_result(Binary, ClassModName, Classes, Warnings, TrailingResult);
        {error, Reason} ->
            {error, format_core_error(Reason)}
    end.

%% Compile trailing expressions attached to a class definition (BT-885).
-spec compile_trailing_expressions(map(), atom()) ->
    {ok, binary(), atom()} | {error, binary()} | none.
compile_trailing_expressions(ClassInfo, ModuleName) ->
    case maps:find(trailing_core_erlang, ClassInfo) of
        {ok, TrailingCoreErlang} ->
            case beamtalk_compiler:compile_core_erlang(TrailingCoreErlang) of
                {ok, _TCompiledMod, TBinary} ->
                    {ok, TBinary, ModuleName};
                {error, Reason} ->
                    {error,
                        iolist_to_binary(
                            io_lib:format("Trailing expression compile error: ~p", [Reason])
                        )}
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
    SourceBin = list_to_binary(Source),
    Options0 = #{stdlib_mode => StdlibMode},
    Options1 = apply_module_name_override(Options0, ModuleNameOverride),
    Options2 = apply_source_path(Options1, Path),
    SuperclassIndex = build_class_superclass_index(),
    Options3 =
        case map_size(SuperclassIndex) of
            0 -> Options2;
            _ -> Options2#{class_superclass_index => SuperclassIndex}
        end,
    ModuleIndex = build_class_module_index(),
    Options =
        case map_size(ModuleIndex) of
            0 -> Options3;
            _ -> Options3#{class_module_index => ModuleIndex}
        end,
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok, #{
                    core_erlang := CoreErlang,
                    module_name := ModNameBin,
                    classes := Classes
                }} ->
                    ModuleName = binary_to_atom(ModNameBin, utf8),
                    compile_file_core(CoreErlang, ModuleName, Classes);
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

%% Compile the Core Erlang generated for a file and extract class metadata.
-spec compile_file_core(binary(), atom(), list()) ->
    {ok, binary(), list(), atom()} | {error, term()}.
compile_file_core(CoreErlang, ModuleName, Classes) ->
    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
        {ok, _CompiledMod, Binary} ->
            ClassNames = [
                #{
                    name => binary_to_list(maps:get(name, C, <<"">>)),
                    superclass => binary_to_list(maps:get(superclass, C, <<"Object">>))
                }
             || C <- Classes
            ],
            {ok, Binary, ClassNames, ModuleName};
        {error, Reason} ->
            {error, {core_compile_error, Reason}}
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

%% @doc Wrap compiler calls with shared error handling for exit/error/throw.
%% ErrorStyle controls error wrapping:
%%   direct  - returns {error, Message} directly (for expression compilation)
%%   wrapped - wraps as {error, {compile_error, Message}} (for file compilation)
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
