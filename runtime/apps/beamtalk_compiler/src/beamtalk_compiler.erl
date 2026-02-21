%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Public API for the Beamtalk compiler (ADR 0022).
%%
%% DDD Context: Compilation (Anti-Corruption Layer boundary)
%%
%% This module is the single entry point for all compilation via OTP Port.
%%
%% All functions delegate to `beamtalk_compiler_server' (port backend).
%%
%% ## API
%%
%% - `compile_expression/3' — Compile a REPL expression
%% - `compile/2' — Compile a file/class definition (`:load')
%% - `diagnostics/1' — Get parse/semantic diagnostics only
%% - `version/0' — Get compiler version
%% - `compile_core_erlang/1' — Core Erlang → BEAM bytecode (in-memory)

-module(beamtalk_compiler).

-export([
    compile_expression/3,
    compile/2,
    diagnostics/1,
    version/0,
    compile_core_erlang/1
]).

%% @doc Compile a REPL expression.
%%
%% `Source' is the expression source code as a binary.
%% `ModuleName' is the unique module name for this evaluation.
%% `KnownVars' is a list of variable name binaries from the REPL session.
%%
%% Returns `{ok, CoreErlang, Warnings}' for expressions,
%% `{ok, class_definition, ClassInfo}' for inline class definitions (BT-571),
%% `{ok, method_definition, MethodInfo}' for standalone method definitions (BT-571),
%% or `{error, Diagnostics}' on failure.
-spec compile_expression(binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [binary()]}.
compile_expression(Source, ModuleName, KnownVars) ->
    beamtalk_compiler_server:compile_expression(Source, ModuleName, KnownVars).

%% @doc Compile a file/class definition.
%%
%% `Source' is the file source code as a binary.
%% `Options' is a map with optional keys:
%%   - `stdlib_mode' (boolean, default false) — enable `@primitive' pragmas
%%   - `workspace_mode' (boolean, default true) — REPL workspace context
%%
%% Returns `{ok, #{core_erlang, module_name, classes, warnings}}' or
%% `{error, Diagnostics}'.
-spec compile(binary(), map()) ->
    {ok, map()} | {error, [binary()]}.
compile(Source, Options) ->
    beamtalk_compiler_server:compile(Source, Options).

%% @doc Get diagnostics for source code (no code generation).
%%
%% Returns `{ok, [#{message, severity, start, end}]}' or `{error, Diagnostics}'.
-spec diagnostics(binary()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source) ->
    beamtalk_compiler_server:diagnostics(Source).

%% @doc Get compiler version.
-spec version() -> {ok, binary()} | {error, term()}.
version() ->
    beamtalk_compiler_server:version().

%% @doc Compile Core Erlang source to BEAM bytecode in memory.
%%
%% Uses `core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'.
%% No temp files on disk (BT-48).
%%
%% Returns `{ok, ModuleName, Binary}' or `{error, Reason}'.
-spec compile_core_erlang(binary()) -> {ok, atom(), binary()} | {error, term()}.
compile_core_erlang(CoreErlangBin) ->
    beamtalk_compiler_server:compile_core_erlang(CoreErlangBin).
