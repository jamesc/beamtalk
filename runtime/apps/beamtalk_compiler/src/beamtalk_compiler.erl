%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler).

%%% **DDD Context:** Compilation (Anti-Corruption Layer)

-moduledoc """
Public API for the Beamtalk compiler (ADR 0022).

This module is the single entry point for all compilation via OTP Port.

All functions delegate to `beamtalk_compiler_server' (port backend).

## API

- `compile_expression/3' — Compile a REPL expression
- `compile/2' — Compile a file/class definition (`:load')
- `diagnostics/1' — Get parse/semantic diagnostics only
- `version/0' — Get compiler version
- `compile_core_erlang/1' — Core Erlang → BEAM bytecode (in-memory)
""".

-export([
    compile_expression/3, compile_expression/4,
    compile_expression_trace/3, compile_expression_trace/4,
    compile/2,
    diagnostics/1,
    version/0,
    compile_core_erlang/1,
    resolve_completion_type/1,
    find_senders_in_source/2,
    find_all_sends_in_source/1,
    find_references_to_in_source/2,
    find_field_readers_in_source/2,
    find_field_writers_in_source/2,
    find_ffi_sites_in_source/4,
    find_announce_sites_in_source/1,
    resolve_method_span/4
]).

-doc """
Compile a REPL expression.

`Source' is the expression source code as a binary.
`ModuleName' is the unique module name for this evaluation.
`KnownVars' is a list of variable name binaries from the REPL session.

Returns `{ok, CoreErlang, Warnings}' for expressions,
`{ok, class_definition, ClassInfo}' for inline class definitions (BT-571),
`{ok, method_definition, MethodInfo}' for standalone method definitions (BT-571),
or `{error, Diagnostics}' on failure, where each diagnostic is a map with
`message', `line' (1-based), and optionally `hint'.
""".
-spec compile_expression(binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {error, [map()]}.
compile_expression(Source, ModuleName, KnownVars) ->
    beamtalk_compiler_server:compile_expression(Source, ModuleName, KnownVars).

-doc """
Compile a REPL expression with optional compilation options.

Options:
  class_superclass_index => #{binary() => binary()} — BT-907: cross-file superclass info
""".
-spec compile_expression(binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {error, [map()]}.
compile_expression(Source, ModuleName, KnownVars, Options) ->
    beamtalk_compiler_server:compile_expression(Source, ModuleName, KnownVars, Options).

-doc """
Compile a REPL expression in trace mode (BT-1238).

Returns `{ok, CoreErlang, Warnings}' where the generated module's `eval/1'
returns `{[{<<"src0">>, Val0}, ...], FinalState}' instead of `{Result, FinalState}'.
""".
-spec compile_expression_trace(binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]} | {error, [map()]}.
compile_expression_trace(Source, ModuleName, KnownVars) ->
    beamtalk_compiler_server:compile_expression_trace(Source, ModuleName, KnownVars, #{}).

-spec compile_expression_trace(binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]} | {error, [map()]}.
compile_expression_trace(Source, ModuleName, KnownVars, Options) ->
    beamtalk_compiler_server:compile_expression_trace(Source, ModuleName, KnownVars, Options).

-doc """
Compile a file/class definition.

`Source' is the file source code as a binary.
`Options' is a map with optional keys:
  - `stdlib_mode' (boolean, default false) — enable `@primitive' pragmas
  - `workspace_mode' (boolean, default true) — REPL workspace context

Returns `{ok, #{core_erlang, module_name, classes, warnings}}',
`{ok, protocol_definition, #{core_erlang, module_name, protocols, warnings}}',
or `{error, Diagnostics}'.
""".
-spec compile(binary(), map()) ->
    {ok, map()} | {ok, protocol_definition, map()} | {error, [map()]}.
compile(Source, Options) ->
    beamtalk_compiler_server:compile(Source, Options).

-doc """
Get diagnostics for source code (no code generation).

Returns `{ok, [#{message, severity, start, end}]}' or `{error, Diagnostics}'.
""".
-spec diagnostics(binary()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source) ->
    beamtalk_compiler_server:diagnostics(Source).

-doc "Get compiler version.".
-spec version() -> {ok, binary()} | {error, term()}.
version() ->
    beamtalk_compiler_server:version().

-doc """
Resolve the type of an expression for REPL completion fallback (BT-1068).

`Expression' is the receiver expression-up-to-cursor with the incomplete
prefix stripped. Returns `{ok, ClassName}' or `{error, type_unknown}'.
""".
-spec resolve_completion_type(binary()) -> {ok, atom()} | {error, type_unknown}.
resolve_completion_type(Expression) ->
    beamtalk_compiler_server:resolve_completion_type(Expression).

-doc """
Compile Core Erlang source to BEAM bytecode in memory.

Uses `core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'.
No temp files on disk (BT-48).

Returns `{ok, ModuleName, Binary}' or `{error, Reason}'.
""".
-spec compile_core_erlang(binary()) -> {ok, atom(), binary()} | {error, term()}.
compile_core_erlang(CoreErlangBin) ->
    beamtalk_compiler_server:compile_core_erlang(CoreErlangBin).

-doc """
Find call sites of a selector in a single method's source (BT-2190).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). `Selector' is the target selector as an atom or
binary (without a leading `#').

Returns `{ok, [Line]}' on success, where each line is a 1-based line number
within `Source'. Returns `{ok, []}' when no senders are found or the source
cannot be parsed. Returns `{error, Diagnostics}' if the compiler port is
unavailable.

Backs `SystemNavigation sendersOf:' for System Browser-style call-site navigation.
""".
-spec find_senders_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_senders_in_source(Source, Selector) ->
    beamtalk_compiler_server:find_senders_in_source(Source, Selector).

-doc """
Find every message send in a single method's source (BT-2206).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). Single-pass companion to `find_senders_in_source/2':
returns EVERY send rather than filtering by one selector.

Returns `{ok, [Send]}' on success, where each `Send' is a map
`#{selector := binary(), line := pos_integer(), recv := self | super | erlang_ffi | other}'.
Returns `{ok, []}' when the source has no sends or cannot be parsed. Returns
`{error, Diagnostics}' if the compiler port is unavailable.

Backs `SystemNavigation unimplementedSelectors' — the classic typo-finder
(`allSentSelectors − allDefinedSelectors').
""".
-spec find_all_sends_in_source(binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_all_sends_in_source(Source) ->
    beamtalk_compiler_server:find_all_sends_in_source(Source).

-doc """
Find every `announce:' emission in a single method's source (BT-2475).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). Recognises `announce:', `announceAndWait:', and
`announceAndWait:timeout:', resolving each event argument to its announcement
class name where it is a constructor call on a bare class reference.

Returns `{ok, [Site]}' on success, where each `Site' is a map
`#{selector := binary(), line := pos_integer(), announcement_class := binary()}'.
The `announcement_class' is an empty binary when the event argument is
unresolvable (`Dynamic'-typed / indirect). Returns `{ok, []}' when the source
has no emissions or cannot be parsed. Returns `{error, Diagnostics}' if the
compiler port is unavailable.

Backs `SystemNavigation announcementsSentBy:' — the static dual of
`AnnouncementNavigation' (ADR 0093 §7).
""".
-spec find_announce_sites_in_source(binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_announce_sites_in_source(Source) ->
    beamtalk_compiler_server:find_announce_sites_in_source(Source).

-doc """
Find references to a class in a single method's source (BT-2203).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). `ClassName' is the target class name as an atom or
binary (without a leading `#').

Returns `{ok, [Line]}' on success, where each line is a 1-based line number
within `Source'. Returns `{ok, []}' when no references are found or the source
cannot be parsed. Returns `{error, Diagnostics}' if the compiler port is
unavailable.

Backs `SystemNavigation referencesTo:' for System Browser-style class-reference
navigation. Mirrors `find_senders_in_source/2' (BT-2190); the underlying
visitor matches `ClassReference' AST nodes (and class names in type
annotations) instead of `MessageSend' nodes.
""".
-spec find_references_to_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_references_to_in_source(Source, ClassName) ->
    beamtalk_compiler_server:find_references_to_in_source(Source, ClassName).

-doc """
Find reads of an field in a single method's source (BT-2208).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). `Field' is the target field name as an
atom or binary (without a leading `#').

Returns `{ok, [Line]}' on success, where each line is a 1-based line number
within `Source' at which the slot is read (`self.x' outside an assignment
target). Returns `{ok, []}' when no reads are found or the source cannot be
parsed. Returns `{error, Diagnostics}' if the compiler port is unavailable.

Backs `SystemNavigation fieldReadersOf:in:' for System Browser-style
"which methods read this slot?" navigation.
""".
-spec find_field_readers_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_readers_in_source(Source, Field) ->
    beamtalk_compiler_server:find_field_readers_in_source(Source, Field).

-doc """
Find writes of an field in a single method's source (BT-2208).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). `Field' is the target field name as an
atom or binary (without a leading `#').

Returns `{ok, [Line]}' on success, where each line is a 1-based line number
within `Source' at which the slot is written (`self.x := ...', the assignment
target). Returns `{ok, []}' when no writes are found or the source cannot be
parsed. Returns `{error, Diagnostics}' if the compiler port is unavailable.

Backs `SystemNavigation fieldWritersOf:in:' for System Browser-style
"which methods write this slot?" navigation.
""".
-spec find_field_writers_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_writers_in_source(Source, Field) ->
    beamtalk_compiler_server:find_field_writers_in_source(Source, Field).

-doc """
Find Erlang FFI call sites in a single method's source (BT-2211).

`Source' is the source of a single compiled method (as returned by
`CompiledMethod source'). `Module' and `Function' name the target Erlang
function (atom or binary, without a leading `#'). `Arity' is a non-negative
integer to constrain the match to that argument count, or the atom `any' to
match any arity.

Returns `{ok, [Line]}' on success, where each line is a 1-based line number
within `Source' at which the named Erlang function is invoked through the
`Erlang' FFI bridge. Returns `{ok, []}' when no sites are found or the source
cannot be parsed. Returns `{error, Diagnostics}' if the compiler port is
unavailable.

Backs `SystemNavigation ffiSitesFor:' for System Browser-style "who calls this
Erlang function?" navigation.
""".
-spec find_ffi_sites_in_source(
    binary(), atom() | binary(), atom() | binary(), non_neg_integer() | any
) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_ffi_sites_in_source(Source, Module, Function, Arity) ->
    beamtalk_compiler_server:find_ffi_sites_in_source(Source, Module, Function, Arity).

-doc """
Resolve the byte span of a method definition in `Source' (ADR 0082 Phase 1).

Given the current on-disk source of a `.bt' file and a target
`(ClassName, Selector, Side)' (`Side' is `instance' or `class'), returns
`{ok, #{start := S, end := E}, PrevSource}' with the exact byte span of that
method's definition and the bytes currently occupying it. Splicing `PrevSource'
back into the span is a no-op, which is the load-bearing property the
`Workspace flush' splice relies on.

Resolution failures (`class_not_found' / `selector_not_found' / `ambiguous')
and transport failures (`port_error' / `noproc' / `timeout') both return
`{error, Reason, Message}'. The live-patch install hook uses a clean `{ok, ...}'
to record a *flushable* ChangeEntry with the resolved span. On error it still
appends a ChangeEntry (the patch is already installed in memory) — for
`selector_not_found' (a brand-new method not yet on disk) the entry stays
flushable with no prior span; any other error downgrades the entry to
`flushable: false' with a `not_flushable_reason'. The error never blocks or
undoes the in-memory install.

Backs the `>>' / `compile:source:' install hook.
""".
-spec resolve_method_span(binary(), atom() | binary(), atom() | binary(), instance | class) ->
    {ok, #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()}
    | {error, atom(), binary()}.
resolve_method_span(Source, ClassName, Selector, Side) ->
    beamtalk_compiler_server:resolve_method_span(Source, ClassName, Selector, Side).
