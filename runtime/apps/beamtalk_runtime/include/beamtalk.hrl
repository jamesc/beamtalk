%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk shared record definitions.
%%%
%%% This header file defines records used by the Beamtalk runtime modules.
%%% Runtime Erlang modules that need these records should include it with:
%%%   -include("beamtalk.hrl").
%%%
%%% Note: Generated Core Erlang modules do not emit this include directive,
%%% but they do generate code that uses the record tuple representation directly.

%% @doc Object reference record.
%%
%% This record bundles class metadata with the actor pid, enabling proper
%% object semantics and reflection:
%% - class: The class name atom (e.g., 'Counter')
%% - class_mod: The class module atom (e.g., 'counter')
%% - pid: The actor process pid
%%
%% Generated code creates these records in spawn/0 and spawn/1 functions:
%%   {'beamtalk_object', 'Counter', 'counter', Pid}
%%
%% Message sends extract the pid using element/2:
%%   call 'erlang':'element'(4, Obj)
%%
%% Following LFE Flavors' #flavor-instance{} pattern.
-record(beamtalk_object, {
    % Class name (e.g., 'Counter')
    class :: atom(),
    % Class module (e.g., 'counter')
    class_mod :: atom(),
    % The actor process
    pid :: pid()
}).

%% @doc Structured error record for runtime errors.
%%
%% All Beamtalk errors use this consistent structure for better tooling
%% and developer experience:
%% - kind: Error category — one of:
%%     does_not_understand | immutable_value | type_error | arity_mismatch |
%%     future_not_awaited | timeout | instantiation_error | file_not_found |
%%     permission_denied | invalid_path | io_error | class_not_found |
%%     no_superclass | class_already_exists | internal_error | dispatch_error |
%%     callback_failed | assertion_failed | runtime_error | erlang_exit |
%%     erlang_throw | missing_parameter | stdlib_shadowing
%% - class: The class name where the error occurred (e.g., 'Integer')
%% - selector: The method that failed (if applicable)
%% - message: Human-readable explanation using user-facing names
%% - hint: Actionable suggestion for fixing the error
%% - details: Additional context map (arity, expected types, etc.)
%%
%% See docs/internal/design-self-as-object.md Section 3.8 for full taxonomy.
-record(beamtalk_error, {
    % Error category (see doc above)
    kind :: atom(),
    % 'Integer', 'Counter', 'String'
    class :: atom(),
    % method that failed
    selector :: atom() | undefined,
    % human-readable explanation
    message :: binary(),
    % actionable suggestion
    hint :: binary() | undefined,
    % additional context (arity, expected types, etc.)
    details :: map()
}).

%% @doc Located error wrapper for compile-time errors with source spans.
%%
%% Compile-time errors include source location information.
%% Runtime errors use stack traces instead, so they don't need spans.
%%
%% - error: The underlying beamtalk_error
%% - span: Source location {file, start_line, start_col, end_line, end_col}
-record(located_error, {
    error :: #beamtalk_error{},
    span :: {binary(), integer(), integer(), integer(), integer()} | undefined
}).

%% @doc CompiledMethod value object type.
%%
%% DDD Context: Object System
%%
%% Represents a method's metadata as returned by the >> operator.
%% This is a tagged map (value type) with '$beamtalk_class' => 'CompiledMethod'.
%%
%% Fields:
%% - $beamtalk_class: Always 'CompiledMethod' (class tag)
%% - __selector__: Method name atom (e.g., getValue, increment)
%% - __source__: Source code binary (or <<"">> if unavailable)
%% - __method_info__: Map with arity and block function
-type compiled_method() :: #{
    '$beamtalk_class' := 'CompiledMethod',
    '__selector__' := atom(),
    '__source__' := binary(),
    '__method_info__' := map()
}.
