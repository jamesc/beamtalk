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
%% ADR 0079 (BT-1990): The `pid` field carries either:
%%   - a raw `pid()` for ordinary actor handles, or
%%   - a `{registered, Name :: atom()}` tuple for name-resolving proxies.
%% The send-site dispatch in `beamtalk_actor` recognises both shapes; the
%% latter forces `gen_server:call(Name, ...)` so that the held reference
%% survives the actor being restarted under its registered name.
-record(beamtalk_object, {
    % Class name (e.g., 'Counter')
    class :: atom(),
    % Class module (e.g., 'counter')
    class_mod :: atom(),
    % The actor process or a `{registered, Name}` reference (ADR 0079)
    pid :: pid() | {registered, atom()}
}).

%% Helper macro to recognise the name-resolving identity shape (ADR 0079).
-define(IS_REGISTERED_REF(X),
    (is_tuple(X) andalso tuple_size(X) =:= 2 andalso element(1, X) =:= registered andalso
        is_atom(element(2, X)))
).

%% @doc Structured error record for runtime errors.
%%
%% All Beamtalk errors use this consistent structure for better tooling
%% and developer experience:
%% - kind: Error category — one of:
%%     does_not_understand | immutable_value | type_error | arity_mismatch |
%%     future_not_awaited | timeout | instantiation_error | file_not_found |
%%     permission_denied | io_error | class_not_found |
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

%% @doc Type alias for Beamtalk class/object references passed through FFI.
%%
%% Beamtalk objects are represented as tuples at the BEAM level. This broad type
%% alias replaces bare `term()` in FFI specs to signal that a parameter is
%% expected to be a Beamtalk object (class tuple or actor reference), not an
%% arbitrary term.
-type beamtalk_object() :: tuple().
-export_type([beamtalk_object/0]).

%% Maximum class hierarchy depth before aborting chain walks.
%% Prevents infinite loops if the ETS hierarchy table ever contains a cycle.
-define(MAX_HIERARCHY_DEPTH, 20).

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
    '__method_info__' := map(),
    '__doc__' := binary() | nil
}.
