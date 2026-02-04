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
    class :: atom(),           % Class name (e.g., 'Counter')
    class_mod :: atom(),       % Class module (e.g., 'counter')
    pid :: pid()               % The actor process
}).

%% @doc Structured error record for runtime errors.
%%
%% All Beamtalk errors use this consistent structure for better tooling
%% and developer experience:
%% - kind: Error category (does_not_understand, immutable_value, type_error, etc.)
%% - class: The class name where the error occurred (e.g., 'Integer')
%% - selector: The method that failed (if applicable)
%% - message: Human-readable explanation using user-facing names
%% - hint: Actionable suggestion for fixing the error
%% - details: Additional context map (arity, expected types, etc.)
%%
%% See docs/internal/design-self-as-object.md Section 3.8 for full taxonomy.
-record(beamtalk_error, {
    kind    :: atom(),              % does_not_understand | immutable_value | type_error | arity_mismatch | future_not_awaited | timeout
    class   :: atom(),              % 'Integer', 'Counter', 'String'
    selector:: atom() | undefined,  % method that failed
    message :: binary(),            % human-readable explanation
    hint    :: binary() | undefined,% actionable suggestion
    details :: map()                % additional context (arity, expected types, etc.)
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
    span  :: {binary(), integer(), integer(), integer(), integer()} | undefined
}).
