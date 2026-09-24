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
%% ADR 0079 / ADR 0126 §3: The `pid` field carries one of:
%%   - a raw `pid()` for ordinary actor handles (local or remote — a remote
%%     pid crosses transparently, `node/1` names its node),
%%   - a `{registered, Name :: atom()}` tuple for a *local* name-resolving
%%     proxy (ADR 0079), or
%%   - a `{registered, Name :: atom(), Node :: node()}` tuple for a
%%     node-qualified proxy — returned by `named:on:`/`allRegisteredOn:`
%%     (ADR 0126 §3) and by rewriting a `{registered, Name}` ref that
%%     crosses a node boundary, so it keeps resolving against its *origin*
%%     node rather than silently re-resolving against whichever node reads
%%     it. `{global, Name :: atom()}` is reserved for the opt-in
%%     cluster-unique `scope: #global` names (ADR 0126 §4) — no code
%%     constructs or matches it yet.
%% The send-site dispatch in `beamtalk_actor` recognises all shapes; a
%% `registered` tuple re-resolves the name to a pid on every send (via
%% `whereis/1`, or an `erpc` `whereis/1` on `Node` for the qualified shape)
%% + `gen_server:call(Pid, ...)`, so the held reference survives the actor
%% being restarted under its registered name.
-record(beamtalk_object, {
    % Class name (e.g., 'Counter')
    class :: atom(),
    % Class module (e.g., 'counter')
    class_mod :: atom(),
    % The actor process or a `{registered, Name}` / `{registered, Name, Node}`
    % / `{global, Name}` reference (ADR 0079, ADR 0126 §3/§4)
    pid :: pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()}
}).

%% Helper macro to recognise the name-resolving identity shape (ADR 0079 /
%% ADR 0126 §3): both the local `{registered, Name}` and the node-qualified
%% `{registered, Name, Node}` forms.
-define(IS_REGISTERED_REF(X),
    (is_tuple(X) andalso element(1, X) =:= registered andalso
        ((tuple_size(X) =:= 2 andalso is_atom(element(2, X))) orelse
            (tuple_size(X) =:= 3 andalso is_atom(element(2, X)) andalso is_atom(element(3, X)))))
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
%%     erlang_throw | missing_parameter | stdlib_shadowing |
%%     stateful_block_dispatch |
%%     node_down | remote_code_mismatch | not_serialisable |
%%     shape_version_ahead | shape_migration_failed |
%%     wire_version_unsupported | invalid_node_name | insecure_distribution
%%       (ADR 0126 §7.1, distribution and location-transparent actors — see
%%       `beamtalk_exception_handler:kind_to_class/1` for their classes)
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

%% @doc REPL/RPC wire protocol version.
%%
%% Single source of truth for the protocol version string reported by both
%% `beamtalk_version:get/0` (the desktop-attach readiness handshake, ADR
%% 0097) and `beamtalk_repl_ops_dev:handle_term(<<"describe">>, ...)` (the
%% REPL `describe` op). Before this macro the two were independent `"2.0"`
%% string literals synced only by a "keep in sync" comment — bump
%% this macro and both call sites move together.
-define(PROTOCOL_VERSION, <<"2.0">>).

%% @doc ADR 0126 §5.1 wire-envelope format version (distinct from any class's
%% own `shapeVersion`).
%%
%% Single source of truth for the leading version tag on the
%% `'$beamtalk_wire'` request envelope (`beamtalk_actor`'s send helpers and
%% `handle_call`/`handle_cast` prelude) and the `'$beamtalk_wire_reply'`
%% tag `beamtalk_actor` wraps a cross-node sync reply / future
%% resolve-or-reject value in before it crosses back over raw distribution
%% (`beamtalk_future`'s state machine unwraps it on receipt). A receiver that
%% does not know the version refuses with `wire_version_unsupported`
%% (ADR 0126 §5.1) rather than guessing at a decode.
-define(BT_WIRE_VERSION, 1).

%% @doc ADR 0110 class-var shadow write-through process-dictionary key atom
%% (ADR 0111 Phase D).
%%
%% Single source of truth for the `'$bt_class_vars_shadow'` atom shared by
%% `beamtalk_class_dispatch:invoke_class_method/7` (reads it back on the
%% `{nlr_relay, ...}` path and erases it in `after` on every path) and this
%% module's own EUnit conformance tests
%% (`runtime/apps/beamtalk_runtime/test/beamtalk_class_dispatch_tests.erl`).
%%
%% The Rust codegen side (`crates/beamtalk-core/src/codegen/core_erlang/
%% expressions.rs::generate_field_assignment`) cannot `-include` this file —
%% it emits the identical atom as literal Core Erlang text
%% (`leaf::atom("$bt_class_vars_shadow")`) into every compiled class method
%% that mutates a class var. Per CLAUDE.md's cross-Rust/Erlang-boundary rule
%% ("needs a shared conformance fixture or code generation, not a comment"),
%% `crates/beamtalk-core/src/codegen/core_erlang/tests/class_var_shadow_contract.rs`
%% reads *this exact file* at test time and asserts the atom text it emits
%% appears in it verbatim — so the two sides cannot drift silently: change
%% the atom here without updating the codegen literal (or vice versa) and
%% that Rust test fails.
-define(BT_CLASS_VARS_SHADOW_KEY_ATOM, '$bt_class_vars_shadow').

%% @doc Process-dictionary key marking
%% "this process is currently executing a `withClassMethod:` child's factory
%% method on behalf of `beamtalk_supervisor:start_child_via_class_method/4`,
%% running directly in the real OTP supervisor process (no process boundary
%% in between — see that function's `put/2` of this key)".
%%
%% `beamtalk_actor:safe_spawn/2` and
%% `beamtalk_class_instantiation:do_class_self_named_spawn/6` check this key
%% to decide whether a `self spawn`/`self spawnWith:`/`self spawnAs:`/
%% `self spawnWith:as:` inside the class method must stay LINKED (this key
%% present — the link is the supervisor's restart mechanism) or must be
%% unlinked (key absent — the more common case of a class method running
%% inside the class's own gen_server, or a plain unsupervised spawn, where a
%% link would incorrectly tie the new actor's lifetime to the caller).
%% Deliberately a *dedicated* key, not `beamtalk_class_name` /
%% `beamtalk_class_module` (also set by `start_child_via_class_method/4`,
%% but *also* set by every class gen_server's own `init/1` via
%% `beamtalk_object_class.erl` — so their presence alone cannot distinguish
%% "inside a real supervisor" from "inside a class's own gen_server").
-define(BT_SUPERVISOR_SPAWN_CONTEXT_KEY, '$bt_supervisor_spawn_context').

%% @doc Is `T` an in-flight `^` non-local return signal?
%%
%% Codegen throws the state-carrying 4-tuple `{'$bt_nlr', Token, Value,
%% State}` (ADR 0041); the 3-tuple is the legacy shape still recognised
%% by `beamtalk_result:'tryDo:'/1`. Both are control-flow signals aimed at a
%% method frame that may live in another process, so any dispatch layer that
%% might intercept a `throw` (class-side self-sends, instance-side extension
%% invocation, `tryDo:`) must relay rather than report them. Single source
%% of truth for the shape check, shared by `beamtalk_class_dispatch.erl`,
%% `beamtalk_dispatch.erl`, and `beamtalk_result.erl` — per CLAUDE.md's
%% no-duplicate-implementation rule, a future NLR shape change only needs to
%% land here.
-define(IS_NLR(T),
    (is_tuple(T) andalso
        (tuple_size(T) =:= 4 orelse tuple_size(T) =:= 3) andalso
        element(1, T) =:= '$bt_nlr')
).

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
