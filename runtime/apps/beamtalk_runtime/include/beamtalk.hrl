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

%% BT-4901: the reserved atoms generated Core Erlang and the runtime agree on
%% (`?BT_NLR_TAG`, `?BT_CONTINUE_TAG`, `?BT_INIT_ACK_KEY`, `?BT_CALL_STACK_KEY`,
%% `?BT_WIRE_TAG`), generated from `sentinel_tags.rs` by `beamtalk build-stdlib`.
-include_lib("beamtalk_runtime/include/beamtalk_sentinel_tags.hrl").

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
%%     it, or
%%   - a `{global, Name :: atom()}` tuple for the opt-in cluster-unique
%%     `scope: #global` names (ADR 0126 §4) — returned by
%%     `named:scope:#global`, backed directly by OTP `global` instead of
%%     the local process registry. Already node-independent (`global`
%%     names resolve the same way from every connected node), so unlike
%%     `{registered, Name}` it is never node-qualified when it crosses a
%%     node boundary — `beamtalk_pid:qualify_registered_ref/1` passes it
%%     through unchanged.
%% The send-site dispatch in `beamtalk_actor` recognises all shapes; a
%% `registered`/`global` tuple re-resolves the name to a pid on every send
%% (via `whereis/1`, an `erpc` `whereis/1` on `Node` for the qualified
%% shape, or `global:whereis_name/1` for the `global` shape)
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

%% @doc Structured error record for runtime errors.
%%
%% All Beamtalk errors use this consistent structure for better tooling
%% and developer experience:
%% - kind: Error category — one of the kinds classified by
%%   `beamtalk_exception_handler:kind_to_class/1` (BT-4912: a drift test,
%%   `crates/beamtalk-codegen/tests/error_kind_drift.rs`, keeps this list,
%%   `kind_to_class/1`, `type ExceptionKind` and the kinds Rust codegen emits
%%   in agreement):
%%     does_not_understand | arity_mismatch | immutable_value | runtime_error |
%%     index_out_of_bounds | empty_collection | empty_queue | not_found |
%%     class_not_found | no_superclass | class_already_exists |
%%     selector_not_found | dispatch_error | callback_failed | actor_dead |
%%     invalid_continuation | invalid_defer | future_not_awaited |
%%     future_expired |
%%     internal_error | key_error | pattern_mismatch | unknown_field |
%%     argument_error | resource_error | process_not_found | timeout_error |
%%     type_error | instantiation_error | erlang_exit | erlang_throw | signal |
%%     node_down | remote_code_mismatch | not_serialisable |
%%     shape_version_ahead | shape_migration_failed | wire_version_unsupported
%%     | insecure_distribution | invalid_node_name | uninitialized_state_error
%%     | stateful_block_dispatch
%%   Any other kind (e.g. `user_error`, `file_not_found`, `io_error`,
%%   `permission_denied`, workspace and supervisor operation errors) is valid
%%   and classifies as plain `Error` (ADR 0015).
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
    % human-readable explanation (a tagged String value — BT-4170)
    message :: {beamtalk_string, binary()},
    % actionable suggestion (a tagged String value — BT-4170)
    hint :: {beamtalk_string, binary()} | undefined,
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

%% @doc BT-3968: the tag on the `handle_continue/2` term carrying an actor's
%% continuation queue, `{continue, {?BT_CONTINUE_TAG, [{Selector, Args}]}}`
%% (`beamtalk_actor:add_continue/2`). Shared with `beamtalk_actor_redaction`,
%% which redacts secret-selector continuations in status and sys logs.

%% @doc BT-3968: process-dictionary key holding an actor's deferred
%% continuation queue (`self continueWith: #sel` from inside a method) as a
%% *reversed* list of `{Selector, Args}`, consumed by
%% `beamtalk_actor:apply_pending_actions/1` once the current gen_server
%% callback returns — it becomes OTP's `{continue, _}` action. Shared with
%% `beamtalk_actor_redaction`, which redacts secret-selector entries in a
%% crash report's process dictionary (BT-4666).
-define(PENDING_CONTINUE_KEY, '$bt_pending_continue').

%% @doc ADR 0126 §3 timeout (ms) for a bounded `erpc:call/5` to a peer node —
%% remote spawn/lookup (`beamtalk_actor`), `Node>>shapeManifest`, and
%% `beamtalk_node_monitor`'s connect-/reload-time shape-skew comparison
%% (Phase 4) all share this single source of truth rather than each pinning
%% its own literal (AGENTS.md "No duplicate implementations").
-define(BT_REMOTE_CALL_TIMEOUT, 5000).

%% @doc BT-5231 (ADR 0126 BT-5193 amendment): timeout (ms) for
%% `beamtalk_actor_registry:actor_started_at/1`'s remote start-time read.
%% `global`'s resolver runs it while a heal's name exchange waits, so it is
%% shorter than `?BT_REMOTE_CALL_TIMEOUT`; a read that times out is
%% `unreachable` and takes the resolver's lower-node tie-break.
-define(BT_GLOBAL_CONFLICT_READ_TIMEOUT, 1000).

%% BT-4701: the module peers are addressed by in `erpc:call/5`s that reach a
%% `beamtalk_actor` function on another node (remote spawn/lookup, class
%% candidates, the `global` conflict resolver's start-time read). The code
%% behind those functions moved to `beamtalk_actor_registry` / `_remote` /
%% `_ffi`, but a peer that predates the split only has them here, so every
%% such call keeps naming this module (its delegates forward). One literal
%% rather than one per caller (AGENTS.md "No duplicate implementations").
-define(BT_ACTOR_WIRE_MODULE, beamtalk_actor).

%% Default `gen_server:stop/3` timeout for `stop` / `stopWith:` (ms). Shared by
%% `beamtalk_actor`'s stop paths and `beamtalk_actor_send`'s `stop` selectors.
-define(STOP_TIMEOUT, 5000).

%% BT-3992: how long a starter (`spawn`, a supervisor starting an actor
%% child) waits for the new actor's `initialize` to return, in ms. Shared by
%% `beamtalk_actor`'s await and `beamtalk_supervisor`'s init-timeout hint
%% (BT-4033), so the hint always names the real limit.
-define(BT_INITIALIZE_TIMEOUT, 5000).

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

%% BT-5213: the head of the raw error term a `String` or `Symbol` ordering
%% comparison (`<`, `<=`, `>`, `>=`) raises for a right operand of another kind:
%% `erlang:error({?BT_ORDERING_TYPE_ERROR_TAG, Left, Op, Right})`. Codegen
%% raises it frame-free on both send paths (`operators::ORDERING_TYPE_ERROR_TAG`
%% in beamtalk-codegen); `beamtalk_exception_handler:wrap_raw/2` turns it into
%% the structured `type_error`. The beamtalk-codegen test
%% `the_ordering_type_error_tag_matches_the_runtime_header` reads this line.
-define(BT_ORDERING_TYPE_ERROR_TAG, beamtalk_ordering_type_error).

%% @doc Is `T` an in-flight `^` non-local return signal?
%%
%% Codegen throws the state-carrying 4-tuple `{'$bt_nlr', Token, Value,
%% State}` (ADR 0041); the 3-tuple is the legacy shape still recognised
%% by `beamtalk_result:tryDo/1`. Both are control-flow signals aimed at a
%% method frame that may live in another process, so any dispatch layer that
%% might intercept a `throw` (class-side self-sends, instance-side extension
%% invocation, `tryDo:`, class-variable `protect/1`, JSON encoding) must
%% relay rather than report them. Single source of truth for the shape check
%% across the runtime and stdlib apps — per AGENTS.md's no-duplicate-
%% implementation rule, a future NLR shape change only needs to land here.
%% `just lint-nlr-tuple-matches` (scripts/ci/lint-nlr-tuple-matches.sh)
%% rejects hand-rolled `{'$bt_nlr', ...}` patterns elsewhere in runtime
%% sources.
-define(IS_NLR(T),
    (is_tuple(T) andalso
        (tuple_size(T) =:= 4 orelse tuple_size(T) =:= 3) andalso
        element(1, T) =:= ?BT_NLR_TAG)
).

%% @doc The state-carrying `^` signal codegen throws (ADR 0041), for runtime
%% code that must throw one itself (`beamtalk_guard_return`, BT-4415). Read
%% it back with `?IS_NLR/1` and `element(2, T)` for the token.
-define(NLR(Token, Value, State), {?BT_NLR_TAG, Token, Value, State}).

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
    ?BT_CLASS_TAG_KEY := 'CompiledMethod',
    '__selector__' := atom(),
    '__source__' := binary(),
    '__method_info__' := map(),
    '__doc__' := binary() | nil
}.

%% BT-4676: name-resolving proxy refs (ADR 0079 / 0126) accepted by the send
%% functions; resolved by resolve_proxy/1.
-type proxy_ref() :: {registered, atom()} | {registered, atom(), node()} | {global, atom()}.
-define(IS_PROXY_REF(R),
    (is_tuple(R) andalso
        ((tuple_size(R) =:= 2 andalso is_atom(element(2, R)) andalso
            (element(1, R) =:= registered orelse element(1, R) =:= global)) orelse
            (tuple_size(R) =:= 3 andalso element(1, R) =:= registered andalso
                is_atom(element(2, R)) andalso is_atom(element(3, R)))))
).
