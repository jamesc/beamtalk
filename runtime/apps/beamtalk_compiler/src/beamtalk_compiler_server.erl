%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_server).

-behaviour(gen_server).

%%% **DDD Context:** Compilation (Anti-Corruption Layer)

-moduledoc """
Gen_server wrapping the compiler OTP Port (ADR 0022, Phase 1).

Owns the port process and serializes compilation requests. The supervisor
restarts this server (and thus re-opens the port) on crashes.

Implements in-memory Core Erlang compilation via
`core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'
to avoid temp files on disk.
""".

-include_lib("kernel/include/logger.hrl").

%% Default `gen_server:call' budget for requests that reach the compiler port.
-define(CALL_TIMEOUT_MS, 30000).
%% Budget for cheap, cache-only or latency-sensitive calls.
-define(QUICK_CALL_TIMEOUT_MS, 5000).
%% `safe_call/4' fallbacks, one pair per result shape.
-define(DIAG_DOWN, {error, [#{message => <<"Compiler server is not available">>}]}).
-define(DIAG_TIMEOUT, {error, [#{message => <<"Compiler server timed out">>}]}).
-define(SPAN_DOWN, {error, noproc, <<"Compiler server is not available">>}).
-define(SPAN_TIMEOUT, {error, timeout, <<"Compiler server timed out">>}).

%% Public API
-export([
    start_link/0, start_link/1,
    compile_expression/3, compile_expression/4,
    compile_expression_trace/3, compile_expression_trace/4,
    compile/2,
    compile_method/3,
    diagnostics/1,
    diagnostics/2,
    diagnostics/3,
    version/0,
    compile_core_erlang/1,
    parse_core_erlang/1,
    register_class/2,
    get_classes/0,
    register_protocol/2,
    register_aliases/1,
    get_aliases/0,
    resolve_completion_type/1,
    find_senders_in_source/2,
    find_all_sends_in_source/1,
    find_references_to_in_source/2,
    find_field_readers_in_source/2,
    find_field_writers_in_source/2,
    find_ffi_sites_in_source/4,
    find_announce_sites_in_source/1,
    resolve_method_span/4,
    resolve_class_span/2,
    categorize_methods/2,
    class_state_field_defaults/2,
    reindent_method_source/2,
    complete_method_source/2,
    find_selector_send_spans/3,
    find_definition_selector_spans/5,
    build_class_module_index_in_source/3,
    module_name_for_path/3,
    scan_source_dependencies/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ifdef(TEST).
-export([
    port_forwards/0,
    remove_class/1,
    remove_protocol/1,
    get_protocols/0,
    handle_compile_response/1,
    handle_diagnostics_response/1,
    handle_version_response/1,
    clear_classes/0,
    inject_diagnostics_failure/1,
    inject_diagnostics_exit/0
]).
-endif.

%% The `beamtalk_compiler_port` functions the `{port_call, F, Args}` request may
%% forward to, as `{Function, Arity}` (arity counts the leading port argument).
-define(PORT_FORWARDS, [
    {find_senders_in_source, 3},
    {find_all_sends_in_source, 2},
    {find_references_to_in_source, 3},
    {find_field_readers_in_source, 3},
    {find_field_writers_in_source, 3},
    {find_ffi_sites_in_source, 5},
    {find_announce_sites_in_source, 2},
    {resolve_method_span, 5},
    {resolve_class_span, 3},
    {find_selector_send_spans, 4},
    {find_definition_selector_spans, 6},
    {categorize_methods, 3},
    {class_state_field_defaults, 3},
    {build_class_module_index_in_source, 4},
    {module_name_for_path, 4},
    {scan_source_dependencies, 2},
    {reindent_method_source, 3},
    {complete_method_source, 3}
]).

-ifdef(TEST).
-doc "The `{Function, Arity}` set `{port_call, _, _}` may forward to (test use).".
-spec port_forwards() -> [{atom(), arity()}].
port_forwards() -> ?PORT_FORWARDS.
-endif.

-record(state, {
    port :: port() | undefined,
    %% ADR 0050 Phase 3: Accumulated class metadata cache.
    %% Maps class name atom → __beamtalk_meta/0 map.
    %% Populated via register_class/2 casts and crash recovery on init.
    %% Entries are removed when a class is removed: the runtime casts
    %% `{remove_class, Name}` from `beamtalk_class_lifecycle:purge_compiler_cache/1`.
    %% Sessions that disconnect without removing their classes leave entries
    %% behind; the `aliases` field below (ADR 0108) shares that limitation.
    classes = #{} :: #{atom() => map()},
    %% Accumulated protocol metadata cache, mirroring `classes`
    %% above but populated via `register_protocol/2`/`remove_protocol/1`
    %% casts from `beamtalk_protocol_registry:register_protocol/1` /
    %% `unregister_protocol/1` instead of class (de)registration. Maps
    %% protocol name atom -> its `register_protocol/1` `Info` map (`name`,
    %% `required_methods`, `type_params`, `extending`, ...). Threaded into
    %% `diagnostics/3` alongside `classes` (same `class_hierarchy => true`
    %% opt-in — see that function's moduledoc) so the runtime-seeded checker
    %% path can recognise a cross-file protocol instead of only ever seeing
    %% its synthetic zero-method class-cache entry, which used to defeat the
    %% nominal-mismatch escape hatch and made every selector
    %% on a protocol-typed receiver look unresolved. Not recovered on init —
    %% unlike `classes`' `recover_from_beam_modules/0`, there is no equivalent
    %% "enumerate every registered protocol in the live image" recovery path
    %% today, so a compiler-server restart starts with an empty cache until
    %% the next protocol (re-)registration repopulates it. Shares `classes`'
    %% no-removal-on-session-disconnect limitation for the same
    %% reason: this is a single node-global accumulator, not per-session.
    protocols = #{} :: #{atom() => map()},
    %% ADR 0108 hot-reload re-check trigger: ambient session type
    %% alias cache, keyed by alias name -> its reparseable `type Name =
    %% <expansion>` source line. Mirrors `classes` above: threaded into
    %% `diagnostics/3`'s request whenever the caller opts into ambient
    %% context via `class_hierarchy => true`, so a re-check round trip
    %% (`beamtalk_recheck.erl`) resolves `::` annotations against the
    %% *current* alias table instead of treating every alias name as an
    %% unresolved nominal class.
    %%
    %% Merged by name (adversarial review finding), NOT whole-map
    %% replacement: `beamtalk_compiler_server` is a single node-global
    %% process (`{local, ?MODULE}`), but each REPL session has its own
    %% independent `beamtalk_repl_state` alias table — a naive "replace with
    %% whatever this cast's caller currently has" would let one connected
    %% session's `register_aliases/1` wipe every *other* session's aliases
    %% from the ambient cache the moment two sessions are open concurrently
    %% (`beamtalk_session_sup` supports exactly this topology). Since alias
    %% names are the merge key and a name collision across two independent
    %% sessions is already a structural error the compiler itself would
    %% reject if either session tried to reference the other's colliding
    %% name, last-write-wins-per-name is the correct merge semantics here —
    %% it exactly mirrors `classes`' own per-name accumulation
    %% (`maps:put/3` in `register_class/2`'s cast handler) rather than
    %% diverging from that precedent. Like `classes`, there is no removal on
    %% session disconnect — an accepted limitation this shares with every
    %% ambient cache in this module. Not a regression introduced here: it is
    %% the pre-existing, already-accepted `classes` limitation carried over
    %% unchanged (`beamtalk_session_sup` termination pruning a departing
    %% session's names from either cache would need to weigh the same
    %% cost/benefit for both, so a fix to one without the other would just
    %% leave them inconsistent).
    aliases = #{} :: #{binary() => binary()}
}).

%%% Public API

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link([]).

-spec start_link(list()) -> gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-doc """
Compile a REPL expression.
Returns `{ok, CoreErlang, Warnings, ExprInfo}' for expressions,
`{ok, class_definition, ClassInfo}' for inline class definitions,
`{ok, method_definition, MethodInfo}' for standalone method definitions,
or `{error, Diagnostics}' on failure, where each diagnostic is a map with
`message', `line' (1-based), and optionally `hint'.
""".
-spec compile_expression(binary(), binary(), [binary()]) ->
    {ok, binary(), [beamtalk_compiler_port:warning()], beamtalk_compiler_port:expression_info()}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {ok, type_alias_definition, map()}
    | {error, [map()]}.
compile_expression(Source, ModuleName, KnownVars) ->
    compile_expression(Source, ModuleName, KnownVars, #{}).

-doc """
Compile a REPL expression with optional compilation options.

Options:
  class_superclass_index => #{binary() => binary()} — cross-file superclass info
""".
-spec compile_expression(binary(), binary(), [binary()], map()) ->
    {ok, binary(), [beamtalk_compiler_port:warning()], beamtalk_compiler_port:expression_info()}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {ok, type_alias_definition, map()}
    | {error, [map()]}.
compile_expression(Source, ModuleName, KnownVars, Options) ->
    gen_server:call(
        ?MODULE, {compile_expression, Source, ModuleName, KnownVars, Options}, ?CALL_TIMEOUT_MS
    ).

-doc "Compile a REPL expression in trace mode.".
-spec compile_expression_trace(binary(), binary(), [binary()]) ->
    {ok, binary(), [beamtalk_compiler_port:warning()], beamtalk_compiler_port:expression_info()}
    | {error, [map()]}.
compile_expression_trace(Source, ModuleName, KnownVars) ->
    compile_expression_trace(Source, ModuleName, KnownVars, #{}).

-spec compile_expression_trace(binary(), binary(), [binary()], map()) ->
    {ok, binary(), [beamtalk_compiler_port:warning()], beamtalk_compiler_port:expression_info()}
    | {error, [map()]}.
compile_expression_trace(Source, ModuleName, KnownVars, Options) ->
    gen_server:call(
        ?MODULE,
        {compile_expression_trace, Source, ModuleName, KnownVars, Options},
        ?CALL_TIMEOUT_MS
    ).

-doc """
Compile a file/class definition.
Options: #{path => binary(), stdlib_mode => boolean(),
protocol_sources => #{binary() => binary()},
protocol_source_paths => #{binary() => binary()}} — the last two are explicit,
opt-in map of protocol name to raw `.bt` source (ADR 0127 §10a / BT-3593),
used by `beamtalk_repl_loader`'s protocol-reload fan-out to carry a
just-edited protocol's full source into each of its users' own recompiles,
so `uses:` flattening sees the new provisions rather than nothing (a
cross-file protocol otherwise only resolves via the ambient, signature-only
`protocol_registry` cache below, which cannot flatten method bodies).
`project_batch => binary()' (BT-4378) tags every compile of one
`load_project' with the same token, so the port verifies the file's
project scope (aliases, FFI registry) once per batch instead of re-statting
the whole project for each file.
Returns `{ok, #{core_erlang, module_name, classes, warnings}}',
`{ok, protocol_definition, #{core_erlang, module_name, protocols, warnings}}',
or `{error, Diagnostics}'.
""".
-spec compile(binary(), map()) ->
    {ok, map()} | {ok, protocol_definition, map()} | {error, [map()]}.
compile(Source, Options) ->
    gen_server:call(?MODULE, {compile, Source, Options}, ?CALL_TIMEOUT_MS).

-doc """
Structured single-method compile (the live-image write-surface idiom).

`ClassSource` is the current full class definition; `MethodSource` is the BARE
method body (comments and all — no `Class >>` prefix). The method is parsed
standalone and merged into the class, so the stored source round-trips exactly.

Options: `#{is_class_method => boolean(), stdlib_mode => boolean(),
module_name => binary(), source_path => binary(),
class_superclass_index => map(), class_module_index => map()}`.

Returns `{ok, #{core_erlang, module_name, classes, selector, is_class_method,
method_source, merged_class_source, warnings}}' or `{error, Diagnostics}'.
""".
-spec compile_method(binary(), binary(), map()) -> {ok, map()} | {error, [map()]}.
compile_method(ClassSource, MethodSource, Options) ->
    gen_server:call(
        ?MODULE, {compile_method, ClassSource, MethodSource, Options}, ?CALL_TIMEOUT_MS
    ).

-doc "Get diagnostics for source code (no code generation).".
-spec diagnostics(binary()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source) ->
    diagnostics(Source, <<"expression">>).

-doc """
Get diagnostics for source code under a parse `Mode' (no code generation).
Equivalent to `diagnostics/3' with `#{}' — no ambient class-hierarchy
awareness (see `diagnostics/3').
""".
-spec diagnostics(binary(), binary()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source, Mode) ->
    diagnostics(Source, Mode, #{}).

-doc """
Get diagnostics for source code under a parse `Mode', with options.

Options:
  class_hierarchy => boolean() | #{atom() => map()} — when `true' (ADR 0105
  Phase 1), threads the ambient class cache (the same
  `register_class' accumulation `compile_expression'/`compile_method' already
  get, ADR 0050 Phase 4) into the request, so a receiver resolving to an
  already-loaded class is checked against that class's *current* interface.
  Defaults to `false' — deliberately opt-in, not the default for
  `diagnostics/1,2', because this command also backs the LiveView cockpit's
  keystroke-driven editor diagnostics
  (`beamtalk_repl_ops_dev:diagnostics_for/3'), and changing what fires on
  every keystroke for every existing caller is a bigger behavioural change
  than this option's one new caller (the re-check orchestration)
  needs.

  Passing a map() instead of `true' (ADR 0105 Phase 3) threads that
  map verbatim as the request's class hierarchy *instead of* the ambient
  cache — a caller-built overlay (e.g. the ambient classes with one
  not-yet-installed signature spliced in) is checked against without ever
  writing to `beamtalk_compiler_server' state, so a hypothetical signature
  never becomes visible to any other request. See
  `beamtalk_recheck:trigger_pending/5', the one caller that needs this.

  The ambient protocol-registry cache (`register_protocol/2`'s
  accumulator) rides this same opt-in — whenever the ambient class cache is
  threaded (the `true' case; a map() overlay only ever replaces the class
  side), `State#state.protocols' is threaded alongside it as the request's
  `protocol_registry'. Without this, a protocol registered in another file
  reaches the checker only as a zero-method class-cache entry (see
  `beamtalk_protocol_registry:create_protocol_class/2'), which defeats the
  nominal-mismatch escape hatch and makes every selector on
  a protocol-typed receiver look unresolved.

  source_path => binary() (BT-4325) — the file the buffer edits. When it
  sits under a `beamtalk.toml', the port checks the buffer with that
  file's project scope (its `[diagnostics]' table, cross-file and
  dependency type aliases, FFI registry) — the scope `compile' gives the
  same file on `load_project' — so the editor and a load agree. Omitted, the
  check keeps the port-process scope. Ignored in `"method"' mode.
""".
-spec diagnostics(binary(), binary(), map()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source, Mode, Options) ->
    gen_server:call(?MODULE, {diagnostics, Source, Mode, Options}, ?CALL_TIMEOUT_MS).

-doc "Get compiler version.".
-spec version() -> {ok, binary()} | {error, term()}.
version() ->
    gen_server:call(?MODULE, version, ?QUICK_CALL_TIMEOUT_MS).

-ifdef(TEST).
-doc """
Clear all cached class metadata, the ambient protocol cache, and the
ambient alias cache (test use only).

ADR 0050 Phase 3 / ADR 0108: used for test isolation —
call before tests that need a clean ambient cache. Synchronous so the next
compile sees all three caches empty.
""".
-spec clear_classes() -> ok.
clear_classes() ->
    gen_server:call(?MODULE, clear_classes, ?QUICK_CALL_TIMEOUT_MS).

-doc """
Force the *next* `diagnostics/3' call to fail with `{error, [#{message =>
Reason}]}' instead of reaching the real compiler port (test use
only).

`beamtalk_recheck:recheck_owner/5' (and `recheck_owner_for_shape/4')'s
`{failed, []}' outcome fires only on a genuine port-level failure — the
compiler port's `handle_diagnostics' always replies `status => ok', even for
source with parse/type errors (those come back as ordinary diagnostics in the
list, never a port-level `{error, _}'). That makes `failed' unreachable via
source content alone, and this codebase has no mocking library — so this is
a deterministic, self-clearing substitute: it flips a flag `handle_call/3'
checks on the *next* `{diagnostics, ...}' request only (any `Mode'/`Options'),
after which the flag resets to `undefined' and every following call reaches
the real port again. Never touches the port itself, so unrelated requests
(`compile_expression', `compile', ...) — and any diagnostics call after the
one consumed — are unaffected.
""".
-spec inject_diagnostics_failure(binary()) -> ok.
inject_diagnostics_failure(Reason) ->
    gen_server:call(?MODULE, {inject_diagnostics_failure, Reason}, ?QUICK_CALL_TIMEOUT_MS).

-doc """
Force the *next* `diagnostics/3' call to exit instead of returning a value at
all — a stricter sibling of `inject_diagnostics_failure/1' (test use
only).

`beamtalk_recheck:recheck_image_class/2' and `recheck_owner_for_leaf_change/3'
each additionally `catch` the compiler port's `gen_server:call` itself exiting
(`noproc`/`timeout`), not just an `{error, Reason}` return —
`inject_diagnostics_failure/1' cannot reach that `catch` clause, since it
still replies normally (with an error *value*) to a live call. This hook
instead arms a flag that makes the *next* `{diagnostics, ...}' request stop
this gen_server without replying at all, so the caller's `gen_server:call`
raises an `exit` — the same shape of failure a real port crash or timeout
would produce.

Deterministic and safe to use per-test: `beamtalk_compiler_sup` supervises
this server with `one_for_one`/`permanent`, so it is restarted immediately
after the stop, and the very next `diagnostics/3` call (in this test or any
other) reaches a fresh, healthy process — no explicit re-clear needed, unlike
`inject_diagnostics_failure/1`'s flag (the fault flags live in the server's
process dictionary, so a restarted process starts with none set). Never touches the port itself, and
unrelated requests (`compile_expression`, `compile`, ...) are unaffected.

**Shared crash budget:** each use spends one of `beamtalk_compiler_sup`'s
`one_for_one` restart allowance (`intensity => 5, period => 60` — production
crash tolerance, not test-specific), and that allowance is NOT reset between
test groups that never stop the `beamtalk_compiler` application between them
(e.g. within a single `*_tests.erl` module using a no-op teardown — see
`beamtalk_compiler_server_tests.erl`'s `stop_compiler/1`). Fine for a
handful of uses; a test suite piling up more than a couple of these within
one un-torn-down run risks exceeding the budget and leaving the server
permanently down (`noproc`) for every subsequent test. Prefer scoping call
sites that *do* `application:stop(beamtalk_compiler)` between test groups
(e.g. `beamtalk_recheck_tests.erl`'s `recheck_teardown/1`) when adding more.
""".
-spec inject_diagnostics_exit() -> ok.
inject_diagnostics_exit() ->
    gen_server:call(?MODULE, inject_diagnostics_exit, ?QUICK_CALL_TIMEOUT_MS).
-endif.

%% `gen_server:call/3' that degrades instead of exiting: `OnDown' is returned
%% when the server is not running, `OnTimeout' when it does not reply in time.
-spec safe_call(term(), timeout(), Down, Timeout) -> term() | Down | Timeout when
    Down :: term(), Timeout :: term().
safe_call(Request, TimeoutMs, OnDown, OnTimeout) ->
    try
        gen_server:call(?MODULE, Request, TimeoutMs)
    catch
        exit:{noproc, _} -> OnDown;
        exit:{timeout, _} -> OnTimeout
    end.

-doc """
Resolve the type of an expression for REPL completion fallback.

`Expression' is the receiver expression-up-to-cursor with the incomplete
prefix already stripped. The class hierarchy is injected automatically from
the server state (ADR 0050 Phase 4).

Returns `{ok, ClassName}' when the type is statically known, or
`{error, type_unknown}' when the type cannot be determined or the compiler
is unavailable.
""".
-spec resolve_completion_type(binary()) -> {ok, atom()} | {error, type_unknown}.
resolve_completion_type(Expression) ->
    safe_call(
        {resolve_completion_type, Expression},
        ?QUICK_CALL_TIMEOUT_MS,
        {error, type_unknown},
        {error, type_unknown}
    ).

-doc """
Find call sites of a selector in a single method's source.

Backs `SystemNavigation sendersOf:' — parses the method source and returns a list
of 1-based line numbers (relative to `Source') where the selector appears
as a `MessageSend' or `Cascade'. Returns `{ok, []}' if no senders are
found; returns `{error, Diagnostics}' if the compiler port is unavailable.
""".
-spec find_senders_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_senders_in_source(Source, Selector) ->
    safe_call(
        {port_call, find_senders_in_source, [Source, Selector]},
        ?CALL_TIMEOUT_MS,
        ?DIAG_DOWN,
        ?DIAG_TIMEOUT
    ).

-doc """
Find every message send in a single method's source.

Backs `SystemNavigation unimplementedSelectors' — parses the method source and
returns every send as a map `#{selector := binary(), line := pos_integer(),
recv := self | super | erlang_ffi | other}'. Returns `{ok, []}' if the source has no sends;
returns `{error, Diagnostics}' if the compiler port is unavailable.
""".
-spec find_all_sends_in_source(binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_all_sends_in_source(Source) ->
    safe_call(
        {port_call, find_all_sends_in_source, [Source]}, ?CALL_TIMEOUT_MS, ?DIAG_DOWN, ?DIAG_TIMEOUT
    ).

-doc """
Find every `announce:' emission in a single method's source.

Backs `SystemNavigation announcementsSentBy:' — parses the method source and
returns every announce emission as a map `#{selector := binary(),
line := pos_integer(), announcement_class := binary()}', where
`announcement_class' is the syntactically-resolved announcement class name
(empty binary when unresolvable). Returns `{ok, []}' if the source has no
emissions; returns `{error, Diagnostics}' if the compiler port is unavailable.
""".
-spec find_announce_sites_in_source(binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_announce_sites_in_source(Source) ->
    safe_call(
        {port_call, find_announce_sites_in_source, [Source]},
        ?CALL_TIMEOUT_MS,
        ?DIAG_DOWN,
        ?DIAG_TIMEOUT
    ).

-doc """
Find references to a class in a single method's source.

Backs `SystemNavigation referencesTo:' — parses the method source and returns a
list of 1-based line numbers (relative to `Source') where the class is named
as a `ClassReference' AST node or in a type annotation. Returns `{ok, []}' if
no references are found; returns `{error, Diagnostics}' if the compiler port
is unavailable.
""".
-spec find_references_to_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_references_to_in_source(Source, ClassName) ->
    safe_call(
        {port_call, find_references_to_in_source, [Source, ClassName]},
        ?CALL_TIMEOUT_MS,
        ?DIAG_DOWN,
        ?DIAG_TIMEOUT
    ).

-doc """
Find reads of an field in a single method's source.

Backs `SystemNavigation fieldReadersOf:in:' — parses the method source and
returns a list of 1-based line numbers (relative to `Source') where the named
slot is read (`self.x' outside an assignment target). Returns `{ok, []}' if no
reads are found; returns `{error, Diagnostics}' if the compiler port is
unavailable.
""".
-spec find_field_readers_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_readers_in_source(Source, Field) ->
    safe_call(
        {port_call, find_field_readers_in_source, [Source, Field]},
        ?CALL_TIMEOUT_MS,
        ?DIAG_DOWN,
        ?DIAG_TIMEOUT
    ).

-doc """
Find writes of an field in a single method's source.

Backs `SystemNavigation fieldWritersOf:in:' — parses the method source and
returns a list of 1-based line numbers (relative to `Source') where the named
slot is written (`self.x := ...', the assignment target). Returns `{ok, []}' if
no writes are found; returns `{error, Diagnostics}' if the compiler port is
unavailable.
""".
-spec find_field_writers_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_writers_in_source(Source, Field) ->
    safe_call(
        {port_call, find_field_writers_in_source, [Source, Field]},
        ?CALL_TIMEOUT_MS,
        ?DIAG_DOWN,
        ?DIAG_TIMEOUT
    ).

-doc """
Find Erlang FFI call sites in a single method's source.

Backs `SystemNavigation ffiSitesFor:' — parses the method source and returns a
list of 1-based line numbers (relative to `Source') where the named Erlang
function (`Module':`Function', optionally constrained to `Arity') is invoked
through the `Erlang' FFI bridge. `Arity' is a non-negative integer to constrain
the match, or the atom `any' to match any arity. Returns `{ok, []}' if no sites
are found; returns `{error, Diagnostics}' if the compiler port is unavailable.
""".
-spec find_ffi_sites_in_source(
    binary(), atom() | binary(), atom() | binary(), non_neg_integer() | any
) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_ffi_sites_in_source(Source, Module, Function, Arity) ->
    safe_call(
        {port_call, find_ffi_sites_in_source, [Source, Module, Function, Arity]},
        ?CALL_TIMEOUT_MS,
        ?DIAG_DOWN,
        ?DIAG_TIMEOUT
    ).

-doc """
Resolve the byte span of a method definition in `Source' (ADR 0082 Phase 1).

Backs the live-patch install hook — given the current on-disk source of a
`.bt' file and a target `(ClassName, Selector, Side)', returns
`{ok, #{start := S, end := E}, PrevSource}' with the exact byte span of that
method's definition and the bytes currently occupying it. Resolution failures
return `{error, Reason, Message}'; transport failures return
`{error, port_error | noproc | timeout, Message}'.
""".
-spec resolve_method_span(binary(), atom() | binary(), atom() | binary(), instance | class) ->
    {ok, #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()}
    | {error, atom(), binary()}.
resolve_method_span(Source, ClassName, Selector, Side) ->
    safe_call(
        {port_call, resolve_method_span, [Source, ClassName, Selector, Side]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Resolve the byte span of a class's header + state declarations in `Source'
(ADR 0082 extension) — never its methods.

Backs the CHANGES dock's disk-vs-memory diff for a `'class-def'' entry (the
cockpit `:def' tab's redefinition of an *existing* class) — given the current
on-disk source of a `.bt' file and a target `ClassName', returns
`{ok, #{start := S, end := E}, PrevSource}' with the byte span of that
class's declaration line through its last `state:'/`field:' declaration and
the bytes currently occupying it. Resolution failures return
`{error, Reason, Message}'; transport failures return
`{error, port_error | noproc | timeout, Message}'.
""".
-spec resolve_class_span(binary(), atom() | binary()) ->
    {ok, #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()}
    | {error, atom(), binary()}.
resolve_class_span(Source, ClassName) ->
    safe_call(
        {port_call, resolve_class_span, [Source, ClassName]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Resolve the exact byte span(s) of every self/super-directed send of
`OldSelector' within `MethodSource' (ADR 0114) — see
`beamtalk_compiler_port:find_selector_send_spans/4' for the full wire shape
and "why not regex" rationale. Backs `Behaviour>>renameSelector:to:''s
reference-site rewrite. Returns `{ok, Occurrences}' (a list of lists — one
inner list per matched self/super send; empty when nothing matches or a
match's keyword arity mismatched); transport failures return
`{error, port_error | noproc | timeout, Message}'.
""".
-spec find_selector_send_spans(binary(), atom() | binary(), atom() | binary()) ->
    {ok, [[#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}]]}
    | {error, atom(), binary()}.
find_selector_send_spans(MethodSource, OldSelector, NewSelector) ->
    safe_call(
        {port_call, find_selector_send_spans, [MethodSource, OldSelector, NewSelector]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Resolve `ClassName''s `(OldSelector, Side)' method DEFINITION's own bare
selector-token span(s) within `Source' (ADR 0114) — see
`beamtalk_compiler_port:find_definition_selector_spans/6' for the full wire
shape. Backs `Behaviour>>renameSelector:to:''s definition-site rewrite — a
narrow selector-token splice, never the whole method body. Returns
`{ok, Spans}' on success; resolution failures (class/selector not found,
ambiguous) return `{error, Reason, Message}'; transport failures return
`{error, port_error | noproc | timeout, Message}'.
""".
-spec find_definition_selector_spans(
    binary(), atom() | binary(), atom() | binary(), atom() | binary(), instance | class
) ->
    {ok, [#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}]}
    | {error, atom(), binary()}.
find_definition_selector_spans(Source, ClassName, OldSelector, NewSelector, Side) ->
    safe_call(
        {port_call, find_definition_selector_spans, [
            Source, ClassName, OldSelector, NewSelector, Side
        ]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Group a class's methods by its `// === Name ===' section dividers
(with `divider_span'/method `span' for the
Cockpit's section-authoring write path) — see
`beamtalk_compiler_port:categorize_methods/3' for the full wire shape.
Returns `{ok, Categories}' on success, `{error, Reason, Message}' on a
resolution failure, or `{error, noproc | timeout, Message}' on transport
failure.
""".
-spec categorize_methods(binary(), atom() | binary()) ->
    {ok, [map()]} | {error, atom(), binary()}.
categorize_methods(Source, ClassName) ->
    safe_call(
        {port_call, categorize_methods, [Source, ClassName]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Build the class→module-name index for a single `src/**/*.bt` file
— see `beamtalk_compiler_port:build_class_module_index_in_source/4' for the
full wire shape. Backs the REPL/workspace cold-load fallback for
`class_module_index' (ADR 0050,
`beamtalk_workspace_native_build:build_source_class_module_index/2'). Returns
`{ok, ModuleName, ClassNames}' on success; a bad `relative_path' returns
`{error, invalid_path_segment | reserved_test_directory, Message}'; transport failures return
`{error, port_error | noproc | timeout, Message}'.
""".
-spec build_class_module_index_in_source(binary(), binary(), binary()) ->
    {ok, binary(), [binary()]} | {error, atom(), binary()}.
build_class_module_index_in_source(Source, RelativePath, PackageName) ->
    safe_call(
        {port_call, build_class_module_index_in_source, [Source, RelativePath, PackageName]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
The module name of the source file at `Path` under `RootKind` (`src' |
`test' | `none') in package `PackageName' — see
`beamtalk_compiler_port:module_name_for_path/4' for the wire shape and the
rule. Returns `{ok, ModuleName}'; a segment outside `[A-Za-z0-9_]' returns
`{error, invalid_path_segment | reserved_test_directory, Message}'; transport failures return
`{error, port_error | noproc | timeout, Message}'.
""".
-spec module_name_for_path(binary(), binary(), src | test | none) ->
    {ok, binary()} | {error, atom(), binary()}.
module_name_for_path(Path, PackageName, RootKind) ->
    safe_call(
        {port_call, module_name_for_path, [Path, PackageName, RootKind]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Class declarations and native-module references in `Source', parsed with the
real grammar (BT-4861) — see `beamtalk_compiler_port:scan_source_dependencies/2'.
""".
-spec scan_source_dependencies(binary()) ->
    {ok, #{classes := [map()], native_refs := [binary()]}} | {error, atom(), binary()}.
scan_source_dependencies(Source) ->
    safe_call(
        {port_call, scan_source_dependencies, [Source]}, ?CALL_TIMEOUT_MS, ?SPAN_DOWN, ?SPAN_TIMEOUT
    ).

-doc """
Field-level default-value presence for `ClassName''s `state:'/`field:'
declarations in `Source' (ADR 0082 extension).

Backs `beamtalk_repl_loader:class_def_source_is_skeleton_shaped/2''s sibling
safety check before marking a `'class-def'' ChangeEntry flushable — see
`beamtalk_compiler_port:class_state_field_defaults/3''s doc for the full
"why". Returns `{ok, #{FieldNameBin => boolean()}}' on success. Resolution
failures return `{error, class_not_found, Message}'; transport failures
return `{error, port_error | noproc | timeout, Message}'.
""".
-spec class_state_field_defaults(binary(), atom() | binary()) ->
    {ok, #{binary() => boolean()}} | {error, atom(), binary()}.
class_state_field_defaults(Source, ClassName) ->
    safe_call(
        {port_call, class_state_field_defaults, [Source, ClassName]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Re-indent a canonical (column-0) method body to `BaseIndent'.

Produces the on-disk byte-span shape from the compiler's canonical
`unparse_method' output, so the live-patch install hook can store a
`source' that is a drop-in for `disk[span]'. Returns `{ok, Source}' or
`{error, Reason, Message}' on transport failure.
""".
-spec reindent_method_source(binary(), binary()) ->
    {ok, binary()} | {error, atom(), binary()}.
reindent_method_source(Source, BaseIndent) ->
    safe_call(
        {port_call, reindent_method_source, [Source, BaseIndent]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
The complete method definition for `Source' patching `Selector' (BT-4911) —
see `beamtalk_compiler_port:complete_method_source/3'. Returns `{ok, Source}'
or `{error, Reason, Message}' on transport failure.
""".
-spec complete_method_source(binary(), binary()) ->
    {ok, binary()} | {error, atom(), binary()}.
complete_method_source(Source, Selector) ->
    safe_call(
        {port_call, complete_method_source, [Source, Selector]},
        ?CALL_TIMEOUT_MS,
        ?SPAN_DOWN,
        ?SPAN_TIMEOUT
    ).

-doc """
Register a class with its metadata in the compiler server cache.

ADR 0050 Phase 3: Fire-and-forget cast. Silently dropped if the server is
not running (e.g. non-REPL compilation or test runs without the server).
""".
-spec register_class(atom(), map()) -> ok.
register_class(ClassName, MetaMap) ->
    try
        gen_server:cast(?MODULE, {register_class, ClassName, MetaMap})
    catch
        _:_ -> ok
    end,
    ok.

-ifdef(TEST).
-doc """
Remove a class from the compiler server's ambient class cache.

Called when a class is removed from the system, closing the class-removal
gap in the `classes` accumulator: redefinition already
overwrites via `register_class/2`, but until now there was no removal path,
so the compiler kept type-checking against classes long gone from the
runtime. Fire-and-forget cast, silently dropped if the server is not
running, mirroring `register_class/2`'s degrade-silently contract.

Not called from production code: `beamtalk_class_lifecycle:purge_compiler_cache/1`
(the real caller, in `beamtalk_runtime`) intentionally bypasses this wrapper
with a raw `gen_server:cast(beamtalk_compiler_server, {remove_class, ClassName})`
to avoid a compile-time dependency in the wrong direction (`beamtalk_runtime`
must not depend on `beamtalk_compiler` — see that module's doc). This
function is exported only under TEST and is exercised directly by
its own tests.
""".
-spec remove_class(atom()) -> ok.
remove_class(ClassName) ->
    try
        gen_server:cast(?MODULE, {remove_class, ClassName})
    catch
        _:_ -> ok
    end,
    ok.
-endif.

-doc """
Return the current ambient class cache map (`register_class/2`'s
accumulator).

Production caller: `beamtalk_recheck:trigger_pending/5` (ADR 0105 Phase 3)
reads this to snapshot a class's current ambient meta before
temporarily splicing a pending signature into it. Also used directly by
tests. Returns an empty map (not an error) if the server is not running,
mirroring `register_class/2`'s degrade-silently contract.
""".
-spec get_classes() -> #{atom() => map()}.
get_classes() ->
    safe_call(get_classes, ?QUICK_CALL_TIMEOUT_MS, #{}, #{}).

-doc """
Register a protocol with its metadata in the compiler server cache,
mirroring `register_class/2`.

Fire-and-forget cast. Silently dropped if the server is not running.
Production caller: `beamtalk_protocol_registry:register_protocol/1` (in
`beamtalk_runtime`), called on every protocol (re-)registration.
""".
-spec register_protocol(atom(), map()) -> ok.
register_protocol(ProtocolName, Info) ->
    try
        gen_server:cast(?MODULE, {register_protocol, ProtocolName, Info})
    catch
        _:_ -> ok
    end,
    ok.

-ifdef(TEST).
-doc """
Remove a protocol from the compiler server's ambient protocol cache,
mirroring `remove_class/1`.

Not called from production code: `beamtalk_protocol_registry:unregister_protocol/1`
(the real caller, in `beamtalk_runtime`) intentionally bypasses this wrapper
with a raw `gen_server:cast(beamtalk_compiler_server, {remove_protocol, ProtocolName})`,
for the identical wrong-direction-dependency reason documented on
`remove_class/1`. This function is exported only under TEST and is
exercised directly by its own tests.
""".
-spec remove_protocol(atom()) -> ok.
remove_protocol(ProtocolName) ->
    try
        gen_server:cast(?MODULE, {remove_protocol, ProtocolName})
    catch
        _:_ -> ok
    end,
    ok.
-endif.

-ifdef(TEST).
-doc """
Return the current ambient protocol cache map (`register_protocol/2`'s
accumulator), mirroring `get_classes/0`.

Used directly by tests. Returns an empty map (not an error) if the server is
not running, mirroring `register_class/2`'s degrade-silently contract.
""".
-spec get_protocols() -> #{atom() => map()}.
get_protocols() ->
    safe_call(get_protocols, ?QUICK_CALL_TIMEOUT_MS, #{}, #{}).
-endif.

-doc """
Merge `AliasSources` — one reparseable `type Name = <expansion>` line per
currently-known alias in *this caller's own session* — into the ambient
session type-alias cache (ADR 0108 hot-reload re-check trigger),
keyed by alias name (see the `aliases` field's doc for why this is a
per-name merge, not a whole-cache replacement — two independent REPL
sessions concurrently registering their own, disjoint alias tables must not
be able to wipe each other's entries).

Synchronous `call`, not a fire-and-forget `cast`: the one production caller
(`beamtalk_repl_eval:handle_type_alias_definition/3`) enqueues the
alias-change re-check trigger immediately afterwards
(`beamtalk_repl_loader_recheck:spawn_alias_change_recheck/1`), and that trigger's
`diagnostics/3` round trip needs this cache update to have *already landed*
by the time it runs — a `cast` only guarantees same-process mailbox
ordering (this call and the subsequent enqueue are sent from the same
process, so a cast would in practice already be safe today), but a `call`
makes that ordering an explicit contract rather than an implicit one two
different message sends must continue to satisfy by construction. Still
degrades silently (returns `ok` instead of raising) if the server is not
running, matching every other ambient-cache API in this module.
""".
-spec register_aliases([binary()]) -> ok.
register_aliases(AliasSources) when is_list(AliasSources) ->
    safe_call({register_aliases, AliasSources}, ?QUICK_CALL_TIMEOUT_MS, ok, ok).

-doc """
Return the current ambient alias cache — every currently-registered alias's
source line, across every session that has called `register_aliases/1`
(order unspecified; callers needing determinism should sort). Production
caller: `diagnostics/3` (via `handle_call`, below) reads this the same way
it reads `get_classes/0`'s cache. Also used directly by tests. Returns `[]`
(not an error) if the server is not running.
""".
-spec get_aliases() -> [binary()].
get_aliases() ->
    safe_call(get_aliases, ?QUICK_CALL_TIMEOUT_MS, [], []).

-doc """
Compile Core Erlang source to BEAM bytecode in memory.

Accepts a binary — the legacy text wire (ADR 0022). Goes through
`core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'.
""".
-spec compile_core_erlang(binary()) ->
    {ok, atom(), binary()} | {error, term()}.
compile_core_erlang(CoreErlangBin) when is_binary(CoreErlangBin) ->
    case parse_core_erlang(CoreErlangBin) of
        {ok, CoreModule} ->
            compile_core_forms(
                CoreModule, [from_core, binary, return_errors, return_warnings, clint]
            );
        {error, _} = Error ->
            Error
    end.

-doc """
Parse Core Erlang source text into a `cerl' module tree, without compiling
or loading it. Runs in the caller (no port, no gen_server call).

The scan/parse half of `compile_core_erlang/1', shared with
`beamtalk_compiler:class_meta_from_core_erlang/1' (BT-4285), which reads a
pending compile's `__beamtalk_meta/0' straight off this tree.
""".
-spec parse_core_erlang(binary()) -> {ok, cerl:c_module()} | {error, term()}.
parse_core_erlang(CoreErlangBin) when is_binary(CoreErlangBin) ->
    CoreErlangStr = binary_to_list(CoreErlangBin),
    case core_scan:string(CoreErlangStr) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, CoreModule} ->
                    {ok, CoreModule};
                {error, ParseError} ->
                    {error, {core_parse_error, ParseError}}
            end;
        {error, ScanError, _Loc} ->
            {error, {core_scan_error, ScanError}}
    end.

%% Internal: shared `compile:forms/2' arm used by both wire shapes.
%%
%% `clint' is passed explicitly by both callers above for
%% documentation purposes, but is a no-op today — see
%% `beamtalk_compile_diagnostics' moduledoc for why `core_lint' already runs
%% unconditionally on this `from_core' pipeline regardless of `clint'.
%% `strong_validation'/`basic_validation' were evaluated and are NOT
%% applicable: both skip code generation, which this path requires.
%%
%% Both callers above pass `return_warnings' so `compile:forms' surfaces
%% every warning instead of silently discarding it. This is the
%% actual Port-owning gen_server (ADR 0022 Phase 1, see moduledoc) backing
%% the REPL/LSP/in-memory compile path, so its warnings reach the
%% developer the same way `beamtalk_build_worker''s do: printed to
%% `standard_error' via `beamtalk_compile_diagnostics:print_warnings/1'.
%% `compile_core_erlang/1''s `{ok, atom(), binary()} | {error, term()}'
%% return contract is unchanged.
compile_core_forms(CoreModule, Options) ->
    case compile:forms(CoreModule, Options) of
        {ok, ModuleName, Binary} ->
            {ok, ModuleName, Binary};
        {ok, ModuleName, Binary, Warnings} ->
            beamtalk_compile_diagnostics:print_warnings(Warnings),
            {ok, ModuleName, Binary};
        {error, Errors, _Warnings} ->
            %% Warnings deliberately NOT printed here (unlike the two `ok'
            %% arms above). `Warnings' is unconditionally `[]' whenever
            %% `Errors' is non-empty for this `from_core' pipeline —
            %% `core_lint_module' is the first pass in
            %% `core_passes(non_verified_core)', before anything that can
            %% emit a warning, and `compile:forms' aborts on its first
            %% failing pass. See `beamtalk_build_worker.erl''s sibling
            %% clause for the full explanation and
            %% `beamtalk_build_worker_tests:compile_forms_error_implies_no_warnings_test/0'
            %% for the pinned regression.
            {error,
                {core_compile_error, #{
                    message => beamtalk_compile_diagnostics:format_errors(Errors),
                    raw => Errors
                }}}
    end.

%%% gen_server callbacks

init(_Args) ->
    %% Not via beamtalk_logging_config:set_domain/1 — this app must start
    %% without beamtalk_runtime on the code path (see recover_from_beam_modules/0).
    ok = logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    process_flag(trap_exit, true),
    Port = open_port(),
    %% ADR 0050 Phase 3: Recover class cache synchronously on start/restart.
    %% Compile requests arriving before recovery completes are queued by the
    %% gen_server mailbox — they will not see an empty cache.
    Classes = recover_from_beam_modules(),
    case map_size(Classes) of
        0 ->
            ok;
        N ->
            ?LOG_INFO("Recovered class cache from loaded BEAM modules", #{
                domain => [beamtalk, runtime],
                classes_recovered => N,
                class_names => maps:keys(Classes)
            })
    end,
    {ok, #state{port = Port, classes = Classes}}.

%% Test builds intercept the fault-injection requests before the real clauses
%% (BT-5023). The fault state lives in the process dictionary, not the `#state{}`
%% record, so a release build has neither the clauses nor any trace of the
%% fields; `-ifdef` can only gate whole functions, hence this wrapper.
-ifdef(TEST).
handle_call({inject_diagnostics_failure, Reason}, _From, State) ->
    put(diagnostics_fault, Reason),
    {reply, ok, State};
handle_call(inject_diagnostics_exit, _From, State) ->
    put(diagnostics_exit_fault, true),
    {reply, ok, State};
handle_call({diagnostics, _Source, _Mode, _Options} = Request, From, State) ->
    case {get(diagnostics_exit_fault), erase(diagnostics_fault)} of
        {true, _} ->
            %% See inject_diagnostics_exit/0: stop without replying so the
            %% caller's gen_server:call exits (`shutdown`, so no crash report;
            %% the `permanent` supervisor restarts it, which clears the flag).
            {stop, shutdown, State};
        {_, undefined} ->
            do_handle_call(Request, From, State);
        {_, Fault} ->
            %% See inject_diagnostics_failure/1: consumed by exactly this one
            %% request, the real port is never touched.
            {reply, {error, [#{message => Fault}]}, State}
    end;
handle_call(Request, From, State) ->
    do_handle_call(Request, From, State).
-else.
handle_call(Request, From, State) ->
    do_handle_call(Request, From, State).
-endif.

do_handle_call({compile_expression, Source, ModuleName, KnownVars, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    %% Default known_type_aliases to the ambient alias cache, the
    %% same backstop `compile`/`compile_method` apply unconditionally below —
    %% but via `maps:merge/2` so an explicit caller-supplied value wins,
    %% rather than an unconditional overwrite. `do_eval/3` (the only
    %% production caller that reaches class/protocol-definition responses)
    %% always threads `beamtalk_repl_state:known_type_alias_sources/1`'s
    %% *full* session list (session-declared aliases plus every
    %% stdlib-seeded one) explicitly; this ambient cache only ever
    %% reflects session-declared aliases (`register_aliases/1`), so an
    %% unconditional overwrite here would silently drop stdlib aliases from
    %% that caller's more complete list. This is purely a backstop for a
    %% caller that doesn't bother passing one.
    %%
    %% The ambient protocol cache rides the same unconditional
    %% injection as `class_hierarchy` — see `handle_call({compile, ...})`'s
    %% identical addition below for why this can't stay opt-in the way
    %% diagnostics/3's is.
    Options1 = inject_ambient_defaulting_aliases(Options, State),
    Result = beamtalk_compiler_port:compile_expression(
        State#state.port, Source, ModuleName, KnownVars, Options1
    ),
    {reply, Result, State};
do_handle_call({compile_expression_trace, Source, ModuleName, KnownVars, Options}, _From, State) ->
    %% See the identical `maps:merge/2` backstop on the
    %% `compile_expression` clause above — same reasoning applies here.
    Options1 = inject_ambient_defaulting_aliases(Options, State),
    Result = beamtalk_compiler_port:compile_expression_trace(
        State#state.port, Source, ModuleName, KnownVars, Options1
    ),
    {reply, Result, State};
do_handle_call({compile, Source, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    %% ADR 0108 hot-reload re-check trigger: likewise inject the
    %% ambient alias cache, unconditionally — unlike diagnostics/3's
    %% `class_hierarchy => true` opt-in (which stays class-context-free by
    %% default for the cockpit editor path), every `compile` caller
    %% installing a class/protocol into the live image needs session alias
    %% context: `AliasRegistry::add_pre_loaded`'s existing collision check
    %% (alias name vs. `hierarchy`/`protocol_registry`) can only fire when
    %% the alias table it's checking against actually includes earlier-turn
    %% aliases (`type Point = Integer` then `Actor subclass: Point` in a
    %% later turn/`:load`).
    %%
    %% The ambient protocol cache rides the same unconditional
    %% injection as `class_hierarchy` above — a live `compile` of a class
    %% whose method signature references a cross-file protocol needs it for
    %% the same nominal-mismatch/Dnu escape hatch wired for
    %% diagnostics/3.
    Options1 = inject_ambient(Options, State),
    Result = do_compile(State#state.port, Source, Options1),
    {reply, Result, State};
do_handle_call({compile_method, ClassSource, MethodSource, Options}, _From, State) ->
    %% See `handle_call({compile, ...})`'s identical comment above.
    Options1 = inject_ambient(Options, State),
    Result = do_compile_method(State#state.port, ClassSource, MethodSource, Options1),
    {reply, Result, State};
do_handle_call({resolve_completion_type, Expression}, _From, State) ->
    %% Forward class hierarchy so user-defined classes are visible.
    Result = beamtalk_compiler_port:resolve_completion_type(
        State#state.port, Expression, State#state.classes
    ),
    {reply, Result, State};
do_handle_call({diagnostics, Source, Mode, Options}, _From, State) ->
    %% ADR 0105 Phase 1: only thread the ambient class cache when
    %% explicitly requested (see diagnostics/3's moduledoc) — the default
    %% stays class-context-free so the keystroke-driven cockpit editor path
    %% is unaffected by this option's addition.
    %%
    %% ADR 0108 hot-reload re-check trigger: the ambient alias
    %% cache rides the same opt-in — a caller asking for ambient class
    %% context (a re-check round trip) needs ambient alias context for
    %% exactly the same reason, so this reuses `class_hierarchy => true`
    %% rather than adding a second, overlapping flag.
    %%
    %% ADR 0105 Phase 3: a map() overlay is threaded verbatim as
    %% this request's class hierarchy instead of `State#state.classes` — the
    %% overlay never gets written into `State`, so it is visible to this one
    %% request only, never to any other caller of this gen_server.
    %%
    %% The ambient protocol cache rides the same opt-in as `Aliases`
    %% above — an overlay only ever replaces the *class* hierarchy (the
    %% "splice a pending signature" use case has no protocol-side
    %% equivalent), so `State#state.protocols` is used verbatim in both the
    %% `true` and `Overlay` arms.
    {Classes, Aliases, Protocols} =
        case maps:get(class_hierarchy, Options, false) of
            true ->
                {
                    State#state.classes,
                    alias_source_list(State#state.aliases),
                    State#state.protocols
                };
            false ->
                {#{}, [], #{}};
            Overlay when is_map(Overlay) ->
                {Overlay, alias_source_list(State#state.aliases), State#state.protocols}
        end,
    Result = do_diagnostics(
        State#state.port,
        Source,
        Mode,
        Classes,
        Aliases,
        Protocols,
        maps:get(source_path, Options, undefined)
    ),
    {reply, Result, State};
do_handle_call({port_call, Fun, Args}, _From, #state{port = Port} = State) ->
    %% The pure forwards (BT-5046): every request that is just
    %% `beamtalk_compiler_port:Fun(Port, Args...)` shares this one clause. The
    %% set is closed (`?PORT_FORWARDS`): anything else is an unknown call, so a
    %% stray request cannot run an arbitrary port function in the server.
    case is_list(Args) andalso lists:member({Fun, length(Args) + 1}, ?PORT_FORWARDS) of
        true ->
            Result = erlang:apply(beamtalk_compiler_port, Fun, [Port | Args]),
            {reply, Result, State};
        false ->
            {reply, {error, unknown_request}, State}
    end;
do_handle_call(version, _From, State) ->
    Result = do_version(State#state.port),
    {reply, Result, State};
do_handle_call(clear_classes, _From, State) ->
    {reply, ok, State#state{classes = #{}, aliases = #{}, protocols = #{}}};
do_handle_call(get_classes, _From, State) ->
    {reply, State#state.classes, State};
do_handle_call(get_protocols, _From, State) ->
    {reply, State#state.protocols, State};
do_handle_call(get_aliases, _From, State) ->
    {reply, alias_source_list(State#state.aliases), State};
do_handle_call({register_aliases, AliasSources}, _From, State) ->
    %% ADR 0108 hot-reload re-check trigger: merge by alias name
    %% (adversarial review finding) — see the `aliases` field's doc for why
    %% this must not be a whole-cache replacement. `alias_name_from_source/1`
    %% parses each `type Name = Expansion` line to extract its merge key;
    %% a line that fails to parse (should be unreachable — every entry comes
    %% from `beamtalk_repl_state:known_type_alias_sources/1`, which always
    %% emits this exact shape) is skipped rather than crashing this call.
    NewEntries = maps:from_list([
        {Name, Source}
     || Source <- AliasSources, {ok, Name} <- [alias_name_from_source(Source)]
    ]),
    {reply, ok, State#state{aliases = maps:merge(State#state.aliases, NewEntries)}};
do_handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_class, ClassName, MetaMap}, State) ->
    %% ADR 0050 Phase 3: Accumulate class metadata; overwrite on redefinition.
    NewClasses = maps:put(ClassName, MetaMap, State#state.classes),
    {noreply, State#state{classes = NewClasses}};
handle_cast({remove_class, ClassName}, State) ->
    %% Drop a removed class from the ambient cache.
    NewClasses = maps:remove(ClassName, State#state.classes),
    {noreply, State#state{classes = NewClasses}};
handle_cast({register_protocol, ProtocolName, Info}, State) ->
    %% Accumulate protocol metadata; overwrite on re-registration,
    %% mirroring register_class/2's cast handler above.
    NewProtocols = maps:put(ProtocolName, Info, State#state.protocols),
    {noreply, State#state{protocols = NewProtocols}};
handle_cast({remove_protocol, ProtocolName}, State) ->
    %% Drop an unregistered protocol from the ambient cache,
    %% mirroring remove_class/1's cast handler above.
    NewProtocols = maps:remove(ProtocolName, State#state.protocols),
    {noreply, State#state{protocols = NewProtocols}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    ?LOG_ERROR("Compiler port exited unexpectedly", #{
        domain => [beamtalk, runtime], status => Status
    }),
    {stop, {port_exit_status, Status}, State};
handle_info({'EXIT', Port, normal}, #state{port = Port} = State) ->
    %% BT-5021: a port that closed normally is the one `port_roundtrip/6`
    %% closes after a request timeout (a late reply must not poison the next
    %% request), or one that finished after `exit_status` was consumed there.
    %% Stopping here discarded the `aliases`/`protocols` caches and every class
    %% not backed by a loaded module, which a restart cannot rebuild. Open a
    %% fresh port and keep the state. A crash (`exit_status` message above, or
    %% an abnormal EXIT below) still stops the server for its supervisor.
    ?LOG_ERROR("Compiler port closed; opening a new port", #{
        domain => [beamtalk, runtime], reason => normal
    }),
    {noreply, State#state{port = open_port()}};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    ?LOG_ERROR("Compiler port EXIT", #{domain => [beamtalk, runtime], reason => Reason}),
    {stop, {port_exit, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) when is_port(Port) ->
    (try
        beamtalk_compiler_port:close(Port)
    catch
        _:_ -> ok
    end),
    ok;
terminate(_Reason, _State) ->
    ok.

%%% Internal functions

-doc """
The ambient alias cache's values, as the `[binary()]` shape every consumer
(`known_type_aliases` request field, `get_aliases/0`'s public contract)
expects — order is whatever `maps:values/1` gives, which is unspecified but
stable within one call; no consumer of `known_type_aliases` depends on
ordering (see `extract_known_type_aliases`'s Rust-side doc, which reparses
each line into an `AliasInfo` independently of its position).
""".
-spec alias_source_list(#{binary() => binary()}) -> [binary()].
alias_source_list(AliasesMap) ->
    maps:values(AliasesMap).

-doc """
Parse a `type Name = Expansion` source line (the exact shape
`beamtalk_repl_state:known_type_alias_sources/1` produces) into its alias
name, for use as `aliases`' merge key. `{ok, Name}` on the expected shape;
`error` for anything else (should be unreachable in production — see this
function's one call site's doc — but degrading to "skip this entry" rather
than crashing the whole `register_aliases/1` call keeps one malformed entry
from taking the rest of a session's alias table down with it).
""".
-spec alias_name_from_source(binary()) -> {ok, binary()} | error.
alias_name_from_source(<<"type ", Rest/binary>>) ->
    case binary:split(Rest, <<" = ">>) of
        [Name, _Expansion] when byte_size(Name) > 0 -> {ok, Name};
        _ -> error
    end;
alias_name_from_source(_) ->
    error.

-doc """
Scan all currently loaded BEAM modules and recover user-class metadata.

ADR 0050 Phase 3 (crash recovery): On compiler server restart the port process
is gone but every Beamtalk class BEAM module is still loaded in the VM.  We
call `__beamtalk_meta/0` on each loaded module and collect the metadata, skipping:
  * Modules that do not export `__beamtalk_meta/0` (non-Beamtalk modules).
  * Classes in `all_builtins/0` — the Rust compiler already has richer data.
  * Old-format modules (no `meta_version` key) are included with their partial
    data; absent keys are treated as zero-values by the Rust deserializer.

Runs synchronously in `init/1` so compile requests queue during recovery.
""".
-spec recover_from_beam_modules() -> #{atom() => map()}.
recover_from_beam_modules() ->
    %% Guard: beamtalk_class_metadata may not be loaded if the runtime
    %% app is absent from the release (e.g. compiler-only deployment).
    Builtins =
        case erlang:function_exported(beamtalk_class_metadata, all_builtins, 0) of
            true -> beamtalk_class_metadata:all_builtins();
            false -> []
        end,
    BuiltinSet = sets:from_list(Builtins),
    %% Same absent-runtime guard as `all_builtins/0` above: without the
    %% runtime app there is no shared meta read to call, so nothing is recovered.
    ReadMeta =
        case erlang:function_exported(beamtalk_class_metadata, read_meta, 1) of
            true -> fun beamtalk_class_metadata:read_meta/1;
            false -> fun(_) -> not_available end
        end,
    AllModules = code:all_loaded(),
    lists:foldl(
        fun({Module, _Path}, Acc) ->
            case ReadMeta(Module) of
                {ok, Meta} ->
                    ClassName = maps:get(class, Meta, undefined),
                    case
                        ClassName =/= undefined andalso
                            is_atom(ClassName) andalso
                            not sets:is_element(ClassName, BuiltinSet)
                    of
                        true -> Acc#{ClassName => Meta};
                        false -> Acc
                    end;
                not_available ->
                    Acc
            end
        end,
        #{},
        AllModules
    ).

open_port() ->
    try
        beamtalk_compiler_port:open()
    catch
        error:{compiler_not_found, _} = Err:Stack ->
            ?LOG_ERROR("Failed to open compiler port", #{
                domain => [beamtalk, runtime], error => Err, stacktrace => Stack
            }),
            error(Err)
    end.

%% Set `Key' on `Request' from `Options' only when the option is present.
-spec put_defined_opts(map(), map(), [atom()]) -> map().
put_defined_opts(Request, Options, Keys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Options, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        Request,
        Keys
    ).

%% The ambient caches the server accumulates (ADR 0050 Phase 4 class
%% hierarchy, the protocol registry, ADR 0108 session type aliases), keyed
%% by the option names the port request carries.
-spec ambient(#state{}) ->
    #{class_hierarchy => map(), protocol_registry => map(), known_type_aliases => [binary()]}.
ambient(State) ->
    #{
        class_hierarchy => State#state.classes,
        protocol_registry => State#state.protocols,
        known_type_aliases => alias_source_list(State#state.aliases)
    }.

%% Inject all three ambient caches into `Options', unconditionally
%% overwriting any caller-supplied value. Used by `compile' and
%% `compile_method': every caller installing a class/protocol into the live
%% image needs the session's class, protocol and alias context (unlike
%% diagnostics/3's `class_hierarchy => true' opt-in).
-spec inject_ambient(map(), #state{}) -> map().
inject_ambient(Options, State) ->
    maps:merge(Options, ambient(State)).

%% As `inject_ambient/2', but `known_type_aliases' is only a backstop: an
%% explicit caller-supplied value wins. `do_eval/3' (the only production
%% caller reaching class/protocol-definition responses) threads the *full*
%% session alias list (session-declared plus stdlib-seeded) itself; the
%% ambient cache only reflects session-declared aliases, so overwriting would
%% silently drop the stdlib ones. Used by `compile_expression' and its trace
%% variant.
-spec inject_ambient_defaulting_aliases(map(), #state{}) -> map().
inject_ambient_defaulting_aliases(Options, State) ->
    #{known_type_aliases := Aliases} = Ambient = ambient(State),
    maps:merge(
        #{known_type_aliases => Aliases},
        maps:merge(Options, maps:remove(known_type_aliases, Ambient))
    ).

%% Send a compile (file) request via the port.
do_compile(Port, Source, Options) ->
    Request0 = #{
        command => compile,
        source => Source,
        stdlib_mode => maps:get(stdlib_mode, Options, false)
    },
    %% Optional module_name override (package-qualified naming) and source
    %% file path (`beamtalk_source' attribute).
    Request7 = beamtalk_compiler_port:add_compile_options(
        put_defined_opts(Request0, Options, [module_name, source_path]), Options
    ),
    %% ADR 0127 §10a / BT-3593: an explicit, per-call, full-source carrier
    %% for `uses:` flattening — unlike `Protocols` above (the ambient,
    %% signature-only cache every compile gets unconditionally), this is
    %% opt-in: only `beamtalk_repl_loader`'s two-stage protocol-reload
    %% fan-out passes it (`beamtalk_repl_compiler:compile_file/5`'s
    %% `PrebuiltIndexes` map merges straight into `Options` — see that
    %% function's doc), carrying the just-edited protocol's raw source for
    %% each of its users' own recompiles. Absent for every other caller, so
    %% this is `maps:get(..., #{})`-defaulted like every other optional
    %% index above, never unconditionally injected — a stale-but-nonempty
    %% ambient copy would risk silently flattening against OLD provisions
    %% instead of failing loudly the way `pre_loaded_protocol_defs` currently
    %% does when it is simply absent.
    ProtocolSources = maps:get(protocol_sources, Options, #{}),
    Request8 =
        case map_size(ProtocolSources) of
            0 -> Request7;
            _ -> Request7#{protocol_sources => ProtocolSources}
        end,
    %% BT-4828: protocol name -> `.bt` path, so codegen's method-lines table
    %% covers methods provided by a protocol in another file.
    ProtocolSourcePaths = maps:map(
        fun(_Name, P) -> unicode:characters_to_binary(P) end,
        maps:get(protocol_source_paths, Options, #{})
    ),
    Request8b =
        case map_size(ProtocolSourcePaths) of
            0 -> Request8;
            _ -> Request8#{protocol_source_paths => ProtocolSourcePaths}
        end,
    %% BT-4378: one token per `load_project' batch, so the port verifies the
    %% project scope once per batch rather than once per file.
    RequestFinal =
        case maps:get(project_batch, Options, undefined) of
            ProjectBatch when is_binary(ProjectBatch) -> Request8b#{project_batch => ProjectBatch};
            _ -> Request8b
        end,
    beamtalk_compiler_port:port_roundtrip(
        Port, RequestFinal, compile, ?CALL_TIMEOUT_MS, fun handle_compile_response/1, diag
    ).

%% Send a structured single-method compile request via the port. Mirrors
%% do_compile/3 but carries class_source + method_source + the method side, and
%% the response also yields the recovered selector + canonical method source.
do_compile_method(Port, ClassSource, MethodSource, Options) ->
    Request0 = #{
        command => compile_method,
        class_source => ClassSource,
        method_source => MethodSource,
        is_class_method => maps:get(is_class_method, Options, false),
        stdlib_mode => maps:get(stdlib_mode, Options, false)
    },
    RequestFinal = beamtalk_compiler_port:add_compile_options(
        put_defined_opts(Request0, Options, [class_name, module_name, source_path, selector]),
        Options
    ),
    beamtalk_compiler_port:port_roundtrip(
        Port,
        RequestFinal,
        compile_method,
        ?CALL_TIMEOUT_MS,
        fun handle_compile_method_response/1,
        diag
    ).

%% Send a diagnostics request via the port. `Mode' selects the parse grammar:
%% `<<"expression">>' (default, top-level script) or `<<"method">>'
%% (bare method body — the System Browser method editor). `Classes' is the
%% ambient class cache (ADR 0105 Phase 1) — ignored by the port in
%% `"method"' mode, which stays class-context-free by design. `Aliases' is
%% the ambient session type-alias cache (ADR 0108 hot-reload re-check
%% trigger), same opt-in and same `"method"'-mode exclusion.
%% `Protocols' is the ambient protocol-registry cache, same opt-in
%% and same `"method"'-mode exclusion as `Classes'/`Aliases'.
%% `SourcePath' (BT-4325), when a binary, scopes the check to that file's
%% project — see `diagnostics/3'.
do_diagnostics(Port, Source, Mode, Classes, Aliases, Protocols, SourcePath) ->
    Request0 = #{
        command => diagnostics,
        source => Source,
        mode => Mode,
        class_hierarchy => Classes,
        known_type_aliases => Aliases,
        protocol_registry => Protocols
    },
    Request =
        case SourcePath of
            Path when is_binary(Path) -> Request0#{source_path => Path};
            _ -> Request0
        end,
    beamtalk_compiler_port:port_roundtrip(
        Port, Request, diagnostics, ?CALL_TIMEOUT_MS, fun handle_diagnostics_response/1, diag
    ).

%% Send a version request via the port.
do_version(Port) ->
    Request = #{command => version},
    beamtalk_compiler_port:port_roundtrip(
        Port,
        Request,
        version,
        ?QUICK_CALL_TIMEOUT_MS,
        fun handle_version_response/1,
        fun
            (exited) -> {error, port_exited};
            (timeout) -> {error, timeout};
            (unavailable) -> {error, port_not_available};
            (malformed) -> {error, decode_error}
        end
    ).

%% Handle response from compile command.
handle_compile_response(
    #{
        status := ok,
        core_erlang := CoreErlang,
        module_name := ModuleName,
        classes := Classes,
        warnings := Warnings
    } = Response
) ->
    %% ADR 0108 hot-reload re-check trigger: the alias names this
    %% compile's annotations transitively referenced — `[]` when omitted
    %% (an older compiler-port binary predating this field). Forwarded so
    %% `beamtalk_repl_compiler`/`beamtalk_repl_loader` can populate
    %% `beamtalk_alias_xref` at class-install time.
    ReferencedAliases = maps:get(referenced_aliases, Response, []),
    {ok, #{
        core_erlang => CoreErlang,
        module_name => ModuleName,
        classes => Classes,
        warnings => Warnings,
        referenced_aliases => ReferencedAliases
    }};
%% Protocol definitions use a different response shape (kind := protocol_definition)
%% and have no `classes` key — they have `protocols` instead.
handle_compile_response(
    #{
        status := ok,
        kind := protocol_definition,
        core_erlang := CoreErlang,
        module_name := ModuleName,
        protocols := Protocols,
        warnings := Warnings
    } = Response
) ->
    %% ADR 0108 hot-reload re-check trigger:
    %% see the class-definition clause immediately above's identical field —
    %% a protocol-defining compile's own method-signature annotations get
    %% the same forwarding so `beamtalk_repl_compiler` can register the same
    %% `beamtalk_alias_xref` dependency edges a class-defining compile gets.
    ReferencedAliases = maps:get(referenced_aliases, Response, []),
    {ok, protocol_definition, #{
        core_erlang => CoreErlang,
        module_name => ModuleName,
        protocols => Protocols,
        warnings => Warnings,
        referenced_aliases => ReferencedAliases
    }};
handle_compile_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, Diagnostics};
handle_compile_response(Other) ->
    beamtalk_compiler_port:unexpected_diag_response("Unexpected compile response", Other).

%% Decode the `compile_method` response: the compiled class module plus the
%% recovered method metadata (canonical source, selector, side).
handle_compile_method_response(
    #{
        status := ok,
        kind := method_definition,
        core_erlang := CoreErlang,
        module_name := ModuleName,
        classes := Classes,
        selector := Selector,
        is_class_method := IsClassMethod,
        method_source := MethodSource,
        merged_class_source := MergedClassSource,
        warnings := Warnings
    } = Response
) ->
    %% ADR 0105 Phase 1: return_type/param_types carry the patched
    %% method's declared signature so the workspace can capture it into the
    %% signature-generation store before install. Defaulted so an older
    %% compiler-port binary that omits these keys still decodes.
    ReturnType = maps:get(return_type, Response, <<"Dynamic">>),
    ParamTypes = maps:get(param_types, Response, []),
    %% ADR 0108 hot-reload re-check trigger: see
    %% handle_compile_response/1's identical field.
    ReferencedAliases = maps:get(referenced_aliases, Response, []),
    {ok, #{
        core_erlang => CoreErlang,
        module_name => ModuleName,
        classes => Classes,
        selector => Selector,
        is_class_method => decode_bool(IsClassMethod),
        method_source => MethodSource,
        merged_class_source => MergedClassSource,
        return_type => ReturnType,
        param_types => ParamTypes,
        warnings => Warnings,
        referenced_aliases => ReferencedAliases
    }};
handle_compile_method_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, Diagnostics};
handle_compile_method_response(Other) ->
    beamtalk_compiler_port:unexpected_diag_response("Unexpected compile response", Other).

%% Normalise the port's `is_class_method` (an atom `true'/`false', or a binary
%% defensively) into an Erlang boolean.
decode_bool(true) -> true;
decode_bool(false) -> false;
decode_bool(<<"true">>) -> true;
decode_bool(_) -> false.

%% Handle response from diagnostics command.
handle_diagnostics_response(#{status := ok, diagnostics := Diagnostics}) ->
    {ok, Diagnostics};
handle_diagnostics_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, Diagnostics};
handle_diagnostics_response(Other) ->
    beamtalk_compiler_port:unexpected_diag_response("Unexpected diagnostics response", Other).

%% Handle response from version command.
handle_version_response(#{status := ok, version := Version}) ->
    {ok, Version};
handle_version_response(Other) ->
    _ = beamtalk_compiler_port:unexpected_port_response("Unexpected version response", Other),
    {error, unexpected_response}.
