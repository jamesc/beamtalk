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
to avoid temp files on disk (BT-48).
""".

-include_lib("kernel/include/logger.hrl").

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
    register_class/2,
    get_classes/0,
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
    reindent_method_source/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ifdef(TEST).
-export([
    handle_compile_response/1,
    handle_diagnostics_response/1,
    handle_version_response/1,
    clear_classes/0,
    inject_diagnostics_failure/1,
    inject_diagnostics_exit/0
]).
-endif.

-record(state, {
    port :: port() | undefined,
    %% ADR 0050 Phase 3: Accumulated class metadata cache.
    %% Maps class name atom → __beamtalk_meta/0 map.
    %% Populated via register_class/2 casts and crash recovery on init.
    %% Accumulates entries from every session that has ever called
    %% register_class/2, with no removal on session disconnect — the
    %% `aliases` field below (ADR 0108, BT-2899) was modeled on this same
    %% cache and shares this exact limitation (BT-2916); see its doc for the
    %% full rationale, which applies here unchanged.
    classes = #{} :: #{atom() => map()},
    %% ADR 0108 hot-reload re-check trigger (BT-2899): ambient session type
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
    %% unchanged (BT-2916 tracks this as a cross-reference, not a bug —
    %% `beamtalk_session_sup` termination pruning a departing session's
    %% names from either cache would need to weigh the same cost/benefit for
    %% both, so a fix to one without the other would just leave them
    %% inconsistent).
    aliases = #{} :: #{binary() => binary()},
    %% BT-2832 (test-only): when set (via inject_diagnostics_failure/1, only
    %% exported in TEST builds), the *next* diagnostics/3 call fails with this
    %% reason instead of reaching the real compiler port, then self-clears.
    %% Always `undefined` in production — nothing outside TEST builds can set
    %% it. See inject_diagnostics_failure/1's doc for why this exists.
    diagnostics_fault = undefined :: undefined | binary(),
    %% BT-2806 (test-only): when `true` (via inject_diagnostics_exit/0, only
    %% exported in TEST builds), the *next* diagnostics/3 call stops this
    %% process instead of replying, simulating the compiler port's
    %% gen_server:call itself exiting (`noproc`/`timeout`) rather than an
    %% ordinary `{error, _}` return. No self-clear flag needed, unlike
    %% `diagnostics_fault` above — the process restart itself is the clear
    %% (a fresh `init/1` always starts with `false`). Always `false` in
    %% production. See inject_diagnostics_exit/0's doc for why this exists.
    diagnostics_exit_fault = false :: boolean()
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
    | {ok, type_alias_definition, map()}
    | {error, [map()]}.
compile_expression(Source, ModuleName, KnownVars) ->
    compile_expression(Source, ModuleName, KnownVars, #{}).

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
    | {ok, type_alias_definition, map()}
    | {error, [map()]}.
compile_expression(Source, ModuleName, KnownVars, Options) ->
    gen_server:call(
        ?MODULE, {compile_expression, Source, ModuleName, KnownVars, Options}, 30000
    ).

-doc "Compile a REPL expression in trace mode (BT-1238).".
-spec compile_expression_trace(binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]} | {error, [map()]}.
compile_expression_trace(Source, ModuleName, KnownVars) ->
    compile_expression_trace(Source, ModuleName, KnownVars, #{}).

-spec compile_expression_trace(binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]} | {error, [map()]}.
compile_expression_trace(Source, ModuleName, KnownVars, Options) ->
    gen_server:call(
        ?MODULE, {compile_expression_trace, Source, ModuleName, KnownVars, Options}, 30000
    ).

-doc """
Compile a file/class definition.
Options: #{path => binary(), stdlib_mode => boolean(), workspace_mode => boolean()}
Returns `{ok, #{core_erlang, module_name, classes, warnings}}',
`{ok, protocol_definition, #{core_erlang, module_name, protocols, warnings}}',
or `{error, Diagnostics}'.
""".
-spec compile(binary(), map()) ->
    {ok, map()} | {ok, protocol_definition, map()} | {error, [map()]}.
compile(Source, Options) ->
    gen_server:call(?MODULE, {compile, Source, Options}, 30000).

-doc """
Structured single-method compile (the live-image write-surface idiom).

`ClassSource` is the current full class definition; `MethodSource` is the BARE
method body (comments and all — no `Class >>` prefix). The method is parsed
standalone and merged into the class, so the stored source round-trips exactly.

Options: `#{is_class_method => boolean(), stdlib_mode => boolean(),
workspace_mode => boolean(), module_name => binary(), source_path => binary(),
class_superclass_index => map(), class_module_index => map()}`.

Returns `{ok, #{core_erlang, module_name, classes, selector, is_class_method,
method_source, merged_class_source, warnings}}' or `{error, Diagnostics}'.
""".
-spec compile_method(binary(), binary(), map()) -> {ok, map()} | {error, [map()]}.
compile_method(ClassSource, MethodSource, Options) ->
    gen_server:call(?MODULE, {compile_method, ClassSource, MethodSource, Options}, 30000).

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
  class_hierarchy => boolean() — when `true' (ADR 0105 Phase 1, BT-2778),
  threads the ambient class cache (the same `register_class' accumulation
  `compile_expression'/`compile_method' already get, ADR 0050 Phase 4) into
  the request, so a receiver resolving to an already-loaded class is checked
  against that class's *current* interface. Defaults to `false' —
  deliberately opt-in, not the default for `diagnostics/1,2', because this
  command also backs the LiveView cockpit's keystroke-driven editor
  diagnostics (BT-2556, `beamtalk_repl_ops_dev:diagnostics_for/2'), and
  changing what fires on every keystroke for every existing caller is a
  bigger behavioural change than this option's one new caller (BT-2778's
  re-check orchestration) needs.
""".
-spec diagnostics(binary(), binary(), map()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source, Mode, Options) ->
    gen_server:call(?MODULE, {diagnostics, Source, Mode, Options}, 30000).

-doc "Get compiler version.".
-spec version() -> {ok, binary()} | {error, term()}.
version() ->
    gen_server:call(?MODULE, version, 5000).

-ifdef(TEST).
-doc """
Clear all cached class metadata *and* the ambient alias cache (test use
only).

ADR 0050 Phase 3 / ADR 0108 (BT-2899): used for test isolation — call before
tests that need a clean ambient cache. Synchronous so the next compile sees
both caches empty.
""".
-spec clear_classes() -> ok.
clear_classes() ->
    gen_server:call(?MODULE, clear_classes, 5000).

-doc """
Force the *next* `diagnostics/3' call to fail with `{error, [#{message =>
Reason}]}' instead of reaching the real compiler port (BT-2832, test use
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
    gen_server:call(?MODULE, {inject_diagnostics_failure, Reason}, 5000).

-doc """
Force the *next* `diagnostics/3' call to exit instead of returning a value at
all — a stricter sibling of `inject_diagnostics_failure/1' (BT-2806, test use
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
`inject_diagnostics_failure/1`'s flag (a fresh `init/1` always starts with
`diagnostics_exit_fault = false`). Never touches the port itself, and
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
    gen_server:call(?MODULE, inject_diagnostics_exit, 5000).
-endif.

-doc """
Resolve the type of an expression for REPL completion fallback (BT-1068).

`Expression' is the receiver expression-up-to-cursor with the incomplete
prefix already stripped. The class hierarchy is injected automatically from
the server state (ADR 0050 Phase 4).

Returns `{ok, ClassName}' when the type is statically known, or
`{error, type_unknown}' when the type cannot be determined or the compiler
is unavailable.
""".
-spec resolve_completion_type(binary()) -> {ok, atom()} | {error, type_unknown}.
resolve_completion_type(Expression) ->
    try
        gen_server:call(?MODULE, {resolve_completion_type, Expression}, 5000)
    catch
        exit:{noproc, _} -> {error, type_unknown};
        exit:timeout -> {error, type_unknown}
    end.

-doc """
Find call sites of a selector in a single method's source (BT-2190).

Backs `SystemNavigation sendersOf:' — parses the method source and returns a list
of 1-based line numbers (relative to `Source') where the selector appears
as a `MessageSend' or `Cascade'. Returns `{ok, []}' if no senders are
found; returns `{error, Diagnostics}' if the compiler port is unavailable.
""".
-spec find_senders_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_senders_in_source(Source, Selector) ->
    try
        gen_server:call(?MODULE, {find_senders_in_source, Source, Selector}, 30000)
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

-doc """
Find every message send in a single method's source (BT-2206).

Backs `SystemNavigation unimplementedSelectors' — parses the method source and
returns every send as a map `#{selector := binary(), line := pos_integer(),
recv := self | super | erlang_ffi | other}'. Returns `{ok, []}' if the source has no sends;
returns `{error, Diagnostics}' if the compiler port is unavailable.
""".
-spec find_all_sends_in_source(binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_all_sends_in_source(Source) ->
    try
        gen_server:call(?MODULE, {find_all_sends_in_source, Source}, 30000)
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

-doc """
Find every `announce:' emission in a single method's source (BT-2475).

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
    try
        gen_server:call(?MODULE, {find_announce_sites_in_source, Source}, 30000)
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

-doc """
Find references to a class in a single method's source (BT-2203).

Backs `SystemNavigation referencesTo:' — parses the method source and returns a
list of 1-based line numbers (relative to `Source') where the class is named
as a `ClassReference' AST node or in a type annotation. Returns `{ok, []}' if
no references are found; returns `{error, Diagnostics}' if the compiler port
is unavailable.
""".
-spec find_references_to_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_references_to_in_source(Source, ClassName) ->
    try
        gen_server:call(?MODULE, {find_references_to_in_source, Source, ClassName}, 30000)
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

-doc """
Find reads of an field in a single method's source (BT-2208).

Backs `SystemNavigation fieldReadersOf:in:' — parses the method source and
returns a list of 1-based line numbers (relative to `Source') where the named
slot is read (`self.x' outside an assignment target). Returns `{ok, []}' if no
reads are found; returns `{error, Diagnostics}' if the compiler port is
unavailable.
""".
-spec find_field_readers_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_readers_in_source(Source, Field) ->
    try
        gen_server:call(?MODULE, {find_field_readers_in_source, Source, Field}, 30000)
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

-doc """
Find writes of an field in a single method's source (BT-2208).

Backs `SystemNavigation fieldWritersOf:in:' — parses the method source and
returns a list of 1-based line numbers (relative to `Source') where the named
slot is written (`self.x := ...', the assignment target). Returns `{ok, []}' if
no writes are found; returns `{error, Diagnostics}' if the compiler port is
unavailable.
""".
-spec find_field_writers_in_source(binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_writers_in_source(Source, Field) ->
    try
        gen_server:call(?MODULE, {find_field_writers_in_source, Source, Field}, 30000)
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

-doc """
Find Erlang FFI call sites in a single method's source (BT-2211).

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
    try
        gen_server:call(
            ?MODULE, {find_ffi_sites_in_source, Source, Module, Function, Arity}, 30000
        )
    catch
        exit:{noproc, _} ->
            {error, [#{message => <<"Compiler server is not available">>}]};
        exit:{timeout, _} ->
            {error, [#{message => <<"Compiler server timed out">>}]}
    end.

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
    try
        gen_server:call(
            ?MODULE, {resolve_method_span, Source, ClassName, Selector, Side}, 30000
        )
    catch
        exit:{noproc, _} ->
            {error, noproc, <<"Compiler server is not available">>};
        exit:{timeout, _} ->
            {error, timeout, <<"Compiler server timed out">>}
    end.

-doc """
Re-indent a canonical (column-0) method body to `BaseIndent' (BT-2584).

Produces the on-disk byte-span shape from the compiler's canonical
`unparse_method' output, so the live-patch install hook can store a
`source' that is a drop-in for `disk[span]'. Returns `{ok, Source}' or
`{error, Reason, Message}' on transport failure.
""".
-spec reindent_method_source(binary(), binary()) ->
    {ok, binary()} | {error, atom(), binary()}.
reindent_method_source(Source, BaseIndent) ->
    try
        gen_server:call(?MODULE, {reindent_method_source, Source, BaseIndent}, 30000)
    catch
        exit:{noproc, _} ->
            {error, noproc, <<"Compiler server is not available">>};
        exit:{timeout, _} ->
            {error, timeout, <<"Compiler server timed out">>}
    end.

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

-doc """
Return the current ambient class cache map (`register_class/2`'s
accumulator).

Production caller: `beamtalk_recheck:trigger_pending/5` (ADR 0105 Phase 3,
BT-2782) reads this to snapshot a class's current ambient meta before
temporarily splicing a pending signature into it. Also used directly by
tests. Returns an empty map (not an error) if the server is not running,
mirroring `register_class/2`'s degrade-silently contract.
""".
-spec get_classes() -> #{atom() => map()}.
get_classes() ->
    try
        gen_server:call(?MODULE, get_classes, 5000)
    catch
        exit:{noproc, _} -> #{};
        exit:{timeout, _} -> #{}
    end.

-doc """
Merge `AliasSources` — one reparseable `type Name = <expansion>` line per
currently-known alias in *this caller's own session* — into the ambient
session type-alias cache (ADR 0108 hot-reload re-check trigger, BT-2899),
keyed by alias name (see the `aliases` field's doc for why this is a
per-name merge, not a whole-cache replacement — two independent REPL
sessions concurrently registering their own, disjoint alias tables must not
be able to wipe each other's entries).

Synchronous `call`, not a fire-and-forget `cast`: the one production caller
(`beamtalk_repl_eval:handle_type_alias_definition/3`) enqueues the
alias-change re-check trigger immediately afterwards
(`beamtalk_repl_loader:spawn_alias_change_recheck/1`), and that trigger's
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
    try
        gen_server:call(?MODULE, {register_aliases, AliasSources}, 5000)
    catch
        exit:{noproc, _} -> ok;
        exit:{timeout, _} -> ok
    end.

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
    try
        gen_server:call(?MODULE, get_aliases, 5000)
    catch
        exit:{noproc, _} -> [];
        exit:{timeout, _} -> []
    end.

-doc """
Compile Core Erlang source to BEAM bytecode in memory.

Two input shapes are accepted:

  * A binary — the legacy text wire (ADR 0022). Goes through
    `core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'.
  * `{cerl, Etf}' — the ADR 0088 Phase 0b napkin wire (BT-2315). The
    `Etf' binary is decoded via `binary_to_term(_, [safe])' and fed
    straight to `compile:forms/2' with `from_core'; the scan/parse pass is
    skipped entirely. The `[safe]' decode is the atom-table-safety
    guarantee from ADR 0022: the cerl record tags are a fixed finite set
    and the Beamtalk-generated module/function atoms are already
    pre-allocated in the text path, so no unknown atoms can reach the VM
    from a Phase 0b payload.

The `{cerl, Etf}' variant is a side-channel for Phase 0b napkin tests
only — it is **not** reachable from production compile paths today.
""".
-spec compile_core_erlang(binary() | {cerl, binary()}) ->
    {ok, atom(), binary()} | {error, term()}.
compile_core_erlang({cerl, Etf}) when is_binary(Etf) ->
    %% ADR 0088 Phase 0b napkin wire (BT-2315).
    %% [safe] prevents atom exhaustion: cerl record tags (c_module, c_literal,
    %% c_var, c_fun, ...) are a fixed finite set already known to the VM via
    %% the OTP `cerl' module, and Beamtalk-generated names are pre-allocated
    %% by the existing text path. Any unknown atom in a Phase 0b payload
    %% indicates a bug, not legitimate traffic, and `badarg' is the right
    %% failure mode.
    try binary_to_term(Etf, [safe]) of
        CoreModule ->
            compile_core_forms(CoreModule, [from_core, binary, return_errors])
    catch
        error:badarg ->
            {error, {cerl_decode_error, unsafe_atoms_or_malformed_etf}}
    end;
compile_core_erlang(CoreErlangBin) when is_binary(CoreErlangBin) ->
    CoreErlangStr = binary_to_list(CoreErlangBin),
    case core_scan:string(CoreErlangStr) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, CoreModule} ->
                    compile_core_forms(
                        CoreModule, [from_core, binary, return_errors]
                    );
                {error, ParseError} ->
                    {error, {core_parse_error, ParseError}}
            end;
        {error, ScanError, _Loc} ->
            {error, {core_scan_error, ScanError}}
    end.

%% Internal: shared `compile:forms/2' arm used by both wire shapes.
compile_core_forms(CoreModule, Options) ->
    case compile:forms(CoreModule, Options) of
        {ok, ModuleName, Binary} ->
            {ok, ModuleName, Binary};
        {ok, ModuleName, Binary, _Warnings} ->
            {ok, ModuleName, Binary};
        {error, Errors, _Warnings} ->
            {error, {core_compile_error, Errors}}
    end.

%%% gen_server callbacks

init(_Args) ->
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

handle_call({compile_expression, Source, ModuleName, KnownVars, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    %% BT-2956: default known_type_aliases to the ambient alias cache, the
    %% same backstop `compile`/`compile_method` apply unconditionally below —
    %% but via `maps:merge/2` so an explicit caller-supplied value wins,
    %% rather than an unconditional overwrite. `do_eval/3` (the only
    %% production caller that reaches class/protocol-definition responses)
    %% always threads `beamtalk_repl_state:known_type_alias_sources/1`'s
    %% *full* session list (session-declared aliases plus every
    %% stdlib-seeded one, BT-2938) explicitly; this ambient cache only ever
    %% reflects session-declared aliases (`register_aliases/1`), so an
    %% unconditional overwrite here would silently drop stdlib aliases from
    %% that caller's more complete list. This is purely a backstop for a
    %% caller that doesn't bother passing one.
    Options1 = maps:merge(
        #{known_type_aliases => alias_source_list(State#state.aliases)},
        Options#{class_hierarchy => State#state.classes}
    ),
    Result = beamtalk_compiler_port:compile_expression(
        State#state.port, Source, ModuleName, KnownVars, Options1
    ),
    {reply, Result, State};
handle_call({compile_expression_trace, Source, ModuleName, KnownVars, Options}, _From, State) ->
    %% BT-2956: see the identical `maps:merge/2` backstop on the
    %% `compile_expression` clause above — same reasoning applies here.
    Options1 = maps:merge(
        #{known_type_aliases => alias_source_list(State#state.aliases)},
        Options#{class_hierarchy => State#state.classes}
    ),
    Result = beamtalk_compiler_port:compile_expression_trace(
        State#state.port, Source, ModuleName, KnownVars, Options1
    ),
    {reply, Result, State};
handle_call({compile, Source, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    %% ADR 0108 hot-reload re-check trigger (BT-2899): likewise inject the
    %% ambient alias cache, unconditionally — unlike diagnostics/3's
    %% `class_hierarchy => true` opt-in (which stays class-context-free by
    %% default for the cockpit editor path, BT-2556), every `compile` caller
    %% installing a class/protocol into the live image needs session alias
    %% context: `AliasRegistry::add_pre_loaded`'s existing collision check
    %% (alias name vs. `hierarchy`/`protocol_registry`) can only fire when
    %% the alias table it's checking against actually includes earlier-turn
    %% aliases — this is the fix for the BT-2912 concrete repro (`type Point
    %% = Integer` then `Actor subclass: Point` in a later turn/`:load`).
    Options1 = Options#{
        class_hierarchy => State#state.classes,
        known_type_aliases => alias_source_list(State#state.aliases)
    },
    Result = do_compile(State#state.port, Source, Options1),
    {reply, Result, State};
handle_call({compile_method, ClassSource, MethodSource, Options}, _From, State) ->
    Options1 = Options#{
        class_hierarchy => State#state.classes,
        known_type_aliases => alias_source_list(State#state.aliases)
    },
    Result = do_compile_method(State#state.port, ClassSource, MethodSource, Options1),
    {reply, Result, State};
handle_call({resolve_completion_type, Expression}, _From, State) ->
    %% BT-1068: Forward class hierarchy so user-defined classes are visible.
    Result = beamtalk_compiler_port:resolve_completion_type(
        State#state.port, Expression, State#state.classes
    ),
    {reply, Result, State};
handle_call(
    {diagnostics, _Source, _Mode, _Options}, _From, #state{diagnostics_exit_fault = true} = State
) ->
    %% BT-2806 (test-only fault injection, see inject_diagnostics_exit/0):
    %% stop without replying — the caller's gen_server:call raises an exit
    %% (`{shutdown, {gen_server, call, [...]}}`) instead of getting a normal
    %% return, simulating the compiler port's call itself exiting. `shutdown`
    %% (not a crash reason) so this produces no crash report noise; the
    %% `permanent` supervisor restarts it regardless of exit reason.
    {stop, shutdown, State};
handle_call(
    {diagnostics, _Source, _Mode, _Options}, _From, #state{diagnostics_fault = Fault} = State
) when
    Fault =/= undefined
->
    %% BT-2832 (test-only fault injection, see inject_diagnostics_failure/1):
    %% consumed by exactly this one request — the real port is never touched,
    %% and the flag clears itself so every following diagnostics call (this
    %% one included, on retry) reaches the real port again.
    {reply, {error, [#{message => Fault}]}, State#state{diagnostics_fault = undefined}};
handle_call({diagnostics, Source, Mode, Options}, _From, State) ->
    %% ADR 0105 Phase 1 (BT-2778): only thread the ambient class cache when
    %% explicitly requested (see diagnostics/3's moduledoc) — the default
    %% stays class-context-free so the keystroke-driven cockpit editor path
    %% (BT-2556) is unaffected by this option's addition.
    %%
    %% ADR 0108 hot-reload re-check trigger (BT-2899): the ambient alias
    %% cache rides the same opt-in — a caller asking for ambient class
    %% context (a re-check round trip) needs ambient alias context for
    %% exactly the same reason, so this reuses `class_hierarchy => true`
    %% rather than adding a second, overlapping flag.
    {Classes, Aliases} =
        case maps:get(class_hierarchy, Options, false) of
            true -> {State#state.classes, alias_source_list(State#state.aliases)};
            false -> {#{}, []}
        end,
    Result = do_diagnostics(State#state.port, Source, Mode, Classes, Aliases),
    {reply, Result, State};
handle_call({find_senders_in_source, Source, Selector}, _From, State) ->
    Result = beamtalk_compiler_port:find_senders_in_source(
        State#state.port, Source, Selector
    ),
    {reply, Result, State};
handle_call({find_all_sends_in_source, Source}, _From, State) ->
    Result = beamtalk_compiler_port:find_all_sends_in_source(
        State#state.port, Source
    ),
    {reply, Result, State};
handle_call({find_references_to_in_source, Source, ClassName}, _From, State) ->
    Result = beamtalk_compiler_port:find_references_to_in_source(
        State#state.port, Source, ClassName
    ),
    {reply, Result, State};
handle_call({find_field_readers_in_source, Source, Field}, _From, State) ->
    Result = beamtalk_compiler_port:find_field_readers_in_source(
        State#state.port, Source, Field
    ),
    {reply, Result, State};
handle_call({find_field_writers_in_source, Source, Field}, _From, State) ->
    Result = beamtalk_compiler_port:find_field_writers_in_source(
        State#state.port, Source, Field
    ),
    {reply, Result, State};
handle_call({find_ffi_sites_in_source, Source, Module, Function, Arity}, _From, State) ->
    Result = beamtalk_compiler_port:find_ffi_sites_in_source(
        State#state.port, Source, Module, Function, Arity
    ),
    {reply, Result, State};
handle_call({find_announce_sites_in_source, Source}, _From, State) ->
    Result = beamtalk_compiler_port:find_announce_sites_in_source(
        State#state.port, Source
    ),
    {reply, Result, State};
handle_call({resolve_method_span, Source, ClassName, Selector, Side}, _From, State) ->
    Result = beamtalk_compiler_port:resolve_method_span(
        State#state.port, Source, ClassName, Selector, Side
    ),
    {reply, Result, State};
handle_call({reindent_method_source, Source, BaseIndent}, _From, State) ->
    Result = beamtalk_compiler_port:reindent_method_source(
        State#state.port, Source, BaseIndent
    ),
    {reply, Result, State};
handle_call(version, _From, State) ->
    Result = do_version(State#state.port),
    {reply, Result, State};
handle_call(clear_classes, _From, State) ->
    {reply, ok, State#state{classes = #{}, aliases = #{}}};
handle_call({inject_diagnostics_failure, Reason}, _From, State) ->
    {reply, ok, State#state{diagnostics_fault = Reason}};
handle_call(inject_diagnostics_exit, _From, State) ->
    {reply, ok, State#state{diagnostics_exit_fault = true}};
handle_call(get_classes, _From, State) ->
    {reply, State#state.classes, State};
handle_call(get_aliases, _From, State) ->
    {reply, alias_source_list(State#state.aliases), State};
handle_call({register_aliases, AliasSources}, _From, State) ->
    %% ADR 0108 hot-reload re-check trigger (BT-2899): merge by alias name
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
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_class, ClassName, MetaMap}, State) ->
    %% ADR 0050 Phase 3: Accumulate class metadata; overwrite on redefinition.
    NewClasses = maps:put(ClassName, MetaMap, State#state.classes),
    {noreply, State#state{classes = NewClasses}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    ?LOG_ERROR("Compiler port exited unexpectedly", #{
        domain => [beamtalk, runtime], status => Status
    }),
    {stop, {port_exit_status, Status}, State};
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
    AllModules = code:all_loaded(),
    lists:foldl(
        fun({Module, _Path}, Acc) ->
            case erlang:function_exported(Module, '__beamtalk_meta', 0) of
                false ->
                    Acc;
                true ->
                    try Module:'__beamtalk_meta'() of
                        Meta when is_map(Meta) ->
                            ClassName = maps:get(class, Meta, undefined),
                            case
                                ClassName =/= undefined andalso
                                    is_atom(ClassName) andalso
                                    not sets:is_element(ClassName, BuiltinSet)
                            of
                                true -> Acc#{ClassName => Meta};
                                false -> Acc
                            end;
                        _ ->
                            Acc
                    catch
                        _:_ ->
                            Acc
                    end
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

%% Send a request via the port and receive the response.
%%
%% Returns:
%%   {ok, Response}         — decoded ETF response from the port
%%   {exit_status, Status}  — port exited before responding
%%   timeout                — port did not respond within Timeout ms
%%   port_not_available     — port is closed (badarg from port_command)
%%   decode_error           — response could not be decoded (unexpected atoms)
-spec send_port_request(port(), map(), timeout()) ->
    {ok, term()} | {exit_status, non_neg_integer()} | timeout | port_not_available | decode_error.
send_port_request(Port, Request, Timeout) ->
    Command = maps:get(command, Request, unknown),
    T0 = erlang:monotonic_time(millisecond),
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    Elapsed = erlang:monotonic_time(millisecond) - T0,
                    try binary_to_term(ResponseBin, [safe]) of
                        %% [safe] prevents atom exhaustion: all response atoms are
                        %% literals in this module and guaranteed to exist.
                        Response ->
                            {ok, Response}
                    catch
                        error:badarg:Stack ->
                            ?LOG_ERROR("Compiler port decode error", #{
                                domain => [beamtalk, runtime],
                                port => Port,
                                elapsed_ms => Elapsed,
                                stacktrace => Stack
                            }),
                            decode_error
                    end;
                {Port, {exit_status, ExitStatus}} ->
                    Elapsed = erlang:monotonic_time(millisecond) - T0,
                    ?LOG_DEBUG("Port exit during request", #{
                        domain => [beamtalk, runtime],
                        command => Command,
                        exit_status => ExitStatus,
                        elapsed_ms => Elapsed
                    }),
                    {exit_status, ExitStatus}
            after Timeout ->
                %% Close the port so any late response cannot poison the next request.
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                timeout
            end
    catch
        error:badarg ->
            port_not_available
    end.

%% Send a compile (file) request via the port.
do_compile(Port, Source, Options) ->
    StdlibMode = maps:get(stdlib_mode, Options, false),
    WorkspaceMode = maps:get(workspace_mode, Options, true),
    %% BT-775: Optional module_name override for package-qualified naming
    ModuleName = maps:get(module_name, Options, undefined),
    %% BT-845/BT-860: Optional source file path for beamtalk_source attribute
    SourcePath = maps:get(source_path, Options, undefined),
    %% BT-905: Optional class superclass index for cross-file value-object inheritance
    ClassSuperclassIndex = maps:get(class_superclass_index, Options, #{}),
    %% Optional class module index for correct cross-directory module names
    ClassModuleIndex = maps:get(class_module_index, Options, #{}),
    Request0 = #{
        command => compile,
        source => Source,
        stdlib_mode => StdlibMode,
        workspace_mode => WorkspaceMode
    },
    Request1 =
        case ModuleName of
            undefined -> Request0;
            _ -> Request0#{module_name => ModuleName}
        end,
    Request2 =
        case SourcePath of
            undefined -> Request1;
            _ -> Request1#{source_path => SourcePath}
        end,
    Request3 =
        case map_size(ClassSuperclassIndex) of
            0 -> Request2;
            _ -> Request2#{class_superclass_index => ClassSuperclassIndex}
        end,
    Request4 =
        case map_size(ClassModuleIndex) of
            0 -> Request3;
            _ -> Request3#{class_module_index => ClassModuleIndex}
        end,
    %% ADR 0050 Phase 4: Inject accumulated class metadata into the port request.
    Classes = maps:get(class_hierarchy, Options, #{}),
    Request5 =
        case map_size(Classes) of
            0 -> Request4;
            _ -> Request4#{class_hierarchy => Classes}
        end,
    %% ADR 0108 hot-reload re-check trigger (BT-2899): likewise inject the
    %% ambient session alias cache — see handle_call({compile, ...})'s doc
    %% for why this is unconditional (not opt-in like diagnostics/3's).
    Aliases = maps:get(known_type_aliases, Options, []),
    RequestFinal =
        case Aliases of
            [] -> Request5;
            _ -> Request5#{known_type_aliases => Aliases}
        end,
    case send_port_request(Port, RequestFinal, 30000) of
        {ok, Response} ->
            handle_compile_response(Response);
        {exit_status, Status} ->
            ?LOG_ERROR("Compiler port exited during compile", #{
                domain => [beamtalk, runtime], status => Status
            }),
            {error, [#{message => <<"Compiler port exited unexpectedly">>}]};
        timeout ->
            {error, [#{message => <<"Compiler port timed out">>}]};
        port_not_available ->
            {error, [#{message => <<"Compiler port is not available">>}]};
        decode_error ->
            {error, [#{message => <<"Compiler port response is malformed">>}]}
    end.

%% Send a structured single-method compile request via the port. Mirrors
%% do_compile/3 but carries class_source + method_source + the method side, and
%% the response also yields the recovered selector + canonical method source.
do_compile_method(Port, ClassSource, MethodSource, Options) ->
    StdlibMode = maps:get(stdlib_mode, Options, false),
    WorkspaceMode = maps:get(workspace_mode, Options, true),
    IsClassMethod = maps:get(is_class_method, Options, false),
    ClassName = maps:get(class_name, Options, undefined),
    ModuleName = maps:get(module_name, Options, undefined),
    SourcePath = maps:get(source_path, Options, undefined),
    ClassSuperclassIndex = maps:get(class_superclass_index, Options, #{}),
    ClassModuleIndex = maps:get(class_module_index, Options, #{}),
    Request0a = #{
        command => compile_method,
        class_source => ClassSource,
        method_source => MethodSource,
        is_class_method => IsClassMethod,
        stdlib_mode => StdlibMode,
        workspace_mode => WorkspaceMode
    },
    Request0 =
        case ClassName of
            undefined -> Request0a;
            _ -> Request0a#{class_name => ClassName}
        end,
    Request1 =
        case ModuleName of
            undefined -> Request0;
            _ -> Request0#{module_name => ModuleName}
        end,
    Request2 =
        case SourcePath of
            undefined -> Request1;
            _ -> Request1#{source_path => SourcePath}
        end,
    Request3 =
        case map_size(ClassSuperclassIndex) of
            0 -> Request2;
            _ -> Request2#{class_superclass_index => ClassSuperclassIndex}
        end,
    Request4 =
        case map_size(ClassModuleIndex) of
            0 -> Request3;
            _ -> Request3#{class_module_index => ClassModuleIndex}
        end,
    Classes = maps:get(class_hierarchy, Options, #{}),
    Request5 =
        case map_size(Classes) of
            0 -> Request4;
            _ -> Request4#{class_hierarchy => Classes}
        end,
    %% ADR 0108 hot-reload re-check trigger (BT-2899): see do_compile/3's
    %% identical addition.
    Aliases = maps:get(known_type_aliases, Options, []),
    RequestFinal =
        case Aliases of
            [] -> Request5;
            _ -> Request5#{known_type_aliases => Aliases}
        end,
    case send_port_request(Port, RequestFinal, 30000) of
        {ok, Response} ->
            handle_compile_method_response(Response);
        {exit_status, Status} ->
            ?LOG_ERROR("Compiler port exited during compile_method", #{
                domain => [beamtalk, runtime], status => Status
            }),
            {error, [#{message => <<"Compiler port exited unexpectedly">>}]};
        timeout ->
            {error, [#{message => <<"Compiler port timed out">>}]};
        port_not_available ->
            {error, [#{message => <<"Compiler port is not available">>}]};
        decode_error ->
            {error, [#{message => <<"Compiler port response is malformed">>}]}
    end.

%% Send a diagnostics request via the port. `Mode' selects the parse grammar
%% (BT-2569): `<<"expression">>' (default, top-level script) or `<<"method">>'
%% (bare method body — the System Browser method editor). `Classes' is the
%% ambient class cache (ADR 0105 Phase 1, BT-2778) — ignored by the port in
%% `"method"' mode, which stays class-context-free by design. `Aliases' is
%% the ambient session type-alias cache (ADR 0108 hot-reload re-check
%% trigger, BT-2899), same opt-in and same `"method"'-mode exclusion.
do_diagnostics(Port, Source, Mode, Classes, Aliases) ->
    Request = #{
        command => diagnostics,
        source => Source,
        mode => Mode,
        class_hierarchy => Classes,
        known_type_aliases => Aliases
    },
    case send_port_request(Port, Request, 30000) of
        {ok, Response} ->
            handle_diagnostics_response(Response);
        {exit_status, Status} ->
            ?LOG_ERROR("Compiler port exited during diagnostics", #{
                domain => [beamtalk, runtime], status => Status
            }),
            {error, [#{message => <<"Compiler port exited unexpectedly">>}]};
        timeout ->
            {error, [#{message => <<"Compiler port timed out">>}]};
        port_not_available ->
            {error, [#{message => <<"Compiler port is not available">>}]};
        decode_error ->
            {error, [#{message => <<"Compiler port response is malformed">>}]}
    end.

%% Send a version request via the port.
do_version(Port) ->
    Request = #{command => version},
    case send_port_request(Port, Request, 5000) of
        {ok, Response} ->
            handle_version_response(Response);
        {exit_status, _} ->
            {error, port_exited};
        timeout ->
            {error, timeout};
        port_not_available ->
            {error, port_not_available};
        decode_error ->
            {error, decode_error}
    end.

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
    %% ADR 0108 hot-reload re-check trigger (BT-2899): the alias names this
    %% compile's annotations transitively referenced — `[]` when omitted
    %% (an older compiler-port binary predating BT-2899). Forwarded so
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
%% BT-1950: Protocol definitions use a different response shape (kind := protocol_definition)
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
    %% ADR 0108 hot-reload re-check trigger (BT-2899 follow-up, BT-2917):
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
    ?LOG_ERROR("Unexpected compile response", #{domain => [beamtalk, runtime], response => Other}),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

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
    %% ADR 0105 Phase 1 (BT-2777): return_type/param_types carry the patched
    %% method's declared signature so the workspace can capture it into the
    %% signature-generation store before install. Defaulted so an older
    %% compiler-port binary (pre-BT-2777) that omits these keys still decodes.
    ReturnType = maps:get(return_type, Response, <<"Dynamic">>),
    ParamTypes = maps:get(param_types, Response, []),
    %% ADR 0108 hot-reload re-check trigger (BT-2899): see
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
    ?LOG_ERROR("Unexpected compile response", #{domain => [beamtalk, runtime], response => Other}),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

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
    ?LOG_ERROR("Unexpected diagnostics response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

%% Handle response from version command.
handle_version_response(#{status := ok, version := Version}) ->
    {ok, Version};
handle_version_response(Other) ->
    ?LOG_ERROR("Unexpected version response", #{domain => [beamtalk, runtime], response => Other}),
    {error, unexpected_response}.
