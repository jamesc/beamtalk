%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_shell).
-behaviour(gen_server).

%%% **DDD Context:** REPL Session Context

-moduledoc """
REPL session shell for workspace

Maintains per-session state including:
- Variable bindings (counter := Counter spawn)
- Session ID
- Evaluation state and counter

Each REPL connection gets its own session process, allowing
multiple users to work in the same workspace with independent
bindings while sharing access to actors and loaded modules.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Public API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    eval/2,
    eval_async/3,
    dispatch_async/5,
    eval_trace/2,
    interrupt/1,
    get_bindings/1,
    get_session_id/1,
    get_session_id/2,
    get_session_meta/1,
    get_session_meta/2,
    clear_bindings/1,
    enqueue_mutation/2,
    load_file/2,
    load_file/3,
    load_source/2,
    unload_module/2,
    remove_from_tracker/2,
    get_module_tracker/1,
    show_codegen/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%% Public API

-doc "Start a REPL shell session (origin metadata defaults to `kind => unknown`).".
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    start_link(SessionId, #{}).

-doc """
Start a REPL shell session carrying session origin/debug metadata.

`Meta` is the map recorded in the REPL state's `client_meta` (always normalised
to include a `kind` key) and surfaced by `Workspace sessions` / `Session info`.
""".
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Meta) when is_map(Meta) ->
    gen_server:start_link(?MODULE, {SessionId, Meta}, []).

-doc "Stop a REPL shell session.".
-spec stop(pid()) -> ok.
stop(SessionPid) ->
    gen_server:stop(SessionPid, normal, 5000).

-doc """
Evaluate an expression in this session.

The `{script_exit, Code, Output, Warnings}` reply (BT-2688) signals a
connected-session `Program exit: Code`: the shell sends it, then terminates this
session (the caller surfaces the status to the connecting client). The shared
node is unaffected.
""".
-spec eval(pid(), string()) ->
    {ok, term(), binary(), [binary()]}
    | {error, term(), binary(), [binary()]}
    | {script_exit, non_neg_integer(), binary(), [binary()]}.
eval(SessionPid, Expression) ->
    gen_server:call(SessionPid, {eval, Expression}, 30000).

-doc """
Evaluate an expression in trace mode (BT-1238).
Returns `{ok, Steps, Output, Warnings}' or `{error, Reason, Output, Warnings}'.
""".
-spec eval_trace(pid(), string()) ->
    {ok, [{binary(), term()}], binary(), [binary()]}
    | {error, term(), binary(), [binary()]}
    | {script_exit, non_neg_integer(), binary(), [binary()]}.
eval_trace(SessionPid, Expression) ->
    gen_server:call(SessionPid, {eval_trace, Expression}, 30000).

-doc """
Evaluate an expression with streaming subscriber (BT-696).

Subscriber receives `{eval_out, Chunk}` messages during eval, then one terminal
message: `{eval_done, Value, Output, Warnings}`, `{eval_error, Reason, Output,
Warnings}`, or — when the expression called `Program exit:` in this connected
session — `{eval_script_exit, Code, Output, Warnings}` (BT-2688), after which the
session shell stops.
""".
-spec eval_async(pid(), string(), pid()) -> ok.
eval_async(SessionPid, Expression, Subscriber) ->
    gen_server:cast(SessionPid, {eval_async, Expression, Subscriber}).

-doc """
Dispatch a class entry method with streaming output (BT-2691, ADR 0099 §3).

The connected-mode `beamtalk run ClassName selector [args] --connect` path: rather
than compiling an expression, run `ClassName selector` against the live image with
`Argv` (the program arguments as a `List(String)`) on a session eval worker.
`SelectorBin` keeps its trailing `:` for the arity-1 keyword (`main:`) form.

Streaming and termination mirror `eval_async/3` exactly — the `Subscriber`
receives `{eval_out, Chunk}` chunks, then one terminal `{eval_done, …}`,
`{eval_error, …}`, or `{eval_script_exit, Code, …}` message — so a connected
`Program exit: N` ends this session while the shared node stays up, and the
connecting client adopts `N` as its process exit status.
""".
-spec dispatch_async(pid(), binary(), binary(), [binary()], pid()) -> ok.
dispatch_async(SessionPid, ClassNameBin, SelectorBin, Argv, Subscriber) ->
    gen_server:cast(SessionPid, {dispatch_async, ClassNameBin, SelectorBin, Argv, Subscriber}).

-doc """
Compile expression and return Core Erlang source (BT-700).
Does NOT evaluate the code.
""".
-spec show_codegen(pid(), string()) -> {ok, binary(), [binary()]} | {error, term(), [binary()]}.
show_codegen(SessionPid, Expression) ->
    gen_server:call(SessionPid, {show_codegen, Expression}, 30000).

-doc """
Interrupt a running evaluation in this session.
If no evaluation is in progress, returns ok immediately.
""".
-spec interrupt(pid()) -> ok.
interrupt(SessionPid) ->
    gen_server:call(SessionPid, interrupt, 5000).

-doc "Get current variable bindings for this session.".
-spec get_bindings(pid()) -> {ok, map()}.
get_bindings(SessionPid) ->
    gen_server:call(SessionPid, get_bindings).

-doc """
Get the protocol session id this shell was started with (BT-2368, ADR 0081
Phase 7).

Used by `beamtalk_session_primitives:liveSessions/0` to mint a `Session` value
per live shell with the same id `withId/1` would resolve.  Like `get_bindings/1`
it is answered in any worker state, so enumeration does not block on a
mid-eval shell.

`get_session_id/2` takes an explicit `Timeout` so a callers enumerating many
shells can bound the wait per shell (a `gen_server:call` timeout raises `exit`,
which the enumeration catches to skip an unresponsive shell rather than stalling
the whole list on the default 5s).
""".
-spec get_session_id(pid()) -> {ok, binary()}.
get_session_id(SessionPid) ->
    gen_server:call(SessionPid, get_session_id).

-spec get_session_id(pid(), timeout()) -> {ok, binary()}.
get_session_id(SessionPid, Timeout) ->
    gen_server:call(SessionPid, get_session_id, Timeout).

-doc """
Get this shell's `{id, meta}` in one call (id plus the origin/debug metadata
map, which always includes `kind`).

Used by `beamtalk_session_primitives` when minting `Session` values so the kind
and other debug fields travel with the id without a second round-trip. Like
`get_session_id/1` it is answered in any worker state. `get_session_meta/2`
takes an explicit `Timeout` so enumeration can bound the per-shell wait.

The `{error, unknown_request}` arm covers hot-code overlap: an older shell whose
code predates this call falls through to the catch-all `handle_call/3` clause,
so callers must degrade gracefully rather than assume `{ok, _, _}`.
""".
-spec get_session_meta(pid()) -> {ok, binary(), map()} | {error, term()}.
get_session_meta(SessionPid) ->
    gen_server:call(SessionPid, get_session_meta).

-spec get_session_meta(pid(), timeout()) -> {ok, binary(), map()} | {error, term()}.
get_session_meta(SessionPid, Timeout) ->
    gen_server:call(SessionPid, get_session_meta, Timeout).

-doc "Clear all variable bindings for this session.".
-spec clear_bindings(pid()) -> ok.
clear_bindings(SessionPid) ->
    gen_server:call(SessionPid, clear_bindings).

-doc """
Enqueue a deferred session-local mutation (BT-2366, ADR 0081 Phase 2).

Called by `beamtalk_session_primitives` from inside this session's eval worker.
The `{op, Key, Value}` tuple is appended to `pending_mutations`; it is applied
(or dropped) by the eval-exit clauses when the worker completes.  Safe from the
worker because the shell is in `noreply` while the worker runs.
""".
-spec enqueue_mutation(pid(), beamtalk_repl_state:mutation()) -> ok.
enqueue_mutation(SessionPid, Mutation) ->
    gen_server:call(SessionPid, {enqueue_mutation, Mutation}).

-doc "Load a Beamtalk source file in this session.".
-spec load_file(pid(), string()) -> {ok, [map()]} | {error, term()}.
load_file(SessionPid, Path) ->
    gen_server:call(SessionPid, {load_file, Path}, 30000).

-doc "Load a Beamtalk source file with pre-built class indexes (BT-1543).".
-spec load_file(pid(), string(), map()) -> {ok, [map()]} | {error, term()}.
load_file(SessionPid, Path, PrebuiltIndexes) ->
    gen_server:call(SessionPid, {load_file, Path, PrebuiltIndexes}, 30000).

-doc "Load Beamtalk source from an inline binary string.".
-spec load_source(pid(), binary()) -> {ok, [map()]} | {error, term()}.
load_source(SessionPid, Source) ->
    gen_server:call(SessionPid, {load_source, Source}, 30000).

-doc "Get the module tracker for this session (user-loaded modules only).".
-spec get_module_tracker(pid()) -> {ok, beamtalk_repl_modules:module_tracker()}.
get_module_tracker(SessionPid) ->
    gen_server:call(SessionPid, get_module_tracker, 5000).

-doc """
Unload a module from this session, purging its code and removing it from the tracker.
""".
-spec unload_module(pid(), atom()) -> ok | {error, #beamtalk_error{}}.
unload_module(SessionPid, Module) ->
    gen_server:call(SessionPid, {unload_module, Module}, 5000).

-doc """
Remove a module from the session tracker only (no BEAM purge).

BT-1239: Used when class removal has already purged the BEAM module
(e.g., via removeFromSystem), so we only need to clean up the tracker.
""".
-spec remove_from_tracker(pid(), atom()) -> ok.
remove_from_tracker(SessionPid, Module) ->
    gen_server:call(SessionPid, {remove_from_tracker, Module}, 5000).

%%% gen_server callbacks

init(SessionId) when is_binary(SessionId) ->
    %% Backward-compatible init for the bare-id arg shape: an in-flight start
    %% across a hot-code reload (old `start_link/1` MFA reaching new code) lands
    %% here. Mirrors the 2-tuple compat clause in `seed_session_context_from/1`
    %% — default to empty origin metadata (→ `kind => unknown`).
    init({SessionId, #{}});
init({SessionId, Meta}) when is_map(Meta) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    %% Create session-specific REPL state
    %% We use undefined for listen_socket and port since session doesn't own TCP connection
    %%
    %% BT-2365 (ADR 0081 Phase 1): the session binding map starts EMPTY — it holds
    %% only session locals. Workspace globals (singletons + bind:as: names) are no
    %% longer eagerly injected here; a free identifier that misses the locals map is
    %% resolved lazily at eval time via beamtalk_workspace:resolve_name/2.
    State0c = beamtalk_repl_state:new(undefined, 0, #{client_meta => Meta}),

    %% Get workspace-wide actor registry
    %% The registry is registered globally in the workspace
    RegistryPid =
        case whereis(beamtalk_actor_registry) of
            undefined ->
                ?LOG_WARNING("Actor registry not found for session ~p", [SessionId], #{
                    domain => [beamtalk, runtime]
                }),
                undefined;
            Pid ->
                Pid
        end,
    State1 = beamtalk_repl_state:set_actor_registry(RegistryPid, State0c),

    %% BT-1242: Join class-removed notification group so this session is notified
    %% when a class is removed via Beamtalk code (ClassName removeFromSystem).
    %% ensure_pg_started/0 mirrors beamtalk_object_class.erl — guarantees pg is
    %% running before join so failures are real errors, not startup-race artefacts.
    beamtalk_class_registry:ensure_pg_started(),
    pg:join(beamtalk_repl_shells, self()),

    {ok, {SessionId, State1, undefined}}.

handle_call({eval, Expression}, From, {SessionId, State, undefined}) ->
    %% Spawn eval in a monitored worker process so it can be interrupted (BT-666)
    Self = self(),
    SessionMeta = beamtalk_repl_state:get_client_meta(State),
    {WorkerPid, MonRef} = spawn_monitor(fun() ->
        %% BT-2365 (ADR 0081): seed the worker process context so primitives that
        %% read it (Session current / Workspace currentSession) see this session.
        seed_session_context(Self, SessionId, SessionMeta),
        Result = beamtalk_repl_eval:do_eval(Expression, State),
        Self ! {eval_result, self(), Result}
    end),
    {noreply, {SessionId, State, {WorkerPid, MonRef, From}}};
handle_call({eval_trace, Expression}, From, {SessionId, State, undefined}) ->
    Self = self(),
    SessionMeta = beamtalk_repl_state:get_client_meta(State),
    {WorkerPid, MonRef} = spawn_monitor(fun() ->
        %% BT-2365 (ADR 0081): seed worker process context (same as the eval path).
        seed_session_context(Self, SessionId, SessionMeta),
        Result = beamtalk_repl_eval:do_eval_trace(Expression, State),
        Self ! {eval_result, self(), Result}
    end),
    {noreply, {SessionId, State, {WorkerPid, MonRef, From}}};
handle_call({eval_trace, _Expression}, _From, {_SessionId, _State, {_Pid, _Ref, _}} = FullState) ->
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    {reply, {error, Err2, <<>>, []}, FullState};
handle_call({eval, _Expression}, _From, {_SessionId, _State, {_Pid, _Ref, _}} = FullState) ->
    %% Already evaluating — reject concurrent eval
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    {reply, {error, Err2, <<>>, []}, FullState};
handle_call(interrupt, _From, {SessionId, State, {WorkerPid, MonRef, EvalFrom}}) ->
    %% Kill the worker process and reply to the waiting eval caller
    erlang:demonitor(MonRef, [flush]),
    exit(WorkerPid, kill),
    %% Flush any eval_result message the worker may have sent before dying
    receive
        {eval_result, WorkerPid, _} -> ok
    after 0 -> ok
    end,
    Err0 = beamtalk_error:new(interrupted, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"Interrupted">>),
    reply_eval(EvalFrom, {eval_error, Err1, <<>>, []}),
    %% BT-1242: Apply any pending module removals that accumulated while the
    %% worker was running.  No WorkerState is returned on interrupt, so we
    %% drain directly from ShellState.
    CleanState0 = drain_pending_removals(State),
    %% BT-2366 (ADR 0081 Phase 2): drain session-local mutations too — an
    %% interrupted eval's issued put/remove/clear edits are not lost.
    CleanState = drain_pending_mutations(CleanState0, SessionId),
    {reply, ok, {SessionId, CleanState, undefined}};
handle_call(interrupt, _From, {_SessionId, _State, undefined} = FullState) ->
    %% No eval in progress — nothing to interrupt
    {reply, ok, FullState};
handle_call({enqueue_mutation, Mutation}, _From, {SessionId, State, Worker}) ->
    %% BT-2366 (ADR 0081 Phase 2): a session-scope write issued by a primitive
    %% running inside this session's eval worker.  Enqueue the {op, Key, Value}
    %% tuple on pending_mutations rather than writing the bindings directly: the
    %% worker holds a state snapshot whose returned WorkerState would clobber a
    %% direct edit when {eval_result, ...} arrives.  This call is synchronous and
    %% completes before the worker sends {eval_result, ...}, so the ShellState
    %% bound in the eval-result handler always reflects the enqueued mutation.
    NewState = beamtalk_repl_state:add_pending_mutation(Mutation, State),
    {reply, ok, {SessionId, NewState, Worker}};
handle_call(get_bindings, _From, {SessionId, State, Worker}) ->
    Bindings = beamtalk_repl_state:get_bindings(State),
    {reply, {ok, Bindings}, {SessionId, State, Worker}};
handle_call(get_session_id, _From, {SessionId, State, Worker}) ->
    %% BT-2368 (ADR 0081 Phase 7): answered in any worker state so
    %% liveSessions/0 enumeration never blocks on a mid-eval shell.
    {reply, {ok, SessionId}, {SessionId, State, Worker}};
handle_call(get_session_meta, _From, {SessionId, State, Worker}) ->
    %% id + origin/debug metadata in one call, for Session-value minting.
    %% Answered in any worker state (same non-blocking discipline as
    %% get_session_id) so enumeration never stalls on a mid-eval shell.
    Meta = beamtalk_repl_state:get_client_meta(State),
    {reply, {ok, SessionId, Meta}, {SessionId, State, Worker}};
handle_call(clear_bindings, _From, {SessionId, State, Worker}) ->
    %% BT-2365 (ADR 0081 Phase 1): clear only the session locals. Workspace globals
    %% (singletons + bind:as: names) are no longer copied into the session map, so
    %% there is nothing to re-inject — they remain available via lazy resolution.
    ClearedKeys = maps:keys(beamtalk_repl_state:get_bindings(State)),
    NewState = beamtalk_repl_state:clear_bindings(State),
    %% BT-2531: the retired `beamtalk_bindings_events` channel pushed a refresh on
    %% clear; restore it as a typed, session-scoped `BindingChanged` per cleared
    %% local so the live bindings pane refreshes (each event is well-formed and the
    %% LiveView's session filter routes it to the right pane).
    announce_bindings_cleared(SessionId, ClearedKeys),
    {reply, ok, {SessionId, NewState, Worker}};
handle_call({load_file, Path}, _From, {SessionId, State, Worker}) ->
    case beamtalk_repl_eval:handle_load(Path, State) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState, Worker}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState, Worker}}
    end;
handle_call({load_file, Path, PrebuiltIndexes}, _From, {SessionId, State, Worker}) ->
    case beamtalk_repl_eval:handle_load(Path, State, PrebuiltIndexes) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState, Worker}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState, Worker}}
    end;
handle_call({load_source, Source}, _From, {SessionId, State, Worker}) ->
    case beamtalk_repl_eval:handle_load_source(Source, "<editor>", State) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState, Worker}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState, Worker}}
    end;
handle_call(get_module_tracker, _From, {SessionId, State, Worker}) ->
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    {reply, {ok, Tracker}, {SessionId, State, Worker}};
handle_call({unload_module, Module}, _From, {SessionId, State, Worker}) ->
    case code:is_loaded(Module) of
        {file, _} ->
            case code:soft_purge(Module) of
                true ->
                    _ = code:delete(Module),
                    Tracker = beamtalk_repl_state:get_module_tracker(State),
                    NewTracker = beamtalk_repl_modules:remove_module(Module, Tracker),
                    NewState = beamtalk_repl_state:set_module_tracker(NewTracker, State),
                    {reply, ok, {SessionId, NewState, Worker}};
                false ->
                    Err0 = beamtalk_error:new(module_in_use, 'Module'),
                    Err1 = beamtalk_error:with_selector(Err0, Module),
                    Err = beamtalk_error:with_hint(
                        Err1,
                        <<"Stop actors using this module first.">>
                    ),
                    {reply, {error, Err}, {SessionId, State, Worker}}
            end;
        false ->
            Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
            Err1 = beamtalk_error:with_selector(Err0, Module),
            Err = beamtalk_error:with_hint(
                Err1,
                <<"Use :load <path> to load it first.">>
            ),
            {reply, {error, Err}, {SessionId, State, Worker}}
    end;
handle_call(
    {remove_from_tracker, _Module}, _From, {_SessionId, _State, {_Pid, _Ref, _}} = FullState
) ->
    %% Reject while an eval worker is active: the worker's snapshot would
    %% overwrite any tracker edit we make here once eval_result arrives.
    {reply, ok, FullState};
handle_call({remove_from_tracker, Module}, _From, {SessionId, State, undefined}) ->
    %% BT-1239: Remove module from tracker only (BEAM purge already done by caller).
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    NewTracker = beamtalk_repl_modules:remove_module(Module, Tracker),
    NewState = beamtalk_repl_state:set_module_tracker(NewTracker, State),
    {reply, ok, {SessionId, NewState, undefined}};
handle_call({show_codegen, _Expression}, _From, {_SessionId, _State, {_Pid, _Ref, _}} = FullState) ->
    %% Reject if eval is in progress
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Wait for the current evaluation to complete.">>),
    {reply, {error, Err2, []}, FullState};
handle_call({show_codegen, Expression}, _From, {SessionId, State, undefined}) ->
    case beamtalk_repl_eval:do_show_codegen(Expression, State) of
        {ok, CoreErlang, Warnings, NewState} ->
            {reply, {ok, CoreErlang, Warnings}, {SessionId, NewState, undefined}};
        {error, Reason, Warnings, NewState} ->
            {reply, {error, Reason, Warnings}, {SessionId, NewState, undefined}}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-doc "BT-696: Async eval with streaming subscriber".
handle_cast({eval_async, Expression, Subscriber}, {SessionId, State, undefined}) ->
    Self = self(),
    SessionMeta = beamtalk_repl_state:get_client_meta(State),
    {WorkerPid, MonRef} = spawn_monitor(fun() ->
        %% BT-2365 (ADR 0081): seed worker process context on the streaming path too
        %% so Session current behaves identically to the synchronous eval path.
        seed_session_context(Self, SessionId, SessionMeta),
        Result = beamtalk_repl_eval:do_eval(Expression, State, Subscriber),
        Self ! {eval_result, self(), Result}
    end),
    {noreply, {SessionId, State, {WorkerPid, MonRef, {async, Subscriber}}}};
handle_cast(
    {eval_async, _Expression, Subscriber}, {_SessionId, _State, {_Pid, _Ref, _}} = FullState
) ->
    %% Already evaluating — reject concurrent eval
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    Subscriber ! {eval_error, Err2, <<>>, []},
    {noreply, FullState};
handle_cast(
    {dispatch_async, ClassNameBin, SelectorBin, Argv, Subscriber}, {SessionId, State, undefined}
) ->
    %% BT-2691: connected-mode `beamtalk run` entry dispatch. Spawns a worker
    %% exactly like {eval_async, …} but runs `do_dispatch/5` (class entry) instead
    %% of compiling an expression, returning the same `eval_result()` shape so the
    %% existing {eval_result, …} handling (including the connected `Program exit:`
    %% / {script_exit, …} branch) applies unchanged.
    Self = self(),
    SessionMeta = beamtalk_repl_state:get_client_meta(State),
    {WorkerPid, MonRef} = spawn_monitor(fun() ->
        seed_session_context(Self, SessionId, SessionMeta),
        Result = beamtalk_repl_eval:do_dispatch(
            ClassNameBin, SelectorBin, Argv, Subscriber, State
        ),
        Self ! {eval_result, self(), Result}
    end),
    {noreply, {SessionId, State, {WorkerPid, MonRef, {async, Subscriber}}}};
handle_cast(
    {dispatch_async, _ClassNameBin, _SelectorBin, _Argv, Subscriber},
    {_SessionId, _State, {_Pid, _Ref, _}} = FullState
) ->
    %% Already evaluating — reject concurrent dispatch (parity with eval_async)
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    Subscriber ! {eval_error, Err2, <<>>, []},
    {noreply, FullState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc """
Worker completed eval successfully (BT-666)

BT-1242: Use ShellState (gen_server state) instead of discarding it: it
carries pending_module_removals collected while the worker ran.  Apply them
to the worker's returned WorkerState so modules removed inside the expression
being executed are not reinstated when the worker snapshot is merged back.
""".
handle_info({eval_result, WorkerPid, Result}, {SessionId, ShellState, {WorkerPid, MonRef, From}}) ->
    erlang:demonitor(MonRef, [flush]),
    case Result of
        {ok, Value, Output, Warnings, WorkerState} ->
            reply_eval(From, {eval_done, Value, Output, Warnings}),
            %% BT-2365 (ADR 0081 Phase 1): no refresh_ws_bindings — workspace globals
            %% are resolved live, not injected into the session map, so there is no
            %% injected copy to reconcile after an eval. The session map holds only
            %% locals (the worker's returned WorkerState).
            %% BT-2366 (ADR 0081 Phase 2): apply removals first, then session-local
            %% mutations on top of the worker's returned locals (so the worker's
            %% own `x := …` is visible and a same-line `bindings at:put:` overrides
            %% it).  Success path applies all queued ops, including `clear`.
            FinalState0 = apply_pending_removals(ShellState, WorkerState),
            FinalState = apply_pending_mutations(ShellState, FinalState0, SessionId),
            {noreply, {SessionId, FinalState, undefined}};
        {error, Reason, Output, Warnings, WorkerState} ->
            reply_eval(From, {eval_error, Reason, Output, Warnings}),
            %% BT-2366 (ADR 0081 Phase 2): on the error path apply put/remove only
            %% (explicit per-key edits the user issued, independent of the failed
            %% expression) and DROP a queued `clear` — `Session current clear. typo`
            %% must not wipe every local because the line after `clear` failed.
            MergedState0 = apply_pending_removals(ShellState, WorkerState),
            MergedState = apply_pending_mutations_no_clear(ShellState, MergedState0, SessionId),
            {noreply, {SessionId, MergedState, undefined}};
        {script_exit, Code, Output, Warnings, _WorkerState} ->
            %% BT-2688 (ADR 0099 §3 / Phase 5): `Program exit: Code` in this
            %% connected session. Reply with the exit status, then stop this
            %% session's shell so the job ends while the shared node stays up. The
            %% shell is a `temporary` child of `beamtalk_session_sup`, so it is not
            %% restarted. Pending session-local mutations are dropped — the session
            %% is ending, so there is nothing left to read them back.
            reply_eval(From, {eval_script_exit, Code, Output, Warnings}),
            {stop, normal, {SessionId, ShellState, undefined}}
    end;
%% Worker process crashed (BT-666)
handle_info(
    {'DOWN', MonRef, process, WorkerPid, Reason},
    {SessionId, State, {WorkerPid, MonRef, From}}
) ->
    Err0 = beamtalk_error:new(eval_crashed, 'REPL'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary(io_lib:format("Evaluation crashed: ~p", [Reason]))
    ),
    reply_eval(From, {eval_error, Err1, <<>>, []}),
    %% BT-1242: Apply any pending module removals that arrived while the
    %% worker was running.  No WorkerState is returned on crash.
    CleanState0 = drain_pending_removals(State),
    %% BT-2366 (ADR 0081 Phase 2): DISCARD session-local mutations — a crashed
    %% worker's partial mutations are not trustworthy.  Reset the queue to [].
    CleanState = beamtalk_repl_state:clear_pending_mutations(CleanState0),
    {noreply, {SessionId, CleanState, undefined}};
%% BT-1242: Class removed via Beamtalk code path — clean up session tracker.
%% Only updates tracker when no eval worker is active: if a worker is running it
%% holds a snapshot of State that would overwrite our edit on eval_result arrival.
%% The code:is_loaded filter in the modules op covers the stale-entry window.
handle_info({class_removed, _ClassName, Module}, {SessionId, State, undefined}) ->
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    NewTracker = beamtalk_repl_modules:remove_module(Module, Tracker),
    NewState = beamtalk_repl_state:set_module_tracker(NewTracker, State),
    {noreply, {SessionId, NewState, undefined}};
handle_info({class_removed, _ClassName, Module}, {SessionId, State, Worker}) ->
    %% Eval worker is active — defer removal: add to pending_module_removals in
    %% the gen_server State so eval_result can apply it to the worker snapshot.
    %% (If we edited the tracker here the worker's returned NewState would
    %% overwrite our change when eval_result is processed.)
    NewState = beamtalk_repl_state:add_pending_module_removal(Module, State),
    {noreply, {SessionId, NewState, Worker}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, {SessionId, _State, {WorkerPid, MonRef, _From}}) ->
    %% BT-666: Kill any running worker to avoid zombie evaluations
    erlang:demonitor(MonRef, [flush]),
    exit(WorkerPid, kill),
    ?LOG_INFO("REPL session terminated", #{
        session => SessionId, reason => Reason, domain => [beamtalk, runtime]
    }),
    ok;
terminate(Reason, {SessionId, _State, _Worker}) ->
    ?LOG_INFO("REPL session terminated", #{
        session => SessionId, reason => Reason, domain => [beamtalk, runtime]
    }),
    ok;
terminate(Reason, _State) ->
    ?LOG_INFO("REPL session terminated", #{reason => Reason, domain => [beamtalk, runtime]}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

-doc """
BT-2365 (ADR 0081): seed the eval-worker process dictionary with the session
context that session-aware primitives read.

`beamtalk_session_pid` is the shell's pid (the long-lived gen_server, not the
short-lived worker) so cross-session reads target the shell; `beamtalk_session_id`
is the protocol session id. A primitive that finds `beamtalk_session_pid =:=
undefined` (e.g. eval outside a shell-spawned worker) returns nil.

Seeded on BOTH spawn paths (synchronous eval/eval_trace and streaming
eval_async) so Session current behaves consistently regardless of eval mode.
""".
-spec seed_session_context(pid(), binary(), map()) -> ok.
seed_session_context(ShellPid, SessionId, Meta) ->
    put(beamtalk_session_pid, ShellPid),
    put(beamtalk_session_id, SessionId),
    %% Origin/debug metadata so `Session current` mints a fully-populated value
    %% (kind, peer, …) without a round-trip back to the shell.
    put(beamtalk_session_meta, Meta),
    ok.

-doc """
BT-1242: Apply pending module removals from ShellState to WorkerState.

ShellState is the gen_server state at eval_result time (may have accumulated
pending_module_removals while the worker ran).  WorkerState is the worker's
returned snapshot, based on the pre-expression state so it still has those
modules in its tracker.  We remove them here so the tracker stays consistent.

The pending_module_removals field in WorkerState is always [] (the worker
never sets it), so the returned state has a clean pending list.
""".
-spec apply_pending_removals(beamtalk_repl_state:state(), beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
apply_pending_removals(ShellState, WorkerState) ->
    case beamtalk_repl_state:get_pending_module_removals(ShellState) of
        [] ->
            WorkerState;
        Pending ->
            Tracker = beamtalk_repl_state:get_module_tracker(WorkerState),
            NewTracker = lists:foldl(
                fun beamtalk_repl_modules:remove_module/2,
                Tracker,
                Pending
            ),
            beamtalk_repl_state:set_module_tracker(NewTracker, WorkerState)
    end.

-doc """
BT-1242: Apply pending module removals directly to State and clear the list.

Used when the worker exits via interrupt or crash (no WorkerState is returned).
In those paths the shell resumes using its own ShellState, so we fold the
pending list over that state's tracker and reset the list to [].
""".
-spec drain_pending_removals(beamtalk_repl_state:state()) -> beamtalk_repl_state:state().
drain_pending_removals(State) ->
    case beamtalk_repl_state:get_pending_module_removals(State) of
        [] ->
            State;
        Pending ->
            Tracker = beamtalk_repl_state:get_module_tracker(State),
            NewTracker = lists:foldl(
                fun beamtalk_repl_modules:remove_module/2,
                Tracker,
                Pending
            ),
            State1 = beamtalk_repl_state:set_module_tracker(NewTracker, State),
            beamtalk_repl_state:clear_pending_module_removals(State1)
    end.

-doc """
BT-2366 (ADR 0081 Phase 2): apply queued session-local mutations on the success
exit path.

Reads the `pending_mutations` queue from ShellState (which accumulated the
primitive enqueues that completed before `{eval_result, …}`) and folds it over
TargetState's locals map in enqueue order, then resets the queue to `[]`.

`put`/`remove` edit a single key; `clear` empties the locals map (no
injected-key caveat now that the map is locals-only, BT-2365).  Mutations apply
on top of the worker's returned bindings already merged into TargetState, so a
same-line `bindings at:put:` overrides the worker's own assignment.
""".
-spec apply_pending_mutations(
    beamtalk_repl_state:state(), beamtalk_repl_state:state(), binary() | nil
) ->
    beamtalk_repl_state:state().
apply_pending_mutations(ShellState, TargetState, SessionId) ->
    Mutations = beamtalk_repl_state:get_pending_mutations(ShellState),
    fold_mutations(Mutations, TargetState, SessionId).

-doc """
BT-2366 (ADR 0081 Phase 2): apply queued session-local mutations on the error
exit path — `put`/`remove` only.

A queued `clear` (whole-session destruction) is dropped: `Session current
clear. someTypo` should not wipe every local because the line after `clear`
failed.  `clear` applies only on the success path.  `put`/`remove` are explicit
per-key edits the user issued, independent of the failed expression, so they
take effect.
""".
-spec apply_pending_mutations_no_clear(
    beamtalk_repl_state:state(), beamtalk_repl_state:state(), binary() | nil
) ->
    beamtalk_repl_state:state().
apply_pending_mutations_no_clear(ShellState, TargetState, SessionId) ->
    Mutations = beamtalk_repl_state:get_pending_mutations(ShellState),
    Kept = [M || M <- Mutations, element(1, M) =/= clear],
    fold_mutations(Kept, TargetState, SessionId).

-doc """
BT-2366 (ADR 0081 Phase 2): apply pending session-local mutations directly to
State and clear the queue.

Used when the worker exits via interrupt (no WorkerState is returned).  The
shell resumes using its own ShellState, so we fold the queue over that state's
locals and reset the queue to `[]`.  All ops apply (interrupt is not an error
path — the user's issued edits stand).
""".
-spec drain_pending_mutations(beamtalk_repl_state:state(), binary() | nil) ->
    beamtalk_repl_state:state().
drain_pending_mutations(State, SessionId) ->
    Mutations = beamtalk_repl_state:get_pending_mutations(State),
    fold_mutations(Mutations, State, SessionId).

-doc """
BT-2366 (ADR 0081 Phase 2): fold a list of `{op, Key, Value}` mutations over the
locals map of State in order, then clear the pending-mutations queue.

Returns State unchanged (apart from clearing the queue) when the list is empty.
""".
-spec fold_mutations([beamtalk_repl_state:mutation()], beamtalk_repl_state:state(), binary() | nil) ->
    beamtalk_repl_state:state().
fold_mutations([], State, _SessionId) ->
    %% Always reset the queue, even when empty, so the shell returns to idle with
    %% a clean slate regardless of which fold path ran.
    beamtalk_repl_state:clear_pending_mutations(State);
fold_mutations(Mutations, State, SessionId) ->
    Bindings0 = beamtalk_repl_state:get_bindings(State),
    Bindings1 = lists:foldl(
        fun(Mutation, Acc) -> apply_one_mutation(Mutation, Acc, SessionId) end,
        Bindings0,
        Mutations
    ),
    State1 = beamtalk_repl_state:set_bindings(Bindings1, State),
    beamtalk_repl_state:clear_pending_mutations(State1).

-doc """
BT-2366: apply a single {op, Key, Value} mutation to a locals map.

BT-2531: each binding mutation also emits a typed, session-scoped `BindingChanged`
so the `bindings` push stream refreshes on `bindings at:put:` / `removeKey:` /
`clear` — not just on `:=` assignment — restoring the coarse refresh the retired
`beamtalk_bindings_events` channel provided. `put` carries the new value; `remove`
and `clear` carry `nil`; `clear` announces one event per local being removed.
""".
-spec apply_one_mutation(beamtalk_repl_state:mutation(), map(), binary() | nil) -> map().
apply_one_mutation({put, Key, Value}, Bindings, SessionId) ->
    beamtalk_repl_eval:announce_binding_changed(Key, Value, SessionId),
    Bindings#{Key => Value};
apply_one_mutation({remove, Key, _}, Bindings, SessionId) ->
    beamtalk_repl_eval:announce_binding_changed(Key, nil, SessionId),
    maps:remove(Key, Bindings);
apply_one_mutation({clear, _, _}, Bindings, SessionId) ->
    announce_bindings_cleared(SessionId, maps:keys(Bindings)),
    #{}.

-doc """
BT-2531: announce a `BindingChanged` (value `nil`) for each cleared local so push
consumers refresh. One event per key keeps each announcement well-formed and lets
the LiveView's session filter route it; an empty session announces nothing.
""".
-spec announce_bindings_cleared(binary() | nil, [atom()]) -> ok.
announce_bindings_cleared(SessionId, Keys) ->
    lists:foreach(
        fun(Key) -> beamtalk_repl_eval:announce_binding_changed(Key, nil, SessionId) end,
        Keys
    ).

-doc "BT-696: Dispatch eval result to sync caller or async subscriber.".
reply_eval({async, Subscriber}, Msg) ->
    Subscriber ! Msg,
    ok;
reply_eval(From, {eval_done, Value, Output, Warnings}) ->
    gen_server:reply(From, {ok, Value, Output, Warnings});
reply_eval(From, {eval_error, Reason, Output, Warnings}) ->
    gen_server:reply(From, {error, Reason, Output, Warnings});
%% BT-2688: connected-session `Program exit:` — surface the status to the caller,
%% which encodes it for the connecting client before the session shell stops.
reply_eval(From, {eval_script_exit, Code, Output, Warnings}) ->
    gen_server:reply(From, {script_exit, Code, Output, Warnings}).
