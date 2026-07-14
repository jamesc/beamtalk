%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_signature_store).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Per-selector signature-generation store (ADR 0105 Phase 1, BT-2777).

Hot-patching clears a method's type metadata on install (`put_method/4`
deliberately wipes `method_signatures`/`method_return_types` per ADR 0050 —
`beamtalk_object_class.erl`; the IDE-save/`compile:source:` recompile path
overwrites it with the *new* generation, losing the *previous* one just as
surely). Either way, once a patch installs, the pre-patch signature is gone
from live class state. This store is the plumbing that survives the wipe: the
install hook (`beamtalk_repl_loader:load_recompiled_method/8`) calls
`capture/4` with the freshly-compiled signature *before* the patch installs,
so the diff always compares the new generation against the one actually
recorded at the previous patch — never against wiped/overwritten class state.

## Generation handling

`capture/4` is the whole contract: given the newly-compiled signature for
`{Class, Selector, Side}`, it looks up whatever was recorded at the *last*
patch (or seeds from the class's original, never-patched `__beamtalk_meta/0`
when this is the first patch this session), classifies the change via
`beamtalk_signature_diff:diff/2`, records the new signature as the entry for
the next patch, and returns `{PreviousSignature, Classification}`. Repeated
edits to the same method therefore chain correctly: generation N's "previous"
is always exactly what generation N-1 installed, not generation 0.

## Session-only, never persisted

State lives in this gen_server's process dictionary-free `#state{}` map —
plain in-memory, no ETS, no disk. It is supervised under
`beamtalk_workspace_sup` alongside `beamtalk_workspace_meta`, so a workspace
restart (a fresh BEAM node) starts a fresh, empty store — exactly the ADR's
"session state, never persisted to the artifact" requirement. `clear/0` gives
callers (e.g. `Workspace changes revert:`) an explicit reset without a full
restart.
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, capture/4, previous/3, rollback/4, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([seed_from_meta/3, meta_type_to_binary/1]).
-endif.

-export_type([side/0, signature/0, maybe_signature/0]).

-type side() :: instance | class.
-type signature() :: #{return_type := binary(), param_types := [binary()]}.
-type maybe_signature() :: signature() | removed | undefined.
-type key() :: {binary(), binary(), side()}.

-record(state, {sigs = #{} :: #{key() => signature() | removed}}).

%%====================================================================
%% API
%%====================================================================

-doc "Start the signature-generation store (one per workspace).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Capture the newly-compiled signature for `{ClassName, Selector, Side}`,
diffing it against whatever was recorded at the previous patch (or the
class's original `__beamtalk_meta/0` for a never-patched method).

`NewSignature` is either a `signature()` map (a patch installing a method) or
the atom `removed` (a method being deleted — `beamtalk_repl_loader:remove_method/3`).

**Must be called *before* the install** (before `code:load_binary/3` for a
patch, before the recompile-without-the-method for a removal) — never after.
The install reloads the class's compiled module under its *existing* atom, so
a call made after install would read the *new* code's `__beamtalk_meta/0` when
seeding a first-ever capture, silently comparing the new generation against
itself (`no_op` every time) instead of the true original. Calling before
install means a load/reload failure would otherwise leave the store holding a
generation that was never actually live — pair every `capture/4` call with a
`rollback/4` call on that failure path (see `rollback/4`).

Returns `{PreviousSignature, Classification}` — the caller (BT-2778's
re-check orchestration) uses `Classification` to decide whether a re-check is
warranted at all (`no_op` short-circuits it).
""".
-spec capture(binary(), binary(), side(), signature() | removed) ->
    {maybe_signature(), beamtalk_signature_diff:classification()}.
capture(ClassName, Selector, Side, NewSignature) when
    is_binary(ClassName), is_binary(Selector), (Side =:= instance orelse Side =:= class)
->
    gen_server:call(?MODULE, {capture, ClassName, Selector, Side, NewSignature}).

-doc """
Read-only lookup of the signature that `capture/4` would currently treat as
"previous" for `{ClassName, Selector, Side}` — from the store if this
selector has been patched this session, else seeded from `__beamtalk_meta/0`.
Does not mutate the store. Exposed for the re-check orchestration (BT-2778)
and for tests.
""".
-spec previous(binary(), binary(), side()) -> maybe_signature().
previous(ClassName, Selector, Side) when
    is_binary(ClassName), is_binary(Selector), (Side =:= instance orelse Side =:= class)
->
    gen_server:call(?MODULE, {previous, ClassName, Selector, Side}).

-doc """
Undo a `capture/4` call whose install/removal turned out to fail, restoring
`{ClassName, Selector, Side}` to `PreviousSignature` (the first element of the
`{PreviousSignature, Classification}` pair that `capture/4` returned).

`capture/4` must run *before* the install it describes (to seed correctly from
still-live pre-patch class state — see its doc), which means its write can't
simply be deferred until success is known. `rollback/4` is the other half:
call it when the install/removal that followed `capture/4` failed, so a failed
attempt never leaves the store holding a generation that was never actually
live. `PreviousSignature` of `undefined` removes the key entirely — the
correct state when the failed attempt was itself the first-ever capture for
this selector (nothing should be there at all).
""".
-spec rollback(binary(), binary(), side(), maybe_signature()) -> ok.
rollback(ClassName, Selector, Side, PreviousSignature) when
    is_binary(ClassName), is_binary(Selector), (Side =:= instance orelse Side =:= class)
->
    gen_server:call(?MODULE, {rollback, ClassName, Selector, Side, PreviousSignature}).

-doc """
Clear every recorded generation (`Workspace changes revert:`, tests). The next
`capture/4` for any selector re-seeds from `__beamtalk_meta/0` as if this were
a fresh workspace.
""".
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #state{}}.

handle_call({capture, ClassName, Selector, Side, NewSignature}, _From, State = #state{sigs = Sigs}) ->
    Key = {ClassName, Selector, Side},
    Prev =
        case maps:find(Key, Sigs) of
            {ok, V} -> V;
            error -> seed_from_meta(ClassName, Selector, Side)
        end,
    Classification = beamtalk_signature_diff:diff(Prev, NewSignature),
    NewSigs = Sigs#{Key => NewSignature},
    {reply, {Prev, Classification}, State#state{sigs = NewSigs}};
handle_call({previous, ClassName, Selector, Side}, _From, State = #state{sigs = Sigs}) ->
    Key = {ClassName, Selector, Side},
    Prev =
        case maps:find(Key, Sigs) of
            {ok, V} -> V;
            error -> seed_from_meta(ClassName, Selector, Side)
        end,
    {reply, Prev, State};
handle_call({rollback, ClassName, Selector, Side, undefined}, _From, State = #state{sigs = Sigs}) ->
    Key = {ClassName, Selector, Side},
    {reply, ok, State#state{sigs = maps:remove(Key, Sigs)}};
handle_call(
    {rollback, ClassName, Selector, Side, PreviousSignature}, _From, State = #state{sigs = Sigs}
) ->
    Key = {ClassName, Selector, Side},
    {reply, ok, State#state{sigs = Sigs#{Key => PreviousSignature}}};
handle_call(clear, _From, State) ->
    {reply, ok, State#state{sigs = #{}}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-doc """
Seed the "original" (never-patched) signature from the class's compiled
`__beamtalk_meta/0`, for the first `capture/4` this session against a
selector. Best-effort: any resolution failure (class not registered, no
`__beamtalk_meta/0` exported, selector not a known method) returns
`undefined` — "no baseline to compare against" — rather than raising, since a
brand-new method or a not-yet-loaded class are both ordinary, not errors. The
ordinary/expected failure modes (`badarg`, `{badmatch, not_found}`, `undef`)
degrade silently; anything else is logged at `?LOG_WARNING` before degrading,
since an unrecognised failure is more likely a real bug than routine absence.
""".
-spec seed_from_meta(binary(), binary(), side()) -> maybe_signature().
seed_from_meta(ClassName, Selector, Side) ->
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        {ok, Module} = beamtalk_class_metadata:lookup_module(ClassAtom),
        %% A qualified remote call auto-loads an as-yet-unloaded module (unlike
        %% erlang:function_exported/3, which only inspects already-loaded code
        %% and silently reports `false` for a module nobody has touched yet —
        %% exactly the state a rarely-hit class can be in during a workspace
        %% session). `undef` from a module with no `__beamtalk_meta/0` (or no
        %% code at all) is caught below like any other resolution failure.
        Meta = Module:'__beamtalk_meta'(),
        InfoKey =
            case Side of
                instance -> method_info;
                class -> class_method_info
            end,
        MethodInfoMap = maps:get(InfoKey, Meta, #{}),
        SelectorAtom = binary_to_existing_atom(Selector, utf8),
        case maps:find(SelectorAtom, MethodInfoMap) of
            {ok, Entry} ->
                #{
                    return_type => meta_type_to_binary(maps:get(return_type, Entry, none)),
                    param_types => [
                        meta_type_to_binary(T)
                     || T <- maps:get(param_types, Entry, [])
                    ]
                };
            error ->
                undefined
        end
    catch
        %% Expected, ordinary resolution failures — silent, no log:
        %%   badarg   — ClassName/Selector isn't an atom yet (brand-new this session).
        %%   {badmatch, not_found} — class not registered in beamtalk_class_metadata.
        %%   undef    — Module has no __beamtalk_meta/0 (or no code at all).
        error:badarg ->
            undefined;
        error:{badmatch, not_found} ->
            undefined;
        error:undef ->
            undefined;
        %% Anything else is unexpected — still degrade to `undefined` (this
        %% function must never crash the capture hook), but log it so a real
        %% bug (e.g. a __beamtalk_meta/0 shape this module doesn't understand)
        %% doesn't silently masquerade as "no baseline to compare against".
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Unexpected failure seeding signature from __beamtalk_meta/0",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassName,
                    selector => Selector,
                    side => Side,
                    domain => [beamtalk, runtime]
                }
            ),
            undefined
    end.

-doc """
Render a `meta_type_repr()` (ADR 0068 — atom, `{type_param, Name, Index}`, or
`{generic, Base, Params}`) as the same type-name string the compiler emits via
`TypeAnnotation:type_name/0`, so a `__beamtalk_meta`-seeded signature compares
equal to a freshly-compiled one when nothing actually changed.
""".
-spec meta_type_to_binary(term()) -> binary().
meta_type_to_binary(none) ->
    <<"Dynamic">>;
meta_type_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
meta_type_to_binary({type_param, Name, _Index}) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
meta_type_to_binary({generic, Base, Params}) when is_atom(Base), is_list(Params) ->
    ParamBins = [meta_type_to_binary(P) || P <- Params],
    iolist_to_binary([
        atom_to_binary(Base, utf8), "(", lists:join(<<", ">>, ParamBins), ")"
    ]);
meta_type_to_binary(Other) ->
    %% Defensive fallback for a shape this module doesn't recognise — never
    %% crash the capture hook over a cosmetic rendering gap.
    iolist_to_binary(io_lib:format("~p", [Other])).
