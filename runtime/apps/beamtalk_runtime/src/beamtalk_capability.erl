%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_capability).

%%% **DDD Context:** Runtime Context

-moduledoc """
Node capability classification for the three workspace modes
(ADR 0125 §1.4, §1.5).

A node runs in one of three modes, fixed when `beamtalk_workspace_sup`
starts:

| Mode | Started by | Compiler | Working tree |
|------|------------|----------|--------------|
| `run` | `beamtalk run`, packaged escript | yes | no REPL, so nothing asks |
| `workspace` | `beamtalk repl` / `workspace` | yes | yes |
| `release` | an OTP release (`beamtalk release`) | only with `include_compiler` | never |

This module is the **single** answer to "is this operation available on this
node?". Every op module and every Beamtalk-level primitive that compiles
source or mutates the workspace consults it (`check/1,3`, `require/3`,
`available/1`) rather than testing the mode itself, so the §1.5 table lives
in exactly one place: `classify/1`.

Each operation falls into one of three classes:

* `always` — needs nothing a release lacks (`run-entry`, `inspect`,
  `actors`, `actor-stats`, `pid-stats`, `sessions`, …). Anything
  `classify/1` does not name is `always`.
* `compiler` — compiles source. Refused in `release` mode with
  `release_mode_no_compiler` unless the release was built with
  `include_compiler`.
* `workspace` — writes to (or rewrites) the working tree or its on-disk
  ChangeLog. Refused in `release` mode with `release_mode_no_workspace`,
  **even with** `include_compiler`: a release has no working tree to flush to.

## Why this module lives in `beamtalk_runtime`

The refusals are raised both by the REPL op layer (`beamtalk_workspace`) and
by `Behaviour` primitives (`beamtalk_behaviour_intrinsics`, in this app).
`beamtalk_runtime` sits below `beamtalk_workspace`, so the classification
lives here as the shared leaf both call, rather than being duplicated.

## Where the mode comes from

`beamtalk_workspace_sup:init/1` records the node's capabilities with
`set/1` before any child starts. A node with nothing recorded (a bare
runtime in a unit test, or any node that never started a workspace
supervisor) reports `workspace` capabilities, so this module only ever
narrows what a node that *declared* itself a release may do.
""".

-include("beamtalk.hrl").

-export([
    set/1,
    clear/0,
    current/0,
    classify/1,
    available/1,
    available/2,
    check/1,
    check/3,
    check/4,
    require/3
]).

-export_type([mode/0, capabilities/0, capability_class/0, operation/0, subject/0]).

-type mode() :: run | workspace | release.

-type capabilities() :: #{mode := mode(), include_compiler := boolean()}.

-type capability_class() :: always | compiler | workspace.

-doc """
An operation to classify. A binary is a REPL protocol op name
(`<<"eval">>`, `<<"load-source">>`, …); an atom is a Beamtalk-level
operation (a `Behaviour`/`Workspace` selector such as `'compile:source:'`
or `flush`, or an internal compile step such as `completion_type_inference`).
""".
-type operation() :: binary() | atom().

-doc """
What a refusal names — `<<"Counter >> increment">>` — or a fun producing it,
evaluated only if the operation is actually refused.
""".
-type subject() :: binary() | fun(() -> binary()).

-define(KEY, {?MODULE, capabilities}).

-define(COMPILER_HINT, <<
    "To change this code: edit the source, `beamtalk release`, and deploy. "
    "To run a live-patchable image in production instead, rebuild with "
    "[release] include-compiler = true (see ADR 0125 section 1.5)."
>>).

-define(WORKSPACE_HINT, <<
    "To change this code: edit the source, `beamtalk release`, and deploy. "
    "[release] include-compiler = true does not re-enable workspace "
    "operations: a release has no working tree (see ADR 0125 section 1.5)."
>>).

%%% Node capabilities

-doc """
Record this node's capabilities. Called once by
`beamtalk_workspace_sup:init/1`; the value is a `persistent_term`, so every
check is a constant-time read with no process round-trip.
""".
-spec set(capabilities()) -> ok.
set(#{mode := Mode, include_compiler := IncludeCompiler} = Caps) when
    (Mode =:= run orelse Mode =:= workspace orelse Mode =:= release),
    is_boolean(IncludeCompiler)
->
    %% persistent_term:put/2 of an unchanged value is a no-op (no global GC).
    persistent_term:put(?KEY, Caps).

-doc "Forget this node's recorded capabilities (tests).".
-spec clear() -> ok.
clear() ->
    _ = persistent_term:erase(?KEY),
    ok.

-doc """
This node's capabilities. Defaults to full `workspace` capabilities when no
workspace supervisor has recorded any.
""".
-spec current() -> capabilities().
current() ->
    persistent_term:get(?KEY, #{mode => workspace, include_compiler => true}).

%%% Classification (ADR 0125 §1.5)

-doc """
The capability an operation needs. This is the §1.5 table; everything it
does not name is `always`.
""".
-spec classify(operation()) -> capability_class().
%% REPL protocol ops that compile source.
classify(<<"eval">>) -> compiler;
classify(<<"load-source">>) -> compiler;
classify(<<"load-project">>) -> compiler;
classify(<<"load-tests">>) -> compiler;
classify(<<"show-codegen">>) -> compiler;
classify(<<"diagnostics">>) -> compiler;
%% REPL protocol ops that write to the working tree or remove a class.
classify(<<"unload">>) -> workspace;
classify(<<"save-native-source">>) -> workspace;
classify(<<"save-section">>) -> workspace;
%% Beamtalk-level operations that compile source.
classify('compile:source:') -> compiler;
classify('tryCompile:source:') -> compiler;
classify('precheckCompile:source:') -> compiler;
classify(reload) -> compiler;
classify('load:') -> compiler;
classify(sync) -> compiler;
classify('newClass:at:') -> compiler;
%% `complete`'s compiler-port type-inference fallback (the op itself is
%% `always`: its tokeniser-driven completions need no compiler).
classify(completion_type_inference) -> compiler;
%% Beamtalk-level operations on the working tree / on-disk ChangeLog
%% (ADR 0082, 0113, 0114).
classify(flush) -> workspace;
classify('flush:') -> workspace;
classify('flush:confirmDestructive:') -> workspace;
classify(flushIncludingDestructive) -> workspace;
%% The autoflush a durable live patch triggers (ADR 0082 Phase 4) is a flush.
classify(autoflush) -> workspace;
classify('moveClass:to:') -> workspace;
classify(removeFromSystem) -> workspace;
classify('renameTo:') -> workspace;
classify('renameSelector:to:') -> workspace;
%% Everything else — `run-entry`, `inspect`, `actors`, `actor-stats`,
%% `pid-stats`, `sessions`, `complete`, `describe`, … — needs nothing a
%% release lacks.
classify(Op) when is_binary(Op); is_atom(Op) -> always.

-doc "Is `Op` available on this node?".
-spec available(operation()) -> boolean().
available(Op) ->
    available(Op, current()).

-doc "Is `Op` available on a node with capabilities `Caps`? Pure.".
-spec available(operation(), capabilities()) -> boolean().
available(Op, Caps) ->
    permitted(classify(Op), Caps).

-spec permitted(capability_class(), capabilities()) -> boolean().
permitted(always, _Caps) -> true;
permitted(_Class, #{mode := Mode}) when Mode =/= release -> true;
permitted(compiler, #{include_compiler := IncludeCompiler}) -> IncludeCompiler;
permitted(workspace, _Caps) -> false.

%%% Checks

-doc """
Check a REPL protocol op against this node's capabilities. Returns `ok`, or
`{error, #beamtalk_error{}}` naming the mode and the alternative.
""".
-spec check(binary()) -> ok | {error, #beamtalk_error{}}.
check(Op) when is_binary(Op) ->
    check(Op, 'REPL', fun() -> iolist_to_binary([<<"The '">>, Op, <<"' operation">>]) end).

-doc """
Check `Op` against this node's capabilities. `Class` is the error's class and
`Subject` names what was refused in the message (e.g. `<<"Counter >> increment">>`);
it may be a zero-arity fun, evaluated only when the operation is refused, so
callers on a hot path pay nothing to describe a refusal that never happens.
""".
-spec check(operation(), atom(), subject()) -> ok | {error, #beamtalk_error{}}.
check(Op, Class, Subject) ->
    check(Op, Class, Subject, current()).

-doc "Pure form of `check/3` against explicit capabilities.".
-spec check(operation(), atom(), subject(), capabilities()) ->
    ok | {error, #beamtalk_error{}}.
check(Op, Class, Subject, Caps) ->
    CapClass = classify(Op),
    case permitted(CapClass, Caps) of
        true -> ok;
        false -> {error, refusal(CapClass, Op, Class, subject_text(Subject))}
    end.

-doc """
Like `check/3`, but raises the refusal as a Beamtalk exception. For
primitives that signal errors rather than return them.
""".
-spec require(operation(), atom(), subject()) -> ok.
require(Op, Class, Subject) ->
    case check(Op, Class, Subject) of
        ok -> ok;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

%%% Refusals (ADR 0125 §1.5 wording)

-spec subject_text(subject()) -> binary().
subject_text(Subject) when is_binary(Subject) -> Subject;
subject_text(Subject) when is_function(Subject, 0) -> Subject().

-spec refusal(compiler | workspace, operation(), atom(), binary()) -> #beamtalk_error{}.
refusal(compiler, Op, Class, Subject) ->
    Message = iolist_to_binary([
        Subject,
        <<
            " cannot be compiled.\n\n"
            "  This node is running an OTP release, which ships no compiler and no\n"
            "  source. Live method patching is a development-mode operation."
        >>
    ]),
    build(release_mode_no_compiler, Op, Class, Message, ?COMPILER_HINT);
refusal(workspace, Op, Class, Subject) ->
    Message = iolist_to_binary([
        Subject,
        <<
            " cannot change the workspace.\n\n"
            "  This node is running an OTP release, which has no working tree and\n"
            "  no on-disk ChangeLog. Flush, removal and rename are development-mode\n"
            "  operations."
        >>
    ]),
    build(release_mode_no_workspace, Op, Class, Message, ?WORKSPACE_HINT).

-spec build(atom(), operation(), atom(), binary(), binary()) -> #beamtalk_error{}.
build(Kind, Op, Class, Message, Hint) ->
    Err0 = beamtalk_error:new(Kind, Class),
    Err1 = beamtalk_error:with_message(Err0, Message),
    Err2 = beamtalk_error:with_hint(Err1, Hint),
    beamtalk_error:with_details(Err2, #{mode => release, operation => Op}).
