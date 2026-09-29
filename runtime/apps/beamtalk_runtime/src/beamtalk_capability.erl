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
* `compiler` — compiles source. Refused whenever the node records
  `include_compiler => false`: in `release` mode with
  `release_mode_no_compiler` (unless the release was built with
  `include_compiler`), and in `run`/`workspace` mode with
  `run_mode_no_compiler` (a packaged escript starts no compiler).
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
    recorded/0,
    require_workspace/1,
    require_workspace/2,
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

-define(NO_WORKSPACE_HINT, <<
    "Workspace operations are available in the REPL (beamtalk repl), "
    "beamtalk run and releases, not under beamtalk test. "
    "For class lookup use Beamtalk classNamed:."
>>).

-define(RUN_COMPILER_HINT, <<
    "This node was started without a compiler (a packaged escript ships none). "
    "Run the project with beamtalk run or beamtalk repl to compile source."
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

-doc """
Forget this node's recorded capabilities. Called when
`beamtalk_workspace_sup` shuts down (via its capability guard child), so a
stopped workspace no longer looks present, and by tests.
""".
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
    case recorded() of
        {ok, Caps} -> Caps;
        none -> #{mode => workspace, include_compiler => true}
    end.

-doc """
The capabilities a workspace supervisor recorded on this node, or `none` if
none has (a bare runtime, e.g. under `beamtalk test`, or after the workspace
shut down). Unlike `current/0` this has no permissive default.
""".
-spec recorded() -> {ok, capabilities()} | none.
recorded() ->
    case persistent_term:get(?KEY, undefined) of
        undefined -> none;
        Caps -> {ok, Caps}
    end.

-doc """
Require a running workspace for `Selector` (ADR 0129 §4). Returns `ok` once a
workspace supervisor has recorded capabilities on this node (any mode),
otherwise raises `#beamtalk_error{kind = no_workspace}` with class
`'Workspace'`. Deliberately stricter than `current/0`, whose permissive
default keeps bare-runtime unit tests working.
""".
-spec require_workspace(atom()) -> ok.
require_workspace(Selector) ->
    require_workspace(Selector, 'Workspace').

-doc "Like `require_workspace/1`, naming `Class` as the refused receiver.".
-spec require_workspace(atom(), atom()) -> ok.
require_workspace(Selector, Class) when is_atom(Selector), is_atom(Class) ->
    case recorded() of
        {ok, _Caps} ->
            ok;
        none ->
            Message = iolist_to_binary(
                io_lib:format(
                    "~ts>>~ts needs a running workspace; none is running on this node",
                    [Class, Selector]
                )
            ),
            Err0 = beamtalk_error:new(no_workspace, Class, Selector),
            Err1 = beamtalk_error:with_message(Err0, Message),
            beamtalk_error:raise(beamtalk_error:with_hint(Err1, ?NO_WORKSPACE_HINT))
    end.

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
%% The `test` op's `file` form (it compiles the file; the class form runs
%% already-loaded tests and is `always`).
classify(test_file) -> compiler;
%% The Inspector's `evaluate:` (ADR 0095) compiles its source.
classify('evaluate:') -> compiler;
%% `complete`'s compiler-port type-inference fallback (the op itself is
%% `always`: its tokeniser-driven completions need no compiler).
classify(completion_type_inference) -> compiler;
%% Beamtalk-level operations on the working tree / on-disk ChangeLog
%% (ADR 0082, 0113, 0114).
classify(flush) -> workspace;
classify('flush:') -> workspace;
classify('flush:confirmDestructive:') -> workspace;
classify(flushIncludingDestructive) -> workspace;
%% `Workspace changes` (the ChangeLog): `flushKinds:` flushes; `revert:`
%% reinstalls, removes or recompiles classes and methods from recorded
%% ChangeEntries (ADR 0082, 0113, 0114).
classify('changes flushKinds:') -> workspace;
classify('changes revert:') -> workspace;
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
permitted(compiler, #{include_compiler := IncludeCompiler}) -> IncludeCompiler;
permitted(workspace, #{mode := Mode}) -> Mode =/= release.

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
        false -> {error, refusal(CapClass, Op, Class, subject_text(Subject), Caps)}
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

-spec refusal(compiler | workspace, operation(), atom(), binary(), capabilities()) ->
    #beamtalk_error{}.
refusal(compiler, Op, Class, Subject, #{mode := Mode}) when Mode =/= release ->
    Message = iolist_to_binary([
        Subject,
        <<
            " cannot be compiled.\n\n"
            "  This node was started without a compiler."
        >>
    ]),
    build(run_mode_no_compiler, Op, Class, Message, ?RUN_COMPILER_HINT, Mode);
refusal(compiler, Op, Class, Subject, _Caps) ->
    Message = iolist_to_binary([
        Subject,
        <<
            " cannot be compiled.\n\n"
            "  This node is running an OTP release, which ships no compiler and no\n"
            "  source. Live method patching is a development-mode operation."
        >>
    ]),
    build(release_mode_no_compiler, Op, Class, Message, ?COMPILER_HINT, release);
refusal(workspace, Op, Class, Subject, _Caps) ->
    Message = iolist_to_binary([
        Subject,
        <<
            " cannot change the workspace.\n\n"
            "  This node is running an OTP release, which has no working tree and\n"
            "  no on-disk ChangeLog. Flush, removal and rename are development-mode\n"
            "  operations."
        >>
    ]),
    build(release_mode_no_workspace, Op, Class, Message, ?WORKSPACE_HINT, release).

-spec build(atom(), operation(), atom(), binary(), binary(), mode()) -> #beamtalk_error{}.
build(Kind, Op, Class, Message, Hint, Mode) ->
    Err0 = beamtalk_error:new(Kind, Class),
    Err1 = beamtalk_error:with_message(Err0, Message),
    Err2 = beamtalk_error:with_hint(Err1, Hint),
    beamtalk_error:with_details(Err2, #{mode => Mode, operation => Op}).
