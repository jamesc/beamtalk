%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_facade).

%%% **DDD Context:** Workspace Context

-moduledoc """
Backing module for the `Workspace` class-side facade (ADR 0129 §4).

`Workspace` is `sealed typed Object subclass: Workspace native:
beamtalk_workspace_facade`; each class-side method is `=> self delegate` (or a
one-line FFI call) into the same-named function here. Every function calls
`beamtalk_capability:require_workspace/1` exactly once, before touching the
workspace, so a node with no running workspace (e.g. `beamtalk test`) raises a
structured `no_workspace` error instead of a confusing downstream failure.
The guard passes in every workspace mode (`run`, `workspace`, `release`);
release-mode refusals (`beamtalk_capability:require/3`) still apply afterwards,
inside `beamtalk_workspace_interface_primitives`.

`isAvailable/0` is the one unguarded entry: it never raises.

`testScope/0` and `testTarget/1` guard the two `Workspace test` selectors,
whose bodies are pure Beamtalk (`TestRunner ...`), and return the value the
body then passes on.
""".

-export([
    isAvailable/0,
    classes/0,
    bindings/0,
    currentSession/0,
    sessions/0,
    sync/0,
    changes/0,
    load/1,
    newClass/2,
    moveClass/2,
    flush/0,
    flush/1,
    flush/2,
    flushIncludingDestructive/0,
    recheckImage/0,
    testScope/0,
    testTarget/1,
    bind/2,
    unbind/1,
    startSupervisor/1,
    stopSupervisor/1,
    autoflush/0,
    autoflush/1,
    dependencies/0
]).

-doc "True iff a workspace is running on this node. Never raises.".
-spec isAvailable() -> boolean().
isAvailable() ->
    beamtalk_capability:recorded() =/= none.

-doc "`Workspace classes`, guarded by `require_workspace/1`.".
-spec classes() -> term().
classes() ->
    ok = beamtalk_capability:require_workspace(classes),
    beamtalk_workspace_interface_primitives:classes().

-doc "`Workspace bindings`, guarded by `require_workspace/1`.".
-spec bindings() -> term().
bindings() ->
    ok = beamtalk_capability:require_workspace(bindings),
    beamtalk_session_primitives:bindingsView().

-doc "`Workspace currentSession`, guarded by `require_workspace/1`.".
-spec currentSession() -> term().
currentSession() ->
    ok = beamtalk_capability:require_workspace(currentSession),
    beamtalk_workspace_interface_primitives:currentSession().

-doc "`Workspace sessions`, guarded by `require_workspace/1`.".
-spec sessions() -> term().
sessions() ->
    ok = beamtalk_capability:require_workspace(sessions),
    beamtalk_workspace_interface_primitives:sessions().

-doc "`Workspace sync`, guarded by `require_workspace/1`.".
-spec sync() -> term().
sync() ->
    ok = beamtalk_capability:require_workspace(sync),
    beamtalk_workspace_interface_primitives:sync().

-doc "`Workspace changes`, guarded by `require_workspace/1`.".
-spec changes() -> term().
changes() ->
    ok = beamtalk_capability:require_workspace(changes),
    beamtalk_workspace_changelog:changeLog().

-doc "`Workspace load:`, guarded by `require_workspace/1`.".
-spec load(term()) -> term().
load(Path) ->
    ok = beamtalk_capability:require_workspace('load:'),
    beamtalk_workspace_interface_primitives:load(Path).

-doc "`Workspace newClass:at:`, guarded by `require_workspace/1`.".
-spec newClass(term(), term()) -> term().
newClass(Source, Path) ->
    ok = beamtalk_capability:require_workspace('newClass:at:'),
    beamtalk_workspace_interface_primitives:newClass(Source, Path).

-doc "`Workspace moveClass:to:`, guarded by `require_workspace/1`.".
-spec moveClass(term(), term()) -> term().
moveClass(ClassArg, NewPath) ->
    ok = beamtalk_capability:require_workspace('moveClass:to:'),
    beamtalk_workspace_interface_primitives:moveClass(ClassArg, NewPath).

-doc "`Workspace flush`, guarded by `require_workspace/1`.".
-spec flush() -> term().
flush() ->
    ok = beamtalk_capability:require_workspace(flush),
    beamtalk_workspace_interface_primitives:flush().

-doc "`Workspace flush:`, guarded by `require_workspace/1`.".
-spec flush(term()) -> term().
flush(Filter) ->
    ok = beamtalk_capability:require_workspace('flush:'),
    beamtalk_workspace_interface_primitives:flush(Filter).

-doc "`Workspace flush:confirmDestructive:`, guarded by `require_workspace/1`.".
-spec flush(term(), term()) -> term().
flush(Filter, ConfirmDestructive) ->
    ok = beamtalk_capability:require_workspace('flush:confirmDestructive:'),
    beamtalk_workspace_interface_primitives:flush(Filter, ConfirmDestructive).

-doc "`Workspace flushIncludingDestructive`, guarded by `require_workspace/1`.".
-spec flushIncludingDestructive() -> term().
flushIncludingDestructive() ->
    ok = beamtalk_capability:require_workspace(flushIncludingDestructive),
    beamtalk_workspace_interface_primitives:flushIncludingDestructive().

-doc "`Workspace recheckImage`, guarded by `require_workspace/1`.".
-spec recheckImage() -> term().
recheckImage() ->
    ok = beamtalk_capability:require_workspace(recheckImage),
    beamtalk_workspace_interface_primitives:recheckImage().

-doc "`Workspace test`, guarded by `require_workspace/1`.".
-spec testScope() -> term().
testScope() ->
    ok = beamtalk_capability:require_workspace(test),
    0.

-doc "`Workspace test:`, guarded by `require_workspace/1`.".
-spec testTarget(term()) -> term().
testTarget(TestClass) ->
    ok = beamtalk_capability:require_workspace('test:'),
    TestClass.

-doc "`Workspace bind:as:`, guarded by `require_workspace/1`.".
-spec bind(term(), term()) -> term().
bind(Value, Name) ->
    ok = beamtalk_capability:require_workspace('bind:as:'),
    beamtalk_workspace_interface_primitives:bind(Value, Name).

-doc "`Workspace unbind:`, guarded by `require_workspace/1`.".
-spec unbind(term()) -> term().
unbind(Name) ->
    ok = beamtalk_capability:require_workspace('unbind:'),
    beamtalk_workspace_interface_primitives:unbind(Name).

-doc "`Workspace startSupervisor:`, guarded by `require_workspace/1`.".
-spec startSupervisor(term()) -> term().
startSupervisor(ClassArg) ->
    ok = beamtalk_capability:require_workspace('startSupervisor:'),
    beamtalk_workspace_interface_primitives:startSupervisor(ClassArg).

-doc "`Workspace stopSupervisor:`, guarded by `require_workspace/1`.".
-spec stopSupervisor(term()) -> term().
stopSupervisor(ClassArg) ->
    ok = beamtalk_capability:require_workspace('stopSupervisor:'),
    beamtalk_workspace_interface_primitives:stopSupervisor(ClassArg).

-doc "`Workspace autoflush`, guarded by `require_workspace/1`.".
-spec autoflush() -> term().
autoflush() ->
    ok = beamtalk_capability:require_workspace(autoflush),
    beamtalk_workspace_interface_primitives:autoflush().

-doc "`Workspace autoflush:`, guarded by `require_workspace/1`.".
-spec autoflush(term()) -> term().
autoflush(Enabled) ->
    ok = beamtalk_capability:require_workspace('autoflush:'),
    beamtalk_workspace_interface_primitives:setAutoflush(Enabled).

-doc "`Workspace dependencies`, guarded by `require_workspace/1`.".
-spec dependencies() -> term().
dependencies() ->
    ok = beamtalk_capability:require_workspace(dependencies),
    beamtalk_workspace_interface_primitives:dependencies().
