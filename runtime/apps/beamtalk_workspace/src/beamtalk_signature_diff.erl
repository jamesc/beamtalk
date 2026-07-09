%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_signature_diff).

%%% **DDD Context:** Workspace Context

-moduledoc """
Pure signature-diff classification (ADR 0105 Phase 1, BT-2777).

Given a method's previous-generation signature and its newly-compiled
signature, classifies the change as `signature_change` (return/param types
differ), `removal` (the selector no longer exists), or `no_op` (nothing
comparable changed, including "no previous generation to compare against").

This module is deliberately side-effect-free — it does not know about the
class registry, the compiler port, or the signature-generation store
(`beamtalk_workspace_signature_store`). Callers pass in already-resolved
signature values; `beamtalk_workspace_signature_store:capture/4` is the usual
caller (it resolves the previous generation and then delegates here).

Shape/`state:` diffs (added/removed/retyped fields) are explicitly out of
scope — ADR 0105 Phase 2.
""".

-export([diff/2]).

-export_type([signature/0, classification/0]).

%% A method's declared signature: return type + positional parameter types,
%% each rendered as the same type-name string the class hierarchy and
%% `__beamtalk_meta/0` use (`TypeAnnotation:type_name/0` on the compiler
%% side). An unannotated return/param is the `<<"Dynamic">>` sentinel, never
%% omitted, so two signatures are always directly comparable.
-type signature() :: #{return_type := binary(), param_types := [binary()]}.

%% `removed` marks that the selector no longer exists on the class (BT-2777
%% acceptance criteria's "removal" case — the method-delete path records this
%% instead of a signature() map). `undefined` marks "no generation recorded
%% yet" — the seed state before any patch, or an unresolvable original
%% (nothing to compare against, so classification degrades to `no_op` rather
%% than risk a false "signature changed").
-type maybe_signature() :: signature() | removed | undefined.

-type classification() :: signature_change | removal | no_op.

-doc """
Classify the change from `Old` (generation N-1) to `New` (generation N).

- `New =:= removed` — the method was deleted. Always `removal`, regardless of
  `Old` (even if `Old` was itself `removed` — a double-delete is still a
  removal report, not a no-op; callers are not expected to invoke `diff/2`
  twice for the same removal, but the classification stays honest either way).
- `Old =:= undefined` — no previous generation to compare against (first-ever
  capture for this selector, or the original signature could not be
  resolved). Reporting `signature_change` here would be a guess with no
  baseline, so this is `no_op` — advisory-never-blocking (ADR 0105) means we
  do not manufacture a finding from nothing.
- `Old =:= New` (structural equality, including `Old =:= removed` and
  `New` being the same map) — `no_op`.
- Anything else — `signature_change`.
""".
-spec diff(maybe_signature(), maybe_signature()) -> classification().
diff(_Old, removed) ->
    removal;
diff(undefined, _New) ->
    no_op;
diff(Old, New) when Old =:= New ->
    no_op;
diff(_Old, _New) ->
    signature_change.
