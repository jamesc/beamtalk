%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_callback).

%%% **DDD Context:** Runtime Context

-moduledoc """
The runtime's seam into the workspace layer (BT-4792).

`beamtalk_runtime` sits below `beamtalk_workspace` and cannot depend on it at
compile time, yet its refactoring and reload primitives (`removeSelector:`,
`renameTo:`, `compile:source:`, `reload`, the Inspector's `evaluate:`) need
the workspace's compile-and-install pipeline. This module is that seam:

* **`-callback`s** declare the operations the workspace layer provides. The
  workspace module that implements them (`beamtalk_repl_eval`) declares
  `-behaviour(beamtalk_workspace_callback)`, so the compiler checks its
  exports against this contract.
* **Wrapper functions** (one per callback) are what runtime code calls. They
  are ordinary exported functions in this app, so `rebar3 xref` sees every
  call site — unlike the `erlang:apply/3` calls they replace.
* **Registration** is a `persistent_term`. The workspace installs its module
  with `register/1` when it starts and removes it with `unregister/0` when it
  stops. With nothing registered every wrapper answers
  `{error, no_workspace}` (the `ok`-returning best-effort ones answer
  `{error, no_workspace}` too, and their callers ignore it).
""".

-include("beamtalk.hrl").

-export([register/1, register_if_loaded/1, unregister/0, registered_module/0]).

%% Wrappers, one per callback.
-export([
    reload_class_file/2,
    eval_with_self/2,
    compile_method/6,
    precheck_method/4,
    remove_method/4,
    emit_remove_change_entry/5,
    emit_extension_remove_change_entry/7,
    rewrite_sites/2,
    validate_sites/2,
    emit_rewrite_change_entry/2,
    capture_class_removal_snapshot/1,
    emit_remove_class_change_entry/4
]).

-define(KEY, {beamtalk_runtime, workspace_callback}).

-type no_workspace() :: {error, no_workspace}.
-export_type([no_workspace/0]).

-callback reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
-callback eval_with_self(term(), binary() | string()) ->
    {ok, term()} | {error, term()}.
-callback compile_method(
    binary(), atom() | binary(), binary(), durable | ephemeral, binary(), human | agent
) -> {ok, binary()} | {error, term()}.
-callback precheck_method(binary(), atom() | binary(), binary(), instance | class) ->
    {ok, term()} | {error, term()}.
-callback remove_method(
    binary(), atom() | binary(), instance | class, refuse_stdlib | allow_stdlib
) -> {ok, binary()} | {error, term()}.
-callback emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) -> ok.
-callback emit_extension_remove_change_entry(
    binary(),
    atom() | binary(),
    instance | class,
    atom() | undefined,
    binary() | undefined,
    binary(),
    human | agent
) -> ok.
-callback rewrite_sites(map() | undefined, [map()]) -> {ok, map()} | {error, term()}.
-callback validate_sites(map() | undefined, [map()]) -> ok | {error, term()}.
-callback emit_rewrite_change_entry(map(), map()) -> ok.
-callback capture_class_removal_snapshot(binary()) -> map().
-callback emit_remove_class_change_entry(binary(), map(), binary(), human | agent) -> ok.

%%% Registration

-doc "Install `Module` as this node's workspace callback module.".
-spec register(module()) -> ok.
register(Module) when is_atom(Module) ->
    %% persistent_term:put/2 of an unchanged value is a no-op (no global GC).
    persistent_term:put(?KEY, Module).

-doc """
Install `Module` only if its code can be loaded; otherwise leave the node as
it is (no workspace). For hosts that have the workspace code on the path but
run no workspace supervisor — `beamtalk test` — where the callback's pure
classification and compile entry points must still answer, exactly as the
`erlang:apply` calls this module replaced did.
""".
-spec register_if_loaded(module()) -> ok.
register_if_loaded(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> register(Module);
        {error, _} -> ok
    end.

-doc "Remove the workspace callback module (the workspace is stopping).".
-spec unregister() -> ok.
unregister() ->
    _ = persistent_term:erase(?KEY),
    ok.

-doc "The registered workspace callback module, or `none`.".
-spec registered_module() -> {ok, module()} | none.
registered_module() ->
    case persistent_term:get(?KEY, undefined) of
        undefined -> none;
        Module -> {ok, Module}
    end.

%%% Wrappers

-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()} | no_workspace().
reload_class_file(Path, ExpectedClassName) ->
    invoke(reload_class_file, [Path, ExpectedClassName]).

-spec eval_with_self(term(), binary() | string()) ->
    {ok, term()} | {error, term()} | no_workspace().
eval_with_self(Self, Source) ->
    invoke(eval_with_self, [Self, Source]).

-spec compile_method(
    binary(), atom() | binary(), binary(), durable | ephemeral, binary(), human | agent
) -> {ok, binary()} | {error, term()} | no_workspace().
compile_method(ClassNameBin, Selector, Source, Intent, Author, AuthorKind) ->
    invoke(compile_method, [ClassNameBin, Selector, Source, Intent, Author, AuthorKind]).

-spec precheck_method(binary(), atom() | binary(), binary(), instance | class) ->
    {ok, term()} | {error, term()} | no_workspace().
precheck_method(ClassNameBin, Selector, Source, Side) ->
    invoke(precheck_method, [ClassNameBin, Selector, Source, Side]).

-spec remove_method(
    binary(), atom() | binary(), instance | class, refuse_stdlib | allow_stdlib
) -> {ok, binary()} | {error, term()} | no_workspace().
remove_method(ClassNameBin, Selector, Side, StdlibPolicy) ->
    invoke(remove_method, [ClassNameBin, Selector, Side, StdlibPolicy]).

-spec emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) -> ok | no_workspace().
emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind) ->
    invoke(emit_remove_change_entry, [ClassNameBin, Selector, Side, Author, AuthorKind]).

-spec emit_extension_remove_change_entry(
    binary(),
    atom() | binary(),
    instance | class,
    atom() | undefined,
    binary() | undefined,
    binary(),
    human | agent
) -> ok | no_workspace().
emit_extension_remove_change_entry(
    ClassNameBin, Selector, Side, Owner, PrevSource, Author, AuthorKind
) ->
    invoke(emit_extension_remove_change_entry, [
        ClassNameBin, Selector, Side, Owner, PrevSource, Author, AuthorKind
    ]).

-spec rewrite_sites(map() | undefined, [map()]) -> {ok, map()} | {error, term()} | no_workspace().
rewrite_sites(DefinitionSite, ReferenceSites) ->
    invoke(rewrite_sites, [DefinitionSite, ReferenceSites]).

-spec validate_sites(map() | undefined, [map()]) -> ok | {error, term()} | no_workspace().
validate_sites(DefinitionSite, ReferenceSites) ->
    invoke(validate_sites, [DefinitionSite, ReferenceSites]).

-spec emit_rewrite_change_entry(map(), map()) -> ok | no_workspace().
emit_rewrite_change_entry(Spec, RewriteResult) ->
    invoke(emit_rewrite_change_entry, [Spec, RewriteResult]).

-spec capture_class_removal_snapshot(binary()) -> map() | no_workspace().
capture_class_removal_snapshot(ClassNameBin) ->
    invoke(capture_class_removal_snapshot, [ClassNameBin]).

-spec emit_remove_class_change_entry(binary(), map(), binary(), human | agent) ->
    ok | no_workspace().
emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind) ->
    invoke(emit_remove_class_change_entry, [ClassNameBin, Snapshot, Author, AuthorKind]).

%%% Internal

-spec invoke(atom(), [term()]) -> term() | no_workspace().
invoke(Function, Args) ->
    case registered_module() of
        {ok, Module} -> erlang:apply(Module, Function, Args);
        none -> {error, no_workspace}
    end.
