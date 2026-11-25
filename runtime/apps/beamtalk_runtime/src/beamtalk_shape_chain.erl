%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_chain).

%%% **DDD Context:** Hot Reload Context

-moduledoc """
Pure migration-chain runner (ADR 0123 Phase 2, BT-3536).

A genuine shared leaf: given a migrations table, a `{From, To}` version pair,
a field dictionary, and an invoker fun, produces the new dictionary. No
class registry, no `__beamtalk_meta`, no process — this is the module hot
reload, persistence, and distribution all share, so "which selectors are
migrations, in what order" is derived once, never re-implemented on either
side of a boundary (`docs/development/architecture-principles.md` §
Duplication & the Shared-Leaf-Module Pattern).

`beamtalk_shape_migration` is the only caller today; it supplies `Migrations`
from `__beamtalk_meta`'s `'shape_migrations'` and `Invoke` as a closure over
`beamtalk_object_class:local_call/3` (ADR 0123 §3, proven by BT-3535). Unit
tests exercise `migrate/4` standalone with a fake `Invoke` fun — no class
registry, no compiled module, no process.

**Chain semantics** (ADR 0123 § Runtime contract › Chain semantics): for each
version `K` in `From .. To-1`, apply `Migrations[K]` if present, else the step
is a no-op — nothing is defaulted or dropped between hooks, because only the
*final* declared shape is known to the reconcile step (which lives in
`beamtalk_shape_migration`, not here). A hook's result must be a `map()`
(a Beamtalk `Dictionary`), else the step fails. `To =< From` (a downgrade or
an already-current chain) runs no steps at all — reconciliation-only is the
caller's job.
""".

-export([migrate/4]).

-export_type([migrations/0, invoke_fun/0, step_error/0]).

-type migrations() :: #{pos_integer() => atom()}.
-type invoke_fun() :: fun((atom(), map()) -> map() | {error, term()}).
-type step_error() :: {Step :: pos_integer(), Selector :: atom(), Reason :: term()}.

-doc """
Run the migration chain from `From` to `To` over `Fields`, using `Invoke` to
apply each declared step's selector.

For each version `K` in `From, From+1, ..., To-1`: if `Migrations` has an
entry for `K`, calls `Invoke(Selector, CurrentFields)`; otherwise the step is
a no-op (`CurrentFields` unchanged). `Invoke`'s result must be a `map()` — a
non-map return, an `{error, Reason}` return, or a raised exception all fail
the step with `{error, {K, Selector, Reason}}`, which halts the chain
immediately (no further steps run).

`To =< From` — a downgrade (`To < From`) or an already-current chain
(`To =:= From`) — runs no steps and returns `Fields` unchanged: this module
has no notion of "reconcile", only "run the declared steps in order".
""".
-spec migrate(migrations(), {From :: integer(), To :: integer()}, Fields :: map(), invoke_fun()) ->
    {ok, NewFields :: map()} | {error, step_error()}.
migrate(Migrations, {From, To}, Fields, Invoke) when
    is_map(Migrations), is_integer(From), is_integer(To), is_map(Fields), is_function(Invoke, 2)
->
    run_steps(Migrations, From, To, Fields, Invoke).

%%====================================================================
%% Internal
%%====================================================================

-spec run_steps(migrations(), integer(), integer(), map(), invoke_fun()) ->
    {ok, map()} | {error, step_error()}.
run_steps(_Migrations, From, To, Fields, _Invoke) when From >= To ->
    {ok, Fields};
run_steps(Migrations, From, To, Fields, Invoke) ->
    case maps:find(From, Migrations) of
        {ok, Selector} ->
            case apply_step(Selector, Fields, Invoke) of
                {ok, NewFields} ->
                    run_steps(Migrations, From + 1, To, NewFields, Invoke);
                {error, Reason} ->
                    {error, {From, Selector, Reason}}
            end;
        error ->
            %% Gap: no migration declared for this version — no-op, per ADR
            %% 0123 (only the final declared shape is known to the reconcile
            %% step, which this pure leaf does not run).
            run_steps(Migrations, From + 1, To, Fields, Invoke)
    end.

-spec apply_step(atom(), map(), invoke_fun()) -> {ok, map()} | {error, term()}.
apply_step(Selector, Fields, Invoke) ->
    try Invoke(Selector, Fields) of
        NewFields when is_map(NewFields) ->
            {ok, NewFields};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {non_dictionary_result, Other}}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.
