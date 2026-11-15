%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_module).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture (BT-4899): assemble, load and unload throwaway Erlang
modules. Replaces the per-file `erl_scan` -> `parse_form` -> `compile:forms`
-> `code:load_binary` pipelines, the hand-built
`{attribute, _, module, _}` form lists, and the
`code:purge` / `code:delete` / `code:purge` teardown triplets.

`SourceOrForms` is either Erlang source text (a string, binary or iolist of
complete top-level forms; no preprocessor, so no macros or records) or a list
of abstract forms.
""".

-export_type([forms/0]).

-type forms() :: [tuple()].

-export([
    compile/1,
    compile/2,
    load/2,
    load_binary/2,
    unload/1,
    with_module/3,
    meta_forms/2,
    meta_module/2
]).

-doc """
Compile source text or abstract forms to `{Module, Binary}`. Compile errors
raise (a `badmatch` carrying the `compile:forms/2` error list).
""".
-spec compile(iodata() | forms()) -> {module(), binary()}.
compile(SourceOrForms) ->
    compile(SourceOrForms, []).

-spec compile(iodata() | forms(), [compile:option()]) ->
    {module(), binary()}.
compile(SourceOrForms, Opts) ->
    {ok, Module, Bin} = compile:forms(forms(SourceOrForms), [binary, return_errors | Opts]),
    {Module, Bin}.

-doc """
Compile and load `SourceOrForms` as `Name`. The module declared by the source
must be `Name`. Returns `ok`; pair with `unload/1` (or use `with_module/3`).
""".
-spec load(module(), iodata() | forms()) -> ok.
load(Name, SourceOrForms) ->
    {Name, Bin} = compile(SourceOrForms),
    load_binary(Name, Bin).

-doc "Load an already-compiled `Binary` as `Name`, with a synthetic file name.".
-spec load_binary(module(), binary()) -> ok.
load_binary(Name, Bin) ->
    {module, Name} = code:load_binary(Name, atom_to_list(Name) ++ ".erl", Bin),
    ok.

-doc "Purge and delete `Name` (idempotent; fine if it was never loaded).".
-spec unload(module()) -> ok.
unload(Name) ->
    _ = code:purge(Name),
    _ = code:delete(Name),
    _ = code:purge(Name),
    ok.

-doc """
Load `Name`, run `Fun/0`, and always unload `Name` afterwards (also when
`Fun` raises). Returns what `Fun` returns.
""".
-spec with_module(module(), iodata() | forms(), fun(() -> Result)) -> Result.
with_module(Name, SourceOrForms, Fun) ->
    ok = load(Name, SourceOrForms),
    try
        Fun()
    after
        unload(Name)
    end.

-doc """
Abstract forms for a module `Name` whose `'__beamtalk_meta'/0` returns the
term `Meta`. Use `meta_forms/2` when the body must be an expression other than
a literal (build the `function` form yourself and compile with `load/2`).
""".
-spec meta_forms(module(), term()) -> forms().
meta_forms(Name, Meta) ->
    [
        {attribute, 1, module, Name},
        {attribute, 2, export, [{'__beamtalk_meta', 0}]},
        {function, 3, '__beamtalk_meta', 0, [
            {clause, 3, [], [], [erl_parse:abstract(Meta, [{line, 3}])]}
        ]}
    ].

-doc "Compile and load a module `Name` whose `'__beamtalk_meta'/0` returns `Meta`.".
-spec meta_module(module(), term()) -> module().
meta_module(Name, Meta) ->
    ok = load(Name, meta_forms(Name, Meta)),
    Name.

%% Source text -> abstract forms; a list of tuples is already forms.
forms([]) ->
    [];
forms(Forms = [First | _]) when is_tuple(First) ->
    Forms;
forms(Source) ->
    {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Source)),
    beamtalk_test_erl_forms:split_forms(Tokens).
