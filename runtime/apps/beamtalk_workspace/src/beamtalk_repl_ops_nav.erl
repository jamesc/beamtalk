%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_nav).

%%% **DDD Context:** REPL Session Context (Navigation bridge)

-moduledoc """
Op handler for the structured `nav-query` operation (BT-2239).

`nav-query` is the runtime-attached navigation channel that the LSP uses to
delegate `textDocument/{references,implementation,callHierarchy}` (and the
matching MCP tools) to `SystemNavigation` / `beamtalk_xref` rather than its
in-process AST walker.

Why a structured op (vs. an `eval` of `SystemNavigation default sendersOf:`):

* `eval` serialises result values through `beamtalk_repl_json:term_to_json/1`,
  which converts Beamtalk `Dictionary` tagged maps to their inspect string
  ("`a Dictionary(...)`"). Parsing that back into typed records on the LSP
  side is fragile.
* `beamtalk_xref` already exposes structured Erlang `site()` records with
  `owner`, `class_side`, `method`, `line`. Emitting those directly as a JSON
  array gives the LSP layer typed data with no inspect-string round-trip.

Request shape:
```
{"op": "nav-query", "id": "...", "kind": "<senders|implementors|references>",
  "selector": "increment"            // for senders/implementors (Beamtalk selector)
  "class":    "Counter"              // for references            (Beamtalk class name)
}
```

Response shape (success):
```
{"id": "...", "status": ["done"],
  "value": {
    "sites": [
      {"class": "Counter", "class_side": false, "method": "increment",
       "line": 7, "source_file": "examples/counter.bt"},
      ...
    ]
  }
}
```

`source_file` is the absolute path recorded in the BEAM module's
`beamtalk_source` attribute (see `beamtalk_reflection:source_file_from_module/1`)
or `null` if the class has no backing source (stdlib, dynamic, bootstrap).
The LSP layer canonicalises against its workspace roots before lookup.

`implementors_of` returns one row per class — `method` and `line` are
populated when the class has its own definition of the selector, otherwise
`null`. The LSP `textDocument/implementation` consumer (BT-2241) reads
`{class, source_file, line}` and ignores `method`.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, describe_ops/0]).

-doc "Handle the `nav-query` op.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"nav-query">>, Params, Msg, _SessionPid) ->
    case validate_params(Params) of
        {ok, {senders, Selector}} ->
            Sites = beamtalk_xref:senders_of(Selector),
            encode_sites(Sites, Msg);
        {ok, {references, ClassName}} ->
            Sites = beamtalk_xref:references_to(ClassName),
            encode_sites(Sites, Msg);
        {ok, {implementors, Selector}} ->
            Pairs = beamtalk_xref:implementors_of(Selector),
            encode_implementors(Pairs, Selector, Msg);
        {error, Reason} ->
            Err = beamtalk_error:new(argument_error, 'REPL'),
            Msg1 = iolist_to_binary([<<"nav-query: ">>, Reason]),
            beamtalk_repl_json:encode_error(
                beamtalk_error:with_message(Err, Msg1), Msg
            )
    end.

-doc "Advertise the `nav-query` op in `describe`.".
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"nav-query">> => #{
            <<"params">> => [<<"kind">>],
            <<"optional">> => [<<"selector">>, <<"class">>]
        }
    }.

%%% ====================================================================
%%% Internal
%%% ====================================================================

-spec validate_params(map()) ->
    {ok, {senders | references | implementors, atom()}}
    | {error, binary()}.
validate_params(Params) ->
    case maps:get(<<"kind">>, Params, undefined) of
        <<"senders">> ->
            with_selector(Params, senders);
        <<"implementors">> ->
            with_selector(Params, implementors);
        <<"references">> ->
            with_class(Params);
        undefined ->
            {error,
                <<"missing required parameter `kind` (one of senders/implementors/references)">>};
        Other when is_binary(Other) ->
            {error,
                iolist_to_binary([
                    <<"unknown kind `">>,
                    Other,
                    <<"` (expected senders/implementors/references)">>
                ])};
        _ ->
            {error, <<"`kind` must be a string">>}
    end.

-spec with_selector(map(), senders | implementors) ->
    {ok, {senders | implementors, atom()}} | {error, binary()}.
with_selector(Params, Kind) ->
    case maps:get(<<"selector">>, Params, undefined) of
        Sel when is_binary(Sel), byte_size(Sel) > 0 ->
            %% binary_to_existing_atom: untrusted client input must not be
            %% able to grow the VM atom table. xref lookups can only
            %% succeed for selectors already known to the runtime, so a
            %% missing-atom error returns an empty result set, not a
            %% validation failure.
            try
                {ok, {Kind, binary_to_existing_atom(Sel, utf8)}}
            catch
                error:badarg ->
                    {ok, {Kind, '__nav_query_unknown__'}}
            end;
        _ ->
            {error, <<"`selector` (non-empty string) is required for senders/implementors">>}
    end.

-spec with_class(map()) -> {ok, {references, atom()}} | {error, binary()}.
with_class(Params) ->
    case maps:get(<<"class">>, Params, undefined) of
        Cls when is_binary(Cls), byte_size(Cls) > 0 ->
            %% binary_to_existing_atom: see comment in with_selector/2.
            try
                {ok, {references, binary_to_existing_atom(Cls, utf8)}}
            catch
                error:badarg ->
                    {ok, {references, '__nav_query_unknown__'}}
            end;
        _ ->
            {error, <<"`class` (non-empty string) is required for references">>}
    end.

-spec encode_sites([beamtalk_xref:site()], beamtalk_repl_protocol:protocol_msg()) -> binary().
encode_sites(Sites, Msg) ->
    Rows = [site_to_row(S) || S <- Sites],
    Value = #{<<"sites">> => Rows},
    %% Bypass term_to_json — the value is already a JSON-shaped map of
    %% lists / binaries / integers / booleans / null, so we pass identity.
    beamtalk_repl_protocol:encode_result(Value, Msg, fun(V) -> V end).

-spec encode_implementors([{atom(), boolean()}], atom(), beamtalk_repl_protocol:protocol_msg()) ->
    binary().
encode_implementors(Pairs, Selector, Msg) ->
    Rows = [implementor_to_row(Cls, ClassSide, Selector) || {Cls, ClassSide} <- Pairs],
    Value = #{<<"sites">> => Rows},
    beamtalk_repl_protocol:encode_result(Value, Msg, fun(V) -> V end).

-spec site_to_row(beamtalk_xref:site()) -> map().
site_to_row(Site) ->
    #{
        owner := Owner,
        class_side := ClassSide,
        method := Method,
        line := Line
    } = Site,
    #{
        <<"class">> => atom_to_binary(Owner, utf8),
        <<"class_side">> => ClassSide,
        <<"method">> => atom_to_binary(Method, utf8),
        <<"line">> => Line,
        <<"source_file">> => source_file_of(Owner)
    }.

-spec implementor_to_row(atom(), boolean(), atom()) -> map().
implementor_to_row(Cls, ClassSide, Selector) ->
    %% For implementors we also surface the line of the method definition
    %% within `Cls` — `beamtalk_xref:method_info/{3,2}` is not exposed today;
    %% the LSP consumer (BT-2241) treats `method`/`line` as best-effort and
    %% falls back to "first line of the class file" when null.
    #{
        <<"class">> => atom_to_binary(Cls, utf8),
        <<"class_side">> => ClassSide,
        <<"method">> => atom_to_binary(Selector, utf8),
        <<"line">> => method_line_of(Cls, ClassSide, Selector),
        <<"source_file">> => source_file_of(Cls)
    }.

-spec source_file_of(atom()) -> binary() | null.
source_file_of(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            null;
        ClassPid ->
            ModuleName = beamtalk_object_class:module_name(ClassPid),
            case beamtalk_reflection:source_file_from_module(ModuleName) of
                nil -> null;
                Path when is_binary(Path) -> Path
            end
    end.

-spec method_line_of(atom(), boolean(), atom()) -> pos_integer() | null.
method_line_of(ClassName, ClassSide, Selector) ->
    %% The xref `method_info()` map carries the method-header line — exactly
    %% what `textDocument/implementation` (BT-2241) needs to anchor the goto.
    case beamtalk_xref:method_info(ClassName, ClassSide, Selector) of
        #{line := Line} when is_integer(Line), Line > 0 -> Line;
        _ -> null
    end.
