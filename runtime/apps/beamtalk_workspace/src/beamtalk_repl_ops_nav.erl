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
{"op": "nav-query", "id": "...",
  "kind": "<senders|implementors|references|required_methods|conforming_classes|
            callers_of_native_module>",
  "selector": "increment"            // for senders/implementors (Beamtalk selector)
  "class":    "Counter"              // for references            (Beamtalk class name)
                                     //  and required_methods/conforming_classes
                                     //  (the *protocol* name, BT-2639)
  "module":   "lists"                // for callers_of_native_module (BT-2669)
                                     //  (the native Erlang module name)
}
```

The `callers_of_native_module` kind is the reverse of "go to native source": given
a native (Erlang) module, it returns the Beamtalk `class>>method` sites that call
into it via `(Erlang <module>) …`, in the same site-row shape as senders so the IDE
reuses the senders/implementors popover (BT-2495). Empty when the module has no
Beamtalk callers.

The protocol kinds (BT-2639) are the protocol equivalent of senders/implementors
— `required_methods` lists a protocol's contract selectors, `conforming_classes`
lists the classes that structurally conform to it. Both take `class` (the
protocol name) and back the System Browser's protocol-definition action row,
mirroring how `senders`/`implementors` back the method-editor action row.

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

-export([handle/4, handle_term/4, describe_ops/0]).

-doc """
Handle the `nav-query` op for the WebSocket transport — encodes the term result
to JSON at the edge (BT-2402).
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for `nav-query` (BT-2402, ADR 0085 read-surface).

Returns `{value, #{<<"sites">> => Rows}}` — the site rows are already a
wire-shaped JSON value (the whole point of `nav-query` is to skip the
`term_to_json` inspect-string round-trip; see the module doc) — or
`{error, #beamtalk_error{}}` on a validation failure.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"nav-query">>, Params, _Msg, _SessionPid) ->
    case validate_params(Params) of
        {ok, {senders, Selector}} ->
            Sites = beamtalk_xref:senders_of(Selector),
            {value, sites_value(Sites)};
        {ok, {references, ClassName}} ->
            Sites = beamtalk_xref:references_to(ClassName),
            {value, sites_value(Sites)};
        {ok, {implementors, Selector}} ->
            Pairs = beamtalk_xref:implementors_of(Selector),
            {value, implementors_value(Pairs, Selector)};
        {ok, {required_methods, ProtocolName}} ->
            Selectors = beamtalk_protocol_registry:required_methods(ProtocolName),
            {value, required_methods_value(Selectors, ProtocolName)};
        {ok, {conforming_classes, ProtocolName}} ->
            Classes = beamtalk_protocol_registry:conforming_classes(ProtocolName),
            {value, conforming_classes_value(Classes, ProtocolName)};
        {ok, {callers_of_native_module, Module}} ->
            Rows = beamtalk_xref:callers_of_native_module(Module),
            {value, native_callers_value(Rows)};
        {error, Reason} ->
            {error,
                beamtalk_repl_errors:make(
                    argument_error, 'REPL', iolist_to_binary([<<"nav-query: ">>, Reason])
                )}
    end.

-doc "Advertise the `nav-query` op in `describe`.".
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"nav-query">> => #{
            <<"params">> => [<<"kind">>],
            <<"optional">> => [<<"selector">>, <<"class">>, <<"module">>]
        }
    }.

%%% ====================================================================
%%% Internal
%%% ====================================================================

-spec validate_params(map()) ->
    {ok,
        {senders | implementors | references | required_methods | conforming_classes |
            callers_of_native_module, atom()}}
    | {error, binary()}.
validate_params(Params) ->
    case maps:get(<<"kind">>, Params, undefined) of
        <<"senders">> ->
            with_selector(Params, senders);
        <<"implementors">> ->
            with_selector(Params, implementors);
        <<"references">> ->
            with_class(Params, references);
        <<"required_methods">> ->
            with_class(Params, required_methods);
        <<"conforming_classes">> ->
            with_class(Params, conforming_classes);
        <<"callers_of_native_module">> ->
            with_module(Params);
        undefined ->
            {error, <<
                "missing required parameter `kind` (one of senders/implementors/"
                "references/required_methods/conforming_classes/callers_of_native_module)"
            >>};
        Other when is_binary(Other) ->
            {error,
                iolist_to_binary([
                    <<"unknown kind `">>,
                    Other,
                    <<
                        "` (expected senders/implementors/references/"
                        "required_methods/conforming_classes/callers_of_native_module)"
                    >>
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

-spec with_class(map(), references | required_methods | conforming_classes) ->
    {ok, {references | required_methods | conforming_classes, atom()}} | {error, binary()}.
with_class(Params, Kind) ->
    case maps:get(<<"class">>, Params, undefined) of
        Cls when is_binary(Cls), byte_size(Cls) > 0 ->
            %% binary_to_existing_atom: see comment in with_selector/2. For the
            %% protocol kinds (BT-2639) an unknown protocol name likewise yields
            %% the shared sentinel — `required_methods`/`conforming_classes`
            %% return [] for an unregistered protocol, not a validation error.
            try
                {ok, {Kind, binary_to_existing_atom(Cls, utf8)}}
            catch
                error:badarg ->
                    {ok, {Kind, '__nav_query_unknown__'}}
            end;
        _ ->
            {error, with_class_error(Kind)}
    end.

-spec with_class_error(references | required_methods | conforming_classes) -> binary().
with_class_error(references) ->
    <<"`class` (non-empty string) is required for references">>;
with_class_error(_ProtocolKind) ->
    <<"`class` (non-empty protocol name) is required for required_methods/conforming_classes">>.

-spec with_module(map()) -> {ok, {callers_of_native_module, atom()}} | {error, binary()}.
with_module(Params) ->
    case maps:get(<<"module">>, Params, undefined) of
        Mod when is_binary(Mod), byte_size(Mod) > 0 ->
            %% binary_to_existing_atom: see comment in with_selector/2. A native
            %% module name that is not yet an atom (the module is not loaded)
            %% yields the shared sentinel — `callers_of_native_module` returns []
            %% for it, not a validation error.
            try
                {ok, {callers_of_native_module, binary_to_existing_atom(Mod, utf8)}}
            catch
                error:badarg ->
                    {ok, {callers_of_native_module, '__nav_query_unknown__'}}
            end;
        _ ->
            {error, <<"`module` (non-empty native module name) is required for callers_of_native_module">>}
    end.

%% The site rows are already a JSON-shaped map of lists / binaries / integers /
%% booleans / null; the `{value, _}` op_result tag encodes them with identity so
%% term_to_json never sees them (BT-2402).
-spec sites_value([beamtalk_xref:site()]) -> map().
sites_value(Sites) ->
    Rows = [site_to_row(S) || S <- Sites],
    #{<<"sites">> => Rows}.

%% BT-2669: native-caller rows reuse the senders/implementors site-row shape so
%% the IDE's existing popover/list renderer (BT-2495) handles them unchanged —
%% clicking a row opens the calling `class>>method`. The `native_caller_row()`
%% already carries `owner`/`class_side`/`method`/`line`; we just attach the
%% source file + origin the renderer needs to open the method.
-spec native_callers_value([beamtalk_xref:native_caller_row()]) -> map().
native_callers_value(Rows) ->
    SiteRows = [native_caller_to_row(R) || R <- Rows],
    #{<<"sites">> => SiteRows}.

-spec native_caller_to_row(beamtalk_xref:native_caller_row()) -> map().
native_caller_to_row(Row) ->
    #{
        owner := Owner,
        class_side := ClassSide,
        method := Method,
        line := Line
    } = Row,
    #{
        <<"class">> => atom_to_binary(Owner, utf8),
        <<"class_side">> => ClassSide,
        <<"method">> => atom_to_binary(Method, utf8),
        <<"line">> => Line,
        <<"source_file">> => source_file_of(Owner),
        <<"source_origin">> => source_origin_of(Owner)
    }.

-spec implementors_value([{atom(), boolean()}], atom()) -> map().
implementors_value(Pairs, Selector) ->
    Rows = [implementor_to_row(Cls, ClassSide, Selector) || {Cls, ClassSide} <- Pairs],
    #{<<"sites">> => Rows}.

%% BT-2639: required-method rows mirror the implementor row shape so the IDE
%% reuses the same popover. The `method` is the required selector; `class` is the
%% owning protocol. `beamtalk_protocol_registry:required_methods/1` prefixes
%% class-side requirements with the literal `class ` (e.g. `'class fromString:'`),
%% which we strip to recover the bare selector and set `class_side => true` — so a
%% click navigates to the right (instance- vs class-side) Implementors.
-spec required_methods_value([atom()], atom()) -> map().
required_methods_value(Selectors, ProtocolName) ->
    Rows = [required_method_to_row(Sel, ProtocolName) || Sel <- Selectors],
    #{<<"sites">> => Rows}.

-spec required_method_to_row(atom(), atom()) -> map().
required_method_to_row(Selector, ProtocolName) ->
    {ClassSide, BareSelector} = split_class_side_selector(Selector),
    #{
        <<"class">> => atom_to_binary(ProtocolName, utf8),
        <<"class_side">> => ClassSide,
        <<"method">> => BareSelector,
        %% A protocol requirement has no defining source line — the row is
        %% navigable via its selector (→ Implementors), not via goto-definition.
        <<"line">> => null,
        <<"source_file">> => source_file_of(ProtocolName),
        <<"source_origin">> => source_origin_of(ProtocolName)
    }.

%% Split a required-method selector atom into `{ClassSide, BareSelectorBin}`.
%% Class-side requirements carry the `class ` prefix (BT-1611); everything else
%% is an instance-side selector.
-spec split_class_side_selector(atom()) -> {boolean(), binary()}.
split_class_side_selector(Selector) ->
    case atom_to_binary(Selector, utf8) of
        <<"class ", Bare/binary>> -> {true, Bare};
        Bin -> {false, Bin}
    end.

%% BT-2639: conforming-class rows carry only a class name — clicking opens that
%% class in the System Browser / definition pane (not a method tab), so `method`
%% is `null` and `class_side` is `false`. The shape stays compatible with the
%% senders/implementors popover rows so the IDE's popover renderer is reused.
-spec conforming_classes_value([atom()], atom()) -> map().
conforming_classes_value(Classes, _ProtocolName) ->
    Rows = [conforming_class_to_row(Cls) || Cls <- Classes],
    #{<<"sites">> => Rows}.

-spec conforming_class_to_row(atom()) -> map().
conforming_class_to_row(Cls) ->
    #{
        <<"class">> => atom_to_binary(Cls, utf8),
        <<"class_side">> => false,
        <<"method">> => null,
        <<"line">> => null,
        <<"source_file">> => source_file_of(Cls),
        <<"source_origin">> => source_origin_of(Cls)
    }.

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
        <<"source_file">> => source_file_of(Owner),
        <<"source_origin">> => source_origin_of(Owner)
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
        <<"source_file">> => source_file_of(Cls),
        <<"source_origin">> => source_origin_of(Cls)
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

-spec source_origin_of(atom()) -> binary().
source_origin_of(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            <<"project">>;
        ClassPid ->
            ModName = beamtalk_object_class:module_name(ClassPid),
            SourceFile =
                case beamtalk_reflection:source_file_from_module(ModName) of
                    nil -> null;
                    Path when is_binary(Path) -> Path
                end,
            beamtalk_repl_ops_browse:source_origin_of(ModName, SourceFile)
    end.

-spec method_line_of(atom(), boolean(), atom()) -> pos_integer() | null.
method_line_of(ClassName, ClassSide, Selector) ->
    %% The xref `method_info()` map carries the method-header line — exactly
    %% what `textDocument/implementation` (BT-2241) needs to anchor the goto.
    case beamtalk_xref:method_info(ClassName, ClassSide, Selector) of
        #{line := Line} when is_integer(Line), Line > 0 -> Line;
        _ -> null
    end.
