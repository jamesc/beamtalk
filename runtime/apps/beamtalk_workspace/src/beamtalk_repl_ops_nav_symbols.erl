%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_nav_symbols).

%%% **DDD Context:** REPL Session Context (Navigation bridge)

-moduledoc """
Op handler for the structured `nav-symbols` operation (BT-2244).

`nav-symbols` is the bulk class+method outline channel that the LSP uses
to back `textDocument/documentSymbol` (per-open-file outline) and
`workspace/symbol` (project-wide Ctrl-T quick-symbol-open) when the
editor's `delegateToRuntime` flag is on and a workspace is attached.

It is a deliberate sibling of `nav-query` (BT-2239) rather than a new
`kind` on that op: `nav-query`'s wire shape is locked to selector-shaped
navigation (senders / implementors / references — selector or class
name argument, one site per call site). The outline payload here is
shaped per-class-with-method-children, which doesn't fit the
single-flat-sites schema. `textDocument/typeHierarchy` (BT-2242) set the
precedent — when the wire shape diverges, add a new op rather than
overloading `nav-query`.

The headline win: classes loaded purely at the REPL (no backing `.bt`
file) — and methods installed via `Behaviour >>` or `compile:source:`
since the last flush — appear in workspace symbols here. The
LSP-side AST/glob fallback can't see either: it walks indexed files on
disk only.

Request shape:
```
{"op": "nav-symbols", "id": "...",
  "scope": "user" | "all"   // optional; default "all"
}
```

`scope` controls the class set:
* `"user"`  — only classes with a backing `.bt` file (mirrors
  `list-classes` `user` filter). The default for the LSP
  `document_symbol` consumer, where a file URI is the lookup key.
* `"all"` / absent — every loaded class, including stdlib, ClassBuilder-
  created, and REPL-only classes. The default for the LSP
  `workspace/symbol` consumer, where the headline win is surfacing
  source-less classes.

Response shape (success):
```
{"id": "...", "status": ["done"],
  "value": {
    "classes": [
      {"name": "Counter",
       "source_file": "/abs/path/examples/counter.bt",
       "line": 1,
       "methods": [
         {"selector": "increment", "class_side": false, "line": 7},
         {"selector": "withInitial:", "class_side": true,  "line": 3}
       ]},
      ...
    ]
  }
}
```

`source_file` is the absolute path recorded in the BEAM module's
`beamtalk_source` attribute, or `null` if the class has no backing
source. `line` is `null` when the runtime has no xref entry for the
class/method — primitives whose source is `nil`, classes still spinning
up before first xref registration.

Classes are sorted by name ascending so the LSP layer can pass the
list straight to the editor; methods within a class follow the
xref-ETS order (`defined_selectors/2` `usort` — alphabetical).

`Behaviour methods` semantics: only locally-defined selectors appear,
not inherited ones. Mirrors `nav-query` `implementors` (which reports
declaring classes only) and `Behaviour methods` (which omits the
inherited tail).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

-doc """
Handle the `nav-symbols` op for the WebSocket transport — encodes the term
result to JSON at the edge (BT-2402).
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for `nav-symbols` (BT-2402, ADR 0085 read-surface).

Returns `{value, #{<<"classes">> => Sorted}}` — the class/method outline is
already a wire-shaped JSON value (see the module doc) — or
`{error, #beamtalk_error{}}` on an invalid `scope`.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"nav-symbols">>, Params, _Msg, _SessionPid) ->
    case validate_scope(maps:get(<<"scope">>, Params, undefined)) of
        {ok, Scope} ->
            ClassPids = safe_all_classes(),
            ClassRows = lists:filtermap(
                fun(Pid) -> class_to_row(Pid, Scope) end,
                ClassPids
            ),
            Sorted = lists:sort(
                fun(A, B) -> maps:get(<<"name">>, A) =< maps:get(<<"name">>, B) end,
                ClassRows
            ),
            {value, #{<<"classes">> => Sorted}};
        {error, Reason} ->
            Err0 = beamtalk_error:new(argument_error, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0, iolist_to_binary([<<"nav-symbols: ">>, Reason])
            ),
            {error, Err1}
    end.

-doc "Advertise the `nav-symbols` op in `describe`.".
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"nav-symbols">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"scope">>]
        }
    }.

%%% ====================================================================
%%% Internal
%%% ====================================================================

-spec validate_scope(term()) -> {ok, all | user} | {error, binary()}.
validate_scope(undefined) ->
    {ok, all};
validate_scope(<<"all">>) ->
    {ok, all};
validate_scope(<<"user">>) ->
    {ok, user};
validate_scope(Other) when is_binary(Other) ->
    {error,
        iolist_to_binary([
            <<"unknown scope `">>, Other, <<"` (expected `user` or `all`)">>
        ])};
validate_scope(_) ->
    {error, <<"`scope` must be a string (`user` or `all`)">>}.

-spec safe_all_classes() -> [pid()].
safe_all_classes() ->
    %% pg may not be running in odd boot states (e.g. the very early
    %% workspace startup). Same defensive shape used by `list-classes`.
    try
        beamtalk_runtime_api:all_classes()
    catch
        _:_ -> []
    end.

-spec class_to_row(pid(), all | user) -> {true, map()} | false.
class_to_row(Pid, Scope) ->
    try
        Name = beamtalk_runtime_api:class_name(Pid),
        ModName = beamtalk_runtime_api:module_name(Pid),
        SourceFile =
            case beamtalk_reflection:source_file_from_module(ModName) of
                nil -> null;
                Path when is_binary(Path) -> Path
            end,
        case {Scope, SourceFile} of
            {user, null} ->
                false;
            _ ->
                Methods = methods_for_class(Name),
                ClassLine = class_header_line(Methods),
                {true, #{
                    <<"name">> => atom_to_binary(Name, utf8),
                    <<"source_file">> => SourceFile,
                    <<"source_origin">> => beamtalk_repl_ops_browse:source_origin_of(ModName, SourceFile),
                    <<"line">> => ClassLine,
                    <<"methods">> => Methods
                }}
        end
    catch
        exit:{noproc, _} ->
            false;
        exit:{timeout, _} ->
            false;
        Class:Reason ->
            ?LOG_WARNING(
                "nav-symbols: skipping class ~p: ~p:~p",
                [Pid, Class, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            false
    end.

-spec methods_for_class(atom()) -> [map()].
methods_for_class(ClassName) ->
    Instance = [
        method_row(ClassName, false, Sel)
     || Sel <- beamtalk_xref:defined_selectors(ClassName, false)
    ],
    ClassSide = [
        method_row(ClassName, true, Sel)
     || Sel <- beamtalk_xref:defined_selectors(ClassName, true)
    ],
    Instance ++ ClassSide.

-spec method_row(atom(), boolean(), atom()) -> map().
method_row(ClassName, ClassSide, Selector) ->
    Line =
        case beamtalk_xref:method_info(ClassName, ClassSide, Selector) of
            #{line := L} when is_integer(L), L > 0 -> L;
            _ -> null
        end,
    #{
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"class_side">> => ClassSide,
        <<"line">> => Line
    }.

-spec class_header_line([map()]) -> pos_integer() | null.
class_header_line(Methods) ->
    %% The xref index records per-method line numbers but not the class
    %% header line. Use the smallest known method line as a best-effort
    %% anchor for the class entry — same behaviour the AST walker
    %% produces for source-backed classes (the class span starts at the
    %% header). If no methods carry a line, the row falls back to `null`
    %% and the LSP consumer renders the class at row 0.
    Lines = [maps:get(<<"line">>, M) || M <- Methods, is_integer(maps:get(<<"line">>, M))],
    case Lines of
        [] -> null;
        _ -> lists:min(Lines)
    end.
