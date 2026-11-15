%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_row).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Row-building helpers shared by the REPL navigation and browser ops
(`beamtalk_repl_ops_nav`, `beamtalk_repl_ops_nav_symbols`,
`beamtalk_repl_ops_browse`, `beamtalk_repl_ops_dev`).

Each helper answers a JSON-shaped value (`binary() | null | pos_integer()`)
so the caller can drop it straight into a row map.
""".

-export([
    source_file_of/1,
    class_source_file/1,
    source_origin_of/1,
    method_line/3,
    first_line/1
]).

-doc "Source file path of a compiled module, or `null` when it has none.".
-spec source_file_of(atom()) -> binary() | null.
source_file_of(ModName) ->
    case beamtalk_reflection:source_file_from_module(ModName) of
        nil -> null;
        Path when is_binary(Path) -> Path
    end.

-doc """
Source file path of a *class* (via its backing module), or `null` when the
class is unknown or has no source.
""".
-spec class_source_file(atom()) -> binary() | null.
class_source_file(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            null;
        ClassPid ->
            source_file_of(beamtalk_object_class:module_name_safe(ClassPid))
    end.

-doc """
Source origin badge (`project` / `dependency` / `stdlib`) of a *class*. An
unknown class reads as `project`.
""".
-spec source_origin_of(atom()) -> binary().
source_origin_of(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            <<"project">>;
        ClassPid ->
            ModName = beamtalk_object_class:module_name_safe(ClassPid),
            beamtalk_repl_ops_browse:source_origin_of(ModName, source_file_of(ModName))
    end.

-doc """
Line of a method's header from the xref index, or `null` when the method is
not indexed. A caller that needs a clickable fallback substitutes its own
default (e.g. `1`) for `null`.
""".
-spec method_line(atom(), boolean(), atom()) -> pos_integer() | null.
method_line(ClassName, ClassSide, Selector) ->
    case beamtalk_xref:method_info(ClassName, ClassSide, Selector) of
        #{line := Line} when is_integer(Line), Line > 0 -> Line;
        _ -> null
    end.

-doc "First line of a doc string (`null` passes through).".
-spec first_line(binary() | null) -> binary() | null.
first_line(null) ->
    null;
first_line(Doc) when is_binary(Doc) ->
    case binary:split(Doc, <<"\n">>) of
        [Line | _] -> Line;
        [] -> Doc
    end.
