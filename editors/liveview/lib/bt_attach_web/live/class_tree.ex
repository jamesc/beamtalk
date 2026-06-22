# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ClassTree do
  @moduledoc """
  Pure layout helpers for the System Browser class tree (ADR 0096, BT-2491).

  Extracted from `BtAttachWeb.WorkspaceLive` so the Hierarchy walk is unit
  testable (BT-2637): it has no LiveView state — it maps the browse-classes rows
  (each a `%{"name" => binary, "superclass" => binary | nil, ...}`) to the
  ordered `{row, indent}` list the template renders.
  """

  @typedoc "A browse-classes row — at minimum `name` and `superclass`."
  @type class_row :: %{optional(String.t()) => term()}

  @doc """
  The Hierarchy view's rows: a flat, ordered list of `{class_row, indent}`
  walking the superclass tree from roots down. `indent` is the **true** depth in
  the superclass chain (BT-2637: previously capped at 2, which collapsed the deep
  kernel chain — ProtoObject → Object → Value → Number → … — onto a single visual
  level and read as flat).

  A class whose superclass is not itself in the browse set is treated as a root
  (indent 0), so an external/kernel superclass doesn't hide its subclasses. Any
  class unreachable from a root — e.g. a member of a superclass cycle in a
  transiently-inconsistent live image, where every member has an in-set
  superclass so none is a root — is appended at indent 0 rather than silently
  dropped from the view (it still renders, just un-nested).
  """
  @spec hierarchy_rows([class_row()]) :: [{class_row(), non_neg_integer()}]
  def hierarchy_rows(classes) do
    names = MapSet.new(classes, &Map.get(&1, "name"))

    by_parent =
      Enum.group_by(classes, fn c ->
        super_name = Map.get(c, "superclass")

        if super_name && MapSet.member?(names, super_name),
          do: super_name,
          else: :__root
      end)

    walked = Enum.reverse(walk_hierarchy(by_parent, :__root, 0, []))
    emitted = MapSet.new(walked, fn {class, _indent} -> Map.get(class, "name") end)
    orphans = for c <- classes, not MapSet.member?(emitted, Map.get(c, "name")), do: {c, 0}
    walked ++ Enum.sort_by(orphans, fn {c, _} -> Map.get(c, "name") end)
  end

  defp walk_hierarchy(by_parent, parent, indent, acc) do
    by_parent
    |> Map.get(parent, [])
    |> Enum.sort_by(&Map.get(&1, "name"))
    |> Enum.reduce(acc, fn class, acc ->
      acc = [{class, indent} | acc]
      walk_hierarchy(by_parent, Map.get(class, "name"), indent + 1, acc)
    end)
  end

  @doc """
  The Hierarchy view's rows under an active source filter (BT-2649), as an
  ordered list of `{class_row, indent, context?}`.

  `all_classes` is the **full** browse set (every loaded class, unfiltered);
  `visible_names` is the `MapSet` of class names that survive the active filter
  (the matching classes). The tree is walked over the *display set* = the visible
  names plus the transitive in-set superclass ancestors needed to connect each
  matching class up to a root. Those connecting ancestors render as **context
  rows** (`context? = true`) — dimmed, non-interactive spine — so the filtered
  view keeps its real shape (e.g. a project `ActorSnapshot` still nests under
  `Actor → Object`) instead of flattening every match to the root.

  A row's `context?` is `true` iff its name is **not** in `visible_names`.
  Indents and ordering match `hierarchy_rows/1` over the display set. A matching
  class whose ancestors are entirely absent from `all_classes` still renders as a
  root (no context rows synthesised — we only ever surface ancestors that already
  exist in the full set, never the whole stdlib tree).
  """
  @spec hierarchy_rows_with_context([class_row()], MapSet.t(String.t())) ::
          [{class_row(), non_neg_integer(), boolean()}]
  def hierarchy_rows_with_context(all_classes, visible_names) do
    super_of =
      Map.new(all_classes, fn c -> {Map.get(c, "name"), Map.get(c, "superclass")} end)

    display_names =
      Enum.reduce(all_classes, MapSet.new(), fn c, acc ->
        name = Map.get(c, "name")

        if MapSet.member?(visible_names, name),
          do: add_ancestors(name, super_of, acc),
          else: acc
      end)

    all_classes
    |> Enum.filter(fn c -> MapSet.member?(display_names, Map.get(c, "name")) end)
    |> hierarchy_rows()
    |> Enum.map(fn {class, indent} ->
      {class, indent, not MapSet.member?(visible_names, Map.get(class, "name"))}
    end)
  end

  # Add `name` and its transitive in-set superclass ancestors to `acc`. Stops at
  # the first ancestor not present in the full set (its superclass is unknown), so
  # we only ever connect matches up through classes that actually exist. Guards
  # against a superclass cycle by not revisiting a name already in `acc`.
  defp add_ancestors(name, super_of, acc) do
    cond do
      name == nil -> acc
      MapSet.member?(acc, name) -> acc
      not Map.has_key?(super_of, name) -> acc
      true -> add_ancestors(Map.get(super_of, name), super_of, MapSet.put(acc, name))
    end
  end
end
