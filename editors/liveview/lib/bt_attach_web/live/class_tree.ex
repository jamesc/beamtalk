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
    by_parent =
      Enum.group_by(classes, fn c ->
        super_name = Map.get(c, "superclass")

        if super_name && Enum.any?(classes, &(Map.get(&1, "name") == super_name)),
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
end
