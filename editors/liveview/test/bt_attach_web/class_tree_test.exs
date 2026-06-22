# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ClassTreeTest do
  @moduledoc """
  Unit tests for the System Browser Hierarchy layout (BT-2637). Pure functions,
  no workspace node — runs in the bare `mix test` lane.
  """
  use ExUnit.Case, async: true

  alias BtAttachWeb.ClassTree

  # Browse-classes rows carry binary `name`/`superclass` (the Erlang side emits
  # `atom_to_binary`); a root's superclass is `nil` (Erlang `null`).
  defp row(name, superclass), do: %{"name" => name, "superclass" => superclass}

  # Reduce the `{row, indent}` output to the `{name, indent}` pairs we assert on.
  defp names(rows), do: Enum.map(rows, fn {c, i} -> {c["name"], i} end)

  describe "hierarchy_rows/1" do
    test "nests a chain Object -> Actor -> ActorSnapshot at increasing indents" do
      classes = [
        row("ActorSnapshot", "Actor"),
        row("Actor", "Object"),
        row("Object", nil)
      ]

      # Order is roots-down, siblings alphabetical; indent is the true depth.
      assert names(ClassTree.hierarchy_rows(classes)) == [
               {"Object", 0},
               {"Actor", 1},
               {"ActorSnapshot", 2}
             ]
    end

    test "lifts the old indent cap so depth > 2 keeps nesting (BT-2637)" do
      # The real kernel chain is deeper than Object -> X -> Y. Before BT-2637 the
      # indent was capped at 2, collapsing every deep class onto one level (flat).
      classes = [
        row("ProtoObject", nil),
        row("Object", "ProtoObject"),
        row("Value", "Object"),
        row("Number", "Value"),
        row("Integer", "Number")
      ]

      assert names(ClassTree.hierarchy_rows(classes)) == [
               {"ProtoObject", 0},
               {"Object", 1},
               {"Value", 2},
               {"Number", 3},
               {"Integer", 4}
             ]
    end

    test "siblings render at the same indent under a shared superclass" do
      classes = [
        row("Object", nil),
        row("Value", "Object"),
        row("Boolean", "Value"),
        row("Number", "Value")
      ]

      assert names(ClassTree.hierarchy_rows(classes)) == [
               {"Object", 0},
               {"Value", 1},
               # alphabetical among siblings
               {"Boolean", 2},
               {"Number", 2}
             ]
    end

    test "a class whose superclass is outside the browse set is a root (no orphan regression)" do
      # `Object`/`ProtoObject` are not loaded; Actor's superclass is out of set, so
      # Actor is a root and ActorSnapshot still nests beneath it.
      classes = [
        row("ActorSnapshot", "Actor"),
        row("Actor", "Object")
      ]

      assert names(ClassTree.hierarchy_rows(classes)) == [
               {"Actor", 0},
               {"ActorSnapshot", 1}
             ]
    end

    test "every input row is emitted exactly once" do
      classes = [
        row("Object", nil),
        row("Actor", "Object"),
        row("ActorSnapshot", "Actor"),
        row("Number", "Object")
      ]

      rows = ClassTree.hierarchy_rows(classes)
      assert length(rows) == length(classes)
      assert MapSet.new(rows, fn {c, _} -> c["name"] end) == MapSet.new(classes, & &1["name"])
    end

    test "a superclass cycle (no root) still renders every member, un-nested" do
      # A transiently-inconsistent image: A <-> B reference each other, so neither
      # is a root. Both must still appear (at indent 0) rather than vanish.
      classes = [
        row("A", "B"),
        row("B", "A")
      ]

      assert names(ClassTree.hierarchy_rows(classes)) == [{"A", 0}, {"B", 0}]
    end
  end
end
