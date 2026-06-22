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

  # Reduce the `{row, indent, context?}` output to `{name, indent, context?}`.
  defp names_ctx(rows), do: Enum.map(rows, fn {c, i, ctx} -> {c["name"], i, ctx} end)

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

  describe "hierarchy_rows_with_context/2 (BT-2649)" do
    test "all classes visible emits zero context rows (= `all` filter, today's behaviour)" do
      classes = [
        row("ActorSnapshot", "Actor"),
        row("Actor", "Object"),
        row("Object", nil)
      ]

      visible = MapSet.new(["ActorSnapshot", "Actor", "Object"])

      assert names_ctx(ClassTree.hierarchy_rows_with_context(classes, visible)) == [
               {"Object", 0, false},
               {"Actor", 1, false},
               {"ActorSnapshot", 2, false}
             ]
    end

    test "keeps the superclass spine as dimmed context rows under a Proj-style filter" do
      # Only the project class matches; its Actor -> Object ancestors live in the
      # full set and must be surfaced as context so the spine stays intact rather
      # than flattening ActorSnapshot to the root.
      classes = [
        row("ActorSnapshot", "Actor"),
        row("Actor", "Object"),
        row("Object", nil)
      ]

      visible = MapSet.new(["ActorSnapshot"])

      assert names_ctx(ClassTree.hierarchy_rows_with_context(classes, visible)) == [
               {"Object", 0, true},
               {"Actor", 1, true},
               {"ActorSnapshot", 2, false}
             ]
    end

    test "includes only the ancestors needed to connect matches, not the whole tree" do
      # Number/Integer are loaded siblings but irrelevant to the visible match, so
      # they are not surfaced as context.
      classes = [
        row("Object", nil),
        row("Actor", "Object"),
        row("ActorSnapshot", "Actor"),
        row("Number", "Object"),
        row("Integer", "Number")
      ]

      visible = MapSet.new(["ActorSnapshot"])

      assert names_ctx(ClassTree.hierarchy_rows_with_context(classes, visible)) == [
               {"Object", 0, true},
               {"Actor", 1, true},
               {"ActorSnapshot", 2, false}
             ]
    end

    test "two matches sharing a spine emit each connecting ancestor once" do
      classes = [
        row("Object", nil),
        row("Actor", "Object"),
        row("ActorSnapshot", "Actor"),
        row("Reactor", "Actor")
      ]

      visible = MapSet.new(["ActorSnapshot", "Reactor"])

      assert names_ctx(ClassTree.hierarchy_rows_with_context(classes, visible)) == [
               {"Object", 0, true},
               {"Actor", 1, true},
               {"ActorSnapshot", 2, false},
               {"Reactor", 2, false}
             ]
    end

    test "a match whose ancestors are absent from the full set renders as a root" do
      # Actor's superclass Object is not in the full set, so no context row exists
      # to synthesise — the matching class is its own root (edge case, no change).
      classes = [
        row("ActorSnapshot", "Actor"),
        row("Actor", "Object")
      ]

      visible = MapSet.new(["Actor"])

      assert names_ctx(ClassTree.hierarchy_rows_with_context(classes, visible)) == [
               {"Actor", 0, false}
             ]
    end

    test "a matching intermediate keeps both an ancestor context row and a descendant context-free" do
      # Actor matches; Object (ancestor) is context, ActorSnapshot (descendant,
      # not matching) is NOT pulled in — only ancestors connect upward.
      classes = [
        row("Object", nil),
        row("Actor", "Object"),
        row("ActorSnapshot", "Actor")
      ]

      visible = MapSet.new(["Actor"])

      assert names_ctx(ClassTree.hierarchy_rows_with_context(classes, visible)) == [
               {"Object", 0, true},
               {"Actor", 1, false}
             ]
    end
  end
end
