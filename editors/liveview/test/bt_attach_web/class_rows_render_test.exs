# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ClassRowsRenderTest do
  @moduledoc """
  Render tests for the System Browser class-tree rows (BT-2649). Exercises the
  `class_rows/1` function component directly via `render_component/2`, so it runs
  in the bare `mix test` lane (no workspace node, no browser).
  """
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest

  defp row(name, superclass), do: %{"name" => name, "superclass" => superclass}

  defp render_rows(rows) do
    render_component(&BtAttachWeb.WorkspaceLive.class_rows/1,
      rows: rows,
      selected_class: nil,
      browser_side: "instance"
    )
  end

  test "a matching row is interactive: it carries the select click + class value" do
    html = render_rows([{row("ActorSnapshot", "Actor"), 2, false}])

    assert html =~ ~s(phx-click="browser_select_class")
    assert html =~ ~s(phx-value-class="ActorSnapshot")
    assert html =~ "ActorSnapshot"
    refute html =~ ~s(class="row subclass context")
  end

  test "a context ancestor row is dimmed and non-interactive" do
    html = render_rows([{row("Object", nil), 0, true}])

    # Dimmed via the `context` class.
    assert html =~ "context"
    # Non-interactive: no select click, no class value, marked disabled + untabbable.
    refute html =~ ~s(phx-click="browser_select_class")
    refute html =~ ~s(phx-value-class)
    assert html =~ ~s(aria-disabled="true")
    assert html =~ ~s(tabindex="-1")
    # The name is still shown so the spine reads.
    assert html =~ "Object"
  end

  test "a context ancestor carries no origin/runtime badges" do
    cls =
      "Object"
      |> row(nil)
      |> Map.merge(%{"source_origin" => "stdlib", "origin" => "runtime"})

    html = render_rows([{cls, 0, true}])

    refute html =~ "source-origin-tag"
    refute html =~ "runtime-tag"
  end

  test "a matching stdlib row still carries its origin badge (no regression)" do
    cls = Map.put(row("Number", "Object"), "source_origin", "stdlib")

    html = render_rows([{cls, 1, false}])

    assert html =~ "source-origin-tag"
    assert html =~ "stdlib"
  end
end
