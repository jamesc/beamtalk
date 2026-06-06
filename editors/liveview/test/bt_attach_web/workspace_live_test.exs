# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveTest do
  @moduledoc """
  End-to-end proof of the Wave-1 LiveView IDE (BT-2407) through the full Phoenix
  LiveView stack against a *real* running Beamtalk workspace node, on the
  BT-2394 Attach topology. Exercises the BT-2399 term-returning op layer (eval)
  and subscription facade (Transcript), plus session-bound eval state.

  Requires a workspace and its cookie in the environment:

      beamtalk workspace create spike --background --persistent
      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' \\
        ~/.beamtalk/workspaces/spike/vm.args)

  Tag-gated: excluded from the default `mix test` run (see test_helper.exs) and
  run only in the workspace-gated CI job.
  """
  use BtAttachWeb.ConnCase
  import Phoenix.LiveViewTest

  # Excluded by default in test_helper.exs unless BT_WORKSPACE_COOKIE is set.
  @moduletag :workspace

  test "eval round-trip renders the workspace result term", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("form") |> render_submit(%{expr: "3 + 4"})
    assert html =~ "7"
  end

  test "eval state persists across evals within a session", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # Bind a variable, then read it back in a later eval on the same session.
    view |> form("form") |> render_submit(%{expr: "x := 21 * 2"})
    html = view |> form("form") |> render_submit(%{expr: "x"})
    assert html =~ "42"
  end

  test "Transcript output streams live into the LiveView", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    marker = "hello-#{System.unique_integer([:positive])}"
    view |> form("form") |> render_submit(%{expr: ~s|Transcript show: "#{marker}"|})

    # The push is delivered asynchronously over distribution; poll the render.
    assert eventually(fn -> render(view) =~ marker end)
  end

  test "bindings pane reflects an eval that defines a binding (BT-2408)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    name = "bt2408_#{System.unique_integer([:positive])}"

    # Defining a binding fires the BT-2399 `bindings` push; the pane re-reads the
    # read-surface and should list the new name and its value live.
    view |> form("form") |> render_submit(%{expr: "#{name} := 123"})

    assert eventually(fn ->
             html = render(view)
             html =~ name and html =~ "123"
           end)
  end

  test "inspecting an object binding renders its live fields (BT-2408)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "Counter#{suffix}"
    name = "counter_#{suffix}"

    # Define a real Actor subclass with an instance field, then spawn + bind it.
    # The whole class source is one eval (newlines and all). Over distribution the
    # binding is a live {:beamtalk_object, …} handle, not a flattened string, so
    # the bindings pane offers an Inspect button.
    class_src = """
    Actor subclass: #{class}
      state: count = 7

      count => self.count
    """

    view |> form("form") |> render_submit(%{expr: class_src})
    view |> form("form") |> render_submit(%{expr: "#{name} := #{class} spawn"})

    assert eventually(fn -> render(view) =~ name end)

    # Follow the reference through the read-surface `inspect` op; the Inspector
    # reads the live actor state and renders its structured instance fields
    # (the `count` field, initialised to 7).
    html = view |> element("button[phx-value-name='#{name}']") |> render_click()

    assert html =~ "Inspecting"
    assert html =~ "count"
    assert html =~ "7"
  end

  test "method editor: save a method, then an eval observes the new behaviour (BT-2409)", %{
    conn: conn
  } do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "EditCounter#{suffix}"

    # Define a real Actor subclass with a value field and a getter returning it.
    class_src = """
    Actor subclass: #{class}
      state: value = 1

      value => self.value
    """

    view |> form("form") |> render_submit(%{expr: class_src})

    # Sanity: the original method returns the initial field value.
    name = "ec_#{suffix}"
    view |> form("form") |> render_submit(%{expr: "#{name} := #{class} spawn"})
    html = view |> form("form") |> render_submit(%{expr: "#{name} value"})
    assert html =~ "1"

    # Save a NEW body for `value` via the write-surface method editor (ADR 0082).
    # The body is passed as a String value end-to-end — no eval-string escaping.
    save_html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "value",
        "source" => "value => self.value + 100"
      })

    assert save_html =~ "Saved value on #{class}"
    # ChangeLog coherence: the saved (class, selector) appears in the changes pane.
    assert save_html =~ class
    assert save_html =~ "value"

    # A subsequent eval on a freshly-spawned actor observes the patched behaviour
    # (compiled + flushed into the live BEAM module on the workspace node).
    name2 = "ec2_#{suffix}"
    view |> form("form") |> render_submit(%{expr: "#{name2} := #{class} spawn"})

    assert eventually(fn ->
             html = view |> form("form") |> render_submit(%{expr: "#{name2} value"})
             html =~ "101"
           end)
  end

  test "method editor: an invalid edit renders a structured error (BT-2409)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    suffix = System.unique_integer([:positive])
    class = "BadEdit#{suffix}"

    class_src = """
    Actor subclass: #{class}
      state: value = 0

      value => self.value
    """

    view |> form("form") |> render_submit(%{expr: class_src})

    # A syntactically broken body fails to compile; the write-surface returns a
    # structured #beamtalk_error{} which the LiveView renders as an actionable
    # message (NOT a flattened internal tuple/string).
    html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "value",
        "source" => "value => self.value +"
      })

    # The save-result success line must NOT appear; an error message must.
    refute html =~ "Saved value on #{class}"
    # Rendered via render_error/1 — a human-readable message, not a raw {error, …}.
    refute html =~ "{:error,"
    assert html =~ "Could not compile" or html =~ "compile" or html =~ "error"
  end

  test "method editor validates an empty class before any save (BT-2409)", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    html =
      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{"class" => "", "selector" => "value", "source" => "value => 1"})

    assert html =~ "Enter a class name"
  end

  # ~6s total — generous for cross-node async transcript delivery under CI load.
  defp eventually(fun, retries \\ 120) do
    cond do
      fun.() ->
        true

      retries == 0 ->
        false

      true ->
        Process.sleep(50)
        eventually(fun, retries - 1)
    end
  end
end
