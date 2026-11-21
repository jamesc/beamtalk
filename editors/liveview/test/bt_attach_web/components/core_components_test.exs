# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Components.CoreComponentsTest do
  @moduledoc """
  Direct component tests for `BtAttachWeb.CoreComponents` (BT-3294).

  These shared function components (flash banners, icons, JS transitions)
  were previously only rendered incidentally as a side effect of
  whichever LiveView page happened to use them, so most prop/slot/conditional
  branches were never exercised. This suite renders each component directly
  via `Phoenix.LiveViewTest.rendered_to_string/1` (for components with slots)
  or `render_component/2` (for the plain functions), covering the meaningful
  variants of each — no workspace node or browser needed.
  """
  use ExUnit.Case, async: true

  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias BtAttachWeb.CoreComponents, as: CC
  alias Phoenix.LiveView.JS

  # ── flash/1 ────────────────────────────────────────────────────────────

  describe "flash/1" do
    test "renders slot content with info styling and the default id" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash kind={:info} flash={%{}}>Welcome Back!</CC.flash>
        """)

      assert html =~ ~s(id="flash-info")
      assert html =~ "Welcome Back!"
      assert html =~ "bg-emerald-50"
    end

    test "renders a flash-map message with error styling and a title icon" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash kind={:error} flash={%{"error" => "Boom"}} title="Error!" />
        """)

      assert html =~ "Boom"
      assert html =~ "Error!"
      assert html =~ "shadow-md"
      assert html =~ "hero-exclamation-circle-mini"
    end

    test "renders nothing when there is no slot content and no flash message" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash kind={:info} flash={%{}} />
        """)

      assert String.trim(html) == ""
    end

    test "forwards a custom id and arbitrary rest attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash id="custom-flash" kind={:info} flash={%{}} data-test="marker">Hi</CC.flash>
        """)

      assert html =~ ~s(id="custom-flash")
      assert html =~ ~s(data-test="marker")
    end
  end

  # ── flash_group/1 ──────────────────────────────────────────────────────

  describe "flash_group/1" do
    test "composes info/error banners from the flash map plus the always-on reconnect banners" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash_group flash={%{"info" => "Saved!", "error" => "Oops!"}} />
        """)

      assert html =~ "Saved!"
      assert html =~ "Oops!"
      assert html =~ "client-error"
      assert html =~ "server-error"
      assert html =~ "Attempting to reconnect"
      assert html =~ "Hang in there"
    end

    test "accepts a custom container id" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CC.flash_group flash={%{}} id="custom-flash-group" />
        """)

      assert html =~ ~s(id="custom-flash-group")
    end
  end

  # ── icon/1 ─────────────────────────────────────────────────────────────

  describe "icon/1" do
    test "combines the hero- name with a custom class" do
      html = render_component(&CC.icon/1, name: "hero-x-mark-solid", class: "h-5 w-5")

      assert html =~ "hero-x-mark-solid"
      assert html =~ "h-5 w-5"
    end

    test "renders with no class by default" do
      html = render_component(&CC.icon/1, name: "hero-arrow-path")

      assert html =~ "hero-arrow-path"
    end

    test "a non hero- name has no matching clause" do
      # icon/1 only defines a `"hero-" <> _` head — documents the contract
      # that this component is heroicons-only, not a generic icon renderer.
      assert_raise FunctionClauseError, fn ->
        render_component(&CC.icon/1, name: "mdi-close")
      end
    end
  end

  # ── JS command helpers ─────────────────────────────────────────────────

  describe "show/1 and hide/1 JS builders" do
    test "show/1 emits a 300ms show op targeting the selector, with a transition triple" do
      encoded = CC.show("#flash") |> Phoenix.HTML.Safe.to_iodata() |> IO.iodata_to_binary()

      assert encoded =~ ~s(&quot;show&quot;)
      assert encoded =~ ~s(&quot;to&quot;:&quot;#flash&quot;)
      assert encoded =~ ~s(&quot;time&quot;:300)
      assert encoded =~ "duration-300"
      assert encoded =~ "opacity-100"
    end

    test "hide/1 emits a 200ms hide op targeting the selector, with a transition triple" do
      encoded = CC.hide("#flash") |> Phoenix.HTML.Safe.to_iodata() |> IO.iodata_to_binary()

      assert encoded =~ ~s(&quot;hide&quot;)
      assert encoded =~ ~s(&quot;to&quot;:&quot;#flash&quot;)
      assert encoded =~ ~s(&quot;time&quot;:200)
      assert encoded =~ "duration-200"
      assert encoded =~ "opacity-0"
    end

    test "show/2 and hide/2 thread an existing JS struct instead of starting a fresh one" do
      encoded =
        %JS{}
        |> JS.push("noop")
        |> CC.show("#a")
        |> CC.hide("#b")
        |> Phoenix.HTML.Safe.to_iodata()
        |> IO.iodata_to_binary()

      assert encoded =~ ~s(&quot;push&quot;)
      assert encoded =~ ~s(&quot;show&quot;)
      assert encoded =~ ~s(&quot;hide&quot;)
      assert encoded =~ "#a"
      assert encoded =~ "#b"
    end
  end
end
