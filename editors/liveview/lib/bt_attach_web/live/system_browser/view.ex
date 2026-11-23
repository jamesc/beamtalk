# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.SystemBrowser.View do
  @moduledoc """
  Pure view helpers for the System Browser: class grouping, source filtering,
  method rows and the source-origin badges. Extracted from
  `BtAttachWeb.Live.SystemBrowser` (BT-5112) with no behaviour change;
  `SystemBrowser` delegates to it so every call site is unchanged.

  """

  alias BtAttachWeb.Live.MethodEditor

  # ── System Browser view helpers (BT-2491) ───────────────────────────────────

  # Category: `{category, [class_row]}` groups, each group's classes sorted by
  # name, the groups themselves sorted by category. A class with no category falls
  # into an "(uncategorized)" bucket rather than vanishing.
  #
  # BT-2557: TestCase subclasses (`is_test`) are pulled into a dedicated "Tests"
  # bucket regardless of their package — the browser surfaces them as a category
  # so a project's tests are one click away once loaded.
  #
  # Public: `WorkspaceLive`'s `system_browser_classes/1` render component calls
  # it directly.
  def category_groups(classes) do
    classes
    |> Enum.group_by(&class_category_bucket/1)
    |> Enum.sort_by(fn {category, _} -> category end)
    |> Enum.map(fn {category, rows} ->
      {category, Enum.sort_by(rows, &Map.get(&1, "name"))}
    end)
  end

  defp class_category_bucket(%{"is_test" => true}), do: "Tests"
  # BT-2615: protocol class objects (ADR 0068) declare no package, so they would
  # otherwise land in "(uncategorized)". Group them under a dedicated "Protocols"
  # bucket — mirroring the "Tests" treatment — so a project's protocols are one
  # click away and don't masquerade as uncategorized classes.
  defp class_category_bucket(%{"is_protocol" => true}), do: "Protocols"
  defp class_category_bucket(class), do: Map.get(class, "category") || "(uncategorized)"

  # Narrow class rows by source origin (BT-2557). `source_origin` is the bare
  # classification "project", "stdlib", or "dependency" (BT-2643 — the package
  # name now lives in a separate `package` field). "all" is the identity filter;
  # an unknown filter also passes everything through (fail-open so the tree is
  # never silently blanked). Public: `WorkspaceLive`'s `system_browser_classes/1`
  # render component calls it directly.
  def filter_by_source(classes, "all"), do: classes

  def filter_by_source(classes, "deps") do
    Enum.filter(classes, &(Map.get(&1, "source_origin") == "dependency"))
  end

  def filter_by_source(classes, src) when src in ~w(project stdlib) do
    Enum.filter(classes, &(Map.get(&1, "source_origin") == src))
  end

  def filter_by_source(classes, _src), do: classes

  # True when no class is selected, or the selected class survives the current
  # source filter (BT-2597). Used to decide whether switching filters should
  # clear a now-hidden selection.
  def selected_class_visible?(%{assigns: %{selected_class: nil}}), do: true

  def selected_class_visible?(%{assigns: assigns}) do
    assigns.browser_classes
    |> filter_by_source(assigns.browser_source)
    |> Enum.any?(&(Map.get(&1, "name") == assigns.selected_class))
  end

  # The flat method list for the current protocol filter: all selectors across the
  # protocol tree (filter = nil → "all") or just the selected protocol's, each
  # carrying its protocol name so the row badge / breadcrumb can show it. Sorted
  # by selector for stable order. Public: `WorkspaceLive`'s
  # `system_browser_methods/1` render component calls it directly.
  def filtered_methods(protocols, filter) do
    protocols
    |> Enum.filter(fn p -> filter == nil or Map.get(p, "name") == filter end)
    |> Enum.flat_map(fn p ->
      name = Map.get(p, "name")
      Enum.map(Map.get(p, "selectors", []), &Map.put(&1, "protocol", name))
    end)
    |> Enum.sort_by(&Map.get(&1, "selector"))
  end

  # Total selector count across the protocol tree (the "all" filter row's count).
  # Public: `WorkspaceLive`'s `system_browser_methods/1` render component calls
  # it directly.
  def protocol_method_count(protocols) do
    Enum.reduce(protocols, 0, fn p, acc -> acc + length(Map.get(p, "selectors", [])) end)
  end

  # A row is "runtime-only" (image-diverged, ADR 0096 / BT-2483) when its origin
  # is `runtime` — in the live image with no static/disk source. The class tree
  # and method list badge these so an observer sees what is not on disk. Public:
  # `BtAttachWeb.Live.MethodEditor`'s `method_source_info/4` (BT-3296) and
  # `WorkspaceLive`'s render template call it directly.
  def runtime_only?(%{"origin" => "runtime"}), do: true
  def runtime_only?(_), do: false

  # A selector row is "derived" (BT-2714) when its xref `source_status` is
  # `synthetic` — a compiler-synthesized method (Value accessors, `with<Field>:`
  # setters, keyword constructors, actor `new`/`spawn`) with no hand-written
  # source line. The method list badges these `derived` so the synthetic magic is
  # visible rather than indistinguishable from real methods. Public:
  # `WorkspaceLive`'s `system_browser_methods/1` render component calls it
  # directly.
  def synthetic?(%{"source_status" => "synthetic"}), do: true
  def synthetic?(_), do: false

  # BT-2735: the method-row hover (`title`). When the browse op enriched the row
  # with a `signature` (and, for a compiler-derived method, a resolved `doc`,
  # BT-2714) the hover reads VS Code-style — the rendered signature over the
  # first line of the doc — so it conveys what the method is without a click.
  # Rows the op left unenriched (hand-written selectors, kept off the browse hot
  # path) fall back to the bare selector, the pre-BT-2735 behaviour. Public:
  # `WorkspaceLive`'s `system_browser_methods/1` render component calls it
  # directly.
  def method_row_title(m) do
    case {presence(m["signature"]), first_doc_line(m["doc"])} do
      {nil, nil} -> presence(m["selector"])
      {sig, nil} -> sig
      {nil, doc} -> "#{m["selector"]}\n#{doc}"
      {sig, doc} -> "#{sig}\n#{doc}"
    end
  end

  # The first non-blank line of a method's `///` doc, or nil when the doc is
  # absent/blank. Keeps the hover to a single descriptive line (the full doc is
  # a click away in the read-only pane).
  defp first_doc_line(doc) when is_binary(doc) do
    doc
    |> String.split("\n", parts: 2)
    |> hd()
    |> presence()
  end

  defp first_doc_line(_), do: nil

  # A binary trimmed to nil when blank, itself otherwise — so an empty
  # signature/doc string reads the same as an absent one.
  defp presence(s) when is_binary(s) do
    case String.trim(s) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp presence(_), do: nil

  # Source origin badge helpers (BT-2552, BT-2643, BT-2641). Two orthogonal
  # fields drive the badge: `source_origin` is the classification ("stdlib" |
  # "dependency" | "project") and keys the css class + title; `package` is the
  # package name and feeds the dependency badge label. The dependency badge reads
  # as a "DEP" marker (parity with the generic "STDLIB" marker), suffixed with
  # the package name when known ("DEP · HTTP") and bare ("DEP") otherwise.
  # Public: `WorkspaceLive`'s render template calls these directly.
  def source_origin_class(%{"source_origin" => "stdlib"}), do: "stdlib"
  def source_origin_class(%{"source_origin" => "dependency"}), do: "dependency"
  def source_origin_class(_), do: "project"

  def source_origin_label(%{"source_origin" => "stdlib"}), do: "stdlib"

  def source_origin_label(%{"source_origin" => "dependency"} = row),
    do: dependency_badge_label(row)

  def source_origin_label(_), do: ""

  # Dependency badge text: "DEP · <pkg>" when the package is known, plain "DEP"
  # when it is absent/unknown. This is the badge-specific label; `package_name/1`
  # (which degrades to "unknown") still serves callers that need the raw package.
  # Public: `BtAttachWeb.Live.MethodEditor`'s `header_package_label/1` (BT-3296)
  # calls it directly for the editor-header badge.
  def dependency_badge_label(row) do
    case Map.get(row, "package") do
      pkg when is_binary(pkg) and pkg != "" -> "DEP · #{pkg}"
      _ -> "DEP"
    end
  end

  def source_origin_title(%{"source_origin" => "stdlib"}), do: "Standard library"

  def source_origin_title(%{"source_origin" => "dependency"} = row),
    do: "Dependency: #{package_name(row)}"

  def source_origin_title(_), do: "Project"

  # The package name carried on a browse row's `package` field (BT-2643). Absent /
  # null packages degrade to "unknown" so a dependency badge never renders blank.
  # Public: `BtAttachWeb.Live.MethodEditor`'s `header_origin_title/1` (BT-3296)
  # calls it directly for the editor-header badge.
  def package_name(row) do
    case Map.get(row, "package") do
      pkg when is_binary(pkg) and pkg != "" -> pkg
      _ -> "unknown"
    end
  end

  # The method shown in the focused tab as a `%{class, side, selector}` ref (or nil
  # for a class-definition tab), so the System Browser can highlight the matching
  # method row. Takes bare `assigns` for the render template. An unsaved new-method
  # tab has no real selector yet (`new: true`, `selector: ""`), so it highlights
  # nothing — returning a `selector: ""` ref would never match a row anyway.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def selected_method_ref(assigns) do
    case MethodEditor.active_tab(assigns) do
      %{new: true} ->
        nil

      %{kind: :method, class: class, side: side, selector: selector} ->
        %{class: class, side: side, selector: selector}

      _ ->
        nil
    end
  end

  # The class whose definition tab is focused, so the System Browser's "class
  # definition" entry can track the editor (mirrors `selected_method_ref/1` for
  # method tabs). nil when the active tab is a method or there is no def tab.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def selected_def_ref(assigns) do
    case MethodEditor.active_tab(assigns) do
      %{kind: :def, class: class} -> class
      _ -> nil
    end
  end
end
