# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.MethodEditor.TabHeader do
  @moduledoc """
  Method-editor header helpers: the keyboard-shortcut hook attributes, the
  synthetic-tab predicate and the package/origin badge labels. Pure;
  extracted from `BtAttachWeb.Live.MethodEditor` (BT-5112), which delegates
  to them so `WorkspaceLive`'s template keeps its call sites.

  """

  alias BtAttachWeb.Live.MethodEditor.Signature
  alias BtAttachWeb.Live.SystemBrowser

  # BT-2588: the `id="method-editor-form"` hook wiring, shared verbatim
  # across both cond branches that can render that id (the active-tab
  # editor form and the no-tab empty-state form) — and by
  # `id="native-editor-form"`, which binds the identical chord. Splatted
  # via `{method_editor_shortcuts_attrs()}` so these renders cannot drift
  # apart again — the original bug was exactly that drift: the empty-state
  # form lacked `phx-hook`/`data-scope`, so opening a tab from that state
  # added `phx-hook` to an ALREADY-mounted DOM node (same id ⇒ LiveView's
  # morphdom patch treats it as an "updated" node, not an "added" one) and
  # `KeyboardShortcuts#mounted()` — which attaches the window keydown
  # listener — never ran.
  #
  # The empty-state form must still mount the hook (so a later transition
  # to the active-tab form is an "updated" patch on an already-mounted
  # hook, not another missed "added" one) but must NOT bind "mod+s" to
  # "submit" there — otherwise ⌘S anywhere on the page while no tab is
  # open (e.g. while typing in the Workspace eval pane) request-submits the
  # hidden empty form and surfaces a spurious "Enter a class name to save a
  # method." error. Callers pass `%{}` for that form; the active-tab forms
  # use the default. Public: `WorkspaceLive`'s render template calls it
  # directly.
  def method_editor_shortcuts_attrs(shortcuts \\ %{"mod+s" => "submit"}) do
    %{
      "phx-hook" => "KeyboardShortcuts",
      "data-scope" => "window",
      "data-shortcuts" => Jason.encode!(shortcuts)
    }
  end

  # BT-2714: whether the *active editor tab* was opened for a
  # compiler-derived method (its `browse-method-source` came back
  # `source_status: synthetic`). Such a tab has no editable body — it
  # renders read-only (the doc block + a "compiler-derived" note) instead
  # of a blank CodeMirror. Bracket-style pattern match so the many
  # non-method tab kinds (class definition, native module) that never set
  # the key read as `false` rather than raising a KeyError. Public:
  # `WorkspaceLive`'s render template calls it directly.
  def synthetic_tab?(%{synthetic: true}), do: true
  def synthetic_tab?(_), do: false

  # BT-2642: editor-header package/origin badge for the active tab. The tab
  # carries `source_origin` ("stdlib" | "dependency" | "project" | nil) and
  # `package`, snapshotted at open from the class's browse row. The header
  # badge reuses BT-2641's vocabulary (`SystemBrowser.dependency_badge_label/1`,
  # shared with the System Browser tree) and extends it to project (which
  # the tree hides but the header shows): stdlib → "STDLIB", dependency →
  # "DEP · <pkg>" (or bare "DEP"), project → the bare project package name.
  # Returns "" when origin is unknown or a project tab carries no package,
  # so the badge simply does not render. Public: `WorkspaceLive`'s render
  # template calls it directly.
  def header_package_label(%{source_origin: "stdlib"}), do: "STDLIB"

  def header_package_label(%{source_origin: "dependency"} = tab),
    do: SystemBrowser.dependency_badge_label(stringify_origin(tab))

  def header_package_label(%{source_origin: "project", package: pkg})
      when is_binary(pkg) and pkg != "",
      do: pkg

  def header_package_label(_tab), do: ""

  # The CSS modifier class keying the header badge color, per origin.
  # Mirrors `BtAttachWeb.Live.SystemBrowser`'s `source_origin_class/1` but
  # reads the tab's atom-keyed `source_origin`. Public: `WorkspaceLive`'s
  # render template calls it directly.
  def header_origin_class(%{source_origin: "stdlib"}), do: "stdlib"
  def header_origin_class(%{source_origin: "dependency"}), do: "dependency"
  def header_origin_class(_tab), do: "project"

  # The tooltip spelling out the origin for the header badge. Project gets
  # a "Project: <pkg>" title (BT-2642 extends the bare "Project" the tree
  # title used). Public: `WorkspaceLive`'s render template calls it
  # directly.
  def header_origin_title(%{source_origin: "stdlib"}), do: "Standard library"

  def header_origin_title(%{source_origin: "dependency"} = tab),
    do: "Dependency: #{SystemBrowser.package_name(stringify_origin(tab))}"

  def header_origin_title(%{source_origin: "project", package: pkg})
      when is_binary(pkg) and pkg != "",
      do: "Project: #{pkg}"

  def header_origin_title(_tab), do: "Project"

  # Bridge the tab's atom-keyed origin/package onto the string-keyed map the
  # BT-2641 browse-row helpers (`SystemBrowser.dependency_badge_label/1`,
  # `SystemBrowser.package_name/1`) expect, so the dependency badge text
  # stays in one place.
  defp stringify_origin(tab),
    do: %{"source_origin" => tab[:source_origin], "package" => tab[:package]}

  # The Class › side › selector breadcrumb label parts for the active tab.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def breadcrumb(%{kind: :def, class: class}), do: {class, nil, "class definition"}
  # A new-method tab has no stored selector yet (BT-2606): derive it live
  # from the body the author is typing so the breadcrumb names the method
  # as soon as a recognizable signature appears, falling back to the "(new
  # method)" placeholder until then (parenthesised so it can't be mistaken
  # for a real selector — BT-2613).
  def breadcrumb(%{new: true, class: class, side: side, source: source}) do
    case Signature.selector(source) do
      "" -> {class, side, "(new method)"}
      selector -> {class, side, selector}
    end
  end

  def breadcrumb(%{class: class, side: side, selector: selector}), do: {class, side, selector}

  # BT-2605: the colored modifier badges shown in the editor header for the
  # active tab. Each entry is `%{label, class, title}` — `label` is the
  # visible text, `class` the CSS modifier class (`.modifier-tag.<class>`),
  # `title` the accessible tooltip naming the modifier. Returns `[]` when
  # the tab carries no modifiers (an instance-side method on a plain class
  # shows nothing). Order is stable: side (Class) first, then class
  # modifiers (sealed/abstract), then native — mirroring the apidocs'
  # labels (Sealed/Abstract) for docs↔IDE parity. Public: `WorkspaceLive`'s
  # render template calls it directly.
  def modifier_badges(tab) do
    side_badge(tab) ++ class_modifier_badges(tab) ++ native_badge(tab)
  end

  # A class-side method gets a distinct "Class" badge; instance-side
  # methods and class-definition tabs get none here. `:def` tabs have
  # `side: nil`.
  defp side_badge(%{side: "class"}) do
    [%{label: "Class", class: "side", title: "Class-side method"}]
  end

  defp side_badge(_tab), do: []

  # The reflected class-level modifiers (`sealed`, `abstract`) cached on the
  # tab — present on every tab kind (BT-2605 threads it onto method tabs
  # too). Labels are capitalized to match the apidocs.
  defp class_modifier_badges(%{class_modifiers: mods}) when is_list(mods) do
    Enum.map(mods, &class_modifier_badge/1)
  end

  # No modifier list (key absent, or `nil` from a transient fetch failure)
  # → no class-modifier badges.
  defp class_modifier_badges(_tab), do: []

  # The canonical class-modifier badge labels/colors, keyed by the
  # reflected modifier atom. Kept exhaustive against `class_modifiers_from/1`'s
  # atom set so a future modifier is a compile-visible addition in both
  # places.
  defp class_modifier_badge(:sealed),
    do: %{label: "Sealed", class: "sealed", title: "Cannot be subclassed by user code"}

  defp class_modifier_badge(:typed),
    do: %{
      label: "Typed",
      class: "typed",
      title: "All fields and methods require type annotations"
    }

  defp class_modifier_badge(:abstract),
    do: %{
      label: "Abstract",
      class: "abstract",
      title: "Must be subclassed; not directly instantiable"
    }

  # A `native:` class (ADR 0056) gets a Native badge. The flag is
  # `class_native` (set on every tab kind) rather than `native_module` so
  # the badge shows on method tabs too, while `native_module` stays
  # reserved for the :def-only native pane.
  defp native_badge(%{class_native: true}) do
    [%{label: "Native", class: "native", title: "Backed by an Erlang module (native: class)"}]
  end

  defp native_badge(_tab), do: []
end
