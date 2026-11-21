defmodule BtAttachWeb.CoreComponents do
  @moduledoc """
  Provides the shared UI components the cockpit renders: the flash banners
  (`flash/1`, `flash_group/1`), dismissable status notices (`notice/1`), icons
  (`icon/1`) and the `show/2` / `hide/2` JS transitions the flash banners use.

  The Phoenix generator's modal, form, input, table and list components were
  removed (BT-5092): nothing in the cockpit rendered them.

  Icons are provided by [heroicons](https://heroicons.com). See `icon/1` for usage.
  """
  use Phoenix.Component

  alias Phoenix.LiveView.JS

  @doc """
  Renders flash notices.

  ## Examples

      <.flash kind={:info} flash={@flash} />
      <.flash kind={:info} phx-mounted={show("#flash")}>Welcome Back!</.flash>
  """
  attr :id, :string, doc: "the optional id of flash container"
  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"
  attr :title, :string, default: nil
  attr :kind, :atom, values: [:info, :error], doc: "used for styling and flash lookup"
  attr :rest, :global, doc: "the arbitrary HTML attributes to add to the flash container"

  slot :inner_block, doc: "the optional inner block that renders the flash message"

  def flash(assigns) do
    assigns = assign_new(assigns, :id, fn -> "flash-#{assigns.kind}" end)

    ~H"""
    <div
      :if={msg = render_slot(@inner_block) || Phoenix.Flash.get(@flash, @kind)}
      id={@id}
      phx-click={JS.push("lv:clear-flash", value: %{key: @kind}) |> hide("##{@id}")}
      role="alert"
      class={[
        "fixed top-2 right-2 mr-2 w-80 sm:w-96 z-50 rounded-lg p-3 ring-1",
        @kind == :info && "bg-emerald-50 text-emerald-800 ring-emerald-500 fill-cyan-900",
        @kind == :error && "bg-rose-50 text-rose-900 shadow-md ring-rose-500 fill-rose-900"
      ]}
      {@rest}
    >
      <p :if={@title} class="flex items-center gap-1.5 text-sm font-semibold leading-6">
        <.icon :if={@kind == :info} name="hero-information-circle-mini" class="h-4 w-4" />
        <.icon :if={@kind == :error} name="hero-exclamation-circle-mini" class="h-4 w-4" />
        {@title}
      </p>
      <p class="mt-2 text-sm leading-5">{msg}</p>
      <button type="button" class="group absolute top-1 right-1 p-2" aria-label="close">
        <.icon name="hero-x-mark-solid" class="h-5 w-5 opacity-40 group-hover:opacity-70" />
      </button>
    </div>
    """
  end

  @doc """
  Shows the flash group with standard titles and content.

  ## Examples

      <.flash_group flash={@flash} />
  """
  attr :flash, :map, required: true, doc: "the map of flash messages"
  attr :id, :string, default: "flash-group", doc: "the optional id of flash container"

  def flash_group(assigns) do
    ~H"""
    <div id={@id}>
      <.flash kind={:info} title="Success!" flash={@flash} />
      <.flash kind={:error} title="Error!" flash={@flash} />
      <.flash
        id="client-error"
        kind={:error}
        title="We can't find the internet"
        phx-disconnected={show(".phx-client-error #client-error")}
        phx-connected={hide("#client-error")}
        hidden
      >
        Attempting to reconnect <.icon name="hero-arrow-path" class="ml-1 h-3 w-3 animate-spin" />
      </.flash>

      <.flash
        id="server-error"
        kind={:error}
        title="Something went wrong!"
        phx-disconnected={show(".phx-server-error #server-error")}
        phx-connected={hide("#server-error")}
        hidden
      >
        Hang in there while we get back on track
        <.icon name="hero-arrow-path" class="ml-1 h-3 w-3 animate-spin" />
      </.flash>
    </div>
    """
  end

  @doc """
  Renders a [Heroicon](https://heroicons.com).

  Heroicons come in three styles – outline, solid, and mini.
  By default, the outline style is used, but solid and mini may
  be applied by using the `-solid` and `-mini` suffix.

  You can customize the size and colors of the icons by setting
  width, height, and background color classes.

  Icons are extracted from the `deps/heroicons` directory and bundled within
  your compiled app.css by the plugin in your `assets/tailwind.config.js`.

  ## Examples

      <.icon name="hero-x-mark-solid" />
      <.icon name="hero-arrow-path" class="ml-1 w-3 h-3 animate-spin" />
  """
  attr :name, :string, required: true
  attr :class, :string, default: nil

  def icon(%{name: "hero-" <> _} = assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  ## JS Commands

  def show(js \\ %JS{}, selector) do
    JS.show(js,
      to: selector,
      time: 300,
      transition:
        {"transition-all transform ease-out duration-300",
         "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95",
         "opacity-100 translate-y-0 sm:scale-100"}
    )
  end

  def hide(js \\ %JS{}, selector) do
    JS.hide(js,
      to: selector,
      time: 200,
      transition:
        {"transition-all transform ease-in duration-200",
         "opacity-100 translate-y-0 sm:scale-100",
         "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"}
    )
  end

  # ── dismissable status notices (BT-2612) ────────────────────────────────────
  #
  # A single reusable notice component for every `io-block` status banner
  # (err/ok/warn/plain), shared by `BtAttachWeb.WorkspaceLive` and its
  # extracted panes (e.g. `BtAttachWeb.Live.Inspector`, BT-3291) — moved here
  # (rather than kept as a private component on the LiveView) so an extracted
  # pane module can render it too without a back-reference to the god module
  # (AGENTS.md's shared-leaf-module rule). Each notice carries a
  # keyboard-accessible `×` dismiss control, wired by the *caller* via
  # `dismiss_attrs` — a map of `phx-*` attributes (e.g. `%{"phx-click" =>
  # "dismiss_notice", "phx-value-key" => "git_error"}`) so top-level scalar
  # assigns route through the generic `dismiss_notice` event while
  # nested/per-window notices route through their own targeted events.
  # Variant maps to the `io-block` modifier class.
  attr :variant, :atom, default: :plain, values: [:err, :ok, :warn, :plain]
  attr :message, :string, required: true
  # Preserve newlines in `message` (e.g. a warning with its `Hint:` line).
  attr :multiline, :boolean, default: false
  # Map of phx-* attributes rendered onto the dismiss button. The caller
  # decides which event/values clear the right backing assign.
  attr :dismiss_attrs, :map, required: true

  def notice(assigns) do
    ~H"""
    <div class={["io-block", notice_variant_class(@variant)]}>
      <span class={["io-block-msg", @multiline && "multiline"]}>{@message}</span>
      <button
        type="button"
        class="io-block-dismiss"
        aria-label="Dismiss notification"
        title="Dismiss"
        {@dismiss_attrs}
      >
        ×
      </button>
    </div>
    """
  end

  defp notice_variant_class(:err), do: "err"
  defp notice_variant_class(:ok), do: "ok"
  defp notice_variant_class(:warn), do: "warn"
  defp notice_variant_class(:plain), do: nil
end
