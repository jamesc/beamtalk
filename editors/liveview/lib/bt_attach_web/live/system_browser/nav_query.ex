# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.SystemBrowser.NavQuery do
  @moduledoc """
  The runtime navigation queries behind the System Browser popovers
  (senders, implementors, protocol and native-module callers). Extracted from
  `BtAttachWeb.Live.SystemBrowser` (BT-5112) with no behaviour change.

  """

  use BtAttachWeb, :html

  alias BtAttach.Facade
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.RequestContext

  # Query senders/implementors of the active method's selector (`nav-query`) and
  # open the result popover. A tab with no selector (a class-definition tab) is a
  # graceful no-op — there is nothing to query. `kind` is `:senders` |
  # `:implementors`; the facade op name matches.
  def run_nav_query(socket, kind) do
    selector =
      case MethodEditor.active_tab(socket.assigns) do
        %{selector: sel} -> sel
        nil -> nil
      end

    run_nav_query_for(socket, kind, selector)
  end

  # Run a senders/implementors `nav-query` for an explicit selector (BT-2639
  # reuses this for the Required-methods → Implementors jump; BT-2495's
  # active-tab path delegates here). A nil/empty selector is a graceful no-op.
  def run_nav_query_for(socket, kind, selector) do
    if is_binary(selector) and selector != "" do
      case Facade.dispatch(kind, %{selector: selector}, RequestContext.build(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: selector, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: selector, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: selector,
              sites: [],
              error: FacadeError.render(reason)
            }
          )

        # Any other shape (version skew, an unexpected reply) degrades to an empty
        # popover with a generic message rather than crashing the LiveView.
        _other ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: selector,
              sites: [],
              error: "Navigation unavailable."
            }
          )
      end
    else
      socket
    end
  end

  # Query a protocol's required methods / conforming classes (`nav-query`
  # `required_methods` / `conforming_classes`) and open the result popover
  # (BT-2639). The protocol equivalent of `run_nav_query/2`; the active tab must
  # be a class-definition tab for a Protocol — otherwise a graceful no-op (the
  # buttons only render for protocols). The popover's `selector` slot carries the
  # protocol name (the popover head shows it next to the kind label, mirroring the
  # method-selector display for senders/implementors). `kind` is
  # `:required_methods` | `:conforming_classes`; the facade op name matches.
  def run_protocol_nav_query(socket, kind) do
    protocol =
      case MethodEditor.active_tab(socket.assigns) do
        %{kind: :def, class: class, is_protocol: true} -> class
        _ -> nil
      end

    if is_binary(protocol) and protocol != "" do
      case Facade.dispatch(kind, %{protocol: protocol}, RequestContext.build(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: protocol, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: protocol, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: protocol,
              sites: [],
              error: FacadeError.render(reason)
            }
          )

        _other ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: protocol,
              sites: [],
              error: "Navigation unavailable."
            }
          )
      end
    else
      socket
    end
  end

  # Query the Beamtalk callers of the focused native tab's module (`nav-query`
  # `callers_of_native_module`, BT-2669) and open the result popover. The active
  # tab must be a `:native` tab — otherwise a graceful no-op (the Callers button
  # only renders on native tabs). The popover's `selector` slot carries the
  # module name (shown next to the "Callers" head, mirroring the selector display
  # for senders/implementors); each row opens the calling method via `nav_open`.
  def run_native_callers_query(socket) do
    module =
      case MethodEditor.active_tab(socket.assigns) do
        %{kind: :native, class: class} -> class
        _ -> nil
      end

    if is_binary(module) and module != "" do
      kind = :callers_of_native_module

      case Facade.dispatch(kind, %{module: module}, RequestContext.build(socket)) do
        {:value, %{"sites" => sites}} when is_list(sites) ->
          assign(socket, nav_popover: %{kind: kind, selector: module, sites: sites})

        {:value, _other} ->
          assign(socket, nav_popover: %{kind: kind, selector: module, sites: []})

        {:error, reason} ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: module,
              sites: [],
              error: FacadeError.render(reason)
            }
          )

        _other ->
          assign(socket,
            nav_popover: %{
              kind: kind,
              selector: module,
              sites: [],
              error: "Navigation unavailable."
            }
          )
      end
    else
      socket
    end
  end
end
