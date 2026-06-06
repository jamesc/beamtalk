defmodule BtAttach.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      BtAttachWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:bt_attach, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: BtAttach.PubSub},
      # Per-tab workspace-session registry (BT-2410 Wave 4): owns the
      # resume/cleanup lifecycle that lets a reconnecting LiveView re-bind to its
      # tab's existing session and reaps sessions for tabs that close. Started
      # before the Endpoint so it is available to the first connected mount.
      BtAttach.SessionRegistry,
      # Start to serve requests, typically the last entry
      BtAttachWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: BtAttach.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    BtAttachWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
