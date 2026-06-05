defmodule BtAttachWeb.Router do
  use BtAttachWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {BtAttachWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", BtAttachWeb do
    pipe_through :browser

    live "/", WorkspaceLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", BtAttachWeb do
  #   pipe_through :api
  # end
end
