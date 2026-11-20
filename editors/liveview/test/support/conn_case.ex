defmodule BtAttachWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use BtAttachWeb.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  @doc """
  Submit `selector`'s form on `view`, then wait for the off-socket work it
  started (BT-5081: Workspace / REPL evals run in a `start_async` task) and
  return the settled HTML. Use instead of a bare `render_submit/2` whenever the
  assertion that follows depends on the eval's result.
  """
  def submit_and_settle(view, selector, params) do
    view |> Phoenix.LiveViewTest.form(selector) |> Phoenix.LiveViewTest.render_submit(params)
    Phoenix.LiveViewTest.render_async(view, 30_000)
  end

  @doc "`render_hook/3` for an eval event, settled like `submit_and_settle/3`."
  def hook_and_settle(view, event, params) do
    Phoenix.LiveViewTest.render_hook(view, event, params)
    Phoenix.LiveViewTest.render_async(view, 30_000)
  end

  using do
    quote do
      # The default endpoint for testing
      @endpoint BtAttachWeb.Endpoint

      use BtAttachWeb, :verified_routes

      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import BtAttachWeb.ConnCase
    end
  end

  setup _tags do
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
