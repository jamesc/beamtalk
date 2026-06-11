# Two opt-in suites are excluded from a bare `mix test`:
#
#   * `:workspace` — needs a running Beamtalk workspace node and its cookie
#     (BT_WORKSPACE_COOKIE). Excluded unless that env is present.
#   * `:playwright` — the browser e2e suite drives a real Chromium via Playwright
#     (needs the workspace AND a Playwright install). Opt in with PHX_PLAYWRIGHT=1.
#
# So a plain `mix test` passes with neither a workspace nor a browser — the
# liveview.yml CI lane — while the e2e lane sets both and runs the lot.
workspace? = System.get_env("BT_WORKSPACE_COOKIE") not in [nil, ""]
playwright? = System.get_env("PHX_PLAYWRIGHT") not in [nil, ""]

exclude =
  []
  |> then(&if(workspace?, do: &1, else: [:workspace | &1]))
  |> then(&if(playwright?, do: &1, else: [:playwright | &1]))

ExUnit.start(exclude: exclude)

# Boot the Playwright Node driver + point the test client at the running endpoint
# only when the browser suite is actually selected — otherwise a bare `mix test`
# would try to spawn Node/Chromium that isn't installed.
if playwright? do
  {:ok, _} = PhoenixTest.Playwright.Supervisor.start_link()
  Application.put_env(:phoenix_test, :base_url, BtAttachWeb.Endpoint.url())
end
