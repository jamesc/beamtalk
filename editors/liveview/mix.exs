defmodule BtAttach.MixProject do
  use Mix.Project

  # Single source of truth: the repo-root VERSION file (CLAUDE.md "Versioning &
  # Releases"). The LiveView IDE ships on its own release lane (BT-2512) but
  # tracks the same version as the eval/Transcript surface it attaches to
  # (BT-2394 spike, BT-2514), so it is read here at compile time rather than
  # hand-edited. Artifact-level dev suffixing (`-dev+<sha>` off a release tag)
  # is applied by the packaging lane (BT-2515/BT-2516), matching how the Erlang
  # apps derive theirs via scripts/version.escript. The fallback keeps `mix`
  # usable when the app is built outside the monorepo tree.
  @version (case File.read(Path.expand("../../VERSION", __DIR__)) do
              {:ok, contents} -> String.trim(contents)
              {:error, _} -> "0.0.0"
            end)

  def project do
    [
      app: :bt_attach,
      version: @version,
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      # Warnings-as-errors for our own code only — deps compile with their
      # upstream settings and routinely warn under newer Elixir releases.
      elixirc_options: [warnings_as_errors: true],
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      releases: releases()
    ]
  end

  # Self-contained, ERTS-embedded OTP release of the LiveView IDE (BT-2513).
  #
  # `MIX_ENV=prod mix release` produces `_build/prod/rel/bt_attach` (the
  # Dockerfile builds it there; `just dist-liveview` — used locally and by the
  # CI release lane — redirects it to `dist-liveview/` at the repo root via
  # `--path`). Runnable with `bin/bt_attach start` and no Elixir/Mix on the
  # host. The server is enabled at boot by `PHX_SERVER=true`
  # (config/runtime.exs); OIDC / workspace-attach env (ADR 0091) is read there
  # too, so the release honours the same configuration as `just web-remote`.
  # This is the artifact the packaging lane ships (BT-2515 archive, BT-2516
  # Docker) — the IDE's own release lane, never the core toolchain bundle
  # (BT-2512).
  defp releases do
    [
      bt_attach: [
        include_executables_for: [:unix],
        applications: [bt_attach: :permanent]
      ]
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {BtAttach.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.7.14"},
      {:phoenix_html, "~> 4.1"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      # Pinned to the 1.0.x line: 1.1 swapped the LiveViewTest DOM parser to
      # lazy_html (a native NIF), whereas 1.0.x uses the pure-Elixir floki below.
      {:phoenix_live_view, "~> 1.0.0"},
      {:floki, ">= 0.30.0", only: :test},
      # Browser e2e: PhoenixTest's unified API driving a *real* Chromium via
      # Playwright, so the connected-only JS hooks (CodeEditor, KeyboardShortcuts,
      # SelectionTracker, TweaksPanel) actually run — they are invisible to
      # LiveViewTest's server-side floki render. See test/.../workspace_browser_test.exs.
      {:phoenix_test, "~> 0.11", only: :test, runtime: false},
      {:phoenix_test_playwright, "~> 0.14", only: :test, runtime: false},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.2", runtime: Mix.env() == :dev},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:jason, "~> 1.2"},
      {:dns_cluster, "~> 0.1.1"},
      {:bandit, "~> 1.5"},
      # OIDC authorization-code + PKCE for the authenticated front (ADR 0091
      # Decision 1). Assent is a lightweight, framework-agnostic auth library
      # (no Plug/DB coupling) that does discovery, PKCE and ID-token validation
      # over OTP's built-in :httpc — we don't roll our own crypto (Principle 5).
      {:assent, "~> 0.2"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  #
  # `mix setup` bootstraps the project: fetch deps, install the esbuild/tailwind
  # binaries, then build assets once. See the module docs for `Mix`.
  defp aliases do
    [
      setup: ["deps.get", "assets.setup", "assets.build"],
      # Test .exs files compile at ExUnit runtime, outside elixirc_options —
      # this flag is the only way to hold them to warnings-as-errors.
      test: ["test --warnings-as-errors"],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind bt_attach", "esbuild bt_attach"],
      "assets.deploy": [
        "tailwind bt_attach --minify",
        "esbuild bt_attach --minify",
        "phx.digest"
      ]
    ]
  end
end
