# The workspace-attach tests need a running Beamtalk workspace and its cookie
# (BT_WORKSPACE_COOKIE). Exclude them unless that env is present, so a bare
# `mix test` passes without a workspace.
exclude = if System.get_env("BT_WORKSPACE_COOKIE") in [nil, ""], do: [:workspace], else: []
ExUnit.start(exclude: exclude)
