# `beamtalk workspace`

Generated from the clap help of the `beamtalk` binary by `scripts/gen_cli_reference.py`;
`just check-cli-reference` fails when this page is out of date. Regenerate with
`just gen-cli-reference`. Workspace state lives under `BEAMTALK_HOME` (default `~/.beamtalk`);
see [environment-variables](development/environment-variables.md).

## `beamtalk workspace`

```text
Manage workspaces (list, stop, status, attach, transcript, logs, create, prune)

Usage: beamtalk workspace [OPTIONS] <COMMAND>

Commands:
  list        List all workspaces
  stop        Stop a running workspace
  status      Show detailed workspace status
  attach      Attach a REPL to a running workspace (without starting one)
  transcript  Stream Transcript output from a running workspace
  logs        View workspace log files
  create      Create a new named workspace
  prune       Delete on-disk state for stopped workspaces
  help        Print this message or the help of the given subcommand(s)

Options:
  -v, --verbose...  Increase logging verbosity (-v: debug, -vv+: trace)
  -h, --help        Print help
```

## `beamtalk workspace list`

```text
List all workspaces

Usage: beamtalk workspace list [OPTIONS]

Options:
      --format <FORMAT>    Output format (`text` or `json` for machine-readable consumption) [default: text]
  -v, --verbose...         Increase logging verbosity (-v: debug, -vv+: trace)
      --status <STATUS>    Only show workspaces with this status (`running` or `stopped`)
      --project <PROJECT>  Only show workspaces whose project path or ID contains this substring (case-insensitive)
  -h, --help               Print help
```

## `beamtalk workspace stop`

```text
Stop a running workspace

Usage: beamtalk workspace stop [OPTIONS] [NAME]

Arguments:
  [NAME]  Workspace name or ID to stop (default: current project's workspace)

Options:
      --force       Force stop without graceful shutdown
  -v, --verbose...  Increase logging verbosity (-v: debug, -vv+: trace)
  -h, --help        Print help
```

## `beamtalk workspace status`

```text
Show detailed workspace status

Usage: beamtalk workspace status [OPTIONS] [NAME]

Arguments:
  [NAME]  Workspace name or ID (default: current project's workspace)

Options:
  -v, --verbose...  Increase logging verbosity (-v: debug, -vv+: trace)
  -h, --help        Print help
```

## `beamtalk workspace attach`

```text
Attach a REPL to a running workspace (without starting one)

Usage: beamtalk workspace attach [OPTIONS] [NAME]

Arguments:
  [NAME]  Workspace name or ID (default: current project's workspace)

Options:
      --port <PORT>      Connect to a workspace at an explicit localhost port
  -v, --verbose...       Increase logging verbosity (-v: debug, -vv+: trace)
      --cookie <COOKIE>  Erlang cookie for authentication (required with --port)
      --no-color         Disable colored output (also respects `NO_COLOR` environment variable)
  -h, --help             Print help
```

## `beamtalk workspace transcript`

```text
Stream Transcript output from a running workspace

Usage: beamtalk workspace transcript [OPTIONS] [NAME]

Arguments:
  [NAME]  Workspace name or ID (default: current project's workspace)

Options:
      --recent <RECENT>  Display last N entries from ring buffer on connect
  -v, --verbose...       Increase logging verbosity (-v: debug, -vv+: trace)
  -h, --help             Print help
```

## `beamtalk workspace logs`

```text
View workspace log files

Shows the most recent workspace's log file. Defaults to the last 50 lines. Use `--follow` to stream new lines, `--level` to filter by severity, or `--path` to print the log file path.

Usage: beamtalk workspace logs [OPTIONS] [NAME]

Arguments:
  [NAME]
          Workspace name or ID (default: the most recent workspace)

Options:
  -f, --follow
          Stream new log lines as they appear (like `tail -f`)

  -v, --verbose...
          Increase logging verbosity (-v: debug, -vv+: trace)

      --level <LEVEL>
          Filter by minimum severity level (debug, info, notice, warning, error)

      --log-format <LOG_FORMAT>
          Format of the log file, used for level filtering
          
          [possible values: text, json]

      --path
          Print the log file path and exit

  -h, --help
          Print help (see a summary with '-h')
```

## `beamtalk workspace create`

```text
Create a new named workspace

Usage: beamtalk workspace create [OPTIONS] <NAME>

Arguments:
  <NAME>  Name for the workspace

Options:
      --background                   Start the workspace node in the background after creation
  -v, --verbose...                   Increase logging verbosity (-v: debug, -vv+: trace)
      --port <PORT>                  TCP port for the workspace WebSocket server (0 = OS-assigned) [default: 0]
      --bind <BIND>                  Network bind address (default: 127.0.0.1)
      --persistent                   Keep workspace running indefinitely (no idle timeout)
      --idle-timeout <IDLE_TIMEOUT>  Max idle seconds before auto-stop (default: 4 hours)
      --confirm-network              Confirm binding to a non-loopback network address
  -h, --help                         Print help
```

## `beamtalk workspace prune`

```text
Delete on-disk state for stopped workspaces

`stop` leaves the workspace directory behind, so stopped entries accumulate in `workspace list`. Prune removes them. Running workspaces and releases are never touched.

Usage: beamtalk workspace prune [OPTIONS]

Options:
      --dry-run
          Show what would be removed without deleting anything

  -v, --verbose...
          Increase logging verbosity (-v: debug, -vv+: trace)

      --project <PROJECT>
          Only prune workspaces whose project path or ID contains this substring (case-insensitive)

  -h, --help
          Print help (see a summary with '-h')
```
