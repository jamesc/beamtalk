# Scripts

Helper scripts for development workflows.

## Scripts

| Script | Location | Description |
|--------|----------|-------------|
| `compile_fixtures.escript` | `runtime/apps/beamtalk_runtime/test_fixtures/` | Compile Beamtalk test fixtures for runtime tests (portable) |

---

## REPL Port Assignment

### Port assignment

REPL ports are **OS-assigned** (ephemeral port 0) by default (BT-192). The OS picks an available port, eliminating all port conflict risk. The actual port is written to `~/.beamtalk/workspaces/<id>/port` and stored in `node.info` for reconnection.

To override: `beamtalk repl --port 9001` or set `BEAMTALK_REPL_PORT` env var.

### Priority order (highest to lowest)

1. CLI flag: `beamtalk repl --port 9001`
2. Environment variable: `BEAMTALK_REPL_PORT`
3. Application config: `runtime/config/sys.config` (`repl_port`)
4. Default values

---

## Customization

### Adding Project-Specific Environment

Edit `devcontainer.json` to add environment variables via `containerEnv` or `remoteEnv`:

```json
"containerEnv": {
  "MY_VAR": "value"
},
"remoteEnv": {
  "MY_TOKEN": "${localEnv:MY_TOKEN}"
}
```

### Changing the Default Model

Edit `model` in `.devcontainer/copilot-config.json` (for Copilot CLI sessions).
