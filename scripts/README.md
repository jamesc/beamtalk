# Scripts

Helper scripts for development workflows.

## Scripts

| Script | Location | Description |
|--------|----------|-------------|
| `compile_fixtures.escript` | `runtime/apps/beamtalk_runtime/test_fixtures/` | Compile Beamtalk test fixtures for runtime tests (portable) |

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
