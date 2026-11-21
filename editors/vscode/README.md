# Beamtalk for VS Code

Language support for [Beamtalk](https://github.com/jamesc/beamtalk) — a Smalltalk-inspired language that compiles to the BEAM virtual machine.

## Features

### Editor support
- **Syntax highlighting** — TextMate grammar for `.bt` files
- **Code completions** — Keywords, identifiers, and class hierarchy methods
- **Hover information** — Type and symbol information on hover
- **Go to definition** — Navigate to symbol definitions
- **Find references** — Locate all usages of a symbol
- **Diagnostics** — Inline errors and warnings
- **Document symbols** — Class and method outline

### Workspace explorer

A dedicated **Beamtalk Workspace** panel in the Activity Bar shows the live state of a running Beamtalk workspace:

- **Classes** — all loaded classes, with live actor count badges
- **Methods** — instance and class methods, grouped under each class
  - **Hover** — shows `///` doc comments from source
  - **Go to source** — jump directly to the method declaration
- **State variables** — each class's state fields with default values and inline comments
  - **Hover** — shows default value and `//` comment from the `state:` declaration
  - **Go to source** — jump to the `state:` declaration
- **Actors** — live actor instances with PID and class

Items without source (e.g. REPL-defined classes) show without navigation icons.

## Requirements

The `beamtalk-lsp` language server and the `beamtalk` CLI must be available. The extension finds `beamtalk-lsp` automatically (the Beamtalk install's sysroot first, then `PATH`) and `beamtalk` on `PATH`; use `beamtalk.server.path` / `beamtalk.binary.path` to point at specific binaries. When building from source:

```bash
cargo install --path crates/beamtalk-lsp
```

A running Beamtalk workspace is required for the Workspace explorer panel.

## Configuration

These are the settings the extension contributes (`contributes.configuration` in `package.json`; `just check-surface-drift` fails if this table and `package.json` disagree on names or defaults).

| Setting | Default | Description |
|---------|---------|-------------|
| `beamtalk.binary.path` | `""` | Path to the `beamtalk` CLI binary. Empty uses `PATH`. Set to e.g. `${workspaceFolder}/target/debug/beamtalk` in dev mode so `beamtalk repl` uses the current build |
| `beamtalk.server.path` | `""` | Override path to the `beamtalk-lsp` binary. Empty uses automatic discovery (sysroot, then `PATH`) |
| `beamtalk.stdlibSourceDir` | `""` | Directory containing stdlib `.bt` sources, so *Go to Definition* resolves stdlib classes. Empty auto-discovers from the `beamtalk-lsp` binary's sysroot. Accepts absolute paths (including outside the project), paths relative to the Beamtalk project root (`beamtalk.toml`), and `${workspaceFolder}` |
| `beamtalk.trace.server` | `"off"` | Trace communication between VS Code and the language server: `off`, `messages` or `verbose` |
| `beamtalk.workspace.autoConnect` | `true` | Auto-connect the Workspace Explorer to a running workspace when a workspace port file appears |
| `beamtalk.repl.ephemeral` | `false` | Start the Beamtalk REPL in ephemeral mode (`-e`): bindings are not persisted across sessions |
| `beamtalk.logs.level` | `"warning"` | Minimum level streamed to the Beamtalk Logs output channel (`debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency`) |

### External stdlib sources

If your project keeps stdlib sources outside the project root, set an absolute path:

```json
{
	"beamtalk.stdlibSourceDir": "/opt/beamtalk/stdlib/src"
}
```

If stdlib is inside the project, you can use a project-relative path:

```json
{
	"beamtalk.stdlibSourceDir": "stdlib/src"
}
```

## License

Apache-2.0
