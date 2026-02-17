# Beamtalk for VS Code

Language support for [Beamtalk](https://github.com/jamesc/beamtalk) — a Smalltalk-inspired language that compiles to the BEAM virtual machine.

## Features

- **Syntax highlighting** — TextMate grammar for `.bt` files
- **Code completions** — Keywords, identifiers, and class hierarchy methods
- **Hover information** — Type and symbol information on hover
- **Go to definition** — Navigate to symbol definitions
- **Find references** — Locate all usages of a symbol
- **Diagnostics** — Inline errors and warnings
- **Document symbols** — Class and method outline

## Requirements

The [beamtalk-lsp](https://github.com/jamesc/beamtalk) server must be installed and available on your PATH.

```bash
cargo install --path crates/beamtalk-lsp
```

## Configuration

| Setting | Default | Description |
|---------|---------|-------------|
| `beamtalk.server.path` | `beamtalk-lsp` | Path to the beamtalk-lsp executable |
| `beamtalk.stdlib.sourceDir` | `""` | Optional path to stdlib `.bt` sources for navigation fallback; accepts absolute paths (including outside project) or paths relative to the Beamtalk project root (`beamtalk.toml`) |

### External stdlib sources

If your project keeps stdlib sources outside the project root, set an absolute path:

```json
{
	"beamtalk.stdlib.sourceDir": "/opt/beamtalk/stdlib/lib"
}
```

If stdlib is inside the project, you can use a project-relative path:

```json
{
	"beamtalk.stdlib.sourceDir": "lib"
}
```

## License

Apache-2.0
