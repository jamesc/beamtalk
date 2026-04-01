# Installation

Beamtalk is distributed as a pre-built binary for Linux, macOS, and Windows,
plus a VS Code extension for the integrated development experience.

## Requirements

- **Erlang/OTP 27+** — the BEAM runtime that Beamtalk compiles to.
  - macOS: `brew install erlang`
  - Ubuntu/Debian: `sudo apt install erlang`
  - Windows: download from [erlang.org/downloads](https://www.erlang.org/downloads)
  - Any platform via [asdf](https://asdf-vm.com/): `asdf plugin add erlang && asdf install erlang 27.0`
- **VS Code 1.85+** — required for the extension (optional for CLI-only use).

## Quick install (Linux and macOS)

The easiest way to install Beamtalk is with the install script:

```
curl -fsSL https://beamtalk.dev/install.sh | sh
```

This downloads the latest release for your platform, extracts it to
`~/.beamtalk`, and adds `~/.beamtalk/bin` to your shell's PATH.

To install a specific version or custom location:

```
curl -fsSL https://beamtalk.dev/install.sh | sh -s -- --version v0.3.1
curl -fsSL https://beamtalk.dev/install.sh | sh -s -- --prefix /usr/local
```

## Manual install from release archive

Download the latest release for your platform from the
[GitHub Releases](https://github.com/jamesc/beamtalk/releases) page.

### Linux

```
beamtalk-<version>-linux-x86_64.tar.gz
```

Extract and add to your PATH:

```
tar xzf beamtalk-<version>-linux-x86_64.tar.gz
export PATH="$HOME/beamtalk-<version>/bin:$PATH"
```

### macOS

```
beamtalk-<version>-macos-arm64.tar.gz    # Apple Silicon
beamtalk-<version>-macos-x86_64.tar.gz   # Intel
```

Extract and add to your PATH:

```
tar xzf beamtalk-<version>-macos-<arch>.tar.gz
export PATH="$HOME/beamtalk-<version>/bin:$PATH"
```

### Windows

```
beamtalk-<version>-windows-x86_64.zip
```

Extract the zip, then add the `bin\` directory to your `PATH` via
**System Properties → Environment Variables → Path → Edit**,
adding `C:\Users\<you>\beamtalk-<version>\bin`.

### Verify

```
beamtalk --version
beamtalk doctor
```

The `doctor` command checks all prerequisites (Erlang/OTP version, `erlc`,
runtime, stdlib) and prints actionable fix instructions for any problems.

## VS Code extension

The Beamtalk VS Code extension provides syntax highlighting, an integrated
REPL, live workspace bindings panel, and inline documentation.

1. Open VS Code and go to the **Extensions** panel (`Ctrl+Shift+X`).
2. Search for **Beamtalk** and click **Install**.
3. Reload the window when prompted.

Alternatively, install the `.vsix` from the
[GitHub Releases](https://github.com/jamesc/beamtalk/releases) page:

```
code --install-extension beamtalk-vscode-<version>.vsix
```

Once installed, open any `.bt` file to activate the extension.
The status bar will show the Beamtalk workspace connection status.

## Quick start

Create a new project:

```
beamtalk new my-project
cd my-project
```

This sets up the standard project file structure. Then start the REPL:

```
beamtalk repl
```

Or open the project folder in VS Code — the extension will start a workspace
automatically.

See [Language Features](language-features.html) for a tour of the language.

## Troubleshooting

If `beamtalk repl` or `beamtalk build` fails with cryptic errors, run:

```bash
beamtalk doctor
```

This checks your environment and reports any issues with clear instructions
on how to fix them. Common problems:

- **Wrong OTP version** — Beamtalk requires Erlang/OTP 27 or later.
- **Missing `erlc`** — usually installed alongside `erl`; reinstall Erlang if missing.
- **Missing stdlib (development)** — run `just build` from the repository root.
- **Missing stdlib (installed)** — reinstall Beamtalk via the install script or set `BEAMTALK_RUNTIME_DIR` to point to a valid sysroot.
