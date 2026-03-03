# Installation

Beamtalk is distributed as a pre-built binary for Linux, macOS, and Windows,
plus a VS Code extension for the integrated development experience.

## Requirements

- **Erlang/OTP 26+** — the BEAM runtime that Beamtalk compiles to.
  - Linux/macOS: install via your system package manager or [kerl](https://github.com/kerl/kerl).
  - Windows: download from [erlang.org/downloads](https://www.erlang.org/downloads).
- **VS Code 1.85+** — required for the extension (optional for CLI-only use).

## Quick install (Linux and macOS)

The easiest way to install Beamtalk is with the install script:

```
curl -fsSL https://beamtalk.dev/install.sh | sh
```

This downloads the latest release for your platform, extracts it to
`~/.beamtalk`, and adds `~/.beamtalk/bin` to your shell's PATH.

## Manual install from release archive

Download the latest release for your platform from the
[GitHub Releases](https://github.com/jamesc/beamtalk/releases) page.

### Linux

```
beamtalk-<version>-x86_64-unknown-linux-gnu.tar.gz   # x86-64
beamtalk-<version>-aarch64-unknown-linux-gnu.tar.gz   # ARM64
```

Extract and add to your PATH:

```
tar xzf beamtalk-<version>-<platform>.tar.gz
echo 'export PATH="$HOME/beamtalk-<version>/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### macOS

```
beamtalk-<version>-aarch64-apple-darwin.tar.gz   # Apple Silicon
beamtalk-<version>-x86_64-apple-darwin.tar.gz    # Intel
```

Extract and add to your PATH:

```
tar xzf beamtalk-<version>-<platform>.tar.gz
echo 'export PATH="$HOME/beamtalk-<version>/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Windows

```
beamtalk-<version>-x86_64-pc-windows-msvc.zip
```

Extract the zip, then add the `bin\` directory to your `PATH` via
**System Properties → Environment Variables → Path → Edit**,
adding `C:\Users\<you>\beamtalk-<version>\bin`.

### Verify

```
beamtalk --version
```

## VS Code extension

The Beamtalk VS Code extension provides syntax highlighting, an integrated
REPL, live workspace bindings panel, and inline documentation.

1. Open VS Code and go to the **Extensions** panel (`Ctrl+Shift+X`).
2. Search for **Beamtalk** and click **Install**.
3. Reload the window when prompted.

Alternatively, install from the command line:

```
code --install-extension jamesc.beamtalk
```

Once installed, open any `.bt` file to activate the extension.
The status bar will show the Beamtalk workspace connection status.

## Quick start

After installation, start the REPL:

```
beamtalk repl
```

Or open a folder containing `.bt` files in VS Code — the extension will
start a workspace automatically.

See [Language Features](language-features.html) for a tour of the language.
