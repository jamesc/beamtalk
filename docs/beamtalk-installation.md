# Installation

Beamtalk is distributed as a pre-built tarball for Linux and macOS, plus a
VS Code extension for the integrated development experience.

## Requirements

- **Erlang/OTP 26+** — the BEAM runtime that Beamtalk compiles to.
  Install via your system package manager or [kerl](https://github.com/kerl/kerl).
- **VS Code 1.85+** — required for the extension (optional for CLI-only use).

## Install from release tarball

1. Download the latest release tarball for your platform from the
   [GitHub Releases](https://github.com/jamesc/beamtalk/releases) page:

   ```
   beamtalk-<version>-x86_64-unknown-linux-gnu.tar.gz   # Linux x86-64
   beamtalk-<version>-aarch64-apple-darwin.tar.gz        # macOS Apple Silicon
   beamtalk-<version>-x86_64-apple-darwin.tar.gz         # macOS Intel
   ```

2. Extract and install:

   ```
   tar xzf beamtalk-<version>-<platform>.tar.gz
   sudo mv beamtalk /usr/local/bin/
   ```

3. Verify the installation:

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
