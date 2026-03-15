[![CI](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml/badge.svg)](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml)
[![API Docs](https://img.shields.io/badge/docs-API%20Reference-blue)](https://www.beamtalk.dev/apidocs/)
[![Rust coverage](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/jamesc/beamtalk/badges/rust-coverage.json)](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml)
[![Erlang coverage](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/jamesc/beamtalk/badges/erlang-coverage.json)](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml)

<picture>
  <source srcset="docs/images/beamtalk-logo-dark.svg" media="(prefers-color-scheme: dark)" style="max-width: 480px; width: 100%; height: auto;" />
  <source srcset="docs/images/beamtalk-logo-light.svg" media="(prefers-color-scheme: light)" style="max-width: 480px; width: 100%; height: auto;" />
  <img src="docs/images/beamtalk-logo-light.svg" width="480" />
</picture>

**A live, interactive Smalltalk-like language for the BEAM VM**

Beamtalk brings Smalltalk's legendary live programming experience to Erlang's battle-tested runtime. While inspired by Smalltalk's syntax and philosophy, Beamtalk makes pragmatic choices for modern development (see [Syntax Rationale](docs/beamtalk-syntax-rationale.md)). Write code in a running system, hot-reload modules without restarts, and scale to millions of concurrent actors.

```beamtalk
// Spawn an actor with state
counter := Counter spawn

// Send messages (sync by default)
counter increment
counter increment
value := counter getValue // => 2


// Send an async message
counter increment!  // => ok
// Cascades - multiple messages to same receiver
Transcript show: "Hello"; cr; show: "World"

// Map literals
config := #{#host => "localhost", #port => 8080}
```

---

## Why Beamtalk?

| Feature | Benefit |
|---------|---------|
| **Interactive-first** | REPL and live workspace, not batch compilation |
| **Hot code reload** | Edit and reload modules in running systems |
| **Actor model** | Actors are BEAM processes with independent fault isolation |
| **Reflection** | Inspect any actor's state and methods at runtime |
| **Runs on BEAM** | Compiles to Core Erlang; deploy to existing OTP infrastructure |
| **Testing built-in** | SUnit-style `TestCase` framework with `beamtalk test` |

---

## Key Features

### Actors as Objects

Every Beamtalk actor is a BEAM process with its own state and mailbox:

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1
  getValue => self.value
```

### Pattern Matching

Match expressions with pattern arms:

```beamtalk
status match: [
  #ok -> "success"
  #error -> "failure"
  _ -> "unknown"
]

// Variable binding in patterns
42 match: [n -> n + 1]  // => 43
```

### Collections

Rich collection types written in Beamtalk itself:

```beamtalk
list := #(1, 2, 3)
list collect: [:x | x * 2]    // => #(2, 4, 6)

dict := #{#name => "Alice", #age => 30}
dict at: #name                 // => Alice
```

### Live Code Reloading

Redefine methods on running actors — state is preserved:

```beamtalk
// In the REPL: redefine a method on a running class
Counter >> increment => self.value := self.value + 10

// Existing actors immediately use the new code
c increment  // now adds 10 instead of 1
```
---

## Getting Started

### Install from Release

The quickest way to get started — downloads a prebuilt binary for your platform:

```bash
curl -fsSL https://jamesc.github.io/beamtalk/install.sh | sh
```

This installs to `~/.beamtalk/bin/`. You can customise the location:

```bash
curl -fsSL https://jamesc.github.io/beamtalk/install.sh | sh -s -- --prefix /usr/local
```

**Prerequisite:** [Erlang/OTP 27+](https://www.erlang.org/downloads) must be installed with `erl` on PATH.

<details>
<summary>Installing Erlang/OTP</summary>

**macOS (Homebrew):**
```bash
brew install erlang
```

**Ubuntu/Debian:**
```bash
sudo apt install erlang
```

**Windows:** Download the installer from [erlang.org](https://www.erlang.org/downloads).

**Version manager (any platform):** [asdf](https://asdf-vm.com/) with [asdf-erlang](https://github.com/asdf-vm/asdf-erlang):
```bash
asdf plugin add erlang
asdf install erlang 27.0
asdf global erlang 27.0
```
</details>

After installation, start the REPL:

```bash
beamtalk repl
```

### Build from Source

<details>
<summary>Prerequisites for building from source</summary>

- **Rust** (latest stable) — [rustup.rs](https://rustup.rs/)
- **Erlang/OTP 27+** with `erl` and `erlc` on PATH
- **rebar3** — Erlang build tool ([rebar3.org](https://rebar3.org/))
- **Just** — command runner (`cargo install just`)
- **Node.js LTS** (optional) — only for building the VS Code extension
</details>

```bash
# Clone and build
git clone https://github.com/jamesc/beamtalk.git
cd beamtalk
just build

# Start the REPL
just repl

# Run tests
beamtalk test              # Run BUnit TestCase tests
just test-stdlib           # Run compiled expression tests
just test-e2e              # Run REPL integration tests

# Run full CI checks
just ci
```

#### Install from Source

```bash
# Install to /usr/local (may need sudo)
just install

# Install to a custom prefix
just install PREFIX=$HOME/.local

# Uninstall
just uninstall PREFIX=$HOME/.local
```

The install layout follows the OTP convention (`PREFIX/lib/beamtalk/lib/<app>/ebin/`), so `beamtalk repl` and `beamtalk build` work correctly from any directory when the binary is on `PATH`.

After installing, verify your environment:

```bash
beamtalk doctor
```

### VS Code Extension (Local Dev)

For local extension development (debug LSP, no `.vsix` packaging):

```bash
just build-vscode
```

Then in VS Code, run **Developer: Install Extension from Location...** and select `editors/vscode`.

If your stdlib source files are outside the project root, set:

```json
{
  "beamtalk.stdlib.sourceDir": "/opt/beamtalk/stdlib/lib"
}
```

`beamtalk.stdlib.sourceDir` can be absolute (including outside the project) or relative to the Beamtalk project root (directory containing `beamtalk.toml`). See [editors/vscode/README.md](editors/vscode/README.md) for full extension configuration.

### REPL Usage

**New to Beamtalk?** See the [REPL Tutorial](examples/repl-tutorial.md) for a complete beginner's guide!

```text
Beamtalk v0.1.0
Type :help for available commands, :exit to quit.

> message := "Hello, Beamtalk!"
"Hello, Beamtalk!"

> 2 + 3 * 4
14

> :load "examples/getting-started/src/hello.bt"
nil

> Hello new greeting
│ Hello, World!
a Hello
```

### Load Files

```text
> :load "examples/getting-started/src/counter.bt"
nil
```

---

## Development Setup

### Prerequisites

#### Required Dependencies

- **Docker Desktop** — For devcontainer support  
- **VS Code** — With the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)  
- **Rust** (latest stable) — For building the compiler  
- **Erlang/OTP 27+** — With `erlc` on PATH

#### Optional Dependencies

- **Node.js LTS + npm** — For building the VS Code extension (`editors/vscode/`)

#### Required Environment Variables

| Variable | Purpose | How to Set |
|----------|---------|------------|
| `GH_TOKEN` | GitHub authentication for devcontainers | `gh auth login`, then `$env:GH_TOKEN = (gh auth token)` |
| `LINEAR_API_TOKEN` | Linear issue tracking | Get from [Linear API settings](https://linear.app/settings/api) |

### Quick Start: Basic Development

**VS Code (Recommended):**

1. Clone the repository
2. Open in VS Code: **File → Open Folder**
3. When prompted, click **Reopen in Container**
4. Wait for container build (~5 minutes first time)
5. Open a terminal and run: `just build`
6. Enable the pre-push lint hook:
   ```bash
   git config core.hooksPath .githooks
   ```

The devcontainer includes all dependencies pre-configured:
- Rust toolchain with `clippy`, `rustfmt`, `rust-analyzer`
- Erlang/OTP 27+ and `rebar3`
- Node.js LTS for build tooling
- GitHub CLI (`gh`) with authentication
- GitHub Copilot CLI for AI assistance
- VS Code extensions for Rust, TOML, Erlang, GitHub, Linear

### GitHub Integration

#### Authentication

The devcontainer uses `GH_TOKEN` for GitHub CLI authentication:

```bash
# Verify authentication (inside container)
gh auth status

# Login manually if needed
gh auth login
```

**Priority order:**
1. `GH_TOKEN` environment variable (from host)
2. VS Code credential helper (auto-configured)
3. Manual `gh auth login`

#### Git Configuration

Set your identity for commits:

**Option 1: Environment variables**
```bash
export GIT_USER_NAME="Your Name"
export GIT_USER_EMAIL="you@example.com"
```

**Option 2: Global git config (inside container)**
```bash
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
```

The `postStartCommand` in `devcontainer.json` automatically configures git from environment variables.

### Commit Signing

#### SSH Signing (Recommended)

Configure SSH signing for verified commits:

**1. Generate SSH signing key (on host):**
```bash
ssh-keygen -t ed25519 -C "you@example.com" -f ~/.ssh/id_ed25519_signing
```

**2. Add public key to GitHub:**
```bash
cat ~/.ssh/id_ed25519_signing.pub
# Copy output to GitHub Settings → SSH and GPG keys → New SSH key (select "Signing Key")
```

**3. Configure git (inside container):**
```bash
git config --global gpg.format ssh
git config --global user.signingkey ~/.ssh/id_ed25519_signing
git config --global commit.gpgsign true
```

**4. Add private key to SSH agent:**
```bash
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519_signing
```

### Installed Tools & Extensions

#### Command Line Tools

| Tool | Version | Purpose |
|------|---------|---------|
| `rustc` | Latest stable | Rust compiler |
| `cargo` | Latest | Rust package manager |
| `clippy` | Latest | Rust linter |
| `rustfmt` | Latest | Rust code formatter |
| `just` | Latest | Task runner (alternative to Make) |
| `erlc` | OTP 27+ | Erlang compiler |
| `erl` | OTP 27+ | Erlang runtime |
| `rebar3` | Latest | Erlang build tool |
| `gh` | Latest | GitHub CLI |
| `copilot` | Latest | GitHub Copilot CLI (AI assistant) |
| `node` | LTS | Node.js runtime |
| `npm` | Latest | Node package manager |

#### Cargo Extensions

Pre-installed via `cargo-binstall`:
- `cargo-watch` — Auto-rebuild on file changes
- `cargo-nextest` — Faster test runner
- `cargo-insta` — Snapshot testing
- `cargo-llvm-cov` — Code coverage reports

#### VS Code Extensions

Automatically installed in devcontainer:
- `rust-lang.rust-analyzer` — Rust language server
- `tamasfe.even-better-toml` — TOML file support
- `vadimcn.vscode-lldb` — Debugger for Rust
- `usernamehw.errorlens` — Inline error display
- `erlang-ls.erlang-ls` — Erlang language server
- `github.copilot` — AI pair programming
- `github.vscode-github-actions` — GitHub Actions integration
- `github.vscode-pull-request-github` — PR management
- `linear.linear` — Linear issue tracking
---

## Project Status

**Active development** — the compiler core is working with an interactive REPL.

### What Works Now

- ✅ **REPL** — Interactive evaluation with variable persistence
- ✅ **Lexer & Parser** — Full expression parsing with error recovery
- ✅ **Core Erlang codegen** — Compiles to BEAM bytecode via `erlc`
- ✅ **Actors** — Spawn actors with state, send sync or async messages, futures with `await`
- ✅ **Field assignments** — Actor state mutations via `:=`
- ✅ **Method dispatch** — Full message routing (unary, binary, keyword)
- ✅ **Pattern matching** — `match:` expressions with literal and variable patterns
- ✅ **Hot code reloading** — Redefine classes/methods on running actors via `>>`
- ✅ **Standard library** — Boolean, Block, Integer, Float, String, Character, Collections
- ✅ **Collections** — List, Dictionary, Set, Tuple, Association
- ✅ **Class system** — Inheritance, `super`, `sealed`, class-side methods, abstract classes
- ✅ **Cascades** — Multiple messages to same receiver
- ✅ **Map literals** — `#{key => value}` syntax with Dictionary codegen
- ✅ **LSP** — Language server with completions, hover, go-to-definition, diagnostics
- ✅ **Testing** — SUnit-style `TestCase` framework (`beamtalk test`)

### Planned

- 📋 **Supervision trees** — OTP supervision as language-level constructs
- 📋 **Live browser** — Smalltalk-style class browser (Phoenix `LiveView`)

---

## Documentation

📚 **[Documentation Index](docs/README.md)** — Start here for a guided tour
🌐 **[API Reference](https://www.beamtalk.dev/apidocs/)** — Standard library API docs (auto-generated)
📖 **[Documentation Site](https://jamesc.github.io/beamtalk/)** — Full docs including language features, principles, and architecture

### Core Documents

- [Design Principles](docs/beamtalk-principles.md) — 13 core principles guiding all decisions
- [Language Features](docs/beamtalk-language-features.md) — Syntax, semantics, and examples
- [Syntax Rationale](docs/beamtalk-syntax-rationale.md) — Why we keep/change Smalltalk conventions
- [Object Model](docs/ADR/0005-beam-object-model-pragmatic-hybrid.md) — How Smalltalk objects map to BEAM
- [Known Limitations](docs/known-limitations.md) — What's not yet supported in v0.1

### Architecture

- [Architecture](docs/beamtalk-architecture.md) — Compiler pipeline, runtime, hot reload
- [Testing Strategy](docs/development/testing-strategy.md) — How we verify compiler correctness

### Tooling & Vision

- [Agent-Native Development](docs/beamtalk-agent-native-development.md) — AI agents as developers and live actor systems

---

## Examples & Standard Library

### Examples ([examples/](examples/))

Simple programs demonstrating language features:

```bash
cargo run -- repl
> :load "examples/getting-started/src/hello.bt"
```

### Standard Library ([stdlib/src/](stdlib/src/))

Foundational classes implementing "everything is a message":

| Class | Description |
|-------|-------------|
| `Actor` | Base class for all actors |
| `Block` | First-class closures |
| `True` / `False` | Boolean control flow |
| `Integer` / `Float` | Numeric types |
| `String` / `Character` | UTF-8 text and characters |
| `List` / `Tuple` | Ordered collections |
| `Set` / `Dictionary` | Unordered collections |
| `Nil` | Null object pattern |
| `TestCase` | SUnit-style test framework |

See [stdlib/src/README.md](stdlib/src/README.md) for full documentation.

---

## Repository Structure

```text
beamtalk/
├── crates/
│   ├── beamtalk-core/       # Lexer, parser, AST, codegen
│   ├── beamtalk-cli/        # Command-line interface & REPL
│   └── beamtalk-lsp/        # Language server (LSP)
├── stdlib/src/                # Standard library (.bt files)
├── runtime/                  # Erlang runtime (actors, REPL backend)
├── stdlib/test/               # BUnit test cases (TestCase classes)
├── tests/                    # Stdlib & E2E tests
├── docs/                     # Design documents
├── examples/                 # Example programs
└── editors/vscode/           # VS Code extension
```

The compiler is written in **Rust** and generates **Core Erlang**, which compiles to BEAM bytecode via `erlc`.

---

## Inspiration

Beamtalk combines ideas from:

- **Smalltalk/Newspeak** — Live programming, message-based syntax, reflection (inspiration, not strict compatibility)
- **Erlang/BEAM** — Actors, fault tolerance, hot code reload, distribution
- **Elixir** — Protocols, comprehensions, with blocks
- **Gleam** — Result types, exhaustive pattern matching
- **Dylan** — Sealing, conditions/restarts, method combinations
- **TypeScript** — Compiler-as-language-service architecture

---

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for how to get started — covering dev setup, running tests, PR guidelines, and where to help.

For AI agent contributors, see [AGENTS.md](AGENTS.md) for detailed development guidelines.

We use [Linear](https://linear.app) for issue tracking (project prefix: `BT`).

---

## License

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for details.
