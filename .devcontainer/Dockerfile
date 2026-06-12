# Beamtalk Development Container
# 
# This Dockerfile installs all core development dependencies:
# - Rust compiler and cargo tools
# - Erlang/OTP for BEAM bytecode
# - Node.js for build tooling
# - GitHub CLI (gh)
# - streamlinear CLI for Linear issue management
# - Debugging and networking utilities
#
# GitHub Copilot CLI and workspace setup are handled by devcontainer.json features.

# trixie: mise's precompiled OTP 28 builds need glibc >= 2.38 (bookworm has
# 2.36), and it matches the Debian base of the shipped IDE image
# (editors/liveview/Dockerfile).
FROM mcr.microsoft.com/devcontainers/rust:1-trixie

# Install git and networking/debugging tools
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        ca-certificates gnupg \
        git gettext-base \
        socat netcat-openbsd lsof unzip \
        curl wget jq htop strace \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Erlang/OTP + Elixir via mise, pinned by the repo-root .tool-versions
# (the same single source of truth dev machines and CI's setup-beam use, so
# the devcontainer toolchain never drifts from what CI compiles with).
# mise fetches precompiled BEAM builds — no source compile.
#
# .tool-versions is copied to / so mise resolves the pins from any directory
# (it walks up from $PWD; the workspace checkout carries the same file).
# MISE_DATA_DIR is world-readable so the vscode user runs the root-installed
# toolchain via the shims on PATH.
ARG MISE_VERSION=v2026.6.3
ENV MISE_DATA_DIR=/usr/local/share/mise
ENV PATH="/usr/local/share/mise/shims:${PATH}"
COPY .tool-versions /.tool-versions
RUN curl -fsSL --retry 5 --retry-connrefused --retry-delay 2 https://mise.run \
        | MISE_VERSION=${MISE_VERSION} MISE_INSTALL_PATH=/usr/local/bin/mise sh \
    && mise install

# Install Node.js (LTS) via NodeSource
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - \
    && apt-get install -y nodejs \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install GitHub CLI
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
    && apt-get update \
    && apt-get install -y gh \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# GitHub Copilot CLI is installed via devcontainer.json feature

# Install additional Rust components
#RUN rustup component add rustfmt clippy rust-src rust-analyzer

# Install cargo tools for development (using cargo-binstall for faster binary installs)
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash \
    && cargo binstall -y cargo-watch cargo-nextest cargo-insta cargo-llvm-cov cargo-fuzz just

# Install streamlinear CLI for Linear issue management (used by Copilot skill)
# npm install -g from a GitHub URL leaves dangling symlinks to a cleaned-up temp dir,
# and npm link fails because the prepare script calls simple-git-hooks.
# Clone to /opt, install mcp deps, and manually symlink the binaries.
RUN git clone --depth 1 https://github.com/obra/streamlinear.git /opt/streamlinear \
    && cd /opt/streamlinear/mcp && npm install --ignore-scripts \
    && NPM_PREFIX="$(npm prefix -g)" \
    && ln -sf /opt/streamlinear "${NPM_PREFIX}/lib/node_modules/streamlinear" \
    && ln -sf /opt/streamlinear/mcp/dist/index.js "${NPM_PREFIX}/bin/streamlinear" \
    && ln -sf /opt/streamlinear/mcp/dist/cli.js "${NPM_PREFIX}/bin/streamlinear-cli" \
    && chmod +x /opt/streamlinear/mcp/dist/index.js /opt/streamlinear/mcp/dist/cli.js

# Install Sourcegraph Amp CLI (as vscode user so it lands in the right home dir)
USER vscode
RUN curl -fsSL https://ampcode.com/install.sh | bash
USER root
RUN echo 'export PATH="/home/vscode/.amp/bin:/home/vscode/.local/bin:$PATH"' > /etc/profile.d/amp.sh \
    && chmod +x /etc/profile.d/amp.sh
ENV PATH="/home/vscode/.amp/bin:/home/vscode/.local/bin:${PATH}"

# Erlang, Elixir, and rebar3 all come from mise (see the .tool-versions block
# above — never the distro `elixir` package, which pins an old erlang).
# Install Hex + rebar3 for the vscode remoteUser so `mix deps.get` works out
# of the box.
USER vscode
RUN ELIXIR_ERL_OPTIONS="+fnu" mix local.hex --force \
    && ELIXIR_ERL_OPTIONS="+fnu" mix local.rebar rebar3 /usr/local/share/mise/shims/rebar3 --force
USER root

# Verify installations
RUN rustc --version && cargo --version && erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell \
    && ELIXIR_ERL_OPTIONS="+fnu" elixir --version && ELIXIR_ERL_OPTIONS="+fnu" mix --version \
    && node --version && npm --version && gh --version && (streamlinear-cli --version 2>/dev/null || true) && amp --version \
    && rebar3 --version

# Pre-create cargo registry directory with correct ownership
# This avoids needing sudo chown in postCreateCommand
RUN mkdir -p /usr/local/cargo/registry && chown -R vscode:vscode /usr/local/cargo
