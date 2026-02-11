# Beamtalk Development Container
# 
# This Dockerfile installs all core development dependencies:
# - Rust compiler and cargo tools
# - Erlang/OTP and rebar3 for BEAM bytecode
# - Node.js for build tooling
# - GitHub CLI (gh)
# - streamlinear CLI for Linear issue management
# - Debugging and networking utilities
#
# GitHub Copilot CLI and workspace setup are handled by devcontainer.json features.

FROM mcr.microsoft.com/devcontainers/rust:1-bookworm

# Install Erlang/OTP, git, and networking/debugging tools
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        erlang rebar3 git gettext-base \
        socat netcat-openbsd lsof \
        curl wget jq htop strace \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

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
    && cargo binstall -y cargo-watch cargo-nextest cargo-insta cargo-llvm-cov just

# Install streamlinear CLI for Linear issue management (used by Copilot skill)
RUN npm install -g streamlinear@github:obra/streamlinear --ignore-scripts

# Verify installations
RUN rustc --version && cargo --version && erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell \
    && node --version && npm --version && gh --version && streamlinear-cli --version 2>/dev/null || true

# Set up Erlang paths
ENV PATH="/usr/lib/erlang/bin:${PATH}"

# Pre-create cargo registry directory with correct ownership
# This avoids needing sudo chown in postCreateCommand
RUN mkdir -p /usr/local/cargo/registry && chown -R vscode:vscode /usr/local/cargo
