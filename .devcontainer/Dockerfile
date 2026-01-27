# Beamtalk Development Container
# Rust compiler + Erlang/OTP for BEAM bytecode generation

FROM mcr.microsoft.com/devcontainers/rust:1-bookworm

# Install Erlang/OTP from Debian packages (OTP 25 on bookworm)
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        erlang \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install additional Rust components
RUN rustup component add rustfmt clippy rust-src rust-analyzer

# Install cargo tools for development (using cargo-binstall for faster binary installs)
RUN curl -L --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/cargo-bins/cargo-binstall/main/install-from-binstall-release.sh | bash \
    && cargo binstall -y cargo-watch cargo-nextest cargo-insta

# Verify installations
RUN rustc --version && cargo --version && erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

# Set up Erlang paths
ENV PATH="/usr/lib/erlang/bin:${PATH}"

WORKDIR /workspace
