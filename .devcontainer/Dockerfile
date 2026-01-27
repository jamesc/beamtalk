# Beamtalk Development Container
# Rust compiler + Erlang/OTP for BEAM bytecode generation

FROM mcr.microsoft.com/devcontainers/rust:1-bookworm

# Install Erlang/OTP from official Erlang Solutions repo
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
        gnupg \
        curl \
        ca-certificates \
    && curl -fsSL https://binaries2.erlang-solutions.com/GPG-KEY-pmanber.asc | gpg --dearmor -o /usr/share/keyrings/erlang-solutions.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/erlang-solutions.gpg] https://binaries2.erlang-solutions.com/debian bookworm contrib" > /etc/apt/sources.list.d/erlang-solutions.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        esl-erlang \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install additional Rust components
RUN rustup component add rustfmt clippy rust-src rust-analyzer

# Install cargo tools for development
RUN cargo install cargo-watch cargo-nextest cargo-insta

# Verify installations
RUN rustc --version && cargo --version && erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

# Set up Erlang paths
ENV PATH="/usr/lib/erlang/bin:${PATH}"

WORKDIR /workspace
