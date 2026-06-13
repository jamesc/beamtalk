# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# LiveView IDE (bt_attach) container image — the turnkey artifact for the
# remote/OIDC deployment (ADR 0091, docs/deployment/remote-liveview-ide.md).
# Ships on the IDE's own release lane (BT-2512), never the core toolchain bundle.
#
# Build context is editors/liveview (the version is injected, not read from the
# repo tree). Build from the repo root with:
#   docker build -f editors/liveview/Dockerfile \
#     --build-arg VERSION="$(cat VERSION)" -t beamtalk-ide editors/liveview
#
# Run (operator supplies the ADR 0091 env; the dist link stays internal):
#   docker run --network host \
#     -e SECRET_KEY_BASE=... -e PHX_HOST=ide.example.com \
#     -e BT_WORKSPACE_NODE=... -e BT_WORKSPACE_COOKIE=... \
#     -e BT_OIDC_ISSUER=... -e BT_OIDC_CLIENT_ID=... -e BT_OIDC_CLIENT_SECRET=... \
#     ghcr.io/jamesc/beamtalk-ide:latest
#
# The only network-facing service is Phoenix's HTTP(S) port. Keep the
# Phoenix<->workspace Erlang distribution link off untrusted networks
# (co-located / private interface) — never expose epmd/dist publicly.

# Tracks the repo-root .tool-versions pins (erlang/elixir). The runtime image
# must stay on the same Debian release as the build image's base (erlang:28 is
# trixie-based) — the release embeds ERTS built against that glibc.
ARG ELIXIR_IMAGE=elixir:1.20.1-otp-28
ARG RUNTIME_IMAGE=debian:trixie-slim

# ─── Build stage ─────────────────────────────────────────────────────────
FROM ${ELIXIR_IMAGE} AS build

ENV MIX_ENV=prod \
    LANG=C.UTF-8

# Node — only the BUILD needs it: esbuild bundles CodeMirror (BT-2538) into
# app.js, after which the release is self-contained (the runtime image below has
# no node). Pin tracks the repo-root .tool-versions; install the official binary
# tarball so the version is exact, not whatever apt ships.
ARG NODE_VERSION=22.22.2
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends curl xz-utils ca-certificates; \
    rm -rf /var/lib/apt/lists/*; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
      amd64) nodearch=x64 ;; \
      arm64) nodearch=arm64 ;; \
      *) echo "unsupported arch: $arch" >&2; exit 1 ;; \
    esac; \
    curl -fsSLo /tmp/node.tar.xz "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-${nodearch}.tar.xz"; \
    tar -xJf /tmp/node.tar.xz -C /usr/local --strip-components=1 --no-same-owner; \
    rm /tmp/node.tar.xz; \
    node --version; npm --version

# bt_attach version tracks the repo VERSION (BT-2514). The build context is the
# editors/liveview dir, so it is injected and written where mix.exs reads it
# (../../VERSION, relative to mix.exs at /src/app/mix.exs).
ARG VERSION=0.0.0
RUN mkdir -p /src && printf '%s' "${VERSION}" > /src/VERSION

WORKDIR /src/app

RUN mix local.hex --force && mix local.rebar --force

# Dependencies first, for layer caching.
COPY mix.exs mix.lock ./
RUN mix deps.get --only prod

# Config is needed to compile deps (Phoenix reads compile-time config).
COPY config ./config
RUN mix deps.compile

# App sources + assets, then bundle (minified + digested) and cut the release.
# Mirrors `just dist-liveview` but stays raw mix: the build context has no
# Justfile, and the split RUN layers above cache deps independently of app
# sources — collapsing into the recipe would rebuild deps on every change.
COPY . ./
RUN mix assets.setup \
 && mix assets.deploy \
 && mix release --overwrite

# ─── Runtime stage ───────────────────────────────────────────────────────
# The release embeds its own ERTS, so the runtime image needs only the C
# libraries OTP links against — no Erlang/Elixir/Mix install.
FROM ${RUNTIME_IMAGE} AS app

ENV LANG=C.UTF-8 \
    PHX_SERVER=true \
    PORT=8443

RUN apt-get update -y \
 && apt-get install -y --no-install-recommends \
      libstdc++6 libncurses6 openssl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Run as a non-root user.
RUN useradd --create-home --uid 1000 app
WORKDIR /app
COPY --from=build --chown=app:app /src/app/_build/prod/rel/bt_attach ./
USER app

EXPOSE 8443

ENTRYPOINT ["/app/bin/bt_attach"]
CMD ["start"]
