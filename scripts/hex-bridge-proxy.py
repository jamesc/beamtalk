#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

"""HTTP-to-HTTPS bridge proxy for hex.pm in cloud environments.

Erlang's httpc module cannot negotiate TLS through egress proxies that
perform TLS interception (MITM).  This script bridges the gap:

  Erlang (plain HTTP) -> hex-bridge (localhost:18081) -> upstream proxy -> hex.pm (HTTPS)

The bridge handles TLS itself using Python's ssl module (which works with
MITM proxies), so Erlang only needs to make plain HTTP requests.

Usage:
  python3 scripts/hex-bridge-proxy.py &

Then configure rebar3 via ~/.config/rebar3/rebar.config:
  {hex, [{repos, [
      #{name => <<"hexpm">>,
        repo_url => <<"http://127.0.0.1:18081">>,
        repo_verify => false,
        repo_verify_origin => false}
  ]}]}.

Environment variables:
  HEX_BRIDGE_PORT      - listen port (default: 18081)
  HEX_BRIDGE_TARGET    - target host (default: repo.hex.pm)
  HTTP_PROXY           - upstream proxy URL with credentials
"""

import base64
import http.server
import os
import socket
import ssl
import sys

LISTEN_PORT = int(os.environ.get("HEX_BRIDGE_PORT", "18081"))
TARGET_HOST = os.environ.get("HEX_BRIDGE_TARGET", "repo.hex.pm")

# Parse upstream proxy from HTTP_PROXY env var
_http_proxy = os.environ.get("HTTP_PROXY", os.environ.get("http_proxy", ""))
if "@" in _http_proxy:
    _prefix = _http_proxy.split("://", 1)[1] if "://" in _http_proxy else _http_proxy
    _userinfo, _hostport = _prefix.rsplit("@", 1)
    PROXY_AUTH = base64.b64encode(_userinfo.encode()).decode()
    _host_port = _hostport.split(":")
    UPSTREAM_HOST = _host_port[0]
    UPSTREAM_PORT = int(_host_port[1]) if len(_host_port) > 1 else 3128
else:
    PROXY_AUTH = ""
    UPSTREAM_HOST = ""
    UPSTREAM_PORT = 0


def https_get(path):
    """Perform HTTPS GET through the upstream proxy's CONNECT tunnel."""
    sock = socket.create_connection((UPSTREAM_HOST, UPSTREAM_PORT), timeout=30)
    # Establish CONNECT tunnel with proxy auth
    connect_req = (
        f"CONNECT {TARGET_HOST}:443 HTTP/1.1\r\n"
        f"Host: {TARGET_HOST}:443\r\n"
        f"Proxy-Authorization: Basic {PROXY_AUTH}\r\n"
        f"\r\n"
    )
    sock.sendall(connect_req.encode())
    resp = b""
    while b"\r\n\r\n" not in resp:
        chunk = sock.recv(4096)
        if not chunk:
            raise ConnectionError("Upstream proxy closed connection")
        resp += chunk
    status_line = resp.split(b"\r\n", 1)[0]
    if b"200" not in status_line:
        raise ConnectionError(f"CONNECT failed: {status_line.decode()}")

    # Wrap in TLS (verify_mode=NONE to accept MITM certs)
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    tls = ctx.wrap_socket(sock, server_hostname=TARGET_HOST)

    # Send HTTP/1.1 request inside the tunnel
    http_req = (
        f"GET {path} HTTP/1.1\r\n"
        f"Host: {TARGET_HOST}\r\n"
        f"User-Agent: rebar3/hex-bridge\r\n"
        f"Accept: */*\r\n"
        f"Connection: close\r\n"
        f"\r\n"
    )
    tls.sendall(http_req.encode())

    # Read full response
    data = b""
    while True:
        try:
            chunk = tls.recv(65536)
            if not chunk:
                break
            data += chunk
        except ssl.SSLError:
            break
    tls.close()
    return data


class HexBridgeHandler(http.server.BaseHTTPRequestHandler):
    """Handle incoming HTTP requests and proxy them to hex.pm over HTTPS."""

    def do_GET(self):
        path = self.path
        # Handle absolute URLs (http://host/path) from HTTP proxy clients
        if path.startswith("http"):
            from urllib.parse import urlparse
            path = urlparse(path).path

        try:
            raw = https_get(path)
            header_end = raw.find(b"\r\n\r\n")
            if header_end < 0:
                self.send_error(502, "No response from upstream")
                return

            header_block = raw[:header_end]
            body = raw[header_end + 4:]

            # Parse status code
            status_line = header_block.split(b"\r\n", 1)[0]
            code = int(status_line.split(b" ", 2)[1])

            self.send_response(code)
            # Forward headers (skip hop-by-hop headers)
            for line in header_block.split(b"\r\n")[1:]:
                if b":" in line:
                    k, v = line.split(b":", 1)
                    key_lower = k.strip().decode().lower()
                    if key_lower not in (
                        "transfer-encoding",
                        "connection",
                        "content-length",
                    ):
                        self.send_header(k.strip().decode(), v.strip().decode())
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        except Exception as e:
            sys.stderr.write(f"[hex-bridge] Error proxying {path}: {e}\n")
            sys.stderr.flush()
            self.send_error(502, str(e))

    def log_message(self, fmt, *args):
        sys.stderr.write(f"[hex-bridge] {fmt % args}\n")
        sys.stderr.flush()


def main():
    if not UPSTREAM_HOST:
        print("No HTTP_PROXY set — hex-bridge not needed, exiting.", flush=True)
        sys.exit(0)

    server = http.server.HTTPServer(("127.0.0.1", LISTEN_PORT), HexBridgeHandler)
    print(
        f"hex-bridge listening on 127.0.0.1:{LISTEN_PORT} -> {TARGET_HOST} "
        f"(via {UPSTREAM_HOST}:{UPSTREAM_PORT})",
        flush=True,
    )
    server.serve_forever()


if __name__ == "__main__":
    main()
