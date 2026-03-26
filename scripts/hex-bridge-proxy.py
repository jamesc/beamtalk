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
from urllib.parse import urlsplit, unquote

LISTEN_PORT = int(os.environ.get("HEX_BRIDGE_PORT", "18081"))
TARGET_HOST = os.environ.get("HEX_BRIDGE_TARGET", "repo.hex.pm")

# Parse upstream proxy from HTTP_PROXY env var using a proper URL parser
# to handle percent-encoded credentials and IPv6 addresses correctly.
_http_proxy = os.environ.get("HTTP_PROXY", os.environ.get("http_proxy", ""))
_parsed_proxy = urlsplit(_http_proxy)
if _parsed_proxy.username:
    _user = unquote(_parsed_proxy.username)
    _pass = unquote(_parsed_proxy.password or "")
    PROXY_AUTH = base64.b64encode(f"{_user}:{_pass}".encode()).decode()
    UPSTREAM_HOST = _parsed_proxy.hostname or ""
    UPSTREAM_PORT = _parsed_proxy.port or 3128
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
    # Parse numeric status code to avoid false matches on other "200" substrings
    try:
        status_code = int(status_line.split(b" ", 2)[1])
    except (IndexError, ValueError):
        status_code = 0
    if status_code != 200:
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


def _dechunk(data):
    """Decode an HTTP chunked transfer-encoded body.

    Handles chunk extensions (e.g. ``1a;foo=bar\\r\\n``) per RFC 7230 §4.1
    by stripping everything after the first ``;`` on the size line.
    """
    result = b""
    while data:
        # Each chunk: hex-size[;ext]\r\n<data>\r\n
        crlf = data.find(b"\r\n")
        if crlf < 0:
            break
        size_field = data[:crlf]
        # Strip optional chunk extensions after ";"
        semi = size_field.find(b";")
        if semi >= 0:
            size_field = size_field[:semi]
        try:
            size = int(size_field.strip(), 16)
        except ValueError:
            break
        if size == 0:
            break
        start = crlf + 2
        result += data[start : start + size]
        data = data[start + size + 2 :]  # skip chunk data + trailing \r\n
    return result


class HexBridgeHandler(http.server.BaseHTTPRequestHandler):
    """Handle incoming HTTP requests and proxy them to hex.pm over HTTPS."""

    def do_GET(self):
        path = self.path
        # Handle absolute URLs (http://host/path?q=1) from HTTP proxy clients
        if path.startswith("http"):
            parsed = urlsplit(path)
            path = parsed.path
            if parsed.query:
                path = f"{path}?{parsed.query}"

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

            # Check if response is chunked and dechunk if so
            is_chunked = False
            for line in header_block.split(b"\r\n")[1:]:
                if b":" in line:
                    k, v = line.split(b":", 1)
                    if k.strip().decode().lower() == "transfer-encoding":
                        if b"chunked" in v.lower():
                            is_chunked = True

            if is_chunked:
                body = _dechunk(body)

            self.send_response(code)
            # Forward headers (skip hop-by-hop and length headers we recompute)
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
