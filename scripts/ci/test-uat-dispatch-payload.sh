#!/usr/bin/env bash
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# Tests scripts/ci/uat-dispatch-payload.sh against a stub `gh`.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "${tmp}"' EXIT
mkdir -p "${tmp}/bin"
H1=$(printf 'a%.0s' {1..64})

cat > "${tmp}/bin/gh" <<STUB
#!/usr/bin/env bash
# The script's own --jq is ignored: emit the raw rows it would produce, sidecars included.
if [ "\$1" = api ]; then
  case "\$2" in
    */releases/tags/*) echo 42 ;;
    */releases/42/assets*)
      printf 'beamtalk-edge-linux-x86_64.tar.gz\tsha256:${H1}\n'
      printf 'beamtalk-edge-linux-x86_64.tar.gz.sha256\tsha256:${H1}\n'
      printf 'beamtalk-vscode-0.6.0.vsix\t\n' ;;
  esac
elif [ "\$1" = release ]; then
  # release download <tag> --pattern <name> --dir <dir> --clobber
  echo "downloading (status line on stdout)"
  printf 'vsix-bytes' > "\$7/\$5"
fi
STUB
chmod +x "${tmp}/bin/gh"
printf 'min-major = 28\nmax-major = 29\n' > "${tmp}/otp.toml"

out="$(PATH="${tmp}/bin:${PATH}" GITHUB_REPOSITORY=o/r OTP_SUPPORT_TOML="${tmp}/otp.toml" \
    "${here}/uat-dispatch-payload.sh" edge 0.6.0-edge+abc edge deadbeef)"
vsix_hash="$(printf 'vsix-bytes' | sha256sum | cut -d' ' -f1)"
expected="$(jq -cn --arg a "${H1}" --arg v "${vsix_hash}" \
  '{channel:"edge",version:"0.6.0-edge+abc",tag:"edge",sha:"deadbeef",
    otp:{"min-major":28,"max-major":29},
    assets:{"beamtalk-edge-linux-x86_64.tar.gz":$a,"beamtalk-vscode-0.6.0.vsix":$v}}')"
if [ "${out}" != "${expected}" ]; then
    echo "FAIL: payload mismatch" >&2
    echo "got:      ${out}" >&2
    echo "expected: ${expected}" >&2
    exit 1
fi
# --otp prints just the window; a toml without it is an error.
[ "$(OTP_SUPPORT_TOML="${tmp}/otp.toml" "${here}/uat-dispatch-payload.sh" --otp)" = '{"min-major":28,"max-major":29}' ] \
    || { echo "FAIL: --otp output" >&2; exit 1; }
printf 'nothing = 1\n' > "${tmp}/bad.toml"
if OTP_SUPPORT_TOML="${tmp}/bad.toml" "${here}/uat-dispatch-payload.sh" --otp 2>/dev/null; then
    echo "FAIL: unparseable toml accepted" >&2
    exit 1
fi
if "${here}/uat-dispatch-payload.sh" bogus v t s 2>/dev/null; then
    echo "FAIL: bad channel accepted" >&2
    exit 1
fi
echo "ok"
