# beamtalk-compiler-port

OTP Port binary for the Beamtalk compiler. Reads ETF-encoded compile requests
from stdin ({packet, 4} framing) and writes ETF-encoded responses to stdout.

See [ADR 0022](../../docs/ADR/0022-embedded-compiler-via-otp-port.md) for architecture details.
