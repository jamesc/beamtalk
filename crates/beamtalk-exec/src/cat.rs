// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `--cat` mode: a `cat` replacement that flushes after every line.
//!
//! **DDD Context:** Runtime
//!
//! Windows programs fully buffer stdout when it is a pipe, so an interactive
//! test against plain `cat` would never see a reply. `beamtalk-exec --cat`
//! copies stdin to stdout and flushes at each newline.
//!
//! The copy is byte-for-byte (BT-5195): input is not decoded, so non-UTF-8
//! bytes and `\r\n` line endings pass through unchanged, and a last line
//! without a newline is still written. The exit status is 0 at end of input
//! and 1 if reading stdin or writing stdout fails.

use std::io::{self, BufRead, Write};

/// Copy `input` to `output`, flushing after each line.
///
/// # Errors
///
/// Reading `input` or writing `output` fails.
pub(crate) fn copy_lines(mut input: impl BufRead, mut output: impl Write) -> io::Result<()> {
    let mut line = Vec::new();
    loop {
        line.clear();
        if input.read_until(b'\n', &mut line)? == 0 {
            return Ok(());
        }
        output.write_all(&line)?;
        output.flush()?;
    }
}

/// Run `--cat` mode on the process's stdin and stdout, then exit.
pub(crate) fn run() -> ! {
    let code = match copy_lines(io::stdin().lock(), io::stdout().lock()) {
        Ok(()) => 0,
        Err(_) => 1,
    };
    std::process::exit(code);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A writer that records how many times it was flushed.
    #[derive(Default)]
    struct Recorder {
        bytes: Vec<u8>,
        flushes: usize,
    }

    impl Write for Recorder {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.bytes.extend_from_slice(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            self.flushes += 1;
            Ok(())
        }
    }

    #[test]
    fn binary_input_and_crlf_pass_through_unchanged() {
        let input: &[u8] = b"caf\xc3\xa9\r\n\xff\xfe\x00binary\nlast line, no newline";
        let mut out = Recorder::default();
        copy_lines(input, &mut out).expect("copy");
        assert_eq!(out.bytes, input);
        assert_eq!(out.flushes, 3, "one flush per line, including the last");
    }

    #[test]
    fn a_read_error_is_reported() {
        struct Failing;
        impl io::Read for Failing {
            fn read(&mut self, _: &mut [u8]) -> io::Result<usize> {
                Err(io::Error::other("boom"))
            }
        }
        let err = copy_lines(io::BufReader::new(Failing), io::sink()).unwrap_err();
        assert_eq!(err.to_string(), "boom");
    }
}
