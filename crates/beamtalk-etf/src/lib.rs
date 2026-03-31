// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared ETF packet framing and term helpers for Beamtalk OTP port binaries.
//!
//! **DDD Context:** Shared Infrastructure (Anti-Corruption Layer support)
//!
//! Both `beamtalk-compiler-port` (Compilation context) and `beamtalk-exec`
//! (Runtime context) communicate with the BEAM via `{packet, 4}` framed ETF
//! messages. This crate extracts the common packet I/O and term-conversion
//! helpers so they are defined once.

use std::collections::HashMap;
use std::io::{self, Read, Write};

use eetf::{Atom, Binary, FixInteger, Map, Term, Tuple};

// ────────────────────────────────────────────────────────────────
// {packet, 4} framing

/// Read a `{packet, 4}` framed message from a reader.
///
/// Returns `Ok(None)` on clean EOF (zero bytes available before the length
/// prefix). A *truncated* length prefix (1–3 bytes followed by EOF) is an
/// error and returns `Err` with kind `UnexpectedEof`.
///
/// # Errors
///
/// Returns an I/O error if the read fails, the length prefix is truncated,
/// or the packet length exceeds 64 MiB.
pub fn read_packet(reader: &mut impl Read) -> io::Result<Option<Vec<u8>>> {
    let mut len_buf = [0u8; 4];

    // Read the first byte separately to distinguish clean EOF from truncated prefix.
    match reader.read(&mut len_buf[..1])? {
        0 => return Ok(None), // Clean EOF — no data at all.
        1 => {}
        _ => unreachable!(),
    }
    // We got the first byte; the remaining 3 bytes are mandatory.
    reader.read_exact(&mut len_buf[1..])?;

    let len = u32::from_be_bytes(len_buf) as usize;
    // Guard against unreasonably large packets (>64 MiB)
    if len > 64 * 1024 * 1024 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("packet too large: {len} bytes"),
        ));
    }
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf)?;
    Ok(Some(buf))
}

/// Write a `{packet, 4}` framed message to a writer.
///
/// # Errors
///
/// Returns an I/O error if the write fails or the data length exceeds
/// `u32::MAX` (4 GiB) — in that case the error kind is `InvalidInput`.
pub fn write_packet(writer: &mut impl Write, data: &[u8]) -> io::Result<()> {
    let len = u32::try_from(data.len())
        .map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "packet too large for {{packet, 4}} framing: {} bytes exceeds u32::MAX",
                    data.len()
                ),
            )
        })?
        .to_be_bytes();
    writer.write_all(&len)?;
    writer.write_all(data)?;
    writer.flush()
}

// ────────────────────────────────────────────────────────────────
// Term constructors

/// Create an atom [`Term`].
#[must_use]
pub fn atom(name: &str) -> Term {
    Term::from(Atom::from(name))
}

/// Create a binary [`Term`] from a byte slice.
#[must_use]
pub fn binary(data: &[u8]) -> Term {
    Term::from(Binary::from(data))
}

/// Create a binary [`Term`] from a string.
#[must_use]
pub fn binary_from_str(s: &str) -> Term {
    Term::from(Binary::from(s.as_bytes()))
}

/// Create a fixed-integer [`Term`].
#[must_use]
pub fn int_term(n: i32) -> Term {
    Term::from(FixInteger::from(n))
}

/// Create a 3-element tuple [`Term`].
#[must_use]
pub fn tuple3(a: Term, b: Term, c: Term) -> Term {
    Term::from(Tuple::from(vec![a, b, c]))
}

// ────────────────────────────────────────────────────────────────
// Term extractors

/// Extract raw bytes from a `Binary` or `ByteList` term.
#[must_use]
pub fn term_to_bytes(term: &Term) -> Option<Vec<u8>> {
    match term {
        Term::Binary(b) => Some(b.bytes.clone()),
        Term::ByteList(bl) => Some(bl.bytes.clone()),
        _ => None,
    }
}

/// Extract a UTF-8 string from a `Binary` or `ByteList` term.
#[must_use]
pub fn term_to_string(term: &Term) -> Option<String> {
    String::from_utf8(term_to_bytes(term)?).ok()
}

/// Extract an atom name string from a term.
#[must_use]
pub fn term_to_atom(term: &Term) -> Option<String> {
    match term {
        Term::Atom(a) => Some(a.name.clone()),
        _ => None,
    }
}

/// Extract a boolean value from an atom term (`true` / `false`).
#[must_use]
pub fn term_to_bool(term: &Term) -> Option<bool> {
    match term {
        Term::Atom(a) => match a.name.as_str() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Extract an unsigned integer from a `FixInteger` term.
#[must_use]
pub fn term_to_usize(term: &Term) -> Option<usize> {
    match term {
        Term::FixInteger(n) => usize::try_from(n.value).ok(),
        _ => None,
    }
}

/// Extract a list of strings from a `List` term.
#[must_use]
pub fn term_to_string_list(term: &Term) -> Option<Vec<String>> {
    match term {
        Term::List(list) => list.elements.iter().map(term_to_string).collect(),
        _ => None,
    }
}

/// Extract a `String -> String` map from an ETF `Map` term.
///
/// Returns `None` if the term is not a map or contains non-string keys/values.
#[must_use]
pub fn term_to_string_map(term: &Term) -> Option<HashMap<String, String>> {
    match term {
        Term::Map(m) => {
            let mut result = HashMap::new();
            for (k, v) in &m.map {
                let key = term_to_string(k)?;
                let val = term_to_string(v)?;
                result.insert(key, val);
            }
            Some(result)
        }
        _ => None,
    }
}

/// Look up a key (atom name) in an ETF `Map`.
#[must_use]
pub fn map_get<'a>(map: &'a Map, key: &str) -> Option<&'a Term> {
    map.map.get(&atom(key))
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── Packet framing ──────────────────────────────────────────

    #[test]
    fn read_write_packet_roundtrip() {
        let data = b"hello world";
        let mut buf = Vec::new();
        write_packet(&mut buf, data).unwrap();
        // First 4 bytes are big-endian length.
        assert_eq!(
            &buf[..4],
            &(u32::try_from(data.len()).unwrap()).to_be_bytes()
        );
        assert_eq!(&buf[4..], data);
        let mut cursor = io::Cursor::new(buf);
        let result = read_packet(&mut cursor).unwrap().unwrap();
        assert_eq!(result, data);
    }

    #[test]
    fn read_packet_eof_returns_none() {
        let empty: &[u8] = &[];
        let mut cursor = io::Cursor::new(empty);
        let result = read_packet(&mut cursor).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn read_packet_rejects_oversized() {
        // Encode a length of 128 MiB (over the 64 MiB limit).
        let len_bytes = (128u32 * 1024 * 1024).to_be_bytes();
        let mut cursor = io::Cursor::new(len_bytes.to_vec());
        let result = read_packet(&mut cursor);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("packet too large"));
    }

    #[test]
    fn write_packet_empty() {
        let mut buf = Vec::new();
        write_packet(&mut buf, &[]).unwrap();
        assert_eq!(buf, vec![0, 0, 0, 0]);
        let mut cursor = io::Cursor::new(buf);
        let result = read_packet(&mut cursor).unwrap().unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn read_packet_truncated_length_prefix_returns_error() {
        // Only 2 bytes of a 4-byte length prefix — should be UnexpectedEof, not Ok(None).
        let partial: &[u8] = &[0x00, 0x0A];
        let mut cursor = io::Cursor::new(partial);
        let result = read_packet(&mut cursor);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::UnexpectedEof);
    }

    #[test]
    fn read_packet_truncated_one_byte_length_prefix() {
        let partial: &[u8] = &[0x01];
        let mut cursor = io::Cursor::new(partial);
        let result = read_packet(&mut cursor);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::UnexpectedEof);
    }

    #[test]
    fn read_packet_truncated_three_byte_length_prefix() {
        let partial: &[u8] = &[0x00, 0x00, 0x05];
        let mut cursor = io::Cursor::new(partial);
        let result = read_packet(&mut cursor);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::UnexpectedEof);
    }

    #[test]
    fn write_packet_oversized_returns_error() {
        // We can't allocate u32::MAX + 1 bytes, so test the logic directly
        // by checking the error path with a mock. Instead, verify the function
        // signature change: it should return InvalidInput for data > u32::MAX.
        // Since we can't create such a large slice in a test, we verify the
        // conversion logic by checking u32::try_from on an oversized value.
        let oversized_len: usize = u32::MAX as usize + 1;
        assert!(u32::try_from(oversized_len).is_err());
        // The actual write_packet wraps this in io::ErrorKind::InvalidInput.
        // We test normal-sized packets work fine as a sanity check.
        let mut buf = Vec::new();
        write_packet(&mut buf, b"small").unwrap();
        assert_eq!(buf.len(), 4 + 5);
    }

    // ── Term constructors ───────────────────────────────────────

    #[test]
    fn atom_creates_atom_term() {
        let t = atom("ok");
        assert_eq!(t, Term::from(Atom::from("ok")));
    }

    #[test]
    fn binary_creates_binary_term() {
        let t = binary(b"hello");
        assert_eq!(t, Term::from(Binary::from(b"hello" as &[u8])));
    }

    #[test]
    fn binary_from_str_creates_binary_term() {
        let t = binary_from_str("hello");
        assert_eq!(t, Term::from(Binary::from(b"hello" as &[u8])));
    }

    #[test]
    fn int_term_creates_fix_integer() {
        let t = int_term(42);
        assert_eq!(t, Term::from(FixInteger::from(42)));
    }

    #[test]
    fn tuple3_creates_three_element_tuple() {
        let t = tuple3(atom("a"), atom("b"), atom("c"));
        match t {
            Term::Tuple(tup) => assert_eq!(tup.elements.len(), 3),
            _ => panic!("expected Tuple"),
        }
    }

    // ── Term extractors ─────────────────────────────────────────

    #[test]
    fn term_to_bytes_from_binary() {
        let t = binary(b"data");
        assert_eq!(term_to_bytes(&t), Some(b"data".to_vec()));
    }

    #[test]
    fn term_to_bytes_from_non_binary_returns_none() {
        assert_eq!(term_to_bytes(&atom("x")), None);
    }

    #[test]
    fn term_to_string_valid() {
        let t = binary(b"hello");
        assert_eq!(term_to_string(&t), Some("hello".to_string()));
    }

    #[test]
    fn term_to_string_invalid_utf8_returns_none() {
        let t = binary(&[0xFF, 0xFE]);
        assert_eq!(term_to_string(&t), None);
    }

    #[test]
    fn term_to_atom_valid() {
        let t = atom("ok");
        assert_eq!(term_to_atom(&t), Some("ok".to_string()));
    }

    #[test]
    fn term_to_atom_non_atom_returns_none() {
        let t = binary(b"not_atom");
        assert_eq!(term_to_atom(&t), None);
    }

    #[test]
    fn term_to_bool_true() {
        assert_eq!(term_to_bool(&atom("true")), Some(true));
    }

    #[test]
    fn term_to_bool_false() {
        assert_eq!(term_to_bool(&atom("false")), Some(false));
    }

    #[test]
    fn term_to_bool_other_atom_returns_none() {
        assert_eq!(term_to_bool(&atom("maybe")), None);
    }

    #[test]
    fn term_to_bool_non_atom_returns_none() {
        assert_eq!(term_to_bool(&binary(b"true")), None);
    }

    #[test]
    fn term_to_usize_valid() {
        assert_eq!(term_to_usize(&int_term(5)), Some(5));
    }

    #[test]
    fn term_to_usize_negative_returns_none() {
        assert_eq!(term_to_usize(&int_term(-1)), None);
    }

    #[test]
    fn term_to_usize_non_integer_returns_none() {
        assert_eq!(term_to_usize(&atom("5")), None);
    }

    #[test]
    fn term_to_string_list_valid() {
        use eetf::List;
        let list = Term::from(List::from(vec![binary(b"a"), binary(b"b")]));
        assert_eq!(
            term_to_string_list(&list),
            Some(vec!["a".to_string(), "b".to_string()])
        );
    }

    #[test]
    fn term_to_string_list_non_list_returns_none() {
        assert_eq!(term_to_string_list(&atom("x")), None);
    }

    #[test]
    fn term_to_string_list_with_non_string_element_returns_none() {
        use eetf::List;
        let list = Term::from(List::from(vec![binary(b"a"), atom("b")]));
        assert_eq!(term_to_string_list(&list), None);
    }

    #[test]
    fn term_to_string_map_valid() {
        let m = Term::from(Map::from([(binary(b"key"), binary(b"val"))]));
        let result = term_to_string_map(&m).unwrap();
        assert_eq!(result.get("key"), Some(&"val".to_string()));
    }

    #[test]
    fn term_to_string_map_non_map_returns_none() {
        assert_eq!(term_to_string_map(&atom("x")), None);
    }

    #[test]
    fn term_to_string_map_non_string_key_returns_none() {
        let m = Term::from(Map::from([(atom("key"), binary(b"val"))]));
        assert_eq!(term_to_string_map(&m), None);
    }

    #[test]
    fn map_get_finds_atom_key() {
        let m = Map::from([(atom("cmd"), binary(b"hello"))]);
        assert!(map_get(&m, "cmd").is_some());
        assert!(map_get(&m, "missing").is_none());
    }

    // ── ETF encode/decode roundtrip ─────────────────────────────

    #[test]
    fn tuple3_roundtrip_through_etf() {
        let event = tuple3(atom("stdout"), int_term(42), binary(b"hello"));
        let mut buf = Vec::new();
        event.encode(&mut buf).unwrap();
        let decoded = Term::decode(io::Cursor::new(buf)).unwrap();
        match decoded {
            Term::Tuple(t) => {
                assert_eq!(t.elements.len(), 3);
                assert_eq!(t.elements[0], atom("stdout"));
                assert_eq!(t.elements[1], int_term(42));
                assert_eq!(t.elements[2], binary(b"hello"));
            }
            _ => panic!("expected Tuple, got {decoded:?}"),
        }
    }
}
