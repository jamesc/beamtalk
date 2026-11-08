// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! FNV-1a hashing shared by codegen (atom-length mangling) and build scripts
//! (spec-mapping stamp). Both consumers need a cheap, deterministic, stable
//! hash without an external crate dependency; placing the single
//! implementation here — a zero-dependency shared leaf — means neither site
//! carries its own copy (architecture-principles.md §6).

/// FNV-1a 64-bit hash.
///
/// Not cryptographic. Used for two purposes:
/// - `beamtalk-codegen`: hashing long selectors that exceed Erlang's 255-char
///   atom limit (`selector_mangler`), producing deterministic `kw_<16-hex>`
///   fallback atoms.
/// - `beamtalk-build`: fingerprinting `beamtalk_spec_reader.erl` as a
///   compile-time stamp, so the FFI type-spec cache is invalidated when the
///   mapping logic changes without depending on `std`'s unstable hasher.
///
/// Standard FNV-1a 64 parameters: offset basis `0xcbf2_9ce4_8422_2325`,
/// prime `0x0000_0100_0000_01b3`.
#[must_use]
pub fn fnv1a_64(data: &[u8]) -> u64 {
    const FNV_OFFSET_BASIS: u64 = 0xcbf2_9ce4_8422_2325;
    const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;
    let mut hash = FNV_OFFSET_BASIS;
    for &byte in data {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_for_same_input() {
        assert_eq!(fnv1a_64(b"hello world"), fnv1a_64(b"hello world"));
    }

    #[test]
    fn differs_for_different_input() {
        assert_ne!(fnv1a_64(b"hello world"), fnv1a_64(b"hello worlds"));
    }

    #[test]
    fn matches_known_fnv1a_64_vector() {
        // Standard FNV-1a 64-bit test vector for the empty string.
        assert_eq!(fnv1a_64(b""), 0xcbf2_9ce4_8422_2325);
    }

    #[test]
    fn matches_known_fnv1a_64_vector_nonempty() {
        // "hello world" per the published FNV test corpus.
        assert_eq!(fnv1a_64(b"hello world"), 0x779a_65e7_023c_d2e7);
    }
}
