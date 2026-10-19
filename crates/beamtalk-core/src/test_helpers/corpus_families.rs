// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3888: one `#[test]` per family of a shared corpus list, so nextest can
//! spread a corpus proof over cores instead of running its whole product on
//! one (the corpus tier's wall time is floored by its longest test).
//!
//! A corpus proof enumerates a list (`FRAMES`, `ARM_FRAMES`, `VT_SHAPES`,
//! ...) against the rest of its product. [`per_family_tests!`] splits the
//! list into index ranges, the families, and gives each one a test calling
//! the proof on that slice; [`assert_families_partition_the_list`] proves,
//! from a test the macro also emits, that the families partition the list
//! and that every family has exactly one test. The split is a partition of
//! the former single loop, never a reduction of it: the union of the
//! per-family tests is the whole proof.
//!
//! Shared by the `beamtalk-codegen` unit tests and the `beamtalk-repl`
//! integration tests (one implementation, the shared-leaf rule).

/// The families of a shared corpus list partition it: in order, contiguous,
/// from `0` to `len`, none empty, and every family has exactly one test
/// (`tested` lists the family each per-family test checks). The proof over
/// the whole list is the union of the per-family tests, so a family that is
/// skipped, overlapped or never given a test would silently shrink the proof.
///
/// # Panics
///
/// Panics when the families do not partition `0..len` or a family is tested
/// more or fewer than once.
pub fn assert_families_partition_the_list(
    families: &[std::ops::Range<usize>],
    len: usize,
    tested: &[usize],
) {
    let mut next = 0;
    for (i, family) in families.iter().enumerate() {
        assert_eq!(
            family.start,
            next,
            "family {i} ({family:?}) does not start where family {} ended ({next})",
            i.wrapping_sub(1)
        );
        assert!(
            family.end > family.start,
            "family {i} ({family:?}) is empty"
        );
        next = family.end;
    }
    assert_eq!(
        next, len,
        "the families end at {next} but the list has {len} entries"
    );
    let mut tested: Vec<usize> = tested.to_vec();
    tested.sort_unstable();
    assert_eq!(
        tested,
        (0..families.len()).collect::<Vec<_>>(),
        "each of the {} families needs exactly one test",
        families.len()
    );
}

/// One `#[test]` per family of a shared corpus list (BT-3888). Each
/// per-family test calls `check` on the list's slice for its family (an index
/// range of `families`); the `partition` test proves, through
/// [`assert_families_partition_the_list`], that the families partition the
/// list and that every family has exactly one test here.
///
/// `list` is anything indexable by a range with a `len()` (a `&[T]` constant,
/// a `Vec<T>` expression); `families` is a `&[Range<usize>]`.
///
/// ```ignore
/// per_family_tests! {
///     list = FRAMES,
///     families = FRAME_FAMILIES,
///     check = every_site_in_a_frame_is_lowered,
///     partition = the_frame_tests_cover_every_frame,
///     every_site_in_a_loop_is_lowered => 0,
///     every_site_in_a_fold_is_lowered => 1,
/// }
/// ```
#[macro_export]
macro_rules! per_family_tests {
    (
        list = $list:expr,
        families = $families:expr,
        check = $check:path,
        partition = $partition:ident,
        $($name:ident => $family:literal),+ $(,)?
    ) => {
        $(
            #[test]
            fn $name() {
                $check(&$list[$families[$family].clone()]);
            }
        )+

        #[test]
        fn $partition() {
            $crate::test_helpers::corpus_families::assert_families_partition_the_list(
                $families,
                $list.len(),
                &[$($family),+],
            );
        }
    };
}

#[cfg(test)]
mod tests {
    use super::assert_families_partition_the_list;

    #[test]
    fn a_partition_with_one_test_per_family_passes() {
        assert_families_partition_the_list(&[0..2, 2..3, 3..7], 7, &[2, 0, 1]);
    }

    #[test]
    #[should_panic(expected = "does not start where family")]
    fn a_gap_between_families_is_reported() {
        assert_families_partition_the_list(&[0..2, 3..7], 7, &[0, 1]);
    }

    #[test]
    #[should_panic(expected = "does not start where family")]
    fn an_overlap_between_families_is_reported() {
        assert_families_partition_the_list(&[0..3, 2..7], 7, &[0, 1]);
    }

    #[test]
    #[should_panic(expected = "is empty")]
    fn an_empty_family_is_reported() {
        assert_families_partition_the_list(&[0..2, 2..2, 2..7], 7, &[0, 1, 2]);
    }

    #[test]
    #[should_panic(expected = "but the list has")]
    fn families_that_stop_short_of_the_list_are_reported() {
        assert_families_partition_the_list(&[0..2, 2..5], 7, &[0, 1]);
    }

    #[test]
    #[should_panic(expected = "needs exactly one test")]
    fn a_family_tested_twice_or_never_is_reported() {
        assert_families_partition_the_list(&[0..2, 2..7], 7, &[0, 0]);
    }
}
