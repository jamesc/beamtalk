# Stdlib API Consistency Rules

Status: decided 20 Nov 2026 (BT-5090). Enforced by
`crates/beamtalk-core/tests/stdlib_api_consistency.rs`.

This note records the decisions behind BT-5090 and the rules they imply for
anyone adding a selector to `stdlib/src`. No selector is removed and no
existing behaviour changes: every rename keeps the old spelling as an alias
that forwards to the canonical selector.

## Decisions

1. **`Array` is an indexed peer of `List` for reading, not for growing.**
   There is no new superclass. `Array` answers the order-dependent,
   non-growing selectors `reversed`, `indexOf:`, `take:`, `drop:`, `sort` and
   `sort:`. It does not gain `add:` or `addFirst:`.
2. **One spelling per concept; the existing majority wins.** `reversed` is the
   spelling, so `String` gains `reversed` and `String reverse` stays as a
   documented deprecated alias. `Set withAll:` is the constructor and
   `Set fromList:` stays as a documented deprecated alias. Other mismatches
   (`warn:`, `totalDays`, `openHandles`) are confirmed first and then follow
   the same rule.
3. **Every time parameter accepts `Integer | Duration`.** An Integer means
   milliseconds, as for `Timer` and `Parallel`. Where `#infinity` is allowed
   the type is `Timeout | Duration`. Integer is not deprecated, and fields keep
   storing milliseconds: a `Duration` is converted once, at the call boundary.
4. **`Enumerable` keeps one required selector** (`elements`). It provides the
   queries `Collection` has and it lacks, each forwarded to `elements`:
   `reject:`, `allSatisfy:`, `noneSatisfy:`, `includes:` and `detect:`.
5. **`Ets` keeps its missing-key behaviour.** `Ets at:` answers `V | Nil`
   because it is shared mutable state and the type forces the caller to handle
   `nil`; `Dictionary at:` raises. Both class docs say so. `Ets` gains
   `at:ifPresent:ifAbsent:`, `do:` and `doWithKey:`.
6. **`Set` docs match the implementation.**
7. **Typing nits** (`unregisterName`, `dir` parameters, `Subprocess` /
   `ReactiveSubprocess` error types) are fixed as long as callers' matchable
   errors do not change.

A compiler-checked deprecation annotation is out of scope: "deprecated" is a
sentence in the alias's doc comment.

## Rules the test enforces

| Rule | Check |
|------|-------|
| Sequence | `List` and `Array` both answer every selector of decision 1. |
| Enumerable | `Enumerable` provides every query of decision 4. |
| Time | Every parameter of a keyword ending in `timeout:`, `after:`, `every:`, `sleep:` or `within:` (case-insensitive) accepts `Duration`, directly or through a type alias such as `Timeout`. The outliers whose names do not match, and `Value` setters generated from an Integer field, are listed by name in the test. |
| Naming | A class that defines `reverse` also defines `reversed`. |

## Expected failures

A rule the stdlib does not yet satisfy is named in `EXPECTED_FAILURES` in the
test. It is not skipped: the test fails when a violation is missing from the
list **and** when a listed entry no longer violates, so the PR that fixes a
rule deletes its entries. The list is empty when BT-5090 is done; do not add
to it.

## Adding a selector

* A new time parameter is `Integer | Duration` (or `Timeout | Duration`).
  Convert once at the boundary; never inside a loop or on a send path.
* A new spelling of an existing concept forwards to the canonical selector;
  say in the doc comment which one is canonical.
* Name the order-dependent counterpart on both `List` and `Array` unless it
  grows the collection.
