# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for check_cache_keys.py (BT-5086)."""

import unittest

import check_cache_keys as c

GOOD = """\
        with:
          key: ${{ runner.os }}-erlang-beam-${{ hashFiles('a') }}-src-${{ hashFiles('b') }}
          restore-keys: |
            ${{ runner.os }}-erlang-beam-${{ hashFiles('a') }}-
"""

BAD = """\
        with:
          key: ${{ runner.os }}-erlang-${{ hashFiles('a', 'b') }}-beam-${{ hashFiles('t') }}
          restore-keys: |
            ${{ runner.os }}-erlang-${{ hashFiles('a') }}-beam-${{ hashFiles('t') }}
            ${{ runner.os }}-erlang-beam-${{ hashFiles('t') }}-
"""


class CheckCacheKeys(unittest.TestCase):
    def test_prefix_is_accepted(self):
        self.assertEqual(c.check_text(GOOD), [])

    def test_non_prefix_is_reported(self):
        self.assertEqual([p[0] for p in c.check_text(BAD)], [4, 5])

    def test_single_line_restore_key(self):
        text = "          key: a-b-c\n          restore-keys: a-x\n"
        self.assertEqual(len(c.check_text(text)), 1)
        text = "          key: a-b-c\n          restore-keys: a-b\n"
        self.assertEqual(c.check_text(text), [])

    def test_unrelated_key_at_other_indent_is_ignored(self):
        text = "key: a\n  restore-keys: |\n    zzz\n"
        self.assertEqual(c.check_text(text), [])


if __name__ == "__main__":
    unittest.main()
