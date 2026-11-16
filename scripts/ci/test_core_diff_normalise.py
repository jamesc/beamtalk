#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Tests for core_diff_normalise.py (BT-4939)."""
import os
import tempfile
import unittest

import core_diff_normalise as n


def lit(path):
    return n.encode(path.encode())


def method_lines(path):
    return "'__beamtalk_method_lines'/0 = fun () ->\n    [{" + lit(path) + ", {{54, 'x'}}}]\n"


class NormaliseBytes(unittest.TestCase):
    def test_roundtrips_the_recorded_encoding(self):
        # The exact shape codegen emits, copied from a real snapshot.
        s = "#{#<47>(8,1,'integer',['unsigned'|['big']]),#<104>(8,1,'integer',['unsigned'|['big']])}#"
        self.assertEqual(lit("/h"), s)
        self.assertEqual(n.decode(n.LITERAL.match(s).group(1)), b"/h")

    def test_two_checkout_directories_normalise_equal(self):
        a = method_lines("/home/user/beamtalk/stdlib/src/actor.bt")
        b = method_lines("/ci/work/a-much/longer/_base-worktree/stdlib/src/actor.bt")
        self.assertNotEqual(a, b)
        self.assertEqual(n.normalise_text(a), n.normalise_text(b))
        self.assertEqual(n.normalise_text(a), method_lines("stdlib/src/actor.bt"))

    def test_fixture_and_test_dirs(self):
        self.assertEqual(
            n.normalise_text(lit("/r/stdlib/test/fixtures/m.bt")),
            lit("stdlib/test/fixtures/m.bt"),
        )

    def test_different_relative_path_still_differs(self):
        a = n.normalise_text(method_lines("/one/stdlib/src/actor.bt"))
        for other in (
            "/two/stdlib/src/object.bt",  # different file name
            "/two/stdlib/test/actor.bt",  # different directory
            "/two/stdlib/src/actor.btx",  # one byte longer
            "/two/stdlib/src/Actor.bt",  # one byte changed
        ):
            self.assertNotEqual(a, n.normalise_text(method_lines(other)), other)

    def test_nested_stdlib_component_is_not_swallowed(self):
        # Only the first `/stdlib/` ends the checkout prefix; a later one is
        # part of the relative path and must survive.
        got = n.normalise_text(lit("/r/stdlib/test/fixtures/stdlib/x.bt"))
        self.assertEqual(got, lit("stdlib/test/fixtures/stdlib/x.bt"))
        self.assertNotEqual(got, lit("stdlib/x.bt"))

    def test_other_byte_literals_are_untouched(self):
        for s in ("/usr/bin/env", "/c", "target/bt-test-tmp/x.dat", "stdlib/src/a.bt", "hello"):
            self.assertEqual(n.normalise_text(lit(s)), lit(s), s)

    def test_idempotent(self):
        once = n.normalise_text(method_lines("/a/b/stdlib/src/actor.bt"))
        self.assertEqual(n.normalise_text(once), once)

    def test_non_ascii_bytes_roundtrip(self):
        s = "/r/stdlib/src/é.bt"
        self.assertEqual(n.normalise_text(lit(s)), lit("stdlib/src/é.bt"))

    def test_text_outside_literals_is_untouched(self):
        t = "'f'/0 = fun () -> \"/a/stdlib/x\" -| [{'file', \"/a/stdlib/x\"}]\n"
        self.assertEqual(n.normalise_text(t), t)


class Leaks(unittest.TestCase):
    def test_detects_plain_and_encoded_root(self):
        self.assertTrue(n.leaks('"/ci/co/x"', "/ci/co"))
        self.assertTrue(n.leaks(lit("/ci/co/other/place.bt"), "/ci/co"))
        self.assertFalse(n.leaks(lit("stdlib/src/a.bt"), "/ci/co"))


class Cli(unittest.TestCase):
    def snapshot(self, files):
        d = tempfile.mkdtemp()
        self.addCleanup(lambda: __import__("shutil").rmtree(d, ignore_errors=True))
        for name, text in files.items():
            with open(os.path.join(d, name), "w", encoding="utf-8") as f:
                f.write(text)
        return d

    def read(self, d, name):
        with open(os.path.join(d, name), encoding="utf-8") as f:
            return f.read()

    def test_rewrites_in_place_and_passes_leak_check(self):
        d = self.snapshot({"a.core": method_lines("/ci/co/stdlib/src/a.bt")})
        self.assertEqual(n.main(["--root", "/ci/co", d]), 0)
        self.assertEqual(self.read(d, "a.core"), method_lines("stdlib/src/a.bt"))

    def test_leak_check_fails_on_root_outside_stdlib(self):
        d = self.snapshot({"a.core": method_lines("/ci/co/target/gen/a.bt")})
        self.assertEqual(n.main(["--root", "/ci/co", d]), 1)

    def test_snapshots_from_two_roots_become_identical(self):
        text = "{} and {}\n"
        one = self.snapshot({"a.core": method_lines("/x/stdlib/src/a.bt") + text})
        two = self.snapshot({"a.core": method_lines("/yy/zz/stdlib/src/a.bt") + text})
        self.assertEqual(n.main([one]), 0)
        self.assertEqual(n.main([two]), 0)
        self.assertEqual(self.read(one, "a.core"), self.read(two, "a.core"))

    def test_ignores_non_core_files(self):
        d = self.snapshot({"a.erl": method_lines("/x/stdlib/src/a.bt")})
        n.main([d])
        self.assertEqual(self.read(d, "a.erl"), method_lines("/x/stdlib/src/a.bt"))


if __name__ == "__main__":
    unittest.main()
