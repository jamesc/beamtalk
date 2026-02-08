#!/usr/bin/env python3
# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
"""Clean up covertool XML reports.

Removes phantom empty packages and shortens path-based package names
to OTP application names (e.g. beamtalk_runtime instead of
beamtalk_runtime.workspaces.research.runtime.apps.beamtalk_runtime.src).
"""

import glob
import os
import sys
import xml.etree.ElementTree as ET


def clean_xml(xml_path):
    tree = ET.parse(xml_path)
    root = tree.getroot()
    packages = root.find("packages")
    if packages is None:
        return 0

    app_name = os.path.basename(xml_path).replace(".covertool.xml", "")
    to_remove = []
    for pkg in packages.findall("package"):
        classes = pkg.find("classes")
        if classes is None or len(classes) == 0:
            to_remove.append(pkg)
        else:
            pkg.set("name", app_name)
    for pkg in to_remove:
        packages.remove(pkg)

    tree.write(xml_path, xml_declaration=True, encoding="utf-8")
    return len(to_remove)


def main():
    covertool_dir = sys.argv[1] if len(sys.argv) > 1 else "_build/test/covertool"
    pattern = os.path.join(covertool_dir, "*.covertool.xml")
    for xml_path in sorted(glob.glob(pattern)):
        removed = clean_xml(xml_path)
        print(f"  ✅ Cleaned {os.path.basename(xml_path)}: removed {removed} empty package(s)")


if __name__ == "__main__":
    main()
