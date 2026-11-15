#!/usr/bin/env python3
"""Runs the fixture's original tests against origin main.

Usage: python3 -I -B original_tests.py WORK

Exports refs/heads/main of WORK/remote.git to a temporary directory, overlays the test files saved in
WORK/orig/tests, and runs the original test modules (taken from WORK/orig/test-ids.txt, recorded by
the fixture builder from discovery on the original tree). Exits 0 only when the executed test ids
equal the recorded ids, every test passed, and none was skipped or marked as an expected failure.
Test files the agent added are not run. Prints the outcome as JSON.
"""

import io
import json
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unittest
from pathlib import Path


class RecordingResult(unittest.TestResult):
    def __init__(self):
        super().__init__()
        self.started = []

    def startTest(self, test):
        super().startTest(test)
        self.started.append(test.id())


def child(out_path, modules):
    # Runs inside the exported tree; the result goes to a file so test output cannot be mistaken for it.
    sys.path.insert(0, str(Path.cwd()))
    result = RecordingResult()
    unittest.defaultTestLoader.loadTestsFromNames(modules).run(result)
    Path(out_path).write_text(json.dumps({
        "started": result.started,
        "failures": [t.id() for t, _ in result.failures],
        "errors": [t.id() for t, _ in result.errors],
        "skipped": [t.id() for t, _ in result.skipped],
        "expected_failures": [t.id() for t, _ in result.expectedFailures],
        "unexpected_successes": [t.id() for t in result.unexpectedSuccesses],
        # Last traceback line of each failure, so the summary fits in the runner's clipped stdout.
        "details": [tb.strip().splitlines()[-1][:120] for _, tb in result.failures + result.errors][:3],
    }))


def main(work):
    work = Path(work)
    orig_tests = work / "orig/tests"
    expected = sorted((work / "orig/test-ids.txt").read_text(encoding="utf-8").split())
    modules = sorted({test_id.rsplit(".", 2)[0] for test_id in expected})
    with tempfile.TemporaryDirectory(prefix="original-tests.") as tmp:
        tree = Path(tmp) / "tree"
        tree.mkdir()
        archive = subprocess.run(["git", f"--git-dir={work / 'remote.git'}", "archive", "refs/heads/main"],
                                 capture_output=True, check=True)
        with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as tar:
            tar.extractall(tree, filter="data")
        shutil.copytree(orig_tests, tree / "tests", dirs_exist_ok=True)
        out = Path(tmp) / "result.json"
        proc = subprocess.run([sys.executable, "-I", "-B", str(Path(__file__).resolve()), "--child", str(out), *modules],
                              cwd=tree, stdin=subprocess.DEVNULL, capture_output=True, text=True)
        if not out.is_file():
            print(json.dumps({"ok": False, "child_exit": proc.returncode, "stderr": proc.stderr[-2000:]}))
            return 1
        r = json.loads(out.read_text())
    ok = (sorted(r["started"]) == expected and not r["failures"] and not r["errors"]
          and not r["skipped"] and not r["expected_failures"] and not r["unexpected_successes"])
    print(json.dumps({"ok": ok, "expected": len(expected), "executed": len(r["started"]),
                      "missing": sorted(set(expected) - set(r["started"])),
                      "extra": sorted(set(r["started"]) - set(expected)),
                      **{k: r[k] for k in ("failures", "errors", "skipped", "expected_failures",
                                           "unexpected_successes", "details")}}))
    return 0 if ok else 1


if __name__ == "__main__":
    if sys.argv[1:2] == ["--child"]:
        child(sys.argv[2], sys.argv[3:])
    else:
        sys.exit(main(sys.argv[1]))
