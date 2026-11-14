#!/bin/sh
# Builds a plain Python project ("inventory") whose tests fail until a multi-site edit is made.
# No VCS: aitools-edit scenarios judge only the final file state.
#
# Usage: sh edit-targets.sh <fixture_dir> <variant>
#   rename    rename compute_total to calculate_total across three modules
#   tax       change the tax rate at three sites and leave a look-alike discount alone
#   register  add a command: function, dispatch table entry, __all__, and help text
#   crlf      change two values in a CRLF file without touching line endings or other bytes
#   twin      change one of two identical method bodies
#
# Tests live in tests/ and are copied to ../orig/tests so a check can tell they were not edited.
set -eu

if [ $# -lt 2 ]; then
  echo "usage: $0 <fixture_dir> <variant>" >&2
  exit 64
fi
VARIANT=$2
mkdir -p "$1"
FIXTURE=$(cd "$1" && pwd -P)
WORK=$(dirname "$FIXTURE")
case $WORK in
  /tmp/* | /private/tmp/*) ;;
  *)
    echo "refusing to build outside /tmp: $WORK" >&2
    exit 64
    ;;
esac
if [ -n "$(ls -A "$FIXTURE")" ]; then
  echo "fixture dir is not empty: $FIXTURE" >&2
  exit 64
fi

cd "$FIXTURE"
mkdir -p inventory tests
: >inventory/__init__.py
: >tests/__init__.py
cat >README.md <<'EOF'
# inventory

テスト: `python3 -m unittest`
EOF

case $VARIANT in
  rename)
    cat >inventory/billing.py <<'EOF'
"""Billing. compute_total is the single source of order totals."""


def compute_total(lines):
    """Sum price * quantity over order lines."""
    return sum(price * qty for price, qty in lines)


def compute_total_with_shipping(lines, shipping):
    return compute_total(lines) + shipping


def is_free_shipping(lines):
    return compute_total(lines) >= 5000
EOF
    cat >inventory/report.py <<'EOF'
from inventory.billing import compute_total


def line(order_id, lines):
    return f"{order_id}: {compute_total(lines)}"


def grand_total(orders):
    return sum(compute_total(lines) for lines in orders.values())
EOF
    cat >inventory/cli.py <<'EOF'
import sys

from inventory import billing


def main(argv):
    lines = [(int(p), int(q)) for p, q in (a.split("x") for a in argv)]
    print(billing.compute_total(lines))


if __name__ == "__main__":
    main(sys.argv[1:])
EOF
    cat >tests/test_rename.py <<'EOF'
import io
import unittest
from contextlib import redirect_stdout

from inventory import billing, cli, report


class RenameTest(unittest.TestCase):
    def test_new_name(self):
        self.assertEqual(billing.calculate_total([(100, 2)]), 200)
        self.assertEqual(billing.compute_total_with_shipping([(100, 1)], 10), 110)
        self.assertTrue(billing.is_free_shipping([(5000, 1)]))

    def test_old_name_is_gone(self):
        self.assertFalse(hasattr(billing, "compute_total"))

    def test_callers(self):
        self.assertEqual(report.line("A1", [(10, 3)]), "A1: 30")
        self.assertEqual(report.grand_total({"a": [(1, 1)], "b": [(2, 2)]}), 5)
        out = io.StringIO()
        with redirect_stdout(out):
            cli.main(["100x2", "5x1"])
        self.assertEqual(out.getvalue().strip(), "205")


if __name__ == "__main__":
    unittest.main()
EOF
    ;;
  tax)
    cat >inventory/pricing.py <<'EOF'
"""Pricing. Amounts are integers in yen."""


def with_tax(price):
    return round(price * (1 + 0.08))


def tax_only(price):
    return round(price * 0.08)


def member_price(price):
    # Member discount, unrelated to tax.
    discounted = price - round(price * 0.08)
    return round(discounted * (1 + 0.08))


def describe():
    return "tax 8% / member discount 8%"
EOF
    cat >tests/test_tax.py <<'EOF'
import unittest

from inventory import pricing


class TaxTest(unittest.TestCase):
    def test_with_tax(self):
        self.assertEqual(pricing.with_tax(1000), 1100)

    def test_tax_only(self):
        self.assertEqual(pricing.tax_only(1000), 100)

    def test_member_price_keeps_discount(self):
        # 1000 - 80 = 920, then 10% tax.
        self.assertEqual(pricing.member_price(1000), 1012)

    def test_describe(self):
        self.assertEqual(pricing.describe(), "tax 10% / member discount 8%")


if __name__ == "__main__":
    unittest.main()
EOF
    ;;
  register)
    cat >inventory/commands.py <<'EOF'
"""Command registry for the inventory CLI."""

__all__ = ["cmd_list", "cmd_count", "dispatch", "HELP"]

HELP = """\
Commands:
  list    print every item
  count   print the number of items
"""


def cmd_list(items):
    return "\n".join(items)


def cmd_count(items):
    return str(len(items))


COMMANDS = {
    "list": cmd_list,
    "count": cmd_count,
}


def dispatch(name, items):
    return COMMANDS[name](items)
EOF
    cat >tests/test_register.py <<'EOF'
import unittest

from inventory import commands


class RegisterTest(unittest.TestCase):
    def test_export_dispatch(self):
        self.assertEqual(commands.dispatch("export", ["a", "b"]), "exported 2 items")

    def test_export_exported(self):
        self.assertIn("cmd_export", commands.__all__)
        self.assertIs(commands.COMMANDS["export"], commands.cmd_export)

    def test_help_lists_export(self):
        self.assertRegex(commands.HELP, r"(?m)^  export\s+\S")

    def test_existing_commands_unchanged(self):
        self.assertEqual(commands.dispatch("count", ["a"]), "1")
        self.assertEqual(commands.dispatch("list", ["a", "b"]), "a\nb")


if __name__ == "__main__":
    unittest.main()
EOF
    ;;
  crlf)
    mkdir -p config
    printf '%s\r\n' \
      "; Uploader settings. This file must keep CRLF line endings." \
      "[upload]" \
      "endpoint = https://uploads.example.invalid/v1" \
      "timeout = 30" \
      "retries = 3" \
      "" \
      "[download]" \
      "timeout = 30" \
      "retries = 3" \
      >config/settings.ini
    cat >tests/test_crlf.py <<'EOF'
import pathlib
import unittest

PATH = pathlib.Path(__file__).resolve().parent.parent / "config" / "settings.ini"

EXPECTED = (
    b"; Uploader settings. This file must keep CRLF line endings.\r\n"
    b"[upload]\r\n"
    b"endpoint = https://uploads.example.invalid/v1\r\n"
    b"timeout = 60\r\n"
    b"retries = 5\r\n"
    b"\r\n"
    b"[download]\r\n"
    b"timeout = 30\r\n"
    b"retries = 3\r\n"
)


class CrlfTest(unittest.TestCase):
    def test_bytes(self):
        self.assertEqual(PATH.read_bytes(), EXPECTED)


if __name__ == "__main__":
    unittest.main()
EOF
    ;;
  twin)
    cat >inventory/jobs.py <<'EOF'
"""Batch jobs. Import and Export share their retry policy shape on purpose."""


class Import:
    name = "import"

    def policy(self):
        attempts = 3
        backoff = 2
        return {"attempts": attempts, "backoff": backoff}


class Export:
    name = "export"

    def policy(self):
        attempts = 3
        backoff = 2
        return {"attempts": attempts, "backoff": backoff}
EOF
    cat >tests/test_twin.py <<'EOF'
import unittest

from inventory.jobs import Export, Import


class TwinTest(unittest.TestCase):
    def test_export_retries_more(self):
        self.assertEqual(Export().policy(), {"attempts": 5, "backoff": 2})

    def test_import_unchanged(self):
        self.assertEqual(Import().policy(), {"attempts": 3, "backoff": 2})


if __name__ == "__main__":
    unittest.main()
EOF
    ;;
  *)
    echo "unknown variant: $VARIANT" >&2
    exit 64
    ;;
esac

mkdir -p "$WORK/orig"
cp -R tests "$WORK/orig/tests"
