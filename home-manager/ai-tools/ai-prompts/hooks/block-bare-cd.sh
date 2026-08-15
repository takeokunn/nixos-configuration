#!/bin/bash

set -euo pipefail

input=$(cat)

if command -v jq &>/dev/null; then
  tool_name=$(echo "$input" | jq -r '.tool_name // ""' 2>/dev/null || echo "")
  command=$(echo "$input" | jq -r '.tool_input.command // ""' 2>/dev/null || echo "")
elif command -v python3 &>/dev/null; then
  tool_name=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("tool_name",""))' 2>/dev/null || echo "")
  command=$(printf '%s' "$input" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d.get("tool_input",{}).get("command",""))' 2>/dev/null || echo "")
else
  exit 0
fi

if [[ $tool_name != "Bash" ]] || [[ -z $command ]]; then
  exit 0
fi

trimmed="$(printf '%s' "$command" | perl -0pe 's/\A\s+//; s/\s+\z//')"

if [[ $trimmed == ALLOW_BARE_CD=1\ * ]]; then
  exit 0
fi
if [[ $trimmed != "cd" && $trimmed != cd\ * ]]; then
  exit 0
fi

if printf '%s' "$trimmed" | perl -0ne 'exit(/(?:&&|\|\||[;|&\n])/ ? 0 : 1)'; then
  exit 0
fi

cat >&2 <<'EOF'
❌ Bare `cd` blocked — it has no effect on the next call

Every Bash tool call starts in the project directory and shell state is not preserved
between calls, so a `cd` on its own changes nothing you can observe later. Measured across
this configuration's transcripts, `cd` is the most-invoked command at 43,749 calls — almost
all of them buying nothing.

Use instead:
  ❌ cd /path/to/dir            (then a separate call expecting to be there)
  ✅ ls /path/to/dir            — absolute paths in the command itself
  ✅ cd /path/to/dir && ls      — one compound command, allowed
  ✅ git -C /path/to/repo status — tool-native directory flags

Override deliberately with:  ALLOW_BARE_CD=1 cd /path
EOF
exit 2
