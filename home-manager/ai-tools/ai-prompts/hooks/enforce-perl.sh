#!/bin/bash

set -euo pipefail

input=$(cat)

if command -v jq &>/dev/null; then
  tool_name=$(echo "$input" | jq -r '.tool_name // ""')
  command=$(echo "$input" | jq -r '.tool_input.command // ""')
else
  tool_name=$(echo "$input" | grep -o '"tool_name":"[^"]*"' | cut -d'"' -f4 || echo "")
  command=$(echo "$input" | grep -o '"command":"[^"]*"' | cut -d'"' -f4 || echo "")
fi

if [[ $tool_name != "Bash" ]] || [[ -z $command ]]; then
  exit 0
fi

if echo "$command" | grep -qE '\b(sed|awk)\b'; then
  cat >&2 <<'EOF'
❌ sed/awk detected - Use perl instead

According to text-processing rules, batch text operations should use perl.

Examples:
  ❌ sed 's/foo/bar/g' file.txt
  ✅ perl -pe 's/foo/bar/g' file.txt

  ❌ awk '{print $1}' file.txt
  ✅ perl -lane 'print $F[0]' file.txt

  ❌ sed -i 's/old/new/g' *.txt
  ✅ perl -pi -e 's/old/new/g' *.txt

Please reformulate your command using perl.
EOF
  exit 2
fi

exit 0
