#!/bin/bash
# Hook to enforce perl usage instead of sed/awk
# Reads tool call JSON from stdin and validates Bash commands

set -euo pipefail

# Read JSON input from stdin
input=$(cat)

# Extract tool name and command using jq (fallback to grep if jq unavailable)
if command -v jq &>/dev/null; then
    tool_name=$(echo "$input" | jq -r '.tool_name // ""')
    command=$(echo "$input" | jq -r '.tool_input.command // ""')
else
    # Fallback: simple grep extraction
    tool_name=$(echo "$input" | grep -o '"tool_name":"[^"]*"' | cut -d'"' -f4 || echo "")
    command=$(echo "$input" | grep -o '"command":"[^"]*"' | cut -d'"' -f4 || echo "")
fi

# Only validate Bash commands
if [[ "$tool_name" != "Bash" ]] || [[ -z "$command" ]]; then
    exit 0
fi

# Check for sed or awk usage (word boundary to avoid false positives)
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
    exit 2  # Block the command
fi

# Command is valid
exit 0
