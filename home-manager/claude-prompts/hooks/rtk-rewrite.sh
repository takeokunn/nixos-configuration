#!/bin/bash
# Hook to rewrite Bash commands through RTK for LLM token compression
# Reads tool call JSON from stdin, rewrites command via rtk rewrite, outputs modified JSON

set -euo pipefail

RTK_BIN="@RTK_BIN@"

# Read JSON input from stdin
input=$(cat)

# Extract tool name
if command -v jq &>/dev/null; then
  tool_name=$(echo "$input" | jq -r '.tool_name // ""')
  command=$(echo "$input" | jq -r '.tool_input.command // ""')
else
  exit 0
fi

# Only process Bash commands
if [[ "$tool_name" != "Bash" ]] || [[ -z "$command" ]]; then
  exit 0
fi

# Skip if rtk binary is not available
if [[ ! -x "$RTK_BIN" ]]; then
  exit 0
fi

# Rewrite the command through RTK
rewritten=$("$RTK_BIN" rewrite "$command" 2>/dev/null || echo "$command")

# Output modified JSON with rewritten command
echo "$input" | jq --arg new_cmd "$rewritten" '.tool_input.command = $new_cmd'
