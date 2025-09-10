#!/usr/bin/env bash

# ref: https://zenn.dev/makotan/articles/db34bb6860cda5
#      https://zenn.dev/pnd/articles/claude-code-statusline

# Read JSON input from stdin
input=$(cat)

MODEL_DISPLAY=$(echo "$input" | jq -r '.model.display_name')
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir')
TRANSCRIPT_PATH=$(echo "$input" | jq -r '.transcript_path')

# gitブランチ情報取得
GIT_BRANCH=""
if git rev-parse &>/dev/null; then
  BRANCH=$(git branch --show-current)
  if [ -n "$BRANCH" ]; then
    GIT_BRANCH=" |  $BRANCH"
  else
    COMMIT_HASH=$(git rev-parse --short HEAD 2>/dev/null)
    if [ -n "$COMMIT_HASH" ]; then
      GIT_BRANCH=" |  HEAD ($COMMIT_HASH)"
    fi
  fi
fi

# token情報取得

if [ -z "$TRANSCRIPT_PATH" ] || [ ! -f "$TRANSCRIPT_PATH" ]; then
  TOKEN_COUNT="_ tkns. (_%)"
else
  # Get last assistant message with usage data using jq
  total_tokens=$(tail -n 100 "$TRANSCRIPT_PATH" 2>/dev/null |
    jq -s 'map(select(.type == "assistant" and .message.usage)) |
    last |
    .message.usage |
    (.input_tokens // 0) +
    (.output_tokens // 0) +
    (.cache_creation_input_tokens // 0) +
  (.cache_read_input_tokens // 0)' 2>/dev/null)

  # Default to 0 if no valid result
  total_tokens=${total_tokens:-0}

  # max token count: 200k
  # compaction threshold: 80% (160k)
  COMPACTION_THRESHOLD=160000
  # Calculate percentage
  percentage=$((total_tokens * 100 / COMPACTION_THRESHOLD))

  # Format token display
  if [ "$total_tokens" -ge 1000 ]; then
    thousands=$(echo "scale=1; $total_tokens/1000" | bc)
    token_display=$(printf "%.1fK" "$thousands")
  else
    token_display="$total_tokens"
  fi

  # Color coding for percentage
  if [ "$percentage" -ge 90 ]; then
    color="\033[31m" # Red
  elif [ "$percentage" -ge 70 ]; then
    color="\033[33m" # Yellow
  else
    color="\033[32m" # Green
  fi

  # Format: "123 tkns. (10%)"
  TOKEN_COUNT=$(echo -e "${token_display} tkns. (${color}${percentage}%\033[0m)")
fi

echo "󰚩 ${MODEL_DISPLAY} |  ${CURRENT_DIR##*/}${GIT_BRANCH} |  ${TOKEN_COUNT}"
