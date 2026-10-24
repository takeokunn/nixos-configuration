#!/usr/bin/env bash

input=$(cat)

# used_percentage is null early in a session and right after /compact. The separator is not
# whitespace: read collapses runs of whitespace IFS characters, so an empty field would shift the rest.
IFS=$'\x1f' read -r MODEL_DISPLAY CURRENT_DIR USED_PCT < <(
  echo "$input" | jq -r '[.model.display_name // "", .workspace.current_dir // "", .context_window.used_percentage // ""] | map(tostring) | join("\u001f")'
)

GIT_BRANCH=""

# The nearest .jj or .git decides: inside a jj workspace of a bare repo, git
# finds the bare root and would report its HEAD instead of this workspace's.
vcs_dir=$PWD
while [ "$vcs_dir" != / ] && [ ! -e "$vcs_dir/.jj" ] && [ ! -e "$vcs_dir/.git" ]; do
  vcs_dir=$(dirname "$vcs_dir")
done

if [ -d "$vcs_dir/.jj" ]; then
  # --ignore-working-copy: a status line must not snapshot the working copy.
  JJ_REF=$(jj --ignore-working-copy -R "$vcs_dir" log --no-graph -r @ \
    -T 'coalesce(bookmarks.join(" "), parents.map(|c| c.bookmarks().join(" ")).join(" "), "@ (" ++ change_id.short(8) ++ ")")' 2>/dev/null)
  if [ -n "$JJ_REF" ]; then
    GIT_BRANCH=" |  $JJ_REF"
  fi
elif git rev-parse &>/dev/null; then
  BRANCH=$(git branch --show-current)
  if [ -n "$BRANCH" ]; then
    GIT_BRANCH=" |  $BRANCH"
  else
    COMMIT_HASH=$(git rev-parse --short HEAD 2>/dev/null)
    if [ -n "$COMMIT_HASH" ]; then
      GIT_BRANCH=" |  HEAD ($COMMIT_HASH)"
    fi
  fi
fi

if [[ ! $USED_PCT =~ ^[0-9]+(\.[0-9]*)?$ ]]; then
  CONTEXT="ctx _%"
else
  percentage=${USED_PCT%%.*}
  if [ "$percentage" -ge 90 ]; then
    color="\033[31m"
  elif [ "$percentage" -ge 70 ]; then
    color="\033[33m"
  else
    color="\033[32m"
  fi
  CONTEXT=$(echo -e "ctx ${color}${percentage}%\033[0m")
fi

echo "󰚩 ${MODEL_DISPLAY} |  ${CURRENT_DIR##*/}${GIT_BRANCH} |  ${CONTEXT}"
