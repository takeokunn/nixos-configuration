#!/bin/bash
# Codex wrapper that marks the session's target directory as trusted when it lies under ~/ghq, so a
# fresh worktree there does not stop at the "Trust this folder?" prompt. Trust also lets the
# directory's own .codex config and hooks load, so every checkout under ~/ghq is treated as trusted.
# Outside ~/ghq nothing is added and Codex prompts as before.
# Usage: ghq-trust.sh <codex-executable> [codex-args...]
#
# The override is an inline table because Codex splits a dotted -c key on every '.', so
# projects."<path>".trust_level breaks on paths such as github.com/<owner>/<repo>.git. Codex merges
# it with config.toml's [projects] and does not write it back. It is inserted before every other
# argument.

set -euo pipefail

exe="$1"
shift

# The directory Codex will run in: the last -C/--cd before `--`, relative to the current directory.
target="$PWD"
expect_dir=0
for arg in "$@"; do
  if ((expect_dir)); then
    target="$arg"
    expect_dir=0
    continue
  fi
  case "$arg" in
  --) break ;;
  -C | --cd) expect_dir=1 ;;
  --cd=*) target="${arg#--cd=}" ;;
  -C?*)
    target="${arg#-C}"
    target="${target#=}"
    ;;
  esac
done

# physical_dir <dir> <var>: sets <var> to the physical path of <dir>, or to "" when it does not
# exist. CDPATH is cleared so a relative -C resolves the way Codex resolves it, and the sentinel
# keeps a directory name ending in a newline from being trimmed into a different path.
physical_dir() {
  local dir
  dir=$(CDPATH='' cd -- "$1" 2>/dev/null && pwd -P && echo x) || dir=""
  printf -v "$2" '%s' "${dir%$'\n'x}"
}

trust=()
# A missing directory or ghq root adds no flag; Codex reports a bad -C itself.
physical_dir "$target" physical
physical_dir "$HOME/ghq" ghq_root
# A TOML basic string cannot hold a raw control character, so such a path keeps the prompt.
if [[ -n $physical && -n $ghq_root && $physical == "$ghq_root"/* && $physical != *[[:cntrl:]]* ]]; then
  key=${physical//\\/\\\\}
  key=${key//\"/\\\"}
  trust=(-c "projects={ \"$key\" = { trust_level = \"trusted\" } }")
fi

exec "$exe" "${trust[@]}" "$@"
