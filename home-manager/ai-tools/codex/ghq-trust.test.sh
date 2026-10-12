#!/bin/bash
# Table-driven test for ghq-trust.sh, run by the `codex-ghq-trust` flake check.
# Usage: ghq-trust.test.sh <path-to-ghq-trust.sh>

set -euo pipefail

wrapper="$1"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
scratch=$(cd "$scratch" && pwd -P)

# The stub stands in for codex and records its argv, one argument per line. It uses /bin/sh because
# the Linux build sandbox has no /bin/bash.
stub="$scratch/codex-stub"
argv_file="$scratch/argv"
cat >"$stub" <<EOF
#!/bin/sh
printf '%s\n' "\$@" >"$argv_file"
exit "\${STUB_EXIT:-0}"
EOF
chmod +x "$stub"

home="$scratch/home"
worktree="$home/ghq/github.com/owner/repo.git/.worktrees/feature"
outside="$scratch/outside"
quoted="$home/ghq/github.com/owner/q\"b\\s"
mkdir -p "$worktree/sub" "$outside" "$quoted" "$home/ghqx"
ln -s "$worktree" "$outside/link-to-worktree"
ln -s "$outside" "$worktree/link-to-outside"

flag_for() {
  printf 'projects={ "%s" = { trust_level = "trusted" } }' "$1"
}

# run <dir> [args...]: runs the wrapper from <dir> with HOME=$home.
run() {
  local dir="$1"
  shift
  rm -f "$argv_file"
  (cd "$dir" && HOME="$home" bash "$wrapper" "$stub" "$@")
}

# expect_argv <description> <expected argv...>: compares the stub's recorded argv.
expect_argv() {
  local description="$1"
  shift
  checked=$((checked + 1))
  local expected actual
  expected=$(printf '%s\n' "$@")
  actual=$(cat "$argv_file" 2>/dev/null || echo "<stub not run>")
  [[ $actual == "$expected" ]] || fail "$description: expected [$expected], got [$actual]"
}

run "$worktree" exec hi
expect_argv "cwd in a ghq worktree" -c "$(flag_for "$worktree")" exec hi

run "$worktree/sub"
expect_argv "cwd below a worktree root" -c "$(flag_for "$worktree/sub")"

run "$outside" exec hi
expect_argv "cwd outside ghq" exec hi

run "$home/ghq"
expect_argv "ghq root itself"

run "$home/ghqx"
expect_argv "sibling sharing the ghq prefix"

run "$outside" -C "$worktree"
expect_argv "-C <dir>" -c "$(flag_for "$worktree")" -C "$worktree"

run "$outside" --cd "$worktree"
expect_argv "--cd <dir>" -c "$(flag_for "$worktree")" --cd "$worktree"

run "$outside" "--cd=$worktree"
expect_argv "--cd=<dir>" -c "$(flag_for "$worktree")" "--cd=$worktree"

run "$outside" "-C$worktree"
expect_argv "-C<dir>" -c "$(flag_for "$worktree")" "-C$worktree"

run "$outside" -C ../home/ghq/github.com/owner/repo.git/.worktrees/feature
expect_argv "relative -C" -c "$(flag_for "$worktree")" -C ../home/ghq/github.com/owner/repo.git/.worktrees/feature

run "$worktree" -C "$outside"
expect_argv "-C outside ghq from inside ghq" -C "$outside"

run "$outside" -C "$outside" -C "$worktree"
expect_argv "last -C wins" -c "$(flag_for "$worktree")" -C "$outside" -C "$worktree"

run "$outside" exec -- -C "$worktree"
expect_argv "-C after -- is data" exec -- -C "$worktree"

run "$worktree" -C "$scratch/missing"
expect_argv "-C to a missing directory" -C "$scratch/missing"

run "$worktree" -C
expect_argv "-C without a value falls back to the cwd" -c "$(flag_for "$worktree")" -C

run "$outside" "-C=$worktree"
expect_argv "-C=<dir>" -c "$(flag_for "$worktree")" "-C=$worktree"

run "$outside" --cd=../home/ghq/github.com/owner/repo.git/.worktrees/feature
expect_argv "relative --cd=" -c "$(flag_for "$worktree")" --cd=../home/ghq/github.com/owner/repo.git/.worktrees/feature

# CDPATH must not redirect a relative -C: Codex resolves it against the cwd.
mkdir -p "$outside/sub"
rm -f "$argv_file"
(cd "$worktree" && CDPATH="$outside" HOME="$home" bash "$wrapper" "$stub" -C sub)
expect_argv "CDPATH is ignored" -c "$(flag_for "$worktree/sub")" -C sub

run "$outside/link-to-worktree"
expect_argv "symlink into ghq resolves to the physical path" -c "$(flag_for "$worktree")"

run "$worktree/link-to-outside"
expect_argv "symlink from ghq to outside is not trusted"

run "$quoted"
escaped=${quoted//\\/\\\\}
escaped=${escaped//\"/\\\"}
expect_argv "quote and backslash are escaped" -c "$(flag_for "$escaped")"
quoted_flag=$(sed -n 2p "$argv_file")
checked=$((checked + 1))
key=$(python3 -c 'import sys, tomllib; print(next(iter(tomllib.loads(sys.argv[1])["projects"])))' "$quoted_flag" 2>&1) ||
  fail "escaped flag is not valid TOML: $key"
[[ $key == "$quoted" ]] || fail "escaped TOML key decodes to [$key], expected [$quoted]"

control="$home/ghq/github.com/owner/ctl"$'\t'"x"
mkdir -p "$control"
run "$control"
expect_argv "control character keeps the prompt"

# A name ending in a newline must not be trimmed into its sibling.
mkdir -p "$home/ghq/github.com/owner/nl" "$home/ghq/github.com/owner/nl"$'\n'
run "$home/ghq/github.com/owner/nl"$'\n'
expect_argv "trailing newline keeps the prompt"

run "$worktree" -c 'model="m"' -c 'projects={ "/other" = { trust_level = "untrusted" } }' exec
expect_argv "existing -c flags keep their order after the trust flag" \
  -c "$(flag_for "$worktree")" -c 'model="m"' -c 'projects={ "/other" = { trust_level = "untrusted" } }' exec

run "$worktree" 'prompt with "quotes" and $HOME'
expect_argv "arguments pass through unexpanded" -c "$(flag_for "$worktree")" 'prompt with "quotes" and $HOME'

# A symlinked ~/ghq still matches through its physical path.
linked_home="$scratch/linked-home"
mkdir -p "$linked_home"
ln -s "$home/ghq" "$linked_home/ghq"
rm -f "$argv_file"
(cd "$worktree" && HOME="$linked_home" bash "$wrapper" "$stub")
expect_argv "symlinked ~/ghq" -c "$(flag_for "$worktree")"

# No ~/ghq at all: no flag, codex still runs.
rm -f "$argv_file"
(cd "$worktree" && HOME="$outside" bash "$wrapper" "$stub")
expect_argv "missing ~/ghq"

checked=$((checked + 1))
status=0
(cd "$worktree" && STUB_EXIT=7 HOME="$home" bash "$wrapper" "$stub") || status=$?
[[ $status -eq 7 ]] || fail "codex exit status: expected 7, got $status"

echo "codex-ghq-trust: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
