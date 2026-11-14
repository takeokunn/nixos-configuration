#!/bin/bash
# Test for copy-local-mcp-servers.sh, run by the `copy-local-mcp-servers` flake check.
# Usage: copy-local-mcp-servers.test.sh <path-to-copy-local-mcp-servers.sh>
# Needs bash, jj, jq and GNU coreutils on PATH.
# Single-quoted jq filters and the literal "$(x)" fixture are meant to stay unexpanded.
# shellcheck disable=SC2016

set -euo pipefail

script="$1"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
scratch=$(cd "$scratch" && pwd -P)

# Nothing may read or write the caller's home, ~/.claude.json, or jj configuration.
export HOME="$scratch/home"
export JJ_CONFIG="$scratch/jjconfig"
export JJ_USER=tester JJ_EMAIL=tester@example.com
mkdir -p "$HOME"
touch "$JJ_CONFIG"
settings="$HOME/.claude.json"

if ! realpath -- / >/dev/null 2>&1; then
  echo "copy-local-mcp-servers test: needs GNU realpath (coreutils) on PATH" >&2
  exit 1
fi

# The script runs with a PATH holding only these tools, so no real claude is reachable.
bash_bin=$(command -v bash)
tools="$scratch/tools"
mkdir -p "$tools"
for tool in jq jj realpath; do
  ln -s "$(command -v "$tool")" "$tools/$tool"
done

# The stub stands in for claude. Each call records its physical cwd and its NUL-separated argv
# under $STUB_LOG, fails for names listed in $STUB_FAIL_NAMES ("|a|b|"), and prints to stdout
# otherwise so a leak is visible. It uses /bin/sh because the Linux build sandbox has no /bin/bash.
stub_dir="$scratch/stub"
mkdir -p "$stub_dir"
cat >"$stub_dir/claude" <<'EOF'
#!/bin/sh
call="$STUB_LOG/$$"
pwd -P >"$call.cwd"
printf '%s\0' "$@" >"$call.argv"
case "$STUB_FAIL_NAMES" in
*"|$5|"*)
  echo "stub refused $5" >&2
  exit 3
  ;;
esac
echo "stub stdout for $5"
EOF
chmod +x "$stub_dir/claude"
with_claude="$stub_dir:$tools"
without_claude="$tools"
log="$scratch/log"

# make_bare_repo <dir>: a jj store at <dir> with no .git entry, built in a scratch directory and
# moved over the way __fzf_ghq_init_jj does it, so the default workspace root is <dir>.
make_bare_repo() {
  local tmp
  mkdir -p "$1/.worktrees"
  tmp=$(mktemp -d "$1/.jj-init.XXXXXX")
  jj git init --no-colocate "$tmp" >/dev/null 2>&1
  mv "$tmp/.jj" "$1/.jj"
  rm -rf "$tmp"
}

# add_workspace <repo> <name> <dir>
add_workspace() {
  jj -R "$1" workspace add --no-colocate --sparse-patterns full --name "$2" -r 'root()' "$3" >/dev/null 2>&1
}

root="$scratch/ghq/r.git"
existing="$root/.worktrees/existing"
target="$root/.worktrees/new"
make_bare_repo "$root"
add_workspace "$root" existing "$existing"
add_workspace "$root" new "$target"
mkdir -p "$existing/sub"
ln -s "$root" "$scratch/root-link"

other="$scratch/ghq/other.git"
other_ws="$other/.worktrees/o"
make_bare_repo "$other"
add_workspace "$other" o "$other_ws"

nonbare="$scratch/nonbare"
nonbare_ws="$scratch/nonbare-ws"
jj git init --colocate "$nonbare" >/dev/null 2>&1
add_workspace "$nonbare" ws "$nonbare_ws"

unrelated="$scratch/unrelated"
mkdir -p "$unrelated"

weird_name='my srv "q" $(x) 日本'
weird_value='{"type":"stdio","command":"echo","args":["a b","\"q\"","$(x)","日本語\nline"],"env":{"K":"v w"}}'

# write_settings <filter>: writes ~/.claude.json from a jq filter that sees the keys as $root,
# $existing, $target, $other_ws, $nonbare, $nonbare_ws and the odd server as $wn/$wv.
write_settings() {
  jq -n --arg root "$root" --arg existing "$existing" --arg target "$target" \
    --arg other_ws "$other_ws" --arg nonbare "$nonbare" --arg nonbare_ws "$nonbare_ws" \
    --arg wn "$weird_name" --argjson wv "$weird_value" "$1" >"$settings"
}

# run <cwd> <path> [args...]: runs the script; sets status, out, err.
run() {
  local dir="$1" path="$2"
  shift 2
  rm -rf "$log"
  mkdir -p "$log"
  status=0
  (cd "$dir" && PATH="$path" STUB_LOG="$log" "$bash_bin" "$script" "$@") \
    >"$scratch/out" 2>"$scratch/err" || status=$?
  out=$(cat "$scratch/out")
  err=$(cat "$scratch/err")
}

# expect_exit <description> <status>: checks the exit status and that stdout stayed empty.
expect_exit() {
  checked=$((checked + 1))
  [[ $status -eq $2 ]] || fail "$1: exit status: expected $2, got $status (stderr: $err)"
  checked=$((checked + 1))
  [[ -z $out ]] || fail "$1: stdout is not empty: [$out]"
}

expect_calls() {
  checked=$((checked + 1))
  local n
  n=$(find "$log" -name '*.argv' | wc -l | tr -d ' ')
  [[ $n -eq $2 ]] || fail "$1: claude calls: expected $2, got $n"
}

expect_err_has() {
  checked=$((checked + 1))
  [[ $err == *"$2"* ]] || fail "$1: stderr lacks [$2]: [$err]"
}

expect_err_empty() {
  checked=$((checked + 1))
  [[ -z $err ]] || fail "$1: stderr is not empty: [$err]"
}

# call_argv <name>: sets argv to the recorded argv of the call registering <name>, and call_cwd to
# its cwd; argv is empty when no call registered it.
call_argv() {
  local file a
  argv=()
  call_cwd=""
  for file in "$log"/*.argv; do
    [[ -e $file ]] || continue
    local -a this=()
    while IFS= read -r -d '' a; do
      this+=("$a")
    done <"$file"
    if [[ ${#this[@]} -ge 5 && ${this[4]} == "$1" ]]; then
      argv=("${this[@]}")
      call_cwd=$(cat "${file%.argv}.cwd")
      return
    fi
  done
}

# expect_registered <description> <name> <expected json>: one call with argv exactly
# `mcp add-json --scope local <name> <json>`, run in the target, with JSON equal to the expected.
expect_registered() {
  local description="$1" name="$2" expected="$3"
  call_argv "$name"
  checked=$((checked + 1))
  if [[ ${#argv[@]} -eq 0 ]]; then
    fail "$description: [$name] was not registered"
    return
  fi
  local prefix="${argv[*]:0:4}"
  [[ ${#argv[@]} -eq 6 && $prefix == "mcp add-json --scope local" && ${argv[4]} == "$name" ]] ||
    fail "$description: argv for [$name]: $(printf '[%s] ' "${argv[@]}")"
  checked=$((checked + 1))
  [[ $call_cwd == "$target" ]] || fail "$description: cwd for [$name]: expected $target, got $call_cwd"
  checked=$((checked + 1))
  [[ $(jq -S . <<<"${argv[5]}") == "$(jq -S . <<<"$expected")" ]] ||
    fail "$description: json for [$name]: expected $expected, got ${argv[5]}"
}

# The stub must be the only claude the script can find, and none in the no-claude PATH.
checked=$((checked + 1))
[[ $(PATH="$with_claude" command -v claude) == "$stub_dir/claude" ]] || fail "stub claude is not first on PATH"
if (PATH="$without_claude" command -v claude >/dev/null 2>&1); then
  echo "copy-local-mcp-servers test: a claude is reachable without the stub; aborting" >&2
  exit 1
fi

# ---- copy from the bare root's key
write_settings '{projects: {
  ($root): {mcpServers: {plain: {command: "p", args: []}, ($wn): $wv}},
  ($target): {other: 1}
}}'
run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "root copy" 0
expect_calls "root copy" 2
expect_registered "root copy" plain '{"command":"p","args":[]}'
expect_registered "root copy: odd name and value" "$weird_name" "$weird_value"
expect_err_has "root copy" "copied 2 local MCP server(s)"

run "$unrelated" "$with_claude" "$scratch/root-link" "$target"
expect_exit "repo given through a symlink" 0
expect_calls "repo given through a symlink" 2

run "$existing" "$with_claude" "$root" ../new
expect_exit "relative target resolves against the cwd" 0
expect_registered "relative target resolves against the cwd" plain '{"command":"p","args":[]}'

# ---- union with the caller's workspace; the root wins on a shared name
write_settings '{projects: {
  ($root): {mcpServers: {shared: {command: "from-root"}, r1: {command: "r1"}}},
  ($existing): {mcpServers: {shared: {command: "from-ws"}, w1: {command: "w1"}}},
  ($target): {mcpServers: {t1: {command: "t1"}}},
  ($other_ws): {mcpServers: {o1: {command: "o1"}}}
}}'
run "$existing/sub" "$with_claude" "$root" "$target"
expect_exit "union with caller workspace" 0
expect_calls "union with caller workspace" 3
expect_registered "union: root wins on a name clash" shared '{"command":"from-root"}'
expect_registered "union: root-only server" r1 '{"command":"r1"}'
expect_registered "union: workspace-only server" w1 '{"command":"w1"}'

run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "caller outside any repo" 0
expect_calls "caller outside any repo" 2
call_argv w1
checked=$((checked + 1))
[[ ${#argv[@]} -eq 0 ]] || fail "caller outside any repo: workspace server w1 was copied"

run "$other_ws" "$with_claude" "$root" "$target"
expect_exit "caller in another repo's workspace" 0
expect_calls "caller in another repo's workspace" 2
call_argv o1
checked=$((checked + 1))
[[ ${#argv[@]} -eq 0 ]] || fail "caller in another repo's workspace: o1 was copied"

run "$target" "$with_claude" "$root" "$target"
expect_exit "caller in the target itself" 0
expect_calls "caller in the target itself" 2
call_argv t1
checked=$((checked + 1))
[[ ${#argv[@]} -eq 0 ]] || fail "caller in the target itself: t1 was re-added"

run "$root" "$with_claude" "$root" "$target"
expect_exit "caller at the bare root" 0
expect_calls "caller at the bare root" 2

# ---- nothing to do
write_settings '{projects: {
  ($nonbare): {mcpServers: {n1: {command: "n1"}}},
  ($nonbare_ws): {mcpServers: {n2: {command: "n2"}}}
}}'
run "$nonbare_ws" "$with_claude" "$nonbare" "$nonbare_ws"
expect_exit "non-bare repo" 0
expect_calls "non-bare repo" 0
expect_err_empty "non-bare repo"

rm -f "$settings"
run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "no ~/.claude.json" 0
expect_calls "no ~/.claude.json" 0
expect_err_empty "no ~/.claude.json"

write_settings '{projects: {($other_ws): {mcpServers: {o1: {command: "o1"}}}}}'
run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "root key absent" 0
expect_calls "root key absent" 0
expect_err_empty "root key absent"

write_settings '{projects: {($root): {mcpServers: {}}}}'
run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "empty mcpServers" 0
expect_calls "empty mcpServers" 0
expect_err_empty "empty mcpServers"

# ---- failures
write_settings '{projects: {
  ($root): {mcpServers: {ok: {command: "ok"}}},
  ($existing): {mcpServers: {"-x": {command: "x"}}}
}}'
run "$existing" "$with_claude" "$root" "$target"
expect_exit "name starting with -" 1
expect_calls "name starting with -" 0
expect_err_has "name starting with -" "-x"

write_settings '{projects: {($root): {mcpServers: {s1: {command: "1"}, s2: {command: "2"}, s3: {command: "3"}}}}}'
STUB_FAIL_NAMES='|s2|' run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "partial failure" 1
expect_calls "partial failure: every server is attempted" 3
expect_registered "partial failure: s3 after the failure" s3 '{"command":"3"}'
expect_err_has "partial failure" "s2 (exit 3): stub refused s2"
checked=$((checked + 1))
[[ $err != *"s1 (exit"* && $err != *"s3 (exit"* ]] || fail "partial failure: a successful server is listed as failed: [$err]"

run "$unrelated" "$without_claude" "$root" "$target"
expect_exit "claude not on PATH" 1
expect_calls "claude not on PATH" 0
expect_err_has "claude not on PATH" "claude is not on PATH"

printf '{not json' >"$settings"
run "$unrelated" "$with_claude" "$root" "$target"
expect_exit "invalid settings JSON" 1
expect_calls "invalid settings JSON" 0
expect_err_has "invalid settings JSON" "cannot read $settings"

for args in 0 1 3; do
  set -- "$root" "$target" extra
  run "$unrelated" "$with_claude" "${@:1:args}"
  expect_exit "$args arguments" 2
  expect_calls "$args arguments" 0
  expect_err_has "$args arguments" "usage: copy-local-mcp-servers <repo> <target>"
done

echo "copy-local-mcp-servers: $checked checks, $failures failures"
[[ $failures -eq 0 ]]
