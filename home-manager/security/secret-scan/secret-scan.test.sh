#!/usr/bin/env bash
# Test for secret-scan.sh, run by the `secret-scan` flake check.
# Usage: secret-scan.test.sh <scan> <the same scan built with a gitleaks that exits 1>
#
# Fake secrets are generated at run time: a literal one in this file would be rejected by the
# repository's gitleaks pre-commit hook. Every fake value contains FAKE.
# check bodies are single-quoted and evaluated by check(), so the variables they read
# expand at check time.
# shellcheck disable=SC2016,SC2034

set -uo pipefail

scan="$1"
broken_scan="$2"
failures=0
checked=0

check() {
  checked=$((checked + 1))
  if eval "$2"; then :; else
    echo "FAIL: $1" >&2
    failures=$((failures + 1))
  fi
}

random() {
  local chars=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 out=""
  for ((n = 0; n < $1; n++)); do out+=${chars:RANDOM % 62:1}; done
  printf '%s' "$out"
}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export HOME="$work/home"
export SECRET_SCAN_STATE_DIR="$work/state"
export SECRET_SCAN_OSASCRIPT="$work/osascript"
notify_log="$work/notify.log"
logs="$work/logs"
# The scan replaces PATH with its own store paths, which hold no bash, so `#!/usr/bin/env bash`
# would fail inside it.
cat >"$SECRET_SCAN_OSASCRIPT" <<EOF
#!$BASH
printf '%s\n' "\${@: -1}" >>"$notify_log"
EOF
chmod +x "$SECRET_SCAN_OSASCRIPT"

# A user config that allowlists everything must not reach the scan.
printf '[allowlist]\nregexes = [".*"]\n' >"$work/allow-all.toml"
export GITLEAKS_CONFIG="$work/allow-all.toml"
export GITLEAKS_CONFIG_TOML='[allowlist]
regexes = [".*"]'

# Low-entropy values such as ghp_ followed by repeated FAKE are not flagged by github-pat.
pat="ghp_FAKE$(random 32)"
# generic-api-key drops values containing "fake", so the TSV-only case uses jwt.
jwt="eyJ$(random 30).eyJ$(random 40).FAKE$(random 30)"
old_pat="ghp_FAKE$(random 32)"
link_pat="ghp_FAKE$(random 32)"

# Only two of the six targets exist; the rest must be skipped. The path contains spaces.
pat_file="$HOME/Library/Application Support/Reinloom/orchestration/run 1/log.txt"
mkdir -p "$(dirname "$pat_file")" "$HOME/.claude/projects/p"
# The inline allow comment must be ignored.
printf 'export GITHUB_TOKEN=%s # gitleaks:allow\n' "$pat" >"$pat_file"
# Older than the first-run window.
printf 'export GITHUB_TOKEN=%s\n' "$old_pat" >"$HOME/.claude/projects/p/old.jsonl"
touch -t 202001010000 "$HOME/.claude/projects/p/old.jsonl"
# Symlinks are not followed out of the targets.
printf 'export GITHUB_TOKEN=%s\n' "$link_pat" >"$work/outside.txt"
ln -s "$work/outside.txt" "$HOME/.claude/projects/p/link.jsonl"

tsv() { cat "$SECRET_SCAN_STATE_DIR"/*.tsv 2>/dev/null; }
rows() { tsv | wc -l | tr -d ' '; }
rule_rows() { tsv | awk -F'\t' -v r="$1" '$3 == r' | wc -l | tr -d ' '; }
run() {
  : >"$notify_log"
  "$1" >>"$logs.out" 2>>"$logs.err"
  status=$?
  last_out=$(tail -n 2 "$logs.out")
}

# 1. First run: only the recent token file is scanned.
run "$scan"
check "first run exits 0 (got $status)" '[ "$status" -eq 0 ]'
check "first run writes exactly one row" '[ "$(rows)" -eq 1 ]'
check "the row is github-pat" '[ "$(rule_rows github-pat)" -eq 1 ]'
check "the row names the file with spaces" 'tsv | cut -f1 | grep -qxF "$pat_file"'
check "the row has three fields" '[ "$(tsv | awk -F"\t" "{print NF}")" = 3 ]'
check "notification carries the count" 'grep -qx "1 secret findings; see $SECRET_SCAN_STATE_DIR" "$notify_log"'
check "stamp exists after success" '[ -f "$SECRET_SCAN_STATE_DIR/last-run" ]'

# 2. Nothing changed: nothing scanned, no row, no notification.
run "$scan"
check "second run exits 0 (got $status)" '[ "$status" -eq 0 ]'
check "second run scans no file" 'grep -qx "secret-scan: 0 changed files" <<<"$last_out"'
check "second run adds no row" '[ "$(rows)" -eq 1 ]'
check "second run does not notify" '[ ! -s "$notify_log" ]'

# 3. A file modified after the stamp is rescanned.
sleep 1
touch "$pat_file"
run "$scan"
check "touched file is rescanned" '[ "$(rule_rows github-pat)" -eq 2 ] && [ "$(rows)" -eq 2 ]'
check "rescan notifies" '[ -s "$notify_log" ]'

# 4. A jwt finding, outside the notified rules, goes to the TSV only.
sleep 1
printf 'Authorization: Bearer %s\n' "$jwt" >"$HOME/.claude/projects/p/jwt.jsonl"
run "$scan"
check "jwt run exits 0 (got $status)" '[ "$status" -eq 0 ]'
check "jwt row is written" '[ "$(rule_rows jwt)" -eq 1 ] && [ "$(rows)" -eq 3 ]'
check "jwt does not notify" '[ ! -s "$notify_log" ]'

# 5. gitleaks failure: non-zero exit, stamp untouched, failure notified.
sleep 1
touch "$pat_file"
cp -p "$SECRET_SCAN_STATE_DIR/last-run" "$work/stamp-before"
run "$broken_scan"
check "failure exits non-zero" '[ "$status" -ne 0 ]'
check "failure leaves the stamp untouched" '! [ "$SECRET_SCAN_STATE_DIR/last-run" -nt "$work/stamp-before" ] && ! [ "$work/stamp-before" -nt "$SECRET_SCAN_STATE_DIR/last-run" ]'
check "failure adds no row" '[ "$(rows)" -eq 3 ]'
check "failure is notified" 'grep -q "secret-scan failed" "$notify_log"'

# 6. The next successful run picks up the file the failed run missed.
run "$scan"
check "recovery run rescans the file" '[ "$(rule_rows github-pat)" -eq 3 ]'

# 7. Several changed files in one run, across targets, are all scanned and the stamp advances.
# 201 exceeds the old 200-file batch; gitleaks given several paths scans its cwd instead.
many_run() {
  local count=$1 before_rows n
  before_rows=$(rule_rows github-pat)
  sleep 1
  mkdir -p "$HOME/.config/codex/sessions/many" "$HOME/.claude/transcripts"
  printf 'GITHUB_TOKEN=ghp_FAKE%s\n' "$(random 32)" >"$HOME/.claude/transcripts/t$count.jsonl"
  for ((n = 1; n < count; n++)); do
    printf 'GITHUB_TOKEN=ghp_FAKE%s\n' "$(random 32)" >"$HOME/.config/codex/sessions/many/s$count-$n.jsonl"
  done
  cp -p "$SECRET_SCAN_STATE_DIR/last-run" "$work/stamp-before"
  sleep 1
  run "$scan"
  check "$count-file run exits 0 (got $status)" '[ "$status" -eq 0 ]'
  check "$count-file run reports $count changed files" 'grep -qx "secret-scan: $count changed files" <<<"$last_out"'
  check "$count-file run writes $count github-pat rows" '[ "$(rule_rows github-pat)" -eq $((before_rows + count)) ]'
  check "$count-file run writes no other rule" '[ "$(rows)" -eq $((before_rows + count + 1)) ]'
  check "$count-file run notifies the count" 'grep -qx "$count secret findings; see $SECRET_SCAN_STATE_DIR" "$notify_log"'
  check "$count-file run advances the stamp" '[ "$SECRET_SCAN_STATE_DIR/last-run" -nt "$work/stamp-before" ]'
}
many_run 2
many_run 201

check "no temporary report is left" '[ -z "$(find "$SECRET_SCAN_STATE_DIR" -name "*.json")" ]'
for value in "$pat" "$jwt" "$old_pat" "$link_pat" FAKE; do
  check "TSV and logs do not contain $(printf %.8s "$value")..." \
    '! grep -rqF -- "$value" "$SECRET_SCAN_STATE_DIR" "$logs.out" "$logs.err" "$notify_log"'
done

echo "secret-scan.test: $checked checks, $failures failures"
[ "$checked" -gt 0 ] && [ "$failures" -eq 0 ]
