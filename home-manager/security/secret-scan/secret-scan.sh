#!/usr/bin/env bash
# Scans agent conversation logs and shell history changed since the previous run with gitleaks.
# Writes only File, StartLine and RuleID to $STATE_DIR/<date>.tsv and notifies the count of
# high-confidence findings. Secret values are never written: the report is redacted and only
# three fields are extracted from it.
#
# Environment: HOME selects the targets and the default state directory. SECRET_SCAN_STATE_DIR
# and SECRET_SCAN_OSASCRIPT override the state directory and the notifier (tests stub both).

set -euo pipefail
umask 077
export PATH="@PATH@"

gitleaks="@GITLEAKS@"
gitleaks_config="@GITLEAKS_CONFIG@"
osascript="${SECRET_SCAN_OSASCRIPT:-/usr/bin/osascript}"
state_dir="${SECRET_SCAN_STATE_DIR:-$HOME/.local/state/secret-scan}"
stamp="$state_dir/last-run"
# First run without a stamp: a full scan takes over 20 minutes, so only the last day is scanned.
first_run_minutes=1440

targets=(
  "$HOME/.claude/projects"
  "$HOME/.claude/transcripts"
  "$HOME/.config/codex/sessions"
  "$HOME/.config/codex/shell_snapshots"
  "$HOME/Library/Application Support/Reinloom/orchestration"
  "$HOME/.local/share/fish/fish_history"
)

notify_rules='["github-oauth","github-pat","github-app-token","github-fine-grained-pat","private-key","aws-access-token","hashicorp-tf-api-token","slack-webhook-url"]'

notify() {
  # The message is passed as an argument so its text is never parsed as AppleScript.
  "$osascript" -e 'on run argv' -e 'display notification (item 1 of argv) with title "secret-scan"' -e 'end run' "$1"
}

mkdir -p "$state_dir"
work=$(mktemp -d "$state_dir/.run.XXXXXX")
trap 'rm -rf "$work"' EXIT
# gitleaks reads .gitleaksignore from the working directory; run from an empty one.
cd "$work"

# Created before selecting files, so files modified during the scan are newer than the next
# run's stamp and get rescanned.
touch "$work/stamp"

newer=(-mmin "-$first_run_minutes")
[ -e "$stamp" ] && newer=(-newer "$stamp")

: >"$work/files"
for target in "${targets[@]}"; do
  [ -e "$target" ] || continue
  # -type f excludes symlinks, so nothing outside the targets is read.
  find "$target" -type f "${newer[@]}" -print0 >>"$work/files"
done
mapfile -d '' -t files <"$work/files"
echo "secret-scan: ${#files[@]} changed files"

fail() {
  echo "secret-scan: $1" >&2
  notify "secret-scan failed; see $state_dir/stderr.log" || true
  exit 1
}

# Own config with built-in rules only: --config takes precedence over GITLEAKS_CONFIG and a
# target's .gitleaks.toml, and the user's config may carry allowlists that would hide findings.
unset GITLEAKS_CONFIG GITLEAKS_CONFIG_TOML
: >"$work/reports"
for file in "${files[@]}"; do
  # One file per call: given several paths, gitleaks 8.30.1 `dir` ignores them and scans the
  # working directory. A single path also keeps that path in the report's File field.
  # --exit-code 0 makes any non-zero status an error rather than "leaks found".
  rm -f "$work/report.json"
  "$gitleaks" dir --no-banner --log-level warn --config "$gitleaks_config" \
    --gitleaks-ignore-path "$work" --ignore-gitleaks-allow --redact=100 --report-format json \
    --report-path "$work/report.json" --exit-code 0 "$file" ||
    fail "gitleaks exited $? on a changed file; stamp not updated"
  cat "$work/report.json" >>"$work/reports" || fail "gitleaks wrote no report; stamp not updated"
done

high=0
if [ "${#files[@]}" -gt 0 ]; then
  jq -s 'add' "$work/reports" >"$work/report.json" || fail "could not parse the gitleaks report"
  jq -r '.[] | [.File, (.StartLine | tostring), .RuleID] | @tsv' "$work/report.json" \
    >>"$state_dir/$(date +%F).tsv"
  high=$(jq --argjson rules "$notify_rules" '[.[] | select(.RuleID as $r | $rules | index($r))] | length' "$work/report.json")
  echo "secret-scan: $(jq length "$work/report.json") findings, $high notifiable"
fi

# The stamp advances only after the notification, so a failed one is retried by rescanning the
# same files; the rescan appends their TSV rows again.
if [ "$high" -gt 0 ]; then
  notify "$high secret findings; see $state_dir" || {
    echo "secret-scan: notification failed; stamp not updated" >&2
    exit 2
  }
fi

mv -f "$work/stamp" "$stamp"
