#!/bin/bash
# Fixture test for redact-secrets.sh.
# Usage: redact-secrets.test.sh <path-to-redact-secrets.sh>
#
# The tool_response fixtures copy the shapes Claude Code 2.1.322 sends for Bash, Read, Grep (three
# output modes), and an MCP tool. Claude Code drops a replacement whose shape differs from the tool's
# output schema, so every redaction case also checks that paths and leaf types are unchanged. Whether
# a newer Claude Code still sends these shapes is outside this test. All tokens are fake.

set -euo pipefail

hook="$1"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

gh_tok='ghp_FAKE0123456789abcdefghijABCDEFGHIJKL'  # gitleaks:allow
gho_tok='gho_FAKEzyxwvutsrqponmlkjihgfedcba987654' # gitleaks:allow
pat_tok='github_pat_FAKE_11ABCDEFG0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMN'
fakes=("$gh_tok" "$gho_tok" "$pat_tok" 'FAKEPASSWORDVALUE' 'FAKEAUTHVALUE' 'FAKENIXVALUE' 'FAKExyzNIX')

# Runs the hook with $2 on stdin in mode $1; sets got, err, rc.
run_hook() {
  rc=0
  got=$(printf '%s' "$2" | bash "$hook" "$1" 2>"$work/err") || rc=$?
  err=$(<"$work/err")
}

no_fakes() {
  local label=$1 text=$2 f
  for f in "${fakes[@]}"; do
    [[ $text != *"$f"* ]] || fail "$label: fake value [$f] survived"
  done
}

# Paths and leaf types, plus every non-string leaf's value, of a JSON document.
shape() {
  jq -c '[paths as $p | [$p, (getpath($p) | type)]] | sort' <<<"$1"
}
non_strings() {
  jq -c '[paths(scalars | select(type != "string")) as $p | [$p, getpath($p)]] | sort' <<<"$1"
}

# expect_redacted MODE LABEL TOOL_NAME TOOL_RESPONSE_JSON RULE...
expect_redacted() {
  checked=$((checked + 1))
  local mode=$1 label=$2 tool=$3 resp=$4
  shift 4
  local payload key out rule
  payload=$(jq -cn --arg t "$tool" --argjson r "$resp" '{hook_event_name:"PostToolUse",tool_name:$t,tool_input:{},tool_response:$r}')
  run_hook "$mode" "$payload"
  [[ $rc -eq 0 ]] || fail "$label: exit $rc"
  [[ -z $err ]] || fail "$label: unexpected stderr"
  no_fakes "$label stdout" "$got"
  if ! jq -e . >/dev/null 2>&1 <<<"$got"; then
    fail "$label: stdout is not JSON"
    return
  fi
  for rule in "$@"; do
    [[ $got == *"[REDACTED:$rule]"* ]] || fail "$label: label [REDACTED:$rule] missing"
  done
  if [[ $mode == codex ]]; then
    jq -e 'keys == ["decision", "reason"] and .decision == "block" and (.reason | type) == "string"' >/dev/null <<<"$got" ||
      fail "$label: codex output has keys or values outside {decision:block, reason:string}: $(jq -c keys <<<"$got")"
    return
  fi
  key=updatedToolOutput
  [[ $tool != mcp__* ]] || key=updatedMCPToolOutput
  jq -e --arg k "$key" '(.hookSpecificOutput | keys | sort) == (["hookEventName", $k] | sort) and .hookSpecificOutput.hookEventName == "PostToolUse" and (keys == ["hookSpecificOutput"])' >/dev/null <<<"$got" ||
    fail "$label: expected only hookSpecificOutput.{hookEventName,$key}, got $(jq -c '.hookSpecificOutput | keys' <<<"$got")"
  out=$(jq -c --arg k "$key" '.hookSpecificOutput[$k]' <<<"$got")
  [[ $(shape "$out") == "$(shape "$resp")" ]] || fail "$label: shape changed: $(shape "$out") vs $(shape "$resp")"
  [[ $(non_strings "$out") == "$(non_strings "$resp")" ]] || fail "$label: non-string values changed"
  # Already-redacted output must not match again, or a second hook pass would mangle the labels.
  run_hook "$mode" "$(jq -cn --arg t "$tool" --argjson r "$out" '{tool_name:$t,tool_response:$r}')"
  [[ -z $got ]] || fail "$label: redacted output matched again: $got"
}

# expect_untouched MODE LABEL TOOL_RESPONSE_JSON
expect_untouched() {
  checked=$((checked + 1))
  local payload
  payload=$(jq -cn --argjson r "$3" '{tool_name:"Bash",tool_input:{},tool_response:$r}')
  run_hook "$1" "$payload"
  [[ $rc -eq 0 && -z $got && -z $err ]] || fail "$2: expected no output and exit 0, got rc=$rc out=[$got] err=[$err]"
}

bash_resp() {
  jq -cn --arg s "$1" '{stdout:$s,stderr:"",interrupted:false,isImage:false,noOutputExpected:false}'
}

# Fixtures in the shapes Claude Code 2.1.322 sends.
expect_redacted claude bash Bash \
  "$(jq -cn --arg s "token: $gh_tok" --arg e "warn $gho_tok" '{stdout:$s,stderr:$e,interrupted:false,isImage:false,noOutputExpected:false}')" \
  github-token
expect_redacted claude read Read \
  "$(jq -cn --arg c $'line1\nTOKEN='"$gh_tok"$'\nline3\n' '{type:"text",file:{filePath:"/w/fake-secret.txt",content:$c,numLines:3,startLine:1,totalLines:3}}')" \
  github-token
expect_redacted claude grep-content Grep \
  "$(jq -cn --arg c "/w/fake-secret.txt:TOKEN=$gh_tok" '{mode:"content",numFiles:0,filenames:[],content:$c,numLines:1,totalLines:1}')" \
  github-token
expect_redacted claude grep-files Grep \
  "$(jq -cn --arg f "/w/$gh_tok.txt" '{mode:"files_with_matches",filenames:["/w/a.txt",$f],numFiles:2,totalFiles:2}')" \
  github-token
expect_redacted claude grep-count Grep \
  "$(jq -cn --arg c "/w/$gh_tok.txt:1" '{mode:"count",numFiles:1,filenames:[],content:$c,numMatches:1}')" \
  github-token
expect_redacted claude mcp mcp__fakesecret__get_fake_token \
  "$(jq -cn --arg t "token is $gh_tok end" '[{type:"text",text:$t}]')" \
  github-token
expect_redacted claude nested Unknown \
  "$(jq -cn --arg a "$pat_tok" --arg b $'protocol=https\npassword=FAKEPASSWORDVALUE\n' \
    '{n:1.5,big:12345678901234,ok:true,none:null,list:[{deep:[$a,7,false]},"plain"],cred:$b}')" \
  github-pat credential-password
expect_redacted claude string-response Bash "$(jq -cn --arg s "x $gh_tok y" '$s')" github-token
expect_redacted claude all-rules-one-string Bash \
  "$(bash_resp $'a '"$gh_tok"' b '"$gho_tok"$'\nAuthorization: token FAKEAUTHVALUE\naccess-tokens = github.com=FAKENIXVALUE gitlab.com=FAKENIXVALUE\npassword=FAKEPASSWORDVALUE')" \
  github-token authorization-header nix-access-tokens credential-password
expect_redacted codex codex-bash Bash "$(bash_resp "out $gh_tok")" github-token
expect_redacted codex codex-string Bash "$(jq -cn --arg s "out $gh_tok" '$s')" github-token
expect_redacted codex codex-mcp mcp__fakesecret__get_fake_token "$(jq -cn --arg t "$gh_tok" '[{type:"text",text:$t}]')" github-token

# Codex's reason is the string itself, not a JSON-quoted copy of it.
checked=$((checked + 1))
run_hook codex "$(jq -cn --arg s "out $gh_tok" '{tool_name:"Bash",tool_response:$s}')"
[[ $(jq -r .reason <<<"$got") == 'out [REDACTED:github-token]' ]] || fail "codex string reason: got [$got]"

# Each rule on its own example (positive) and on a near miss that must stay untouched.
expect_redacted claude rule-github-token Bash "$(bash_resp "$gho_tok")" github-token
expect_redacted claude rule-github-pat Bash "$(bash_resp "$pat_tok")" github-pat
expect_redacted claude rule-nix-access-tokens Bash "$(bash_resp 'NIX_CONFIG=access-tokens=github.com=FAKENIXVALUE')" nix-access-tokens
expect_redacted claude rule-credential-password Bash "$(bash_resp 'password=FAKEPASSWORDVALUE')" credential-password
expect_redacted claude rule-credential-password-crlf Bash "$(bash_resp $'username=x\r\npassword=FAKEPASSWORDVALUE\r\n')" credential-password
expect_redacted claude rule-authorization Bash "$(bash_resp '> authorization:  BEARER FAKEAUTHVALUE')" authorization-header
expect_redacted claude rule-authorization-basic Bash "$(bash_resp 'Authorization: Basic FAKEAUTHVALUE')" authorization-header

expect_untouched claude near-github-short "$(bash_resp 'ghp_short and ghp_FAKE0123456789abcdefghijABCDEFGHIJK')"
expect_untouched claude near-github-prefix "$(bash_resp 'ghx_FAKE0123456789abcdefghijABCDEFGHIJKL')"
expect_untouched claude near-github-pat-short "$(bash_resp 'github_pat_FAKE_11ABCDEFG0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGH')"
expect_untouched claude near-access-tokens-word "$(bash_resp $'access-tokens\nset access-tokens = \n')"
expect_untouched claude near-password-prose "$(bash_resp $'Enter your password: \nuse password=foo in the config\npasswords=\npassword=\n')"
expect_untouched claude near-authorization-no-value "$(bash_resp $'Authorization: Bearer\nAuthorization: Bearer \nAuthorization: Digest abc')"
expect_untouched codex near-codex "$(bash_resp 'nothing here')"
expect_untouched claude no-strings '{"interrupted":false,"n":3,"list":[1,null,true]}'
expect_untouched claude no-response 'null'

# Source text that builds a credential setting or header from a variable or command is not a secret.
expect_untouched claude near-nix-config-source "$(bash_resp '  set -x NIX_CONFIG "access-tokens = github.com="(gh auth token)')"
expect_untouched claude near-nix-config-variable "$(bash_resp $'access-tokens = github.com=$GH_TOKEN\naccess-tokens = github.com=\'x\'')"
expect_untouched claude near-authorization-variable "$(bash_resp $'curl -H "Authorization: Bearer $TOKEN" url\ncurl -H "Authorization: token ${GH_TOKEN}"\nAuthorization: Bearer $(gh auth token)\nAuthorization: Bearer <YOUR_TOKEN>')"
expect_untouched claude near-authorization-prose "$(bash_resp $'HTTP authorization: basic auth is enough\nAuthorization: basic authentication.\nauthorization: token-based\nauthorization: token v2')"

# expect_exact LABEL RAW_PAYLOAD EXPECTED: claude mode, updatedToolOutput.stdout must equal EXPECTED.
expect_exact() {
  checked=$((checked + 1))
  run_hook claude "$2"
  local actual
  actual=$(jq -r '.hookSpecificOutput.updatedToolOutput.stdout' <<<"$got" 2>/dev/null) || actual="<not JSON: $got>"
  [[ $rc -eq 0 && $actual == "$3" ]] || fail "$1: expected [$3], got [$actual]"
  no_fakes "$1" "$got"
}
stdout_payload() {
  jq -cn --arg s "$1" '{tool_name:"Bash",tool_response:{stdout:$s}}'
}

# The value stops before a closing quote, so the surrounding quoting survives.
expect_exact authorization-quoted "$(stdout_payload "curl -H \"Authorization: token $gh_tok\" url")" \
  'curl -H "[REDACTED:authorization-header]" url'
expect_exact nix-access-tokens-pairs "$(stdout_payload "access-tokens = github.com=$gh_tok gitlab.com=FAKExyzNIX")" \
  '[REDACTED:nix-access-tokens]'
expect_exact nix-config-quoted "$(stdout_payload 'NIX_CONFIG="access-tokens = github.com=FAKENIXVALUE" nix build')" \
  'NIX_CONFIG="[REDACTED:nix-access-tokens]" nix build'

# jq rejects a lone high surrogate escape; the retry must still redact, keep a valid pair and an escaped
# backslash before `u` as they are, and turn only the lone ones into U+FFFD.
expect_exact lone-surrogate \
  "{\"tool_name\":\"Bash\",\"tool_response\":{\"stdout\":\"token $gh_tok cut \\ud83d lit \\\\ud83d pair \\ud83d\\ude00 low \\ude00 end \\ud83d\"}}" \
  "token [REDACTED:github-token] cut $(printf '\xef\xbf\xbd') lit \\ud83d pair $(printf '\xf0\x9f\x98\x80') low $(printf '\xef\xbf\xbd') end $(printf '\xef\xbf\xbd')"
checked=$((checked + 1))
run_hook codex "{\"tool_name\":\"Bash\",\"tool_response\":\"$gh_tok \\ud800\"}"
[[ $(jq -r .reason <<<"$got" 2>/dev/null) == "[REDACTED:github-token] $(printf '\xef\xbf\xbd')" ]] ||
  fail "codex lone surrogate: got [$got]"

# A missing or non-string tool_name must not stop the redaction.
for name in '' ',"tool_name":7'; do
  checked=$((checked + 1))
  run_hook claude "{\"tool_response\":\"$gh_tok\"$name}"
  [[ $(jq -r '.hookSpecificOutput.updatedToolOutput' <<<"$got" 2>/dev/null) == '[REDACTED:github-token]' ]] ||
    fail "tool_name [$name]: got [$got]"
done

# Hook failures print nothing that could carry the input and exit 0.
for mode in claude codex; do
  checked=$((checked + 1))
  run_hook "$mode" "{\"tool_name\":\"Bash\",\"tool_response\":{\"stdout\":\"$gh_tok"
  [[ $rc -eq 0 && -z $got ]] || fail "$mode malformed JSON: rc=$rc out=[$got]"
  no_fakes "$mode malformed JSON stderr" "$err"
done

checked=$((checked + 1))
run_hook unknown-mode "{\"tool_name\":\"Bash\",\"tool_response\":\"$gh_tok\"}"
[[ $rc -eq 0 && -z $got && -z $err ]] || fail "unknown mode: rc=$rc out=[$got] err=[$err]"

# Without jq on PATH the hook must still exit 0 silently.
checked=$((checked + 1))
bare_dir="$work/bare"
mkdir "$bare_dir"
rc=0
got=$(printf '%s' "{\"tool_name\":\"Bash\",\"tool_response\":\"$gh_tok\"}" | PATH="$bare_dir" "$BASH" "$hook" claude 2>"$work/err") || rc=$?
err=$(<"$work/err")
[[ $rc -eq 0 && -z $got ]] || fail "no jq on PATH: rc=$rc out=[$got]"
no_fakes "no jq on PATH stderr" "$err"

echo "redact-secrets: $checked cases, $failures failures"
[[ $failures -eq 0 ]]
