#!/bin/bash
# Sends file-tool and Bash events through guard-and-guide with rules.toml and checks which ones it denies,
# for the `guard-and-guide-rules` flake check. Usage: rules.test.sh <guard-and-guide binary> <rules.toml>

set -euo pipefail

gg="$1"
rules="$2"
failures=0
checked=0

event() {
  jq -nc --arg t "$1" --argjson i "$2" '{hook_event_name: "PreToolUse", tool_name: $t, tool_input: $i, session_id: "s", transcript_path: "/x", cwd: "/x"}'
}

# guard-and-guide prints nothing when no rule matches.
decision() {
  local out
  out=$(event "$1" "$2" | "$gg" --config "$rules")
  if [[ -z $out ]]; then echo allow; else jq -r '.hookSpecificOutput.permissionDecision' <<<"$out"; fi
}

expect() {
  checked=$((checked + 1))
  local want="$1" tool="$2" path="$3" got
  got=$(decision "$tool" "$(jq -nc --arg p "$path" '{file_path: $p}')")
  [[ $got == "$want" ]] || {
    echo "FAIL: expected $want, got $got: $tool $path" >&2
    failures=$((failures + 1))
  }
}

expect_cmd() {
  checked=$((checked + 1))
  local want="$1" cmd="$2" got
  got=$(decision Bash "$(jq -nc --arg c "$cmd" '{command: $c}')")
  [[ $got == "$want" ]] || {
    echo "FAIL: expected $want, got $got: Bash $cmd" >&2
    failures=$((failures + 1))
  }
}

# Read, Write, and Edit all reach the `File` rules. MultiEdit and NotebookEdit are not covered by any
# matcher in rules.toml, so they have no cases here.
for tool in Read Write Edit; do
  # .env: `\.env$` matches a file named *.env only; .env.local and similar are not matched.
  expect deny "$tool" '/work/app/.env'
  expect deny "$tool" '.env'
  expect deny "$tool" '/work/app/.ENV'
  expect deny "$tool" '/work/app/prod.Env'
  expect allow "$tool" '/work/app/.env.local'
  expect allow "$tool" '/work/app/.env.example'
  expect allow "$tool" '/work/app/environment.md'

  # Key names are anchored to the file name; any id_<word> is a key, an extension makes it something else.
  expect deny "$tool" '/work/keys/id_rsa'
  expect deny "$tool" 'id_ed25519'
  expect deny "$tool" '/work/keys/id_ecdsa'
  expect deny "$tool" '/work/keys/id_rsa_work'
  expect deny "$tool" '/work/keys/id_dsa'
  expect deny "$tool" '/work/keys/ID_RSA'
  expect allow "$tool" '/work/docs/id_rsa_notes.md'
  expect allow "$tool" '/work/docs/my_id_rsa'
  expect allow "$tool" '/work/keys/id_rsa.pub'
  expect allow "$tool" '/work/keys/id_ed25519.pub'

  # Other credential files.
  expect deny "$tool" '/home/u/.aws/credentials'
  expect deny "$tool" '/home/u/.aws/config'
  expect deny "$tool" '/home/u/.AWS/credentials'
  expect deny "$tool" '/home/u/.git-credentials'
  expect deny "$tool" '/home/u/.netrc'
  expect deny "$tool" '/home/u/.npmrc'
  expect deny "$tool" '/work/app/.npmrc'
  expect allow "$tool" '/work/app/.npmrc.example'
  expect allow "$tool" '/work/app/README.md'
done

# .ssh/ read: config, known_hosts, known_hosts.old and *.pub directly in the directory are readable.
for ssh in .ssh .SSH .Ssh; do
  expect deny Read "/home/u/$ssh/id_ed25519"
  expect deny Read "/home/u/$ssh/id_rsa"
  expect deny Read "/home/u/$ssh/id_ecdsa"
  expect deny Read "/home/u/$ssh/custom_key"
  expect deny Read "/home/u/$ssh/authorized_keys"
  expect deny Read "/home/u/$ssh/config.bak"
  expect deny Read "/home/u/$ssh/xconfig"
  expect deny Read "/home/u/$ssh/known_hosts2"
  expect deny Read "/home/u/$ssh/known_hosts.older"
  expect deny Read "/home/u/$ssh/known_hosts.old.bak"
  expect deny Read "/home/u/$ssh/pub"
  expect deny Read "/home/u/$ssh/key.pub.bak"
  expect deny Read "/home/u/$ssh/sub/id_rsa"
  expect deny Read "/home/u/$ssh/sub/config"
  expect deny Read "/home/u/$ssh/config/../id_rsa"
  expect deny Read "/home/u/$ssh/"
  expect deny Read "/home/u/$ssh/config/.ssh/id_rsa"
  expect allow Read "/home/u/$ssh/config"
  expect allow Read "/home/u/$ssh/known_hosts"
  expect allow Read "/home/u/$ssh/known_hosts.old"
  expect allow Read "/home/u/$ssh/id_ed25519.pub"
  expect allow Read "/home/u/$ssh/id_rsa.pub"
  expect allow Read "/home/u/$ssh/custom_key.pub"
  expect allow Read "/home/u/$ssh/.pub"
done
expect allow Read '/home/u/.sshx'

# .ssh/ write: Write and Edit block every path there, the readable ones included.
for tool in Write Edit; do
  for ssh in .ssh .SSH; do
    expect deny "$tool" "/home/u/$ssh/config"
    expect deny "$tool" "/home/u/$ssh/known_hosts"
    expect deny "$tool" "/home/u/$ssh/known_hosts.old"
    expect deny "$tool" "/home/u/$ssh/id_rsa.pub"
    expect deny "$tool" "/home/u/$ssh/authorized_keys"
    expect deny "$tool" "/home/u/$ssh/id_ed25519"
    expect deny "$tool" "/home/u/$ssh/sub/config"
    expect deny "$tool" "/home/u/$ssh/"
  done
  expect allow "$tool" '/home/u/.sshx'
  expect allow "$tool" '/home/u/notes/ssh.md'
done

# .git/config and .git/hooks/ (and the bare-repo form) are not writable: git runs what they name.
for tool in Write Edit; do
  expect deny "$tool" '/work/app/.git/config'
  expect deny "$tool" '/work/app/.git/hooks/pre-commit'
  expect deny "$tool" '/work/app/.git/hooks/pre-push.sample'
  expect deny "$tool" '/work/repo.git/config'
  expect deny "$tool" '/work/repo.git/hooks/post-merge'
  expect deny "$tool" '/work/app/.git/modules/sub/config'
  expect deny "$tool" '/work/app/.git/modules/sub/hooks/pre-commit'
  expect deny "$tool" '.git/config'
  expect allow "$tool" '/work/app/.gitignore'
  expect allow "$tool" '/work/app/.github/workflows/ci.yml'
  expect allow "$tool" '/work/app/.git/info/exclude'
  expect allow "$tool" '/work/app/gitconfig/notes.md'
  expect allow "$tool" '/work/app/docs/git/hooks.md'
  expect allow "$tool" '/work/app/.config/git/ignore'
done
# Reading them stays possible.
expect allow Read '/work/app/.git/config'
expect allow Read '/work/app/.git/hooks/pre-commit'

# rm on a root, a home directory, or a top-level system directory is blocked; deeper paths pass.
for cmd in 'rm -rf /' 'rm -rf /*' 'rm -rf /.*' 'rm -rf //' 'rm -rf ///' 'rm -rf //*' 'rm -fr /' 'rm -r -f /' 'rm -rf -- /' \
  'rm -rf --no-preserve-root /' 'sudo rm -rf /' '/bin/rm -rf /' 'cd x && rm -rf /' 'true; rm -rf /*' \
  'echo a | xargs rm -rf /' '(rm -rf /)' 'rm -rf "/"' "rm -rf '/'" 'rm -rf /Users' 'rm -rf /Users/' \
  'rm -rf /Users/*' 'rm -rf /home' 'rm -rf /home/*' 'rm -rf /Volumes/' 'rm -rf /var' 'rm -rf /private/var/*' \
  'rm -rf ~' 'rm -rf ~/' 'rm -rf ~/*' 'rm -rf ~/.*' 'rm -rf $HOME' 'rm -rf $HOME/' 'rm -rf $HOME/*' \
  'rm -rf ${HOME}' 'rm -rf "$HOME"' 'rm -rf "$HOME/"*' 'rm -rf /tmp/x /' 'rm -rf a / b' 'rm -rf /;' \
  'rm -rf /&&echo' 'rm -rf / # note'; do
  expect_cmd deny "$cmd"
done
for cmd in 'rm -rf /tmp/x' 'rm -rf /tmp' 'rm -rf /tmp/' 'rm -rf /private/tmp/x' 'rm -rf /var/folders/zz/abc' \
  'rm -rf /var/tmp/x' 'rm -rf /Users/me/work/x' 'rm -rf /home/me/x' 'rm -rf ~/work/x' 'rm -rf $HOME/work/x' \
  'rm -rf ${HOME}/x' 'rm -rf ./build' 'rm -rf build/' 'rm -rf node_modules' 'rm /tmp/file' 'rm -f x.txt' \
  'rm -rf a/b/' 'echo rm -rf x; ls /' 'rm a; ls /' 'rm a;ls /' 'ls /' 'ls -la ~' 'farm -rf /' 'rm -rf /tmpfoo/x'; do
  expect_cmd allow "$cmd"
done

# A Bash event is not a file-tool event: the File rules leave shell commands to the other hooks.
checked=$((checked + 1))
out=$(event Bash '{"command": "cat /home/u/.ssh/id_rsa"}' | "$gg" --config "$rules")
[[ -z $out ]] || {
  echo "FAIL: a Bash command was decided by a File rule: $out" >&2
  failures=$((failures + 1))
}

if ((failures > 0)); then
  echo "$failures of $checked cases failed" >&2
  exit 1
fi
echo "all $checked cases passed"
