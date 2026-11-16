#!/bin/bash
# Table-driven test for gh-secret-guard.sh, run by the `gh-secret-guard` flake check.
# Usage: gh-secret-guard.test.sh <guard built against stub-gh> <the same guard with a missing gitleaks>
#
# The stub gh records its arguments, standard input, and GH_TOKEN under $STUB_LOG, and exits with
# $STUB_EXIT. For `auth token` it prints $STUB_TOKEN for the account `someone` (or no -u) and
# $STUB_TOKEN-<account> for any other. Fake secrets are generated at run time: a literal one in
# this file would be rejected by the repository's gitleaks pre-commit hook.

set -uo pipefail

# The original egress checks apply to people too; agent-only policy is exercised separately below.
unset CLAUDECODE CODEX_THREAD_ID

guard="$1"
broken_guard="$2"
failures=0
checked=0

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

work=$(mktemp -d)
trap 'chmod -R u+rwx "$work"; rm -rf "$work"' EXIT
cd "$work" || exit 1
export STUB_LOG="$work/stub"
mkdir -p "$work/home"
export HOME="$work/home"
export GH_TOKEN=fixture-gh-token-not-a-credential

random() { perl -e 'my @c = ("A" .. "Z", "a" .. "z", 0 .. 9); print map { $c[rand @c] } 1 .. shift' "$1"; }

# gitleaks' github-oauth rule needs gho_ plus 36 alphanumerics above an entropy threshold.
token="gho_$(random 36)"
# Formats gitleaks does not flag, caught only by the exact-match check.
export STUB_TOKEN="plainaccounttoken$(random 8)"
export MY_SERVICE_API_KEY="plainservicekey$(random 8)"

mkdir -p ghconfig
export GH_CONFIG_DIR="$work/ghconfig"
printf 'github.com:\n    users:\n        someone:\n    user: someone\n' >ghconfig/hosts.yml

printf 'Verification: NODE_AUTH_TOKEN=%s pnpm install\n' "$token" >leaky.md
printf 'Verification: pnpm install exited 0\n' >clean.md
cp clean.md unreadable.md
chmod 000 unreadable.md

# Sets $status; stderr goes to $work/err.
run() {
  local input=$1
  shift
  rm -rf "$STUB_LOG"
  status=0
  "$guard" "$@" <"$input" >/dev/null 2>"$work/err" || status=$?
}

expect_block() {
  local input=$1
  shift
  checked=$((checked + 1))
  run "$input" "$@"
  [[ $status -eq 1 ]] || fail "expected block (exit 1) for [$*], got $status"
  grep -q 'gh refused to run' "$work/err" || fail "block for [$*] printed no banner"
  [[ ! -e $STUB_LOG/argv ]] || fail "block for [$*] still ran gh"
  for s in "$token" "$STUB_TOKEN" "$MY_SERVICE_API_KEY"; do
    if grep -qF "$s" "$work/err"; then fail "block for [$*] echoed a secret"; fi
  done
}

expect_allow() {
  local input=$1
  shift
  checked=$((checked + 1))
  run "$input" "$@"
  [[ $status -eq 0 ]] || fail "expected allow for [$*], got $status: $(cat "$work/err")"
  [[ -e $STUB_LOG/argv ]] || fail "allow for [$*] did not run gh"
}

# --- Secrets in arguments (gitleaks) ---

expect_block /dev/null pr review 77 --comment --body "Verification: NODE_AUTH_TOKEN=$token pnpm install"
grep -q 'github-oauth' "$work/err" || fail "gitleaks block does not name the rule"
expect_block /dev/null api repos/o/r/issues/1/comments -f "body=$token"
expect_block /dev/null issue create --title "$token" --body x
expect_block /dev/null pr comment 1 --body "$token # gitleaks:allow"

# --- Secrets this machine holds (exact match) ---

expect_block /dev/null pr comment 1 --body "token: $STUB_TOKEN"
grep -q 'credential held on this machine' "$work/err" || fail "exact-match block does not explain itself"
expect_block /dev/null api user -H "Authorization: Bearer $MY_SERVICE_API_KEY"
printf 'key %s\n' "$MY_SERVICE_API_KEY" >held.md
expect_block /dev/null pr comment 1 --body-file held.md

# --- Files gh reads ---

expect_block /dev/null pr review 77 --body-file leaky.md
expect_block /dev/null pr review 77 --body-file=leaky.md
expect_block /dev/null pr create -F leaky.md --title t
expect_block /dev/null pr create -Fleaky.md --title t
expect_block /dev/null pr create -d -F leaky.md --title t
expect_block /dev/null issue comment 1 -F "$work/leaky.md"
expect_block /dev/null release create v1 --notes-file leaky.md
expect_block /dev/null release upload v1 "leaky.md#notes"
expect_block /dev/null api repos/o/r/issues/1/comments -F body=@leaky.md
expect_block /dev/null api gists -F 'files[x][content]=@leaky.md'
expect_block /dev/null api repos/o/r/issues/1/comments --input leaky.md
expect_block /dev/null gist create clean.md leaky.md
expect_block /dev/null gist edit abc123 -a leaky.md

# --- Standard input gh reads ---

expect_block leaky.md pr comment 1 --body-file -
expect_block leaky.md pr comment 1 -F -
expect_block leaky.md pr comment 1 --body-file=-
expect_block leaky.md pr comment 1 -F-
expect_block leaky.md pr comment 1 --body-file=/dev/stdin
expect_block leaky.md pr comment 1 --body-file /dev/stdin
expect_block leaky.md api repos/o/r/issues/1/comments -F body=@-
expect_block leaky.md api repos/o/r/issues/1/comments --input -
expect_block leaky.md gist create
expect_block leaky.md gist create -f notes.md

# --- Commit and tag messages gh can turn into a body, however the flags are spelled ---

# Isolated from the caller's git config, which may require hooks or commit signing.
commit() {
  git -C "$1" -c user.name=t -c user.email=t@example.com -c core.hooksPath=/dev/null -c commit.gpgsign=false \
    commit -q --allow-empty -m "$2" || fail "could not create a fixture commit in $1"
}
git init -q repo
git init -q clean-repo
commit repo "debug: NODE_AUTH_TOKEN=$token"
commit clean-repo "chore: clean"
git -C repo -c user.name=t -c user.email=t@example.com -c tag.gpgsign=false tag -a v1 -m "debug: NODE_AUTH_TOKEN=$token"

cd repo || exit 1
expect_block /dev/null pr create --fill
expect_block /dev/null pr create -fd
expect_block /dev/null pr -R o/r create --fill
expect_block /dev/null release create v1 --notes-from-tag
expect_block /dev/null release create v1 --notes-from-tag=true
expect_allow /dev/null pr view 1
cd ../clean-repo || exit 1
expect_allow /dev/null pr create --fill
cd "$work" || exit 1

# --- Aliases run later with arguments the guard never sees ---

expect_block /dev/null alias set p "api --input leaky.md"
expect_block /dev/null alias import aliases.yml

# --- Fail closed ---

expect_block /dev/null pr comment 1 --body-file unreadable.md
expect_block /dev/null pr comment 1 --body-file <(cat clean.md)
checked=$((checked + 1))
status=0
"$broken_guard" pr comment 1 --body "pnpm install exited 0" </dev/null >/dev/null 2>"$work/err" || status=$?
[[ $status -eq 1 ]] || fail "expected the broken scanner to block, got $status"
grep -q 'did not complete' "$work/err" || fail "broken scanner block did not explain itself"

# --- Allowed, and passed through unchanged ---

expect_allow /dev/null pr comment 1 --body "pnpm install exited 0"
[[ $(cat "$STUB_LOG/argv") == "$(printf '%s\n' pr comment 1 --body "pnpm install exited 0")" ]] ||
  fail "arguments were not passed through unchanged"

expect_allow /dev/null pr review 77 --body-file clean.md
expect_allow clean.md pr comment 1 --body-file -
cmp -s clean.md "$STUB_LOG/stdin" || fail "standard input did not reach gh intact"
expect_allow clean.md pr comment 1 --body-file /dev/stdin
cmp -s clean.md "$STUB_LOG/stdin" || fail "/dev/stdin did not reach gh intact"
expect_allow clean.md gist create
cmp -s clean.md "$STUB_LOG/stdin" || fail "gist create did not receive its standard input"

expect_allow /dev/null pr comment 1 --body-file missing.md
expect_allow /dev/null pr view 77 --json body --jq .body
expect_allow /dev/null api repos/o/r -f "title=fix: refresh token handling"
# Short and multi-line values of secret-named variables must not block ordinary text.
MY_SHORT_TOKEN=abc MULTI_LINE_TOKEN=$(printf "first line of a value\n\nlast line") expect_allow /dev/null pr comment 1 --body "abc and hello"

# With a file argument gist create does not read stdin, so the guard must not either: reading it
# would stall on a pipe that stays open. The leaky stdin proves it was left unread.
expect_allow leaky.md gist create clean.md

expect_allow /dev/null pr comment 1 --body 'Set it with NODE_AUTH_TOKEN=$(gh auth token), never the value'

# auth and secret carry credentials by design and are not scanned.
printf '%s\n' "$token" >token.txt
expect_allow token.txt auth login --with-token
cmp -s token.txt "$STUB_LOG/stdin" || fail "auth did not receive its standard input"
expect_allow /dev/null secret set NAME --body "$token"

# gh's exit status reaches the caller.
checked=$((checked + 1))
status=0
STUB_EXIT=4 "$guard" pr view 1 </dev/null >/dev/null 2>&1 || status=$?
[[ $status -eq 4 ]] || fail "expected gh's exit status 4 to pass through, got $status"

# D1: post-expansion argv policy, with both runtime markers and a human control.
expect_allow /dev/null pr merge 1
expect_allow /dev/null config get oauth_token -h github.com
printf 'aliases:\n  m: pr merge\n' >"$GH_CONFIG_DIR/config.yml"
printf '{"query":"query{viewer{login}}"}\n' >query.json
# Generate JSON with escaped mutation spelling without relying on shell escape interpretation.
printf '%s\n' '{"query":"mutation{merge\u0050ullRequest{clientMutationId}}"}' >escaped.json
printf '%s\n' 'mutation{mergePullRequest{clientMutationId}}' >merge.graphql
for marker in CLAUDECODE CODEX_THREAD_ID; do
  export "$marker=fixture"
  while IFS=$'\t' read -r expected line; do
    read -r -a argv <<<"$line"
    if [[ $expected == stdin ]]; then
      expect_block escaped.json "${argv[@]}"
    elif [[ $expected == allow ]]; then
      expect_allow /dev/null "${argv[@]}"
    else
      expect_block /dev/null "${argv[@]}"
    fi
  done <<'CASES'
allow	pr create --title t --body b
allow	pr create --title merge-fix --body b
allow	pr comment 1 --body hi
allow	pr view 1
allow	pr list --search merge
allow	repo view o/r
allow	issue create --title t --body b
allow	api repos/o/r/pulls
allow	api repos/o/r/pulls/1/comments -f body=x
allow	api -X POST repos/o/r/pulls/1/reviews -f event=COMMENT
allow	api repos/o/r/pulls/1/comments/2/replies -f body=x
allow	api repos/{owner}/{repo}/pulls/1/comments -f body=x
allow	api -X PATCH repos/o/r/issues/comments/5 -f body=x
allow	api graphql -f query=query{viewer{login}}
block	api graphql?query=mutation%7B%6dergePullRequest%7BclientMutationId%7D%7D
allow	api graphql -f query=mutation{resolveReviewThread(input:{threadId:"x"}){clientMutationId}}
allow	api graphql --input query.json
block	m 1
block	probe pr merge 1
allow	ext list
block	copilot
block	extension exec probe
block	--repo o/r pr merge 1
block	pr merge 1
block	pr merge --auto --squash 1
block	pr -R o/r merge 1
block	pr --repo o/r merge 1
block	pr --body x merge 1
block	pr -b x merge 1
block	pr -t s merge 1
block	pr --subject s merge 1
block	pr -F f merge 1
block	pr --squash 1 merge
block	repo --yes o/r delete
block	auth -u USER token
block	auth -h github.com token
block	alias --clobber m set pr
block	extension --force . install
block	ext --x probe exec
block	pr --repo
block	pr --repo= merge 1
block	pr --hostname
block	pr --unknown=value view
block	pr -Ro/r merge 1
block	pr --repo=o/r merge 1
allow	pr -R o/r view 1
allow	pr --repo=o/r view 1
allow	pr -Ro/r view 1
allow	pr --hostname github.com view 1
allow	pr --hostname=github.com view 1
allow	pr -h
allow	pr --help
block	repo delete o/r --yes
block	api -X PUT repos/o/r/pulls/1/merge
block	api --method=put repos/o/r/pulls/1/merge
block	api -XPUT repos//o/r/pulls/1/merge
block	api -X PUT repos/o/r/./pulls/1/merge
block	api -X PUT repos/o/x/../r/pulls/1/merge
block	api -X PUT repositories/123/pulls/1/merge
block	api -X PUT repos/o/r/pulls/1/%6derge
block	api https://api.github.com/repos/o/r/pulls/1/merge -X PUT
block	api -X DELETE repos/o/r
block	api -X DELETE //repos/o/r
block	api -X POST repos/o/r/merges -f base=main -f head=x
block	api graphql -f query=mutation{mergePullRequest(input:{pullRequestId:"x"}){clientMutationId}}
block	api graphql -f query=mutation{enablePullRequestAutoMerge(input:{pullRequestId:"x"}){clientMutationId}}
block	api graphql -f query=mutation{enqueuePullRequest(input:{pullRequestId:"x"}){clientMutationId}}
block	api graphql -f query=mutation{deleteRepository(input:{repositoryId:"x"}){clientMutationId}}
block	api graphql -F query=@merge.graphql
block	api graphql --input escaped.json
stdin	api graphql --input -
block	ext exec probe
block	ext install probe
block	extension upgrade
block	alias set m pr
block	alias import missing.yml
allow	alias list
allow	extension list
allow	cs list
block	auth token
block	auth status -t
block	auth status --show-token
allow	auth status
allow	auth git-credential
block	api graphql --input missing.json
block	api graphql --input clean.md
block	api graphql -F query=@missing.graphql
block	api --method
block	api --unrecognised=value repos/o/r
block	api -X=PUT repos/o/r/pulls/1/merge
allow	api -XPOST repos/o/r/git/tags -f tag=v1
allow	api -XHEAD repos/o/r
block	api graphql -F query=mutation{merge{repo}{clientMutationId}}
block	api graphql -Fquery=mutation{delete{repo}{clientMutationId}}
block	api graphql -F=query=mutation{merge{owner}{clientMutationId}}
block	api graphql --field query=mutation{enable{repo}AutoMerge{clientMutationId}}
block	api graphql --field=query=mutation{merge{branch}{clientMutationId}}
block	api -F query=mutation{merge{repo}{clientMutationId}} graphql
block	api graphql -F query=mutation{merge:repo(input:{}){clientMutationId}}
allow	api graphql -f query=mutation{merge{repo}{clientMutationId}}
allow	api graphql -fquery=mutation{merge{owner}{clientMutationId}}
allow	api graphql --raw-field=query=mutation{merge{branch}{clientMutationId}}
allow	api repos/o/r/issues -F title={repo}
block	config get oauth_token
block	config get oauth_token -h github.com
block	config get -h github.com oauth_token
block	config get --host=github.com oauth_token
block	config get -hgithub.com oauth_token
block	config get -- oauth_token
block	config --host github.com get oauth_token
allow	config get git_protocol -h github.com
allow	config get --host github.com git_protocol
CASES
  expect_allow query.json api graphql --input -
  cmp -s query.json "$STUB_LOG/stdin" || fail "GraphQL stdin changed before reaching gh"
  expect_block /dev/null pr comment 1 --body "$token"
  expect_block /dev/null -R o/r pr view 1
  grep -q 'options before the gh command' "$work/err" || fail "a pre-command option is blocked with the wrong reason"
  # A merge passes only with the approval prefix, and never with --admin or through gh api.
  expect_block /dev/null pr merge 1
  grep -q 'ALLOW_GH_MERGE=1' "$work/err" || fail "the merge block does not show the approval prefix"
  ALLOW_GH_MERGE=1 expect_allow /dev/null pr merge 1
  ALLOW_GH_MERGE=1 expect_allow /dev/null pr merge --squash --delete-branch 1
  ALLOW_GH_MERGE=1 expect_allow /dev/null pr merge --auto --squash 1
  ALLOW_GH_MERGE=1 expect_allow /dev/null pr -R o/r merge 1
  ALLOW_GH_MERGE=0 expect_block /dev/null pr merge 1
  ALLOW_GH_MERGE=yes expect_block /dev/null pr merge 1
  ALLOW_GH_MERGE=1 expect_block /dev/null pr merge --admin 1
  grep -q 'bypasses branch protection' "$work/err" || fail "the --admin block does not explain itself"
  ALLOW_GH_MERGE=1 expect_block /dev/null pr merge 1 --admin=true
  ALLOW_GH_MERGE=1 expect_block /dev/null pr merge 1 --body "$token"
  ALLOW_GH_MERGE=1 expect_block /dev/null api -X PUT repos/o/r/pulls/1/merge
  ALLOW_GH_MERGE=1 expect_block /dev/null api graphql -f 'query=mutation{mergePullRequest(input:{pullRequestId:"x"}){clientMutationId}}'
  ALLOW_GH_MERGE=1 expect_block /dev/null repo delete o/r --yes
  # gh's own help example: placeholders in typed variables, the document in a raw field.
  expect_allow /dev/null api graphql -F owner={owner} -F name={repo} -f query='query($name: String!, $owner: String!) { repository(owner: $owner, name: $name) { releases(last: 3) { nodes { tagName } } } }'
  unset "$marker"
done

# --- The account follows the target repository's owner ---

# An account named <another account>-<owner> serves that owner's repositories; the rest keep the
# active one.
mkdir -p accounts
export GH_CONFIG_DIR="$work/accounts"
printf 'github.com:\n    users:\n        someone:\n        someone-example-org:\n    user: someone\n' >accounts/hosts.yml
unset GH_TOKEN GITHUB_TOKEN GH_REPO
org_token="$STUB_TOKEN-someone-example-org"

# Expects gh to run with GH_TOKEN set to $1 (empty: unset).
expect_token() {
  local want=$1
  shift
  expect_allow "$@"
  [[ -e $STUB_LOG/gh_token && $(cat "$STUB_LOG/gh_token") == "$want" ]] ||
    fail "expected GH_TOKEN [$want] for [${*:2}], got [$(cat "$STUB_LOG/gh_token" 2>/dev/null)]"
}

git init -q org-repo
git -C org-repo remote add origin git@github.com:example-org/x.git
git init -q upper-repo
git -C upper-repo remote add origin ssh://git@github.com/Example-Org/x
git init -q other-repo
git -C other-repo remote add origin https://github.com/other/x

expect_token "$org_token" /dev/null pr view 1 -R example-org/x
expect_token "$org_token" /dev/null pr -R example-org/x view 1
expect_token "$org_token" /dev/null pr view 1 -Rexample-org/x
expect_token "$org_token" /dev/null pr view 1 --repo=https://github.com/example-org/x
expect_token "$org_token" /dev/null pr view 1 --repo github.com/example-org/x
expect_token "$org_token" /dev/null api repos/example-org/x/pulls
expect_token "$org_token" /dev/null api /orgs/example-org/repos
expect_token "$org_token" /dev/null api -X POST repos/example-org/x/issues -f title=t
expect_token "$org_token" /dev/null api https://api.github.com/repos/example-org/x
expect_token "$org_token" /dev/null pr view https://github.com/example-org/x/pull/1
expect_token "$org_token" /dev/null repo view example-org/x --json nameWithOwner
expect_token "$org_token" /dev/null repo clone example-org/x.git dir
expect_token "" /dev/null repo view other/x
expect_token "" /dev/null pr view 1 --jq example-org/x
GH_REPO=example-org/x expect_token "$org_token" /dev/null pr view 1
expect_token "" /dev/null pr view 1 -R other/x
expect_token "" /dev/null pr view 1 -R ghe.example.com/example-org/x
expect_token "" /dev/null api user
expect_token "" /dev/null api repos/example-org-two/x
# someone-example-org is someone-example plus -org, but no account someone-example is logged in.
expect_token "" /dev/null api repos/org/x
expect_token "" /dev/null auth status
GH_TOKEN=caller-token expect_token caller-token /dev/null pr view 1 -R example-org/x
GITHUB_TOKEN=caller-token expect_token "" /dev/null pr view 1 -R example-org/x
CLAUDECODE=fixture expect_token "$org_token" /dev/null pr -R example-org/x view 1
CLAUDECODE=fixture ALLOW_GH_MERGE=1 expect_token "$org_token" /dev/null pr -R example-org/x merge 1

cd org-repo || exit 1
expect_token "$org_token" /dev/null pr view 1
expect_token "$org_token" /dev/null api 'repos/{owner}/{repo}/pulls'
expect_token "" /dev/null pr view 1 -R other/x
cd ../upper-repo || exit 1
expect_token "$org_token" /dev/null pr view 1
cd ../other-repo || exit 1
expect_token "" /dev/null pr view 1
expect_token "$org_token" /dev/null pr view 1 -R example-org/x
cd "$work" || exit 1

# git's credential request names the repository path when credential.useHttpPath is set.
printf 'protocol=https\nhost=github.com\npath=example-org/x.git\n\n' >cred-org
printf 'protocol=https\nhost=github.com\npath=other/x.git\n\n' >cred-other
printf 'protocol=https\nhost=github.com\n\n' >cred-nopath
printf 'protocol=https\nhost=gist.github.com\npath=example-org/x.git\n\n' >cred-gist
expect_token "$org_token" cred-org auth git-credential get
cmp -s cred-org "$STUB_LOG/stdin" || fail "the credential request did not reach gh intact"
printf 'protocol=https\nhost=github.com\npath=example-org/x.git' >cred-unterminated
expect_token "$org_token" cred-unterminated auth git-credential get
expect_token "" cred-other auth git-credential get
expect_token "" cred-nopath auth git-credential get
expect_token "" cred-gist auth git-credential get
expect_token "" cred-org auth git-credential store
CLAUDECODE=fixture expect_token "$org_token" cred-org auth git-credential get

# Two accounts for one owner is ambiguous, so nothing runs rather than a guess.
printf '        other:\n        other-example-org:\n' >>accounts/hosts.yml
expect_block /dev/null pr view 1 -R example-org/x
grep -q 'ambiguous' "$work/err" || fail "the ambiguous-account block does not explain itself"
expect_block cred-org auth git-credential get
expect_token "" /dev/null pr view 1 -R other/x

if [[ $failures -gt 0 ]]; then
  echo "$failures failures across $checked cases" >&2
  exit 1
fi
[[ $checked -gt 0 ]] || {
  echo "FAIL: no cases ran" >&2
  exit 1
}
echo "all $checked cases passed"
