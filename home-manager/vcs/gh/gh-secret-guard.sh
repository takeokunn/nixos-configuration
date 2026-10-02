# Stands in for gh and refuses to run it when what gh would send contains a secret. Checked are
# every argument (already expanded by the shell), every regular file an argument names, standard
# input when gh reads it, and the commit and tag messages gh turns into a body. Two checks:
# gitleaks' built-in rules, and an exact match against the secrets this machine actually holds
# (every gh account's token and environment variables named like a secret), which catches values
# whose format gitleaks does not know.
#
# auth and secret pass through: they carry credentials to GitHub by design, and git's credential
# helper runs `gh auth git-credential` on every fetch and push. Fails closed, with no override.
#
# Not covered: a secret split across words, gh extensions, and commits that gh pushes, which are
# git's to check.

set -uo pipefail

real_gh=@GH@
gitleaks=@GITLEAKS@
gitleaks_config=@GITLEAKS_CONFIG@
jq=@JQ@
git=@GIT@

block() {
  cat >&2 <<EOF
gh refused to run (gh-secret-guard): $1

Write the name of the environment variable or credential store instead of its value, and never
paste executed command lines or shell output into a body, comment, or field. If this is wrong,
stop and ask the user.
EOF
  exit 1
}

case "${1-}" in
"" | auth | secret | help | completion | version | --version | -h | --help)
  exec "$real_gh" "$@"
  ;;
esac
# An alias runs later with arguments this guard never sees.
[[ ${1-} == alias && (${2-} == set || ${2-} == import) ]] && block "gh aliases are not allowed; run the full command"

work=$(mktemp -d "${TMPDIR:-/tmp}/gh-secret-guard.XXXXXX") || block "could not create a temporary directory"
trap 'rm -rf "$work"' EXIT
content="$work/content"
printf '%s\n' "$@" >"$content" || block "could not record the arguments"
append() { cat "$@" >>"$content" || block "could not read what gh would send"; }

# Paths given as themselves or after `=`, `@`, a glued short flag, or before a release asset's
# `#label`. Checking one that gh does not upload costs nothing.
files=()
reads_stdin=0
for arg in "$@"; do
  glued=""
  [[ $arg =~ ^-[A-Za-z] ]] && glued=${arg:2}
  for path in "$arg" "${arg#*=}" "${arg#*@}" "$glued" "${arg%%#*}"; do
    if [[ $path == - || $path == /dev/stdin ]]; then
      reads_stdin=1
    elif [[ -n $path && -e $path && ! -d $path ]]; then
      [[ -f $path ]] || block "$path is not a regular file; write the content to a file and pass that"
      files+=("$path")
    fi
  done
done
# gist create with no file to upload reads its content from standard input.
[[ "${1-} ${2-}" == "gist create" && ${#files[@]} -eq 0 ]] && reads_stdin=1

# gh gets a copy of standard input, so the bytes checked are the bytes sent. A terminal is left
# alone: that is a person typing.
stdin_copy=""
if ((reads_stdin)) && [[ ! -t 0 ]]; then
  stdin_copy="$work/stdin"
  cat >"$stdin_copy" || block "could not read standard input"
  append "$stdin_copy"
fi
((${#files[@]} == 0)) || append -- "${files[@]}"

# Text gh can build from git: commit messages (pr create --fill and its spellings) and tag messages
# (release --notes-from-tag). Read whenever the subcommand could use them, since matching the flags
# misses combined shorthands like -fd. Outside a repository there is nothing to read and gh fails
# anyway.
if [[ ${1-} == pr && " $* " == *" create "* ]]; then
  "$git" log -n 100 --format=%B >>"$content" 2>/dev/null
fi
if [[ ${1-} == release && (" $* " == *" create "* || " $* " == *" edit "*) ]]; then
  "$git" tag -l --format='%(contents)' >>"$content" 2>/dev/null
fi

# Lines of at least 16 characters: shorter ones (a placeholder, "true", a blank line in a multi-line
# value) would block ordinary text.
secrets=()
add_secret() {
  local line
  while IFS= read -r line; do
    ((${#line} >= 16)) && secrets+=("$line")
  done <<<"$1"
}
hosts_file="${GH_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}/gh}/hosts.yml"
if [[ -r $hosts_file ]]; then
  host=""
  while IFS= read -r line; do
    if [[ $line =~ ^([^[:space:]#][^:]*):[[:space:]]*$ ]]; then
      host=${BASH_REMATCH[1]}
    elif [[ -n $host && $line =~ ^\ {8}([^[:space:]:]+):[[:space:]]*$ ]]; then
      account=${BASH_REMATCH[1]}
      held=$("$real_gh" auth token -h "$host" -u "$account" 2>/dev/null) ||
        block "could not read the token of $account on $host to check against it"
      add_secret "$held"
    fi
  done <"$hosts_file"
fi
# `export -p` rather than `compgen -e`: the non-interactive bash this runs under is built without
# programmable completion, so compgen does not exist.
shopt -s nocasematch
while IFS= read -r line; do
  [[ $line =~ ^declare\ -x\ ([A-Za-z_][A-Za-z0-9_]*)= ]] || continue
  name=${BASH_REMATCH[1]}
  [[ $name =~ (TOKEN|SECRET|PASSWORD|PASSWD|API_?KEY|PRIVATE_?KEY|CREDENTIAL) ]] && add_secret "${!name}"
done < <(export -p)
shopt -u nocasematch
for s in "${secrets[@]+"${secrets[@]}"}"; do
  # The value goes in on stdin rather than argv, where `ps` would show it.
  grep -aqF -f - "$content" <<<"$s"
  case $? in
  0) block "what gh would send contains the value of a credential held on this machine. The value is not shown." ;;
  1) ;;
  *) block "the credential check did not complete" ;;
  esac
done

# Leaving the caller's directory keeps a .gitleaksignore there from applying.
report=$(cd / && "$gitleaks" stdin --no-banner --no-color --redact --log-level error \
  --ignore-gitleaks-allow --exit-code 10 --report-format json --report-path - \
  --config "$gitleaks_config" <"$content")
status=$?
if ((status == 10)); then
  rules=$(printf '%s' "$report" | "$jq" -r '[.[].RuleID] | unique | join(", ")' 2>/dev/null)
  block "gitleaks found a secret (${rules:-unknown rule}) in what gh would send. The value is not shown."
elif ((status != 0)); then
  block "the secret scan did not complete (gitleaks exit $status), so nothing was sent"
fi

if [[ -n $stdin_copy ]]; then
  "$real_gh" "$@" <"$stdin_copy"
else
  "$real_gh" "$@"
fi
