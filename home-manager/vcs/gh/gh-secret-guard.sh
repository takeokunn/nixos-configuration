# Stands in for gh and refuses to run it when what gh would send contains a secret. Checked are
# every argument (already expanded by the shell), every regular file an argument names, standard
# input when gh reads it, and the commit and tag messages gh turns into a body. Two checks:
# gitleaks' built-in rules, and an exact match against the secrets this machine actually holds
# (every gh account's token and environment variables named like a secret), which catches values
# whose format gitleaks does not know.
#
# Agent processes (CLAUDECODE or CODEX_THREAD_ID) also refuse repository deletion, token output,
# alias/extension execution, and merges other than an approved `ALLOW_GH_MERGE=1 gh pr merge`.
# REST writes use an endpoint allowlist; GraphQL documents are checked after JSON decoding. auth
# and secret otherwise carry credentials by design, and git's credential helper must remain usable.
# Fails closed; the merge prefix is the only override.
#
# Every run, people's included, also picks the github.com account itself instead of leaving it to
# the active account in hosts.yml, which any session can switch; see use_account_for.
#
# Not covered: secrets split across words, interactive editor input, and commits gh pushes. Agent
# policy does not cover deliberately cleared markers, direct upstream binaries, or other API clients.

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

agent=0
[[ -n ${CLAUDECODE:-} || -n ${CODEX_THREAD_ID:-} ]] && agent=1
if ((agent)); then
        case "${1-}" in
        "" | --version | -h | --help) ;;
        agent-task | alias | api | attestation | auth | browse | cache | codespace | cs | completion | config | discussion | extension | ext | gist | gpg-key | help | issue | label | licenses | org | pr | preview | project | release | repo | ruleset | run | search | secret | skill | ssh-key | status | variable | version | workflow) ;;
        -*) block "options before the gh command are ambiguous; put them after the subcommand" ;;
        *) block "the command is not a recognised gh built-in; aliases and extensions are not allowed" ;;
        esac
        sub=""
        sub_args=("${@:2}")
        i=0
        # api parses its own options below; other commands must name their subcommand unambiguously.
        while [[ ${1-} != api ]] && ((i < ${#sub_args[@]})); do
                case ${sub_args[i]} in
                -R | --repo | --hostname)
                        ((i + 1 < ${#sub_args[@]})) && [[ -n ${sub_args[i + 1]} && ${sub_args[i + 1]} != -* ]] ||
                                block "a pre-subcommand option is missing its value"
                        ((i += 2))
                        continue
                        ;;
                -R?* | --repo=?* | --hostname=?*) ;;
                -h | --help)
                        ((i + 1 == ${#sub_args[@]})) || block "help before a subcommand is ambiguous; put help last"
                        ;;
                -*) block "put command-specific options after the gh subcommand" ;;
                *)
                        sub=${sub_args[i]}
                        break
                        ;;
                esac
                ((i += 1))
        done
        case "${1-} $sub" in
        "pr merge")
                # The variable records the user's approval. An agent can set it unasked, so it stops an
                # accidental merge, not a deliberate one.
                [[ ${ALLOW_GH_MERGE-} == 1 ]] ||
                        block "merging needs the user's approval. Ask the user; once they approve this merge, rerun the same command prefixed with ALLOW_GH_MERGE=1"
                for arg in "${sub_args[@]}"; do
                        [[ $arg == --admin || $arg == --admin=* ]] && block "--admin bypasses branch protection and is not allowed from agents"
                done
                ;;
        "repo delete") block "repository deletion requires the user to run gh" ;;
        "alias set" | "alias import") block "gh aliases are not allowed; run the full command" ;;
        "extension install" | "extension exec" | "extension upgrade" | "ext install" | "ext exec" | "ext upgrade") block "extension installation and execution are not allowed from agents" ;;
        "auth token") block "token output is not allowed from agents" ;;
        "auth switch" | "auth logout") block "the active account is shared by every session; choose the account for one command with GH_ACCOUNT=<login> instead" ;;
        "config get")
                ((i += 1))
                while ((i < ${#sub_args[@]})); do
                        case ${sub_args[i]} in
                        -h | --host)
                                ((i + 1 < ${#sub_args[@]})) || block "config get is missing its host value"
                                ((i += 2))
                                continue
                                ;;
                        -h?* | --host=* | --help) ;;
                        --) ;;
                        -*) block "config get option is not recognised by the guard" ;;
                        oauth_token) block "token output is not allowed from agents" ;;
                        esac
                        ((i += 1))
                done
                ;;
        "auth status")
                for arg in "${sub_args[@]}"; do
                        [[ $arg == --show-token || $arg == --show-token=* || $arg =~ ^-[^-]*t ]] &&
                                block "token output is not allowed from agents"
                done
                ;;
        esac
fi

hosts_file="${GH_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}/gh}/hosts.yml"
# Prints "<host> <account>" for every account logged in to gh.
hosts_accounts() {
        [[ -r $hosts_file ]] || return 0
        local line host=""
        while IFS= read -r line; do
                if [[ $line =~ ^([^[:space:]#][^:]*):[[:space:]]*$ ]]; then
                        host=${BASH_REMATCH[1]}
                elif [[ -n $host && $line =~ ^\ {8}([^[:space:]:]+):[[:space:]]*$ ]]; then
                        printf '%s %s\n' "$host" "${BASH_REMATCH[1]}"
                fi
        done <"$hosts_file"
}

# Exports GH_TOKEN for the github.com account this process uses, in this order: the account named
# by GH_ACCOUNT; a token the caller exported; for owner O, the account named A-O where A is another
# logged-in account; otherwise the account the other account names are built on. The A-O rule
# chooses a work account without its organisation being written down here, and requiring A keeps
# the account someone-example-org from also matching an owner named org. The active account in
# hosts.yml is never consulted: every shell and agent session shares it, so a switch in one
# session would otherwise change the account of all the others.
use_account_for() {
        local owner=$1 host account base match="" count=0 accounts=() bases=()
        while read -r host account; do
                [[ $host == github.com ]] && accounts+=("$account")
        done < <(hosts_accounts)
        shopt -s nocasematch
        if [[ -n ${GH_ACCOUNT-} ]]; then
                for account in "${accounts[@]+"${accounts[@]}"}"; do
                        [[ $account == "$GH_ACCOUNT" ]] && match=$account
                done
                shopt -u nocasematch
                [[ -n $match ]] || block "GH_ACCOUNT names $GH_ACCOUNT, which is not logged in to github.com"
        else
                if [[ -n ${GH_TOKEN-}${GITHUB_TOKEN-} ]] || ((${#accounts[@]} == 0)); then
                        shopt -u nocasematch
                        return 0
                fi
                if [[ $owner =~ ^[A-Za-z0-9][A-Za-z0-9-]*$ ]]; then
                        for account in "${accounts[@]}"; do
                                for base in "${accounts[@]}"; do
                                        if [[ $account == "$base-$owner" ]]; then
                                                match=$account
                                                ((count += 1))
                                                break
                                        fi
                                done
                        done
                fi
                if ((count == 0)); then
                        if ((${#accounts[@]} == 1)); then
                                match=${accounts[0]}
                        else
                                for base in "${accounts[@]}"; do
                                        for account in "${accounts[@]}"; do
                                                if [[ $account == "$base"-?* ]]; then
                                                        bases+=("$base")
                                                        break
                                                fi
                                        done
                                done
                                ((${#bases[@]} == 1)) && match=${bases[0]}
                        fi
                fi
                shopt -u nocasematch
                ((count <= 1)) || block "more than one logged-in account is named *-$owner, so the account to use is ambiguous"
                [[ -n $match ]] || block "no default github.com account: exactly one logged-in account must be the one the other account names are built on. Choose one with GH_ACCOUNT=<login>"
        fi
        GH_TOKEN=$(unset GH_TOKEN GITHUB_TOKEN && "$real_gh" auth token -h github.com -u "$match" 2>/dev/null) &&
                [[ -n $GH_TOKEN ]] || block "could not read the token of $match"
        export GH_TOKEN
}

# The owner of the repository a gh run is about: --repo, then GH_REPO, then a gh api endpoint, an
# OWNER/REPO argument of gh repo, or a github.com URL argument, then the origin remote of the
# current directory. Prints nothing when an explicit repository is not on github.com.
target_owner() {
        local arg prev="" repo="" url
        for arg in "$@"; do
                case $prev in -R | --repo) repo=$arg ;; esac
                case $arg in -R?*) repo=${arg#-R} ;; --repo=*) repo=${arg#--repo=} ;; esac
                prev=$arg
        done
        repo=${repo:-${GH_REPO-}}
        if [[ -n $repo ]]; then
                repo=${repo#https://}
                repo=${repo#github.com/}
                [[ $repo =~ ^([^/]+)/[^/]+/?$ ]] && printf '%s' "${BASH_REMATCH[1]}"
                return 0
        fi
        for arg in "${@:2}"; do
                if [[ ${1-} == api && $arg =~ ^(https://api\.github\.com)?/*(repos|orgs)/+([^/?{}]+) ]]; then
                        printf '%s' "${BASH_REMATCH[3]}"
                        return 0
                elif [[ ${1-} == repo && $arg =~ ^([A-Za-z0-9][A-Za-z0-9-]*)/[A-Za-z0-9._-]+$ ]]; then
                        printf '%s' "${BASH_REMATCH[1]}"
                        return 0
                elif [[ $arg =~ ^https://github\.com/([^/?#]+)/ ]]; then
                        printf '%s' "${BASH_REMATCH[1]}"
                        return 0
                fi
        done
        url=$("$git" remote get-url origin 2>/dev/null) || return 0
        [[ $url =~ github\.com[:/]+([^/]+)/ ]] && printf '%s' "${BASH_REMATCH[1]}"
        return 0
}

# git asks for a credential with key=value lines on standard input; with credential.useHttpPath
# they carry the repository path, whose first segment is the owner.
if [[ "${1-} ${2-}" == "auth git-credential" ]]; then
        request=()
        while IFS= read -r line || [[ -n $line ]]; do
                [[ -n $line ]] || break
                request+=("$line")
        done
        if [[ ${3-} == get ]]; then
                protocol="" host="" path=""
                for line in "${request[@]+"${request[@]}"}"; do
                        case $line in
                        protocol=*) protocol=${line#*=} ;;
                        host=*) host=${line#*=} ;;
                        path=*) path=${line#*=} ;;
                        esac
                done
                [[ $protocol == https && $host == github.com ]] && use_account_for "${path%%/*}"
        fi
        exec "$real_gh" "$@" < <(printf '%s\n' "${request[@]+"${request[@]}"}" "")
fi
# `gh auth token` naming no account prints the token use_account_for picks for the current
# directory; any other form goes to gh unchanged.
if [[ "${1-} ${2-}" == "auth token" ]]; then
        token_args=("${@:3}")
        i=0
        while ((i < ${#token_args[@]})); do
                case ${token_args[i]} in
                -h | --hostname) [[ ${token_args[i + 1]-} == github.com ]] && ((i += 1)) || break ;;
                -hgithub.com | -h=github.com | --hostname=github.com) ;;
                *) break ;;
                esac
                ((i += 1))
        done
        if ((i == ${#token_args[@]})); then
                use_account_for "$(target_owner auth token)"
                if [[ -n ${GH_TOKEN-}${GITHUB_TOKEN-} ]]; then
                        printf '%s\n' "${GH_TOKEN:-$GITHUB_TOKEN}"
                        exit 0
                fi
        fi
fi
case "${1-}" in
"" | auth | help | completion | version | --version | -h | --help) ;;
*) use_account_for "$(target_owner "$@")" ;;
esac

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

if ((agent)) && [[ ${1-} == api ]]; then
        method="" endpoint="" input="" has_body=0 typed_placeholder=0
        # gh's own pattern for typed fields, (\:(owner|repo|branch)\b|\{[a-z]+\}), with \b spelled out.
        placeholder='(:(owner|repo|branch)([^A-Za-z0-9_]|$)|[{][a-z]+[}])'
        api_args=("${@:2}")
        i=0
        while ((i < ${#api_args[@]})); do
                arg=${api_args[i]}
                option=${arg%%=*}
                value=""
                case $arg in
                -X?* | -f?* | -F?* | -H?* | -q?* | -t?* | -p?*)
                        option=${arg:0:2}
                        value=${arg:2}
                        value=${value#=}
                        ;;
                --*=*) value=${arg#*=} ;;
                esac
                case $option in
                -X | --method | -f | -F | --raw-field | --field | --input | -H | --header | -q | --jq | -t | --template | -p | --preview | --hostname | --cache)
                        if [[ $arg == "$option" ]]; then
                                ((i += 1))
                                ((i < ${#api_args[@]})) || block "gh api is missing an option value"
                                value=${api_args[i]}
                        fi
                        case $option in
                        -X | --method)
                                method=$(printf '%s' "$value" | tr '[:lower:]' '[:upper:]') || block "gh api method could not be parsed"
                                [[ -n $method ]] || block "gh api method could not be parsed"
                                ;;
                        --input)
                                has_body=1
                                input=$value
                                [[ -n $input ]] || block "gh api has an empty input path"
                                ;;
                        -f | -F | --raw-field | --field)
                                has_body=1
                                [[ $value == *=* ]] || block "gh api field could not be parsed"
                                # Filled variables stay data; only the document fields can gain a mutation name.
                                if [[ $option == -F || $option == --field ]] && [[ ${value%%=*} == query || ${value%%=*} == operationName ]] &&
                                        [[ ${value#*=} =~ $placeholder ]]; then
                                        typed_placeholder=1
                                fi
                                if [[ $option == -F || $option == --field ]] && [[ ${value#*=} == @* ]]; then
                                        source_path=${value#*=@}
                                        [[ $source_path == - || $source_path == /dev/stdin ]] && source_path=$stdin_copy
                                        [[ -n $source_path && -f $source_path && -r $source_path ]] || block "gh api field file could not be read"
                                fi
                                ;;
                        esac
                        ;;
                -i | --include | --paginate | --slurp | --silent | --verbose | --allow-escape-sequences) [[ $arg == "$option" ]] || block "gh api flag could not be parsed" ;;
                --help | -h) ;;
                -*) block "gh api option is not recognised by the guard" ;;
                *)
                        [[ -z $endpoint ]] || block "gh api has multiple endpoints"
                        endpoint=$arg
                        ;;
                esac
                ((i += 1))
        done
        [[ -n $endpoint ]] || block "gh api endpoint could not be parsed"
        if [[ -z $method ]]; then
                if ((has_body)); then method=POST; else method=GET; fi
        fi
        case $method in GET | HEAD | POST | PATCH | PUT | DELETE | OPTIONS) ;; *) block "gh api method could not be parsed" ;; esac
        path=${endpoint#http*://*/}
        path=${path#/}
        path=${path%%\?*}
        path=${path#api/v3/}
        if [[ $path == graphql || $path == api/graphql ]]; then
                ((typed_placeholder == 0)) || block "GraphQL typed-field placeholders cannot be verified; use a literal raw field"
                [[ $endpoint != *\?* ]] || block "GraphQL query strings cannot be verified; use a query field or JSON input"
                doc="$work/graphql"
                cp "$content" "$doc" || block "could not record the GraphQL document"
                if [[ -n $input ]]; then
                        source_path=$input
                        [[ $source_path == - || $source_path == /dev/stdin ]] && source_path=$stdin_copy
                        [[ -n $source_path && -f $source_path && -r $source_path ]] || block "the GraphQL input could not be read"
                        "$jq" -er 'if type == "object" and (.query | type) == "string" then .. | strings else error("invalid body") end' \
                                <"$source_path" >>"$doc" 2>/dev/null || block "the GraphQL input is not a readable JSON query"
                fi
                grep -qE '(^|[^A-Za-z0-9_])(mergePullRequest|enablePullRequestAutoMerge|enqueuePullRequest|mergeBranch|deleteRepository)([^A-Za-z0-9_]|$)' "$doc"
                case $? in
                0) block "GraphQL merge, auto-merge, merge queue, and repository deletion are not allowed" ;;
                1) ;;
                *) block "the GraphQL check did not complete" ;;
                esac
        elif [[ $method != GET && $method != HEAD ]]; then
                seg='[A-Za-z0-9_{}-][A-Za-z0-9_.{}-]*'
                n='[0-9]+'
                allowed="^repos/$seg/$seg/(pulls|issues)(/$n(/(comments|reviews|requested_reviewers|labels|assignees))?)?$"
                allowed+="|^repos/$seg/$seg/pulls/$n/(comments/$n/replies|reviews/$n(/events)?)$"
                allowed+="|^repos/$seg/$seg/(pulls|issues)/comments/$n$"
                allowed+="|^repos/$seg/$seg/git/(refs|tags)$"
                [[ $path =~ $allowed ]] || block "the REST write endpoint is not on the agent allowlist; ask the user"
        fi
fi

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
while read -r host account; do
        held=$("$real_gh" auth token -h "$host" -u "$account" 2>/dev/null) ||
                block "could not read the token of $account on $host to check against it"
        add_secret "$held"
done < <(hosts_accounts)
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
