#!/bin/bash
# Copies Claude Code's local-scope MCP servers to a new jj workspace of a ghq bare repo.
# Usage: copy-local-mcp-servers <repo> <target>
#
# `claude mcp add --scope local` stores a server in ~/.claude.json under
# .projects["<realpath>"].mcpServers, so a workspace created with `jj workspace add --no-colocate`
# under <repo>.git/.worktrees gets its own key and sees none of the repo's servers. The servers come
# from the bare root's key and, when the cwd is inside another workspace of the same repo, from that
# workspace's key; on a name clash the bare root's entry wins. Every message goes to stderr: the
# callers print the new path on stdout.
# Register ClickUp as `clickup`: the managed-settings deny rules match that name.
# Exit status: 0 when everything was copied or there was nothing to do, 1 on any failure, 2 on a
# wrong argument count.

set -euo pipefail

prog=copy-local-mcp-servers

warn() {
        printf '%s: %s\n' "$prog" "$1" >&2
}

if (($# != 2)); then
        warn "usage: $prog <repo> <target>"
        exit 2
fi

repo="$1"
target="$2"

# A non-bare repo's workspaces share its key, so there is nothing to copy.
if [[ -e $repo/.git || -L $repo/.git ]]; then
        exit 0
fi

settings="$HOME/.claude.json"
[[ -f $settings ]] || exit 0

root_key=$(realpath -- "$repo")
target_key=$(realpath -- "$target")

caller_key=""
if caller_root=$(jj --ignore-working-copy workspace root 2>/dev/null) &&
        default_root=$(jj --ignore-working-copy workspace root --name default 2>/dev/null) &&
        [[ $(realpath -- "$default_root") == "$root_key" ]]; then
        caller_root=$(realpath -- "$caller_root")
        if [[ $caller_root != "$root_key" && $caller_root != "$target_key" ]]; then
                caller_key=$caller_root
        fi
fi

if ! servers=$(jq --compact-output --arg root "$root_key" --arg caller "$caller_key" \
        '(if $caller == "" then {} else .projects[$caller].mcpServers // {} end)
    + (.projects[$root].mcpServers // {})' "$settings" 2>&1); then
        warn "cannot read $settings: $servers"
        exit 1
fi

[[ $servers == "{}" ]] && exit 0

# A name starting with "-" would be parsed as an option by claude.
bad_names=$(jq --raw-output '[keys[] | select(startswith("-"))] | join(", ")' <<<"$servers")
if [[ -n $bad_names ]]; then
        warn "refusing server names starting with '-': $bad_names"
        exit 1
fi

if ! command -v claude >/dev/null; then
        warn "claude is not on PATH; no MCP servers copied to $target_key"
        exit 1
fi

copied=0
failures=()
while IFS= read -r -d '' name && IFS= read -r -d '' json; do
        status=0
        output=$(cd -- "$target_key" && claude mcp add-json --scope local "$name" "$json" 2>&1) || status=$?
        if ((status == 0)); then
                copied=$((copied + 1))
        else
                failures+=("$name (exit $status): $output")
        fi
done < <(jq --raw-output0 'to_entries[] | .key, (.value | tojson)' <<<"$servers")

if ((${#failures[@]} > 0)); then
        warn "$copied copied, ${#failures[@]} failed for $target_key:$(printf '\n  %s' "${failures[@]}")"
        exit 1
fi

warn "copied $copied local MCP server(s) to $target_key"
