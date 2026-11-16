#!/usr/bin/env bash
# Freeze the AI config at revision ca44fc04 for the eval harness.
#
# Usage: bash home-manager/ai-tools/evals/freeze-old-config.sh
#
# 1. Exports every file under home-manager/ai-tools/ at ca44fc04 into
#    ~/.local/state/ai-evals/old-config/ (paths relative to home-manager/ai-tools/).
# 2. Assembles ~/.local/state/ai-evals/config-old/ as a CLAUDE_CONFIG_DIR holding what
#    claude-code/default.nix and agent-skills/default.nix at ca44fc04 install into ~/.claude.
#    home-manager writes context, agents, commands and output styles verbatim (no frontmatter is
#    added), and agent-skills copies skill trees unchanged, so plain copies reproduce them.
# 3. Creates an empty ~/.local/state/ai-evals/config-new/.
#
# Rerunning replaces only the managed entries in config-old (CLAUDE.md, agents, commands,
# output-styles, skills, hooks, MANIFEST), so a login stored there survives.
#
# No settings.json is written. Hooks, outputStyle and permissions.defaultMode=bypassPermissions come
# from the system managed-settings file (/Library/Application Support/ClaudeCode/managed-settings.json
# on macOS), which applies to every CLAUDE_CONFIG_DIR. Its hook commands name ~/.claude/hooks/ by
# absolute path, not the config dir, so registering the hooks again in config-old/settings.json would
# run each hook twice per tool call. The hooks/ copy here is for reference and for diffing against
# ~/.claude/hooks. ~/.claude/settings.json is not copied: its hooks belong to a third-party tool and
# its model choice is not part of ca44fc04; pass the model explicitly when running the evals.
set -euo pipefail

REV=ca44fc04
STATE="$HOME/.local/state/ai-evals"
OLD="$STATE/old-config"
CFG="$STATE/config-old"
NEW="$STATE/config-new"
INSTALLED_SKILLS="$HOME/.claude/skills"

# agent-skills/default.nix at ca44fc04 enables every skill of the "paredit-cli" and "aitools" sources
# and "webapp-testing" (anthropic) and "yomiyasu" explicitly. Their sources are flake inputs, not files
# in this repository, so they are copied from the installed bundle.
EXTERNAL_SKILLS=(aitools paredit-cli webapp-testing yomiyasu)

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"

die() {
  echo "freeze-old-config: $*" >&2
  exit 1
}

# Removes a path only when it resolves inside $STATE.
safe_rm() {
  local target="$1" parent resolved
  [ -e "$target" ] || [ -L "$target" ] || return 0
  parent="$(cd "$(dirname "$target")" && pwd -P)"
  resolved="$parent/$(basename "$target")"
  case "$resolved" in
  "$state_real"/?*) rm -rf -- "$resolved" ;;
  *) die "refusing to delete $resolved: outside $state_real" ;;
  esac
}

mkdir -p "$STATE"
state_real="$(cd "$STATE" && pwd -P)"

jjr() {
  jj --ignore-working-copy -R "$repo_root" "$@"
}

# 1. Raw export.
files="$(jjr file list -r "$REV" home-manager/ai-tools)"
[ -n "$files" ] || die "no files under home-manager/ai-tools at $REV"
safe_rm "$OLD"
mkdir -p "$OLD"
while IFS= read -r path; do
  rel="${path#home-manager/ai-tools/}"
  mkdir -p "$OLD/$(dirname "$rel")"
  jjr file show -r "$REV" "$path" >"$OLD/$rel"
done <<<"$files"

# 2. config-old.
prompts="$OLD/ai-prompts"
mkdir -p "$CFG"
for entry in CLAUDE.md agents commands output-styles skills hooks MANIFEST; do
  safe_rm "$CFG/$entry"
done

cp "$prompts/CLAUDE.md" "$CFG/CLAUDE.md"

# claude-code/default.nix lists the agents explicitly; every file in ai-prompts/agents/ is listed.
mkdir -p "$CFG/agents"
cp "$prompts"/agents/*.md "$CFG/agents/"

mkdir -p "$CFG/commands"
for dir in "$prompts"/commands/*/; do
  name="$(basename "$dir")"
  cp "$dir/SKILL.md" "$CFG/commands/$name.md"
done

mkdir -p "$CFG/output-styles"
cp "$prompts/output-styles/explanatory-strict.md" "$CFG/output-styles/"

mkdir -p "$CFG/hooks"
cp "$prompts/hooks/block-destructive-git.sh" "$CFG/hooks/block-destructive-git"
cp "$prompts/hooks/aitools-rewrite.sh" "$CFG/hooks/aitools-rewrite"
chmod 755 "$CFG/hooks/block-destructive-git" "$CFG/hooks/aitools-rewrite"

mkdir -p "$CFG/skills"
custom_skills=()
for dir in "$OLD"/agent-skills/skills/*/; do
  [ -f "$dir/SKILL.md" ] || continue
  name="$(basename "$dir")"
  cp -R "$dir" "$CFG/skills/$name"
  custom_skills+=("$name")
done

external_lines=()
for name in "${EXTERNAL_SKILLS[@]}"; do
  src="$INSTALLED_SKILLS/$name"
  [ -f "$src/SKILL.md" ] || die "external skill $name not installed at $src"
  cp -RL "$src" "$CFG/skills/$name"
  chmod -R u+w "$CFG/skills/$name"
  # The store path identifies which build of the skill was frozen.
  external_lines+=("$name <- installed bundle $(readlink "$src" || echo "$src")")
done

{
  echo "config-old assembled from revision $REV by freeze-old-config.sh"
  echo
  echo "From $REV (home-manager/ai-tools/ai-prompts):"
  echo "  CLAUDE.md, agents/*.md, commands/<name>.md (commands/<name>/SKILL.md),"
  echo "  output-styles/explanatory-strict.md, hooks/block-destructive-git, hooks/aitools-rewrite"
  echo
  echo "Skills from $REV (agent-skills/skills):"
  printf '  %s\n' "${custom_skills[@]}"
  echo
  echo "Skills from the currently installed ~/.claude/skills bundle, NOT from $REV"
  echo "(flake inputs; their content is whatever the installed bundle holds):"
  printf '  %s\n' "${external_lines[@]}"
  echo
  echo "Not reproduced:"
  echo "  settings.json: hooks, outputStyle and defaultMode come from the system managed settings,"
  echo "    whose hook commands reference ~/.claude/hooks (identical to $REV when frozen)."
  echo "  skills/claude-code-home-manager: home-manager's generated MCP plugin; evals run with"
  echo "    --strict-mcp-config."
  echo "  commands as Codex skills: agent-skills installs them for codex only."
} >"$CFG/MANIFEST"

mkdir -p "$NEW"

echo "old-config: $(find "$OLD" -type f | wc -l | tr -d ' ') files"
echo "config-old: $(find "$CFG" -type f | wc -l | tr -d ' ') files"
