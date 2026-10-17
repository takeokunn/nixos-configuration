{
  config,
  pkgs,
  lib,
  anthropic-skills,
  paredit-cli-skills,
  aitools-skills,
  yomiyasu-skill,
  ...
}:
let
  commandSkills = import ./command-skills.nix { inherit pkgs; };

  # Same expansion as CODEX_HOME in codex/default.nix; the activation environment may not export it.
  codexSkillsDest = "\${CODEX_HOME:-$HOME/.config/codex}/skills";
in
{
  programs.agent-skills.enable = true;

  programs.agent-skills.sources.custom.path = ./skills;
  programs.agent-skills.sources.custom.filter.maxDepth = 1;
  programs.agent-skills.sources.anthropic.path = anthropic-skills;
  programs.agent-skills.sources.anthropic.subdir = "skills";
  programs.agent-skills.sources."paredit-cli".path = paredit-cli-skills;
  programs.agent-skills.sources."paredit-cli".subdir = "skills";
  programs.agent-skills.sources."aitools".path = aitools-skills;
  programs.agent-skills.sources."aitools".subdir = "skills";
  # Upstream ships the skill twice with identical SKILL.md, scripts and references (checked at the
  # locked rev): at the repository root and under skills/yomiyasu/. The nested copy is installed so the
  # bundle excludes the root's tests/, articles/ and evals/.
  programs.agent-skills.sources."yomiyasu".path = yomiyasu-skill;
  programs.agent-skills.sources."yomiyasu".subdir = "skills";
  programs.agent-skills.sources."commands".path = commandSkills.source;
  programs.agent-skills.sources."commands".filter.maxDepth = 1;

  # Installed skill metadata consumes catalog context even when its body is not loaded. Keep broad
  # sources selective; measure the emitted catalog for each client rather than the source inventory.
  # Before pruning, check the analysis window, source freshness, and subagent coverage. A missing
  # invocation record is not proof of no use, including bodies read without a Skill tool call.
  programs.agent-skills.skills.enableAll = [
    "custom"
    "paredit-cli"
    "aitools"
  ];

  # An unknown entry fails evaluation rather than being skipped, so this list cannot rot silently.
  programs.agent-skills.skills.enable = [
    "discernment-nudge"
    "webapp-testing"
    "yomiyasu"
  ];

  # Claude Code and OpenCode serve ai-prompts/commands/ as slash commands, so the same prompts are
  # skills only for Codex, which has no slash commands.
  programs.agent-skills.skills.explicit = lib.genAttrs commandSkills.names (_: {
    from = "commands";
    agents = [ "codex" ];
  });

  programs.agent-skills.targets.claude.enable = true;
  programs.agent-skills.targets.codex = {
    enable = true;
    dest = codexSkillsDest;
  };
  programs.agent-skills.targets.opencode.enable = true;

  # One-shot migration. The sync refuses a non-empty destination without its marker, and Codex's
  # skills directory used to be filled by home-manager links into the store. Adopt it only when every
  # file is such a link (or under Codex's own .system): anything else is hand-placed work that the
  # sync's --delete would remove. The marker fields follow has_valid_marker in agent-skills-nix
  # scripts/sync.sh at the locked rev. Remove this entry once every host has been activated.
  home.activation.adoptCodexSkills = lib.hm.dag.entryBetween [ "agent-skills" ] [ "writeBoundary" ] ''
    codex_skills="''${CODEX_HOME:-$HOME/.config/codex}/skills"
    codex_marker="$codex_skills/.agent-skills-managed.json"
    if [ -d "$codex_skills" ] && [ ! -e "$codex_marker" ] && [ -z "''${DRY_RUN:-}" ]; then
      foreign="$(${pkgs.findutils}/bin/find "$codex_skills" -mindepth 1 -path "$codex_skills/.system" -prune -o ! -type d ! -lname '/nix/store/*' -print -quit)"
      if [ -z "$foreign" ]; then
        ${pkgs.jq}/bin/jq -n --arg bundle "${config.programs.agent-skills.targetBundlePaths.codex}" \
          '{schemaVersion: 1, managedBy: "agent-skills-nix", mode: "global", bundle: $bundle, target: "codex", structure: "symlink-tree"}' \
          > "$codex_marker"
      else
        echo "adoptCodexSkills: not adopting $codex_skills; unmanaged entry: $foreign" >&2
      fi
    fi
  '';
}
