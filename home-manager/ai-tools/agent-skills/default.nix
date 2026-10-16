{
  anthropic-skills,
  paredit-cli-skills,
  aitools-skills,
  yomiyasu-skill,
  ...
}:
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

  programs.agent-skills.targets.claude.enable = true;
}
