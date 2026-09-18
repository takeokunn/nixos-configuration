{
  anthropic-skills,
  ast-grep-skill,
  paredit-cli-skills,
  ...
}:
{
  programs.agent-skills.enable = true;

  programs.agent-skills.sources.custom.path = ./skills;
  programs.agent-skills.sources.custom.filter.maxDepth = 1;
  programs.agent-skills.sources.anthropic.path = anthropic-skills;
  programs.agent-skills.sources.anthropic.subdir = "skills";
  programs.agent-skills.sources."ast-grep".path = ast-grep-skill;
  programs.agent-skills.sources."ast-grep".subdir = "ast-grep/skills";
  programs.agent-skills.sources."paredit-cli".path = paredit-cli-skills;
  programs.agent-skills.sources."paredit-cli".subdir = "skills";

  # Every installed skill's name and description is resident in the system prompt of every session,
  # whether or not it ever fires, so a source is enabled wholesale only where most of it earns that.
  # anthropic is named per skill instead: its document and design skills (docx, pptx, xlsx, pdf,
  # canvas-design, theme-factory, slack-gif-creator, algorithmic-art, web-artifacts-builder,
  # brand-guidelines, frontend-design, doc-coauthoring, internal-comms, academy-guide) and the whole
  # aws source cost 11,515 bytes of description between them against one measured invocation.
  # Re-measure before adding a source back: count `tool_use` blocks with name=="Skill" over
  # `~/.claude/projects/**/*.jsonl`, counting paths containing `/subagents/` separately, since most
  # loads here come from agents rather than from the model reading a description.
  programs.agent-skills.skills.enableAll = [
    "custom"
    "ast-grep"
    "paredit-cli"
  ];

  # An unknown entry fails evaluation rather than being skipped, so this list cannot rot silently.
  programs.agent-skills.skills.enable = [
    "claude-api"
    "discernment-nudge"
    "mcp-builder"
    "outline"
    "skill-creator"
    "webapp-testing"
  ];

  programs.agent-skills.targets.claude.enable = true;
}
