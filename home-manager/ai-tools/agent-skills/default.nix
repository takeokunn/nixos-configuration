{
  anthropic-skills,
  aws-agent-skills,
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
  programs.agent-skills.sources.aws.path = aws-agent-skills;
  programs.agent-skills.sources.aws.subdir = "skills";
  programs.agent-skills.sources."ast-grep".path = ast-grep-skill;
  programs.agent-skills.sources."ast-grep".subdir = "ast-grep/skills";
  programs.agent-skills.sources."paredit-cli".path = paredit-cli-skills;
  programs.agent-skills.sources."paredit-cli".subdir = "skills";

  programs.agent-skills.skills.enableAll = [
    "custom"
    "anthropic"
    "aws"
    "ast-grep"
    "paredit-cli"
  ];

  programs.agent-skills.targets.claude.enable = true;
}
