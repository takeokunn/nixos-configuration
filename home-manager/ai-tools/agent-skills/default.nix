{
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
  deno-skills,
  aws-agent-skills,
  microsoft-skills,
  scientific-skills,
  context7-skills,
  ast-grep-skill,
}:
{
  programs.agent-skills.enable = true;

  programs.agent-skills.sources.custom.path = ./skills;
  programs.agent-skills.sources.custom.filter.maxDepth = 1;
  programs.agent-skills.sources.anthropic.path = anthropic-skills;
  programs.agent-skills.sources.anthropic.subdir = "skills";
  programs.agent-skills.sources.cloudflare.path = cloudflare-skills;
  programs.agent-skills.sources.cloudflare.subdir = "skills";
  programs.agent-skills.sources.hashicorp.path = hashicorp-agent-skills;
  programs.agent-skills.sources.hashicorp.filter.maxDepth = 4;
  programs.agent-skills.sources.deno.path = deno-skills;
  programs.agent-skills.sources.deno.subdir = "skills";
  programs.agent-skills.sources.aws.path = aws-agent-skills;
  programs.agent-skills.sources.aws.subdir = "skills";
  programs.agent-skills.sources.microsoft.path = microsoft-skills;
  programs.agent-skills.sources.microsoft.subdir = ".github/skills";
  # Exclude mcp-builder and skill-creator which conflict with anthropic source
  programs.agent-skills.sources.microsoft.filter.nameRegex =
    "cloud-solution-architect|continual-learning|copilot-sdk|entra-agent-id|frontend-design-review|github-issue-creator|podcast-generation";
  programs.agent-skills.sources.scientific.path = scientific-skills;
  programs.agent-skills.sources.scientific.subdir = "scientific-skills";
  # Exclude docx, pdf, pptx, xlsx which conflict with anthropic source
  # Regex matches any string that is NOT exactly one of those four names
  programs.agent-skills.sources.scientific.filter.nameRegex =
    "[^p]..|.[^d].|..[^f]|[^dpx]...|d[^o]..|do[^c].|doc[^x]|p[^p]..|pp[^t].|ppt[^x]|x[^l]..|xl[^s].|xls[^x]|.{5,}";
  programs.agent-skills.sources.context7.path = context7-skills;
  programs.agent-skills.sources.context7.subdir = "skills";
  programs.agent-skills.sources."ast-grep".path = ast-grep-skill;
  programs.agent-skills.sources."ast-grep".subdir = "ast-grep/skills";

  programs.agent-skills.skills.enableAll = [
    "custom"
    "anthropic"
    "cloudflare"
    "hashicorp"
    "deno"
    "aws"
    "microsoft"
    "scientific"
    "context7"
    "ast-grep"
  ];

  programs.agent-skills.targets.claude.enable = true;
}
