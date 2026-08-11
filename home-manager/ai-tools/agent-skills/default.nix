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
  paredit-cli-skills,
  ...
}:
{
  programs.agent-skills.enable = true;

  programs.agent-skills.sources.custom.path = ./skills;
  programs.agent-skills.sources.custom.filter.maxDepth = 1;
  programs.agent-skills.sources.anthropic.path = anthropic-skills;
  programs.agent-skills.sources.anthropic.subdir = "skills";
  # Declared but left out of enableAll below, same mechanism as scientific further down: the
  # catalog is built from every declared source regardless of what enableAll selects, so the
  # source block has to stay even though nothing from it is installed.
  programs.agent-skills.sources.cloudflare.path = cloudflare-skills;
  programs.agent-skills.sources.cloudflare.subdir = "skills";
  programs.agent-skills.sources.hashicorp.path = hashicorp-agent-skills;
  programs.agent-skills.sources.hashicorp.filter.maxDepth = 4;
  # Declared but left out of enableAll below; see the cloudflare comment above.
  programs.agent-skills.sources.deno.path = deno-skills;
  programs.agent-skills.sources.deno.subdir = "skills";
  programs.agent-skills.sources.aws.path = aws-agent-skills;
  programs.agent-skills.sources.aws.subdir = "skills";
  programs.agent-skills.sources.microsoft.path = microsoft-skills;
  programs.agent-skills.sources.microsoft.subdir = ".github/skills";
  # Exclude mcp-builder and skill-creator which conflict with anthropic source
  programs.agent-skills.sources.microsoft.filter.nameRegex =
    "cloud-solution-architect|continual-learning|copilot-sdk|entra-agent-id|frontend-design-review|github-issue-creator|podcast-generation";
  # Declared but left out of enableAll below, so none of its skills are installed. The source
  # block still has to stay: the catalog is built from every declared source and throws on a
  # duplicate skill id regardless of what enableAll selects, so removing the regex would break
  # evaluation even though nothing here is selected. Delete the source and the flake input
  # together or not at all.
  programs.agent-skills.sources.scientific.path = scientific-skills;
  programs.agent-skills.sources.scientific.subdir = "skills";
  # Exclude docx, pdf, pptx, xlsx which conflict with anthropic source
  # Regex matches any string that is NOT exactly one of those four names
  programs.agent-skills.sources.scientific.filter.nameRegex =
    "[^p]..|.[^d].|..[^f]|[^dpx]...|d[^o]..|do[^c].|doc[^x]|p[^p]..|pp[^t].|ppt[^x]|x[^l]..|xl[^s].|xls[^x]|.{5,}";
  programs.agent-skills.sources.context7.path = context7-skills;
  programs.agent-skills.sources.context7.subdir = "skills";
  programs.agent-skills.sources."ast-grep".path = ast-grep-skill;
  programs.agent-skills.sources."ast-grep".subdir = "ast-grep/skills";
  programs.agent-skills.sources."paredit-cli".path = paredit-cli-skills;
  programs.agent-skills.sources."paredit-cli".subdir = "skills";

  # scientific is absent on purpose. Its skills cover protein assays, single-cell RNA, quantum
  # circuits and clinical trials, none of which this account works on, and their descriptions sit
  # in every session's context whether or not a skill is ever invoked — the body loads on demand,
  # the description does not. Re-add the string here to restore them.
  programs.agent-skills.skills.enableAll = [
    "custom"
    "anthropic"
    "hashicorp"
    "aws"
    "microsoft"
    "context7"
    "ast-grep"
    "paredit-cli"
  ];

  programs.agent-skills.targets.claude.enable = true;
}
