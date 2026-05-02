{
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
  deno-skills,
  aws-agent-skills,
  microsoft-skills,
  scientific-skills,
  context7-skills,
}:
{
  programs.agent-skills = {
    enable = true;
    sources = {
      custom = {
        path = ./skills;
        filter.maxDepth = 1;
      };
      anthropic = {
        path = anthropic-skills;
        subdir = "skills";
      };
      cloudflare = {
        path = cloudflare-skills;
        subdir = "skills";
      };
      hashicorp = {
        path = hashicorp-agent-skills;
        filter.maxDepth = 4;
      };
      deno = {
        path = deno-skills;
        subdir = "skills";
      };
      aws = {
        path = aws-agent-skills;
        subdir = "skills";
      };
      microsoft = {
        path = microsoft-skills;
        subdir = ".github/skills";
        # Exclude mcp-builder and skill-creator which conflict with anthropic source
        filter.nameRegex = "cloud-solution-architect|continual-learning|copilot-sdk|entra-agent-id|frontend-design-review|github-issue-creator|podcast-generation";
      };
      scientific = {
        path = scientific-skills;
        subdir = "scientific-skills";
        # Exclude docx, pdf, pptx, xlsx which conflict with anthropic source
        # Regex matches any string that is NOT exactly one of those four names
        filter.nameRegex = "[^p]..|.[^d].|..[^f]|[^dpx]...|d[^o]..|do[^c].|doc[^x]|p[^p]..|pp[^t].|ppt[^x]|x[^l]..|xl[^s].|xls[^x]|.{5,}";
      };
      context7 = {
        path = context7-skills;
        subdir = "skills";
      };
    };
    skills = {
      enableAll = [
        "custom"
        "anthropic"
        "cloudflare"
        "hashicorp"
        "deno"
        "aws"
        "microsoft"
        "scientific"
        "context7"
      ];
    };
    targets.claude.enable = true;
  };
}
