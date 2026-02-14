{
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
  deno-skills,
  aws-agent-skills,
  microsoft-skills,
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
        filter.nameRegex = ".*-(ts|rust)";
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
      ];
    };
    targets.claude.enable = true;
  };
}
