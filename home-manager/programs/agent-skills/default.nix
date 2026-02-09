{
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
}:
{
  programs.agent-skills = {
    enable = true;
    sources = {
      custom = {
        path = ./skills;
        filter.maxDepth = 2;
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
    };
    skills = {
      enableAll = [
        "custom"
        "anthropic"
        "cloudflare"
        "hashicorp"
      ];
    };
    targets.claude.enable = true;
  };
}
