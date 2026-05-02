{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
  anthropic-skills,
  cloudflare-skills,
  hashicorp-agent-skills,
  deno-skills,
  aws-agent-skills,
  microsoft-skills,
  scientific-skills,
  context7-skills,
}:
let
  claude-code = import ./claude-code { inherit pkgs llmAgentsPkgs mcp-servers-nix; };
  opencode = import ./opencode { inherit pkgs nurPkgs llmAgentsPkgs mcp-servers-nix; };
  agent-skills = import ./agent-skills {
    inherit
      anthropic-skills
      cloudflare-skills
      hashicorp-agent-skills
      deno-skills
      aws-agent-skills
      microsoft-skills
      scientific-skills
      context7-skills
      ;
  };
in
[ claude-code opencode agent-skills ./serena ]
