# home-manager wiring shared by the NixOS and nix-darwin hosts.
{
  inputs,
  system,
  username,
}:
{
  home-manager.useUserPackages = true;
  home-manager.users.${username}.imports = [ ../home-manager/advanced.nix ];
  home-manager.sharedModules = [
    inputs.nixvim.homeModules.nixvim
    inputs.agent-skills.homeManagerModules.default
    inputs.git-bulk-clean.homeManagerModules.default
  ];
  home-manager.extraSpecialArgs = {
    inherit system;
    inherit (inputs)
      nixpkgs
      mcp-servers-nix
      llm-agents
      cclens
      nur-packages
      anthropic-skills
      paredit-cli-skills
      aitools-skills
      yomiyasu-skill
      ollama-bge-m3
      emacs-overlay
      org-babel
      ;
  };
}
