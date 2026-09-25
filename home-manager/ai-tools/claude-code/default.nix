{
  pkgs,
  nurPkgs,
  mcp-servers-nix,
  llmAgentsPkgs,
  ...
}:
let
  ai-prompts-path = ../ai-prompts;

  shared = import ../shared { inherit (pkgs) lib; };

  claude-code-fixed = llmAgentsPkgs.claude-code.overrideAttrs (_: {
    doInstallCheck = false;
  });

  readFiles =
    dir: names:
    builtins.listToAttrs (
      map (name: {
        inherit name;
        value = builtins.readFile "${dir}/${name}.md";
      }) names
    );
in
{
  programs.claude-code.enable = true;
  programs.claude-code.package = claude-code-fixed;
  programs.claude-code.context = builtins.readFile "${ai-prompts-path}/CLAUDE.md";
  # `programs.claude-code.settings` is deliberately unset. Setting it makes home-manager place
  # `~/.claude/settings.json` as a read-only store symlink (`home.file`, mode 444), and Claude Code
  # expects that file to be writable: it persists a `/model` choice there, and tools that install
  # hook entries resolve the path and write through it. Both fail silently against a store symlink.
  # Everything this repository pins now lives in the managed-settings file instead, declared once in
  # `shared/claude-code-managed-settings.nix` and placed per platform by `nix-darwin/config/` and
  # `nixos/config/`. The hook scripts below still land in `~/.claude/hooks/`, which is where those
  # managed settings reference them.

  programs.claude-code.agents = readFiles "${ai-prompts-path}/agents" [
    "code-quality"
    "design"
    "docs"
    "explore"
    "general-purpose"
    "infra"
    "performance"
    "quality-assurance"
    "security"
    "test"
    "verification"
  ];

  programs.claude-code.commands = readFiles "${ai-prompts-path}/commands" [
    "ask"
    "bug"
    "define"
    "execute"
    "execute-full"
    "markdown"
    "qa"
    "upstream"
  ];

  programs.claude-code.outputStyles = readFiles "${ai-prompts-path}/output-styles" [
    "explanatory-strict"
  ];

  programs.claude-code.hooks.block-destructive-git = builtins.readFile "${ai-prompts-path}/hooks/block-destructive-git.sh";
  programs.claude-code.hooks.block-bare-cd = builtins.readFile "${ai-prompts-path}/hooks/block-bare-cd.sh";
  programs.claude-code.hooks.rtk-rewrite =
    builtins.replaceStrings [ "@RTK_BIN@" ] [ "${llmAgentsPkgs.rtk}/bin/rtk" ]
      (builtins.readFile "${ai-prompts-path}/hooks/rtk-rewrite.sh");

  programs.claude-code.mcpServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      programs.context7.enable = true;
      programs.playwright.enable = true;
      programs.slite.enable = true;
      programs.clickup.enable = true;
    }).config.settings.servers
    // shared.mcpServers { inherit nurPkgs; };
}
