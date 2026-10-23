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

  # The postFixup override below adds NODE_EXTRA_CA_CERTS to the claude wrapper's environment. When
  # the launching process sets CLAUDE_CODE_PROVIDER_MANAGED_BY_HOST, Claude Code ignores the
  # managed-settings entry for it (shared/claude-code-managed-settings.nix explains why the bundle is
  # needed) and logs "skipping settings-sourced NODE_EXTRA_CA_CERTS under host-managed provider".
  # --set-default lets a caller's own value win.
  #
  # The flag goes into upstream's single wrapProgram call, right after its CLAUDE_CODE_CERT_STORE
  # flag, because upstream notes that wrapping a second time renames the process. The assert fails
  # evaluation when upstream rewrites that call, instead of silently dropping the bundle.
  wrapperSplicePoint = "--set-default CLAUDE_CODE_CERT_STORE system";
  claude-code-fixed = llmAgentsPkgs.claude-code.overrideAttrs (old: {
    doInstallCheck = false;
    postFixup =
      assert pkgs.lib.assertMsg (pkgs.lib.hasInfix wrapperSplicePoint old.postFixup)
        "llm-agents claude-code: postFixup no longer contains `${wrapperSplicePoint}`, where NODE_EXTRA_CA_CERTS is spliced in";
      builtins.replaceStrings
        [ wrapperSplicePoint ]
        [
          "${wrapperSplicePoint} \\\n      --set-default NODE_EXTRA_CA_CERTS ${shared.claudeCodeCaBundle}"
        ]
        old.postFixup;
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

  programs.claude-code.commands = pkgs.lib.genAttrs [
    "ask"
    "bug"
    "define"
    "execute"
    "execute-full"
    "markdown"
    "qa"
  ] (name: builtins.readFile "${ai-prompts-path}/commands/${name}/SKILL.md");

  programs.claude-code.outputStyles = readFiles "${ai-prompts-path}/output-styles" [
    "explanatory-strict"
  ];

  programs.claude-code.hooks.block-destructive-git = builtins.readFile "${ai-prompts-path}/hooks/block-destructive-git.sh";
  programs.claude-code.hooks.block-bare-cd = builtins.readFile "${ai-prompts-path}/hooks/block-bare-cd.sh";
  programs.claude-code.hooks.aitools-rewrite = builtins.readFile "${ai-prompts-path}/hooks/aitools-rewrite.sh";
  programs.claude-code.hooks.prose-skill-gate = builtins.readFile "${ai-prompts-path}/hooks/prose-skill-gate.sh";

  programs.claude-code.mcpServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      programs.context7.enable = true;
      programs.playwright.enable = true;
      programs.slite.enable = true;
      programs.clickup.enable = true;
    }).config.settings.servers
    // shared.mcpServers { inherit nurPkgs; };
}
