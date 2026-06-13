{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  promptsPath = ./prompts;
  skillsPath = ./prompts/skills;

  skillFileAttrs =
    let
      dirs = builtins.readDir skillsPath;
      skillNames = builtins.filter (n: dirs.${n} == "directory") (builtins.attrNames dirs);
    in
    builtins.listToAttrs (
      map (skill: {
        name = ".codex/skills/${skill}/SKILL.md";
        value = { source = "${skillsPath}/${skill}/SKILL.md"; };
      }) skillNames
    );

  nixMcpServers = (mcp-servers-nix.lib.evalModule pkgs {
    programs.context7.enable = true;
    programs.playwright.enable = true;
  }).config.settings.servers;

  pickMcpServer =
    server: builtins.intersectAttrs { command = null; args = null; env = null; } server;

  codexConfig = (pkgs.formats.toml { }).generate "codex-config.toml" {
    model = "gpt-5.5";
    model_provider = "openai";
    approval_policy = "on-request";
    sandbox_mode = "workspace-write";
    model_auto_compact_token_limit = 50000;

    mcp_servers = {
      context7 = pickMcpServer nixMcpServers.context7;
      playwright = pickMcpServer nixMcpServers.playwright;
      serena = {
        command = "${nurPkgs.serena}/bin/serena";
        args = [
          "start-mcp-server"
          "--context"
          "claude-code"
          "--enable-web-dashboard"
          "false"
        ];
      };
      deepwiki = {
        type = "http";
        url = "https://mcp.deepwiki.com/mcp";
      };
    };
  };
in
{
  home.packages = [ llmAgentsPkgs.codex ];

  home.file = skillFileAttrs // {
    ".codex/AGENTS.md" = {
      source = "${promptsPath}/AGENTS.md";
      force = true;
    };
    ".codex/config.toml" = {
      source = codexConfig;
      force = true;
    };
  };

  home.sessionVariables = {
    CODEX_DISABLE_TELEMETRY = "1";
  };
}
