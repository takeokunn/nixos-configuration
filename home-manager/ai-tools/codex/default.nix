{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  promptsPath = ./prompts;
  skillsPath = ./prompts/skills;
  inherit (pkgs) lib;

  skillFileAttrs =
    let
      dirs = builtins.readDir skillsPath;
      skillNames = builtins.filter (n: dirs.${n} == "directory") (builtins.attrNames dirs);
    in
    builtins.listToAttrs (
      map (skill: {
        name = "codex/skills/${skill}/SKILL.md";
        value = {
          source = "${skillsPath}/${skill}/SKILL.md";
          force = true;
        };
      }) skillNames
    );

  nixMcpServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      programs.context7.enable = true;
      programs.playwright.enable = true;
    }).config.settings.servers;

  pickMcpServer =
    server:
    builtins.intersectAttrs {
      type = null;
      command = null;
      args = null;
      env = null;
      url = null;
    } server;

  codexSettings = {
    model = "gpt-5.5";
    model_provider = "openai";
    approval_policy = "on-request";
    sandbox_mode = "danger-full-access";
    model_auto_compact_token_limit = 50000;
    # Nix manages the codex package; disable the built-in updater.
    check_for_update_on_startup = false;
    suppress_unstable_features_warning = true;
    analytics = { enabled = false; };
    feedback = { enabled = false; };
  };

  codexMcpServers = {
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

  cleanAttrs = lib.filterAttrs (_: v: v != null && v != [ ] && v != { });

  toTomlInline =
    value:
    if builtins.isString value then
      builtins.toJSON value
    else if builtins.isInt value then
      toString value
    else if builtins.isBool value then
      lib.boolToString value
    else if builtins.isList value then
      "[${lib.concatMapStringsSep ", " toTomlInline value}]"
    else if builtins.isAttrs value then
      "{ ${
        lib.concatStringsSep ", " (
          lib.mapAttrsToList (name: attrValue: "${name} = ${toTomlInline attrValue}") (cleanAttrs value)
        )
      } }"
    else
      throw "Unsupported Codex config value: ${builtins.typeOf value}";

  codexSettingFlags = lib.mapAttrsToList (
    name: value: "-c ${lib.escapeShellArg "${name}=${toTomlInline value}"}"
  ) codexSettings;

  codexMcpFlags = lib.mapAttrsToList (
    name: server: "-c ${lib.escapeShellArg "mcp_servers.${name}=${toTomlInline server}"}"
  ) codexMcpServers;

  codexWrapped = pkgs.symlinkJoin {
    name = "codex";
    paths = [ llmAgentsPkgs.codex ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/codex ${
        lib.concatMapStringsSep " " (flag: "--add-flags ${lib.escapeShellArg flag}") (
          codexSettingFlags ++ codexMcpFlags
        )
      }
    '';
  };
in
{
  home.packages = [ codexWrapped ];

  xdg.configFile = skillFileAttrs // {
    "codex/AGENTS.md" = {
      source = "${promptsPath}/AGENTS.md";
      force = true;
    };
  };

  home.sessionVariables = {
    CODEX_DISABLE_TELEMETRY = "1";
    CODEX_HOME = "$HOME/.config/codex";
  };
}
