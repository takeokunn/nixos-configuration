{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  inherit (pkgs) lib;

  aiPromptsPath = ../ai-prompts;
  commandPromptsPath = aiPromptsPath + "/commands";

  codexRuntimeAdapter = ''
    <codex_runtime_adapter>
      <purpose>Apply the shared Claude/OpenCode orchestration prompt in Codex while preserving Codex tool semantics.</purpose>
      <rules priority="critical">
        <rule>The SSoT for core behavior is ai-prompts/CLAUDE.md. The SSoT for slash-command skill bodies is ai-prompts/commands/*.md.</rule>
        <rule>When the shared prompt mentions Claude-only mechanisms, translate the intent to the Codex tools available in the current session instead of treating those names as literal requirements.</rule>
        <rule>Keep the shared policies authoritative: evidence-first work, Serena memory/symbol usage, parallel independent reads, no git write operations unless explicitly requested, and explicit verification reporting.</rule>
      </rules>
      <tool_mapping>
        <map from="Task tool / sub-agents / subagent_type">Use an explicit multi-agent tool when one is available; otherwise decompose with the plan tool, run independent investigations in parallel with multi_tool_use.parallel, and synthesize the results directly.</map>
        <map from="AskUserQuestion">Use request_user_input when available; otherwise ask the user a concise blocking question.</map>
        <map from="run_in_background">Use exec_command sessions for long-running processes and poll them before finishing.</map>
        <map from="Bash / Read / Edit / Write">Use exec_command for shell reads/commands, apply_patch for manual file edits, and Serena symbol tools for code navigation and targeted edits.</map>
        <map from="WebSearch / WebFetch">Use web.run when current external information is required; for library/framework APIs prefer Context7 official docs first.</map>
        <map from="Playwright MCP">Use Playwright MCP for browser verification, screenshots, console logs, and interaction checks.</map>
        <map from="DeepWiki MCP">Use DeepWiki for repository-level questions about public GitHub repositories.</map>
      </tool_mapping>
      <execution_guidance>
        <rule>Respond in the user's language; for Japanese sessions, use Japanese unless the user asks otherwise.</rule>
        <rule>For repo work, activate Serena and check onboarding before symbolic investigation when Serena is available.</rule>
        <rule>If a shared command asks for delegation but no multi-agent tool is available, perform the delegated checks yourself and label the checked roles or concerns in the final synthesis.</rule>
        <rule>After changes, run the narrowest meaningful formatter, parser, or test command. If verification cannot be run, state exactly why.</rule>
      </execution_guidance>
    </codex_runtime_adapter>

  '';

  codexAgents = pkgs.writeText "codex-AGENTS.md" ''
    ${codexRuntimeAdapter}
    ${builtins.readFile (aiPromptsPath + "/CLAUDE.md")}
  '';

  commandFiles = builtins.readDir commandPromptsPath;
  skillNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: commandFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames commandFiles
    )
  );

  commandPromptToCodexSkill =
    skill:
    let
      content = builtins.readFile (commandPromptsPath + "/${skill}.md");
      lines = lib.splitString "\n" content;
      descriptionLine = builtins.elemAt lines 2;
      body = lib.concatStringsSep "\n" (lib.drop 4 lines);
    in
    assert lib.hasPrefix "description: " descriptionLine;
    pkgs.writeText "codex-skill-${skill}.md" ''
      ---
      name: ${skill}
      ${descriptionLine}
      ---
      ${body}
    '';

  skillFileAttrs = builtins.listToAttrs (
    map (skill: {
      name = "codex/skills/${skill}/SKILL.md";
      value = {
        source = commandPromptToCodexSkill skill;
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
    analytics = {
      enabled = false;
    };
    feedback = {
      enabled = false;
    };
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
      source = codexAgents;
      force = true;
    };
  };

  home.sessionVariables = {
    CODEX_DISABLE_TELEMETRY = "1";
    CODEX_HOME = "$HOME/.config/codex";
  };
}
