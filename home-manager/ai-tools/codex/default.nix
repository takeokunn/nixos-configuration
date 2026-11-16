{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
  ...
}:
let
  inherit (pkgs) lib;

  shared = import ../shared { inherit lib; };

  aiPromptsPath = ../ai-prompts;
  agentPromptsPath = aiPromptsPath + "/agents";

  codexRuntimeAdapter = ''
    ## codex_runtime_adapter

    The sections below and the skills were written for Claude Code. Apply them in Codex with Codex tool semantics.

    - When a prompt names a Claude-only mechanism, translate the intent to the Codex tools available in the current session instead of treating the name as a literal requirement.
    - If the user's instructions conflict with a skill's instructions, follow the user's instructions.

    ### Tool mapping

    | Claude Code mechanism | Codex equivalent |
    |---|---|
    | Agent tool / sub-agents / subagent_type | Use the available multi-agent tool and agent catalog. If a requested role is unavailable, perform its checks yourself and identify that limitation; do not recreate a role that requires unavailable runtime restrictions. |
    | AskUserQuestion | Use request_user_input when available and permitted by the current mode and question type; otherwise ask the user a concise blocking question. |
    | run_in_background | Use exec_command sessions for long-running processes and poll them before finishing. |
    | Bash / Read / Edit / Write | Use exec_command for shell commands; file operations follow environment_facts. |
    | WebSearch / WebFetch | Use web.run when current external information is required. |
    | Playwright MCP | Use Playwright MCP for browser verification, screenshots, console logs, and interaction checks. |
  '';

  # Claude Code loads the output style separately; Codex gets its body appended so replies follow the same rules.
  outputStyleBody =
    (shared.parseFrontmatter (
      builtins.readFile (aiPromptsPath + "/output-styles/explanatory-strict.md")
    )).body;

  codexAgents = ''
    ${codexRuntimeAdapter}
    ${builtins.readFile (aiPromptsPath + "/CLAUDE.md")}
    ## output_style

    ${outputStyleBody}
  '';

  agentFiles = builtins.readDir agentPromptsPath;
  agentNames = map (name: lib.removeSuffix ".md" name) (
    # Codex role overrides cannot enforce per-agent sandbox or MCP restrictions.
    builtins.filter (
      name:
      agentFiles.${name} == "regular"
      && lib.hasSuffix ".md" name
      && !shared.agentIsReadOnly
        (shared.parseFrontmatter (builtins.readFile (agentPromptsPath + "/${name}"))).frontmatterLines
    ) (builtins.attrNames agentFiles)
  );

  agentPromptToCodexAgent =
    agent:
    let
      content = builtins.readFile (agentPromptsPath + "/${agent}.md");
      parsed = shared.parseFrontmatter content;
      nameLine = shared.findLineWithPrefix "name: " parsed.frontmatterLines;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
      name = lib.removePrefix "name: " nameLine;
      description = shared.decodeFrontmatterScalar (lib.removePrefix "description: " descriptionLine);
      claudeModel = lib.removePrefix "model: " (
        shared.findLineWithPrefix "model: " parsed.frontmatterLines
      );
      reasoningEffort =
        {
          claude-sonnet-5-5 = "medium";
          claude-opus-5-5 = "high";
        }
        .${claudeModel};
    in
    pkgs.writeText "codex-agent-${agent}.toml" ''
      name = ${builtins.toJSON name}
      description = ${builtins.toJSON description}
      model = ${builtins.toJSON codexSettings.model}
      model_reasoning_effort = ${builtins.toJSON reasoningEffort}
      developer_instructions = ${builtins.toJSON parsed.body}
    '';

  agentFileAttrs = builtins.listToAttrs (
    map (agent: {
      name = "codex/agents/${agent}.toml";
      value = {
        source = agentPromptToCodexAgent agent;
        force = true;
      };
    }) agentNames
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

  codexMcpServers = {
    context7 = pickMcpServer nixMcpServers.context7;
    playwright = pickMcpServer nixMcpServers.playwright;
  }
  // shared.mcpServers { inherit nurPkgs; };

  codexSettings = {
    model = "gpt-6.1-sol";
    model_reasoning_effort = "medium";
    # Nix manages the codex package; disable the built-in updater.
    check_for_update_on_startup = false;
    suppress_unstable_features_warning = true;
    analytics = {
      enabled = false;
    };
    feedback = {
      enabled = false;
    };
    # A second layer behind the wrapper's unsets, for launches that bypass the wrapper. It covers
    # only this CODEX_HOME's config.toml, which Codex can rewrite. Patterns are wildcards, not regexes.
    shell_environment_policy.exclude = [
      "NIX_CONFIG"
      "GH_*"
      "GITHUB_*"
    ];
    mcp_servers = lib.mapAttrs (
      _: server: lib.filterAttrs (_: v: v != null && v != [ ] && v != { }) server
    ) codexMcpServers;
  };

  # The wrapper limits automatic trust to the configured owners; see ghq-trust.sh.
  codexGhqTrust = pkgs.writeShellScript "codex-ghq-trust" (builtins.readFile ./ghq-trust.sh);

  # approval_policy and sandbox_mode are command-line overrides, not settings: config.toml is mutable
  # and Codex can rewrite it, while a -c flag always wins over it.
  codexForcedFlags = [
    "-c"
    ''approval_policy="never"''
    "-c"
    ''sandbox_mode="danger-full-access"''
  ];

  # The version in the name lets programs.codex choose the TOML layout from lib.getVersion.
  codexWrapped = pkgs.symlinkJoin {
    name = "codex-${llmAgentsPkgs.codex.version}";
    paths = [ llmAgentsPkgs.codex ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm $out/bin/codex
      makeWrapper ${codexGhqTrust} $out/bin/codex --add-flags ${lib.escapeShellArg "${llmAgentsPkgs.codex}/bin/codex ${lib.escapeShellArgs codexForcedFlags}"} ${
        lib.concatMapStringsSep " " (
          name: "--unset ${lib.escapeShellArg name}"
        ) shared.agentScrubbedEnvNames
      }
    '';
  };
in
{
  programs.codex = {
    enable = true;
    package = codexWrapped;
    # config.toml stays writable: Codex records project trust, hook trust and model notices in it.
    # Declared values win on every activation.
    mutableSettings = true;
    settings = codexSettings;
    context = codexAgents;
    # Safety hooks and deny rules live only in /etc/codex/requirements.toml.
  };

  xdg.configFile = agentFileAttrs;
}
