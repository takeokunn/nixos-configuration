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
  commandPromptsPath = aiPromptsPath + "/commands";
  agentPromptsPath = aiPromptsPath + "/agents";
  customSkillsPath = ../agent-skills/skills;

  codexRuntimeAdapter = ''
    ## codex_runtime_adapter

    Apply the shared Claude/OpenCode orchestration prompt in Codex while preserving Codex tool semantics.

    ### Critical rules

    - The SSoT for core behavior is ai-prompts/CLAUDE.md. The SSoT for slash-command skill bodies is ai-prompts/commands/*.md. The SSoT for Codex custom agents is ai-prompts/agents/*.md.
    - When the shared prompt mentions Claude-only mechanisms, translate the intent to the Codex tools available in the current session instead of treating those names as literal requirements.
    - Keep the shared policies authoritative: evidence-first work, Serena memory/symbol usage, parallel independent reads, no git write operations unless explicitly requested, and explicit verification reporting.

    ### Tool mapping

    | Shared mechanism | Codex equivalent |
    |---|---|
    | Task tool / sub-agents / subagent_type | Use the available multi-agent tool and agent catalog. If a requested role is unavailable, perform its checks yourself and identify that limitation; do not recreate a role that requires unavailable runtime restrictions. |
    | AskUserQuestion | Use request_user_input when available and permitted by the current mode and question type; otherwise ask the user a concise blocking question. |
    | run_in_background | Use exec_command sessions for long-running processes and poll them before finishing. |
    | Bash / Read / Edit / Write | Use exec_command for shell reads/commands, apply_patch for manual file edits, and Serena symbol tools for code navigation and targeted edits. |
    | WebSearch / WebFetch | Use web.run when current external information is required; for library/framework APIs prefer Context7 official docs first. |
    | Playwright MCP | Use Playwright MCP for browser verification, screenshots, console logs, and interaction checks. |
    | DeepWiki MCP | Use DeepWiki for repository-level questions about public GitHub repositories. |

    ### Execution guidance

    - For repo work, activate Serena and check onboarding before symbolic investigation when Serena is available.
    - After changes, run the narrowest meaningful formatter, parser, or test command. If verification cannot be run, state exactly why.

  '';

  codexAgents = pkgs.writeText "codex-AGENTS.md" ''
    ${codexRuntimeAdapter}
    ${builtins.readFile (aiPromptsPath + "/CLAUDE.md")}
  '';

  commandFiles = builtins.readDir commandPromptsPath;
  agentFiles = builtins.readDir agentPromptsPath;
  skillNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: commandFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames commandFiles
    )
  );
  agentNames = map (name: lib.removeSuffix ".md" name) (
    # Codex 0.154.0 role overrides cannot enforce per-agent sandbox or MCP restrictions.
    builtins.filter (
      name:
      agentFiles.${name} == "regular"
      && lib.hasSuffix ".md" name
      && !shared.agentIsReadOnly
        (shared.parseFrontmatter (builtins.readFile (agentPromptsPath + "/${name}"))).frontmatterLines
    ) (builtins.attrNames agentFiles)
  );

  commandPromptToCodexSkill =
    skill:
    let
      content = builtins.readFile (commandPromptsPath + "/${skill}.md");
      parsed = shared.parseFrontmatter content;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
    in
    pkgs.writeText "codex-skill-${skill}.md" ''
      ---
      name: ${skill}
      ${descriptionLine}
      ---
      ${parsed.body}
    '';

  agentPromptToCodexAgent =
    agent:
    let
      content = builtins.readFile (agentPromptsPath + "/${agent}.md");
      parsed = shared.parseFrontmatter content;
      nameLine = shared.findLineWithPrefix "name: " parsed.frontmatterLines;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
      name = lib.removePrefix "name: " nameLine;
      description = shared.decodeFrontmatterScalar (lib.removePrefix "description: " descriptionLine);
    in
    pkgs.writeText "codex-agent-${agent}.toml" ''
      name = ${builtins.toJSON name}
      description = ${builtins.toJSON description}
      developer_instructions = ${builtins.toJSON parsed.body}
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

  agentFileAttrs = builtins.listToAttrs (
    map (agent: {
      name = "codex/agents/${agent}.toml";
      value = {
        source = agentPromptToCodexAgent agent;
        force = true;
      };
    }) agentNames
  );

  customSkillDirEntries = builtins.readDir customSkillsPath;
  customSkillNames = builtins.filter (name: customSkillDirEntries.${name} == "directory") (
    builtins.attrNames customSkillDirEntries
  );

  customSkillCollisions = builtins.filter (name: builtins.elem name skillNames) customSkillNames;

  customSkillFileAttrs =
    if customSkillCollisions != [ ] then
      throw ''
        codex: skill name collision between agent-skills/skills/ and ai-prompts/commands/: ${lib.concatStringsSep ", " customSkillCollisions}. Rename the skill directory or the command file so the two do not both resolve to
        codex/skills/<name>/SKILL.md.''
    else
      builtins.listToAttrs (
        map (name: {
          name = "codex/skills/${name}/SKILL.md";
          value = {
            source = customSkillsPath + "/${name}/SKILL.md";
            force = true;
          };
        }) customSkillNames
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

  codexHookScript =
    name: pkgs.writeShellScript name (builtins.readFile (aiPromptsPath + "/hooks/${name}.sh"));

  codexSettings = {
    model = "gpt-5.6-luna";
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
    hooks = {
      PreToolUse = [
        {
          matcher = "^Bash$";
          hooks = map (name: {
            type = "command";
            command = "${codexHookScript name}";
          }) shared.guardrailHookNames;
        }
      ];
    };
    # Manual escape hatch for a capacity-limited primary model: `codex --profile fallback`.
    profiles = {
      fallback = {
        model = "gpt-5.4-mini";
      };
    };
  };

  codexMcpServers = {
    context7 = pickMcpServer nixMcpServers.context7;
    playwright = pickMcpServer nixMcpServers.playwright;
  }
  // shared.mcpServers { inherit nurPkgs; };

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

  xdg.configFile =
    skillFileAttrs
    // agentFileAttrs
    // customSkillFileAttrs
    // {
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
