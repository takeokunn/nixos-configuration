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
  # FR-003: the custom skills also installed for claude-code, discovered the same
  # readDir way as commands/agents just below rather than a hardcoded list.
  customSkillsPath = ../agent-skills/skills;

  codexRuntimeAdapter = ''
    <codex_runtime_adapter>
      <purpose>Apply the shared Claude/OpenCode orchestration prompt in Codex while preserving Codex tool semantics.</purpose>
      <rules priority="critical">
        <rule>The SSoT for core behavior is ai-prompts/CLAUDE.md. The SSoT for slash-command skill bodies is ai-prompts/commands/*.md. The SSoT for Codex custom agents is ai-prompts/agents/*.md.</rule>
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
  agentFiles = builtins.readDir agentPromptsPath;
  skillNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: commandFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames commandFiles
    )
  );
  agentNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: agentFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames agentFiles
    )
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
      description = lib.removePrefix "description: " descriptionLine;
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

  # FR-003: codex searches $CODEX_HOME/skills (= ~/.config/codex/skills, set below), so the
  # custom skills land there via the same xdg.configFile mechanism as the command-derived
  # skills above. $CODEX_HOME/skills was chosen over ~/.agents/skills specifically because
  # ~/.agents/skills is ALSO read by opencode — opencode already discovers these same
  # skills globally from ~/.claude/skills, so writing them to ~/.agents/skills as well would
  # double-register every one of those names under opencode. codex has no such overlap:
  # $CODEX_HOME/skills is codex-only.
  customSkillDirEntries = builtins.readDir customSkillsPath;
  customSkillNames = builtins.filter (name: customSkillDirEntries.${name} == "directory") (
    builtins.attrNames customSkillDirEntries
  );

  # A custom skill and a command-derived codex skill both resolve to
  # codex/skills/<name>/SKILL.md; a name collision would have the second xdg.configFile entry
  # silently win rather than surface as a build problem, so fail the build instead.
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

  # Codex has no `programs.codex.hooks`-style module option and this file takes no `config`
  # argument (unlike claude-code/default.nix), so each script is built as a standalone Nix
  # store executable and referenced by its store path directly in codexSettings.hooks below.
  # A store path needs no $HOME/config-dir expansion, unlike claude-code's
  # `${config.programs.claude-code.configDir}/hooks/<name>` scheme.
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
    # FR-008: the same guardrail roster claude-code/default.nix registers (shared.guardrailHookNames),
    # ported to Codex's native hook mechanism. Both engines auto-detect exact-string vs. regex
    # matchers from the same character-class rule, so "^Bash$" and "Bash" are equivalent here;
    # "^Bash$" is used just to spell it out as an explicit regex.
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
    # No CLI-level auto-retry exists, so this is a documented quick switch rather than an
    # automatic one.
    profiles = {
      fallback = {
        model = "gpt-5.4-mini";
      };
    };
  };

  # FR-008: serena/deepwiki/metabase-mcp come from shared/default.nix, which is also where
  # metabase-mcp was missing from before this change (claude-code and opencode both already
  # had it). context7/playwright stay codex-local, matching claude-code and opencode each
  # declaring their own extra servers.
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
