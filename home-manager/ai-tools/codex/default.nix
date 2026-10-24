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

    Apply the shared Claude/OpenCode orchestration prompt in Codex while preserving Codex tool semantics.

    ### Critical rules

    - The SSoT for core behavior is ai-prompts/CLAUDE.md. The SSoT for slash-command skill bodies is ai-prompts/commands/*/SKILL.md. The SSoT for Codex custom agents is ai-prompts/agents/*.md.
    - When the shared prompt mentions Claude-only mechanisms, translate the intent to the Codex tools available in the current session instead of treating those names as literal requirements.
    - Keep the shared policies authoritative: evidence-first work, Serena memory/symbol usage, parallel independent reads, jj as the default version-control workflow, version-control writes only through jj (git is read-only), and explicit verification reporting.

    ### Tool mapping

    | Shared mechanism | Codex equivalent |
    |---|---|
    | Task tool / sub-agents / subagent_type | Use the available multi-agent tool and agent catalog. If a requested role is unavailable, perform its checks yourself and identify that limitation; do not recreate a role that requires unavailable runtime restrictions. |
    | AskUserQuestion | Use request_user_input when available and permitted by the current mode and question type; otherwise ask the user a concise blocking question. |
    | run_in_background | Use exec_command sessions for long-running processes and poll them before finishing. |
    | Bash / Read / Edit / Write | Use exec_command for shell reads/commands, apply_patch for manual file edits, and Serena symbol tools for code navigation and targeted edits. |
    | WebSearch / WebFetch | Use web.run when current external information is required; for library/framework APIs prefer Context7 official docs first. |
    | Playwright MCP | Use Playwright MCP for browser verification, screenshots, console logs, and interaction checks. |

    ### Execution guidance

    - After changes, run the narrowest meaningful formatter, parser, or test command. If verification cannot be run, state exactly why.

  '';

  codexAgents = ''
    ${codexRuntimeAdapter}
    ${builtins.readFile (aiPromptsPath + "/CLAUDE.md")}
  '';

  agentFiles = builtins.readDir agentPromptsPath;
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

  codexHookScript =
    name: pkgs.writeShellScript name (builtins.readFile (aiPromptsPath + "/hooks/${name}.sh"));

  # aitools-rewrite defaults to Claude Code's output shape. Codex rejects an updatedInput that is not
  # paired with permissionDecision "allow", so its entry passes `codex` from a wrapper rather than
  # relying on how Codex splits a hook's command string.
  codexAitoolsRewrite = pkgs.writeShellScript "aitools-rewrite-codex" ''
    exec ${codexHookScript "aitools-rewrite"} codex
  '';

  # execpolicy matches whole argv tokens from the front, so only a literal command, or one ending in
  # " *" after literal tokens, has an equivalent. Patterns such as `rm -rf /etc*` or `gh auth status *-t*`
  # return null and are listed in the generated file instead of being dropped unseen.
  denyPatternToTokens =
    pattern:
    let
      literal = lib.removeSuffix " *" pattern;
    in
    if lib.hasInfix "*" literal then null else lib.splitString " " literal;

  # A deny prefix that is also the start of a read-only git command would forbid that read as well:
  # the bare "git stash" pattern becomes ["git","stash"], which covers `git stash list`. Such a
  # pattern is left out of the rules and listed as unenforced; block-destructive-git handles it.
  readOnlyGitTokens = map (r: [ "git" ] ++ lib.splitString " " r) shared.gitReadSubcommands;
  shadowsRead =
    tokens: lib.any (read: lib.take (builtins.length tokens) read == tokens) readOnlyGitTokens;

  convertedDenyTokens = lib.unique (
    lib.filter (tokens: tokens != null && !shadowsRead tokens) (
      map denyPatternToTokens shared.bashDenyPatterns
    )
  );
  unconvertedDenyPatterns = lib.filter (
    p:
    let
      tokens = denyPatternToTokens p;
    in
    tokens == null || shadowsRead tokens
  ) shared.bashDenyPatterns;

  # With approval_policy "never" there is no prompt to fall back on, so these rules and the PreToolUse
  # hooks are the only guards on shell commands.
  codexDenyRules = ''
    # Generated from shared.bashDenyPatterns. Not expressible as an execpolicy prefix, or a prefix that
    # would also forbid a read-only git command, and so not enforced here:
    ${lib.concatMapStringsSep "\n" (p: "#   ${p}") unconvertedDenyPatterns}
    ${lib.concatMapStringsSep "\n" (
      tokens: ''prefix_rule(pattern = ${builtins.toJSON tokens}, decision = "forbidden")''
    ) convertedDenyTokens}
  '';

  codexMcpServers = {
    context7 = pickMcpServer nixMcpServers.context7;
    playwright = pickMcpServer nixMcpServers.playwright;
  }
  // removeAttrs (shared.mcpServers { inherit nurPkgs; }) [ "deepwiki" ];

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
    mcp_servers = lib.mapAttrs (
      _: server: lib.filterAttrs (_: v: v != null && v != [ ] && v != { }) server
    ) codexMcpServers;
  };

  # Registered once, here; settings.hooks stays unset so Codex sees a single hook source.
  codexHooks = {
    PreToolUse = [
      {
        matcher = "^Bash$";
        hooks =
          map (name: {
            type = "command";
            command = "${codexHookScript name}";
          }) shared.guardrailHookNames
          ++ [
            {
              type = "command";
              command = "${codexAitoolsRewrite}";
            }
          ];
      }
    ];
  };

  # Trusts the target directory when it lies under ~/ghq; see ghq-trust.sh.
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
      makeWrapper ${codexGhqTrust} $out/bin/codex --add-flags ${lib.escapeShellArg "${llmAgentsPkgs.codex}/bin/codex ${lib.escapeShellArgs codexForcedFlags}"}
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
    hooks = codexHooks;
    rules.deny = codexDenyRules;
  };

  xdg.configFile = agentFileAttrs;

  # CODEX_HOME itself comes from programs.codex (home.preferXdgDirectories).
  home.sessionVariables.CODEX_DISABLE_TELEMETRY = "1";
}
