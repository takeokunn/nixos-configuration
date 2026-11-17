{
  config,
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
  # --set-default lets a caller's own value win. The same splice also unsets
  # shared.agentScrubbedEnvNames, so tokens in the launching shell never reach Claude Code's commands.
  #
  # The flags go into upstream's single wrapProgram call, right after its --argv0 flag, because
  # wrapping a second time renames the process. The assert fails evaluation when upstream rewrites
  # that call, instead of silently dropping the bundle and the unsets.
  wrapperSplicePoint = "--argv0 claude";
  claude-code-fixed = llmAgentsPkgs.claude-code.overrideAttrs (old: {
    doInstallCheck = false;
    postFixup =
      assert pkgs.lib.assertMsg (pkgs.lib.hasInfix wrapperSplicePoint old.postFixup)
        "llm-agents claude-code: postFixup no longer contains `${wrapperSplicePoint}`, where NODE_EXTRA_CA_CERTS and the env unsets are spliced in";
      builtins.replaceStrings
        [ wrapperSplicePoint ]
        [
          (
            "${wrapperSplicePoint} \\\n      --set-default NODE_EXTRA_CA_CERTS ${shared.claudeCodeCaBundle}"
            + pkgs.lib.concatMapStrings (
              name: " \\\n      --unset ${pkgs.lib.escapeShellArg name}"
            ) shared.agentScrubbedEnvNames
          )
        ]
        old.postFixup;
  });

  commandNames = builtins.attrNames (
    pkgs.lib.filterAttrs (_: type: type == "directory") (
      builtins.readDir (ai-prompts-path + "/commands")
    )
  );

  agentNames = map (pkgs.lib.removeSuffix ".md") (
    builtins.attrNames (
      pkgs.lib.filterAttrs (name: type: type == "regular" && pkgs.lib.hasSuffix ".md" name) (
        builtins.readDir (ai-prompts-path + "/agents")
      )
    )
  );

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
  # Pinned: the module's default moves to the XDG config directory once home.stateVersion reaches 26.11
  # or home.preferXdgDirectories is set, and the managed hook commands hardcode this path.
  programs.claude-code.configDir = "${config.home.homeDirectory}/.claude";
  programs.claude-code.context = builtins.readFile "${ai-prompts-path}/CLAUDE.md";
  # mutableSettings merges the declared keys into a writable `~/.claude/settings.json` on activation
  # instead of placing a read-only store symlink, so Claude Code can still persist a `/model` choice
  # and tools can still install hook entries there. Declared keys win on every activation.
  # `hooks` is deliberately not declared: the merge replaces arrays, which would erase hook entries
  # other tools installed. Policy (hooks, permissions, env) lives in the managed-settings file,
  # `shared/claude-code-managed-settings.nix`; the hook scripts below land in `~/.claude/hooks/`,
  # where those managed settings reference them.
  programs.claude-code.mutableSettings = true;
  programs.claude-code.settings = {
    model = "opus";
    modelSettings.claude-opus-5-5.effortLevel = "high";
  };

  programs.claude-code.agents = readFiles "${ai-prompts-path}/agents" agentNames;

  programs.claude-code.commands = pkgs.lib.genAttrs commandNames (
    name: builtins.readFile "${ai-prompts-path}/commands/${name}/SKILL.md"
  );

  programs.claude-code.outputStyles = readFiles "${ai-prompts-path}/output-styles" [
    "explanatory-strict"
  ];

  programs.claude-code.hooks.block-destructive-git = pkgs.writeShellScript "block-destructive-git" ''
    # Keep jj on the caller's PATH so the scan uses the same binary as the push.
    export PATH=${
      pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.jq
        pkgs.perl
        pkgs.python3
      ]
    }:"$PATH"
    export GITLEAKS_BIN=${pkgs.gitleaks}/bin/gitleaks
    exec ${pkgs.bash}/bin/bash ${ai-prompts-path}/hooks/block-destructive-git.sh "$@"
  '';
  programs.claude-code.hooks.aitools-rewrite = builtins.readFile "${ai-prompts-path}/hooks/aitools-rewrite.sh";

  programs.claude-code.mcpServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      programs.context7.enable = true;
      programs.playwright.enable = true;
      programs.slite.enable = true;
    }).config.settings.servers
    // shared.mcpServers {
      inherit nurPkgs;
      context = "claude-code";
    };
}
