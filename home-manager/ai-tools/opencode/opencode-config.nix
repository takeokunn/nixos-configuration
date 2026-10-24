{
  pkgs,
  mcp-servers-nix,
  models,
}:
let
  inherit (pkgs) lib;

  shared = import ../shared { inherit lib; };

  # mcp-servers-nix resolves the packaged servers; the opencode flavor's file output is not
  # reachable as an attribute set, so evaluate the generic flavor and translate it here.
  packagedServers =
    (mcp-servers-nix.lib.evalModule pkgs {
      flavor = "claude";
      programs = {
        context7.enable = true;
        playwright.enable = true;
      };
    }).config.settings.servers;

  # The shared deny list cannot hold `rm -rf /*`, because the glob also matches `/tmp/x`; Claude Code
  # blocks the literal forms with a regex rule in ai-prompts/hooks/rules.toml. opencode runs no hooks, so
  # it takes the broad pattern and accepts that it also denies `rm -rf /tmp/x`.
  opencodeExtraDenyPatterns = [ "rm -rf /*" ];

  # `git fetch` is left out of the read allows: Claude Code and Codex vet its URL, option, and refspec
  # forms with block-destructive-git, which opencode never runs, so fetch falls to the `git *` ask default.
  opencodeGitReads = lib.filter (r: r != "fetch") shared.gitReadSubcommands;

  providerTimeoutOpts = {
    timeout = 600000;
    chunkTimeout = 60000;
  };
in
{
  plugin = [ "oh-my-openagent" ];
  model = models.default.model;
  small_model = models.default.model;
  share = "disabled";
  autoupdate = false;

  provider."opencode-go".options = providerTimeoutOpts;
  provider.openai.options = providerTimeoutOpts;
  provider.anthropic.options = providerTimeoutOpts;
  provider."github-copilot".options = providerTimeoutOpts;

  compaction.auto = true;
  compaction.prune = true;
  compaction.reserved = 10000;

  watcher.ignore = [
    ".devenv/**"
    ".direnv/**"
    ".terraform/**"
    "result/**"
    ".git/**"
    "node_modules/**"
    "flake.lock"
  ];

  mcp = lib.mapAttrs (_: shared.mcpServerToOpencode) packagedServers;

  # Rules are matched in order and the last match wins. JSON keys serialise sorted, so the
  # catch-all "*" comes first, then "git *" (ask), and each narrower git pattern follows it: the
  # read-only allows and the write denies never overlap. A git command that matches neither list
  # stays at ask.
  permission = {
    bash = {
      "*" = "allow";
      "git" = "ask";
      "git *" = "ask";
      "jj git push*" = "ask";
      "gh pr create*" = "ask";
      "gh pr merge*" = "ask";
    }
    // lib.genAttrs (shared.bashDenyPatterns ++ opencodeExtraDenyPatterns) (_: "deny")
    // lib.listToAttrs (
      lib.concatMap (r: [
        (lib.nameValuePair "git ${r}" "allow")
        (lib.nameValuePair "git ${r} *" "allow")
      ]) opencodeGitReads
    );
    edit = "allow";
    read = "allow";
    glob = "allow";
    grep = "allow";
    webfetch = "allow";
  };
}
