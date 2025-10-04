{
  pkgs,
  mcp-servers-nix,
  nodePkgs,
}:
let
  tomlFormat = pkgs.formats.toml { };

  config = mcp-servers-nix.lib.mkConfig pkgs {
    flavor = "codex";
    format = "toml-inline";
    fileName = ".mcp.toml";
    programs = {
      context7.enable = true;
      playwright.enable = true;
      terraform.enable = true;
      nixos.enable = true;
      serena = {
        enable = true;
        args = [
          "--context"
          "ide-assistant"
          "--enable-web-dashboard"
          "False"
        ];
      };
    };
  };
  settings = {
    model = "gpt-5-codex";
    model_reasoning_summary = "auto";
    network_access = true;
    approval_policy = "never";
    sandbox_mode = "danger-full-access";
    trust_level = "trusted";
    notify = [
      "terminal-notifier"
      "-message"
      "\"対応完了しました\""
      "-title"
      "\"Codex\""
      "-sound"
      "Blow"
    ];
    model_reasoning_effort = "high";
    tools.web_search = true;
  };
in
{
  programs.codex = {
    enable = true;
    package = pkgs.symlinkJoin {
      name = "codex";
      paths = [ nodePkgs."@openai/codex" ];
      nativeBuildInputs = with pkgs; [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/codex "--add-flags" "-c '$(cat ${config})'"
      '';
    };
    custom-instructions = builtins.readFile ./AGENTS.md;
  };

  home.file = {
    ".codex/config.toml".source = tomlFormat.generate "codex-config" settings;
    # ".codex/prompts/ask.md".source = ./prompts/ask.md;
    # ".codex/prompts/bug.md".source = ./prompts/bug.md;
    # ".codex/prompts/define.md".source = ./prompts/define.md;
    # ".codex/prompts/execute.md".source = ./prompts/execute.md;
    # ".codex/prompts/markdown.md".source = ./prompts/markdown.md;
  };
}
