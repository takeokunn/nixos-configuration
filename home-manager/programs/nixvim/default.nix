{ pkgs, sources }:
let
  # 基本設定のインポート
  optionsConfig = import ./options.nix;
  globalsConfig = import ./globals.nix;
  keymapsConfig = import ./keymaps.nix;

  # プラグイン設定のインポート
  lspConfig = import ./plugins/lsp.nix { inherit pkgs; };
  uiConfig = import ./plugins/ui.nix { inherit pkgs; };
  telescopeConfig = import ./plugins/telescope.nix { inherit pkgs; };
  gitConfig = import ./plugins/git.nix { inherit pkgs sources; };
  editorConfig = import ./plugins/editor.nix { inherit pkgs sources; };
  japaneseConfig = import ./plugins/japanese.nix { inherit pkgs sources; };
  miscConfig = import ./plugins/misc.nix { inherit pkgs sources; };

  # すべての設定をマージ
  mergedConfig = pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate (pkgs.lib.recursiveUpdate optionsConfig globalsConfig) keymapsConfig) lspConfig) uiConfig) telescopeConfig) gitConfig) editorConfig) japaneseConfig) miscConfig;
in
{
  programs.nixvim = pkgs.lib.recursiveUpdate mergedConfig {
    enable = true;

    # syntax on
    extraConfigLua = ''
      vim.cmd.syntax("on")
    '';
  };

  # fishの設定
  programs.fish = {
    interactiveShellInit = ''
      set -x MANPAGER "nvim -c ASMANPAGER -"
    '';

    shellAliases = {
      aibo = "nvim -c 'Aibo claude --dangerously-skip-permissions'";
    };
  };
}
