{ pkgs, sources }:
let
  # 基本設定のインポート
  options = import ./options.nix;
  globals = import ./globals.nix;
  keymaps = import ./keymaps.nix;

  # プラグイン設定のインポート
  # lsp = import ./plugins/lsp.nix { inherit pkgs; };
  # uiConfig = import ./plugins/ui.nix { inherit pkgs; };
  # telescopeConfig = import ./plugins/telescope.nix { inherit pkgs; };
  # gitConfig = import ./plugins/git.nix { inherit pkgs sources; };
  # editorConfig = import ./plugins/editor.nix { inherit pkgs sources; };
  # japaneseConfig = import ./plugins/japanese.nix { inherit pkgs sources; };
  # miscConfig = import ./plugins/misc.nix { inherit pkgs sources; };
in
{
  imports = [
    options
    globals
    keymaps
  ];

  programs.nixvim = {
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
