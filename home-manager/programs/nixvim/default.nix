{ pkgs, sources }:
let
  # カスタムプラグインのインポート
  customPlugins = import ./plugins/custom { inherit pkgs sources; };

  # プラグイン設定のインポート
  allPlugins = import ./plugins { inherit pkgs sources customPlugins; };

  # コア設定
  core = import ./core.nix { inherit pkgs; };
  globals = import ./globals.nix;

  # オプション
  displayOpts = import ./options/display.nix;
  editingOpts = import ./options/editing.nix;
  searchOpts = import ./options/search.nix;

  # キーマップ
  basicKeymaps = import ./keymaps/basic.nix;
  windowKeymaps = import ./keymaps/window.nix;
in
{
  programs.nixvim = core // allPlugins // {
    globals = globals;
    opts = displayOpts // editingOpts // searchOpts;
    keymaps = basicKeymaps ++ windowKeymaps;
  };
}
