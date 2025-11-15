{ pkgs, sources }:
let
  customPackages = import ./packages { inherit pkgs sources; };
  plugins = import ./plugins { inherit pkgs sources customPackages; };
  opts = import ./opts;
  keymaps = import ./keymaps;
in
{
  programs.nixvim = {
    inherit opts keymaps;

    enable = true;
    enableMan = true;
    defaultEditor = true;

    editorconfig.enable = true;
    colorschemes.dracula.enable = true;

    globals = {
      mapleader = ",";
      maplocalleader = ",";
    };

    luaLoader.enable = true;
    performance.byteCompileLua.enable = true;

    clipboard.providers = {
      pbcopy.enable = pkgs.stdenv.isDarwin;
      wl-copy.enable = pkgs.stdenv.isLinux;
    };
  }
  // plugins;
}
