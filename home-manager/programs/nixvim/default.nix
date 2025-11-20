{ pkgs, sources }:
let
  customPackages = import ./packages { inherit pkgs sources; };
  plugins = import ./plugins { inherit pkgs sources customPackages; };
  opts = import ./opts;
  baseKeymaps = import ./keymaps;
  basicGlobals = import ./globals;

  keymaps = baseKeymaps ++ (plugins.keymaps or [ ]);
  globals = basicGlobals // (plugins.globals or { });
  userCommands = plugins.userCommands or { };
in
{
  programs.nixvim = {
    inherit
      opts
      keymaps
      globals
      userCommands
      ;
    inherit (plugins) plugins extraPlugins;

    enable = true;
    enableMan = true;
    defaultEditor = true;

    editorconfig.enable = true;
    colorschemes.dracula.enable = true;

    luaLoader.enable = true;
    performance.byteCompileLua = {
      enable = true;
      nvimRuntime = true;
      configs = true;
      plugins = true;
    };

    clipboard.providers = {
      pbcopy.enable = pkgs.stdenv.isDarwin;
      wl-copy.enable = pkgs.stdenv.isLinux;
    };

    _module.args = {
      inherit customPackages;
    };

    imports = [
      ./modules/vim-sandwich.nix
      ./modules/skkeleton.nix
      ./modules/oil-git-status.nix
    ];
  };
}
