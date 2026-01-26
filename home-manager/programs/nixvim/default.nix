{ pkgs, nurPkgs }:
let
  customPackages = import ./packages { inherit pkgs nurPkgs; };
  plugins = import ./plugins { inherit pkgs customPackages; };
  opts = import ./opts;
  baseKeymaps = import ./keymaps;
  basicGlobals = import ./globals;

  keymaps = baseKeymaps ++ (plugins.keymaps or [ ]);
  globals = basicGlobals // (plugins.globals or { });
  userCommands = plugins.userCommands or { };
  extraConfigLua = plugins.extraConfigLua or "";
in
{
  programs.nixvim = {
    inherit
      opts
      keymaps
      globals
      userCommands
      extraConfigLua
      ;
    inherit (plugins) plugins extraPlugins;

    enable = true;
    enableMan = true;
    defaultEditor = false;

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
  };
}
