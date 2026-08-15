{ pkgs, nurPkgs, ... }:
let
  customPackages = import ./packages { inherit nurPkgs; };
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
  programs.nixvim.opts = opts;
  programs.nixvim.keymaps = keymaps;
  programs.nixvim.globals = globals;
  programs.nixvim.userCommands = userCommands;
  programs.nixvim.extraConfigLua = extraConfigLua;
  programs.nixvim.plugins = plugins.plugins;
  programs.nixvim.extraPlugins = plugins.extraPlugins;

  programs.nixvim.enable = true;
  programs.nixvim.version.enableNixpkgsReleaseCheck = false;
  # nixpkgs' pandoc lacks Lua support on this cache, which nixvim's man-docs
  # build needs (`pandoc --lua-filter`); disable manpage generation to unblock builds.
  programs.nixvim.enableMan = false;

  programs.nixvim.editorconfig.enable = true;
  programs.nixvim.colorschemes.dracula.enable = true;

  programs.nixvim.luaLoader.enable = true;
  programs.nixvim.performance.byteCompileLua.enable = true;
  programs.nixvim.performance.byteCompileLua.nvimRuntime = true;
  programs.nixvim.performance.byteCompileLua.configs = true;
  programs.nixvim.performance.byteCompileLua.plugins = true;

  programs.nixvim.clipboard.providers.pbcopy.enable = pkgs.stdenv.isDarwin;
  programs.nixvim.clipboard.providers.wl-copy.enable = pkgs.stdenv.isLinux;
}
