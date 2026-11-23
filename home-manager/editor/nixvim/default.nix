{
  config,
  options,
  pkgs,
  nurPkgs,
  ...
}:
let
  d = import ../../../shared/dracula-roles.nix { inherit config options pkgs; };
  hex = color: "#${color}";
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
  # dracula.nvim rather than dracula/vim: only dracula.nvim accepts a palette from outside, and
  # this one comes from Stylix. Stylix has nothing darker than its background, so menu and black
  # (darker than bg in Dracula) use base01, and the bright colors repeat the kitty palette.
  programs.nixvim.colorschemes.dracula-nvim.enable = true;
  programs.nixvim.colorschemes.dracula-nvim.settings.colors = {
    bg = hex d.background;
    fg = hex d.foreground;
    selection = hex d.selection;
    comment = hex d.comment;
    red = hex d.red;
    orange = hex d.orange;
    yellow = hex d.yellow;
    green = hex d.green;
    purple = hex d.purple;
    cyan = hex d.cyan;
    pink = hex d.pink;
    bright_red = hex d.red;
    bright_green = hex d.green;
    bright_yellow = hex d.yellow;
    bright_blue = hex d.blue;
    bright_magenta = hex d.pink;
    bright_cyan = hex d.cyan;
    bright_white = hex d.brightWhite;
    menu = hex d.surface;
    visual = hex d.selection;
    gutter_fg = hex d.comment;
    nontext = hex d.selection;
    white = hex d.muted;
    black = hex d.surface;
  };

  programs.nixvim.luaLoader.enable = true;
  programs.nixvim.performance.byteCompileLua.enable = true;
  programs.nixvim.performance.byteCompileLua.nvimRuntime = true;
  programs.nixvim.performance.byteCompileLua.configs = true;
  programs.nixvim.performance.byteCompileLua.plugins = true;

  programs.nixvim.clipboard.providers.pbcopy.enable = pkgs.stdenv.hostPlatform.isDarwin;
  programs.nixvim.clipboard.providers.wl-copy.enable = pkgs.stdenv.hostPlatform.isLinux;
}
