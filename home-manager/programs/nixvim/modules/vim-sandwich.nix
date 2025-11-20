{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.plugins.vim-sandwich;
in
{
  options.plugins.vim-sandwich = {
    enable = lib.mkEnableOption "vim-sandwich configuration";

    skipSpace = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Skip spaces when adding surroundings";
    };
  };

  config = lib.mkIf cfg.enable {
    extraPlugins = [ pkgs.vimPlugins.vim-sandwich ];

    extraConfigLua = ''
      vim.call('operator#sandwich#set', 'add', 'char', 'skip_space', ${
        if cfg.skipSpace then "1" else "0"
      })
    '';
  };
}
