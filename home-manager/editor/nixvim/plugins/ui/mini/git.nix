let
  utils = import ./utils.nix;
  inherit (utils) mkMiniKeymap;
in
{
  plugins.mini.modules = {
    diff = {
      view = {
        style = "sign";
        signs = {
          add = "│";
          change = "│";
          delete = "_";
        };
      };
    };

    git = { };
  };

  keymaps = [
    {
      mode = "n";
      key = "]h";
      action.__raw = "function() require('mini.diff').goto_hunk('next') end";
      options.desc = "Next diff hunk";
    }
    {
      mode = "n";
      key = "[h";
      action.__raw = "function() require('mini.diff').goto_hunk('prev') end";
      options.desc = "Previous diff hunk";
    }
    (mkMiniKeymap "n" "<leader>go" "diff" "toggle_overlay" "Toggle diff overlay")
  ];
}
