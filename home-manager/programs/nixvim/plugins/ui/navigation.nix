{ pkgs }:
{
  # mini.clue is configured in ui/mini.nix (replaces which-key)
  # Keep trouble and aerial - no direct mini.nvim equivalent

  plugins.trouble = {
    enable = true;
    settings = {
      keys = {
        "<Esc>" = "close";
      };
    };
  };

  plugins.aerial = {
    enable = true;
    settings = {
      backends = [
        "lsp"
        "treesitter"
      ];
      layout = {
        default_direction = "right";
        placement = "edge";
      };
      keymaps = {
        "<Esc>" = "actions.close";
      };
    };
    lazyLoad.settings.cmd = "AerialToggle";
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>a";
      action = "<cmd>AerialToggle<CR>";
      options.desc = "Toggle Aerial";
    }
  ];
}
